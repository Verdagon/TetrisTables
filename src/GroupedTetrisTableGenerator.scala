
case class ChunkedTetrisTable[K, V](
    hasher: K => Int,
    directory: Array[Option[DirectoryEntry]],
    combinedBuckets: Array[Option[(K, V)]]) {
  def get(key: K): Option[V] = {
    val hash = hasher(key);
    val indexInDirectory = hash % directory.length;
    directory(indexInDirectory) match {
      case None => None
      case Some(DirectoryEntry(bucketBeginIndex, bucketSize)) => {
        val indexInBucket = hash % bucketSize;
        val indexInCombinedBuckets = bucketBeginIndex + indexInBucket
        combinedBuckets(indexInCombinedBuckets) match {
          case None => None
          case Some((foundKey, foundValue)) => {
            if (foundKey == key) Some(foundValue) else None
          }
        }
      }
    }
  }
}

case class Table[K](members: Array[Option[K]])
case class BucketTable[K](bucketIndex: Int, table: Table[K])

case class DirectoryEntry(indexInTable: Int, size: Int)

case class IntermediateTetrisTable[K](
    bucketStartIndexByBucketIndex: Map[Int, Int],
    members: Array[Option[K]])

class GroupedTetrisTableGenerator[K, V] {
  def generateTetrisTable(map: Map[K, V], hasher: K => Int): ChunkedTetrisTable[K, V] = {
    if (map.isEmpty) {
      ChunkedTetrisTable[K, V](hasher, Array(), Array())
    }
    val hashes = map.keys.zip(map.keys).toMap.mapValues(hasher);

    val initialDirectorySize = Math.ceil(map.size / 4.0).toInt;
    val listBuckets = bucketize(hashes, initialDirectorySize);
    val tablesByBucketIndex =
      listBuckets.zipWithIndex.map({
        case (listBucket, index) => BucketTable(index, Table[K](hashifyBucket(hashes, listBucket)))
      })

    val bucketGroupSizeInitial = 300; // chosen arbitrarily, must experiment
    val numBucketGroups = Math.max(map.size / bucketGroupSizeInitial, 1);
    val hashBucketGroups = groupHashBuckets(tablesByBucketIndex, numBucketGroups);

    val directoriesAndTables: List[(Map[Int, DirectoryEntry], Table[K])] =
      hashBucketGroups.map(hashBucketsInGroup => {
        tetris(hashes, hashBucketsInGroup)
      })

    val (directoryMap, table) = combineGroups(0, directoriesAndTables)

    val directoryArray =
      listBuckets.indices.toArray.map(i => directoryMap.get(i))

    val tableWithKeysAndValues =
      table.map(maybeKey => maybeKey.map(key => (key -> map(key))))

    ChunkedTetrisTable[K, V](hasher, directoryArray, tableWithKeysAndValues.toArray)

    //    ChunkedTetrisTable[K, V](hasher, Array(), Array())
  }

  private def combineGroups(
      combinedTableSizeSoFar: Int,
      directoriesAndTables: List[(Map[Int, DirectoryEntry], Table[K])]):
  (Map[Int, DirectoryEntry], List[Option[K]]) = {
    directoriesAndTables match {
      case Nil => {
        (Map(), List())
      }
      case (headDirectory, headTable) :: tailDirectoriesAndTables => {
        val combinedTableSizeAfterThis = combinedTableSizeSoFar + headTable.members.length;
        val (tailDirectory, tailCombinedTable) =
          combineGroups(combinedTableSizeAfterThis, tailDirectoriesAndTables)
        val adjustedHeadDirectory =
          headDirectory.mapValues({
            case DirectoryEntry(indexInTable, size) => DirectoryEntry(indexInTable + combinedTableSizeSoFar, size)
          })
        val newDirectory = tailDirectory ++ adjustedHeadDirectory
        val combinedTable = headTable.members.toList ++ tailCombinedTable
        (newDirectory, combinedTable)
      }
    }
  }

  private def groupHashBuckets(
      bucketTables: List[BucketTable[K]],
      numBucketGroups: Int):
  List[List[BucketTable[K]]] = {
    val bucketTablesAndScores = bucketTables.map(bucketTable => (bucketTable, getBucketScore(bucketTable)));

    val bucketTablesByScore =
      bucketTablesAndScores.foldLeft(Map[(Int, Int), List[BucketTable[K]]]())({
        case (bucketTablesByScore, (bucketTable, score)) => {
          bucketTablesByScore + (score -> (bucketTable :: bucketTablesByScore.getOrElse(score, List())))
        }
      })

    val sortedBuckets = bucketTablesByScore.values.reduceRight(_ ++ _)

    val numberedHashBuckets =
      sortedBuckets.zipWithIndex.map({
        case (bucketTable, index) => (index % numBucketGroups, bucketTable)
      });
    val initialBucketGroups: Array[List[BucketTable[K]]] =
      (0 until numBucketGroups).map(_ => List()).toArray
    // Round robin insert them into the buckets
    val resultBucketGroups =
      numberedHashBuckets.foldLeft(initialBucketGroups)({
        case (oldBucketGroups, (groupIndex, hashBucket)) => {
          val oldBucketGroup = oldBucketGroups(groupIndex)
          val newBucketGroup = oldBucketGroup :+ hashBucket
          val newBucketGroups = oldBucketGroups.updated(groupIndex, newBucketGroup)
          newBucketGroups
        }
      })
    resultBucketGroups.toList
  }

  private def tetris(
      hashes: Map[K, Int],
      hashBuckets: List[BucketTable[K]]):
  (Map[Int, DirectoryEntry], Table[K]) = {
    // hashBuckets is already sorted
    tetrisInner(hashes, Map(), Vector(), hashBuckets)
  }

  private def getSpan(bucket: Array[Option[K]]): Int = {
    bucket
        .toList
        .foldLeft(List[Option[K]]())({
          case (Nil, None) => Nil
          case (previous, anything) => previous :+ anything
        })
        .foldRight(List[Option[K]]())({
          case (None, Nil) => Nil
          case (anything, subsequent) => anything :: subsequent
        })
        .length
  }

  private def getBucketScore(bucket: BucketTable[K]): (Int, Int) = {
    (getSpan(bucket.table.members), bucket.table.members.count(_.nonEmpty))
  }

  private def tetrisInner(
      hashes: Map[K, Int],
      directory: Map[Int, DirectoryEntry],
      combinedTable: Vector[Option[K]],
      hashBuckets: List[BucketTable[K]]):
  (Map[Int, DirectoryEntry], Table[K]) = {
    hashBuckets match {
      case Nil => (directory, Table[K](combinedTable.toArray))
      case thisHashBucket :: remainingHashBuckets => {
        val (newCombinedBuckets, bucketStartIndex) = merge(hashes, combinedTable, thisHashBucket.table)

        //        directory.get(thisHashBucket.bucketIndex) match {
        //          case Some((existingBucketStartIndex, size)) => assert(bucketStartIndex == thisHashBucket.bucketIndex)
        //          case _ => throw new RuntimeException("NO")
        //        }
        assert(!directory.contains(thisHashBucket.bucketIndex))

        val newDirectoryEntry =
          DirectoryEntry(bucketStartIndex, thisHashBucket.table.members.length);
        val newDirectoryMapEntry = (thisHashBucket.bucketIndex -> newDirectoryEntry)
        val newDirectory = (directory + newDirectoryMapEntry)
        tetrisInner(hashes, newDirectory, newCombinedBuckets, remainingHashBuckets)
      }
    }
  }

  private def merge(
      hashes: Map[K, Int],
      combinedBuckets: Vector[Option[K]],
      bucket: Table[K]):
  (Vector[Option[K]], Int) = {
    mergeInner(hashes, combinedBuckets, bucket, 0)
  }

  private def mergeInner(
      hashes: Map[K, Int],
      tetrisTable: Vector[Option[K]],
      bucket: Table[K],
      indexToInsertBucket: Int):
  (Vector[Option[K]], Int) = {
    val mergesCleanly =
      tetrisTable.drop(indexToInsertBucket).zip(bucket.members).forall({
        case (None, _) => true
        case (_, None) => true
        case (_, _) => false
      })
    if (mergesCleanly) {
      val tetrisTableEndIndex = tetrisTable.size
      val bucketEndIndexInTetrisTable = indexToInsertBucket + bucket.members.length
      val tetrisTableNewSize = Math.max(tetrisTableEndIndex, bucketEndIndexInTetrisTable)
      val numEmptiesNeededAtEndOfTetrisTable = tetrisTableNewSize - tetrisTable.size
      val expandedTetrisTable = tetrisTable ++ (0 until numEmptiesNeededAtEndOfTetrisTable).map(_ => None)
      val mergedTetrisTable =
        bucket.members.zipWithIndex.foldLeft(expandedTetrisTable)({
          case (currentTetrisTable, (bucketMember, indexInBucket)) => {
            val indexInTable = indexToInsertBucket + indexInBucket;
            val currentTableMember = currentTetrisTable(indexInTable)
            val newTableMember =
              (currentTableMember, bucketMember) match {
                case (tetrisTableMember, None) => tetrisTableMember
                case (None, Some(key)) => Some(key)
              };
            currentTetrisTable.updated(indexInTable, newTableMember)
          }
        })
      (mergedTetrisTable, indexToInsertBucket)
    } else {
      mergeInner(hashes, tetrisTable, bucket, indexToInsertBucket + 1)
    }
  }

  private def bucketize(hashes: Map[K, Int], directorySize: Int): List[List[K]] = {
    val initialBuckets = (0 until directorySize).map(_ => List[K]()).toVector
    val filledBuckets =
      hashes.foldLeft(initialBuckets)({
        case (buckets, (key, hash)) => {
          val index = hash % directorySize;
          buckets.updated(index, key :: buckets(index))
        }
      })
    // It's *extremely* unlikely, but if any of the buckets are too large (contain more than a
    // fourth of all the values) then we need to pick a different directorySize.
    if (filledBuckets.forall(_.size <= directorySize / 4)) {
      // It's fine, return it.
      filledBuckets.toList
    } else {
      // Try again with a slightly larger top level size.
      bucketize(hashes, directorySize + 1)
    }
  }

  private def hashifyBucket(hashes: Map[K, Int], listBucket: List[K]): Array[Option[K]] = {
    hashifyBucketInner(hashes, listBucket, listBucket.size)
  }

  private def hashifyBucketInner(hashes: Map[K, Int], listBucket: List[K], hashBucketSize: Int): Array[Option[K]] = {
    val initialHashBucket: Array[Option[K]] = (0 until hashBucketSize).map(_ => None).toArray
    val resultMaybeHashBucket =
      listBucket.foldLeft[Option[Array[Option[K]]]](Some(initialHashBucket))((maybeHashBucket, key) => {
        maybeHashBucket match {
          case None => None
          case Some(hashBucket) => {
            val index = hashes(key) % hashBucketSize;
            hashBucket(index) match {
              case Some(_) => None
              case None => Some(hashBucket.updated(index, Some(key)))
            }
          }
        }
      })
    resultMaybeHashBucket match {
      case Some(resultHashBucket) => resultHashBucket
      case None => hashifyBucketInner(hashes, listBucket, hashBucketSize + 1)
    }
  }
}

object GroupedTetrisTableGenerator {
  def generateRandomNums(numRandoms: Int): Set[Int] = {
    var result: Set[Int] = Set();
    while (result.size < numRandoms) {
      result = result + (Math.random() * 2000000000).toInt;
    }
    result
  }

  def main(args: Array[String]): Unit = {
    (1 to 10).map(_ * 1000).foreach(numElements => {
      val startTime = System.currentTimeMillis
      val numTrials = 1000;
      val results =
        (0 until numTrials).map(j => {
          val map = generateRandomNums(numElements).map(i => i -> i).toMap;
          val table = new GroupedTetrisTableGenerator[Int, Int]().generateTetrisTable(map, key => key);
          table.directory.length * 2 + table.combinedBuckets.length
        });
      val endTime = System.currentTimeMillis

      val averageSpace = results.sum * 1.0 / numTrials;
      val roundedAverageSpace = (averageSpace * 1000).round / 1000.0;
      val averageSpaceRatio = averageSpace / numElements;
      val roundedAverageSpaceRatio = (averageSpaceRatio * 1000).round / 1000.0;

      val worstSpace = results.max;
      val worstSpaceRatio = worstSpace * 1.0 / numElements;
      val roundedWorstSpaceRatio = (worstSpaceRatio * 1000).round / 1000.0;

      val averageOverhead = results.sum * 1.0 / numTrials - numElements;
      val roundedAverageOverhead = (averageOverhead * 1000).round / 1000.0;
      val averageOverheadRatio = averageOverhead / numElements;
      val roundedAverageOverheadRatio = (averageOverheadRatio * 1000).round / 1000.0;

      val worstOverhead = results.max - numElements;
      val worstOverheadRatio = worstOverhead * 1.0 / numElements;
      val roundedWorstOverheadRatio = (worstOverheadRatio * 1000).round / 1000.0;

      val averageTime = (endTime - startTime) * 1.0 / numTrials;
      val roundedAverageTime = (averageTime * 1000).round / 1000.0;
      val averageTimeRatio = averageTime / numElements;
      val roundedAverageTimeRatio = (averageTimeRatio * 1000).round / 1000.0;

      println(
        numElements + ":" +
            " AvgSpace " + roundedAverageSpace + " (" + roundedAverageSpaceRatio + "n)" +
            " WorstSpace " + worstSpace + " (" + roundedWorstSpaceRatio + "n)" +
            " AvgOverhead " + roundedAverageOverhead + " (" + roundedAverageOverheadRatio + "n)" +
            " WorstOverhead " + worstOverhead + " (" + roundedWorstOverheadRatio + "n)" +
            " AvgTime " + roundedAverageTime + " (" + roundedAverageTimeRatio + "n)")
    })
  }
}
