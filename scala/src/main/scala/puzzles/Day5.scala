package puzzles

case class Almanac(seeds: Seq[Long], categoryMaps: Seq[CategoryMap]) {
  def withNewMap(m: CategoryMap): Almanac = this.copy(categoryMaps = this.categoryMaps.appended(m))
  def withNewRange(r: Range): Almanac = {
    val m = this.categoryMaps.last.withNewRange(r)
    this.copy(categoryMaps = this.categoryMaps.dropRight(1).appended(m))
  }
  def applyOnSeeds: Seq[Long] = this.seeds.map(seed => this.categoryMaps.foldLeft(seed) {
    case (source, map) => map.applyOnSource(source)
  })
  def seedRanges: Seq[SeedRange] = seeds.grouped(2).map(seedRange => SeedRange(seedRange.head, seedRange.last)).toSeq
  def applyOnSeedRanges: Seq[SeedRange] = this.categoryMaps.foldLeft(seedRanges) {
    case (sourceRanges, map) => map.applyOnSeedRanges(sourceRanges)
  }
}
case class CategoryMap(from: String, to: String, ranges: Seq[Range]) {
  def withNewRange(r: Range): CategoryMap = this.copy(ranges = this.ranges.appended(r))
  def applyOnSource(source: Long): Long = {
    this.ranges
      .find(_.isForSource(source))
      .map(_.applyOnSource(source))
      .getOrElse(defaultMapping(source))
  }
  private def defaultMapping(source: Long): Long = source
  // Return (mapped, unmapped)
  def applyOnSeedRanges(seedRanges: Seq[SeedRange]): Seq[SeedRange] = {
    val (mapped, unmapped) = ranges.foldLeft(Seq.empty[SeedRange], seedRanges) { case ((mapped, unmapped), r ) =>
      val maybeMap = unmapped.map(um => r.applyOnRange(um))
      val newMapped = maybeMap.map(_._1).filter(_.isDefined).map(_.get)
      val remainsUnmapped = maybeMap.flatMap(_._2)
      (mapped.appendedAll(newMapped), remainsUnmapped)
    }
    // Optimize here for merging continuous ranges
    mapped.appendedAll(unmapped)
  }
}
case class Range(dStart: Long, sStart: Long, length: Long) {
  def isForSource(source: Long): Boolean = source >= sStart && source < sStart + length
  def largestMappedSource: Long = this.sStart + length - 1
  def applyOnSource(source: Long): Long = dStart + (source - sStart)
  // Return (seedRange mapped, seedranges untouched)
  def applyOnRange(seedRange: SeedRange): (Option[SeedRange], Seq[SeedRange]) = {
    if ((seedRange.start > this.largestMappedSource) || (seedRange.largestSeed < this.sStart))
      (None, Seq(seedRange))
    else {
      val firstSeedToMap = Math.max(seedRange.start, this.sStart)
      val lastSeedToMap = Math.min(seedRange.largestSeed, this.largestMappedSource)
      val firstDest = applyOnSource(firstSeedToMap)
      val lastDest = applyOnSource(lastSeedToMap)
      val mappedRange = SeedRange(firstDest, lastDest - firstDest + 1)
      val unmappedInFront = if (firstSeedToMap > seedRange.start) {
        Some(SeedRange(seedRange.start, firstSeedToMap - seedRange.start))
      } else {
        None
      }
      val unmappedInBack = if (lastSeedToMap < seedRange.largestSeed) {
        Some(SeedRange(lastSeedToMap + 1, seedRange.largestSeed - lastSeedToMap))
      } else {
        None
      }
      (Some(mappedRange), Seq(unmappedInFront, unmappedInBack).filter(_.isDefined).map(_.get))
    }
  }
}
case class SeedRange(start: Long, length: Long) {
  def largestSeed: Long = start + length - 1
}

object Day5 {
  private def parseAlmanac(dataLines: Seq[String]): Almanac = {
    val seeds = dataLines.head.drop(7).split(" ").map(_.toLong)
    dataLines.drop(2).foldLeft(Almanac(seeds, Seq.empty)) { case (almanac, line) =>
      line match {
        case s"$from-to-$to map:" => almanac.withNewMap(CategoryMap(from, to, Seq.empty))
        case s"$dStart $sStart $length" => almanac.withNewRange(Range(dStart.toLong, sStart.toLong, length.toLong))
        case _ => almanac
      }
    }
  }

  def task1(dataLines: Seq[String]): Long =
    parseAlmanac(dataLines).applyOnSeeds.min

  def task2(dataLines: Seq[String]): Long =
    parseAlmanac(dataLines).applyOnSeedRanges.map(_.start).min
}
