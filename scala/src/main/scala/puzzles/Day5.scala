package puzzles

object Day5 {

  case class Almanac(seeds: Seq[Long], categoryMaps: Seq[CategoryMap]) {
    def withNewMap(m: CategoryMap): Almanac = this.copy(categoryMaps = this.categoryMaps.appended(m))
    def withNewRange(r: Range): Almanac = {
      val m = this.categoryMaps.last.withNewRange(r)
      this.copy(categoryMaps = this.categoryMaps.dropRight(1).appended(m))
    }
    def applyOnSeeds: Seq[Long] = this.seeds.map(seed => this.categoryMaps.foldLeft(seed) {
      case (source, map) => map.applyOnSource(source)
    })
    def seedRanges: Seq[SourceRange] = seeds
      .grouped(2)
      .map(seedRange => SourceRange(seedRange.head, seedRange.last))
      .toSeq
    def applyOnSeedRanges: Seq[SourceRange] = this.categoryMaps.foldLeft(seedRanges) {
      case (sourceRanges, map) => map.applyOnSourceRange(sourceRanges)
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
    def applyOnSourceRange(seedRanges: Seq[SourceRange]): Seq[SourceRange] = {
      val (mapped, unmapped) = ranges.foldLeft(Seq.empty[SourceRange], seedRanges) { case ((mapped, unmapped), range) =>
        val maybeMap = unmapped.map(um => range.applyOnSourceRange(um))
        val newMapped = maybeMap.map(_._1).filter(_.isDefined).map(_.get)
        val remainsUnmapped = maybeMap.flatMap(_._2)
        (mapped.appendedAll(newMapped), remainsUnmapped)
      }
      // TODO: Maybe optimize here for merging continuous ranges
      mapped.appendedAll(unmapped)
    }
  }

  case class Range(dstStart: Long, srcStart: Long, length: Long) {
    def isForSource(src: Long): Boolean = src >= this.srcStart && src < this.srcStart + length
    def srcEnd: Long = this.srcStart + length - 1
    def applyOnSource(src: Long): Long = dstStart + (src - this.srcStart)
    // Returns (source range mapped, source ranges untouched)
    def applyOnSourceRange(sourceRange: SourceRange): (Option[SourceRange], Seq[SourceRange]) = {
      if ((sourceRange.start > this.srcEnd) || (sourceRange.end < this.srcStart))
        (None, Seq(sourceRange))
      else {
        val firstSourceToMap = Math.max(sourceRange.start, this.srcStart)
        val lastSourceToMap = Math.min(sourceRange.end, this.srcEnd)
        val mapped = SourceRange(applyOnSource(firstSourceToMap), lastSourceToMap - firstSourceToMap + 1)
        val unmapped = Seq(
          SourceRange(sourceRange.start, firstSourceToMap - sourceRange.start), // invalid if no unmapped in front
          SourceRange(lastSourceToMap + 1, sourceRange.end - lastSourceToMap)   // invalid if no unmapped in back
        ).filter(_.isValid)
        (Some(mapped), unmapped)
      }
    }
  }

  case class SourceRange(start: Long, length: Long) {
    def end: Long = start + length - 1
    def isValid: Boolean = length > 0
  }

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

  def task1(dataLines: Seq[String]): Long = parseAlmanac(dataLines).applyOnSeeds.min
  def task2(dataLines: Seq[String]): Long = parseAlmanac(dataLines).applyOnSeedRanges.map(_.start).min
}
