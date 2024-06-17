#' Plot CpG islands
#'
#' `plot_cpgislands` downloads and plots CpG islands of the chosen genomic region as a segment plot visualizing the position of islands.
#'   Information of the CpG islands are obtained from the UCSC Genome Browser https://genome.ucsc.edu/cgi-bin/hgTables.
#'
#' @inheritParams plot_all
#'
#' @return Segment plot indicating the position of CpG islands within the genomic region.
#' @export
#'
#' @examples
#' plot_cpgislands("mm39", 8, 8628165, 8684055)
plot_cpgislands <- function(genome, chr, startpos, endpos) {

  session <- browserSession("UCSC")

  genome(session) <- genome

  chromosome <- paste("chr", chr, sep="")

  range <- GRanges(seqnames = chromosome, ranges = IRanges(start = startpos, end = endpos))

  track <- "cpgIslandExtUnmasked"

  # Create a UCSC table query for the specified region
  query <- ucscTableQuery(session, table=track, range=range)

  cpgIslands <- getTable(query)

  if (nrow(cpgIslands) == 0) {
    # Return an empty plot if no CpG islands are found
      ggplot() +
      geom_blank() +
      xlim(startpos, endpos) +
      ylim(0, 1) +
      ggtitle("No CpG islands found") +
      theme_minimal() +
      theme(axis.title.y = element_text(size=10),
            axis.title.x = element_blank(),
            axis.line.x = element_line(linewidth=0.5, colour = "black", linetype=1),
            axis.text.x = element_text(size = 8),
            axis.text = element_blank(),
            axis.ticks.x = element_line(linewidth = 0.5),
            plot.margin = margin(0,1,0,1, "cm")) +
      theme(panel.grid = element_blank(),
            plot.margin = margin(0,1,0,1, "cm"))

  } else {

  ggplot(cpgIslands, aes(y = 0.5, x = chromStart, xend = chromEnd, yend = 0.5)) +
    geom_segment(linewidth = 100, color = "darkred", alpha = 0.7) +
    xlim(startpos, endpos) +
    ylim(0,1)+
    labs( y="CpG islands") +
    theme_minimal() +
    theme(axis.title.y = element_text(size=10),
          axis.title.x = element_blank(),
          axis.line.x = element_line(linewidth=0.5, colour = "black", linetype=1),
          axis.text.x = element_text(size = 8),
          axis.text = element_blank(),
          axis.ticks.x = element_line(linewidth = 0.5),
          plot.margin = margin(0,1,0,1, "cm")) +
    theme(panel.grid = element_blank(),
          plot.margin = margin(0,1,0,1, "cm"))
  }
}
