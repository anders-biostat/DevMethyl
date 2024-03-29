#' Plot cpg islands of specific genomic region
#'
#' @param txtpath Character string containing pathway to text file. Alternatively, can be connection. File contains information about CpG islands retrieved from https://genome.ucsc.edu/cgi-bin/hgTables . Use for "table" either cpgIslandExt or cpgIslandExtUnmasked.
#' @param chr integer number of chromosome
#' @param startpos integer defining the start position of the analysed genomic region
#' @param endpos integer defining the end position of the analysed genomic region
#'
#' @return segment plot
#' @export
#'
#' @examples \dontrun{plot_cpgislands("~/gpgi_file", 8, 8628165, 8684055)}
plot_cpgislands <- function(txtpath, chr, startpos, endpos) {

  read.table(txtpath) -> cpgi

  table_scheme <- c("bin", "chrom", "chrStart", "chrEnd", "name", "length", "cpgNum", "cgNum", "perCpG", "perGC", "obsExp")

  cpgi$V5 <- paste(cpgi$V5, cpgi$V6)

  cpgi$V3 <- cpgi$V3 +1
  cpgi$V4 <- cpgi$V4 +1

  cpgi %>%
    dplyr::select(!V6) -> cpgi

  names(cpgi) <- table_scheme

  cpgi%>%
    dplyr::filter(chrom==paste("chr", chr, sep=""), !chrStart > endpos, !chrEnd < startpos) -> cpg_islands


  ggplot(cpg_islands, aes(y = 0.5, x = chrStart, xend = chrEnd, yend = 0.5)) +
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
