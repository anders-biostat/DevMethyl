#' Plot regulatory features of genomic region
#'
#' @param gffpath Location of the gff file to be read. Can be a single string of the file path, the URL or can be a connection.
#' @param chr integer number of chromosome
#' @param startpos integer defining the start position of the analysed genomic region
#' @param endpos integer defining the end position of the analysed genomic region
#' @param start_VR integer defining the start position of the variable region
#' @param end_VR integer defining the end position of the variable region
#'
#' @return segment plot
#' @export
#'
#' @examples plot_regulation("https://ftp.ensembl.org/pub/release-110/regulation/mus_musculus/mus_musculus.GRCm39.Regulatory_Build.regulatory_features.20221007.gff.gz", 8, 8628165, 8684055, 8653165, 8659055)
plot_regulation <- function(gffpath, chr, startpos, endpos, start_VR=0, end_VR=0) {

  get_regulation(gffpath, chr, startpos, endpos) -> feat_reg

  feat_reg$start[feat_reg$start < startpos] <- startpos
  feat_reg$end[feat_reg$end > endpos] <- endpos

  ggplot(feat_reg, aes(y = type, x = start, xend = end, yend = type)) +
    geom_segment(linewidth = 4, color = "black", alpha = 0.7) +
    labs( x= "genomic position") +
    xlim(startpos, endpos) +
    scale_y_discrete(position = "right") +
    theme_minimal() +
    theme(axis.line.x = element_line(linewidth=0.5, colour = "black", linetype=1),
          axis.title.y=element_blank(),
          axis.title.x = element_text(size=10),
          axis.text = element_text(size = 10),
          axis.ticks.x = element_line(linewidth = 0.5),
          plot.margin = margin(0,1,0,1, "cm")) +
    theme(panel.grid.major.x = element_blank(),  # Remove horizontal grid lines
          panel.grid.minor.x = element_blank(),
          axis.title.x = element_text(margin = margin(t = 10))) +
    annotate("rect",xmin=start_VR, xmax=end_VR, ymin=0, ymax=0.05, color="transparent", fill="red")

}
