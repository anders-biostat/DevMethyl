#' Plot Ensembl regulatory features
#'
#' `plot_regfeat` downloads regulatory features found between `startpos`and `endpos` of the chromosome `chr` from the ensemble data set chosen and plots the data as a segment plot.
#'   For this, copy the link or download the GTF file from the Ensembl FTP site \url{https://ftp.ensembl.org/pub/}.
#'   Regulatory features include promoters, enhancers, open chromatin regions, transcription factor binding and CTF binding sites.
#'   More information here: \url{https://www.ensembl.org/info/genome/funcgen/data/regulatory-features.html}.
#'
#' @inheritParams plot_all
#'
#' @return Segment plot of regulatory features with colored annotation of a variable region if provided.
#' @export
#'
#' @seealso [get_regfeat()] to receive the data frame used for plotting.
#'
#' @examples plot_regulation("https://ftp.ensembl.org/pub/release-110/regulation/mus_musculus/mus_musculus.GRCm39.Regulatory_Build.regulatory_features.20221007.gff.gz", 8, 8628165, 8684055, 8653165, 8659055)
#'
#' reg <- readGFF("https://ftp.ensembl.org/pub/release-110/regulation/mus_musculus/mus_musculus.GRCm39.Regulatory_Build.regulatory_features.20221007.gff.gz")
#' plot_regfeat(reg, 8, 8628165, 8684055)
plot_regfeat <- function(regpath, chr, startpos, endpos, start_VR=0, end_VR=0) {

  get_regfeat(regpath, chr, startpos, endpos) -> feat_reg

  feat_reg$start[feat_reg$start < startpos] <- startpos
  feat_reg$end[feat_reg$end > endpos] <- endpos

  if (nrow(feat_reg) == 0) {
    ggplot() +
      geom_blank() +
      xlim(startpos, endpos) +
      ggtitle("No regulatory features found") +
      theme_minimal() +
      theme(axis.line.x = element_line(linewidth=0.5, colour = "black", linetype=1),
            axis.title.y=element_blank(),
            axis.title.x = element_text(size=10),
            axis.text.x = element_text(size = 10),
            axis.text.y = element_blank(),
            axis.ticks.x = element_line(linewidth = 0.5),
            plot.margin = margin(0,1,0,1, "cm")) +
      theme(panel.grid = element_blank(),
            axis.title.x = element_text(margin = margin(t = 10))) +
      annotate("rect",xmin=start_VR, xmax=end_VR, ymin=0, ymax=0.05, color="transparent", fill="red")

  } else {

  ggplot(feat_reg, aes(y = feature_type, x = start, xend = end, yend = feature_type)) +
    geom_segment(linewidth = 4, color = "black", alpha = 0.7) +
    labs( x= "genomic position") +
    xlim(startpos, endpos) +
    scale_y_discrete(position = "left") +
    theme_minimal() +
    theme(axis.line.x = element_line(linewidth=0.5, colour = "black", linetype=1),
          axis.title.y=element_blank(),
          axis.title.x = element_text(size=10),
          axis.text = element_text(size = 10),
          axis.ticks.x = element_line(linewidth = 0.5),
          plot.margin = margin(0,1,0,1, "cm")) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.title.x = element_text(margin = margin(t = 10))) +
    annotate("rect",xmin=start_VR, xmax=end_VR, ymin=0, ymax=0.05, color="transparent", fill="red")

  }
}
