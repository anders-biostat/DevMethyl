#' Plot positions of CpG sites
#'
#' `plot_cpgs` downloads the sequence of the region of interest for a chosen species from \url{https://rest.ensembl.org} and extracts the positions of all CpG sites found.
#'   The CpG sites are then visualized as a bar plot to show the distribution across the genomic region. The genomic range is divided into 50 equal parts, and number of CpG sites in each part is calculated and displayed as a bar.
#'
#' @inheritParams plot_all
#'
#' @return Bar plot visualizing the distribution of CpG sites within a genomic range.
#' @export
#'
#' @seealso [get_cpgs()] to receive the integer list used for plotting.
#'
#' @examples plot_cpgs("mouse", "GRCm38", 8, 8628165, 8684055, is_GRC=TRUE)
plot_cpgs <- function(species, genome, chr, startpos, endpos, is_GRC=FALSE) {

  cpg_positions <- get_cpgs(species, genome, chr, startpos, endpos, is_GRC=is_GRC)

  if(is.null(cpg_positions)) {
    ggplot() +
      geom_blank() +
      ggtitle("No CpG sites found") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 10),
            axis.title.x = element_blank(),
            axis.text.x = element_text(),
            axis.ticks.x = element_line(),
            axis.ticks.y.left = element_line(),
            axis.title.y = element_text(size=10),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            plot.margin = margin(0,1,0,1, "cm"),
            axis.line.y = element_line(color = "grey"))

  } else {
  count_bp <- endpos-startpos

  x_axis <- cut(cpg_positions, breaks=50, labels = seq(startpos,floor(endpos-floor(count_bp/50)), by=floor(count_bp/50)) )

  as.data.frame(cpg_positions) -> cpg_df

        ggplot(cpg_df, aes(x=x_axis) )+
                  geom_bar() +
                  labs(y="CpG count") +
                  scale_x_discrete(breaks=c(startpos,max(levels(x_axis)))) +
                  theme_minimal() +
                  theme(axis.text.y = element_text(size = 10),
                        axis.title.x = element_blank(),
                        axis.text.x = element_text(),
                        axis.ticks.x = element_line(),
                        axis.ticks.y.left = element_line(),
                        axis.title.y = element_text(size=10),
                        panel.grid.minor = element_blank(),
                        panel.grid.major = element_blank(),
                        plot.margin = margin(0,1,0,1, "cm"),
                        axis.line.y = element_line(color = "grey"))

 }
}
