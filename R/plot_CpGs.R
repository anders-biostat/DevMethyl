#' Get bar plot visualizing amounts of CpGs within a genomic region
#'
#' @param chr integer number of chromosome
#' @param startpos integer defining the start position of the analysed genomic region
#' @param endpos integer defining the end position of the analysed genomic region
#'
#' @return bar plot
#' @export
#'
#' @examples plot_CpGs(8, 8628165, 8684055)
plot_CpGs <- function(chr, startpos, endpos) {

  cpg_positions <- get_CpGs(chr, startpos, endpos)

  count_bp <- endpos-startpos

  x_axis <- cut(cpg_positions, breaks=50, labels = seq(startpos,floor(endpos-floor(count_bp/50)), by=floor(count_bp/50)) )

  as.data.frame(cpg_positions) -> cpg_df

  barplot_cpgs <- ggplot(cpg_df, aes(x=x_axis) )+
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

  return(barplot_cpgs)
}
