#' Plot positions of CpG sites
#'
#' `plot_cpgs` downloads the sequence of a region of interest for a chosen species from \url{https://rest.ensembl.org} and extracts the positions of all CpG sites found.
#'   The CpG sites are visualized as a bar plot to show the distribution across the genomic region. The genomic range is divided into equal bins, and the number of CpG sites are calculated and displayed as a bar for each bin.
#'
#' @inheritParams plot_all
#'
#' @return Bar plot visualizing the distribution of CpG sites within a genomic range.
#' @export
#'
#' @seealso [get_cpgs()] to receive the integer list used for plotting.
#'
#' @examples plot_cpgs("mouse", "GRCm38", 8, 8628165, 8684055, n_bins = 50, is_GRC = TRUE)
plot_cpgs <- function(species, genome, chr, startpos, endpos, n_bins = 50, is_GRC = FALSE) {

  cpg_positions <- get_cpgs(species, genome, chr, startpos, endpos, is_GRC = is_GRC)

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
            axis.title.y = element_text(size = 10),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            plot.margin = margin(0, 1, 0, 1, "cm"),
            axis.line.y = element_line(color = "grey"))

  } else {

  count_bp <- endpos-startpos
  bin_width <- floor(count_bp/n_bins)
  break_points <- seq(startpos, endpos, by = bin_width)

  x_axis <- cut(cpg_positions, breaks = break_points, include.lowest = TRUE, labels = FALSE)


  bin_labels <- break_points[-length(break_points)]
  x_axis_factor <- factor(bin_labels[x_axis])


  cpg_df <- data.frame(
    cpg_positions = cpg_positions,
    x_axis = x_axis_factor)


  tick_labels <- round(seq(startpos, endpos, length.out = 6))
  tick_breaks <- as.character(sapply(tick_labels, function(x) bin_labels[which.min(abs(bin_labels - x))]))

        ggplot(cpg_df, aes(x = x_axis) )+
                  geom_bar() +
                  labs(y = "CpG count") +
                  scale_x_discrete(breaks = tick_breaks, labels = tick_labels) +
                  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
                  theme_minimal() +
                  theme(axis.text.y = element_text(size = 10),
                        axis.text.x = element_text(),
                        axis.ticks.x = element_line(),
                        axis.ticks.y.left = element_line(),
                        axis.title.y = element_text(size = 10),
                        axis.title.x = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.grid.major = element_blank(),
                        plot.margin = margin(0, 1, 0, 1, "cm"),
                        axis.line.y = element_line(color = "grey"),
                        axis.line.x = element_line(linewidth = 0.5, colour = "black", linetype = 1))

 }
}
