#' Plot methylation data
#'
#' `plot_methyl` generates a tile plot to visualize the smoothed methylation values from temporal scNMT-seq data, highlighting changes during differentiation.
#'
#' @param m Matrix received from `smooth`. Contains smoothed values and pseudotime of scNMT-seq data of cells at various differentiation stages.
#' @inheritParams smooth
#'
#' @return Tile plot visualizing methylation pattern of a genomic region at various differentiation stages.
#' @export
#'
#' @examples
#' # create example data frame
#' pos = c(5618, 5619, 5620,5621, 5622, 5623, 5624,5625, 5626,5627)
#' pt = c(1,1,2,2,3,3,4,4,5,5)
#' methyl = c(1,1,1,-1,1,-1,-1,1,-1,-1)
#' data.frame( pos, pt, methyl) -> mappedpt
#'
#' # create matrix
#' smooth(mappedpt, 2, 2, 0.1, 0.1,  xrange = range(mappedpt$pos), trange=range(mappedpt$pt)) -> m
#'
#' plot_methyl(m, mappedpt, 8628165, 8684055)
plot_methyl <- function(m, mappedpt, startpos, endpos) {

  as.data.frame(m)  -> df

  colnames(df) <- gsub("^V", "", colnames(df))

  df %>%
    mutate(index = row_number()) %>%
    pivot_longer( cols = -index, names_to = "ptime", values_to = "value") -> df

  df$ptime <- as.integer(df$ptime)

  df%>%
    ggplot(aes(x=index, y=ptime, fill=value)) +
    geom_tile() +
    labs(fill = "methylation status") +
    scale_fill_viridis(option= "D",
                       breaks= c(-0.8, 0.8),
                       labels = c("unmethylated", "methylated"),
                       limits= c(-1, 1)) +
    scale_y_continuous(breaks = seq(0, max(df$ptime), by = floor(max(df$ptime) / floor(max(mappedpt$pt)) ) ),
                       labels = seq(0, max(mappedpt$pt), by = 1),
                       expand = c(0, 0) ) +
    scale_x_continuous(breaks = c(seq(0, max(df$index), length.out=4)),
                       labels = c(round(seq(startpos, endpos, length.out=4)) ),
                       expand = expansion(add = c(1, 1))) +
    theme( panel.background = element_rect(fill = "transparent")) +
    theme(axis.title.x = element_text(size = 10),
          axis.text.x = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 10),
          legend.key.size = unit(0.4, "cm"),
          legend.box.spacing = unit(0.1, "cm")) +
    labs(x = "genomic position", y = "ptime")

}




