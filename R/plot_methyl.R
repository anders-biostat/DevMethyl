
#' Plot mapped methylation data
#'
#' @param m matrix received from *gauss_kernel_2d*
#' @param mappedpt data frame containing pseudo times received from *map_methyl*
#'
#' @return tile plot
#' @export
#'
#' @examples
plot_methyl <- function(m, mappedpt) {

  as.data.frame(m)  -> df #or should I insteadt include the function for m and mappedpt?

  colnames(df) <- gsub("^V", "", colnames(df))

  df %>%
    mutate(pos = row_number()) %>%
    pivot_longer( cols = -pos, names_to = "ptime", values_to = "Value") -> df

  df$ptime <- as.integer(df$ptime)

  df%>%
    ggplot(aes(x=pos, y=ptime, fill=Value)) +
    geom_tile() +
    labs(fill = "methylation status") +
    scale_fill_viridis(option= "D",
                       breaks= c(0.8,-0.8),
                       labels = c("methylated", "unmethylated")) +
    scale_y_continuous(breaks = seq(0, max(df$ptime), by = floor(max(df$ptime) / floor(max(mappedpt$pt)))),
                       labels = seq(0, max(mappedpt$pt), by = 1),
                       expand = c(0, 0) )+
   scale_x_continuous(breaks = c(seq(0, max(df$pos), by = floor(max(df$pos)/4)) ),
                       labels = function(x) x + start_position ,
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





