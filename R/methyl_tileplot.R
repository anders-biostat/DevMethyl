
methyl_tileplot <- function(m) {

  df <- as.data.frame(m)

  df%>%
    ggplot(aes(x=x0, y=t0, fill=smooth_value)) +
    geom_tile() +
    labs(fill = "Methylation Status") +
    scale_fill_viridis(option= "D",
                       breaks= c(0.8,-0.8),
                       labels = c("methylated", "unmethylated")) + #A-H
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme( panel.background = element_rect(fill = "transparent")) +
    theme(axis.title.x=element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          legend.title= element_text(size = 10),
          legend.text = element_text(size = 10),
          legend.key.size = unit(0.4, "cm"),
          legend.box.spacing = unit(0.1, "cm"))  +
    labs( y="ptime")

}

