#' Plotting of CpG, GpC and gene data
#'
#' @param meta data frame of meta data containing cell IDs ("cell_id_dna") and pseudo time ("ptime)
#' @param header data frame containing cell IDs for spM
#' @param header_acc data frame containing cell IDs for spM
#' @param hx width of the kernel
#' @param ht height of the kernel
#' @param chr integer number of chromosome
#' @param start_VR integer defining the start position of the variable region
#' @param end_VR integer defining the end position of the variable region
#' @param cpgifile path to txt file containing information about CpG islands retrieved from ...
#' @param npz pathway of npz file
#' @param npz_acc pathway of npz file containing GpC methylation. Used for chromatin accessibility analysis
#' @param featurefile pathway of gtf file downloaded from ensemble containing ...
#' @param genefile pathway to gtf file downloaded from...
#' @param startpos integer defining the start position of the analysed genomic region
#' @param endpos integer defining the end position of the analysed genomic region
#'
#' @return Plot
#' @export
#'
#' @examples
plot_all <- function(cpgifile, npz, meta, header, npz_acc, header_acc, featurefile, genefile, hx, ht, chr, startpos, endpos, start_VR, end_VR) {

  # CpGislands
        read.table(cpgifile) -> cpgi

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
                axis.text = element_blank(),
                plot.margin = margin(0,1,0,1, "cm")) +
          theme(panel.grid = element_blank(),
                plot.margin = margin(0,1,0,1, "cm")) -> cpgi_plot

  # CpGs
          cpg_positions <- get_CpGs(chr, startpos, endpos)

          count_bp <- endpos-startpos

          x_axis <- cut(cpg_positions, breaks=50, labels = seq(startpos,floor(endpos-floor(count_bp/50)), by=floor(count_bp/50)) )

          as.data.frame(cpg_positions) -> cpg_df

          ggplot(cpg_df, aes(x=x_axis) )+
            geom_bar() +
            labs(y="CpG count") +
            theme_minimal() +
            theme(axis.title.y = element_text(size=10),
                  axis.text.y = element_text(size = 10),
                  axis.ticks.y.left = element_line(),
                  axis.title.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.grid.major = element_blank(),
                  plot.margin = margin(0,1,0,1, "cm"),
                  axis.line.y = element_line(color = "grey")) -> cpgs_plot


  # DNA methylation
          npz.to.spM(npz) -> sp
          map_methyl(sp, meta, header, startpos, endpos) -> mappedpt
          gauss_kernel_2d(mappedpt, hx, ht) -> m

          as.data.frame(m)  -> df

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
            scale_x_continuous(expand = c(0, 0)) +
            theme( panel.background = element_rect(fill = "transparent")) +
            theme(axis.title.x=element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.title.y = element_text(size = 10),
                  axis.text.y = element_text(size = 10),
                  legend.title = element_text(size = 10),
                  legend.text = element_text(size = 10),
                  legend.key.size = unit(0.4, "cm"),
                  legend.box.spacing = unit(0.1, "cm")) +
            labs(y = "ptime") -> methyl_plot


  # Chromatin accessibility
          npz.to.spM(npz_acc) -> sp_acc
          map_methyl(sp_acc, meta, header_acc, startpos, endpos) -> mappedpt_acc
          gauss_kernel_2d(mappedpt_acc, hx, ht) -> m_acc

          as.data.frame(m_acc)  -> df_acc

          colnames(df_acc) <- gsub("^V", "", colnames(df_acc))

          df_acc %>%
            mutate(pos = row_number()) %>%
            pivot_longer( cols = -pos, names_to = "ptime", values_to = "Value") -> df_acc

          df_acc$ptime <- as.integer(df_acc$ptime)

          df_acc%>%
            ggplot(aes(x=pos, y=ptime, fill=Value)) +
            geom_tile(show.legend = FALSE) +
            labs(fill = "methylation status") +
            scale_fill_viridis(option= "D",
                               breaks= c(0.8,-0.8),
                               labels = c("methylated", "unmethylated")) +
            scale_y_continuous(breaks = seq(0, max(df_acc$ptime), by = floor(max(df_acc$ptime) / floor(max(mappedpt$pt)))),
                               labels = seq(0, max(mappedpt$pt), by = 1),
                               expand = c(0, 0) )+
            scale_x_continuous(expand = c(0, 0)) +
            theme( panel.background = element_rect(fill = "transparent")) +
            theme(axis.title.x=element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.title.y = element_text(size = 10),
                  axis.text.y = element_text(size = 10)) +
            labs( y = "ptime") -> acc_plot

  # gene model
          get_genemodel(genefile, chr, startpos, endpos) -> reg

          reg$start[reg$start < startpos] <- startpos
          reg$end[reg$end > endpos] <- endpos

          reg %>% dplyr::filter(type=="gene") -> reg_genes

          reg %>% dplyr::filter(type!="gene") %>%
            dplyr::rename( substart = start, subend = end ) %>%
            left_join( dplyr::select( reg_genes, gene_id, start, end ), by="gene_id") -> reg_subgenes

          reg_subgenes$type <- factor(reg_subgenes$type, levels =c( "exon", "CDS", "start_codon", "stop_codon", "five_prime_utr",  "three_prime_utr", "Selenocysteine"))

          ggplot( NULL, aes( xmin=startpos, xmax=endpos, y=gene_name, forward=strand_boolean ) ) +
            geom_gene_arrow( data=reg_genes) +
            geom_subgene_arrow( aes(xsubmin=substart, xsubmax=subend, fill=type ), color=NA, data=reg_subgenes ) +
            xlim(startpos, endpos) +
            theme_genes() +
            theme(axis.line.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  legend.text = element_text(size = 10),
                  legend.key.size = unit(0.4, "cm"),
                  legend.title = element_text(size=10),
                  axis.text.y = element_text(size = 10),
                  axis.title.y = element_blank()) +
            guides( fill = guide_legend( ncol= 2)) -> genemodel_plot


  # genomic features
          plot_regfeatures(featurefile, chr, startpos, endpos, start_VR, end_VR) -> feat_plot

  # combine plots
          design <- "
           1111
           2222
           3333
           3333
           4444
           4444
           5555
           6666
           "
          cpgi_plot + cpgs_plot + methyl_plot  + acc_plot + genemodel_plot + feat_plot + plot_layout(design=design, guide="collect") -> combined_plot

  return(combined_plot)
}
