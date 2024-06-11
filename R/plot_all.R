#' Plotting of CpG, GpC and gene data
#'
#' @param meta data frame of meta data containing cell IDs ("cell_id_dna") and pseudo time ("ptime")
#' @param header data frame containing cell IDs for spM
#' @param header_acc data frame containing cell IDs for spM of accessibility data?
#' @param hx width of kernel
#' @param ht height of kernel
#' @param chr integer number of chromosome
#' @param start_VR integer defining the start position of the variable region.
#' @param end_VR integer defining the end position of the variable region.
#' @param cpgipath Character string containing pathway to text file. Alternatively, can be connection. File contains information about CpG islands retrieved from https://genome.ucsc.edu/cgi-bin/hgTables . Use for "table" wether cpgIslandExt or cpgIslandExtUnmasked.
#' @param spM dgTMatrix containing CpG methylation.
#' @param spMacc dgTMatrix containing GpC methylation. Used for chromatin accessibility analysis
#' @param featurepath Location of the gff file to be read. Can be a single string of the file path or the URL or can be a connection.
#' @param genepath Location of the gtf file to be read. Can be a single string containing the file path or the URL or can be a connection.
#' @param startpos integer defining the start position of the analysed genomic region.
#' @param endpos integer defining the end position of the analysed genomic region.
#'
#' @return Plot
#' @export
#'
#' @examples \dontrun{plot_all(cpgipath, spM, meta, header, spMacc, header_acc, featurepath, genepath, 400, 0.08, 8, 8628165, 8684055, 8653165, 8659055)}
plot_all <- function(cpgipath, spM, meta, header, spMacc, header_acc, featurepath, genepath, hx, ht, chr, startpos, endpos, start_VR, end_VR) {

  # CpGislands
        read.table(cpgipath) -> cpgi

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
          cpg_positions <- get_cpgs(chr, startpos, endpos)

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
          map_methyl(spM, meta, header, startpos, endpos) -> mappedpt
          smooth2d(mappedpt, hx, ht) -> m

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
                               breaks= c(-0.8, 0.8),
                               labels = c("unmethylated", "methylated"),
                               limits= c(-1, 1)) +
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
          map_methyl(spMacc, meta, header_acc, startpos, endpos) -> mappedpt_acc
          smooth2d(mappedpt_acc, hx, ht) -> m_acc

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
                               breaks= c(-0.8, 0.8),
                               labels = c("unmethylated", "methylated"),
                               limits= c(-1, 1)) +
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
          get_genemodel(genepath, chr, startpos, endpos) -> reg

          reg$start[reg$start < startpos] <- startpos
          reg$end[reg$end > endpos] <- endpos

          reg %>% dplyr::filter(type=="gene") -> reg_genes

          reg %>% dplyr::filter(type!="gene") %>%
            dplyr::rename( substart = start, subend = end ) %>%
            left_join( dplyr::select( reg_genes, gene_id, start, end ), by="gene_id") -> reg_subgenes

          reg_subgenes$type <- factor(reg_subgenes$type, levels =c( "exon", "CDS", "start_codon", "stop_codon", "five_prime_utr",  "three_prime_utr", "Selenocysteine"))

          ggplot( NULL, aes( xmin=start, xmax=end, y=gene_name, forward=strand_boolean ) ) +
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
          plot_regulation(featurepath, chr, startpos, endpos, start_VR, end_VR) -> feat_plot

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
          cpgi_plot + cpgs_plot + methyl_plot  + acc_plot + genemodel_plot + feat_plot + plot_layout(design=design) -> combined_plot

  return(combined_plot)
}
