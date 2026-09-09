#' Create summary plot of CpG and GpC methylation data with genomic information
#'
#' `plot_all` creates a summary plot combining all plots producible with the `DevMethyl` package.
#'   Using patchwork, the CpG and GpC methylation tile plots created by `plot_methyl`, the CpGi segment plot, the CpGs bar plot, the gene annotation arrow plot
#'   and the segment plot of regulatory features are combined into one plot.
#'   Based on the size of the genomic region analysed, this analysis might take a while.
#'   This functions returns empty plots within the summary plot, if errors occurs or if no data is found. To find the reason of an empty plot try the individual function.
#'
#' @param species String of species name/alias.
#' @param genome String of the genome version used. Nomenclature from UCSC Genome Browser and Genome Reference Consortium are both acceptable inputs.
#' @param meta Data frame of meta data containing cell IDs ("cell_id_dna") and pseudotime ("ptime").
#' @param header,header_acc Data frames containing cell IDs for spM or spMacc. Cell IDs have to have same format as in meta.
#' @param hx,ht Numeric values defining bandwidth of the Gaussian kernel in x and t direction.
#' @param chr Integer number of chromosome.
#' @param start_VMR,end_VMR Integers defining the start and end position of the variable methylated region. Default is 0.
#' @param spM,spMacc dgTMatrices containing CpG or GpC methylation.
#' @param regpath Regulatory features of the chosen species retrieved from the ensemble FTP site \url{https://ftp.ensembl.org/pub/}.
#' Provide either a single character string containing the path or URL of a GFF file, or a GFF-derived data frame created with readGFF() or returned by get_regfeat().
#' @param genepath Genome of the chosen species retrieved from the ensemble FTP site \url{https://ftp.ensembl.org/pub/}.
#' Provide either a single character string containing the path or URL of a GFF file, or a GFF-derived data frame created with readGFF() or returned by get_regfeat().
#' @param startpos,endpos Integers defining the start and end position of the analysed genomic region.
#' @param n_bins Integer defining the number of bins to group the cpg sites.
#' @param is_GRC A boolean argument. Use TRUE, if the `genome` input follows GRC nomenclature. This will allow `plot_all` to bypass an inner function.
#' Use FALSE if the `genome` input does not follow GRC nomenclature or if you are unsure. In this case, the inner function will be executed to retrieve the correct format. Default is FALSE.
#' @param delx,delt Numeric values defining spacings of the grids in x and t directions.
#'
#' @return Patchwork plot combining tile plots of CpG and GpC methylation with plots containing genomic information (CpG islands, CpG site, gene annotations and regulatory features).
#'
#' @importFrom ggplot2 ggplot aes geom_blank geom_bar geom_tile geom_segment geom_bar ggtitle labs xlim ylim scale_x_continuous scale_y_continuous theme_minimal theme guides guide_legend element_blank element_text element_line element_rect margin
#' @importFrom grid unit
#' @export
#'
#' @examples \dontrun{plot_all(species = "mouse",
#'                             genome ="GRCm38",
#'                             spM = spM,
#'                             spMacc = spMacc,
#'                             meta = meta,
#'                             header= header,
#'                             header_acc = header_acc,
#'                             genepath = genepath,
#'                             regpath = regpath,
#'                             chr = 8,
#'                             startpos = 8628165,
#'                             endpos = 8684055,
#'                             hx = 400,
#'                             ht = 0.08,
#'                             start_VMR =8653165,
#'                             end_VMR = 8659055,
#'                             n_bins = 50,
#'                             is_GRC = TRUE)}
plot_all <- function(species, genome, spM, spMacc, meta, header, header_acc, genepath, regpath, chr, startpos, endpos, hx, ht, start_VMR=0, end_VMR=0, delx = (hx/10), delt = (ht/10), n_bins = 50,  is_GRC = FALSE) {

  altGenomenclature(species, genome) -> genomeIDs

  # CpGislands
            session <- rtracklayer::browserSession("UCSC")
            GenomeInfoDb::genome(session) <- genomeIDs[[2]]
            chromosome <- paste("chr", chr, sep = "")
            range <- GenomicRanges::GRanges(seqnames = chromosome, ranges = IRanges::IRanges(start = startpos, end = endpos))
            query <- rtracklayer::ucscTableQuery(session, table = "cpgIslandExtUnmasked", range = range)

            cpgIslands <- rtracklayer::getTable(query)

            if (nrow(cpgIslands) == 0) {
              # Return an empty plot if no CpG islands are found
                ggplot() +
                geom_blank() +
                xlim(startpos, endpos) +
                ylim(0, 1) +
                ggtitle("No CpG islands found") +
                theme_minimal() +
                theme(axis.title.y = element_text(size = 10),
                      axis.title.x = element_blank(),
                      axis.text = element_blank(),
                      plot.margin = margin(0, 1, 0, 1, "cm")) +
                theme(panel.grid = element_blank(),
                      plot.margin = margin(0, 1, 0, 1, "cm")) -> cpgi_plot

            } else {
                  ggplot(cpgIslands, aes(y = 0.5, x = chromStart, xend = chromEnd, yend = 0.5)) +
                    geom_segment(linewidth = 100, color = "darkred", alpha = 0.7) +
                    xlim(startpos, endpos) +
                    ylim(0, 1)+
                    labs(y ="CpG islands") +
                    theme_minimal() +
                    theme(axis.title.y = element_text(size = 10),
                          axis.title.x = element_blank(),
                          axis.text = element_blank(),
                          plot.margin = margin(0, 1, 0, 1, "cm")) +
                    theme(panel.grid = element_blank(),
                          plot.margin = margin(0, 1, 0, 1, "cm")) -> cpgi_plot
            }

  # CpGs
            error_message <- NULL

            cpg_positions <- tryCatch({
              get_cpgs(species, genome, chr, startpos, endpos, is_GRC = is_GRC)
            }, error = function(e) {
              error_message <<- conditionMessage(e)
              return(NULL)
            })

          if(is.null(cpg_positions)) {
            ggplot() +
              geom_blank() +
              ggtitle("No CpG sites found") +
              theme_minimal() +
              theme(axis.title.y = element_text(size = 10),
                    axis.text.y = element_text(size = 10),
                    axis.ticks.y.left = element_line(),
                    axis.title.x = element_blank(),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.grid.major = element_blank(),
                    plot.margin = margin(0,1,0,1, "cm"),
                    axis.line.y = element_line(color = "grey")) -> cpgs_plot

          } else {

            count_bp <- endpos-startpos
            bin_width <- floor(count_bp/n_bins)
            break_points <- seq(startpos, endpos, by = bin_width)

            x_axis <- cut(cpg_positions, breaks = break_points, include.lowest = TRUE, labels = FALSE)

            cpg_df <- data.frame(
              cpg_positions = cpg_positions,
              x_axis = factor(x_axis))

          ggplot(cpg_df, aes(x=x_axis)) +
            geom_bar() +
            labs(y="CpG count") +
            theme_minimal() +
            theme(axis.title.y = element_text(size = 10),
                  axis.text.y = element_text(size = 10),
                  axis.ticks.y.left = element_line(),
                  axis.title.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.grid.major = element_blank(),
                  plot.margin = margin(0,1,0,1, "cm"),
                  axis.line.y = element_line(color = "grey")) -> cpgs_plot
}

  # DNA methylation
          map_methyl(spM, meta, header, startpos, endpos) -> mappedpt

          smooth(mappedpt, hx, ht, delx, delt) -> m

          as.data.frame(m)  -> df

          colnames(df) <- gsub("^V", "", colnames(df))

          df %>%
            dplyr::mutate(pos = dplyr::row_number()) %>%
            tidyr::pivot_longer(cols = -pos, names_to = "ptime", values_to = "Value") -> df

          df$ptime <- as.integer(df$ptime)

          df%>%
            ggplot(aes(x = pos, y = ptime, fill = Value)) +
            geom_tile() +
            labs(fill = "methylation status") +
            viridis::scale_fill_viridis(option = "D",
                               breaks = c(-0.8, 0.8),
                               labels = c("unmethylated", "methylated"),
                               limits = c(-1, 1)) +
            scale_y_continuous(breaks = seq(0, max(df$ptime), by = floor(max(df$ptime) / floor(max(mappedpt$pt)))),
                               labels = seq(0, max(mappedpt$pt), by = 1),
                               expand = c(0, 0)) +
            scale_x_continuous(expand = c(0, 0)) +
            theme(panel.background = element_rect(fill = "transparent")) +
            theme(axis.title.x = element_blank(),
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

          smooth(mappedpt_acc, hx, ht, delx, delt) -> m_acc

          as.data.frame(m_acc)  -> df_acc

          colnames(df_acc) <- gsub("^V", "", colnames(df_acc))

          df_acc %>%
            dplyr::mutate(index = dplyr::row_number()) %>%
            tidyr::pivot_longer(cols = -index, names_to = "ptime", values_to = "value") -> df_acc

          df_acc$ptime <- as.integer(df_acc$ptime)

          df_acc%>%
            ggplot(aes(x = index, y = ptime, fill = value)) +
            geom_tile(show.legend = FALSE) +
            labs(fill = "methylation status") +
            viridis::scale_fill_viridis(option = "D",
                               breaks = c(-0.8, 0.8),
                               labels = c("unmethylated", "methylated"),
                               limits = c(-1, 1)) +
            scale_y_continuous(breaks = seq(0, max(df_acc$ptime), by = floor(max(df_acc$ptime) / floor(max(mappedpt$pt)))),
                               labels = seq(0, max(mappedpt$pt), by = 1),
                               expand = c(0, 0)) +
            scale_x_continuous(expand = c(0, 0)) +
            theme(panel.background = element_rect(fill = "transparent")) +
            theme(axis.title.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.title.y = element_text(size = 10),
                  axis.text.y = element_text(size = 10)) +
            labs(y = "ptime") -> acc_plot

  # gene model

          get_genemodel(genepath, chr, startpos, endpos) -> reg

          reg$start[reg$start < startpos] <- startpos
          reg$end[reg$end > endpos] <- endpos

          reg %>% dplyr::filter(type == "gene") -> reg_genes

          reg %>% dplyr::filter(type != "gene") %>%
            dplyr::rename(substart = start, subend = end) %>%
            dplyr::left_join(dplyr::select(reg_genes, gene_id, start, end), by ="gene_id") -> reg_subgenes

          reg_subgenes$type <- factor(reg_subgenes$type, levels = c("exon", "CDS", "start_codon", "stop_codon", "five_prime_utr",  "three_prime_utr", "Selenocysteine"))


          if (nrow(reg_subgenes) == 0) {
              ggplot() +
              geom_blank() +
              xlim(startpos, endpos) +
              ggtitle("No gene annotations found") +
              gggenes::theme_genes() +
              theme(axis.line.x = element_blank(),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank(),
                    legend.text = element_text(size = 10),
                    legend.key.size = unit(0.4, "cm"),
                    legend.title = element_text(size = 10),
                    axis.text.y = element_text(size = 10),
                    axis.title.y = element_blank()) +
              guides(fill = guide_legend(ncol = 2)) -> genemodel_plot

          } else {
          ggplot(NULL, aes(xmin = start, xmax = end, y = gene_name, forward = strand_boolean)) +
            gggenes::geom_gene_arrow(data = reg_genes) +
            gggenes::geom_subgene_arrow(aes(xsubmin = substart, xsubmax = subend, fill = type), color = NA, data = reg_subgenes) +
            xlim(startpos, endpos) +
            gggenes::theme_genes() +
            theme(axis.line.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  legend.text = element_text(size = 10),
                  legend.key.size = unit(0.4, "cm"),
                  legend.title = element_text(size = 10),
                  axis.text.y = element_text(size = 10),
                  axis.title.y = element_blank()) +
            guides(fill = guide_legend(ncol = 2)) -> genemodel_plot
}

  # genomic features
          plot_regfeat(regpath, chr, startpos, endpos, start_VMR, end_VMR) -> feat_plot

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
          cpgi_plot + cpgs_plot + methyl_plot  + acc_plot + genemodel_plot + feat_plot + patchwork::plot_layout(design = design) -> combined_plot

  return(combined_plot)
}
