#' Plot Ensembl genemodel
#'
#' `plot_genemodel` downloads the gene annotations between `startpos`and `endpos` of the chromosome `chr` from the chosen species and plots the data as a arrow plot.
#'   Gene annotation includes genes, exons, CDS, start codons, stop codons, five prime utrs, three prime utrs and Selenocysteines.
#'   For this, copy the link or download the GTF file from the Ensembl FTP site \url{https://ftp.ensembl.org/pub/}.
#'
#' @inheritParams plot_all
#'
#' @return Arrow plot of genes including colored annotations of the genomic features.
#' @export
#'
#' @seealso [get_genemodel()] to receive the data frame used for plotting.
#'
#' @examples plot_genemodel("https://ftp.ensembl.org/pub/release-110/gtf/mus_musculus/Mus_musculus.GRCm39.110.gtf.gz", 8,  8628165, 8684055)
#'
#' genes <- readGFF("https://ftp.ensembl.org/pub/release-110/gtf/mus_musculus/Mus_musculus.GRCm39.110.gtf.gz")
#' plot_genemodel(genes, 8, 8628165, 8684055)
plot_genemodel <- function(genepath, chr, startpos, endpos) {

  get_genemodel(genepath, chr, startpos, endpos) -> reg

  reg$start[reg$start < startpos] <- startpos
  reg$end[reg$end > endpos] <- endpos

  reg %>% dplyr::filter(type=="gene") -> reg_genes

  reg %>% dplyr::filter(type!="gene") %>%
    dplyr::rename( substart = start, subend = end ) %>%
    left_join( dplyr::select( reg_genes, gene_id, start, end ), by="gene_id") -> reg_subgenes

  reg_subgenes$type <- factor(reg_subgenes$type, levels =c( "exon", "CDS", "start_codon", "stop_codon", "five_prime_utr",  "three_prime_utr", "Selenocysteine"))

  if (nrow(reg_subgenes) == 0) {
      ggplot() +
      geom_blank() +
      xlim(startpos, endpos) +
      ggtitle("No gene annotations found") +
      theme_genes() +
      theme(legend.text = element_text(size = 10),
            legend.key.size = unit(0.4, "cm"),
            legend.title = element_text(size=10),
            axis.text.y = element_text(size = 10)) +
      guides( fill = guide_legend( ncol= 2))

  } else {
  ggplot( NULL, aes( xmin=start, xmax=end, y=gene_name, forward=strand_boolean ) ) +
    geom_gene_arrow( data=reg_genes) +
    geom_subgene_arrow( aes(xsubmin=substart, xsubmax=subend, fill=type ), color=NA, data=reg_subgenes ) +
    xlim(startpos, endpos) +
    theme_genes() +
    theme(legend.text = element_text(size = 10),
          legend.key.size = unit(0.4, "cm"),
          legend.title = element_text(size=10),
          axis.text.y = element_text(size = 10),
          axis.title.y = element_blank())  +
    guides( fill = guide_legend( ncol= 2))

  }
}
