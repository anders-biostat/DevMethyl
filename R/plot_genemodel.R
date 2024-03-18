#' Plot gene model
#'
#' @param gtf pathway to gtf file downloaded from...
#' @param start_pos integer defining the start position of the analysed genomic region
#' @param end_pos integer defining the end position of the analysed genomic region
#'
#' @return arrow plot
#' @export
#'
#' @examples
plot_genemodel <- function(gtf, chr, start_pos, end_pos) {

  get_genemodel(gtf, chr, start_pos, end_pos) -> reg

  reg$start[reg$start < start_pos] <- start_pos
  reg$end[reg$end > end_pos] <- end_pos

  reg %>% dplyr::filter(type=="gene") -> reg_genes

  reg %>% dplyr::filter(type!="gene") %>%
    dplyr::rename( substart = start, subend = end ) %>%
    left_join( dplyr::select( reg_genes, gene_id, start, end ), by="gene_id") -> reg_subgenes

  reg_subgenes$type <- factor(reg_subgenes$type, levels =c( "exon", "CDS", "start_codon", "stop_codon", "five_prime_utr",  "three_prime_utr", "Selenocysteine"))

  ggplot( NULL, aes( xmin=start, xmax=end, y=gene_name, forward=strand_boolean ) ) +
    geom_gene_arrow( data=reg_genes) +
    geom_subgene_arrow( aes(xsubmin=substart, xsubmax=subend, fill=type ), color=NA, data=reg_subgenes ) +
    xlim(start_position, end_position) +
    theme_genes() +
    theme(legend.text = element_text(size = 10),
          legend.key.size = unit(0.4, "cm"),
          legend.title = element_text(size=10),
          axis.text.y = element_text(size = 10)) +
    guides( fill = guide_legend( ncol= 2))

}
