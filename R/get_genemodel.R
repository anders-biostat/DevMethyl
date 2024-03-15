#' Title
#'
#' @param gtf
#' @param start_pos
#' @param end_pos
#'
#' @return
#' @export
#'
#' @examples
get_genemodel <- function(gtf, start_pos, end_pos) {

  ens<- readGFF(gtf)

  ens %>%
    dplyr::filter(seqid==chromosome) %>%
    dplyr::filter( start < end_pos , end > start_pos, type != "transcript") %>% # removed transcript?
    mutate(strand_boolean = if_else(strand=="+", TRUE, FALSE) ) -> reg

  #Solving problem of those genes starting outside of our genomic range
  reg_adj = reg
  reg_adj$start[reg_adj$start < start_pos] <- start_pos
  reg_adj$end[reg_adj$end > end_pos] <- end_pos

  reg_adj %>% dplyr::filter(type=="gene") -> reg_genes

  reg_adj %>% dplyr::filter(type!="gene") %>%
    dplyr::rename( substart = start, subend = end ) %>%
    left_join( dplyr::select( reg_genes, gene_id, start, end ), by="gene_id") -> reg_subgenes


  # adjust level for coloring later -> mostly not necessary
  reg_subgenes$type <- factor(reg_subgenes$type, levels =c( "exon", "CDS", "start_codon", "stop_codon", "five_prime_utr",  "three_prime_utr", "Selenocysteine"))

  #plot w/ legend
  ggplot( NULL, aes( xmin=start, xmax=end, y=gene_name, forward=strand_boolean ) ) +
    geom_gene_arrow( data=reg_genes) +
    geom_subgene_arrow( aes(xsubmin=substart, xsubmax=subend, fill=type ), color=NA, data=reg_subgenes ) +
    xlim(start_position, end_position) +
    theme_genes() +
    theme(axis.title.y=element_blank(),
          axis.line.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.text = element_text(size = 10),
          legend.key.size = unit(0.4, "cm"),
          legend.title = element_text(size=10),
          axis.text.y = element_text(size = 10)) +
    guides( fill = guide_legend( ncol= 2))
}
