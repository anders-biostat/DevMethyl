#' Plot genemodel
#'
#' `plot_genemodel` downloads the gene annotations between `startpos`and `endpos` of the chromosome `chr` from the chosen species and plots the data as a arrow plot.
#' For this you have to copy the link or download the GTF file of the ensemble FTP site https://ftp.ensembl.org/pub/.
#'
#' @param gtfpath Location of the gtf file to be read. Can be a single string of the file directory or of the URL or can be a connection.
#' @param chr Integer number of chromosome.
#' @param startpos,endpos Integer defining the start and end position of the analysed genomic region.
#'
#' @return Arrow plot of genes including colored annotations of the genomic features.
#' @export
#'
#' @examples get_genemodel("https://ftp.ensembl.org/pub/release-110/gtf/mus_musculus/Mus_musculus.GRCm39.110.gtf.gz", 8,  8628165, 8684055)
plot_genemodel <- function(gtfpath, chr, startpos, endpos) {

  get_genemodel(gtfpath, chr, startpos, endpos) -> reg

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
    theme(legend.text = element_text(size = 10),
          legend.key.size = unit(0.4, "cm"),
          legend.title = element_text(size=10),
          axis.text.y = element_text(size = 10)) +
    guides( fill = guide_legend( ncol= 2))

}
