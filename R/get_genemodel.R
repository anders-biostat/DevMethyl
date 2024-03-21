#' Get gene model of genomic region
#'
#' @param gtfpath Location of the gtf file to be read. Can be a single string containing the file path or the URL or can be a connection.
#' @param chr integer number of chromosome
#' @param startpos integer defining the start position of the analysed genomic region
#' @param endpos integer defining the end position of the analysed genomic region
#'
#' @return data frame
#' @export
#'
#' @examples get_genemodel("https://ftp.ensembl.org/pub/release-110/gtf/mus_musculus/Mus_musculus.GRCm39.110.gtf.gz", 8,  8628165, 8684055)
get_genemodel <- function(gtfpath, chr, startpos, endpos) {

  ens<- readGFF(gtfpath)

  ens %>%
    dplyr::filter(seqid==chr) %>%
    dplyr::filter( start < endpos , end > startpos, type != "transcript") %>%
    mutate(strand_boolean = if_else(strand=="+", TRUE, FALSE) ) -> reg


 return(reg)
}
