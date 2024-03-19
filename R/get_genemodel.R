#' Get gene model of genomic region
#'
#' @param gtf pathway to gtf file downloaded from...
#' @param chr integer number of chromosome
#' @param startpos integer defining the start position of the analysed genomic region
#' @param endpos integer defining the end position of the analysed genomic region
#'
#' @return data frame
#' @export
#'
#' @examples
get_genemodel <- function(gtf, chr, startpos, endpos) {

  ens<- readGFF(gtf)

  ens %>%
    dplyr::filter(seqid==chr) %>%
    dplyr::filter( start < endpos , end > startpos, type != "transcript") %>%
    mutate(strand_boolean = if_else(strand=="+", TRUE, FALSE) ) -> reg


 return(reg)
}
