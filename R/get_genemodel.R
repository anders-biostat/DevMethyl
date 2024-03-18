#' Get gene model of genomic region
#'
#' @param gtf pathway to gtf file downloaded from...
#' @param start_pos integer defining the start position of the analysed genomic region
#' @param end_pos integer defining the end position of the analysed genomic region
#'
#' @return data frame
#' @export
#'
#' @examples
get_genemodel <- function(gtf, chr, start_pos, end_pos) {

  ens<- readGFF(gtf)

  ens %>%
    dplyr::filter(seqid==chr) %>%
    dplyr::filter( start < end_pos , end > start_pos, type != "transcript") %>%
    mutate(strand_boolean = if_else(strand=="+", TRUE, FALSE) ) -> reg


 return(reg)
}
