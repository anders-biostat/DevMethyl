#' Get regulatory features of genomic region
#'
#' @param regfeatures pathway of gtf file downloaded from ensemble containing ...
#' @param chr integer number of chromosome
#' @param startpos integer defining the start position of the analysed genomic region
#' @param endpos integer defining the end position of the analysed genomic region
#'
#' @return data frame ...
#' @export
#'
#' @examples
get_regfeatures <- function(regfeatures, chr, startpos, endpos) {

  ens_feat <- readGFF(regfeatures)

  ens_feat %>%
    dplyr::filter(seqid==chr) %>%
    dplyr::filter(!start > endpos , !end < startpos) -> feat_reg

 return(feat_reg)

}
