#' Get regulatory features of genomic region
#'
#' @param regfeatures pathway of gtf file downloaded from ensemble containing ...
#'
#' @return data frame ...
#' @export
#'
#' @examples
get_regfeatures <- function(regfeatures) {

  ens_feat <- readGFF(regfeatures)

  ens_feat %>%
    dplyr::filter(seqid==chromosome) %>%
    dplyr::filter(!start > end_position , !end < start_position) -> feat_reg

 return(feat_reg)

}
