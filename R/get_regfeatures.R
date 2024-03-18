#' Get regulatory features of genomic region
#'
#' @param regfeatures pathway of gtf file downloaded from ensemble containing ...
#'
#' @return data frame ...
#' @export
#'
#' @examples
get_regfeatures <- function(regfeatures, chr, start_pos, end_pos) {

  ens_feat <- readGFF(regfeatures)

  ens_feat %>%
    dplyr::filter(seqid==chr) %>%
    dplyr::filter(!start > end_pos , !end < start_pos) -> feat_reg

 return(feat_reg)

}
