#' Get regulatory features of genomic region
#'
#' @param gffpath Location of the gff file to be read. Can be a single string of the file path or the URL or can be a connection.
#' @param chr integer number of chromosome
#' @param startpos integer defining the start position of the analysed genomic region
#' @param endpos integer defining the end position of the analysed genomic region
#'
#' @return data frame ...
#' @export
#'
#' @examples get_regulation("https://ftp.ensembl.org/pub/release-110/regulation/mus_musculus/mus_musculus.GRCm39.Regulatory_Build.regulatory_features.20221007.gff.gz", 8, 8628165, 8684055)
get_regulation <- function(gffpath, chr, startpos, endpos) {

  ens_feat <- readGFF(gffpath)

  ens_feat %>%
    dplyr::filter(seqid==chr) %>%
    dplyr::filter(!start > endpos , !end < startpos) -> feat_reg

 return(feat_reg)

}
