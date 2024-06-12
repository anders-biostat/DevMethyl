#' Obtain ensemble regulatory features
#'
#' `get_regulation` downloads regulatory features found between `startpos`and `endpos` of the chromosome `chr` from the ensemble data set chosen.
#' Regulatory features include promoters, enhancers, open chromatin regions, transcription factor binding and CTF binding sites.
#' More information here: https://www.ensembl.org/info/genome/funcgen/data/regulatory-features.html.
#'
#' @param gffpath Location of the gff file to be read. Can be a single string of the file directory, the URL or can be a connection.
#' @param chr Integer number of chromosome.
#' @param startpos,endpos Integers defining the start and end position of the analysed genomic region.
#'
#' @return Data frame containing 14 columns (seqnames, start, end, width, strand, source, type, score, phase, ID, bound_end, bound_start, description, feature_type).
#' @export
#'
#' @examples get_regulation("https://ftp.ensembl.org/pub/release-110/regulation/mus_musculus/mus_musculus.GRCm39.Regulatory_Build.regulatory_features.20221007.gff.gz", 8, 8628165, 8684055)
get_regulation <- function(gffpath, chr, startpos, endpos) {

  ens_feat<- as.data.frame(import.gff(gffpath))

  ens_feat %>%
    dplyr::filter(seqnames==chr) %>%
    dplyr::filter(!start > endpos , !end < startpos) -> feat_reg

 return(feat_reg)

}
