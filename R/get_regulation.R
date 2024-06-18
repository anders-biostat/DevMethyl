#' Obtain ensemble regulatory features
#'
#' `get_regulation` downloads regulatory features found between `startpos`and `endpos` of the chromosome `chr` from the ensemble dataset chosen.
#'   For this, copy the link or download the GTF file from the ensemble FTP site \url{https://ftp.ensembl.org/pub/}.
#'   Regulatory features include promoters, enhancers, open chromatin regions, transcription factor binding and CTF binding sites.
#'   More information here: \url{https://www.ensembl.org/info/genome/funcgen/data/regulatory-features.html}.
#'
#' @inheritParams plot_all
#'
#' @return Data frame containing 14 columns (seqnames, start, end, width, strand, source, type, score, phase, ID, bound_end, bound_start, description, feature_type).
#' @export
#'
#' @seealso [plot_genemodel()] to plot data fame as an annotated segment plot.
#'
#' @examples get_regulation("https://ftp.ensembl.org/pub/release-110/regulation/mus_musculus/mus_musculus.GRCm39.Regulatory_Build.regulatory_features.20221007.gff.gz", 8, 8628165, 8684055)
get_regulation <- function(featurepath, chr, startpos, endpos) {

  ens_feat<- as.data.frame(import.gff(featurepath))

  ens_feat %>%
    dplyr::filter(seqnames==chr) %>%
    dplyr::filter(!start > endpos , !end < startpos) -> feat_reg

 return(feat_reg)

}
