#' Obtain Ensembl regulatory features
#'
#' `get_regfeat` downloads regulatory features found between `startpos` and `endpos` of the chromosome `chr` from the chosen Ensembl dataset.
#'   For this, copy the link or download the GTF file from the Ensembl FTP site \url{https://ftp.ensembl.org/pub/}.
#'   Regulatory features include promoters, enhancers, open chromatin regions, transcription factor binding and CTF binding sites.
#'   More information here: \url{https://www.ensembl.org/info/genome/funcgen/data/regulatory-features.html}.
#'
#' @inheritParams plot_all
#'
#' @return Data frame containing 14 columns (`seqnames`, `start`, `end`, `width`, `strand`, `source`, `type`, `score`, `phase`, `ID`, `bound_end`, `bound_start`, `description`, `feature_type`).
#' @export
#'
#' @seealso [plot_regfeat()] to plot data fame as an annotated segment plot.
#'
#' @examples get_regfeat("https://ftp.ensembl.org/pub/release-110/regulation/mus_musculus/mus_musculus.GRCm39.Regulatory_Build.regulatory_features.20221007.gff.gz", 8, 8628165, 8684055)
#'
#' # or
#' \dontrun{
#' reg <- readGFF("https://ftp.ensembl.org/pub/release-110/regulation/mus_musculus/mus_musculus.GRCm39.Regulatory_Build.regulatory_features.20221007.gff.gz")
#' get_regfeat(reg, 8, 8628165, 8684055)}
get_regfeat <- function(regpath, chr, startpos, endpos) {

  if (is.data.frame(regpath)) {
    ens_feat <- regpath %>%
                dplyr::filter(seqid == chr) %>%
                dplyr::filter(!start > endpos , !end < startpos)}
  else {
    gr <- GRanges(paste(chr, paste(startpos, endpos, sep = "-"), sep = ":"))
    ens_feat <- as.data.frame(import.gff(regpath, which = gr))
  }

 return(ens_feat)

}
