#' Obtain Ensembl regulatory features
#'
#' `get_regfeat` downloads regulatory features found between `startpos` and `endpos` of the chromosome `chr` from the chosen Ensembl dataset.
#'   For this, copy the link or download the GTF file from the Ensembl FTP site \url{https://ftp.ensembl.org/pub/}.
#'   Regulatory features include promoters, enhancers, open chromatin regions, transcription factor binding and CTF binding sites.
#'   More information here: \url{https://www.ensembl.org/info/genome/funcgen/data/regulatory-features.html}.
#'
#' @inheritParams plot_all
#'
#' @return A data frame containing the regulatory features and the following 14 columns:
#'  `seqnames`, `start`, `end`, `width`, `strand`, `source`, `type`, `score`, `phase`, `ID`, `bound_end`, `bound_start`, `description`, and `feature_type`.
#'  If the input is a data frame created with `readGFF()`, the `width` column is not included.
#' @export
#'
#' @seealso [plot_regfeat()] to plot data fame as an annotated segment plot.
#'
#' @examples
#' reg_url <- "https://ftp.ensembl.org/pub/release-110/regulation/mus_musculus/mus_musculus.GRCm39.Regulatory_Build.regulatory_features.20221007.gff.gz"
#' get_regfeat(reg_url, 8, 8628165, 8684055)
#'
#'
get_regfeat <- function(regpath, chr, startpos, endpos) {

  if (is.data.frame(regpath)) {

    # Standardize GFF naming to Bioconductor naming
    if ("seqid" %in% names(regpath)) {
      regpath <- dplyr::rename(regpath, seqnames = seqid)
    }

    if (!"seqnames" %in% names(regpath)) {
      stop("The data frame must contain a `seqid` or `seqnames` column.")
    }

    ens_feat <- regpath[
        regpath$seqnames == chr &
        regpath$start <= endpos &
        regpath$end >= startpos,
      ,
      drop = FALSE]

  } else {
    gr <- GenomicRanges::GRanges(paste(chr, paste(startpos, endpos, sep = "-"), sep = ":"))
    ens_feat <- as.data.frame(rtracklayer::import.gff(regpath, which = gr))
  }

 return(ens_feat)

}
