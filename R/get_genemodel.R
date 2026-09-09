#' Obtain Ensembl genemodel
#'
#' `get_genemodel` downloads the gene annotations between `startpos` and `endpos` of the chromosome `chr` from the chosen species and removes transcript annotations.
#'   Gene annotation includes genes, exons, CDS, start codons, stop codons, five prime utrs, three prime utrs and Selenocysteines.
#'   For this, copy the link or download the GTF file from the Ensembl FTP site \url{https://ftp.ensembl.org/pub/}.
#'
#' @inheritParams plot_all
#'
#' @return Data frame containing various columns, depending on the chosen genome version and release.
#' Possible column names include `seqname`, `start`, `end`, `width`, `strand`, `source`, `type` , `gene_id` and `strand_boolean`.
#'
#' @export
#'
#' @seealso [plot_genemodel()] to plot data frame as an annotated arrow plot.
#'
#' @examples
#' genes_url <- "https://ftp.ensembl.org/pub/release-110/gtf/mus_musculus/Mus_musculus.GRCm39.110.gtf.gz"
#' get_genemodel(genes_url, 8,  8628165, 8684055)
#'
get_genemodel <- function(genepath, chr, startpos, endpos) {

  if (is.data.frame(genepath)) {
    if ("seqid" %in% names(genepath)) {
      genepath <- dplyr::rename(genepath, seqnames = seqid)
    }

    if (!"seqnames" %in% names(genepath)) {
      stop("The data frame must contain a `seqid` or `seqnames` column.")
    }

    ens <- genepath[
        genepath$seqnames == chr &
        genepath$start <= endpos &
        genepath$end >= startpos,
      ,
      drop = FALSE]

    }
  else {
    gr <- GenomicRanges::GRanges(paste(chr, paste(startpos, endpos, sep = "-"), sep = ":"))
    ens <- as.data.frame(rtracklayer::import.gff(genepath, which = gr))
    }

  reg <- ens %>%
        dplyr::filter(type != "transcript") %>%
        dplyr::mutate(strand_boolean = dplyr::if_else(strand == "+", TRUE, FALSE) )


 return(reg)
}


