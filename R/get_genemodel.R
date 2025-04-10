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
#' @examples get_genemodel("https://ftp.ensembl.org/pub/release-110/gtf/mus_musculus/Mus_musculus.GRCm39.110.gtf.gz", 8,  8628165, 8684055)
#'
#' # or
#' \dontrun{
#' genes <- readGFF("https://ftp.ensembl.org/pub/release-110/gtf/mus_musculus/Mus_musculus.GRCm39.110.gtf.gz")
#' get_genemodel(genes, 8, 8628165, 8684055)}
get_genemodel <- function(genepath, chr, startpos, endpos) {

  gr <- GRanges(paste(chr, paste(startpos, endpos, sep = "-"), sep = ":"))

  if (is.data.frame(genepath)) {
    ens <- genepath %>%
            dplyr::filter(seqid == chr) %>%
            dplyr::filter(start < endpos, end > startpos)  }
  else {
    ens <- as.data.frame(import.gff(genepath, which = gr)) }

  ens %>%
    dplyr::filter(type != "transcript") %>%
    mutate(strand_boolean = if_else(strand == "+", TRUE, FALSE) ) -> reg


 return(reg)
}
