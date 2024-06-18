#' Obtain ensemble genemodel
#'
#' `get_genemodel` downloads the gene annotations between `startpos` and `endpos` of the chromosome `chr` from the chosen species.
#'   For this, copy the link or download the GTF file from the ensemble FTP site \url{https://ftp.ensembl.org/pub/}.
#'
#' @inheritParams plot_all
#'
#' @return Data frame containing 28 columns (seqname, start, end, width, strand, source, type, score, phase, gene_id, gene_version, gene_name, gene_source, gene_biotype, transcript_id, transcript_version, transcript_name, transcript_source, transcript_biotype, tag, transcript_support_level, exon_number, exon_id, exon_version, ccds_id, protein_id, protein_version, strand_boolean).
#' @export
#'
#' @seealso [plot_genemodel()] to plot data frame as an annotated arrow plot.
#'
#' @examples get_genemodel("https://ftp.ensembl.org/pub/release-110/gtf/mus_musculus/Mus_musculus.GRCm39.110.gtf.gz", 8,  8628165, 8684055)
get_genemodel <- function(genepath, chr, startpos, endpos) {

  gr <- GRanges(paste(chr, paste(startpos, endpos, sep = "-"), sep = ":"))

  ens<- as.data.frame(import.gff(genepath, which=gr))

  ens %>%
    dplyr::filter(type != "transcript") %>%
    mutate(strand_boolean = if_else(strand=="+", TRUE, FALSE) ) -> reg


 return(reg)
}
