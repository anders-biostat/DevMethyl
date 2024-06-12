#' Obtain positions of CpGs
#'
#' `get_cpgs` downloads the sequence of the region of interest for a chosen species from `https://rest.ensembl.org` and extracts the positions of all `CG` sites found.
#'
#' @param species String of species name/alias.
#' @param chr Integer number of chromosome.
#' @param startpos,endpos Integers defining the start and end position of the analysed genomic region.
#'
#' @return Integer list containing genomic positions of each CpG site found within the given region.
#' @export
#'
#' @examples get_cpgs("mouse", 8, 8628165, 8684055)
get_cpgs <- function(species, chr, startpos, endpos) {

  ext_muster <- "/sequence/region/%s/%s:%s..%s:1?"

  ext <- sprintf(ext_muster, species, chr, startpos, endpos)

  server <- "https://rest.ensembl.org"

  url <- GET(paste(server,  ext , sep = ""), content_type("text/plain"))

  stop_for_status(url)
  sequence <- content(url)

  cpg_positions <- unlist(gregexpr("CG", sequence)) + startpos

  return(cpg_positions)
}
