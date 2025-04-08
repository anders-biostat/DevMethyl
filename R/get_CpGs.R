#' Obtain positions of CpG sites
#'
#' `get_cpgs` downloads the sequence of a region of interest for a chosen species from \url{https://rest.ensembl.org} and extracts the positions of all CpG sites found.
#'
#' @inheritParams plot_all
#'
#' @return Integer list containing genomic positions of each CpG site found within the given region.
#' @export
#'
#' @seealso [plot_cpgs()] to plot integer list as a bar plot.
#'
#' @examples get_cpgs("mouse", "GRCm38", 8, 8628165, 8684055, is_GRC = TRUE)
get_cpgs <- function(species, genome, chr, startpos, endpos, is_GRC = FALSE) {

  if (is_GRC) {
    genomeID <- genome
  }
  else {
    listGenomes <- altGenomenclature(species, genome)
    genomeID <-  listGenomes[[1]]
  }

  ext_muster <- "/sequence/region/%s/%s:%s..%s:1?coord_system_version=%s"

  ext <- sprintf(ext_muster, species, chr, startpos, endpos, genomeID)

  server <- "https://rest.ensembl.org"

  url <- GET(paste(server,  ext , sep = ""), content_type("text/plain"))

  stop_for_status(url)
  sequence <- content(url)

  cpg_positions <- unlist(gregexpr("CG", sequence))

  if (all(cpg_positions == -1)) {
    return(NULL)
  } else {
    return(cpg_positions + (startpos-1))

  }

}
