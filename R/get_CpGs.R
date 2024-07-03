#' Obtain positions of CGs
#'
#' `get_cpgs` downloads the sequence of the region of interest for a chosen species from \url{https://rest.ensembl.org} and extracts the positions of all CG sites found.
#'
#' @inheritParams plot_all
#'
#' @return Integer list containing genomic positions of each CG site found within the given region.
#' @export
#'
#' @seealso [plot_cpgs()] to plot integer list as a bar plot.
#'
#' @examples get_cpgs("mouse", "GRCm38", 8, 8628165, 8684055)
get_cpgs <- function(species, GRCgenome, chr, startpos, endpos) {

  ext_muster <- "/sequence/region/%s/%s:%s..%s:1?coord_system_version=%s"

  ext <- sprintf(ext_muster, species, chr, startpos, endpos, GRCgenome)

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
