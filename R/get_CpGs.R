#' Get Positions of CpGs in a genomic region
#'
#' @param chr integer number of chromosome
#' @param startpos integer defining the start position of the analysed genomic region
#' @param endpos integer defining the end position of the analysed genomic region
#'
#' @return integer list
#' @export
#'
#' @examples get_cpgs(8, 8628165, 8684055)
get_cpgs <- function(chr, startpos, endpos) {

  ext_muster <- "/sequence/region/mouse/%s:%s..%s:1?"

  ext <- sprintf(ext_muster, chr, startpos, endpos)

  server <- "https://rest.ensembl.org"

  url <- GET(paste(server,  ext , sep = ""), content_type("text/plain"))

  stop_for_status(url)
  sequence <- content(url)

  cpg_positions <- unlist(gregexpr("CG", sequence)) + startpos

  return(cpg_positions)
}
