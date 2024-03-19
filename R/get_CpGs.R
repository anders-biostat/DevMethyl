#' Get Positions of CpGs in a genomic region
#'
#' @param chr integer number of chromosome
#' @param startpos integer defining the start position of the analysed genomic region
#' @param endpos integer defining the end position of the analysed genomic region
#'
#' @return integer list
#' @export
#'
#' @examples
get_CpGs <- function(chr, startpos, endpos) {

  ext_muster <- "/sequence/region/mouse/%s:%s..%s:1?"

  ext <- sprintf(ext_muster, chr, startpos, endpos)

  server <- "https://rest.ensembl.org"

  url <- GET(paste(server,  ext , sep = ""), content_type("text/plain"))

  stop_for_status(url)
  sequence <- content(url)

  cpg_positions<- unlist(gregexpr("CG", sequence))

  return(cpg_positions)
}
