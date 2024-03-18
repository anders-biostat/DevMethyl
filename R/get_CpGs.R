#' Get Positions of CpGs in a genomic region
#'
#' @param chr integer number of chromosome
#' @param start integer defining the start position of the analysed genomic region
#' @param end integer defining the end position of the analysed genomic region
#'
#' @return integer list
#' @export
#'
#' @examples
get_CpGs <- function(chr, start, end) {

  ext_muster <- "/sequence/region/mouse/%s:%s..%s:1?"

  ext <- sprintf(ext_muster,chr, start, end)

  server <- "https://rest.ensembl.org"

  url <- GET(paste(server,  ext , sep = ""), content_type("text/plain"))

  stop_for_status(url)
  sequence <- content(url)

  cpg_positions<- unlist(gregexpr("CG", sequence))

  return(cpg_positions)
}
