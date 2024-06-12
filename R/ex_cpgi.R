#' Unmasked Cpgislands of mus musculus chromosome 8
#'
#'
#' Data file containing unmasked CpG islands of mus musculus chr8:1-130,127,694.
#'
#' @docType data
#'
#' @usage data(ex_cpgi)
#'
#' @format A data frame containing 1313 rows and 12 columns
#' \describe{
#'   \item{V1}{Indexing field}
#'   \item{V2}{Reference sequence chromosome or scaffold}
#'   \item{V3}{Start position in chromosome}
#'   \item{V4}{End position in chromosome}
#'   \item{V5}{island name part1}
#'   \item{V6}{island name part2}
#'   \item{V7}{Island length}
#'   \item{V8}{Number of CpGs in island}
#'   \item{V9}{Number of C and G in island}
#'   \item{V10}{Percentage of island that is CpG}
#'   \item{V11}{Percentage of island that is C or G}
#'   \item{V12}{Ratio of observed (cpgNum) to expected (numC*numG/length) CpG in island}}
#'
#' @keywords datasets
#'
#' @references UCSC Table Browser: Karolchik D, Hinrichs AS, Furey TS, Roskin KM, Sugnet CW, Haussler D, Kent WJ. The UCSC Table Browser data retrieval tool. Nucleic Acids Res. 2004 Jan 1;32(Database issue):D493-6.
#'
#' @source \href{https://genome.ucsc.edu/cgi-bin/hgTables}{UCSC Table Browser}
#'
#' @examples \dontrun{
#' cpgipath <- "~/ex_cpgi"
#' write.table(ex_cpgi, file=cpgipath, row.names = FALSE, col.names = FALSE)
#' plot_CpGislands(cpgipath, 8, 8628165, 8684055)}
"ex_cpgi"
