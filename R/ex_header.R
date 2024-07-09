#' Headers of example sparse Matrix
#'
#'
#' Data frame with cell IDs for example sparse Matrix ex_spM.
#'
#' @docType data
#' @usage data(ex_header)
#'
#' @format A data frame containing 833 rows and 1 column
#' \describe{
#'   \item{cell_id}{cell identifier} }
#'
#' @source Text file containing headers of the sparse Matrices from \href{https://doi.org/10.1101/2022.07.13.499860}{(Kremer et al., 2022)} was saved as a data frame with one column called "cell_id" containing the cell IDs.
#' The cell IDs were adjusted to fit the cell IDs of the meta data by removing underscores, that are not followed by an "A", and everything that follows.
#
#' @references Kremer, L. P., Cerrizuela, S., Shukairi, M. E. A., Ellinger, T., Straub, J., Dehler, S., Korkmaz, A., Weichenhan, D., Plass, C., Anders, S., & Martin-Villalba, A. (2022). <br>
#' Single-cell triple-omics uncovers DNA methylation as key feature of stemness in the healthy and ischemic adult brain. <br>
#' bioRxiv, 2022.2007.2013.499860 \url{https://doi.org/10.1101/2022.07.13.499860}.
#'
#' @keywords datasets
"ex_header"
