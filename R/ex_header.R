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
#' @source Text file containing headers of the sparse Matrices from \href{https://doi.org/10.1038/s41586-024-07898-9}{(Kremer, Cerrizuela, et al., 2024)} was saved as a data frame with one column called "cell_id" containing the cell IDs.
#' The cell IDs were adjusted to fit the cell IDs of the meta data by removing underscores, that are not followed by an "A", and everything that follows.
#
#' @references Kremer, L. P. M., Cerrizuela, S., El-Sammak, H., Al Shukairi, M. E., Ellinger, T., Straub, J., Korkmaz, A., Volk, K., Brunken, J., Kleber, S., Anders, S., & Martin-Villalba, A. (2024). DNA methylation controls stemness of astrocytes in health and ischaemia. Nature, 634(8033), 415-423. https://doi.org/10.1038/s41586-024-07898-9
#'
#' @keywords datasets
"ex_header"
