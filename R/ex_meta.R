#' Meta data of example sparse matrix
#'
#'
#' Data frame of meta data for example sparse matrix ex_spM, containing cell ids and pseudotimes.
#'
#' @docType data
#'
#' @usage data(ex_meta)
#'
#' @format A data frame containing 493 rows and 2 columns
#' \describe{
#'   \item{cell_id_dna}{cell id}
#'   \item{ptime}{pseudotime 1}
#'   \item{ptime2}{pseudotime 2}
#'   \item{pt_avg}{average pseudotime}}
#'
#' @source Data frame containing cell annotations from \href{https://doi.org/10.1101/2022.07.13.499860}{(Kremer et al., 2022)}. Additionally, average values of the pseudotimes were calculated.
#'
#' @references Kremer, L. P., Cerrizuela, S., Shukairi, M. E. A., Ellinger, T., Straub, J., Dehler, S., Korkmaz, A., Weichenhan, D., Plass, C., Anders, S., & Martin-Villalba, A. (2022). <br>
#' Single-cell triple-omics uncovers DNA methylation as key feature of stemness in the healthy and ischemic adult brain. <br>
#' bioRxiv, 2022.2007.2013.499860 \url{https://doi.org/10.1101/2022.07.13.499860}.
#'
#' @keywords datasets
"ex_meta"
