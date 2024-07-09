#' Example dgTMatrix of sample CpG methylation data
#'
#'
#' Sparse Matrix of format `dgTMatrix` containing sample CpG methylation data of astrocytes and the NSC lineage obtained via scNMT-seq.
#'
#' @docType data
#'
#' @usage data(ex_spM)
#'
#' @format dgTMatrix with dimensions 70001 x 833
#'
#' @source Sampling of a sparse matrix for chromosome 8 `spM[8620000:8690000, 1:833]` from scNMT-seq data of `Mus musculus` received from \href{https://doi.org/10.1101/2022.07.13.499860}{(Kremer et al., 2022)}.
#'
#' @references Kremer, L. P., Cerrizuela, S., Shukairi, M. E. A., Ellinger, T., Straub, J., Dehler, S., Korkmaz, A., Weichenhan, D., Plass, C., Anders, S., & Martin-Villalba, A. (2022). <br>
#' Single-cell triple-omics uncovers DNA methylation as key feature of stemness in the healthy and ischemic adult brain. <br>
#' bioRxiv, 2022.2007.2013.499860 \url{https://doi.org/10.1101/2022.07.13.499860}.
#'
#' @keywords datasets
"ex_spM"
