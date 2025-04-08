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
#' @source Sampling of a sparse matrix for chromosome 8 `spM[8620000:8690000, 1:833]` from scNMT-seq data of `Mus musculus` received from \href{https://doi.org/10.1038/s41586-024-07898-9}{(Kremer, Cerrizuela, et al., 2024)}.
#'
#' @references Kremer, L. P. M., Cerrizuela, S., El-Sammak, H., Al Shukairi, M. E., Ellinger, T., Straub, J., Korkmaz, A., Volk, K., Brunken, J., Kleber, S., Anders, S., & Martin-Villalba, A. (2024). DNA methylation controls stemness of astrocytes in health and ischaemia. Nature, 634(8033), 415-423. https://doi.org/10.1038/s41586-024-07898-9
#'
#' @keywords datasets
"ex_spM"
