#' Map sparse matrix to pseudotime
#'
#' `map_methyl` maps the sparse methylation matrix to its meta data. It uses the `header` to assign column names (cell IDs) to the sparse Matrix, then sorts and maps the matrix columns to pseudotimes based on `meta`.
#'
#' @inheritParams plot_all
#' @param spM TsparseMatrix containing sparse methylation data (either CpG or GpC) from scNMT-seq with j = indices, p = indptr and x = data. Can be generated from npz-files using `npz.to.spM`.
#' @param header Data frame with a column `cell_id` mapping the sparse matrix column indices to cell IDs. This must match the format used in `meta$cell_id_dna`.
#'
#' @return Data frame with three columns, containing information of spM for the selected genomic region, sorted by ptime.
#' \describe{
#'   \item{pos}{Genomic position (row index of the matrix).}
#'   \item{pt}{Pseudotime value for the corresponding cell.}
#'   \item{methyl}{Methylation status: -1 (unmethylated), 1 (methylated).}
#' }
#' @export
#'
#' @examples map_methyl(ex_spM, ex_meta, ex_header, 8000, 64000)
map_methyl <- function(spM, meta, header, startpos, endpos) {

    cell_ID <- header$cell_id
    colnames(spM) <- cell_ID

  #prepare meta data
    meta %>%
      as.data.frame() %>%
      dplyr::select(cell_id_dna, pt_avg) %>%
      filter(!is.na(pt_avg)) -> meta2

      sorted_cellID_df <- meta2[order(meta2$pt_avg), ]
      rownames(sorted_cellID_df) <- NULL
      sorted_cellID <- sorted_cellID_df$cell_id_dna

    TspM <- as(spM[startpos:endpos, sorted_cellID], "TsparseMatrix")

  #match sorted ptime with spMatrix
    sorted_cellID_df %>%
      mutate(index = 0:(n() - 1)) %>%
      dplyr::select(index, pt_avg) -> sorted_ptime

    mappedpt <- sorted_ptime$pt_avg[match(TspM@j, sorted_ptime$index)]

    mappedpt_df <- data.frame(pos = TspM@i, pt = mappedpt, methyl = TspM@x)

  return(mappedpt_df)
}
