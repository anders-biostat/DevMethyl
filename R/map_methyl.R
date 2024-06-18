#' Map sparse matrix to pseudotime
#'
#' `map_methyl` maps the sparse Matrix containing the methylation data to its meta data. For this, the cell ids are retrieved from `header` and put as column names of `spM`. `meta` is used to sort the cell ids according to their pseudotimes and map the pseudotimes to the methylation data of the genomic region of interest.
#'
#' @inheritParams plot_all
#' @param spM Tsparse matrix with j = indices, p = indptr and x = data. Can be received using `npz.to.spM`. Contains either CpG or GpC methylation data from scNMT-seq.
#' @param header Data frame containing cell IDs for spM. Cell IDs have to have same format as in meta.
#'
#' @return Data frame containing information of spM for the selected genomic region, sorted by ptime. Here, `pos` is the row index indicating the genomic position, `pt` contains the sorted pseudotimes and `methyl` contains either -1 or 1, specifying the methylation status (un-/methylated).
#' @export
#'
#' @examples map_methyl(ex_spM, ex_meta, ex_header, 8000, 64000)
map_methyl <- function(spM, meta, header, startpos, endpos ) {

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

    TspM <- as( spM[startpos:endpos, sorted_cellID], "TsparseMatrix" )

  #match sorted ptime with spMatrix
    sorted_cellID_df %>%
      mutate(index = 0:(n() - 1)) %>%
      dplyr::select(index, pt_avg) -> sorted_ptime

    mappedpt <- sorted_ptime$pt_avg[match(TspM@j, sorted_ptime$index)]

    mappedpt_df <- data.frame(pos = TspM@i, pt = mappedpt, methyl = TspM@x)

  return(mappedpt_df)
}
