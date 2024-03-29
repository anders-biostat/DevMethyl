#' Map methylation data to pseudo time
#'
#' @param spM sparse Matrix received from function `npz.to.spM` or with same format
#' @param meta data frame of meta data containing cell IDs ("cell_id_dna") and pseudo time ("ptime")
#' @param header data frame containing cell IDs for spM, cell IDs have to have same format as in meta
#' @param startpos integer defining the start position of the analysed genomic region
#' @param endpos integer defining the end position of the analysed genomic region
#'
#' @return data frame containing information of spM for the selected genomic region, sorted by ptime. Here i is the row index giving information of the genomic position , ptimes contains the sorted pseudo times of the cells and data contains either value -1 or 1 specifying the methylation status (unmethylated, methylated).
#' @export
#'
#' @examples \dontrun{map_methyl( spM, meta, header, 8628165, 8684055)}
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
