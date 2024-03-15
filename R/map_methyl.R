#' Map methylation data to pseudotime
#'
#' @param spM sparse Matrix received from function *npz.to.spM* or with same format
#' @param meta data frame of meta data containing cell IDs ("cell_id_dna") and pseudotime ("ptime)
#' @param header data frame containing cell IDs for spM
#' @param start integer defining the start position of the analysed genomic region
#' @param end integer defining the end position of the analysed genomic region
#'
#' @return data frame containing information of spM for the selected genomic region, sorted by ptime. Here i is the row index giving information of the genomic position , ptimes contains the sorted pseudo times of the cells and data contains either value -1 or 1 specifying the methylation status (unmethylated, methylated).
#' @export
#'
#' @examples
map_methyl <- function(spM, meta, header, start, end ) {

  #prepare headers and add to spM
    names(header)[names(header) == "."] <- "cell_id" # should this be removed for package?

    header %>%
      mutate(cell_id = sub("_(?!A).*", "",cell_id, perl = TRUE)) -> header_df

    cell_ID <- header_df$cell_id
    colnames(spM) <- cell_ID

  #prepare meta data
    meta %>%
      as.data.frame() %>%
      dplyr::select(cell_id_dna, ptime) %>%
      filter(!is.na(ptime)) -> meta

      sorted_cellID_df <- meta[order(meta$ptime), ]
      rownames(sorted_cellID_df) <- NULL
      sorted_cellID <- sorted_cellID_df$cell_id_dna

  #created TspM for selected region(rows?), containing only columns?/cellIDs with ptime
    TspM <- as( spM[start:end, sorted_cellID], "TsparseMatrix" )

  #match sorted ptime with spMatrix
    sorted_cellID_df %>%
      mutate(index = 0:(n() - 1)) %>%
      dplyr::select(index, ptime) -> sorted_ptime

    mappedpt <- sorted_ptime$ptime[match(TspM@j, sorted_ptime$index)]

    mappedpt_df <- data.frame(pos = TspM@i, pt = mappedpt, methyl = TspM@x)

  return(mappedpt_df)
}
