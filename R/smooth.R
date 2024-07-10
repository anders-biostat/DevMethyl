#' Apply Gaussian filter to temporal methylation data of scNMT-seq
#'
#' `smooth` smooths scNMT data by calculating weighted averages within a Gaussian window.
#'    For this it iterates over the data points, calculating a Gaussian-weighted sum within a window around each point, and normalizes it by the total weight.
#'
#' @inheritParams plot_all
#' @param mappedpt Data frame of the sparse matrix obtained from `map_methyl`. Contains relative genomic position (pos), pseudotime of the cells (pt) and methylation status (methyl), which is -1 or 1 (un-/methylated) for the selected region of the scNMT-seq data.
#' @param xrange Vector containing minimum and maximum of the genomic region.
#' @param trange Vector containing minimum and maximum of the pseudo time.
#'
#' @return  Matrix with smoothed `methyl` values of scNMT-seq data over a specific grid.
#' @export
#'
#' @examples
#' # create example data frame
#' pos = c(5618, 5619, 5620,5621, 5622, 5623, 5624,5625, 5626,5627)
#' pt = c(1,1,2,2,3,3,4,4,5,5)
#' methyl = c(1,1,1,-1,1,-1,-1,1,-1,-1)
#' data.frame( pos, pt, methyl) -> mappedpt
#'
#' smooth(mappedpt, 2, 2, 0.1, 0.1,  xrange = range(mappedpt$pos), trange=range(mappedpt$pt))
smooth <- function(mappedpt, hx, ht, delx = (hx/10), delt = (ht/10),  xrange = range(mappedpt$pos), trange=range(mappedpt$pt) ) {

  pyfcts$gaussian_smoothing(mappedpt$pos, mappedpt$pt, mappedpt$methyl, xrange[1], xrange[2], delx, trange[1], trange[2], delt, hx, ht)

}

