#' Apply Gaussian filter to 2D methylation data
#'
#' `smooth2d` smooths the scNMT data by sliding a window of size ... over the data and applies the gaussian kernel of size (hx,ht)
#'
#' @inheritParams plot_all
#' @param mappedpt Data frame of the sparse matrix received from `map_methyl`. Contains relative genomic position (pos), pseudotime of the cells (pt) and methylation status (methyl) -1 or 1 (un-/methylated) of the selected region, sorted by pseudotime.
#' @param delx,delt Width and height of grids.
#' @param xrange Vector containing minimum and maximum of the genomic region.
#' @param trange Vector containing minimum and maximum of the pseudo time.
#'
#' @return  Matrix with values smoothed and pseudotime of scNMT-seq data of cells at various differentiation stages.
#' @export
#'
#' @examples
#'
#' # create example data frame
#' pos = c(5618, 5619, 5620,5621, 5622, 5623, 5624,5625, 5626,5627)
#' pt = c(1,1,2,2,3,3,4,4,5,5)
#' methyl = c(1,1,1,-1,1,-1,-1,1,-1,-1)
#' data.frame( pos, pt, methyl) -> mappedpt
#'
#' smooth2d(mappedpt, 2, 2, 0.1, 0.1,  xrange = range(mappedpt$pos), trange=range(mappedpt$pt))
smooth2d <- function(mappedpt, hx, ht, delx = (hx/10), delt = (ht/10),  xrange = range(mappedpt$pos), trange=range(mappedpt$pt) ) {

  pyfcts$gaussian_smoothing(mappedpt$pos, mappedpt$pt, mappedpt$methyl, xrange[1], xrange[2], delx, trange[1], trange[2], delt, hx, ht)

}

