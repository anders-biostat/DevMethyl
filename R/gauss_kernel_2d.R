#' Gaussian Filter applied to 2 dimensinal methylation data
#'
#' @param hx width of the kernel  ?
#' @param ht hight of the kernel ?
#' @param mappedpt dataframe received from function ptime_order containing relative genomic position (pos), pseudotime of the cells (pt) and methylation status (methyl) -1 or 1 (un-/methylated).
#' @param delx width of the window?
#' @param delt height of the window ?
#' @param xrange vector containing minimum and maximum of the genomic region
#' @param trange vector containing minimum and maximum of the pseudo time
#'
#' @return matrix
#' @export
#'
#' @examples
gauss_kernel_2d <- function(mappedpt, hx, ht, delx = (hx/10), delt = (ht/10),  xrange = range(mappedpt$pos), trange=range(mappedpt$pt) ) {

  pyfcts$gaussian_smoothing(mappedpt$pos, mappedpt$pt, mappedpt$methyl, xrange[1], xrange[2], delx, trange[1], trange[2], delt, hx, ht)

}

#' test
#'
#' @return double
#' @export
#'
#' @examples
test_numba <- function() {
  pyfcts$norm_pdf(1)
}
