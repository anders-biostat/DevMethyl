#' Gaussian Filter applied to 2 dimensional methylation data
#'
#' @param hx width of the kernel
#' @param ht height of the kernel
#' @param mappedpt data frame received from function *ptime_order* containing relative genomic position (pos), pseudo time of the cells (pt) and methylation status (methyl) -1 or 1 (un-/methylated).
#' @param delx width of the grids
#' @param delt height of the grids
#' @param xrange vector containing minimum and maximum of the genomic region
#' @param trange vector containing minimum and maximum of the pseudo time
#'
#' @return matrix ...
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
