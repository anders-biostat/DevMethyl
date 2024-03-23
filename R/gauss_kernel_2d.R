#' Gaussian Filter applied to 2 dimensional methylation data
#'
#' @param hx width of kernel
#' @param ht height of kernel
#' @param mappedpt data frame received from function *ptime_order* containing relative genomic position (pos), pseudo time of the cells (pt) and methylation status (methyl) -1 or 1 (un-/methylated).
#' @param delx width of grids
#' @param delt height of grids
#' @param xrange vector containing minimum and maximum of the genomic region
#' @param trange vector containing minimum and maximum of the pseudo time
#'
#' @return matrix ...
#' @export
#'
#' @examples \dontrun{gauss_kernel_2d(mappedpt, 400, 0.8, 40, 0.08,  xrange = range(mappedpt$pos), trange=range(mappedpt$pt)}
gauss_kernel_2d <- function(mappedpt, hx, ht, delx = (hx/10), delt = (ht/10),  xrange = range(mappedpt$pos), trange=range(mappedpt$pt) ) {

  pyfcts$gaussian_smoothing(mappedpt$pos, mappedpt$pt, mappedpt$methyl, xrange[1], xrange[2], delx, trange[1], trange[2], delt, hx, ht)

}

