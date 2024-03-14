#' Gaussian Filter applied to 2 dimensinal methylation data
#'
#' @param x
#' @param t
#' @param v
#' @param xmin
#' @param xmax
#' @param delta_x
#' @param tmin
#' @param tmax
#' @param delta_t
#' @param hx
#' @param ht
#'
#' @return
#' @export
#'
#' @examples
gauss_kernel_2d <- function(mappedpt, hx, ht, delx = (hx/10), delt = (ht/10),  xrange = range(mappedpt$pos), trange=range(mappedpt$pt) ) {

  pyfcts$gaussian_smoothing(mappedpt$pos, mappedpt$pt, mappedpt$methyl, xrange[1], xrange[2], delx, trange[1], trange[2], delt, hx, ht)

}

#' Title
#'
#' @return
#' @export
#'
#' @examples
test_numba <- function() {
  pyfcts$norm_pdf(1)
}
