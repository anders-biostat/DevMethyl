#' Title2
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
gauss_kernel_2d <- function(x, t, v, xmin, xmax, delta_x, tmin, tmax, delta_t, hx, ht) {

  pyfcts$gaussian_smoothing(x, t, v, xmin, xmax, delta_x, tmin, tmax, delta_t, hx, ht)

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
