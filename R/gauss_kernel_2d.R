gauss_kernel_2d <- function(x, t, v, xmin, xmax, delta_x, tmin, tmax, delta_t, hx, ht) {

  #has to be right wd

  # package_file does not give detailed path
  py_run_file(paste(package_file("inst"), "/extdata/smooth_fct_final.py", sep=""))

  py_install("numba")
  num <- import(numba)
  py_gaussian_smoothing(x, t, v, xmin, xmax, delta_x, tmin, tmax, delta_t, hx, ht)

}
