#' Convert NumPy file to sparse Matrix
#'
#' `npz.to.spM` converts a NumPy file to a Tsparse matrix employable in R.
#'
#' @param npzPath Directory of NumPy file.
#'
#' @return Tsparse matrix with j = indices, p = indptr and x = data.
#' @export
#'
#' @examples
#' file_paths <- load_exdata()
#' methylpath <- file_paths[[1]]
#' npz.to.spM(methylpath)
npz.to.spM <- function(npzPath) {

  numpy <- reticulate::import("numpy")  # only works with this line in the function
  npz <- numpy$load(npzPath)

  sp <- sparseMatrix( j=npz["indices"], p=npz["indptr"], x=npz["data"], index1=FALSE, dims=npz["shape"], repr="T" )
  return(sp)
}
