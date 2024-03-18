#' Transform npz file to sparse Matrix
#'
#' @param npzPath pathway of npz file
#'
#' @return Tsparse Matrix with j = indices, p = indptr and x = data
#' @export
#'
#' @examples
npz.to.spM <- function(npzPath) {

  numpy <- reticulate::import("numpy")  # somehow only works with this line in the function
  npz <- numpy$load(npzPath)

  sp <- sparseMatrix( j=npz["indices"], p=npz["indptr"], x=npz["data"], index1=FALSE, dims=npz["shape"], repr="T" )
  return(sp)
}
