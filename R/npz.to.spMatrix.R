#' Transform npz file to sparse Matrix
#'
#' @param npzPath pathway of npz file
#'
#' @return
#' @export
#'
#' @examples
npz.to.spMatrix <- function(npzPath) {
# need reticulate and Matrix
npz <- numpy$load(npzPath)
sp <- sparseMatrix( j=npz["indices"], p=npz["indptr"], x=npz["data"], index1=FALSE, dims=npz["shape"], repr="T" )
return(sp)
}
