#' Download example npz data
#'
#' @return temporary directory
#' @export
#'
#' @examples load_exdata()
load_exdata <- function() {

  url <- "https://heibox.uni-heidelberg.de/f/5f14053387914445b89f/?dl=1"
  tempfile <- file.path(tempdir(), "8_trunc")
  download.file(url, tempfile, mode = "wb")


  url2 <- "https://heibox.uni-heidelberg.de/f/b76f7887a3f746b98e84/?dl=1"
  tempfile2 <- file.path(tempdir(), "8_GpC_trunc")
  download.file(url2, tempfile2, mode = "wb")


  url3 <- "https://heibox.uni-heidelberg.de/f/745dfb5989c2471caa98/?dl=1"
  tempfile3 <- file.path(tempdir(), "cpgIslandExtUnmasked")
  download.file(url3, tempfile3, mode = "wb")

  url4 <- "https://heibox.uni-heidelberg.de/f/fdc8bb53bf784cb092ab/?dl=1"
  tempfile4 <- file.path(tempdir(), "cell-annotation")
  download.file(url4, tempfile4, mode = "wb")

  # Return the loaded data
  return(list(tempfile, tempfile2, tempfile3, tempfile4))
}
