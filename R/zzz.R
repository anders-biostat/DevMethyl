.onLoad <- function(libname, pkgname) {

  res <- try( reticulate::import("numba"), silent=TRUE )
  if( inherits( res, "try-error" ) ) {
    if( grep( "ModuleNotFoundError", res ) )
      stop( "Please install Python package 'numba' by typing: reticulate::py_install('numba')" )
    else
      stop( res )
  }

  pyfcts <<- py_run_file( file.path( system.file("extdata",package="DevMethyl"), "smooth_fct_final.py" ) )

}
