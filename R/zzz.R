pyfcts <- NULL

.onLoad <- function(libname, pkgname) {

  numpy <- reticulate::import("numpy")

  res <- try( reticulate::import("numba"), silent=TRUE )

  if( inherits( res, "try-error" ) ) {
    if( grepl( "ModuleNotFoundError", as.character(res) ) )
      stop( "Please install Python package 'numba' by typing: reticulate::py_install('numba')" )
    else
      stop( res )
  }

  pyfcts <<- reticulate::py_run_file( file.path( system.file("extdata",package=pkgname,
                                                             "smooth_fct_final.py" ,
                                                              mustWork = TRUE) ))

}
