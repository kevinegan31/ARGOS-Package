#' @importFrom reticulate source_python
.onLoad <- function(libname, pkgname) {
  python_script <- system.file("dynamical_systems.py", package = pkgname, mustWork = TRUE)
  reticulate::source_python(python_script)
}
