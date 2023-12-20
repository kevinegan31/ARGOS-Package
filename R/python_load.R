# #' @importFrom reticulate source_python
# .onLoad <- function(libname, pkgname) {
#   python_script <- system.file("dynamical_systems.py", package = pkgname, mustWork = TRUE)
#   reticulate::source_python(python_script)
# }

#' @importFrom reticulate source_python
.onLoad <- function(libname, pkgname) {
  # Source the Python script
  python_script <- system.file("dynamical_systems.py", package = pkgname, mustWork = TRUE)
  reticulate::source_python(python_script)

  # Check for and possibly install 'scipy'
  if (!reticulate::py_module_available("scipy")) {
    packageStartupMessage("The 'scipy' Python module is required but not available. Attempting to install 'scipy'.")
    reticulate::py_install("scipy")
  }
}
