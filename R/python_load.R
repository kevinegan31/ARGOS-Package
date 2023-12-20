#' @importFrom reticulate source_python
.onLoad <- function(libname, pkgname) {
  python_script <- system.file("dynamical_systems.py", package = pkgname, mustWork = TRUE)
  reticulate::source_python(python_script)
}

.onAttach <- function(libname, pkgname) {
  # Inform the user about the 'scipy' dependency
  if (!reticulate::py_module_available("scipy")) {
    packageStartupMessage("The 'scipy' Python module is required but not installed. ",
                          "Please install 'scipy' in your Python environment.")
  }
}
