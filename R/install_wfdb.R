#' Install the `wfdb` Python Module
#'
#' @param packages additional packages to install.  If `wfdb` is missing,
#' it will be added
#' @param ... Additional arguments to pass to [reticulate::py_install()]
#'
#' @return Output of [reticulate::py_install]
#' @export
#' @rdname wfdb_setup
#' @examples
#' if (have_wfdb()) {
#'    wfdb_version()
#' }
#'
install_wfdb = function(..., packages = "wfdb") {
  packages = unique(c("wfdb", packages))
  reticulate::py_install(packages = packages, ...)
}

#' @export
#' @rdname wfdb_setup
have_wfdb = function() {
  reticulate::py_module_available("wfdb")
}


module_version = function(module = "numpy") {
  assertthat::is.scalar(module)
  if (!reticulate::py_module_available(module)) {
    stop(paste0(module, " is not installed!"))
  }
  df = reticulate::py_list_packages()
  df$version[df$package == module]
}

#' @export
#' @rdname wfdb_setup
wfdb_version = function() {
  module_version("wfdb")
}

