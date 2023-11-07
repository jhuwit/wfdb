#' The `wfdb` Python Module
#'
#' @return A `python.builtin.module`
#' @export
#'
#' @examplesIf have_wfdb()
#' wfdb()
wfdb = function() {
  mod = reticulate::import("wfdb")
  mod
}
