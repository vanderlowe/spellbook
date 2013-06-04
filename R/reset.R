#' Reinstall package from GitHub
#' 
#' Download and re-install \code{spellbook}.
#' @export
#' @import devtools
reset <- function() {
  source(system.file("basic_reload.R", package = "spellbook")) 
}
