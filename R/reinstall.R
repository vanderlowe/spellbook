#' Reinstall \code{spellbook}
#' 
#' Download and re-install \code{spellbook} from github.
#' @export
#' @import devtools
reinstall <- function(filename) {
  source(system.file("reinstall_spellbook.R", package = "spellbook")) 
}
