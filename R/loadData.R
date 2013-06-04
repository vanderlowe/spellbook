#' Load data and run tests on it
#' 
#' Runs scripts located in \code{inst/extdata}. These scripts reconstruct key data for easy (re)use.
#' @export
loadData <- function(filename) {
  if (missing(filename)) {
    source(system.file("load_data.R", package = "spellbook"))
  } else {
    source(system.file("extdata", paste(filename, "R", sep = "."), package = "spellbook"))
    message(sprintf("Loaded '%s'", filename))
  }  
}
