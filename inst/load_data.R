require(magic)
require(data.table)
require(testthat)

data.files <- list.files(system.file("extdata", package="spellbook"))

for (f in data.files) {
  source(system.file("extdata", f, package="spellbook"))
}
