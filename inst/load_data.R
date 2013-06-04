require(magic)
require(data.table)
require(testthat)

data.files <- list.files(system.file("data", package="spellbook"))

for (f in data.files) {
  source(f)
}
