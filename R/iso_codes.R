iso2  <- function(country) {
  if (nchar(country) ==  2) {return(country)}
  if (nchar(country) ==  3) {return(getISO2(country))}
  stop("You must provide a valid 2- or 3-letter ISO country code.")
}

getISO2 <- function(iso3) {
  return(
    magicSQL(sprintf("SELECT ISOalpha2 FROM countries WHERE ISOalpha3 = '%s'", iso3), "cpw_meta")[,1]
  )
}

getISO3 <- function(iso2) {
  return(
    magicSQL(sprintf("SELECT ISOalpha3 FROM countries WHERE ISOalpha2 = '%s'", iso2), "cpw_meta")[,1]
  )
}

getISOs <- function(iso.length) {
  if (iso.length %in% 2:3) {
    return(magicSQL(sprintf("SELECT ISOalpha%i FROM countries", iso.length), "cpw_meta")[,1])
  } else {
    stop("Only 2 and 3 are valid arguments")
  }
}