require(magic)

# Get country information
countries <- magicSQL("SELECT ISOalpha2, name FROM countries", "cpw_meta")

# Store country names for quick matching
country.names <- countries$name

# Get donors
donors <- magicSQL("SELECT * FROM donors", "cpw_Crises")

for (i in 1:nrow(donors)) {
  donor <- donors[i, ]
  if (donor$name %in% country.names) {
    iso2 <- countries[countries$name == donor$name, "ISOalpha2"]
    cat(donor$name, iso2, "\n")
    sql <- sprintf("UPDATE donors SET ISOalpha2 = '%s', type = 'Nation' WHERE name = '%s'",
                    iso2, donor$name
                   )
    magicSQL(sql, "cpw_Crises")
  }
}

# Check for missing countries
imported.countries <- magicSQL("SELECT ISOalpha2 FROM donors", "cpw_Crises")[, 1]
missing.countries <- data.frame(ISOalpha2 = setdiff(countries$ISOalpha2, imported.countries), stringsAsFactors=F)
merge(missing.countries, countries, all.x = T, all.y = F)  # Requires manual processing