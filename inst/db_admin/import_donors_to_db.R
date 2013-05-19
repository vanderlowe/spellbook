require(magic)

# Get country information
countries <- magicSQL("SELECT ISOalpha2, name FROM countries", "cpw_meta")

# Store country names for quick matching
country.names <- countries$name

# Get unique donors from donations data
donors <- magicSQL("SELECT DISTINCT(donor) FROM donations", "cpw_Crises")

for (i in 1:nrow(donors)) {
  donor <- donors[i, ]
  print(donor)
  sql <- sprintf("INSERT INTO donors (name) VALUES (\"%s\")", gsub("'","''", donor))
  magicSQL(sql, "cpw_Crises")
}
