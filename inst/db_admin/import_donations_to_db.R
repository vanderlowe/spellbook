# Get all disasters
disasters <- magicSQL("SELECT id FROM emergencies", "cpw_Crises")[, 1]
length(disasters)

# Get already imported disasters
imported.disasters <- magicSQL("SELECT DISTINCT(emergency_id) FROM donations", "cpw_Crises")[, 1]
length(imported.disasters)

# Remove disaster ids already in the database to avoid duplication
disasters <- setdiff(disasters, imported.disasters)
length(disasters)

save.donation <- function(disaster, donation) {
  sql <- sprintf("INSERT INTO donations (emergency_id, donor, amount) VALUES (%i, \"%s\", \"%s\")",
                 disaster, donation$type, as.character(donation$amount)
                 )
  magicSQL(sql, "cpw_Crises")
}

for (disaster in disasters) {
  cat(disaster, "...")
  
  donations <- getDonors(disaster)
  if (is.null(donations)) {next}  # No donors to given disaster, skip
  
  cat(nrow(donations), "donations\n")
  
  for (i in 1:nrow(donations)) {
    donation <- donations[i, ]
    save.donation(disaster, donation)
  }
}
