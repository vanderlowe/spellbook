require(magic)

# Load country-level data
world.data <- data.table(magic::countries())
setkey(world.data, "name")

# Load emergecy-level data
crises <- data.table(magicSQL("SELECT id AS emergency_id, ISOalpha2 AS iso2, ISOalpha3 AS iso3, year, month, title, funding, pledges FROM emergencies WHERE month IS NOT NULL AND type = 'Natural Disaster'", "cpw_Crises"))
setkey(crises, "emergency_id")
donor.count <- data.table(magicSQL("SELECT emergency_id, COUNT(donor) as Donors FROM donations GROUP BY emergency_id", "cpw_Crises"))
setkey(donor.count, "emergency_id")
crises <- merge(crises, donor.count)

# Donor countries
donor.countries <- data.table(magicSQL("SELECT emergency_id, amount, donor, ISOalpha2 as iso2 FROM donations LEFT JOIN donors ON donations.donor = donors.name WHERE ISOalpha2 IS NOT NULL AND amount > 0", "cpw_Crises"))
setkey(donor.countries, "emergency_id")




