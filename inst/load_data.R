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

# Disaster count per country
crisis.count <- crises[, list('Crises' = length(unique(emergency_id))), by = list(iso3)]
magicMap(crisis.count, "Crises", catMethod = "pretty")
ggplot(crisis.count, aes(x = reorder(iso3, Crises), y = Crises)) + geom_bar()

# Total funding
funding.amount <- crises[, list(Funding = sum(funding)), by = list(iso3)]
magicMap(funding.amount, "Funding", catMethod = "pretty")
ggplot(funding.amount, aes(x = reorder(iso3, Funding), y = Funding)) + geom_bar()

# Total donors
donors.per.country <- crises[, list(Donors = sum(Donors)), by = iso3]
ggplot(donors.per.country, aes(x = reorder(iso3, Donors), y = Donors)) + geom_bar()

# Donor countries
donor.countries <- data.table(magicSQL("SELECT emergency_id, amount, donor, ISOalpha2 as iso2 FROM donations LEFT JOIN donors ON donations.donor = donors.name WHERE ISOalpha2 IS NOT NULL AND amount > 0", "cpw_Crises"))
setkey(donor.countries, "emergency_id")

ggplot(donor.count, aes(x = reorder(iso3, Funding), y = Funding)) + geom_bar()

