# Donor countries
donor.countries <- data.table(magicSQL("SELECT emergency_id, amount, donor, ISOalpha2 as iso2 FROM donations LEFT JOIN donors ON donations.donor = donors.name WHERE ISOalpha2 IS NOT NULL AND amount > 0", "cpw_Crises"))
setkey(donor.countries, "emergency_id")
