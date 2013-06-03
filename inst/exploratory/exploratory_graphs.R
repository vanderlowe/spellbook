require(magic)
require(ggplot2)

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