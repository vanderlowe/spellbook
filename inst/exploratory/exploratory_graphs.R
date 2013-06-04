require(magic)
require(spellbook)
require(ggplot2)
require(devtools)
require(data.table)
source_url("https://raw.github.com/vanderlowe/spellbook/master/inst/load_data.R")

# Disaster count per country
crisis.count <- crises[, list('Crises' = length(unique(emergency_id))), by = list(iso3)]
# Map
magicMap(crisis.count, "Crises", catMethod = "pretty")
# Bar plot
ggplot(crisis.count, aes(x = reorder(iso3, Crises), y = Crises)) + geom_bar()

# Total funding
funding.amount <- crises[, list(Funding = sum(funding)), by = list(iso3)]
# Map
magicMap(funding.amount, "Funding", catMethod = "pretty")
# Bar plot
ggplot(funding.amount, aes(x = reorder(iso3, Funding), y = Funding)) + geom_bar()

# Total donors
donors.per.country <- crises[, list(Donors = sum(Donors)), by = iso3]
# Map
magicMap(donors.per.country, data.column="Donors")
# Bar plot
ggplot(donors.per.country, aes(x = reorder(iso3, Donors), y = Donors)) + geom_bar()

# Plot top 20 crises
top20 <- funding.amount[order(Funding, decreasing=T),][1:20, iso3]

  for (country in top20) {
    cat(country, "\n")
    print(plot.crises(country))
  }
