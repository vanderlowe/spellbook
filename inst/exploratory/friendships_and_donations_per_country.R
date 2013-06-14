require(magic)
require(spellbook)
Sys.setlocale(locale="C")

# Step 1: Get basic crisis data
loadData("crises")
crisis.ids <- unique(crises$emergency_id)

# Step 2: Get Facebook friendship data for the month of the crisis
incoming.friendships <- lapply(crisis.ids, getGlobalIncomingFacebookRelationsByDisaster)
incoming.friendships <- do.call(rbind, incoming.friendships)
incoming.friendships <- data.table(incoming.friendships)
setkeyv(incoming.friendships, c("emergency_id", "target_country", "source_country", "year", "month"))

test_that("The incoming friendship data is of expected size", {
  emergency.count <- length(crisis.ids)
  country.count <- nrow(allCountries())
  expect_equal(nrow(incoming.friendships), country.count * emergency.count)
})

# Calculate total number of friendships per country per month
incoming.friendships[, total_friendships := sum(friendships), by = list(year, month, target_country)]

# Step 3: Get donations by donor countries
getDonations <- function(id) {
  cat(id, "\n")
  sql <- sprintf("SELECT emergency_id, donor, amount, ISOalpha2 as donorISO FROM donations LEFT JOIN donors ON donations.donor = donors.name WHERE emergency_id = %i AND ISOalpha2 IS NOT NULL", id)
  return(magicSQL(sql, "cpw_Crises"))
}

donations <- lapply(crisis.ids, getDonations)
donations <- do.call(rbind, donations)
donations <- data.table(donations)
donations <- donations[, list(emergency_id, source_country = donorISO, amount)]
setkeyv(donations, c("emergency_id", "source_country"))

# National donor statistics
donations.by.country <- donations[, list(total_amount = sum(amount)), by = source_country]

# Step 4: Merge data
# Merge friendships with donations
friendships.with.donations <- merge(incoming.friendships, donations, by = c("emergency_id", "source_country"), all.x = T, all.y = T)
setkeyv(friendships.with.donations, c("emergency_id", "target_country", "year", "month", "source_country"))
test_that("The data is of expected size", {
  expect_equal(nrow(incoming.friendships), nrow(friendships.with.donations))
})

# Merge crises with friendships and donations data
crises <- merge(crises, friendships.with.donations, by = c("emergency_id", "target_country", "year", "month"), all.x = T, all.y = T)
setkeyv(crises.with.friendships, c("emergency_id", "target_country", "year", "month", "source_country"))

# Step 5: Calculate percentages

# Step 6: National level data
country.data.timeseries <- data.table(magicSQL("SELECT Code AS country, Year, GDP, GDP_PPP, InternetUsers, Population FROM countries_by_year", "cpw_meta"))

require(ggplot2)
ggplot(all, aes(x = log(donor_friend_percent), y = log(funding_percent), group = emergency_id, color = target_country)) + geom_point(alpha = .4)

cor.test(log(all$funding_percent), log(all$donor_friend_percent), use = "complete.obs")

hist(log(all$funding_percent))
hist(all$friend_percent)

require(lme4)
model1 <- lmer(log(funding_percent) ~ 1 + 
                 log(friend_percent) + 
                 log(target_Population) + 
                 log(donor_Population) + 
                 log(donor_GDP) + 
                 log(target_GDP) + 
                 (1|target_country), 
               data = all, na.action = na.omit)
summary(model1)


