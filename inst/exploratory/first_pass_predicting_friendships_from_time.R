require(magic)
require(spellbook)
require(testthat)
require(data.table)
require(ggplot2)
require(lme4)
require(pbkrtest)

options(scipen = 999)

# To test whether natural disasters have an impact on the formation of international friendships,
# we used data spanning six months before a natural disaster, the month during which the disaster occurred,
# and the following six months after the disaster. For each month, we had data for how many new friendships
# were formed between any two countries.
fb <- data.table(magicSQL("SELECT * FROM fb_disasters", "cpw_iv244"))

test_that("Data passes basic assumptions", {
  expect_true(any(is.na(fb)) == F)  # No missing data
  expect_true(max(fb$YearMonth) <= "201211")  # No data beyond Nov 2012
  expect_true(min(fb$YearMonth) >= "200605")  # No data before May 2012
  
  iso.codes <- magicSQL("SELECT ISOalpha2 FROM countries", "cpw_meta")[, 1]
  expect_identical(setdiff(unique(fb$Friender_Country), iso.codes), character(0))  # All friender countries have valid codes
  expect_identical(setdiff(unique(fb$Friended_Country), iso.codes), character(0))  # All friended countries have valid codes
})

# These data were aggregated to show the sum of all new friendships that were 1) formed within the disaster country (i.e., domestic), 
# 2) initiated by a resident of the disaster country, and 3) received by a resident in the disaster country.
# The data were indexed by a 'countdown' variable, for which months with negative values (from -6 to -1) indicated months before the disaster, zero indicated the month of the disaster,
# and positive values (1 through 6) indicated months after the disaster.
# A 'Crisis' dummy variable was set to zero for months before the disaster and one for the disaster month and following months.
fb.simple <- fb[, list(Date = unique(YearMonth), Friendships = sum(Count, na.rm = T), linear.time = numeric(0)), by = list(Countdown, Disaster_id, Country = Disaster_ISOalpha3, Crisis)]
fb.simple$Year <- as.numeric(substring(fb.simple$Date, 1, 4))
setkeyv(fb.simple, c("Disaster_id", "Country", "Year"))

# TEMP: Filter out last month of data that are at the end of the data range.
fb.simple <- fb.simple[!Date >= "201211", ]

# To account for the passage of time in terms of calendar days, each month in the data were labeled sequentially from May 2006 (marked as month 0) to Nov 2012 (month 78).
months <- unique(fb.simple$Date)

current.month.count <- 0
while (length(months) > 0) {
  thisMonth <- min(months)
  fb.simple[Date == thisMonth, linear.time := as.integer(current.month.count)]
  months <- setdiff(months, thisMonth)
  current.month.count <- current.month.count + 1
}

# Also, a quadratic form of linear time was added.
fb.simple$quadratic.time <- fb.simple$linear.time^2

# Since the distribution of the friendships was highly skewed, the variable was log-transformed to better approximate a normal distribution.
# ggplot(fb.simple, aes(x = Friendships)) + geom_histogram()
# ggplot(fb.simple, aes(x = log(Friendships))) + geom_histogram()
fb.simple$Friendships.log <- log(fb.simple$Friendships)

# Magic spaghetti graph of all disasters in chronological order.
country.names <- fb.simple[, list(Date = max(.SD$Date), Friendships = tail(.SD$Friendships, 1)), by = list(Country, Disaster_id)]

ggplot(fb.simple, aes(x = Date, y = Friendships)) + 
  geom_line(aes(group = Disaster_id, color = Country)) + 
  theme(legend.position = "none") +
  geom_text(data = country.names, aes(label = Country), hjust = 0.5, vjust = 1)

# Magic spaghetti graph of all disasters (as if they all occurred at the same time)
# Sidenote: http://www.youtube.com/watch?v=N9sqrtDLmXY
country.names <- fb.simple[, list(Countdown = max(.SD$Countdown), Friendships = tail(.SD$Friendships, 1)), by = list(Country, Disaster_id)]

ggplot(fb.simple, aes(x = Countdown, y = Friendships)) + 
  geom_line(aes(group = Disaster_id, color = Country)) + 
  theme(legend.position = "none") +
  geom_text(data = country.names, aes(label = Country), hjust = 0.5, vjust = 1)

# By itself, mere linear passing of time (as indexed by the linear) was a significant predictor of friendship formation, as Facebook has acquired more users over time.
fit.time <- lm(Friendships ~ linear.time + quadratic.time, data = fb.simple)
summary(fit.time)

fit.time.log <- lm(Friendships.log ~ linear.time + quadratic.time, data = fb.simple)
summary(fit.time.log)

# However, the interaction of the crisis dummy variable and countdown was not significant.
fit.timexcrisis <- lm(Friendships ~ Countdown*Crisis + linear.time + quadratic.time, data = fb.simple)
summary(fit.timexcrisis)

fit.timexcrisis.log <- lm(Friendships.log ~ Countdown*Crisis + linear.time + quadratic.time, data = fb.simple)
summary(fit.timexcrisis.log)

# To control for the magnitude of the disaster, the total amount of funding donated to each disaster were added as a control variable. 
donations <- magicSQL("SELECT emergency_id as Disaster_id, sum(amount) as Donation FROM donations WHERE amount > 1 GROUP BY emergency_id", "cpw_Crises")

# Because majority of the disasters only received modest amounts of donations, but few received very large donations, the distribution was very skewed.
# This was corrected by log-transforming the donations variable.
ggplot(donations, aes(x = Donation)) + geom_histogram()
ggplot(donations, aes(x = log(Donation))) + geom_histogram()
donations$Donation.log <- log(donations$Donation)

fb.simple <- merge(fb.simple, donations, by = "Disaster_id")

# The donation amount a disaster received was a significant predictor for the non-log-transformed model,
fit.timexcrisis.donations <- lm(Friendships ~ Countdown*Crisis + linear.time + quadratic.time + Donation, data = fb.simple)
summary(fit.timexcrisis.donations)

# but not for the log-transformed one.
fit.timexcrisis.donations.log <- lm(Friendships.log ~ Countdown*Crisis + linear.time + quadratic.time + Donation.log, data = fb.simple)
summary(fit.timexcrisis.donations.log)

# To control for the effect of differing popularity of internet in the country, 
# yearly data about internet penetration per country (i.e., percentage of internet users) were added to the model.
internet <- data.table(magicSQL("SELECT countries.ISOalpha3 as Country, Year, InternetUsers as Penetration FROM `countries_by_year` LEFT JOIN `countries` ON countries_by_year.Code = countries.ISOalpha2", "cpw_meta"))
setkeyv(internet, c("Country", "Year"))
fb.simple <- merge(fb.simple, internet)

# but not for the log-transformed one.
fit.timexcrisis.donations.penetration <- lm(Friendships ~ Countdown*Crisis + linear.time + quadratic.time + Donation + Penetration, data = fb.simple)
summary(fit.timexcrisis.donations.penetration)

fit.timexcrisis.donations.penetration.log <- lm(Friendships.log ~ Countdown*Crisis + linear.time + quadratic.time + Donation.log + Penetration, data = fb.simple)
summary(fit.timexcrisis.donations.penetration.log)

# Simplest model, R^2 = .66
fit <- lm(Friendships.log ~ linear.time + Penetration, data = fb.simple)
summary(fit)

# Multilevel model
multi.baseline <- lmer(
  Friendships.log ~ 
    linear.time + 
    quadratic.time + 
    Countdown + 
    (1|Disaster_id) + 
    (1|Country), 
  data = fb.simple
)


multi.crisis <- lmer(
  Friendships.log ~ 
    linear.time + 
    quadratic.time + 
    Countdown*Crisis + 
    (linear.time|Disaster_id) + 
    (1|Country), 
  data = fb.simple
)

# http://seriousstats.wordpress.com/2013/04/18/using-multilevel-models-to-get-accurate-inferences-for-repeated-measures-anova-designs/
KRmodcomp(multi.crisis, multi.baseline)