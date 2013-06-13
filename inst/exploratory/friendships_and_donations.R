require(ggplot2)

f <- vector(mode="double", length = nrow(crises))
crises$friends <- f

for (i in 1:nrow(crises)) {
  cat(i, "\n")
  iso.code <- crises[i, iso2]
  year <- crises[i, year]
  month <- crises[i, month]
  
  friendships <- magicSQL(
    sprintf(
    "SELECT SUM(Count) as Friendships FROM `Friendships_Directional_Final` 
    WHERE Friended_Country = '%s' AND Year = %i AND Month = %i AND Friender_Country != '%s'",
    iso.code, year, month, iso.code),
    "cpw_meta")[, 1]
  
  crises[i, friends:= friendships]
}

# Diagnostic plots
ggplot(crises, aes(x = log(funding))) + geom_histogram()
ggplot(crises, aes(x = log(friends))) + geom_histogram()

ggplot(crises, aes(x = log(friends), y = log(funding), color = year)) + geom_point() + theme_bw()

# Per year
ggplot(crises[year == "2006"], aes(x = log(friends), y = log(funding), color = year)) + geom_point() + theme_bw()
ggplot(crises[year == "2007"], aes(x = log(friends), y = log(funding), color = year)) + geom_point() + theme_bw()
ggplot(crises[year == "2008"], aes(x = log(friends), y = log(funding), color = year)) + geom_point() + theme_bw()
ggplot(crises[year == "2009"], aes(x = log(friends), y = log(funding), color = year)) + geom_point() + theme_bw()
ggplot(crises[year == "2010"], aes(x = log(friends), y = log(funding), color = year)) + geom_point() + theme_bw()
ggplot(crises[year == "2011"], aes(x = log(friends), y = log(funding), color = year)) + geom_point() + theme_bw()
ggplot(crises[year == "2012"], aes(x = log(friends), y = log(funding), color = year)) + geom_point() + theme_bw()

crises$friends
