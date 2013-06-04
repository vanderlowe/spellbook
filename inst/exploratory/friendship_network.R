require(magic)
require(ggplot2)
require(devtools)
require(data.table)
require(igraph)
require(RColorBrewer)
source_url("https://raw.github.com/vanderlowe/spellbook/master/inst/load_data.R")

getFriends <- function(id) {
  target <- crises[emergency_id == id, iso2]
  year <- crises[emergency_id == id, year]
  month <- crises[emergency_id == id, month]
  sql <- 
    sprintf("SELECT Friender_Country, Count FROM Friendships_Directional_Final WHERE Friended_Country = '%s' AND Year = %i AND Month = %i AND Friender_Country != '%s'",
            target, year, month, target)
  return(magicSQL(sql, "cpw_meta"))
}

ties <- data.table(
  from = character(0), 
  to = character(0), 
  year = numeric(0), 
  Count = numeric(0)
)

# Donations graph
recipients <- unique(crises$iso3)

# Get donor network by year
for (thisCountry in recipients) {
  thisCountry <- iso2(thisCountry)
  cat(thisCountry, "\n")
  theseDisasters <- crises[iso2 == thisCountry, emergency_id]
  
  for (thisDisaster in theseDisasters) {
    cat(thisDisaster, "\n")
    thisYear <- crises[emergency_id == thisDisaster, year]
    theseFriends <- getFriends(thisDisaster)
    if (identical(theseFriends, character(0))) {next}
    if (nrow(theseFriends) == 0) {next}
    theseResults <- data.frame(from = theseFriends$Friender_Country, to = thisCountry, year = thisYear)
    theseResults <- merge(theseResults, theseFriends, by.x = "from", by.y = "Friender_Country")
    ties <- rbind(ties, theseResults)
  }  
}

included.countries <- unique(c(as.character(ties$to), as.character(ties$from)))
nodes <- world.data[ISOalpha2 %in% included.countries, list(iso2 = ISOalpha2, name, region, x = capital_longitude, y = capital_latitude)]

year.range <- unique(crises$year)[order(unique(crises$year))]

pdf("international_friends_2006-2012.pdf", width = 25, height = 19)
  for (thisYear in year.range) {
    g <- graph.data.frame(ties[year == thisYear,], directed = T, vertices = nodes)
    my.pal <- brewer.pal(n = 8, name="Set1")
    
      plot(g,
         layout = as.matrix(cbind(nodes$x, nodes$y)),
         vertex.label = V(g)$name,
         vertex.label.family = "sans",
         vertex.label.color = "black",
         vertex.label.cex = 0.75,
         vertex.color=my.pal[as.factor(V(g)$region)],
         vertex.size = 3,
         edge.arrow.size = 0.75,
         edge.color = "black"
      )
    title(thisYear)
  }
dev.off()