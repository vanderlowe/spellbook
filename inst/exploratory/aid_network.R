require(magic)
require(ggplot2)
require(devtools)
require(data.table)
require(igraph)
require(RColorBrewer)
source_url("https://raw.github.com/vanderlowe/spellbook/master/inst/load_data.R")

getDonors <- function(id) {
  return(donor.countries[emergency_id == id, ]$iso2)
}

ties <- data.table(from = character(0), to = character(0), year = numeric(0))

recipients <- unique(crises$iso3)

# Get donor network by year
for (thisCountry in recipients) {
  thisCountry <- iso2(thisCountry)
  cat(thisCountry, "\n")
  theseDisasters <- crises[iso2 == thisCountry, emergency_id]
  
  for (thisDisaster in theseDisasters) {
    cat(thisDisaster, "\n")
    thisYear <- crises[emergency_id == thisDisaster, year]
    theseDonors <- getDonors(thisDisaster)
    if (identical(theseDonors, character(0))) {next}
    theseResults <- data.frame(from = theseDonors, to = thisCountry, year = thisYear)
    ties <- rbind(ties, theseResults)
  }  
}
ties <- ties[!from == as.character(to),] # Drop self-donation

included.countries <- unique(c(as.character(ties$to), as.character(ties$from)))
nodes <- world.data[ISOalpha2 %in% included.countries, list(iso2 = ISOalpha2, name, region, x = capital_longitude, y = capital_latitude)]

year.range <- unique(crises$year)[order(unique(crises$year))]

pdf("international_aid_2006-2012.pdf", width = 25, height = 19)
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