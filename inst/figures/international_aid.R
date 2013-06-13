require(igraph)
require(RColorBrewer)
loadData("world.data")
loadData("donations")

included.countries <- unique(c(as.character(donations$to), as.character(donations$from)))
nodes <- world.data[ISOalpha2 %in% included.countries, list(iso2 = ISOalpha2, name, region, x = capital_longitude, y = capital_latitude)]

year.range <- unique(crises$year)[order(unique(crises$year))]

pdf("international_aid_2006-2012.pdf", width = 25, height = 19)
for (thisYear in year.range) {
  g <- graph.data.frame(donations[year == thisYear,], directed = T, vertices = nodes)
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