require(magic)
require(spellbook)
require(ggplot2)

obl <- data.frame(date = "2011 05 02", name = "OBL shot")

pdf("/Users/Ilmo/Desktop/Arab Spring.pdf", paper="a4r")

tn.events <- data.frame(date = "2010 12 18", name = "Protests start")
plot.facebook("TN", rbind(tn.events, obl))

eg.events <- data.frame(date = "2011 01 25", name = "Protests start")
plot.facebook("EG", rbind(eg.events, obl))

ly.events <- data.frame(date = "2011 02 15", name = "Protests start")
plot.facebook("LY", rbind(ly.events, obl))

ye.events <- data.frame(date = "2011 01 15", name = "Protests start")
plot.facebook("YE", rbind(ye.events, obl))

sy.events <- data.frame(date = "2011 01 26", name = "Protests start")
plot.facebook("SY", rbind(sy.events, obl))

bh.events <- data.frame(date = "2011 02 14", name = "Protests start")
plot.facebook("BH", rbind(bh.events, obl))

dev.off()