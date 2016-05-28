source("Daten.R")
source("Analyse.R")

startDatum <- as.Date("2015-05-01")
endDatum <- as.Date("2016-05-23")
aktienSymbole <- as.vector(unlist(read.csv("Hilfsdaten/comdirectIndizes.csv", header = FALSE, sep = ";")))

holeTangecyPortfolio(aktienSymbole, startDatum, endDatum, "d")