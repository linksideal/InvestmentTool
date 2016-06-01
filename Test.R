setwd("D:/Investment Tool")

source("Daten.R")
source("Analyse.R")

startDatum <- as.Date("2015-04-23")
endDatum <- as.Date("2016-05-23")
aktienSymbole <- as.vector(unlist(read.csv("Hilfsdaten/comdirectIndizes.csv", header = FALSE, sep = ";")))

gewichte <- holeTangecyPortfolio(aktienSymbole, startDatum-100, startDatum-10, "d")

#holePortfolioReturn(aktienSymbole, gewichte, startDatum, endDatum)

sapply(seq(startDatum, by = -100, length = 10), holePortfolioReturn, aktienSymbole = aktienSymbole, gewichte = holeTangecyPortfolio(aktienSymbole, startDatum-100, startDatum-10, "d"), endDatum = startDatum + 30, simplify = TRUE)
