#Kommentar

#
# Funktion berechnet für gegebene Aktien das Tangency Portfolio aus,
# wobei zur SchÃ¤tzung der Erwartung und der Covarianzen der Zeitraum
# von startDatum bis endDatum mit Diskretisierung diskretisierung verwendet wird.
# Außerdem wird angemonnen, dass der Risk-Free-Return = 0.
#
# aktienSymbole - Vektor von Aktien-Symbolen (siehe Yahoo Finance, z.B. ADS.DE für Adidas AG Xetra), als String
# startDatum, endDatum - im Format "YYYY-MM-DD", z.B. "2010-12-31", als String
# diskretisierung - d=tägliche Werte, wöchentliche Werte, m=monatliche Werte, v=Dividendenausschüttungen, als String
#
holeTangecyPortfolio <- function(aktienSymbole, startDatum, endDatum, diskretisierung){
  
  # Bereinigte Daten werden geholt
  zeitReihen <- holeBereinigteZeitreihen(aktienSymbole, startDatum, endDatum, diskretisierung, TRUE, TRUE)
  
  if (length(aktienSymbole) > length(zeitReihen[[1]]$Date)){
    warning("!!! Warnung: holeTangecyPortfolio !!! Weniger Beobachtungen als Aktien! Das führt zu Multikoliniearität in der Covarianz-Matrix!")
  }
  
  # Adjusted-Close-Matrix anlegen; erste Spalte beinhaltet die Adjusted Close Werte der ersten Aktie
  matrix <- cbind(zeitReihen[[1]]$Adj.Close)
  
  # Erwartungswert-Vektor anlegen
  Mu <- vector(mode = "double", length = length(zeitReihen))
  # Erster Eintrag ist Erwartungswert des Adjusted-Close der ersten Aktie
  Mu[1] <- mean(zeitReihen[[1]]$Adj.Close)
  
  # alle übrigen Komponenten der Matrix und der Vektors werden belegt
  for(i in 2:length(zeitReihen)){
    matrix <- cbind(matrix,zeitReihen[[i]]$Adj.Close)
    Mu[i] <- mean(zeitReihen[[i]]$Adj.Close)
  }
  
  # Covarianz Matrix der Matrix wird berechnet
  Epsilon <- cov(matrix)
  
  # Invertieren der Covarianz Matrix
  EpsilonInv <- solve(Epsilon)
  
  # Vektor aus Einsen
  Eins <- rep(1, length(zeitReihen))
  
  # Tangency-Portfolio-Formel für Risk-Free-Return = 0
  Zaehler <- EpsilonInv %*% Mu 
  Nenner <- Eins %*% Zaehler
  return(Zaehler/as.double(Nenner))
}

#
# Funktion gibt Return eines Portfolios das aus den Aktien aktienSymbole besteht, welche
# mit gewichte gewichtet sind, zum startDatum gekauft und zum endDatum verkauft werden
#
holePortfolioReturn <- function(aktienSymbole, gewichte, startDatum, endDatum){
  einkaufsPreis <- sum(holeTagesWerte(aktienSymbole, startDatum, TRUE, TRUE)*gewichte)
  verkaufsPreis <- sum(holeTagesWerte(aktienSymbole, endDatum, TRUE, TRUE)*gewichte)
  return((verkaufsPreis-einkaufsPreis)/einkaufsPreis)
}