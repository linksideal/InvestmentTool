#
# Funktion gibt historische Daten von einschlie�lich 3. Januar 2001 verf�gbar bis gestern zur�ck
#
# Input:
# aktienSymbol - Aktien-Symbol (siehe Yahoo Finance, z.B. ADS.DE f�r Adidas AG Xetra), als String
# startDatum, endDatum - im Format "YYYY-MM-DD", z.B. "2010-12-31", als String
# diskretisierung - d=t�gliche Werte, w�chentliche Werte, m=monatliche Werte, v=Dividendenaussch�ttungen, als String
# keinVolumenEinschliessen - FALSE=Beobachtungen mit Trade-Volumen gleich 0 werden ausgeschlossen, TRUE=solche Beobachtungen werden mit ausgegeben
#
# Quelle: http://brusdeylins.info/tips_and_tricks/yahoo-finance-api/
#
holeHistoricalYahooData <- function(aktienSymbol, startDatum, endDatum, diskretisierung, keinVolumenEinschliessen){
  # gegebenes Start- und Enddatum wird als Datumstyp gecastet
  startDatum <- as.Date(startDatum)
  endDatum <- as.Date(endDatum)
  
  # Tage und Jahre werden als Character gecastet
  startTagChar <- format(startDatum,"%d")
  startJahrChar <- format(startDatum,"%Y")
  endTagChar <- format(endDatum,"%d")
  endJahrChar <- format(endDatum,"%Y")
  
  # Monate werden als Integer gecastet, weil sie weiterverarbeitet werden müssen
  startMonatInt <- as.integer(format(startDatum,"%m"))
  endMonatInt <- as.integer(format(endDatum,"%m"))
  
  # Monate müssen im format "00", "01", ..., "11" angegeben werden
  startMonatChar <- formatC(startMonatInt - 1, width = 2, flag = "0")
  endMonatChar <- formatC(endMonatInt - 1, width = 2, flag = "0")
  
  # Datum-Strings werden zusammengesetzt
  startDatum <- paste("a=", startMonatChar, "&b=", startTagChar, "&c=", startJahrChar, sep="")
  endDatum <- paste("d=",endMonatChar, "&e=", endTagChar, "&f=", endJahrChar, sep="")
  
  # Gesamte URL wird zusammengesetzt
  url <- paste("http://ichart.finance.yahoo.com/table.csv?s=", aktienSymbol,"&", startDatum, "&", endDatum, "&g=", as.character(diskretisierung), "&ignore=.cvs", sep="")
  
  # Daten werden als Data.Frame abgelegt
  data <- read.csv(url, header = TRUE, stringsAsFactors=FALSE)
  
  # Beobachtungen mit Trade-Volumen gleich 0 werden ggf. ausgeschlossen
  if (keinVolumenEinschliessen == FALSE){
    data <- subset(data, Volume > 0)
  }
  
  return(data)
}


#
# Funktion gibt Liste von Zeitreihen verschiedener Aktien zur�ck
#
# Input:
# aktienSymbole - Vektor von Aktien-Symbolen (siehe Yahoo Finance, z.B. ADS.DE f�r Adidas AG Xetra), als String
# startDatum, endDatum - im Format "YYYY-MM-DD", z.B. "2010-12-31", als String
# diskretisierung - d=t�gliche Werte, w�chentliche Werte, m=monatliche Werte, v=Dividendenaussch�ttungen, als String
# keinVolumenEinschliessen - FALSE=Beobachtungen mit Trade-Volumen gleich 0 werden ausgeschlossen, TRUE=solche Beobachtungen werden mit ausgegeben
#
holeZeitreihen <- function(aktienSymbole, startDatum, endDatum, diskretisierung, keinVolumenEinschliessen){
  zeitReihen <- list()
  for (i in 1:length(aktienSymbole)) {
    zeitReihen[[i]] <- holeHistoricalYahooData(aktienSymbole[i], startDatum, endDatum, diskretisierung, keinVolumenEinschliessen)
  }
  return(zeitReihen)
}


#
# Funktion gibt f�r eine Liste von Zeitreihen einen Vektor alle Datums aus, die in allen Zeitreihen vorkommen
#
# Input:
# zeitReihen - Liste von Zeitreihen
#
holeDatumsSchnittmenge <- function(zeitReihen){
  dates1 <- zeitReihen[[1]]$Date
  for (i in 2:length(zeitReihen)){
    dates2 <- zeitReihen[[i]]$Date
    dates1 <- dates1[dates1 %in% dates2]
  }
  return(dates1)
}


#
# Funktion reduziert gegeben Zeitreihen auf gegeben Datums und gibt die reduzierte Zeitreihen zur�ck
#
# Input:
# zeitReihen - Liste von Zeitreihen
# datums - Vektor von Datums
#
reduziereZeitReihenAufDatums <- function(zeitReihen, datums){
  for (i in 1:length(zeitReihen)){
    zeitReihen[[i]] <- zeitReihen[[i]][zeitReihen[[i]]$Date %in% datums,]
  }
  return(zeitReihen)
}

#
# Funktion gibt bereinigte Zeitreihen verschiedener Aktien als Liste von Data.Frames zur�ck
#
# Input:
# aktienSymbole - Vektor von Aktien-Symbolen (siehe Yahoo Finance, z.B. ADS.DE f�r Adidas AG Xetra), als String
# startDatum, endDatum - im Format "YYYY-MM-DD", z.B. "2010-12-31", als String
# diskretisierung - d=t�gliche Werte, w�chentliche Werte, m=monatliche Werte, v=Dividendenaussch�ttungen, als String
# keinVolumenEinschliessen - FALSE=Beobachtungen mit Trade-Volumen gleich 0 werden ausgeschlossen, TRUE=solche Beobachtungen werden mit ausgegeben
# reduziertAufSchnittmenge - FALSE=Zeitreihen der verschiedenen Aktien k�nnen unterschiedliche Datums enthalten, TRUE=Zeitreihen der versch. Aktien enthalten nur die Schnittmenge ihrer Datums
#
holeDaten <- function(aktienSymbole, startDatum, endDatum, diskretisierung, keinVolumenEinschliessen, reduziertAufSchnittmenge){
  zeitReihen <- holeZeitreihen(aktienSymbole, startDatum, endDatum, diskretisierung, keinVolumenEinschliessen)
  if (reduziertAufSchnittmenge == TRUE){
    datums <- holeDatumsSchnittmenge(zeitReihen)
    zeitReihen <- reduziereZeitReihenAufDatums(zeitReihen, datums)
  }
  return(zeitReihen)
}


#
# Funktion berechnet f�r gegebene Aktien das Tangency Portfolio aus,
# wobei zur Sch�tzung der Erwartung und der Covarianzen der Zeitraum
# von startDatum bis endDatum mit Diskretisierung diskretisierung verwendet wird.
# Au�erdem wird angemonnen, dass der Risk-Free-Return = 0.
#
# aktienSymbole - Vektor von Aktien-Symbolen (siehe Yahoo Finance, z.B. ADS.DE f�r Adidas AG Xetra), als String
# startDatum, endDatum - im Format "YYYY-MM-DD", z.B. "2010-12-31", als String
# diskretisierung - d=t�gliche Werte, w�chentliche Werte, m=monatliche Werte, v=Dividendenaussch�ttungen, als String
#
holeTangecyPortfolio <- function(aktienSymbole, startDatum, endDatum, diskretisierung){
  
  # Bereinigte Daten werden geholt
  zeitReihen <- holeDaten(aktienSymbole, startDatum, endDatum, diskretisierung, TRUE, TRUE)
  
  if (length(aktienSymbole) > length(zeitReihen[[1]]$Date)){
    warning("!!! Warnung: holeTangecyPortfolio !!! Weniger Beobachtungen als Aktien! Das f�hrt zu Multikoliniearit�t in der Covarianz-Matrix!")
  }
  
  # Adjusted-Close-Matrix anlegen; erste Spalte beinhaltet die Adjusted Close Werte der ersten Aktie
  matrix <- cbind(zeitReihen[[1]]$Adj.Close)
  
  # Erwartungswert-Vektor anlegen
  Mu <- vector(mode = "double", length = length(zeitReihen))
  # Erster Eintrag ist Erwartungswert des Adjusted-Close der ersten Aktie
  Mu[1] <- mean(zeitReihen[[1]]$Adj.Close)
  
  # alle �brigen Komponenten der Matrix und der Vektors werden belegt
  for(i in 2:length(zeitReihen)){
    matrix <- cbind(matrix,zeitReihen[[i]]$Adj.Close)
    Mu[i] <- mean(zeitReihen[[i]]$Adj.Close)
  }
  
  # Covarianz Matrix der Matrix wird berechnet
  Epsilon <- cov(matrix)
  
  # Invertieren der Covarianz Matrix
  EpsilonInv <- solve(Epsilon)
  
  # Vektor aus Einsen
  Eins <- seq(1, by=0, length = length(zeitReihen))
  
  # Tangency-Portfolio-Formel f�r Risk-Free-Return = 0
  Zaehler <- EpsilonInv %*% Mu 
  Nenner <- Eins %*% Zaehler
  return(Zaehler/as.double(Nenner))
}