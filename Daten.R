#
# Funktion gibt historische Daten von einschließlich 3. Januar 2001 verfügbar bis gestern zurück
#
# Input:
# aktienSymbol - Aktien-Symbol (siehe Yahoo Finance, z.B. ADS.DE für Adidas AG Xetra), als String
# startDatum, endDatum - im Format "YYYY-MM-DD", z.B. "2010-12-31", als String
# diskretisierung - d=tägliche Werte, wöchentliche Werte, m=monatliche Werte, v=Dividendenausschüttungen, als String
# keinVolumenEinschliessen - FALSE=Beobachtungen mit Trade-Volumen gleich 0 werden ausgeschlossen, TRUE=solche Beobachtungen werden mit ausgegeben
#
# Quelle: http://brusdeylins.info/tips_and_tricks/yahoo-finance-api/
#
holeZeitreihe <- function(aktienSymbol, startDatum, endDatum, diskretisierung, keinVolumenEinschliessen){
  # gegebenes Start- und Enddatum wird als Datumstyp gecastet
  startDatum <- as.Date(startDatum)
  endDatum <- as.Date(endDatum)
  
  # Tage und Jahre werden als Character gecastet
  startTagChar <- format(startDatum,"%d")
  startJahrChar <- format(startDatum,"%Y")
  endTagChar <- format(endDatum,"%d")
  endJahrChar <- format(endDatum,"%Y")
  
  # Monate werden als Integer gecastet, weil sie weiterverarbeitet werden mÃ¼ssen
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
# Funktion gibt Liste von Zeitreihen verschiedener Aktien zurück
#
# Input:
# aktienSymbole - Vektor von Aktien-Symbolen (siehe Yahoo Finance, z.B. ADS.DE für Adidas AG Xetra), als String
# startDatum, endDatum - im Format "YYYY-MM-DD", z.B. "2010-12-31", als String
# diskretisierung - d=tägliche Werte, wöchentliche Werte, m=monatliche Werte, v=Dividendenausschüttungen, als String
# keinVolumenEinschliessen - FALSE=Beobachtungen mit Trade-Volumen gleich 0 werden ausgeschlossen, TRUE=solche Beobachtungen werden mit ausgegeben
#
holeZeitreihen <- function(aktienSymbole, startDatum, endDatum, diskretisierung, keinVolumenEinschliessen){
  zeitReihen <- list()
  for (i in 1:length(aktienSymbole)) {
    zeitReihen[[i]] <- holeZeitreihe(aktienSymbole[i], startDatum, endDatum, diskretisierung, keinVolumenEinschliessen)
  }
  return(zeitReihen)
}


#
# Funktion gibt für eine Liste von Zeitreihen einen Vektor alle Datums aus, die in allen Zeitreihen vorkommen
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
# Funktion reduziert gegeben Zeitreihen auf gegeben Datums und gibt die reduzierte Zeitreihen zurück
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
# Funktion gibt bereinigte Zeitreihen verschiedener Aktien als Liste von Data.Frames zurück
#
# Input:
# aktienSymbole - Vektor von Aktien-Symbolen (siehe Yahoo Finance, z.B. ADS.DE für Adidas AG Xetra), als String
# startDatum, endDatum - im Format "YYYY-MM-DD", z.B. "2010-12-31", als String
# diskretisierung - d=tägliche Werte, wöchentliche Werte, m=monatliche Werte, v=Dividendenausschüttungen, als String
# keinVolumenEinschliessen - FALSE=Beobachtungen mit Trade-Volumen gleich 0 werden ausgeschlossen, TRUE=solche Beobachtungen werden mit ausgegeben
# reduziertAufSchnittmenge - FALSE=Zeitreihen der verschiedenen Aktien können unterschiedliche Datums enthalten, TRUE=Zeitreihen der versch. Aktien enthalten nur die Schnittmenge ihrer Datums
#
holeBereinigteZeitreihen <- function(aktienSymbole, startDatum, endDatum, diskretisierung, keinVolumenEinschliessen, reduziertAufSchnittmenge){
  zeitReihen <- holeZeitreihen(aktienSymbole, startDatum, endDatum, diskretisierung, keinVolumenEinschliessen)
  if (reduziertAufSchnittmenge == TRUE){
    datums <- holeDatumsSchnittmenge(zeitReihen)
    zeitReihen <- reduziereZeitReihenAufDatums(zeitReihen, datums)
  }
  return(zeitReihen)
}

#
#
#
holeNaechstenHandelstag <- function(aktienSymbol, datum, vorwaertsSuchen){
  datum <- as.Date(datum)
  ersterHandelstag <- tryCatch(
    {
      zeitReihe <- holeZeitreihe(aktienSymbol = aktienSymbol, startDatum = datum, endDatum = datum, diskretisierung = "d", keinVolumenEinschliessen = TRUE)
      if(length(zeitReihe$Date) > 0){
        zeitReihe$Date
      }else{
        stop()
      }
    },
    error = function(cond){
      if(vorwaertsSuchen == TRUE){
        return(holeNaechstenHandelstag(aktienSymbol, datum+1, vorwaertsSuchen))
      }else{
        return(holeNaechstenHandelstag(aktienSymbol, datum-1, vorwaertsSuchen))
      }
    }
  )
  return(ersterHandelstag)
}

#
# Funktion gibt Adj.Close Wert zu übergebener Aktien zum gegebenen Datum
# Falls zu gebenem Datum kein Adj.Close Wert vorhanden ist wird nächster
#
# Input:
# aktienSymbol - Yahoo Kürzel einer Aktien
# datum - Datum in der Form "JJJJ-MM-DD"
# vorwaertsSuchen - FALSE = Letzter verfügbarer Adj.Close Wert wird zurückgegeben,
#                   TRUE  = Nächster verfügbarer Adj.Close Wert wird zurückgegeben
#
holeTagesWert <- function(aktienSymbol, datum, vorwaertsSuchen){
  datum <- holeNaechstenHandelstag(aktienSymbol = aktienSymbol, datum = datum, vorwaertsSuchen = vorwaertsSuchen)
  return(holeZeitreihe(aktienSymbol = aktienSymbol, startDatum = datum, endDatum = datum, diskretisierung = "d", keinVolumenEinschliessen = TRUE)$Adj.Close)
}

#
# Funktion gibt Adj. Close Wert zu übergebenem Array von Aktien zum gegebenem Datum
#
# Input:
# aktienSymbole - Array von Aktien
# datum - Datum in der Form "JJJJ-MM-DD"
#
holeTagesWerte <- function(aktienSymbole, datum, vorwaertsSuchen, gleichzeitig){
  if( gleichzeitig == FALSE ){
    return(sapply(aktienSymbole, holeTagesWert, datum = datum, vorwaertsSuchen = vorwaertsSuchen, simplify = TRUE))
  }else{
    naechsterHandelstage <- sapply(aktienSymbole, holeNaechstenHandelstag, datum = datum, vorwaertsSuchen = vorwaertsSuchen, simplify = TRUE)
    if(vorwaertsSuchen == TRUE){
      naechsterHandelstag = max(naechsterHandelstage)
    }else{
      naechsteHandelstag = min(naechsterHandelstage)
    }
    return(sapply(aktienSymbole, holeTagesWert, datum = naechsterHandelstag, vorwaertsSuchen = vorwaertsSuchen, simplify = TRUE))
  }
}