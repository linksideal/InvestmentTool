# Funktion gibt historische Daten von einschließlich 3. Januar 2001 verfügbar bis gestern zurück
#
# Input:
# aktienSymbol - Aktien-Symbol (siehe Yahoo Finance, z.B. ADS.DE für Adidas AG Xetra), als String
# startDatum, endDatum - im Format "YYYY-MM-DD", z.B. "2010-12-31", als String
# diskretisierung - d=tägliche Werte, w=wöchentliche Werte, m=monatliche Werte, v=Dividendenausschüttungen, als String
#
# Quelle: http://brusdeylins.info/tips_and_tricks/yahoo-finance-api/

getHistoricalYahooData <- function(aktienSymbol, startDatum, endDatum, diskretisierung){
  # gegebenes Start- und Enddatum wird als Datumstyp gecastet
  startDatum <- as.Date(startDatum)
  endDatum <- as.Date(endDatum)
  
  #Tage und Jahre werden als Character gecastet
  startTagChar <- format(startDatum,"%d")
  startJahrChar <- format(startDatum,"%Y")
  endTagChar <- format(endDatum,"%d")
  endJahrChar <- format(endDatum,"%Y")
  
  #Monate werden als Integer gecastet, weil sie weiterverarbeitet werden müssen
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
  
  return(read.csv(url))
}

zeitReiheAdidas <- getHistoricalYahooData("ADS.DE", "2015-5-1", "2016-5-23", "d")
zeitReiheFresenius <- getHistoricalYahooData("FME.DE", "2015-5-1", "2016-5-23", "d")
