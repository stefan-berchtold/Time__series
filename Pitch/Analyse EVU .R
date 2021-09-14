library (xts)
library (fBasics)
library (ggfortify)
library (ggplot2)
library (timeSeries)
library (tseries)
library (zoo)
library (plotly)
library (vars)
library (fpp2)

#Einlesen csv.Datei
co2_evu <- read.csv("timeseries-1.csv", header = TRUE, sep = ";")
daten <-xts(co2_evu[,-1], order.by=as.Date(co2_evu$X, "%d.%m.%y"))



#Umwandlung in ts
attr(daten, 'frequency') <- 7
periodicity(daten)
plot(decompose(as.ts(daten$CO2.Prc)))


## CO2
plot(log(daten$CO2.Prc), main = "Wachstumsrate CO2-Preis")
plot(daten$CO2.Prc, main = "CO2-Preis pro Tonne in Euro")
plot(daten$CO2.Vol, main = "Gehandeltes Volumen von C02-Zertifikate")
dco2 <- diff(log(daten$CO2.Prc))[-1]
co2_returns <- ts(diff(log(daten$CO2.Prc)))[-1]
plot(co2_returns)
boxplot(co2_returns, main = "BoxPlot CO2 Returns")


# Saisonalität untersuchen mittels Decompose
attr(daten, 'frequency') <- 7
periodicity(daten)
plot(decompose(as.ts(daten$CO2.Prc))) #Saisonalität nicht relevant

# Histogramm von C02-Preis
hist(diff(log(daten$CO2.Prc)), breaks = 50, main = "Histogram of continuous C02 returns", xlab="C02 returns")
basicStats(diff(log(daten$CO2.Prc)))

# Stationarität untersuchen (falls nicht gegeben: Diff und Log)
adf.test(daten$CO2.Prc)

# Stationarität nicht gegeben -> diff und log
adf.test(diff(log(daten$CO2.Prc))[-1])
acf(diff(log(daten$CO2.Prc))[-1], main = "ACF von CO2-Preis")



##BKW
plot(daten$BKW, main = "Aktienkurs BKW")
plot(log(daten$BKW), main = "Wachstumsrate Aktienkurs BKW", ylim = c(3, 4.4))
plot(diff(log(daten$BKW)))
dbkw <- diff(log(daten$BKW))[-1]
bkw_returns <- ts(diff(log(daten$BKW)))[-1]
boxplot(bkw_returns, main = "BoxPlot BKW Returns")

# Saisonalität untersuchen mittels Decompose
attr(daten, 'frequency') <- 7
periodicity(daten)
plot(decompose(as.ts(daten$BKW))) #Saisonalität nicht relevant

# Histogramm von BKW-Aktienindex
hist(diff(log(daten$BKW)), breaks = 50, main = "Histogram of continuous BKW returns", xlab="BKW returns")
basicStats(diff(log(daten$BKW)))

# Stationarität untersuchen (falls nicht gegeben: Diff und Log)
adf.test(daten$BKW)

# Stationarität nicht gegeben -> diff und log
adf.test(diff(log(daten$BKW))[-1])
acf(diff(log(daten$BKW))[-1], main = "ACF von BKW-Aktienkurs")

#Scatterplot Returns BKW - CO2
plot(co2_returns, bkw_returns, main = "Wachstumsraten CO2 vs. BKW")
abline(lm(bkw_returns~co2_returns))

#VAR
summary(VAR(cbind(dbkw, dco2), type = "const", ic = "AIC"))
var_bkw <- (VAR(cbind(dbkw, dco2), type = "const", ic = "AIC"))

#Granger-Kausalität
causality(var_bkw, cause = "CO2.Prc")
causality(var_bkw, cause = "BKW")

#Forecast
plot(forecast(dbkw, h = 100, level = 0.95))


#RWE
plot(daten$RWE, main = "Aktienkurs RWE")
plot(log(daten$RWE), main = "Wachstumsrate Aktienkurs RWE", ylim = c(2, 4))
plot(diff(log(daten$RWE)))
drwe <- diff(log(daten$RWE))[-1]
rwe_returns <- ts(diff(log(daten$RWE)))[-1]
boxplot(rwe_returns, main = "BoxPlot RWE Returns")

# Saisonalität untersuchen mittels Decompose
attr(daten, 'frequency') <- 7
periodicity(daten)
plot(decompose(as.ts(daten$RWE))) #Saisonalität nicht relevant

# Histogramm von RWE-Aktienindex
hist(diff(log(daten$RWE)), breaks = 50, main = "Histogram of continuous RWE returns", xlab="RWE returns")
basicStats(diff(log(daten$RWE)))

# Stationarität untersuchen (falls nicht gegeben: Diff und Log)
adf.test(daten$RWE)

# Stationarität nicht gegeben -> diff und log
adf.test(diff(log(daten$RWE))[-1])
acf(diff(log(daten$RWE))[-1], main = "ACF von RWE-Aktienkurs")

#Scatterplot
plot(co2_returns, rwe_returns, main = "Wachstumsraten CO2 vs. RWE")
abline(lm(rwe_returns~co2_returns))

#VAR
summary(VAR(cbind(dco2, drwe), type = "const", ic = "AIC"))
var_rwe <- (VAR(cbind(dco2, drwe), type = "const", ic = "AIC"))

#Granger-Kausalität
causality(var_rwe, cause = "CO2.Prc")
causality(var_rwe, cause = "RWE")

#Forecast
plot(forecast(drwe, h = 100, level = 0.95))


##Verbund
plot(daten$Verbund, main = "Aktienkurs Verbund")
plot(log(daten$Verbund), main = "Wachstumsrate Aktienkurs Verbund", ylim = c(2,4.5))
plot(diff(log(daten$Verbund)))
dverbund <- diff(log(daten$Verbund))[-1]
verbund_returns <- ts(diff(log(daten$Verbund)))[-1]
boxplot(verbund_returns, main = "BoxPlot Verbund Returns")

# Saisonalität untersuchen mittels Decompose
attr(daten, 'frequency') <- 7
periodicity(daten)
plot(decompose(as.ts(daten$Verbund))) #Saisonalität nicht relevant

# Histogramm von Verbund-Aktienindex
hist(diff(log(daten$Verbund)), breaks = 50, main = "Histogram of continuous Verbund returns", xlab="Verbund returns")
basicStats(diff(log(daten$Verbund)))

# Stationarität untersuchen (falls nicht gegeben: Diff und Log)
adf.test(daten$Verbund)

# Stationarität nicht gegeben -> diff und log
adf.test(diff(log(daten$Verbund))[-1])
acf(diff(log(daten$Verbund))[-1], main = "ACF von Verbund-Aktienkurs")

# Scatterplot C02 Verbund
plot(co2_returns, verbund_returns, main = "Wachstumsraten CO2 vs. Verbund")
abline(lm(verbund_returns~co2_returns))
lm(verbund_returns~co2_returns)

#VAR
summary(VAR(cbind(dco2, dverbund), type = "const", ic = "AIC"))
var_verbund <- VAR(cbind(dco2, dverbund), type = "const", ic = "AIC")

#Granger Kausalität
causality(var_verbund, cause = "CO2.Prc")
causality(var_verbund, cause = "Verbund")

#Schocken
feir <- irf(var_verbund, n.ahead = 4, ortho = TRUE, runs = 1000)
plot(feir)

#Forecast
plot(forecast(dverbund, h = 100, level = 0.95))


##Grafiken 
autoplot(log(daten[,-1:-3]), facets = FALSE)+
  ggtitle("Wachstumsraten Aktienindices EVU") + xlab("Jahr") + ylab("Wachstumsrate")+
  labs(color="Legende") +
  scale_colour_manual(breaks = c("BKW", "RWE", "Verbund"), values = c("red", "blue", "purple"))

#mit C02-Preis
autoplot(log(daten[,-2:-3]), facets = FALSE)+
  ggtitle("Wachstumsraten Aktienkurse EVU und CO2-Preis") + xlab("Jahr") + ylab("Wachstumsrate")+
  labs(color="Legende") +
  scale_colour_manual(breaks = c("CO2.Prc", "BKW", "RWE", "Verbund"), 
                      values = c("brown", "red", "blue", "purple"))

#3D-Grafik BKW - RWE - CO2
plot_ly(x = as.numeric(dbkw$BKW), y = as.numeric(drwe$RWE), 
        z= as.numeric(dco2$CO2.Prc),
        type="scatter3d", mode="markers", color=as.numeric(dco2$CO2.Prc),
        size = 1) %>%
  layout(
    title = "Wachstumsraten BKW, RWE, CO2-Preis",
    scene = list(
      xaxis = list(title = "BKW"),
      yaxis = list(title = "RWE"),
      zaxis = list(title = "CO2-Preis")
    ))

#3D-Grafik Verbund - RWE - CO2
plot_ly(x = as.numeric(dverbund$Verbund), y = as.numeric(drwe$RWE), 
        z= as.numeric(dco2$CO2.Prc),
        type="scatter3d", mode="markers", color=as.numeric(dco2$CO2.Prc),
        size = 1) %>%
  layout(
    title = "Wachstumsraten Verbund, RWE, CO2-Preis",
    scene = list(
      xaxis = list(title = "Verbund"),
      yaxis = list(title = "RWE"),
      zaxis = list(title = "CO2-Preis")
    ))



