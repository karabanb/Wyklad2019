gcrm() # rm(list=ls())
gcHi(2) # Hello World!

library(data.table)
# wizualizacja korelacji
#library(corrgram)
#library(corrplot)

load("KrukUWr2019.RData")

#!######################################### eksploracja danych - poznanie danych
# z lotu ptaka
cases[, summary(.SD), .SDcols=setdiff(names(cases), "CaseId")] 

# szczegó³owy pogl¹d na TOA - pocz¹tkowe zad³u¿enie to jedna z najwa¿niejszych 
# charakterystyk sprawy
cases[, quantile(TOA, probs=seq(0, 1, 0.05))] # jedni wol¹ liczby
plot(sort(cases$TOA)) # inni wol¹ wykres
head(cases[order(-TOA), ], 15) # najwiêksze salda (super z³ote karty)

# zró¿nicowanie salda wzglêdem produktu
cases[, .(.N,
  Min=quantile(TOA, probs=0),
  Q05=quantile(TOA, probs=0.05),
  Q1=quantile(TOA, probs=0.25),
  Med=quantile(TOA, probs=0.5),
  Q3=quantile(TOA, probs=0.75),
  Q95=quantile(TOA, probs=0.95),
  Max=quantile(TOA, probs=1)), by=Product]

# tabela kontyngencji
countTable <- table(cases$Product,
  cut(cases$TOA, breaks=quantile(cases$TOA, 
    probs=seq(from=0, to=1, by=0.2), include.lowest=TRUE)))
# wizualizacja czêstoœci tabeli kontyngencji
barplot(prop.table(countTable, 1), col=c("darkblue","darkred"), beside=TRUE)

# zró¿nicowanie salda wzglêdem produktu i p³ci
cases[, .(.N,
  Min=quantile(TOA, probs=0),
  Q05=quantile(TOA, probs=0.05),
  Q1=quantile(TOA, probs=0.25),
  Med=quantile(TOA, probs=0.5),
  Q3=quantile(TOA, probs=0.75),
  Q95=quantile(TOA, probs=0.95),
  Max=quantile(TOA, probs=1)), by=.(Gender, Product)][order(Gender, Product)] 

# zró¿nicowanie salda wzglêdem etapu egzekucji
cases[, .(.N,
  Min=quantile(TOA, probs=0),
  Q05=quantile(TOA, probs=0.05),
  Q1=quantile(TOA, probs=0.25),
  Med=quantile(TOA, probs=0.5),
  Q3=quantile(TOA, probs=0.75),
  Q95=quantile(TOA, probs=0.95),
  Max=quantile(TOA, probs=1)), by=.(Bailiff, ClosedExecution)]

# zale¿noœæ pomiêdzy zad³u¿eniem a kapita³em
plot(cases$Principal, cases$TOA, pch='.')
abline(lm(cases$TOA~cases$Principal), col="tomato", lwd=3)

# zale¿noœæ pomiêdzy zad³u¿eniem a DPD
plot(cases$DPD, cases$TOA, pch='.')
abline(lm(cases$TOA~cases$DPD), col="tomato", lwd=3)
# czêsto do wykrywania trendów u¿ywa siê nieparametrycznego estymatora loess
loessCurve <- lowess(cases$DPD, cases$TOA, f=1/3, delta=33.27)
lines(loessCurve$x, loessCurve$y, col="darkgreen", lwd=2)

# proste uzupe³nienie braków poprzez œredni¹
# brakami danych bêdziemy zajmowaæ siê oddzielnie w przysz³oœci 
cases[, MeanSalary:=ifelse(is.na(MeanSalary), 
  mean(MeanSalary, na.rm=TRUE), MeanSalary)]

plot(cases$MeanSalary, cases$TOA, pch='.')
abline(lm(cases$TOA~cases$MeanSalary), col="tomato", lwd=3)

# zale¿noœæ pomiêdzy zad³u¿eniem a wiekiem
plot(cases$Age, cases$TOA, pch='.')
abline(lm(cases$TOA~cases$Age), col="tomato", lwd=3)
abline(lm(cases[Age > -1, ]$TOA~cases[Age > -1, ]$Age), col="red", lwd=3, lty=2)
loessCurve <- lowess(cases[Age > -1, ]$Age, cases[Age > -1, ]$TOA, f=1/3)
lines(loessCurve$x, loessCurve$y, col="darkgreen", lwd=2)

# dyskretyzacja (mo¿e u³atwiæ wizualizacjê)   
cases[, 
  TOAband:=cut(TOA, 
    breaks=c(0, 1000, 2000, 4000, 6000, 10000, 20000, 40000, 65000, 100000))]
cases[, .(.N, AvgTOA=mean(TOA)), by=TOAband][order(AvgTOA)]

# w¹sy 1.5*(Q3 - Q1)
plot(cases$TOAband, cases$Age)
# ?boxplot 

# a jak wygl¹da to na poprzednich zmiennych??
plot(cases$TOAband, cases$Principal)
plot(cases$TOAband, cases$DPD)
plot(cases$TOAband, cases$MeanSalary)

# Uwaga: Land to raczej factor (nie ma porz¹dku w tych wartoœciach)
plot(as.factor(cases$Land), cases$TOA)


########################################## zdarzenia maj¹ dodatkowy wymiar czasu
# dla ka¿dej sprawy tabela events ma 12 wierszy z 12 miesiêcy obs³ugi
events[cases][, .N, by=CaseId][N != 12, ]

# NA w przypadku zdarzeñ oznacza, ¿e zdarzenie nie wyst¹pi³o
events[is.na(NumberOfCalls), NumberOfCalls:=0]
events[is.na(NumberOfCallsWithClient), NumberOfCallsWithClient:=0]
events[is.na(NumberOfPayment), NumberOfPayment:=0]
events[is.na(PaymentAmount), PaymentAmount:=0]

tmp <- events[cases][,
  .(NumberOfCases=.N,
    NumberOfCalls=sum(NumberOfCalls),
    SR=sum(PaymentAmount)/sum(TOA)),
  by=.(Month, TOAband)][order(TOAband, Month)]

categories <- sort(unique(cases$TOAband))
colLine <- rainbow(length(categories))

# próby telefoniczne - prezentacja intensywnoœci procesu
# najpierw dotarcie do klienta a potem monitoring ugód
plot(1:12, tmp[TOAband == categories[1], 
  ]$NumberOfCalls, ylim=c(0, 1.05*max(tmp$NumberOfCalls)), 
  type="l", col=colLine[1], lwd= 3,
  main="NumberOfCalls in TOA band", ylab="NumberOfCalls", xlab="MonthOfService")

for (i in 2:length(categories)) {
  lines(1:12, tmp[TOAband == categories[i], ]$NumberOfCalls, col=colLine[i],
  lty=ifelse(i %% 6 == 0, 6, i %% 6), lwd= 3)
}

legend("topright", legend=categories, 
  lty=c(1:6, 1:3), col=colLine, lwd=rep(3, 9))

# g³ówny cel (kasa)
# skutecznoœæ windykacji jest zró¿nicowana wzglêdem salda
# tu tak¿e widaæ efekt ugodowoœci Kruka
plot(1:12, tmp[TOAband == categories[1], 
  ]$SR, ylim=c(0, 1.05*max(tmp$SR)), 
  type="l", col=colLine[1], lwd= 3,
  main="SR in TOA band", ylab="SR", xlab="MonthOfService")

for (i in 2:length(categories)) {
  lines(1:12, tmp[TOAband == categories[i], ]$SR, col=colLine[i],
  lty=ifelse(i %% 6 == 0, 6, i %% 6), lwd= 3)
}

legend("topright", legend=categories, 
  lty=c(1:6, 1:3), col=colLine, lwd=rep(3, 9))

# ¿eby zejœæ na sprawê trzeba ustaliæ moment w czasie
# (tu nas interesuje czy ktoœ zap³aci³ w miesi¹cu) 
tmp <- events[cases][Month <= 6,
  .(NumberOfCallsWithClient=sum(NumberOfCallsWithClient),
    NumberOfPayment=sum(ifelse(NumberOfPayment > 0, 1, 0))),
  by=.(CaseId, TOAband)]

plot(tmp$NumberOfCallsWithClient, tmp$NumberOfPayment)
abline(lm(tmp$NumberOfPayment~tmp$NumberOfCallsWithClient), col="tomato", lwd=3)

# lub okno czasowe
tmp <- events[cases][Month > 3 & Month <= 6,
  .(NumberOfCallsWithClient=sum(NumberOfCallsWithClient),
    NumberOfPayment=sum(ifelse(NumberOfPayment > 0, 1, 0))),
  by=.(CaseId, TOAband)]

plot(tmp$NumberOfCallsWithClient, tmp$NumberOfPayment)
abline(lm(tmp$NumberOfPayment~tmp$NumberOfCallsWithClient), col="tomato", lwd=3)

#!#################################################################### korelacje
# https://pl.wikipedia.org/wiki/Wsp%C3%B3%C5%82czynnik_korelacji_Pearsona
# https://pl.wikipedia.org/wiki/Wsp%C3%B3%C5%82czynnik_korelacji_rang_Spearmana
# https://pl.wikipedia.org/wiki/Tau_Kendalla

##################################### Uwaga: weryfikowana jest zale¿noœæ liniowa
x <- seq(from=-1, to=1, by=0.01)
y1 <- 2*x + rnorm(length(x), sd=0.1)
cor(x, y1)
y2 <- -2*x + rnorm(length(x), sd=0.1)
cor(x, y2)
y3 <- 2*x^2 + rnorm(length(x), sd=0.1)
cor(x, y3)

plot(x, y1, col="darkred", pch=16, main="Czy wszystkie zbiory s¹ skorelowane?")
lines(x, y2, col="darkblue", type="p", pch=16)
lines(x, y3, col="darkgreen", type="p", pch=16)
legend("bottom", legend=c("2x", "-2*x", "2*x^2"), pch=rep(16,3),
  col=c("darkred", "darkblue", "darkgreen"))

# Uwaga: Jakie s¹ ró¿nice pomiêdzy wspó³czynnikami korelacji Pearsona, 
#        Spearmana i Kendalla? Jakie to ma znaczenie?

# liczbowo
# (zwróæcie uwagê na parametr "pairwise.complete.obs")
corMatrix <- cor(cases[, .SD, .SDcols=
  setdiff(names(cases), c("CaseId", "Product", "Gender", "Land", "TOAband"))], 
  use="pairwise.complete.obs", method="spearman") 

# wizualnie
corrgram::corrgram(corMatrix, order=FALSE, 
  lower.panel=corrgram::panel.shade, upper.panel=corrgram::panel.pie, 
  text.panel=corrgram::panel.txt, col.regions=colorRampPalette(
    c("darkgoldenrod4", "burlywood1", "white", "darkkhaki", "darkgreen")), 
  main="Korelacje", cor.method="spearman")
  
corrplot::corrplot(corMatrix, method="ellipse")

###################################################### test istotnoœci korelacji
cor.test(cases$LoanAmount, cases$Principal, 
  method="pearson", alternative="two.sided", conf.level=0.95)
  
cor.test(cases$TOA, cases$Age, 
  method="pearson", alternative="two.sided", conf.level=0.95)

# prymitywna podpróbka
cor.test(cases[1:1000, ]$LoanAmount, cases[1:1000, ]$Principal, 
  method="pearson", alternative="two.sided", conf.level=0.95)
  
cor.test(cases[1:10000, ]$TOA, cases[1:10000, ]$Age, 
  method="pearson", alternative="two.sided", conf.level=0.95)

# dla danych porz¹dkowych lepszym wyborem jest spearman
cor.test(cases$LoanAmount, as.integer(cases$TOAband), 
  method="spearman", alternative="two.sided", conf.level=0.95)

# na laborkê....
#vars <- setdiff(names(casesTmp), 
#  c("CaseId", "Product", "Gender", "Land", "TOAband"))
#
#corrSignificance <- data.table()
#
#for (i in 1:(length(vars) - 1)) {
#  for (j in (i+1):length(vars)) {
#    test <- cor.test(casesTmp[[vars[i]]], casesTmp[[vars[j]]], 
#      method="pearson", alternative="two.sided", conf.level=0.95)
#  
#    corrSignificance <- rbindlist(list(corrSignificance, 
#      data.table(V1=vars[i], V2=vars[j], pValue=test$p.value)))
#  }
#}
#
#corrSignificance[pValue > 0.05, ]

# Uwaga: Globalne zale¿noœci/wnioski mog¹ byæ nieprawdziwe lokalnie i odwrotnie.
# Uwaga: Do tematu wrócimy przy okazji miar asocjacji.

#!############################## co mo¿emy wycisn¹æ z danych - dodatkowe zmienne
# przekszta³cenia funkcyjne
cases[, LogTOA:=log(TOA)]

hist(cases$TOA) # odstaj¹ca
hist(cases$LogTOA) # logarytm to szczegó³ny przypadek transfomacji Box'a-Cox'a
# Uwaga: W zagadnieniach finansowych skoœne rozk³ady nie nale¿¹ do rzadkoœci

# zmienne pochodne - sp³acenie kredytu
# Uwaga: W finansach czêsto u¿ytecznymi s¹ zmienne ilorazowe
cases[Product == "Cash loan", LoanRepayment:=1 - Principal/LoanAmount]

# wartoœci ujemne
plot(sort(cases[Product == "Cash loan", LoanRepayment]))
head(cases[Product == "Cash loan", ][order(LoanRepayment)], 30)

# ustawmy bariery (uwaga: lepiej nie tworzyæ du¿ych atomów)
cases[LoanRepayment < 0, .N]
cases[LoanRepayment < 0, LoanRepayment:=0]

plot(cases$TOAband, cases$LoanRepayment, cex.axis=0.5)

# atom mo¿e zaciemniæ resztê rozk³adu
hist(cases$M_LastPaymentToImportDate)

# pierwszy miesi¹c kontaktu
tmp <- events[cases][NumberOfCallsWithClient > 0,
  .(MinMonth_CWC=min(Month)),
  by=.(CaseId, TOAband)]
  
plot(sort(tmp$MinMonth_CWC))  

# czy wp³ata po telefonie 2M (przyczyna - skutek?)
tmp <- events[NumberOfPayment > 0, .(CaseId, MO=Month)]
setDT(tmp, key="CaseId")

tmp <- events[tmp, allow.cartesian=TRUE][MO - Month <= 2 & Month <= MO, ]   
tmp <- tmp[, .(PaymentAfterCWC=
  ifelse(sum(NumberOfCallsWithClient, na.rm=T) > 0, 1L, 0L)), by=.(CaseId, MO)]

setnames(tmp, names(tmp), c("CaseId", "Month", "PaymentAfterCWC"))
setDT(tmp, key=c("CaseId", "Month"))

events <- tmp[events]

events[, .N, by=PaymentAfterCWC]

# i co Wam jeszcze przyjdzie do g³owy (pod warunkiem, ¿e bêdzie u¿yteczne)

# Uwaga: W dzisiejszych czasach czêsto wzbogaca siê dane o czynniki makro...
# (co czêsto rodzi pytania o sezonowoœæ)