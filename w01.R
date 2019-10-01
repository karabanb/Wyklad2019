gcrm() # rm(list=ls())
gcHi(1) # Hello World!
   
#!################################################################### data.table
# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html
# https://www.r-bloggers.com/intro-to-the-data-table-package/
# http://datacamp-community.s3.amazonaws.com/6fdf799f-76ba-45b1-b8d8-39c4d4211c31
# https://s3.amazonaws.com/assets.datacamp.com/img/blog/data+table+cheat+sheet.pdf
# http://datatable.r-forge.r-project.org/datatable-intro.pdf
# https://github.com/Rdatatable/data.table/wiki

library(data.table) # "efektywna" struktura danych
?data.table

dTable <- data.table(iris) # as.data.table() 
dTable # domyœlnie jeœli tabela ma conajmniej 100 wierszy to wyœwietla 5 pierw. i 5 ost.

args(data.table:::print.data.table) # mo¿na zmieniæ to zachowanie
getOption("datatable.print.nrows")
options(datatable.print.nrows=50L)
options()$datatable.print.nrows

grep("datatable", names(options()), value=TRUE) # za³adowanie pakietu data.table dodaje te¿ inne parametry

getDTthreads() # pakiet wspiera wielow¹tkowe obliczenia

class(dTable) # data.table dziedziczy z data.frame
# dlatego mo¿na u¿ywaæ data.table wszêdzie tam, gdzie u¿ywaæ mo¿na data.frame

#!########################################## ró¿nice i podobieñstwa z data.frame
dTable[5, ] # mo¿na wybraæ wiersz
dTable[, 2] # kolumnê ju¿ te¿ (kiedyœ tak nie by³o) 
dTable[[2]] # bardziej lista ni¿ macierz (efektywniejszy sposób wg manual)
dim(dTable) # mo¿emy sprawdziæ wymiar
names(dTable) # mo¿emy sprawdziæ nazwy kolumn

# kiedyœ poni¿sza zmiana nazw kolumn rzuca³a warningiem, ¿e jest 
# niefektywne dzia³anie
address(dTable)
names(dTable) <- c("sLength", "sWidth", "pLength", "pWidth", "species") 
  # warninga nie ma 
address(dTable) # ale pod spodem nowa kopia siê utworzy³a
setnames(dTable, names(dTable), c("sLength", "sWidth", "pLength", "pWidth",
  "species")) # warning mia³ wsk. jak to zrobiæ efektywnie
address(dTable) # i bez kopii pod spodem

# sql: union mo¿na w stylu data.frame
dTable <- rbind(dTable, dTable) # z³¹czenie dwóch tabel, lub dodanie wierszy
# ale w necie znajdziecie, ¿e tak jest efektywniej
dTable <- rbindlist(list(dTable, dTable))

# copy-on-modify
df1 <- data.frame(a=1:3, u=runif(3))
df2 <- df1

df1$a <- -df1$a # zmieniamy df1
df2 # zosta³o niezmienione
identical(df1, df2) # co innego

# pass by reference  
dt1 <- data.table(a=1:3, u=runif(3))
dt2 <- dt1

dt1[, a:=-a] # zmieniamy dt1
dt2 # zosta³ tak¿e zmienione 
identical(dt1, dt2) # to samo

# Uwaga: Ma to swoje konsekwencje w przypadku, gdy piszemy funkcje, 
#        których argumentami s¹ struktury data.table

# mo¿na wymusiæ kopiowanie
dt3 <- copy(dt1) # wykonujemy kopiê
dt1[, a:=-a] # zmieniamy dt1
identical(dt1, dt2) # to samo
identical(dt1, dt3) # ju¿ nie to samo

# w przypadku data.table to tak¿e jest niefektywne
address(dt1)
dt1$a <- -dt1$a # kopia pod spodem
address(dt1)

#!#################################################### dodawanie/usuwanie kolumn
dTable[, X:=sample(c("G", "C", "H"), dTable[, .N], replace=TRUE)] # znakowa
dTable[, Y:=rnorm(dTable[, .N])] # liczbowa
dTable[, `:=`(
  Coin=sample(c("O" ,"R"), dTable[, .N], replace=TRUE),
  Cube=sample(1:6, dTable[, .N], replace=TRUE))] # dodanie kilku kolumn 
dTable[, W:=Y*sLength] # dodanie kolumny wyliczeniowej

dTable[, Id:=.I] # operator .I nr wiersza (przyda siê póŸniej klucz)

dTable[, X:=NULL] # usuniêcie kolumny

dTable[, Y:=2*Y]

#!################################# eksploracja danych w tabeli (trochê jak sql)
dTable[Cube == 6 & species == "setosa", ] # "przed przecinkiem where"

dTable[, .N] # liczba wierszy/obserwacji (count)
dTable[, .N, by=Coin] # liczba obserwacji w podgrupach (group by)
dTable[, .N,
  by=list(species, Cube)][order(species, Cube), ] # wiêcej kategorii + sort
# w przypadku data.table mamy to¿samoœæ list() i .()
dTable[, list(.N,
  SumCube=sum(Cube),
  AVGsLength=mean(sLength)), by=Coin] # wiêcej pól obliczeniowych (plus aliasy)
dTable[, .(.N,
  SumCube=sum(Cube),
  AVGsLength=mean(sLength)), by=Coin] # to samo (trzy znaki mniej do napisania)

dTable[, .SD, .SDcols=c("species", "Id")] # wybór wskazanych kolumn
dTable[, head(.SD, 30),
  .SDcols=c("Id", "species", "sLength", "sWidth")] # .SD mo¿na podaæ do funkcji

#!############################################################# joinowanie tabel
dTable2 <- copy(dTable)

setkey(dTable, Id) # ustawienie klucza na kolumnie Id
setDT(dTable2, key=c("Id")) # ustawianie klucza na wielu kolumnach

tables() # u¿yteczna funkcja do œledzenia tabel w pamiêci (na czym KEY)

dTable[dTable2] # domyœlnie join po kolejnych kolumnach klucza
# zwróæcie uwagê na kolumny i.xxx dla zdublowanych nazw kolumn

dTable[dTable2, on=.(Cube, Coin)] # lub wskazuj¹c konkretne kolumny(ERROR)
# uwaga na joiny wzglêdem kluczy w relacji wiele do wielu
dTable[dTable2, on=.(Cube, Coin), allow.cartesian=TRUE] # domyœlne ograniczenie
# ¿eby przypadkiem nie wysadziæ Ramu w powietrze

dTableTmp <- dTable[1:3, ] # mniejsze tabelki
dTableTmp[1, Coin:="K"] # zmiana wartoœci w pierwszym wierszu
dTableTmp2 <- dTable2[1:3, ] # mniejsze tabelki

dTableTmp2[dTableTmp, on=.(Coin)] # left join
dTableTmp2[dTableTmp, on=.(Coin), nomatch=0] # inner join

# dodatkowo joinowaæ mo¿na na podstawie nierónoœci
# zachêcam do przejrzenia dokumentacji
#                    dTable     dTable2  dTable     dTable2
dTable[dTable2, on=.(pLength <= sLength, sLength <= pLength),
  nomatch=0, allow.cartesian=TRUE]

# pakiet data.table ma te¿ dwie bardzo u¿yteczne funkcje fread i fwrite
# do wczytywania i zapisywania plików csv (wielow¹tkowo - co ma znaczenie przy 
# wiêkszych plikach maj¹cych paredziesi¹t GB)

#!################################################################ zbiory danych
gcrm() # rm(list=ls())
load("KrukUWr2019.RData")
tables() # tabele ³¹cz¹ siê kluczem CaseId

# cases - tabela spraw; zmienne opisuj¹ce sprawê - jej cechy z "dnia zakupu"

# TOA - Total Outstanding Amount; pocz¹tkowe zad³u¿enie
# Principal - kapita³
# Interest - odsetki
# Other - inne
# TOA = Principal + Interest + Other # powinno zachodziæ
cases[, .(.N, AvgTOA=mean(TOA))]

# Product - typ produktu
cases[, .(.N, AvgTOA=mean(TOA)), by=Product]

# LoanAmount - wartoœæ po¿yczki; Co prezentuje ta zmienna dla kart kredytowych?
cases[, .(.N,
  AvgLA=mean(LoanAmount, na.rm=T),
  NA_N=sum(is.na(LoanAmount))), by=Product] # limit na karcie

# D_ContractDateToImportDate - dni od otwarcia/udzielenia po¿yczki
# DPD - Day Past Due; dni od najstarszej zaleg³ej p³atnoœci
# ExternalAgency - czy na sprawie by³a zlecana windykacja przed sprzeda¿¹
cases[, .N, by=ExternalAgency] # NA - nie wiadomo

# Bailiff - czy sprawa by³a przekazana do komornika
# ClosedExecution - czy na sprawie by³a umorzona egzekucja
cases[, .N, by=.(Bailiff, ClosedExecution)][
  order(Bailiff, ClosedExecution)]

# Age - wiek (w chwili zakupu sprawy)
# Gender - p³eæ
cases[, .N, by=Age][order(Age)] # zwróæcie uwagê na wartoœæ -1
cases[Age == -1, .N, by=Gender] # takie sprawy nie maj¹ te¿ okreœlone p³ci

# LastPaymentAmount - wartoœæ ostatniej wp³aty przed cesj¹/zakupem
# M_LastPaymentToImportDate - liczba miesiêcy od ostatniej wp³aty do cesji (40M)
cases[, .(.N,
  SumP=sum(LastPaymentAmount, na.rm=T),
  AvgP=mean(LastPaymentAmount, na.rm=T)),
  by=M_LastPaymentToImportDate][order(M_LastPaymentToImportDate)]
  # NA - nie ma informacji o p³atnoœciach przed cesj¹
  # 50 - wp³ata by³a dalej ni¿ 36M (nie by³o wp³aty w 36M)
  # Uwaga: Usuwamy NA z wartoœci wp³at, bo wiemy, ¿e by³a wp³ata,
  #        ale nie zawsze znamy jej wartoœæ

# Land - podzia³ terytorialny (np. na województwa)
# GDPPerCapita - PKB na osobê w Land
# MeanSalary - œrednia p³aca w Land
cases[, .(.N, MaxMS=max(MeanSalary), MaxGDP=max(GDPPerCapita)),
  by=Land][order(Land)] # nr Land trzeba traktowaæ jako factor

# PopulationInCity - liczba mieszkañców w miejscowoœci zamieszkania d³u¿nika
#                    (na jakiejœ skali)
cases[, .(
  MinPop=min(PopulationInCity, na.rm=T),
  AvgPop=mean(PopulationInCity, na.rm=T),
  MaxPop=max(PopulationInCity, na.rm=T)), by=Land][order(Land)]

# events - tabela zdarzeñ; zmienne opisuj¹ce proces oraz jego wyniki
#          w kolejnych miesi¹cach obs³ugi 1M - 12M

# Month - miesi¹c obs³ugi
# NumberOfCalls - liczba wykonanych telefonów w sprawie
# NumberOfCallsWithClient - liczba po³¹czeñ z klientem/d³u¿nikiem
# NumberOfLettersSent - liczba wys³anych listów
# NumberOfVisits - liczba wizyt
# NumberOfLettersReceived - liczba listów otrzymanych
# NumberOfAgreementConcluded - liczba ugód wystawionych
# NumberOfAgreementSigned - liczba ugód podpisanych
# TransferToLegalProcess - przekazanie do s¹du
# NumberOfPayment - liczba wp³at
# PaymentAmount - wartoœæ wp³at

# Uwaga: Wartoœæ NA nie znaczy, ¿e nie mamy informacji czy zdarzenie wyst¹pi³o,
# znaczy, ¿e zdarzenie nie wyst¹pi³o.

#!################################################################## ciekawostki
x # nie ma x
eval(parse(text="x <- 6.9")) # wykonanie komendy zapisanej w stringu
x # jest x
chV = "x" # nazwa kolumny
get(chV) # dobranie siê do wartoœci kryj¹cej siê w zmiennej o danej nazwie

library(Rcpp) # kolejny sposób na przyspieszenie obliczeñ (potrzebne RTools3.5)
# http://adv-r.had.co.nz/Rcpp.html
# http://dirk.eddelbuettel.com/code/rcpp/Rcpp-quickref.pdf
cppFunction('int add(int x, int y, int z) {
  int sum = x + y + z;
  return sum;
}')

add(1, 2, 3)
