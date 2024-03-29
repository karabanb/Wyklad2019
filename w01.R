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
dTable # domy�lnie je�li tabela ma conajmniej 100 wierszy to wy�wietla 5 pierw. i 5 ost.

args(data.table:::print.data.table) # mo�na zmieni� to zachowanie
getOption("datatable.print.nrows")
options(datatable.print.nrows=50L)
options()$datatable.print.nrows

grep("datatable", names(options()), value=TRUE) # za�adowanie pakietu data.table dodaje te� inne parametry

getDTthreads() # pakiet wspiera wielow�tkowe obliczenia

class(dTable) # data.table dziedziczy z data.frame
# dlatego mo�na u�ywa� data.table wsz�dzie tam, gdzie u�ywa� mo�na data.frame

#!########################################## r�nice i podobie�stwa z data.frame
dTable[5, ] # mo�na wybra� wiersz
dTable[, 2] # kolumn� ju� te� (kiedy� tak nie by�o) 
dTable[[2]] # bardziej lista ni� macierz (efektywniejszy spos�b wg manual)
dim(dTable) # mo�emy sprawdzi� wymiar
names(dTable) # mo�emy sprawdzi� nazwy kolumn

# kiedy� poni�sza zmiana nazw kolumn rzuca�a warningiem, �e jest 
# niefektywne dzia�anie
address(dTable)
names(dTable) <- c("sLength", "sWidth", "pLength", "pWidth", "species") 
  # warninga nie ma 
address(dTable) # ale pod spodem nowa kopia si� utworzy�a
setnames(dTable, names(dTable), c("sLength", "sWidth", "pLength", "pWidth",
  "species")) # warning mia� wsk. jak to zrobi� efektywnie
address(dTable) # i bez kopii pod spodem

# sql: union mo�na w stylu data.frame
dTable <- rbind(dTable, dTable) # z��czenie dw�ch tabel, lub dodanie wierszy
# ale w necie znajdziecie, �e tak jest efektywniej
dTable <- rbindlist(list(dTable, dTable))

# copy-on-modify
df1 <- data.frame(a=1:3, u=runif(3))
df2 <- df1

df1$a <- -df1$a # zmieniamy df1
df2 # zosta�o niezmienione
identical(df1, df2) # co innego

# pass by reference  
dt1 <- data.table(a=1:3, u=runif(3))
dt2 <- dt1

dt1[, a:=-a] # zmieniamy dt1
dt2 # zosta� tak�e zmienione 
identical(dt1, dt2) # to samo

# Uwaga: Ma to swoje konsekwencje w przypadku, gdy piszemy funkcje, 
#        kt�rych argumentami s� struktury data.table

# mo�na wymusi� kopiowanie
dt3 <- copy(dt1) # wykonujemy kopi�
dt1[, a:=-a] # zmieniamy dt1
identical(dt1, dt2) # to samo
identical(dt1, dt3) # ju� nie to samo

# w przypadku data.table to tak�e jest niefektywne
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

dTable[, Id:=.I] # operator .I nr wiersza (przyda si� p�niej klucz)

dTable[, X:=NULL] # usuni�cie kolumny

dTable[, Y:=2*Y]

#!################################# eksploracja danych w tabeli (troch� jak sql)
dTable[Cube == 6 & species == "setosa", ] # "przed przecinkiem where"

dTable[, .N] # liczba wierszy/obserwacji (count)
dTable[, .N, by=Coin] # liczba obserwacji w podgrupach (group by)
dTable[, .N,
  by=list(species, Cube)][order(species, Cube), ] # wi�cej kategorii + sort
# w przypadku data.table mamy to�samo�� list() i .()
dTable[, list(.N,
  SumCube=sum(Cube),
  AVGsLength=mean(sLength)), by=Coin] # wi�cej p�l obliczeniowych (plus aliasy)
dTable[, .(.N,
  SumCube=sum(Cube),
  AVGsLength=mean(sLength)), by=Coin] # to samo (trzy znaki mniej do napisania)

dTable[, .SD, .SDcols=c("species", "Id")] # wyb�r wskazanych kolumn
dTable[, head(.SD, 30),
  .SDcols=c("Id", "species", "sLength", "sWidth")] # .SD mo�na poda� do funkcji

#!############################################################# joinowanie tabel
dTable2 <- copy(dTable)

setkey(dTable, Id) # ustawienie klucza na kolumnie Id
setDT(dTable2, key=c("Id")) # ustawianie klucza na wielu kolumnach

tables() # u�yteczna funkcja do �ledzenia tabel w pami�ci (na czym KEY)

dTable[dTable2] # domy�lnie join po kolejnych kolumnach klucza
# zwr��cie uwag� na kolumny i.xxx dla zdublowanych nazw kolumn

dTable[dTable2, on=.(Cube, Coin)] # lub wskazuj�c konkretne kolumny(ERROR)
# uwaga na joiny wzgl�dem kluczy w relacji wiele do wielu
dTable[dTable2, on=.(Cube, Coin), allow.cartesian=TRUE] # domy�lne ograniczenie
# �eby przypadkiem nie wysadzi� Ramu w powietrze

dTableTmp <- dTable[1:3, ] # mniejsze tabelki
dTableTmp[1, Coin:="K"] # zmiana warto�ci w pierwszym wierszu
dTableTmp2 <- dTable2[1:3, ] # mniejsze tabelki

dTableTmp2[dTableTmp, on=.(Coin)] # left join
dTableTmp2[dTableTmp, on=.(Coin), nomatch=0] # inner join

# dodatkowo joinowa� mo�na na podstawie nier�no�ci
# zach�cam do przejrzenia dokumentacji
#                    dTable     dTable2  dTable     dTable2
dTable[dTable2, on=.(pLength <= sLength, sLength <= pLength),
  nomatch=0, allow.cartesian=TRUE]

# pakiet data.table ma te� dwie bardzo u�yteczne funkcje fread i fwrite
# do wczytywania i zapisywania plik�w csv (wielow�tkowo - co ma znaczenie przy 
# wi�kszych plikach maj�cych paredziesi�t GB)

#!################################################################ zbiory danych
gcrm() # rm(list=ls())
load("KrukUWr2019.RData")
tables() # tabele ��cz� si� kluczem CaseId

# cases - tabela spraw; zmienne opisuj�ce spraw� - jej cechy z "dnia zakupu"

# TOA - Total Outstanding Amount; pocz�tkowe zad�u�enie
# Principal - kapita�
# Interest - odsetki
# Other - inne
# TOA = Principal + Interest + Other # powinno zachodzi�
cases[, .(.N, AvgTOA=mean(TOA))]

# Product - typ produktu
cases[, .(.N, AvgTOA=mean(TOA)), by=Product]

# LoanAmount - warto�� po�yczki; Co prezentuje ta zmienna dla kart kredytowych?
cases[, .(.N,
  AvgLA=mean(LoanAmount, na.rm=T),
  NA_N=sum(is.na(LoanAmount))), by=Product] # limit na karcie

# D_ContractDateToImportDate - dni od otwarcia/udzielenia po�yczki
# DPD - Day Past Due; dni od najstarszej zaleg�ej p�atno�ci
# ExternalAgency - czy na sprawie by�a zlecana windykacja przed sprzeda��
cases[, .N, by=ExternalAgency] # NA - nie wiadomo

# Bailiff - czy sprawa by�a przekazana do komornika
# ClosedExecution - czy na sprawie by�a umorzona egzekucja
cases[, .N, by=.(Bailiff, ClosedExecution)][
  order(Bailiff, ClosedExecution)]

# Age - wiek (w chwili zakupu sprawy)
# Gender - p�e�
cases[, .N, by=Age][order(Age)] # zwr��cie uwag� na warto�� -1
cases[Age == -1, .N, by=Gender] # takie sprawy nie maj� te� okre�lone p�ci

# LastPaymentAmount - warto�� ostatniej wp�aty przed cesj�/zakupem
# M_LastPaymentToImportDate - liczba miesi�cy od ostatniej wp�aty do cesji (40M)
cases[, .(.N,
  SumP=sum(LastPaymentAmount, na.rm=T),
  AvgP=mean(LastPaymentAmount, na.rm=T)),
  by=M_LastPaymentToImportDate][order(M_LastPaymentToImportDate)]
  # NA - nie ma informacji o p�atno�ciach przed cesj�
  # 50 - wp�ata by�a dalej ni� 36M (nie by�o wp�aty w 36M)
  # Uwaga: Usuwamy NA z warto�ci wp�at, bo wiemy, �e by�a wp�ata,
  #        ale nie zawsze znamy jej warto��

# Land - podzia� terytorialny (np. na wojew�dztwa)
# GDPPerCapita - PKB na osob� w Land
# MeanSalary - �rednia p�aca w Land
cases[, .(.N, MaxMS=max(MeanSalary), MaxGDP=max(GDPPerCapita)),
  by=Land][order(Land)] # nr Land trzeba traktowa� jako factor

# PopulationInCity - liczba mieszka�c�w w miejscowo�ci zamieszkania d�u�nika
#                    (na jakiej� skali)
cases[, .(
  MinPop=min(PopulationInCity, na.rm=T),
  AvgPop=mean(PopulationInCity, na.rm=T),
  MaxPop=max(PopulationInCity, na.rm=T)), by=Land][order(Land)]

# events - tabela zdarze�; zmienne opisuj�ce proces oraz jego wyniki
#          w kolejnych miesi�cach obs�ugi 1M - 12M

# Month - miesi�c obs�ugi
# NumberOfCalls - liczba wykonanych telefon�w w sprawie
# NumberOfCallsWithClient - liczba po��cze� z klientem/d�u�nikiem
# NumberOfLettersSent - liczba wys�anych list�w
# NumberOfVisits - liczba wizyt
# NumberOfLettersReceived - liczba list�w otrzymanych
# NumberOfAgreementConcluded - liczba ug�d wystawionych
# NumberOfAgreementSigned - liczba ug�d podpisanych
# TransferToLegalProcess - przekazanie do s�du
# NumberOfPayment - liczba wp�at
# PaymentAmount - warto�� wp�at

# Uwaga: Warto�� NA nie znaczy, �e nie mamy informacji czy zdarzenie wyst�pi�o,
# znaczy, �e zdarzenie nie wyst�pi�o.

#!################################################################## ciekawostki
x # nie ma x
eval(parse(text="x <- 6.9")) # wykonanie komendy zapisanej w stringu
x # jest x
chV = "x" # nazwa kolumny
get(chV) # dobranie si� do warto�ci kryj�cej si� w zmiennej o danej nazwie

library(Rcpp) # kolejny spos�b na przyspieszenie oblicze� (potrzebne RTools3.5)
# http://adv-r.had.co.nz/Rcpp.html
# http://dirk.eddelbuettel.com/code/rcpp/Rcpp-quickref.pdf
cppFunction('int add(int x, int y, int z) {
  int sum = x + y + z;
  return sum;
}')

add(1, 2, 3)
