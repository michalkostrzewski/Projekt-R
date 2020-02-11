library(plotly)
library(dplyr)
library(janitor)
#### Œrednia i minimalna pensja####
# Plik wziêty z bazy danych GUS
pensja_df = read.csv('pensja.csv',header = TRUE,sep = ";",encoding = "UTF-8",stringsAsFactors=FALSE)
pensja_df = subset(pensja_df, select = -c(Kod,X,Nazwa))
pensja_df = t(pensja_df)

pensja_df = unname(pensja_df)
pensja_df = as.data.frame(pensja_df) #Jest bez 2019

# Miesiêczna pensja w województwach
DOLNO = as.numeric(gsub(",", ".", as.character(pensja_df$V1)))
KUJAW = as.numeric(gsub(",", ".", as.character(pensja_df$V2)))
LUBEL = as.numeric(gsub(",", ".", as.character(pensja_df$V3)))
LUBUS = as.numeric(gsub(",", ".", as.character(pensja_df$V4)))
LODZK = as.numeric(gsub(",", ".", as.character(pensja_df$V5)))
MALOP = as.numeric(gsub(",", ".", as.character(pensja_df$V6)))
MAZOW = as.numeric(gsub(",", ".", as.character(pensja_df$V7)))
OPOLS = as.numeric(gsub(",", ".", as.character(pensja_df$V8)))
PODKA = as.numeric(gsub(",", ".", as.character(pensja_df$V9)))
PODLA = as.numeric(gsub(",", ".", as.character(pensja_df$V10)))
POMOR = as.numeric(gsub(",", ".", as.character(pensja_df$V11)))
SLASK = as.numeric(gsub(",", ".", as.character(pensja_df$V12)))
SWIET = as.numeric(gsub(",", ".", as.character(pensja_df$V13)))
WARMI = as.numeric(gsub(",", ".", as.character(pensja_df$V14)))
WIELK = as.numeric(gsub(",", ".", as.character(pensja_df$V15)))
ZACHO = as.numeric(gsub(",", ".", as.character(pensja_df$V16)))


#Wykres
srednie_pensja_plot = plot_ly(x = 1:13) %>%
         layout(title = 'Wzrost œredniej pensji w latach 2006-2019', xaxis = list(title = 'Rok', 
         ticktext = list("2006", "2007", "2008", "2009", "2010", "2011","2012","2013","2014","2015","2016","2017","2018"), 
         tickvals = list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)),
         yaxis = list(title = 'Œrednia pensja (z³)')) %>%
  add_lines(y = DOLNO, name = "Dolnoœl¹skie")%>%
  add_lines(y = KUJAW, name = "Kujawsko-pomorskie")%>%
  add_lines(y = LUBEL, name = "Lubelskie")%>%
  add_lines(y = LUBUS, name = "Lubuskie")%>%
  add_lines(y = LODZK, name = "£ódzkie")%>%
  add_lines(y = MALOP, name = "Ma³opolskie")%>%
  add_lines(y = MAZOW, name = "Mazowieckie")%>%
  add_lines(y = OPOLS, name = "Opolskiea")%>%
  add_lines(y = PODKA, name = "Podkarpackie")%>%
  add_lines(y = PODLA, name = "Podlaskie")%>%
  add_lines(y = POMOR, name = "pomorskie")%>%
  add_lines(y = SLASK, name = "Œl¹skie")%>%
  add_lines(y = SWIET, name = "œwiêtokrzyskie")%>%
  add_lines(y = WARMI, name = "Warmiñsko-mazurskie")%>%
  add_lines(y = WIELK, name = "Wielkopolskie")%>%
  add_lines(y = ZACHO, name = "Zachodniopomorskie")

### Minimalna pensja 2006-2019 ###

placa_min = c(899,936,1126,1276,1317,1386,1500,1600,1680,1750,1850,2000,2100,2250)
placa_min_df = data.frame('','','','','','','','','','','','','', stringsAsFactors = FALSE)
names(placa_min_df) = c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016",
                        "2017","2018")

#Wykres
o = 1:14
placa_min_plot = plot_ly(x = o) %>%
  layout(title = 'Wzrost p³acy minimalnej brutto w latach 2006-2019', xaxis = list(title = 'Rok', 
  ticktext = list("2006", "2007", "2008", "2009", "2010", "2011","2012","2013","2014","2015","2016","2017","2018"), 
  tickvals = list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)),
  yaxis = list(title = 'Kwota brutto (z³)')) %>%
  add_lines(y = placa_min, name = "P³aca minimalna")
# Uzupe³niam tabelê wartoœciami p³acy minimalnej
for (i in 1:13) {
  placa_min_df[1,i] = as.numeric(placa_min[i])
  placa_min_df[2,i] = as.numeric(placa_min[i])
  placa_min_df[3,i] = as.numeric(placa_min[i])
  placa_min_df[4,i] = as.numeric(placa_min[i])
  placa_min_df[5,i] = as.numeric(placa_min[i])
  placa_min_df[6,i] = as.numeric(placa_min[i])
  placa_min_df[7,i] = as.numeric(placa_min[i])
  placa_min_df[8,i] = as.numeric(placa_min[i])
  placa_min_df[9,i] = as.numeric(placa_min[i])
  placa_min_df[10,i] = as.numeric(placa_min[i])
  placa_min_df[11,i] = as.numeric(placa_min[i])
  placa_min_df[12,i] = as.numeric(placa_min[i])
  placa_min_df[13,i] = as.numeric(placa_min[i])
  placa_min_df[14,i] = as.numeric(placa_min[i])
  placa_min_df[15,i] = as.numeric(placa_min[i])
  placa_min_df[16,i] = as.numeric(placa_min[i])
}

placa_min_df = t(placa_min_df)
placa_min_df = unname(placa_min_df)
placa_min_df = as.data.frame(placa_min_df)

### Minimalna pensja 2006-2019 ###

placa_min2012_2019 = c(1500,1600,1680,1750,1850,2000,2100,2250)
placa_min2012_2019_df = data.frame('','','','','','','','', stringsAsFactors = FALSE)
names(placa_min2012_2019_df) = c("2012","2013","2014","2015","2016","2017","2018")

# Uzupe³niam tabelê wartoœciami p³acy minimalnej
for (i in 1:8) {
  placa_min2012_2019_df[1,i] = as.numeric(placa_min2012_2019[i])
  placa_min2012_2019_df[2,i] = as.numeric(placa_min2012_2019[i])
  placa_min2012_2019_df[3,i] = as.numeric(placa_min2012_2019[i])
  placa_min2012_2019_df[4,i] = as.numeric(placa_min2012_2019[i])
  placa_min2012_2019_df[5,i] = as.numeric(placa_min2012_2019[i])
  placa_min2012_2019_df[6,i] = as.numeric(placa_min2012_2019[i])
  placa_min2012_2019_df[7,i] = as.numeric(placa_min2012_2019[i])
  placa_min2012_2019_df[8,i] = as.numeric(placa_min2012_2019[i])
  placa_min2012_2019_df[9,i] = as.numeric(placa_min2012_2019[i])
  placa_min2012_2019_df[10,i] = as.numeric(placa_min2012_2019[i])
  placa_min2012_2019_df[11,i] = as.numeric(placa_min2012_2019[i])
  placa_min2012_2019_df[12,i] = as.numeric(placa_min2012_2019[i])
  placa_min2012_2019_df[13,i] = as.numeric(placa_min2012_2019[i])
  placa_min2012_2019_df[14,i] = as.numeric(placa_min2012_2019[i])
  placa_min2012_2019_df[15,i] = as.numeric(placa_min2012_2019[i])
  placa_min2012_2019_df[16,i] = as.numeric(placa_min2012_2019[i])
}

placa_min2012_2019_df = t(placa_min2012_2019_df)
placa_min2012_2019_df = unname(placa_min2012_2019_df)
placa_min2012_2019_df = as.data.frame(placa_min2012_2019_df)

#### Województwe i daty ####
wojewodztwa = c("dolnoœlaskim","kujawsko-pomorskim","lubelskim","lubuskim","³ódzkim",
                "ma³opolskim","mazowieckim","opolskim","podkarpackim","podlaskim",
                "pomorskim","œlaskim","œwietokrzyskim","warminsko-mazurskim",
                "wielkopolskim","zachodniopomorskim")

daty = seq(as.Date("2006/1/1"), by = "month", length.out = 168, format = "%Y %B")
daty = format(daty, "%B %Y")

#### Wczytywanie plików ####
ryz_df = read.csv('(1) Ry¿.csv',header = TRUE,sep = ";",encoding = "UTF-8",stringsAsFactors=FALSE)
kurczak_df = read.csv('(9) Kurczêta patroszone.csv',header = TRUE,sep = ";",encoding = "UTF-8",stringsAsFactors=FALSE)
kielbasa_wedzona_df = read.csv('(12) Kie³basa wêdzona.csv',header = TRUE,sep = ";",encoding = "UTF-8",stringsAsFactors=FALSE)
kielbasa_wedzona_df_2 = read.csv('(12) Kie³basa wêdzone 2.csv',header = TRUE,sep = ";",encoding = "UTF-8",stringsAsFactors=FALSE)
smietana_df = read.csv('(19) Œmietana 18% - 200g.csv',header = TRUE,sep = ";",encoding = "UTF-8",stringsAsFactors=FALSE)
maslo_df = read.csv('(22) Mas³o - 200g.csv',header = TRUE,sep = ";",encoding = "UTF-8",stringsAsFactors=FALSE)
czekolada_df = read.csv('(32) Czekolada - 100g.csv',header = TRUE,sep = ";",encoding = "UTF-8",stringsAsFactors=FALSE)
spodnie_df = read.csv('(42) Spodnie jeans 06-17.csv',header = TRUE,sep = ";",encoding = "UTF-8",stringsAsFactors=FALSE)
spodnie_df2 = read.csv('(42) Spodnie jeans 18-19.csv',header = TRUE,sep = ";",encoding = "UTF-8",stringsAsFactors=FALSE)
buty_df = read.csv('(45) Pó³buty damskie.csv',header = TRUE,sep = ";",encoding = "UTF-8",stringsAsFactors=FALSE)
zlewozmywak_df = read.csv('(47) Zlewozmywak.csv',header = TRUE,sep = ";",encoding = "UTF-8",stringsAsFactors=FALSE)
lekarz_df = read.csv('(54) Lekarz.csv',header = TRUE,sep = ";",encoding = "UTF-8",stringsAsFactors=FALSE)




#### Ry¿ edycja nag³ówków i tabel ####
ryz_df = subset(ryz_df, select = -c(Kod,X)) #Usuwam zbêdn¹ kolumnê Kod i X

#Edtujê nazwy kolumn na sensowne
names(ryz_df) <- gsub("styczeñ.ry¿...za.1kg.cena.", "styczeñ.", names(ryz_df))
names(ryz_df) <- gsub("luty.ry¿...za.1kg.cena.", "luty.", names(ryz_df))
names(ryz_df) <- gsub("marzec.ry¿...za.1kg.cena.", "marzec.", names(ryz_df))
names(ryz_df) <- gsub("kwiecieñ.ry¿...za.1kg.cena.", "kwiecieñ.", names(ryz_df))
names(ryz_df) <- gsub("maj.ry¿...za.1kg.cena.", "maj.", names(ryz_df))
names(ryz_df) <- gsub("czerwiec.ry¿...za.1kg.cena.", "czerwiec.", names(ryz_df))
names(ryz_df) <- gsub("lipiec.ry¿...za.1kg.cena.", "lipiec.", names(ryz_df))
names(ryz_df) <- gsub("sierpieñ.ry¿...za.1kg.cena.", "sierpieñ.", names(ryz_df))
names(ryz_df) <- gsub("wrzesieñ.ry¿...za.1kg.cena.", "wrzesieñ.", names(ryz_df))
names(ryz_df) <- gsub("paŸdziernik.ry¿...za.1kg.cena.", "paŸdziernik.", names(ryz_df))
names(ryz_df) <- gsub("listopad.ry¿...za.1kg.cena.", "listopad.", names(ryz_df))
names(ryz_df) <- gsub("grudzieñ.ry¿...za.1kg.cena.", "grudzieñ.", names(ryz_df))
names(ryz_df) <- gsub("..z³.", "", names(ryz_df))

#Nie znalaz³em inteligentnego sposobu na sensowne posortowanie chronologiczne wiêc robiê to ³opatologicznie
names(ryz_df) <- gsub("styczeñ.2006", "2006 1 styczeñ", names(ryz_df))
names(ryz_df) <- gsub("styczeñ.2007", "2007 1 styczeñ", names(ryz_df))
names(ryz_df) <- gsub("styczeñ.2008", "2008 1 styczeñ", names(ryz_df))
names(ryz_df) <- gsub("styczeñ.2009", "2009 1 styczeñ", names(ryz_df))
names(ryz_df) <- gsub("styczeñ.2010", "2010 1 styczeñ", names(ryz_df))
names(ryz_df) <- gsub("styczeñ.2011", "2011 1 styczeñ", names(ryz_df))
names(ryz_df) <- gsub("styczeñ.2012", "2012 1 styczeñ", names(ryz_df))
names(ryz_df) <- gsub("styczeñ.2013", "2013 1 styczeñ", names(ryz_df))
names(ryz_df) <- gsub("styczeñ.2014", "2014 1 styczeñ", names(ryz_df))
names(ryz_df) <- gsub("styczeñ.2015", "2015 1 styczeñ", names(ryz_df))
names(ryz_df) <- gsub("styczeñ.2016", "2016 1 styczeñ", names(ryz_df))
names(ryz_df) <- gsub("styczeñ.2017", "2017 1 styczeñ", names(ryz_df))
names(ryz_df) <- gsub("styczeñ.2018", "2018 1 styczeñ", names(ryz_df))
names(ryz_df) <- gsub("styczeñ.2019", "2019 1 styczeñ", names(ryz_df))
names(ryz_df) <- gsub("luty.2006", "2006 2 luty", names(ryz_df))
names(ryz_df) <- gsub("luty.2007", "2007 2 luty", names(ryz_df))
names(ryz_df) <- gsub("luty.2008", "2008 2 luty", names(ryz_df))
names(ryz_df) <- gsub("luty.2009", "2009 2 luty", names(ryz_df))
names(ryz_df) <- gsub("luty.2010", "2010 2 luty", names(ryz_df))
names(ryz_df) <- gsub("luty.2011", "2011 2 luty", names(ryz_df))
names(ryz_df) <- gsub("luty.2012", "2012 2 luty", names(ryz_df))
names(ryz_df) <- gsub("luty.2013", "2013 2 luty", names(ryz_df))
names(ryz_df) <- gsub("luty.2014", "2014 2 luty", names(ryz_df))
names(ryz_df) <- gsub("luty.2015", "2015 2 luty", names(ryz_df))
names(ryz_df) <- gsub("luty.2016", "2016 2 luty", names(ryz_df))
names(ryz_df) <- gsub("luty.2017", "2017 2 luty", names(ryz_df))
names(ryz_df) <- gsub("luty.2018", "2018 2 luty", names(ryz_df))
names(ryz_df) <- gsub("luty.2019", "2019 2 luty", names(ryz_df))
names(ryz_df) <- gsub("marzec.2006", "2006 3 marzec", names(ryz_df))
names(ryz_df) <- gsub("marzec.2007", "2007 3 marzec", names(ryz_df))
names(ryz_df) <- gsub("marzec.2008", "2008 3 marzec", names(ryz_df))
names(ryz_df) <- gsub("marzec.2009", "2009 3 marzec", names(ryz_df))
names(ryz_df) <- gsub("marzec.2010", "2010 3 marzec", names(ryz_df))
names(ryz_df) <- gsub("marzec.2011", "2011 3 marzec", names(ryz_df))
names(ryz_df) <- gsub("marzec.2012", "2012 3 marzec", names(ryz_df))
names(ryz_df) <- gsub("marzec.2013", "2013 3 marzec", names(ryz_df))
names(ryz_df) <- gsub("marzec.2014", "2014 3 marzec", names(ryz_df))
names(ryz_df) <- gsub("marzec.2015", "2015 3 marzec", names(ryz_df))
names(ryz_df) <- gsub("marzec.2016", "2016 3 marzec", names(ryz_df))
names(ryz_df) <- gsub("marzec.2017", "2017 3 marzec", names(ryz_df))
names(ryz_df) <- gsub("marzec.2018", "2018 3 marzec", names(ryz_df))
names(ryz_df) <- gsub("marzec.2019", "2019 3 marzec", names(ryz_df))
names(ryz_df) <- gsub("kwiecieñ.2006", "2006 4 kwiecieñ", names(ryz_df))
names(ryz_df) <- gsub("kwiecieñ.2007", "2007 4 kwiecieñ", names(ryz_df))
names(ryz_df) <- gsub("kwiecieñ.2008", "2008 4 kwiecieñ", names(ryz_df))
names(ryz_df) <- gsub("kwiecieñ.2009", "2009 4 kwiecieñ", names(ryz_df))
names(ryz_df) <- gsub("kwiecieñ.2010", "2010 4 kwiecieñ", names(ryz_df))
names(ryz_df) <- gsub("kwiecieñ.2011", "2011 4 kwiecieñ", names(ryz_df))
names(ryz_df) <- gsub("kwiecieñ.2012", "2012 4 kwiecieñ", names(ryz_df))
names(ryz_df) <- gsub("kwiecieñ.2013", "2013 4 kwiecieñ", names(ryz_df))
names(ryz_df) <- gsub("kwiecieñ.2014", "2014 4 kwiecieñ", names(ryz_df))
names(ryz_df) <- gsub("kwiecieñ.2015", "2015 4 kwiecieñ", names(ryz_df))
names(ryz_df) <- gsub("kwiecieñ.2016", "2016 4 kwiecieñ", names(ryz_df))
names(ryz_df) <- gsub("kwiecieñ.2017", "2017 4 kwiecieñ", names(ryz_df))
names(ryz_df) <- gsub("kwiecieñ.2018", "2018 4 kwiecieñ", names(ryz_df))
names(ryz_df) <- gsub("kwiecieñ.2019", "2019 4 kwiecieñ", names(ryz_df))
names(ryz_df) <- gsub("maj.2006", "2006 5 maj", names(ryz_df))
names(ryz_df) <- gsub("maj.2007", "2007 5 maj", names(ryz_df))
names(ryz_df) <- gsub("maj.2008", "2008 5 maj", names(ryz_df))
names(ryz_df) <- gsub("maj.2009", "2009 5 maj", names(ryz_df))
names(ryz_df) <- gsub("maj.2010", "2010 5 maj", names(ryz_df))
names(ryz_df) <- gsub("maj.2011", "2011 5 maj", names(ryz_df))
names(ryz_df) <- gsub("maj.2012", "2012 5 maj", names(ryz_df))
names(ryz_df) <- gsub("maj.2013", "2013 5 maj", names(ryz_df))
names(ryz_df) <- gsub("maj.2014", "2014 5 maj", names(ryz_df))
names(ryz_df) <- gsub("maj.2015", "2015 5 maj", names(ryz_df))
names(ryz_df) <- gsub("maj.2016", "2016 5 maj", names(ryz_df))
names(ryz_df) <- gsub("maj.2017", "2017 5 maj", names(ryz_df))
names(ryz_df) <- gsub("maj.2018", "2018 5 maj", names(ryz_df))
names(ryz_df) <- gsub("maj.2019", "2019 5 maj", names(ryz_df))
names(ryz_df) <- gsub("czerwiec.2006", "2006 6 czerwiec", names(ryz_df))
names(ryz_df) <- gsub("czerwiec.2007", "2007 6 czerwiec", names(ryz_df))
names(ryz_df) <- gsub("czerwiec.2008", "2008 6 czerwiec", names(ryz_df))
names(ryz_df) <- gsub("czerwiec.2009", "2009 6 czerwiec", names(ryz_df))
names(ryz_df) <- gsub("czerwiec.2010", "2010 6 czerwiec", names(ryz_df))
names(ryz_df) <- gsub("czerwiec.2011", "2011 6 czerwiec", names(ryz_df))
names(ryz_df) <- gsub("czerwiec.2012", "2012 6 czerwiec", names(ryz_df))
names(ryz_df) <- gsub("czerwiec.2013", "2013 6 czerwiec", names(ryz_df))
names(ryz_df) <- gsub("czerwiec.2014", "2014 6 czerwiec", names(ryz_df))
names(ryz_df) <- gsub("czerwiec.2015", "2015 6 czerwiec", names(ryz_df))
names(ryz_df) <- gsub("czerwiec.2016", "2016 6 czerwiec", names(ryz_df))
names(ryz_df) <- gsub("czerwiec.2017", "2017 6 czerwiec", names(ryz_df))
names(ryz_df) <- gsub("czerwiec.2018", "2018 6 czerwiec", names(ryz_df))
names(ryz_df) <- gsub("czerwiec.2019", "2019 6 czerwiec", names(ryz_df))
names(ryz_df) <- gsub("lipiec.2006", "2006 7 lipiec", names(ryz_df))
names(ryz_df) <- gsub("lipiec.2007", "2007 7 lipiec", names(ryz_df))
names(ryz_df) <- gsub("lipiec.2008", "2008 7 lipiec", names(ryz_df))
names(ryz_df) <- gsub("lipiec.2009", "2009 7 lipiec", names(ryz_df))
names(ryz_df) <- gsub("lipiec.2010", "2010 7 lipiec", names(ryz_df))
names(ryz_df) <- gsub("lipiec.2011", "2011 7 lipiec", names(ryz_df))
names(ryz_df) <- gsub("lipiec.2012", "2012 7 lipiec", names(ryz_df))
names(ryz_df) <- gsub("lipiec.2013", "2013 7 lipiec", names(ryz_df))
names(ryz_df) <- gsub("lipiec.2014", "2014 7 lipiec", names(ryz_df))
names(ryz_df) <- gsub("lipiec.2015", "2015 7 lipiec", names(ryz_df))
names(ryz_df) <- gsub("lipiec.2016", "2016 7 lipiec", names(ryz_df))
names(ryz_df) <- gsub("lipiec.2017", "2017 7 lipiec", names(ryz_df))
names(ryz_df) <- gsub("lipiec.2018", "2018 7 lipiec", names(ryz_df))
names(ryz_df) <- gsub("lipiec.2019", "2019 7 lipiec", names(ryz_df))
names(ryz_df) <- gsub("sierpieñ.2006", "2006 8 sierpieñ", names(ryz_df))
names(ryz_df) <- gsub("sierpieñ.2007", "2007 8 sierpieñ", names(ryz_df))
names(ryz_df) <- gsub("sierpieñ.2008", "2008 8 sierpieñ", names(ryz_df))
names(ryz_df) <- gsub("sierpieñ.2009", "2009 8 sierpieñ", names(ryz_df))
names(ryz_df) <- gsub("sierpieñ.2010", "2010 8 sierpieñ", names(ryz_df))
names(ryz_df) <- gsub("sierpieñ.2011", "2011 8 sierpieñ", names(ryz_df))
names(ryz_df) <- gsub("sierpieñ.2012", "2012 8 sierpieñ", names(ryz_df))
names(ryz_df) <- gsub("sierpieñ.2013", "2013 8 sierpieñ", names(ryz_df))
names(ryz_df) <- gsub("sierpieñ.2014", "2014 8 sierpieñ", names(ryz_df))
names(ryz_df) <- gsub("sierpieñ.2015", "2015 8 sierpieñ", names(ryz_df))
names(ryz_df) <- gsub("sierpieñ.2016", "2016 8 sierpieñ", names(ryz_df))
names(ryz_df) <- gsub("sierpieñ.2017", "2017 8 sierpieñ", names(ryz_df))
names(ryz_df) <- gsub("sierpieñ.2018", "2018 8 sierpieñ", names(ryz_df))
names(ryz_df) <- gsub("sierpieñ.2019", "2019 8 sierpieñ", names(ryz_df))
names(ryz_df) <- gsub("wrzesieñ.2006", "2006 9 wrzesieñ", names(ryz_df))
names(ryz_df) <- gsub("wrzesieñ.2007", "2007 9 wrzesieñ", names(ryz_df))
names(ryz_df) <- gsub("wrzesieñ.2008", "2008 9 wrzesieñ", names(ryz_df))
names(ryz_df) <- gsub("wrzesieñ.2009", "2009 9 wrzesieñ", names(ryz_df))
names(ryz_df) <- gsub("wrzesieñ.2010", "2010 9 wrzesieñ", names(ryz_df))
names(ryz_df) <- gsub("wrzesieñ.2011", "2011 9 wrzesieñ", names(ryz_df))
names(ryz_df) <- gsub("wrzesieñ.2012", "2012 9 wrzesieñ", names(ryz_df))
names(ryz_df) <- gsub("wrzesieñ.2013", "2013 9 wrzesieñ", names(ryz_df))
names(ryz_df) <- gsub("wrzesieñ.2014", "2014 9 wrzesieñ", names(ryz_df))
names(ryz_df) <- gsub("wrzesieñ.2015", "2015 9 wrzesieñ", names(ryz_df))
names(ryz_df) <- gsub("wrzesieñ.2016", "2016 9 wrzesieñ", names(ryz_df))
names(ryz_df) <- gsub("wrzesieñ.2017", "2017 9 wrzesieñ", names(ryz_df))
names(ryz_df) <- gsub("wrzesieñ.2018", "2018 9 wrzesieñ", names(ryz_df))
names(ryz_df) <- gsub("wrzesieñ.2019", "2019 9 wrzesieñ", names(ryz_df))
names(ryz_df) <- gsub("paŸdziernik.2006", "2006 90 paŸdziernik", names(ryz_df))
names(ryz_df) <- gsub("paŸdziernik.2007", "2007 90 paŸdziernik", names(ryz_df))
names(ryz_df) <- gsub("paŸdziernik.2008", "2008 90 paŸdziernik", names(ryz_df))
names(ryz_df) <- gsub("paŸdziernik.2009", "2009 90 paŸdziernik", names(ryz_df))
names(ryz_df) <- gsub("paŸdziernik.2010", "2010 90 paŸdziernik", names(ryz_df))
names(ryz_df) <- gsub("paŸdziernik.2011", "2011 90 paŸdziernik", names(ryz_df))
names(ryz_df) <- gsub("paŸdziernik.2012", "2012 90 paŸdziernik", names(ryz_df))
names(ryz_df) <- gsub("paŸdziernik.2013", "2013 90 paŸdziernik", names(ryz_df))
names(ryz_df) <- gsub("paŸdziernik.2014", "2014 90 paŸdziernik", names(ryz_df))
names(ryz_df) <- gsub("paŸdziernik.2015", "2015 90 paŸdziernik", names(ryz_df))
names(ryz_df) <- gsub("paŸdziernik.2016", "2016 90 paŸdziernik", names(ryz_df))
names(ryz_df) <- gsub("paŸdziernik.2017", "2017 90 paŸdziernik", names(ryz_df))
names(ryz_df) <- gsub("paŸdziernik.2018", "2018 90 paŸdziernik", names(ryz_df))
names(ryz_df) <- gsub("paŸdziernik.2019", "2019 90 paŸdziernik", names(ryz_df))
names(ryz_df) <- gsub("listopad.2006", "2006 91 listopad", names(ryz_df))
names(ryz_df) <- gsub("listopad.2007", "2007 91 listopad", names(ryz_df))
names(ryz_df) <- gsub("listopad.2008", "2008 91 listopad", names(ryz_df))
names(ryz_df) <- gsub("listopad.2009", "2009 91 listopad", names(ryz_df))
names(ryz_df) <- gsub("listopad.2010", "2010 91 listopad", names(ryz_df))
names(ryz_df) <- gsub("listopad.2011", "2011 91 listopad", names(ryz_df))
names(ryz_df) <- gsub("listopad.2012", "2012 91 listopad", names(ryz_df))
names(ryz_df) <- gsub("listopad.2013", "2013 91 listopad", names(ryz_df))
names(ryz_df) <- gsub("listopad.2014", "2014 91 listopad", names(ryz_df))
names(ryz_df) <- gsub("listopad.2015", "2015 91 listopad", names(ryz_df))
names(ryz_df) <- gsub("listopad.2016", "2016 91 listopad", names(ryz_df))
names(ryz_df) <- gsub("listopad.2017", "2017 91 listopad", names(ryz_df))
names(ryz_df) <- gsub("listopad.2018", "2018 91 listopad", names(ryz_df))
names(ryz_df) <- gsub("listopad.2019", "2019 91 listopad", names(ryz_df))
names(ryz_df) <- gsub("grudzieñ.2006", "2006 92 grudzieñ", names(ryz_df))
names(ryz_df) <- gsub("grudzieñ.2007", "2007 92 grudzieñ", names(ryz_df))
names(ryz_df) <- gsub("grudzieñ.2008", "2008 92 grudzieñ", names(ryz_df))
names(ryz_df) <- gsub("grudzieñ.2009", "2009 92 grudzieñ", names(ryz_df))
names(ryz_df) <- gsub("grudzieñ.2010", "2010 92 grudzieñ", names(ryz_df))
names(ryz_df) <- gsub("grudzieñ.2011", "2011 92 grudzieñ", names(ryz_df))
names(ryz_df) <- gsub("grudzieñ.2012", "2012 92 grudzieñ", names(ryz_df))
names(ryz_df) <- gsub("grudzieñ.2013", "2013 92 grudzieñ", names(ryz_df))
names(ryz_df) <- gsub("grudzieñ.2014", "2014 92 grudzieñ", names(ryz_df))
names(ryz_df) <- gsub("grudzieñ.2015", "2015 92 grudzieñ", names(ryz_df))
names(ryz_df) <- gsub("grudzieñ.2016", "2016 92 grudzieñ", names(ryz_df))
names(ryz_df) <- gsub("grudzieñ.2017", "2017 92 grudzieñ", names(ryz_df))
names(ryz_df) <- gsub("grudzieñ.2018", "2018 92 grudzieñ", names(ryz_df))
names(ryz_df) <- gsub("grudzieñ.2019", "2019 92 grudzieñ", names(ryz_df))
ryz_df = ryz_df[sort(colnames(ryz_df))] #Sortujê chronologicznie
ryz_df = subset(ryz_df, select = -c(Nazwa) ) # Usuwam zbêdn¹ kolumnê z nazwazmi województw


ryz_df = t(ryz_df) #Tramsponujê dataframe ¿eby poszczególne kolumny by³y województwami 
ryz_df = unname(ryz_df) #Usuwam zbêdne teraz nazwy
ryz_df = as.data.frame(ryz_df) #Przekszta³cam na dataframe


#Normalnie nie da³o siê przkonwertowaæ danych na numeric, bo zamiast kropek by³y 
# przecinki i niemi³osiernie d³ugo siê nad tym g³owi³em. Ca³y czas otrzymywa³em 
#wartoœci NA
ryz_dolnoslaskie = as.numeric(gsub(",", ".", as.character(ryz_df$V1)))
ryz_kujawsko_pomorskie = as.numeric(gsub(",", ".", as.character(ryz_df$V2)))
ryz_lubelskie = as.numeric(gsub(",", ".", as.character(ryz_df$V3)))
ryz_lubuskie = as.numeric(gsub(",", ".", as.character(ryz_df$V4)))
ryz_lodzkie = as.numeric(gsub(",", ".", as.character(ryz_df$V5)))
ryz_malopolskie = as.numeric(gsub(",", ".", as.character(ryz_df$V6)))
ryz_mazowieckie = as.numeric(gsub(",", ".", as.character(ryz_df$V7)))
ryz_opolskie = as.numeric(gsub(",", ".", as.character(ryz_df$V8)))
ryz_podkarpackie = as.numeric(gsub(",", ".", as.character(ryz_df$V9)))
ryz_podlaskie = as.numeric(gsub(",", ".", as.character(ryz_df$V10)))
ryz_pomorskie = as.numeric(gsub(",", ".", as.character(ryz_df$V11)))
ryz_slaskie = as.numeric(gsub(",", ".", as.character(ryz_df$V12)))
ryz_swietokrzyskie = as.numeric(gsub(",", ".", as.character(ryz_df$V13)))
ryz_warminsko_mazurskie = as.numeric(gsub(",", ".", as.character(ryz_df$V14)))
ryz_wielkopolskie = as.numeric(gsub(",", ".", as.character(ryz_df$V15)))
ryz_zachodniopomorskie = as.numeric(gsub(",", ".", as.character(ryz_df$V16)))

#### Wykres cen ry¿u ####
y = (1:168)
#Wykres dla ryzu i wszystkich województw
ryz_plot = plot_ly(x = y )%>%
  layout(title = 'Zmiana cen ry¿u w latach 2006-2019', xaxis = list(title = 'Rok', 
                                                                    ticktext = list("2006", "2007", "2008", "2009", "2010", "2011","2012","2013","2014","2015","2016","2017","2018","2019"), 
                                                                    tickvals = list(1, 12, 24, 36, 48, 60, 72, 84, 96, 108, 120, 132, 144, 156)),
         yaxis = list(title = 'cena (z³)')) %>%
  add_lines(y = ryz_dolnoslaskie, name = "dolnoœlaskie")%>%
  add_lines(y = ryz_kujawsko_pomorskie, name = "kujawsko-pomorskie")%>%
  add_lines(y = ryz_lubelskie, name = "lubelskie")%>%
  add_lines(y = ryz_lubuskie, name = "lubuskie")%>%
  add_lines(y = ryz_lodzkie, name = "³ódzkie")%>%
  add_lines(y = ryz_malopolskie, name = "ma³opolskie")%>%
  add_lines(y = ryz_mazowieckie, name = "mazowieckie")%>%
  add_lines(y = ryz_opolskie, name = "opolskie")%>%
  add_lines(y = ryz_podkarpackie, name = "podkarpackie")%>%
  add_lines(y = ryz_podlaskie, name = "podlaskie")%>%
  add_lines(y = ryz_pomorskie, name = "pomorskie")%>%
  add_lines(y = ryz_slaskie, name = "œl¹skie")%>%
  add_lines(y = ryz_swietokrzyskie, name = "œwietokrzyskie")%>%
  add_lines(y = ryz_warminsko_mazurskie, name = "warmiñsko_mazurskie")%>%
  add_lines(y = ryz_wielkopolskie, name = "wielkopolskie")%>%
  add_lines(y = ryz_zachodniopomorskie, name = "zachodniopomorskie")
#print(ryz_plot)

#### Œrednie roczne ceny ry¿u ####
roczne_sr_ryz = data.frame('','','','','','','','','','','','','', stringsAsFactors = FALSE)
names(roczne_sr_ryz) = c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016",
                         "2017","2018")

for (i in 1:13) {
  roczne_sr_ryz[1,i] = round(mean(ryz_dolnoslaskie[(1+(i-1)*12):(12*i)]),digits = 2)
  roczne_sr_ryz[2,i] = round(mean(ryz_kujawsko_pomorskie [(1+(i-1)*12):(12*i)]),digits = 2)
  roczne_sr_ryz[3,i] = round(mean(ryz_lubelskie [(1+(i-1)*12):(12*i)]),digits = 2)
  roczne_sr_ryz[4,i] = round(mean(ryz_lubuskie [(1+(i-1)*12):(12*i)]),digits = 2)
  roczne_sr_ryz[5,i] = round(mean(ryz_lodzkie [(1+(i-1)*12):(12*i)]),digits = 2)
  roczne_sr_ryz[6,i] = round(mean(ryz_malopolskie [(1+(i-1)*12):(12*i)]),digits = 2)
  roczne_sr_ryz[7,i] = round(mean(ryz_mazowieckie [(1+(i-1)*12):(12*i)]),digits = 2)
  roczne_sr_ryz[8,i] = round(mean(ryz_opolskie [(1+(i-1)*12):(12*i)]),digits = 2)
  roczne_sr_ryz[9,i] = round(mean(ryz_podkarpackie [(1+(i-1)*12):(12*i)]),digits = 2)
  roczne_sr_ryz[10,i] = round(mean(ryz_podlaskie [(1+(i-1)*12):(12*i)]),digits = 2)
  roczne_sr_ryz[11,i] = round(mean(ryz_pomorskie [(1+(i-1)*12):(12*i)]),digits = 2)
  roczne_sr_ryz[12,i] = round(mean(ryz_slaskie [(1+(i-1)*12):(12*i)]),digits = 2)
  roczne_sr_ryz[13,i] = round(mean(ryz_swietokrzyskie [(1+(i-1)*12):(12*i)]),digits = 2)
  roczne_sr_ryz[14,i] = round(mean(ryz_warminsko_mazurskie [(1+(i-1)*12):(12*i)]),digits = 2)
  roczne_sr_ryz[15,i] = round(mean(ryz_wielkopolskie [(1+(i-1)*12):(12*i)]),digits = 2)
  roczne_sr_ryz[16,i] = round(mean(ryz_zachodniopomorskie[(1+(i-1)*12):(12*i)]),digits = 2)
}
roczne_sr_ryz = t(roczne_sr_ryz)

roczne_sr_ryz = unname(roczne_sr_ryz) #Usuwam zbêdne teraz nazwy
roczne_sr_ryz = as.data.frame(roczne_sr_ryz) #Przekszta³cam na dataframe

#### Œrednia cena ry¿u dla ka¿dego wojewódstwa ####
ryz_dolnoslaskie_sr = round(mean(ryz_dolnoslaskie), digits = 2)               #1
ryz_kujawsko_pomorskie_sr = round(mean(ryz_kujawsko_pomorskie), digits = 2)   #2
ryz_lubelskie_sr = round(mean(ryz_lubelskie), digits = 2)                     #3
ryz_lubuskie_sr = round(mean(ryz_lubuskie), digits = 2)                       #4
ryz_lodzkie_sr = round(mean(ryz_lodzkie), digits = 2)                         #5
ryz_malopolskie_sr = round(mean(ryz_malopolskie), digits = 2)                 #6
ryz_mazowieckie_sr = round(mean(ryz_mazowieckie), digits = 2)                 #7
ryz_opolskie_sr = round(mean(ryz_opolskie), digits = 2)                       #8
ryz_podkarpackie_sr = round(mean(ryz_podkarpackie), digits = 2)               #9
ryz_podlaskie_sr = round(mean(ryz_podlaskie), digits = 2)                     #10
ryz_pomorskie_sr = round(mean(ryz_pomorskie), digits = 2)                     #11
ryz_slaskie_sr = round(mean(ryz_slaskie), digits = 2)                         #12
ryz_swietokrzyskie_sr = round(mean(ryz_swietokrzyskie), digits = 2)           #13
ryz_warminsko_mazurskie_sr = round(mean(ryz_warminsko_mazurskie), digits = 2) #14
ryz_wielkopolskie_sr = round(mean(ryz_wielkopolskie), digits = 2)             #15
ryz_zachodniopomorskie_sr = round(mean(ryz_zachodniopomorskie), digits = 2)   #16



#### Œrednie, najw, najmn ceny w jednym wektorze ####
sr_cena_ryz = c(ryz_dolnoslaskie_sr,ryz_kujawsko_pomorskie_sr,ryz_lubelskie_sr,ryz_lubuskie_sr,ryz_lodzkie_sr,
                ryz_malopolskie_sr,ryz_mazowieckie_sr,ryz_opolskie_sr,ryz_podkarpackie_sr,ryz_podlaskie_sr,
                ryz_pomorskie_sr,ryz_slaskie_sr,ryz_swietokrzyskie_sr,ryz_warminsko_mazurskie_sr,
                ryz_wielkopolskie_sr,ryz_zachodniopomorskie_sr)

ryz_najmn_w_kazdym_woj = c(min(ryz_dolnoslaskie),min(ryz_kujawsko_pomorskie),min(ryz_lubelskie),
                           min(ryz_lubuskie),min(ryz_lodzkie),min(ryz_malopolskie),
                           min(ryz_mazowieckie),min(ryz_opolskie),min(ryz_podkarpackie),
                           min(ryz_podlaskie),min(ryz_pomorskie),min(ryz_slaskie),
                           min(ryz_swietokrzyskie),min(ryz_warminsko_mazurskie),
                           min(ryz_wielkopolskie),min(ryz_zachodniopomorskie))

ryz_najw_w_kazdym_woj = c(max(ryz_dolnoslaskie),max(ryz_kujawsko_pomorskie),max(ryz_lubelskie),
                          max(ryz_lubuskie),max(ryz_lodzkie),max(ryz_malopolskie),
                          max(ryz_mazowieckie),max(ryz_opolskie),max(ryz_podkarpackie),
                          max(ryz_podlaskie),max(ryz_pomorskie),max(ryz_slaskie),
                          max(ryz_swietokrzyskie),max(ryz_warminsko_mazurskie),
                          max(ryz_wielkopolskie),max(ryz_zachodniopomorskie))

###  Najwy¿sze i najni¿sze, ró¿ne

ryz_sr_cena = mean(sr_cena_ryz) #Œrednia cena ry¿u dla ca³ego kraju
ryz_odsd_cena = sd(sr_cena_ryz)  #Odchylenie standardowe ceny ry¿u

ryz_najw_sr = max(sr_cena_ryz) #Najwy¿sza œrednia cena ry¿u
ryz_najm_sr = min(sr_cena_ryz) #Najni¿sza œrednia cena ry¿u
ryz_najmn_k = min(ryz_najmn_w_kazdym_woj) #Najni¿sza cena kiedykolwiek
ryz_najw_k = max(ryz_najw_w_kazdym_woj) #Najwy¿sza cena kiedykolwiek


rnajw_sr = as.numeric(match(ryz_najw_sr,sr_cena_ryz))  #Numer województwa
rnajmn_sr = as.numeric(match(ryz_najm_sr,sr_cena_ryz)) #Numer województwa
print(paste("Najwy¿sza œrednia cena ry¿u jest w województwie", wojewodztwa[rnajw_sr],
            "i wynosi",ryz_najw_sr))
print(paste("Najni¿sza œrednia cena ry¿u jest w województwie", wojewodztwa[rnajmn_sr],
            "i wynosi",ryz_najm_sr))


rnajmn_kazde = as.numeric(match(ryz_najmn_k,ryz_najmn_w_kazdym_woj))  #Numer województwa
rnajw_kazde = as.numeric(match(ryz_najw_k,ryz_najw_w_kazdym_woj))     #Numer województwa
rnajmn_kiedy =  match(ryz_najmn_k,ryz_opolskie)                   #Kiedy
rnajw_kiedy =  match(ryz_najw_k,ryz_swietokrzyskie)               #Kiedy

print(paste("Najni¿sza cena ry¿u by³a", daty[rnajmn_kiedy], "w województwie", 
            wojewodztwa[rnajmn_kazde],"i wynosi³a", ryz_najmn_k, "z³"))

print(paste("Najwy¿sza cena ry¿u by³a", daty[rnajw_kiedy], wojewodztwa[rnajw_kazde],
            "i wynosi³a", ryz_najw_k, "z³"))
print(paste("Odchylenie standardowe cen ry¿u wynosi", round(ryz_odsd_cena, digits = 4), "z³otego"))

#### Korelacja p³acy minimalnej do œredniej ceny ry¿u ####
ryz_placa_min_kor = c(cor(as.numeric(as.character(placa_min_df$V1)), as.numeric(as.character(roczne_sr_ryz$V1))),
                      cor(as.numeric(as.character(placa_min_df$V2)), as.numeric(as.character(roczne_sr_ryz$V2))),
                      cor(as.numeric(as.character(placa_min_df$V3)), as.numeric(as.character(roczne_sr_ryz$V3))),
                      cor(as.numeric(as.character(placa_min_df$V4)), as.numeric(as.character(roczne_sr_ryz$V4))),
                      cor(as.numeric(as.character(placa_min_df$V5)), as.numeric(as.character(roczne_sr_ryz$V5))),
                      cor(as.numeric(as.character(placa_min_df$V6)), as.numeric(as.character(roczne_sr_ryz$V6))),
                      cor(as.numeric(as.character(placa_min_df$V7)), as.numeric(as.character(roczne_sr_ryz$V7))),
                      cor(as.numeric(as.character(placa_min_df$V8)), as.numeric(as.character(roczne_sr_ryz$V8))),
                      cor(as.numeric(as.character(placa_min_df$V9)), as.numeric(as.character(roczne_sr_ryz$V9))),
                      cor(as.numeric(as.character(placa_min_df$V10)), as.numeric(as.character(roczne_sr_ryz$V10))),
                      cor(as.numeric(as.character(placa_min_df$V11)), as.numeric(as.character(roczne_sr_ryz$V11))),
                      cor(as.numeric(as.character(placa_min_df$V12)), as.numeric(as.character(roczne_sr_ryz$V12))),
                      cor(as.numeric(as.character(placa_min_df$V13)), as.numeric(as.character(roczne_sr_ryz$V13))))


print(paste("Uœredniona korelacja minimalnej pensji do ceny ry¿u wynosi", round(mean(ryz_placa_min_kor), digits = 4), "co wskazuje na raczej ma³¹ korelacjê"))
#### Korelacja œredniej pensji do œredniej ceny ry¿u ####

ryz_sr_pensja_kor = c(cor(DOLNO, as.numeric(as.character(roczne_sr_ryz$V1))),
                      cor(KUJAW, as.numeric(as.character(roczne_sr_ryz$V2))),
                      cor(LUBEL, as.numeric(as.character(roczne_sr_ryz$V3))),
                      cor(LUBUS, as.numeric(as.character(roczne_sr_ryz$V4))),
                      cor(LODZK, as.numeric(as.character(roczne_sr_ryz$V5))),
                      cor(MALOP, as.numeric(as.character(roczne_sr_ryz$V6))),
                      cor(MAZOW, as.numeric(as.character(roczne_sr_ryz$V7))),
                      cor(OPOLS, as.numeric(as.character(roczne_sr_ryz$V8))),
                      cor(PODKA, as.numeric(as.character(roczne_sr_ryz$V9))),
                      cor(PODLA, as.numeric(as.character(roczne_sr_ryz$V10))),
                      cor(POMOR, as.numeric(as.character(roczne_sr_ryz$V11))),
                      cor(SLASK, as.numeric(as.character(roczne_sr_ryz$V12))),
                      cor(SWIET, as.numeric(as.character(roczne_sr_ryz$V13))),
                      cor(WARMI, as.numeric(as.character(roczne_sr_ryz$V14))),
                      cor(WIELK, as.numeric(as.character(roczne_sr_ryz$V15))),
                      cor(ZACHO, as.numeric(as.character(roczne_sr_ryz$V16))))
sr_ryz_kor = mean(ryz_sr_pensja_kor)
print(paste("Uœredniona korelacja œredniej pensji do ceny ry¿u wynosi", round(sr_ryz_kor, digits = 4), "co wskazuje na raczej ma³¹ korelacjê"))
print(".........................................................")









#### Kurczak edycja nag³ówków i tabel ####
kurczak_df = subset(kurczak_df, select = -c(Kod,X) ) #Usuwam zbêdn¹ kolumnê Kod i X

#Edtujê nazwy kolumn na sensowne
names(kurczak_df) <- gsub("styczeñ.kurczêta.patroszone...za.1kg.cena.", "styczeñ.", names(kurczak_df))
names(kurczak_df) <- gsub("luty.kurczêta.patroszone...za.1kg.cena.", "luty.", names(kurczak_df))
names(kurczak_df) <- gsub("marzec.kurczêta.patroszone...za.1kg.cena.", "marzec.", names(kurczak_df))
names(kurczak_df) <- gsub("kwiecieñ.kurczêta.patroszone...za.1kg.cena.", "kwiecieñ.", names(kurczak_df))
names(kurczak_df) <- gsub("maj.kurczêta.patroszone...za.1kg.cena.", "maj.", names(kurczak_df))
names(kurczak_df) <- gsub("czerwiec.kurczêta.patroszone...za.1kg.cena.", "czerwiec.", names(kurczak_df))
names(kurczak_df) <- gsub("lipiec.kurczêta.patroszone...za.1kg.cena.", "lipiec.", names(kurczak_df))
names(kurczak_df) <- gsub("sierpieñ.kurczêta.patroszone...za.1kg.cena.", "sierpieñ.", names(kurczak_df))
names(kurczak_df) <- gsub("wrzesieñ.kurczêta.patroszone...za.1kg.cena.", "wrzesieñ.", names(kurczak_df))
names(kurczak_df) <- gsub("paŸdziernik.kurczêta.patroszone...za.1kg.cena.", "paŸdziernik.", names(kurczak_df))
names(kurczak_df) <- gsub("listopad.kurczêta.patroszone...za.1kg.cena.", "listopad.", names(kurczak_df))
names(kurczak_df) <- gsub("grudzieñ.kurczêta.patroszone...za.1kg.cena.", "grudzieñ.", names(kurczak_df))
names(kurczak_df) <- gsub("..z³.", "", names(kurczak_df))

#Nie znalaz³em inteligentnego sposobu na sensowne posortowanie chronologiczne wiêc robiê to ³opatologicznie
names(kurczak_df) <- gsub("styczeñ.2006", "2006 1 styczeñ", names(kurczak_df))
names(kurczak_df) <- gsub("styczeñ.2007", "2007 1 styczeñ", names(kurczak_df))
names(kurczak_df) <- gsub("styczeñ.2008", "2008 1 styczeñ", names(kurczak_df))
names(kurczak_df) <- gsub("styczeñ.2009", "2009 1 styczeñ", names(kurczak_df))
names(kurczak_df) <- gsub("styczeñ.2010", "2010 1 styczeñ", names(kurczak_df))
names(kurczak_df) <- gsub("styczeñ.2011", "2011 1 styczeñ", names(kurczak_df))
names(kurczak_df) <- gsub("styczeñ.2012", "2012 1 styczeñ", names(kurczak_df))
names(kurczak_df) <- gsub("styczeñ.2013", "2013 1 styczeñ", names(kurczak_df))
names(kurczak_df) <- gsub("styczeñ.2014", "2014 1 styczeñ", names(kurczak_df))
names(kurczak_df) <- gsub("styczeñ.2015", "2015 1 styczeñ", names(kurczak_df))
names(kurczak_df) <- gsub("styczeñ.2016", "2016 1 styczeñ", names(kurczak_df))
names(kurczak_df) <- gsub("styczeñ.2017", "2017 1 styczeñ", names(kurczak_df))
names(kurczak_df) <- gsub("styczeñ.2018", "2018 1 styczeñ", names(kurczak_df))
names(kurczak_df) <- gsub("styczeñ.2019", "2019 1 styczeñ", names(kurczak_df))
names(kurczak_df) <- gsub("luty.2006", "2006 2 luty", names(kurczak_df))
names(kurczak_df) <- gsub("luty.2007", "2007 2 luty", names(kurczak_df))
names(kurczak_df) <- gsub("luty.2008", "2008 2 luty", names(kurczak_df))
names(kurczak_df) <- gsub("luty.2009", "2009 2 luty", names(kurczak_df))
names(kurczak_df) <- gsub("luty.2010", "2010 2 luty", names(kurczak_df))
names(kurczak_df) <- gsub("luty.2011", "2011 2 luty", names(kurczak_df))
names(kurczak_df) <- gsub("luty.2012", "2012 2 luty", names(kurczak_df))
names(kurczak_df) <- gsub("luty.2013", "2013 2 luty", names(kurczak_df))
names(kurczak_df) <- gsub("luty.2014", "2014 2 luty", names(kurczak_df))
names(kurczak_df) <- gsub("luty.2015", "2015 2 luty", names(kurczak_df))
names(kurczak_df) <- gsub("luty.2016", "2016 2 luty", names(kurczak_df))
names(kurczak_df) <- gsub("luty.2017", "2017 2 luty", names(kurczak_df))
names(kurczak_df) <- gsub("luty.2018", "2018 2 luty", names(kurczak_df))
names(kurczak_df) <- gsub("luty.2019", "2019 2 luty", names(kurczak_df))
names(kurczak_df) <- gsub("marzec.2006", "2006 3 marzec", names(kurczak_df))
names(kurczak_df) <- gsub("marzec.2007", "2007 3 marzec", names(kurczak_df))
names(kurczak_df) <- gsub("marzec.2008", "2008 3 marzec", names(kurczak_df))
names(kurczak_df) <- gsub("marzec.2009", "2009 3 marzec", names(kurczak_df))
names(kurczak_df) <- gsub("marzec.2010", "2010 3 marzec", names(kurczak_df))
names(kurczak_df) <- gsub("marzec.2011", "2011 3 marzec", names(kurczak_df))
names(kurczak_df) <- gsub("marzec.2012", "2012 3 marzec", names(kurczak_df))
names(kurczak_df) <- gsub("marzec.2013", "2013 3 marzec", names(kurczak_df))
names(kurczak_df) <- gsub("marzec.2014", "2014 3 marzec", names(kurczak_df))
names(kurczak_df) <- gsub("marzec.2015", "2015 3 marzec", names(kurczak_df))
names(kurczak_df) <- gsub("marzec.2016", "2016 3 marzec", names(kurczak_df))
names(kurczak_df) <- gsub("marzec.2017", "2017 3 marzec", names(kurczak_df))
names(kurczak_df) <- gsub("marzec.2018", "2018 3 marzec", names(kurczak_df))
names(kurczak_df) <- gsub("marzec.2019", "2019 3 marzec", names(kurczak_df))
names(kurczak_df) <- gsub("kwiecieñ.2006", "2006 4 kwiecieñ", names(kurczak_df))
names(kurczak_df) <- gsub("kwiecieñ.2007", "2007 4 kwiecieñ", names(kurczak_df))
names(kurczak_df) <- gsub("kwiecieñ.2008", "2008 4 kwiecieñ", names(kurczak_df))
names(kurczak_df) <- gsub("kwiecieñ.2009", "2009 4 kwiecieñ", names(kurczak_df))
names(kurczak_df) <- gsub("kwiecieñ.2010", "2010 4 kwiecieñ", names(kurczak_df))
names(kurczak_df) <- gsub("kwiecieñ.2011", "2011 4 kwiecieñ", names(kurczak_df))
names(kurczak_df) <- gsub("kwiecieñ.2012", "2012 4 kwiecieñ", names(kurczak_df))
names(kurczak_df) <- gsub("kwiecieñ.2013", "2013 4 kwiecieñ", names(kurczak_df))
names(kurczak_df) <- gsub("kwiecieñ.2014", "2014 4 kwiecieñ", names(kurczak_df))
names(kurczak_df) <- gsub("kwiecieñ.2015", "2015 4 kwiecieñ", names(kurczak_df))
names(kurczak_df) <- gsub("kwiecieñ.2016", "2016 4 kwiecieñ", names(kurczak_df))
names(kurczak_df) <- gsub("kwiecieñ.2017", "2017 4 kwiecieñ", names(kurczak_df))
names(kurczak_df) <- gsub("kwiecieñ.2018", "2018 4 kwiecieñ", names(kurczak_df))
names(kurczak_df) <- gsub("kwiecieñ.2019", "2019 4 kwiecieñ", names(kurczak_df))
names(kurczak_df) <- gsub("maj.2006", "2006 5 maj", names(kurczak_df))
names(kurczak_df) <- gsub("maj.2007", "2007 5 maj", names(kurczak_df))
names(kurczak_df) <- gsub("maj.2008", "2008 5 maj", names(kurczak_df))
names(kurczak_df) <- gsub("maj.2009", "2009 5 maj", names(kurczak_df))
names(kurczak_df) <- gsub("maj.2010", "2010 5 maj", names(kurczak_df))
names(kurczak_df) <- gsub("maj.2011", "2011 5 maj", names(kurczak_df))
names(kurczak_df) <- gsub("maj.2012", "2012 5 maj", names(kurczak_df))
names(kurczak_df) <- gsub("maj.2013", "2013 5 maj", names(kurczak_df))
names(kurczak_df) <- gsub("maj.2014", "2014 5 maj", names(kurczak_df))
names(kurczak_df) <- gsub("maj.2015", "2015 5 maj", names(kurczak_df))
names(kurczak_df) <- gsub("maj.2016", "2016 5 maj", names(kurczak_df))
names(kurczak_df) <- gsub("maj.2017", "2017 5 maj", names(kurczak_df))
names(kurczak_df) <- gsub("maj.2018", "2018 5 maj", names(kurczak_df))
names(kurczak_df) <- gsub("maj.2019", "2019 5 maj", names(kurczak_df))
names(kurczak_df) <- gsub("czerwiec.2006", "2006 6 czerwiec", names(kurczak_df))
names(kurczak_df) <- gsub("czerwiec.2007", "2007 6 czerwiec", names(kurczak_df))
names(kurczak_df) <- gsub("czerwiec.2008", "2008 6 czerwiec", names(kurczak_df))
names(kurczak_df) <- gsub("czerwiec.2009", "2009 6 czerwiec", names(kurczak_df))
names(kurczak_df) <- gsub("czerwiec.2010", "2010 6 czerwiec", names(kurczak_df))
names(kurczak_df) <- gsub("czerwiec.2011", "2011 6 czerwiec", names(kurczak_df))
names(kurczak_df) <- gsub("czerwiec.2012", "2012 6 czerwiec", names(kurczak_df))
names(kurczak_df) <- gsub("czerwiec.2013", "2013 6 czerwiec", names(kurczak_df))
names(kurczak_df) <- gsub("czerwiec.2014", "2014 6 czerwiec", names(kurczak_df))
names(kurczak_df) <- gsub("czerwiec.2015", "2015 6 czerwiec", names(kurczak_df))
names(kurczak_df) <- gsub("czerwiec.2016", "2016 6 czerwiec", names(kurczak_df))
names(kurczak_df) <- gsub("czerwiec.2017", "2017 6 czerwiec", names(kurczak_df))
names(kurczak_df) <- gsub("czerwiec.2018", "2018 6 czerwiec", names(kurczak_df))
names(kurczak_df) <- gsub("czerwiec.2019", "2019 6 czerwiec", names(kurczak_df))
names(kurczak_df) <- gsub("lipiec.2006", "2006 7 lipiec", names(kurczak_df))
names(kurczak_df) <- gsub("lipiec.2007", "2007 7 lipiec", names(kurczak_df))
names(kurczak_df) <- gsub("lipiec.2008", "2008 7 lipiec", names(kurczak_df))
names(kurczak_df) <- gsub("lipiec.2009", "2009 7 lipiec", names(kurczak_df))
names(kurczak_df) <- gsub("lipiec.2010", "2010 7 lipiec", names(kurczak_df))
names(kurczak_df) <- gsub("lipiec.2011", "2011 7 lipiec", names(kurczak_df))
names(kurczak_df) <- gsub("lipiec.2012", "2012 7 lipiec", names(kurczak_df))
names(kurczak_df) <- gsub("lipiec.2013", "2013 7 lipiec", names(kurczak_df))
names(kurczak_df) <- gsub("lipiec.2014", "2014 7 lipiec", names(kurczak_df))
names(kurczak_df) <- gsub("lipiec.2015", "2015 7 lipiec", names(kurczak_df))
names(kurczak_df) <- gsub("lipiec.2016", "2016 7 lipiec", names(kurczak_df))
names(kurczak_df) <- gsub("lipiec.2017", "2017 7 lipiec", names(kurczak_df))
names(kurczak_df) <- gsub("lipiec.2018", "2018 7 lipiec", names(kurczak_df))
names(kurczak_df) <- gsub("lipiec.2019", "2019 7 lipiec", names(kurczak_df))
names(kurczak_df) <- gsub("sierpieñ.2006", "2006 8 sierpieñ", names(kurczak_df))
names(kurczak_df) <- gsub("sierpieñ.2007", "2007 8 sierpieñ", names(kurczak_df))
names(kurczak_df) <- gsub("sierpieñ.2008", "2008 8 sierpieñ", names(kurczak_df))
names(kurczak_df) <- gsub("sierpieñ.2009", "2009 8 sierpieñ", names(kurczak_df))
names(kurczak_df) <- gsub("sierpieñ.2010", "2010 8 sierpieñ", names(kurczak_df))
names(kurczak_df) <- gsub("sierpieñ.2011", "2011 8 sierpieñ", names(kurczak_df))
names(kurczak_df) <- gsub("sierpieñ.2012", "2012 8 sierpieñ", names(kurczak_df))
names(kurczak_df) <- gsub("sierpieñ.2013", "2013 8 sierpieñ", names(kurczak_df))
names(kurczak_df) <- gsub("sierpieñ.2014", "2014 8 sierpieñ", names(kurczak_df))
names(kurczak_df) <- gsub("sierpieñ.2015", "2015 8 sierpieñ", names(kurczak_df))
names(kurczak_df) <- gsub("sierpieñ.2016", "2016 8 sierpieñ", names(kurczak_df))
names(kurczak_df) <- gsub("sierpieñ.2017", "2017 8 sierpieñ", names(kurczak_df))
names(kurczak_df) <- gsub("sierpieñ.2018", "2018 8 sierpieñ", names(kurczak_df))
names(kurczak_df) <- gsub("sierpieñ.2019", "2019 8 sierpieñ", names(kurczak_df))
names(kurczak_df) <- gsub("wrzesieñ.2006", "2006 9 wrzesieñ", names(kurczak_df))
names(kurczak_df) <- gsub("wrzesieñ.2007", "2007 9 wrzesieñ", names(kurczak_df))
names(kurczak_df) <- gsub("wrzesieñ.2008", "2008 9 wrzesieñ", names(kurczak_df))
names(kurczak_df) <- gsub("wrzesieñ.2009", "2009 9 wrzesieñ", names(kurczak_df))
names(kurczak_df) <- gsub("wrzesieñ.2010", "2010 9 wrzesieñ", names(kurczak_df))
names(kurczak_df) <- gsub("wrzesieñ.2011", "2011 9 wrzesieñ", names(kurczak_df))
names(kurczak_df) <- gsub("wrzesieñ.2012", "2012 9 wrzesieñ", names(kurczak_df))
names(kurczak_df) <- gsub("wrzesieñ.2013", "2013 9 wrzesieñ", names(kurczak_df))
names(kurczak_df) <- gsub("wrzesieñ.2014", "2014 9 wrzesieñ", names(kurczak_df))
names(kurczak_df) <- gsub("wrzesieñ.2015", "2015 9 wrzesieñ", names(kurczak_df))
names(kurczak_df) <- gsub("wrzesieñ.2016", "2016 9 wrzesieñ", names(kurczak_df))
names(kurczak_df) <- gsub("wrzesieñ.2017", "2017 9 wrzesieñ", names(kurczak_df))
names(kurczak_df) <- gsub("wrzesieñ.2018", "2018 9 wrzesieñ", names(kurczak_df))
names(kurczak_df) <- gsub("wrzesieñ.2019", "2019 9 wrzesieñ", names(kurczak_df))
names(kurczak_df) <- gsub("paŸdziernik.2006", "2006 90 paŸdziernik", names(kurczak_df))
names(kurczak_df) <- gsub("paŸdziernik.2007", "2007 90 paŸdziernik", names(kurczak_df))
names(kurczak_df) <- gsub("paŸdziernik.2008", "2008 90 paŸdziernik", names(kurczak_df))
names(kurczak_df) <- gsub("paŸdziernik.2009", "2009 90 paŸdziernik", names(kurczak_df))
names(kurczak_df) <- gsub("paŸdziernik.2010", "2010 90 paŸdziernik", names(kurczak_df))
names(kurczak_df) <- gsub("paŸdziernik.2011", "2011 90 paŸdziernik", names(kurczak_df))
names(kurczak_df) <- gsub("paŸdziernik.2012", "2012 90 paŸdziernik", names(kurczak_df))
names(kurczak_df) <- gsub("paŸdziernik.2013", "2013 90 paŸdziernik", names(kurczak_df))
names(kurczak_df) <- gsub("paŸdziernik.2014", "2014 90 paŸdziernik", names(kurczak_df))
names(kurczak_df) <- gsub("paŸdziernik.2015", "2015 90 paŸdziernik", names(kurczak_df))
names(kurczak_df) <- gsub("paŸdziernik.2016", "2016 90 paŸdziernik", names(kurczak_df))
names(kurczak_df) <- gsub("paŸdziernik.2017", "2017 90 paŸdziernik", names(kurczak_df))
names(kurczak_df) <- gsub("paŸdziernik.2018", "2018 90 paŸdziernik", names(kurczak_df))
names(kurczak_df) <- gsub("paŸdziernik.2019", "2019 90 paŸdziernik", names(kurczak_df))
names(kurczak_df) <- gsub("listopad.2006", "2006 91 listopad", names(kurczak_df))
names(kurczak_df) <- gsub("listopad.2007", "2007 91 listopad", names(kurczak_df))
names(kurczak_df) <- gsub("listopad.2008", "2008 91 listopad", names(kurczak_df))
names(kurczak_df) <- gsub("listopad.2009", "2009 91 listopad", names(kurczak_df))
names(kurczak_df) <- gsub("listopad.2010", "2010 91 listopad", names(kurczak_df))
names(kurczak_df) <- gsub("listopad.2011", "2011 91 listopad", names(kurczak_df))
names(kurczak_df) <- gsub("listopad.2012", "2012 91 listopad", names(kurczak_df))
names(kurczak_df) <- gsub("listopad.2013", "2013 91 listopad", names(kurczak_df))
names(kurczak_df) <- gsub("listopad.2014", "2014 91 listopad", names(kurczak_df))
names(kurczak_df) <- gsub("listopad.2015", "2015 91 listopad", names(kurczak_df))
names(kurczak_df) <- gsub("listopad.2016", "2016 91 listopad", names(kurczak_df))
names(kurczak_df) <- gsub("listopad.2017", "2017 91 listopad", names(kurczak_df))
names(kurczak_df) <- gsub("listopad.2018", "2018 91 listopad", names(kurczak_df))
names(kurczak_df) <- gsub("listopad.2019", "2019 91 listopad", names(kurczak_df))
names(kurczak_df) <- gsub("grudzieñ.2006", "2006 92 grudzieñ", names(kurczak_df))
names(kurczak_df) <- gsub("grudzieñ.2007", "2007 92 grudzieñ", names(kurczak_df))
names(kurczak_df) <- gsub("grudzieñ.2008", "2008 92 grudzieñ", names(kurczak_df))
names(kurczak_df) <- gsub("grudzieñ.2009", "2009 92 grudzieñ", names(kurczak_df))
names(kurczak_df) <- gsub("grudzieñ.2010", "2010 92 grudzieñ", names(kurczak_df))
names(kurczak_df) <- gsub("grudzieñ.2011", "2011 92 grudzieñ", names(kurczak_df))
names(kurczak_df) <- gsub("grudzieñ.2012", "2012 92 grudzieñ", names(kurczak_df))
names(kurczak_df) <- gsub("grudzieñ.2013", "2013 92 grudzieñ", names(kurczak_df))
names(kurczak_df) <- gsub("grudzieñ.2014", "2014 92 grudzieñ", names(kurczak_df))
names(kurczak_df) <- gsub("grudzieñ.2015", "2015 92 grudzieñ", names(kurczak_df))
names(kurczak_df) <- gsub("grudzieñ.2016", "2016 92 grudzieñ", names(kurczak_df))
names(kurczak_df) <- gsub("grudzieñ.2017", "2017 92 grudzieñ", names(kurczak_df))
names(kurczak_df) <- gsub("grudzieñ.2018", "2018 92 grudzieñ", names(kurczak_df))
names(kurczak_df) <- gsub("grudzieñ.2019", "2019 92 grudzieñ", names(kurczak_df))
kurczak_df = kurczak_df[sort(colnames(kurczak_df))] #Sortujê chronologicznie
kurczak_df = subset(kurczak_df, select = -c(Nazwa) ) # Usuwam zbêdn¹ kolumnê z nazwazmi województw


kurczak_df = t(kurczak_df) #Tramsponujê dataframe ¿eby poszczególne kolumny by³y województwami 
kurczak_df = unname(kurczak_df) #Usuwam zbêdne teraz nazwy
kurczak_df = as.data.frame(kurczak_df) #Przekszta³cam na dataframe


# Normalnie nie da³o siê przkonwertowaæ danych na numeric, bo zamiast kropek by³y 
# przecinki i niemi³osiernie d³ugo siê nad tym g³owi³em. Ca³y czas otrzymywa³em 
# wartoœci NA
kurczak_dolnoslaskie = as.numeric(gsub(",", ".", as.character(kurczak_df$V1)))
kurczak_kujawsko_pomorskie = as.numeric(gsub(",", ".", as.character(kurczak_df$V2)))
kurczak_lubelskie = as.numeric(gsub(",", ".", as.character(kurczak_df$V3)))
kurczak_lubuskie = as.numeric(gsub(",", ".", as.character(kurczak_df$V4)))
kurczak_lodzkie = as.numeric(gsub(",", ".", as.character(kurczak_df$V5)))
kurczak_malopolskie = as.numeric(gsub(",", ".", as.character(kurczak_df$V6)))
kurczak_mazowieckie = as.numeric(gsub(",", ".", as.character(kurczak_df$V7)))
kurczak_opolskie = as.numeric(gsub(",", ".", as.character(kurczak_df$V8)))
kurczak_podkarpackie = as.numeric(gsub(",", ".", as.character(kurczak_df$V9)))
kurczak_podlaskie = as.numeric(gsub(",", ".", as.character(kurczak_df$V10)))
kurczak_pomorskie = as.numeric(gsub(",", ".", as.character(kurczak_df$V11)))
kurczak_slaskie = as.numeric(gsub(",", ".", as.character(kurczak_df$V12)))
kurczak_swietokrzyskie = as.numeric(gsub(",", ".", as.character(kurczak_df$V13)))
kurczak_warminsko_mazurskie = as.numeric(gsub(",", ".", as.character(kurczak_df$V14)))
kurczak_wielkopolskie = as.numeric(gsub(",", ".", as.character(kurczak_df$V15)))
kurczak_zachodniopomorskie = as.numeric(gsub(",", ".", as.character(kurczak_df$V16)))

#### Wykres cen kurczaka ####
y = (1:168)
#Wykres dla kurczaka i wszystkich województw
kurczak_plot = plot_ly(x = y )%>%
  layout(title = 'Zmiana cen kurczaka w latach 2006-2019', xaxis = list(title = 'Rok', 
        ticktext = list("2006", "2007", "2008", "2009", "2010", "2011","2012","2013","2014","2015","2016","2017","2018","2019"), 
        tickvals = list(1, 12, 24, 36, 48, 60, 72, 84, 96, 108, 120, 132, 144, 156)),
         yaxis = list(title = 'cena (z³)')) %>%
  add_lines(y = kurczak_dolnoslaskie, name = "dolnoœlaskie")%>%
  add_lines(y = kurczak_kujawsko_pomorskie, name = "kujawsko-pomorskie")%>%
  add_lines(y = kurczak_lubelskie, name = "lubelskie")%>%
  add_lines(y = kurczak_lubuskie, name = "lubuskie")%>%
  add_lines(y = kurczak_lodzkie, name = "³ódzkie")%>%
  add_lines(y = kurczak_malopolskie, name = "ma³opolskie")%>%
  add_lines(y = kurczak_mazowieckie, name = "mazowieckie")%>%
  add_lines(y = kurczak_opolskie, name = "opolskie")%>%
  add_lines(y = kurczak_podkarpackie, name = "podkarpackie")%>%
  add_lines(y = kurczak_podlaskie, name = "podlaskie")%>%
  add_lines(y = kurczak_pomorskie, name = "pomorskie")%>%
  add_lines(y = kurczak_slaskie, name = "œl¹skie")%>%
  add_lines(y = kurczak_swietokrzyskie, name = "œwietokrzyskie")%>%
  add_lines(y = kurczak_warminsko_mazurskie, name = "warmiñsko_mazurskie")%>%
  add_lines(y = kurczak_wielkopolskie, name = "wielkopolskie")%>%
  add_lines(y = kurczak_zachodniopomorskie, name = "zachodniopomorskie")
#print(kurczak_plot)

#### Œrednie roczne ceny kurczaka ####
roczne_sr_kurczak = data.frame('','','','','','','','','','','','','', stringsAsFactors = FALSE)
names(roczne_sr_kurczak) = c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016",
                             "2017","2018")

for (i in 1:13) {
  roczne_sr_kurczak[1,i] = round(mean(kurczak_dolnoslaskie[(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_kurczak[2,i] = round(mean(kurczak_kujawsko_pomorskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_kurczak[3,i] = round(mean(kurczak_lubelskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_kurczak[4,i] = round(mean(kurczak_lubuskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_kurczak[5,i] = round(mean(kurczak_lodzkie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_kurczak[6,i] = round(mean(kurczak_malopolskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_kurczak[7,i] = round(mean(kurczak_mazowieckie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_kurczak[8,i] = round(mean(kurczak_opolskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_kurczak[9,i] = round(mean(kurczak_podkarpackie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_kurczak[10,i] = round(mean(kurczak_podlaskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_kurczak[11,i] = round(mean(kurczak_pomorskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_kurczak[12,i] = round(mean(kurczak_slaskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_kurczak[13,i] = round(mean(kurczak_swietokrzyskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_kurczak[14,i] = round(mean(kurczak_warminsko_mazurskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_kurczak[15,i] = round(mean(kurczak_wielkopolskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_kurczak[16,i] = round(mean(kurczak_zachodniopomorskie[(1+(i-1)*12):(12*i)]), digits = 2)
}
roczne_sr_kurczak = t(roczne_sr_kurczak)
roczne_sr_kurczak = unname(roczne_sr_kurczak) #Usuwam zbêdne teraz nazwy
roczne_sr_kurczak = as.data.frame(roczne_sr_kurczak) #Przekszta³cam na dataframe

#### Œrednia cena kurczaka dla ka¿dego wojewódstwa ####
kurczak_dolnoslaskie_sr = round(mean(kurczak_dolnoslaskie), digits = 2)               #1
kurczak_kujawsko_pomorskie_sr = round(mean(kurczak_kujawsko_pomorskie), digits = 2)   #2
kurczak_lubelskie_sr = round(mean(kurczak_lubelskie), digits = 2)                     #3
kurczak_lubuskie_sr = round(mean(kurczak_lubuskie), digits = 2)                       #4
kurczak_lodzkie_sr = round(mean(kurczak_lodzkie), digits = 2)                         #5
kurczak_malopolskie_sr = round(mean(kurczak_malopolskie), digits = 2)                 #6
kurczak_mazowieckie_sr = round(mean(kurczak_mazowieckie), digits = 2)                 #7
kurczak_opolskie_sr = round(mean(kurczak_opolskie), digits = 2)                       #8
kurczak_podkarpackie_sr = round(mean(kurczak_podkarpackie), digits = 2)               #9
kurczak_podlaskie_sr = round(mean(kurczak_podlaskie), digits = 2)                     #10
kurczak_pomorskie_sr = round(mean(kurczak_pomorskie), digits = 2)                     #11
kurczak_slaskie_sr = round(mean(kurczak_slaskie), digits = 2)                         #12
kurczak_swietokrzyskie_sr = round(mean(kurczak_swietokrzyskie), digits = 2)           #13
kurczak_warminsko_mazurskie_sr = round(mean(kurczak_warminsko_mazurskie), digits = 2) #14
kurczak_wielkopolskie_sr = round(mean(kurczak_wielkopolskie), digits = 2)             #15
kurczak_zachodniopomorskie_sr = round(mean(kurczak_zachodniopomorskie), digits = 2)   #16



#### Kurczak œrednie, najw, najmn ceny w jednym wektorze ####
sr_cena_kurczak = c(kurczak_dolnoslaskie_sr,kurczak_kujawsko_pomorskie_sr,kurczak_lubelskie_sr,kurczak_lubuskie_sr,kurczak_lodzkie_sr,
                    kurczak_malopolskie_sr,kurczak_mazowieckie_sr,kurczak_opolskie_sr,kurczak_podkarpackie_sr,kurczak_podlaskie_sr,
                    kurczak_pomorskie_sr,kurczak_slaskie_sr,kurczak_swietokrzyskie_sr,kurczak_warminsko_mazurskie_sr,
                    kurczak_wielkopolskie_sr,kurczak_zachodniopomorskie_sr)

kurczak_najmn_w_kazdym_woj = c(min(kurczak_dolnoslaskie),min(kurczak_kujawsko_pomorskie),min(kurczak_lubelskie),
                               min(kurczak_lubuskie),min(kurczak_lodzkie),min(kurczak_malopolskie),
                               min(kurczak_mazowieckie),min(kurczak_opolskie),min(kurczak_podkarpackie),
                               min(kurczak_podlaskie),min(kurczak_pomorskie),min(kurczak_slaskie),
                               min(kurczak_swietokrzyskie),min(kurczak_warminsko_mazurskie),
                               min(kurczak_wielkopolskie),min(kurczak_zachodniopomorskie))

kurczak_najw_w_kazdym_woj = c(max(kurczak_dolnoslaskie),max(kurczak_kujawsko_pomorskie),max(kurczak_lubelskie),
                              max(kurczak_lubuskie),max(kurczak_lodzkie),max(kurczak_malopolskie),
                              max(kurczak_mazowieckie),max(kurczak_opolskie),max(kurczak_podkarpackie),
                              max(kurczak_podlaskie),max(kurczak_pomorskie),max(kurczak_slaskie),
                              max(kurczak_swietokrzyskie),max(kurczak_warminsko_mazurskie),
                              max(kurczak_wielkopolskie),max(kurczak_zachodniopomorskie))

###  Najwy¿sze i najni¿sze, ró¿ne

kurczak_sr_cena = mean(sr_cena_kurczak) #Œrednia cena ry¿u dla ca³ego kraju
kurczak_odsd_cena = sd(sr_cena_kurczak)  #Odchylenie standardowe ceny ry¿u

kurczak_najw_sr = max(sr_cena_kurczak) #Najwy¿sza œrednia cena ry¿u
kurczak_najm_sr = min(sr_cena_kurczak) #Najni¿sza œrednia cena ry¿u
kurczak_najmn_k = min(kurczak_najmn_w_kazdym_woj) #Najni¿sza cena kiedykolwiek
kurczak_najw_k = max(kurczak_najw_w_kazdym_woj) #Najwy¿sza cena kiedykolwiek


kur_najw_sr = as.numeric(match(kurczak_najw_sr,sr_cena_kurczak))  #Numer województwa
kur_najmn_sr = as.numeric(match(kurczak_najm_sr,sr_cena_kurczak)) #Numer województwa
print(paste("Najwy¿sza œrednia cena kurczaka jest w województwie", wojewodztwa[kur_najw_sr],
            "i wynosi",kurczak_najw_sr))
print(paste("Najni¿sza œrednia cena kurczaka jest w województwie", wojewodztwa[kur_najmn_sr],
            "i wynosi",kurczak_najm_sr))


kur_najmn_kazde = as.numeric(match(kurczak_najmn_k,kurczak_najmn_w_kazdym_woj))  #Numer województwa
kur_najw_kazde = as.numeric(match(kurczak_najw_k,kurczak_najw_w_kazdym_woj))     #Numer województwa
kur_najmn_kiedy =  match(kurczak_najmn_k,kurczak_zachodniopomorskie)             #Kiedy
kur_najw_kiedy =  match(kurczak_najw_k,kurczak_swietokrzyskie)                   #Kiedy

print(paste("Najni¿sza cena kurczaka by³a w", daty[kur_najmn_kiedy], "w województwie", 
            wojewodztwa[kur_najmn_kazde],"i wynosi³a", kurczak_najmn_k, "z³"))

print(paste("Najwy¿sza cena kurczaka by³a w", daty[kur_najw_kiedy], "w województwie",
            wojewodztwa[kur_najw_kazde],"i wynosi³a", kurczak_najw_k, "z³"))

print(paste("Odchylenie standardowe cen kurczaka wynosi", round(kurczak_odsd_cena, digits = 4), "z³otego"))

#### Korelacja p³acy minimalnej do œredniej ceny kurczaka ####
kurczak_placa_min_kor = c(cor(as.numeric(as.character(placa_min_df$V1)), as.numeric(as.character(roczne_sr_kurczak$V1))),
                          cor(as.numeric(as.character(placa_min_df$V2)), as.numeric(as.character(roczne_sr_kurczak$V2))),
                          cor(as.numeric(as.character(placa_min_df$V3)), as.numeric(as.character(roczne_sr_kurczak$V3))),
                          cor(as.numeric(as.character(placa_min_df$V4)), as.numeric(as.character(roczne_sr_kurczak$V4))),
                          cor(as.numeric(as.character(placa_min_df$V5)), as.numeric(as.character(roczne_sr_kurczak$V5))),
                          cor(as.numeric(as.character(placa_min_df$V6)), as.numeric(as.character(roczne_sr_kurczak$V6))),
                          cor(as.numeric(as.character(placa_min_df$V7)), as.numeric(as.character(roczne_sr_kurczak$V7))),
                          cor(as.numeric(as.character(placa_min_df$V8)), as.numeric(as.character(roczne_sr_kurczak$V8))),
                          cor(as.numeric(as.character(placa_min_df$V9)), as.numeric(as.character(roczne_sr_kurczak$V9))),
                          cor(as.numeric(as.character(placa_min_df$V10)), as.numeric(as.character(roczne_sr_kurczak$V10))),
                          cor(as.numeric(as.character(placa_min_df$V11)), as.numeric(as.character(roczne_sr_kurczak$V11))),
                          cor(as.numeric(as.character(placa_min_df$V12)), as.numeric(as.character(roczne_sr_kurczak$V12))),
                          cor(as.numeric(as.character(placa_min_df$V13)), as.numeric(as.character(roczne_sr_kurczak$V13))))


print(paste("Uœredniona korelacja minimalnej pensji do ceny kurczaka wynosi", round(mean(kurczak_placa_min_kor), 
                                                                                    digits = 4), "co wskazuje na umiarkowan¹ korelacjê"))
#### Korelacja œredniej pensji do œredniej ceny kurczaka ####

kurczak_sr_pensja_kor = c(cor(DOLNO, as.numeric(as.character(roczne_sr_kurczak$V1))),
                          cor(KUJAW, as.numeric(as.character(roczne_sr_kurczak$V2))),
                          cor(LUBEL, as.numeric(as.character(roczne_sr_kurczak$V3))),
                          cor(LUBUS, as.numeric(as.character(roczne_sr_kurczak$V4))),
                          cor(LODZK, as.numeric(as.character(roczne_sr_kurczak$V5))),
                          cor(MALOP, as.numeric(as.character(roczne_sr_kurczak$V6))),
                          cor(MAZOW, as.numeric(as.character(roczne_sr_kurczak$V7))),
                          cor(OPOLS, as.numeric(as.character(roczne_sr_kurczak$V8))),
                          cor(PODKA, as.numeric(as.character(roczne_sr_kurczak$V9))),
                          cor(PODLA, as.numeric(as.character(roczne_sr_kurczak$V10))),
                          cor(POMOR, as.numeric(as.character(roczne_sr_kurczak$V11))),
                          cor(SLASK, as.numeric(as.character(roczne_sr_kurczak$V12))),
                          cor(SWIET, as.numeric(as.character(roczne_sr_kurczak$V13))),
                          cor(WARMI, as.numeric(as.character(roczne_sr_kurczak$V14))),
                          cor(WIELK, as.numeric(as.character(roczne_sr_kurczak$V15))),
                          cor(ZACHO, as.numeric(as.character(roczne_sr_kurczak$V16))))
sr_kurczak_kor = mean(kurczak_sr_pensja_kor)
print(paste("Uœredniona korelacja œredniej pensji do ceny kurczaka wynosi", round(sr_kurczak_kor, 
                                                                                  digits = 4), "co wskazuje na umiarkowan¹ korelacjê"))
print(".........................................................")












#### Kie³basa edycja nag³ówków i tabel ####
kielbasa_wedzona_df = kielbasa_wedzona_df %>% remove_empty("cols") # Usuwam puste kolumny
kielbasa_wedzona_df_2 = kielbasa_wedzona_df_2 %>% remove_empty("cols") # Usuwam puste kolumny
kielbasa_wedzona_df_2 = select(kielbasa_wedzona_df_2, -contains('2016')) #Usuwam 2016 rok
kielbasa_wedzona_df = full_join(kielbasa_wedzona_df, kielbasa_wedzona_df_2) #£¹czê dataframy
rm(kielbasa_wedzona_df_2)
kielbasa_wedzona_df = subset(kielbasa_wedzona_df, select = -c(Kod,Nazwa))

#Edtujê nazwy kolumn na sensowne
names(kielbasa_wedzona_df) <- gsub("styczeñ.kie³basa.wêdzona...za.1kg.cena.", "styczeñ.", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("styczeñ.kie³basa.wêdzona..2....za.1kg.cena.", "styczeñ.", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("luty.kie³basa.wêdzona...za.1kg.cena.", "luty.", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("luty.kie³basa.wêdzona..2....za.1kg.cena.", "luty.", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("marzec.kie³basa.wêdzona...za.1kg.cena.", "marzec.", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("marzec.kie³basa.wêdzona..2....za.1kg.cena.", "marzec.", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("kwiecieñ.kie³basa.wêdzona...za.1kg.cena.", "kwiecieñ.", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("kwiecieñ.kie³basa.wêdzona..2....za.1kg.cena.", "kwiecieñ.", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("maj.kie³basa.wêdzona...za.1kg.cena.", "maj.", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("maj.kie³basa.wêdzona..2....za.1kg.cena.", "maj.", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("czerwiec.kie³basa.wêdzona...za.1kg.cena.", "czerwiec.", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("czerwiec.kie³basa.wêdzona..2....za.1kg.cena.", "czerwiec.", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("lipiec.kie³basa.wêdzona...za.1kg.cena.", "lipiec.", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("lipiec.kie³basa.wêdzona..2....za.1kg.cena.", "lipiec.", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("sierpieñ.kie³basa.wêdzona...za.1kg.cena.", "sierpieñ.", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("sierpieñ.kie³basa.wêdzona..2....za.1kg.cena.", "sierpieñ.", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("wrzesieñ.kie³basa.wêdzona...za.1kg.cena.", "wrzesieñ.", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("wrzesieñ.kie³basa.wêdzona..2....za.1kg.cena.", "wrzesieñ.", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("paŸdziernik.kie³basa.wêdzona...za.1kg.cena.", "paŸdziernik.", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("paŸdziernik.kie³basa.wêdzona..2....za.1kg.cena.", "paŸdziernik.", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("listopad.kie³basa.wêdzona...za.1kg.cena.", "listopad.", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("listopad.kie³basa.wêdzona..2....za.1kg.cena.", "listopad.", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("grudzieñ.kie³basa.wêdzona...za.1kg.cena.", "grudzieñ.", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("grudzieñ.kie³basa.wêdzona..2....za.1kg.cena.", "grudzieñ.", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("..z³.", "", names(kielbasa_wedzona_df))

#Nie znalaz³em inteligentnego sposobu na sensowne posortowanie chronologiczne wiêc robiê to ³opatologicznie
names(kielbasa_wedzona_df) <- gsub("styczeñ.2006", "2006 1 styczeñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("styczeñ.2007", "2007 1 styczeñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("styczeñ.2008", "2008 1 styczeñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("styczeñ.2009", "2009 1 styczeñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("styczeñ.2010", "2010 1 styczeñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("styczeñ.2011", "2011 1 styczeñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("styczeñ.2012", "2012 1 styczeñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("styczeñ.2013", "2013 1 styczeñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("styczeñ.2014", "2014 1 styczeñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("styczeñ.2015", "2015 1 styczeñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("styczeñ.2016", "2016 1 styczeñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("styczeñ.2017", "2017 1 styczeñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("styczeñ.2018", "2018 1 styczeñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("styczeñ.2019", "2019 1 styczeñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("luty.2006", "2006 2 luty", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("luty.2007", "2007 2 luty", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("luty.2008", "2008 2 luty", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("luty.2009", "2009 2 luty", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("luty.2010", "2010 2 luty", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("luty.2011", "2011 2 luty", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("luty.2012", "2012 2 luty", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("luty.2013", "2013 2 luty", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("luty.2014", "2014 2 luty", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("luty.2015", "2015 2 luty", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("luty.2016", "2016 2 luty", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("luty.2017", "2017 2 luty", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("luty.2018", "2018 2 luty", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("luty.2019", "2019 2 luty", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("marzec.2006", "2006 3 marzec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("marzec.2007", "2007 3 marzec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("marzec.2008", "2008 3 marzec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("marzec.2009", "2009 3 marzec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("marzec.2010", "2010 3 marzec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("marzec.2011", "2011 3 marzec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("marzec.2012", "2012 3 marzec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("marzec.2013", "2013 3 marzec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("marzec.2014", "2014 3 marzec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("marzec.2015", "2015 3 marzec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("marzec.2016", "2016 3 marzec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("marzec.2017", "2017 3 marzec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("marzec.2018", "2018 3 marzec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("marzec.2019", "2019 3 marzec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("kwiecieñ.2006", "2006 4 kwiecieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("kwiecieñ.2007", "2007 4 kwiecieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("kwiecieñ.2008", "2008 4 kwiecieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("kwiecieñ.2009", "2009 4 kwiecieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("kwiecieñ.2010", "2010 4 kwiecieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("kwiecieñ.2011", "2011 4 kwiecieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("kwiecieñ.2012", "2012 4 kwiecieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("kwiecieñ.2013", "2013 4 kwiecieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("kwiecieñ.2014", "2014 4 kwiecieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("kwiecieñ.2015", "2015 4 kwiecieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("kwiecieñ.2016", "2016 4 kwiecieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("kwiecieñ.2017", "2017 4 kwiecieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("kwiecieñ.2018", "2018 4 kwiecieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("kwiecieñ.2019", "2019 4 kwiecieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("maj.2006", "2006 5 maj", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("maj.2007", "2007 5 maj", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("maj.2008", "2008 5 maj", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("maj.2009", "2009 5 maj", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("maj.2010", "2010 5 maj", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("maj.2011", "2011 5 maj", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("maj.2012", "2012 5 maj", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("maj.2013", "2013 5 maj", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("maj.2014", "2014 5 maj", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("maj.2015", "2015 5 maj", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("maj.2016", "2016 5 maj", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("maj.2017", "2017 5 maj", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("maj.2018", "2018 5 maj", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("maj.2019", "2019 5 maj", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("czerwiec.2006", "2006 6 czerwiec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("czerwiec.2007", "2007 6 czerwiec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("czerwiec.2008", "2008 6 czerwiec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("czerwiec.2009", "2009 6 czerwiec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("czerwiec.2010", "2010 6 czerwiec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("czerwiec.2011", "2011 6 czerwiec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("czerwiec.2012", "2012 6 czerwiec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("czerwiec.2013", "2013 6 czerwiec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("czerwiec.2014", "2014 6 czerwiec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("czerwiec.2015", "2015 6 czerwiec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("czerwiec.2016", "2016 6 czerwiec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("czerwiec.2017", "2017 6 czerwiec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("czerwiec.2018", "2018 6 czerwiec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("czerwiec.2019", "2019 6 czerwiec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("lipiec.2006", "2006 7 lipiec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("lipiec.2007", "2007 7 lipiec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("lipiec.2008", "2008 7 lipiec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("lipiec.2009", "2009 7 lipiec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("lipiec.2010", "2010 7 lipiec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("lipiec.2011", "2011 7 lipiec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("lipiec.2012", "2012 7 lipiec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("lipiec.2013", "2013 7 lipiec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("lipiec.2014", "2014 7 lipiec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("lipiec.2015", "2015 7 lipiec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("lipiec.2016", "2016 7 lipiec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("lipiec.2017", "2017 7 lipiec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("lipiec.2018", "2018 7 lipiec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("lipiec.2019", "2019 7 lipiec", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("sierpieñ.2006", "2006 8 sierpieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("sierpieñ.2007", "2007 8 sierpieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("sierpieñ.2008", "2008 8 sierpieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("sierpieñ.2009", "2009 8 sierpieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("sierpieñ.2010", "2010 8 sierpieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("sierpieñ.2011", "2011 8 sierpieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("sierpieñ.2012", "2012 8 sierpieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("sierpieñ.2013", "2013 8 sierpieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("sierpieñ.2014", "2014 8 sierpieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("sierpieñ.2015", "2015 8 sierpieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("sierpieñ.2016", "2016 8 sierpieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("sierpieñ.2017", "2017 8 sierpieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("sierpieñ.2018", "2018 8 sierpieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("sierpieñ.2019", "2019 8 sierpieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("wrzesieñ.2006", "2006 9 wrzesieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("wrzesieñ.2007", "2007 9 wrzesieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("wrzesieñ.2008", "2008 9 wrzesieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("wrzesieñ.2009", "2009 9 wrzesieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("wrzesieñ.2010", "2010 9 wrzesieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("wrzesieñ.2011", "2011 9 wrzesieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("wrzesieñ.2012", "2012 9 wrzesieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("wrzesieñ.2013", "2013 9 wrzesieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("wrzesieñ.2014", "2014 9 wrzesieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("wrzesieñ.2015", "2015 9 wrzesieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("wrzesieñ.2016", "2016 9 wrzesieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("wrzesieñ.2017", "2017 9 wrzesieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("wrzesieñ.2018", "2018 9 wrzesieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("wrzesieñ.2019", "2019 9 wrzesieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("paŸdziernik.2006", "2006 90 paŸdziernik", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("paŸdziernik.2007", "2007 90 paŸdziernik", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("paŸdziernik.2008", "2008 90 paŸdziernik", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("paŸdziernik.2009", "2009 90 paŸdziernik", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("paŸdziernik.2010", "2010 90 paŸdziernik", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("paŸdziernik.2011", "2011 90 paŸdziernik", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("paŸdziernik.2012", "2012 90 paŸdziernik", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("paŸdziernik.2013", "2013 90 paŸdziernik", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("paŸdziernik.2014", "2014 90 paŸdziernik", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("paŸdziernik.2015", "2015 90 paŸdziernik", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("paŸdziernik.2016", "2016 90 paŸdziernik", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("paŸdziernik.2017", "2017 90 paŸdziernik", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("paŸdziernik.2018", "2018 90 paŸdziernik", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("paŸdziernik.2019", "2019 90 paŸdziernik", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("listopad.2006", "2006 91 listopad", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("listopad.2007", "2007 91 listopad", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("listopad.2008", "2008 91 listopad", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("listopad.2009", "2009 91 listopad", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("listopad.2010", "2010 91 listopad", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("listopad.2011", "2011 91 listopad", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("listopad.2012", "2012 91 listopad", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("listopad.2013", "2013 91 listopad", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("listopad.2014", "2014 91 listopad", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("listopad.2015", "2015 91 listopad", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("listopad.2016", "2016 91 listopad", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("listopad.2017", "2017 91 listopad", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("listopad.2018", "2018 91 listopad", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("listopad.2019", "2019 91 listopad", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("grudzieñ.2006", "2006 92 grudzieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("grudzieñ.2007", "2007 92 grudzieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("grudzieñ.2008", "2008 92 grudzieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("grudzieñ.2009", "2009 92 grudzieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("grudzieñ.2010", "2010 92 grudzieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("grudzieñ.2011", "2011 92 grudzieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("grudzieñ.2012", "2012 92 grudzieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("grudzieñ.2013", "2013 92 grudzieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("grudzieñ.2014", "2014 92 grudzieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("grudzieñ.2015", "2015 92 grudzieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("grudzieñ.2016", "2016 92 grudzieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("grudzieñ.2017", "2017 92 grudzieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("grudzieñ.2018", "2018 92 grudzieñ", names(kielbasa_wedzona_df))
names(kielbasa_wedzona_df) <- gsub("grudzieñ.2019", "2019 92 grudzieñ", names(kielbasa_wedzona_df))
kielbasa_wedzona_df = kielbasa_wedzona_df[sort(colnames(kielbasa_wedzona_df))] #Sortujê chronologicznie
#kielbasa_wedzona_df = subset(kielbasa_wedzona_df, select = -c(Nazwa) ) # Usuwam zbêdn¹ kolumnê z nazwazmi województw


kielbasa_wedzona_df = t(kielbasa_wedzona_df) #Tramsponujê dataframe ¿eby poszczególne kolumny by³y województwami 
kielbasa_wedzona_df = unname(kielbasa_wedzona_df) #Usuwam zbêdne teraz nazwy
kielbasa_wedzona_df = as.data.frame(kielbasa_wedzona_df) #Przekszta³cam na dataframe


# Normalnie nie da³o siê przkonwertowaæ danych na numeric, bo zamiast kropek by³y 
# przecinki i niemi³osiernie d³ugo siê nad tym g³owi³em. Ca³y czas otrzymywa³em 
# wartoœci NA
kielbasa_wedzona_dolnoslaskie = as.numeric(gsub(",", ".", as.character(kielbasa_wedzona_df$V1)))
kielbasa_wedzona_kujawsko_pomorskie = as.numeric(gsub(",", ".", as.character(kielbasa_wedzona_df$V2)))
kielbasa_wedzona_lubelskie = as.numeric(gsub(",", ".", as.character(kielbasa_wedzona_df$V3)))
kielbasa_wedzona_lubuskie = as.numeric(gsub(",", ".", as.character(kielbasa_wedzona_df$V4)))
kielbasa_wedzona_lodzkie = as.numeric(gsub(",", ".", as.character(kielbasa_wedzona_df$V5)))
kielbasa_wedzona_malopolskie = as.numeric(gsub(",", ".", as.character(kielbasa_wedzona_df$V6)))
kielbasa_wedzona_mazowieckie = as.numeric(gsub(",", ".", as.character(kielbasa_wedzona_df$V7)))
kielbasa_wedzona_opolskie = as.numeric(gsub(",", ".", as.character(kielbasa_wedzona_df$V8)))
kielbasa_wedzona_podkarpackie = as.numeric(gsub(",", ".", as.character(kielbasa_wedzona_df$V9)))
kielbasa_wedzona_podlaskie = as.numeric(gsub(",", ".", as.character(kielbasa_wedzona_df$V10)))
kielbasa_wedzona_pomorskie = as.numeric(gsub(",", ".", as.character(kielbasa_wedzona_df$V11)))
kielbasa_wedzona_slaskie = as.numeric(gsub(",", ".", as.character(kielbasa_wedzona_df$V12)))
kielbasa_wedzona_swietokrzyskie = as.numeric(gsub(",", ".", as.character(kielbasa_wedzona_df$V13)))
kielbasa_wedzona_warminsko_mazurskie = as.numeric(gsub(",", ".", as.character(kielbasa_wedzona_df$V14)))
kielbasa_wedzona_wielkopolskie = as.numeric(gsub(",", ".", as.character(kielbasa_wedzona_df$V15)))
kielbasa_wedzona_zachodniopomorskie = as.numeric(gsub(",", ".", as.character(kielbasa_wedzona_df$V16)))

#### Wykres cen kie³basy wêdzonej ####
y = (1:168)
#Wykres dla kie³basy i wszystkich województw
kielbasa_plot = plot_ly(x = y )%>%
  layout(title = 'Zmiana cen kie³basy wêdzonej w latach 2006-2019', xaxis = list(title = 'Rok', 
      ticktext = list("2006", "2007", "2008", "2009", "2010", "2011","2012","2013","2014","2015","2016","2017","2018","2019"), 
        tickvals = list(1, 12, 24, 36, 48, 60, 72, 84, 96, 108, 120, 132, 144, 156)),
         yaxis = list(title = 'cena (z³)')) %>%
  add_lines(y = kielbasa_wedzona_dolnoslaskie, name = "dolnoœlaskie")%>%
  add_lines(y = kielbasa_wedzona_kujawsko_pomorskie, name = "kujawsko-pomorskie")%>%
  add_lines(y = kielbasa_wedzona_lubelskie, name = "lubelskie")%>%
  add_lines(y = kielbasa_wedzona_lubuskie, name = "lubuskie")%>%
  add_lines(y = kielbasa_wedzona_lodzkie, name = "³ódzkie")%>%
  add_lines(y = kielbasa_wedzona_malopolskie, name = "ma³opolskie")%>%
  add_lines(y = kielbasa_wedzona_mazowieckie, name = "mazowieckie")%>%
  add_lines(y = kielbasa_wedzona_opolskie, name = "opolskie")%>%
  add_lines(y = kielbasa_wedzona_podkarpackie, name = "podkarpackie")%>%
  add_lines(y = kielbasa_wedzona_podlaskie, name = "podlaskie")%>%
  add_lines(y = kielbasa_wedzona_pomorskie, name = "pomorskie")%>%
  add_lines(y = kielbasa_wedzona_slaskie, name = "œl¹skie")%>%
  add_lines(y = kielbasa_wedzona_swietokrzyskie, name = "œwietokrzyskie")%>%
  add_lines(y = kielbasa_wedzona_warminsko_mazurskie, name = "warmiñsko_mazurskie")%>%
  add_lines(y = kielbasa_wedzona_wielkopolskie, name = "wielkopolskie")%>%
  add_lines(y = kielbasa_wedzona_zachodniopomorskie, name = "zachodniopomorskie")
#print(kielbasa_plot)

#### Œrednie roczne ceny kie³basy wêdzonej ####
roczne_sr_kielbasa = data.frame('','','','','','','','','','','','','', stringsAsFactors = FALSE)
names(roczne_sr_kielbasa) = c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016",
                              "2017","2018")

for (i in 1:13) {
  roczne_sr_kielbasa[1,i] = round(mean(kielbasa_wedzona_dolnoslaskie[(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_kielbasa[2,i] = round(mean(kielbasa_wedzona_kujawsko_pomorskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_kielbasa[3,i] = round(mean(kielbasa_wedzona_lubelskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_kielbasa[4,i] = round(mean(kielbasa_wedzona_lubuskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_kielbasa[5,i] = round(mean(kielbasa_wedzona_lodzkie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_kielbasa[6,i] = round(mean(kielbasa_wedzona_malopolskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_kielbasa[7,i] = round(mean(kielbasa_wedzona_mazowieckie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_kielbasa[8,i] = round(mean(kielbasa_wedzona_opolskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_kielbasa[9,i] = round(mean(kielbasa_wedzona_podkarpackie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_kielbasa[10,i] = round(mean(kielbasa_wedzona_podlaskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_kielbasa[11,i] = round(mean(kielbasa_wedzona_pomorskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_kielbasa[12,i] = round(mean(kielbasa_wedzona_slaskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_kielbasa[13,i] = round(mean(kielbasa_wedzona_swietokrzyskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_kielbasa[14,i] = round(mean(kielbasa_wedzona_warminsko_mazurskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_kielbasa[15,i] = round(mean(kielbasa_wedzona_wielkopolskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_kielbasa[16,i] = round(mean(kielbasa_wedzona_zachodniopomorskie[(1+(i-1)*12):(12*i)]), digits = 2)
}
roczne_sr_kielbasa = t(roczne_sr_kielbasa)
roczne_sr_kielbasa = unname(roczne_sr_kielbasa) #Usuwam zbêdne teraz nazwy
roczne_sr_kielbasa = as.data.frame(roczne_sr_kielbasa) #Przekszta³cam na dataframe

#### Œrednia cena kie³basy wêdzonej dla ka¿dego wojewódstwa ####
kielbasa_dolnoslaskie_sr = round(mean(kielbasa_wedzona_dolnoslaskie), digits = 2)               #1
kielbasa_kujawsko_pomorskie_sr = round(mean(kielbasa_wedzona_kujawsko_pomorskie), digits = 2)   #2
kielbasa_lubelskie_sr = round(mean(kielbasa_wedzona_lubelskie), digits = 2)                     #3
kielbasa_lubuskie_sr = round(mean(kielbasa_wedzona_lubuskie), digits = 2)                       #4
kielbasa_lodzkie_sr = round(mean(kielbasa_wedzona_lodzkie), digits = 2)                         #5
kielbasa_malopolskie_sr = round(mean(kielbasa_wedzona_malopolskie), digits = 2)                 #6
kielbasa_mazowieckie_sr = round(mean(kielbasa_wedzona_mazowieckie), digits = 2)                 #7
kielbasa_opolskie_sr = round(mean(kielbasa_wedzona_opolskie), digits = 2)                       #8
kielbasa_podkarpackie_sr = round(mean(kielbasa_wedzona_podkarpackie), digits = 2)               #9
kielbasa_podlaskie_sr = round(mean(kielbasa_wedzona_podlaskie), digits = 2)                     #10
kielbasa_pomorskie_sr = round(mean(kielbasa_wedzona_pomorskie), digits = 2)                     #11
kielbasa_slaskie_sr = round(mean(kielbasa_wedzona_slaskie), digits = 2)                         #12
kielbasa_swietokrzyskie_sr = round(mean(kielbasa_wedzona_swietokrzyskie), digits = 2)           #13
kielbasa_warminsko_mazurskie_sr = round(mean(kielbasa_wedzona_warminsko_mazurskie), digits = 2) #14
kielbasa_wielkopolskie_sr = round(mean(kielbasa_wedzona_wielkopolskie), digits = 2)             #15
kielbasa_zachodniopomorskie_sr = round(mean(kielbasa_wedzona_zachodniopomorskie), digits = 2)   #16



#### Kie³basa wêdzona œrednie, najw, najmn ceny w jednym wektorze ####
sr_cena_kielbasa = c(kielbasa_dolnoslaskie_sr,kielbasa_kujawsko_pomorskie_sr,kielbasa_lubelskie_sr,kielbasa_lubuskie_sr,kielbasa_lodzkie_sr,
                     kielbasa_malopolskie_sr,kielbasa_mazowieckie_sr,kielbasa_opolskie_sr,kielbasa_podkarpackie_sr,kielbasa_podlaskie_sr,
                     kielbasa_pomorskie_sr,kielbasa_slaskie_sr,kielbasa_swietokrzyskie_sr,kielbasa_warminsko_mazurskie_sr,
                     kielbasa_wielkopolskie_sr,kielbasa_zachodniopomorskie_sr)

kielbasa_najmn_w_kazdym_woj = c(min(kielbasa_wedzona_dolnoslaskie),min(kielbasa_wedzona_kujawsko_pomorskie),min(kielbasa_wedzona_lubelskie),
                                min(kielbasa_wedzona_lubuskie),min(kielbasa_wedzona_lodzkie),min(kielbasa_wedzona_malopolskie),
                                min(kielbasa_wedzona_mazowieckie),min(kielbasa_wedzona_opolskie),min(kielbasa_wedzona_podkarpackie),
                                min(kielbasa_wedzona_podlaskie),min(kielbasa_wedzona_pomorskie),min(kielbasa_wedzona_slaskie),
                                min(kielbasa_wedzona_swietokrzyskie),min(kielbasa_wedzona_warminsko_mazurskie),
                                min(kielbasa_wedzona_wielkopolskie),min(kielbasa_wedzona_zachodniopomorskie))

kielbasa_najw_w_kazdym_woj = c(max(kielbasa_wedzona_dolnoslaskie),max(kielbasa_wedzona_kujawsko_pomorskie),max(kielbasa_wedzona_lubelskie),
                               max(kielbasa_wedzona_lubuskie),max(kielbasa_wedzona_lodzkie),max(kielbasa_wedzona_malopolskie),
                               max(kielbasa_wedzona_mazowieckie),max(kielbasa_wedzona_opolskie),max(kielbasa_wedzona_podkarpackie),
                               max(kielbasa_wedzona_podlaskie),max(kielbasa_wedzona_pomorskie),max(kielbasa_wedzona_slaskie),
                               max(kielbasa_wedzona_swietokrzyskie),max(kielbasa_wedzona_warminsko_mazurskie),
                               max(kielbasa_wedzona_wielkopolskie),max(kielbasa_wedzona_zachodniopomorskie))

###  Najwy¿sze i najni¿sze, ró¿ne

kielbasa_sr_cena = mean(sr_cena_kielbasa) #Œrednia cena kie³basy dla ca³ego kraju
kielbasa_odsd_cena = sd(sr_cena_kielbasa)  #Odchylenie standardowe ceny kie³basy

kielbasa_najw_sr = max(sr_cena_kielbasa) #Najwy¿sza œrednia cena kie³basy
kielbasa_najm_sr = min(sr_cena_kielbasa) #Najni¿sza œrednia cena kie³basy
kielbasa_najmn_k = min(kielbasa_najmn_w_kazdym_woj) #Najni¿sza cena kie³basy kiedykolwiek
kielbasa_najw_k = max(kielbasa_najw_w_kazdym_woj)   #Najwy¿sza cena kie³basy kiedykolwiek


kiel_najw_sr = as.numeric(match(kielbasa_najw_sr,sr_cena_kielbasa))  #Numer województwa
kiel_najmn_sr = as.numeric(match(kielbasa_najm_sr,sr_cena_kielbasa)) #Numer województwa
print(paste("Najwy¿sza œrednia cena kie³basy jest w województwie", wojewodztwa[kiel_najw_sr],
            "i wynosi",kielbasa_najw_sr))
print(paste("Najni¿sza œrednia cena kie³basy jest w województwie", wojewodztwa[kiel_najmn_sr],
            "i wynosi",kielbasa_najm_sr))


kiel_najmn_kazde = as.numeric(match(kielbasa_najmn_k, kielbasa_najmn_w_kazdym_woj))        #Numer województwa
kiel_najw_kazde = as.numeric(match(kielbasa_najw_k,   kielbasa_najw_w_kazdym_woj))         #Numer województwa
kiel_najmn_kiedy =  match(kielbasa_najmn_k,           kielbasa_wedzona_wielkopolskie)      #Kiedy
kiel_najw_kiedy =  match(kielbasa_najw_k,             kielbasa_wedzona_zachodniopomorskie) #Kiedy

print(paste("Najni¿sza cena kie³basy by³a w", daty[kiel_najmn_kiedy], "w województwie", 
            wojewodztwa[kiel_najmn_kazde],"i wynosi³a", kielbasa_najmn_k, "z³"))

print(paste("Najwy¿sza cena kie³basy by³a w", daty[kiel_najw_kiedy], "w województwie",
            wojewodztwa[kiel_najw_kazde],"i wynosi³a", kielbasa_najw_k, "z³"))

print(paste("Odchylenie standardowe cen kie³basy wynosi", round(kielbasa_odsd_cena, digits = 2), "z³otego"))

#### Korelacja p³acy minimalnej do œredniej ceny kie³basy wêdzonej ####
kielbasa_placa_min_kor = c(cor(as.numeric(as.character(placa_min_df$V1)), as.numeric(as.character(roczne_sr_kielbasa$V1))),
                           cor(as.numeric(as.character(placa_min_df$V2)), as.numeric(as.character(roczne_sr_kielbasa$V2))),
                           cor(as.numeric(as.character(placa_min_df$V3)), as.numeric(as.character(roczne_sr_kielbasa$V3))),
                           cor(as.numeric(as.character(placa_min_df$V4)), as.numeric(as.character(roczne_sr_kielbasa$V4))),
                           cor(as.numeric(as.character(placa_min_df$V5)), as.numeric(as.character(roczne_sr_kielbasa$V5))),
                           cor(as.numeric(as.character(placa_min_df$V6)), as.numeric(as.character(roczne_sr_kielbasa$V6))),
                           cor(as.numeric(as.character(placa_min_df$V7)), as.numeric(as.character(roczne_sr_kielbasa$V7))),
                           cor(as.numeric(as.character(placa_min_df$V8)), as.numeric(as.character(roczne_sr_kielbasa$V8))),
                           cor(as.numeric(as.character(placa_min_df$V9)), as.numeric(as.character(roczne_sr_kielbasa$V9))),
                           cor(as.numeric(as.character(placa_min_df$V10)), as.numeric(as.character(roczne_sr_kielbasa$V10))),
                           cor(as.numeric(as.character(placa_min_df$V11)), as.numeric(as.character(roczne_sr_kielbasa$V11))),
                           cor(as.numeric(as.character(placa_min_df$V12)), as.numeric(as.character(roczne_sr_kielbasa$V12))),
                           cor(as.numeric(as.character(placa_min_df$V13)), as.numeric(as.character(roczne_sr_kielbasa$V13))))


print(paste("Uœredniona korelacja minimalnej pensji do ceny kielbasy wynosi", round(mean(kielbasa_placa_min_kor), 
                                                                                    digits = 4), "co wskazuje na bardzo siln¹ korelacjê"))
#### Korelacja œredniej pensji do œredniej ceny kie³basy wêdzonej ####

kielbasa_sr_pensja_kor = c(cor(DOLNO, as.numeric(as.character(roczne_sr_kielbasa$V1))),
                           cor(KUJAW, as.numeric(as.character(roczne_sr_kielbasa$V2))),
                           cor(LUBEL, as.numeric(as.character(roczne_sr_kielbasa$V3))),
                           cor(LUBUS, as.numeric(as.character(roczne_sr_kielbasa$V4))),
                           cor(LODZK, as.numeric(as.character(roczne_sr_kielbasa$V5))),
                           cor(MALOP, as.numeric(as.character(roczne_sr_kielbasa$V6))),
                           cor(MAZOW, as.numeric(as.character(roczne_sr_kielbasa$V7))),
                           cor(OPOLS, as.numeric(as.character(roczne_sr_kielbasa$V8))),
                           cor(PODKA, as.numeric(as.character(roczne_sr_kielbasa$V9))),
                           cor(PODLA, as.numeric(as.character(roczne_sr_kielbasa$V10))),
                           cor(POMOR, as.numeric(as.character(roczne_sr_kielbasa$V11))),
                           cor(SLASK, as.numeric(as.character(roczne_sr_kielbasa$V12))),
                           cor(SWIET, as.numeric(as.character(roczne_sr_kielbasa$V13))),
                           cor(WARMI, as.numeric(as.character(roczne_sr_kielbasa$V14))),
                           cor(WIELK, as.numeric(as.character(roczne_sr_kielbasa$V15))),
                           cor(ZACHO, as.numeric(as.character(roczne_sr_kielbasa$V16))))
sr_kielbasa_kor = mean(kielbasa_sr_pensja_kor)
print(paste("Uœredniona korelacja œredniej pensji do ceny kielbasy wynosi", round(sr_kielbasa_kor, 
                                                                                  digits = 4), "co wskazuje na bardzo siln¹ korelacjê"))
print(".........................................................")





#### Œmietana edycja nag³ówków i tabel ####
smietana_df = smietana_df %>% remove_empty("cols") # Usuwam puste kolumny
smietana_df = subset(smietana_df, select = -c(Kod,Nazwa))

#Edtujê nazwy kolumn na sensowne
names(smietana_df) <- gsub("styczeñ.œmietana.o.zawartoœci.t³uszczu.18....za.200.g.cena.", "styczeñ.", names(smietana_df))
names(smietana_df) <- gsub("luty.œmietana.o.zawartoœci.t³uszczu.18....za.200.g.cena.", "luty.", names(smietana_df))
names(smietana_df) <- gsub("marzec.œmietana.o.zawartoœci.t³uszczu.18....za.200.g.cena.", "marzec.", names(smietana_df))
names(smietana_df) <- gsub("kwiecieñ.œmietana.o.zawartoœci.t³uszczu.18....za.200.g.cena.", "kwiecieñ.", names(smietana_df))
names(smietana_df) <- gsub("maj.œmietana.o.zawartoœci.t³uszczu.18....za.200.g.cena.", "maj.", names(smietana_df))
names(smietana_df) <- gsub("czerwiec.œmietana.o.zawartoœci.t³uszczu.18....za.200.g.cena.", "czerwiec.", names(smietana_df))
names(smietana_df) <- gsub("lipiec.œmietana.o.zawartoœci.t³uszczu.18....za.200.g.cena.", "lipiec.", names(smietana_df))
names(smietana_df) <- gsub("sierpieñ.œmietana.o.zawartoœci.t³uszczu.18....za.200.g.cena.", "sierpieñ.", names(smietana_df))
names(smietana_df) <- gsub("wrzesieñ.œmietana.o.zawartoœci.t³uszczu.18....za.200.g.cena.", "wrzesieñ.", names(smietana_df))
names(smietana_df) <- gsub("paŸdziernik.œmietana.o.zawartoœci.t³uszczu.18....za.200.g.cena.", "paŸdziernik.", names(smietana_df))
names(smietana_df) <- gsub("listopad.œmietana.o.zawartoœci.t³uszczu.18....za.200.g.cena.", "listopad.", names(smietana_df))
names(smietana_df) <- gsub("grudzieñ.œmietana.o.zawartoœci.t³uszczu.18....za.200.g.cena.", "grudzieñ.", names(smietana_df))
names(smietana_df) <- gsub("..z³.", "", names(smietana_df))

#Nie znalaz³em inteligentnego sposobu na sensowne posortowanie chronologiczne wiêc robiê to ³opatologicznie
names(smietana_df) <- gsub("styczeñ.2006", "2006 1 styczeñ", names(smietana_df))
names(smietana_df) <- gsub("styczeñ.2007", "2007 1 styczeñ", names(smietana_df))
names(smietana_df) <- gsub("styczeñ.2008", "2008 1 styczeñ", names(smietana_df))
names(smietana_df) <- gsub("styczeñ.2009", "2009 1 styczeñ", names(smietana_df))
names(smietana_df) <- gsub("styczeñ.2010", "2010 1 styczeñ", names(smietana_df))
names(smietana_df) <- gsub("styczeñ.2011", "2011 1 styczeñ", names(smietana_df))
names(smietana_df) <- gsub("styczeñ.2012", "2012 1 styczeñ", names(smietana_df))
names(smietana_df) <- gsub("styczeñ.2013", "2013 1 styczeñ", names(smietana_df))
names(smietana_df) <- gsub("styczeñ.2014", "2014 1 styczeñ", names(smietana_df))
names(smietana_df) <- gsub("styczeñ.2015", "2015 1 styczeñ", names(smietana_df))
names(smietana_df) <- gsub("styczeñ.2016", "2016 1 styczeñ", names(smietana_df))
names(smietana_df) <- gsub("styczeñ.2017", "2017 1 styczeñ", names(smietana_df))
names(smietana_df) <- gsub("styczeñ.2018", "2018 1 styczeñ", names(smietana_df))
names(smietana_df) <- gsub("styczeñ.2019", "2019 1 styczeñ", names(smietana_df))
names(smietana_df) <- gsub("luty.2006", "2006 2 luty", names(smietana_df))
names(smietana_df) <- gsub("luty.2007", "2007 2 luty", names(smietana_df))
names(smietana_df) <- gsub("luty.2008", "2008 2 luty", names(smietana_df))
names(smietana_df) <- gsub("luty.2009", "2009 2 luty", names(smietana_df))
names(smietana_df) <- gsub("luty.2010", "2010 2 luty", names(smietana_df))
names(smietana_df) <- gsub("luty.2011", "2011 2 luty", names(smietana_df))
names(smietana_df) <- gsub("luty.2012", "2012 2 luty", names(smietana_df))
names(smietana_df) <- gsub("luty.2013", "2013 2 luty", names(smietana_df))
names(smietana_df) <- gsub("luty.2014", "2014 2 luty", names(smietana_df))
names(smietana_df) <- gsub("luty.2015", "2015 2 luty", names(smietana_df))
names(smietana_df) <- gsub("luty.2016", "2016 2 luty", names(smietana_df))
names(smietana_df) <- gsub("luty.2017", "2017 2 luty", names(smietana_df))
names(smietana_df) <- gsub("luty.2018", "2018 2 luty", names(smietana_df))
names(smietana_df) <- gsub("luty.2019", "2019 2 luty", names(smietana_df))
names(smietana_df) <- gsub("marzec.2006", "2006 3 marzec", names(smietana_df))
names(smietana_df) <- gsub("marzec.2007", "2007 3 marzec", names(smietana_df))
names(smietana_df) <- gsub("marzec.2008", "2008 3 marzec", names(smietana_df))
names(smietana_df) <- gsub("marzec.2009", "2009 3 marzec", names(smietana_df))
names(smietana_df) <- gsub("marzec.2010", "2010 3 marzec", names(smietana_df))
names(smietana_df) <- gsub("marzec.2011", "2011 3 marzec", names(smietana_df))
names(smietana_df) <- gsub("marzec.2012", "2012 3 marzec", names(smietana_df))
names(smietana_df) <- gsub("marzec.2013", "2013 3 marzec", names(smietana_df))
names(smietana_df) <- gsub("marzec.2014", "2014 3 marzec", names(smietana_df))
names(smietana_df) <- gsub("marzec.2015", "2015 3 marzec", names(smietana_df))
names(smietana_df) <- gsub("marzec.2016", "2016 3 marzec", names(smietana_df))
names(smietana_df) <- gsub("marzec.2017", "2017 3 marzec", names(smietana_df))
names(smietana_df) <- gsub("marzec.2018", "2018 3 marzec", names(smietana_df))
names(smietana_df) <- gsub("marzec.2019", "2019 3 marzec", names(smietana_df))
names(smietana_df) <- gsub("kwiecieñ.2006", "2006 4 kwiecieñ", names(smietana_df))
names(smietana_df) <- gsub("kwiecieñ.2007", "2007 4 kwiecieñ", names(smietana_df))
names(smietana_df) <- gsub("kwiecieñ.2008", "2008 4 kwiecieñ", names(smietana_df))
names(smietana_df) <- gsub("kwiecieñ.2009", "2009 4 kwiecieñ", names(smietana_df))
names(smietana_df) <- gsub("kwiecieñ.2010", "2010 4 kwiecieñ", names(smietana_df))
names(smietana_df) <- gsub("kwiecieñ.2011", "2011 4 kwiecieñ", names(smietana_df))
names(smietana_df) <- gsub("kwiecieñ.2012", "2012 4 kwiecieñ", names(smietana_df))
names(smietana_df) <- gsub("kwiecieñ.2013", "2013 4 kwiecieñ", names(smietana_df))
names(smietana_df) <- gsub("kwiecieñ.2014", "2014 4 kwiecieñ", names(smietana_df))
names(smietana_df) <- gsub("kwiecieñ.2015", "2015 4 kwiecieñ", names(smietana_df))
names(smietana_df) <- gsub("kwiecieñ.2016", "2016 4 kwiecieñ", names(smietana_df))
names(smietana_df) <- gsub("kwiecieñ.2017", "2017 4 kwiecieñ", names(smietana_df))
names(smietana_df) <- gsub("kwiecieñ.2018", "2018 4 kwiecieñ", names(smietana_df))
names(smietana_df) <- gsub("kwiecieñ.2019", "2019 4 kwiecieñ", names(smietana_df))
names(smietana_df) <- gsub("maj.2006", "2006 5 maj", names(smietana_df))
names(smietana_df) <- gsub("maj.2007", "2007 5 maj", names(smietana_df))
names(smietana_df) <- gsub("maj.2008", "2008 5 maj", names(smietana_df))
names(smietana_df) <- gsub("maj.2009", "2009 5 maj", names(smietana_df))
names(smietana_df) <- gsub("maj.2010", "2010 5 maj", names(smietana_df))
names(smietana_df) <- gsub("maj.2011", "2011 5 maj", names(smietana_df))
names(smietana_df) <- gsub("maj.2012", "2012 5 maj", names(smietana_df))
names(smietana_df) <- gsub("maj.2013", "2013 5 maj", names(smietana_df))
names(smietana_df) <- gsub("maj.2014", "2014 5 maj", names(smietana_df))
names(smietana_df) <- gsub("maj.2015", "2015 5 maj", names(smietana_df))
names(smietana_df) <- gsub("maj.2016", "2016 5 maj", names(smietana_df))
names(smietana_df) <- gsub("maj.2017", "2017 5 maj", names(smietana_df))
names(smietana_df) <- gsub("maj.2018", "2018 5 maj", names(smietana_df))
names(smietana_df) <- gsub("maj.2019", "2019 5 maj", names(smietana_df))
names(smietana_df) <- gsub("czerwiec.2006", "2006 6 czerwiec", names(smietana_df))
names(smietana_df) <- gsub("czerwiec.2007", "2007 6 czerwiec", names(smietana_df))
names(smietana_df) <- gsub("czerwiec.2008", "2008 6 czerwiec", names(smietana_df))
names(smietana_df) <- gsub("czerwiec.2009", "2009 6 czerwiec", names(smietana_df))
names(smietana_df) <- gsub("czerwiec.2010", "2010 6 czerwiec", names(smietana_df))
names(smietana_df) <- gsub("czerwiec.2011", "2011 6 czerwiec", names(smietana_df))
names(smietana_df) <- gsub("czerwiec.2012", "2012 6 czerwiec", names(smietana_df))
names(smietana_df) <- gsub("czerwiec.2013", "2013 6 czerwiec", names(smietana_df))
names(smietana_df) <- gsub("czerwiec.2014", "2014 6 czerwiec", names(smietana_df))
names(smietana_df) <- gsub("czerwiec.2015", "2015 6 czerwiec", names(smietana_df))
names(smietana_df) <- gsub("czerwiec.2016", "2016 6 czerwiec", names(smietana_df))
names(smietana_df) <- gsub("czerwiec.2017", "2017 6 czerwiec", names(smietana_df))
names(smietana_df) <- gsub("czerwiec.2018", "2018 6 czerwiec", names(smietana_df))
names(smietana_df) <- gsub("czerwiec.2019", "2019 6 czerwiec", names(smietana_df))
names(smietana_df) <- gsub("lipiec.2006", "2006 7 lipiec", names(smietana_df))
names(smietana_df) <- gsub("lipiec.2007", "2007 7 lipiec", names(smietana_df))
names(smietana_df) <- gsub("lipiec.2008", "2008 7 lipiec", names(smietana_df))
names(smietana_df) <- gsub("lipiec.2009", "2009 7 lipiec", names(smietana_df))
names(smietana_df) <- gsub("lipiec.2010", "2010 7 lipiec", names(smietana_df))
names(smietana_df) <- gsub("lipiec.2011", "2011 7 lipiec", names(smietana_df))
names(smietana_df) <- gsub("lipiec.2012", "2012 7 lipiec", names(smietana_df))
names(smietana_df) <- gsub("lipiec.2013", "2013 7 lipiec", names(smietana_df))
names(smietana_df) <- gsub("lipiec.2014", "2014 7 lipiec", names(smietana_df))
names(smietana_df) <- gsub("lipiec.2015", "2015 7 lipiec", names(smietana_df))
names(smietana_df) <- gsub("lipiec.2016", "2016 7 lipiec", names(smietana_df))
names(smietana_df) <- gsub("lipiec.2017", "2017 7 lipiec", names(smietana_df))
names(smietana_df) <- gsub("lipiec.2018", "2018 7 lipiec", names(smietana_df))
names(smietana_df) <- gsub("lipiec.2019", "2019 7 lipiec", names(smietana_df))
names(smietana_df) <- gsub("sierpieñ.2006", "2006 8 sierpieñ", names(smietana_df))
names(smietana_df) <- gsub("sierpieñ.2007", "2007 8 sierpieñ", names(smietana_df))
names(smietana_df) <- gsub("sierpieñ.2008", "2008 8 sierpieñ", names(smietana_df))
names(smietana_df) <- gsub("sierpieñ.2009", "2009 8 sierpieñ", names(smietana_df))
names(smietana_df) <- gsub("sierpieñ.2010", "2010 8 sierpieñ", names(smietana_df))
names(smietana_df) <- gsub("sierpieñ.2011", "2011 8 sierpieñ", names(smietana_df))
names(smietana_df) <- gsub("sierpieñ.2012", "2012 8 sierpieñ", names(smietana_df))
names(smietana_df) <- gsub("sierpieñ.2013", "2013 8 sierpieñ", names(smietana_df))
names(smietana_df) <- gsub("sierpieñ.2014", "2014 8 sierpieñ", names(smietana_df))
names(smietana_df) <- gsub("sierpieñ.2015", "2015 8 sierpieñ", names(smietana_df))
names(smietana_df) <- gsub("sierpieñ.2016", "2016 8 sierpieñ", names(smietana_df))
names(smietana_df) <- gsub("sierpieñ.2017", "2017 8 sierpieñ", names(smietana_df))
names(smietana_df) <- gsub("sierpieñ.2018", "2018 8 sierpieñ", names(smietana_df))
names(smietana_df) <- gsub("sierpieñ.2019", "2019 8 sierpieñ", names(smietana_df))
names(smietana_df) <- gsub("wrzesieñ.2006", "2006 9 wrzesieñ", names(smietana_df))
names(smietana_df) <- gsub("wrzesieñ.2007", "2007 9 wrzesieñ", names(smietana_df))
names(smietana_df) <- gsub("wrzesieñ.2008", "2008 9 wrzesieñ", names(smietana_df))
names(smietana_df) <- gsub("wrzesieñ.2009", "2009 9 wrzesieñ", names(smietana_df))
names(smietana_df) <- gsub("wrzesieñ.2010", "2010 9 wrzesieñ", names(smietana_df))
names(smietana_df) <- gsub("wrzesieñ.2011", "2011 9 wrzesieñ", names(smietana_df))
names(smietana_df) <- gsub("wrzesieñ.2012", "2012 9 wrzesieñ", names(smietana_df))
names(smietana_df) <- gsub("wrzesieñ.2013", "2013 9 wrzesieñ", names(smietana_df))
names(smietana_df) <- gsub("wrzesieñ.2014", "2014 9 wrzesieñ", names(smietana_df))
names(smietana_df) <- gsub("wrzesieñ.2015", "2015 9 wrzesieñ", names(smietana_df))
names(smietana_df) <- gsub("wrzesieñ.2016", "2016 9 wrzesieñ", names(smietana_df))
names(smietana_df) <- gsub("wrzesieñ.2017", "2017 9 wrzesieñ", names(smietana_df))
names(smietana_df) <- gsub("wrzesieñ.2018", "2018 9 wrzesieñ", names(smietana_df))
names(smietana_df) <- gsub("wrzesieñ.2019", "2019 9 wrzesieñ", names(smietana_df))
names(smietana_df) <- gsub("paŸdziernik.2006", "2006 90 paŸdziernik", names(smietana_df))
names(smietana_df) <- gsub("paŸdziernik.2007", "2007 90 paŸdziernik", names(smietana_df))
names(smietana_df) <- gsub("paŸdziernik.2008", "2008 90 paŸdziernik", names(smietana_df))
names(smietana_df) <- gsub("paŸdziernik.2009", "2009 90 paŸdziernik", names(smietana_df))
names(smietana_df) <- gsub("paŸdziernik.2010", "2010 90 paŸdziernik", names(smietana_df))
names(smietana_df) <- gsub("paŸdziernik.2011", "2011 90 paŸdziernik", names(smietana_df))
names(smietana_df) <- gsub("paŸdziernik.2012", "2012 90 paŸdziernik", names(smietana_df))
names(smietana_df) <- gsub("paŸdziernik.2013", "2013 90 paŸdziernik", names(smietana_df))
names(smietana_df) <- gsub("paŸdziernik.2014", "2014 90 paŸdziernik", names(smietana_df))
names(smietana_df) <- gsub("paŸdziernik.2015", "2015 90 paŸdziernik", names(smietana_df))
names(smietana_df) <- gsub("paŸdziernik.2016", "2016 90 paŸdziernik", names(smietana_df))
names(smietana_df) <- gsub("paŸdziernik.2017", "2017 90 paŸdziernik", names(smietana_df))
names(smietana_df) <- gsub("paŸdziernik.2018", "2018 90 paŸdziernik", names(smietana_df))
names(smietana_df) <- gsub("paŸdziernik.2019", "2019 90 paŸdziernik", names(smietana_df))
names(smietana_df) <- gsub("listopad.2006", "2006 91 listopad", names(smietana_df))
names(smietana_df) <- gsub("listopad.2007", "2007 91 listopad", names(smietana_df))
names(smietana_df) <- gsub("listopad.2008", "2008 91 listopad", names(smietana_df))
names(smietana_df) <- gsub("listopad.2009", "2009 91 listopad", names(smietana_df))
names(smietana_df) <- gsub("listopad.2010", "2010 91 listopad", names(smietana_df))
names(smietana_df) <- gsub("listopad.2011", "2011 91 listopad", names(smietana_df))
names(smietana_df) <- gsub("listopad.2012", "2012 91 listopad", names(smietana_df))
names(smietana_df) <- gsub("listopad.2013", "2013 91 listopad", names(smietana_df))
names(smietana_df) <- gsub("listopad.2014", "2014 91 listopad", names(smietana_df))
names(smietana_df) <- gsub("listopad.2015", "2015 91 listopad", names(smietana_df))
names(smietana_df) <- gsub("listopad.2016", "2016 91 listopad", names(smietana_df))
names(smietana_df) <- gsub("listopad.2017", "2017 91 listopad", names(smietana_df))
names(smietana_df) <- gsub("listopad.2018", "2018 91 listopad", names(smietana_df))
names(smietana_df) <- gsub("listopad.2019", "2019 91 listopad", names(smietana_df))
names(smietana_df) <- gsub("grudzieñ.2006", "2006 92 grudzieñ", names(smietana_df))
names(smietana_df) <- gsub("grudzieñ.2007", "2007 92 grudzieñ", names(smietana_df))
names(smietana_df) <- gsub("grudzieñ.2008", "2008 92 grudzieñ", names(smietana_df))
names(smietana_df) <- gsub("grudzieñ.2009", "2009 92 grudzieñ", names(smietana_df))
names(smietana_df) <- gsub("grudzieñ.2010", "2010 92 grudzieñ", names(smietana_df))
names(smietana_df) <- gsub("grudzieñ.2011", "2011 92 grudzieñ", names(smietana_df))
names(smietana_df) <- gsub("grudzieñ.2012", "2012 92 grudzieñ", names(smietana_df))
names(smietana_df) <- gsub("grudzieñ.2013", "2013 92 grudzieñ", names(smietana_df))
names(smietana_df) <- gsub("grudzieñ.2014", "2014 92 grudzieñ", names(smietana_df))
names(smietana_df) <- gsub("grudzieñ.2015", "2015 92 grudzieñ", names(smietana_df))
names(smietana_df) <- gsub("grudzieñ.2016", "2016 92 grudzieñ", names(smietana_df))
names(smietana_df) <- gsub("grudzieñ.2017", "2017 92 grudzieñ", names(smietana_df))
names(smietana_df) <- gsub("grudzieñ.2018", "2018 92 grudzieñ", names(smietana_df))
names(smietana_df) <- gsub("grudzieñ.2019", "2019 92 grudzieñ", names(smietana_df))
smietana_df = smietana_df[sort(colnames(smietana_df))] #Sortujê chronologicznie
#smietana_df = subset(smietana_df, select = -c(Nazwa) ) # Usuwam zbêdn¹ kolumnê z nazwazmi województw


smietana_df = t(smietana_df) #Tramsponujê dataframe ¿eby poszczególne kolumny by³y województwami 
smietana_df = unname(smietana_df) #Usuwam zbêdne teraz nazwy
smietana_df = as.data.frame(smietana_df) #Przekszta³cam na dataframe


# Normalnie nie da³o siê przkonwertowaæ danych na numeric, bo zamiast kropek by³y 
# przecinki i niemi³osiernie d³ugo siê nad tym g³owi³em. Ca³y czas otrzymywa³em 
# wartoœci NA
smietana_dolnoslaskie = as.numeric(gsub(",", ".", as.character(smietana_df$V1)))
smietana_kujawsko_pomorskie = as.numeric(gsub(",", ".", as.character(smietana_df$V2)))
smietana_lubelskie = as.numeric(gsub(",", ".", as.character(smietana_df$V3)))
smietana_lubuskie = as.numeric(gsub(",", ".", as.character(smietana_df$V4)))
smietana_lodzkie = as.numeric(gsub(",", ".", as.character(smietana_df$V5)))
smietana_malopolskie = as.numeric(gsub(",", ".", as.character(smietana_df$V6)))
smietana_mazowieckie = as.numeric(gsub(",", ".", as.character(smietana_df$V7)))
smietana_opolskie = as.numeric(gsub(",", ".", as.character(smietana_df$V8)))
smietana_podkarpackie = as.numeric(gsub(",", ".", as.character(smietana_df$V9)))
smietana_podlaskie = as.numeric(gsub(",", ".", as.character(smietana_df$V10)))
smietana_pomorskie = as.numeric(gsub(",", ".", as.character(smietana_df$V11)))
smietana_slaskie = as.numeric(gsub(",", ".", as.character(smietana_df$V12)))
smietana_swietokrzyskie = as.numeric(gsub(",", ".", as.character(smietana_df$V13)))
smietana_warminsko_mazurskie = as.numeric(gsub(",", ".", as.character(smietana_df$V14)))
smietana_wielkopolskie = as.numeric(gsub(",", ".", as.character(smietana_df$V15)))
smietana_zachodniopomorskie = as.numeric(gsub(",", ".", as.character(smietana_df$V16)))

#### Wykres cen œmietany ####
u = (1:96)
#Wykres dla œmietany i wszystkich województw
smietana_plot = plot_ly(x = u )%>%
  layout(title = 'Zmiana cen œmietany w latach 2012-2019', xaxis = list(title = 'Rok', 
                                                                        ticktext = list("2012","2013","2014","2015","2016","2017","2018","2019"), 
                                                                        tickvals = list(1, 12, 24, 36, 48, 60, 72, 84)),
         yaxis = list(title = 'cena (z³)')) %>%
  add_lines(y = smietana_dolnoslaskie, name = "dolnoœlaskie")%>%
  add_lines(y = smietana_kujawsko_pomorskie, name = "kujawsko-pomorskie")%>%
  add_lines(y = smietana_lubelskie, name = "lubelskie")%>%
  add_lines(y = smietana_lubuskie, name = "lubuskie")%>%
  add_lines(y = smietana_lodzkie, name = "³ódzkie")%>%
  add_lines(y = smietana_malopolskie, name = "ma³opolskie")%>%
  add_lines(y = smietana_mazowieckie, name = "mazowieckie")%>%
  add_lines(y = smietana_opolskie, name = "opolskie")%>%
  add_lines(y = smietana_podkarpackie, name = "podkarpackie")%>%
  add_lines(y = smietana_podlaskie, name = "podlaskie")%>%
  add_lines(y = smietana_pomorskie, name = "pomorskie")%>%
  add_lines(y = smietana_slaskie, name = "œl¹skie")%>%
  add_lines(y = smietana_swietokrzyskie, name = "œwietokrzyskie")%>%
  add_lines(y = smietana_warminsko_mazurskie, name = "warmiñsko_mazurskie")%>%
  add_lines(y = smietana_wielkopolskie, name = "wielkopolskie")%>%
  add_lines(y = smietana_zachodniopomorskie, name = "zachodniopomorskie")
#print(kielbasa_plot)

#### Œrednie roczne ceny œmietany ####
roczne_sr_smietana = data.frame('','','','','','','','', stringsAsFactors = FALSE)
names(roczne_sr_smietana) = c("2012","2013","2014","2015","2016",
                              "2017","2018")

for (i in 1:8) {
  roczne_sr_smietana[1,i] = round(mean(smietana_dolnoslaskie[(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_smietana[2,i] = round(mean(smietana_kujawsko_pomorskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_smietana[3,i] = round(mean(smietana_lubelskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_smietana[4,i] = round(mean(smietana_lubuskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_smietana[5,i] = round(mean(smietana_lodzkie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_smietana[6,i] = round(mean(smietana_malopolskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_smietana[7,i] = round(mean(smietana_mazowieckie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_smietana[8,i] = round(mean(smietana_opolskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_smietana[9,i] = round(mean(smietana_podkarpackie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_smietana[10,i] = round(mean(smietana_podlaskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_smietana[11,i] = round(mean(smietana_pomorskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_smietana[12,i] = round(mean(smietana_slaskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_smietana[13,i] = round(mean(smietana_swietokrzyskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_smietana[14,i] = round(mean(smietana_warminsko_mazurskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_smietana[15,i] = round(mean(smietana_wielkopolskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_smietana[16,i] = round(mean(smietana_zachodniopomorskie[(1+(i-1)*12):(12*i)]), digits = 2)
}
roczne_sr_smietana = t(roczne_sr_smietana)
roczne_sr_smietana = unname(roczne_sr_smietana) #Usuwam zbêdne teraz nazwy
roczne_sr_smietana = as.data.frame(roczne_sr_smietana) #Przekszta³cam na dataframe

#### Œrednia cena œmietany dla ka¿dego wojewódstwa ####
smietana_dolnoslaskie_sr = round(mean(smietana_dolnoslaskie), digits = 2)               #1
smietana_kujawsko_pomorskie_sr = round(mean(smietana_kujawsko_pomorskie), digits = 2)   #2
smietana_lubelskie_sr = round(mean(smietana_lubelskie), digits = 2)                     #3
smietana_lubuskie_sr = round(mean(smietana_lubuskie), digits = 2)                       #4
smietana_lodzkie_sr = round(mean(smietana_lodzkie), digits = 2)                         #5
smietana_malopolskie_sr = round(mean(smietana_malopolskie), digits = 2)                 #6
smietana_mazowieckie_sr = round(mean(smietana_mazowieckie), digits = 2)                 #7
smietana_opolskie_sr = round(mean(smietana_opolskie), digits = 2)                       #8
smietana_podkarpackie_sr = round(mean(smietana_podkarpackie), digits = 2)               #9
smietana_podlaskie_sr = round(mean(smietana_podlaskie), digits = 2)                     #10
smietana_pomorskie_sr = round(mean(smietana_pomorskie), digits = 2)                     #11
smietana_slaskie_sr = round(mean(smietana_slaskie), digits = 2)                         #12
smietana_swietokrzyskie_sr = round(mean(smietana_swietokrzyskie), digits = 2)           #13
smietana_warminsko_mazurskie_sr = round(mean(smietana_warminsko_mazurskie), digits = 2) #14
smietana_wielkopolskie_sr = round(mean(smietana_wielkopolskie), digits = 2)             #15
smietana_zachodniopomorskie_sr = round(mean(smietana_zachodniopomorskie), digits = 2)   #16



#### Œmietana œrednie, najw, najmn ceny w jednym wektorze ####
sr_cena_smietana = c(smietana_dolnoslaskie_sr,smietana_kujawsko_pomorskie_sr,smietana_lubelskie_sr,smietana_lubuskie_sr,smietana_lodzkie_sr,
                     smietana_malopolskie_sr,smietana_mazowieckie_sr,smietana_opolskie_sr,smietana_podkarpackie_sr,smietana_podlaskie_sr,
                     smietana_pomorskie_sr,smietana_slaskie_sr,smietana_swietokrzyskie_sr,smietana_warminsko_mazurskie_sr,
                     smietana_wielkopolskie_sr,smietana_zachodniopomorskie_sr)

smietana_najmn_w_kazdym_woj = c(min(smietana_dolnoslaskie),min(smietana_kujawsko_pomorskie),min(smietana_lubelskie),
                                min(smietana_lubuskie),min(smietana_lodzkie),min(smietana_malopolskie),
                                min(smietana_mazowieckie),min(smietana_opolskie),min(smietana_podkarpackie),
                                min(smietana_podlaskie),min(smietana_pomorskie),min(smietana_slaskie),
                                min(smietana_swietokrzyskie),min(smietana_warminsko_mazurskie),
                                min(smietana_wielkopolskie),min(smietana_zachodniopomorskie))

smietana_najw_w_kazdym_woj = c(max(smietana_dolnoslaskie),max(smietana_kujawsko_pomorskie),max(smietana_lubelskie),
                               max(smietana_lubuskie),max(smietana_lodzkie),max(smietana_malopolskie),
                               max(smietana_mazowieckie),max(smietana_opolskie),max(smietana_podkarpackie),
                               max(smietana_podlaskie),max(smietana_pomorskie),max(smietana_slaskie),
                               max(smietana_swietokrzyskie),max(smietana_warminsko_mazurskie),
                               max(smietana_wielkopolskie),max(smietana_zachodniopomorskie))

###  Najwy¿sze i najni¿sze, ró¿ne

smietana_sr_cena = mean(sr_cena_smietana) #Œrednia cena œmietany dla ca³ego kraju
smietana_odsd_cena = sd(sr_cena_smietana)  #Odchylenie standardowe ceny œmietany

smietana_najw_sr = max(sr_cena_smietana) #Najwy¿sza œrednia cena œmietany
smietana_najm_sr = min(sr_cena_smietana) #Najni¿sza œrednia cena œmietany
smietana_najmn_k = min(smietana_najmn_w_kazdym_woj) #Najni¿sza cena œmietany kiedykolwiek
smietana_najw_k = max(smietana_najw_w_kazdym_woj)   #Najwy¿sza cena œmietany kiedykolwiek


smiet_najw_sr = as.numeric(match(smietana_najw_sr,sr_cena_smietana))  #Numer województwa
smiet_najmn_sr = as.numeric(match(smietana_najm_sr,sr_cena_smietana)) #Numer województwa
print(paste("Najwy¿sza œrednia cena œmietany jest w województwie", wojewodztwa[smiet_najw_sr],
            "i wynosi",smietana_najw_sr))
print(paste("Najni¿sza œrednia cena œmietany jest w województwie", wojewodztwa[smiet_najmn_sr],
            "i wynosi",smietana_najm_sr))


smietana_najmn_kazde = as.numeric(match(smietana_najmn_k, smietana_najmn_w_kazdym_woj)) #Numer województwa
smietana_najw_kazde = as.numeric(match(smietana_najw_k,   smietana_najw_w_kazdym_woj))  #Numer województwa
smietana_najmn_kiedy =  match(smietana_najmn_k,           smietana_swietokrzyskie)      #Kiedy
smietana_najw_kiedy =  match(smietana_najw_k,             smietana_dolnoslaskie)        #Kiedy

print(paste("Najni¿sza cena œmietany by³a w", daty[73:168][smietana_najmn_kiedy], "w województwie", 
            wojewodztwa[smietana_najmn_kazde],"i wynosi³a", smietana_najmn_k, "z³"))

print(paste("Najwy¿sza cena œmietany by³a w", daty[73:168][smietana_najw_kiedy], "w województwie",
            wojewodztwa[smietana_najw_kazde],"i wynosi³a", smietana_najw_k, "z³"))

print(paste("Odchylenie standardowe cen œmietany wynosi", round(smietana_odsd_cena, digits = 2), "z³otego"))

#### Korelacja p³acy minimalnej do œredniej ceny œmietany ####
smietana_placa_min_kor = c(cor(as.numeric(as.character(placa_min2012_2019_df$V1)), as.numeric(as.character(roczne_sr_smietana$V1))),
                           cor(as.numeric(as.character(placa_min2012_2019_df$V2)), as.numeric(as.character(roczne_sr_smietana$V2))),
                           cor(as.numeric(as.character(placa_min2012_2019_df$V3)), as.numeric(as.character(roczne_sr_smietana$V3))),
                           cor(as.numeric(as.character(placa_min2012_2019_df$V4)), as.numeric(as.character(roczne_sr_smietana$V4))),
                           cor(as.numeric(as.character(placa_min2012_2019_df$V5)), as.numeric(as.character(roczne_sr_smietana$V5))),
                           cor(as.numeric(as.character(placa_min2012_2019_df$V6)), as.numeric(as.character(roczne_sr_smietana$V6))),
                           cor(as.numeric(as.character(placa_min2012_2019_df$V7)), as.numeric(as.character(roczne_sr_smietana$V7))),
                           cor(as.numeric(as.character(placa_min2012_2019_df$V8)), as.numeric(as.character(roczne_sr_smietana$V8))),
                           cor(as.numeric(as.character(placa_min2012_2019_df$V9)), as.numeric(as.character(roczne_sr_smietana$V9))),
                           cor(as.numeric(as.character(placa_min2012_2019_df$V10)), as.numeric(as.character(roczne_sr_smietana$V10))),
                           cor(as.numeric(as.character(placa_min2012_2019_df$V11)), as.numeric(as.character(roczne_sr_smietana$V11))),
                           cor(as.numeric(as.character(placa_min2012_2019_df$V12)), as.numeric(as.character(roczne_sr_smietana$V12))),
                           cor(as.numeric(as.character(placa_min2012_2019_df$V13)), as.numeric(as.character(roczne_sr_smietana$V13))))


print(paste("Uœredniona korelacja minimalnej pensji do ceny œmietany wynosi", round(mean(smietana_placa_min_kor), 
                                                                                    digits = 4), "co wskazuje na umiarkowanie silnê korelacjê"))
#### Korelacja œredniej pensji do œredniej ceny œmietany ####

smietana_sr_pensja_kor = c(cor(DOLNO[6:13], as.numeric(as.character(roczne_sr_smietana$V1))),
                           cor(KUJAW[6:13], as.numeric(as.character(roczne_sr_smietana$V2))),
                           cor(LUBEL[6:13], as.numeric(as.character(roczne_sr_smietana$V3))),
                           cor(LUBUS[6:13], as.numeric(as.character(roczne_sr_smietana$V4))),
                           cor(LODZK[6:13], as.numeric(as.character(roczne_sr_smietana$V5))),
                           cor(MALOP[6:13], as.numeric(as.character(roczne_sr_smietana$V6))),
                           cor(MAZOW[6:13], as.numeric(as.character(roczne_sr_smietana$V7))),
                           cor(OPOLS[6:13], as.numeric(as.character(roczne_sr_smietana$V8))),
                           cor(PODKA[6:13], as.numeric(as.character(roczne_sr_smietana$V9))),
                           cor(PODLA[6:13], as.numeric(as.character(roczne_sr_smietana$V10))),
                           cor(POMOR[6:13], as.numeric(as.character(roczne_sr_smietana$V11))),
                           cor(SLASK[6:13], as.numeric(as.character(roczne_sr_smietana$V12))),
                           cor(SWIET[6:13], as.numeric(as.character(roczne_sr_smietana$V13))),
                           cor(WARMI[6:13], as.numeric(as.character(roczne_sr_smietana$V14))),
                           cor(WIELK[6:13], as.numeric(as.character(roczne_sr_smietana$V15))),
                           cor(ZACHO[6:13], as.numeric(as.character(roczne_sr_smietana$V16))))
sr_smietana_kor = mean(smietana_sr_pensja_kor)
print(paste("Uœredniona korelacja œredniej pensji do ceny œmietany wynosi", round(sr_smietana_kor, 
                                                                                  digits = 4), "co wskazuje na umiarkowanie silnê korelacjê"))
print(".........................................................")




#### Mas³o edycja nag³ówków i tabel ####
maslo_df = maslo_df %>% remove_empty("cols") # Usuwam puste kolumny
maslo_df = subset(maslo_df, select = -c(Kod,Nazwa))

#Edtujê nazwy kolumn na sensowne
names(maslo_df) <- gsub("styczeñ.mas³o.œwie¿e.o.zawartoœci.t³uszczu.ok..82.5....za.200.g.cena.", "styczeñ.", names(maslo_df))
names(maslo_df) <- gsub("luty.mas³o.œwie¿e.o.zawartoœci.t³uszczu.ok..82.5....za.200.g.cena.", "luty.", names(maslo_df))
names(maslo_df) <- gsub("marzec.mas³o.œwie¿e.o.zawartoœci.t³uszczu.ok..82.5....za.200.g.cena.", "marzec.", names(maslo_df))
names(maslo_df) <- gsub("kwiecieñ.mas³o.œwie¿e.o.zawartoœci.t³uszczu.ok..82.5....za.200.g.cena.", "kwiecieñ.", names(maslo_df))
names(maslo_df) <- gsub("maj.mas³o.œwie¿e.o.zawartoœci.t³uszczu.ok..82.5....za.200.g.cena.", "maj.", names(maslo_df))
names(maslo_df) <- gsub("czerwiec.mas³o.œwie¿e.o.zawartoœci.t³uszczu.ok..82.5....za.200.g.cena.", "czerwiec.", names(maslo_df))
names(maslo_df) <- gsub("lipiec.mas³o.œwie¿e.o.zawartoœci.t³uszczu.ok..82.5....za.200.g.cena.", "lipiec.", names(maslo_df))
names(maslo_df) <- gsub("sierpieñ.mas³o.œwie¿e.o.zawartoœci.t³uszczu.ok..82.5....za.200.g.cena.", "sierpieñ.", names(maslo_df))
names(maslo_df) <- gsub("wrzesieñ.mas³o.œwie¿e.o.zawartoœci.t³uszczu.ok..82.5....za.200.g.cena.", "wrzesieñ.", names(maslo_df))
names(maslo_df) <- gsub("paŸdziernik.mas³o.œwie¿e.o.zawartoœci.t³uszczu.ok..82.5....za.200.g.cena.", "paŸdziernik.", names(maslo_df))
names(maslo_df) <- gsub("listopad.mas³o.œwie¿e.o.zawartoœci.t³uszczu.ok..82.5....za.200.g.cena.", "listopad.", names(maslo_df))
names(maslo_df) <- gsub("grudzieñ.mas³o.œwie¿e.o.zawartoœci.t³uszczu.ok..82.5....za.200.g.cena.", "grudzieñ.", names(maslo_df))
names(maslo_df) <- gsub("..z³.", "", names(maslo_df))

#Nie znalaz³em inteligentnego sposobu na sensowne posortowanie chronologiczne wiêc robiê to ³opatologicznie
names(maslo_df) <- gsub("styczeñ.2006", "2006 1 styczeñ", names(maslo_df))
names(maslo_df) <- gsub("styczeñ.2007", "2007 1 styczeñ", names(maslo_df))
names(maslo_df) <- gsub("styczeñ.2008", "2008 1 styczeñ", names(maslo_df))
names(maslo_df) <- gsub("styczeñ.2009", "2009 1 styczeñ", names(maslo_df))
names(maslo_df) <- gsub("styczeñ.2010", "2010 1 styczeñ", names(maslo_df))
names(maslo_df) <- gsub("styczeñ.2011", "2011 1 styczeñ", names(maslo_df))
names(maslo_df) <- gsub("styczeñ.2012", "2012 1 styczeñ", names(maslo_df))
names(maslo_df) <- gsub("styczeñ.2013", "2013 1 styczeñ", names(maslo_df))
names(maslo_df) <- gsub("styczeñ.2014", "2014 1 styczeñ", names(maslo_df))
names(maslo_df) <- gsub("styczeñ.2015", "2015 1 styczeñ", names(maslo_df))
names(maslo_df) <- gsub("styczeñ.2016", "2016 1 styczeñ", names(maslo_df))
names(maslo_df) <- gsub("styczeñ.2017", "2017 1 styczeñ", names(maslo_df))
names(maslo_df) <- gsub("styczeñ.2018", "2018 1 styczeñ", names(maslo_df))
names(maslo_df) <- gsub("styczeñ.2019", "2019 1 styczeñ", names(maslo_df))
names(maslo_df) <- gsub("luty.2006", "2006 2 luty", names(maslo_df))
names(maslo_df) <- gsub("luty.2007", "2007 2 luty", names(maslo_df))
names(maslo_df) <- gsub("luty.2008", "2008 2 luty", names(maslo_df))
names(maslo_df) <- gsub("luty.2009", "2009 2 luty", names(maslo_df))
names(maslo_df) <- gsub("luty.2010", "2010 2 luty", names(maslo_df))
names(maslo_df) <- gsub("luty.2011", "2011 2 luty", names(maslo_df))
names(maslo_df) <- gsub("luty.2012", "2012 2 luty", names(maslo_df))
names(maslo_df) <- gsub("luty.2013", "2013 2 luty", names(maslo_df))
names(maslo_df) <- gsub("luty.2014", "2014 2 luty", names(maslo_df))
names(maslo_df) <- gsub("luty.2015", "2015 2 luty", names(maslo_df))
names(maslo_df) <- gsub("luty.2016", "2016 2 luty", names(maslo_df))
names(maslo_df) <- gsub("luty.2017", "2017 2 luty", names(maslo_df))
names(maslo_df) <- gsub("luty.2018", "2018 2 luty", names(maslo_df))
names(maslo_df) <- gsub("luty.2019", "2019 2 luty", names(maslo_df))
names(maslo_df) <- gsub("marzec.2006", "2006 3 marzec", names(maslo_df))
names(maslo_df) <- gsub("marzec.2007", "2007 3 marzec", names(maslo_df))
names(maslo_df) <- gsub("marzec.2008", "2008 3 marzec", names(maslo_df))
names(maslo_df) <- gsub("marzec.2009", "2009 3 marzec", names(maslo_df))
names(maslo_df) <- gsub("marzec.2010", "2010 3 marzec", names(maslo_df))
names(maslo_df) <- gsub("marzec.2011", "2011 3 marzec", names(maslo_df))
names(maslo_df) <- gsub("marzec.2012", "2012 3 marzec", names(maslo_df))
names(maslo_df) <- gsub("marzec.2013", "2013 3 marzec", names(maslo_df))
names(maslo_df) <- gsub("marzec.2014", "2014 3 marzec", names(maslo_df))
names(maslo_df) <- gsub("marzec.2015", "2015 3 marzec", names(maslo_df))
names(maslo_df) <- gsub("marzec.2016", "2016 3 marzec", names(maslo_df))
names(maslo_df) <- gsub("marzec.2017", "2017 3 marzec", names(maslo_df))
names(maslo_df) <- gsub("marzec.2018", "2018 3 marzec", names(maslo_df))
names(maslo_df) <- gsub("marzec.2019", "2019 3 marzec", names(maslo_df))
names(maslo_df) <- gsub("kwiecieñ.2006", "2006 4 kwiecieñ", names(maslo_df))
names(maslo_df) <- gsub("kwiecieñ.2007", "2007 4 kwiecieñ", names(maslo_df))
names(maslo_df) <- gsub("kwiecieñ.2008", "2008 4 kwiecieñ", names(maslo_df))
names(maslo_df) <- gsub("kwiecieñ.2009", "2009 4 kwiecieñ", names(maslo_df))
names(maslo_df) <- gsub("kwiecieñ.2010", "2010 4 kwiecieñ", names(maslo_df))
names(maslo_df) <- gsub("kwiecieñ.2011", "2011 4 kwiecieñ", names(maslo_df))
names(maslo_df) <- gsub("kwiecieñ.2012", "2012 4 kwiecieñ", names(maslo_df))
names(maslo_df) <- gsub("kwiecieñ.2013", "2013 4 kwiecieñ", names(maslo_df))
names(maslo_df) <- gsub("kwiecieñ.2014", "2014 4 kwiecieñ", names(maslo_df))
names(maslo_df) <- gsub("kwiecieñ.2015", "2015 4 kwiecieñ", names(maslo_df))
names(maslo_df) <- gsub("kwiecieñ.2016", "2016 4 kwiecieñ", names(maslo_df))
names(maslo_df) <- gsub("kwiecieñ.2017", "2017 4 kwiecieñ", names(maslo_df))
names(maslo_df) <- gsub("kwiecieñ.2018", "2018 4 kwiecieñ", names(maslo_df))
names(maslo_df) <- gsub("kwiecieñ.2019", "2019 4 kwiecieñ", names(maslo_df))
names(maslo_df) <- gsub("maj.2006", "2006 5 maj", names(maslo_df))
names(maslo_df) <- gsub("maj.2007", "2007 5 maj", names(maslo_df))
names(maslo_df) <- gsub("maj.2008", "2008 5 maj", names(maslo_df))
names(maslo_df) <- gsub("maj.2009", "2009 5 maj", names(maslo_df))
names(maslo_df) <- gsub("maj.2010", "2010 5 maj", names(maslo_df))
names(maslo_df) <- gsub("maj.2011", "2011 5 maj", names(maslo_df))
names(maslo_df) <- gsub("maj.2012", "2012 5 maj", names(maslo_df))
names(maslo_df) <- gsub("maj.2013", "2013 5 maj", names(maslo_df))
names(maslo_df) <- gsub("maj.2014", "2014 5 maj", names(maslo_df))
names(maslo_df) <- gsub("maj.2015", "2015 5 maj", names(maslo_df))
names(maslo_df) <- gsub("maj.2016", "2016 5 maj", names(maslo_df))
names(maslo_df) <- gsub("maj.2017", "2017 5 maj", names(maslo_df))
names(maslo_df) <- gsub("maj.2018", "2018 5 maj", names(maslo_df))
names(maslo_df) <- gsub("maj.2019", "2019 5 maj", names(maslo_df))
names(maslo_df) <- gsub("czerwiec.2006", "2006 6 czerwiec", names(maslo_df))
names(maslo_df) <- gsub("czerwiec.2007", "2007 6 czerwiec", names(maslo_df))
names(maslo_df) <- gsub("czerwiec.2008", "2008 6 czerwiec", names(maslo_df))
names(maslo_df) <- gsub("czerwiec.2009", "2009 6 czerwiec", names(maslo_df))
names(maslo_df) <- gsub("czerwiec.2010", "2010 6 czerwiec", names(maslo_df))
names(maslo_df) <- gsub("czerwiec.2011", "2011 6 czerwiec", names(maslo_df))
names(maslo_df) <- gsub("czerwiec.2012", "2012 6 czerwiec", names(maslo_df))
names(maslo_df) <- gsub("czerwiec.2013", "2013 6 czerwiec", names(maslo_df))
names(maslo_df) <- gsub("czerwiec.2014", "2014 6 czerwiec", names(maslo_df))
names(maslo_df) <- gsub("czerwiec.2015", "2015 6 czerwiec", names(maslo_df))
names(maslo_df) <- gsub("czerwiec.2016", "2016 6 czerwiec", names(maslo_df))
names(maslo_df) <- gsub("czerwiec.2017", "2017 6 czerwiec", names(maslo_df))
names(maslo_df) <- gsub("czerwiec.2018", "2018 6 czerwiec", names(maslo_df))
names(maslo_df) <- gsub("czerwiec.2019", "2019 6 czerwiec", names(maslo_df))
names(maslo_df) <- gsub("lipiec.2006", "2006 7 lipiec", names(maslo_df))
names(maslo_df) <- gsub("lipiec.2007", "2007 7 lipiec", names(maslo_df))
names(maslo_df) <- gsub("lipiec.2008", "2008 7 lipiec", names(maslo_df))
names(maslo_df) <- gsub("lipiec.2009", "2009 7 lipiec", names(maslo_df))
names(maslo_df) <- gsub("lipiec.2010", "2010 7 lipiec", names(maslo_df))
names(maslo_df) <- gsub("lipiec.2011", "2011 7 lipiec", names(maslo_df))
names(maslo_df) <- gsub("lipiec.2012", "2012 7 lipiec", names(maslo_df))
names(maslo_df) <- gsub("lipiec.2013", "2013 7 lipiec", names(maslo_df))
names(maslo_df) <- gsub("lipiec.2014", "2014 7 lipiec", names(maslo_df))
names(maslo_df) <- gsub("lipiec.2015", "2015 7 lipiec", names(maslo_df))
names(maslo_df) <- gsub("lipiec.2016", "2016 7 lipiec", names(maslo_df))
names(maslo_df) <- gsub("lipiec.2017", "2017 7 lipiec", names(maslo_df))
names(maslo_df) <- gsub("lipiec.2018", "2018 7 lipiec", names(maslo_df))
names(maslo_df) <- gsub("lipiec.2019", "2019 7 lipiec", names(maslo_df))
names(maslo_df) <- gsub("sierpieñ.2006", "2006 8 sierpieñ", names(maslo_df))
names(maslo_df) <- gsub("sierpieñ.2007", "2007 8 sierpieñ", names(maslo_df))
names(maslo_df) <- gsub("sierpieñ.2008", "2008 8 sierpieñ", names(maslo_df))
names(maslo_df) <- gsub("sierpieñ.2009", "2009 8 sierpieñ", names(maslo_df))
names(maslo_df) <- gsub("sierpieñ.2010", "2010 8 sierpieñ", names(maslo_df))
names(maslo_df) <- gsub("sierpieñ.2011", "2011 8 sierpieñ", names(maslo_df))
names(maslo_df) <- gsub("sierpieñ.2012", "2012 8 sierpieñ", names(maslo_df))
names(maslo_df) <- gsub("sierpieñ.2013", "2013 8 sierpieñ", names(maslo_df))
names(maslo_df) <- gsub("sierpieñ.2014", "2014 8 sierpieñ", names(maslo_df))
names(maslo_df) <- gsub("sierpieñ.2015", "2015 8 sierpieñ", names(maslo_df))
names(maslo_df) <- gsub("sierpieñ.2016", "2016 8 sierpieñ", names(maslo_df))
names(maslo_df) <- gsub("sierpieñ.2017", "2017 8 sierpieñ", names(maslo_df))
names(maslo_df) <- gsub("sierpieñ.2018", "2018 8 sierpieñ", names(maslo_df))
names(maslo_df) <- gsub("sierpieñ.2019", "2019 8 sierpieñ", names(maslo_df))
names(maslo_df) <- gsub("wrzesieñ.2006", "2006 9 wrzesieñ", names(maslo_df))
names(maslo_df) <- gsub("wrzesieñ.2007", "2007 9 wrzesieñ", names(maslo_df))
names(maslo_df) <- gsub("wrzesieñ.2008", "2008 9 wrzesieñ", names(maslo_df))
names(maslo_df) <- gsub("wrzesieñ.2009", "2009 9 wrzesieñ", names(maslo_df))
names(maslo_df) <- gsub("wrzesieñ.2010", "2010 9 wrzesieñ", names(maslo_df))
names(maslo_df) <- gsub("wrzesieñ.2011", "2011 9 wrzesieñ", names(maslo_df))
names(maslo_df) <- gsub("wrzesieñ.2012", "2012 9 wrzesieñ", names(maslo_df))
names(maslo_df) <- gsub("wrzesieñ.2013", "2013 9 wrzesieñ", names(maslo_df))
names(maslo_df) <- gsub("wrzesieñ.2014", "2014 9 wrzesieñ", names(maslo_df))
names(maslo_df) <- gsub("wrzesieñ.2015", "2015 9 wrzesieñ", names(maslo_df))
names(maslo_df) <- gsub("wrzesieñ.2016", "2016 9 wrzesieñ", names(maslo_df))
names(maslo_df) <- gsub("wrzesieñ.2017", "2017 9 wrzesieñ", names(maslo_df))
names(maslo_df) <- gsub("wrzesieñ.2018", "2018 9 wrzesieñ", names(maslo_df))
names(maslo_df) <- gsub("wrzesieñ.2019", "2019 9 wrzesieñ", names(maslo_df))
names(maslo_df) <- gsub("paŸdziernik.2006", "2006 90 paŸdziernik", names(maslo_df))
names(maslo_df) <- gsub("paŸdziernik.2007", "2007 90 paŸdziernik", names(maslo_df))
names(maslo_df) <- gsub("paŸdziernik.2008", "2008 90 paŸdziernik", names(maslo_df))
names(maslo_df) <- gsub("paŸdziernik.2009", "2009 90 paŸdziernik", names(maslo_df))
names(maslo_df) <- gsub("paŸdziernik.2010", "2010 90 paŸdziernik", names(maslo_df))
names(maslo_df) <- gsub("paŸdziernik.2011", "2011 90 paŸdziernik", names(maslo_df))
names(maslo_df) <- gsub("paŸdziernik.2012", "2012 90 paŸdziernik", names(maslo_df))
names(maslo_df) <- gsub("paŸdziernik.2013", "2013 90 paŸdziernik", names(maslo_df))
names(maslo_df) <- gsub("paŸdziernik.2014", "2014 90 paŸdziernik", names(maslo_df))
names(maslo_df) <- gsub("paŸdziernik.2015", "2015 90 paŸdziernik", names(maslo_df))
names(maslo_df) <- gsub("paŸdziernik.2016", "2016 90 paŸdziernik", names(maslo_df))
names(maslo_df) <- gsub("paŸdziernik.2017", "2017 90 paŸdziernik", names(maslo_df))
names(maslo_df) <- gsub("paŸdziernik.2018", "2018 90 paŸdziernik", names(maslo_df))
names(maslo_df) <- gsub("paŸdziernik.2019", "2019 90 paŸdziernik", names(maslo_df))
names(maslo_df) <- gsub("listopad.2006", "2006 91 listopad", names(maslo_df))
names(maslo_df) <- gsub("listopad.2007", "2007 91 listopad", names(maslo_df))
names(maslo_df) <- gsub("listopad.2008", "2008 91 listopad", names(maslo_df))
names(maslo_df) <- gsub("listopad.2009", "2009 91 listopad", names(maslo_df))
names(maslo_df) <- gsub("listopad.2010", "2010 91 listopad", names(maslo_df))
names(maslo_df) <- gsub("listopad.2011", "2011 91 listopad", names(maslo_df))
names(maslo_df) <- gsub("listopad.2012", "2012 91 listopad", names(maslo_df))
names(maslo_df) <- gsub("listopad.2013", "2013 91 listopad", names(maslo_df))
names(maslo_df) <- gsub("listopad.2014", "2014 91 listopad", names(maslo_df))
names(maslo_df) <- gsub("listopad.2015", "2015 91 listopad", names(maslo_df))
names(maslo_df) <- gsub("listopad.2016", "2016 91 listopad", names(maslo_df))
names(maslo_df) <- gsub("listopad.2017", "2017 91 listopad", names(maslo_df))
names(maslo_df) <- gsub("listopad.2018", "2018 91 listopad", names(maslo_df))
names(maslo_df) <- gsub("listopad.2019", "2019 91 listopad", names(maslo_df))
names(maslo_df) <- gsub("grudzieñ.2006", "2006 92 grudzieñ", names(maslo_df))
names(maslo_df) <- gsub("grudzieñ.2007", "2007 92 grudzieñ", names(maslo_df))
names(maslo_df) <- gsub("grudzieñ.2008", "2008 92 grudzieñ", names(maslo_df))
names(maslo_df) <- gsub("grudzieñ.2009", "2009 92 grudzieñ", names(maslo_df))
names(maslo_df) <- gsub("grudzieñ.2010", "2010 92 grudzieñ", names(maslo_df))
names(maslo_df) <- gsub("grudzieñ.2011", "2011 92 grudzieñ", names(maslo_df))
names(maslo_df) <- gsub("grudzieñ.2012", "2012 92 grudzieñ", names(maslo_df))
names(maslo_df) <- gsub("grudzieñ.2013", "2013 92 grudzieñ", names(maslo_df))
names(maslo_df) <- gsub("grudzieñ.2014", "2014 92 grudzieñ", names(maslo_df))
names(maslo_df) <- gsub("grudzieñ.2015", "2015 92 grudzieñ", names(maslo_df))
names(maslo_df) <- gsub("grudzieñ.2016", "2016 92 grudzieñ", names(maslo_df))
names(maslo_df) <- gsub("grudzieñ.2017", "2017 92 grudzieñ", names(maslo_df))
names(maslo_df) <- gsub("grudzieñ.2018", "2018 92 grudzieñ", names(maslo_df))
names(maslo_df) <- gsub("grudzieñ.2019", "2019 92 grudzieñ", names(maslo_df))
maslo_df = maslo_df[sort(colnames(maslo_df))] #Sortujê chronologicznie
#maslo_df = subset(maslo_df, select = -c(Nazwa) ) # Usuwam zbêdn¹ kolumnê z nazwazmi województw


maslo_df = t(maslo_df) #Tramsponujê dataframe ¿eby poszczególne kolumny by³y województwami 
maslo_df = unname(maslo_df) #Usuwam zbêdne teraz nazwy
maslo_df = as.data.frame(maslo_df) #Przekszta³cam na dataframe


# Normalnie nie da³o siê przkonwertowaæ danych na numeric, bo zamiast kropek by³y 
# przecinki i niemi³osiernie d³ugo siê nad tym g³owi³em. Ca³y czas otrzymywa³em 
# wartoœci NA
maslo_dolnoslaskie = as.numeric(gsub(",", ".", as.character(maslo_df$V1)))
maslo_kujawsko_pomorskie = as.numeric(gsub(",", ".", as.character(maslo_df$V2)))
maslo_lubelskie = as.numeric(gsub(",", ".", as.character(maslo_df$V3)))
maslo_lubuskie = as.numeric(gsub(",", ".", as.character(maslo_df$V4)))
maslo_lodzkie = as.numeric(gsub(",", ".", as.character(maslo_df$V5)))
maslo_malopolskie = as.numeric(gsub(",", ".", as.character(maslo_df$V6)))
maslo_mazowieckie = as.numeric(gsub(",", ".", as.character(maslo_df$V7)))
maslo_opolskie = as.numeric(gsub(",", ".", as.character(maslo_df$V8)))
maslo_podkarpackie = as.numeric(gsub(",", ".", as.character(maslo_df$V9)))
maslo_podlaskie = as.numeric(gsub(",", ".", as.character(maslo_df$V10)))
maslo_pomorskie = as.numeric(gsub(",", ".", as.character(maslo_df$V11)))
maslo_slaskie = as.numeric(gsub(",", ".", as.character(maslo_df$V12)))
maslo_swietokrzyskie = as.numeric(gsub(",", ".", as.character(maslo_df$V13)))
maslo_warminsko_mazurskie = as.numeric(gsub(",", ".", as.character(maslo_df$V14)))
maslo_wielkopolskie = as.numeric(gsub(",", ".", as.character(maslo_df$V15)))
maslo_zachodniopomorskie = as.numeric(gsub(",", ".", as.character(maslo_df$V16)))

#### Wykres cen mas³a ####
y = (1:168)
#Wykres dla mas³¹ i wszystkich województw
maslo_plot = plot_ly(x = y )%>%
  layout(title = 'Zmiana cen mas³a w latach 2006-2019', xaxis = list(title = 'Rok', 
                                                                     ticktext = list("2006", "2007", "2008", "2009", "2010", "2011","2012","2013","2014","2015","2016","2017","2018","2019"), 
                                                                     tickvals = list(1, 12, 24, 36, 48, 60, 72, 84, 96, 108, 120, 132, 144, 156)),
         yaxis = list(title = 'cena (z³)')) %>%
  add_lines(y = maslo_dolnoslaskie, name = "dolnoœlaskie")%>%
  add_lines(y = maslo_kujawsko_pomorskie, name = "kujawsko-pomorskie")%>%
  add_lines(y = maslo_lubelskie, name = "lubelskie")%>%
  add_lines(y = maslo_lubuskie, name = "lubuskie")%>%
  add_lines(y = maslo_lodzkie, name = "³ódzkie")%>%
  add_lines(y = maslo_malopolskie, name = "ma³opolskie")%>%
  add_lines(y = maslo_mazowieckie, name = "mazowieckie")%>%
  add_lines(y = maslo_opolskie, name = "opolskie")%>%
  add_lines(y = maslo_podkarpackie, name = "podkarpackie")%>%
  add_lines(y = maslo_podlaskie, name = "podlaskie")%>%
  add_lines(y = maslo_pomorskie, name = "pomorskie")%>%
  add_lines(y = maslo_slaskie, name = "œl¹skie")%>%
  add_lines(y = maslo_swietokrzyskie, name = "œwietokrzyskie")%>%
  add_lines(y = maslo_warminsko_mazurskie, name = "warmiñsko_mazurskie")%>%
  add_lines(y = maslo_wielkopolskie, name = "wielkopolskie")%>%
  add_lines(y = maslo_zachodniopomorskie, name = "zachodniopomorskie")
#print(kielbasa_plot)

#### Œrednie roczne ceny mas³a ####
roczne_sr_maslo = data.frame('','','','','','','','','','','','','', stringsAsFactors = FALSE)
names(roczne_sr_maslo) = c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016",
                           "2017","2018")
for (i in 1:13) {
  roczne_sr_maslo[1,i] = round(mean(maslo_dolnoslaskie[(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_maslo[2,i] = round(mean(maslo_kujawsko_pomorskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_maslo[3,i] = round(mean(maslo_lubelskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_maslo[4,i] = round(mean(maslo_lubuskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_maslo[5,i] = round(mean(maslo_lodzkie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_maslo[6,i] = round(mean(maslo_malopolskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_maslo[7,i] = round(mean(maslo_mazowieckie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_maslo[8,i] = round(mean(maslo_opolskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_maslo[9,i] = round(mean(maslo_podkarpackie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_maslo[10,i] = round(mean(maslo_podlaskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_maslo[11,i] = round(mean(maslo_pomorskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_maslo[12,i] = round(mean(maslo_slaskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_maslo[13,i] = round(mean(maslo_swietokrzyskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_maslo[14,i] = round(mean(maslo_warminsko_mazurskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_maslo[15,i] = round(mean(maslo_wielkopolskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_maslo[16,i] = round(mean(maslo_zachodniopomorskie[(1+(i-1)*12):(12*i)]), digits = 2)
}
roczne_sr_maslo = t(roczne_sr_maslo)
roczne_sr_maslo = unname(roczne_sr_maslo) #Usuwam zbêdne teraz nazwy
roczne_sr_maslo = as.data.frame(roczne_sr_maslo) #Przekszta³cam na dataframe

#### Œrednia cena mas³a dla ka¿dego wojewódstwa ####
maslo_dolnoslaskie_sr = round(mean(maslo_dolnoslaskie), digits = 2)               #1
maslo_kujawsko_pomorskie_sr = round(mean(maslo_kujawsko_pomorskie), digits = 2)   #2
maslo_lubelskie_sr = round(mean(maslo_lubelskie), digits = 2)                     #3
maslo_lubuskie_sr = round(mean(maslo_lubuskie), digits = 2)                       #4
maslo_lodzkie_sr = round(mean(maslo_lodzkie), digits = 2)                         #5
maslo_malopolskie_sr = round(mean(maslo_malopolskie), digits = 2)                 #6
maslo_mazowieckie_sr = round(mean(maslo_mazowieckie), digits = 2)                 #7
maslo_opolskie_sr = round(mean(maslo_opolskie), digits = 2)                       #8
maslo_podkarpackie_sr = round(mean(maslo_podkarpackie), digits = 2)               #9
maslo_podlaskie_sr = round(mean(maslo_podlaskie), digits = 2)                     #10
maslo_pomorskie_sr = round(mean(maslo_pomorskie), digits = 2)                     #11
maslo_slaskie_sr = round(mean(maslo_slaskie), digits = 2)                         #12
maslo_swietokrzyskie_sr = round(mean(maslo_swietokrzyskie), digits = 2)           #13
maslo_warminsko_mazurskie_sr = round(mean(maslo_warminsko_mazurskie), digits = 2) #14
maslo_wielkopolskie_sr = round(mean(maslo_wielkopolskie), digits = 2)             #15
maslo_zachodniopomorskie_sr = round(mean(maslo_zachodniopomorskie), digits = 2)   #16



#### Mas³o œrednie, najw, najmn ceny w jednym wektorze ####
sr_cena_maslo = c(maslo_dolnoslaskie_sr,maslo_kujawsko_pomorskie_sr,maslo_lubelskie_sr,maslo_lubuskie_sr,maslo_lodzkie_sr,
                  maslo_malopolskie_sr,maslo_mazowieckie_sr,maslo_opolskie_sr,maslo_podkarpackie_sr,maslo_podlaskie_sr,
                  maslo_pomorskie_sr,maslo_slaskie_sr,maslo_swietokrzyskie_sr,maslo_warminsko_mazurskie_sr,
                  maslo_wielkopolskie_sr,maslo_zachodniopomorskie_sr)

maslo_najmn_w_kazdym_woj = c(min(maslo_dolnoslaskie),min(maslo_kujawsko_pomorskie),min(maslo_lubelskie),
                             min(maslo_lubuskie),min(maslo_lodzkie),min(maslo_malopolskie),
                             min(maslo_mazowieckie),min(maslo_opolskie),min(maslo_podkarpackie),
                             min(maslo_podlaskie),min(maslo_pomorskie),min(maslo_slaskie),
                             min(maslo_swietokrzyskie),min(maslo_warminsko_mazurskie),
                             min(maslo_wielkopolskie),min(maslo_zachodniopomorskie))

maslo_najw_w_kazdym_woj = c(max(maslo_dolnoslaskie),max(maslo_kujawsko_pomorskie),max(maslo_lubelskie),
                            max(maslo_lubuskie),max(maslo_lodzkie),max(maslo_malopolskie),
                            max(maslo_mazowieckie),max(maslo_opolskie),max(maslo_podkarpackie),
                            max(maslo_podlaskie),max(maslo_pomorskie),max(maslo_slaskie),
                            max(maslo_swietokrzyskie),max(maslo_warminsko_mazurskie),
                            max(maslo_wielkopolskie),max(maslo_zachodniopomorskie))

###  Najwy¿sze i najni¿sze, ró¿ne

maslo_sr_cena = mean(sr_cena_maslo) #Œrednia cena mas³a dla ca³ego kraju
maslo_odsd_cena = sd(sr_cena_maslo)  #Odchylenie standardowe ceny mas³a

maslo_najw_sr = max(sr_cena_maslo) #Najwy¿sza œrednia cena mas³a
maslo_najm_sr = min(sr_cena_maslo) #Najni¿sza œrednia cena mas³a
maslo_najmn_k = min(maslo_najmn_w_kazdym_woj) #Najni¿sza cena mas³a kiedykolwiek
maslo_najw_k = max(maslo_najw_w_kazdym_woj)   #Najwy¿sza cena mas³a kiedykolwiek


mas_najw_sr = as.numeric(match(maslo_najw_sr,sr_cena_maslo))  #Numer województwa
mas_najmn_sr = as.numeric(match(maslo_najm_sr,sr_cena_maslo)) #Numer województwa
print(paste("Najwy¿sza œrednia cena mas³a jest w województwie", wojewodztwa[mas_najw_sr],
            "i wynosi",maslo_najw_sr,"za 200g"))
print(paste("Najni¿sza œrednia cena mas³a jest w województwie", wojewodztwa[mas_najmn_sr],
            "i wynosi",maslo_najm_sr,"za 200g"))


maslo_najmn_kazde = as.numeric(match(maslo_najmn_k, maslo_najmn_w_kazdym_woj))        #Numer województwa
maslo_najw_kazde = as.numeric(match(maslo_najw_k,   maslo_najw_w_kazdym_woj))         #Numer województwa
maslo_najmn_kiedy =  match(maslo_najmn_k,           maslo_opolskie)                   #Kiedy
maslo_najw_kiedy =  match(maslo_najw_k,             maslo_zachodniopomorskie)         #Kiedy

print(paste("Najni¿sza cena mas³a by³a w", daty[maslo_najmn_kiedy], "w województwie", 
            wojewodztwa[maslo_najmn_kazde],"i wynosi³a", maslo_najmn_k, "z³ za 200g"))

print(paste("Najwy¿sza cena mas³a by³a w", daty[maslo_najw_kiedy], "w województwie",
            wojewodztwa[maslo_najw_kazde],"i wynosi³a", maslo_najw_k, "z³ za 200g"))

print(paste("Odchylenie standardowe cen mas³a wynosi", round(maslo_odsd_cena, digits = 2), "z³otego"))

#### Korelacja p³acy minimalnej do œredniej ceny mas³a ####
maslo_placa_min_kor = c(cor(as.numeric(as.character(placa_min_df$V1)), as.numeric(as.character(roczne_sr_maslo$V1))),
                        cor(as.numeric(as.character(placa_min_df$V2)), as.numeric(as.character(roczne_sr_maslo$V2))),
                        cor(as.numeric(as.character(placa_min_df$V3)), as.numeric(as.character(roczne_sr_maslo$V3))),
                        cor(as.numeric(as.character(placa_min_df$V4)), as.numeric(as.character(roczne_sr_maslo$V4))),
                        cor(as.numeric(as.character(placa_min_df$V5)), as.numeric(as.character(roczne_sr_maslo$V5))),
                        cor(as.numeric(as.character(placa_min_df$V6)), as.numeric(as.character(roczne_sr_maslo$V6))),
                        cor(as.numeric(as.character(placa_min_df$V7)), as.numeric(as.character(roczne_sr_maslo$V7))),
                        cor(as.numeric(as.character(placa_min_df$V8)), as.numeric(as.character(roczne_sr_maslo$V8))),
                        cor(as.numeric(as.character(placa_min_df$V9)), as.numeric(as.character(roczne_sr_maslo$V9))),
                        cor(as.numeric(as.character(placa_min_df$V10)), as.numeric(as.character(roczne_sr_maslo$V10))),
                        cor(as.numeric(as.character(placa_min_df$V11)), as.numeric(as.character(roczne_sr_maslo$V11))),
                        cor(as.numeric(as.character(placa_min_df$V12)), as.numeric(as.character(roczne_sr_maslo$V12))),
                        cor(as.numeric(as.character(placa_min_df$V13)), as.numeric(as.character(roczne_sr_maslo$V13))))


print(paste("Uœredniona korelacja minimalnej pensji do ceny mas³a wynosi", round(mean(maslo_placa_min_kor), 
                                                                                 digits = 4), "co wskazuje na siln¹ korelacjê"))
#### Korelacja œredniej pensji do œredniej ceny mas³a ####

maslo_sr_pensja_kor = c(cor(DOLNO, as.numeric(as.character(roczne_sr_maslo$V1))),
                        cor(KUJAW, as.numeric(as.character(roczne_sr_maslo$V2))),
                        cor(LUBEL, as.numeric(as.character(roczne_sr_maslo$V3))),
                        cor(LUBUS, as.numeric(as.character(roczne_sr_maslo$V4))),
                        cor(LODZK, as.numeric(as.character(roczne_sr_maslo$V5))),
                        cor(MALOP, as.numeric(as.character(roczne_sr_maslo$V6))),
                        cor(MAZOW, as.numeric(as.character(roczne_sr_maslo$V7))),
                        cor(OPOLS, as.numeric(as.character(roczne_sr_maslo$V8))),
                        cor(PODKA, as.numeric(as.character(roczne_sr_maslo$V9))),
                        cor(PODLA, as.numeric(as.character(roczne_sr_maslo$V10))),
                        cor(POMOR, as.numeric(as.character(roczne_sr_maslo$V11))),
                        cor(SLASK, as.numeric(as.character(roczne_sr_maslo$V12))),
                        cor(SWIET, as.numeric(as.character(roczne_sr_maslo$V13))),
                        cor(WARMI, as.numeric(as.character(roczne_sr_maslo$V14))),
                        cor(WIELK, as.numeric(as.character(roczne_sr_maslo$V15))),
                        cor(ZACHO, as.numeric(as.character(roczne_sr_maslo$V16))))
sr_maslo_kor = mean(maslo_sr_pensja_kor)
print(paste("Uœredniona korelacja œredniej pensji do ceny mas³a wynosi", round(sr_maslo_kor, 
                                                                               digits = 4), "co wskazuje na siln¹ korelacjê"))
print(".........................................................")




#### Czekolada edycja nag³ówków i tabel ####
czekolada_df = czekolada_df %>% remove_empty("cols") # Usuwam puste kolumny
czekolada_df = subset(czekolada_df, select = -c(Kod,Nazwa))

#Edtujê nazwy kolumn na sensowne
names(czekolada_df) <- gsub("styczeñ.czekolada.mleczna...za.100g.cena.", "styczeñ.", names(czekolada_df))
names(czekolada_df) <- gsub("luty.czekolada.mleczna...za.100g.cena.", "luty.", names(czekolada_df))
names(czekolada_df) <- gsub("marzec.czekolada.mleczna...za.100g.cena.", "marzec.", names(czekolada_df))
names(czekolada_df) <- gsub("kwiecieñ.czekolada.mleczna...za.100g.cena.", "kwiecieñ.", names(czekolada_df))
names(czekolada_df) <- gsub("maj.czekolada.mleczna...za.100g.cena.", "maj.", names(czekolada_df))
names(czekolada_df) <- gsub("czerwiec.czekolada.mleczna...za.100g.cena.", "czerwiec.", names(czekolada_df))
names(czekolada_df) <- gsub("lipiec.czekolada.mleczna...za.100g.cena.", "lipiec.", names(czekolada_df))
names(czekolada_df) <- gsub("sierpieñ.czekolada.mleczna...za.100g.cena.", "sierpieñ.", names(czekolada_df))
names(czekolada_df) <- gsub("wrzesieñ.czekolada.mleczna...za.100g.cena.", "wrzesieñ.", names(czekolada_df))
names(czekolada_df) <- gsub("paŸdziernik.czekolada.mleczna...za.100g.cena.", "paŸdziernik.", names(czekolada_df))
names(czekolada_df) <- gsub("listopad.czekolada.mleczna...za.100g.cena.", "listopad.", names(czekolada_df))
names(czekolada_df) <- gsub("grudzieñ.czekolada.mleczna...za.100g.cena.", "grudzieñ.", names(czekolada_df))
names(czekolada_df) <- gsub("..z³.", "", names(czekolada_df))

#Nie znalaz³em inteligentnego sposobu na sensowne posortowanie chronologiczne wiêc robiê to ³opatologicznie
names(czekolada_df) <- gsub("styczeñ.2006", "2006 1 styczeñ", names(czekolada_df))
names(czekolada_df) <- gsub("styczeñ.2007", "2007 1 styczeñ", names(czekolada_df))
names(czekolada_df) <- gsub("styczeñ.2008", "2008 1 styczeñ", names(czekolada_df))
names(czekolada_df) <- gsub("styczeñ.2009", "2009 1 styczeñ", names(czekolada_df))
names(czekolada_df) <- gsub("styczeñ.2010", "2010 1 styczeñ", names(czekolada_df))
names(czekolada_df) <- gsub("styczeñ.2011", "2011 1 styczeñ", names(czekolada_df))
names(czekolada_df) <- gsub("styczeñ.2012", "2012 1 styczeñ", names(czekolada_df))
names(czekolada_df) <- gsub("styczeñ.2013", "2013 1 styczeñ", names(czekolada_df))
names(czekolada_df) <- gsub("styczeñ.2014", "2014 1 styczeñ", names(czekolada_df))
names(czekolada_df) <- gsub("styczeñ.2015", "2015 1 styczeñ", names(czekolada_df))
names(czekolada_df) <- gsub("styczeñ.2016", "2016 1 styczeñ", names(czekolada_df))
names(czekolada_df) <- gsub("styczeñ.2017", "2017 1 styczeñ", names(czekolada_df))
names(czekolada_df) <- gsub("styczeñ.2018", "2018 1 styczeñ", names(czekolada_df))
names(czekolada_df) <- gsub("styczeñ.2019", "2019 1 styczeñ", names(czekolada_df))
names(czekolada_df) <- gsub("luty.2006", "2006 2 luty", names(czekolada_df))
names(czekolada_df) <- gsub("luty.2007", "2007 2 luty", names(czekolada_df))
names(czekolada_df) <- gsub("luty.2008", "2008 2 luty", names(czekolada_df))
names(czekolada_df) <- gsub("luty.2009", "2009 2 luty", names(czekolada_df))
names(czekolada_df) <- gsub("luty.2010", "2010 2 luty", names(czekolada_df))
names(czekolada_df) <- gsub("luty.2011", "2011 2 luty", names(czekolada_df))
names(czekolada_df) <- gsub("luty.2012", "2012 2 luty", names(czekolada_df))
names(czekolada_df) <- gsub("luty.2013", "2013 2 luty", names(czekolada_df))
names(czekolada_df) <- gsub("luty.2014", "2014 2 luty", names(czekolada_df))
names(czekolada_df) <- gsub("luty.2015", "2015 2 luty", names(czekolada_df))
names(czekolada_df) <- gsub("luty.2016", "2016 2 luty", names(czekolada_df))
names(czekolada_df) <- gsub("luty.2017", "2017 2 luty", names(czekolada_df))
names(czekolada_df) <- gsub("luty.2018", "2018 2 luty", names(czekolada_df))
names(czekolada_df) <- gsub("luty.2019", "2019 2 luty", names(czekolada_df))
names(czekolada_df) <- gsub("marzec.2006", "2006 3 marzec", names(czekolada_df))
names(czekolada_df) <- gsub("marzec.2007", "2007 3 marzec", names(czekolada_df))
names(czekolada_df) <- gsub("marzec.2008", "2008 3 marzec", names(czekolada_df))
names(czekolada_df) <- gsub("marzec.2009", "2009 3 marzec", names(czekolada_df))
names(czekolada_df) <- gsub("marzec.2010", "2010 3 marzec", names(czekolada_df))
names(czekolada_df) <- gsub("marzec.2011", "2011 3 marzec", names(czekolada_df))
names(czekolada_df) <- gsub("marzec.2012", "2012 3 marzec", names(czekolada_df))
names(czekolada_df) <- gsub("marzec.2013", "2013 3 marzec", names(czekolada_df))
names(czekolada_df) <- gsub("marzec.2014", "2014 3 marzec", names(czekolada_df))
names(czekolada_df) <- gsub("marzec.2015", "2015 3 marzec", names(czekolada_df))
names(czekolada_df) <- gsub("marzec.2016", "2016 3 marzec", names(czekolada_df))
names(czekolada_df) <- gsub("marzec.2017", "2017 3 marzec", names(czekolada_df))
names(czekolada_df) <- gsub("marzec.2018", "2018 3 marzec", names(czekolada_df))
names(czekolada_df) <- gsub("marzec.2019", "2019 3 marzec", names(czekolada_df))
names(czekolada_df) <- gsub("kwiecieñ.2006", "2006 4 kwiecieñ", names(czekolada_df))
names(czekolada_df) <- gsub("kwiecieñ.2007", "2007 4 kwiecieñ", names(czekolada_df))
names(czekolada_df) <- gsub("kwiecieñ.2008", "2008 4 kwiecieñ", names(czekolada_df))
names(czekolada_df) <- gsub("kwiecieñ.2009", "2009 4 kwiecieñ", names(czekolada_df))
names(czekolada_df) <- gsub("kwiecieñ.2010", "2010 4 kwiecieñ", names(czekolada_df))
names(czekolada_df) <- gsub("kwiecieñ.2011", "2011 4 kwiecieñ", names(czekolada_df))
names(czekolada_df) <- gsub("kwiecieñ.2012", "2012 4 kwiecieñ", names(czekolada_df))
names(czekolada_df) <- gsub("kwiecieñ.2013", "2013 4 kwiecieñ", names(czekolada_df))
names(czekolada_df) <- gsub("kwiecieñ.2014", "2014 4 kwiecieñ", names(czekolada_df))
names(czekolada_df) <- gsub("kwiecieñ.2015", "2015 4 kwiecieñ", names(czekolada_df))
names(czekolada_df) <- gsub("kwiecieñ.2016", "2016 4 kwiecieñ", names(czekolada_df))
names(czekolada_df) <- gsub("kwiecieñ.2017", "2017 4 kwiecieñ", names(czekolada_df))
names(czekolada_df) <- gsub("kwiecieñ.2018", "2018 4 kwiecieñ", names(czekolada_df))
names(czekolada_df) <- gsub("kwiecieñ.2019", "2019 4 kwiecieñ", names(czekolada_df))
names(czekolada_df) <- gsub("maj.2006", "2006 5 maj", names(czekolada_df))
names(czekolada_df) <- gsub("maj.2007", "2007 5 maj", names(czekolada_df))
names(czekolada_df) <- gsub("maj.2008", "2008 5 maj", names(czekolada_df))
names(czekolada_df) <- gsub("maj.2009", "2009 5 maj", names(czekolada_df))
names(czekolada_df) <- gsub("maj.2010", "2010 5 maj", names(czekolada_df))
names(czekolada_df) <- gsub("maj.2011", "2011 5 maj", names(czekolada_df))
names(czekolada_df) <- gsub("maj.2012", "2012 5 maj", names(czekolada_df))
names(czekolada_df) <- gsub("maj.2013", "2013 5 maj", names(czekolada_df))
names(czekolada_df) <- gsub("maj.2014", "2014 5 maj", names(czekolada_df))
names(czekolada_df) <- gsub("maj.2015", "2015 5 maj", names(czekolada_df))
names(czekolada_df) <- gsub("maj.2016", "2016 5 maj", names(czekolada_df))
names(czekolada_df) <- gsub("maj.2017", "2017 5 maj", names(czekolada_df))
names(czekolada_df) <- gsub("maj.2018", "2018 5 maj", names(czekolada_df))
names(czekolada_df) <- gsub("maj.2019", "2019 5 maj", names(czekolada_df))
names(czekolada_df) <- gsub("czerwiec.2006", "2006 6 czerwiec", names(czekolada_df))
names(czekolada_df) <- gsub("czerwiec.2007", "2007 6 czerwiec", names(czekolada_df))
names(czekolada_df) <- gsub("czerwiec.2008", "2008 6 czerwiec", names(czekolada_df))
names(czekolada_df) <- gsub("czerwiec.2009", "2009 6 czerwiec", names(czekolada_df))
names(czekolada_df) <- gsub("czerwiec.2010", "2010 6 czerwiec", names(czekolada_df))
names(czekolada_df) <- gsub("czerwiec.2011", "2011 6 czerwiec", names(czekolada_df))
names(czekolada_df) <- gsub("czerwiec.2012", "2012 6 czerwiec", names(czekolada_df))
names(czekolada_df) <- gsub("czerwiec.2013", "2013 6 czerwiec", names(czekolada_df))
names(czekolada_df) <- gsub("czerwiec.2014", "2014 6 czerwiec", names(czekolada_df))
names(czekolada_df) <- gsub("czerwiec.2015", "2015 6 czerwiec", names(czekolada_df))
names(czekolada_df) <- gsub("czerwiec.2016", "2016 6 czerwiec", names(czekolada_df))
names(czekolada_df) <- gsub("czerwiec.2017", "2017 6 czerwiec", names(czekolada_df))
names(czekolada_df) <- gsub("czerwiec.2018", "2018 6 czerwiec", names(czekolada_df))
names(czekolada_df) <- gsub("czerwiec.2019", "2019 6 czerwiec", names(czekolada_df))
names(czekolada_df) <- gsub("lipiec.2006", "2006 7 lipiec", names(czekolada_df))
names(czekolada_df) <- gsub("lipiec.2007", "2007 7 lipiec", names(czekolada_df))
names(czekolada_df) <- gsub("lipiec.2008", "2008 7 lipiec", names(czekolada_df))
names(czekolada_df) <- gsub("lipiec.2009", "2009 7 lipiec", names(czekolada_df))
names(czekolada_df) <- gsub("lipiec.2010", "2010 7 lipiec", names(czekolada_df))
names(czekolada_df) <- gsub("lipiec.2011", "2011 7 lipiec", names(czekolada_df))
names(czekolada_df) <- gsub("lipiec.2012", "2012 7 lipiec", names(czekolada_df))
names(czekolada_df) <- gsub("lipiec.2013", "2013 7 lipiec", names(czekolada_df))
names(czekolada_df) <- gsub("lipiec.2014", "2014 7 lipiec", names(czekolada_df))
names(czekolada_df) <- gsub("lipiec.2015", "2015 7 lipiec", names(czekolada_df))
names(czekolada_df) <- gsub("lipiec.2016", "2016 7 lipiec", names(czekolada_df))
names(czekolada_df) <- gsub("lipiec.2017", "2017 7 lipiec", names(czekolada_df))
names(czekolada_df) <- gsub("lipiec.2018", "2018 7 lipiec", names(czekolada_df))
names(czekolada_df) <- gsub("lipiec.2019", "2019 7 lipiec", names(czekolada_df))
names(czekolada_df) <- gsub("sierpieñ.2006", "2006 8 sierpieñ", names(czekolada_df))
names(czekolada_df) <- gsub("sierpieñ.2007", "2007 8 sierpieñ", names(czekolada_df))
names(czekolada_df) <- gsub("sierpieñ.2008", "2008 8 sierpieñ", names(czekolada_df))
names(czekolada_df) <- gsub("sierpieñ.2009", "2009 8 sierpieñ", names(czekolada_df))
names(czekolada_df) <- gsub("sierpieñ.2010", "2010 8 sierpieñ", names(czekolada_df))
names(czekolada_df) <- gsub("sierpieñ.2011", "2011 8 sierpieñ", names(czekolada_df))
names(czekolada_df) <- gsub("sierpieñ.2012", "2012 8 sierpieñ", names(czekolada_df))
names(czekolada_df) <- gsub("sierpieñ.2013", "2013 8 sierpieñ", names(czekolada_df))
names(czekolada_df) <- gsub("sierpieñ.2014", "2014 8 sierpieñ", names(czekolada_df))
names(czekolada_df) <- gsub("sierpieñ.2015", "2015 8 sierpieñ", names(czekolada_df))
names(czekolada_df) <- gsub("sierpieñ.2016", "2016 8 sierpieñ", names(czekolada_df))
names(czekolada_df) <- gsub("sierpieñ.2017", "2017 8 sierpieñ", names(czekolada_df))
names(czekolada_df) <- gsub("sierpieñ.2018", "2018 8 sierpieñ", names(czekolada_df))
names(czekolada_df) <- gsub("sierpieñ.2019", "2019 8 sierpieñ", names(czekolada_df))
names(czekolada_df) <- gsub("wrzesieñ.2006", "2006 9 wrzesieñ", names(czekolada_df))
names(czekolada_df) <- gsub("wrzesieñ.2007", "2007 9 wrzesieñ", names(czekolada_df))
names(czekolada_df) <- gsub("wrzesieñ.2008", "2008 9 wrzesieñ", names(czekolada_df))
names(czekolada_df) <- gsub("wrzesieñ.2009", "2009 9 wrzesieñ", names(czekolada_df))
names(czekolada_df) <- gsub("wrzesieñ.2010", "2010 9 wrzesieñ", names(czekolada_df))
names(czekolada_df) <- gsub("wrzesieñ.2011", "2011 9 wrzesieñ", names(czekolada_df))
names(czekolada_df) <- gsub("wrzesieñ.2012", "2012 9 wrzesieñ", names(czekolada_df))
names(czekolada_df) <- gsub("wrzesieñ.2013", "2013 9 wrzesieñ", names(czekolada_df))
names(czekolada_df) <- gsub("wrzesieñ.2014", "2014 9 wrzesieñ", names(czekolada_df))
names(czekolada_df) <- gsub("wrzesieñ.2015", "2015 9 wrzesieñ", names(czekolada_df))
names(czekolada_df) <- gsub("wrzesieñ.2016", "2016 9 wrzesieñ", names(czekolada_df))
names(czekolada_df) <- gsub("wrzesieñ.2017", "2017 9 wrzesieñ", names(czekolada_df))
names(czekolada_df) <- gsub("wrzesieñ.2018", "2018 9 wrzesieñ", names(czekolada_df))
names(czekolada_df) <- gsub("wrzesieñ.2019", "2019 9 wrzesieñ", names(czekolada_df))
names(czekolada_df) <- gsub("paŸdziernik.2006", "2006 90 paŸdziernik", names(czekolada_df))
names(czekolada_df) <- gsub("paŸdziernik.2007", "2007 90 paŸdziernik", names(czekolada_df))
names(czekolada_df) <- gsub("paŸdziernik.2008", "2008 90 paŸdziernik", names(czekolada_df))
names(czekolada_df) <- gsub("paŸdziernik.2009", "2009 90 paŸdziernik", names(czekolada_df))
names(czekolada_df) <- gsub("paŸdziernik.2010", "2010 90 paŸdziernik", names(czekolada_df))
names(czekolada_df) <- gsub("paŸdziernik.2011", "2011 90 paŸdziernik", names(czekolada_df))
names(czekolada_df) <- gsub("paŸdziernik.2012", "2012 90 paŸdziernik", names(czekolada_df))
names(czekolada_df) <- gsub("paŸdziernik.2013", "2013 90 paŸdziernik", names(czekolada_df))
names(czekolada_df) <- gsub("paŸdziernik.2014", "2014 90 paŸdziernik", names(czekolada_df))
names(czekolada_df) <- gsub("paŸdziernik.2015", "2015 90 paŸdziernik", names(czekolada_df))
names(czekolada_df) <- gsub("paŸdziernik.2016", "2016 90 paŸdziernik", names(czekolada_df))
names(czekolada_df) <- gsub("paŸdziernik.2017", "2017 90 paŸdziernik", names(czekolada_df))
names(czekolada_df) <- gsub("paŸdziernik.2018", "2018 90 paŸdziernik", names(czekolada_df))
names(czekolada_df) <- gsub("paŸdziernik.2019", "2019 90 paŸdziernik", names(czekolada_df))
names(czekolada_df) <- gsub("listopad.2006", "2006 91 listopad", names(czekolada_df))
names(czekolada_df) <- gsub("listopad.2007", "2007 91 listopad", names(czekolada_df))
names(czekolada_df) <- gsub("listopad.2008", "2008 91 listopad", names(czekolada_df))
names(czekolada_df) <- gsub("listopad.2009", "2009 91 listopad", names(czekolada_df))
names(czekolada_df) <- gsub("listopad.2010", "2010 91 listopad", names(czekolada_df))
names(czekolada_df) <- gsub("listopad.2011", "2011 91 listopad", names(czekolada_df))
names(czekolada_df) <- gsub("listopad.2012", "2012 91 listopad", names(czekolada_df))
names(czekolada_df) <- gsub("listopad.2013", "2013 91 listopad", names(czekolada_df))
names(czekolada_df) <- gsub("listopad.2014", "2014 91 listopad", names(czekolada_df))
names(czekolada_df) <- gsub("listopad.2015", "2015 91 listopad", names(czekolada_df))
names(czekolada_df) <- gsub("listopad.2016", "2016 91 listopad", names(czekolada_df))
names(czekolada_df) <- gsub("listopad.2017", "2017 91 listopad", names(czekolada_df))
names(czekolada_df) <- gsub("listopad.2018", "2018 91 listopad", names(czekolada_df))
names(czekolada_df) <- gsub("listopad.2019", "2019 91 listopad", names(czekolada_df))
names(czekolada_df) <- gsub("grudzieñ.2006", "2006 92 grudzieñ", names(czekolada_df))
names(czekolada_df) <- gsub("grudzieñ.2007", "2007 92 grudzieñ", names(czekolada_df))
names(czekolada_df) <- gsub("grudzieñ.2008", "2008 92 grudzieñ", names(czekolada_df))
names(czekolada_df) <- gsub("grudzieñ.2009", "2009 92 grudzieñ", names(czekolada_df))
names(czekolada_df) <- gsub("grudzieñ.2010", "2010 92 grudzieñ", names(czekolada_df))
names(czekolada_df) <- gsub("grudzieñ.2011", "2011 92 grudzieñ", names(czekolada_df))
names(czekolada_df) <- gsub("grudzieñ.2012", "2012 92 grudzieñ", names(czekolada_df))
names(czekolada_df) <- gsub("grudzieñ.2013", "2013 92 grudzieñ", names(czekolada_df))
names(czekolada_df) <- gsub("grudzieñ.2014", "2014 92 grudzieñ", names(czekolada_df))
names(czekolada_df) <- gsub("grudzieñ.2015", "2015 92 grudzieñ", names(czekolada_df))
names(czekolada_df) <- gsub("grudzieñ.2016", "2016 92 grudzieñ", names(czekolada_df))
names(czekolada_df) <- gsub("grudzieñ.2017", "2017 92 grudzieñ", names(czekolada_df))
names(czekolada_df) <- gsub("grudzieñ.2018", "2018 92 grudzieñ", names(czekolada_df))
names(czekolada_df) <- gsub("grudzieñ.2019", "2019 92 grudzieñ", names(czekolada_df))
czekolada_df = czekolada_df[sort(colnames(czekolada_df))] #Sortujê chronologicznie
#czekolada_df = subset(czekolada_df, select = -c(Nazwa) ) # Usuwam zbêdn¹ kolumnê z nazwazmi województw


czekolada_df = t(czekolada_df) #Tramsponujê dataframe ¿eby poszczególne kolumny by³y województwami 
czekolada_df = unname(czekolada_df) #Usuwam zbêdne teraz nazwy
czekolada_df = as.data.frame(czekolada_df) #Przekszta³cam na dataframe


# Normalnie nie da³o siê przkonwertowaæ danych na numeric, bo zamiast kropek by³y 
# przecinki i niemi³osiernie d³ugo siê nad tym g³owi³em. Ca³y czas otrzymywa³em 
# wartoœci NA
czekolada_dolnoslaskie = as.numeric(gsub(",", ".", as.character(czekolada_df$V1)))
czekolada_kujawsko_pomorskie = as.numeric(gsub(",", ".", as.character(czekolada_df$V2)))
czekolada_lubelskie = as.numeric(gsub(",", ".", as.character(czekolada_df$V3)))
czekolada_lubuskie = as.numeric(gsub(",", ".", as.character(czekolada_df$V4)))
czekolada_lodzkie = as.numeric(gsub(",", ".", as.character(czekolada_df$V5)))
czekolada_malopolskie = as.numeric(gsub(",", ".", as.character(czekolada_df$V6)))
czekolada_mazowieckie = as.numeric(gsub(",", ".", as.character(czekolada_df$V7)))
czekolada_opolskie = as.numeric(gsub(",", ".", as.character(czekolada_df$V8)))
czekolada_podkarpackie = as.numeric(gsub(",", ".", as.character(czekolada_df$V9)))
czekolada_podlaskie = as.numeric(gsub(",", ".", as.character(czekolada_df$V10)))
czekolada_pomorskie = as.numeric(gsub(",", ".", as.character(czekolada_df$V11)))
czekolada_slaskie = as.numeric(gsub(",", ".", as.character(czekolada_df$V12)))
czekolada_swietokrzyskie = as.numeric(gsub(",", ".", as.character(czekolada_df$V13)))
czekolada_warminsko_mazurskie = as.numeric(gsub(",", ".", as.character(czekolada_df$V14)))
czekolada_wielkopolskie = as.numeric(gsub(",", ".", as.character(czekolada_df$V15)))
czekolada_zachodniopomorskie = as.numeric(gsub(",", ".", as.character(czekolada_df$V16)))

#### Wykres cen czekolady ####
y = (1:168)
#Wykres dla czekolady i wszystkich województw
czekolada_plot = plot_ly(x = y )%>%
  layout(title = 'Zmiana cen czekolady w latach 2006-2019', xaxis = list(title = 'Rok', 
                                                                         ticktext = list("2006", "2007", "2008", "2009", "2010", "2011","2012","2013","2014","2015","2016","2017","2018","2019"), 
                                                                         tickvals = list(1, 12, 24, 36, 48, 60, 72, 84, 96, 108, 120, 132, 144, 156)),
         yaxis = list(title = 'cena (z³)')) %>%
  add_lines(y = czekolada_dolnoslaskie, name = "dolnoœlaskie")%>%
  add_lines(y = czekolada_kujawsko_pomorskie, name = "kujawsko-pomorskie")%>%
  add_lines(y = czekolada_lubelskie, name = "lubelskie")%>%
  add_lines(y = czekolada_lubuskie, name = "lubuskie")%>%
  add_lines(y = czekolada_lodzkie, name = "³ódzkie")%>%
  add_lines(y = czekolada_malopolskie, name = "ma³opolskie")%>%
  add_lines(y = czekolada_mazowieckie, name = "mazowieckie")%>%
  add_lines(y = czekolada_opolskie, name = "opolskie")%>%
  add_lines(y = czekolada_podkarpackie, name = "podkarpackie")%>%
  add_lines(y = czekolada_podlaskie, name = "podlaskie")%>%
  add_lines(y = czekolada_pomorskie, name = "pomorskie")%>%
  add_lines(y = czekolada_slaskie, name = "œl¹skie")%>%
  add_lines(y = czekolada_swietokrzyskie, name = "œwietokrzyskie")%>%
  add_lines(y = czekolada_warminsko_mazurskie, name = "warmiñsko_mazurskie")%>%
  add_lines(y = czekolada_wielkopolskie, name = "wielkopolskie")%>%
  add_lines(y = czekolada_zachodniopomorskie, name = "zachodniopomorskie")
#print(kielbasa_plot)

#### Œrednie roczne ceny czekolady ####
roczne_sr_czekolada = data.frame('','','','','','','','','','','','','', stringsAsFactors = FALSE)
names(roczne_sr_czekolada) = c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016",
                               "2017","2018")
for (i in 1:13) {
  roczne_sr_czekolada[1,i] = round(mean(czekolada_dolnoslaskie[(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_czekolada[2,i] = round(mean(czekolada_kujawsko_pomorskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_czekolada[3,i] = round(mean(czekolada_lubelskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_czekolada[4,i] = round(mean(czekolada_lubuskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_czekolada[5,i] = round(mean(czekolada_lodzkie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_czekolada[6,i] = round(mean(czekolada_malopolskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_czekolada[7,i] = round(mean(czekolada_mazowieckie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_czekolada[8,i] = round(mean(czekolada_opolskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_czekolada[9,i] = round(mean(czekolada_podkarpackie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_czekolada[10,i] = round(mean(czekolada_podlaskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_czekolada[11,i] = round(mean(czekolada_pomorskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_czekolada[12,i] = round(mean(czekolada_slaskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_czekolada[13,i] = round(mean(czekolada_swietokrzyskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_czekolada[14,i] = round(mean(czekolada_warminsko_mazurskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_czekolada[15,i] = round(mean(czekolada_wielkopolskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_czekolada[16,i] = round(mean(czekolada_zachodniopomorskie[(1+(i-1)*12):(12*i)]), digits = 2)
}
roczne_sr_czekolada = t(roczne_sr_czekolada)
roczne_sr_czekolada = unname(roczne_sr_czekolada) #Usuwam zbêdne teraz nazwy
roczne_sr_czekolada = as.data.frame(roczne_sr_czekolada) #Przekszta³cam na dataframe

#### Œrednia cena czekolady dla ka¿dego wojewódstwa ####
czekolada_dolnoslaskie_sr = round(mean(czekolada_dolnoslaskie), digits = 2)               #1
czekolada_kujawsko_pomorskie_sr = round(mean(czekolada_kujawsko_pomorskie), digits = 2)   #2
czekolada_lubelskie_sr = round(mean(czekolada_lubelskie), digits = 2)                     #3
czekolada_lubuskie_sr = round(mean(czekolada_lubuskie), digits = 2)                       #4
czekolada_lodzkie_sr = round(mean(czekolada_lodzkie), digits = 2)                         #5
czekolada_malopolskie_sr = round(mean(czekolada_malopolskie), digits = 2)                 #6
czekolada_mazowieckie_sr = round(mean(czekolada_mazowieckie), digits = 2)                 #7
czekolada_opolskie_sr = round(mean(czekolada_opolskie), digits = 2)                       #8
czekolada_podkarpackie_sr = round(mean(czekolada_podkarpackie), digits = 2)               #9
czekolada_podlaskie_sr = round(mean(czekolada_podlaskie), digits = 2)                     #10
czekolada_pomorskie_sr = round(mean(czekolada_pomorskie), digits = 2)                     #11
czekolada_slaskie_sr = round(mean(czekolada_slaskie), digits = 2)                         #12
czekolada_swietokrzyskie_sr = round(mean(czekolada_swietokrzyskie), digits = 2)           #13
czekolada_warminsko_mazurskie_sr = round(mean(czekolada_warminsko_mazurskie), digits = 2) #14
czekolada_wielkopolskie_sr = round(mean(czekolada_wielkopolskie), digits = 2)             #15
czekolada_zachodniopomorskie_sr = round(mean(czekolada_zachodniopomorskie), digits = 2)   #16



#### Czekolada œrednie, najw, najmn ceny w jednym wektorze ####
sr_cena_czekolada = c(czekolada_dolnoslaskie_sr,czekolada_kujawsko_pomorskie_sr,czekolada_lubelskie_sr,czekolada_lubuskie_sr,czekolada_lodzkie_sr,
                      czekolada_malopolskie_sr,czekolada_mazowieckie_sr,czekolada_opolskie_sr,czekolada_podkarpackie_sr,czekolada_podlaskie_sr,
                      czekolada_pomorskie_sr,czekolada_slaskie_sr,czekolada_swietokrzyskie_sr,czekolada_warminsko_mazurskie_sr,
                      czekolada_wielkopolskie_sr,czekolada_zachodniopomorskie_sr)

czekolada_najmn_w_kazdym_woj = c(min(czekolada_dolnoslaskie),min(czekolada_kujawsko_pomorskie),min(czekolada_lubelskie),
                                 min(czekolada_lubuskie),min(czekolada_lodzkie),min(czekolada_malopolskie),
                                 min(czekolada_mazowieckie),min(czekolada_opolskie),min(czekolada_podkarpackie),
                                 min(czekolada_podlaskie),min(czekolada_pomorskie),min(czekolada_slaskie),
                                 min(czekolada_swietokrzyskie),min(czekolada_warminsko_mazurskie),
                                 min(czekolada_wielkopolskie),min(czekolada_zachodniopomorskie))

czekolada_najw_w_kazdym_woj = c(max(czekolada_dolnoslaskie),max(czekolada_kujawsko_pomorskie),max(czekolada_lubelskie),
                                max(czekolada_lubuskie),max(czekolada_lodzkie),max(czekolada_malopolskie),
                                max(czekolada_mazowieckie),max(czekolada_opolskie),max(czekolada_podkarpackie),
                                max(czekolada_podlaskie),max(czekolada_pomorskie),max(czekolada_slaskie),
                                max(czekolada_swietokrzyskie),max(czekolada_warminsko_mazurskie),
                                max(czekolada_wielkopolskie),max(czekolada_zachodniopomorskie))

###  Najwy¿sze i najni¿sze, ró¿ne

czekolada_sr_cena = mean(sr_cena_czekolada) #Œrednia cena czekolady dla ca³ego kraju
czekolada_odsd_cena = sd(sr_cena_czekolada)  #Odchylenie standardowe ceny czekolady

czekolada_najw_sr = max(sr_cena_czekolada) #Najwy¿sza œrednia cena czekolady
czekolada_najm_sr = min(sr_cena_czekolada) #Najni¿sza œrednia cena czekolady
czekolada_najmn_k = min(czekolada_najmn_w_kazdym_woj) #Najni¿sza cena czekolady kiedykolwiek
czekolada_najw_k = max(czekolada_najw_w_kazdym_woj)   #Najwy¿sza cena czekolady kiedykolwiek


czek_najw_sr = as.numeric(match(czekolada_najw_sr,sr_cena_czekolada))  #Numer województwa
czek_najmn_sr = as.numeric(match(czekolada_najm_sr,sr_cena_czekolada)) #Numer województwa
print(paste("Najwy¿sza œrednia cena czekolady jest w województwie", wojewodztwa[czek_najw_sr],
            "i wynosi",czekolada_najw_sr,"za"))
print(paste("Najni¿sza œrednia cena czekolady jest w województwie", wojewodztwa[czek_najmn_sr],
            "i wynosi",czekolada_najm_sr,"za"))


czekolada_najmn_kazde = as.numeric(match(czekolada_najmn_k, czekolada_najmn_w_kazdym_woj))  #Numer województwa
czekolada_najw_kazde = as.numeric(match(czekolada_najw_k,   czekolada_najw_w_kazdym_woj))   #Numer województwa
czekolada_najmn_kiedy =  match(czekolada_najmn_k,           czekolada_slaskie)              #Kiedy
czekolada_najw_kiedy =  match(czekolada_najw_k,             czekolada_podkarpackie)         #Kiedy

print(paste("Najni¿sza cena czekolady by³a w", daty[czekolada_najmn_kiedy], "w województwie", 
            wojewodztwa[czekolada_najmn_kazde],"i wynosi³a", czekolada_najmn_k, "z³"))

print(paste("Najwy¿sza cena czekolady by³a w", daty[czekolada_najw_kiedy], "w województwie",
            wojewodztwa[czekolada_najw_kazde],"i wynosi³a", czekolada_najw_k, "z³"))

print(paste("Odchylenie standardowe cen czekolady wynosi", round(czekolada_odsd_cena, digits = 2), "z³otego"))

#### Korelacja p³acy minimalnej do œredniej ceny czekolady ####
czekolada_placa_min_kor = c(cor(as.numeric(as.character(placa_min_df$V1)), as.numeric(as.character(roczne_sr_czekolada$V1))),
                            cor(as.numeric(as.character(placa_min_df$V2)), as.numeric(as.character(roczne_sr_czekolada$V2))),
                            cor(as.numeric(as.character(placa_min_df$V3)), as.numeric(as.character(roczne_sr_czekolada$V3))),
                            cor(as.numeric(as.character(placa_min_df$V4)), as.numeric(as.character(roczne_sr_czekolada$V4))),
                            cor(as.numeric(as.character(placa_min_df$V5)), as.numeric(as.character(roczne_sr_czekolada$V5))),
                            cor(as.numeric(as.character(placa_min_df$V6)), as.numeric(as.character(roczne_sr_czekolada$V6))),
                            cor(as.numeric(as.character(placa_min_df$V7)), as.numeric(as.character(roczne_sr_czekolada$V7))),
                            cor(as.numeric(as.character(placa_min_df$V8)), as.numeric(as.character(roczne_sr_czekolada$V8))),
                            cor(as.numeric(as.character(placa_min_df$V9)), as.numeric(as.character(roczne_sr_czekolada$V9))),
                            cor(as.numeric(as.character(placa_min_df$V10)), as.numeric(as.character(roczne_sr_czekolada$V10))),
                            cor(as.numeric(as.character(placa_min_df$V11)), as.numeric(as.character(roczne_sr_czekolada$V11))),
                            cor(as.numeric(as.character(placa_min_df$V12)), as.numeric(as.character(roczne_sr_czekolada$V12))),
                            cor(as.numeric(as.character(placa_min_df$V13)), as.numeric(as.character(roczne_sr_czekolada$V13))))


print(paste("Uœredniona korelacja minimalnej pensji do ceny czekolady wynosi", round(mean(czekolada_placa_min_kor), 
                                                                                     digits = 4), "co wskazuje na bardzo siln¹ korelacjê"))
#### Korelacja œredniej pensji do œredniej ceny czekolady ####

czekolada_sr_pensja_kor = c(cor(DOLNO, as.numeric(as.character(roczne_sr_czekolada$V1))),
                            cor(KUJAW, as.numeric(as.character(roczne_sr_czekolada$V2))),
                            cor(LUBEL, as.numeric(as.character(roczne_sr_czekolada$V3))),
                            cor(LUBUS, as.numeric(as.character(roczne_sr_czekolada$V4))),
                            cor(LODZK, as.numeric(as.character(roczne_sr_czekolada$V5))),
                            cor(MALOP, as.numeric(as.character(roczne_sr_czekolada$V6))),
                            cor(MAZOW, as.numeric(as.character(roczne_sr_czekolada$V7))),
                            cor(OPOLS, as.numeric(as.character(roczne_sr_czekolada$V8))),
                            cor(PODKA, as.numeric(as.character(roczne_sr_czekolada$V9))),
                            cor(PODLA, as.numeric(as.character(roczne_sr_czekolada$V10))),
                            cor(POMOR, as.numeric(as.character(roczne_sr_czekolada$V11))),
                            cor(SLASK, as.numeric(as.character(roczne_sr_czekolada$V12))),
                            cor(SWIET, as.numeric(as.character(roczne_sr_czekolada$V12))),
                            cor(WARMI, as.numeric(as.character(roczne_sr_czekolada$V12))),
                            cor(WIELK, as.numeric(as.character(roczne_sr_czekolada$V12))),
                            cor(ZACHO, as.numeric(as.character(roczne_sr_czekolada$V13))))
sr_czekolada_kor = mean(czekolada_sr_pensja_kor)
print(paste("Uœredniona korelacja œredniej pensji do ceny czekolady wynosi", round(sr_czekolada_kor, 
                                                                                   digits = 4), "co wskazuje na bardzo siln¹ korelacjê"))
print(".........................................................")




#### Spodnie edycja nag³ówków i tabel ####
spodnie_df = spodnie_df %>% remove_empty("cols") # Usuwam puste kolumny
spodnie_df2 = spodnie_df2 %>% remove_empty("cols") # Usuwam puste kolumny
spodnie_df = full_join(spodnie_df, spodnie_df2)
rm(spodnie_df2)
spodnie_df = subset(spodnie_df, select = -c(Kod,Nazwa))

#Edtujê nazwy kolumn na sensowne
names(spodnie_df) <- gsub("styczeñ.spodnie..6.11.lat..z.tkaniny.typu.jeans..2..cena.", "styczeñ.", names(spodnie_df))
names(spodnie_df) <- gsub("luty.spodnie..6.11.lat..z.tkaniny.typu.jeans..2..cena.", "luty.", names(spodnie_df))
names(spodnie_df) <- gsub("marzec.spodnie..6.11.lat..z.tkaniny.typu.jeans..2..cena.", "marzec.", names(spodnie_df))
names(spodnie_df) <- gsub("kwiecieñ.spodnie..6.11.lat..z.tkaniny.typu.jeans..2..cena.", "kwiecieñ.", names(spodnie_df))
names(spodnie_df) <- gsub("maj.spodnie..6.11.lat..z.tkaniny.typu.jeans..2..cena.", "maj.", names(spodnie_df))
names(spodnie_df) <- gsub("czerwiec.spodnie..6.11.lat..z.tkaniny.typu.jeans..2..cena.", "czerwiec.", names(spodnie_df))
names(spodnie_df) <- gsub("lipiec.spodnie..6.11.lat..z.tkaniny.typu.jeans..2..cena.", "lipiec.", names(spodnie_df))
names(spodnie_df) <- gsub("sierpieñ.spodnie..6.11.lat..z.tkaniny.typu.jeans..2..cena.", "sierpieñ.", names(spodnie_df))
names(spodnie_df) <- gsub("wrzesieñ.spodnie..6.11.lat..z.tkaniny.typu.jeans..2..cena.", "wrzesieñ.", names(spodnie_df))
names(spodnie_df) <- gsub("paŸdziernik.spodnie..6.11.lat..z.tkaniny.typu.jeans..2..cena.", "paŸdziernik.", names(spodnie_df))
names(spodnie_df) <- gsub("listopad.spodnie..6.11.lat..z.tkaniny.typu.jeans..2..cena.", "listopad.", names(spodnie_df))
names(spodnie_df) <- gsub("grudzieñ.spodnie..6.11.lat..z.tkaniny.typu.jeans..2..cena.", "grudzieñ.", names(spodnie_df))
names(spodnie_df) <- gsub("..z³.", "", names(spodnie_df))

#Nie znalaz³em inteligentnego sposobu na sensowne posortowanie chronologiczne wiêc robiê to ³opatologicznie
names(spodnie_df) <- gsub("styczeñ.2006", "2006 1 styczeñ", names(spodnie_df))
names(spodnie_df) <- gsub("styczeñ.2007", "2007 1 styczeñ", names(spodnie_df))
names(spodnie_df) <- gsub("styczeñ.2008", "2008 1 styczeñ", names(spodnie_df))
names(spodnie_df) <- gsub("styczeñ.2009", "2009 1 styczeñ", names(spodnie_df))
names(spodnie_df) <- gsub("styczeñ.2010", "2010 1 styczeñ", names(spodnie_df))
names(spodnie_df) <- gsub("styczeñ.2011", "2011 1 styczeñ", names(spodnie_df))
names(spodnie_df) <- gsub("styczeñ.2012", "2012 1 styczeñ", names(spodnie_df))
names(spodnie_df) <- gsub("styczeñ.2013", "2013 1 styczeñ", names(spodnie_df))
names(spodnie_df) <- gsub("styczeñ.2014", "2014 1 styczeñ", names(spodnie_df))
names(spodnie_df) <- gsub("styczeñ.2015", "2015 1 styczeñ", names(spodnie_df))
names(spodnie_df) <- gsub("styczeñ.2016", "2016 1 styczeñ", names(spodnie_df))
names(spodnie_df) <- gsub("styczeñ.2017", "2017 1 styczeñ", names(spodnie_df))
names(spodnie_df) <- gsub("styczeñ.2018", "2018 1 styczeñ", names(spodnie_df))
names(spodnie_df) <- gsub("styczeñ.2019", "2019 1 styczeñ", names(spodnie_df))
names(spodnie_df) <- gsub("luty.2006", "2006 2 luty", names(spodnie_df))
names(spodnie_df) <- gsub("luty.2007", "2007 2 luty", names(spodnie_df))
names(spodnie_df) <- gsub("luty.2008", "2008 2 luty", names(spodnie_df))
names(spodnie_df) <- gsub("luty.2009", "2009 2 luty", names(spodnie_df))
names(spodnie_df) <- gsub("luty.2010", "2010 2 luty", names(spodnie_df))
names(spodnie_df) <- gsub("luty.2011", "2011 2 luty", names(spodnie_df))
names(spodnie_df) <- gsub("luty.2012", "2012 2 luty", names(spodnie_df))
names(spodnie_df) <- gsub("luty.2013", "2013 2 luty", names(spodnie_df))
names(spodnie_df) <- gsub("luty.2014", "2014 2 luty", names(spodnie_df))
names(spodnie_df) <- gsub("luty.2015", "2015 2 luty", names(spodnie_df))
names(spodnie_df) <- gsub("luty.2016", "2016 2 luty", names(spodnie_df))
names(spodnie_df) <- gsub("luty.2017", "2017 2 luty", names(spodnie_df))
names(spodnie_df) <- gsub("luty.2018", "2018 2 luty", names(spodnie_df))
names(spodnie_df) <- gsub("luty.2019", "2019 2 luty", names(spodnie_df))
names(spodnie_df) <- gsub("marzec.2006", "2006 3 marzec", names(spodnie_df))
names(spodnie_df) <- gsub("marzec.2007", "2007 3 marzec", names(spodnie_df))
names(spodnie_df) <- gsub("marzec.2008", "2008 3 marzec", names(spodnie_df))
names(spodnie_df) <- gsub("marzec.2009", "2009 3 marzec", names(spodnie_df))
names(spodnie_df) <- gsub("marzec.2010", "2010 3 marzec", names(spodnie_df))
names(spodnie_df) <- gsub("marzec.2011", "2011 3 marzec", names(spodnie_df))
names(spodnie_df) <- gsub("marzec.2012", "2012 3 marzec", names(spodnie_df))
names(spodnie_df) <- gsub("marzec.2013", "2013 3 marzec", names(spodnie_df))
names(spodnie_df) <- gsub("marzec.2014", "2014 3 marzec", names(spodnie_df))
names(spodnie_df) <- gsub("marzec.2015", "2015 3 marzec", names(spodnie_df))
names(spodnie_df) <- gsub("marzec.2016", "2016 3 marzec", names(spodnie_df))
names(spodnie_df) <- gsub("marzec.2017", "2017 3 marzec", names(spodnie_df))
names(spodnie_df) <- gsub("marzec.2018", "2018 3 marzec", names(spodnie_df))
names(spodnie_df) <- gsub("marzec.2019", "2019 3 marzec", names(spodnie_df))
names(spodnie_df) <- gsub("kwiecieñ.2006", "2006 4 kwiecieñ", names(spodnie_df))
names(spodnie_df) <- gsub("kwiecieñ.2007", "2007 4 kwiecieñ", names(spodnie_df))
names(spodnie_df) <- gsub("kwiecieñ.2008", "2008 4 kwiecieñ", names(spodnie_df))
names(spodnie_df) <- gsub("kwiecieñ.2009", "2009 4 kwiecieñ", names(spodnie_df))
names(spodnie_df) <- gsub("kwiecieñ.2010", "2010 4 kwiecieñ", names(spodnie_df))
names(spodnie_df) <- gsub("kwiecieñ.2011", "2011 4 kwiecieñ", names(spodnie_df))
names(spodnie_df) <- gsub("kwiecieñ.2012", "2012 4 kwiecieñ", names(spodnie_df))
names(spodnie_df) <- gsub("kwiecieñ.2013", "2013 4 kwiecieñ", names(spodnie_df))
names(spodnie_df) <- gsub("kwiecieñ.2014", "2014 4 kwiecieñ", names(spodnie_df))
names(spodnie_df) <- gsub("kwiecieñ.2015", "2015 4 kwiecieñ", names(spodnie_df))
names(spodnie_df) <- gsub("kwiecieñ.2016", "2016 4 kwiecieñ", names(spodnie_df))
names(spodnie_df) <- gsub("kwiecieñ.2017", "2017 4 kwiecieñ", names(spodnie_df))
names(spodnie_df) <- gsub("kwiecieñ.2018", "2018 4 kwiecieñ", names(spodnie_df))
names(spodnie_df) <- gsub("kwiecieñ.2019", "2019 4 kwiecieñ", names(spodnie_df))
names(spodnie_df) <- gsub("maj.2006", "2006 5 maj", names(spodnie_df))
names(spodnie_df) <- gsub("maj.2007", "2007 5 maj", names(spodnie_df))
names(spodnie_df) <- gsub("maj.2008", "2008 5 maj", names(spodnie_df))
names(spodnie_df) <- gsub("maj.2009", "2009 5 maj", names(spodnie_df))
names(spodnie_df) <- gsub("maj.2010", "2010 5 maj", names(spodnie_df))
names(spodnie_df) <- gsub("maj.2011", "2011 5 maj", names(spodnie_df))
names(spodnie_df) <- gsub("maj.2012", "2012 5 maj", names(spodnie_df))
names(spodnie_df) <- gsub("maj.2013", "2013 5 maj", names(spodnie_df))
names(spodnie_df) <- gsub("maj.2014", "2014 5 maj", names(spodnie_df))
names(spodnie_df) <- gsub("maj.2015", "2015 5 maj", names(spodnie_df))
names(spodnie_df) <- gsub("maj.2016", "2016 5 maj", names(spodnie_df))
names(spodnie_df) <- gsub("maj.2017", "2017 5 maj", names(spodnie_df))
names(spodnie_df) <- gsub("maj.2018", "2018 5 maj", names(spodnie_df))
names(spodnie_df) <- gsub("maj.2019", "2019 5 maj", names(spodnie_df))
names(spodnie_df) <- gsub("czerwiec.2006", "2006 6 czerwiec", names(spodnie_df))
names(spodnie_df) <- gsub("czerwiec.2007", "2007 6 czerwiec", names(spodnie_df))
names(spodnie_df) <- gsub("czerwiec.2008", "2008 6 czerwiec", names(spodnie_df))
names(spodnie_df) <- gsub("czerwiec.2009", "2009 6 czerwiec", names(spodnie_df))
names(spodnie_df) <- gsub("czerwiec.2010", "2010 6 czerwiec", names(spodnie_df))
names(spodnie_df) <- gsub("czerwiec.2011", "2011 6 czerwiec", names(spodnie_df))
names(spodnie_df) <- gsub("czerwiec.2012", "2012 6 czerwiec", names(spodnie_df))
names(spodnie_df) <- gsub("czerwiec.2013", "2013 6 czerwiec", names(spodnie_df))
names(spodnie_df) <- gsub("czerwiec.2014", "2014 6 czerwiec", names(spodnie_df))
names(spodnie_df) <- gsub("czerwiec.2015", "2015 6 czerwiec", names(spodnie_df))
names(spodnie_df) <- gsub("czerwiec.2016", "2016 6 czerwiec", names(spodnie_df))
names(spodnie_df) <- gsub("czerwiec.2017", "2017 6 czerwiec", names(spodnie_df))
names(spodnie_df) <- gsub("czerwiec.2018", "2018 6 czerwiec", names(spodnie_df))
names(spodnie_df) <- gsub("czerwiec.2019", "2019 6 czerwiec", names(spodnie_df))
names(spodnie_df) <- gsub("lipiec.2006", "2006 7 lipiec", names(spodnie_df))
names(spodnie_df) <- gsub("lipiec.2007", "2007 7 lipiec", names(spodnie_df))
names(spodnie_df) <- gsub("lipiec.2008", "2008 7 lipiec", names(spodnie_df))
names(spodnie_df) <- gsub("lipiec.2009", "2009 7 lipiec", names(spodnie_df))
names(spodnie_df) <- gsub("lipiec.2010", "2010 7 lipiec", names(spodnie_df))
names(spodnie_df) <- gsub("lipiec.2011", "2011 7 lipiec", names(spodnie_df))
names(spodnie_df) <- gsub("lipiec.2012", "2012 7 lipiec", names(spodnie_df))
names(spodnie_df) <- gsub("lipiec.2013", "2013 7 lipiec", names(spodnie_df))
names(spodnie_df) <- gsub("lipiec.2014", "2014 7 lipiec", names(spodnie_df))
names(spodnie_df) <- gsub("lipiec.2015", "2015 7 lipiec", names(spodnie_df))
names(spodnie_df) <- gsub("lipiec.2016", "2016 7 lipiec", names(spodnie_df))
names(spodnie_df) <- gsub("lipiec.2017", "2017 7 lipiec", names(spodnie_df))
names(spodnie_df) <- gsub("lipiec.2018", "2018 7 lipiec", names(spodnie_df))
names(spodnie_df) <- gsub("lipiec.2019", "2019 7 lipiec", names(spodnie_df))
names(spodnie_df) <- gsub("sierpieñ.2006", "2006 8 sierpieñ", names(spodnie_df))
names(spodnie_df) <- gsub("sierpieñ.2007", "2007 8 sierpieñ", names(spodnie_df))
names(spodnie_df) <- gsub("sierpieñ.2008", "2008 8 sierpieñ", names(spodnie_df))
names(spodnie_df) <- gsub("sierpieñ.2009", "2009 8 sierpieñ", names(spodnie_df))
names(spodnie_df) <- gsub("sierpieñ.2010", "2010 8 sierpieñ", names(spodnie_df))
names(spodnie_df) <- gsub("sierpieñ.2011", "2011 8 sierpieñ", names(spodnie_df))
names(spodnie_df) <- gsub("sierpieñ.2012", "2012 8 sierpieñ", names(spodnie_df))
names(spodnie_df) <- gsub("sierpieñ.2013", "2013 8 sierpieñ", names(spodnie_df))
names(spodnie_df) <- gsub("sierpieñ.2014", "2014 8 sierpieñ", names(spodnie_df))
names(spodnie_df) <- gsub("sierpieñ.2015", "2015 8 sierpieñ", names(spodnie_df))
names(spodnie_df) <- gsub("sierpieñ.2016", "2016 8 sierpieñ", names(spodnie_df))
names(spodnie_df) <- gsub("sierpieñ.2017", "2017 8 sierpieñ", names(spodnie_df))
names(spodnie_df) <- gsub("sierpieñ.2018", "2018 8 sierpieñ", names(spodnie_df))
names(spodnie_df) <- gsub("sierpieñ.2019", "2019 8 sierpieñ", names(spodnie_df))
names(spodnie_df) <- gsub("wrzesieñ.2006", "2006 9 wrzesieñ", names(spodnie_df))
names(spodnie_df) <- gsub("wrzesieñ.2007", "2007 9 wrzesieñ", names(spodnie_df))
names(spodnie_df) <- gsub("wrzesieñ.2008", "2008 9 wrzesieñ", names(spodnie_df))
names(spodnie_df) <- gsub("wrzesieñ.2009", "2009 9 wrzesieñ", names(spodnie_df))
names(spodnie_df) <- gsub("wrzesieñ.2010", "2010 9 wrzesieñ", names(spodnie_df))
names(spodnie_df) <- gsub("wrzesieñ.2011", "2011 9 wrzesieñ", names(spodnie_df))
names(spodnie_df) <- gsub("wrzesieñ.2012", "2012 9 wrzesieñ", names(spodnie_df))
names(spodnie_df) <- gsub("wrzesieñ.2013", "2013 9 wrzesieñ", names(spodnie_df))
names(spodnie_df) <- gsub("wrzesieñ.2014", "2014 9 wrzesieñ", names(spodnie_df))
names(spodnie_df) <- gsub("wrzesieñ.2015", "2015 9 wrzesieñ", names(spodnie_df))
names(spodnie_df) <- gsub("wrzesieñ.2016", "2016 9 wrzesieñ", names(spodnie_df))
names(spodnie_df) <- gsub("wrzesieñ.2017", "2017 9 wrzesieñ", names(spodnie_df))
names(spodnie_df) <- gsub("wrzesieñ.2018", "2018 9 wrzesieñ", names(spodnie_df))
names(spodnie_df) <- gsub("wrzesieñ.2019", "2019 9 wrzesieñ", names(spodnie_df))
names(spodnie_df) <- gsub("paŸdziernik.2006", "2006 90 paŸdziernik", names(spodnie_df))
names(spodnie_df) <- gsub("paŸdziernik.2007", "2007 90 paŸdziernik", names(spodnie_df))
names(spodnie_df) <- gsub("paŸdziernik.2008", "2008 90 paŸdziernik", names(spodnie_df))
names(spodnie_df) <- gsub("paŸdziernik.2009", "2009 90 paŸdziernik", names(spodnie_df))
names(spodnie_df) <- gsub("paŸdziernik.2010", "2010 90 paŸdziernik", names(spodnie_df))
names(spodnie_df) <- gsub("paŸdziernik.2011", "2011 90 paŸdziernik", names(spodnie_df))
names(spodnie_df) <- gsub("paŸdziernik.2012", "2012 90 paŸdziernik", names(spodnie_df))
names(spodnie_df) <- gsub("paŸdziernik.2013", "2013 90 paŸdziernik", names(spodnie_df))
names(spodnie_df) <- gsub("paŸdziernik.2014", "2014 90 paŸdziernik", names(spodnie_df))
names(spodnie_df) <- gsub("paŸdziernik.2015", "2015 90 paŸdziernik", names(spodnie_df))
names(spodnie_df) <- gsub("paŸdziernik.2016", "2016 90 paŸdziernik", names(spodnie_df))
names(spodnie_df) <- gsub("paŸdziernik.2017", "2017 90 paŸdziernik", names(spodnie_df))
names(spodnie_df) <- gsub("paŸdziernik.2018", "2018 90 paŸdziernik", names(spodnie_df))
names(spodnie_df) <- gsub("paŸdziernik.2019", "2019 90 paŸdziernik", names(spodnie_df))
names(spodnie_df) <- gsub("listopad.2006", "2006 91 listopad", names(spodnie_df))
names(spodnie_df) <- gsub("listopad.2007", "2007 91 listopad", names(spodnie_df))
names(spodnie_df) <- gsub("listopad.2008", "2008 91 listopad", names(spodnie_df))
names(spodnie_df) <- gsub("listopad.2009", "2009 91 listopad", names(spodnie_df))
names(spodnie_df) <- gsub("listopad.2010", "2010 91 listopad", names(spodnie_df))
names(spodnie_df) <- gsub("listopad.2011", "2011 91 listopad", names(spodnie_df))
names(spodnie_df) <- gsub("listopad.2012", "2012 91 listopad", names(spodnie_df))
names(spodnie_df) <- gsub("listopad.2013", "2013 91 listopad", names(spodnie_df))
names(spodnie_df) <- gsub("listopad.2014", "2014 91 listopad", names(spodnie_df))
names(spodnie_df) <- gsub("listopad.2015", "2015 91 listopad", names(spodnie_df))
names(spodnie_df) <- gsub("listopad.2016", "2016 91 listopad", names(spodnie_df))
names(spodnie_df) <- gsub("listopad.2017", "2017 91 listopad", names(spodnie_df))
names(spodnie_df) <- gsub("listopad.2018", "2018 91 listopad", names(spodnie_df))
names(spodnie_df) <- gsub("listopad.2019", "2019 91 listopad", names(spodnie_df))
names(spodnie_df) <- gsub("grudzieñ.2006", "2006 92 grudzieñ", names(spodnie_df))
names(spodnie_df) <- gsub("grudzieñ.2007", "2007 92 grudzieñ", names(spodnie_df))
names(spodnie_df) <- gsub("grudzieñ.2008", "2008 92 grudzieñ", names(spodnie_df))
names(spodnie_df) <- gsub("grudzieñ.2009", "2009 92 grudzieñ", names(spodnie_df))
names(spodnie_df) <- gsub("grudzieñ.2010", "2010 92 grudzieñ", names(spodnie_df))
names(spodnie_df) <- gsub("grudzieñ.2011", "2011 92 grudzieñ", names(spodnie_df))
names(spodnie_df) <- gsub("grudzieñ.2012", "2012 92 grudzieñ", names(spodnie_df))
names(spodnie_df) <- gsub("grudzieñ.2013", "2013 92 grudzieñ", names(spodnie_df))
names(spodnie_df) <- gsub("grudzieñ.2014", "2014 92 grudzieñ", names(spodnie_df))
names(spodnie_df) <- gsub("grudzieñ.2015", "2015 92 grudzieñ", names(spodnie_df))
names(spodnie_df) <- gsub("grudzieñ.2016", "2016 92 grudzieñ", names(spodnie_df))
names(spodnie_df) <- gsub("grudzieñ.2017", "2017 92 grudzieñ", names(spodnie_df))
names(spodnie_df) <- gsub("grudzieñ.2018", "2018 92 grudzieñ", names(spodnie_df))
names(spodnie_df) <- gsub("grudzieñ.2019", "2019 92 grudzieñ", names(spodnie_df))
spodnie_df = spodnie_df[sort(colnames(spodnie_df))] #Sortujê chronologicznie
#spodnie_df = subset(spodnie_df, select = -c(Nazwa) ) # Usuwam zbêdn¹ kolumnê z nazwazmi województw


spodnie_df = t(spodnie_df) #Tramsponujê dataframe ¿eby poszczególne kolumny by³y województwami 
spodnie_df = unname(spodnie_df) #Usuwam zbêdne teraz nazwy
spodnie_df = as.data.frame(spodnie_df) #Przekszta³cam na dataframe


# Normalnie nie da³o siê przkonwertowaæ danych na numeric, bo zamiast kropek by³y 
# przecinki i niemi³osiernie d³ugo siê nad tym g³owi³em. Ca³y czas otrzymywa³em 
# wartoœci NA
spodnie_dolnoslaskie = as.numeric(gsub(",", ".", as.character(spodnie_df$V1)))
spodnie_kujawsko_pomorskie = as.numeric(gsub(",", ".", as.character(spodnie_df$V2)))
spodnie_lubelskie = as.numeric(gsub(",", ".", as.character(spodnie_df$V3)))
spodnie_lubuskie = as.numeric(gsub(",", ".", as.character(spodnie_df$V4)))
spodnie_lodzkie = as.numeric(gsub(",", ".", as.character(spodnie_df$V5)))
spodnie_malopolskie = as.numeric(gsub(",", ".", as.character(spodnie_df$V6)))
spodnie_mazowieckie = as.numeric(gsub(",", ".", as.character(spodnie_df$V7)))
spodnie_opolskie = as.numeric(gsub(",", ".", as.character(spodnie_df$V8)))
spodnie_podkarpackie = as.numeric(gsub(",", ".", as.character(spodnie_df$V9)))
spodnie_podlaskie = as.numeric(gsub(",", ".", as.character(spodnie_df$V10)))
spodnie_pomorskie = as.numeric(gsub(",", ".", as.character(spodnie_df$V11)))
spodnie_slaskie = as.numeric(gsub(",", ".", as.character(spodnie_df$V12)))
spodnie_swietokrzyskie = as.numeric(gsub(",", ".", as.character(spodnie_df$V13)))
spodnie_warminsko_mazurskie = as.numeric(gsub(",", ".", as.character(spodnie_df$V14)))
spodnie_wielkopolskie = as.numeric(gsub(",", ".", as.character(spodnie_df$V15)))
spodnie_zachodniopomorskie = as.numeric(gsub(",", ".", as.character(spodnie_df$V16)))

#### Wykres cen spodni ####
y = (1:168)
#Wykres dla spodni i wszystkich województw
spodnie_plot = plot_ly(x = y )%>%
  layout(title = 'Zmiana cen spodni(6-11 lat) z tkaniny jeans w latach 2006-2019', xaxis = list(title = 'Rok', 
                                                                                                ticktext = list("2006", "2007", "2008", "2009", "2010", "2011","2012","2013","2014","2015","2016","2017","2018","2019"), 
                                                                                                tickvals = list(1, 12, 24, 36, 48, 60, 72, 84, 96, 108, 120, 132, 144, 156)),
         yaxis = list(title = 'cena (z³)')) %>%
  add_lines(y = spodnie_dolnoslaskie, name = "dolnoœlaskie")%>%
  add_lines(y = spodnie_kujawsko_pomorskie, name = "kujawsko-pomorskie")%>%
  add_lines(y = spodnie_lubelskie, name = "lubelskie")%>%
  add_lines(y = spodnie_lubuskie, name = "lubuskie")%>%
  add_lines(y = spodnie_lodzkie, name = "³ódzkie")%>%
  add_lines(y = spodnie_malopolskie, name = "ma³opolskie")%>%
  add_lines(y = spodnie_mazowieckie, name = "mazowieckie")%>%
  add_lines(y = spodnie_opolskie, name = "opolskie")%>%
  add_lines(y = spodnie_podkarpackie, name = "podkarpackie")%>%
  add_lines(y = spodnie_podlaskie, name = "podlaskie")%>%
  add_lines(y = spodnie_pomorskie, name = "pomorskie")%>%
  add_lines(y = spodnie_slaskie, name = "œl¹skie")%>%
  add_lines(y = spodnie_swietokrzyskie, name = "œwietokrzyskie")%>%
  add_lines(y = spodnie_warminsko_mazurskie, name = "warmiñsko_mazurskie")%>%
  add_lines(y = spodnie_wielkopolskie, name = "wielkopolskie")%>%
  add_lines(y = spodnie_zachodniopomorskie, name = "zachodniopomorskie")
#print(spodnie_plot)

#### Œrednie roczne ceny spodni ####
roczne_sr_spodnie = data.frame('','','','','','','','','','','','','', stringsAsFactors = FALSE)
names(roczne_sr_spodnie) = c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016",
                             "2017","2018")
for (i in 1:13) {
  roczne_sr_spodnie[1,i] = round(mean(spodnie_dolnoslaskie[(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_spodnie[2,i] = round(mean(spodnie_kujawsko_pomorskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_spodnie[3,i] = round(mean(spodnie_lubelskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_spodnie[4,i] = round(mean(spodnie_lubuskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_spodnie[5,i] = round(mean(spodnie_lodzkie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_spodnie[6,i] = round(mean(spodnie_malopolskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_spodnie[7,i] = round(mean(spodnie_mazowieckie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_spodnie[8,i] = round(mean(spodnie_opolskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_spodnie[9,i] = round(mean(spodnie_podkarpackie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_spodnie[10,i] = round(mean(spodnie_podlaskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_spodnie[11,i] = round(mean(spodnie_pomorskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_spodnie[12,i] = round(mean(spodnie_slaskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_spodnie[13,i] = round(mean(spodnie_swietokrzyskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_spodnie[14,i] = round(mean(spodnie_warminsko_mazurskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_spodnie[15,i] = round(mean(spodnie_wielkopolskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_spodnie[16,i] = round(mean(spodnie_zachodniopomorskie[(1+(i-1)*12):(12*i)]), digits = 2)
}
roczne_sr_spodnie = t(roczne_sr_spodnie)
roczne_sr_spodnie = unname(roczne_sr_spodnie) #Usuwam zbêdne teraz nazwy
roczne_sr_spodnie = as.data.frame(roczne_sr_spodnie) #Przekszta³cam na dataframe

#### Œrednia cena spodni dla ka¿dego wojewódstwa ####
spodnie_dolnoslaskie_sr = round(mean(spodnie_dolnoslaskie), digits = 2)               #1
spodnie_kujawsko_pomorskie_sr = round(mean(spodnie_kujawsko_pomorskie), digits = 2)   #2
spodnie_lubelskie_sr = round(mean(spodnie_lubelskie), digits = 2)                     #3
spodnie_lubuskie_sr = round(mean(spodnie_lubuskie), digits = 2)                       #4
spodnie_lodzkie_sr = round(mean(spodnie_lodzkie), digits = 2)                         #5
spodnie_malopolskie_sr = round(mean(spodnie_malopolskie), digits = 2)                 #6
spodnie_mazowieckie_sr = round(mean(spodnie_mazowieckie), digits = 2)                 #7
spodnie_opolskie_sr = round(mean(spodnie_opolskie), digits = 2)                       #8
spodnie_podkarpackie_sr = round(mean(spodnie_podkarpackie), digits = 2)               #9
spodnie_podlaskie_sr = round(mean(spodnie_podlaskie), digits = 2)                     #10
spodnie_pomorskie_sr = round(mean(spodnie_pomorskie), digits = 2)                     #11
spodnie_slaskie_sr = round(mean(spodnie_slaskie), digits = 2)                         #12
spodnie_swietokrzyskie_sr = round(mean(spodnie_swietokrzyskie), digits = 2)           #13
spodnie_warminsko_mazurskie_sr = round(mean(spodnie_warminsko_mazurskie), digits = 2) #14
spodnie_wielkopolskie_sr = round(mean(spodnie_wielkopolskie), digits = 2)             #15
spodnie_zachodniopomorskie_sr = round(mean(spodnie_zachodniopomorskie), digits = 2)   #16



#### spodnie œrednie, najw, najmn ceny w jednym wektorze ####
sr_cena_spodnie = c(spodnie_dolnoslaskie_sr,spodnie_kujawsko_pomorskie_sr,spodnie_lubelskie_sr,spodnie_lubuskie_sr,spodnie_lodzkie_sr,
                    spodnie_malopolskie_sr,spodnie_mazowieckie_sr,spodnie_opolskie_sr,spodnie_podkarpackie_sr,spodnie_podlaskie_sr,
                    spodnie_pomorskie_sr,spodnie_slaskie_sr,spodnie_swietokrzyskie_sr,spodnie_warminsko_mazurskie_sr,
                    spodnie_wielkopolskie_sr,spodnie_zachodniopomorskie_sr)

spodnie_najmn_w_kazdym_woj = c(min(spodnie_dolnoslaskie),min(spodnie_kujawsko_pomorskie),min(spodnie_lubelskie),
                               min(spodnie_lubuskie),min(spodnie_lodzkie),min(spodnie_malopolskie),
                               min(spodnie_mazowieckie),min(spodnie_opolskie),min(spodnie_podkarpackie),
                               min(spodnie_podlaskie),min(spodnie_pomorskie),min(spodnie_slaskie),
                               min(spodnie_swietokrzyskie),min(spodnie_warminsko_mazurskie),
                               min(spodnie_wielkopolskie),min(spodnie_zachodniopomorskie))

spodnie_najw_w_kazdym_woj = c(max(spodnie_dolnoslaskie),max(spodnie_kujawsko_pomorskie),max(spodnie_lubelskie),
                              max(spodnie_lubuskie),max(spodnie_lodzkie),max(spodnie_malopolskie),
                              max(spodnie_mazowieckie),max(spodnie_opolskie),max(spodnie_podkarpackie),
                              max(spodnie_podlaskie),max(spodnie_pomorskie),max(spodnie_slaskie),
                              max(spodnie_swietokrzyskie),max(spodnie_warminsko_mazurskie),
                              max(spodnie_wielkopolskie),max(spodnie_zachodniopomorskie))

###  Najwy¿sze i najni¿sze, ró¿ne

spodnie_sr_cena = mean(sr_cena_spodnie) #Œrednia cena spodni dla ca³ego kraju
spodnie_odsd_cena = sd(sr_cena_spodnie)  #Odchylenie standardowe ceny spodni

spodnie_najw_sr = max(sr_cena_spodnie) #Najwy¿sza œrednia cena spodni
spodnie_najm_sr = min(sr_cena_spodnie) #Najni¿sza œrednia cena spodni
spodnie_najmn_k = min(spodnie_najmn_w_kazdym_woj) #Najni¿sza cena spodni kiedykolwiek
spodnie_najw_k = max(spodnie_najw_w_kazdym_woj)   #Najwy¿sza cena spodni kiedykolwiek


spod_najw_sr = as.numeric(match(spodnie_najw_sr,sr_cena_spodnie))  #Numer województwa
spod_najmn_sr = as.numeric(match(spodnie_najm_sr,sr_cena_spodnie)) #Numer województwa
print(paste("Najwy¿sza œrednia cena spodni jest w województwie", wojewodztwa[spod_najw_sr],
            "i wynosi",spodnie_najw_sr))
print(paste("Najni¿sza œrednia cena spodni jest w województwie", wojewodztwa[spod_najmn_sr],
            "i wynosi",spodnie_najm_sr))


spodnie_najmn_kazde = as.numeric(match(spodnie_najmn_k, spodnie_najmn_w_kazdym_woj))  #Numer województwa
spodnie_najw_kazde = as.numeric(match(spodnie_najw_k,   spodnie_najw_w_kazdym_woj))   #Numer województwa
spodnie_najmn_kiedy =  match(spodnie_najmn_k,           spodnie_zachodniopomorskie)   #Kiedy
spodnie_najw_kiedy =  match(spodnie_najw_k,             spodnie_swietokrzyskie)       #Kiedy

print(paste("Najni¿sza cena spodni by³a w", daty[spodnie_najmn_kiedy], "w województwie", 
            wojewodztwa[spodnie_najmn_kazde],"i wynosi³a", spodnie_najmn_k))

print(paste("Najwy¿sza cena spodni by³a w", daty[spodnie_najw_kiedy], "w województwie",
            wojewodztwa[spodnie_najw_kazde],"i wynosi³a", spodnie_najw_k))

print(paste("Odchylenie standardowe cen spodni wynosi", round(spodnie_odsd_cena, digits = 2), "z³otego"))

#### Korelacja p³acy minimalnej do œredniej ceny spodni ####
spodnie_placa_min_kor = c(cor(as.numeric(as.character(placa_min_df$V1)), as.numeric(as.character(roczne_sr_spodnie$V1))),
                          cor(as.numeric(as.character(placa_min_df$V2)), as.numeric(as.character(roczne_sr_spodnie$V2))),
                          cor(as.numeric(as.character(placa_min_df$V3)), as.numeric(as.character(roczne_sr_spodnie$V3))),
                          cor(as.numeric(as.character(placa_min_df$V4)), as.numeric(as.character(roczne_sr_spodnie$V4))),
                          cor(as.numeric(as.character(placa_min_df$V5)), as.numeric(as.character(roczne_sr_spodnie$V5))),
                          cor(as.numeric(as.character(placa_min_df$V6)), as.numeric(as.character(roczne_sr_spodnie$V6))),
                          cor(as.numeric(as.character(placa_min_df$V7)), as.numeric(as.character(roczne_sr_spodnie$V7))),
                          cor(as.numeric(as.character(placa_min_df$V8)), as.numeric(as.character(roczne_sr_spodnie$V8))),
                          cor(as.numeric(as.character(placa_min_df$V9)), as.numeric(as.character(roczne_sr_spodnie$V9))),
                          cor(as.numeric(as.character(placa_min_df$V10)), as.numeric(as.character(roczne_sr_spodnie$V10))),
                          cor(as.numeric(as.character(placa_min_df$V11)), as.numeric(as.character(roczne_sr_spodnie$V11))),
                          cor(as.numeric(as.character(placa_min_df$V12)), as.numeric(as.character(roczne_sr_spodnie$V12))),
                          cor(as.numeric(as.character(placa_min_df$V13)), as.numeric(as.character(roczne_sr_spodnie$V13))))


print(paste("Uœredniona korelacja minimalnej pensji do ceny spodni wynosi", round(mean(spodnie_placa_min_kor), 
                                                                                  digits = 4), "co wskazuje na brak korelacji"))
#### Korelacja œredniej pensji do œredniej ceny spodni ####

spodnie_sr_pensja_kor = c(cor(DOLNO, as.numeric(as.character(roczne_sr_spodnie$V1))),
                          cor(KUJAW, as.numeric(as.character(roczne_sr_spodnie$V2))),
                          cor(LUBEL, as.numeric(as.character(roczne_sr_spodnie$V3))),
                          cor(LUBUS, as.numeric(as.character(roczne_sr_spodnie$V4))),
                          cor(LODZK, as.numeric(as.character(roczne_sr_spodnie$V5))),
                          cor(MALOP, as.numeric(as.character(roczne_sr_spodnie$V6))),
                          cor(MAZOW, as.numeric(as.character(roczne_sr_spodnie$V7))),
                          cor(OPOLS, as.numeric(as.character(roczne_sr_spodnie$V8))),
                          cor(PODKA, as.numeric(as.character(roczne_sr_spodnie$V9))),
                          cor(PODLA, as.numeric(as.character(roczne_sr_spodnie$V10))),
                          cor(POMOR, as.numeric(as.character(roczne_sr_spodnie$V11))),
                          cor(SLASK, as.numeric(as.character(roczne_sr_spodnie$V12))),
                          cor(SWIET, as.numeric(as.character(roczne_sr_spodnie$V13))),
                          cor(WARMI, as.numeric(as.character(roczne_sr_spodnie$V14))),
                          cor(WIELK, as.numeric(as.character(roczne_sr_spodnie$V15))),
                          cor(ZACHO, as.numeric(as.character(roczne_sr_spodnie$V16))))
sr_spodnie_kor = mean(spodnie_sr_pensja_kor)
print(paste("Uœredniona korelacja œredniej pensji do ceny spodni wynosi", round(sr_spodnie_kor, 
                                                                                digits = 4), "co wskazuje na brak korelacji"))
print(".........................................................")




#### Pó³buty edycja nag³ówków i tabel ####
buty_df = buty_df %>% remove_empty("cols") # Usuwam puste kolumny
buty_df = subset(buty_df, select = -c(Kod,Nazwa))

#Edtujê nazwy kolumn na sensowne
names(buty_df) <- gsub("styczeñ.pó³buty.damskie.skórzane.na.podeszwie.nieskórzanej...za.1parê.cena.", "styczeñ.", names(buty_df))
names(buty_df) <- gsub("luty.pó³buty.damskie.skórzane.na.podeszwie.nieskórzanej...za.1parê.cena.", "luty.", names(buty_df))
names(buty_df) <- gsub("marzec.pó³buty.damskie.skórzane.na.podeszwie.nieskórzanej...za.1parê.cena.", "marzec.", names(buty_df))
names(buty_df) <- gsub("kwiecieñ.pó³buty.damskie.skórzane.na.podeszwie.nieskórzanej...za.1parê.cena.", "kwiecieñ.", names(buty_df))
names(buty_df) <- gsub("maj.pó³buty.damskie.skórzane.na.podeszwie.nieskórzanej...za.1parê.cena.", "maj.", names(buty_df))
names(buty_df) <- gsub("czerwiec.pó³buty.damskie.skórzane.na.podeszwie.nieskórzanej...za.1parê.cena.", "czerwiec.", names(buty_df))
names(buty_df) <- gsub("lipiec.pó³buty.damskie.skórzane.na.podeszwie.nieskórzanej...za.1parê.cena.", "lipiec.", names(buty_df))
names(buty_df) <- gsub("sierpieñ.pó³buty.damskie.skórzane.na.podeszwie.nieskórzanej...za.1parê.cena.", "sierpieñ.", names(buty_df))
names(buty_df) <- gsub("wrzesieñ.pó³buty.damskie.skórzane.na.podeszwie.nieskórzanej...za.1parê.cena.", "wrzesieñ.", names(buty_df))
names(buty_df) <- gsub("paŸdziernik.pó³buty.damskie.skórzane.na.podeszwie.nieskórzanej...za.1parê.cena.", "paŸdziernik.", names(buty_df))
names(buty_df) <- gsub("listopad.pó³buty.damskie.skórzane.na.podeszwie.nieskórzanej...za.1parê.cena.", "listopad.", names(buty_df))
names(buty_df) <- gsub("grudzieñ.pó³buty.damskie.skórzane.na.podeszwie.nieskórzanej...za.1parê.cena.", "grudzieñ.", names(buty_df))
names(buty_df) <- gsub("..z³.", "", names(buty_df))

#Nie znalaz³em inteligentnego sposobu na sensowne posortowanie chronologiczne wiêc robiê to ³opatologicznie
names(buty_df) <- gsub("styczeñ.2006", "2006 1 styczeñ", names(buty_df))
names(buty_df) <- gsub("styczeñ.2007", "2007 1 styczeñ", names(buty_df))
names(buty_df) <- gsub("styczeñ.2008", "2008 1 styczeñ", names(buty_df))
names(buty_df) <- gsub("styczeñ.2009", "2009 1 styczeñ", names(buty_df))
names(buty_df) <- gsub("styczeñ.2010", "2010 1 styczeñ", names(buty_df))
names(buty_df) <- gsub("styczeñ.2011", "2011 1 styczeñ", names(buty_df))
names(buty_df) <- gsub("styczeñ.2012", "2012 1 styczeñ", names(buty_df))
names(buty_df) <- gsub("styczeñ.2013", "2013 1 styczeñ", names(buty_df))
names(buty_df) <- gsub("styczeñ.2014", "2014 1 styczeñ", names(buty_df))
names(buty_df) <- gsub("styczeñ.2015", "2015 1 styczeñ", names(buty_df))
names(buty_df) <- gsub("styczeñ.2016", "2016 1 styczeñ", names(buty_df))
names(buty_df) <- gsub("styczeñ.2017", "2017 1 styczeñ", names(buty_df))
names(buty_df) <- gsub("styczeñ.2018", "2018 1 styczeñ", names(buty_df))
names(buty_df) <- gsub("styczeñ.2019", "2019 1 styczeñ", names(buty_df))
names(buty_df) <- gsub("luty.2006", "2006 2 luty", names(buty_df))
names(buty_df) <- gsub("luty.2007", "2007 2 luty", names(buty_df))
names(buty_df) <- gsub("luty.2008", "2008 2 luty", names(buty_df))
names(buty_df) <- gsub("luty.2009", "2009 2 luty", names(buty_df))
names(buty_df) <- gsub("luty.2010", "2010 2 luty", names(buty_df))
names(buty_df) <- gsub("luty.2011", "2011 2 luty", names(buty_df))
names(buty_df) <- gsub("luty.2012", "2012 2 luty", names(buty_df))
names(buty_df) <- gsub("luty.2013", "2013 2 luty", names(buty_df))
names(buty_df) <- gsub("luty.2014", "2014 2 luty", names(buty_df))
names(buty_df) <- gsub("luty.2015", "2015 2 luty", names(buty_df))
names(buty_df) <- gsub("luty.2016", "2016 2 luty", names(buty_df))
names(buty_df) <- gsub("luty.2017", "2017 2 luty", names(buty_df))
names(buty_df) <- gsub("luty.2018", "2018 2 luty", names(buty_df))
names(buty_df) <- gsub("luty.2019", "2019 2 luty", names(buty_df))
names(buty_df) <- gsub("marzec.2006", "2006 3 marzec", names(buty_df))
names(buty_df) <- gsub("marzec.2007", "2007 3 marzec", names(buty_df))
names(buty_df) <- gsub("marzec.2008", "2008 3 marzec", names(buty_df))
names(buty_df) <- gsub("marzec.2009", "2009 3 marzec", names(buty_df))
names(buty_df) <- gsub("marzec.2010", "2010 3 marzec", names(buty_df))
names(buty_df) <- gsub("marzec.2011", "2011 3 marzec", names(buty_df))
names(buty_df) <- gsub("marzec.2012", "2012 3 marzec", names(buty_df))
names(buty_df) <- gsub("marzec.2013", "2013 3 marzec", names(buty_df))
names(buty_df) <- gsub("marzec.2014", "2014 3 marzec", names(buty_df))
names(buty_df) <- gsub("marzec.2015", "2015 3 marzec", names(buty_df))
names(buty_df) <- gsub("marzec.2016", "2016 3 marzec", names(buty_df))
names(buty_df) <- gsub("marzec.2017", "2017 3 marzec", names(buty_df))
names(buty_df) <- gsub("marzec.2018", "2018 3 marzec", names(buty_df))
names(buty_df) <- gsub("marzec.2019", "2019 3 marzec", names(buty_df))
names(buty_df) <- gsub("kwiecieñ.2006", "2006 4 kwiecieñ", names(buty_df))
names(buty_df) <- gsub("kwiecieñ.2007", "2007 4 kwiecieñ", names(buty_df))
names(buty_df) <- gsub("kwiecieñ.2008", "2008 4 kwiecieñ", names(buty_df))
names(buty_df) <- gsub("kwiecieñ.2009", "2009 4 kwiecieñ", names(buty_df))
names(buty_df) <- gsub("kwiecieñ.2010", "2010 4 kwiecieñ", names(buty_df))
names(buty_df) <- gsub("kwiecieñ.2011", "2011 4 kwiecieñ", names(buty_df))
names(buty_df) <- gsub("kwiecieñ.2012", "2012 4 kwiecieñ", names(buty_df))
names(buty_df) <- gsub("kwiecieñ.2013", "2013 4 kwiecieñ", names(buty_df))
names(buty_df) <- gsub("kwiecieñ.2014", "2014 4 kwiecieñ", names(buty_df))
names(buty_df) <- gsub("kwiecieñ.2015", "2015 4 kwiecieñ", names(buty_df))
names(buty_df) <- gsub("kwiecieñ.2016", "2016 4 kwiecieñ", names(buty_df))
names(buty_df) <- gsub("kwiecieñ.2017", "2017 4 kwiecieñ", names(buty_df))
names(buty_df) <- gsub("kwiecieñ.2018", "2018 4 kwiecieñ", names(buty_df))
names(buty_df) <- gsub("kwiecieñ.2019", "2019 4 kwiecieñ", names(buty_df))
names(buty_df) <- gsub("maj.2006", "2006 5 maj", names(buty_df))
names(buty_df) <- gsub("maj.2007", "2007 5 maj", names(buty_df))
names(buty_df) <- gsub("maj.2008", "2008 5 maj", names(buty_df))
names(buty_df) <- gsub("maj.2009", "2009 5 maj", names(buty_df))
names(buty_df) <- gsub("maj.2010", "2010 5 maj", names(buty_df))
names(buty_df) <- gsub("maj.2011", "2011 5 maj", names(buty_df))
names(buty_df) <- gsub("maj.2012", "2012 5 maj", names(buty_df))
names(buty_df) <- gsub("maj.2013", "2013 5 maj", names(buty_df))
names(buty_df) <- gsub("maj.2014", "2014 5 maj", names(buty_df))
names(buty_df) <- gsub("maj.2015", "2015 5 maj", names(buty_df))
names(buty_df) <- gsub("maj.2016", "2016 5 maj", names(buty_df))
names(buty_df) <- gsub("maj.2017", "2017 5 maj", names(buty_df))
names(buty_df) <- gsub("maj.2018", "2018 5 maj", names(buty_df))
names(buty_df) <- gsub("maj.2019", "2019 5 maj", names(buty_df))
names(buty_df) <- gsub("czerwiec.2006", "2006 6 czerwiec", names(buty_df))
names(buty_df) <- gsub("czerwiec.2007", "2007 6 czerwiec", names(buty_df))
names(buty_df) <- gsub("czerwiec.2008", "2008 6 czerwiec", names(buty_df))
names(buty_df) <- gsub("czerwiec.2009", "2009 6 czerwiec", names(buty_df))
names(buty_df) <- gsub("czerwiec.2010", "2010 6 czerwiec", names(buty_df))
names(buty_df) <- gsub("czerwiec.2011", "2011 6 czerwiec", names(buty_df))
names(buty_df) <- gsub("czerwiec.2012", "2012 6 czerwiec", names(buty_df))
names(buty_df) <- gsub("czerwiec.2013", "2013 6 czerwiec", names(buty_df))
names(buty_df) <- gsub("czerwiec.2014", "2014 6 czerwiec", names(buty_df))
names(buty_df) <- gsub("czerwiec.2015", "2015 6 czerwiec", names(buty_df))
names(buty_df) <- gsub("czerwiec.2016", "2016 6 czerwiec", names(buty_df))
names(buty_df) <- gsub("czerwiec.2017", "2017 6 czerwiec", names(buty_df))
names(buty_df) <- gsub("czerwiec.2018", "2018 6 czerwiec", names(buty_df))
names(buty_df) <- gsub("czerwiec.2019", "2019 6 czerwiec", names(buty_df))
names(buty_df) <- gsub("lipiec.2006", "2006 7 lipiec", names(buty_df))
names(buty_df) <- gsub("lipiec.2007", "2007 7 lipiec", names(buty_df))
names(buty_df) <- gsub("lipiec.2008", "2008 7 lipiec", names(buty_df))
names(buty_df) <- gsub("lipiec.2009", "2009 7 lipiec", names(buty_df))
names(buty_df) <- gsub("lipiec.2010", "2010 7 lipiec", names(buty_df))
names(buty_df) <- gsub("lipiec.2011", "2011 7 lipiec", names(buty_df))
names(buty_df) <- gsub("lipiec.2012", "2012 7 lipiec", names(buty_df))
names(buty_df) <- gsub("lipiec.2013", "2013 7 lipiec", names(buty_df))
names(buty_df) <- gsub("lipiec.2014", "2014 7 lipiec", names(buty_df))
names(buty_df) <- gsub("lipiec.2015", "2015 7 lipiec", names(buty_df))
names(buty_df) <- gsub("lipiec.2016", "2016 7 lipiec", names(buty_df))
names(buty_df) <- gsub("lipiec.2017", "2017 7 lipiec", names(buty_df))
names(buty_df) <- gsub("lipiec.2018", "2018 7 lipiec", names(buty_df))
names(buty_df) <- gsub("lipiec.2019", "2019 7 lipiec", names(buty_df))
names(buty_df) <- gsub("sierpieñ.2006", "2006 8 sierpieñ", names(buty_df))
names(buty_df) <- gsub("sierpieñ.2007", "2007 8 sierpieñ", names(buty_df))
names(buty_df) <- gsub("sierpieñ.2008", "2008 8 sierpieñ", names(buty_df))
names(buty_df) <- gsub("sierpieñ.2009", "2009 8 sierpieñ", names(buty_df))
names(buty_df) <- gsub("sierpieñ.2010", "2010 8 sierpieñ", names(buty_df))
names(buty_df) <- gsub("sierpieñ.2011", "2011 8 sierpieñ", names(buty_df))
names(buty_df) <- gsub("sierpieñ.2012", "2012 8 sierpieñ", names(buty_df))
names(buty_df) <- gsub("sierpieñ.2013", "2013 8 sierpieñ", names(buty_df))
names(buty_df) <- gsub("sierpieñ.2014", "2014 8 sierpieñ", names(buty_df))
names(buty_df) <- gsub("sierpieñ.2015", "2015 8 sierpieñ", names(buty_df))
names(buty_df) <- gsub("sierpieñ.2016", "2016 8 sierpieñ", names(buty_df))
names(buty_df) <- gsub("sierpieñ.2017", "2017 8 sierpieñ", names(buty_df))
names(buty_df) <- gsub("sierpieñ.2018", "2018 8 sierpieñ", names(buty_df))
names(buty_df) <- gsub("sierpieñ.2019", "2019 8 sierpieñ", names(buty_df))
names(buty_df) <- gsub("wrzesieñ.2006", "2006 9 wrzesieñ", names(buty_df))
names(buty_df) <- gsub("wrzesieñ.2007", "2007 9 wrzesieñ", names(buty_df))
names(buty_df) <- gsub("wrzesieñ.2008", "2008 9 wrzesieñ", names(buty_df))
names(buty_df) <- gsub("wrzesieñ.2009", "2009 9 wrzesieñ", names(buty_df))
names(buty_df) <- gsub("wrzesieñ.2010", "2010 9 wrzesieñ", names(buty_df))
names(buty_df) <- gsub("wrzesieñ.2011", "2011 9 wrzesieñ", names(buty_df))
names(buty_df) <- gsub("wrzesieñ.2012", "2012 9 wrzesieñ", names(buty_df))
names(buty_df) <- gsub("wrzesieñ.2013", "2013 9 wrzesieñ", names(buty_df))
names(buty_df) <- gsub("wrzesieñ.2014", "2014 9 wrzesieñ", names(buty_df))
names(buty_df) <- gsub("wrzesieñ.2015", "2015 9 wrzesieñ", names(buty_df))
names(buty_df) <- gsub("wrzesieñ.2016", "2016 9 wrzesieñ", names(buty_df))
names(buty_df) <- gsub("wrzesieñ.2017", "2017 9 wrzesieñ", names(buty_df))
names(buty_df) <- gsub("wrzesieñ.2018", "2018 9 wrzesieñ", names(buty_df))
names(buty_df) <- gsub("wrzesieñ.2019", "2019 9 wrzesieñ", names(buty_df))
names(buty_df) <- gsub("paŸdziernik.2006", "2006 90 paŸdziernik", names(buty_df))
names(buty_df) <- gsub("paŸdziernik.2007", "2007 90 paŸdziernik", names(buty_df))
names(buty_df) <- gsub("paŸdziernik.2008", "2008 90 paŸdziernik", names(buty_df))
names(buty_df) <- gsub("paŸdziernik.2009", "2009 90 paŸdziernik", names(buty_df))
names(buty_df) <- gsub("paŸdziernik.2010", "2010 90 paŸdziernik", names(buty_df))
names(buty_df) <- gsub("paŸdziernik.2011", "2011 90 paŸdziernik", names(buty_df))
names(buty_df) <- gsub("paŸdziernik.2012", "2012 90 paŸdziernik", names(buty_df))
names(buty_df) <- gsub("paŸdziernik.2013", "2013 90 paŸdziernik", names(buty_df))
names(buty_df) <- gsub("paŸdziernik.2014", "2014 90 paŸdziernik", names(buty_df))
names(buty_df) <- gsub("paŸdziernik.2015", "2015 90 paŸdziernik", names(buty_df))
names(buty_df) <- gsub("paŸdziernik.2016", "2016 90 paŸdziernik", names(buty_df))
names(buty_df) <- gsub("paŸdziernik.2017", "2017 90 paŸdziernik", names(buty_df))
names(buty_df) <- gsub("paŸdziernik.2018", "2018 90 paŸdziernik", names(buty_df))
names(buty_df) <- gsub("paŸdziernik.2019", "2019 90 paŸdziernik", names(buty_df))
names(buty_df) <- gsub("listopad.2006", "2006 91 listopad", names(buty_df))
names(buty_df) <- gsub("listopad.2007", "2007 91 listopad", names(buty_df))
names(buty_df) <- gsub("listopad.2008", "2008 91 listopad", names(buty_df))
names(buty_df) <- gsub("listopad.2009", "2009 91 listopad", names(buty_df))
names(buty_df) <- gsub("listopad.2010", "2010 91 listopad", names(buty_df))
names(buty_df) <- gsub("listopad.2011", "2011 91 listopad", names(buty_df))
names(buty_df) <- gsub("listopad.2012", "2012 91 listopad", names(buty_df))
names(buty_df) <- gsub("listopad.2013", "2013 91 listopad", names(buty_df))
names(buty_df) <- gsub("listopad.2014", "2014 91 listopad", names(buty_df))
names(buty_df) <- gsub("listopad.2015", "2015 91 listopad", names(buty_df))
names(buty_df) <- gsub("listopad.2016", "2016 91 listopad", names(buty_df))
names(buty_df) <- gsub("listopad.2017", "2017 91 listopad", names(buty_df))
names(buty_df) <- gsub("listopad.2018", "2018 91 listopad", names(buty_df))
names(buty_df) <- gsub("listopad.2019", "2019 91 listopad", names(buty_df))
names(buty_df) <- gsub("grudzieñ.2006", "2006 92 grudzieñ", names(buty_df))
names(buty_df) <- gsub("grudzieñ.2007", "2007 92 grudzieñ", names(buty_df))
names(buty_df) <- gsub("grudzieñ.2008", "2008 92 grudzieñ", names(buty_df))
names(buty_df) <- gsub("grudzieñ.2009", "2009 92 grudzieñ", names(buty_df))
names(buty_df) <- gsub("grudzieñ.2010", "2010 92 grudzieñ", names(buty_df))
names(buty_df) <- gsub("grudzieñ.2011", "2011 92 grudzieñ", names(buty_df))
names(buty_df) <- gsub("grudzieñ.2012", "2012 92 grudzieñ", names(buty_df))
names(buty_df) <- gsub("grudzieñ.2013", "2013 92 grudzieñ", names(buty_df))
names(buty_df) <- gsub("grudzieñ.2014", "2014 92 grudzieñ", names(buty_df))
names(buty_df) <- gsub("grudzieñ.2015", "2015 92 grudzieñ", names(buty_df))
names(buty_df) <- gsub("grudzieñ.2016", "2016 92 grudzieñ", names(buty_df))
names(buty_df) <- gsub("grudzieñ.2017", "2017 92 grudzieñ", names(buty_df))
names(buty_df) <- gsub("grudzieñ.2018", "2018 92 grudzieñ", names(buty_df))
names(buty_df) <- gsub("grudzieñ.2019", "2019 92 grudzieñ", names(buty_df))
buty_df = buty_df[sort(colnames(buty_df))] #Sortujê chronologicznie
#buty_df = subset(buty_df, select = -c(Nazwa) ) # Usuwam zbêdn¹ kolumnê z nazwazmi województw


buty_df = t(buty_df) #Tramsponujê dataframe ¿eby poszczególne kolumny by³y województwami 
buty_df = unname(buty_df) #Usuwam zbêdne teraz nazwy
buty_df = as.data.frame(buty_df) #Przekszta³cam na dataframe


# Normalnie nie da³o siê przkonwertowaæ danych na numeric, bo zamiast kropek by³y 
# przecinki i niemi³osiernie d³ugo siê nad tym g³owi³em. Ca³y czas otrzymywa³em 
# wartoœci NA
buty_dolnoslaskie = as.numeric(gsub(",", ".", as.character(buty_df$V1)))
buty_kujawsko_pomorskie = as.numeric(gsub(",", ".", as.character(buty_df$V2)))
buty_lubelskie = as.numeric(gsub(",", ".", as.character(buty_df$V3)))
buty_lubuskie = as.numeric(gsub(",", ".", as.character(buty_df$V4)))
buty_lodzkie = as.numeric(gsub(",", ".", as.character(buty_df$V5)))
buty_malopolskie = as.numeric(gsub(",", ".", as.character(buty_df$V6)))
buty_mazowieckie = as.numeric(gsub(",", ".", as.character(buty_df$V7)))
buty_opolskie = as.numeric(gsub(",", ".", as.character(buty_df$V8)))
buty_podkarpackie = as.numeric(gsub(",", ".", as.character(buty_df$V9)))
buty_podlaskie = as.numeric(gsub(",", ".", as.character(buty_df$V10)))
buty_pomorskie = as.numeric(gsub(",", ".", as.character(buty_df$V11)))
buty_slaskie = as.numeric(gsub(",", ".", as.character(buty_df$V12)))
buty_swietokrzyskie = as.numeric(gsub(",", ".", as.character(buty_df$V13)))
buty_warminsko_mazurskie = as.numeric(gsub(",", ".", as.character(buty_df$V14)))
buty_wielkopolskie = as.numeric(gsub(",", ".", as.character(buty_df$V15)))
buty_zachodniopomorskie = as.numeric(gsub(",", ".", as.character(buty_df$V16)))

#### Wykres cen pó³butów ####
y = (1:168)
#Wykres dla buty i wszystkich województw
buty_plot = plot_ly(x = y )%>%
  layout(title = 'Zmiana cen pó³butów damskich w latach 2006-2019', xaxis = list(title = 'Rok', 
                                                                                 ticktext = list("2006", "2007", "2008", "2009", "2010", "2011","2012","2013","2014","2015","2016","2017","2018","2019"), 
                                                                                 tickvals = list(1, 12, 24, 36, 48, 60, 72, 84, 96, 108, 120, 132, 144, 156)),
         yaxis = list(title = 'cena (z³)')) %>%
  add_lines(y = buty_dolnoslaskie, name = "dolnoœlaskie")%>%
  add_lines(y = buty_kujawsko_pomorskie, name = "kujawsko-pomorskie")%>%
  add_lines(y = buty_lubelskie, name = "lubelskie")%>%
  add_lines(y = buty_lubuskie, name = "lubuskie")%>%
  add_lines(y = buty_lodzkie, name = "³ódzkie")%>%
  add_lines(y = buty_malopolskie, name = "ma³opolskie")%>%
  add_lines(y = buty_mazowieckie, name = "mazowieckie")%>%
  add_lines(y = buty_opolskie, name = "opolskie")%>%
  add_lines(y = buty_podkarpackie, name = "podkarpackie")%>%
  add_lines(y = buty_podlaskie, name = "podlaskie")%>%
  add_lines(y = buty_pomorskie, name = "pomorskie")%>%
  add_lines(y = buty_slaskie, name = "œl¹skie")%>%
  add_lines(y = buty_swietokrzyskie, name = "œwietokrzyskie")%>%
  add_lines(y = buty_warminsko_mazurskie, name = "warmiñsko_mazurskie")%>%
  add_lines(y = buty_wielkopolskie, name = "wielkopolskie")%>%
  add_lines(y = buty_zachodniopomorskie, name = "zachodniopomorskie")
#print(buty_plot)

#### Œrednie roczne ceny pó³butów ####
roczne_sr_buty = data.frame('','','','','','','','','','','','','', stringsAsFactors = FALSE)
names(roczne_sr_buty) = c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016",
                          "2017","2018")
for (i in 1:13) {
  roczne_sr_buty[1,i] = round(mean(buty_dolnoslaskie[(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_buty[2,i] = round(mean(buty_kujawsko_pomorskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_buty[3,i] = round(mean(buty_lubelskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_buty[4,i] = round(mean(buty_lubuskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_buty[5,i] = round(mean(buty_lodzkie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_buty[6,i] = round(mean(buty_malopolskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_buty[7,i] = round(mean(buty_mazowieckie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_buty[8,i] = round(mean(buty_opolskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_buty[9,i] = round(mean(buty_podkarpackie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_buty[10,i] = round(mean(buty_podlaskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_buty[11,i] = round(mean(buty_pomorskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_buty[12,i] = round(mean(buty_slaskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_buty[13,i] = round(mean(buty_swietokrzyskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_buty[14,i] = round(mean(buty_warminsko_mazurskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_buty[15,i] = round(mean(buty_wielkopolskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_buty[16,i] = round(mean(buty_zachodniopomorskie[(1+(i-1)*12):(12*i)]), digits = 2)
}
roczne_sr_buty = t(roczne_sr_buty)
roczne_sr_buty = unname(roczne_sr_buty) #Usuwam zbêdne teraz nazwy
roczne_sr_buty = as.data.frame(roczne_sr_buty) #Przekszta³cam na dataframe

#### Œrednia cena pó³butów dla ka¿dego wojewódstwa ####
buty_dolnoslaskie_sr = round(mean(buty_dolnoslaskie), digits = 2)               #1
buty_kujawsko_pomorskie_sr = round(mean(buty_kujawsko_pomorskie), digits = 2)   #2
buty_lubelskie_sr = round(mean(buty_lubelskie), digits = 2)                     #3
buty_lubuskie_sr = round(mean(buty_lubuskie), digits = 2)                       #4
buty_lodzkie_sr = round(mean(buty_lodzkie), digits = 2)                         #5
buty_malopolskie_sr = round(mean(buty_malopolskie), digits = 2)                 #6
buty_mazowieckie_sr = round(mean(buty_mazowieckie), digits = 2)                 #7
buty_opolskie_sr = round(mean(buty_opolskie), digits = 2)                       #8
buty_podkarpackie_sr = round(mean(buty_podkarpackie), digits = 2)               #9
buty_podlaskie_sr = round(mean(buty_podlaskie), digits = 2)                     #10
buty_pomorskie_sr = round(mean(buty_pomorskie), digits = 2)                     #11
buty_slaskie_sr = round(mean(buty_slaskie), digits = 2)                         #12
buty_swietokrzyskie_sr = round(mean(buty_swietokrzyskie), digits = 2)           #13
buty_warminsko_mazurskie_sr = round(mean(buty_warminsko_mazurskie), digits = 2) #14
buty_wielkopolskie_sr = round(mean(buty_wielkopolskie), digits = 2)             #15
buty_zachodniopomorskie_sr = round(mean(buty_zachodniopomorskie), digits = 2)   #16



#### Pó³buty œrednie, najw, najmn ceny w jednym wektorze ####
sr_cena_buty = c(buty_dolnoslaskie_sr,buty_kujawsko_pomorskie_sr,buty_lubelskie_sr,buty_lubuskie_sr,buty_lodzkie_sr,
                 buty_malopolskie_sr,buty_mazowieckie_sr,buty_opolskie_sr,buty_podkarpackie_sr,buty_podlaskie_sr,
                 buty_pomorskie_sr,buty_slaskie_sr,buty_swietokrzyskie_sr,buty_warminsko_mazurskie_sr,
                 buty_wielkopolskie_sr,buty_zachodniopomorskie_sr)

buty_najmn_w_kazdym_woj = c(min(buty_dolnoslaskie),min(buty_kujawsko_pomorskie),min(buty_lubelskie),
                            min(buty_lubuskie),min(buty_lodzkie),min(buty_malopolskie),
                            min(buty_mazowieckie),min(buty_opolskie),min(buty_podkarpackie),
                            min(buty_podlaskie),min(buty_pomorskie),min(buty_slaskie),
                            min(buty_swietokrzyskie),min(buty_warminsko_mazurskie),
                            min(buty_wielkopolskie),min(buty_zachodniopomorskie))

buty_najw_w_kazdym_woj = c(max(buty_dolnoslaskie),max(buty_kujawsko_pomorskie),max(buty_lubelskie),
                           max(buty_lubuskie),max(buty_lodzkie),max(buty_malopolskie),
                           max(buty_mazowieckie),max(buty_opolskie),max(buty_podkarpackie),
                           max(buty_podlaskie),max(buty_pomorskie),max(buty_slaskie),
                           max(buty_swietokrzyskie),max(buty_warminsko_mazurskie),
                           max(buty_wielkopolskie),max(buty_zachodniopomorskie))

###  Najwy¿sze i najni¿sze, ró¿ne

buty_sr_cena = mean(sr_cena_buty) #Œrednia cena pó³butów dla ca³ego kraju
buty_odsd_cena = sd(sr_cena_buty)  #Odchylenie standardowe ceny pó³butów

buty_najw_sr = max(sr_cena_buty) #Najwy¿sza œrednia cena pó³butów
buty_najm_sr = min(sr_cena_buty) #Najni¿sza œrednia cena pó³butów
buty_najmn_k = min(buty_najmn_w_kazdym_woj) #Najni¿sza cena pó³butów kiedykolwiek
buty_najw_k = max(buty_najw_w_kazdym_woj)   #Najwy¿sza cena pó³butów kiedykolwiek


but_najw_sr = as.numeric(match(buty_najw_sr,sr_cena_buty))  #Numer województwa
but_najmn_sr = as.numeric(match(buty_najm_sr,sr_cena_buty)) #Numer województwa
print(paste("Najwy¿sza œrednia cena butów jest w województwie", wojewodztwa[but_najw_sr],
            "i wynosi",buty_najw_sr, "z³"))
print(paste("Najni¿sza œrednia cena butów jest w województwie", wojewodztwa[but_najmn_sr],
            "i wynosi",buty_najm_sr, "z³"))


buty_najmn_kazde = as.numeric(match(buty_najmn_k, buty_najmn_w_kazdym_woj))  #Numer województwa
buty_najw_kazde = as.numeric(match(buty_najw_k,   buty_najw_w_kazdym_woj))   #Numer województwa
buty_najmn_kiedy =  match(buty_najmn_k,           buty_podlaskie)            #Kiedy
buty_najw_kiedy =  match(buty_najw_k,             buty_lubelskie)            #Kiedy

print(paste("Najni¿sza cena butów by³a w", daty[buty_najmn_kiedy], "w województwie", 
            wojewodztwa[buty_najmn_kazde],"i wynosi³a", buty_najmn_k, "z³"))

print(paste("Najwy¿sza cena butów by³a w", daty[buty_najw_kiedy], "w województwie",
            wojewodztwa[buty_najw_kazde],"i wynosi³a", buty_najw_k, "z³"))

print(paste("Odchylenie standardowe cen butów wynosi", round(buty_odsd_cena, digits = 2), "z³otego"))

#### Korelacja p³acy minimalnej do œredniej ceny pó³butów ####
buty_placa_min_kor = c(cor(as.numeric(as.character(placa_min_df$V1)), as.numeric(as.character(roczne_sr_buty$V1))),
                       cor(as.numeric(as.character(placa_min_df$V2)), as.numeric(as.character(roczne_sr_buty$V2))),
                       cor(as.numeric(as.character(placa_min_df$V3)), as.numeric(as.character(roczne_sr_buty$V3))),
                       cor(as.numeric(as.character(placa_min_df$V4)), as.numeric(as.character(roczne_sr_buty$V4))),
                       cor(as.numeric(as.character(placa_min_df$V5)), as.numeric(as.character(roczne_sr_buty$V5))),
                       cor(as.numeric(as.character(placa_min_df$V6)), as.numeric(as.character(roczne_sr_buty$V6))),
                       cor(as.numeric(as.character(placa_min_df$V7)), as.numeric(as.character(roczne_sr_buty$V7))),
                       cor(as.numeric(as.character(placa_min_df$V8)), as.numeric(as.character(roczne_sr_buty$V8))),
                       cor(as.numeric(as.character(placa_min_df$V9)), as.numeric(as.character(roczne_sr_buty$V9))),
                       cor(as.numeric(as.character(placa_min_df$V10)), as.numeric(as.character(roczne_sr_buty$V10))),
                       cor(as.numeric(as.character(placa_min_df$V11)), as.numeric(as.character(roczne_sr_buty$V11))),
                       cor(as.numeric(as.character(placa_min_df$V12)), as.numeric(as.character(roczne_sr_buty$V12))),
                       cor(as.numeric(as.character(placa_min_df$V13)), as.numeric(as.character(roczne_sr_buty$V13))))


print(paste("Uœredniona korelacja minimalnej pensji do ceny butów wynosi", round(mean(buty_placa_min_kor), 
                                                                                    digits = 4), "co wskazuje na siln¹ korelacjê"))
#### Korelacja œredniej pensji do œredniej ceny pó³butów ####

buty_sr_pensja_kor = c(cor(DOLNO, as.numeric(as.character(roczne_sr_buty$V1))),
                       cor(KUJAW, as.numeric(as.character(roczne_sr_buty$V2))),
                       cor(LUBEL, as.numeric(as.character(roczne_sr_buty$V3))),
                       cor(LUBUS, as.numeric(as.character(roczne_sr_buty$V4))),
                       cor(LODZK, as.numeric(as.character(roczne_sr_buty$V5))),
                       cor(MALOP, as.numeric(as.character(roczne_sr_buty$V6))),
                       cor(MAZOW, as.numeric(as.character(roczne_sr_buty$V7))),
                       cor(OPOLS, as.numeric(as.character(roczne_sr_buty$V8))),
                       cor(PODKA, as.numeric(as.character(roczne_sr_buty$V9))),
                       cor(PODLA, as.numeric(as.character(roczne_sr_buty$V10))),
                       cor(POMOR, as.numeric(as.character(roczne_sr_buty$V11))),
                       cor(SLASK, as.numeric(as.character(roczne_sr_buty$V13))),
                       cor(SWIET, as.numeric(as.character(roczne_sr_buty$V14))),
                       cor(WARMI, as.numeric(as.character(roczne_sr_buty$V15))),
                       cor(WIELK, as.numeric(as.character(roczne_sr_buty$V16))),
                       cor(ZACHO, as.numeric(as.character(roczne_sr_buty$V13))))
sr_buty_kor = mean(buty_sr_pensja_kor)
print(paste("Uœredniona korelacja œredniej pensji do ceny butów wynosi", round(sr_buty_kor, 
                                                                                  digits = 4), "co wskazuje na siln¹ korelacjê"))
print(".........................................................")




#### Zlewozmywak edycja nag³ówków i tabel ####
zlewozmywak_df = zlewozmywak_df %>% remove_empty("cols") # Usuwam puste kolumny
zlewozmywak_df = subset(zlewozmywak_df, select = -c(Kod,Nazwa))

#Edtujê nazwy kolumn na sensowne
names(zlewozmywak_df) <- gsub("styczeñ.bateria.zlewozmywakowa.cena.", "styczeñ.", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("luty.bateria.zlewozmywakowa.cena.", "luty.", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("marzec.bateria.zlewozmywakowa.cena.", "marzec.", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("kwiecieñ.bateria.zlewozmywakowa.cena.", "kwiecieñ.", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("maj.bateria.zlewozmywakowa.cena.", "maj.", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("czerwiec.bateria.zlewozmywakowa.cena.", "czerwiec.", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("lipiec.bateria.zlewozmywakowa.cena.", "lipiec.", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("sierpieñ.bateria.zlewozmywakowa.cena.", "sierpieñ.", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("wrzesieñ.bateria.zlewozmywakowa.cena.", "wrzesieñ.", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("paŸdziernik.bateria.zlewozmywakowa.cena.", "paŸdziernik.", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("listopad.bateria.zlewozmywakowa.cena.", "listopad.", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("grudzieñ.bateria.zlewozmywakowa.cena.", "grudzieñ.", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("..z³.", "", names(zlewozmywak_df))

#Nie znalaz³em inteligentnego sposobu na sensowne posortowanie chronologiczne wiêc robiê to ³opatologicznie
names(zlewozmywak_df) <- gsub("styczeñ.2006", "2006 1 styczeñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("styczeñ.2007", "2007 1 styczeñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("styczeñ.2008", "2008 1 styczeñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("styczeñ.2009", "2009 1 styczeñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("styczeñ.2010", "2010 1 styczeñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("styczeñ.2011", "2011 1 styczeñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("styczeñ.2012", "2012 1 styczeñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("styczeñ.2013", "2013 1 styczeñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("styczeñ.2014", "2014 1 styczeñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("styczeñ.2015", "2015 1 styczeñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("styczeñ.2016", "2016 1 styczeñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("styczeñ.2017", "2017 1 styczeñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("styczeñ.2018", "2018 1 styczeñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("styczeñ.2019", "2019 1 styczeñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("luty.2006", "2006 2 luty", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("luty.2007", "2007 2 luty", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("luty.2008", "2008 2 luty", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("luty.2009", "2009 2 luty", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("luty.2010", "2010 2 luty", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("luty.2011", "2011 2 luty", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("luty.2012", "2012 2 luty", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("luty.2013", "2013 2 luty", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("luty.2014", "2014 2 luty", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("luty.2015", "2015 2 luty", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("luty.2016", "2016 2 luty", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("luty.2017", "2017 2 luty", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("luty.2018", "2018 2 luty", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("luty.2019", "2019 2 luty", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("marzec.2006", "2006 3 marzec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("marzec.2007", "2007 3 marzec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("marzec.2008", "2008 3 marzec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("marzec.2009", "2009 3 marzec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("marzec.2010", "2010 3 marzec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("marzec.2011", "2011 3 marzec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("marzec.2012", "2012 3 marzec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("marzec.2013", "2013 3 marzec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("marzec.2014", "2014 3 marzec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("marzec.2015", "2015 3 marzec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("marzec.2016", "2016 3 marzec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("marzec.2017", "2017 3 marzec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("marzec.2018", "2018 3 marzec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("marzec.2019", "2019 3 marzec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("kwiecieñ.2006", "2006 4 kwiecieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("kwiecieñ.2007", "2007 4 kwiecieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("kwiecieñ.2008", "2008 4 kwiecieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("kwiecieñ.2009", "2009 4 kwiecieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("kwiecieñ.2010", "2010 4 kwiecieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("kwiecieñ.2011", "2011 4 kwiecieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("kwiecieñ.2012", "2012 4 kwiecieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("kwiecieñ.2013", "2013 4 kwiecieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("kwiecieñ.2014", "2014 4 kwiecieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("kwiecieñ.2015", "2015 4 kwiecieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("kwiecieñ.2016", "2016 4 kwiecieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("kwiecieñ.2017", "2017 4 kwiecieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("kwiecieñ.2018", "2018 4 kwiecieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("kwiecieñ.2019", "2019 4 kwiecieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("maj.2006", "2006 5 maj", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("maj.2007", "2007 5 maj", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("maj.2008", "2008 5 maj", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("maj.2009", "2009 5 maj", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("maj.2010", "2010 5 maj", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("maj.2011", "2011 5 maj", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("maj.2012", "2012 5 maj", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("maj.2013", "2013 5 maj", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("maj.2014", "2014 5 maj", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("maj.2015", "2015 5 maj", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("maj.2016", "2016 5 maj", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("maj.2017", "2017 5 maj", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("maj.2018", "2018 5 maj", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("maj.2019", "2019 5 maj", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("czerwiec.2006", "2006 6 czerwiec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("czerwiec.2007", "2007 6 czerwiec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("czerwiec.2008", "2008 6 czerwiec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("czerwiec.2009", "2009 6 czerwiec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("czerwiec.2010", "2010 6 czerwiec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("czerwiec.2011", "2011 6 czerwiec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("czerwiec.2012", "2012 6 czerwiec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("czerwiec.2013", "2013 6 czerwiec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("czerwiec.2014", "2014 6 czerwiec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("czerwiec.2015", "2015 6 czerwiec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("czerwiec.2016", "2016 6 czerwiec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("czerwiec.2017", "2017 6 czerwiec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("czerwiec.2018", "2018 6 czerwiec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("czerwiec.2019", "2019 6 czerwiec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("lipiec.2006", "2006 7 lipiec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("lipiec.2007", "2007 7 lipiec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("lipiec.2008", "2008 7 lipiec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("lipiec.2009", "2009 7 lipiec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("lipiec.2010", "2010 7 lipiec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("lipiec.2011", "2011 7 lipiec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("lipiec.2012", "2012 7 lipiec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("lipiec.2013", "2013 7 lipiec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("lipiec.2014", "2014 7 lipiec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("lipiec.2015", "2015 7 lipiec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("lipiec.2016", "2016 7 lipiec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("lipiec.2017", "2017 7 lipiec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("lipiec.2018", "2018 7 lipiec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("lipiec.2019", "2019 7 lipiec", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("sierpieñ.2006", "2006 8 sierpieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("sierpieñ.2007", "2007 8 sierpieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("sierpieñ.2008", "2008 8 sierpieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("sierpieñ.2009", "2009 8 sierpieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("sierpieñ.2010", "2010 8 sierpieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("sierpieñ.2011", "2011 8 sierpieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("sierpieñ.2012", "2012 8 sierpieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("sierpieñ.2013", "2013 8 sierpieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("sierpieñ.2014", "2014 8 sierpieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("sierpieñ.2015", "2015 8 sierpieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("sierpieñ.2016", "2016 8 sierpieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("sierpieñ.2017", "2017 8 sierpieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("sierpieñ.2018", "2018 8 sierpieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("sierpieñ.2019", "2019 8 sierpieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("wrzesieñ.2006", "2006 9 wrzesieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("wrzesieñ.2007", "2007 9 wrzesieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("wrzesieñ.2008", "2008 9 wrzesieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("wrzesieñ.2009", "2009 9 wrzesieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("wrzesieñ.2010", "2010 9 wrzesieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("wrzesieñ.2011", "2011 9 wrzesieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("wrzesieñ.2012", "2012 9 wrzesieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("wrzesieñ.2013", "2013 9 wrzesieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("wrzesieñ.2014", "2014 9 wrzesieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("wrzesieñ.2015", "2015 9 wrzesieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("wrzesieñ.2016", "2016 9 wrzesieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("wrzesieñ.2017", "2017 9 wrzesieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("wrzesieñ.2018", "2018 9 wrzesieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("wrzesieñ.2019", "2019 9 wrzesieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("paŸdziernik.2006", "2006 90 paŸdziernik", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("paŸdziernik.2007", "2007 90 paŸdziernik", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("paŸdziernik.2008", "2008 90 paŸdziernik", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("paŸdziernik.2009", "2009 90 paŸdziernik", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("paŸdziernik.2010", "2010 90 paŸdziernik", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("paŸdziernik.2011", "2011 90 paŸdziernik", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("paŸdziernik.2012", "2012 90 paŸdziernik", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("paŸdziernik.2013", "2013 90 paŸdziernik", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("paŸdziernik.2014", "2014 90 paŸdziernik", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("paŸdziernik.2015", "2015 90 paŸdziernik", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("paŸdziernik.2016", "2016 90 paŸdziernik", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("paŸdziernik.2017", "2017 90 paŸdziernik", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("paŸdziernik.2018", "2018 90 paŸdziernik", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("paŸdziernik.2019", "2019 90 paŸdziernik", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("listopad.2006", "2006 91 listopad", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("listopad.2007", "2007 91 listopad", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("listopad.2008", "2008 91 listopad", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("listopad.2009", "2009 91 listopad", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("listopad.2010", "2010 91 listopad", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("listopad.2011", "2011 91 listopad", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("listopad.2012", "2012 91 listopad", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("listopad.2013", "2013 91 listopad", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("listopad.2014", "2014 91 listopad", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("listopad.2015", "2015 91 listopad", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("listopad.2016", "2016 91 listopad", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("listopad.2017", "2017 91 listopad", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("listopad.2018", "2018 91 listopad", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("listopad.2019", "2019 91 listopad", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("grudzieñ.2006", "2006 92 grudzieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("grudzieñ.2007", "2007 92 grudzieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("grudzieñ.2008", "2008 92 grudzieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("grudzieñ.2009", "2009 92 grudzieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("grudzieñ.2010", "2010 92 grudzieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("grudzieñ.2011", "2011 92 grudzieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("grudzieñ.2012", "2012 92 grudzieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("grudzieñ.2013", "2013 92 grudzieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("grudzieñ.2014", "2014 92 grudzieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("grudzieñ.2015", "2015 92 grudzieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("grudzieñ.2016", "2016 92 grudzieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("grudzieñ.2017", "2017 92 grudzieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("grudzieñ.2018", "2018 92 grudzieñ", names(zlewozmywak_df))
names(zlewozmywak_df) <- gsub("grudzieñ.2019", "2019 92 grudzieñ", names(zlewozmywak_df))
zlewozmywak_df = zlewozmywak_df[sort(colnames(zlewozmywak_df))] #Sortujê chronologicznie
#zlewozmywak_df = subset(zlewozmywak_df, select = -c(Nazwa) ) # Usuwam zbêdn¹ kolumnê z nazwazmi województw


zlewozmywak_df = t(zlewozmywak_df) #Tramsponujê dataframe ¿eby poszczególne kolumny by³y województwami 
zlewozmywak_df = unname(zlewozmywak_df) #Usuwam zbêdne teraz nazwy
zlewozmywak_df = as.data.frame(zlewozmywak_df) #Przekszta³cam na dataframe


# Normalnie nie da³o siê przkonwertowaæ danych na numeric, bo zamiast kropek by³y 
# przecinki i niemi³osiernie d³ugo siê nad tym g³owi³em. Ca³y czas otrzymywa³em 
# wartoœci NA
zlewozmywak_dolnoslaskie = as.numeric(gsub(",", ".", as.character(zlewozmywak_df$V1)))
zlewozmywak_kujawsko_pomorskie = as.numeric(gsub(",", ".", as.character(zlewozmywak_df$V2)))
zlewozmywak_lubelskie = as.numeric(gsub(",", ".", as.character(zlewozmywak_df$V3)))
zlewozmywak_lubuskie = as.numeric(gsub(",", ".", as.character(zlewozmywak_df$V4)))
zlewozmywak_lodzkie = as.numeric(gsub(",", ".", as.character(zlewozmywak_df$V5)))
zlewozmywak_malopolskie = as.numeric(gsub(",", ".", as.character(zlewozmywak_df$V6)))
zlewozmywak_mazowieckie = as.numeric(gsub(",", ".", as.character(zlewozmywak_df$V7)))
zlewozmywak_opolskie = as.numeric(gsub(",", ".", as.character(zlewozmywak_df$V8)))
zlewozmywak_podkarpackie = as.numeric(gsub(",", ".", as.character(zlewozmywak_df$V9)))
zlewozmywak_podlaskie = as.numeric(gsub(",", ".", as.character(zlewozmywak_df$V10)))
zlewozmywak_pomorskie = as.numeric(gsub(",", ".", as.character(zlewozmywak_df$V11)))
zlewozmywak_slaskie = as.numeric(gsub(",", ".", as.character(zlewozmywak_df$V12)))
zlewozmywak_swietokrzyskie = as.numeric(gsub(",", ".", as.character(zlewozmywak_df$V13)))
zlewozmywak_warminsko_mazurskie = as.numeric(gsub(",", ".", as.character(zlewozmywak_df$V14)))
zlewozmywak_wielkopolskie = as.numeric(gsub(",", ".", as.character(zlewozmywak_df$V15)))
zlewozmywak_zachodniopomorskie = as.numeric(gsub(",", ".", as.character(zlewozmywak_df$V16)))

#### Wykres cen zlewozmywaka ####
y = (1:168)
#Wykres dla zlewozmywaka i wszystkich województw
zlewozmywak_plot = plot_ly(x = y )%>%
  layout(title = 'Zmiana cen baterii zlewozmywakowej w latach 2006-2019', xaxis = list(title = 'Rok', 
                                                                                       ticktext = list("2006", "2007", "2008", "2009", "2010", "2011","2012","2013","2014","2015","2016","2017","2018","2019"), 
                                                                                       tickvals = list(1, 12, 24, 36, 48, 60, 72, 84, 96, 108, 120, 132, 144, 156)),
         yaxis = list(title = 'cena (z³)')) %>%
  add_lines(y = zlewozmywak_dolnoslaskie, name = "dolnoœlaskie")%>%
  add_lines(y = zlewozmywak_kujawsko_pomorskie, name = "kujawsko-pomorskie")%>%
  add_lines(y = zlewozmywak_lubelskie, name = "lubelskie")%>%
  add_lines(y = zlewozmywak_lubuskie, name = "lubuskie")%>%
  add_lines(y = zlewozmywak_lodzkie, name = "³ódzkie")%>%
  add_lines(y = zlewozmywak_malopolskie, name = "ma³opolskie")%>%
  add_lines(y = zlewozmywak_mazowieckie, name = "mazowieckie")%>%
  add_lines(y = zlewozmywak_opolskie, name = "opolskie")%>%
  add_lines(y = zlewozmywak_podkarpackie, name = "podkarpackie")%>%
  add_lines(y = zlewozmywak_podlaskie, name = "podlaskie")%>%
  add_lines(y = zlewozmywak_pomorskie, name = "pomorskie")%>%
  add_lines(y = zlewozmywak_slaskie, name = "œl¹skie")%>%
  add_lines(y = zlewozmywak_swietokrzyskie, name = "œwietokrzyskie")%>%
  add_lines(y = zlewozmywak_warminsko_mazurskie, name = "warmiñsko_mazurskie")%>%
  add_lines(y = zlewozmywak_wielkopolskie, name = "wielkopolskie")%>%
  add_lines(y = zlewozmywak_zachodniopomorskie, name = "zachodniopomorskie")
#print(zlewozmywak_plot)

#### Œrednie roczne ceny zlewozmywaka ####
roczne_sr_zlewozmywak = data.frame('','','','','','','','','','','','','', stringsAsFactors = FALSE)
names(roczne_sr_zlewozmywak) = c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016",
                                 "2017","2018")
for (i in 1:13) {
  roczne_sr_zlewozmywak[1,i] = round(mean(zlewozmywak_dolnoslaskie[(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_zlewozmywak[2,i] = round(mean(zlewozmywak_kujawsko_pomorskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_zlewozmywak[3,i] = round(mean(zlewozmywak_lubelskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_zlewozmywak[4,i] = round(mean(zlewozmywak_lubuskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_zlewozmywak[5,i] = round(mean(zlewozmywak_lodzkie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_zlewozmywak[6,i] = round(mean(zlewozmywak_malopolskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_zlewozmywak[7,i] = round(mean(zlewozmywak_mazowieckie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_zlewozmywak[8,i] = round(mean(zlewozmywak_opolskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_zlewozmywak[9,i] = round(mean(zlewozmywak_podkarpackie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_zlewozmywak[10,i] = round(mean(zlewozmywak_podlaskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_zlewozmywak[11,i] = round(mean(zlewozmywak_pomorskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_zlewozmywak[12,i] = round(mean(zlewozmywak_slaskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_zlewozmywak[13,i] = round(mean(zlewozmywak_swietokrzyskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_zlewozmywak[14,i] = round(mean(zlewozmywak_warminsko_mazurskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_zlewozmywak[15,i] = round(mean(zlewozmywak_wielkopolskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_zlewozmywak[16,i] = round(mean(zlewozmywak_zachodniopomorskie[(1+(i-1)*12):(12*i)]), digits = 2)
}
roczne_sr_zlewozmywak = t(roczne_sr_zlewozmywak)
roczne_sr_zlewozmywak = unname(roczne_sr_zlewozmywak) #Usuwam zbêdne teraz nazwy
roczne_sr_zlewozmywak = as.data.frame(roczne_sr_zlewozmywak) #Przekszta³cam na dataframe

#### Œrednia cena zlewozmywaka dla ka¿dego wojewódstwa ####
zlewozmywak_dolnoslaskie_sr = round(mean(zlewozmywak_dolnoslaskie), digits = 2)               #1
zlewozmywak_kujawsko_pomorskie_sr = round(mean(zlewozmywak_kujawsko_pomorskie), digits = 2)   #2
zlewozmywak_lubelskie_sr = round(mean(zlewozmywak_lubelskie), digits = 2)                     #3
zlewozmywak_lubuskie_sr = round(mean(zlewozmywak_lubuskie), digits = 2)                       #4
zlewozmywak_lodzkie_sr = round(mean(zlewozmywak_lodzkie), digits = 2)                         #5
zlewozmywak_malopolskie_sr = round(mean(zlewozmywak_malopolskie), digits = 2)                 #6
zlewozmywak_mazowieckie_sr = round(mean(zlewozmywak_mazowieckie), digits = 2)                 #7
zlewozmywak_opolskie_sr = round(mean(zlewozmywak_opolskie), digits = 2)                       #8
zlewozmywak_podkarpackie_sr = round(mean(zlewozmywak_podkarpackie), digits = 2)               #9
zlewozmywak_podlaskie_sr = round(mean(zlewozmywak_podlaskie), digits = 2)                     #10
zlewozmywak_pomorskie_sr = round(mean(zlewozmywak_pomorskie), digits = 2)                     #11
zlewozmywak_slaskie_sr = round(mean(zlewozmywak_slaskie), digits = 2)                         #12
zlewozmywak_swietokrzyskie_sr = round(mean(zlewozmywak_swietokrzyskie), digits = 2)           #13
zlewozmywak_warminsko_mazurskie_sr = round(mean(zlewozmywak_warminsko_mazurskie), digits = 2) #14
zlewozmywak_wielkopolskie_sr = round(mean(zlewozmywak_wielkopolskie), digits = 2)             #15
zlewozmywak_zachodniopomorskie_sr = round(mean(zlewozmywak_zachodniopomorskie), digits = 2)   #16



#### Zlewozmywak œrednie, najw, najmn ceny w jednym wektorze ####
sr_cena_zlewozmywak = c(zlewozmywak_dolnoslaskie_sr,zlewozmywak_kujawsko_pomorskie_sr,zlewozmywak_lubelskie_sr,zlewozmywak_lubuskie_sr,zlewozmywak_lodzkie_sr,
                        zlewozmywak_malopolskie_sr,zlewozmywak_mazowieckie_sr,zlewozmywak_opolskie_sr,zlewozmywak_podkarpackie_sr,zlewozmywak_podlaskie_sr,
                        zlewozmywak_pomorskie_sr,zlewozmywak_slaskie_sr,zlewozmywak_swietokrzyskie_sr,zlewozmywak_warminsko_mazurskie_sr,
                        zlewozmywak_wielkopolskie_sr,zlewozmywak_zachodniopomorskie_sr)

zlewozmywak_najmn_w_kazdym_woj = c(min(zlewozmywak_dolnoslaskie),min(zlewozmywak_kujawsko_pomorskie),min(zlewozmywak_lubelskie),
                                   min(zlewozmywak_lubuskie),min(zlewozmywak_lodzkie),min(zlewozmywak_malopolskie),
                                   min(zlewozmywak_mazowieckie),min(zlewozmywak_opolskie),min(zlewozmywak_podkarpackie),
                                   min(zlewozmywak_podlaskie),min(zlewozmywak_pomorskie),min(zlewozmywak_slaskie),
                                   min(zlewozmywak_swietokrzyskie),min(zlewozmywak_warminsko_mazurskie),
                                   min(zlewozmywak_wielkopolskie),min(zlewozmywak_zachodniopomorskie))

zlewozmywak_najw_w_kazdym_woj = c(max(zlewozmywak_dolnoslaskie),max(zlewozmywak_kujawsko_pomorskie),max(zlewozmywak_lubelskie),
                                  max(zlewozmywak_lubuskie),max(zlewozmywak_lodzkie),max(zlewozmywak_malopolskie),
                                  max(zlewozmywak_mazowieckie),max(zlewozmywak_opolskie),max(zlewozmywak_podkarpackie),
                                  max(zlewozmywak_podlaskie),max(zlewozmywak_pomorskie),max(zlewozmywak_slaskie),
                                  max(zlewozmywak_swietokrzyskie),max(zlewozmywak_warminsko_mazurskie),
                                  max(zlewozmywak_wielkopolskie),max(zlewozmywak_zachodniopomorskie))

###  Najwy¿sze i najni¿sze, ró¿ne

zlewozmywak_sr_cena = mean(sr_cena_zlewozmywak) #Œrednia cena zlewozmywaka dla ca³ego kraju
zlewozmywak_odsd_cena = sd(sr_cena_zlewozmywak)  #Odchylenie standardowe ceny zlewozmywaka

zlewozmywak_najw_sr = max(sr_cena_zlewozmywak) #Najwy¿sza œrednia cena zlewozmywaka
zlewozmywak_najm_sr = min(sr_cena_zlewozmywak) #Najni¿sza œrednia cena zlewozmywaka
zlewozmywak_najmn_k = min(zlewozmywak_najmn_w_kazdym_woj) #Najni¿sza cena zlewozmywaka kiedykolwiek
zlewozmywak_najw_k = max(zlewozmywak_najw_w_kazdym_woj)   #Najwy¿sza cena zlewozmywaka kiedykolwiek


zlew_najw_sr = as.numeric(match(zlewozmywak_najw_sr,sr_cena_zlewozmywak))  #Numer województwa
zlew_najmn_sr = as.numeric(match(zlewozmywak_najm_sr,sr_cena_zlewozmywak)) #Numer województwa
print(paste("Najwy¿sza œrednia cena zlewozmywaka jest w województwie", wojewodztwa[zlew_najw_sr],
            "i wynosi",zlewozmywak_najw_sr))
print(paste("Najni¿sza œrednia cena zlewozmywaka jest w województwie", wojewodztwa[zlew_najmn_sr],
            "i wynosi",zlewozmywak_najm_sr))


zlewozmywak_najmn_kazde = as.numeric(match(zlewozmywak_najmn_k, zlewozmywak_najmn_w_kazdym_woj))  #Numer województwa
zlewozmywak_najw_kazde = as.numeric(match(zlewozmywak_najw_k,   zlewozmywak_najw_w_kazdym_woj))   #Numer województwa
zlewozmywak_najmn_kiedy =  match(zlewozmywak_najmn_k,           zlewozmywak_swietokrzyskie)   #Kiedy
zlewozmywak_najw_kiedy =  match(zlewozmywak_najw_k,             zlewozmywak_opolskie)       #Kiedy

print(paste("Najni¿sza cena zlewozmywaka by³a w", daty[zlewozmywak_najmn_kiedy], "w województwie", 
            wojewodztwa[zlewozmywak_najmn_kazde],"i wynosi³a", zlewozmywak_najmn_k, "z³"))

print(paste("Najwy¿sza cena zlewozmywaka by³a w", daty[zlewozmywak_najw_kiedy], "w województwie",
            wojewodztwa[zlewozmywak_najw_kazde],"i wynosi³a", zlewozmywak_najw_k, "z³"))

print(paste("Odchylenie standardowe cen zlewozmywaka wynosi", round(zlewozmywak_odsd_cena, digits = 2), "z³otego"))

#### Korelacja p³acy minimalnej do œredniej ceny zlewozmywaka ####
zlewozmywak_placa_min_kor = c(cor(as.numeric(as.character(placa_min_df$V1)), as.numeric(as.character(roczne_sr_zlewozmywak$V1))),
                              cor(as.numeric(as.character(placa_min_df$V2)), as.numeric(as.character(roczne_sr_zlewozmywak$V2))),
                              cor(as.numeric(as.character(placa_min_df$V3)), as.numeric(as.character(roczne_sr_zlewozmywak$V3))),
                              cor(as.numeric(as.character(placa_min_df$V4)), as.numeric(as.character(roczne_sr_zlewozmywak$V4))),
                              cor(as.numeric(as.character(placa_min_df$V5)), as.numeric(as.character(roczne_sr_zlewozmywak$V5))),
                              cor(as.numeric(as.character(placa_min_df$V6)), as.numeric(as.character(roczne_sr_zlewozmywak$V6))),
                              cor(as.numeric(as.character(placa_min_df$V7)), as.numeric(as.character(roczne_sr_zlewozmywak$V7))),
                              cor(as.numeric(as.character(placa_min_df$V8)), as.numeric(as.character(roczne_sr_zlewozmywak$V8))),
                              cor(as.numeric(as.character(placa_min_df$V9)), as.numeric(as.character(roczne_sr_zlewozmywak$V9))),
                              cor(as.numeric(as.character(placa_min_df$V10)), as.numeric(as.character(roczne_sr_zlewozmywak$V10))),
                              cor(as.numeric(as.character(placa_min_df$V11)), as.numeric(as.character(roczne_sr_zlewozmywak$V11))),
                              cor(as.numeric(as.character(placa_min_df$V12)), as.numeric(as.character(roczne_sr_zlewozmywak$V12))),
                              cor(as.numeric(as.character(placa_min_df$V13)), as.numeric(as.character(roczne_sr_zlewozmywak$V13))))


print(paste("Uœredniona korelacja minimalnej pensji do ceny zlewozmywaka wynosi", round(mean(zlewozmywak_placa_min_kor), 
                                                                                        digits = 4), "co wskazuje na siln¹ korelacjê"))
#### Korelacja œredniej pensji do œredniej ceny zlewozmywaka ####

zlewozmywak_sr_pensja_kor = c(cor(DOLNO, as.numeric(as.character(roczne_sr_zlewozmywak$V1))),
                              cor(KUJAW, as.numeric(as.character(roczne_sr_zlewozmywak$V2))),
                              cor(LUBEL, as.numeric(as.character(roczne_sr_zlewozmywak$V3))),
                              cor(LUBUS, as.numeric(as.character(roczne_sr_zlewozmywak$V4))),
                              cor(LODZK, as.numeric(as.character(roczne_sr_zlewozmywak$V5))),
                              cor(MALOP, as.numeric(as.character(roczne_sr_zlewozmywak$V6))),
                              cor(MAZOW, as.numeric(as.character(roczne_sr_zlewozmywak$V7))),
                              cor(OPOLS, as.numeric(as.character(roczne_sr_zlewozmywak$V8))),
                              cor(PODKA, as.numeric(as.character(roczne_sr_zlewozmywak$V9))),
                              cor(PODLA, as.numeric(as.character(roczne_sr_zlewozmywak$V10))),
                              cor(POMOR, as.numeric(as.character(roczne_sr_zlewozmywak$V11))),
                              cor(SLASK, as.numeric(as.character(roczne_sr_zlewozmywak$V12))),
                              cor(SWIET, as.numeric(as.character(roczne_sr_zlewozmywak$V13))),
                              cor(WARMI, as.numeric(as.character(roczne_sr_zlewozmywak$V14))),
                              cor(WIELK, as.numeric(as.character(roczne_sr_zlewozmywak$V15))),
                              cor(ZACHO, as.numeric(as.character(roczne_sr_zlewozmywak$V16))))
sr_zlewozmywak_kor = mean(zlewozmywak_sr_pensja_kor)
print(paste("Uœredniona korelacja œredniej pensji do ceny zlewozmywaka wynosi", round(sr_zlewozmywak_kor, 
                                                                                      digits = 4), "co wskazuje na siln¹ korelacjê"))
print(".........................................................")






#### Lekarz edycja nag³ówków i tabel ####
lekarz_df = lekarz_df %>% remove_empty("cols") # Usuwam puste kolumny
lekarz_df = subset(lekarz_df, select = -c(Kod,Nazwa))

#Edtujê nazwy kolumn na sensowne
names(lekarz_df) <- gsub("styczeñ.wizyta.u.lekarza.specjalisty.cena.", "styczeñ.", names(lekarz_df))
names(lekarz_df) <- gsub("luty.wizyta.u.lekarza.specjalisty.cena.", "luty.", names(lekarz_df))
names(lekarz_df) <- gsub("marzec.wizyta.u.lekarza.specjalisty.cena.", "marzec.", names(lekarz_df))
names(lekarz_df) <- gsub("kwiecieñ.wizyta.u.lekarza.specjalisty.cena.", "kwiecieñ.", names(lekarz_df))
names(lekarz_df) <- gsub("maj.wizyta.u.lekarza.specjalisty.cena.", "maj.", names(lekarz_df))
names(lekarz_df) <- gsub("czerwiec.wizyta.u.lekarza.specjalisty.cena.", "czerwiec.", names(lekarz_df))
names(lekarz_df) <- gsub("lipiec.wizyta.u.lekarza.specjalisty.cena.", "lipiec.", names(lekarz_df))
names(lekarz_df) <- gsub("sierpieñ.wizyta.u.lekarza.specjalisty.cena.", "sierpieñ.", names(lekarz_df))
names(lekarz_df) <- gsub("wrzesieñ.wizyta.u.lekarza.specjalisty.cena.", "wrzesieñ.", names(lekarz_df))
names(lekarz_df) <- gsub("paŸdziernik.wizyta.u.lekarza.specjalisty.cena.", "paŸdziernik.", names(lekarz_df))
names(lekarz_df) <- gsub("listopad.wizyta.u.lekarza.specjalisty.cena.", "listopad.", names(lekarz_df))
names(lekarz_df) <- gsub("grudzieñ.wizyta.u.lekarza.specjalisty.cena.", "grudzieñ.", names(lekarz_df))
names(lekarz_df) <- gsub("..z³.", "", names(lekarz_df))

#Nie znalaz³em inteligentnego sposobu na sensowne posortowanie chronologiczne wiêc robiê to ³opatologicznie
names(lekarz_df) <- gsub("styczeñ.2006", "2006 1 styczeñ", names(lekarz_df))
names(lekarz_df) <- gsub("styczeñ.2007", "2007 1 styczeñ", names(lekarz_df))
names(lekarz_df) <- gsub("styczeñ.2008", "2008 1 styczeñ", names(lekarz_df))
names(lekarz_df) <- gsub("styczeñ.2009", "2009 1 styczeñ", names(lekarz_df))
names(lekarz_df) <- gsub("styczeñ.2010", "2010 1 styczeñ", names(lekarz_df))
names(lekarz_df) <- gsub("styczeñ.2011", "2011 1 styczeñ", names(lekarz_df))
names(lekarz_df) <- gsub("styczeñ.2012", "2012 1 styczeñ", names(lekarz_df))
names(lekarz_df) <- gsub("styczeñ.2013", "2013 1 styczeñ", names(lekarz_df))
names(lekarz_df) <- gsub("styczeñ.2014", "2014 1 styczeñ", names(lekarz_df))
names(lekarz_df) <- gsub("styczeñ.2015", "2015 1 styczeñ", names(lekarz_df))
names(lekarz_df) <- gsub("styczeñ.2016", "2016 1 styczeñ", names(lekarz_df))
names(lekarz_df) <- gsub("styczeñ.2017", "2017 1 styczeñ", names(lekarz_df))
names(lekarz_df) <- gsub("styczeñ.2018", "2018 1 styczeñ", names(lekarz_df))
names(lekarz_df) <- gsub("styczeñ.2019", "2019 1 styczeñ", names(lekarz_df))
names(lekarz_df) <- gsub("luty.2006", "2006 2 luty", names(lekarz_df))
names(lekarz_df) <- gsub("luty.2007", "2007 2 luty", names(lekarz_df))
names(lekarz_df) <- gsub("luty.2008", "2008 2 luty", names(lekarz_df))
names(lekarz_df) <- gsub("luty.2009", "2009 2 luty", names(lekarz_df))
names(lekarz_df) <- gsub("luty.2010", "2010 2 luty", names(lekarz_df))
names(lekarz_df) <- gsub("luty.2011", "2011 2 luty", names(lekarz_df))
names(lekarz_df) <- gsub("luty.2012", "2012 2 luty", names(lekarz_df))
names(lekarz_df) <- gsub("luty.2013", "2013 2 luty", names(lekarz_df))
names(lekarz_df) <- gsub("luty.2014", "2014 2 luty", names(lekarz_df))
names(lekarz_df) <- gsub("luty.2015", "2015 2 luty", names(lekarz_df))
names(lekarz_df) <- gsub("luty.2016", "2016 2 luty", names(lekarz_df))
names(lekarz_df) <- gsub("luty.2017", "2017 2 luty", names(lekarz_df))
names(lekarz_df) <- gsub("luty.2018", "2018 2 luty", names(lekarz_df))
names(lekarz_df) <- gsub("luty.2019", "2019 2 luty", names(lekarz_df))
names(lekarz_df) <- gsub("marzec.2006", "2006 3 marzec", names(lekarz_df))
names(lekarz_df) <- gsub("marzec.2007", "2007 3 marzec", names(lekarz_df))
names(lekarz_df) <- gsub("marzec.2008", "2008 3 marzec", names(lekarz_df))
names(lekarz_df) <- gsub("marzec.2009", "2009 3 marzec", names(lekarz_df))
names(lekarz_df) <- gsub("marzec.2010", "2010 3 marzec", names(lekarz_df))
names(lekarz_df) <- gsub("marzec.2011", "2011 3 marzec", names(lekarz_df))
names(lekarz_df) <- gsub("marzec.2012", "2012 3 marzec", names(lekarz_df))
names(lekarz_df) <- gsub("marzec.2013", "2013 3 marzec", names(lekarz_df))
names(lekarz_df) <- gsub("marzec.2014", "2014 3 marzec", names(lekarz_df))
names(lekarz_df) <- gsub("marzec.2015", "2015 3 marzec", names(lekarz_df))
names(lekarz_df) <- gsub("marzec.2016", "2016 3 marzec", names(lekarz_df))
names(lekarz_df) <- gsub("marzec.2017", "2017 3 marzec", names(lekarz_df))
names(lekarz_df) <- gsub("marzec.2018", "2018 3 marzec", names(lekarz_df))
names(lekarz_df) <- gsub("marzec.2019", "2019 3 marzec", names(lekarz_df))
names(lekarz_df) <- gsub("kwiecieñ.2006", "2006 4 kwiecieñ", names(lekarz_df))
names(lekarz_df) <- gsub("kwiecieñ.2007", "2007 4 kwiecieñ", names(lekarz_df))
names(lekarz_df) <- gsub("kwiecieñ.2008", "2008 4 kwiecieñ", names(lekarz_df))
names(lekarz_df) <- gsub("kwiecieñ.2009", "2009 4 kwiecieñ", names(lekarz_df))
names(lekarz_df) <- gsub("kwiecieñ.2010", "2010 4 kwiecieñ", names(lekarz_df))
names(lekarz_df) <- gsub("kwiecieñ.2011", "2011 4 kwiecieñ", names(lekarz_df))
names(lekarz_df) <- gsub("kwiecieñ.2012", "2012 4 kwiecieñ", names(lekarz_df))
names(lekarz_df) <- gsub("kwiecieñ.2013", "2013 4 kwiecieñ", names(lekarz_df))
names(lekarz_df) <- gsub("kwiecieñ.2014", "2014 4 kwiecieñ", names(lekarz_df))
names(lekarz_df) <- gsub("kwiecieñ.2015", "2015 4 kwiecieñ", names(lekarz_df))
names(lekarz_df) <- gsub("kwiecieñ.2016", "2016 4 kwiecieñ", names(lekarz_df))
names(lekarz_df) <- gsub("kwiecieñ.2017", "2017 4 kwiecieñ", names(lekarz_df))
names(lekarz_df) <- gsub("kwiecieñ.2018", "2018 4 kwiecieñ", names(lekarz_df))
names(lekarz_df) <- gsub("kwiecieñ.2019", "2019 4 kwiecieñ", names(lekarz_df))
names(lekarz_df) <- gsub("maj.2006", "2006 5 maj", names(lekarz_df))
names(lekarz_df) <- gsub("maj.2007", "2007 5 maj", names(lekarz_df))
names(lekarz_df) <- gsub("maj.2008", "2008 5 maj", names(lekarz_df))
names(lekarz_df) <- gsub("maj.2009", "2009 5 maj", names(lekarz_df))
names(lekarz_df) <- gsub("maj.2010", "2010 5 maj", names(lekarz_df))
names(lekarz_df) <- gsub("maj.2011", "2011 5 maj", names(lekarz_df))
names(lekarz_df) <- gsub("maj.2012", "2012 5 maj", names(lekarz_df))
names(lekarz_df) <- gsub("maj.2013", "2013 5 maj", names(lekarz_df))
names(lekarz_df) <- gsub("maj.2014", "2014 5 maj", names(lekarz_df))
names(lekarz_df) <- gsub("maj.2015", "2015 5 maj", names(lekarz_df))
names(lekarz_df) <- gsub("maj.2016", "2016 5 maj", names(lekarz_df))
names(lekarz_df) <- gsub("maj.2017", "2017 5 maj", names(lekarz_df))
names(lekarz_df) <- gsub("maj.2018", "2018 5 maj", names(lekarz_df))
names(lekarz_df) <- gsub("maj.2019", "2019 5 maj", names(lekarz_df))
names(lekarz_df) <- gsub("czerwiec.2006", "2006 6 czerwiec", names(lekarz_df))
names(lekarz_df) <- gsub("czerwiec.2007", "2007 6 czerwiec", names(lekarz_df))
names(lekarz_df) <- gsub("czerwiec.2008", "2008 6 czerwiec", names(lekarz_df))
names(lekarz_df) <- gsub("czerwiec.2009", "2009 6 czerwiec", names(lekarz_df))
names(lekarz_df) <- gsub("czerwiec.2010", "2010 6 czerwiec", names(lekarz_df))
names(lekarz_df) <- gsub("czerwiec.2011", "2011 6 czerwiec", names(lekarz_df))
names(lekarz_df) <- gsub("czerwiec.2012", "2012 6 czerwiec", names(lekarz_df))
names(lekarz_df) <- gsub("czerwiec.2013", "2013 6 czerwiec", names(lekarz_df))
names(lekarz_df) <- gsub("czerwiec.2014", "2014 6 czerwiec", names(lekarz_df))
names(lekarz_df) <- gsub("czerwiec.2015", "2015 6 czerwiec", names(lekarz_df))
names(lekarz_df) <- gsub("czerwiec.2016", "2016 6 czerwiec", names(lekarz_df))
names(lekarz_df) <- gsub("czerwiec.2017", "2017 6 czerwiec", names(lekarz_df))
names(lekarz_df) <- gsub("czerwiec.2018", "2018 6 czerwiec", names(lekarz_df))
names(lekarz_df) <- gsub("czerwiec.2019", "2019 6 czerwiec", names(lekarz_df))
names(lekarz_df) <- gsub("lipiec.2006", "2006 7 lipiec", names(lekarz_df))
names(lekarz_df) <- gsub("lipiec.2007", "2007 7 lipiec", names(lekarz_df))
names(lekarz_df) <- gsub("lipiec.2008", "2008 7 lipiec", names(lekarz_df))
names(lekarz_df) <- gsub("lipiec.2009", "2009 7 lipiec", names(lekarz_df))
names(lekarz_df) <- gsub("lipiec.2010", "2010 7 lipiec", names(lekarz_df))
names(lekarz_df) <- gsub("lipiec.2011", "2011 7 lipiec", names(lekarz_df))
names(lekarz_df) <- gsub("lipiec.2012", "2012 7 lipiec", names(lekarz_df))
names(lekarz_df) <- gsub("lipiec.2013", "2013 7 lipiec", names(lekarz_df))
names(lekarz_df) <- gsub("lipiec.2014", "2014 7 lipiec", names(lekarz_df))
names(lekarz_df) <- gsub("lipiec.2015", "2015 7 lipiec", names(lekarz_df))
names(lekarz_df) <- gsub("lipiec.2016", "2016 7 lipiec", names(lekarz_df))
names(lekarz_df) <- gsub("lipiec.2017", "2017 7 lipiec", names(lekarz_df))
names(lekarz_df) <- gsub("lipiec.2018", "2018 7 lipiec", names(lekarz_df))
names(lekarz_df) <- gsub("lipiec.2019", "2019 7 lipiec", names(lekarz_df))
names(lekarz_df) <- gsub("sierpieñ.2006", "2006 8 sierpieñ", names(lekarz_df))
names(lekarz_df) <- gsub("sierpieñ.2007", "2007 8 sierpieñ", names(lekarz_df))
names(lekarz_df) <- gsub("sierpieñ.2008", "2008 8 sierpieñ", names(lekarz_df))
names(lekarz_df) <- gsub("sierpieñ.2009", "2009 8 sierpieñ", names(lekarz_df))
names(lekarz_df) <- gsub("sierpieñ.2010", "2010 8 sierpieñ", names(lekarz_df))
names(lekarz_df) <- gsub("sierpieñ.2011", "2011 8 sierpieñ", names(lekarz_df))
names(lekarz_df) <- gsub("sierpieñ.2012", "2012 8 sierpieñ", names(lekarz_df))
names(lekarz_df) <- gsub("sierpieñ.2013", "2013 8 sierpieñ", names(lekarz_df))
names(lekarz_df) <- gsub("sierpieñ.2014", "2014 8 sierpieñ", names(lekarz_df))
names(lekarz_df) <- gsub("sierpieñ.2015", "2015 8 sierpieñ", names(lekarz_df))
names(lekarz_df) <- gsub("sierpieñ.2016", "2016 8 sierpieñ", names(lekarz_df))
names(lekarz_df) <- gsub("sierpieñ.2017", "2017 8 sierpieñ", names(lekarz_df))
names(lekarz_df) <- gsub("sierpieñ.2018", "2018 8 sierpieñ", names(lekarz_df))
names(lekarz_df) <- gsub("sierpieñ.2019", "2019 8 sierpieñ", names(lekarz_df))
names(lekarz_df) <- gsub("wrzesieñ.2006", "2006 9 wrzesieñ", names(lekarz_df))
names(lekarz_df) <- gsub("wrzesieñ.2007", "2007 9 wrzesieñ", names(lekarz_df))
names(lekarz_df) <- gsub("wrzesieñ.2008", "2008 9 wrzesieñ", names(lekarz_df))
names(lekarz_df) <- gsub("wrzesieñ.2009", "2009 9 wrzesieñ", names(lekarz_df))
names(lekarz_df) <- gsub("wrzesieñ.2010", "2010 9 wrzesieñ", names(lekarz_df))
names(lekarz_df) <- gsub("wrzesieñ.2011", "2011 9 wrzesieñ", names(lekarz_df))
names(lekarz_df) <- gsub("wrzesieñ.2012", "2012 9 wrzesieñ", names(lekarz_df))
names(lekarz_df) <- gsub("wrzesieñ.2013", "2013 9 wrzesieñ", names(lekarz_df))
names(lekarz_df) <- gsub("wrzesieñ.2014", "2014 9 wrzesieñ", names(lekarz_df))
names(lekarz_df) <- gsub("wrzesieñ.2015", "2015 9 wrzesieñ", names(lekarz_df))
names(lekarz_df) <- gsub("wrzesieñ.2016", "2016 9 wrzesieñ", names(lekarz_df))
names(lekarz_df) <- gsub("wrzesieñ.2017", "2017 9 wrzesieñ", names(lekarz_df))
names(lekarz_df) <- gsub("wrzesieñ.2018", "2018 9 wrzesieñ", names(lekarz_df))
names(lekarz_df) <- gsub("wrzesieñ.2019", "2019 9 wrzesieñ", names(lekarz_df))
names(lekarz_df) <- gsub("paŸdziernik.2006", "2006 90 paŸdziernik", names(lekarz_df))
names(lekarz_df) <- gsub("paŸdziernik.2007", "2007 90 paŸdziernik", names(lekarz_df))
names(lekarz_df) <- gsub("paŸdziernik.2008", "2008 90 paŸdziernik", names(lekarz_df))
names(lekarz_df) <- gsub("paŸdziernik.2009", "2009 90 paŸdziernik", names(lekarz_df))
names(lekarz_df) <- gsub("paŸdziernik.2010", "2010 90 paŸdziernik", names(lekarz_df))
names(lekarz_df) <- gsub("paŸdziernik.2011", "2011 90 paŸdziernik", names(lekarz_df))
names(lekarz_df) <- gsub("paŸdziernik.2012", "2012 90 paŸdziernik", names(lekarz_df))
names(lekarz_df) <- gsub("paŸdziernik.2013", "2013 90 paŸdziernik", names(lekarz_df))
names(lekarz_df) <- gsub("paŸdziernik.2014", "2014 90 paŸdziernik", names(lekarz_df))
names(lekarz_df) <- gsub("paŸdziernik.2015", "2015 90 paŸdziernik", names(lekarz_df))
names(lekarz_df) <- gsub("paŸdziernik.2016", "2016 90 paŸdziernik", names(lekarz_df))
names(lekarz_df) <- gsub("paŸdziernik.2017", "2017 90 paŸdziernik", names(lekarz_df))
names(lekarz_df) <- gsub("paŸdziernik.2018", "2018 90 paŸdziernik", names(lekarz_df))
names(lekarz_df) <- gsub("paŸdziernik.2019", "2019 90 paŸdziernik", names(lekarz_df))
names(lekarz_df) <- gsub("listopad.2006", "2006 91 listopad", names(lekarz_df))
names(lekarz_df) <- gsub("listopad.2007", "2007 91 listopad", names(lekarz_df))
names(lekarz_df) <- gsub("listopad.2008", "2008 91 listopad", names(lekarz_df))
names(lekarz_df) <- gsub("listopad.2009", "2009 91 listopad", names(lekarz_df))
names(lekarz_df) <- gsub("listopad.2010", "2010 91 listopad", names(lekarz_df))
names(lekarz_df) <- gsub("listopad.2011", "2011 91 listopad", names(lekarz_df))
names(lekarz_df) <- gsub("listopad.2012", "2012 91 listopad", names(lekarz_df))
names(lekarz_df) <- gsub("listopad.2013", "2013 91 listopad", names(lekarz_df))
names(lekarz_df) <- gsub("listopad.2014", "2014 91 listopad", names(lekarz_df))
names(lekarz_df) <- gsub("listopad.2015", "2015 91 listopad", names(lekarz_df))
names(lekarz_df) <- gsub("listopad.2016", "2016 91 listopad", names(lekarz_df))
names(lekarz_df) <- gsub("listopad.2017", "2017 91 listopad", names(lekarz_df))
names(lekarz_df) <- gsub("listopad.2018", "2018 91 listopad", names(lekarz_df))
names(lekarz_df) <- gsub("listopad.2019", "2019 91 listopad", names(lekarz_df))
names(lekarz_df) <- gsub("grudzieñ.2006", "2006 92 grudzieñ", names(lekarz_df))
names(lekarz_df) <- gsub("grudzieñ.2007", "2007 92 grudzieñ", names(lekarz_df))
names(lekarz_df) <- gsub("grudzieñ.2008", "2008 92 grudzieñ", names(lekarz_df))
names(lekarz_df) <- gsub("grudzieñ.2009", "2009 92 grudzieñ", names(lekarz_df))
names(lekarz_df) <- gsub("grudzieñ.2010", "2010 92 grudzieñ", names(lekarz_df))
names(lekarz_df) <- gsub("grudzieñ.2011", "2011 92 grudzieñ", names(lekarz_df))
names(lekarz_df) <- gsub("grudzieñ.2012", "2012 92 grudzieñ", names(lekarz_df))
names(lekarz_df) <- gsub("grudzieñ.2013", "2013 92 grudzieñ", names(lekarz_df))
names(lekarz_df) <- gsub("grudzieñ.2014", "2014 92 grudzieñ", names(lekarz_df))
names(lekarz_df) <- gsub("grudzieñ.2015", "2015 92 grudzieñ", names(lekarz_df))
names(lekarz_df) <- gsub("grudzieñ.2016", "2016 92 grudzieñ", names(lekarz_df))
names(lekarz_df) <- gsub("grudzieñ.2017", "2017 92 grudzieñ", names(lekarz_df))
names(lekarz_df) <- gsub("grudzieñ.2018", "2018 92 grudzieñ", names(lekarz_df))
names(lekarz_df) <- gsub("grudzieñ.2019", "2019 92 grudzieñ", names(lekarz_df))
lekarz_df = lekarz_df[sort(colnames(lekarz_df))] #Sortujê chronologicznie
#lekarz_df = subset(lekarz_df, select = -c(Nazwa) ) # Usuwam zbêdn¹ kolumnê z nazwazmi województw


lekarz_df = t(lekarz_df) #Tramsponujê dataframe ¿eby poszczególne kolumny by³y województwami 
lekarz_df = unname(lekarz_df) #Usuwam zbêdne teraz nazwy
lekarz_df = as.data.frame(lekarz_df) #Przekszta³cam na dataframe


# Normalnie nie da³o siê przkonwertowaæ danych na numeric, bo zamiast kropek by³y 
# przecinki i niemi³osiernie d³ugo siê nad tym g³owi³em. Ca³y czas otrzymywa³em 
# wartoœci NA
lekarz_dolnoslaskie = as.numeric(gsub(",", ".", as.character(lekarz_df$V1)))
lekarz_kujawsko_pomorskie = as.numeric(gsub(",", ".", as.character(lekarz_df$V2)))
lekarz_lubelskie = as.numeric(gsub(",", ".", as.character(lekarz_df$V3)))
lekarz_lubuskie = as.numeric(gsub(",", ".", as.character(lekarz_df$V4)))
lekarz_lodzkie = as.numeric(gsub(",", ".", as.character(lekarz_df$V5)))
lekarz_malopolskie = as.numeric(gsub(",", ".", as.character(lekarz_df$V6)))
lekarz_mazowieckie = as.numeric(gsub(",", ".", as.character(lekarz_df$V7)))
lekarz_opolskie = as.numeric(gsub(",", ".", as.character(lekarz_df$V8)))
lekarz_podkarpackie = as.numeric(gsub(",", ".", as.character(lekarz_df$V9)))
lekarz_podlaskie = as.numeric(gsub(",", ".", as.character(lekarz_df$V10)))
lekarz_pomorskie = as.numeric(gsub(",", ".", as.character(lekarz_df$V11)))
lekarz_slaskie = as.numeric(gsub(",", ".", as.character(lekarz_df$V12)))
lekarz_swietokrzyskie = as.numeric(gsub(",", ".", as.character(lekarz_df$V13)))
lekarz_warminsko_mazurskie = as.numeric(gsub(",", ".", as.character(lekarz_df$V14)))
lekarz_wielkopolskie = as.numeric(gsub(",", ".", as.character(lekarz_df$V15)))
lekarz_zachodniopomorskie = as.numeric(gsub(",", ".", as.character(lekarz_df$V16)))

#### Wykres cen lakarza ####
y = (1:168)
#Wykres dla lekarza i wszystkich województw
lekarz_plot = plot_ly(x = y )%>%
  layout(title = 'Zmiana cen wyzyty u lekarza specjalisty w latach 2006-2019', xaxis = list(title = 'Rok', 
                                                                                            ticktext = list("2006", "2007", "2008", "2009", "2010", "2011","2012","2013","2014","2015","2016","2017","2018","2019"), 
                                                                                            tickvals = list(1, 12, 24, 36, 48, 60, 72, 84, 96, 108, 120, 132, 144, 156)),
         yaxis = list(title = 'cena (z³)')) %>%
  add_lines(y = lekarz_dolnoslaskie, name = "dolnoœlaskie")%>%
  add_lines(y = lekarz_kujawsko_pomorskie, name = "kujawsko-pomorskie")%>%
  add_lines(y = lekarz_lubelskie, name = "lubelskie")%>%
  add_lines(y = lekarz_lubuskie, name = "lubuskie")%>%
  add_lines(y = lekarz_lodzkie, name = "³ódzkie")%>%
  add_lines(y = lekarz_malopolskie, name = "ma³opolskie")%>%
  add_lines(y = lekarz_mazowieckie, name = "mazowieckie")%>%
  add_lines(y = lekarz_opolskie, name = "opolskie")%>%
  add_lines(y = lekarz_podkarpackie, name = "podkarpackie")%>%
  add_lines(y = lekarz_podlaskie, name = "podlaskie")%>%
  add_lines(y = lekarz_pomorskie, name = "pomorskie")%>%
  add_lines(y = lekarz_slaskie, name = "œl¹skie")%>%
  add_lines(y = lekarz_swietokrzyskie, name = "œwietokrzyskie")%>%
  add_lines(y = lekarz_warminsko_mazurskie, name = "warmiñsko_mazurskie")%>%
  add_lines(y = lekarz_wielkopolskie, name = "wielkopolskie")%>%
  add_lines(y = lekarz_zachodniopomorskie, name = "zachodniopomorskie")
#print(lekarz_plot)

#### Œrednie roczne ceny lekarza ####
roczne_sr_lekarz = data.frame('','','','','','','','','','','','','', stringsAsFactors = FALSE)
names(roczne_sr_lekarz) = c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016",
                            "2017","2018")
for (i in 1:13) {
  roczne_sr_lekarz[1,i] = round(mean(lekarz_dolnoslaskie[(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_lekarz[2,i] = round(mean(lekarz_kujawsko_pomorskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_lekarz[3,i] = round(mean(lekarz_lubelskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_lekarz[4,i] = round(mean(lekarz_lubuskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_lekarz[5,i] = round(mean(lekarz_lodzkie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_lekarz[6,i] = round(mean(lekarz_malopolskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_lekarz[7,i] = round(mean(lekarz_mazowieckie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_lekarz[8,i] = round(mean(lekarz_opolskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_lekarz[9,i] = round(mean(lekarz_podkarpackie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_lekarz[10,i] = round(mean(lekarz_podlaskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_lekarz[11,i] = round(mean(lekarz_pomorskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_lekarz[12,i] = round(mean(lekarz_slaskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_lekarz[13,i] = round(mean(lekarz_swietokrzyskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_lekarz[14,i] = round(mean(lekarz_warminsko_mazurskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_lekarz[15,i] = round(mean(lekarz_wielkopolskie [(1+(i-1)*12):(12*i)]), digits = 2)
  roczne_sr_lekarz[16,i] = round(mean(lekarz_zachodniopomorskie[(1+(i-1)*12):(12*i)]), digits = 2)
}
roczne_sr_lekarz = t(roczne_sr_lekarz)
roczne_sr_lekarz = unname(roczne_sr_lekarz) #Usuwam zbêdne teraz nazwy
roczne_sr_lekarz = as.data.frame(roczne_sr_lekarz) #Przekszta³cam na dataframe

#### Œrednia cena lekarza dla ka¿dego wojewódstwa ####
lekarz_dolnoslaskie_sr = round(mean(lekarz_dolnoslaskie), digits = 2)               #1
lekarz_kujawsko_pomorskie_sr = round(mean(lekarz_kujawsko_pomorskie), digits = 2)   #2
lekarz_lubelskie_sr = round(mean(lekarz_lubelskie), digits = 2)                     #3
lekarz_lubuskie_sr = round(mean(lekarz_lubuskie), digits = 2)                       #4
lekarz_lodzkie_sr = round(mean(lekarz_lodzkie), digits = 2)                         #5
lekarz_malopolskie_sr = round(mean(lekarz_malopolskie), digits = 2)                 #6
lekarz_mazowieckie_sr = round(mean(lekarz_mazowieckie), digits = 2)                 #7
lekarz_opolskie_sr = round(mean(lekarz_opolskie), digits = 2)                       #8
lekarz_podkarpackie_sr = round(mean(lekarz_podkarpackie), digits = 2)               #9
lekarz_podlaskie_sr = round(mean(lekarz_podlaskie), digits = 2)                     #10
lekarz_pomorskie_sr = round(mean(lekarz_pomorskie), digits = 2)                     #11
lekarz_slaskie_sr = round(mean(lekarz_slaskie), digits = 2)                         #12
lekarz_swietokrzyskie_sr = round(mean(lekarz_swietokrzyskie), digits = 2)           #13
lekarz_warminsko_mazurskie_sr = round(mean(lekarz_warminsko_mazurskie), digits = 2) #14
lekarz_wielkopolskie_sr = round(mean(lekarz_wielkopolskie), digits = 2)             #15
lekarz_zachodniopomorskie_sr = round(mean(lekarz_zachodniopomorskie), digits = 2)   #16



#### Lekarz œrednie, najw, najmn ceny w jednym wektorze ####
sr_cena_lekarz = c(lekarz_dolnoslaskie_sr,lekarz_kujawsko_pomorskie_sr,lekarz_lubelskie_sr,lekarz_lubuskie_sr,lekarz_lodzkie_sr,
                   lekarz_malopolskie_sr,lekarz_mazowieckie_sr,lekarz_opolskie_sr,lekarz_podkarpackie_sr,lekarz_podlaskie_sr,
                   lekarz_pomorskie_sr,lekarz_slaskie_sr,lekarz_swietokrzyskie_sr,lekarz_warminsko_mazurskie_sr,
                   lekarz_wielkopolskie_sr,lekarz_zachodniopomorskie_sr)

lekarz_najmn_w_kazdym_woj = c(min(lekarz_dolnoslaskie),min(lekarz_kujawsko_pomorskie),min(lekarz_lubelskie),
                              min(lekarz_lubuskie),min(lekarz_lodzkie),min(lekarz_malopolskie),
                              min(lekarz_mazowieckie),min(lekarz_opolskie),min(lekarz_podkarpackie),
                              min(lekarz_podlaskie),min(lekarz_pomorskie),min(lekarz_slaskie),
                              min(lekarz_swietokrzyskie),min(lekarz_warminsko_mazurskie),
                              min(lekarz_wielkopolskie),min(lekarz_zachodniopomorskie))

lekarz_najw_w_kazdym_woj = c(max(lekarz_dolnoslaskie),max(lekarz_kujawsko_pomorskie),max(lekarz_lubelskie),
                             max(lekarz_lubuskie),max(lekarz_lodzkie),max(lekarz_malopolskie),
                             max(lekarz_mazowieckie),max(lekarz_opolskie),max(lekarz_podkarpackie),
                             max(lekarz_podlaskie),max(lekarz_pomorskie),max(lekarz_slaskie),
                             max(lekarz_swietokrzyskie),max(lekarz_warminsko_mazurskie),
                             max(lekarz_wielkopolskie),max(lekarz_zachodniopomorskie))

###  Najwy¿sze i najni¿sze, ró¿ne

lekarz_sr_cena = mean(sr_cena_lekarz) #Œrednia cena lekarza dla ca³ego kraju
lekarz_odsd_cena = sd(sr_cena_lekarz)  #Odchylenie standardowe ceny lekarza

lekarz_najw_sr = max(sr_cena_lekarz) #Najwy¿sza œrednia cena lekarza
lekarz_najm_sr = min(sr_cena_lekarz) #Najni¿sza œrednia cena lekarza
lekarz_najmn_k = min(lekarz_najmn_w_kazdym_woj) #Najni¿sza cena lekarza kiedykolwiek
lekarz_najw_k = max(lekarz_najw_w_kazdym_woj)   #Najwy¿sza cena lekarza kiedykolwiek


lek_najw_sr = as.numeric(match(lekarz_najw_sr,sr_cena_lekarz))  #Numer województwa
lek_najmn_sr = as.numeric(match(lekarz_najm_sr,sr_cena_lekarz)) #Numer województwa
print(paste("Najwy¿sza œrednia cena wizyty u lekarza specjalisty jest w województwie", wojewodztwa[lek_najw_sr],
            "i wynosi",lekarz_najw_sr))
print(paste("Najni¿sza œrednia cena wizyty u lekarza specjalisty jest w województwie", wojewodztwa[lek_najmn_sr],
            "i wynosi",lekarz_najm_sr))


lekarz_najmn_kazde = as.numeric(match(lekarz_najmn_k, lekarz_najmn_w_kazdym_woj))  #Numer województwa
lekarz_najw_kazde = as.numeric(match(lekarz_najw_k,   lekarz_najw_w_kazdym_woj))   #Numer województwa
lekarz_najmn_kiedy =  match(lekarz_najmn_k,           lekarz_warminsko_mazurskie)  #Kiedy
lekarz_najw_kiedy =  match(lekarz_najw_k,             lekarz_wielkopolskie )       #Kiedy

print(paste("Najni¿sza cena wizyty u lekarza specjalisty by³a w", daty[lekarz_najmn_kiedy], "w województwie", 
            wojewodztwa[lekarz_najmn_kazde],"i wynosi³a", lekarz_najmn_k, "z³"))

print(paste("Najwy¿sza cena wizyty u lekarza specjalisty by³a w", daty[lekarz_najw_kiedy], "w województwie",
            wojewodztwa[lekarz_najw_kazde],"i wynosi³a", lekarz_najw_k, "z³"))

print(paste("Odchylenie standardowe cen wizyty u lekarza specjalisty wynosi", round(lekarz_odsd_cena, digits = 2), "z³otego"))

#### Korelacja p³acy minimalnej do œredniej ceny lekarza ####
lekarz_placa_min_kor = c(cor(as.numeric(as.character(placa_min_df$V1)), as.numeric(as.character(roczne_sr_lekarz$V1))),
                         cor(as.numeric(as.character(placa_min_df$V2)), as.numeric(as.character(roczne_sr_lekarz$V2))),
                         cor(as.numeric(as.character(placa_min_df$V3)), as.numeric(as.character(roczne_sr_lekarz$V3))),
                         cor(as.numeric(as.character(placa_min_df$V4)), as.numeric(as.character(roczne_sr_lekarz$V4))),
                         cor(as.numeric(as.character(placa_min_df$V5)), as.numeric(as.character(roczne_sr_lekarz$V5))),
                         cor(as.numeric(as.character(placa_min_df$V6)), as.numeric(as.character(roczne_sr_lekarz$V6))),
                         cor(as.numeric(as.character(placa_min_df$V7)), as.numeric(as.character(roczne_sr_lekarz$V7))),
                         cor(as.numeric(as.character(placa_min_df$V8)), as.numeric(as.character(roczne_sr_lekarz$V8))),
                         cor(as.numeric(as.character(placa_min_df$V9)), as.numeric(as.character(roczne_sr_lekarz$V9))),
                         cor(as.numeric(as.character(placa_min_df$V10)), as.numeric(as.character(roczne_sr_lekarz$V10))),
                         cor(as.numeric(as.character(placa_min_df$V11)), as.numeric(as.character(roczne_sr_lekarz$V11))),
                         cor(as.numeric(as.character(placa_min_df$V12)), as.numeric(as.character(roczne_sr_lekarz$V12))),
                         cor(as.numeric(as.character(placa_min_df$V13)), as.numeric(as.character(roczne_sr_lekarz$V13))))


print(paste("Uœredniona korelacja minimalnej pensji do ceny lekarza wynosi", round(mean(lekarz_placa_min_kor), 
                                                                                   digits = 4), "co wskazuje na bardzo siln¹ korelacjê"))
#### Korelacja œredniej pensji do œredniej ceny lekarza ####

lekarz_sr_pensja_kor = c(cor(DOLNO, as.numeric(as.character(roczne_sr_lekarz$V1))),
                         cor(KUJAW, as.numeric(as.character(roczne_sr_lekarz$V2))),
                         cor(LUBEL, as.numeric(as.character(roczne_sr_lekarz$V3))),
                         cor(LUBUS, as.numeric(as.character(roczne_sr_lekarz$V4))),
                         cor(LODZK, as.numeric(as.character(roczne_sr_lekarz$V5))),
                         cor(MALOP, as.numeric(as.character(roczne_sr_lekarz$V6))),
                         cor(MAZOW, as.numeric(as.character(roczne_sr_lekarz$V7))),
                         cor(OPOLS, as.numeric(as.character(roczne_sr_lekarz$V8))),
                         cor(PODKA, as.numeric(as.character(roczne_sr_lekarz$V9))),
                         cor(PODLA, as.numeric(as.character(roczne_sr_lekarz$V10))),
                         cor(POMOR, as.numeric(as.character(roczne_sr_lekarz$V11))),
                         cor(SLASK, as.numeric(as.character(roczne_sr_lekarz$V12))),
                         cor(SWIET, as.numeric(as.character(roczne_sr_lekarz$V13))),
                         cor(WARMI, as.numeric(as.character(roczne_sr_lekarz$V14))),
                         cor(WIELK, as.numeric(as.character(roczne_sr_lekarz$V15))),
                         cor(ZACHO, as.numeric(as.character(roczne_sr_lekarz$V16))))
sr_lekarz_kor = mean(lekarz_sr_pensja_kor)
print(paste("Uœredniona korelacja œredniej pensji do ceny lekarza wynosi", round(sr_lekarz_kor, 
             digits = 4), "co wskazuje na bardzo siln¹ korelacjê"))

