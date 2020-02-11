library(ggplot2)
library(gridExtra)
library(grid)
library(tidyr)
library(dplyr)
library(SmarterPoland)
library(ggpubr)


#Wybranie sciezki z danymi dla poszczegolnych lat
sciezka_do_danych <- "Tu wpisz sciezke do danych"
setwd(sciezka_do_danych)

#Sciagniecie listy wszystkich plikow o rozszerzeniu .csv
tabela = list.files(sciezka_do_danych, pattern="*.csv")
#Stworzenie obiektu lista dla danych z poszczegolnych lat
lista_plikow = list()

#Petla dla wszystkich (14.) plikow z danymi
for (i in 1:length(tabela)){
  
  #Zapisywanie pliku z danymi (z kodowaniem w standardzie UTF-8) do tymczasowej zmiennej
  roczne_dane <-  read.csv2(tabela[i], encoding = "UTF-8", header = FALSE)
  #Zapisanie jako macierz > transpozycja macierzy zeby zamienic kolumny z wierszami > zapisanie jako data.frame do tymczasowej zmiennej
  roczne_dane <- as.data.frame(t(as.matrix(roczne_dane)))
  
  #Warunek dla pierwszego pliku i calej reszty
  if (i == 1){
    #Usuniecie ostatniego wiersza -> przy transpozycji tworzy sie pusty wiersz z NA
    roczne_dane <- roczne_dane[-nrow(roczne_dane), ]
    #Zapisanie do listy z plikami
    lista_plikow[[i]] <- roczne_dane 
  } else {
    #Usuniecie dwoch pierwszych wierszy z nazwami wojewodztw oraz kodami oraz ostatniego pustego
    roczne_dane <- roczne_dane[c(-1,-2, -nrow(roczne_dane)), ]
    lista_plikow[[i]] <- roczne_dane 
  }
  
}

#Uzycie funkcji rbind do scalenia wszystkich plikow po wierszach
glowne_dane <- do.call(rbind, lista_plikow) 
#Usuniecie pierwszego wiersza z kodami
glowne_dane <- glowne_dane[-1,]

#Wrzucenie pierwszego wiersza jako nazwy kolumn
names(glowne_dane) <- as.matrix(glowne_dane[1, ])
#Usuniecie pierwszego wiersza z niepotrzebnymi nazwami kolumn
glowne_dane <- glowne_dane[-1, ]

#Rozszczepienie pierwszej kolumny
glowne_dane <- separate(data = glowne_dane, col = Nazwa, into = c("MIESIAC", "PRODUKT", "USUN1", "ROK", "USUN2"), sep = ";")
#Usuniecie niepotrzebnych kolumn
glowne_dane <- glowne_dane[,c(-3, -5)]

#Zmiana "," na "." dla kolumn z danymi
#Dane zostaly sciagniete w ten sposob z GUS 
start <- which( colnames(glowne_dane)=="POLSKA" )
koniec <- which( colnames(glowne_dane)=="ZACHODNIOPOMORSKIE" )

for (i in start:koniec){
  glowne_dane[,i] <- as.numeric(as.character( sub(",", ".", glowne_dane[,i] ))) 
}

glowne_dane$ROK <- as.integer(glowne_dane$ROK)
glowne_dane$PRODUKT <- as.factor(glowne_dane$PRODUKT)

############################################################### STWORZENIE WERTYKALNEGO ZESTAWU DANYCH

#Ustawianie working directory. Pojedyncze \ czasami nie dziala i wyrzuca blad dlatego trzeba dac \\
setwd("Tu wpisz sciezke do danych")

#stworzenie drugiego zestawu danych "podloznych" - wyciagniecie cen dla wszystkich wojewodztw do jednej kolumny. gather() z library(tidyr)
set_data = gather(glowne_dane, "OBSZAR", "CENA", POLSKA:ZACHODNIOPOMORSKIE)

#zgrupowanie danych wejsciowych po ROK, OBSZAR, PRODUKT i obliczenie srednich cen i odchylen z miesiecznych wartosci.
#Produktem jest tablica pozbawiona informacji o miesiacach ale ze srednimi cenami dla calego roku.
#group_by() i summarise() z library(dplyr)
zestaw_sr_odch <- set_data %>% group_by(ROK, OBSZAR, PRODUKT) %>% summarise(SREDNIA = mean(CENA), ODCH = sd(CENA))


############################################################### WYKRESY DLA POLSKI (BOXPLOT)

#Stworzenie zestawu tylko dla Polski uzywajac funkcji subset 
zestaw_polska_box <- subset(set_data, set_data$OBSZAR == "POLSKA")

#Okreslenie zmiennej jako listy
lista_wykresow_polska_box <- list()
#Petla for dla wektora unikalnych wartosci kolumny PRODUKT. Powtarza sie tyle razy ile jest unikalnych wartosci.
for (i in 1:length(unique(zestaw_polska_box$PRODUKT))){
  
  #Stworzenie tymczasowego podzestawu danych dla aktualnej wartosci z wektora unikalnych wartosci PRODUKT
  podzestaw = subset(zestaw_polska_box, zestaw_polska_box$PRODUKT == unique(zestaw_polska_box$PRODUKT)[i])
  #Zapisywanie wykresow dla Polski dla wszystkich produktow uzywajac wartosci z aktualnego podzestawu
  #lista_wykresow_polska_box bedzie lista wykresow dla Polski dla kazdego unikalnego produktu
  #as.factor -> zeby bylo widac kazdy rok, ggplot() z library(ggplot2)
  lista_wykresow_polska_box[[i]] <- ggplot(data = podzestaw, aes(x = as.factor(ROK), y = CENA)) +
    #Aby uzyskac wykres boxplot
    geom_boxplot() +
    #Podpisy dla osi
    labs(x = "ROK", y = "SREDNI KOSZT (zl)") +
    #Tutul dla kazdego z wykresow -> kazdy kolejny wiersz z wektora unikalnych produktow
    ggtitle(unique(zestaw_polska_box$PRODUKT)[i])
} 

# grid.arrange() z library(gridExtra)
do.call(grid.arrange, c(lista_wykresow_polska_box, ncol = 2))


############################################################### WYKRESY DLA POLSKI (POROWNANIE Z MINIMALNA KRAJOWA)

#Stworzenie podzestawu dla Polski aby porownac wartosci z minimalna krajowa
zestaw_polska_min <- subset(zestaw_sr_odch, zestaw_sr_odch$OBSZAR == "POLSKA")
#Zapisanie pliku z wartosciami minimalnej kraowej do zmeinnej (schemat kodowanie UTF-8)
zestaw_minimalna <- read.csv("MINIMALNA.csv", encoding = "UTF-8")
#Z wektora nazw zmiennej frame_data trzeba znalezc niepoprawnie nazwana kolumne (blad podczas pobierania danych) i zamienic na odczytywalna nazwe
names(zestaw_minimalna)[names(zestaw_minimalna) == "X.U.FEFF.ROK"] <- "ROK"


lista_wykresow_polska_min <- list()
#Zmienna skala osi y w zaleznosci od produktu

for (i in 1:length(unique(zestaw_polska_min$PRODUKT))){
  
  podzestaw = subset(zestaw_polska_min, zestaw_polska_min$PRODUKT == unique(zestaw_polska_min$PRODUKT)[i])
  #Zlaczenie zestawu dla minimalnej oraz obecnego tymczasowego podzestawu po kolumnie ROK
  podzestaw <- (merge(zestaw_minimalna, podzestaw, by = "ROK"))
  #Zapisanie do zmiennej wartosci korelacji pomiedzy zmianami w srednich cenach aktualnego produktu oraz zmian w minimalnej wyplacie
  #grobTree() z library(grid)
  korelacja = grobTree(textGrob(paste("Korelacja Pearsona : ", round(cor(podzestaw$SREDNIA, podzestaw$MINIMALNA), 4) ), 
                                x = 0.40, y = 0.80, hjust = 0, gp = gpar(col = "red", fontsize = 11, fontface = "bold")))
  
  #Wykres zostanie przedstawiony jako procentowe zmiany w srednich cenach i wzroscie minimalnej krajowej.
  podzestaw <- podzestaw %>% mutate(
    prct_zmiana_sr_cen = ((SREDNIA - lag(SREDNIA))/(ROK - lag(ROK)))/SREDNIA * 100,
    prct_zmiana_minim = ((MINIMALNA - lag(MINIMALNA))/(ROK - lag(ROK)))/MINIMALNA * 100
    )
  #Wyrzucenie NA i przeskalowanie do 100%
  podzestaw[is.na(podzestaw)] <- 0
  
  lista_wykresow_polska_min[[i]] <- ggplot(data = podzestaw, aes(x = ROK)) +
    #Trzeba zmienic skale zeby bylo widac zmaine dla obu wykresow jednoczesnie
    geom_line(aes(y = prct_zmiana_sr_cen, colour = "Zmiana Srednich Cen (%)")) +
    geom_line(aes(y = prct_zmiana_minim, colour = "Zmiana min wynagr (%)")) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_x_continuous(breaks = seq(min(podzestaw$ROK), max(podzestaw$ROK), 1)) +
    geom_point(aes(y = prct_zmiana_sr_cen), size=3, shape=21) +
    geom_point(aes(y = prct_zmiana_minim), size=3, shape=21) +
    #Odchylenie od sredniej ze zmiana skali
    #geom_errorbar(aes(ymin=SREDNIA - ODCH, ymax=SREDNIA + ODCH), width=0.2, size=0.5) +
    #Dodatnie napisu z wartoscia korelacji
    annotation_custom(korelacja) + 
    labs(x = "ROK", y = "ZMIANA") +
    ggtitle(unique(zestaw_polska_min$PRODUKT)[i])
} 

do.call(grid.arrange, c(lista_wykresow_polska_min, ncol = 2))

#Wartosci dla korelacji Pearsona: 

#Silnia ujemna  = -1.0 <-> -0.5
#Slaba          = -0.5 <-> 0.5
#Silna dodatnia =  0.5 <-> 1.0


############################################################### WYKRESY DLA SWIADCZEN SOCJALNYCH 500+


zestaw_polska_socj <- subset(zestaw_sr_odch, zestaw_sr_odch$OBSZAR == "POLSKA")
lista_wykresow_polska_socj <- list()
#Ustalenie wzoru do okreslenia linii trendu y = a*x + b
formula <- y ~ x

for (i in 1:length(unique(zestaw_polska_socj$PRODUKT))){
  
  podzestaw = subset(zestaw_polska_socj, zestaw_polska_socj$PRODUKT == unique(zestaw_polska_socj$PRODUKT)[i])
  socjal_legenda = grobTree(textGrob(paste("Wprowadzenie 500+ (2016r)"), 
                                x = 0.60, y = 0.80, hjust = 0, gp = gpar(col = "black", fontsize = 11, fontface = "bold")))
  #Dodanie pomocnej kolumny do podzestawu. Pomoze w stworzeniu osobnych linii trendu dla okresu przed i po wprowadzeniu 500+
  podzestaw$OKRES <- ifelse(podzestaw$ROK >= 2016, "PO", "PRZED")
  
  lista_wykresow_polska_socj[[i]] <- ggplot(data = podzestaw, aes(x = ROK, y = SREDNIA, colour = OKRES)) +
    scale_x_continuous(breaks = seq(min(podzestaw$ROK), max(podzestaw$ROK), 1)) +
    geom_point(aes(y = SREDNIA), size=3, shape=21) +
    geom_errorbar(aes(ymin=SREDNIA - ODCH, ymax=SREDNIA + ODCH), width=0.2, size=0.5) +
    geom_vline(xintercept=as.numeric(2016), linetype=1) +
    #Dodanie linii trendu dla roznych wartosci z kolumny OKRES przy pomocy wczesniej ustalonego wzoru
    stat_smooth(aes(fill = OKRES, color = OKRES), method = "lm", formula = formula) +
    #Dodanie rownania linii trendu na wykres
    #stat_regline_equation() library(ggpubr)
    stat_regline_equation(
      aes(label =  paste(..eq.label..)),
      formula = formula
    ) +
    
    annotation_custom(socjal_legenda) + 
    labs(x = "ROK", y = "SREDNI KOSZT (zl)") +
    ggtitle(unique(zestaw_polska_socj$PRODUKT)[i])
} 

do.call(grid.arrange, c(lista_wykresow_polska_socj, ncol = 2))

#Jezeli dla obu rownan wspolczynnik a jest ujemny to obie linie trendu ida w dol.
#Jezeli dla obu zas jest dodatni to obie ida w gore.
#Nastepnie delta(y)/delta(x) dla obu rownan zeby sprawdzic dla ktorej linii trendu zmiany zachodza szybciej.
#Czy przed wprowadzeniem czy po wprowadzeniu 500+.

############################################################## WYKRESY DLA WOJEWODZTW

zestaw_ex_polska <- subset(zestaw_sr_odch, zestaw_sr_odch$OBSZAR != "POLSKA")
lista_wykresow_wojew <- list()

for (i in 1:length(unique(zestaw_ex_polska$PRODUKT))){
  podzestaw = subset(zestaw_ex_polska, zestaw_ex_polska$PRODUKT == unique(zestaw_ex_polska$PRODUKT)[i])
  lista_wykresow_wojew[[i]] <- ggplot(data = podzestaw, aes(x = ROK, y = SREDNIA, color = OBSZAR)) +
    scale_x_continuous(breaks = seq(min(podzestaw$ROK), max(podzestaw$ROK), 1)) +
    geom_line() +
    geom_point() + 
    geom_errorbar(aes(ymin=SREDNIA - ODCH, ymax=SREDNIA + ODCH), width=0.2, size=0.5) +
    labs(x = "ROK", y = "SREDNI KOSZT (zl)") +
    ggtitle(unique(zestaw_ex_polska$PRODUKT)[i])
} 

do.call(grid.arrange,c(lista_wykresow_wojew, ncol = 2))


#Mozna przyjac teze, ze najtaniej bylo wtedy i tam gdzie najwiecej razy mozna sobie bylo pozwolic na kupienie wszystkich wymienionych produktow.
#Dlatego najtaniej bedzie tam gdzie stosunek sumy kosztu wszystkich produktow do wyplaty minimalnej bedzie najwyzszy
#a najtaniej tam gdzie wartosc ta bedzie najmniejsza

#Zsumowanie cen dla zestawu produktow dla kazdego wojewodztwa i dla kazdego roku i zapisanie do nowego data.frame
zestaw_sum_stosunek <- zestaw_ex_polska %>% group_by(ROK, OBSZAR) %>% summarise(SREDNIA = sum(SREDNIA))
#Zmiana nazwy kolumny zeby bardziej sugerowal jej zawartosc
names(zestaw_sum_stosunek)[names(zestaw_sum_stosunek) == "SREDNIA"] <- "SUMA"

#Scalenie ze soba dwoch zestawow danych uzywajac kolumny ROK
zestaw_sum_stosunek <- (merge(zestaw_minimalna, zestaw_sum_stosunek, by = 'ROK'))
#Podzielenie przez siebie wyplaty minimalnej i sumy cen wszystkich produktow
zestaw_sum_stosunek$STOSUNEK <- zestaw_sum_stosunek$MINIMALNA/zestaw_sum_stosunek$SUMA

#Wyciagniecie wiersza z max wartoscia w kolumnie STOSUNEK -> tutaj bedzie najtaniej
zestaw_sum_stosunek[which.max(zestaw_sum_stosunek$STOSUNEK),]
#Wyciagniecie wiersza z min wartoscia w kolumnie STOSUNEK -> tutaj bedzie najdrozej
zestaw_sum_stosunek[which.min(zestaw_sum_stosunek$STOSUNEK),]


############################################################## EKSTRAPOLACJA NIEKOMPLETNYCH DANYCH
library(zoo) #<- dodanie nowej biblioteki potrzebnej do ekstrapolowania danych

#Z racji tego, ze widac brak danych dla gazety regionalnej w wojewodztwie Slaskim oraz 
#brak jakichkolwiek danych dla mydla toaletowego po roku 2017 mozna sprobowac ekstrapolacji ich srednich cen
#(bez odchylen bo nie mamy danych do tego) tak zeby dostac brakujace wartosci.
#Bedzie to ekstrapolacja liniowa dla gazety oraz nie-liniowa dla mydla.

#GAZETA REGIONALNA
dane_gazeta <- subset(zestaw_sr_odch, zestaw_sr_odch$PRODUKT == "gazeta regionalna")

dane_split <- split(dane_gazeta, dane_gazeta$OBSZAR)
dane_gazeta_slaskie <- dane_split$ŚLĄSKIE


#Prezentacja wykresu dla wszystkich wojewodztw dla gazety
ggplot(data = dane_gazeta, aes(x = ROK, y = SREDNIA, color = OBSZAR)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=SREDNIA - ODCH, ymax=SREDNIA + ODCH)) +
  labs(x = "ROK", y = "SREDNI KOSZT (zl)")

#Prezentacja wykresu tylko dla wojewodztwa slaskiego dla gazety. Widac na nich brak danych dla ostatnich dwoch lat.
ggplot(data = dane_gazeta_slaskie, aes(x = ROK, y = SREDNIA, color = OBSZAR)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=SREDNIA - ODCH, ymax=SREDNIA + ODCH)) +
  labs(x = "ROK", y = "SREDNI KOSZT (zl)")


#----------------------------------------------------------------------------

#Zamiana wartosci 0 na NA dla kazdego wystapienia 0 w dane_gazeta data.frame
dane_gazeta[dane_gazeta==0] <- NA
#Sprawdzenie czy podmiana sie udala - powinno pokazywac TRUE dla podmienionych komorek
is.na.data.frame(dane_gazeta)

#Zgrupowanie danych po wojewodztwach
dane_gazeta <- group_by(dane_gazeta, OBSZAR)
#Zamienienie NA na ekstrapolowane wartosci dla kazdego z wojewodztw osobno (Slask)
#Zapisanie wynikow do nowej kolumny SREDNIA_EKSTRAPOLACJA
dane_gazeta <- mutate(dane_gazeta, SREDNIA_EKSTRAPOLACJA=na.spline(SREDNIA))

#Prezentacja wykresow dla wszystkch wojewodztw wraz z ekstrapolowanymi danymi
ggplot(data = dane_gazeta, aes(x = ROK, y = SREDNIA_EKSTRAPOLACJA, color = OBSZAR)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=SREDNIA - ODCH, ymax=SREDNIA + ODCH)) +
  labs(x = "ROK", y = "SREDNI KOSZT (zl)")

#----------------------------------------------------------------------------
dane_split <- split(dane_gazeta, dane_gazeta$OBSZAR)
dane_gazeta_slaskie <- dane_split$ŚLĄSKIE

#Prezentacja wykresu dla Slaska wraz z ekstrapolowanymi danymi
ggplot(data = dane_gazeta_slaskie, aes(x = ROK, y = SREDNIA_EKSTRAPOLACJA, color = OBSZAR)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=SREDNIA - ODCH, ymax=SREDNIA + ODCH)) +
  labs(x = "ROK", y = "SREDNI KOSZT (zl)")


#MYDLO TOALETOWE
dane_split <- split(zestaw_sr_odch, zestaw_sr_odch$PRODUKT)
dane_mydlo <- dane_split$`mydło toaletowe - za 100g`

#Zgrupowanie danych dla mydla po wojewodztwach
#Dodanie pustego wiersza dla roku 2018
dane_mydlo <- dane_mydlo %>% 
  group_by(OBSZAR) %>% 
  do(add_row(., OBSZAR=unique(.$OBSZAR), ROK = last(.$ROK)+1, PRODUKT = unique(.$PRODUKT), SREDNIA=NA, ODCH=NA))

#Zgrupowanie danych dla mydla po wojewodztwach
#Dodanie pustego wiersza dla roku 2019
dane_mydlo <- dane_mydlo %>% 
  group_by(OBSZAR) %>% 
  do(add_row(., OBSZAR=unique(.$OBSZAR), ROK = last(.$ROK)+1, PRODUKT = unique(.$PRODUKT), SREDNIA=NA, ODCH=NA))

dane_mydlo <- group_by(dane_mydlo, OBSZAR)
#Nieliniowa ekstrapolacja danych dla kazdego z wojewodztw z osobna 
dane_mydlo <- mutate(dane_mydlo, SREDNIA_EKSTRAPOLACJA=na.spline(SREDNIA, method = "natural"))

#Prezentacja wynikow ekstrapolowanych wartosci dla wszystkich wojewodztw
ggplot(data = dane_mydlo, aes(x = ROK, y = SREDNIA_EKSTRAPOLACJA, color = OBSZAR)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=SREDNIA - ODCH, ymax=SREDNIA + ODCH)) +
  labs(x = "ROK", y = "SREDNI KOSZT (zl)")

############################################################### WYKRESY DLA KRACHU NA GIELDZIE 


#Uzywajac danych warto sprobowac sprawdzenic czy krach na gieldzie w 2008 mial wplyw na to
#jak wygladaja linie trendu dla poszczeglnych okresow PRZED i PO

zestaw_polska_krach <- subset(zestaw_sr_odch, zestaw_sr_odch$OBSZAR == "POLSKA")
lista_wykresow_polska_krach <- list()
formula <- y ~ x
#formula <- y ~ poly(x, 3, raw = TRUE)

for (i in 1:length(unique(zestaw_polska_krach$PRODUKT))){
  
  podzestaw = subset(zestaw_polska_krach, zestaw_polska_krach$PRODUKT == unique(zestaw_polska_krach$PRODUKT)[i])
  krach_legenda = grobTree(textGrob(paste("Krach na gieldzie (2008r)"), 
                                     x = 0.60, y = 0.80, hjust = 0, gp = gpar(col = "black", fontsize = 11, fontface = "bold")))
  
  podzestaw$OKRES <- ifelse(podzestaw$ROK >= 2009, "PO", "PRZED")
  
  lista_wykresow_polska_krach[[i]] <- ggplot(data = podzestaw, aes(x = ROK, y = SREDNIA, colour = OKRES)) +
    scale_x_continuous(breaks = seq(min(podzestaw$ROK), max(podzestaw$ROK), 1)) +
    geom_point(aes(y = SREDNIA), size=3, shape=21) +
    geom_errorbar(aes(ymin=SREDNIA - ODCH, ymax=SREDNIA + ODCH), width=0.2, size=0.5) +
    geom_vline(xintercept=as.numeric(2008), linetype=1) +
    
    stat_smooth(aes(fill = OKRES, color = OKRES), method = "lm", formula = formula) +
    
    stat_regline_equation(
      aes(label =  paste(..eq.label..)),
      formula = formula
    ) +
    
    annotation_custom(krach_legenda) + 
    labs(x = "ROK", y = "SREDNI KOSZT (zl)") +
    ggtitle(unique(zestaw_polska_krach$PRODUKT)[i])
} 

do.call(grid.arrange, c(lista_wykresow_polska_krach, ncol = 2))
