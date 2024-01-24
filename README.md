# Projekt przejściowy 2

Instrukcja dla analizy sprzedaży gier wideo

Użyte biblioteki R

library(ggplot2)
library(dplyr)
library(forcats) #uporządkowanie wartości w wykresach
library(tidyr)
library(ggpubr)

Opis projektu i przegląd danych

Podany skrypt R analizuje zestaw danych zawierający informacje o sprzedaży gier wideo od 1980 do 2020 roku, obejmujący 16 598 wpisów. Zestaw danych zawiera różne kolumny, takie jak Rank, Name, Platform, Year, Genre, Publisher, NA_Sales, EU_Sales, JP_Sales, Other_Sales i Global_Sales.

Kolumny:

Rank: Pozycja w rankingu sprzedaży.
Name: Nazwa gry.
Platform: Platforma gry.
Year: Rok wydania gry.
Genre: Gatunek gry.
Publisher: Wydawca gry.
NA_Sales: Sprzedaż w Ameryce Północnej (w milionach USD).
EU_Sales: Sprzedaż w Europie (w milionach USD).
JP_Sales: Sprzedaż w Japonii (w milionach USD).
Other_Sales: Sprzedaż w innych regionach (w milionach USD).
Global_Sales: Całkowita sprzedaż globalna (w milionach USD).

Cele analizy:

1. Porównanie sprzedaży gier według rynku, platformy, gatunku i wydawcy.
2. Wizualizacja wyników za pomocą różnych wykresów.
3. Ładowanie Danych, podsumowanie i operacje na danych.


# Ładowanie danych i podsumowanie
gry <- read.csv("vgsales.csv")
summary(gry)
str(gry)

# Obsługa brakujących wartości
gry <- gry[complete.cases(gry),]

# Dodanie kolumny Dekada
gry <- gry %>%
  mutate(Dekada = if_else(Rok >= 2000,
                          paste0(Rok  %/% 10 * 10),
                          paste0(1900 + (Rok - 1900) %/% 10 * 10)))

# Usunięcie danych z obecnej dekady
gry <- gry[!(gry$Dekada=="2020"),]

# Dodanie zakresu lat
gry <- gry %>%
  mutate(Zakres_lat = if_else(Rok >= 2000,
                               "2000-2019",
                               "1980-1999"))
Analizy
1. Sprzedaż według Rynków

# Obliczanie sprzedaży na całym świecie
sprzedaz_na_calym_swiecie <- data.frame(sprzedaz = c(sum(gry$NA_Sales), sum(gry$EU_Sales), sum(gry$JP_Sales), sum(gry$Other_Sales)),
                            procent_calkowitej_sprzedazy = c(sum(gry$NA_Sales)/sum(gry$Global_Sales), sum(gry$EU_Sales)/sum(gry$Global_Sales),
                                                           sum(gry$JP_Sales)/sum(gry$Global_Sales),sum(gry$Other_Sales)/sum(gry$Global_Sales)))
rownames(sprzedaz_na_calym_swiecie) <- c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales")

# Tworzenie wykresu sprzedaży na całym świecie
wykres_sprzedazy_na_calym_swiecie <- ggplot(data = sprzedaz_na_calym_swiecie, aes(x=rownames(sprzedaz_na_calym_swiecie), y=sprzedaz)) + 
  geom_bar(stat = "identity", fill="darkblue") +
  ggtitle("Sprzedaż na całym świecie (w mln USD)") +
  xlab("Rynki") +
  geom_text(aes(label = sprzedaz), vjust = 1.5, colour = "white")
  
2. Sprzedaż według dekad

# Sprzedaż globalna według dekady
sprzedaz_globalna_na_dekade <- gry %>%
  group_by(Dekada) %>%
  summarise(sprzedaz_globalna=sum(Global_Sales))

# Tworzenie wykresu sprzedaży globalnej według dekady
wykres_sprzedazy_globalnej_na_dekade <- ggplot(data = sprzedaz_globalna_na_dekade, aes(x=Dekada, y=sprzedaz_globalna)) + 
  geom_bar(stat = "identity", fill="darkblue") +
  ggtitle("Sprzedaż globalna według dekady (w mln USD)") +
  geom_text(aes(label = sprzedaz_globalna), vjust = 1.5, colour = "white")
  
3. Sprzedaż według Platformy
de
# Sprzedaż globalna według platformy
sprzedaz_globalna_na_platforme <- gry %>%
  group_by(Platforma) %>%
  summarise(liczba=n(), sprzedaz_globalna=sum(Global_Sales), procent_calkowitej_sprzedazy = percent(sum(Global_Sales)/sprzedaz_calosci)) %>%
  arrange(desc(sprzedaz_globalna))

# Usuwanie platform o globalnej sprzedaży < 2 mln USD
sprzedaz_globalna_na_platforme <- sprzedaz_globalna_na_platforme[!(sprzedaz_globalna_na_platforme$sprzedaz_globalna<2),]

# Tworzenie wykresu sprzedaży globalnej według platformy
wykres_sprzedazy_globalnej_na_platforme <- ggplot(data = sprzedaz_globalna_na_platforme, aes(x=reorder(Platforma, -sprzedaz_globalna), y=sprzedaz_globalna)) + 
  geom_bar(stat = "identity", fill='lightblue') +
  ggtitle("Sprzedaż globalna według platformy z procentowym udziałem sprzedaży ogółem") +
  xlab("Rynki") +
  geom_text(aes(label = paste(sprzedaz_globalna,"\n",procent_calkowitej_sprzedazy)), vjust = 1.5, colour = "white")

4. Kategorie gier w zależności od sprzedaży globalnej

# Sprzedaż globalna według kategorii (gatunku) gier
sprzedaz_globalna_wg_gatunku <- gry %>%
  group_by(Gatunek) %>%
  summarise(liczba=n(), sprzedaz_globalna=sum(Global_Sales), procent_calkowitej_sprzedazy = percent(sum(Global_Sales)/sprzedaz_calosci)) %>%
  arrange(desc(sprzedaz_globalna))

# Tworzenie wykresu sprzedaży globalnej według gatunku gier
wykres_sprzedazy_globalnej_wg_gatunku <- ggplot(data = sprzedaz_globalna_wg_gatunku, aes(x=reorder(Gatunek, -sprzedaz_globalna), y=sprzedaz_globalna)) + 
  geom_bar(stat = "identity", fill='lightblue') +
  ggtitle("Sprzedaż globalna według gatunku gier z procentowym udziałem sprzedaży ogółem") +
  xlab("Gatunki gier") +
  geom_text(aes(label = paste(sprzedaz_globalna,"\n",procent_calkowitej_sprzedazy)), vjust = 1.5, colour = "white")

5. Najlepsi wydawcy (pod względem sprzedaży globalnej)

# Sprzedaż globalna według wydawcy
sprzedaz_globalna_wg_wydawcy <- gry %>%
  group_by(Wydawca) %>%
  summarise(liczba=n(), sprzedaz_globalna=sum(Global_Sales), procent_calkowitej_sprzedazy = percent(sum(Global_Sales)/sprzedaz_calosci)) %>%
  arrange(desc(sprzedaz_globalna))

# Usuwanie wydawców o globalnej sprzedaży < 5 mln USD
sprzedaz_globalna_wg_wydawcy <- sprzedaz_globalna_wg_wydawcy[!(sprzedaz_globalna_wg_wydawcy$sprzedaz_globalna<5),]

# Tworzenie wykresu sprzedaży globalnej według wydawcy
wykres_sprzedazy_globalnej_wg_wydawcy <- ggplot(data = sprzedaz_globalna_wg_wydawcy, aes(x=reorder(Wydawca, -sprzedaz_globalna), y=sprzedaz_globalna)) + 
  geom_bar(stat = "identity", fill='lightblue') +
  ggtitle("Sprzedaż globalna według wydawcy z procentowym udziałem sprzedaży ogółem") +
  xlab("Wydawcy") +
  geom_text(aes(label = paste(sprzedaz_globalna,"\n",procent_calkowitej_sprzedazy)), vjust = 1.5, colour = "white")

6. Korelacja między zmiennymi (sprzedaż w różnych regionach)

# Macierz korelacji
macierz_korelacji <- gry %>%
  select(NA_Sales, EU_Sales, JP_Sales, Other_Sales, Global_Sales) %>%
  cor()

# Tworzenie wykresu macierzy korelacji
wykres_macierzy_korelacji <- ggcorrplot(macierz_korelacji, hc.order = TRUE, type = "lower", lab = TRUE)
Wnioski
Oceniając wyniki analizy, można wyciągnąć różne wnioski dotyczące trendów sprzedaży gier wideo. Wykresy i statystyki pozwalają zrozumieć, jak różne czynniki wpływają na sprzedaż, takie jak regiony, platformy, gatunki i wydawcy. Przeanalizowanie korelacji między sprzedażą w różnych regionach również pomaga zidentyfikować, czy sukces w jednym regionie przekłada się na sukces w innych obszarach.
