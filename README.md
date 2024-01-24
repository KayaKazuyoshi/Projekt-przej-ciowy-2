# Projekt przejściowy 2

Instrukcja dla analizy sprzedaży gier wideo

Użyte biblioteki R

ggplot2

dplyr

forcats uporządkowanie wartości w wykresach

tidyr

ggpubr


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


Ładowanie danych i podsumowanie
gry <- read.csv("vgsales.csv")
summary(gry)
str(gry)

Obsługa brakujących wartości
gry <- gry[complete.cases(gry),]

Dodanie kolumny Dekada
gry <- gry %>%
  mutate(Dekada = if_else(Rok >= 2000,
                          paste0(Rok  %/% 10 * 10),
                          paste0(1900 + (Rok - 1900) %/% 10 * 10)))

Usunięcie danych z obecnej dekady
gry <- gry[!(gry$Dekada=="2020"),]

Dodanie zakresu lat
gry <- gry %>%
  mutate(Zakres_lat = if_else(Rok >= 2000,
                               "2000-2019",
                               "1980-1999"))
Analizy

1. Sprzedaż według rynków
 Obliczanie sprzedaży na całym świecie
 Tworzenie wykresu sprzedaży na całym świecie
2. Sprzedaż według dekad
 Sprzedaż globalna według dekady
 Tworzenie wykresu sprzedaży globalnej według dekady
3. Sprzedaż według Platformy
 Sprzedaż globalna według platformy
 Usuwanie platform o globalnej sprzedaży < 2 mln USD
 Tworzenie wykresu sprzedaży globalnej według platformy
4. Kategorie gier w zależności od sprzedaży globalnej
 Sprzedaż globalna według kategorii (gatunku) gier
 Tworzenie wykresu sprzedaży globalnej według gatunku gier
5. Najlepsi wydawcy (pod względem sprzedaży globalnej)
 Sprzedaż globalna według wydawcy
 Usuwanie wydawców o globalnej sprzedaży < 5 mln USD
 Tworzenie wykresu sprzedaży globalnej według wydawcy
6. Korelacja między zmiennymi (sprzedaż w różnych regionach)
 Macierz korelacji
 Tworzenie wykresu macierzy korelacji

Wnioski
Oceniając wyniki analizy, można wyciągnąć różne wnioski dotyczące trendów sprzedaży gier wideo. Wykresy i statystyki pozwalają zrozumieć, jak różne czynniki wpływają na sprzedaż, takie jak regiony, platformy, gatunki i wydawcy. Przeanalizowanie korelacji między sprzedażą w różnych regionach również pomaga zidentyfikować, czy sukces w jednym regionie przekłada się na sukces w innych obszarach.
