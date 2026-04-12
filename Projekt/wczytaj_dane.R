setwd("D:/POLITECHNIKA/SEMESTR 4/STATYSTYKA/P/PROJEKT W R/projekt/Projekt")
dane = read.csv('dane_wej.csv', header = TRUE, row.names = 1)  
# funkcja read.csv czyta plik csv gdzie separatorem jest przecinek a cześć ułamkowa zaczyna się kropką
# header = TRUE oznacza, że plik zawiera wiersz nagłówkowy, będą to nazwy kolumn w strukturze data.frame
# data.frame to lista w R będą w niej zapisane 3 wektory: lp, zawartosc_cukru, województwo
# row.names = 1 oznacza, że pierwsza kolumna pliku csv zostanie użyta jako nazwy wierszy (indeksy), a nie jako zwykła kolumna danych.                                  
dane
dane$zawartosc_cukru  # wyświetla kolumne (wektor) o nazwie zawartosc_cukru

calosc = dane$zawartosc_cukru
lubuskie = dane$zawartosc_cukru[dane$wojewodztwo == "lubuskie"]
wielkopolskie = dane$zawartosc_cukru[dane$wojewodztwo == "wielkopolskie"]
