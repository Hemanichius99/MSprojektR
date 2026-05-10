setwd("D:/POLITECHNIKA/SEMESTR 4/STATYSTYKA/P/PROJEKT W R/projekt/Projekt")
dane = read.csv('dane_wej.csv', header = TRUE, row.names = 1)  
# Funkcja read.csv czyta plik csv gdzie separatorem jest przecinek a cześć ułamkowa zaczyna się kropką.
# header = TRUE oznacza, że plik zawiera wiersz nagłówkowy, będą to nazwy kolumn w strukturze data.frame.
# data.frame to lista w R będą w niej zapisane 3 wektory: lp, zawartosc_cukru, województwo.
# row.names = 1 oznacza, że pierwsza kolumna pliku csv zostanie użyta jako nazwy wierszy (indeksy), a nie jako kolumna danych                                  
  
calosc = dane$zawartosc_cukru                                              # Kolumna (wektor) zawierający zawartości cukru w burakach dla obu województw o nazwie calosc
lubuskie = dane$zawartosc_cukru[dane$wojewodztwo == "lubuskie"]            # Kolumna (wektor) zawierający zawartości cukru w burakach dla województwa lubuskiego o nazwie lubuskie
wielkopolskie = dane$zawartosc_cukru[dane$wojewodztwo == "wielkopolskie"]  # Kolumna (wektor) zawierający zawartości cukru w burakach dla województwa wielkopolskiego o nazwie wielkopolskie