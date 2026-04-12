setwd("D:/POLITECHNIKA/SEMESTR 4/STATYSTYKA/P/PROJEKT W R/projekt/Projekt")
library(nortest)
library(moments)

# WCZYTANIE DANYCH
# source('wczytaj_dane.R', echo = TRUE)
source('wczytaj_dane.R')

#--------------------------
#PODPUNKT 1.
#source('funkcje_p1.R', echo = TRUE)
source('funkcje_p1.R')

bp(calosc, 'calosc')
bp(lubuskie, 'lubuskie')
bp(wielkopolskie, 'wielkopolskie')

bbpp = bp(lubuskie, 'lubuskie')
bbpp$stats

#PODPUNKT 2.
#source('funkcje_p2.R', echo = TRUE)
source('funkcje_p2.R')