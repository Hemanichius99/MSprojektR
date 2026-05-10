setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("wykresy.R")
source("miary.R")
source("zad5.R")
source("hipotezy.R")
source("zad4.R")
source("zad3.R")

lubuskie = read.csv("lubuskie.csv", header = FALSE)
wektor_lubuskie = unlist(lubuskie, use.names = FALSE)

wielkopolskie = read.csv("wielkopolskie.csv", header = FALSE)
wektor_wielkopolskie = unlist(wielkopolskie, use.names = FALSE)

# zadanie 1
bbpp(wektor_wielkopolskie, "Wielkopolskie")
bbpp(wektor_lubuskie, "lubuskie")

#zadanie 2
#a i b w jednej tabeli

#obliczone wartości LUBUSKIE
cat("\n=== MIARY: WOJEWÓDZTWO LUBUSKIE ===\n")
miary_L = oblicz_miary(wektor_lubuskie)
print(miary_L, row.names = FALSE)

#obliczone wartości WIELKOPOLSKIE
cat("\n=== MIARY: WOJEWÓDZTWO WIELKOPOLSKIE ===\n")
miary_W = oblicz_miary(wektor_wielkopolskie)
print(miary_W, row.names = FALSE)

#WYKRESY
lubuskie_rozdzielczy = seq(min(wektor_lubuskie), max(wektor_lubuskie), length.out = 8)
phg(wektor_lubuskie, "lubuskie rozdzielczy", lubuskie_rozdzielczy)

wielkopolskie_rozdzielczy = seq(min(wektor_wielkopolskie), max(wektor_wielkopolskie), length.out = 8)
phg(wektor_wielkopolskie, "wielkopolskie rozdzielczy", wielkopolskie_rozdzielczy)

#zadanie 3

# Analiza dla Województwa Lubuskiego
wynik3_lubuskie <- test_lillieforsa(wektor_lubuskie, "Województwo Lubuskie")
drukuj_test_normalnosci(wynik3_lubuskie)

# Analiza dla Województwa Wielkopolskiego
wynik3_wielkopolskie <- test_lillieforsa(wektor_wielkopolskie, "Województwo Wielkopolskie")
drukuj_test_normalnosci(wynik3_wielkopolskie)

#zad4
wynik_zad4 <- przedzialowo_cukier_region(lubuskie)
wypisz_z4(wynik_zad4, "Wojewodztwo lubuskie")

#zadanie 5
wynik <- analiza_odchylenia_region(wektor_wielkopolskie)
drukuj_wyniki(wynik, "Województwo Wielkopolskie")

#zadanie 6
alpha = 0.05
mhipo(wektor_lubuskie, wektor_wielkopolskie, alpha)


#zadanie 7
ptt(wektor_lubuskie, wektor_wielkopolskie, alpha, hipoteza = "dwustronny", R = 10000)