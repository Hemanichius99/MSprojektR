setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("wykresy.R")
source("miary.R")

lubuskie = read.csv("lubuskie.csv", header = FALSE)
wektor_lubuskie = unlist(lubuskie, use.names = FALSE)

wielkopolskie = read.csv("wielkopolskie.csv", header = FALSE)
wektor_wielkopolskie = unlist(wielkopolskie, use.names = FALSE)

# zadanie 1
bbpp(wektor_wielkopolskie, "Wielkopolskie")
bbpp(wektor_lubuskie, "lubuskie")

#zadanie 2A
#a

#obliczone wartości LUBUSKIE
cat("\n=== MIARY: WOJEWÓDZTWO LUBUSKIE ===\n")
miary_L = oblicz_miary(wektor_lubuskie)
print(miary_L, row.names = FALSE)

#obliczone wartości WIELKOPOLSKIE
cat("\n=== MIARY: WOJEWÓDZTWO WIELKOPOLSKIE ===\n")
miary_W = oblicz_miary(wektor_wielkopolskie)
print(miary_W, row.names = FALSE)

#b 

#obliczone wartości LUBUSKIE

#TODO

#obliczone wartości WIELKOPOLSKIE

#TODO

#WYKRESY
lubuskie_rozdzielczy = seq(min(wektor_lubuskie), max(wektor_lubuskie), length.out = 8)
phg(wektor_lubuskie, "lubuskie rozdzielczy", lubuskie_rozdzielczy)

wielkopolskie_rozdzielczy = seq(min(wektor_wielkopolskie), max(wektor_wielkopolskie), length.out = 8)
phg(wektor_wielkopolskie, "wielkopolskie rozdzielczy", wielkopolskie_rozdzielczy)
