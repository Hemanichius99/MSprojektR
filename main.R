setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("wykresy.R")

lubuskie = read.csv("lubuskie.csv", header = FALSE)
wektor_lubuskie = unlist(lubuskie, use.names = FALSE)

wielkopolskie = read.csv("wielkopolskie.csv", header = FALSE)
wektor_wielkopolskie = unlist(wielkopolskie, use.names = FALSE)

# zadanie 1
bbpp(wektor_wielkopolskie, nazwa = "Wielkopolskie")
bbpp(wektor_lubuskie, nazwa = "lubuskie")

#zadanie 2
lubuskie_rozdzielczy = seq(min(wektor_lubuskie), max(wektor_lubuskie), length.out = 8)
phg(wektor_lubuskie, "lubuskie rozdzielczy", lubuskie_rozdzielczy)

wielkopolskie_rozdzielczy = seq(min(wektor_wielkopolskie), max(wektor_wielkopolskie), length.out = 8)
phg(wektor_wielkopolskie, "wielkopolskie rozdzielczy", wielkopolskie_rozdzielczy)
