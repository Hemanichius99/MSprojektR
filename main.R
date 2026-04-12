setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("wykresy.R")

lubuskie = read.csv("lubuskie.csv", header = FALSE)
wektor_lubuskie = unlist(lubuskie, use.names = FALSE)

rysuj_wykresy(wektor_lubuskie, nazwa = "lubuskie")
