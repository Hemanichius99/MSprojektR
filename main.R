setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("wykresy.R")

lubuskie = read.csv("lubuskie.csv", header = FALSE)
wektor_lubuskie = unlist(lubuskie, use.names = FALSE)

wielkopolskie = read.csv("wielkopolskie.csv", header = FALSE)
wektor_wielkopolskie = unlist(wielkopolskie, use.names = FALSE)

# zadanie 1
bbpp(wektor_wielkopolskie, nazwa = "Wielkopolskie")
bbpp(wektor_lubuskie, nazwa = "lubuskie")