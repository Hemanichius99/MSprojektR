setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

lubuskie = read.csv("lubuskie.csv", header = FALSE)
wektor_lubuskie = unlist(lubuskie, use.names = FALSE)

print(wektor_lubuskie)