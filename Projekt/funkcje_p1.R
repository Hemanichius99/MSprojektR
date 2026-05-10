# PODPUNKT 1.
setwd("D:/POLITECHNIKA/SEMESTR 4/STATYSTYKA/P/PROJEKT W R/projekt/Projekt")
# source('wczytaj_dane.R', echo = TRUE)
source('wczytaj_dane.R')

bp = function(x, tytul)
{
  # min() podaje wartość minimalną danych
  # max() podaje wartość maksymalną danych
  # mean() liczy średnią
  # sd() liczy odchylenie standardowe
  # 2 element - dolna granica „pudełka”, 4 element - górna granica „pudełka”
  xx = c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
  
  ll = list(x, xx)
  
  boxplot(ll,
          main = tytul,
          names = c('Mediana', 'Wartość oczekiwana'),
          ylim = c(10,35)
         )
}