# PODPUNKT 1.
setwd("D:/POLITECHNIKA/SEMESTR 4/STATYSTYKA/P/PROJEKT W R/projekt/Projekt")
bp = function(x, tytul)
{
  # mean() liczy średnią
  # sd() liczy odchylenie standardowe
  # 2 element - pierwszy kwartyl, 4 element - trzeci kwartyl
  xx = c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
  
  ll = list(x, xx)
  
  boxplot(ll,
          main = tytul,
          names = c('mediana', 'wartość oczekiwana')
        )
}
# bp(wielkopolskie, 'wielkopolskie')