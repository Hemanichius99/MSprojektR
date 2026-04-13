# wykresy.R

bbpp = function(x, tytul)
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

phg = function(wektor, name, breaks = "Sturges") {
  hist(wektor,
       main = name,
       xlab = "Zawartość cukru [%]",
       breaks = breaks,
       xaxt = "n")
  
  if (is.numeric(breaks)) {
    srodki = (breaks[-length(breaks)] + breaks[-1]) / 2
    axis(1, at = srodki, labels = round(srodki, 2))
  } else {
    axis(1)  # domyślna oś X dla Sturges
  }
}