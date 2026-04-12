# wykresy.R
bbpp = function(wektor, nazwa = "dane") {
  
  par(mfrow = c(1, 2))
  
  bp = boxplot(wektor,
                main = "Kwartyle",
                ylab = "Zawartość cukru [%]")
  
  m = mean(wektor)
  s = sd(wektor)
  
  stats = list(
    stats = matrix(c(m - 2*s, m - s, m, m + s, m + 2*s), nrow = 5),
    n = length(wektor),
    out = numeric(0),
    group = factor(1),
    names = nazwa
  )
  
  bxp(stats,
      main = "Średnia i odchylenie standardowe",
      ylab = "Zawartość cukru [%]",
      boxfill = "grey80",
      ylim = bp$stats[c(1,5),])
  
  par(mfrow = c(1, 1))
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