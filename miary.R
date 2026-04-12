# miary.R

oblicz_miary = function(wektor) {
  n = length(wektor)
  
  #MIARY POŁOŻENIA
  sr = mean(wektor) #średnia
  med = median(wektor) #mediana
  
  ux = unique(wektor)
  moda = ux[which.max(tabulate(match(wektor, ux)))] #moda
  
  # Podobno type = 6 jest typowym używanym w statystyce i jest dokładniejszy niż basic (7) z excela
  q_wynik = quantile(wektor, probs = c(0.25, 0.75), type = 6)
  q1 = q_wynik[1] #Q1
  q3 = q_wynik[2] #Q3

  #MIARY ROZPROSZENIA
  
  # Wariancja i odchylenie Z PRÓBY (S*^2, S*) 
  war_proba = var(wektor) # S*^2 
  sd_proba = sd(wektor)   # S*
  
  # Wariancja i odchylenie Z POPULACJI (S^2, S) - bez gwiazdki
  war_populacja = sum((wektor - sr)^2) / n # S^2 
  sd_populacja = sqrt(war_populacja)       # S
  d1 = sum(abs(wektor - sr)) / n # odchylenie przeciętne od średniej
  d2 = sum(abs(wektor - med)) / n # odchylenie przeciętne od mediany
  odchylenie = sd_populacja #JAK COŚ JEST ŹLE LUB ZŁY TYP ODCHYLENIA DO OBLICZEŃ TO TU ZMIENIĆ między "sd_proba" a "sd_populacja"
  q = (q3 - q1) / 2 # odchylenie ćwiartkowe
  v = (odchylenie / sr) * 100 # współczynnik zmienności [%]
  vq = (q / med) * 100 # pozycyjny współczynnik zmienności [%]
  
  #MIARY ASYMETRII I KONCENTRACJI
  
  m3 = sum((wektor - sr)^3) / n
  m4 = sum((wektor - sr)^4) / n
  
  as = m3 / (odchylenie^3) # skośność
  krt = m4 / (odchylenie^4) # kurtoza
  g2 = krt - 3 # eksces
  
  #ZAPIS + OPIS WYNIKÓW
  wyniki = c(
    "Średnia:" = sr,
    "Mediana" = med,
    "Moda" = moda,
    "Q1" = q1,
    "Q3" = q3,
    "Wariancja S*^2" = war_proba,
    "Odchylenie standardowe S*" = sd_proba,
    "Wariancja S^2" = war_populacja,
    "Odchylenie standardowe S" = sd_populacja,
    "Odchylenie przeciętne d1" = d1,
    "Odchylenie przeciętne od mediany d2" = d2,
    "Odchylenie ćwiartkowe Q" = q,
    "Współczynnik zmienności v [%]" = v,
    "Pozycyjny współczynnik zmienności vq [%]" = vq,
    "Skośność as" = as,
    "Kurtoza krt" = krt,
    "Eksces g2" = g2
  )
  
  tabela_wynikow = data.frame(
    Miara = names(wyniki),
    Wartosc = as.numeric(round(wyniki, 4))
  )
  
  
  return(tabela_wynikow)
  
}
  