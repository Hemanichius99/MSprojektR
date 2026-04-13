# miary.R

f_sr = function(x)
{
  bb = seq(min(x), max(x), length.out = 8)  # wektor wartośći od min do max równoodległych od siebie i podzielonych na 8 części
  hh = hist(x, bb, xlim = c(10, 35), labels = TRUE, freq = FALSE, plot = FALSE)
}










#funkcja zwracająca szereg rozdzielczy
wart_sr = function (wektor) {
  sr_wektor = f_sr(wektor)
  
  
  # wartość średnia szereg rozdzielczy
  xsr_sr1 = sum(sr_wektor$mids * sr_wektor$counts) / sum(sr_wektor$counts);
  
  
  # wariancja nieobciążona s2*szereg rozdzielczy
  var_sr1 = sum(sr_wektor$counts * (sr_wektor$mids - xsr_sr1)^2) / (sum(sr_wektor$counts) - 1)
  
  # wariancja s2 szereg rozdzielczy
  var_sr1_n = sum((sr_wektor$mids - xsr_sr1)^2 * sr_wektor$counts) /
    sum(sr_wektor$counts)
  
  # odchylenie standardowe s*szereg rozdzielczy
  os_sr1_r = sqrt(var_sr1)
  
  # odchylenie standardowe s szereg rozdzielczy
  os_sr1_r_n = sqrt(var_sr1_n)
  
  
  
  
  
  # kwartyle szereg rozdzielczy
  kwartyl_sr <- function(sr, p) {
    n <- sum(sr$counts)
    cum <- cumsum(sr$counts)
    
    i <- which(cum >= p * n)[1]
    
    x0 <- sr$breaks[i]
    ni <- sr$counts[i]
    h <- sr$breaks[i+1] - sr$breaks[i]
    cum_prev <- ifelse(i == 1, 0, cum[i-1])
    
    q <- x0 + ((p*n - cum_prev) / ni) * h
    return(q)
  }
  
  # kwartle
  q1_sr1 = kwartyl_sr(sr_wektor, 0.25)
  m_sr1  = kwartyl_sr(sr_wektor, 0.5)
  q3_sr1 = kwartyl_sr(sr_wektor, 0.75)
  
  
  # moda szereg rozdzielczy
  moda_sr <- function(sr) {
    i <- which.max(sr$counts)
    
    ni <- sr$counts[i]
    ni_1 <- ifelse(i == 1, 0, sr$counts[i-1])
    ni1 <- ifelse(i == length(sr$counts), 0, sr$counts[i+1])
    
    x0 <- sr$breaks[i]
    h <- sr$breaks[i+1] - sr$breaks[i]
    
    mo <- x0 + ((ni - ni_1) / ((ni - ni_1) + (ni - ni1))) * h
    return(mo)
  }
  
  moda_sr1 = moda_sr(sr_wektor)
  
  # skośność szereg rozdzielczy
  sk_sr1 = sum((sr_wektor$mids - xsr_sr1)^3) / (length(wektor) * os_sr1_r^3)
  
  
  # kurtoza szereg rozdzielczy
  kur_sr1 = sum((sr_wektor$mids - xsr_sr1)^4) / (length(wektor) * os_sr1_r^4)
  
  
  
  # odchylenie przeciętne od średniej szereg rozdzielczy
  d1_sr1 = sum(abs(sr_wektor$mids - xsr_sr1) * sr_wektor$counts) / sum(sr_wektor$counts)
  
  # odchylenie przeciętne od mediany szereg rozdzielczy
  d2_sr1 = sum(abs(sr_wektor$mids - m_sr1) * sr_wektor$counts) / sum(sr_wektor$counts)
  
  
  # odchylenie ćwiartkowe szereg rozdzielczy
  Q_sr1 = (q3_sr1 - q1_sr1) / 2
  
  # współczynnik zmienności szereg rozdzielczy
  v_sr1 = (os_sr1_r / xsr_sr1) * 100
  
  # pozycyjny współczynnik zmienności szereg rozdzielczy
  vq_sr1 = ((q3_sr1 - q1_sr1) / (2 * m_sr1)) * 100
  
  #srednia, mediana, moda, q1, q3, wariancja*, odch stan *,wariancja, odch stand, odch przec, odch przec od mediany,odch ćwiart, wsp zmienności, poz wsp zmiennosci, skosnosc, kurtoza, eksces                                    
  wynik_sr = c(
    xsr_sr1, m_sr1, moda_sr1, q1_sr1, q3_sr1, var_sr1,  os_sr1_r, var_sr1_n, os_sr1_r_n,d1_sr1, d2_sr1, Q_sr1, v_sr1, vq_sr1, sk_sr1, kur_sr1, 0
  )
  
  return (wynik_sr)
}
#koniec szeregu rozdzielczego











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
  
  
  #łączenie podpunktów a i b
  szer_rozdz = wart_sr(wektor)
  
  tabela_wynikow = data.frame(
    Miara = names(wyniki),
    wart_szer_szczeg = as.numeric(round(wyniki, 4)),
    wart_szer_rozdziel = as.numeric(round(szer_rozdz, 4))
  )
  
  
  return(tabela_wynikow)
  
}