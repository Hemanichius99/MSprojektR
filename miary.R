# miary.R

f_sr = function(x)
{
  bb = seq(min(x), max(x), length.out = 8)
  hh = hist(x, breaks = bb, plot = FALSE)
  return(hh)
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
  sk_sr1 = sum(((sr_wektor$mids - xsr_sr1)^3) * sr_wektor$counts) / (sum(sr_wektor$counts) * os_sr1_r_n^3)
  
  # kurtoza szereg rozdzielczy
  kur_sr1 = sum(((sr_wektor$mids - xsr_sr1)^4) * sr_wektor$counts) / (sum(sr_wektor$counts) * os_sr1_r_n^4)
  
  # eksces szereg rozdzielczy
  ex_sr1 = kur_sr1 - 3
  
  # odchylenie przeciętne od średniej szereg rozdzielczy
  d1_sr1 = sum(abs(sr_wektor$mids - xsr_sr1) * sr_wektor$counts) / sum(sr_wektor$counts)
  
  # odchylenie przeciętne od mediany szereg rozdzielczy
  d2_sr1 = sum(abs(sr_wektor$mids - m_sr1) * sr_wektor$counts) / sum(sr_wektor$counts)
  
  # odchylenie ćwiartkowe szereg rozdzielczy
  Q_sr1 = (q3_sr1 - q1_sr1) / 2
  
  # współczynnik zmienności szereg rozdzielczy
  v_sr1 = (os_sr1_r_n / xsr_sr1) * 100
  
  # pozycyjny współczynnik zmienności szereg rozdzielczy
  vq_sr1 = (Q_sr1 / m_sr1) * 100
  
  wynik_sr = c(
    xsr_sr1, m_sr1, moda_sr1, q1_sr1, q3_sr1, var_sr1,  os_sr1_r, var_sr1_n, os_sr1_r_n, d1_sr1, d2_sr1, Q_sr1, v_sr1, vq_sr1, sk_sr1, kur_sr1, ex_sr1
  )
  
  return (wynik_sr)
}

oblicz_miary = function(wektor) {
  n = length(wektor)
  
  #MIARY POŁOŻENIA
  sr = mean(wektor)
  med = median(wektor)
  
  ux = unique(wektor)
  moda = ux[which.max(tabulate(match(wektor, ux)))]
  
  q_wynik = quantile(wektor, probs = c(0.25, 0.75), type = 6)
  q1 = q_wynik[1]
  q3 = q_wynik[2]
  
  #MIARY ROZPROSZENIA
  war_proba = var(wektor)
  sd_proba = sd(wektor)
  
  war_populacja = sum((wektor - sr)^2) / n
  sd_populacja = sqrt(war_populacja)
  
  d1 = sum(abs(wektor - sr)) / n
  d2 = sum(abs(wektor - med)) / n
  
  odchylenie = sd_populacja
  q = (q3 - q1) / 2
  v = (odchylenie / sr) * 100
  vq = (q / med) * 100
  
  #MIARY ASYMETRII I KONCENTRACJI
  m3 = sum((wektor - sr)^3) / n
  m4 = sum((wektor - sr)^4) / n
  
  as = m3 / (odchylenie^3)
  krt = m4 / (odchylenie^4)
  g2 = krt - 3
  
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
  
  szer_rozdz = wart_sr(wektor)
  
  tabela_wynikow = data.frame(
    Miara = names(wyniki),
    wart_szer_szczeg = as.numeric(round(wyniki, 4)),
    wart_szer_rozdziel = as.numeric(round(szer_rozdz, 4))
  )
  
  return(tabela_wynikow)
}