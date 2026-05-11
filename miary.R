#miary.R

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

oblicz_miary = function(wektor,tytul = "MIARY STATYSTYCZNE") {
  n = length(wektor)
  zakres_min = min(wektor)
  zakres_max = max(wektor)
  
  # wartość średnia szereg szczegółowy
  sr = mean(wektor)
  
  # mediana szereg szczegółowy
  med = median(wektor)
  
  # moda szereg szczegółowy
  ux = unique(wektor)
  moda = ux[which.max(tabulate(match(wektor, ux)))]
  
  # kwartyle szereg szczegółowy (Q1 i Q3)
  q_wynik = quantile(wektor, probs = c(0.25, 0.75), type = 6)
  q1 = q_wynik[1]
  q3 = q_wynik[2]
  
  # wariancja nieobciążona s2* szereg szczegółowy
  war_proba = var(wektor)
  
  # odchylenie standardowe nieobciążone s* szereg szczegółowy
  sd_proba = sd(wektor)
  
  # wariancja s2 szereg szczegółowy
  war_populacja = sum((wektor - sr)^2) / n
  
  # odchylenie standardowe s szereg szczegółowy
  sd_populacja = sqrt(war_populacja)
  
  # odchylenie przeciętne od średniej d1 szereg szczegółowy
  d1 = sum(abs(wektor - sr)) / n
  
  # odchylenie przeciętne od mediany d2 szereg szczegółowy
  d2 = sum(abs(wektor - med)) / n
  
  odchylenie = sd_populacja
  
  # odchylenie ćwiartkowe szereg szczegółowy
  q = (q3 - q1) / 2
  
  # współczynnik zmienności szereg szczegółowy
  v = (odchylenie / sr) * 100
  
  # pozycyjny współczynnik zmienności szereg szczegółowy
  vq = (q / med) * 100
  
  # momenty centralne (trzeci i czwarty)
  m3 = sum((wektor - sr)^3) / n
  m4 = sum((wektor - sr)^4) / n
  
  # skośność szereg szczegółowy
  as = m3 / (odchylenie^3)
  
  # kurtoza szereg szczegółowy
  krt = m4 / (odchylenie^4)
  
  # eksces szereg szczegółowy
  g2 = krt - 3
  
  # Pobranie wyników dla szeregu rozdzielczego
  szer_rozdz = wart_sr(wektor)
  
  # Przygotowanie danych do wyświetlania (zaokrąglenia zgodne z Twoim wzorem)
  w = list()
  # Położenie
  w["wsss"] = round(sr, 6); w["wssr"] = round(szer_rozdz[1], 6)
  w["mess"] = round(med, 6); w["mesr"] = round(szer_rozdz[2], 6)
  w["moss"] = round(moda, 6); w["mosr"] = round(szer_rozdz[3], 6)
  w["q1ss"] = round(q1, 6); w["q1sr"] = round(szer_rozdz[4], 6)
  w["q3ss"] = round(q3, 6); w["q3sr"] = round(szer_rozdz[5], 6)
  # Rozproszenie
  w["wnss"] = round(war_proba, 6); w["wnsr"] = round(szer_rozdz[6], 6)
  w["osnss"] = round(sd_proba, 6); w["osnsr"] = round(szer_rozdz[7], 6)
  w["wss"] = round(war_populacja, 6); w["wsr"] = round(szer_rozdz[8], 6)
  w["osss"] = round(sd_populacja, 6); w["ossr"] = round(szer_rozdz[9], 6)
  w["oposss"] = round(d1, 6); w["opossr"] = round(szer_rozdz[10], 6)
  w["opomess"] = round(d2, 6); w["opomesr"] = round(szer_rozdz[11], 6)
  w["oćss"] = round(q, 6); w["oćsr"] = round(szer_rozdz[12], 6)
  w["wzss"] = round(v, 6); w["wzsr"] = round(szer_rozdz[13], 6)
  w["pwzss"] = round(vq, 6); w["pwzsr"] = round(szer_rozdz[14], 6)
  # Asymetria
  w["sss"] = round(as, 6); w["ssr"] = round(szer_rozdz[15], 6)
  w["kss"] = round(krt, 6); w["ksr"] = round(szer_rozdz[16], 6)
  w["ess"] = round(g2, 6); w["esr"] = round(szer_rozdz[17], 6)
  
  # --- WYŚWIETLANIE (Zgodne z Twoim wzorem formatowania) ---
  cat("\n\n")
  cat("=== MIARY:", tytul, "===\n")
  cat(format("Liczba obserwacji =", justify = 'right', width = 44), sep = '', format(n, width = 17, justify = 'right'), '\n')
  cat(format("Zakres =", justify = 'right', width = 44), sep = '', format('<', width = 5, justify = 'right'), format(zakres_min, width = 1, justify = 'right'), format(', ', width = 1, justify = 'right'), format(zakres_max, width = 1, justify = 'right'), format('>', width = 1, justify = 'right'),'\n\n')
  
  cat(format("Nazwa statystyki", justify = 'right', width = 44), sep = '', format("Sz. szcz.", width = 17, justify = 'right'), format("Sz. roz.", width = 12, justify = 'right'), '')
  
  cat("\n\nMiary położenia:\n")
  cat("==========================================================================\n")
  cat(format("Wartość średnia", justify = 'right', width = 44), sep = '', format(w["wsss"], width = 17, justify = 'right'), format(w["wssr"], width = 12, justify = 'right'), '\n',
      format("Mediana", justify = 'right', width = 44), format(w["mess"], width = 17, justify = 'right'), format(w["mesr"], width = 12, justify = 'right'), '\n',
      format("Moda", justify = 'right', width = 44), format(w["moss"], width = 17, justify = 'right'), format(w["mosr"], width = 12, justify = 'right'), '\n',
      format("Q1", justify = 'right', width = 44), format(w["q1ss"], width = 17, justify = 'right'), format(w["q1sr"], width = 12, justify = 'right'), '\n',
      format("Q3", justify = 'right', width = 44), format(w["q3ss"], width = 17, justify = 'right'), format(w["q3sr"], width = 12, justify = 'right'), '\n')
  
  cat("\n\nMiary rozproszenia:\n")
  cat("==========================================================================\n")
  cat(format("Wariancja nieobciążona S*2", justify = 'right', width = 44), sep = '', format(w["wnss"], width = 17, justify = 'right'), format(w["wnsr"], width = 12, justify = 'right'), '\n',
      format("Odchylenie standardowe nieobciążone S*", justify = 'right', width = 44), format(w["osnss"], width = 17, justify = 'right'), format(w["osnsr"], width = 12, justify = 'right'), '\n',
      format("Wariancja S2", justify = 'right', width = 44), format(w["wss"], width = 17, justify = 'right'), format(w["wsr"], width = 12, justify = 'right'), '\n',
      format("Odchylenie standardowe S", justify = 'right', width = 44), format(w["osss"], width = 17, justify = 'right'), format(w["ossr"], width = 12, justify = 'right'), '\n',
      format("Odchylenie przeciętne d1", justify = 'right', width = 44), format(w["oposss"], width = 17, justify = 'right'), format(w["opossr"], width = 12, justify = 'right'), '\n',
      format("Odchylenie przeciętne od mediany d2", justify = 'right', width = 44), format(w["opomess"], width = 17, justify = 'right'), format(w["opomesr"], width = 12, justify = 'right'), '\n',
      format("Odchylenie ćwiartkowe", justify = 'right', width = 44), format(w["oćss"], width = 17, justify = 'right'), format(w["oćsr"], width = 12, justify = 'right'), '\n',
      format("Współczynnik zmienności w % v", justify = 'right', width = 44), format(w["wzss"], width = 17, justify = 'right'), format(w["wzsr"], width = 12, justify = 'right'), '\n',
      format("Pozycyjny współczynnik zmienności w % vq", justify = 'right', width = 44), format(w["pwzss"], width = 17, justify = 'right'), format(w["pwzsr"], width = 12, justify = 'right'), '\n')
  
  cat("\n\nMiary asymetrii i koncentracji:\n")
  cat("==========================================================================\n")
  cat(format("Skośność as", justify = 'right', width = 44), sep = '', format(w["sss"], width = 17, justify = 'right'), format(w["ssr"], width = 12, justify = 'right'), '\n',
      format("Kurtoza krt", justify = 'right', width = 44), format(w["kss"], width = 17, justify = 'right'), format(w["ksr"], width = 12, justify = 'right'), '\n',
      format("Eksces g2", justify = 'right', width = 44), format(w["ess"], width = 17, justify = 'right'), format(w["esr"], width = 12, justify = 'right'), '\n')
  
  invisible(NULL)
}