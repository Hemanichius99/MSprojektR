mhipo = function(w1, w2, alpha, hipoteza = "dwustronny") {
  
  # hipoteza: "dwustronny"   -> H1: mu1 != mu2
  #           "prawostronny" -> H1: mu1 >  mu2
  #           "lewostronny"  -> H1: mu1 <  mu2
  
  # DANE PODSTAWOWE
  n1 = length(w1)
  n2 = length(w2)
  
  sr1 = sum(w1) / n1
  sr2 = sum(w2) / n2
  
  # WARIANCJE PRÓBKOWE
  s1 = sum((w1 - sr1)^2) / (n1 - 1)
  s2 = sum((w2 - sr2)^2) / (n2 - 1)
  
  # TEST F (równość wariancji)
  F_stat = max(s1, s2) / min(s1, s2)
  
  if (s1 >= s2) {
    df_F1 = n1 - 1
    df_F2 = n2 - 1
  } else {
    df_F1 = n2 - 1
    df_F2 = n1 - 1
  }
  
  F_kr = qf(1 - alpha, df_F1, df_F2)
  rowne_wariancje = (F_stat <= F_kr)
  
  # STATYSTYKA T
  if (rowne_wariancje) {
    sp2    = ((n1 - 1) * s1 + (n2 - 1) * s2) / (n1 + n2 - 2)
    t_stat = (sr1 - sr2) / sqrt(sp2 * (1/n1 + 1/n2))
    df_t   = n1 + n2 - 2
    metoda = "pooled t-Student (Model 2)"
    
  } else {
    se     = sqrt(s1 / (n1 - 1) + s2 / (n2 - 1))
    t_stat = (sr1 - sr2) / se
    df_t   = NA  # Cochran-Cox nie używa df_t, kwantyl obliczany później
    metoda = "Cochran-Cox (Model 3)"
  }
  
  # WARTOŚĆ KRYTYCZNA i DECYZJA zależne od wyboru hipotezy
  hipoteza = tolower(hipoteza)
  
  # Pomocnicza funkcja kwantyla Cochrana-Coxa
  kwantyl_CC = function(p) {
    t1 = qt(p, n1 - 1)
    t2 = qt(p, n2 - 1)
    w1_cc = s1 / (n1 - 1)
    w2_cc = s2 / (n2 - 1)
    (w1_cc * t1 + w2_cc * t2) / (w1_cc + w2_cc)
  }
  
  if (hipoteza == "dwustronny") {
    if (rowne_wariancje) {
      t_kr = qt(1 - alpha/2, df_t)
    } else {
      t_kr = kwantyl_CC(1 - alpha/2)
    }
    odrzuc  = (abs(t_stat) > t_kr)
    opis_H0 = "mu1 = mu2"
    opis_H1 = "mu1 != mu2"
    warunek = paste("|t| =", round(abs(t_stat), 4), ">  t_kr =", round(t_kr, 4))
    
  } else if (hipoteza == "prawostronny") {
    if (rowne_wariancje) {
      t_kr = qt(1 - alpha, df_t)
    } else {
      t_kr = kwantyl_CC(1 - alpha)
    }
    odrzuc  = (t_stat > t_kr)
    opis_H0 = "mu1 <= mu2"
    opis_H1 = "mu1 > mu2"
    warunek = paste("t =", round(t_stat, 4), ">  t_kr =", round(t_kr, 4))
    
  } else if (hipoteza == "lewostronny") {
    if (rowne_wariancje) {
      t_kr = qt(alpha, df_t)
    } else {
      t_kr = kwantyl_CC(alpha)   # wartość ujemna
    }
    odrzuc  = (t_stat < t_kr)
    opis_H0 = "mu1 >= mu2"
    opis_H1 = "mu1 < mu2"
    warunek = paste("t =", round(t_stat, 4), "<  t_kr =", round(t_kr, 4))
    
  } else {
    stop("Nieznany typ hipotezy. Użyj: 'dwustronny', 'prawostronny' lub 'lewostronny'.")
  }
  
  # WYNIKI
  cat("\n=== HIPOTEZA ===\n")
  cat("H0:", opis_H0, "\n")
  cat("H1:", opis_H1, "\n")
  cat("Poziom istotnosci alpha =", alpha, "\n")
  
  cat("\n=== TEST F (rownosc wariancji) ===\n")
  cat("F =", round(F_stat, 4), "| F_kr =", round(F_kr, 4),
      "| Wariancje:", ifelse(rowne_wariancje, "ROWNE", "NIEROWNE"), "\n")
  cat("Metoda:", metoda, "\n")
  
  cat("\n=== TEST T ===\n")
  cat("Srednia w1 =", round(sr1, 4), "\n")
  cat("Srednia w2 =", round(sr2, 4), "\n")
  if (rowne_wariancje) {
    cat("t =", round(t_stat, 4), "| t_kr =", round(t_kr, 4), "| df =", round(df_t, 2), "\n")
  } else {
    cat("C =", round(t_stat, 4), "| c(p,n1,n2) =", round(t_kr, 4), "| df = brak (Cochran-Cox)\n")
  }
  
  cat("\n=== DECYZJA ===\n")
  if (odrzuc) {
    cat(warunek, ": ODRZUCAMY H0\n")
    cat("Na poziomie istotnosci", alpha, "– podstawy do przyjecia H1 (", opis_H1, ")\n")
  } else {
    cat(warunek, ": BRAK PODSTAW do odrzucenia H0\n")
    cat("Na poziomie istotnosci", alpha, "– brak dowodow na H1 (", opis_H1, ")\n")
  }
  
  invisible(list(t = t_stat, t_kr = t_kr, df = df_t, sr1 = sr1, sr2 = sr2,
                 rowne_wariancje = rowne_wariancje, odrzuc_H0 = odrzuc))
}


ptt = function(w1, w2, alpha = 0.05, hipoteza = "dwustronny", R = 10000) {
  
  # hipoteza: "dwustronny"   -> H1: mu1 != mu2
  #           "prawostronny" -> H1: mu1 >  mu2
  #           "lewostronny"  -> H1: mu1 <  mu2
  
  hipoteza  = tolower(hipoteza)
  n1        = length(w1)
  n2        = length(w2)
  pula      = c(w1, w2)
  n_total   = n1 + n2
  
  # Zaobserwowana różnica średnich
  t_obs = mean(w1) - mean(w2)
  
  # Symulacja Monte Carlo przy R permutacji, w naszym przypadku 10k
  t_perm = numeric(R)
  for (i in 1:R) {
    losowanie  = sample(pula, n_total, replace = FALSE)
    g1         = losowanie[1:n1]
    g2         = losowanie[(n1+1):n_total]
    t_perm[i]  = mean(g1) - mean(g2)
  }
  
  # p-value zależne od hipotezy
  if (hipoteza == "dwustronny") {
    p_val   = mean(abs(t_perm) >= abs(t_obs))
    opis_H0 = "mu1 = mu2"
    opis_H1 = "mu1 != mu2"
  } else if (hipoteza == "prawostronny") {
    p_val   = mean(t_perm >= t_obs)
    opis_H0 = "mu1 <= mu2"
    opis_H1 = "mu1 > mu2"
  } else if (hipoteza == "lewostronny") {
    p_val   = mean(t_perm <= t_obs)
    opis_H0 = "mu1 >= mu2"
    opis_H1 = "mu1 < mu2"
  } else {
    stop("Nieznany typ hipotezy. Uzyj: 'dwustronny', 'prawostronny' lub 'lewostronny'.")
  }
  
  # Wyniki
  cat("\n=== TEST PERMUTACYJNY (Monte Carlo) ===\n")
  cat("H0:", opis_H0, "\n")
  cat("H1:", opis_H1, "\n")
  cat("Poziom istotnosci alpha =", alpha, "\n")
  cat("Liczba permutacji R    =", R, "\n")
  cat("\nZaobserwowana roznica srednich =", round(t_obs, 4), "\n")
  cat("p-value =", formatC(p_val, format = "f", digits = 4), "\n")
  
  cat("\n=== DECYZJA ===\n")
  if (p_val <= alpha) {
    cat("p <=", alpha, ": ODRZUCAMY H0\n")
    cat("Na poziomie istotnosci", alpha, "– podstawy do przyjecia H1 (", opis_H1, ")\n")
  } else {
    cat("p >", alpha, ": BRAK PODSTAW do odrzucenia H0\n")
    cat("Na poziomie istotnosci", alpha, "– brak dowodow na H1 (", opis_H1, ")\n")
  }
  
  invisible(list(t_obs = t_obs, t_perm = t_perm, p_val = p_val))
}