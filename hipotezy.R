mhipo = function(w1, w2) {
  
  # DANE PODSTAWOWE
  n1 = length(w1)
  n2 = length(w2)
  
  sr1 = sum(w1) / n1
  sr2 = sum(w2) / n2
  
  # WARIANCJE PRÓBKOWE
  s1 = sum((w1 - sr1)^2) / (n1 - 1)
  s2 = sum((w2 - sr2)^2) / (n2 - 1)
  
  # TEST F (równość wariancji)
  # H0: wariancje równe, H1: różne (dwustronny)
  F_stat = max(s1, s2) / min(s1, s2)
  
  # stopnie swobody: większa wariancja w liczniku
  if (s1 >= s2) {
    df_F1 = n1 - 1
    df_F2 = n2 - 1
  } else {
    df_F1 = n2 - 1
    df_F2 = n1 - 1
  }
  
  F_kr = qf(0.95, df_F1, df_F2)   # test dwustronny alpha=0.05 -> 0.95 w prawym ogonie
  rowne_wariancje = (F_stat <= F_kr)
  
  # STATYSTYKA T
  if (rowne_wariancje) {
    # Wariancje równe - pooled
    sp2 = ((n1 - 1) * s1 + (n2 - 1) * s2) / (n1 + n2 - 2)
    sp  = sqrt(sp2)
    t_stat = (sr1 - sr2) / (sp * sqrt(1/n1 + 1/n2))
    df_t = n1 + n2 - 2
  } else {
    # Wariancje nierówne - Welch
    se = sqrt(s1/n1 + s2/n2)
    t_stat = (sr1 - sr2) / se
    # stopnie swobody Welcha
    df_t = (s1/n1 + s2/n2)^2 / ((s1/n1)^2/(n1-1) + (s2/n2)^2/(n2-1))
  }
  
  # --- (test prawostronny: mu_L > mu_W)
  t_kr = qt(0.95, df_t)   # jednostronny prawy, alpha = 0.05
  
  cat("\n=== TEST T (mu_L > mu_W) ===\n")
  cat("Srednia w1 (lubuskie)    =", sr1, "\n")
  cat("Srednia w2 (wielkopolskie) =", sr2, "\n")
  cat("t =", t_stat, "| t_kr =", t_kr, "| df =", df_t, "\n")
  
  # --- DECYZJA ---
  cat("\n=== DECYZJA ===\n")
  if (t_stat > t_kr) {
    cat("t > t_kr: ODRZUCAMY H0\n")
    cat("Na poziomie istotnosci 0.05 zawartość cukru w lubuskim jest istotnie wieksza.\n")
  } else {
    cat("t <= t_kr: BRAK PODSTAW do odrzucenia H0\n")
    cat("Na poziomie istotnosci 0.05 nie mozna twierdzic, ze lubuskie ma wieksza zawartosc cukru.\n")
  }
}