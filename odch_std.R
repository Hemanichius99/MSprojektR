# Funkcja wykonujaca pelna estymacje dla jednego regionu
analiza_odchylenia_region <- function(dane, conf_level = 0.95) {
  n <- length(dane)
  if (n < 2) stop("Próba musi zawierać co najmniej 2 elementy.")
  
  # Średnia arytmetyczna
  srednia <- sum(dane) / n
  
  # Wariancja (wariancja z próby s^2)
  wariancja <- sum((dane - srednia)^2) / (n - 1)
  
  # Odchylenie standardowe (s)
  s <- sqrt(wariancja)
  
  # Statystyka chi-kwadrat
  alpha <- 1 - conf_level
  df <- n - 1
  
  # Kwantyle rozkładu chi-kwadrat 
  chi_dolny <- qchisq(1 - alpha/2, df)
  chi_gorny <- qchisq(alpha/2, df)
  
  # Przedział ufności dla odchylenia standardowego (sigma)
  L <- sqrt((df * wariancja) / chi_dolny)
  P <- sqrt((df * wariancja) / chi_gorny)
  
  # --- PRECYZJA I TESTY ---
  # Względna precyzja oszacowania (procentowa)
  d <- (P - L) / 2
  precyzja <- (d / s) * 100
  
  # Test normalności 
  test_norm <- shapiro.test(dane)
  
  # Zwracanie wyników
  list(
    n = n,
    s = s,
    przedzial = c(L, P),
    precyzja = precyzja,
    p_val_shapiro = test_norm$p.value
  )
}

# Funkcja do ładnego wyświetlania wyników :3
drukuj_wyniki <- function(res, nazwa_regionu) {
  cat("\n====================================================\n")
  cat(sprintf("ANALIZA DLA REGIONU: %s\n", nazwa_regionu))
  cat("====================================================\n")
  cat(sprintf("Liczba dostaw (n):      %d\n", res$n))
  cat(sprintf("Odchylenie (s):         %.4f%%\n", res$s))
  cat(sprintf("Przedział ufności:      [%.4f%%, %.4f%%]\n", res$przedzial[1], res$przedzial[2]))
  cat(sprintf("Względna precyzja:      %.2f%%\n", res$precyzja))
  cat("----------------------------------------------------\n")
  cat(sprintf("Test normalności p-val: %.4f\n", res$p_val_shapiro))
  
  if(res$p_val_shapiro > 0.05) {
    cat("WYNIK: Rozkład normalny zachowany. Można uogólniać.\n")
  } else {
    cat("WYNIK: Brak normalności. Uogólnianie ryzykowne.\n")
  }
  cat("====================================================\n")
}