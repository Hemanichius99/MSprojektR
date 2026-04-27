# Funkcja wykonująca test Kołmogorowa-Lillieforsa
test_lillieforsa <- function(dane, nazwa_grupy) {
  n <- length(dane)
  dane_sort <- sort(dane)
  
  # Parametry z próby
  m <- mean(dane)
  sig <- sd(dane)
  
  # Standaryzacja i dystrybuanta teoretyczna F0
  # Używamy pnorm do wyliczenia prawdopodobieństwa w rozkładzie N(m, sig)
  F0 <- pnorm(dane_sort, mean = m, sd = sig)
  
  # Dystrybuanta empiryczna
  i_nad_n <- (1:n) / n
  i_odj_1_nad_n <- ((1:n) - 1) / n
  
  # Statystyka D (maksymalna różnica)
  d_plus <- max(abs(i_nad_n - F0))
  d_minus <- max(abs(i_odj_1_nad_n - F0))
  D_n <- max(d_plus, d_minus)
  
  # Wyznaczenie wartości krytycznej (Lilliefors dla alfa = 0.05)
  # Przybliżenie: 0.886 / sqrt(n)
  k_n <- 0.886 / sqrt(n)
  
  # Decyzja
  odrzucamy <- D_n > k_n
  
  return(list(
    nazwa = nazwa_grupy,
    statystyka = D_n,
    wartosc_krytyczna = k_n,
    odrzucamy = odrzucamy,
    n = n
  ))
}

# Funkcja drukująca wyniki w czytelny sposób
drukuj_test_normalnosci <- function(wynik) {
  cat("\n--- Test zgodności Kołmogorowa-Lillieforsa ---\n")
  cat("Grupa: ", wynik$nazwa, "\n")
  cat("Liczebność (n): ", wynik$n, "\n")
  cat("Statystyka D: ", round(wynik$statystyka, 4), "\n")
  cat("Wartość krytyczna: ", round(wynik$wartosc_krytyczna, 4), "\n")
  
  if (wynik$odrzucamy) {
    cat("Wynik: Odrzucamy H0. Rozkład NIE jest normalny.\n")
  } else {
    cat("Wynik: Brak podstaw do odrzucenia H0. Rozkład może być normalny.\n")
  }
  cat("----------------------------------------------\n")
}