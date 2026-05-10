# PODPUNKT 2.
setwd("D:/POLITECHNIKA/SEMESTR 4/STATYSTYKA/P/PROJEKT W R/projekt/Projekt")

# source('wczytaj_dane.R', echo = TRUE)
source('wczytaj_dane.R')

# Funkcja rysująca histogram częstości
sr = function(x, tytul)
{
bb = seq(min(x), max(x), length.out = 8)    # Wektor wartośći od minmalnej do maksymalnej równoodległych od siebie i podzielonych na k = 8 części ≈ sqrt(length([lubuskie | wielkopolskie]))
                                            # Rozstęp h = (xmax - xmin) / k
hh = hist(x, main = tytul, bb, xlim = c(10, 35), labels = TRUE, freq = FALSE)
}

wyswietl_lubuskie = function(wl)
{
  cat("\n\n")
  cat("WOJEWÓDZTWO LUBUSKIE\n")
  cat(format("Liczba obserwacji =",                      justify = 'right', width = 44), sep = '', format(length(lubuskie), width = 17, justify = 'right'), '\n')
  cat(format("Zakres =",                                 justify = 'right', width = 44), sep = '', format('<', width = 5, justify = 'right'), format(min(lubuskie), width = 1, justify = 'right'), format(', ', width = 1, justify = 'right'), format(max(lubuskie), width = 1, justify = 'right'), format('>', width = 1, justify = 'right'),'\n\n')
  cat(format("Nazwa statystyki",                         justify = 'right', width = 44), sep = '', format("Sz. szcz.", width = 17, justify = 'right'),   format("Sz. roz.", width = 12, justify = 'right'), '')
  cat("\n\nMiary położenia:\n")
  cat("==========================================================================\n")
  cat(format("Wartość średnia",                          justify = 'right', width = 44), sep = '', format(wl["wsss"], width = 17, justify = 'right'),    format(wl["wssr"], width = 12, justify = 'right'), '\n',
      format("Mediana",                                  justify = 'right', width = 44),           format(wl["mess"], width = 17, justify = 'right'),    format(wl["mesr"], width = 12, justify = 'right'), '\n',
      format("Moda",                                     justify = 'right', width = 44),           format(wl["moss"], width = 17, justify = 'right'),    format(wl["mosr"], width = 12, justify = 'right'), '\n',
      format("Q1",                                       justify = 'right', width = 44),           format(wl["q1ss"], width = 17, justify = 'right'),    format(wl["q1sr"], width = 12, justify = 'right'), '\n',
      format("Q3",                                       justify = 'right', width = 44),           format(wl["q3ss"], width = 17, justify = 'right'),    format(wl["q3sr"], width = 12, justify = 'right'), '\n')

  
  cat("\n\nMiary rozproszenia:\n")
  cat("==========================================================================\n")
  cat(format("Wariancja nieobciążona S*2",               justify = 'right', width = 44), sep = '', format(wl["wnss"], width = 17, justify = 'right'),    format(wl["wnsr"], width = 12, justify = 'right'), '\n',
      format("Odchylenie standardowe nieobciążone S*",   justify = 'right', width = 44),           format(wl["osnss"], width = 17, justify = 'right'),   format(wl["osnsr"], width = 12, justify = 'right'), '\n',
      format("Wariancja S2",                             justify = 'right', width = 44),           format(wl["wss"], width = 17, justify = 'right'),     format(wl["wsr"], width = 12, justify = 'right'), '\n',
      format("Odchylenie standardowe S",                 justify = 'right', width = 44),           format(wl["osss"], width = 17, justify = 'right'),    format(wl["ossr"], width = 12, justify = 'right'), '\n',
      format("Odchylenie przeciętne d1",                 justify = 'right', width = 44),           format(wl["oposss"], width = 17, justify = 'right'),  format(wl["opossr"], width = 12, justify = 'right'), '\n',
      format("Odchylenie przeciętne od mediany d2",      justify = 'right', width = 44),           format(wl["opomess"], width = 17, justify = 'right'), format(wl["opomesr"], width = 12, justify = 'right'), '\n',
      format("Odchylenie ćwiartkowe",                    justify = 'right', width = 44),           format(wl["oćss"], width = 17, justify = 'right'),    format(wl["oćsr"], width = 12, justify = 'right'), '\n',
      format("Wzpółczynnik zmienności w % v",            justify = 'right', width = 44),           format(wl["wzss"], width = 17, justify = 'right'),    format(wl["wzsr"], width = 12, justify = 'right'), '\n',
      format("Pozycyjny wzpółczynnik zmienności w % vq", justify = 'right', width = 44),           format(wl["pwzss"], width = 17, justify = 'right'),   format(wl["pwzsr"], width = 12, justify = 'right'), '\n')
  
  
  cat("\n\nMiary asymetrii i koncentracji:\n")
  cat("==========================================================================\n")
  cat(format("Skośność as",                              justify = 'right', width = 44), sep = '', format(wl["sss"], width = 17, justify = 'right'),     format(wl["ssr"], width = 12, justify = 'right'), '\n',
      format("Kurtoza krt",                              justify = 'right', width = 44),           format(wl["kss"], width = 17, justify = 'right'),     format(wl["ksr"], width = 12, justify = 'right'), '\n',
      format("Eksces g2",                                justify = 'right', width = 44),           format(wl["ess"], width = 17, justify = 'right'),     format(wl["esr"], width = 12, justify = 'right'), '\n')
}


wyswietl_wielkopolskie = function(ww)
{
  cat("\n\n")
  cat("WOJEWÓDZTWO WIELKOPOLSKIE\n")
  cat(format("Liczba obserwacji =",                      justify = 'right', width = 44), sep = '', format(length(wielkopolskie), width = 17, justify = 'right'), '\n')
  cat(format("Zakres =",                                 justify = 'right', width = 44), sep = '', format('<', width = 5, justify = 'right'), format(min(wielkopolskie), width = 1, justify = 'right'), format(', ', width = 1, justify = 'right'), format(max(wielkopolskie), width = 1, justify = 'right'), format('>', width = 1, justify = 'right'),'\n\n')
  cat(format("Nazwa statystyki",                         justify = 'right', width = 44), sep = '', format("Sz. szcz.", width = 17, justify = 'right'),   format("Sz. roz.", width = 12, justify = 'right'), '')
  cat("\n\nMiary położenia:\n")
  cat("==========================================================================\n")
  cat(format("Wartość średnia",                          justify = 'right', width = 44), sep = '', format(ww["wsss"], width = 17, justify = 'right'),    format(ww["wssr"], width = 12, justify = 'right'), '\n',
      format("Mediana",                                  justify = 'right', width = 44),           format(ww["mess"], width = 17, justify = 'right'),    format(ww["mesr"], width = 12, justify = 'right'), '\n',
      format("Moda",                                     justify = 'right', width = 44),           format(ww["moss"], width = 17, justify = 'right'),    format(ww["mosr"], width = 12, justify = 'right'), '\n',
      format("Q1",                                       justify = 'right', width = 44),           format(ww["q1ss"], width = 17, justify = 'right'),    format(ww["q1sr"], width = 12, justify = 'right'), '\n',
      format("Q3",                                       justify = 'right', width = 44),           format(ww["q3ss"], width = 17, justify = 'right'),    format(ww["q3sr"], width = 12, justify = 'right'), '\n')
  
  
  cat("\n\nMiary rozproszenia:\n")
  cat("==========================================================================\n")
  cat(format("Wariancja nieobciążona S*2",               justify = 'right', width = 44), sep = '', format(ww["wnss"], width = 17, justify = 'right'),    format(ww["wnsr"], width = 12, justify = 'right'), '\n',
      format("Odchylenie standardowe nieobciążone S*",   justify = 'right', width = 44),           format(ww["osnss"], width = 17, justify = 'right'),   format(ww["osnsr"], width = 12, justify = 'right'), '\n',
      format("Wariancja S2",                             justify = 'right', width = 44),           format(ww["wss"], width = 17, justify = 'right'),     format(ww["wsr"], width = 12, justify = 'right'), '\n',
      format("Odchylenie standardowe S",                 justify = 'right', width = 44),           format(ww["osss"], width = 17, justify = 'right'),    format(ww["ossr"], width = 12, justify = 'right'), '\n',
      format("Odchylenie przeciętne d1",                 justify = 'right', width = 44),           format(ww["oposss"], width = 17, justify = 'right'),  format(ww["opossr"], width = 12, justify = 'right'), '\n',
      format("Odchylenie przeciętne od mediany d2",      justify = 'right', width = 44),           format(ww["opomess"], width = 17, justify = 'right'), format(ww["opomesr"], width = 12, justify = 'right'), '\n',
      format("Odchylenie ćwiartkowe",                    justify = 'right', width = 44),           format(ww["oćss"], width = 17, justify = 'right'),    format(ww["oćsr"], width = 12, justify = 'right'), '\n',
      format("Wzpółczynnik zmienności w % v",            justify = 'right', width = 44),           format(ww["wzss"], width = 17, justify = 'right'),    format(ww["wzsr"], width = 12, justify = 'right'), '\n',
      format("Pozycyjny wzpółczynnik zmienności w % vq", justify = 'right', width = 44),           format(ww["pwzss"], width = 17, justify = 'right'),   format(ww["pwzsr"], width = 12, justify = 'right'), '\n')
  
  
  cat("\n\nMiary asymetrii i koncentracji:\n")
  cat("==========================================================================\n")
  cat(format("Skośność as",                              justify = 'right', width = 44), sep = '', format(ww["sss"], width = 17, justify = 'right'),     format(ww["ssr"], width = 12, justify = 'right'), '\n',
      format("Kurtoza krt",                              justify = 'right', width = 44),           format(ww["kss"], width = 17, justify = 'right'),     format(ww["ksr"], width = 12, justify = 'right'), '\n',
      format("Eksces g2",                                justify = 'right', width = 44),           format(ww["ess"], width = 17, justify = 'right'),     format(ww["esr"], width = 12, justify = 'right'), '\n')
}

# Histogram
sr_calosc = sr(calosc, 'Oba województwa')
sr_lubuskie = sr(lubuskie, 'Województwo lubuskie')
sr_wielkopolskie = sr(wielkopolskie, 'Województwo wielkopolskie')

# Statystyki
wl = c()
ww = c()

# Wartość średnia szereg szczegółowy
xsr_ss1 = mean(lubuskie);xsr_ss1
xsr_ss2 = mean(wielkopolskie);xsr_ss2
wl['wsss'] = round(xsr_ss1,6)
ww['wsss'] = round(xsr_ss2,6)

# Wartość średnia szereg rozdzielczy
xsr_sr1 = sum(sr_lubuskie$mids * sr_lubuskie$counts) / sum(sr_lubuskie$counts);xsr_sr1
xsr_sr2 = sum(sr_wielkopolskie$mids * sr_wielkopolskie$counts) / sum(sr_wielkopolskie$counts);xsr_sr2
wl['wssr'] = round(xsr_sr1,6)
ww['wssr'] = round(xsr_sr2,6)

# Wariancja nieobciążona s2*szereg szczegółowy
var_ss1_n = var(lubuskie);var_ss1_n
var_ss2_n = var(wielkopolskie);var_ss2_n
wl['wnss'] = round(var_ss1_n,6)
ww['wnss'] = round(var_ss2_n,6)

# Wariancja nieobciążona s2*szereg rozdzielczy
var_sr1_n = sum(sr_lubuskie$counts * (sr_lubuskie$mids - xsr_sr1)^2) / (sum(sr_lubuskie$counts) - 1); var_sr1_n
var_sr2_n = sum(sr_wielkopolskie$counts * (sr_wielkopolskie$mids - xsr_sr2)^2) / (sum(sr_wielkopolskie$counts) - 1); var_sr2_n
wl['wnsr'] = round(var_sr1_n,6)
ww['wnsr'] = round(var_sr2_n,6)

# Wariancja s2 szereg szczegółowy
wariancja_obciazona <- function(x) 
{
  
  mean((x - mean(x))^2)
}
var_ss1 = wariancja_obciazona(lubuskie);var_ss1
var_ss2 = wariancja_obciazona(wielkopolskie);var_ss2
wl['wss'] = round(var_ss1,6)
ww['wss'] = round(var_ss2,6)

# Wariancja s2 szereg rozdzielczy
var_sr1 = sum((sr_lubuskie$mids - xsr_sr1)^2 * sr_lubuskie$counts) /
  sum(sr_lubuskie$counts);var_sr1

var_sr2 = sum((sr_wielkopolskie$mids - xsr_sr2)^2 * sr_wielkopolskie$counts) /
  sum(sr_wielkopolskie$counts);var_sr2

wl['wsr'] = round(var_sr1,6)
ww['wsr'] = round(var_sr2,6)

# Odchylenie standardowe nieobciążone s*szereg szczegółowy
os_ss1_n = sd(lubuskie);os_ss1_n
os_ss2_n = sd(wielkopolskie);os_ss2_n
wl['osnss'] = round(os_ss1_n,6)
ww['osnss'] = round(os_ss2_n,6)

# Odchylenie standardowe nieobciążone s*szereg rozdzielczy
os_sr1_r_n = sqrt(var_sr1_n); os_sr1_r_n
os_sr2_r_n = sqrt(var_sr2_n); os_sr2_r_n
wl['osnsr'] = round(os_sr1_r_n,6)
ww['osnsr'] = round(os_sr2_r_n,6)

# Odchylenie standardowe s szereg szczegółowy
os_ss1 = sqrt(var_ss1);os_ss1
os_ss2 = sqrt(var_ss2);os_ss2
wl['osss'] = round(os_ss1,6)
ww['osss'] = round(os_ss2,6)

# Odchylenie standardowe s szereg rozdzielczy
os_sr1_r = sqrt(var_sr1); os_sr1_r
os_sr2_r = sqrt(var_sr2); os_sr2_r
wl['ossr'] = round(os_sr1_r,6)
ww['ossr'] = round(os_sr2_r,6)

# Kwartyle szereg szczegółowy
Q1 <- function(x) 
{
  x <- sort(x)
  n <- length(x)
  
  idx <- ceiling((n + 1) / 4)
  q1 <- x[idx]
  
  return(q1)
}

ME <- function(x) 
{
  x <- sort(x)
  n <- length(x)
  
  q2 <- median(x)
  
  return(q2)
}

Q3 <- function(x) 
{
  x <- sort(x)
  n <- length(x)
  
  idx <- floor(3 * (n + 1) / 4)
  q3 <- x[idx]
  
  return(q3)
}

q1_ss1 = Q1(lubuskie);q1_ss1  # Q1
m_ss1 = ME(lubuskie);m_ss1     # Mediana
q3_ss1 = Q3(lubuskie);q3_ss1  # Q3
wl['q1ss'] = round(q1_ss1,6)
wl['mess'] = round(m_ss1,6)
wl['q3ss'] = round(q3_ss1,6)

q1_ss2 = Q1(wielkopolskie);q1_ss2  # Q1
m_ss2 = ME(wielkopolskie);m_ss2     # Mediana
q3_ss2 = Q3(wielkopolskie);q3_ss2  # Q3
ww['q1ss'] = round(q1_ss2,6)
ww['mess'] = round(m_ss2,6)
ww['q3ss'] = round(q3_ss2,6)


# Kwartyle szereg rozdzielczy
Q1_2 <- function(sr)
{
  n <- sum(sr$counts)  # Liczebność cechy
  cum <- cumsum(sr$counts) # Wektor przechowujący sumy skomulowane dla poszczególnych przedziałów cechy
  
  idx = ceiling((n + 1) / 4)
  i <- which(cum >= idx)[1]
  
  x0 <- sr$breaks[i]
  ni <- sr$counts[i]
  h <- sr$breaks[i+1] - sr$breaks[i]
  cum_prev <- ifelse(i == 1, 0, cum[i-1])
  
  q <- x0 + ((0.25 * n - cum_prev) / ni) * h
  return(q)
}

Q3_2 <- function(sr)
{
  n <- sum(sr$counts)  # Liczebność cechy
  cum <- cumsum(sr$counts) # Wektor przechowujący sumy skomulowane dla poszczególnych przedziałów cechy
  
  idx = floor((3*(n + 1)) / 4)
  i <- which(cum >= idx)[1]
  
  x0 <- sr$breaks[i]
  ni <- sr$counts[i]
  h <- sr$breaks[i+1] - sr$breaks[i]
  cum_prev <- ifelse(i == 1, 0, cum[i-1])
  
  q <- x0 + ((0.75 * n - cum_prev) / ni) * h
  return(q)
}

ME_2 <- function(sr)
{
  n <- sum(sr$counts)  # Liczebność cechy
  cum <- cumsum(sr$counts) # Wektor przechowujący sumy skomulowane dla poszczególnych przedziałów cechy
  idx = 0
  if (n %% 2 == 0) # Parzyste
  {
    idx = (n / 2)
  }
  else # Nieparzyste
  {
    idx = (n + 1 / 2)
  }
  
  i <- which(cum >= idx)[1]
  
  x0 <- sr$breaks[i]
  ni <- sr$counts[i]
  h <- sr$breaks[i+1] - sr$breaks[i]
  cum_prev <- ifelse(i == 1, 0, cum[i-1])
  
  q <- x0 + ((0.5 * n - cum_prev) / ni) * h
  return(q)
}

# Lubuskie
q1_sr1 = Q1_2(sr_lubuskie);q1_sr1
m_sr1  = ME_2(sr_lubuskie);m_sr1
q3_sr1 = Q3_2(sr_lubuskie);q3_sr1
wl['q1sr'] = round(q1_sr1,6)
wl['mesr'] = round(m_sr1,6)
wl['q3sr'] = round(q3_sr1,6)


# Wielkopolskie
q1_sr2 = Q1_2(sr_wielkopolskie);q1_sr2
m_sr2  = ME_2(sr_wielkopolskie);m_sr2
q3_sr2 = Q3_2(sr_wielkopolskie);q3_sr2
ww['q1sr'] = round(q1_sr2,6)
ww['mesr'] = round(m_sr2, 6)
ww['q3sr'] = round(q3_sr2,6)

# Moda szereg szczegółowy
moda <- function(x) 
{
  
  t <- table(x) # table() tworzy tablicę liczebności
  
  max_licz <- max(t) # max() znajduje największą liczebność
  
  kandydaci <- names(t)[t == max_licz] # Znajdowanie wszystkich wartości z największą liczebnością
  
  if(length(kandydaci) == 1) # Sprawdzenie czy liczba wartości z największą liczebnościa == 1
  {
    
    cat("Moda istnieje\n")
    
    return(as.numeric(kandydaci)) # Zwrócenie jako liczby wartości która występuje najczęściej
    
  } 
  else # Liczba wartości z największą liczebnościa > 1
  {
    
    cat("Brak jednej mody - wiele maksimów\n")
    
    return("NULL")
  }
}

moda_ss1 = moda(lubuskie); moda_ss1
moda_ss2 = moda(wielkopolskie); moda_ss2
wl['moss'] = moda_ss1
ww['moss'] = moda_ss2


# Moda szereg rozdzielczy
moda_przedzialowa <- function(h) 
{
  
  counts <- h$counts
  breaks <- h$breaks
  
  max_count <- max(counts)
  
  klasy_modalne <- which(counts == max_count)
  
  # Sprawdzenie liczby klas modalnych
  if(length(klasy_modalne) > 1) {
    
    cat("Brak mody - wiele klas o tej samej największej liczebności\n")
    
    return(NULL)
  }
  
  k <- klasy_modalne
  
  # nie można liczyć gdy modalna jest skrajna
  if(k == 1 || k == length(counts)) {
    
    cat("Nie można wyznaczyć mody dla skrajnej klasy\n")
    
    return(NULL)
  }
  
  fm  <- counts[k]
  fm1 <- counts[k - 1]
  fp1 <- counts[k + 1]
  
  L <- breaks[k]
  
  hwidth <- breaks[k + 1] - breaks[k]
  
  D <- L + ((fm - fm1) /
              ((fm - fm1) + (fm - fp1))) * hwidth
  
  return(D)
}
moda_sr1 = moda_przedzialowa(sr_lubuskie); moda_sr1
moda_sr2 = moda_przedzialowa(sr_wielkopolskie); moda_sr2
wl['mosr'] = round(moda_sr1,6)
ww['mosr'] = round(moda_sr2,6)

# Skorśność szereg szczegółowy
sk_ss1 = (mean((lubuskie - xsr_ss1)^3)) / (os_ss1)^3; sk_ss1
sk_ss2 = (mean((wielkopolskie - xsr_ss2)^3)) / (os_ss2)^3; sk_ss2

wl['sss'] = round(sk_ss1,6)
ww['sss'] = round(sk_ss2,6)

# Skorśność szereg rozdzielczy
sk_sr1 = sum((sr_lubuskie$mids - xsr_sr1)^3) /
  (length(lubuskie) * os_sr1_r^3);sk_sr1

sk_sr2 = sum((sr_wielkopolskie$mids - xsr_sr2)^3) /
  (length(wielkopolskie) * os_sr2_r^3);sk_sr2

wl['ssr'] = round(sk_sr1,6)
ww['ssr'] = round(sk_sr2,6)


# Kurtoza szereg szczegółowy
kur_ss1 = (mean((lubuskie - xsr_ss1)^4)) / (os_ss1)^4; kur_ss1
kur_ss2 = (mean((wielkopolskie - xsr_ss2)^4)) / (os_ss2)^4; kur_ss2

wl['kss'] = round(kur_ss1,6)
ww['kss'] = round(kur_ss2,6)

# Kurtoza szereg rozdzielczy
kur_sr1 = sum((sr_lubuskie$mids - xsr_sr1)^4) /
  (length(lubuskie) * os_sr1_r^4);kur_sr1

kur_sr2 = sum((sr_wielkopolskie$mids - xsr_sr2)^4) /
  (length(wielkopolskie) * os_sr2_r^4);kur_sr2

wl['ksr'] = round(kur_sr1,6)
ww['ksr'] = round(kur_sr2,6)

# Odchylenie przeciętne od średniej szereg szczegółowy
d1_ss1 = mean(abs(lubuskie - xsr_ss1));d1_ss1
d1_ss2 = mean(abs(wielkopolskie - xsr_ss2));d1_ss2
wl['oposss'] = round(d1_ss1,6)
ww['oposss'] = round(d1_ss2,6)

# Odchylenie przeciętne od średniej szereg rozdzielczy
d1_sr1 = sum(abs(sr_lubuskie$mids - xsr_sr1) * sr_lubuskie$counts) /
  sum(sr_lubuskie$counts);d1_sr1

d1_sr2 = sum(abs(sr_wielkopolskie$mids - xsr_sr2) * sr_wielkopolskie$counts) /
  sum(sr_wielkopolskie$counts);d1_sr2

wl['opossr'] = round(d1_sr1,6)
ww['opossr'] = round(d1_sr2,6)

# Odchylenie przeciętne od mediany szereg szczegółowy
d2_ss1 = mean(abs(lubuskie - m_ss1));d2_ss1
d2_ss2 = mean(abs(wielkopolskie - m_ss2));d2_ss2 

wl['opomess'] = round(d2_ss1,6)
ww['opomess'] = round(d2_ss2,6)

# Odchylenie przeciętne od mediany szereg rozdzielczy
d2_sr1 = sum(abs(sr_lubuskie$mids - m_sr1) * sr_lubuskie$counts) /
  sum(sr_lubuskie$counts);d2_sr1

d2_sr2 = sum(abs(sr_wielkopolskie$mids - m_sr2) * sr_wielkopolskie$counts) /
  sum(sr_wielkopolskie$counts);d2_sr2

wl['opomesr'] = round(d2_sr1,6)
ww['opomesr'] = round(d2_sr2,6)

# Odchylenie ćwiartkowe szereg szczegółowy
Q_ss1 = (q3_ss1 - q1_ss1) / 2;Q_ss1 
Q_ss2 = (q3_ss2 - q1_ss2) / 2;Q_ss2

wl['oćss'] = round(Q_ss1,6) 
ww['oćss'] = round(Q_ss2,6)


# Odchylenie ćwiartkowe szereg rozdzielczy
Q_sr1 = (q3_sr1 - q1_sr1) / 2;Q_sr1
Q_sr2 = (q3_sr2 - q1_sr2) / 2;Q_sr2

wl['oćsr'] = round(Q_sr1,6) 
ww['oćsr'] = round(Q_sr2,6)

# Współczynnik zmienności szereg szczegółowy
v_ss1 = (os_ss1 / xsr_ss1) * 100;v_ss1
v_ss2 = (os_ss2 / xsr_ss2) * 100;v_ss2

wl['wzss'] = round(v_ss1,6)
ww['wzss'] = round(v_ss2,6)

# Współczynnik zmienności szereg rozdzielczy
v_sr1 = (os_sr1_r / xsr_sr1) * 100;v_sr1
v_sr2 = (os_sr2_r / xsr_sr2) * 100;v_sr2

wl['wzsr'] = round(v_sr1,6)
ww['wzsr'] = round(v_sr2,6)

# Pozycyjny współczynnik zmienności szereg szczegółowy
vq_ss1 = ((q3_ss1 - q1_ss1) / (2 * m_ss1)) * 100;vq_ss1
vq_ss2 = ((q3_ss2 - q1_ss2) / (2 * m_ss2)) * 100;vq_ss2

wl['pwzss'] = round(vq_ss1,6)
ww['pwzss'] = round(vq_ss2,6)

# Pozycyjny współczynnik zmienności szereg rozdzielczy
vq_sr1 = ((q3_sr1 - q1_sr1) / (2 * m_sr1)) * 100;vq_sr1
vq_sr2 = ((q3_sr2 - q1_sr2) / (2 * m_sr2)) * 100;vq_sr2

wl['pwzsr'] = round(vq_sr1,6)
ww['pwzsr'] = round(vq_sr2,6)

# Eksces szereg szczegółowy
g2_ss1 = kur_ss1 - 3;g2_ss1
g2_ss2 = kur_ss2 - 3;g2_ss2

wl['ess'] = round(g2_ss1,6)
ww['ess'] = round(g2_ss2,6)

# Eksces szereg rozdzielczy
g2_sr1 = kur_sr1 - 3;g2_sr1
g2_sr2 = kur_sr2 - 3;g2_sr2

wl['esr'] = round(g2_sr1,6)
ww['esr'] = round(g2_sr2,6)

