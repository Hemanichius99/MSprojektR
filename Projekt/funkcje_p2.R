# PODPUNKT 2.
setwd("D:/POLITECHNIKA/SEMESTR 4/STATYSTYKA/P/PROJEKT W R/projekt/Projekt")

sr = function(x)
{
bb = seq(min(x), max(x), length.out = 8)  # wektor wartośći od min do max równoodległych od siebie i podzielonych na 8 części
hh = hist(x, bb, xlim = c(10, 35), labels = TRUE, freq = FALSE)
}

wyswietl_lubuskie = function(wl)
{
  cat("\n\n")
  cat("WOJEWÓDZTWO LUBUSKIE\n")
  cat(format("Nazwa statystyki",                         justify = 'right', width = 44), sep = '', format("Sz. szcz.", width = 17, justify = 'right'),   format("Sz. roz.", width = 12, justify = 'right'), '\n')
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
      format("Odczylenie standardowe S2",                justify = 'right', width = 44),           format(wl["osss"], width = 17, justify = 'right'),    format(wl["ossr"], width = 12, justify = 'right'), '\n',
      format("Odczylenie przeciętne d1",                 justify = 'right', width = 44),           format(wl["oposss"], width = 17, justify = 'right'),  format(wl["opossr"], width = 12, justify = 'right'), '\n',
      format("Odchylenie przeciętne od mediany d2",      justify = 'right', width = 44),           format(wl["opomess"], width = 17, justify = 'right'), format(wl["opomesr"], width = 12, justify = 'right'), '\n',
      format("Odchylenie ćwiartkowe",                    justify = 'right', width = 44),           format(wl["oćss"], width = 17, justify = 'right'),    format(wl["oćsr"], width = 12, justify = 'right'), '\n',
      format("Wzpółczynnik zmienności w % v",            justify = 'right', width = 44),           format(wl["wzss"], width = 17, justify = 'right'),    format(wl["wzsr"], width = 12, justify = 'right'), '\n',
      format("Pozycyjny wzpółczynnik zmienności w % vq", justify = 'right', width = 44),           format(wl["pwzss"], width = 17, justify = 'right'),   format(wl["pwzsr"], width = 12, justify = 'right'), '\n')
  
  
  cat("\n\nMiary asymetrii i koncentracji:\n")
  cat("==========================================================================\n")
  cat(format("Skośność as",                              justify = 'right', width = 44), sep = '', format(wl["sss"], width = 17, justify = 'right'),     format(wl["ssr"], width = 12, justify = 'right'), '\n',
      format("Kurtoza krt",                              justify = 'right', width = 44),           format(wl["kss"], width = 17, justify = 'right'),     format(wl["ksr"], width = 12, justify = 'right'), '\n',
      format("Eksces",                                   justify = 'right', width = 44),           format(wl["ess"], width = 17, justify = 'right'),     format(wl["esr"], width = 12, justify = 'right'), '\n')
}


wyswietl_wielkopolskie = function(ww)
{
  cat("\n\n")
  cat("WOJEWÓDZTWO WIELKOPOLSKIE\n")
  cat(format("Nazwa statystyki",                         justify = 'right', width = 44), sep = '', format("Sz. szcz.", width = 17, justify = 'right'),   format("Sz. roz.", width = 12, justify = 'right'), '\n')
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
      format("Odczylenie standardowe S2",                justify = 'right', width = 44),           format(ww["osss"], width = 17, justify = 'right'),    format(ww["ossr"], width = 12, justify = 'right'), '\n',
      format("Odczylenie przeciętne d1",                 justify = 'right', width = 44),           format(ww["oposss"], width = 17, justify = 'right'),  format(ww["opossr"], width = 12, justify = 'right'), '\n',
      format("Odchylenie przeciętne od mediany d2",      justify = 'right', width = 44),           format(ww["opomess"], width = 17, justify = 'right'), format(ww["opomesr"], width = 12, justify = 'right'), '\n',
      format("Odchylenie ćwiartkowe",                    justify = 'right', width = 44),           format(ww["oćss"], width = 17, justify = 'right'),    format(ww["oćsr"], width = 12, justify = 'right'), '\n',
      format("Wzpółczynnik zmienności w % v",            justify = 'right', width = 44),           format(ww["wzss"], width = 17, justify = 'right'),    format(ww["wzsr"], width = 12, justify = 'right'), '\n',
      format("Pozycyjny wzpółczynnik zmienności w % vq", justify = 'right', width = 44),           format(ww["pwzss"], width = 17, justify = 'right'),   format(ww["pwzsr"], width = 12, justify = 'right'), '\n')
  
  
  cat("\n\nMiary asymetrii i koncentracji:\n")
  cat("==========================================================================\n")
  cat(format("Skośność as",                              justify = 'right', width = 44), sep = '', format(ww["sss"], width = 17, justify = 'right'),     format(ww["ssr"], width = 12, justify = 'right'), '\n',
      format("Kurtoza krt",                              justify = 'right', width = 44),           format(ww["kss"], width = 17, justify = 'right'),     format(ww["ksr"], width = 12, justify = 'right'), '\n',
      format("Eksces",                                   justify = 'right', width = 44),           format(ww["ess"], width = 17, justify = 'right'),     format(ww["esr"], width = 12, justify = 'right'), '\n')
}

# Histogram
sr_calosc = sr(calosc)
sr_lubuskie = sr(lubuskie)
sr_wielkopolskie = sr(wielkopolskie)

# Statystyki
wl = c()
ww = c()
# wartość średnia szereg szczegółowy
xsr_ss1 = mean(lubuskie);xsr_ss1
xsr_ss2 = mean(wielkopolskie);xsr_ss2
wl['wsss'] = xsr_ss1
ww['wsss'] = xsr_ss2

# wartość średnia szereg rozdzielczy
xsr_sr1 = sum(sr_lubuskie$mids * sr_lubuskie$counts) / sum(sr_lubuskie$counts);xsr_sr1
xsr_sr2 = sum(sr_wielkopolskie$mids * sr_wielkopolskie$counts) / sum(sr_wielkopolskie$counts);xsr_sr2
wl['wssr'] = xsr_sr1
ww['wssr'] = xsr_sr2

# wariancja nieobciążona s2*szereg szczegółowy
var_ss1 = var(lubuskie);var_ss1
var_ss2 = var(wielkopolskie);var_ss2
wl['wnss'] = var_ss1
ww['wnss'] = var_ss2

# wariancja nieobciążona s2*szereg rozdzielczy
var_sr1 = sum(sr_lubuskie$counts * (sr_lubuskie$mids - xsr_sr1)^2) / (sum(sr_lubuskie$counts) - 1); var_sr1
var_sr2 = sum(sr_wielkopolskie$counts * (sr_wielkopolskie$mids - xsr_sr2)^2) / (sum(sr_wielkopolskie$counts) - 1); var_sr2
wl['wnsr'] = var_sr1
ww['wnsr'] = var_sr2

# wariancja s2 szereg rozdzielczy
var_sr1_n = sum((sr_lubuskie$mids - xsr_sr1)^2 * sr_lubuskie$counts) /
  sum(sr_lubuskie$counts);var_sr1_n

var_sr2_n = sum((sr_wielkopolskie$mids - xsr_sr2)^2 * sr_wielkopolskie$counts) /
  sum(sr_wielkopolskie$counts);var_sr2_n

wl['wsr'] = var_sr1_n
ww['wsr'] = var_sr2_n

# odchylenie standardowe s*szereg szczegółowy
os_ss1 = sd(lubuskie);os_ss1
os_ss2 = sd(wielkopolskie);os_ss2
wl['osnss'] = os_ss1
ww['osnss'] = os_ss2

# odchylenie standardowe s*szereg rozdzielczy
os_sr1_r = sqrt(var_sr1); os_sr1_r
os_sr2_r = sqrt(var_sr2); os_sr2_r
wl['osnsr'] = os_sr1_r
ww['osnsr'] = os_sr2_r

# odchylenie standardowe s szereg rozdzielczy
os_sr1_r_n = sqrt(var_sr1_n); os_sr1_r_n
os_sr2_r_n = sqrt(var_sr2_n ); os_sr2_r_n
wl['ossr'] = os_sr1_r_n
ww['ossr'] = os_sr2_r_n

# kwartyle szereg szczegółowy
q1_ss1 = quantile(lubuskie, 0.25);q1_ss1  # Q1
m_ss1 = quantile(lubuskie, 0.5);m_ss1     # mediana
q3_ss1 = quantile(lubuskie, 0.75);q3_ss1  # Q3
wl['q1ss'] = q1_ss1
wl['mess'] = m_ss1
wl['q3ss'] = q3_ss1

q1_ss2 = quantile(wielkopolskie, 0.25);q1_ss2  # Q1
m_ss2 = quantile(wielkopolskie, 0.5);m_ss2     # mediana
q3_ss2 = quantile(wielkopolskie, 0.75);q3_ss2  # Q3
ww['q1ss'] = q1_ss2
ww['mess'] = m_ss2
ww['q3ss'] = q3_ss2

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

# Lubuskie
q1_sr1 = kwartyl_sr(sr_lubuskie, 0.25); q1_sr1
m_sr1  = kwartyl_sr(sr_lubuskie, 0.5);  m_sr1
q3_sr1 = kwartyl_sr(sr_lubuskie, 0.75); q3_sr1
wl['q1sr'] = q1_sr1
wl['mesr'] = m_sr1
wl['q3sr'] = q3_sr1


# Wielkopolskie
q1_sr2 = kwartyl_sr(sr_wielkopolskie, 0.25); q1_sr2
m_sr2  = kwartyl_sr(sr_wielkopolskie, 0.5);  m_sr2
q3_sr2 = kwartyl_sr(sr_wielkopolskie, 0.75); q3_sr2
ww['q1sr'] = q1_sr2
ww['mesr'] = m_sr2
ww['q3sr'] = q3_sr2

# moda szereg szczegółowy
moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

moda_ss1 = moda(lubuskie); moda_ss1
moda_ss2 = moda(wielkopolskie); moda_ss2
wl['moss'] = moda_ss1
ww['moss'] = moda_ss2


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

moda_sr1 = moda_sr(sr_lubuskie); moda_sr1
moda_sr2 = moda_sr(sr_wielkopolskie); moda_sr2
wl['mosr'] = moda_sr1
ww['mosr'] = moda_sr2

# skorśność szereg szczegółowy
sk_ss1 = skewness(lubuskie); sk_ss1
sk_ss2 = skewness(wielkopolskie); sk_ss2

wl['sss'] = sk_ss1
ww['sss'] = sk_ss2

# skorśność szereg rozdzielczy
sk_sr1 = sum((sr_lubuskie$mids - xsr_sr1)^3) /
  (length(lubuskie) * os_sr1_r^3)
sk_sr1

sk_sr2 = sum((sr_wielkopolskie$mids - xsr_sr2)^3) /
  (length(wielkopolskie) * os_sr2_r^3)
sk_sr2

wl['ssr'] = sk_sr1
ww['ssr'] = sk_sr2

# kurtoza szereg szczegółowy
kur_ss1 = kurtosis(lubuskie); kur_ss1
kur_ss2 = kurtosis(wielkopolskie); kur_ss2

wl['kss'] = kur_ss1
ww['kss'] = kur_ss2

# kurtoza szereg rozdzielczy
kur_sr1 = sum((sr_lubuskie$mids - xsr_sr1)^4) /
  (length(lubuskie) * os_sr1_r^4)
kur_sr1

kur_sr2 = sum((sr_wielkopolskie$mids - xsr_sr2)^4) /
  (length(wielkopolskie) * os_sr2_r^4)
kur_sr2

wl['ksr'] = kur_sr1
ww['ksr'] = kur_sr2

# odchylenie przeciętne od średniej szereg szczegółowy
d1_ss1 = mean(abs(lubuskie - xsr_ss1));d1_ss1
d1_ss2 = mean(abs(wielkopolskie - xsr_ss2));d1_ss2
wl['oposss'] = d1_ss1
ww['oposss'] = d1_ss2

# odchylenie przeciętne od średniej szereg rozdzielczy
d1_sr1 = sum(abs(sr_lubuskie$mids - xsr_sr1) * sr_lubuskie$counts) /
  sum(sr_lubuskie$counts);d1_sr1

d1_sr2 = sum(abs(sr_wielkopolskie$mids - xsr_sr2) * sr_wielkopolskie$counts) /
  sum(sr_wielkopolskie$counts);d1_sr2

wl['opossr'] = d1_sr1
ww['opossr'] = d1_sr2

# odchylenie przeciętne od mediany szereg szczegółowy
d2_ss1 = mean(abs(lubuskie - m_ss1));d2_ss1
d2_ss2 = mean(abs(wielkopolskie - m_ss2));d2_ss2 

wl['opomess'] = d2_ss1
ww['opomess'] = d2_ss2

# odchylenie przeciętne od mediany szereg rozdzielczy
d2_sr1 = sum(abs(sr_lubuskie$mids - m_sr1) * sr_lubuskie$counts) /
  sum(sr_lubuskie$counts);d2_sr1

d2_sr2 = sum(abs(sr_wielkopolskie$mids - m_sr2) * sr_wielkopolskie$counts) /
  sum(sr_wielkopolskie$counts);d2_sr2

wl['opomesr'] = d2_sr1
ww['opomesr'] = d2_sr2

# odchylenie ćwiartkowe szereg szczegółowy
Q_ss1 = (q3_ss1 - q1_ss1) / 2;Q_ss1 
Q_ss2 = (q3_ss2 - q1_ss2) / 2;Q_ss2

wl['oćss'] = Q_ss1 
ww['oćss'] = Q_ss2


# odchylenie ćwiartkowe szereg rozdzielczy
Q_sr1 = (q3_sr1 - q1_sr1) / 2;Q_sr1
Q_sr2 = (q3_sr2 - q1_sr2) / 2;Q_sr2

wl['oćsr'] = Q_sr1 
ww['oćsr'] = Q_sr2

# współczynnik zmienności szereg szczegółowy
v_ss1 = (os_ss1 / xsr_ss1) * 100;v_ss1
v_ss2 = (os_ss2 / xsr_ss2) * 100;v_ss2

wl['wzss'] = v_ss1
ww['wzss'] = v_ss2

# współczynnik zmienności szereg rozdzielczy
v_sr1 = (os_sr1_r / xsr_sr1) * 100;v_sr1
v_sr2 = (os_sr2_r / xsr_sr2) * 100;v_sr2

wl['wzsr'] = v_sr1
ww['wzsr'] = v_sr2

# pozycyjny współczynnik zmienności szereg szczegółowy
vq_ss1 = ((q3_ss1 - q1_ss1) / (2 * m_ss1)) * 100;vq_ss1
vq_ss2 = ((q3_ss2 - q1_ss2) / (2 * m_ss2)) * 100;vq_ss2

wl['pwzss'] = vq_ss1
ww['pwzss'] = vq_ss2

# pozycyjny współczynnik zmienności szereg rozdzielczy
vq_sr1 = ((q3_sr1 - q1_sr1) / (2 * m_sr1)) * 100;vq_sr1
vq_sr2 = ((q3_sr2 - q1_sr2) / (2 * m_sr2)) * 100;vq_sr2

wl['pwzsr'] = vq_sr1
ww['pwzsr'] = vq_sr2
# eksces szereg szczegółowy
g2_ss1 = kur_ss1 - 3;g2_ss1
g2_ss2 = kur_ss2 - 3;g2_ss2

wl['ess'] = g2_ss1
ww['ess'] = g2_ss2