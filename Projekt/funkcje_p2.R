# PODPUNKT 2.
setwd("D:/POLITECHNIKA/SEMESTR 4/STATYSTYKA/P/PROJEKT W R/projekt/Projekt")
sr = function(x)
{
bb = seq(min(x), max(x), length.out = 8)  # wektor wartośći od min do max równoodległych od siebie i podzielonych na 8 części
hh = hist(x, bb, xlim = c(10, 35), labels = TRUE, freq = FALSE)
}

sr_calosc = sr(calosc)
sr_lubuskie = sr(lubuskie)
sr_wielkopolskie = sr(wielkopolskie)

# wartość średnia szereg szczegółowy
xsr_ss1 = mean(lubuskie);xsr_ss1
xsr_ss2 = mean(wielkopolskie);xsr_ss1

# wartość średnia szereg rozdzielczy
xsr_sr1 = sum(sr_lubuskie$mids * sr_lubuskie$counts) / sum(sr_lubuskie$counts);xsr_sr1
xsr_sr2 = sum(sr_wielkopolskie$mids * sr_wielkopolskie$counts) / sum(sr_wielkopolskie$counts);xsr_sr2

# wariancja nieobciążona s2*szereg szczegółowy
var_ss1 = var(lubuskie);var_ss1
var_ss2 = var(wielkopolskie);var_ss2

# wariancja nieobciążona s2*szereg rozdzielczy
var_sr1 = sum(sr_lubuskie$counts * (sr_lubuskie$mids - xsr_sr1)^2) / (sum(sr_lubuskie$counts) - 1); var_sr1
var_sr2 = sum(sr_wielkopolskie$counts * (sr_wielkopolskie$mids - xsr_sr2)^2) / (sum(sr_wielkopolskie$counts) - 1); var_sr2

# wariancja s2 szereg rozdzielczy
var_sr1_n = sum((sr_lubuskie$mids - xsr_sr1)^2 * sr_lubuskie$counts) /
  sum(sr_lubuskie$counts);var_sr1_n

var_sr2_n = sum((sr_wielkopolskie$mids - xsr_sr2)^2 * sr_wielkopolskie$counts) /
  sum(sr_wielkopolskie$counts);var_sr2_n

# odchylenie standardowe s*szereg szczegółowy
os_ss1 = sd(lubuskie);os_ss1
os_ss2 = sd(wielkopolskie);os_ss2

# odchylenie standardowe s*szereg rozdzielczy
os_sr1_r = sqrt(var_sr1); os_sr1_r
os_sr2_r = sqrt(var_sr2); os_sr2_r

# odchylenie standardowe s szereg rozdzielczy
os_sr1_r_n = sqrt(var_sr1_n); os_sr1_r_n
os_sr2_r_n = sqrt(var_sr2_n ); os_sr2_r_n

# kwartyle szereg szczegółowy
q1_ss1 = quantile(lubuskie, 0.25);q1_ss1  # Q1
m_ss1 = quantile(lubuskie, 0.5);m_ss1     # mediana
q3_ss1 = quantile(lubuskie, 0.75);q3_ss1  # Q3

q1_ss2 = quantile(wielkopolskie, 0.25);q1_ss2  # Q1
m_ss2 = quantile(wielkopolskie, 0.5);m_ss2     # mediana
q3_ss2 = quantile(wielkopolskie, 0.75);q3_ss2  # Q3

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

# Wielkopolskie
q1_sr2 = kwartyl_sr(sr_wielkopolskie, 0.25); q1_sr2
m_sr2  = kwartyl_sr(sr_wielkopolskie, 0.5);  m_sr2
q3_sr2 = kwartyl_sr(sr_wielkopolskie, 0.75); q3_sr2

# moda szereg szczegółowy
moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

moda_ss1 = moda(lubuskie); moda_ss1
moda_ss2 = moda(wielkopolskie); moda_ss2

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

# skorśność szereg szczegółowy
sk_ss1 = skewness(lubuskie); sk_ss1
sk_ss2 = skewness(wielkopolskie); sk_ss2

# skorśność szereg rozdzielczy
sk_sr1 = sum((sr_lubuskie$mids - xsr_sr1)^3) /
  (length(lubuskie) * os_sr1_r^3)
sk_sr1

sk_sr2 = sum((sr_wielkopolskie$mids - xsr_sr2)^3) /
  (length(wielkopolskie) * os_sr2_r^3)
sk_sr2

# kurtoza szereg szczegółowy
kur_ss1 = kurtosis(lubuskie); kur_ss1
kur_ss2 = kurtosis(wielkopolskie); kur_ss2

# kurtoza szereg rozdzielczy
kur_sr1 = sum((sr_lubuskie$mids - xsr_sr1)^4) /
  (length(lubuskie) * os_sr1_r^4)
kur_sr1

kur_sr2 = sum((sr_wielkopolskie$mids - xsr_sr2)^4) /
  (length(wielkopolskie) * os_sr2_r^4)
kur_sr2

# odchylenie przeciętne od średniej szereg szczegółowy
d1_ss1 = mean(abs(lubuskie - xsr_ss1));d1_ss1
d1_ss2 = mean(abs(wielkopolskie - xsr_ss2));d1_ss2 

# odchylenie przeciętne od średniej szereg rozdzielczy
d1_sr1 = sum(abs(sr_lubuskie$mids - xsr_sr1) * sr_lubuskie$counts) /
  sum(sr_lubuskie$counts);d1_sr1

d1_sr2 = sum(abs(sr_wielkopolskie$mids - xsr_sr2) * sr_wielkopolskie$counts) /
  sum(sr_wielkopolskie$counts);d1_sr2

# odchylenie przeciętne od mediany szereg szczegółowy
d2_ss1 = mean(abs(lubuskie - m_ss1));d2_ss1
d2_ss2 = mean(abs(wielkopolskie - m_ss2));d2_ss2 

# odchylenie przeciętne od mediany szereg rozdzielczy
d2_sr1 = sum(abs(sr_lubuskie$mids - m_sr1) * sr_lubuskie$counts) /
  sum(sr_lubuskie$counts);d2_sr1

d2_sr2 = sum(abs(sr_wielkopolskie$mids - m_sr2) * sr_wielkopolskie$counts) /
  sum(sr_wielkopolskie$counts);d2_sr2

# odchylenie ćwiartkowe szereg szczegółowy
Q_ss1 = (q3_ss1 - q1_ss1) / 2;Q_ss1 
Q_ss2 = (q3_ss2 - q1_ss2) / 2;Q_ss2


# odchylenie ćwiartkowe szereg rozdzielczy
Q_sr1 = (q3_sr1 - q1_sr1) / 2;Q_sr1
Q_sr2 = (q3_sr2 - q1_sr2) / 2;Q_sr2

# współczynnik zmienności szereg szczegółowy
v_ss1 = (os_ss1 / xsr_ss1) * 100;v_ss1
v_ss2 = (os_ss2 / xsr_ss2) * 100;v_ss2

# współczynnik zmienności szereg rozdzielczy
v_sr1 = (os_sr1_r / xsr_sr1) * 100;v_sr1
v_sr2 = (os_sr2_r / xsr_sr2) * 100;v_sr2

# pozycyjny współczynnik zmienności szereg szczegółowy
vq_ss1 = ((q3_ss1 - q1_ss1) / (2 * m_ss1)) * 100;vq_ss1
vq_ss2 = ((q3_ss2 - q1_ss2) / (2 * m_ss2)) * 100;vq_ss2

# pozycyjny współczynnik zmienności szereg rozdzielczy
vq_sr1 = ((q3_sr1 - q1_sr1) / (2 * m_sr1)) * 100;vq_sr1
vq_sr2 = ((q3_sr2 - q1_sr2) / (2 * m_sr2)) * 100;vq_sr2

# eksces szereg szczegółowy
g2_ss1 = kur_ss1 - 3;g2_ss1
g2_ss2 = kur_ss2 - 3;g2_ss2