# PODPUNKT 3.
setwd("D:/POLITECHNIKA/SEMESTR 4/STATYSTYKA/P/PROJEKT W R/projekt/Projekt")

# source('wczytaj_dane.R', echo = TRUE)
source('wczytaj_dane.R')
source('funkcje_p2.R')
lubuskie_sort = sort(lubuskie)
wielkopolskie_sort = sort(wielkopolskie)

# Wartości średnie
m_lubuskie = as.double(wl["wsss"])
m_wielkopolskie = as.double(ww["wsss"])

# Odchylenia standardowe
sig_lubuskie = as.double(wl["osss"])
sig_wielkopolskie = as.double(ww["osss"])

# -- Obliczenia statystyki Kołmogorowa --

# Wartości standaryzowane (xi-m)/sigma
standaryz_x_lubuskie = (lubuskie_sort - m_lubuskie) / sig_lubuskie
standaryz_x_wielkopolskie = (wielkopolskie_sort - m_wielkopolskie) / sig_wielkopolskie

# Wyliczenia granicy prawostronnej dystrybuanty empirycznej: i/n i=1,2,3.....,n
i_nad_n_l =(1:length(lubuskie_sort)) / length(lubuskie_sort) 
i_nad_n_w =(1:length(wielkopolskie_sort)) / length(wielkopolskie_sort)

# Wyliczenia granicy lewostronnej dystrybuanty empirycznej: (i-1)/n i=1,2,3.....,n
i_odj_1_nad_n_l =((1:length(lubuskie_sort)) - 1) / length(lubuskie_sort)
i_odj_1_nad_n_w =((1:length(wielkopolskie_sort)) - 1) / length(wielkopolskie_sort)

# Dystrybuanta F0
x1 = 1:length(lubuskie_sort)
x2 = 1:length(wielkopolskie_sort)
F0_lubuskie = pnorm(lubuskie_sort, mean = m_lubuskie, sd = sig_lubuskie)
F0_wielkopolskie = pnorm(wielkopolskie_sort, mean = m_wielkopolskie, sd = sig_wielkopolskie)

# |i/n - F0|
d_plus_l = abs(i_nad_n_l - F0_lubuskie)
d_plus_w = abs(i_nad_n_w - F0_wielkopolskie)

# |(i-1)/n - F0|
d_minus_l = abs(i_odj_1_nad_n_l - F0_lubuskie)
d_minus_w = abs(i_odj_1_nad_n_w - F0_wielkopolskie)

# d+54 (lubuskie) i d+50 (wielkopolskie)
d_plus_54_l = max(d_plus_l)
d_plus_50_w = max(d_plus_w)

# d-54 (lubuskie) i d-50 (wielkopolskie)

d_minus_54_l = max(d_minus_l)
d_minus_50_w = max(d_minus_w)

# D54 (lubuskie) i D50(wielkopolskie)

D_54_l = max(d_plus_54_l, d_minus_54_l);D_54_l
D_50_w = max(d_plus_50_w, d_minus_50_w);D_50_w

# Wartość krytyczna statystyki D54 (lubuskie) odczytana z tablic rozkładu Kołmogorowa 
# dla przyjętego poziomu istotności wynosi k54 = 0,886/sqrt(54)

k54 = 0.120

# Wartość krytyczna statystyki D50 (wielkopolskie) odczytana z tablic rozkładu Kołmogorowa 
# dla przyjętego poziomu istotności wynosi k50 = 0,886/sqrt(50)

k50 = 0.125

# Test Kołmogorowa lubuskie
cat('=================================================================================================================================================\n')
cat('WYNIK TESTU KOŁMOGOROWA-LILLIEFORSA DLA WOJEWÓDZTWA LUBUSKIEGO\n')
cat(format("Statystyka testowa Dn =", justify = 'right', width = 44), sep = '', format(D_54_l, width = 12, justify = 'right'), '\n')
cat(format("Przedział krytyczny =", justify = 'right', width = 44), sep = '', format('<', width = 5, justify = 'right'), format(k54, width = 1, justify = 'right'), format(', ', width = 1, justify = 'right'), format(1, width = 1, justify = 'right'), format('>', width = 1, justify = 'right'),'\n\n')
wynik_lubuskie = (D_54_l >= k54 & D_54_l <= 1)
if(wynik_lubuskie)
{
  cat ('Test Kołmogorowa odrzuca hipotezę zerową, że rozkład zawartości cukru w procentach w dostawach buraków cukrowych ma rozkład normalny')
}else
{
  cat ('Test Kołmogorowa nie odrzuca hipotezy zerowej, że rozkład zawartości cukru w procentach w dostawach buraków cukrowych ma rozkład normalny')
}
cat('\n=================================================================================================================================================\n')
# wartość statystyki D_54_l należy do przedziału krytycznego więc odrzuca hipoteze zerową,
# mówiącą o tym, że rozkład zawartości cukru w procentach u dostawców w województwie lubuskim jest rozkładem normalnym



# Test Kołmogorowa wielkopolskie
cat('=================================================================================================================================================\n')
cat('WYNIK TESTU KOŁMOGOROWA-LILLIEFORSA DLA WOJEWÓDZTWA WIELKOPOLSKIEGO\n')
cat(format("Statystyka testowa Dn =", justify = 'right', width = 44), sep = '', format(D_50_w, width = 12, justify = 'right'), '\n')
cat(format("Przedział krytyczny =", justify = 'right', width = 44), sep = '', format('<', width = 5, justify = 'right'), format(k50 , width = 1, justify = 'right'), format(', ', width = 1, justify = 'right'), format(1, width = 1, justify = 'right'), format('>', width = 1, justify = 'right'),'\n\n')
wynik_wielkopolskie = (D_50_w >= k50 & D_50_w <= 1)
if(wynik_wielkopolskie)
{
  cat ('Test Kołmogorowa odrzuca hipotezę zerową, że rozkład zawartości cukru w procentach w dostawach buraków cukrowych ma rozkład normalny')
}else
{
  cat ('Test Kołmogorowa nie odrzuca hipotezy zerowej, że rozkład zawartości cukru w procentach w dostawach buraków cukrowych ma rozkład normalny')
}
cat('\n=================================================================================================================================================\n')
# wartość statystyki D_50_w należy do przedziału krytycznego więc odrzuca hipoteze zerową,
# mówiącą o tym, że rozkład zawartości cukru w procentach u dostawców w województwie wielkopolskim jest rozkładem normalnym

