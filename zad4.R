#przecietna zawartosc cukru w procentach w regionie
przedzialowo_cukier_region <- function(wektor_z4)
{
  # wektor_z4=wektor_lubuskie;
  #ilosc pomiarow
  z4_n=length(wektor_z4);
  
  if (z4_n < 30) stop("nie zaimplemetowano algorytmu dla malych danych")
  
  #srednia z pomiarow
  srednia_z4=(sum(wektor_z4))/z4_n;
  
  #wsp ufnosci dla alfa=0.95
  tablica_095=1.96;
  
  #wariancja
  wariancja_z4 <- sum((wektor_z4 - srednia_z4)^2) / (z4_n - 1)
  
  #odchylenie standardowe
  s_z4 <- sqrt(wariancja_z4)
  
  #blad maks
  bladMaks_z4=tablica_095*s_z4/sqrt(z4_n);
  
  #dolna granica
  min_z4=srednia_z4-bladMaks_z4;
  
  #gorna granica
  max_z4=srednia_z4+bladMaks_z4;
  
  #wzgledna precyzja
  wzgl_prec_z4 = bladMaks_z4*100/srednia_z4;
  
  
  #sprawdzenie czy obliczona precyzja przekracza prog 5%
  if(wzgl_prec_z4<5){
    mozna_z4="Uzyskano wysoka precyzje, mozna uogolnic wyniki na cale wojewodztwo.";
  }
  else{
    mozna_z4="Uzyskano niewystarczajaca precyzje, aby uogonic wyniki na cale wojewodztwo.";
  }
  
  #zwracanie wynikow
  return_z4=list(
    dolna_granica=min_z4,
    gorna_granica=max_z4,
    precyzja=wzgl_prec_z4,
    analiza=mozna_z4
  )
  return_z4;
}



# zajumana funkcja do ładnego wyświetlania wyników
wypisz_z4 <- function(lista, nazwa_regionu) {
  cat("\n====================================================\n")
  cat(sprintf("SZACOWANIE PROCENTOWEJ ZAWARTOSCI CUKRU W %s\n", nazwa_regionu))
  cat("====================================================\n")
  cat(sprintf("Minimum przedzialu:          %.4f%%\n", lista$dolna_granica))
  cat(sprintf("Maksimum przedzialu:         %.4f%%\n", lista$gorna_granica))
  cat(sprintf("Wzgledna precyzja:           %.2f%%\n", lista$precyzja))
  cat(sprintf(lista$analiza))
  cat("\n====================================================\n")
}





