install.packages("tm", "sentimentr", "ggplot2", "dplyr", "proxy", "stringr", "stringi")
library("tm")
library("sentimentr")
library("ggplot2")
library("dplyr")
library("proxy")
library("stringr")
library("stringi")

#------------------------------------------------------------------------------------------------
# Zadanie 1
#------------------------------------------------------------------------------------------------
moja_pg_funkcja_wczytanie_danych_z_wielu_plikow <- function(nazwa_folderu = ""){
  
  sciezka <- paste0("/",nazwa_folderu)
  
  sciezka_all <- paste0(getwd(), sciezka)

  tekst_zrodlo <- DirSource(sciezka_all, encoding = "UTF-8", mode = "text")

  tekst_korpus <- VCorpus(tekst_zrodlo)
}

setwd("C:/Users/Anita/Documents/analiza danych i text mining/egzamin/egzamin")
getwd()
hrabia_wczytany_do_korpusu <- moja_pg_funkcja_wczytanie_danych_z_wielu_plikow("hrabia10")
hrabia_wczytany_do_korpusu
hrabia_wczytany_do_korpusu[[10]]$content


moja_pg_funkcja_czysc_korpus <- function(korpus_do_czyszczenia){

  korpus_do_czyszczenia <- tm_map(korpus_do_czyszczenia, content_transformer(tolower))

  korpus_do_czyszczenia <- tm_map(korpus_do_czyszczenia, removePunctuation)
  
  korpus_do_czyszczenia <- tm_map(korpus_do_czyszczenia, removeNumbers)

  korpus_do_czyszczenia <- tm_map(korpus_do_czyszczenia, removeWords, stopwords("en"))
  
  korpus_do_czyszczenia <- tm_map(korpus_do_czyszczenia, stripWhitespace)
  
  korpus_do_czyszczenia <- tm_map(korpus_do_czyszczenia, content_transformer(function(x) trimws(x)))

  korpus_do_czyszczenia <- tm_map(korpus_do_czyszczenia, content_transformer(function(x) stri_remove_empty(x, na_empty = FALSE)))
}

hrabia_oczyszczony_korpus <- moja_pg_funkcja_czysc_korpus(hrabia_wczytany_do_korpusu)
hrabia_oczyszczony_korpus[[10]]$content




#------------------------------------------------------------------------------------------------
# Zadanie 2
#------------------------------------------------------------------------------------------------
moja_pg_funkcja_steeming_korpusu <- function(korpus_do_steemingu){
  
  korpus_do_steemingu <- tm_map(korpus_do_steemingu, stemDocument)

  macierz_term_dokument <- TermDocumentMatrix(korpus_do_steemingu)
}
hrabia_macierz_term_dokument <- moja_pg_funkcja_steeming_korpusu(hrabia_oczyszczony_korpus)
hrabia_macierz_dokument_term <- as.DocumentTermMatrix(hrabia_macierz_term_dokument)


moja_pg_funkcja_analiza_korespondencji<- function(macierz_dokument_term_do_analizy){
  
  analiza_matrix <- as.matrix(macierz_dokument_term_do_analizy)
  
  wynik <- ca(analiza_matrix)
}

przyslowia_dokument_term <- f_przeksztalc_wektor_na_macierz_dokument_term(przyslowia_lemy)
wynik <- moja_pg_funkcja_analiza_korespondencji(przyslowia_dokument_term)

summary(wynik)
plot(wynik)
fviz_ca(wynik, repel = TRUE)



#------------------------------------------------------------------------------------------------
# Zadanie 3
#------------------------------------------------------------------------------------------------
moja_pg_funkcja_wyodrebnij_zdania<- function(wektor_do_wyodrebniania){
  return (get_sentences(wektor_do_wyodrebniania))
}

rozdzial_1 <- hrabia_oczyszczony_korpus[[1]]$content
rozdzial_1_characters <- as.character(rozdzial_1)
hrabia_wyodrebnione_zdanie <- moja_pg_funkcja_wyodrebnij_zdania(rozdzial_1_characters)


moja_pg_funkcja_analiza_sentymentu <- function(wektor_do_sentymentu){
  return (sentiment(wektor_do_sentymentu, polarity_dt = lexicon::hash_sentiment_huliu))
}

wynik_analizy_sentymentu <- moja_pg_funkcja_analiza_sentymentu(hrabia_wyodrebnione_zdanie)

moja_pg_funkcja_rysuj_wykres_analizy_sentymentu <- function(pg_sentymenty){
  moje_kolory <- c("deeppink", "gold", "green3")
  
  pg_sentymenty %>%
    mutate(kolor = ifelse(sentiment == 0, "Neutralna", ifelse(sentiment < 0, "Negatywna", "Pozytywna"))) %>%
    ggplot(aes(element_id, sentiment, fill = kolor, color = kolor)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = moje_kolory) +
    scale_color_manual(values = moje_kolory) +
    labs(x = "Opinie", y = "Ocena sentymentu") +
    theme_minimal()
}
moja_pg_funkcja_rysuj_wykres_analizy_sentymentu(wynik_analizy_sentymentu)



#------------------------------------------------------------------------------------------------
# Zadanie 4
#------------------------------------------------------------------------------------------------
hrabia_macierz_term_dokument <- moja_pg_funkcja_steeming_korpusu(hrabia_oczyszczony_korpus)
hrabia_macierz_dokument_term <- as.DocumentTermMatrix(hrabia_macierz_term_dokument)


moja_pg_funkcja_oblicz_odleglosci_miedzy_dokumentami<- function(macierz_dokument_term_do_obliczen){
  
  dokumenty_data_matrix <- as.matrix(macierz_dokument_term_do_obliczen)
  odl <- dist(dokumenty_data_matrix, method = "jaccard")
}

hrabia_odleglosci <- moja_pg_funkcja_oblicz_odleglosci_miedzy_dokumentami(hrabia_macierz_dokument_term)
hrabia_odleglosci

hg <- hclust(hrabia_odleglosci, method = "ward.D")
hg_ladne <- as.dendrogram(hg)
hg_ladne <- color_labels(hg_ladne,3, col = c("royalblue", "orangered", "seagreen"))
hg_ladne <- color_branches(hg_ladne,3, col = c("royalblue", "orangered", "seagreen"))
plot(hg_ladne, main = paste("Dendogram na podstawie miary:\n", "Jaccard"))
rect.dendrogram(hg_ladne, k = 3, border = "grey20", lty = 2)