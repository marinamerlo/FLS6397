### nomes <- c("Cassio Santos Pinto de Oliveira", "Marina Merlo, "Thiago de M. Queiroz Moreira")
### programa <- "Mestrado em Ciencia Politica"
### n_usp <- c(6912700, 7197987, 5934108)
### data_entrega: "12/06/2017"

#instalando e carregando os pacotes necessários:

#install.packages("XML")
library(XML)
#install.packages("SnowballC")
library(SnowballC)
#install.packages('tm')
library(tm)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("rvest")
library(rvest)
#install.packages("stringr")
library(stringr)
#install.packages("wordcloud")
library(wordcloud)

library(tidytext)
library(dplyr)
library(ggplot2)
library(tidyr)

#### PARTE 1 ####

#loop para pegar os links das páginas
url_base <- "http://www.camara.sp.gov.br/page/2/?s=vereadora"
url_base <- gsub("2", "num", url_base)
print(url_base)
noticias <- c()

#o site da câmara indica que existem 137 páginas de resultados. 
#para puxar todos, trocar o númer na primeira linha do for
#aqui, estou fazendo com menos para testar o script.
for (i in 3:13){
  print(i)
  url <- gsub("num", i, url_base)
  
  pagina <- readLines(url)
  pagina <- htmlParse(pagina)
  pagina <- xmlRoot(pagina)
  
  noticia <- xpathSApply(pagina, "//article//h1[@class='h2 entry-title']/a", xmlValue)
  
  noticias <- c(noticias, as.character(noticia))
}


#### PARTE 2 ####

noticias_wc <- Corpus(VectorSource(noticias))#vetor somente com as falas
noticias_wc  <- tm_map(noticias_wc, removePunctuation) #remove pontuação
noticias_wc <- tm_map(noticias_wc, removeNumbers) #remove números
noticias_wc <- tm_map(noticias_wc, content_transformer(tolower)) #deixa tudo em minúsculo
noticias_wc <- tm_map(noticias_wc, removeWords, stopwords("pt")) #remove stop words do português
noticias_wc <- tm_map(noticias_wc, stripWhitespace) #remove espaços em branco
noticias_wc <- tm_map(noticias_wc, removeWords, c("são","paulo", "câmara", "cmsp", "sobre", "municipal", "paulista", "vereador", "vereadora", "vereadores"))
noticias_wc <- tm_map(noticias_wc , PlainTextDocument) #transforma tudo num texto só
#fazendo o gráfico
par(bg='black')
graf.vereadoras <- wordcloud(noticias_wc,scale=c(3,0.3), min.freq = 1, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(11, "BuPu"))

#### PARTE 3 ####

noticias_df <- data_frame(id_noticia = 1:length(noticias), 
                           text = noticias)

glimpse(noticias_df)

noticias_token <- noticias_df %>%
  unnest_tokens(word, text)
glimpse(noticias_token)

#está dando problema! Não exclui as palavras...
stopwords_pt <- c(stopwords("pt"), "são","paulo", "câmara", "cmsp", 
                  "sobre", "municipal", "paulista", "vereador", 
                  "vereadora", "vereadores", "é")
stopwords_pt_df <- data.frame(word = stopwords_pt)

noticias_token <- noticias_token %>%
  anti_join(stopwords_pt_df, by = "word")

noticias_token %>%
  count(word, sort = TRUE)

noticias_token %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n)) %>%
  filter(n>5) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() + 
  labs(title ="Frequência de palavras", x = "Palavra", y = "Frequência") + 
  theme_minimal()

par(bg='white')
noticias_token %>%
  count(word, sort = TRUE) %>%
  with(wordcloud(word, n, max.words = 50))
