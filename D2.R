install.packages("tidyverse")
library(rvest)
library(XML)
library(tidyverse)

#raspando a lista de links
links <- html("http://www.ipu.org/wmn-e/classif-arc.htm")%>% 
  html_nodes("a") %>% 
  html_attr("href")
#selecionando apenas os links que contém as páginas que queremos raspar
#links <- links[4:226]
links <- links[4:15]
links


#loop pra juntar tudo
baseurl <- "http://www.ipu.org"
dados <- data.frame()
for (link in links) {
  url <- paste(baseurl, link, sep = "")
  lista.tabelas <- readHTMLTable(url)
  tabela <- as.data.frame(lista.tabelas[[1]],stringsAsFactors = FALSE) %>%
            filter(complete.cases(.))
    dados <- bind_rows(tabela) %>%
      mutate(id = link) %>%
      mutate(id = gsub("/wmn-e/arc/classif", "", id)) %>%
      mutate(id = gsub(".htm", "", id)) %>%
      separate(id, into = c("date", "year"), sep = 4) %>%
      separate(date, into = c("day", "month"), sep = 2)
}

#dar nomes pras colunas
#tirar os % pra virar númer
#transformar v4 e v7 - elections - em data separada. tirar os pontos e usar separate
#tirar v1
#checar nomes



####testes#####

tabela$id <- link

tabela <- tabela %>%
    mutate(id = gsub("/wmn-e/arc/classif", "", id)) %>%
    mutate(id = gsub(".htm", "", id)) %>%
    separate(id, into = c("date", "year"), sep = 4) %>%
    separate(date, into = c("day", "month"), sep = 2)
    



##transformando
dates <- as.data.frame(links)
dates$fulldate<- gsub("/wmn-e/arc/classif", "", dates$links)
dates$fulldate<- gsub(".htm", "", dates$fulldate)
dates <- dates %>%
  mutate(fulldate2 = fulldate) %>%
  separate(fulldate2, into = c("date", "year"), sep = 4) %>%
  separate(date, into = c("day", "month"), sep = 2)
