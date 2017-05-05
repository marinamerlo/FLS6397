#install.packages("rvest")
#install.packages("XML")
#install.packages("tidyverse")
library(rvest)
library(XML)
library(tidyverse)

setwd("D:/Dropbox/Mestrado/Disciplinas/Programacao")

#raspando a lista de links
links <- html("http://www.ipu.org/wmn-e/classif-arc.htm")%>% 
  html_nodes("a") %>% 
  html_attr("href")

# preparando para o loop: cria a url completa e cria um data frame vazio para receber os dados
baseurl <- "http://www.ipu.org" 

#entre 1997 e 2008, a tabela que contém os dados é a de [3]. De 2009 em diante, é a [1].
#como a estrutura do site é diferente entre os anos, precisaremos raspar os anos em duas etapas.

#dataframe para poder selecionar os anos mais fácil:
anos <- as.data.frame(links[4:226]) %>%
  rename(id = `links[4:226]`) %>%
  mutate(links = id) %>%
  mutate(id = gsub("/wmn-e/arc/classif", "", id)) %>%
  mutate(id = gsub(".htm", "", id)) %>%
  separate(id, into = c("date", "data_year"), sep = 4) %>%
  separate(date, into = c("data_day", "data_month"), sep = 2) %>%
  mutate(data_year = ifelse(data_year >= 97, paste0("19", data_year), paste0("20",data_year))) %>%
  mutate_each(funs(as.numeric), -links)

##selecionando os links anteriores a 2008. Seleciona janeiro ou fevereiro de cada ano para rodar mais rápido.
selecao <- anos %>%
  filter(data_year <= 2008 & (data_month == 1 | data_month == 2 ))

#retorna pra lista os links selecionados
links <- as.list(selecao$links)
baseurl <- "http://www.ipu.org" 
dados2008 <- data.frame()

#fazendo o loop para a seleção dos links de 1997 a 2008
for (link in links) { #para cada link que está na lista de links
    url <- paste(baseurl, link, sep = "") #criar a URL completa
    lista.tabelas <- readHTMLTable(url) #extrair as tabelas da página
    tabela <- tbl_df(lista.tabelas[[3]]) %>% #cria um data frame a partir da tabela que contém os dados
      filter(complete.cases(.)) %>% #limpa os dados, deixando apenas as linhas com as observações e não outras colunas
      mutate(id = link) #cria uma coluna com o link de cada página, para poder mais adiante ter a informação da data da coleta do dado
    dados2008 <- bind_rows(dados2008, tabela) #empilha o data frame de cada link com o que já foi criado
}


#fazendo para 2009 em diante:
##selecionando os links anteriores a 2008. Seleciona janeiro ou fevereiro de cada ano para rodar mais rápido.
selecao <- anos %>%
  filter(data_year >= 2009 & (data_month == 1 | data_month == 2))

#retorna pra lista os links selecionados
links <- as.list(selecao$links)
dados2009 <- data.frame()

#fazendo o loop para a seleção dos links de 1997 a 2008
for (link in links) { #para cada link que está na lista de links
  url <- paste(baseurl, link, sep = "") #criar a URL completa
  lista.tabelas <- readHTMLTable(url) #extrair as tabelas da página
  tabela <- tbl_df(lista.tabelas[[3]]) %>% #cria um data frame a partir da tabela que contém os dados
    filter(complete.cases(.)) %>% #limpa os dados, deixando apenas as linhas com as observações e não outras colunas
    mutate(id = link) #cria uma coluna com o link de cada página, para poder mais adiante ter a informação da data da coleta do dado
  dados2009 <- bind_rows(dados2009, tabela) #empilha o data frame de cada link com o que já foi criado
}

dados <- bind_rows(dados2009, dados2008)


##arumando os dados
teste <- dados %>%
  mutate(id = gsub("/wmn-e/arc/classif", "", id)) %>%
  mutate(id = gsub(".htm", "", id)) %>%
  separate(id, into = c("date", "data_year"), sep = 4) %>%
  separate(date, into = c("data_day", "data_month"), sep = 2) %>%
  mutate(data_year = ifelse(data_year >= 97, paste0("19", data_year), paste0("20",data_year))) %>%
  select(-V1, -V6, -V10) %>%
  rename(country = V2,
         lower_elections = V3,
         lower_seats = V4,
         lower_seats_w = V5,
         upper_elections = V7,
         upper_seats = V8,
         upper_seats_w = V9) %>%
  mutate(country = gsub('[0-9]+', "", country)) %>%
  mutate(lower_seats = as.numeric(lower_seats)) %>%
  mutate(lower_seats_w = as.numeric(lower_seats_w)) %>%
  mutate(lower_seats_w_per = as.numeric(lower_seats_w / lower_seats)) %>%
  mutate(upper_seats = as.numeric(upper_seats)) %>%
  mutate(upper_seats_w = as.numeric(upper_seats_w)) %>%
  mutate(upper_seats_w_per = as.numeric(upper_seats_w / upper_seats)) %>%
  mutate(data_year = as.numeric(data_year))

summary(teste$data_year)
write.csv2(teste, "dados.csv", row.names = F)

#criando variáveis de média de mulheres na Upper e Lower houses por ano:
lower_mean <- teste %>%
  group_by(country, data_year) %>%
  summarise(lower_seats_w_ymean = mean(lower_seats_w_per)) 

#fazendo um gráfico com todos (fica sujo, mas só pra ver que as observações estão contínuas ao longo dos anos)
p1 <- ggplot() + 
  geom_line(aes(y = lower_seats_w_ymean, x = as.numeric(data_year), group = as.factor(country)),
                           data = lower_mean, stat="identity") + 
  geom_point()
p1
