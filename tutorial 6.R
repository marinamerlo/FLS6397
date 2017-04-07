##Tutorial 6##
install.packages("data.table")
install.packages("dplyr")
library(data.table)
library(dplyr)

##aparentemente agora tem um pacote pros dois!
install.packages("dtplyr")
library(dtplyr)

setwd("C:/Users/6837258/Documents/FLS6397")
unzip("201701_BolsaFamiliaSacado.zip")
list.files()
file.remove("201701_BolsaFamiliaSacado.zip")

saques <- fread("201701_BolsaFamiliaSacado.csv", encoding = "Latin-1")

set.seed(20170208)
saques = saques[sample(nrow(saques), 10000), ]

head(saques)
dim(saques)
names(saques)

saques <- saques %>% 
  rename(nis = `NIS Favorecido`, uf = UF, munic = `Nome Município`, mes = `Mês Referência Parcela`) %>%
  mutate(valor = as.numeric(gsub(",", "", saques$`Valor Parcela`)))


saques %>% group_by(uf) %>% summarise(contagem = n())
hist(saques$valor, main = "Distribuição dos valores sacados em jan/2017", xlab = "R$", ylab = "Frequência")

saques %>% group_by(uf) %>% summarise(valores = sum(valor))

## # ... with 17 more rows -> como ampliar e ver tudo? porque ele dá assim? 

library(readr)
saques_amostra_201701 <- read_delim("https://raw.githubusercontent.com/leobarone/FLS6397/master/data/saques_amostra_201701.csv", delim = ";", col_names = T)

##dúvida: é um output normal?
#Parsed with column specification:
 # cols(
  #  UF = col_character(),
  #  `Código SIAFI Município` = col_integer(),
   # `Nome Município` = col_character(),
    #`Código Função` = col_integer(),
    #`Código Subfunção` = col_integer(),
# `Código Programa` = col_integer(),
# `Código Ação` = col_integer(),
# `NIS Favorecido` = col_double(),
#  `Nome Favorecido` = col_character(),
# `Fonte-Finalidade` = col_character(),
#  `Mês Referência Parcela` = col_integer(),
#  `Valor Parcela` = col_double(),
#  `Mês Competência` = col_character(),
#  `Data do Saque` = col_character()
#  )
#Warning: 1 parsing failure.
#row           col               expected  actual
#7401 Valor Parcela no trailing characters ,022.00

glimpse(saques_amostra_201701)

names(saques_amostra_201701)

saques_amostra_201701 <- rename(saques_amostra_201701, uf = UF, munic = `Nome Município`)

#sempre pula linha depois do pipe? 

saques_amostra_201701 <- saques_amostra_201701 %>% 
  rename(cod_munic = `Código SIAFI Município`,
         nome = `Nome Favorecido`, 
         valor = `Valor Parcela`, 
         mes = `Mês Competência`,
         data_saque = `Data do Saque`)
glimpse(saques_amostra_201701)

saques_amostra_201701 <- select(saques_amostra_201701, uf, munic, cod_munic, nome, valor, mes, data_saque)

##porque aqui precisou por dois pipes? só um nao dá?
saques_amostra_201701 <- saques_amostra_201701 %>% 
  rename(uf = UF, munic = `Nome Município`,
         cod_munic = `Código SIAFI Município`, nome = `Nome Favorecido`,
         valor = `Valor Parcela`, mes = `Mês Competência`, data_saque =`Data do Saque`)  %>%
  select(uf, munic, cod_munic, nome, valor, mes, data_saque)

dim(saques_amostra_201701)


saques_amostra_201701 <- saques_amostra_201701 %>% mutate(nome_min = tolower(nome))
#o mutate sempre automaticamente cria uma nova variável? 

saques_amostra_201701 <- saques_amostra_201701 %>% 
  mutate(valor_num = gsub(",", "", valor)) %>% 
  mutate(valor_num = as.numeric(valor_num))

View(saques_amostra_201701)

saques_amostra_201701 <- saques_amostra_201701 %>% 
  mutate(valor_num = as.numeric(gsub(",", "", valor)))

saques_amostra_201701 <- saques_amostra_201701 %>% 
  mutate(valor_dolar = valor / 3.2, valor10 = valor_num + 10)

#o que é o Inf nesse caso? como ele pode variar? sempre é utilizado dentro do cut?
saques_amostra_201701 <- saques_amostra_201701 %>% 
  mutate(valor_categorico = cut(valor_num, c(0, 300, Inf), c("Baixo", "Alto")))

table(saques_amostra_201701$mes)

saques_amostra_201701 <- saques_amostra_201701 %>% 
  mutate(ano = recode(mes, "11/2016" = "2016", "12/2016" = "2016", "01/2017" = "2017"))

##Exercício

saques_amostra_201701 <- saques_amostra_201701 %>% 
  mutate(valor_categorico2 = cut(valor_num, c(0, 100, Inf), c("pouquinho", "melhorzinho")))
glimpse(saques_amostra_201701)

saques_amostra_201701 <- saques_amostra_201701 %>% 
  mutate(valor_euro = valor / 2.9)
glimpse(saques_amostra_201701)
#porque a categoria fica dbl?

#o que é o Inf nesse caso? como ele pode variar? sempre é utilizado dentro do cut?
saques_amostra_201701 <- saques_amostra_201701 %>% 
  mutate(valor_categorico3 = recode(valor_categorico, "Baixo" = "Abaixo de R$300", "Alto" = "Acima de R$300"))
glimpse(saques_amostra_201701)
#porque a categoria fica fctr?

saques_amostra_201701 <- saques_amostra_201701 %>% 
  mutate(mes_novo = recode(mes, "11/2016" = "Novembro", "12/2016" = "Dezembro", "01/2017" = "Janeiro"))
glimpse(saques_amostra_201701)

saques_amostra_201701 <- saques_amostra_201701 %>% 
  mutate(mes_novo2 = mes,
         mes_novo2 = replace(mes, mes == "11/2016", "Novembro"),
         mes_novo2 = replace(mes, mes == "12/2016", "Dezembro"),
         mes_novo2 = replace(mes, mes == "01/2017", "Janeiro"))

##filtrando linhas

saques_amostra_ES <- saques_amostra_201701 %>% filter(uf == "ES")
saques_amostra_ES <-filter(saques_amostra_201701, uf == "ES")

saques_amostra_CO <- saques_amostra_201701 %>% 
  filter(uf == "MT" | uf == "MS" | uf == "Df" | uf == "GO")

saques_amostra_MT_2016 <- saques_amostra_201701 %>% filter(uf == "MT" & ano == "2016")

saques_janeiro <- saques_amostra_201701 %>% filter(mes_novo == "Janeiro")

saques_500 <- saques_amostra_201701 %>% filter(valor_num > 500)

saques_sul <- saques_amostra_201701 %>% filter(uf == "RS" | uf == "PR" | uf == "SC")

contagem_uf <- saques_amostra_201701 %>% 
  group_by(uf) %>% 
  summarise(contagem = n())

contagem_uf <- saques_amostra_201701 %>% 
  group_by(uf) %>% 
  summarise(contagem = n())

valores_uf <- saques_amostra_201701 %>% 
  group_by(uf) %>% 
  summarise(contagem = n(),
            soma = sum(valor),
            media = mean(valor),
            mediana = median(valor),
            desvio = sd(valor),
            minimo = min(valor),
            maximo = max(valor))

valores_mes <- saques_amostra_201701 %>% 
  group_by(mes) %>% 
  summarise(contagem = n(),
            soma = sum(valor),
            media = mean(valor))
contagem_uf_mes <- saques_amostra_201701 %>% 
  group_by(uf, mes) %>% 
  summarise(contagem = n())

valores_uf_mes <- saques_amostra_201701 %>% 
  group_by(uf, mes) %>% 
  summarise(contagem = n(),
            soma = sum(valor),
            media = mean(valor),
            desvio = sd(valor))
saques_amostra_munic <- saques_amostra_201701 %>% 
  group_by(munic) %>% 
  summarise(contagem = n(),
            soma = sum(valor),
            media = mean(valor))
valores_uf <- valores_uf %>% arrange(soma)

valores_uf <- saques_amostra_201701 %>% 
  group_by(uf) %>% 
  summarise(contagem = n(),
            soma = sum(valor),
            media = mean(valor),
            mediana = median(valor),
            desvio = sd(valor),
            minimo = min(valor),
            maximo = max(valor)) %>%
  arrange(soma)

valores_uf <- valores_uf %>% arrange(desc(soma))
valores_uf <- valores_uf %>% arrange(desc(mediana), maximo)
