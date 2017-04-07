library(dplyr)
library(readr)
pagamentos11 <- read_delim("https://raw.githubusercontent.com/leobarone/FLS6397/master/data/pagamentos11.csv", delim = ";", col_names = T)
pagamentos17 <- read_delim("https://raw.githubusercontent.com/leobarone/FLS6397/master/data/pagamentos17.csv", delim = ";", col_names = T)

##Renomeie as variáveis "NIS Favorecido", "Nome Favorecido" e "Valor Parcela" 
##para "nis", "nome" e "valor", repectivamente.

#tem como mudar pros dois ao mesmo tempo, se eles tem colunas iguais?
pagamentos11 <- pagamentos11 %>% 
  rename(nis = `NIS Favorecido`,
         nome = `Nome Favorecido`, 
         valor = `Valor Parcela`)
glimpse(pagamentos11)

pagamentos17 <- pagamentos17 %>% 
  rename(nis = `NIS Favorecido`,
         nome = `Nome Favorecido`, 
         valor = `Valor Parcela`)
glimpse(pagamentos17)


##Transforme a variável valor em numérica.
#nao entendi isso da vírgula. parece que já tava certo...
#e o tipo <dbl> é igual o <num> ou o <int>?

pagamentos11 <- pagamentos11 %>% 
  mutate(valor_num = gsub(",", "", valor), valor_num = as.numeric(valor_num))
glimpse(pagamentos11)

pagamentos17 <- pagamentos17 %>% 
  mutate(valor_num = gsub(",", "", valor), valor_num = as.numeric(valor_num))
glimpse(pagamentos17)

##Selecione apenas as três variáveis renomeadas.

pagamentos11 <- pagamentos11 %>% select(nis, nome, valor)
dim(pagamentos11)

pagamentos17 <- pagamentos17 %>% select(nis, nome, valor)
dim(pagamentos17)

##Quantas linhas tem cada base de dados?
dim(pagamentos11)
#39 linhas
dim(pagamentos17)
#48 linhas


###LEFT E RIGHT JOIN###

#variavel.x = primeira tabela (pagamentos11), variavel.y = segunda tabela (pagamentos17)
comb_left <- left_join(pagamentos11, pagamentos17, by = "nis") %>%
 rename(valor11 = valor.x, nome11 = nome.x, valor17 = valor.y, nome17 = nome.y)

comb_right <- right_join(pagamentos11, pagamentos17, by = "nis") %>%
  rename(valor11 = valor.x, nome11 = nome.x, 
         valor17 = valor.y, nome17 = nome.y)

##inner e full

comb_inner <- inner_join(pagamentos11, pagamentos17, by = "nis")
comb_inner <- comb_inner %>% rename(valor11 = valor.x, nome11 = nome.x, 
                                    valor17 = valor.y, nome17 = nome.y)

comb_full <- full_join(pagamentos11, pagamentos17, by = "nis")
comb_full <- comb_full %>% rename(valor11 = valor.x, nome11 = nome.x, 
                                  valor17 = valor.y, nome17 = nome.y)
comb_semi <- semi_join(pagamentos11, pagamentos17, by = "nis")


###exercício####

library(readr)
saques_amostra_201701 <- read_delim("https://raw.githubusercontent.com/leobarone/FLS6397/master/data/saques_amostra_201701.csv", delim = ";", col_names = T)

saques_amostra_201701 <- saques_amostra_201701 %>% 
  rename(uf = UF, 
         munic = `Nome Município`,
         cod_munic = `Código SIAFI Município`, 
         nome = `Nome Favorecido`,
         valor = `Valor Parcela`, 
         mes = `Mês Competência`, 
         data_saque =`Data do Saque`,
         nis = `NIS Favorecido`)  %>%
  select(uf, munic, cod_munic, nome, valor, mes, data_saque, nis) %>%
  mutate(valor_num = as.numeric(gsub(",", "", valor)))

valor_mun <- saques_amostra_201701 %>%
  group_by(munic) %>% 
  summarise(contagem = n(),
            soma_mun = sum(valor),
            media_mun = mean(valor),
            mediana_mun = median(valor),
            desvio_mun = sd(valor),
            minimo_mun = min(valor),
            maximo_mun = max(valor))

saques_amostra_201701 <- saques_amostra_201701 %>% 
left_join(valor_mun, by = "munic")


valores_UF <- saques_amostra_201701 %>% 
  group_by(uf) %>% 
  summarise(contagem = n(),
            soma_uf = sum(valor_num),
            media_uf = mean(valor_num),
            mediana_uf = median(valor_num),
            desvio_uf = sd(valor_num),
            minimo_uf = min(valor_num),
            maximo_uf = max(valor_num))
saques_amostra_201701 <- saques_amostra_201701 %>% 
  left_join(valores_UF, by = "uf")

##exercício
saques_amostra_201701 <- saques_amostra_201701 %>% 
  mutate(percen_ind = valor / soma_uf)
glimpse(saques_amostra_201701)
