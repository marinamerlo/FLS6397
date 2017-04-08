### nome <- "Marina Merlo"
### programa <- "Mestrado em Ciência Política"
### n_usp <- 7197987
### data_entrega: "xx/xx/2017"

#configurando a pasta em que os arquivos serão salvos
setwd("Y:/Marina/FLS6937/Desafio1")
#abrindo os pacotes que vou usar usar. Eles já estavam instalados. 
#install.packages("readr")
#install.packages("dplyr")
library(readr)
library(dplyr)

########################################
###### Parte 1 - abrindo os dados ######
########################################

#baixando o arquivo com os resultados eleitorais de 2016
url_result <- "http://agencia.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona/votacao_candidato_munzona_2016.zip"
download.file(url_result, "temp.zip", quiet = F)
#descompactando o arquivo e removendo o .zip da pasta
unzip("temp.zip")
file.remove("temp.zip")

#baixando o arquivo com os dados de candidaturas
url_cand <- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_2016.zip"
download.file(url_cand, "temp.zip", quiet = F)
#descompactando o arquivo e removendo o .zip da pasta
unzip("temp.zip")
file.remove("temp.zip")


##selecionando os arquivos da região Sudeste
#(troquei o Sul pelo Sudeste porque tava pra montar o banco do município de São Paulo pra minha dissertação)

#criando uma lista de todos os arquivos contidos na pasta
lista.arquivos <-list.files(file.path(getwd()))
print(lista.arquivos)
#criando uma lista para pegar somente os documentos de votação
lista.resultados <- grep(pattern="votacao_candidato_munzona_2016_", lista.arquivos, value=TRUE)
print(lista.resultados)

#pegando somente os arquivos dos estados do Sudeste
lista.resultados <- lista.resultados[c(7,10,18,25)]
print(lista.resultados)

#criando o dataframe vazio que receberá os dados
resultados <- data.frame()

#Loop para coletar os dados que queremos:
#vai abrir cada uma das listas, renomear as colunas de acordo com o indicado no arquivo LEIAME.
#incluir no dataframe vazio

for(arquivo in lista.resultados){
  print (arquivo)
  d <- read_delim(file.path(getwd(),arquivo), 
                      col_names = c("DATA_GERACAO",
                                    "HORA_GERACAO",
                                    "ANO_ELEICAO", 
                                    "NUM_TURNO",
                                    "DESCRICAO_ELEICAO",
                                    "SIGLA_UF",
                                    "SIGLA_UE",
                                    "CODIGO_MUNICIPIO",
                                    "NOME_MUNICIPIO",
                                    "NUMERO_ZONA",
                                    "CODIGO_CARGO",
                                    "NUMERO_CAND", 
                                    "SEQUENCIAL_CANDIDATO",
                                    "NOME_CANDIDATO",
                                    "NOME_URNA_CANDIDATO",
                                    "DESCRICAO_CARGO",
                                    "COD_SIT_CAND_SUPERIOR",
                                    "DESC_SIT_CAND_SUPERIOR",
                                    "CODIGO_SIT_CANDIDATO",
                                    "DESC_SIT_CANDIDATO",
                                    "CODIGO_SIT_CAND_TOT",
                                    "DESC_SIT_CAND_TOT",
                                    "NUMERO_PARTIDO",
                                    "SIGLA_PARTIDO",
                                    "NOME_PARTIDO",
                                    "SEQUENCIAL_LEGENDA",
                                    "NOME_COLIGACAO",
                                    "COMPOSICAO_LEGENDA",
                                    "TOTAL_VOTOS",
                                    "TRANSITO"),
                  delim = ";", fileEncoding="latin1")
      resultados <-bind_rows(resultados, d)
} 
#checando se os quatro estados foram empilhados
table(resultados$SIGLA_UF)


##repetindo os passos para criar o arquivo das candidaturas##

#criando uma lista de todos os arquivos contidos na pasta
lista.arquivos <-list.files(file.path(getwd()))
print(lista.arquivos)
#criando uma lista para pegar somente os documentos de votação
lista.candidatos <- grep(pattern="consulta_cand_2016_", lista.arquivos, value=TRUE)
print(lista.candidatos)

#pegando somente os arquivos dos estados do Sudeste
lista.candidatos <- lista.candidatos[c(8,11,19,26)]
print(lista.candidatos)

#criando o dataframe vazio que receberá os dados
candidatos <- data.frame()

#Loop para coletar os dados que queremos:
#vai abrir cada uma das listas, renomear as colunas de acordo com o indicado no arquivo LEIAME.
#incluir no dataframe vazio


for(arquivo in lista.candidatos){
  print (arquivo)
  d <- read_delim(file.path(getwd(),arquivo), 
                  col_names = c("DATA_GERACAO",
                                "HORA_GERACAO",
                                "ANO_ELEICAO",
                                "NUM_TURNO",
                                "DESCRICAO_ELEICAO",
                                "SIGLA_UF",
                                "SIGLA_UE",
                                "DESCRICAO_UE",
                                "CODIGO_CARGO",
                                "DESC_CARGO",
                                "NOME_CANDIDATO",
                                "SEQUENCIAL_CANDIDATO",
                                "NUMERO_CANDIDATO",
                                "CPF_CAND",
                                "NOME_URNA_CANDIDATO",
                                "COD_SITUACAO_CANDIDATURA",
                                "DES_SITUACAO_CANDIDATURA",
                                "NUMERO_PARTIDO",
                                "SIGLA_PARTIDO",
                                "NOME_PARTIDO",
                                "CODIGO_LEGENDA",
                                "SIGLA_LEGENDA",
                                "COMPOSICAO_LEGENDA",
                                "NOME_LEGENDA",
                                "CODIGO_OCUPACAO",
                                "DESCRICAO_OCUPACAO",
                                "DATA_NASCIMENTO",
                                "NUM_TITULO_ELEITORAL_CANDIDATO",
                                "IDADE_DATA_ELEICAO",
                                "CODIGO_SEXO",
                                "DESCRICAO_SEXO",
                                "COD_GRAU_INSTRUCAO",
                                "DESCRICAO_GRAU_INSTRUCAO",
                                "CODIGO_ESTADO_CIVIL",
                                "DESCRICAO_ESTADO_CIVIL",
                                "COD_COR_RACA",
                                "DESC_COR_RACA",
                                "CODIGO_NACIONALIDADE",
                                "DESCRICAO_NACIONALIDADE",
                                "SIGLA_UF_NASCIMENTO",
                                "CODIGO_MUNICIPIO_NASCIMENTO",
                                "NOME_MUNICIPIO_NASCIMENTO",
                                "DESPESA_MAX_CAMPANHA",
                                "COD_SIT_TOT_TURNO",
                                "DESC_SIT_TOT_TURNO"),
                  delim = ";", fileEncoding="latin1")
  candidatos <-bind_rows(candidatos, d)
} 

#checando se os quatro estados foram empilhados
table(candidatos$SIGLA_UF)


############################################
###### Parte 2 - data frame resultados #####
############################################

#selecionando as linhas que contem resultados para prefeites

resultados <- resultados %>% 
  filter(DESCRICAO_CARGO == "PREFEITO")
dim(pagamentos17)

