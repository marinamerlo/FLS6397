### nome <- "Marina Merlo"
### programa <- "Mestrado em Ciência Política"
### n_usp <- "7197987"
### data_entrega: "05/05/2017"

#################################################################################################################
############  RASPAGEM DE DADOS DE REPRESENTAÇÃO DE MULHERES NOS PARLAMENTOS (1997-2017) DO IPU  ################
#################################################################################################################

##O Interparliamentary Union é uma organização internacional fundada em 1889 e trabalha junto com a ONU e todos os parlamentos
##associados tanto para se manter, quanto para reunir informações e fiscalizar o trabalho dos parlamentos no mundo.
##parte desse trabalho é o IPU Women (http://www.ipu.org/iss-e/women.htm), em que se monitora o avanço dos países 
##em relação a representação de mulheres nos parlamentos. Esse monitoramento engloba a disponibilização de dados comparativos, 
##documentos e registro do trabalho que os parlamentos tem feito em prol da mulher na política.
##O IPU Women disponibiliza dados da composição da Câmara Baixa e da Câmara Alta de 193 países desde 1997 até hoje, com atualização
##mensal dos dados desde 2003. Infelizmente, a forma como esses dados é disponibilizada complica sua coleta: há um link específico
##para cada atualização mensal dos dados, resultando em mais de 200 tabelas que são mostradas em webpages separadas. 
##Com as ferramentas de webscrapping, é possível automatizar essa coleta e rapidamente organizar os dados.

#############################################
############PREPARANDO O R ###################
##############################################

##pacotes necessários:
#install.packages("rvest")
#install.packages("XML")
#install.packages("tidyverse")
library(rvest)
library(XML)
library(tidyverse)

#indicando onde os dados ficarão salvos
setwd("D:/Dropbox/Mestrado/Disciplinas/Programacao")

#############################################
############RASPANDO OS LINKS################
##############################################

#A página do arquivo dos dados (http://www.ipu.org/wmn-e/world-arc.htm) mostra links individuais para cada ano e mês em que os dados
#foram coletados. Num primeiro momento, vou extrair desta página todos os elementos que contém um link, pela tag <a href>:
links <- html("http://www.ipu.org/wmn-e/classif-arc.htm")%>% 
  html_nodes("a") %>% 
  html_attr("href")

#vendo quais links foram capturados
print(links)
#os links que me interessa vão do 4 ao 226. Até aqui, a solução parece simples: verificar a estrutura de cada link (como, por exemplo, 
#aqui: http://www.ipu.org/wmn-e/arc/world010317.htm), identificando onde estão as tabelas que quero extrair e preparar o loop. Usando
#o link de março de 2017 como exemplo, vi que a tabela com os dados estava na posição 1. Porém, a primeira vez que rodei o loop,
#apenas os dados entre 2009 e 2017 eram capturados. Então, vi que entre 1997 e 2008, a tabela com os dados estava na posição 3 no html.

#A solução que pensei não deve ser a mais inteligente, mas funcionou. Criei um banco de dados que indicava o dia, mês e ano de cada link
#a partir do seu endereço, e então a partir do filtro desse banco, conseguia separar uma lista de links para cada período. Não consegui
#fazer um loop único que funcionasse na lógica de "if year >=2009, então capture a tabela 1, else, tabela 3". Fiz dois loops e depois
#juntei o banco.

#############################################
############SEPARANDO OS LINKS################
##############################################


#Para facilitar essa separação dos links, fiz o seguinte raciocício: no próprio endereço do link está indicada a data do banco de dados,
#na seguinte estrutura: DIAMÊSANO.htm. A rotina abaixo desmembra essa informação e a deixa limpa:

anos <- as.data.frame(links[4:226]) %>% #cria um banco de dados a partir da lista de links
  rename(id = `links[4:226]`) %>% #renomeia a variável que contém os links
  mutate(links = id) %>% #duplica a variável, para poder manter a informação do endereço
  mutate(id = gsub("/wmn-e/arc/classif", "", id)) %>% #retira a primeira parte do link
  mutate(id = gsub(".htm", "", id)) %>% #retira a segunda parte do link
  separate(id, into = c("date", "data_year"), sep = 4) %>% #separa a data do link em DIAMÊS e ANO
  separate(date, into = c("data_day", "data_month"), sep = 2) %>% #separa o DIAMÊS em DIA e MÊS
  mutate(data_year = ifelse(data_year >= 97, paste0("19", data_year), paste0("20",data_year))) %>% #Completa o ano
  mutate_each(funs(as.numeric), -links) #transforma as datas em variável numérica

##selecionando os links que são anteriores a 2008. Nesse caso, optei por seleciona janeiro ou fevereiro de cada ano para 
#o script rodar mais rápido.
selecao <- anos %>%
  filter(data_year <= 2008 & (data_month == 1 | data_month == 2 ))
#retorna os links selecionados para o formato de lista:
links <- as.list(selecao$links)


#############################################
############CAPTURANDO AS TABELAS############
##############################################

#cria um url base para ser colado com os links capturados e completar o endereço
baseurl <- "http://www.ipu.org" 
#prepara a base de dados que receberá as tabelas
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
selecao <- anos %>%
  filter(data_year >= 2009 & (data_month == 1 | data_month == 2))
links <- as.list(selecao$links)
dados2009 <- data.frame()
for (link in links) { 
  url <- paste(baseurl, link, sep = "") 
  lista.tabelas <- readHTMLTable(url) 
  tabela <- tbl_df(lista.tabelas[[1]]) %>% #essa é a única diferença em relação ao loop anterior: indica a tabela 1 e não a 3 
                                           #Queria ter conseguido incluir essa variação num loop único...
    filter(complete.cases(.)) %>% 
    mutate(id = link) 
  dados2009 <- bind_rows(dados2009, tabela) 
}

#juntando os dois bancos coletados:
dados <- bind_rows(dados2009, dados2008)


#############################################
############ORGANIZANDO OS DADOS##############
##############################################

teste <- dados %>%
#cria de novo a data da coleta dos dados a partir da estrutura do link, como fiz para a seleção dos links:
  mutate(id = gsub("/wmn-e/arc/classif", "", id)) %>%
  mutate(id = gsub(".htm", "", id)) %>%
  separate(id, into = c("date", "data_year"), sep = 4) %>%
  separate(date, into = c("data_day", "data_month"), sep = 2) %>%
  mutate(data_year = ifelse(data_year >= 97, paste0("19", data_year), paste0("20",data_year))) %>%
#joga fora a variável do rank do país e a variável de proporção de mulheres pra cada uma das câmaras. 
#optei por excluir e recria-la porque o dado vinha com % e o gsub não estava funcionaod.
  select(-V1, -V6, -V10) %>%
#renomeando as variáveis 
rename(country = V2,
         lower_elections = V3,
         lower_seats = V4,
         lower_seats_w = V5,
         upper_elections = V7,
         upper_seats = V8,
         upper_seats_w = V9) %>%
  mutate(country = gsub('[0-9]+', "", country)) %>% #retira eventuais notas de rodapé vinculadas ao nome do país
#criando novamente as variáveis de proporção de mulheres a partir dos dados de vagas totais e vagas ocupadas por mulheres: 
mutate(lower_seats = as.numeric(lower_seats)) %>%
  mutate(lower_seats_w = as.numeric(lower_seats_w)) %>%
  mutate(lower_seats_w_per = as.numeric(lower_seats_w / lower_seats)) %>%
  mutate(upper_seats = as.numeric(upper_seats)) %>%
  mutate(upper_seats_w = as.numeric(upper_seats_w)) %>%
  mutate(upper_seats_w_per = as.numeric(upper_seats_w / upper_seats)) %>%
#deixa a variável do ano como numérica
  mutate(data_year = as.numeric(data_year))

#salvando os dados arrumados no WD.
write.csv2(teste, "dados.csv", row.names = F)

#############################################
############VISUALIZANDO OS DADOS#############
##############################################

#como primeiro exercício de visualização dos dados, fiz um gráfico de linhas para ver como o Brasil evoluiu nos últimos 20
#anos em comparação com os outros países.

#Primeiro, criei um banco que contém as médias de representação por ano por país - já que raspamos os meses de janeiro e/ou fevereiro
#optei por criar apenas para a Câmara Baixa de cada país porque vários não tem um Câmara Alta:
lower_mean <- teste %>%
  group_by(country, data_year) %>%
  summarise(lower_seats_w_ymean = mean(lower_seats_w_per)) 

#variável para indicar o Brasil, útil para o gráfico:
lower_mean$br <- ifelse(lower_mean$country == "Brazil", c("Brazil"), c("Others"))

#fazendo um gráfico com todos os países e anos coletados, destacando o Brasil:
p1 <- ggplot() + 
  geom_line(aes(y = lower_seats_w_ymean, #no eixo y: a proporção média de mulheres
x = as.numeric(data_year), #no eixo x: o ano
group = as.factor(country), #agrupa as linhas por país
alpha=as.factor(br), #estabelece a transparência por ser Brasil ou não
color=as.factor(br),#estabelece a cor por ser Brasil ou não
size=as.factor(br)),#estabelece o tamanho da linha por ser Brasil ou não
                           data = lower_mean, stat="identity") + #usando o banco das médias, preenchendo pela variável
  geom_point()+ #plota cada ponto de cada ano de cada país em cada proporção
  theme_bw()+ #deixa o fundo branco
  scale_size_manual(values = c(1.5, 0.3)) +	#faz uma linha mais grossa para o Brasil
  scale_alpha_manual(values = c(1, 0.3)) + #faz uma linha mais clara para os outros países
  scale_color_manual(name = "", labels=c("Brazil", ""), values = c("red","black")) + #deixa o Brasil destacado em vermelho
  labs(title = "Porcentagem de mulheres na Câmara Baixa no Mundo (1997-2009)",
  x = "Ano", y="Porcentagem de mulheres")  #inclui os labels do gráfico
#plota o gráfico 
p1
#salva o gráfico no WD
ggsave("boxplot_votos.png", width = 10, height = 5)

##como o Brasil está? Bem mal. Vemos que sua linha é praticamente reta, enquanto muitos países conseguiram ampliar a representação de 
#mulheres ao longo dos anos. 

#############################################
############ PRÓXIMOS PASSOS #################
##############################################

#Conseguir vincular esse dado com outros bancos com informações dos países. Infelizmente a única chave é o nome do país,
#e sabemos como usar nomes como chaves pode ser problemático.

#Conseguir formatar a informação da data das eleições. Eles mudaram ao longo dos anos a forma de reportar a data, então quando
#tentei usar o separate, ele ficava todo bagunçado. Precisa indentificar a estrutura da data dos anos e criar um loop para arrumar
#de acordo com cada um. 
