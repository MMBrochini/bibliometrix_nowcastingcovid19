############################################################################
#                                                                          #
#  Mônica Miguel Brochini                                                  #
#  Programa de Pós-graduação em Saúde Coletiva/IESC/UFRJ                   #
#  Doutorado, Linha de Pesquisa: Epidemiologia e Políticas de Saúde        #
#  Orientadores: Antonio José Leal Costa e Natália Santana Paiva           #
#  Artigo 1:                                     #
#  "In God we trust; all others must bring data" (William Edwards Deming)  #
#                                                                          #
###########################################################################

#instalando o pacote principal versão CRAN que é atualizada mensalmente:

#getOption('timeout')
#options(timeout=300)

#install.packages("bibliometrix", dependencies = TRUE)

#install.packages("Matrix")

#instalando a versão beta direto do github do autor que atualiza o pacote diariamente:

#install.packages("remotes")         
#remotes::install_github("massimoaria/bibliometrix")
#remotes::install_github("massimoaria/bibliometrixData")

#abrindo a interface: (apenas para quem deseja utilizar o shiny)
#bibliometrix::biblioshiny()

#carregando alguns pacotes;
library(ggplot2)
library(dplyr)
library(bibliometrix)

#Carregando e convertendo conjunto de dados
#O arquivo pode ser lido e convertido no Rstudio usando a função convert2df:

#---file =  um vetor de caracteres contendo o nome dos arquivos de exportação baixados do site SCOPUS, 
#Clarivate Analytics WOS, Digital Science Dimenions, PubMed ou Cochrane. 

#file <- "https://www.bibliometrix.org/datasets/savedrecs.bib"

#file <- ("savedrecs.bib")

#M <- convert2df(file = file, dbsource = "isi", format = "bibtex")


#---------repetindo para cada base de dados:
#WOS:
banco1 <- ("savedrecs.bib")

wos <- convert2df(file = banco1, dbsource = "isi", format = "bibtex")

#Scopus:
banco2 <- ("scopus.bib")

scop <- convert2df(file = banco2, dbsource = "scopus", format = "bibtex")

#Pubmed (importar as duas bases das duas chaves utilizadas na busca)
banco3 <- ("pubmed-covid-19OR-set.txt")

pubm <-convert2df(file = banco3, dbsource = "pubmed", format = "pubmed")

banco4 <- ("pubmed-covid-19OR-set_banco2.txt")

pubm2 <-convert2df(file = banco4, dbsource = "pubmed", format = "pubmed")

#conferir as informações de exportação das bases!

#merge/junção das bases Scopus & WOS que foram importadas no passo anterior
#incluindo a remoção dos artigos duplicados

dado <- bibliometrix::mergeDbSources(wos, scop, pubm, pubm2,
                                      remove.duplicated = TRUE)

#Retirando as publicações duplicadas permanecebram no banco 18 artigos!

#CR10 <- citations(dado, field = "article", sep = ";")
#cbind(CR10$Cited[1:10])

##########################################
#conferir se mantém as colunas similares #
#########################################

#Análise descritiva do quadro de dados bibliográficos...
#A função biblioAnalysis calcula as principais medidas bibliométricas usando esta sintaxe:

resultados <- biblioAnalysis(dado, sep = ";")
resultados

table(dado$AU)

#vendo índices descritivos:
#Hindex-H
index_h <- Hindex(dado, field = "author", 
                  elements="BASTOS L", sep = ";", years = 10)

Hindex(dado, field = "BASTOS L", elements = NULL, sep = ";", years = 10)
library(tidyverse)

#removendo artigo duplicado do mesmo autor e um artigo da ciência atuarial
#usando subset variáveis autores e DOI indicando os campos específicos para identificação da linha correta

data<- subset(dado, !AU %in% c("RICHARDS S"))
data_2<- subset(data,!DI %in% c("2020.10.18.20209189"))

#Banco final = data_2

#Quais tipos de trabalhos existem na base? 
#Ruim né ler pelo console!
library(dplyr)
library(knitr)

#Para resumir os principais resultados da análise bibliométrica, 
#use o summary da função genérica. Apresenta as principais informações 
#sobre o quadro de dados bibliográficos e diversas tabelas, 
#como produção científica anual, principais manuscritos 
#por número de citações, autores mais produtivos, países mais produtivos, 
#citação total por país, fontes mais relevantes (periódicos) e palavras-chave mais relevantes.

options(width=100)

S <- summary(object = dado, k = 10, pause = TRUE)
S

#Alguns gráficos básicos podem ser desenhados usando a função genérica:
windows()
plot(x = dado, k = 10, pause = TRUE)

#A função de dominance calcula a classificação de dominância dos autores 
#conforme proposto por Kumar & Kumar, 2008.
#DF <- dominance(dado, k = 5)
#DF
#rm(DF)

topAU <- authorProdOverTime(data_2, k = 10, graph = TRUE)

windows()
topAU

#Bipartite networks
# cocMatrix é uma função geral para calcular uma rede
#selecionando um dos atributos de metadados.
#Por exemplo, para criar uma fonte Manuscrito x Publicação de rede, 
#você deve usar a tag de campo “SO”:

A <- cocMatrix(data_2, Field = "SO", sep = ";")

sort(Matrix::colSums(A), decreasing = TRUE)[1:5]

#A é uma matriz binária retangular, representando uma rede bipartida onde 
#linhas e colunas são manuscritos e fontes, respectivamente.

#Rede de autoria
B <- cocMatrix(data_2, Field = "AU", sep = ".  ")
B

#O índice h é uma métrica em nível de autor que tenta medir a produtividade 
#e o impacto de citação das publicações de um cientista ou acadêmico.

#O índice é baseado no conjunto de artigos mais citados dos cientistas e 
#no número de citações que eles receberam em outras publicações.

index_h <- Hindex(dado, field = "author", 
                  elements="LIPSITCH M", sep = ";", years = 10)

index_h$H

#Summary
options(width=160)
results <- biblioAnalysis(data_2)
summary(results, k=10, pause=F, width=130)

#matriz de co-citação
NetMatrix <- biblioNetwork(data_2, analysis = "co-citation", network = "references", sep = ";")
net=networkPlot(NetMatrix, n = 50, Title = "Co-Citation Network", type = "fruchterman", 
                size.cex=TRUE, size=20, remove.multiple=FALSE, labelsize=1,edgesize = 10, edges.min=5)

netstat <- networkStat(NetMatrix)
summary(netstat,k=10)

#co-citação de revistas
M=metaTagExtraction(dado,"CR_SO",sep=";")

NetMatrix2 <- biblioNetwork(M, analysis = "co-citation", network = "sources", sep = ";")
net=networkPlot(NetMatrix2, n = 50, Title = "Co-Citation Network", type = "auto", 
                size.cex=TRUE, size=15, remove.multiple=FALSE, labelsize=1,edgesize = 10, edges.min=5)
netstat <- networkStat(NetMatrix2)
summary(netstat,k=10)

#estrutura ocnceitutal
NetMatrix3 <- biblioNetwork(dado, analysis = "co-occurrences", network = "keywords", sep = ";")
net=networkPlot(NetMatrix3, normalize="association", n = 50, Title = "Keyword Co-occurrences", 
                type = "fruchterman", size.cex=TRUE, size=20, remove.multiple=F, edgesize = 10, 
                labelsize=5,label.cex=TRUE,label.n=30,edges.min=2)

#Mapa
#exemplo
#data(scientometrics, package = "bibliometrixData")
#CS <- conceptualStructure(scientometrics, field="ID", method="CA",
                          #stemming=FALSE, minDegree=3, k.max = 5)

#data(dado, package = "bibliometrixData")

CS<-conceptualStructure(dado, method="CA", field="ID", minDegree=15, 
                            clust=3, stemming=FALSE, labelsize=10,documents=18)
conceptualStructure(
  data_2,
  field = "DE",
  ngrams = 1,
  method = "CA",
  quali.supp = NULL,
  quanti.supp = NULL,
  minDegree = 2,
  clust = "auto",
  k.max = 20,
  stemming = FALSE,
  labelsize = 11,
  documents = 1,
  graph = TRUE,
  remove.terms = NULL,
  synonyms = NULL
)

#países
da <- metaTagExtraction(data_2, Field = "AU_CO", sep = ";")
windows()
NetMatrix <- biblioNetwork(da, analysis = "collaboration",  network = "countries", sep = ";")
net=networkPlot(NetMatrix,  n = dim(NetMatrix)[1], Title = "Country collaboration",type = "circle", 
                size=10,size.cex=T,edgesize = 1,labelsize=1, cluster="none")
NetMatrix


NetMatrix <- biblioNetwork(dado, analysis = "collaboration",  network = "universities", sep = ";") 
net=networkPlot(NetMatrix,  n = 50, Title = "Edu collaboration",type = "auto", size=4,size.cex=F,
                edgesize = 3,labelsize=1)

couplingMap(
  data_2,
  analysis = "documents",
  field = "CR",
  n = 500,
  label.term = NULL,
  ngrams = 1,
  impact.measure = "local",
  minfreq = 5,
  community.repulsion = 0.1,
  stemming = FALSE,
  size = 0.5,
  n.labels = 1,
  repel = TRUE,
  cluster = "walktrap"
)

normalizeSimilarity(NetMatrix, type = "association")

tableTag(
  data_2,
  Tag = "CR",
  sep = ";",
  ngrams = 1,
  remove.terms = NULL,
  synonyms = NULL
)


#network
windows()
biblioNetwork(
  data_2,
  analysis = "coupling",
  network = "authors",
  n = NULL,
  sep = ";",
  short = FALSE,
  shortlabel = TRUE,
  remove.terms = NULL,
  synonyms = NULL
)

#bradford
bradford(dado)
M <- convert2df(file = file, dbsource = "isi", format = "bibtex")
BR <- bradford(M)

window()
conceptualStructure(
  data_2,
  field = "DE_TM",
  ngrams = 1,
  method = "CA",
  quali.supp = NULL,
  quanti.supp = NULL,
  minDegree = 2,
  clust = "2",
  k.max = 5,
  stemming = FALSE,
  labelsize = 17,
  documents = 16,
  graph = TRUE,
  remove.terms = NULL,
  synonyms = NULL
)

dominance(results, k = 10)

fieldByYear(
  data_2,
  field = "ID",
  timespan = NULL,
  min.freq = 2,
  n.items = 5,
  labelsize = NULL,
  remove.terms = NULL,
  synonyms = NULL,
  dynamic.plot = FALSE,
  graph = TRUE
)

threeFieldsPlot(data_2, fields = c("AU", "DE", "SO"), n = c(20, 20, 20))

termExtraction(
  data_2,
  Field = "TI",
  ngrams = 1,
  stemming = FALSE,
  language = "english",
  remove.numbers = TRUE,
  remove.terms = NULL,
  keep.terms = NULL,
  synonyms = NULL,
  verbose = TRUE
)

threeFieldsPlot(data_2, fields = c("AU", "DE", "SO"), n = c(20, 20, 20))

