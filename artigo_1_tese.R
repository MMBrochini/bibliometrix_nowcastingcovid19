############################################################################
#                                                                          #
#  Mônica Miguel Brochini                                                  #
#  Programa de Pós-graduação em Saúde Coletiva/IESC/UFRJ                   #
#  Doutorado, Linha de Pesquisa: Epidemiologia e Políticas de Saúde        #
#  Orientadores: Antonio José Leal Costa e Natália Santana Paiva           #
#  Artigo 1:                                                               #
#  "In God we trust; all others must bring data" (William Edwards Deming)  #
#                                                                          #
###########################################################################

#instalando o pacote principal versão CRAN que é atualizada mensalmente:

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
data_1 <- ("savedrecs_base.bib")

wos <- convert2df(file = data_1, dbsource = "isi", format = "bibtex")

data_2 <- ("savedrecs_baseII.bib")

wos_2 <- convert2df(file = data_2, dbsource = "isi", format = "bibtex")


#Scopus:
data_3 <- ("scopus.bib")

scop <- convert2df(file = data_3, dbsource = "scopus", format = "bibtex")

#Pubmed (importar as duas bases das duas chaves utilizadas na busca)
data_4 <- ("pubmed-reportingd-set.txt")

pubm <-convert2df(file = data_4, dbsource = "pubmed", format = "pubmed")

#As bases finais fornecem um panorama muito similar de observações!

#conferir sempre as informações de exportação das bases!

#merge/junção das bases Scopus & WOS que foram importadas no passo anterior
#incluindo a remoção dos artigos duplicados

dado <- bibliometrix::mergeDbSources(wos, wos_2, scop, pubm,
                                     remove.duplicated = TRUE)

#37 duplicados removidos!
#removendo artigo duplicado na linha 3, 10 e 16 após leitura
library(dplyr)
dado_2 <- slice(dado, -3)
dado_3 <- slice(dado_2, -10)
dado_4 <- slice(dado_3, -16)

#bibliometric
results <- biblioAnalysis(dado_4, sep = ";")
results

#matriz de co-citação
NetMatrix <- biblioNetwork(dado_4, analysis = "co-citation", network = "references", sep = ";")
net=networkPlot(NetMatrix, n = 40, Title = "Co-Citation Network", type = "fruchterman", 
                size.cex=TRUE, size=15, remove.multiple=TRUE, labelsize=1,edgesize = 10, edges.min=5)
NetMatrix
netstat <- networkStat(NetMatrix)
summary(netstat,k=10)

NetMatrix <- biblioNetwork(dado_4, analysis = "co-citation", network = "references", sep = ";")
net=networkPlot(NetMatrix, n = 50, Title = "Co-Citation Network", 
type = "fruchterman", size.cex=TRUE, size=20, remove.multiple=FALSE, labelsize=1,
edgesize = 10, edges.min=5)

  
#Historiograph - Direct citation linkages

histResults <- histNetwork(dado_4, sep = ";")
options(width = 130)
net <- histPlot(histResults_2, size = 5, labelsize = 4)


#The conceptual structure - Co-Word Analysis

NetMatrix <- biblioNetwork(dado_4, analysis = "co-occurrences", network = "keywords", sep = ";")
net=networkPlot(NetMatrix, normalize="association", n = 50, Title = "Keyword Co-occurrences", 
type = "fruchterman", size.cex=TRUE, size=20, remove.multiple=F, edgesize = 10, 
labelsize=5,label.cex=TRUE,label.n=30,edges.min=2)

#The conceptual structure - Co-Word Analysis
NetMatrix_2 <- biblioNetwork(dado_4, analysis = "co-occurrences", network = "keywords", sep = ";")
net=networkPlot(NetMatrix_2, normalize="association", n = 50, 
Title = "Keyword Co-occurrences", type = "fruchterman", size.cex=TRUE, size=20, 
remove.multiple=F, edgesize = 10, labelsize=5,label.cex=TRUE,label.n=30,edges.min=2)

#análise de correspondência
conceptualStructure(
  dado_4,
  field = "DE",
  ngrams = 1,
  method = "MCA",
  quali.supp = NULL,
  quanti.supp = NULL,
  minDegree = 2,
  clust = 3,
  k.max = 5,
  stemming = FALSE,
  labelsize = 17,
  documents = 2,
  graph = TRUE,
  remove.terms = NULL,
  synonyms = NULL
)

#Mapa temático
thematicMap(
  dado_4,
  field = "DE",
  n = 250,
  minfreq = 10,
  ngrams = 1,
  stemming = FALSE,
  size = 2.5,
  n.labels = 5,
  community.repulsion = 0.5,
  repel = TRUE,
  remove.terms = NULL,
  synonyms = NULL,
  cluster = "walktrap"
)

#is a character. It indicates the type of cluster to perform among 
#("optimal", "louvain","leiden", "infomap","edge_betweenness","walktrap", 
#"spinglass", "leading_eigen", "fast_greedy").

#tree
threeFieldsPlot(dado_4, fields = c("AU", "DE", "SO"), n = c(20, 20, 20))

#países
M <- metaTagExtraction(dado_4, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(data_2, analysis = "collaboration",  network = "countries", sep = ";")
net=networkPlot(NetMatrix,  n = dim(NetMatrix)[1], Title = "Country collaboration",
type = "circle", size=10,size.cex=T,edgesize = 1,labelsize=0.6, cluster="none")


NetMatrix_3 <- biblioNetwork(dado_4, analysis = "collaboration",  network = "authors", sep = ";")
net=networkPlot(NetMatrix,  n = 50, Title = "Author collaboration",type = "auto", size=10,
                size.cex=T,edgesize = 3,labelsize=1)



threeFieldsPlot(dado_4, fields = c("AU", "DE", "SO"), n = c(20, 20, 20))

