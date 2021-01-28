##############################################################################
#     Scientometric Analysis of Auschwitz-Birkenau
#       Holocaust remembrance day: 28 January
##############################################################################

#Loading necessary package
library(bibliometrix)

# Loading data
setwd("C:/Users/Matshisela/Documents/Auschwitz-Birkenau")


file <- "savedrecs.bib"

M <- convert2df(file = file, dbsource = "isi", format = "bibtex")

#Bibliometric Analysis

results <- biblioAnalysis(M, sep = ";")
options(width=100)
S <- summary(object = results, k = 10, pause = FALSE)

#Plotting graphs
plot(x = results, k = 10, pause = FALSE)

# most frequent cited manuscripts:
CR <- citations(M, field = "article", sep = ";")
cbind(CR$Cited[1:10])

#  most frequent cited first authors:
CR <- citations(M, field = "author", sep = ";")
cbind(CR$Cited[1:10])

# Top-Authors' Productivity over the Time
topAU <- authorProdOverTime(M, k = 10, graph = TRUE)

## Table: Author's productivity per year
head(topAU$dfAU)


#classical article coupling network:
NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "references", sep = ".  ")

NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "authors", sep = ";")

#Bibliographic co-citation
NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ".  ")

# Bibliographic collaboration
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "authors", sep = ";")


# Create a country collaboration network

M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")

#Country Scientific Collaboration
# Plot the network
net=networkPlot(NetMatrix, n = dim(NetMatrix)[1], Title = "Country Collaboration", type = "circle", size=TRUE, remove.multiple=FALSE,labelsize=0.7,cluster="none")


# Keyword co-occurrences
# Create keyword co-occurrences network

NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Keyword Co-occurrences", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)

#Co-Word Analysis: The conceptual structure of a field
# Conceptual Structure using keywords (method="CA")

CS <- conceptualStructure(M,field="ID", method="CA", minDegree=4, clust=5, stemming=FALSE, labelsize=10, documents=10)
