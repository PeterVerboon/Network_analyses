
setwd("~/Documents/Open Universiteit/Onderzoek/Methodologie/Network analysis")
getwd()


library("qgraph")

Data1 <- read.csv("~/Documents/Open Universiteit/Onderzoek/Methodologie/Network analysis/Data1.csv", sep=";")

data <- Data1[,c(2,3,4,5,6,7,8,9)]

qgraph(cor(data))

C <- cor(data)

groep = c("A","A","A","B","B","C","C","C")

Q <- qgraph(cor(data), groups = groep)

Q <- qgraph(Q, minimum = 0.25, borders = F, vsize = 5)

qgraph(Q, layout = "groups")

qgraph(C, graph = "concentration", minimum = .25, groups = groep)

qgraph(C, graph = "factorial", minimum = .25, groups = groep)

qgraph.panel(C, minimum = .25, groups = groep, vsize = 3, cut = 0.3)



