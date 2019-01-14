
## Code for the analysis of a network

options(digits = 3)

require(userfriendlyscience)
require(qgraph)
require(lme4)
require(arm)                    # contains se.fixef function used after lmer

## Activate functions addTrans and lagESM 

dat0 <- getData()             # First remove value labels in SPSS
getData(filename="/Users/peterverboon/Documents/Open Universiteit/Onderzoek/Project Nieuwsbeleving
        /Studie5 Master2017/Datanews2German.sav");
load("DataNews.rda")


dat <- DataNews

# Select subset of persons if necessary

dat <- subset(dat0, News_YesNO == 1)       # select low or high well being
names(dat)



# variable names used in network

vars <- c("Fearful","Hopeful","Anxious","Down","Irritated","Relaxed","Insecure")
labs <- c("FF","HO","ANX","DOW", "IRR","REL","INS")


#### Basic correlation plot

apply(!is.na(dat[,vars]),2,sum)
a <- cor(dat[,vars], use = "pairwise.complete.obs")

qgraph(a, minimum=.3, graph = "cor", sampleSize = 320, layout = "spring")           # graph of correlation matrix


 #### 

dat1 <- dat[,c("Participant","daynr","beepnr","Gender2",vars)]

a <- esmNetwork(dat=dat, subjnr="Participant", daynr = "daynr", beepnr="beepnr",
                vars = vars,
                #covs = "Gender2",
                lagn = 1,
                labs = labs)

plot(a)
names(dat1)





### Betweenness centrality

C <- centrality(G, alpha = 1, posfun = abs, all.shortest.paths = FALSE)
           
C$Betweenness
C$Closeness
C$OutDegree
C$InDegree
C$ShortestPaths
C$ShortestPathLengths







