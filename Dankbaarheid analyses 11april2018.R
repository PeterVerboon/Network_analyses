
## Code for the analysis of a network

options(digits = 3)

require(userfriendlyscience)
#require(dplyr)
require(qgraph)
require(lme4)
require(arm)                    # contains se.fixef function used after lmer

## Activate functions addTrans and lagESM 

dat0 <- getData()             # First remove value labels in SPSS
#getData(filename="/Users/peterverboon/Documents/Open Universiteit/Onderzoek/Methodologie/Network_analyses/Dankbaarheid_novalue_labs.sav");
dat <- dat0

# Select subset of persons if necessary

dat <- subset(dat0, mhc_total >= 40)       # select low or high well being
names(dat)

# variable names used in network

vars <- c("pa_1","pa_2","pa_3","ac_2","ev_1","se_1")

"so_1_6c"

dat1 <- dat0[,c("idnum__c","dayno","beepno", "age","gender",vars)]

dat1 <- dplyr::rename(dat1, subjnr=idnum__c, daynr = dayno, beepnr=beepno)   

dat1 <- dplyr::rename(dat1, beepno=beepnr)   




#### Basic statistics

apply(!is.na(dat1[,vars]),2,sum)
a <- cor(dat1[,vars], use = "pairwise.complete.obs")

qgraph(a, minimum=.3, graph = "cor", sampleSize = 320, layout = "spring")           # graph of correlation matrix


 #### Construct lagged variables


dat2 <- LagESM(dat1, subjnr="subjnr",daynr="daynr",beepnr="beepnr", lagn=1, vars)


########################################################
####### Figure 1: The population network ###############
########################################################

### Fitting the data with multilevel-VAR method with the lme4 or lmer function



# Vector of predictor names (lagged variables)

varsp <- paste0(vars[1],"L1")
for (i in 2:length(vars)) {
  varsp <- paste0(varsp, " + ", vars[i],"L1")
}


# add names (if any) of predictors not used in network (covariates)
pred0 <- c("gender")

pred0 <- ifelse(is.null(pred0), "", paste0(pred0," + "))

# all predictors, only random intercept
pred1 <- paste0("(",pred0, varsp," + (1|subjnr))")


#pred2 = "(gender + pa_1L1 + pa_2L1 + pa_3L1 + da_1L1 + da_2L1 + da_3L1) + (pa_1L1 + pa_2L1 + pa_3L1 + da_1L1 + da_2L1 + da_3L1 |subjnr)"                     # first put predictors not used in network with random effects
#pred2 <- paste0("(",pred0, varsp," + (",varsp," |subjnr))")

# these random effects are necessary for Figure 2 and 3

nvars = length(vars)                 # number of variables involved in the network analyses
npred = length(pred0) + nvars        # number of predictors involved in the analyses


## NB If there are always missing in any two or more variables, there is no data ## 

model1=list()

for (j in 1:nvars) {
  ff=as.formula(paste(vars[j],"~",pred1,sep=""))
  model1[[j]]<-lmer(ff,data=dat2,REML=FALSE)
  print(j)
}




###  inferring the coefficients or connection strengths for the network from the fitted model1

require(arm)                    # contains se.fixef function used after lmer

coef1=data.frame(matrix(unlist(lapply(model1,fixef),use.names=FALSE),byrow=TRUE, ncol=(npred+1))) 
colnames(coef1)=names(fixef(model1[[1]]))
rownames(coef1)=vars

se.coef1=data.frame(matrix(unlist(lapply(model1,se.fixef),use.names=FALSE),byrow=TRUE,ncol=(npred+1))) 
colnames(se.coef1)=names(fixef(model1[[1]]))
rownames(se.coef1)=vars



###  Making a Figure of the baseline average population network with Qgraph; pvalue < 0.05
###  Only the coefficients from the columns 2 to nnvars+1 (not the intercepts) of the nvars items are needed. 

labs <- c("PA1","PA2","PA3","AC","EV","SE","so")

pdf("Figure.pdf", width=6.83,height=6.83,useDingbats=F)

E <- cbind(from=rep(1:nvars,each=nvars),to=rep(1:nvars,nvars),weigth=unlist(coef1[,(2+npred-nvars):(npred+1)]))
pvals <- 2*(1-pnorm(abs(unlist(coef1[,(2+npred-nvars):(npred+1)]/se.coef1[,(2+npred-nvars):(npred+1)]) )))
edge.color <- addTrans(ifelse(E[,3]>0, "green3", "red3"), ifelse(pvals<0.01, 255, 0))

G <- qgraph(E,fade=FALSE,layout="spring",labels=labs,lty=ifelse(E[,3]>0.1,1,5),
             edge.labels=F,edge.color=edge.color)
plot(G)

dev.off()





### Betweenness centrality

C <- centrality(G, alpha = 1, posfun = abs, all.shortest.paths = FALSE)
           
C$Betweenness
C$Closeness
C$OutDegree
C$InDegree
C$ShortestPaths
C$ShortestPathLengths







