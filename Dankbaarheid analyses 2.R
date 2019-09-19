
# We hebben 4 negatief affect items (na_1 tem na_4), 3 positief affect items (pa_1 tem pa_3) en 
# 3 dankbaarheids affect items (da_1 tem da_3).
# Ik weet niet of je een netwerk van al die affect items (dus pa + na+ da) kunt maken? 
# Of eentje van de positief affect items en de dankbaarheids items? Of anders eentje van 
# alle positief affect items + enkel da_3.  
# Ik ben met name wel niewsgierig hoe da_3 zich in een netwerk van positieve affecten gedraagt.

## Versie 2 van dit script bevat ook de variabelen mbt tot SE en NA.


options(digits = 3)

require(userfriendlyscience)
require(plyr)                 # for rename function
# require(dplyr)
require(qgraph)


dat <- getData()             # First remove value labels in SPSS

vars <- c("pa_1","pa_2","pa_3","da_1","da_2","da_3","na_1","na_2","na_3","na_4","se_1","se_2","se_3")


attach(dat1)

dat3 <- dat[,c("idnum__c","dayno","beepno", "age","gender", vars)]
dat4 <- rename(dat3, c("idnum__c"="subjnr","dayno" = "dagnr","beepno"="beepnr"))     # NB from plyr()


#### Basic statistics

apply(!is.na(dat3[,vars]),2,sum)
a <- cor(dat3[,vars], use = "pairwise.complete.obs")

qgraph(a, minimum=.3, graph = "cor", sampleSize = 320, layout = "spring")           # graph of correlation matrix



 #### Construct lagged variables


dat4 <- LagESM(dat4, lagn=1, vars)
head(dat2)




########################################################
####### Figure 1: The population network ###############
########################################################

### Fitting the data with multilevel-VAR method with the lme4 or lmer function

require(lme4)

model1=list()

pred1 = "(gender + pa_1L1 + pa_2L1 + pa_3L1 + da_1L1 + da_2L1 + da_3L1 + na_1L1 + na_2L1 + na_3L1 + na_4L1 + se_1L1 + se_2L1 + se_3L1 + (1|subjnr))"  
           # first put predictors not used in network, only random intercept


 pred1 = "(gender + pa_1L1 + pa_2L1 + pa_3L1 + da_1L1 + da_2L1 + da_3L1) + (pa_1L1 + pa_2L1 + pa_3L1 + da_1L1 + da_2L1 + da_3L1 |subjnr)"                     # first put predictors not used in network with random effects
# these random effects are necessary for Figure 2 and 3

nvars = 13    # number of variables involved in the network analyses
npred = 14    # number of predictors involved in the analyses

for (j in 1:nvars) {
  ff=as.formula(paste(vars[j],"~",pred1,sep=""))
  model1[[j]]<-lmer(ff,data=dat4,REML=FALSE)
  
  print(j)
}



###  inferring the coefficients or connection strengths for the network from the fitted model1

require(arm)                    # se.fixef function used after lmer

coef1=data.frame(matrix(unlist(lapply(model1,fixef),use.names=FALSE),byrow=TRUE, ncol=(npred+1))) 
se.coef1=data.frame(matrix(unlist(lapply(model1,se.fixef),use.names=FALSE),byrow=TRUE,ncol=(npred+1))) 
colnames(coef1)=names(fixef(model1[[1]]))
colnames(se.coef1)=names(fixef(model1[[1]]))
rownames(coef1)=vars
rownames(se.coef1)=vars



###  Making a Figure of the baseline average population network with Qgraph; pvalue < 0.05
###  Only the coefficients from the columns 2 to nnvars+1 (not the intercepts) of the nvars items are needed. 

require(qgraph)

pdf("Figure1_13vars.pdf", width=6.83,height=6.83,useDingbats=F)

labs <- c("PA1","PA2","PA3","DA1","DA2","DA3","NA1","NA2","NA3","NA4","SE1","SE2","SE3")

E <- cbind(from=rep(1:nvars,each=nvars),to=rep(1:nvars,nvars),weigth=unlist(coef1[,(2+npred-nvars):(npred+1)]))
pvals <- 2*(1-pnorm(abs(unlist(coef1[,(2+npred-nvars):(npred+1)]/se.coef1[,(2+npred-nvars):(npred+1)]) )))
edge.color <- addTrans(ifelse(E[,3]>0, "green3", "red3"), ifelse(pvals<0.005, 255, 0))
G <- qgraph(E,fade=FALSE,layout="spring",labels=labs,lty=ifelse(E[,3]>0.1,1,5),
             edge.labels=F,edge.color=edge.color)
G
dev.off()

### Betweenness centrality

C <- centrality(G, alpha = 1, posfun = abs, all.shortest.paths = FALSE)
           
C$Betweenness
C$Closeness
C$OutDegree
C$InDegree
C$ShortestPaths
C$ShortestPathLengths


######################################################
####### Figure 2 and 3: Individual differences #######
######################################################

###  with "VV" the individual differences are taken from the fitted model1, each link now indicates the amount of individual variability
###  the random slope effects are selected in the VarCorr output
###  Assumed that there are 1 (intercept) + nvars random effecten

VV <- sqrt(t(matrix(unlist(lapply(model1,function(x){VV=diag(lme4::VarCorr(x)$subjnr[2:(nvars+1),2:(nvars+1)])})),nvars,nvars)))


###the network figure of individual differences
pdf("Figure2_13vars.pdf", width=6.83,height=6.83,useDingbats=F)

E <- cbind(from=rep(1:nvars,each=nvars),to=rep(1:nvars,nvars),weigth=as.vector(VV))
edge.color <- addTrans("blue", ifelse(E[,3]>.095, 255, 20))
qgraph(E,fade=FALSE,layout="circular",labels=labs,lty=ifelse(E[,3]>0,1,5),edge.labels=F,edge.color=edge.color)

dev.off()



###  Network figure of two individuals; N=1 networks
###  First the coefficients for all individuals are inferred

nsub <- length(unique(dat2$subjnr))

cc <- list(model1[[1]],model1[[2]],model1[[3]],model1[[4]],model1[[5]],model1[[6]],
           model1[[7]],model1[[8]],model1[[9]],model1[[10]],model1[[11]],model1[[12]],model1[[13]])      # NB length is number of variables !!
mat.ind <- array(0,c(nvars,nvars,nsub))
for (x in 1:nsub){
  for (ii in 1:nvars){
   mat.ind[,ii,x] = as.numeric(fixef(cc[[ii]])[(2+npred-nvars):(npred+1)]) +ranef(cc[[ii]])$subjnr[x,]                   # only random intercept  
#   mat.ind[,ii,x] = as.numeric((fixef(cc[[ii]])[(2+npred-nvars):(npred+1)]) + (ranef(cc[[ii]])$subjnr[x,2:(nvars+1)])) # add this when random effects in model
  }}


### The networks are made

## individual 1 and subject 50 were taken as an example 

pdf("Figure3_13vars.pdf", width=6.83*2,height=4*2,useDingbats=F)
par(mfcol=1:2, mfrow=1:2)

jj=rep(1:nvars,each = nvars)
jk=rep(1:nvars,nvars)
E1=data.frame(from=jk,to=jj,weight=as.vector(mat.ind[,,10]))      
qgraph(E1,layout="spring",labels=labs,lty=ifelse(E1[,3]>0,1,5),edge.labels=F,mar=c(5,5,5,5))    #,filetype="pdf")

E2=data.frame(from=jk,to=jj,weight=as.vector(mat.ind[,,50]))     
qgraph(E2,layout="spring",labels=labs,lty=ifelse(E2[,3]>0,1,5),edge.labels=F,mar=c(5,5,5,5))    #,filetype="pdf")

E3=data.frame(from=jk,to=jj,weight=as.vector(mat.ind[,,75]))     
qgraph(E3,layout="spring",labels=labs,lty=ifelse(E2[,3]>0,1,5),edge.labels=F,mar=c(5,5,5,5))    #,filetype="pdf")

E4=data.frame(from=jk,to=jj,weight=as.vector(mat.ind[,,105]))     
qgraph(E4,layout="spring",labels=labs,lty=ifelse(E2[,3]>0,1,5),edge.labels=F,mar=c(5,5,5,5))    #,filetype="pdf")

dev.off()




######################################################
####### Figure 4: Centrality betweenness #############
######################################################

### Global network analyses: Age, centrality betweenness

# Making three age groups:low, mid, high

breaks <-  c(-Inf,quantile(dat$age,c(0.25,0.75),na.rm=T),Inf)
age_mid_low_high <- cut(dat$age,breaks=breaks,include.lowest=TRUE)
#age_mid_low_high <- factor(age_mid_low_high,levels(age_mid_low_high)[c(2,1,3)])  # to make the middle group reference group, we put it at the first instead of the second level
#factor(neur_mid_low_high)                                                        # by checking the levels of the factor you see that it was done correctly
dat2$age3 <- age_mid_low_high


# Fitting the age model without random effects

pred1lm <- "(gender + pa_1L1 + pa_2L1 + pa_3L1+ da_1L1 + da_2L1 + da_3L1) + age3:(pa_1L1 + pa_2L1 + pa_3L1+ da_1L1+ da_2L1+ da_3L1)"
model2 <- list()
for (j in 1:nvars){
  ff=as.formula(paste(vars[j],"~", pred1lm, sep=""))
  model2[[j]]<-lm(ff,data=dat2,na.action=na.exclude)
  print(j)
}


# Taking out the coefficients
# ncol = intercept + geslacht(1) + npred + (categories Age-1) + npred*(categories Age-1) = 1 + 1 + 6 + 6*2  = 20 

coeflm <- data.frame(matrix(unlist(lapply(model2,coef),use.names=FALSE),byrow=TRUE,ncol=20))
colnames(coeflm) <- names(coef(model2[[1]]))
predX <- data.frame(matrix(unlist(lapply(model2,fitted),use.names=FALSE),byrow=FALSE,ncol=nvars))
colnames(predX) <- vars


# For the residuals na's are not excluded

# model2 <- list()
# 
# for (j in 1:nvars){
#   ff <- as.formula(paste(vars[j],"~",pred1lm,sep=""))
#   model2[[j]] <- lm(ff,data=dat2)
#   print(j)
# }

# Taking out the residuals
residX <- data.frame(matrix(unlist(lapply(model2,resid),use.names=FALSE),byrow=TRUE,ncol=nvars))


# Simulation of betweenness centrality using a parametric bootstrap method


alpha <- 1.5                                                   # (see paper of Opsahl et al., 2010)

MM <- dat2[,c("subjnr","dagnr","beepnr","gender","age3")]
MM <- cbind(MM,1:dim(dat2)[1])
colnames(MM)[dim(MM)[2]] <- "ind"
MMlag <- LagESM(MM,lagn=1, c("ind","age3"))
Xfix <- MMlag[,1:5]
lag.ind <- MMlag[,7]

Nrep <- 1000                                                # number of bootstrap replications

cLOW.REP <- matrix(NA,Nrep,nvars)
cMID.REP <- matrix(NA,Nrep,nvars)
cHIGH.REP <- matrix(NA,Nrep,nvars)

for (rep in 1:Nrep) {
  err.ind <- sample(2000,length(predX[,1]),replace=TRUE)
  error <- residX[err.ind,]
  yrep <- predX+error
  colnames(yrep) <- vars
  yrepL <- yrep[lag.ind,]
  colnames(yrepL) <- paste(vars,"L1",sep="")
  Yrep <- cbind(Xfix,yrep,yrepL)
  print(rep)
  modellmREP <- list()
  for (j in 1:nvars){
    ff <- as.formula(paste(vars[j],"~",pred1lm,sep=""))
    modellmREP[[j]] <- lm(ff,data=Yrep)
  }
  coeflmREP <- data.frame(matrix(unlist(lapply(modellmREP,coef),use.names=FALSE),byrow=TRUE,ncol=20))
  colnames(coeflmREP) <- names(coef(modellmREP[[1]]))
  coefLOWREP <- coeflmREP[,3:(nvars+2)]                       # extract main effects because low group is reference category
  coefMIDREP <- coefLOWREP+coeflmREP[,seq(9,20,2)]           # extract interactions of vars with second category
  coefHIGHREP <- coefLOWREP+coeflmREP[,seq(10,20,2)]          # extract interactions of vars with third category
  
  E <- cbind(from=rep(1:nvars,each=nvars),to=rep(1:nvars,nvars),weigth=unlist(coefLOWREP))
  Qlow <- qgraph(E,DoNotPlot=TRUE)
  cLOW.REP[rep,] <- centrality(Qlow,alpha=alpha)$Betweenness
  
  E <- cbind(from=rep(1:nvars,each=nvars),to=rep(1:nvars,nvars),weigth=unlist(coefMIDREP))
  Qmid <- qgraph(E,DoNotPlot=TRUE)
  cMID.REP[rep,] <- centrality(Qmid,alpha=alpha)$Betweenness
  
  E <- cbind(from=rep(1:nvars,each=nvars),to=rep(1:nvars,nvars),weigth=unlist(coefHIGHREP))
  Qhigh <- qgraph(E,DoNotPlot=TRUE)
  cHIGH.REP[rep,] <- centrality(Qhigh,alpha=alpha)$Betweenness
}


## Making the figure

pdf("Figure4.pdf",useDingbats=F)

error.bar <- function(x, y, upper, lower=upper, length=0.0,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

#letvec <- c(64+3,64+5,64+23,64+6,64+19,64+18)   #letters
strletvec=c("p1","p2","p3","d1","d2","d3")
ord <- c(1,2,3,4,5,6)
par(mfrow=c(1,3), mai = c(0.4,0.4,0.4,0.2))   #mar=c(4,4.5,1,2),

lwdg=1.5
lwdc=2

lowq=.025
highq=.975

lowq2=.25
highq2=.75

### LOW break variable
MLOW <- apply(cLOW.REP,2,median)
Q25LOW <- apply(cLOW.REP,2,quantile,lowq)
Q75LOW <- apply(cLOW.REP,2,quantile,highq)

Q25LOW2 <- apply(cLOW.REP,2,quantile,lowq2)
Q75LOW2 <- apply(cLOW.REP,2,quantile,highq2)

plot(1,0,col=0,xlim=c(1,6.5),ylim=c(0,12),xlab="",bty="n",ylab="Centrality index (betweenness)",xaxt="n",cex.lab=1.1,cex.axis=1.0,main="LOW age")  #yaxt='n',ylab='',bty="n")#,ylab="centrality index")
axis(1,pos=0,at=1:6,labels=strletvec[ord],cex.axis=1.0)
error.bar(x=1:nvars,y=MLOW[ord],upper=Q75LOW[ord]-MLOW[ord],lower=MLOW[ord]-Q25LOW[ord],lwd=lwdg,col="grey85")
error.bar(x=1:nvars,y=MLOW[ord],upper=Q75LOW2[ord]-MLOW[ord],lower=MLOW[ord]-Q25LOW2[ord],lwd=lwdc,col="grey50")

points(MLOW[ord],pch=19,type="b")

### MID break variable
MMID=apply(cMID.REP,2,median)
meanMID=apply(log(cMID.REP+1),2,mean)
Q25MID=apply(cMID.REP,2,quantile,lowq)
Q75MID=apply(cMID.REP,2,quantile,highq)

Q25MID2=apply(cMID.REP,2,quantile,lowq2)
Q75MID2=apply(cMID.REP,2,quantile,highq2)

plot(1,0,col=0,xlim=c(1,6.5),ylim=c(0,12),xlab="",bty="n",ylab="",yaxt="n",xaxt="n",cex.lab=1.1,cex.axis=1.0, main="MID age") #yaxt='n',ylab='',bty="n") #,ylab="centrality index")
axis(1,pos=0,at=1:6,labels=strletvec[ord],cex.axis=1.0)

error.bar(x=1:6,y=MMID[ord],upper=Q75MID[ord]-MMID[ord],lower=MMID[ord]-Q25MID[ord],lwd=lwdg,col="grey85")
error.bar(x=1:6,y=MMID[ord],upper=Q75MID2[ord]-MMID[ord],lower=MMID[ord]-Q25MID2[ord],lwd=lwdc,col="grey50")
points(MMID[ord],pch=19,type="b")

### HIGH break variable
MHIGH=apply(cHIGH.REP,2,median)
meanHIGH=apply(log(cHIGH.REP+1),2,mean)
Q25HIGH=apply(cHIGH.REP,2,quantile,lowq)
Q75HIGH=apply(cHIGH.REP,2,quantile,highq)
Q25HIGH2=apply(cHIGH.REP,2,quantile,lowq2)
Q75HIGH2=apply(cHIGH.REP,2,quantile,highq2)

plot(1,0,col=0,xlim=c(1,6.5),ylim=c(0,12),xlab="",bty="n",ylab="",yaxt="n",xaxt="n",cex.lab=1.1,cex.axis=1.0,main="HIGH age") #yaxt='n',ylab='',bty="n")    #,ylab="centrality index")
axis(1,pos=0,at=1:6,labels=strletvec[ord],cex.axis=1.0)
error.bar(x=1:6,y=MHIGH[ord],upper=Q75HIGH[ord]-MHIGH[ord],lower=MHIGH[ord]-Q25HIGH[ord],lwd=lwdg,col="grey85")
error.bar(x=1:6,y=MHIGH[ord],upper=Q75HIGH2[ord]-MHIGH[ord],lower=MHIGH[ord]-Q25HIGH2[ord],lwd=lwdc,col="grey50")
points(MHIGH[ord],pch=19,type="b")


dev.off()

