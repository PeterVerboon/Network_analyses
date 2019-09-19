
require(userfriendlyscience)
require(dplyr)                  # arrange function
require(lme4)                   # lmer function

require(mvtnorm)
require(arm)                    # se.fixef function used after lmer
require(qgraph)



dat <- getData()
dat <- getData(filename="/Users/peterverboon/Documents/Open Universiteit/Onderzoek/Project Catherine/Boonen_zonder30.sav");
#  getData(filename="//srv-hrl-03v/homedir$/pve/Mijn documenten/Onderzoek/Project Catherine/Boonen_zonder30.sav");

columns <- c("NAffc","PAffc", "POEsc","STRc","Zintentie","SEc","CRc","modelling","rookgedrag") # these are the  variables used in the analysis


dat1 <- dat[,c("subjnr","dagnr","beepnr","geslacht",columns)]

dat1$mod <- as.numeric(dat1$modelling) -1
dat1$lap <- as.numeric(dat1$rookgedrag) - 1

columns <- c("NAffc","PAffc","POEsc","STRc","Zintentie","SEc","CRc","mod","lap")

dat2 <- LagESM(dat1, lagn=1, columns)                                # add lagged variables to data



########################################################
####### Figure 1: The population network ###############
########################################################

### Fitting the data with multilevel-VAR method with the lme4 or lmer function

model1=list()

pred1 = "(NAffcL1 + PAffcL1 + POEscL1 + STRcL1 + ZintentieL1 + SEcL1 + CRcL1 + modL1 + lapL1 + beepnr)  + (1|subjnr)"   # first put predictors not used in network, only random intercept
pred1 = "(geslacht + NAffcL1 + POEscL1 + StresscL1 + ZintentieL1 + SEcL1 + CRcL1)  + 
         (NAffcL1 + PAffcL1 + STRcL1 + ZintentieL1 + SEcL1 + CRcL1|subjnr)"                     # first put predictors not used in network with random effects
                                                                                              # these random effects are necessary for Figure 2 and 3



nvars = 10    # total number of  variables involved in the network 
ndich = 2    # number of dichotomous vars (listed after numerical vars)
npred = 10    # number of predictors involved in the analyses, including those not in the network (listed at beginning)

for (j in 1:(nvars-ndich)) {
  ff=as.formula(paste(columns[j],"~",pred1,sep=""))
  model1[[j]]<-lmer(ff,data=dat2,REML=FALSE)
  
  print(j)
}

for (j in (npred-ndich+1):nvars) {
  ff=as.formula(paste(columns[j],"~",pred1,sep=""))
  model1[[j]]<-glmer(ff,data=dat2,family = binomial)
  
  print(j)
}

BIC1=unlist(lapply(model1,BIC))

sum(BIC1)




###inferring the coefficients or connection strengths for the network from the fitted model1

coef1=data.frame(matrix(unlist(lapply(model1,fixef),use.names=FALSE),byrow=TRUE, ncol=(npred+1))) 
se.coef1=data.frame(matrix(unlist(lapply(model1,se.fixef),use.names=FALSE),byrow=TRUE,ncol=(npred+1))) 
colnames(coef1)=names(fixef(model1[[1]]))
colnames(se.coef1)=names(fixef(model1[[1]]))
rownames(coef1)=columns
rownames(se.coef1)=columns

extrarow <- rep(0,(nvars+1))
coef1 <- rbind(coef1,extrarow)
rownames(coef1)=c(columns, "beep")
extrarow <- rep(1,(nvars+1))
se.coef1 <- rbind(se.coef1,extrarow)
rownames(se.coef1)=c(columns, "beep")

labs <- c("NA","PA","POE","STR","INT","SE","CR","MOD","LAP","beep")

###  making a Figure of the baseline average population network with Qgraph; pvalue<0.05
###   Only the coefficients from the columns 2 to nnvars+1 (not the intercepts) of the nvars items are needed. 

pdf("Figure Smoking Lapse Network.pdf", width=6.83,height=6.83,useDingbats=F)

E <- cbind(from=rep(1:nvars,each=nvars),to=rep(1:nvars,nvars),weigth=unlist(coef1[,(2+npred-nvars):(npred+1)]))
pvals <- 2*(1-pnorm(abs(unlist(coef1[,(2):(npred+1)]/se.coef1[,(2):(npred+1)]) )))
edge.color <- addTrans(ifelse(E[,3]>0, "green3", "red3"), ifelse(pvals<0.10, 255, 0))
qgraph(E,fade=FALSE,layout="spring",labels=labs,lty=ifelse(E[,3]>0,1,5),edge.labels=F,edge.color=edge.color)

dev.off()




######################################################
####### Figure 2 and 3: Individual differences #######
######################################################

###  with "VV" the individual differences are taken from the fitted model1, each link now indicates the amount of individual variability
###  the random slope effects are selected in the VarCorr output
###  Assumed that there are 1 (intercept) + nvars random effecten

VV <- sqrt(t(matrix(unlist(lapply(model1,function(x){VV=diag(VarCorr(x)$subjnr[2:(nvars+1),2:(nvars+1)])})),nvars,nvars)))


###the network figure of individual differences
pdf("Figure2.pdf", width=6.83,height=6.83,useDingbats=F)

E <- cbind(from=rep(1:nvars,each=nvars),to=rep(1:nvars,nvars),weigth=as.vector(VV))
edge.color <- addTrans("blue", ifelse(E[,3]>.095, 255, 20))
qgraph(E,fade=FALSE,layout="circular",labels=c("NA","PA","STR","I","SE","CR"),lty=ifelse(E[,3]>0,1,5),edge.labels=F,edge.color=edge.color)

dev.off()



###  Network figure of two individuals; N=1 networks
###  First the coefficients for all individuals are inferred

nsub <- length(unique(dat2$subjnr))

cc <- list(model1[[1]],model1[[2]],model1[[3]],model1[[4]],model1[[5]],model1[[6]])
mat.ind <- array(0,c(nvars,nvars,49))
for (x in 1:nsub){
  for (ii in 1:nvars){
    mat.ind[,ii,x] = as.numeric(fixef(cc[[ii]])[(2+npred-nvars):(npred+1)])     # +ranef(cc[[ii]])$subjnr[x,2:(nvars+1)])) # add this when random effects in model
  }}
mat.ind

### The networks are made

pdf("Figure3.pdf", width=6.83*2,height=4*2,useDingbats=F)
par(mfcol=1:2)

jj=rep(1:nvars,each = nvars)
jk=rep(1:nvars,nvars)
E1=data.frame(from=jk,to=jj,weight=as.vector(mat.ind[,,2]))      ## individual 1 and subject 40 were taken as an example in the paper
qgraph(E1,layout="circular",labels=c("NA","PA","STR","I","SE","CR"),lty=ifelse(E1[,3]>0,1,5),edge.labels=F,mar=c(5,5,5,5))    #,filetype="pdf")

E2=data.frame(from=jk,to=jj,weight=as.vector(mat.ind[,,30]))     ##individual 1 and subject 40 were taken as an example in the paper
qgraph(E2,layout="circular",labels=c("NA","PA","STR","I","SE","CR"),lty=ifelse(E2[,3]>0,1,5),edge.labels=F,mar=c(5,5,5,5))    #,filetype="pdf")

dev.off()



######################################################
####### Figure 4: Centrality betweenness #############
######################################################

###Global network analyses: Age, centrality betweenness

# Making three age groups:low, mid, high

breaks <-  c(-Inf,quantile(dat$leeftijd,c(0.25,0.75),na.rm=T),Inf)
age_mid_low_high <- cut(dat$leeftijd,breaks=breaks,include.lowest=TRUE)
#age_mid_low_high <- factor(age_mid_low_high,levels(age_mid_low_high)[c(2,1,3)])  # to make the middle group reference group, we put it at the first instead of the second level
#factor(neur_mid_low_high)                                                        # by checking the levels of the factor you see that it was done correctly
dat2$age3 <- age_mid_low_high


# Fitting the age model without random effects

pred1lm <- "(geslacht + NAffcL1 + PAffcL1 + STRcL1 + ZintentieL1 + SEcL1 + CRcL1) + age3:(NAffcL1 + PAffcL1 + STRcL1 + ZintentieL1 + SEcL1 + CRcL1)"
model2 <- list()
for (j in 1:nvars){
  ff=as.formula(paste(columns[j],"~", pred1lm, sep=""))
  model2[[j]]<-lm(ff,data=dat2,na.action=na.exclude)
  print(j)
}


# Taking out the coefficients
# ncol = intercept + geslacht(1) + npred + (categories Age-1) + npred*(categories Age-1) = 1 + 1 + 6 + 6*2  = 20 

coeflm <- data.frame(matrix(unlist(lapply(model2,coef),use.names=FALSE),byrow=TRUE,ncol=20))
colnames(coeflm) <- names(coef(model2[[1]]))
predX <- data.frame(matrix(unlist(lapply(model2,fitted),use.names=FALSE),byrow=FALSE,ncol=nvars))
colnames(predX) <- columns


# For the residuals na's are not excluded

model2 <- list()

for (j in 1:6){
  ff <- as.formula(paste(columns[j],"~",pred1lm,sep=""))
  model2[[j]] <- lm(ff,data=dat2)
  print(j)
}

# Taking out the residuals
residX <- data.frame(matrix(unlist(lapply(model2,resid),use.names=FALSE),byrow=TRUE,ncol=nvars))


#Simulation of betweenness centrality using a parametric bootstrap method


alpha <- 1.5                                                   # (see paper of Opsahl et al., 2010)
MM <- dat2[,c("subjnr","dagnr","beepnr","geslacht","age3")]
MM <- cbind(MM,1:dim(dat2)[1])
colnames(MM)[dim(MM)[2]] <- "ind"
MMlag <- LagESM(MM,lagn=1, c("ind","age3"))
Xfix <- MMlag[,1:5]
lag.ind <- MMlag[,7]

Nrep <- 100
cLOW.REP <- matrix(NA,Nrep,nvars)
cMID.REP <- matrix(NA,Nrep,nvars)
cHIGH.REP <- matrix(NA,Nrep,nvars)

for (rep in 1:Nrep){
  err.ind <- sample(1500,2935,replace=TRUE)
  error <- residX[err.ind,]
  yrep <- predX+error
  colnames(yrep) <- columns
  yrepL <- yrep[lag.ind,]
  colnames(yrepL) <- paste(columns,"L1",sep="")
  Yrep <- cbind(Xfix,yrep,yrepL)
  print(rep)
  modellmREP <- list()
  for (j in 1:nvars){
    ff <- as.formula(paste(columns[j],"~",pred1lm,sep=""))
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


#Making the figure

pdf("Figure4.pdf",useDingbats=F)

error.bar <- function(x, y, upper, lower=upper, length=0.0,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

#letvec <- c(64+3,64+5,64+23,64+6,64+19,64+18)   #letters
strletvec=c("N","P","ST","I","S","C")
ord <- c(1,2,3,4,5,6)
par(mfrow=c(1,3),mar=c(4,4.5,1,2))

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

plot(1,0,col=0,xlim=c(1,6.5),ylim=c(0,12),xlab="",bty="n",ylab="Centrality index (betweenness)",xaxt="n",cex.lab=1.3,cex.axis=1.0,main="LOW")  #yaxt='n',ylab='',bty="n")#,ylab="centrality index")
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

plot(1,0,col=0,xlim=c(1,6.5),ylim=c(0,12),xlab="",bty="n",ylab="",yaxt="n",xaxt="n",cex.lab=1.3,cex.axis=1.3,main="MID") #yaxt='n',ylab='',bty="n") #,ylab="centrality index")
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
plot(1,0,col=0,xlim=c(1,6.5),ylim=c(0,12),xlab="",bty="n",ylab="",yaxt="n",xaxt="n",cex.lab=1.3,cex.axis=1.3,main="HIGH") #yaxt='n',ylab='',bty="n")    #,ylab="centrality index")
axis(1,pos=0,at=1:6,labels=strletvec[ord],cex.axis=1.0)
error.bar(x=1:6,y=MHIGH[ord],upper=Q75HIGH[ord]-MHIGH[ord],lower=MHIGH[ord]-Q25HIGH[ord],lwd=lwdg,col="grey85")
error.bar(x=1:6,y=MHIGH[ord],upper=Q75HIGH2[ord]-MHIGH[ord],lower=MHIGH[ord]-Q25HIGH2[ord],lwd=lwdc,col="grey50")
points(MHIGH[ord],pch=19,type="b")


dev.off()
