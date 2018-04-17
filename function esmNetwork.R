

esmNetwork <- function(dat, subjnr, daynr, beepnr, vars, covs=NULL,lagn=1, labs=NULL, titlePlot="Figure"){
  
 
   dat1 <- dat[,c(subjnr,daynr,beepnr, covs,vars)]
  
   
   if (is.null(labs)) { labs <- vars}

    # Vector of predictor names (lagged variables)
   
   varsp <- paste0(vars[1],"L",lagn)
   for (i in 2:length(vars)) {
     varsp <- paste0(varsp, " + ", vars[i],"L",lagn)
   }
   
   covs2 <- ifelse(is.null(covs), "", paste0(covs," + "))
   
   # all predictors, only random intercept
   pred1 <- paste0("(",covs2, varsp," + (1|",subjnr,"))")
   
   nvars = length(vars)                 # number of variables involved in the network analyses
   npred = length(covs) + nvars        # number of predictors involved in the analyses
   
 
  ### Construct lagged variables
   
  dat2 <- LagESM(dat1, subjnr=subjnr,daynr=daynr,beepnr=beepnr, lagn=lagn, vars)
  
  
  model1=list()
  
  ### run MLA for all variables in network
  
  for (j in 1:nvars) {
    ff=as.formula(paste(vars[j],"~",pred1,sep="")); 
    model1[[j]]<-lmer(ff,data=dat2,REML=FALSE)
    print(j)
  }
  
  
  ###  inferring the coefficients or connection strengths for the network from the fitted model1
  
  
  
  coef1=data.frame(matrix(unlist(lapply(model1,fixef),use.names=FALSE),byrow=TRUE, ncol=(npred+1))) 
  colnames(coef1)=names(fixef(model1[[1]]))
  rownames(coef1)=vars
  
  se.coef1=data.frame(matrix(unlist(lapply(model1,se.fixef),use.names=FALSE),byrow=TRUE,ncol=(npred+1))) 
  colnames(se.coef1)=names(fixef(model1[[1]]))
  rownames(se.coef1)=vars

  
  E <- cbind(from=rep(1:nvars,each=nvars),to=rep(1:nvars,nvars),weigth=unlist(coef1[,(2+npred-nvars):(npred+1)]))
  pvals <- 2*(1-pnorm(abs(unlist(coef1[,(2+npred-nvars):(npred+1)]/se.coef1[,(2+npred-nvars):(npred+1)]) )))
  edge.color <- addTrans(ifelse(E[,3]>0, "green3", "red3"), ifelse(pvals<0.01, 255, 0))
  
  G <- qgraph(E,fade=FALSE,layout="spring",labels=labs,lty=ifelse(E[,3]>0.1,1,5),
              edge.labels=F,edge.color=edge.color, title=titlePlot)
  
  return(G)
  
  
}  # end function


# test



vars <- c("pa_1","pa_2","pa_3","ac_2","ev_1","se_1")
labs <- c("PA1","PA2","PA3","AC","EV","SE")


plot(a)


a <- esmNetwork(dat=dat, subjnr="idnum__c", daynr = "dayno", beepnr="beepno",
                vars = vars,
                covs = "gender",
                lagn = 1,
                labs = labs)


