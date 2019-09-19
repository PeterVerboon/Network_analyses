

### load packages

devtools::install_github("PeterVerboon/lagnetw")
remotes::install_github("PeterVerboon/lagnetw")

library(NetworkComparisonTest)

library(lagnetw)

### load data
load("gratitudeESM.rda")     ### loads data frame  "dat"
data("gratitude")

dat <- gratitude

dat$group <- dat$wellBeing

vars <- c("pa_1","pa_2","pa_3","na_1","na_2","na_3")

labs <- c("PA1","PA2","PA3","NA1","NA2","NA3")

#dat <- dat[,c("subjnr", "day","time","dayno","beepno","gender",vars, "wellBeing")]

### make sure data are ordered by subjno and then beepNumber
# dat <- dat[order(dat$subjnr, dat$dayno, dat$beepno),]
# 
# dat1 <- lagnetw::centerESM(data = dat, subjnr = "subjnr", addmeans = F, varnames = vars, center = "grand_mean")
# dat1 <- lagnetw::lagESM(data = dat1, subjnr = "subjnr", level2 = "dayno", level1 = "beepno", lagn = 1, varnames = vars )





res <- esmNetwork2(dat = dat, subjnr="subjnr",level1 = "beepno", optim = "Nelder_Mead",
                  level2= "dayno", vars = vars, labs = labs, groups = "wellBeing", lagn = 1)

res$output$centralityMeasures

out <- conDif(dat=dat, vars=vars, group="wellBeing", subjnr="subjnr", level1="beepno", level2 = "dayno", 
              randomVars = F, perms = 10, optim = "Nelder_Mead" ) 

res$output$pl
res$output$graph
res$graph 
res$results

out$output$pvals
out$output$resObserved1 
out$output$permutations

plot(out)

a <- sigConnect(dat = dat , vars , group="wellBeing", subjnr ="subjnr", level = "beepno", 
                randomVars = FALSE, perms = 100, optim = "Nelder_Mead")

a$output$observedDifs
a$output$meanDifs
a$output$pvalues.def1
a$output$pvalues.def2
a$output$plimit_adjusted



 

