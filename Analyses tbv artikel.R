

devtools::install_github("PeterVerboon/lagnetw")

library(lagnetw)

require(userfriendlyscience)
require(ggplot2)

getDat()
gratitude <- dat
save(gratitude, file="gratitude.rda")

load("gratitude.rda")
vars <- c("pa_1","pa_2","pa_3","na_1","na_2","na_3")




table(gratitude$mhc_total)
quantile(gratitude$mhc_total, na.rm = TRUE)   ## well-being

gratitude$group <- 0

gratitude$group[gratitude$mhc_total <= 35] <- 1
gratitude$group[gratitude$mhc_total >= 49] <- 2
table(gratitude$group)

dat <- subset(gratitude, group > 0)
table(dat$group)

dat1 <- dat[,c("idnum__c","beepno","dayno","group",vars)]
dat1$subjnr <- dat1$idnum__c

out <- testCF(dat=dat1, vars=vars, group="group", subjnr="idnum__c",
              level1="beepno", level2 = "dayno", 
              randomVars = F, perms = 100) 

out$output$fixedEffects1 - out$output$fixedEffects2

out$output$fixedEffects2
out$output$pvals
out$output$perms

plot.testCF(out, type=1)
plot.testCF(out, type=2)
plot.testCF(out, type=3)
plot.testCF(out, type=4)


res <- esmNetwork(dat = dat1, subjnr="subjnr",level1 = "beepno",
                  level2= "dayno", vars = vars,  lagn = 1)

res$output$centralityMeasures

