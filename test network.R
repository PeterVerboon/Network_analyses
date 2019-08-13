
library(roxygen2)

devtools::install_github("crsh/papaja")
devtools::install_github("crsh/papaja@devel")

## 

devtools::create("lagnetw")    # NB "lagnetw" wordt in map "lagnetw" gemaaakt: even kopieren
setwd("./lagnetw")
devtools::document()
devtools::use_data(DataNews, overwrite = TRUE)

devtools::use_package("lme4")
devtools::use_package("qgraph")
devtools::use_package("arm")
devtools::check(document = TRUE)
setwd("..")
devtools::install("lagnetw")

#setwd("D:/R Git projects")
#devtools::install("lagnetw")


##### JAMOVI

setwd("~/Documents/Open Universiteit/Onderzoek/Methodologie/lagnetw")
setwd("~/Documents/Open Universiteit/Onderzoek/Methodologie/jlagnetw")
jmvtools::check()
options(jamovi_home='/Applications/jamovi.app')


setwd("D:/R Git projects/lagnetw")
jmvtools::check(home='C:\\Program Files\\jamovi 0.9.2.8\\bin')
options(jamovi_home='C:\\Program Files\\jamovi 0.9.2.8\\bin')
setwd("./lagnetw")
jmvtools::addAnalysis(name='network', title='Lagged Network')
jmvtools::install()



#### 

devtools::install_github("PeterVerboon/lagnetw")

library(lagnetw)


# variable names used in network

vars <- c("Fearful","Hopeful","Anxious","Down","Irritated","Relaxed","Insecure")
labs <- c("FF","HO","ANX","DOW", "IRR","REL","INS")

a <- esmNetwork(dat=dat, subjnr="Participant", daynr = "daynr", beepnr="beepnr",
                vars = vars,
                #covs = "Gender2",
                lagn = 1,
                labs = labs)

plot(a)
names(dat1)


