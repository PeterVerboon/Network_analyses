
install.packages("remotes")
remotes::install_github("SachaEpskamp/bootnet")

library("bootnet")
library("qgraph")
library("psych")
data(bfi)
Data <- bfi[,c(10:25)]

## example
Network <- estimateNetwork(Data,default = "EBICglasso")
boot1 <- bootnet(Network, nBoots = 200, nCores = 2)
plot(boot1, labels = FALSE, order = "sample")
differenceTest(boot1, 3, 4, "strength")
plot(boot1, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
plot(boot1, "strength")
boot2 <- bootnet(Network, nBoots = 200, type = "case", nCores = 2)
plot(boot2)
corStability(boot2)

library("graphicalVAR")
library("lvnet")

# Detrending significant linear trends:
for (v in seq_along(vars)){
  ff <- as.formula(paste0(vars[[v]]," ~ beepno"))
  fit <- lm(ff, data = dat)
  if (anova(fit)$P[1] < 0.05){
    message(paste("Detrending variable",v))
    dat[[vars[v]]][!is.na(dat[[vars[[v]]]])] <- residuals(fit)
  }
}

# Run analysis (excluding nights using dayvar = 'date'):
Res <- graphicalVAR(dat, gamma = 0, vars = vars, dayvar = "dayno")
summary(Res)
plot(Res)

# Plot results:
contemporaneous <- qgraph(Res$PCC, fade = FALSE, labels = gsub("\\.","\n",vars),  theme = "colorblind")
temporal <- qgraph(Res$PDC, fade = FALSE, labels = gsub("\\.","\n",vars),  theme = "colorblind")

### Fitting the model in lvnet ###
# Extract augmented data:
laggedData <- Res$data$data_l[,-1]
names(laggedData) <- paste0(names(laggedData),"_lag1")
currentData <- Res$data$data_c
# Combine:
augData <- cbind(laggedData, currentData)

# Remove periods (required for OpenMx):
names(augData) <- gsub("\\.","_",names(augData))

## construct model matrices ##
nVar <- ncol(currentData)

# Lambda (identity):
Lambda <- diag(nVar*2)

# Theta (zeros):
Theta <- matrix(0, nVar*2, nVar*2)

# Beta (block)
gvarBeta <- ifelse(Res$beta[,-1]==0,0,NA)
O <- matrix(0, nVar, nVar)
Beta <- rbind(
  cbind(O, O),
  cbind(gvarBeta, O)
)

# Latent network (exo block for t-1, cont network for t):
gvarKappa <- ifelse(Res$kappa==0,0,NA)
Omega_psi <- rbind(
  cbind(matrix(NA,nVar,nVar), O),
  cbind(O, gvarKappa)
)
diag(Omega_psi) <- 0

# Free latent scale:
delta_psi <- diag(NA, nVar*2)

# Fit model:
lvfit <- lvnet(augData, lambda = Lambda, theta = Theta,
               omega_psi = Omega_psi, beta = Beta,
               delta_psi = delta_psi, fitFunction = "ML",
               exogenous = 1:nVar)

# Judge fit:
lvfit
summary(lvfit)

# Compare to latent variable model:
# Lambda (equality constrained factor loadings):
Lambda <- rbind(
  cbind(
    c(1,paste0("l",2:nVar)),0
  ),
  cbind(
    0,c(1,paste0("l",2:nVar))
  )
)

# Equality constrained residuals:
Theta <- diag(nVar*2)
for (i in 1:nVar){
  Theta[i,nVar+i] <- Theta[nVar+i,i] <- NA
}
diag(Theta) <- c(paste0("t",1:nVar),paste0("t",1:nVar))

# Beta (regression t-1 -> t1):
Beta <- matrix(c(
  0,0,
  NA,0
),2,2,byrow=TRUE)

# Psi (exo var and residual)
Psi <- diag(NA, 2)

# Equality constrained residuals:
Theta[1:nVar,1:nVar] <- NA_character_

# Fit model:
lvfit2 <- lvnet(augData, lambda = Lambda, theta = Theta,
                beta = Beta, psi = Psi, fitFunction = "ML",
                exogenous = 1:nVar)

# Compare the models:
tab <- lvnetCompare(lvfit, lvfit2)
tab

# Create the plot in paper:
loopRot <- c(NA,NA,NA,NA,1.5*pi,NA,NA)
pdf("Figure.pdf",width=8,height=4)
layout(t(1:2))
qgraph(Res$PDC, vsize = 13, mar = c(5,5,5,5), title = "(a) Temporal network", asize = 6, labels = gsub("\\.","\n",vars), 
       directed = TRUE, loopRotation = loopRot, theme = "colorblind")
par(mar=c(0,0,0,0))
box()
qgraph(Res$PCC, vsize = 13, mar = c(5,5,5,5), title = "(b) Contemporaneous network", 
       labels = gsub("\\.","\n",vars), theme = "colorblind")
par(mar=c(0,0,0,0))
box()
dev.off()




##  our network
a <- bootnet(dat[,vars], 
             default = "EBICglasso", 
             model = "graphicalVAR",
             nBoots = 100)
summary(a)
plot(a)
a1 <- estimateNetwork(dat[,vars], 
             default = "EBICglasso")
summary(a1)
plot(a1)
boota1 <- bootnet(a1, nBoots = 100, type = "nonparametric", nCores = 2)
plot(boota1)
differenceTest(boota1, x = 3, y = 4)
corStability(boota1)


a2 <- estimateNetwork(dat[,vars], 
             default = "none", 
             fun = esmNetwork2,
             verbose = TRUE,
             subjnr="subjnr",level1 = "beepno",level2= "dayno", vars = vars)
summary(a2)
plot(a2)

boota2 <- bootnet(a2, nBoots = 100, type = "nonparametric", nCores = 2)
boota2 <- bootnet(a2, nBoots = 100, type = "case", nCores = 2)
plot(boota2)
differenceTest(boota2, x = 3, y = 1)
corStability(boota2)



a3 <- bootnet(dat[,-c(1:5)], 
             default = "none", 
             fun = esmNetwork2,
             model = "detect",
             verbose = TRUE,
             statistics = c("edge","strength"),
             nBoots = 10,
             type = "case", nCores = 2,
             subjnr="subjnr",level1 = "beepno",level2= "dayno", vars = vars)

summary(a3)
a3$sample
plot(a3)



