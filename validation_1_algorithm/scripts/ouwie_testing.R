library(OUwie)
data(tworegime)
source("../utility/functions.R")

#Calculate the likelihood based on known values of
#alpha, sigma^2, and theta:
alpha=c(0.1,0.2)
sigma.sq=c(0.2,0.4)
theta=c(1,2)
names(alpha) <- names(sigma.sq) <- names(theta) <- c("1", "2")
cont <- trait$X
names(cont) <- trait$Genus_species

OUwie.fixed(sm,trait,model=c("OUMVA"), simmap.tree=TRUE, scaleHeight=FALSE,
            clade=NULL, alpha=alpha,sigma.sq=sigma.sq,theta=theta, algorithm="three.point")


sd_logL_pruning(sm, cont, alpha, sigma.sq, theta)
sd_logL_vcv(sm, cont, alpha, sigma.sq, theta)





