#Monte Carlo filtering
#based on Saltelli 2010
# D.Inman 10122016

rm(list=ls())
R_LIBS= ("/home/R/library")
options(scipen=999) #turn scientific notation off
setwd("~/GitHub/r-sensitivity/")
results.path <- ("~/GitHub/r-sensitivity/results/")
#Libraries
library (data.table)
library (dplyr)

#Functions
boot.samples <- function (numeric.variable, boots) { for (i in 1:boots)
{df <- numeric.variable[sample(nrow(numeric.variable), boots, replace=TRUE),]
 return (df)
}}                                                  

#load data
load (paste (results.path, "ad.results.RDA"))

#loop through the B.list to resample each factor
LIST <- list()
for (i in 1:length (B.list)) {
  LIST [[i]] <- boot.samples(B.list[[i]], 15000)
  LIST [[i]] <- LIST[[i]]$value
  B.boot.sample <- (LIST)
}
names(B.boot.sample) <- names(B.list) # put the factor names back in
B.boot.sample <- do.call (cbind, B.boot.sample) # assemple the list elements 

#loop through the B.bar.list to resample each factor
LIST <- list()
for (i in 1:length (B.bar.list)) {
  LIST [[i]] <- boot.samples(B.bar.list[[i]], 15000)
  LIST [[i]] <- LIST[[i]]$value
  B.bar.boot.sample <- (LIST)
}
names(B.bar.boot.sample) <- names(B.list)
B.bar.boot.sample <- do.call (cbind, B.bar.boot.sample)

####################
# bootstrapped results for the two sets for sig factors only
dt <- sig.results$factor #significant factors names 
dt2 <- B.boot.sample #bootstrapped sample for group B
cols <- (colnames(dt2) %in% dt)#logical, factors in B that are in the significant group
B.sig <- subset(dt2,,cols)
cor (B.sig) #test to double check our factors line up
B.sig.boot <- melt (B.sig)
rm(dt2, cols)

dt3 <- B.bar.boot.sample #bootstrapped sample for group B.bar
cols<-(colnames(dt3) %in% dt)#logical, factors in B.bar that are in the significant group
B.bar.sig<-subset(dt3,,cols)
cor (B.bar.sig) #test to double check our factors line up
B.bar.sig.boot <- melt (B.bar.sig)

boots <- cbind(B.sig.boot, B.bar.sig.boot)
setnames (boots, c("permutation", "factor", "B.boot", "permutation2", "factor2", "B.bar.boot"))
bootstrapped.samples <- data.table (permutation = boots$permutation, factor = boots$factor, B.boot = 
                                      boots$B.boot, B.bar.boot = boots$B.bar.boot)

save (bootstrapped.samples, B.sig, B.bar.sig, file = paste (results.path, "bootstrapped.samples.RDA"))
write.csv (bootstrapped.samples, paste (results.path, "bootstrapped.samples.csv"))


