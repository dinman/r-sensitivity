#Monte Carlo filtering
#based on Saltelli 2010
# D.Inman 10122016

rm(list=ls())
options(scipen=999) #turn scientific notation off
results.path <- ("results")
#Libraries
library (data.table)
library (dplyr)
library (kSamples)

#Functions
get.cdf <- function (numeric.variable, df) {
  Fn <- ecdf (numeric.variable)
  return (knots (Fn))
}

boot.samples <- function (numeric.variable, boots) { for (i in 1:boots)
{df <- numeric.variable[sample(nrow(numeric.variable), boots, replace=TRUE),]
 return (df)
}}    



#load data
load (paste (results.path, "AD.results.RDA", sep="/"))
load (paste (results.path, "bootstrapped.samples.RDA", sep="/"))
load (paste (results.path, "new.design.RDA", sep="/"))

#now lets take a closer look at our resampled study design
df.1 <- new.design
merge.var.names <- paste (df.1$variable, df.1$sub1, df.1$sub2)
new.design <- data.table(variable = merge.var.names, run = df.1$run, value = df.1$value)
resampled.design <- new.design [(new.design$variable %in% sig.results$factor)]
resampled.design.list <- split(resampled.design, resampled.design$variable)

LIST <- list()
for (i in 1:length (resampled.design.list)) {
  LIST [[i]] <- boot.samples(resampled.design.list[[i]], 15000)
  LIST [[i]] <- LIST[[i]]$value
  resampled.design.boot <- (LIST)
}

names(resampled.design.boot) <- names(resampled.design.list)
resampled.design.sample <- do.call (cbind, resampled.design.boot)

summary (resampled.design.sample)
resampled.design.sample <- melt (resampled.design.sample)
setnames (resampled.design.sample, c("perm", "factor", "value"))
write.csv (file = "resampled.design.csv", resampled.design.sample) #plot this with the other sets

#lets now do a quick significance test between the new study and B and B-bar

#get the cdf for the new design
new.design.list <- split(new.design, new.design$variable)

#B.bar.summary <- lapply(B.bar.list, summary)
LIST <- list ()
for (i in 1:length (new.design.list)) {
  LIST [[i]] <- get.cdf (new.design.list[[i]]$value, new.design.list[[i]])
  new.design.cdf <- LIST
}
names (new.design.cdf) <- names(new.design.list)#apply the vbl names to the list elements

#perform AD to test if the new design = to the frontier set
LIST <- list()  
for (i in 1:length (new.design.cdf)){
  LIST [[i]] <- ad.test (new.design.cdf[[i]], B.cdf[[i]], method = "simulated", Nsim = 5000) #Nsim = bootstraps
  ad.list <- (LIST)  
}
#names (ad.list) <- B.names

LIST <- list()
for (i in 1:length (ad.list)) {
  LIST [[i]] <- ad.list[[i]]$ad[1,4]
  test.results <- (LIST)
}
names (test.results) <- names(new.design.list)

ad.results <- as.data.table (do.call (rbind, test.results)) #this takes the list elements from AD results and assembles them as a datatable
ad.results <- cbind (B.names, ad.results) #put the factor names in. Need to be careful here to make sure the order did not change
setnames(ad.results, c("factor", "sim.p.value")) 
rm(ad.list, test.results)

sig.results <- subset (ad.results, ad.results$sim.p.value < 0.05)#subset the AD results based on p-value
