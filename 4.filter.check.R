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
library (kSamples)

#load data
load (paste (results.path, "AD.results.RDA"))
load (paste (results.path, "bootstrapped.samples.RDA"))
load (paste (results.path, "new.design.RDA"))

#now lets take a closer look at our resampled study design

merge.var.names <- paste (df$variable, df$sub1, df$sub2)
new.design <- data.table(variable = merge.var.names, run = df$run, value = df$value)
resampled.pilot <- new.design [(new.design$variable %in% sig.results$factor)]
resampled.pilot.list <- split(resampled.pilot, resampled.pilot$variable)

LIST <- list()
for (i in 1:length (resampled.pilot.list)) {
  LIST [[i]] <- boot.samples(resampled.pilot.list[[i]], 15000)
  LIST [[i]] <- LIST[[i]]$value
  resampled.pilot.boot <- (LIST)
}
names(resampled.pilot.boot) <- names(resampled.pilot.list)
resampled.pilot.sample <- do.call (cbind, resampled.pilot.boot)

summary (resampled.pilot.sample)
resampled.pilot.sample <- melt (resampled.pilot.sample)
setnames (resampled.pilot.sample, c("perm", "factor", "value"))
write.csv (file = "resampled.etoh.pilot.csv", resampled.pilot.sample) #plot this with the other sets

#lets now do a quick significance test between the new study and B and B-bar

#get the cdf for the new design
new.design.list <- split(new.design, new.design$variable)

#B.bar.summary <- lapply(B.bar.list, summary)
LIST <- list ()
for (i in 1:length (new.design.list)) {
  LIST [[i]] <- get.cdf (new.design.list[[i]]$value, new.design.list[[i]])
  new.design.cdf <- LIST
}
names (new.design.cdf) <- B.bar.names#apply the vbl names to the list elements

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
#names (test.results) <- B.names

ad.results <- as.data.table (do.call (rbind, test.results)) #this takes the list elements from AD results and assembles them as a datatable
ad.results <- cbind (B.names, ad.results) #put the factor names in. Need to be careful here to make sure the order did not change
setnames(ad.results, c("factor", "sim.p.value")) 
rm(ad.list, test.results)

sig.results <- subset (ad.results, ad.results$sim.p.value < 0.05)#subset the AD results based on p-value
