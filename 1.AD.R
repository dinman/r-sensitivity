#Monte Carlo filtering
#based on Saltelli 2010
# D.Inman 10122016

rm(list=ls())
R_LIBS= ("/home/R/library")
options(scipen=999) #turn scientific notation off
setwd("~/GitHub/r-sensitivity/")
results.path <- ("~/GitHub/r-sensitivity/results/")
#Libraries
library (kSamples)
library (data.table)
library (dplyr)

#Functions
get.cdf <- function (numeric.variable, df) {
  Fn <- ecdf (numeric.variable)
  return (knots (Fn))
}
#study design can't have years, only duration
load ("design.RDA")
design <- as.data.table (design.in.bsm.format)#need the design with "duration" as a variable
df <- design
merge.var.names <- paste (df$variable, df$sub1, df$sub2)
design <- data.table(variable = merge.var.names, run = df$run, value = df$value)
# local sensitivity analysis
# define behavioural (B) and non-behavioral (B.bar) sets to test for significant diff in their input settings
# select runs of interest (B) from an excel or csv file that is chosen based on tableau
# Use the MCF results to filter the input set
B.runs <- read.table ("B.csv", header=TRUE, sep=",")#these are the runs of interest Laura chose these based on tableau
B <- subset(design, design$run %in% B.runs$run)
B.sorted <- setorder (B, variable)
B.list <- split(B.sorted, B.sorted$variable)
B.names <- as.character (unique (B.sorted$variable))

LIST <- list ()
for (i in 1: length(B.list)) {
  LIST [[i]] <- get.cdf (B.list[[i]]$value, B.list[[i]])
  B.cdf <- LIST
}
names (B.cdf) <- B.names #apply the vbl names to the list elements
rm(design.in.bsm.format, B, B.sorted, merge.var.names, df)

#get the cdf for Bbar
B.bar <- design [ !(design$run %in% B.runs$run), ]#this creates the Bbar by subtracting B from the design
B.bar.sorted <- setorder (B.bar, variable)
B.bar.list <- split(B.bar.sorted, B.bar.sorted$variable)
B.bar.names <- as.character (unique (B.bar.sorted$variable))

LIST <- list ()
for (i in 1:length (B.bar.list)) {
  LIST [[i]] <- get.cdf (B.bar.list[[i]]$value, B.bar.list[[i]])
  B.bar.cdf <- LIST
}
names (B.bar.cdf) <- B.bar.names#apply the vbl names to the list elements
rm(B.bar, B.bar.sorted, B.runs, design, B.bar.names, LIST)

#perform Anderson-Darling test to compare B to Bbar. H0 = B and Bbar are from the same population; H1 = B and Bbar are not from the same population
#pvalues are based on 5000 bootstraps
LIST <- list()  
for (i in 1:length (B.cdf)){
  LIST [[i]] <- ad.test (B.cdf[[i]], B.bar.cdf[[i]], method = "simulated", Nsim = 500) #Nsim = bootstraps
  ad.list <- (LIST)  
}
names (ad.list) <- B.names

LIST <- list()
  for (i in 1:length (ad.list)) {
    LIST [[i]] <- ad.list[[i]]$ad[1,4]
    test.results <- (LIST)
  }

ad.results <- as.data.table (do.call (rbind, test.results)) #this takes the list elements from AD results and assembles them as a datatable
ad.results <- cbind (B.names, ad.results) #put the factor names in. Need to be careful here to make sure the order did not change
setnames(ad.results, c("factor", "sim.p.value")) 
sig.results <- subset (ad.results, ad.results$sim.p.value < 0.05)#subset the AD results based on p-value
save(ad.results, sig.results, B.list, B.bar.list, B.cdf, file = paste (results.path,"AD.results.RDA"))






