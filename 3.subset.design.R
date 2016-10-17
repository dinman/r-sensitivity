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
#load data
load (paste (results.path, "bootstrapped.samples.RDA"))
load ("design.RDA")

#filtering criteria - B-Q3 to Bbar-Q3
filter.upper <- apply (B.sig, 2, quantile, c(0.95))
filter.lower <- apply (B.bar.sig, 2, quantile, c(0.75))
filter <- cbind (filter.upper, filter.lower)
rm(B.sig, B.bar.sig, filter.upper, filter.lower)

#the naming convention used in the BSM is ugly and needs some work before we can use it in R
#change variable names in the large study design
df<-design.in.bsm.format
merge.var.names <- paste (df$variable, df$sub1, df$sub2)
temp.design <- data.table(variable = merge.var.names, run = df$run, value = df$value)

df <- as.data.frame (lapply(temp.design, function(x) gsub(" ", ".", x)))
df <- dcast (df, run ~ variable)

df[,2:46] = apply(df[,2:46], 2, function(x) as.numeric(as.character(x))) # the data have to be numeric prior to performing the logical expression below


sub <- subset (df, ! (CE.Background.Subs.FCI.NA > 0.50 | CE.Background.Subs.Feedstock.NA > 13 | CE.Policy.Dur.FCI.BC > 14.25 |
                        CE.Policy.Dur.Feedstock.BC > 17 | CE.Policy.Dur.Price.BC > 10 | CE.Startup.Subs.FCI.NA > 0.57 |
                        CE.Startup.Subs.Loan.NA > 0.58 | DS.Frac.FCI.subs.NA.NA > 0.82))

new.set.runs <- data.table (run = sub$run)

new.design <- design.in.bsm.format [(design.in.bsm.format$run %in% new.set.runs$run)]

save (new.design, df,  file = paste (results.path, "new.design.RDA"))

write.csv (new.design, file = paste (results.path, "mcf.filtered.design.csv")) #output this to be run in BSM

