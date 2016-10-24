#Monte Carlo filtering
#based on Saltelli 2010
# D.Inman 10122016

rm(list=ls())
R_LIBS= ("/home/R/library")
options(scipen=999) #turn scientific notation off
setwd("~/GitHub/mcf-data/")

#load data
load ("mcf.data.RDA")

df <- study.design             
sub <- subset (df, ! (CE.Background.Subs.Feedstock.NA > 19 | CE.Policy.Dur.FCI.BC > 29 | CE.Policy.Dur.FCI.TC > 29
                        |CE.Policy.Start.Price.BC > 2024))






