rm(list=ls())
R_LIBS= ("/home/R/library")
options(scipen=999) #turn scientific notation off
setwd("~/GitHub/r-sensitivity/")
results.path <- ("~/GitHub/r-sensitivity/results/")

source ("1.AD.R")
source ("2.bootstrap.R")
source ("3.subset.design.R")