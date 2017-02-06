# set your local environment
source("local.R")
setwd(working_dir)

# install virtual environment manager
install.packages("packrat")

# create packrat project
packrat::init()

# install packages
install.packages(c("kSamples", "data.table", "dplyr", "reshape2"))

# save environment
packrat::snapshot()

