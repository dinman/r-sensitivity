get.cdf <- function (numeric.variable, df) {
  Fn <- ecdf (numeric.variable)
  return (knots (Fn))
}

get.ad <- function (selection, design, boots){
  
  B <- selection
  design <- melt (design, id.vars = "run_id")
  
    #subset the study design based on B
  B <- subset (design, design$run_id %in% B$run_id)
  B <- setorder (B, variable)
  B.list <- split(B, B$variable)
  B.names <- as.character (unique (B$variable))
  
  #get the cumulative dist function for B
  LIST <- list ()
  for (i in 1: length(B.list)) {
    LIST [[i]] <- get.cdf (B.list[[i]]$value, B.list[[i]])
    B.cdf <- LIST
  }
  names (B.cdf) <- B.names #apply the vbl names to the list elements
  
  #Bbar are all other model runs
  B.bar <- design [ !(design$run_id %in% B$run), ]#this creates the Bbar by subtracting B from the design
  B.bar.sorted <- setorder (B.bar, variable)
  B.bar.list <- split(B.bar.sorted, B.bar.sorted$variable)
  B.bar.names <- as.character (unique (B.bar.sorted$variable))
  
  #get the cumulative dist function for Bbar
  LIST <- list ()
  for (i in 1:length (B.bar.list)) {
    LIST [[i]] <- get.cdf (B.bar.list[[i]]$value, B.bar.list[[i]])
    B.bar.cdf <- LIST
  }
  names (B.bar.cdf) <- B.bar.names#apply the vbl names to the list elements
  
  #perform Anderson-Darling test to compare B to Bbar. H0 = B and Bbar are from the same population; H1 = B and Bbar are not from the same population
  #pvalues are based on bootstraps
        LIST <- list()  
        for (i in 1:length (B.cdf)){
          LIST [[i]] <- ad.test (B.cdf[[i]], B.bar.cdf[[i]], method = "simulated", Nsim = boots) #Nsim = bootstraps
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
  
  sig.results <- subset (ad.results, ad.results$sim.p.value < 0.01)#subset the AD results based on p-value
  
  sig.results.design <- subset (B, variable %in% sig.results$factor)
  
  setorder (sig.results.design, run_id, variable)
  
  B.summary <- tapply (sig.results.design$value, sig.results.design$variable, summary)
  
  B.summary <- as.data.table (do.call (rbind, B.summary))
  
  
  B.bar <- subset (B.bar, variable %in% sig.results$factor)
  
  B.bar.summary <- tapply (B.bar$value, B.bar$variable, summary)
  
  B.bar.summary <- as.data.table (do.call (rbind, B.bar.summary))
  
  results <- data.table (factor = sig.results$factor, p.value = sig.results$sim.p.value, 
                         B.med = B.summary$Median, B.bar.med = B.bar.summary$Median)
  return (results)
  
  }
  