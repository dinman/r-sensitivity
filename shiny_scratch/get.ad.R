get.cdf <- function (numeric.factor, df) {
  Fn <- ecdf (numeric.factor)
  return (knots (Fn))
}

get.ad <- function (selection, design, boots, design.melted = FALSE){
  B <- selection
  
  ifelse (design.melted == FALSE,
          design <- melt (design, id.vars = "run_id"),
          design <- design)
  
  print('Subsetting design')
  incProgress(0.1, detail = "Subsetting design...")
  
    #subset the study design based on B
  B <- subset (design, design$run_id %in% B$run_id)
  B <- setorder (B, factor)
  B.list <- split(B, B$factor)
  B.names <- as.character (unique (B$factor))
  
  #get the cumulative dist function for B
  print("Getting CDF for B")
  LIST <- list ()
  for (i in 1: length(B.list)) {
    LIST [[i]] <- get.cdf (B.list[[i]]$value, B.list[[i]])
    B.cdf <- LIST
  }
  names (B.cdf) <- B.names #apply the vbl names to the list elements
  
  #print("Selecting B-bar")
  #Bbar are all other model runs
  names(vbsa.design)[names(vbsa.design) == "run"] <- "run_id"
  B.bar <- design [! (design$run_id %in% B$run), ]#this creates the Bbar by subtracting B from the design
  #B.bar <- subset (design, design$run_id %in% bbar$run_id)
  B.bar.sorted <- setorder (B.bar, factor)
  B.bar.list <- split(B.bar.sorted, B.bar.sorted$factor)
  B.bar.names <- as.character (unique (B.bar.sorted$factor))

  print("Getting CDF for B-bar")
  incProgress(0.1, detail = "Getting CDF for B-bar...")
  #get the cumulative dist function for Bbar
  LIST <- list ()
  for (i in 1:length (B.bar.list)) {
    LIST [[i]] <- get.cdf (B.bar.list[[i]]$value, B.bar.list[[i]])
    B.bar.cdf <- LIST
  }
  names (B.bar.cdf) <- B.bar.names#apply the vbl names to the list elements
  
  print("Performing AD test to compare B to B-bar")
  incProgress(0.3, detail = "Performing AD test to compare B to B-bar...")
  #perform Anderson-Darling test to compare B to Bbar. H0 = B and Bbar are from the same population; H1 = B and Bbar are not from the same population
  #pvalues are based on bootstraps
        LIST <- list()  
        for (i in 1:length (B.cdf)){
          LIST [[i]] <- ad.test (B.cdf[[i]], B.bar.cdf[[i]], method = "simulated", Nsim = boots) #Nsim = bootstraps
          ad.list <- (LIST)  
        }
        
  
  names (ad.list) <- B.names
  
  print("Creating list of results")
  incProgress(0.1, detail = "Compiling results...")
  LIST <- list()
  for (i in 1:length (ad.list)) {
    LIST [[i]] <- ad.list[[i]]$ad[1, 3]
    test.results <- (LIST)
  }
  
  print("Assembling results")
  ad.results <- as.data.table (do.call (rbind, test.results)) #this takes the list elements from AD results and assembles them as a datatable
  
  print("Adding factor names")
  ad.results <- cbind (B.names, ad.results) #put the factor names in. Need to be careful here to make sure the order did not change
  setnames(ad.results, c("factor", "sim.p.value")) 

  print("Selecting significant results")
  incProgress(0.1, detail = "Selecting significant results...")
  
  sig.results <- subset (ad.results, ad.results$sim.p.value < 1e9) #0.000000001)#subset the AD results based on p-value
  
  sig.results.design <- subset (B, factor %in% sig.results$factor)
  
  setorder (sig.results.design, run_id, factor)
  
  print("Summarizing results")
  B.summary <- tapply (sig.results.design$value, sig.results.design$factor, summary)
  
  B.names <- names (B.summary)
  
  B.summary <- as.data.table (do.call (rbind, B.summary))
  
  B.summary$factor <- B.names
  
  setorder (B.summary, factor)
  
  B.bar <- subset (B.bar, factor %in% sig.results$factor)
  
  B.bar.summary <- tapply (B.bar$value, B.bar$factor, summary)
  
  B.bar.names <- names (B.bar.summary)
  
  B.bar.summary <- as.data.table (do.call (rbind, B.bar.summary))
  
  B.bar.summary$factor <- B.bar.names
  
  setorder (B.bar.summary, factor)
  
  incProgress(0.1, detail = "Returning results...")
  
  results <- data.table (factor = sig.results$factor, p.value = sig.results$sim.p.value, 
                         B.med = B.summary$Median, B.bar.med = B.bar.summary$Median)
  return (results)
  
  }
  