run.mood <- function (selection, design, boots, design.melted = FALSE, alpha){
  
  B <- selection
  
  ifelse (design.melted == FALSE,
  design <- melt (design, id.vars = "run_id"),
  design <- design)
  
     #subset the study design based on B
  print('Subsetting design')
  incProgress(0.1, detail = "Subsetting design...")
  
  B <- subset (design, design$run_id %in% B$run_id)
  B <- setorder (B, factor)
  B.list <- split(B, B$factor)
  B.names <- as.character (unique (B$factor))
  
  
  #Bbar are all other model runs
  print("Selecting B-bar")
  names(vbsa.design)[names(vbsa.design) == "run"] <- "run_id"
  B.bar <- design [! (design$run_id %in% B$run), ]
  #B.bar <- subset (design, design$run_id %in% bbar$run_id)
  B.bar.sorted <- setorder (B.bar, factor)
  B.bar.list <- split(B.bar.sorted, B.bar.sorted$factor)
  B.bar.names <- as.character (unique (B.bar.sorted$factor))
  
  incProgress(0.3, detail = "Performing Mood's test to compare B to B-bar...")
   
  LIST <- list()  
     for (i in 1:length (B.list)){
          LIST [[i]] <- mood.test (B.list[[i]]$value, B.bar.list[[i]]$value)
          mood.list <- (LIST)  
        }
        
  
  names (mood.list) <- B.names
  
  str(mood.list[[1]][2]$p.value)
  
  #print("Creating list of results")
  print("Creating list of results")
  incProgress(0.1, detail = "Compiling results...")
  
  LIST <- list()
    for (i in 1:length (mood.list)) {
        LIST [[i]] <- mood.list[[i]][2]$p.value
        test.results <- (LIST)
      }
  
  #print("Assembling results")
  mood.results <- as.data.table (do.call (rbind, test.results)) #this takes the list elements from AD results and assembles them as a datatable
  
  mood.results <- cbind (B.names, mood.results) #put the factor names in. Need to be careful here to make sure the order did not change
  setnames(mood.results, c("factor", "p.value")) 

  #print("Selecting significant results")
  print("Selecting significant results")
  incProgress(0.1, detail = "Selecting significant results...")
  
  comps <- (length (mood.list))
  print(comps)
  pval <- (1 - alpha)/comps
  print(pval)
  print(mood.results$p.value)
  sig.results <- subset (mood.results, mood.results$p.value < pval)#subset the AD results based on p-value
  
  sig.results.design <- subset (B, factor %in% sig.results$factor)
  
  setorder (sig.results.design, run_id, factor)
  
  #print("Summarizing results")
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

  results <- data.table (factor = sig.results$factor,
                         B.med = B.summary$Median, B.bar.med = B.bar.summary$Median)
  return (results)
  
  
  }
  