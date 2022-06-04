rasch_table <- function(model, append.rmsea = TRUE, boot = 1000, print.plots = TRUE, rmsea_cut = .1 ) {
  
  require(mirt)
  
  # Get difficulties + SE
  par.table <- round(mirt::coef(model, IRTpars = T, as.data.frame = T, printSE = T),2)
  par.table <- par.table[seq(2,nrow(par.table)-2,4),]
  
  # Get infit/outfit, z+msq
  itemfit.table <- mirt::itemfit(model, method = "ML", fit_stats = "infit", na.rm = T)
  itemfit.table[,2:5] <- round(itemfit.table[,2:4], 2)
  
  # Bind them together
  table <- cbind(par.table, itemfit.table)
  
  # Rename b column and move id col
  rownames(table) <- NULL
  table <- table[,c(3,1,2,4:7)]
  colnames(table)[2] <- "b" 
  
  # If RMSEA requested, estimate X2*_df and append it to the table
  if(append.rmsea == TRUE) {
    print("Computing RMSEA for X2*_df")
    rmsea.table <-  mirt::itemfit(model, method = "ML", fit_stats = "X2*_df", na.rm = T, boot_dfapprox = boot)[,4:5] 
    colnames(rmsea.table) <- c("RMSEA", "p")
    rmsea.table$p.bonferroni <-  rmsea.table$p*nrow(rmsea.table)
    rmsea.table$p.bonferroni <- ifelse( rmsea.table$p.bonferroni > 1, 1,  rmsea.table$p.bonferroni)
    table <- cbind(table, round(rmsea.table,2))
    
  } 
  
  # If plots requested, print items with RMSEA over the specified cut-off
    if(print.plots == TRUE) {
    
    print("Printing plots for misfitting items. You may specify RMSEA cut-off via the rmsea_cut argument.")
    selected_items <- as.numeric(rownames(rmsea.table[rmsea.table$RMSEA > rmsea_cut,]))
    
    for (item in selected_items) {
      p <- mirt::itemfit(model, empirical.plot = item, xlim = c(-3,3), empirical.CI = .90, which.items = item)
      print(p)
    }
    
  } 
  
  return(table)
} 