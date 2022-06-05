### A mirt helper function that quickly creates a pastable table with:
#                    item difficulties, SE, infit/outfit, RMSEA, p and p with Bonferroni correction
#                    optionally prints empirical vs. expected item plots titled with item names in your data

# Improvements over the original: faster table creation, easy plot renaming via "title" argument,
#                                 does not name plots by item order but by colname

# source("https://raw.githubusercontent.com/PetrPalisek/rasch_table/main/rasch_table.R")


rasch_table <- function(model, append.rmsea = FALSE, boot = 1000, print.plots = FALSE, rmsea_cut = .1,
                        title = "Empirical plot for item", save = FALSE) {
  
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
    
    print("Bootstrapping for mirt::itemfit method X2*_df")
    
    rmsea.table <-  mirt::itemfit(model, method = "ML", fit_stats = "X2*_df", na.rm = T, boot_dfapprox = boot)[,4:5] 
    colnames(rmsea.table) <- c("RMSEA", "p")
    
    rmsea.table$p.bonferroni <-  rmsea.table$p*nrow(rmsea.table)
    rmsea.table$p.bonferroni <- ifelse( rmsea.table$p.bonferroni > 1, 1,  rmsea.table$p.bonferroni)
    
    table <- cbind(table, round(rmsea.table,2))
    
  } 
  
  # If plots requested, print items with RMSEA over the specified cut-off
    if(print.plots == TRUE & append.rmsea == TRUE) {
    
    print("Printing misfit plots for misfitting items. You may specify RMSEA cut-off via the rmsea_cut argument.")
      
    table$number <- as.numeric(rownames( table))
    items_to_plot <- table[table$RMSEA > rmsea_cut,]

    for (item in items_to_plot$number) {
      p <- mirt::itemfit(model, empirical.plot = item, xlim = c(-3,3), empirical.CI = .90, which.items = item)
      p[["main"]] <- paste0(title, " ", table[item, "item"])
      print(p) 
      
      if(save == TRUE) {
        
        png(paste0("misfit_plot", table[item, "item"], ".png"), width = 10, height = 7, units = "in", res = 300)
        print(p)
        dev.off()
        
        
      } else next }
    } else if (print.plots == TRUE & append.rmsea == FALSE) {
      
      print("Printing misfit plots for all items.")
      
      table$number <- as.numeric(rownames(table))
      items_to_plot <- table
      
      for (item in items_to_plot$number) {
        p <- mirt::itemfit(model, empirical.plot = item, xlim = c(-3,3), empirical.CI = .90, which.items = item)
        p[["main"]] <- paste0(title, " ", table[item, "item"])
        print(p)
        
        if(save == TRUE) {
          
          png(paste0("misfit_plot", table[item, "item"], ".png"), width = 10, height = 7, units = "in", res = 300)
          print(p)
          dev.off()
          
          
        } else next
    } 
  

    }  
  return(table[,1:ncol(table)-1])
}


