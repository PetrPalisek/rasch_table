mirt_plots <- function(model, save = FALSE){
  require(mirt)
  
  # Get column names for items
  item_ids <- names(
    eval(model@Call[["data"]])
  )
  
  # Loop through empirical plots outputted by mirt::itemfit
  for(item in 1:model@Data[["nitems"]]){
    
    
    p <- mirt::itemfit(model, empirical.plot = item)
    p[["main"]] <- paste("Empirical plot for item", item_ids[item], sep = " ")
    print(p)
    
  # Store them if requested  
    if(save == TRUE){
      png(paste0("misfit_plot", item_ids[item], ".png"), width = 10, height = 7, units = "in", res = 300)
      print(p)
      dev.off()
    }
  }
  
  
}
