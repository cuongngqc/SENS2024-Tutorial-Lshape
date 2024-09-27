gg_correlation_loadings_plot_0617 <- function(SOPLS,PCP,Xname,Yname,
                                         comps=1:2,Ylab=F){
  
  # SOPLS <- so.step1
  # PCP <- PCP
  # Xname <- c("qda","auc")
  # Yname <- "cons"
  # comps=1:2
  # Ylab=F
  
  X1ncol <- dim(SOPLS$data$X[[1]])[2]
  X2ncol <- dim(SOPLS$data$X[[2]])[2]
  Yncol <- dim(SOPLS$data$Y)[2]
  
  X1colaname <- paste0(colnames(SOPLS$data$X[[1]]),".1")
  X2colaname <- paste0(colnames(SOPLS$data$X[[2]]),".2")
  YcolName <- colnames(SOPLS$data$Y)
  
  ## create data
  corXL <- list()
  for (i in 1:length(SOPLS$data$X)) {
    corXL[[i]] <- cor(SOPLS$data$X[[i]], PCP$scores[,comps])
  }
  
  corY <- cor(SOPLS$data$Y, PCP$scores[,comps])
  
  corX.rb <- do.call(rbind,corXL)
  rbdta <- rbind(corX.rb, corY)
  dtaplot <- data.frame(rbdta, lab = NA, gr = NA)
  # dtaplot$lab <- rownames(dtaplot)
  
  dtaplot$lab <- c(X1colaname, X2colaname, YcolName)
  
  # dtaplot$lab[1:(X1ncol+X2ncol)] <- ""  #hide attribute labels
  
  if (!Ylab) dtaplot$lab[(X1ncol+X2ncol+1):(X1ncol+X2ncol+Yncol)] <- ""  #hide consumer labels
  
  dtaplot$gr[1:X1ncol] <- rep(Xname[1],X1ncol)
  dtaplot$gr[(X1ncol+1):(X1ncol+X2ncol)] <- rep(Xname[2],X2ncol)
  dtaplot$gr[(X1ncol+X2ncol+1):(X1ncol+X2ncol+Yncol)] <- rep(Yname,Yncol)
  
  ## plot
  p <- ggplot(dtaplot, aes(Comp.1, Comp.2, label = lab)) + 
    geom_point(color = ifelse(dtaplot$gr == Yname, "forestgreen", 
                              ifelse(dtaplot$gr == Xname[1], "red", "blue")),
               shape = ifelse(dtaplot$gr == Yname, 18, 
                              ifelse(dtaplot$gr == Xname[1], 1, 2))) +
    geom_text_repel(colour = ifelse(dtaplot$gr == Yname, "forestgreen", 
                                    ifelse(dtaplot$gr == Xname[1], "red", "blue")),
                    max.overlaps = 15) +
    xlim(-1,1) + ylim(-1,1)
  p1 <- p + geom_vline(xintercept = 0, linetype = 3) +
    geom_hline(yintercept = 0, linetype = 3) + 
    labs(title = "PCP correlation loadings",
         x = paste0("\n Comp 1 (", round(PCP$explvar[1],1), "%)"),
         y = paste0("\n Comp.2 (", round(PCP$explvar[2],1),"%)")) +
    theme(plot.title = element_text(hjust = 0.5, size = rel(1.3), face = "bold"),
          panel.background = element_rect(fill = "white", colour = "black"))
  print(p1)
  
}