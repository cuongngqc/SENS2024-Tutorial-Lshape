gg_correlation_loadings_plot_Ydum <- function(SOPLS,PCP,Xname,Yname,
                                         comps=1:2,Ylab=F,digit){
  
  # SOPLS <- so.step2
  # PCP <- PCP
  # Xname <- c("health","taste")
  # Yname <- "dum"
  # comps=1:2
  # Ylab=F
  # digit=2
  
  cols <- palette(c("black", "red", "green3", "blue", "magenta",
                    "darkgoldenrod", "darkgreen", "darkgray", "cyan",
                    "violet", "turquoise", "orange", "lightpink",
                    "lavender", "yellow", "lightgreen", "lightgrey",
                    "lightblue", "darkkhaki", "darkmagenta", "darkolivegreen",
                    "lightcyan", "darkorange", "darkorchid", "darkred",
                    "darksalmon", "darkseagreen", "darkslateblue",
                    "darkslategray", "darkslategrey", "darkturquoise",
                    "darkviolet", "lightgray", "lightsalmon", "lightyellow",
                    "maroon"))
  
  X1ncol <- dim(SOPLS$data$X[[1]])[2]
  X2ncol <- dim(SOPLS$data$X[[2]])[2]
  Yncol <- dim(SOPLS$data$Y)[2]
  
  ## create data
  corXL <- list()
  for (i in 1:length(SOPLS$data$X)) {
    corXL[[i]] <- cor(SOPLS$data$X[[i]], PCP$scores[,comps])
  }
  
  corY <- cor(SOPLS$data$Y, PCP$scores[,comps])
  
  corX.rb <- do.call(rbind,corXL)
  rbdta <- rbind(corX.rb, corY)
  dtaplot <- data.frame(rbdta, lab = NA, gr = NA, seg = NA)
  dtaplot$lab <- rownames(dtaplot)
  colnames(dtaplot)[1:2] <- c("Comp.1","Comp.2")
  
  if (!Ylab) dtaplot$lab[(X1ncol+X2ncol+1):(X1ncol+X2ncol+Yncol)] <- ""  #hide consumer labels
  
  dtaplot$gr[1:X1ncol] <- rep(Xname[1],X1ncol)
  dtaplot$gr[(X1ncol+1):(X1ncol+X2ncol)] <- rep(Xname[2],X2ncol)
  dtaplot$gr[(X1ncol+X2ncol+1):(X1ncol+X2ncol+Yncol)] <- rep(Yname,Yncol)
  
  dtaplot$seg[1:(X1ncol+X2ncol)] <- ""
  dtaplot$seg[(X1ncol+X2ncol+1):(X1ncol+X2ncol+Yncol)] <- 1:Yncol
  
  
  ## plot
  p <- ggplot(dtaplot, aes(Comp.1, Comp.2, label = lab)) + 
    geom_point(color = ifelse(dtaplot$gr == Yname, ifelse(dtaplot$seg == 1, cols[5],
                                                          ifelse(dtaplot$seg == 2, cols[6], cols[21])),
                              ifelse(dtaplot$gr == Xname[1], "red", "blue")),
               shape = ifelse(dtaplot$gr == Yname, 1, 
                              ifelse(dtaplot$gr == Xname[1], 1, 2)),
               size = ifelse(dtaplot$gr == Yname, 10, 1.5)) +
    geom_text_repel(colour = ifelse(dtaplot$gr == Yname, "forestgreen", 
                                    ifelse(dtaplot$gr == Xname[1], "red", "blue")),
                    max.overlaps = 15) +
    geom_text(aes(label = seg),
              colour = ifelse(dtaplot$gr == Yname, ifelse(dtaplot$seg == 1, cols[5],
                                                          ifelse(dtaplot$seg == 2, cols[6], cols[21])), "black"),
              fontface = "plain") +  #"plain", "bold", "italic", "bold.italic"
    xlim(-1,1) + ylim(-1,1)
  
  p1 <- p + geom_vline(xintercept = 0, linetype = 3) +
    geom_hline(yintercept = 0, linetype = 3) + 
    labs(title = "Correlation loadings",
         x = paste0("\n Comp 1 (", round(PCP$explvar[1],digit), "%)"),
         y = paste0("\n Comp 2 (", round(PCP$explvar[2],digit), "%)")) +
    theme(plot.title = element_text(hjust = 0.5, size = rel(1.3), face = "bold"),
          panel.background = element_rect(fill = "white", colour = "black"))
  
  print(p1)
  
}
