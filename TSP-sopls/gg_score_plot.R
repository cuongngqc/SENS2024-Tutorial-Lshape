gg_score_plot <- function(PCP, row_name, title_name, ck){ #use for both SO-PLS, MB-PLS
  
  # PCP = mb.step1
  # row_name = NULL
  # title_name = NULL
  # ck = F
  
  if (ck) {
    PCPscore <- as.data.frame(PCP$scores[,,drop=F])
    PCPscore <- -1*PCPscore  #check
  } else {
    PCPscore <- as.data.frame(PCP$scores[,,drop=F])
  }
  
  colnames(PCPscore) <- paste0("Comp.",1:length(PCP$explvar))
  PCPscore$lab <- row_name
  
  p <- ggplot(PCPscore, aes(Comp.1, Comp.2, label=lab)) + 
    geom_point(color = "black", shape = 15) +
    geom_text_repel(colour = "black", max.overlaps = 15)
  p1 <- p + geom_vline(xintercept = 0, linetype = 3) +
    geom_hline(yintercept = 0, linetype = 3) + 
    labs(title = title_name,
         x = paste0("\n Comp 1 (", round(PCP$explvar[1],1), "%)"),
         y = paste0("\n Comp 2 (", round(PCP$explvar[2],1),"%)")) +
    theme(plot.title = element_text(hjust = 0.5, size = rel(1.3), face = "bold"),
          panel.background = element_rect(fill = "white", colour = "black"))
  print(p1)
}