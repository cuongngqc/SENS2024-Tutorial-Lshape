# clean
rm(list = ls()); graphics.off()

library(openxlsx)
library(dplyr)
library(lpls)

library(matlib)
library(dummies)
library(ggplot2)
library(ggrepel)


# import data -------------------------------------------------------------

Xdta <- read.xlsx("senso_std.xlsx", sheet = 1, rowNames = T, colNames = T)

Ydta <- read.xlsx("likYdumave_dct.xlsx", sheet = 1, rowNames = T, colNames = T)

Zdta <- read.xlsx("Zsc.xlsx", sheet = 1, rowNames = T)

rownames(Xdta) <- rownames(Ydta)

save(Xdta, Ydta, Zdta, file = "lplsDta.RData")

##
load("lplsDta.RData")

shortName <- substr(colnames(Ydta),2,4)


# centering and scaling data ----------------------------------------------

Xsc <- as.matrix(Xdta)
Zsc <- as.matrix(Zdta)
Ydct <- as.matrix(Ydta)

colnames(Ydct) <- rownames(Zsc) <- shortName


# -------------------------------------------------------------------------
# run lpls by in-house codes ----------------------------------------------

maxPC = 5

Wx <- matrix(NA, nrow = ncol(Xsc), ncol = maxPC)
Wz <- matrix(NA, nrow = ncol(Zsc), ncol = maxPC)
Tx <- matrix(NA, nrow = nrow(Xsc), ncol = maxPC)
Tz <- matrix(NA, nrow = nrow(Zsc), ncol = maxPC)
Px <- matrix(NA, nrow = ncol(Xsc), ncol = maxPC)
Pz <- matrix(NA, nrow = ncol(Zsc), ncol = maxPC)
expl_X<-rep(NA,maxPC)
expl_Z<-rep(NA,maxPC)
expl_Yx<-rep(NA,maxPC)
expl_Yz<-rep(NA,maxPC)

X_i <- Xsc; Z_i <- Zsc

for (i in 1:maxPC) {
  # i = 1
  dtarun <- t(X_i) %*% Ydct %*% Z_i
  resSVD <- svd(dtarun)
  
  ## weights
  w_X_i <- resSVD$u[,1]
  w_Z_i <- resSVD$v[,1]
  
  ## scores
  t_X_i <- X_i %*% w_X_i
  t_Z_i <- Z_i %*% w_Z_i
  
  ## loadings
  p_X_i <- t(X_i) %*% t_X_i %*% ((t(t_X_i) %*% t_X_i)^-1)
  p_Z_i <- t(Z_i) %*% t_Z_i %*% ((t(t_Z_i) %*% t_Z_i)^-1)
  
  ## residuals
  E_X_i <- X_i - (t_X_i %*% t(p_X_i))
  E_Z_i <- Z_i - (t_Z_i %*% t(p_Z_i))
  
  ## % var explained
  expl_X[i]<-(t(t_X_i) %*% t_X_i)/sum(diag(t(Xsc)%*%Xsc))
  expl_Z[i]<-(t(t_Z_i) %*% t_Z_i)/sum(diag(t(Zsc)%*%Zsc))
  Ypredx_i=t_X_i %*% ((t(t_X_i) %*% t_X_i)^-1)%*%t(t_X_i)%*%Ydct
  expl_Yx[i]<-sum(diag(Ypredx_i %*% t(Ypredx_i)))/sum(diag(Ydct%*%t(Ydct)))
  Ypredz_i=t_Z_i %*% ((t(t_Z_i) %*% t_Z_i)^-1)%*%t(t_Z_i)%*%t(Ydct)
  expl_Yz[i]<-sum(diag(t(Ypredz_i) %*% Ypredz_i))/sum(diag(t(Ydct)%*%Ydct))
  
  ## save resutls
  Wx[,i] <- w_X_i; Wz[,i] <- w_Z_i
  Tx[,i] <- t_X_i; Tz[,i] <- t_Z_i
  Px[,i] <- p_X_i; Pz[,i] <- p_Z_i
  
  ## deflated X,Z
  X_i <- E_X_i
  Z_i <- E_Z_i
}


# % explained variance ---------------------------------------------------

# Xsc explained by the Tx comp  *100
round(expl_X*100,1)
# t(Zsc) explained by the Tz comp *100
round(expl_Z*100,1)
# Ydct explained by the Tx comp *100 (products liking scores explained by the products attributes components) )
round(expl_Yx*100,1)
# t(Ydct) explained by the Tz  *100 (consumers liking scores explained by the consumers attributes components)
round(expl_Yz*100,1)


# Correlations between data and model -------------------------------------

## X-variables and X-score vectors Tx
R_X_Tx <- matrix(NA, nrow = ncol(Xsc), ncol = maxPC)
for (comp in 1:maxPC) {
  #comp = 1
  for (k in 1:ncol(Xsc)){
    #k = 1
    r_k_comp <- (t(Xsc[,k]) %*% Tx[,comp]) / (sqrt(t(Xsc[,k]) %*% Xsc[,k]) * sqrt(t(Tx[,comp]) %*% Tx[,comp])) 
    R_X_Tx[k,comp] <- r_k_comp
  }
}

## Z-vaiables and Z-score vectors Tz
R_Z_Tz <- matrix(NA, nrow = ncol(Zsc), ncol = maxPC)
for (comp in 1:maxPC) {
  #comp = 1
  for (l in 1:ncol(Zsc)){
    #l = 1
    r_l_comp <- (t(Zsc[,l]) %*% Tz[,comp]) / (sqrt(t(Zsc[,l]) %*% Zsc[,l]) * sqrt(t(Tz[,comp]) %*% Tz[,comp])) 
    R_Z_Tz[l,comp] <- r_l_comp
  }
}

## Y-vaiables and X-score vectors Tx
R_Y_Tx <- matrix(NA, nrow = ncol(Ydct), ncol = maxPC)
for (comp in 1:maxPC) {
  #comp = 1
  for (j in 1:ncol(Ydct)){
    #j = 1
    r_j_comp <- (t(Ydct[,j]) %*% Tx[,comp]) / (sqrt(t(Ydct[,j]) %*% Ydct[,j]) * sqrt(t(Tx[,comp]) %*% Tx[,comp])) 
    R_Y_Tx[j,comp] <- r_j_comp
  }
}

## Y-vaiables and Z-score vectors Tz
R_Y_Tz <- matrix(NA, nrow = nrow(Ydct), ncol = maxPC)
for (comp in 1:maxPC) {
  #comp = 1
  for (i in 1:nrow(Ydct)){
    #i = 1
    r_i_comp <- (Ydct[i,] %*% Tz[,comp]) / (sqrt(Ydct[i,] %*% t(Ydct[i,,drop=F])) * sqrt(t(Tz[,comp]) %*% Tz[,comp])) 
    R_Y_Tz[i,comp] <- r_i_comp
  }
}

## X-indicator and X-score vectors Tx
df <- data.frame(id = 1:8, product = paste0("P",1:8))
df <- cbind(df, dummy(df$product, sep = ""))
Imat <- as.matrix(df[,-c(1,2)])

R_I_Tx <- matrix(NA, nrow = nrow(Imat), ncol = maxPC)

for (comp in 1:maxPC) {
  #comp = 1
  for (i in 1:nrow(Imat)){
    #i = 1
    r_i_comp <- (t(Imat[,i]) %*% Tx[,comp]) / (sqrt(t(Imat[,i]) %*% Imat[,i]) * sqrt(t(Tx[,comp]) %*% Tx[,comp]))
    #r_i_comp <- cov(Imatc[,i],Tx[,comp])/(sd(Imat[,i])*sd(Tx[,comp]))
    R_I_Tx[i,comp] <- r_i_comp
  }
}


# plot --------------------------------------------------------------------

plt = 1  #1 to plot

if (plt == 1) {
  windows(10,10)
  dtaplot <- data.frame(R_X_Tx, lab = colnames(Xsc))
  p <- ggplot(dtaplot, aes(X1, X2, label = lab)) +  #X1, X2 are colnames of dtaplot[,1:2] 
    geom_point(color = "red", shape = 4) +
    geom_text_repel(colour = "red") +
    xlim(-1,1) + ylim(-1,1)
  p1 <- p + geom_vline(xintercept = 0, linetype = 3) +
    geom_hline(yintercept = 0, linetype = 3) + 
    labs(title = "X-variables (Sensory attributes) \n", 
         x = "\n PC 1", y = "PC 2 \n") +
    theme(plot.title = element_text(hjust = 0.5, size = rel(1.3), face = "bold"),
          panel.background = element_rect(fill = "white", colour = "black"))
  print(p1)
  # savePlot("X-variables (Sensory attributes)", type = "tiff")
  savePlot("X-variables (Sensory attributes)", type = "pdf")
  
  ## R_Z_Tz (consumer attitudes)
  windows(10,10)
  dtaplot <- data.frame(R_Z_Tz, lab = colnames(Zsc))
  p <- ggplot(dtaplot, aes(X1, X2, label = lab)) + 
    geom_point(color = "black", shape = 6) +
    geom_text_repel(colour = "black") +
    xlim(-1,1) + ylim(-1,1)
  p1 <- p + geom_vline(xintercept = 0, linetype = 3) +
    geom_hline(yintercept = 0, linetype = 3) + 
    labs(title = "Z-variables (Consumer attitudes) \n", 
         x = "\n PC 1", y = "PC 2 \n") +
    theme(plot.title = element_text(hjust = 0.5, size = rel(1.3), face = "bold"),
          panel.background = element_rect(fill = "white", colour = "black"))
  print(p1)
  # savePlot("Z-variables (Consumer attitudes)", type = "tiff")
  savePlot("Z-variables (Consumer attitudes)", type = "pdf")
  
  ## R_Y_Tx (liking vs Tx)
  windows(10,10)
  dtaplot <- data.frame(R_Y_Tx, lab = colnames(Ydct))
  
  # dtaplot$lab <- ""   # Hide all of the text labels
  
  p <- ggplot(dtaplot, aes(X1, X2, label = lab)) + 
    geom_point(color = "blue", shape = 16) +
    geom_text_repel(colour = "blue") +
    xlim(-1,1) + ylim(-1,1)
  p1 <- p + geom_vline(xintercept = 0, linetype = 3) +
    geom_hline(yintercept = 0, linetype = 3) + 
    labs(title = "Y-variables (Consumer liking vs Tx) \n", 
         x = "\n PC 1", y = "PC 2 \n") +
    theme(plot.title = element_text(hjust = 0.5, size = rel(1.3), face = "bold"),
          panel.background = element_rect(fill = "white", colour = "black"))
  print(p1)
  # savePlot("Y-variables (Liking vs Tx)", type = "tiff")
  savePlot("Y-variables (Liking vs Tx)", type = "pdf")
  
  ## R_Y_Tz (liking vs Tz)
  windows(10,10)
  dtaplot <- data.frame(R_Y_Tz, lab = rownames(Ydct))
  p <- ggplot(dtaplot, aes(X1, X2, label = lab)) + 
    geom_point(color = "blue", shape = 1) +
    geom_text_repel(colour = "blue") +
    xlim(-1,1) + ylim(-1,1)
  p1 <- p + geom_vline(xintercept = 0, linetype = 3) +
    geom_hline(yintercept = 0, linetype = 3) + 
    labs(title = "Product vs Tz \n", 
         x = "\n PC 1", y = "PC 2 \n") +
    theme(plot.title = element_text(hjust = 0.5, size = rel(1.3), face = "bold"),
          panel.background = element_rect(fill = "white", colour = "black"))
  print(p1)
  # savePlot("Product vs Tz", type = "tiff")
  savePlot("Product vs Tz", type = "pdf")
  
  ## R_I_Tx (product vs Tx)
  windows(10,10)
  dtaplot <- data.frame(R_I_Tx, lab = rownames(Xsc))
  p <- ggplot(dtaplot, aes(X1, X2, label = lab)) + 
    geom_point(color = "forestgreen", shape = 8) +
    geom_text_repel(colour = "forestgreen") +
    xlim(-1,1) + ylim(-1,1)
  p1 <- p + geom_vline(xintercept = 0, linetype = 3) +
    geom_hline(yintercept = 0, linetype = 3) + 
    labs(title = "Product vs Tx \n", 
         x = "\n PC 1", y = "PC 2 \n") +
    theme(plot.title = element_text(hjust = 0.5, size = rel(1.3), face = "bold"),
          panel.background = element_rect(fill = "white", colour = "black"))
  print(p1)
  # savePlot("Product vs Tx", type = "tiff")
  savePlot("Product vs Tx", type = "pdf")
  
  
  ## R_Y_Tz and R_I_Tx in same plot
  rbdta <- rbind(R_Y_Tz, R_I_Tx)
  dtaplot <- data.frame(rbdta, lab = NA, gr = NA)
  dtaplot$lab[1:8] <- paste0("P",1:8,"_Tz")
  dtaplot$lab[9:16] <- paste0("P",1:8,"_I")
  dtaplot$gr[1:8] <- rep("Tz",8)
  dtaplot$gr[9:16] <- rep("I",8)
  
  windows(10,10)
  p <- ggplot(dtaplot, aes(X1, X2, label = lab)) + 
    geom_point(color = ifelse(dtaplot$gr == "Tz", "blue", "forestgreen"),
               shape = ifelse(dtaplot$gr == "Tz", 1, 8)) +
    geom_text_repel(colour = ifelse(dtaplot$gr == "Tz", "blue", "forestgreen")) +
    xlim(-1,1) + ylim(-1,1)
  p1 <- p + geom_vline(xintercept = 0, linetype = 3) +
    geom_hline(yintercept = 0, linetype = 3) + 
    labs(title = "Product vs Tz, I \n", 
         x = "\n PC 1", y = "PC 2 \n") +
    theme(plot.title = element_text(hjust = 0.5, size = rel(1.3), face = "bold"),
          panel.background = element_rect(fill = "white", colour = "black"))
  print(p1)
  # savePlot("Product vs Tz, I", type = "tiff")
  savePlot("Product vs Tz, I", type = "pdf")
}


# -------------------------------------------------------------------------
# run lpls by lpls pack. --------------------------------------------------

X1 = as.matrix(Xsc); X2 = as.matrix(Ydct); 
X3 = as.matrix(t(Zsc))  #X3: matrix of attitudes as rows, consumers as columns 

save(X1, X2, X3, file = "lplsDta_X123.RData")
load("lplsDta_X123.RData")


## run endo-LPLS
fit.endo <- lpls(X1,X2,t(X3), npc=2, type="endo")

##
expl.varX1 <- round(fit.endo$vars$X1varprop * 100,0)
expl.varX2 <- round(fit.endo$vars$X2varprop * 100,0)
expl.varX3 <- round(fit.endo$vars$X3varprop * 100,0)

save(fit.endo, expl.varX1, expl.varX2, expl.varX3,
     file = "endoLPLS_res.Rdata")

## run exo-LPLS with non-orthogonal scores:
fit.exo <- lpls(X1,X2,t(X3), npc=2, type="exo")

##
expl.varX1 <- round(fit.exo$vars$X1varprop * 100,0)
expl.varX2 <- round(fit.exo$vars$X2varprop * 100,0)
expl.varX3 <- round(fit.exo$vars$X3varprop * 100,0)

save(fit.exo, expl.varX1, expl.varX2, expl.varX3,
     file = "exoLPLS_res.Rdata")
