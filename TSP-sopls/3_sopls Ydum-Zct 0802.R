# clean
rm(list = ls()); graphics.off()

library(openxlsx)
library(multiblock)
library(ggrepel)
library(ggpubr)

# plotType = "tif"


# import Zs data ----------------------------------------------------------

## Z raw data
rawdata <- read.xlsx("z_dataset_consumer_Cuong.xlsx", rowNames = T)  #center: consumer across each group of attitude
Zraw <- as.data.frame(t(rawdata[-c(1,2),]))

Zraw_hea <- Zraw[,1:20]
Zraw_tas <- Zraw[,21:ncol(Zraw)]

# set names for further anal.
hea <- Zraw_hea
tas <- Zraw_tas


# import Y ----------------------------------------------------------------

load("dtaYdum.Rdata")
Ydum <- dtaYdum

# Ydum <- dtaYdum[,-3]  #remove cluster 3


# combine data ------------------------------------------------------------

dataset <- cbind(tas, hea, Ydum)

## remove clus3, and consumers in this cluster
dataset <- dataset[which(dataset$clus3 == 0), ]
dataset <- dataset[,-ncol(dataset)]

colnames(dataset)


# Create data I() ---------------------------------------------------------

dataI <- data.frame(tas=I(as.matrix(dataset[,1:18])),
                    hea=I(as.matrix(dataset[,19:38])),
                    Ydum=I(as.matrix(dataset[,39:40])))   #remove cluster 2, Z_center (instead of scale)



# run sopls ---------------------------------------------------------------

### select one of two X orders

# so.step2 <- sopls(Ydum ~ tas + hea, data = dataI,
#                   ncomp=c(2,2), max_comps=6, validation="CV", segments=5,
#                   sequential = T, scale = F)

so.step2 <- sopls(Ydum ~ hea + tas, data = dataI,
                  ncomp=c(2,2), max_comps=6, validation="CV", segments=5,
                  sequential = T, scale = F)

# print(so.step2)
summary(so.step2)

source("xplained.R")
xplained(so.step2)

## Maage plot

png("Maage plot YZ (Sequential).png", width = 1000, height = 1000, res = 150)

old.par <- par(mfrow=c(2,2))
maageSeq(so.step2, expl_var = F)
maageSeq(so.step2, expl_var = F, 1)
maageSeq(so.step2, expl_var = F, c(1,1))  #(1,1) for health-taste, (1,0) for taste-health
par(old.par)

dev.off()


## PCP
optcomp <- c(1,1)
PCP <- pcp(so.step2, ncomp = optcomp)


# -------------------------------------------------------------------------
# plot --------------------------------------------------------------------

consName <- rownames(dataset)
YdumName <- colnames(dataset)[39:40]  #need to set


# PCP score ---------------------------------------------------------------

png("PCP Ydum scores.png", width = 1000, height = 1000, res = 150)

source("gg_score_plot 0711.R")
gg_score_plot(PCP, consName, "PCP scores", ck=F, digit = 2)

dev.off()


# PCP Z&Y correlation loadings --------------------------------------------

# Znames <- c("taste","health"); Yname <- "Ydum"

Znames <- c("health","taste"); Yname <- "Ydum"

png("PCP Z&Ydum correlation loadings.png", width = 1000, height = 1000, res = 150)

source("gg_correlation_loadings_plot_Ydum 0711.R")
gg_correlation_loadings_plot_Ydum(so.step2,PCP,Znames,Yname,1:2,Ylab = F,digit=2)

dev.off()
