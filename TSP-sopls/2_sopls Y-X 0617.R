# clean
rm(list = ls()); graphics.off()

library(openxlsx)
library(multiblock)
library(ggrepel)

plotType = "tif"


# import data -------------------------------------------------------------

## X-blocks

## data with all attributes
load("dataAUC_intervals.Rdata")

##
braw <- dataL[[2]]
mraw <- dataL[[3]]
eraw <- dataL[[4]]
meraw <- dataL[[5]]

b_ct <- scale(braw, center = T, scale = F)
m_ct <- scale(mraw, center = T, scale = F)
e_ct <- scale(eraw, center = T, scale = F)
me_ct <- scale(meraw, center = T, scale = F)

b_sc <- scale(braw, center = T, scale = T)
m_sc <- scale(mraw, center = T, scale = T)
e_sc <- scale(eraw, center = T, scale = T)
me_sc <- scale(meraw, center = T, scale = T)


## Y-block
lik <- read.xlsx("datlik_ct.xlsx", rowNames = T)


# combine data ------------------------------------------------------------

# -------------------------------------------------------------------------
## b, m, e
# -------------------------------------------------------------------------

bdata <- b_sc
medata <- me_sc

dataset <- cbind(bdata, medata, lik)

## check colnames
colnames(dataset)


# Create data I() ---------------------------------------------------------

## b,m-e
dataI <- data.frame(bdata=I(as.matrix(dataset[,1:10])),
                    medata=I(as.matrix(dataset[,11:20])),
                    lik=I(as.matrix(dataset[,21:121])))


# run sopls ---------------------------------------------------------------

so.step1 <- sopls(lik ~ bdata + medata, data = dataI,
                  ncomp=c(2,2), max_comps=6, validation="CV", segments=5,
                  sequential = T, scale = F)

# print(so.step1)
summary(so.step1)

source("xplained.R")
xplained(so.step1)


## Maage plot
windows(10,10)
old.par <- par(mfrow=c(2,2))
maageSeq(so.step1, expl_var = F)
maageSeq(so.step1, expl_var = F, 2)
maageSeq(so.step1, expl_var = F, c(2,1))
par(old.par)

savePlot("Maage plot (Sequential)", type = plotType)


## extract residuals
dimnames(so.step1$fitted)[[3]]
residuals <- so.step1$data$Y - so.step1$fitted[,,8]  #component combination 8: [2,1]
# save(residuals, file="resMat_sopls.Rdata")


## PCP
optcomp <- c(2,1)
PCP <- pcp(so.step1, ncomp = optcomp)
# summary(PCP)

PCP$explvar
cumsum(PCP$explvar)


# -------------------------------------------------------------------------
# plot --------------------------------------------------------------------

prodName <- rownames(dataset)
consName <- colnames(dataset)[21:121]  #need to set


# PCP score ---------------------------------------------------------------

# windows(10,10); scoreplot(PCP, labels = prodName, 
#                           xlim = c(-0.7,0.7), ylim = c(-0.7, 0.7)) #score

windows(10,10)
source("gg_score_plot.R")
gg_score_plot(PCP, prodName, "PCP scores", ck=F)
savePlot("PCP Y scores", type = plotType)


# PCP X&Y correlation loadings --------------------------------------------

## correlation loadings by ggplot2 (nqc)

Xnames <- c("TDS (b)","TDS (m-e)"); Yname <- "cons"

windows(10,10)
source("gg_correlation_loadings_plot_0617.R")
gg_correlation_loadings_plot_0617(so.step1,PCP,Xnames,Yname,Ylab = F)
savePlot("PCP X&Y correlation loadings", type = plotType)


# get clusters from PCP correlation loadings plot ----------------------------------------

corY <- cor(so.step1$data$Y, PCP$scores[,1:2])  #Y correlation-loadings
varcoord <- corY

clus1 <- varcoord[which(varcoord[,1] > 0 & varcoord[,2] > 0),]
clus2 <- varcoord[which(varcoord[,1] > 0 & varcoord[,2] < 0),]
clus3 <- varcoord[which(varcoord[,1] < 0),]

clus <- list(clus1, clus2, clus3)

save(clus, file="clusSOPLSplot.Rdata")
