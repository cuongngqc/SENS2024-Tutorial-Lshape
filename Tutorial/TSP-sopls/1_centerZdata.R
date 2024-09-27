# clean
rm(list = ls()); graphics.off()

library(openxlsx)

rawdata <- read.xlsx("z_dataset_consumer_Cuong.xlsx", rowNames = T)

Zraw <- as.data.frame(t(rawdata[-c(1,2),]))

Zsc <- scale(Zraw, center = T, scale = T)
Zct <- scale(Zraw, center = T, scale = F)

write.xlsx(as.data.frame(Zct), "Zct.xlsx", rowNames = T)
