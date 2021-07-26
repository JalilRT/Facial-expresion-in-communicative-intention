library(data.table)
library(tidyverse)
library(corrplot)
library(cowplot)
library(gridGraphics)

#colocar el directorio donde están los archivos
setwd('/home/jalil/Documents/Doctorado/Pragmaticlab/JN_Article/exp2')

## Chi
Data.exp2 <- read_csv('AH_Emo_dat.csv')

Data.exp2$Emocion <- Data.exp2$Emocion %>%
  str_replace_all("Alegria", "Joy") %>%
  str_replace_all("Enojo", "Anger") %>% 
  str_replace_all("Tristeza", "Sadness") %>%
  str_replace_all("Miedo","Fear") %>% 
  str_replace_all("Sorpresa", "Surprise") %>% 
  str_replace_all("Asco", "Disgust")
Data.exp2$AH <- Data.exp2$AH %>%
  str_replace_all("Orden", "Order") %>%
  str_replace_all("Exigencia", "Demand") %>% 
  str_replace_all("Peticion", "Petition") %>% 
  str_replace_all("Ruego", "E.Request")

chiData.exp2 <- chisq.test(Data.exp2$Emocion, Data.exp2$AH, correct = FALSE)
chiData.exp2$observed

#### Figure 3, corr ###
corrplot(t(chiData.exp2$residuals), is.corr = FALSE,method="color",
         tl.cex = 1.1, cl.ratio = 0.4,bg="black",addgrid.col = "black",
         tl.col = "black",tl.srt=45,col= colorRampPalette(c("#0072B5FF","white","#BC3C29FF"))(9)) 
mtext("X²(4, 4560) = 11877; p < 0.001", at=4.5, line=-26, cex=1)
#mtext("A", at=0, line=-2.5, cex=1.5)
grid.echo()
fig2 <- grid.grab()

ggsave2("fig2.png",fig2,width = 10.2,height = 6,dpi = 500)
