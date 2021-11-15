#Rtsne Examples using iris
library(Rtsne)
library(ggplot2)
library(dplyr)
library(magrittr)
library(ggrepel)
#library(xlsx)
library(fingerprint)
library(factoextra)
library(rcdk)
library(plotly)
library(openxlsx)


#Generating PCA
mydata <- iris

pca = prcomp(mydata[,-5],
             center = T,
             scale. = T)
screeplot(pca,type = "line")

data_plot <- as.data.frame(pca$x)
data_plot$source <- mydata$Species
#data_plot$source <- gsub("_.*.", "", data_plot$source ) 
variance.percent_1 <- round(factoextra::get_eig(pca)$variance.percent,digits = 2)
pca.plot <- ggplot(data_plot,aes(x=PC1,y=PC2,color=source)) + 
  geom_point(aes(alpha=0.5)) +
  theme(panel.background = element_blank(),
        legend.position = "right",
        panel.border=element_rect(fill=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background=element_blank(),
        axis.text.x=element_text(colour="black"),
        axis.text.y=element_text(colour="black"),
        axis.ticks=element_line(colour="black"),
        plot.margin=unit(c(1,1,1,1),"line")) +
  xlab(paste0("PC1"," (",variance.percent_1[1],"%)")) +
  ylab(paste0("PC1"," (",variance.percent_1[2],"%)")) +
  scale_alpha_continuous(guide=FALSE) +
  stat_ellipse( level = 0.95)+
  labs(title = "PCA")
pca.plot



#Generate TSNE
mydata.tsne <- unique(iris)
mydata.tsne$Species <- as.factor(mydata.tsne$Species)

tsne = Rtsne(mydata.tsne[,-5], dims = 2, perplexity = 30, 
             verbose=TRUE, max_iter = 1000, pca = T)

set.seed(42)
tsne.res <- Rtsne(mydata.tsne, perplexity = 30, check_duplicates = FALSE, pca = FALSE, verbose = TRUE)
tsne_out_DF <- data.frame(x = tsne.res$Y[,1], y = tsne.res$Y[,2])
g <- ggplot(tsne_out_DF,aes(x,y, colour = mydata.tsne$Species)) +  geom_point(alpha = 0.8) + theme_bw() + ggtitle("IRIS - TSNE")
ggplotly(g)


