library(Rtsne)
library(ggplot2)
library(dplyr)
library(magrittr)
library(ggrepel)
library(xlsx)
library(fingerprint)
library(factoextra)
library(rcdk)
library(plotly)

#Original
iris_unique <- unique(iris) # Remove duplicates
iris_matrix <- as.matrix(iris_unique[,1:4])
# Set a seed if you want reproducible results
set.seed(42)
tsne_out <- Rtsne(iris_matrix,pca=FALSE,perplexity=30,theta=0.0) # Run TSNE
tsne_out_iris <- data.frame(x = tsne_out$Y[,1], y = tsne_out$Y[,2])
Groups <- factor(iris_unique$Species)
g <- ggplot(tsne_out_iris,aes(x,y, colour = Groups)) +  geom_point(alpha = 0.8) + theme_bw() + ggtitle("tSNE")
ggplotly(g)

#Shuffle
iris_shuffle <- iris_unique[sample(nrow(iris_unique)),]
iris_matrix2 <- as.matrix(iris_shuffle[,1:4])
# Set a seed if you want reproducible results
#set.seed(42)
tsne_out2 <- Rtsne(iris_matrix2,pca=FALSE,perplexity=30,theta=0.0) # Run TSNE
tsne_out2_iris <- data.frame(x = tsne_out2$Y[,1], y = tsne_out2$Y[,2])
Groups <- factor(iris_shuffle$Species)
g <- ggplot(tsne_out_iris,aes(x,y, colour = Groups)) +  geom_point(alpha = 0.8) + theme_bw() + ggtitle("tSNE")
ggplotly(g)