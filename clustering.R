datdaadata("USArrests")

df <- scale(USArrests)



tsne.res <- Rtsne(df, perplexity = 10, check_duplicates = FALSE, pca = FALSE, verbose = TRUE)
tsne_US <- data.frame(x = tsne.res$Y[,1], y = tsne.res$Y[,2])
g <- ggplot(tsne_US,aes(x,y)) +  geom_point(alpha = 0.8) + theme_bw() + ggtitle(paste("US Arrests tsne"))
ggplotly(g)






head(df, n = 3)

fviz_nbclust(tsne_US, kmeans, method = "wss")

#Before TSNE
km.res <- kmeans(df, 4, nstart = 25)
#print(km.res)
fviz_cluster(km.res, df,
             palette = "Set1", ggtheme = theme_minimal())
#After TSNE
#k = 4
set.seed(123)
km.res <- kmeans(tsne_US, 4, nstart = 25)
#print(km.res)
fviz_cluster(km.res, tsne_US,
             palette = "Set1", ggtheme = theme_minimal())





join_scaled <- scale(joined[,-(1:3)])
fviz_nbclust(join_scaled, kmeans, method = "wss")+
  geom_vline(xintercept = 4, linetype = 2)

km.res <- kmeans(join_scaled, 4, nstart = 25)

fviz_cluster(km.res, join_scaled,
             palette = "Set2", ggtheme = theme_minimal())

#combine with original
dd <- cbind(joined[,1:3], cluster = km.res$cluster)

#TSNE clustering
tsne_scale <- scale(tsne_out_DF)
fviz_nbclust(tsne_scale, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)
km.res <- kmeans(tsne_scale, 10, nstart = 30)

print(km.res)
fviz_cluster(km.res, tsne_scale, geom = c("point"),
             palette = "Set1", ggtheme = theme_classic())
