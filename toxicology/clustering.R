library(factoextra)
#df <- scale(USArrests)


create_clusters <- function(data){
  
  #number of clusters
  #fviz_nbclust(tsne_US, kmeans, method = "wss")
  
  km.res <- kmeans(data, 4, nstart = 25)
  
  cluster_plot <- fviz_cluster(km.res, data,
                               palette = "Set1", ggtheme = theme_minimal())
  
  all_data <- as.data.frame(cbind(data, cluster = km.res$cluster))
  cluster_plot
  ret <- list(cluster_plot, all_data)
  return(ret)
}


#Test output of function
# df <- scale(USArrests)
# results <- create_clusters(df)
# details <- results[[2]]
# clust_plot <- results[[1]]


#tsne.res <- Rtsne(df, perplexity = 10, check_duplicates = FALSE, pca = FALSE, verbose = TRUE)
#tsne_US <- data.frame(x = tsne.res$Y[,1], y = tsne.res$Y[,2])
#g <- ggplot(tsne_US,aes(x,y)) +  geom_point(alpha = 0.8) + theme_bw() + ggtitle(paste("US Arrests tsne"))
#ggplotly(g)

#head(df, n = 3)
# 
# fviz_nbclust(tsne_US, kmeans, method = "wss")
# 
# #Before TSNE
# km.res <- kmeans(df, 4, nstart = 25)
# #print(km.res)
# fviz_cluster(km.res, df,
#              palette = "Set1", ggtheme = theme_minimal())
# 
# #combine with original
# dd <- as.data.frame(cbind(df, cluster = km.res$cluster))
# km.res
# #After TSNE
# #k = 4
# set.seed(123)
# km.res2 <- kmeans(tsne_US, 4, nstart = 25)
# #print(km.res)
# fviz_cluster(km.res2, tsne_US,
#              palette = "Set1", ggtheme = theme_minimal())
# 
# 
# print(km.res)
# 
# 
# join_scaled <- scale(joined[,-(1:3)])
# fviz_nbclust(join_scaled, kmeans, method = "wss")+
#   geom_vline(xintercept = 4, linetype = 2)
# 
# km.res <- kmeans(join_scaled, 4, nstart = 25)
# 
# fviz_cluster(km.res, join_scaled,
#              palette = "Set2", ggtheme = theme_minimal())
# 
# #combine with original
# dd <- cbind(joined[,1:3], cluster = km.res$cluster)
# 
# #TSNE clustering
# tsne_scale <- scale(tsne_out_DF)
# fviz_nbclust(tsne_scale, kmeans, method = "wss") +
#   geom_vline(xintercept = 4, linetype = 2)
# km.res <- kmeans(tsne_scale, 10, nstart = 30)
# 
# print(km.res)
# fviz_cluster(km.res, tsne_scale, geom = c("point"),
#              palette = "Set1", ggtheme = theme_classic())
