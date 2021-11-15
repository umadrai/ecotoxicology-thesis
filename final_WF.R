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

##### Normalising Function #####
normalise_mm <- function(x){
  z = x
  if(min(x) < max(x)){
    z = (x - min(x)) / (max(x) - min(x))
  }
  return (z)
}


# Function to generate fps matrix along with smiles
fps_matrix <- function(fplist, sp){
  #df = data.frame()
  size <- fplist[[1]]@nbit
  mat <- matrix(0, nrow = length(fplist), ncol = size+1)
  count <- 1
  
  for (i in fplist){
    #df[count, i@bits] <- 1
    
    mat[count, i@bits] <- 1
    mat[count, size+1] <- as.character(sp[count])
    #mat <- cbind(mat, alls[i])
    count <- count +1
  }
  mat # <- cbind(sp, mat)
  
}

###################################################
# tsne_dc <- Rtsne(d_c[,15:180], perplexity = 50, check_duplicates = FALSE, pca = TRUE, verbose = TRUE)
# s <- factor(d_c[,13])
# s
# 
# par(mfrow=c(1,1)) # To plot two images side-by-side
# #plot(tsne_dc$Y, col = "blue", pch = 19, cex = 1.5) # Plotting the first image
# plot(tsne_dc$Y, col = "black", bg= (s), pch = 21, cex = 1.5)
# levels(IR_species)
# 
# tsne_dc$Y
# is.data.frame(tsne_dc$Y)
# 
# c <- as.data.frame(tsne_dc$Y)
# c
# 
# info.norm = tibble(Compound = s)
# 
# info.norm %<>% mutate(DIM_1 = tsne_dc$Y[, 1], DIM_2 = tsne_dc$Y[,2])
# 
# ggplot(info.norm, aes(x = DIM_1, y = DIM_2, colour = Compound)) + 
#   geom_point(alpha = 0.8) + theme_bw()
# 
# 
# 
# ###PCa 
# pca.norm = prcomp(d_c[, 15:180])
# pca.norm$x
# pca.norm$scale
# info.norm %<>% cbind(pca.norm$x[, 1:166])
# ggplot(info.norm, aes(x = PC1, y = PC2, colour = Compound)) + 
#   geom_point(alpha = 0.8) + theme_bw()
# 
# summary(pca.norm)
# 
# 
# #### Kaggle
# # visualizing
# colors = rainbow(length(unique(d_c$source)))
# names(colors) = unique(d_c$source)
# plot(tsne_dc$Y, t='n', main="tsne")
# text(tsne_dc$Y, labels=d_c$source, col=colors[d_c$source])

#################
#### Reading all the groups files 
read_file <- openxlsx::read.xlsx(xlsxFile = "C:\\Users\\uulhassa\\Desktop\\URai\\Zet o Map\\Matthias\\zet-o-map_chem_space_fps.xlsx", 1)

#column_names <- names(read_file)
#column_names

#EXTRACTING columns which will be used
use_df <- read_file %>% select(contains(c("id", "smiles", "Sources")))
#names(use_df)[length(use_df)] <- "SMILES"


## Fingerprints List which needs to be added.
## MAACS, Morgan, Atom-pairs and Atom


#Elements with no SMILES
no_smiles <- use_df[which(is.na(use_df$smiles)),]

#Without NAs.
main <- filter(use_df,!is.na(use_df$smiles))

#### Fingerprints Amino
mols_d <- parse.smiles(main$smiles,omit.nulls = TRUE)

# Getting NULL smiles, will be 0 when omit.nulls = TRUE
null_mols <- Filter(is.null, mols_d)

# Getinng smiles to filter out original df later
null_all <- (attributes(null_mols))$names
null_sp <- strsplit(null_all, " ")

#not null
not_null <- Filter(Negate(is.null), mols_d)

# Generating maccs fps
fps_final <- lapply(not_null, get.fingerprint, type = "maccs", fp.mode = "bit")

# Get smiles from s4 objects and create a list of smiles.
# Helps to append the smiles in fps matrix later to helps to join original and fps matrix/DF
alls <- (attributes(fps_final))$names
smiles_list <- strsplit(alls, " ")
expanded_fps <- as.data.frame(fps_matrix(fps_final, smiles_list))

#Change name of last column so it can be used in join
names(expanded_fps)[length(names(expanded_fps))] <- "smiles"

##############

#Inner join removes null parsed smiles from resulted joined DF as
#expanded_fps only has parsed smiles matrix values.

joined <- inner_join(main, expanded_fps, by = "smiles", copy = FALSE)

#### To get index of V1 column
col_index <- which(colnames(joined) == "V1")
# Converting character type columns to numeric for normalisation.
joined[,col_index:length(joined)] <- lapply(joined[,col_index:length(joined)], as.numeric)
# Normalizing using function normalise_mm
#joined[,col_index:length(joined)] <- as.data.frame(lapply(joined[,col_index:length(joined)], normalise_mm))
#joined[,4:169] <- as.data.frame(lapply(joined[,4:169], normalise_mm))

joined[, col_index: length(joined)] <- normalise_mm(joined[, col_index: length(joined)])
#joined2 <- joined

#Duplicattes
dup <- duplicated(joined)
joined <- joined[!duplicated(joined$smiles),]

#groups <- as.factor(use_df$)
#TSNE
set.seed(42)
tsne.res <- Rtsne(joined, perplexity = 30, check_duplicates = FALSE, pca = FALSE, verbose = TRUE)
tsne_out_DF <- data.frame(x = tsne.res$Y[,1], y = tsne.res$Y[,2])
g <- ggplot(tsne_out_DF,aes(x,y)) +  geom_point(alpha = 0.8) + theme_bw() + ggtitle("tSNE")
ggplotly(g)



#Generating PCA
res <- prcomp(joined[,col_index:length(joined)])
res <- fviz_pca_ind(res, col.ind = "cos2", label="none", addEllipses=TRUE, ellipse.level=0.5)
res

#########
# main with x,y values from tsne.
#main <- cbind(info.norm[,2:3])
#orig <- main[,1:5]
#orig <- cbind(orig, info.norm[2:3])


# For sampling (Not used for now)
#set.seed(42)
##############
#### shuffle rows of df randomly to test the output on tsne
# main2 <- joined[sample(nrow(joined)),]
# tsne2 <- Rtsne(main2, perplexity = 30, check_duplicates = FALSE, pca = TRUE, verbose = TRUE)
# info.norm = tibble(Compound = groups)
# info.norm %<>% mutate(DIM_1 = tsne2$Y[, 1], DIM_2 = tsne2$Y[,2])
# f <- ggplot(info.norm, aes(x = DIM_1, y = DIM_2, colour = Compound)) + 
#   geom_point(alpha = 0.8) + theme_bw()
# fig <- ggplotly(f)
# fig
# tsne_out_DF2 <- data.frame(x = tsne2$Y[,1], y = tsne2$Y[,2])
# g <- ggplot(tsne_out_DF2,aes(x,y)) +  geom_point(alpha = 0.8) + theme_bw() + ggtitle("tSNE")
# ggplotly(g)

