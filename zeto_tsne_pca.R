#### Zet o Map ####
#Imports
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



normalise_mm <- function(x){
  z = x
  if(min(x) < max(x)){
    z = (x - min(x)) / (max(x) - min(x))
  }
  
  #ifelse(min(x) < max(x), z = (x - min(x)) / (max(x) - min(x)), z = x)
  
  return (z)
}

# Function to generate matrix
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

# Read Excel file
zeto <- read.xlsx(file = "C:\\Users\\uulhassa\\Desktop\\URai\\Zet o Map\\Zetera_GEO_Seazit_coloured.xlsx", 5, header = TRUE)

#Removing duplicate column
zeto <- zeto[-c(2)]

#Elements with no CAS
no_smiles <- zeto[which(is.na(zeto$smile)),]

#Without NAs.
zeto2 <- filter(zeto,!is.na(zeto$smile))

#Generating fingerprints
mols_d <- parse.smiles(zeto2[,6],omit.nulls = FALSE)

#null smiles
null_mols <- Filter(is.null, mols_d)
#Getinng smiles to filter out original df later
null_all <- (attributes(null_mols))$names
null_sp <- strsplit(alls, " ")


#not null
not_null <- Filter(Negate(is.null), mols_d)

fps_zeto<- lapply(not_null, get.fingerprint, type = "maccs", fp.mode = "bit")
#zeto_expanded <- fp.to.matrix(fps_zeto)

### using function fps_matrix ###
#Get smiles from s4 objects and create a list of smiles.
alls <- (attributes(fps_zeto))$names
sp <- strsplit(alls, " ")

zeto_expanded <- as.data.frame(fps_matrix(fps_zeto, sp))

# Zeto expanded has fps and smiles
#Inner join removes null parsed smiles from resulted joined DF as
#zeto_expanded only has parsed smiles matrix values.
names(zeto_expanded)[167] <- "smile"
joined <- inner_join(zeto2, zeto_expanded, by = "smile", copy = FALSE)

# Converting character type columns to numeric for normalisation.
joined[,8:173] <- lapply(joined[,8:173], as.numeric)
#is.matrix(fpd_expanded)
joined[,8:173] <- normalise_mm(joined[,8:173])

#Created subgroup as most values of colums are NA.
sub_group <- joined[,c(1,6, 8:173)]
#Creating TSNE and PCA
set.seed(42)
zeto_tsne <- Rtsne(sub_group, perplexity = 10, theta = 0.0, max_iter = 2000, check_duplicates = FALSE, pca = TRUE, verbose = TRUE)
tsne_out_DF <- data.frame(x = zeto_tsne$Y[,1], y = zeto_tsne$Y[,2])

special <- c("64175", "5436431", "1746016", "50328",
             "80057", "79947", "51525","84742", "1763231",
             "45298906", "375735", "57465288", "302794", "7784465",
             "17804352", "137304", "7440439", "67747095", "60571",
             "75912", "6051872", "129000", "87865", "7440224")


#Missing blue colored CAS.
missing <- c("79947","1763231")

#Assigning colors column
sub_group["group"] = "Normal"
#Assigning Special to blue colored CAS
for (x in special) {
  if (x %in% sub_group$cas) {
    sub_group$group[sub_group$cas == x] <-  "Special"
  }
}

Groups <- factor(sub_group$group)
g <- ggplot(tsne_out_DF,aes(x,y, colour = Groups)) +  geom_point(alpha = 0.8) + theme_bw() + ggtitle("tSNE")
ggplotly(g)


#Generating PCA
res <- prcomp(sub_group[3:168])
res <- fviz_pca_ind(res, col.ind = "cos2", label="none", habillage=sub_group$group, addEllipses=TRUE, ellipse.level=0.5)

#Smiles which were not parsed filtering so can be written to xlsx file.
sp_smiles <- filter(zeto2,zeto2$smile %in% null_sp)