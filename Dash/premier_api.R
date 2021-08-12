library(Rtsne)
library(ggplot2)
library(dplyr)
library(magrittr)
library(ggrepel)
library(xlsx)
library(plotly)
library(rcdk)
library(xlsx)
library(fingerprint)
library(factoextra)
library(tsne)


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


#Reading Data from Premier API CSV.
premier <- read.csv("PREMIER_API_list_INCHI.csv", header = TRUE, sep = ";")

#Filling missing Smiles from DB SMILES.
#allsmiles <- ifelse((premier$Pubchem_can_SMILES == "" | premier$Pubchem_can_SMILES == -7777), premier$DB_SMILES, premier$Pubchem_can_SMILES)
#pubchem$Pubchem_can_SMILES <- allsmiles

#asg <- subset(premier, ((premier$Pubchem_can_SMILES == "" )& premier$DB_SMILES != ""))

# Checking Empty or invalid smiles
missng <- subset(premier, (Pubchem_can_SMILES == -7777 | Pubchem_can_SMILES == ""))
not_miss <- premier[ !(premier$Pubchem_can_SMILES %in% missng$Pubchem_can_SMILES), ]

# Using this dataset with first 15 columns
premier_used <- not_miss[,1:15]

# Generating Fingerprints matrix
mols_d <- parse.smiles(premier_used[,15])
fps_d <- lapply(mols_d, get.fingerprint, type = "maccs", fp.mode = "bit")
# fpd_expanded <- fp.to.matrix(fps_d)
try <- fp.to.matrix(fps_d)
try <- normalise_mm(try[,1:166]) 
  



#Get smiles from s4 objects and create a list of smiles.
alls <- (attributes(fps_d))$names
sp <- strsplit(alls, " ")

fpd_expanded <- as.data.frame(fps_matrix(fps_d, sp))
# Converting character type columns to numeric for normalisation.
fpd_expanded[,1:166] <- lapply(fpd_expanded[,1:166], as.numeric)
#is.matrix(fpd_expanded)
fpd_expanded[,1:166] <- normalise_mm(fpd_expanded[,1:166])

is.data.frame(premier_used)
fpd_expanded <- as.data.frame(fpd_expanded)
names(fpd_expanded)[167] <- "smiles"
names(premier_used)[15] <- "smiles"
#Joining orig and fingerprints matrix.
joined <- inner_join(premier_used, fpd_expanded, by = "smiles", copy = FALSE)

#Dealing with duplicates
dup <- duplicated(joined)
unique_set <- joined[!duplicated(joined$smiles),]

#joined <- cbind(premier_used, fpd_expanded)
#normalize
#unique_set[,16:181] <- normalise_mm(unique_set[,16:181])

tsne_dc <- Rtsne(unique_set[,16:181], perplexity = 50, check_duplicates = FALSE, pca = TRUE, verbose = TRUE)
#tsne_dc$Y

groups <- as.factor(unique_set$Pubchem_Name)
groups
info.norm = tibble(Compound = groups)
info.norm %<>% mutate(DIM_1 = tsne_dc$Y[, 1], DIM_2 = tsne_dc$Y[,2])
f <- ggplot(info.norm, aes(x = DIM_1, y = DIM_2, colour = 1)) + 
  geom_point(alpha = 0.8) + theme_bw()
fig <- ggplotly(f)
fig


joined[,16:(181)] <- as.data.frame(lapply(joined[,16:181], normalise_mm))


#Filling SMILES from DB to Pubchem to ALL SMILES
pubchem_smiles <- pubchem$Pubchem_can_SMILES
dbsmiles <- db$DB_SMILES

# Filling missing smiles in Pubchem with Smiles from DB.
allsmiles <- ifelse((pubchem$Pubchem_can_SMILES == "" | pubchem$Pubchem_can_SMILES == -7777), db$DB_SMILES, pubchem$Pubchem_can_SMILES)
pubchem$Pubchem_can_SMILES <- allsmiles
#which(allsmiles == -7777)


#Generating TSNE
joined <- cbind(pubchem, fpd_expanded)
which(joined$Pubchem_can_SMILES == "")

#Converting fps_d to Dataframe
n <- length(fps_d[[1]])
fps_df <- structure(fps_d, row.names = c(NA, -n), .Names = seq_along(fps_d), class = "data.frame")

