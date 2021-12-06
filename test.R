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

#standard worked
#maccs worked
#extended worked
#graph worked
#circular worked
#pubchem not worked

# Create list of the fingerprints types
fps_list = list("standard", "maccs", "extended", "graph", "circular", "pubchem")
message = "Select the type of Fingerprints:
        1: Standard
        2: MACCS
        3: Extended
        4: Graph
        5: Circular
        6: PubChem"
if(interactive()){
  user_input <- readline(prompt = message)
  type_fps <- fps_list[[as.integer(user_input)]]
}
else { type_fps <- "maccs"}

# Generating Fingerprints (fps)
fps_final <- lapply(not_null, get.fingerprint, type = "graph", fp.mode = "bit")

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
g <- ggplot(tsne_out_DF,aes(x,y)) +  geom_point(alpha = 0.8) + theme_bw() + ggtitle(paste("tSNE - ", type_fps, "FPS", sep = " "))
ggplotly(g)



#Generating PCA
res <- prcomp(joined[,col_index:length(joined)])
res <- fviz_pca_ind(res, col.ind = "cos2", label="none", addEllipses=TRUE, ellipse.level=0.5)
res

