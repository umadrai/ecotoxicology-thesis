library(openxlsx)
library(tidyr)
library(dplyr)
library(data.table)
library(stringr)
library(ggplot2)
library(ggbreak)
#STEPS for EXTACTING DATA
#1 Read all data
#2 Only keep the columns which are required (CAS, SMILES, Species, Effect, Endpoint, Duration value, Duration Unit, Unit)
#3 Extract data for species (Daphnia magna and Danio rerio)

# Convert mg/L to mol/L
# first mg to g divide by 1000
# Then divide by mol weight. that is mol/L


##### Time Units conversion to hours.
# d = 24 hours
# week =168
# 60 mins = 1 hour
# 3600 sec = 1 hour
convert_to_hours <- function(time, unit){
  
  print(length(time))
  print(length(unit))
  #Check for lengths of both list
  if( length(time) != length(unit)){
    stop ("Error! Some values are missing")
  }
  # Create duration empty list
  duration = list()
  for (i in 1:length(unit)){
    #print(i, time[i]), unit[i])
    #cat(sprintf("%.0f %.0f %s \n", i, time[i], unit[i]))
    if(is.na(time[i])){
      duration[i] = 0
    }
    else if (unit[i] == "h"){
      duration[i] = time[i]
    }
    else if (unit[i] == "d") {
      duration[i] = time[i] * 24
    }
    else if (unit[i] == "wk") {
      duration[i] = time[i] * 168
    }
    else if (unit[i] == "min") {
      duration[i] = time[i] / 60
    }
    else if (unit[i] == "Second(s)") {
      duration[i] = time[i] / 3600
    }
    # if(i == 54){
    #   print(5*24)
    #   print(time[i])
    #   print(i)
    #   print(time[54])
    # }
  }
  return (duration)
}


# # Function for plotting uses log10 trans
charts_plot <- function(dataa, x, y){

   #x <- as.data.frame(dataa[,2])
   #y <- as.data.frame(dataa[,3])
   #print(dim(x))
   #dataa %>% mutate(low = dataa * 0.75, high = dataa * 1.25)

   plot <- ggplot(data = dataa, aes(x = x, y = y)) +
     geom_point() + scale_x_continuous(trans='log10') +
     scale_y_continuous(trans='log10') + geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)

   return(plot)
}

#Trying if function works.
plt1 <- charts_plot(mean_24, mean_24$mol_L.x, mean_24$mol_L.y)
plt1





 #tst <- apply(final_LCEC[,c('Duration','Duration_Unit')],1, function(x) convert_to_hours(x['Duration'], x['Duration_Unit']))

# Step 1: Reading excel file
read_file <- openxlsx::read.xlsx(xlsxFile = "G:\\thess\\Thesis\\URai\\Final DATA\\ECOTOX new_orig.xlsx", 1)

# Step 2: Keep required columns
all_data <- read_file[, c(2,3,6,10,11,12,14,20,22)]
old_names <- colnames(all_data)
new_names <- c("CAS", "Smiles", "Species", "Endpoint", "Effect", "Duration", "Duration_Unit", "Value", "Unit")

setnames(all_data, old = old_names, new = new_names)


#To remove NAs USE:
#remove_na_CAS <- all_data %>% drop_na(CAS.Number)

# Step 3: Extract Data for Species
danio_rerio <- select(filter(all_data, Species == "Danio rerio"), new_names)
na_Smies_danio <- danio_rerio[is.na(danio_rerio$Smiles),]

daphnia_magna <- select(filter(all_data, Species == "Daphnia magna"), new_names)
na_Smies_daphnia <- daphnia_magna[is.na(daphnia_magna$Smiles),]

#Separating into LC50 + EC50 and LOEC + NOEC
# LC50 + EC50
danio_rerio_LC <- select(filter(danio_rerio, (Endpoint == "LC50" | Endpoint == "EC50") & Effect == "Mortality"), new_names)
#NA Smiles in danio_rerio_LC
sum(is.na(danio_rerio_LC$Smiles))
ppm_danio <- filter(danio_rerio_LC, str_detect(Unit,"ppm"))
#Filter out ppm
danio_rer <- anti_join(danio_rerio_LC, ppm_danio)
#70

daphnia_manga_LC <- select(filter(daphnia_magna, (Endpoint == "LC50" | Endpoint == "EC50") & Effect == "Mortality"), new_names)
#NA Smiles in daphnia_magna_LC
sum(is.na(daphnia_manga_LC$Smiles))
ppm_daphnia <- filter(daphnia_manga_LC, str_detect(Unit,"ppm"))
#Filter out ppm
daphni_mag <- anti_join(daphnia_manga_LC, ppm_daphnia)

#192

# LOEC + NOEC
danio_rerio_Lo <- select(filter(danio_rerio, (Endpoint == "LOEC" | Endpoint == "NOEC") & Effect == "Mortality"), new_names)
#NA Smiles in danio_rerio_Lo
sum(is.na(danio_rerio_Lo$Smiles))
#77

daphnia_manga_Lo <- select(filter(daphnia_magna, (Endpoint == "LOEC" | Endpoint == "NOEC") & Effect == "Mortality"), new_names)
#NA Smiles in daphnia_magna_Lo
sum(is.na(daphnia_manga_Lo$Smiles))
#77


# Concatenate Danio rerio and Daphnia magna data for both EC50 + LC50
# and LOEC + NOEC
final_LCEC <- rbind(danio_rerio_LC, daphnia_manga_LC)
final_LOEC <- rbind(danio_rerio_Lo, daphnia_manga_Lo)

# Extracting ppm units
ppm_LCEC <- filter(final_LCEC, str_detect(Unit,"ppm"))
ppm_LOEC <- filter(final_LOEC, str_detect(Unit, "ppm"))
# Removing ppm units from final LCEC and LOEC
final_LCEC <- anti_join(final_LCEC, ppm_LCEC)
final_LOEC <- anti_join(final_LOEC, ppm_LOEC)

# Common Danio Daphnia LC EC
danio_cas <- as.data.frame(danio_rer$CAS)
setnames(danio_cas, old ="danio_rer$CAS", new = "cas")
daphnia_cas <- as.data.frame( daphni_mag$CAS)
setnames(daphnia_cas, old ="daphni_mag$CAS", new = "cas")
common_LC <- inner_join(danio_cas, daphnia_cas)

# Getting all the data with common CAS
common_all <- inner_join(final_LCEC, common_LC, by = c("CAS" = "cas"))
#Remove NA values unit cells.
common_no_NA <- common_all[!is.na(common_all$Value),]
# Creating other units DF
other_unit <- common_no_NA[common_no_NA$Unit != "mg/L",]
#Removing other units from sum
only_mgl <- anti_join(common_no_NA, other_unit)
#Remove CAS values which only have once species.
# 55406-53-6, 50-00-0, 2234562, 142-59-6
single_sp = data.frame(CAS = c("55406-53-6", "50-00-0", "2234562", "142-59-6"))
#only_mgl_final <- anti_join(only_mgl, single_sp) Works fine but using the method below
all_dur_units <- only_mgl[!(only_mgl$CAS %in% single_sp$CAS),]

# Convert all Duration to h.
# time = list(96, 2, 1, 3600, 120, 96, 7)
# un = list('h','d', 'wk','d','Second(s)','min','d')
duration <- convert_to_hours(all_dur_units$Duration, all_dur_units$Duration_Unit)
only_mgl_hours <- as.data.frame(matrix(unlist(duration)), rows= length(duration), by.row = TRUE)
only_mgl_hours[] <- lapply(only_mgl_hours, as.integer)
colnames(only_mgl_hours)[1] <- "Duration"
#Add h unit column 
only_mgl_hours["Duration_Unit"] <- "h"
# Replace 0s with Mean
#only_mgl_hours[only_mgl_hours$Duration == 0,1] <- as.integer(mean(only_mgl_hours$Duration))

final_data_LCEC <- cbind(only_mgl_final[,-c(6,7)], only_mgl_hours)

# Writing output to EXCEL
openxlsx::write.xlsx(final_data_LCEC, "LC_EC50_new.xlsx")
openxlsx::write.xlsx(final_LOEC, "NOEC_LOEC2.xlsx")





# Read Molecular Weights (cleaned file)
m_weights <- openxlsx::read.xlsx(xlsxFile = "C:\\Users\\uulhassa\\Desktop\\URai\\Final DATA\\v2\\mol_weights.xlsx", 2)
mw_list <- (m_weights$CAS.Number)
"9016-45-9" %in% final_LCEC$CAS
which(final_LCEC$CAS == "9016-45-9")
with_mw <- merge(final_data_LCEC, m_weights, by.x = "CAS", by.y = "CAS.Number",all.x = TRUE)

#CAS with no molecular weights.
cas_no_mw <- testing[which(is.na(testing$Molecular.Weight)),]

#Remove Cas which dont have molecular weights
LCEC50 <- anti_join(testing, cas_no_mw)
# In g-L: Divide by 100
LCEC50$new_Value <- as.numeric(LCEC50$Value)/1000
#Remove Da unit from Mol weight column
LCEC50$Molecular.Weight <- gsub(" Da", "", LCEC50$Molecular.Weight)
# Divide by Mol weights to get MOL/L
LCEC50$mol_L <- as.numeric(LCEC50$new_Value)/ as.numeric(LCEC50$Molecular.Weight)
# Removing extra columns
LCEC50 <- LCEC50[, -c(6,7,10,11)]
# Writing Final dataset for LC EC 50.
openxlsx::write.xlsx(LCEC50, "LC_EC50_final.xlsx", overwrite = TRUE)









# Read final data file.
#C:\Users\uulhassa\Desktop\URai\Final DATA\v2
data <- openxlsx::read.xlsx(xlsxFile = "C:\\Users\\umadu\\Documents\\ecotoxicology-thesis\\Data\\LCEC_50_final.xlsx", 1)

# Cross Joining data and filtering, Using duration to keep it same for both species.
crossed <- left_join(data, data, by = c("CAS", "Duration"))

# Dropping extra columns 
crossed <- crossed[, -c(9,11,12,13)]

# Question for Sylvia: What to do with other values which dont have second species for comparison.
# Use first CAS for example.

#Keep different species
crossed <- filter(crossed, Species.x == "Daphnia magna"  & Species.y == "Danio rerio")

# Set first species to Daphnia magna: Done in above filter
#crossed_final <- filter(crossed, Species.x == "Daphnia magna")

#Data for 24,48,72,96 days 
hours24 <- crossed[(crossed$Duration >= 20 & crossed$Duration <= 26),]
hours48 <- crossed[(crossed$Duration >= 44 & crossed$Duration <= 53),]
hours72 <- crossed[(crossed$Duration >= 68 & crossed$Duration <= 72),]
hours96 <- crossed[(crossed$Duration >= 90 & crossed$Duration <= 100),]
# This bin is empty as there is no crossed data for this duration
# Can be because it only appears for one species.
hours480 <- crossed[(crossed$Duration >= 480 & crossed$Duration <= 552),]

# MEAN, MEDIAN and STANDARD DEVIATION 24
mean_24 <- hours24 %>% group_by(CAS) %>% summarise_at(vars(mol_L.x, mol_L.y), mean)
median_24 <- hours24 %>% group_by(CAS) %>% summarise_at(vars(mol_L.x, mol_L.y), median)
#sd_24 <- hours24 %>% group_by(CAS) %>% summarise_at(vars(mol_L.x, mol_L.y), sd)

#Chart for 24 hours
plot_mean_24 <- charts_plot(mean_24, mean_24$mol_L.x, mean_24$mol_L.y)
plot_mean_24


plot_median_24 <- charts_plot(median_24, median_24$mol_L.x, median_24$mol_L.y)
plot_median_24

#SD chart as 0 sd value for Danio Rerio. the points which are at y = 0
sd_24 <- sd_24[!is.na(sd_24$mol_L.x),]
plot_sd_24 <- charts_plot(sd_24, sd_24$mol_L.x, sd_24$mol_L.y)
plot_sd_24


# MEAN, MEDIAN and STANDARD DEVIATION 48
mean_48 <- hours48 %>% group_by(CAS) %>% summarise_at(vars(mol_L.x, mol_L.y), mean)
median_48 <- hours48 %>% group_by(CAS) %>% summarise_at(vars(mol_L.x, mol_L.y), median)
#sd_48 <- hours48 %>% group_by(CAS) %>% summarise_at(vars(mol_L.x, mol_L.y), sd)


#Chart for 48 hours
plot_mean_48 <- charts_plot(mean_48, mean_48$mol_L.x, mean_48$mol_L.y)
plot_mean_48

plot_median_48 <- charts_plot(median_48, median_48$mol_L.x, median_48$mol_L.y)
plot_median_48

#SD chart as 0 sd value for Danio Rerio. the points which are at y = 0
sd_48 <- sd_48[!is.na(sd_48$mol_L.x),]
plot_sd_48 <- charts_plot(sd_48, sd_48$mol_L.x, sd_48$mol_L.y)
plot_sd_48


# MEAN, MEDIAN and STANDARD DEVIATION 72
mean_72 <- hours72 %>% group_by(CAS) %>% summarise_at(vars(mol_L.x, mol_L.y), mean)
median_72 <- hours72 %>% group_by(CAS) %>% summarise_at(vars(mol_L.x, mol_L.y), median)
#sd_72 <- hours72 %>% group_by(CAS) %>% summarise_at(vars(mol_L.x, mol_L.y), sd)


#Chart for 48 hours
plot_mean_72 <- charts_plot(mean_72, mean_72$mol_L.x, mean_72$mol_L.y)
plot_mean_72

plot_median_72 <- charts_plot(median_72, median_72$mol_L.x, median_72$mol_L.y)
plot_median_72

#SD chart as 0 sd value for Danio Rerio. the points which are at y = 0
sd_72 <- sd_72[!is.na(sd_72$mol_L.x),]
plot_sd_72 <- charts_plot(sd_72, sd_72$mol_L.x, sd_72$mol_L.y)
plot_sd_72


# MEAN, MEDIAN and STANDARD DEVIATION 96
mean_96 <- hours96 %>% group_by(CAS) %>% summarise_at(vars(mol_L.x, mol_L.y), mean)
median_96 <- hours96 %>% group_by(CAS) %>% summarise_at(vars(mol_L.x, mol_L.y), median)
#sd_96 <- hours96 %>% group_by(CAS) %>% summarise_at(vars(mol_L.x, mol_L.y), sd)

#Chart for 48 hours
plot_mean_96 <- charts_plot(mean_96, mean_96$mol_L.x, mean_96$mol_L.y)
plot_mean_96

plot_median_96 <- charts_plot(median_96, median_96$mol_L.x, median_96$mol_L.y)
plot_median_96

#SD chart as 0 sd value for Danio Rerio. the points which are at y = 0
sd_96 <- sd_96[!is.na(sd_96$mol_L.x),]
plot_sd_96 <- charts_plot(sd_96, sd_96$mol_L.x, sd_96$mol_L.y)
plot_sd_96
