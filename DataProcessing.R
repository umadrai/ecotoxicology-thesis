library(openxlsx)
library(tidyr)
library(dplyr)
library(data.table)
library(stringr)
#STEPS for EXTACTING DATA
#1 Read all data
#2 Only keep the columns which are required (CAS, SMILES, Species, Effect, Endpoint, Duration value, Duration Unit, Unit)
#3 Extract data for species (Daphnia magna and Danio rerio)


##### Time Units conversion to hours.
# d = 24 hours
# week =168
# 60 mins = 1 hour
# 3600 sec = 1 hour
convert_to_hours <- function(time, unit){
  
  #Check for lengths of both list
  if( length(time) != length(unit)){
    stop ("Error! Some values are missing")
  }
  # Create duration empty list
  duration = list()
  for (i in 1:length(unit)){
    if (unit[[i]] == 'd') {
      duration[[i]] = time[[i]] * 24
    }
    else if (unit[[i]] == "wk") {
      duration[[i]] = time[[i]] * 168
    }
    else if (unit[[i]] == "min") {
      duration[[i]] = time[[i]] / 60
    }
    else if (unit[[i]] == "Second(s)") {
      duration[[i]] = time[[i]] / 3600
    }
  }
  return (duration)
}

#tst <- apply(final_LCEC[,c('Duration','Duration_Unit')],1, function(x) convert_to_hours(x['Duration'], x['Duration_Unit']))

# Step 1: Reading excel file
read_file <- openxlsx::read.xlsx(xlsxFile = "C:\\Users\\uulhassa\\Desktop\\URai\\Final DATA\\ECOTOX new_orig.xlsx", 1)

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

# Writing output to EXCEL
openxlsx::write.xlsx(final_LCEC, "LC_EC502.xlsx")
openxlsx::write.xlsx(final_LOEC, "NOEC_LOEC2.xlsx")

# Common Danio Daphnia LC EC
danio_cas <- as.data.frame(danio_rer$CAS)
daphnia_cas <- as.data.frame( daphni_mag$CAS)
common_LC <- inner_join(danio_cas, daphnia_cas)
