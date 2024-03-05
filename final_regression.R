library(fingerprint)
library(factoextra)
library(dplyr)
source("data_Shiny.R", local = TRUE)


# Function to plot regression charts
charts_plot <- function(dataa, x, y){
  #print(dataa)
  #x <- as.data.frame(dataa[,2])
  #y <- as.data.frame(dataa[,3])
  #print(dim(x))
  #dataa %>% mutate(low = dataa * 0.75, high = dataa * 1.25)
  
  # plot <- ggplot(data = dataa, aes(x = x, y = y)) +
  #   geom_point() + scale_x_continuous(trans='log10') +
  #   scale_y_continuous(trans='log10') + 
  #   geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  #   xlab("Daphnia magna") +ylab("Danio Rerio")
  # 
  print(dataa)
  # Fit regression model
  fit <- lm(x ~ y , data = dataa)
  #print(fit$model)
  predictions <- data.frame(ID = rownames(dataa), Predicted = predict(fit))
  print(predictions)
  plot <- ggplot(fit$model, aes_string(x = names(fit$model[1]), y = names(fit$model)[2])) + 
    geom_point() +scale_x_continuous(trans='log10') +
    scale_y_continuous(trans='log10') + 
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 3),
                       "Intercept =",signif(fit$coef[[1]],3 ),
                       " Slope =",signif(fit$coef[[2]], 3),
                       " P =",signif(summary(fit)$coef[2,4], 3)), x = "Danio Rerio", y = "Daphnia Magna")
  
  
  
  return(plot)
}

# Read final data file.
#C:\Users\uulhassa\Desktop\URai\Final DATA\v2
#data <- openxlsx::read.xlsx(xlsxFile = "C:\\Users\\umadu\\Documents\\ecotoxicology-thesis\\Data\\LCEC_50_final.xlsx", 1)

regression <- function(data){
  
  crossed <- left_join(data, data, by = c("CAS", "Duration"))
  
  # Dropping extra columns 
  crossed <- crossed[, -c(9,11,12,13)]
  # Keeping CAS, Duration, 
  
  
  
  # Question for Sylvia: What to do with other values which dont have second species for comparison.
  # Use first CAS for example.
  
  #Keep different species
  crossed <- filter(crossed, Species.x == "Daphnia magna"  & Species.y == "Danio rerio")
  names(crossed)[2] <- "Smiles"
  names(crossed)[4] <- "Endpoint"
  names(crossed)[5] <- "Effect"
  names(crossed)[7] <- "Duration_Unit"
  
  # Generating smiles for required SMILEs
  mols_d2 <- parse.smiles(crossed$Smiles,omit.nulls = TRUE)
  fps_final2 <- lapply(mols_d2, get.fingerprint, type = "maccs", fp.mode = "bit")
  alls2 <- (attributes(fps_final2))$names
  smiles_list2 <- strsplit(alls2, " ")
  expanded_fps2 <- as.data.frame(fps_matrix(fps_final2, smiles_list2))
  
  names(expanded_fps2)[length(names(expanded_fps2))] <- "Smiles"
  
  ##############
  
  #Inner join removes null parsed smiles from resulted joined DF as
  #expanded_fps only has parsed smiles matrix values.
  
  joined2 <- inner_join(crossed, expanded_fps2, by = "Smiles", copy = FALSE)
  print(unique(joined2))
  
  
  # Writing output to EXCEL
  #openxlsx::write.xlsx(crossed, "all_data_regression.xlsx")
  
  # Set first species to Daphnia magna: Done in above filter
  #crossed_final <- filter(crossed, Species.x == "Daphnia magna")
  
  #Data for 24,48,72,96 days 
  hours24 <- crossed[(crossed$Duration >= 20 & crossed$Duration <= 26),]
  hours48 <- crossed[(crossed$Duration >= 44 & crossed$Duration <= 53),]
  hours72 <- crossed[(crossed$Duration >= 68 & crossed$Duration <= 72),]
  hours96 <- crossed[(crossed$Duration >= 90 & crossed$Duration <= 100),]
  
  
  # MEAN, MEDIAN and STANDARD DEVIATION 24
  mean_24 <- hours24 %>% group_by(CAS) %>% summarise_at(vars(mol_L.x, mol_L.y), mean)
  median_24 <- hours24 %>% group_by(CAS) %>% summarise_at(vars(mol_L.x, mol_L.y), median)
  #sd_24 <- hours24 %>% group_by(CAS) %>% summarise_at(vars(mol_L.x, mol_L.y), sd)
  
  #Chart for 24 hours
  plot_mean_24 <- charts_plot(mean_24, mean_24$mol_L.x, mean_24$mol_L.y)
  #(plot_mean_24)
  
  
  plot_median_24 <- charts_plot(median_24, median_24$mol_L.x, median_24$mol_L.y)
  #plot_median_24
  
  #SD chart as 0 sd value for Danio Rerio. the points which are at y = 0
  # sd_24 <- sd_24[!is.na(sd_24$mol_L.x),]
  # plot_sd_24 <- charts_plot(sd_24, sd_24$mol_L.x, sd_24$mol_L.y)
  # plot_sd_24
  
  
  # MEAN, MEDIAN and STANDARD DEVIATION 48
  mean_48 <- hours48 %>% group_by(CAS) %>% summarise_at(vars(mol_L.x, mol_L.y), mean)
  median_48 <- hours48 %>% group_by(CAS) %>% summarise_at(vars(mol_L.x, mol_L.y), median)
  #sd_48 <- hours48 %>% group_by(CAS) %>% summarise_at(vars(mol_L.x, mol_L.y), sd)
  
  
  #Chart for 48 hours
  plot_mean_48 <- charts_plot(mean_48, mean_48$mol_L.x, mean_48$mol_L.y)
  #plot_mean_48
  
  plot_median_48 <- charts_plot(median_48, median_48$mol_L.x, median_48$mol_L.y)
#  plot_median_48
  
  #SD chart as 0 sd value for Danio Rerio. the points which are at y = 0
  # sd_48 <- sd_48[!is.na(sd_48$mol_L.x),]
  # plot_sd_48 <- charts_plot(sd_48, sd_48$mol_L.x, sd_48$mol_L.y)
  # plot_sd_48
  
  
  # MEAN, MEDIAN and STANDARD DEVIATION 72
  mean_72 <- hours72 %>% group_by(CAS) %>% summarise_at(vars(mol_L.x, mol_L.y), mean)
  median_72 <- hours72 %>% group_by(CAS) %>% summarise_at(vars(mol_L.x, mol_L.y), median)
  #sd_72 <- hours72 %>% group_by(CAS) %>% summarise_at(vars(mol_L.x, mol_L.y), sd)
  
  
  #Chart for 48 hours
  plot_mean_72 <- charts_plot(mean_72, mean_72$mol_L.x, mean_72$mol_L.y)
 # plot_mean_72
  
  plot_median_72 <- charts_plot(median_72, median_72$mol_L.x, median_72$mol_L.y)
  #plot_median_72
  
  #SD chart as 0 sd value for Danio Rerio. the points which are at y = 0
  # sd_72 <- sd_72[!is.na(sd_72$mol_L.x),]
  # plot_sd_72 <- charts_plot(sd_72, sd_72$mol_L.x, sd_72$mol_L.y)
  # plot_sd_72
  
  
  # MEAN, MEDIAN and STANDARD DEVIATION 96
  mean_96 <- hours96 %>% group_by(CAS) %>% summarise_at(vars(mol_L.x, mol_L.y), mean)
  median_96 <- hours96 %>% group_by(CAS) %>% summarise_at(vars(mol_L.x, mol_L.y), median)
  #sd_96 <- hours96 %>% group_by(CAS) %>% summarise_at(vars(mol_L.x, mol_L.y), sd)
  
  #Chart for 48 hours
  plot_mean_96 <- charts_plot(mean_96, mean_96$mol_L.x, mean_96$mol_L.y)
  #plot_mean_96
  
  plot_median_96 <- charts_plot(median_96, median_96$mol_L.x, median_96$mol_L.y)
  #plot_median_96
  
  # Create list all plots to return
  #all_plots <- c(plot_mean_24,plot_mean_48, plot_mean_72, plot_mean_96, plot_median_24, plot_median_48,plot_median_72,plot_median_96)
  
  
  # Testing 
  #plot_crossed <- charts_plot(crossed, crossed$mol_L.x, crossed$mol_L.y)
   mean_24$Duration <- 24
   mean_48$Duration <- 48
   mean_72$Duration <- 72
   mean_96$Duration <- 96
   all_mean <- rbind(mean_24,mean_48,mean_72,mean_96)
   
   print(all_mean)
   
   all_mean_plot <- charts_plot(all_mean, all_mean$mol_L.x, all_mean$mol_L.y)
   # Writing output to EXCEL
   #openxlsx::write.xlsx(all_mean, "all_mean_reg.xlsx")
  # #plot_all <- charts_plot(all_mean, all_mean$mol_L.x, all_mean$mol_L.y)
  # 
  # 
  # # Fit regression model
  # fit <- lm(mean_48$mol_L.x ~ mean_48$mol_L.y, data = mean_48)
  # 
  # plot_all <- ggplot(fit$model, aes_string(x = names(fit$model[1]), y = names(fit$model)[2])) + 
  #   geom_point() +scale_x_continuous(trans='log10') +
  #   scale_y_continuous(trans='log10') + 
  #   stat_smooth(method = "lm", col = "red") +
  #   labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 3),
  #                      "Intercept =",signif(fit$coef[[1]],3 ),
  #                      " Slope =",signif(fit$coef[[2]], 3),
  #                      " P =",signif(summary(fit)$coef[2,4], 3)), x = "Danio Rerio", y = "Daphnia Magna")
  # 
  # 
  # plot_all
  #SD chart as 0 sd value for Danio Rerio. the points which are at y = 0
  # sd_96 <- sd_96[!is.na(sd_96$mol_L.x),]
  # plot_sd_96 <- charts_plot(sd_96, sd_96$mol_L.x, sd_96$mol_L.y)
  # plot_sd_96
  return (all_mean_plot)
}