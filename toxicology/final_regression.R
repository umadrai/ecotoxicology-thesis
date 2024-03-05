# Function to plot regression charts
charts_plot <- function(dataa, x, y){
  
  #x <- as.data.frame(dataa[,2])
  #y <- as.data.frame(dataa[,3])
  #print(dim(x))
  #dataa %>% mutate(low = dataa * 0.75, high = dataa * 1.25)
  
  plot <- ggplot(data = dataa, aes(x = x, y = y)) +
    geom_point() + scale_x_continuous(trans='log10') +
    scale_y_continuous(trans='log10') + 
    geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
    xlab("Daphnia magna") +ylab("Danio Rerio")
  
  return(plot)
}

# Read final data file.
#C:\Users\uulhassa\Desktop\URai\Final DATA\v2
#data <- openxlsx::read.xlsx(xlsxFile = "C:\\Users\\umadu\\Documents\\ecotoxicology-thesis\\Data\\LC_EC50_final.xlsx", 3)

regression <- function(data){
  
  crossed <- left_join(data, data, by = c("CAS", "Duration"))
  
  # Dropping extra columns 
  crossed <- crossed[, -c(9,11,12,13)]
  # Keeping CAS, Duration, 
  
  
  
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
  
  
  
  #SD chart as 0 sd value for Danio Rerio. the points which are at y = 0
  # sd_96 <- sd_96[!is.na(sd_96$mol_L.x),]
  # plot_sd_96 <- charts_plot(sd_96, sd_96$mol_L.x, sd_96$mol_L.y)
  # plot_sd_96
  return (plot_mean_24)
}