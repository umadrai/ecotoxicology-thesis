#### LCEC50 Data

data <- openxlsx::read.xlsx(xlsxFile = "C:\\Users\\umadu\\Documents\\ecotoxicology-thesis\\Data\\LCEC_50_final.xlsx", 1)
indices <- sample(1:nrow(hours24), 0.8*nrow(hours24))  # row indices for training data
training_Data <- hours24[indices,]  # model training data
#trainingIrisData <- trainingIrisData[, -c(2,4,5)]
test_Data <- hours24[-indices,]

fit_model <- lm(mol_L.x ~ mol_L.y, data = hours24)
prediction <- predict(fit_model, test_Data, interval = "confidence", level = 0.90)

summary(fit_model)


#using crossed data
indices <- sample(1:nrow(crossed), 0.8*nrow(crossed))  # row indices for training data
training_Data <- crossed[indices,]  # model training data
#trainingIrisData <- trainingIrisData[, -c(2,4,5)]
test_Data <- crossed[-indices,]

fit_model <- lm(mol_L.x ~ mol_L.y, data = crossed)
#prediction <- predict(fit_model, test_Data, interval = "confidence", level = 0.90)

actual_pred <- data.frame(cbind(test_Data, pred = prediction))

ggplot(data = actual_pred, aes(x = x, y = y)) +
  geom_point() + scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') + 
  xlab("Daphnia magna") +ylab("Danio Rerio")