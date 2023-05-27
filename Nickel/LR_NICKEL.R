library(randomForest)
library(ithir)
library(MASS)
library(caret)
library(ggplot2)
library(rasterVis)
library(lattice)
library(latticeExtra)

LR_HEAVY_METALS_NICKEL <-read.csv(file.choose(),header =T, sep = ",") 
set.seed(2022)
str(LR_HEAVY_METALS_NICKEL)
par(mfrow = c(1,1))
###Non-Transformed - Co
##hist(LR_HEAVY_METALS_rfe$Co,  xlab = "Co - Nontransformed")
###qqnorm(LR_HEAVY_METALS_rfe$Co, plot.it = TRUE, pch = 4, cex =  0.7, xlab = "Co - Nontransformed")
##qqline(LR_HEAVY_METALS_rfe$Co, col = "blue", lwd = 2)




########################

ctrl_lm <- trainControl(method= "LOOCV")


NICKEL_LOOCV <- train(Ni ~ SAGA_Aspect + SAGA_MRRTF + SAGA_Valley_Depth + SAGA_PlanCurvature + 
                           INR_Mean, 
                         method ="lmStepAIC", 
                         data = LR_HEAVY_METALS_NICKEL, 
                         trainControl=ctrl_lm)

NICKEL_LOOCV
NICKEL_LOOCV$finalModel
NICKEL_LOOCV$results
summary(NICKEL_LOOCV)

par(mfrow = c(2,1))
hist(NICKEL_LOOCV$finalModel$residuals, col= "blue", xlab = "Ni_Residual", main="Histogram Plot")
qqnorm (NICKEL_LOOCV$finalModel$residuals, plot.it = TRUE, pch = 4, cex =  0.7)
qqline(NICKEL_LOOCV$finalModel$residuals, col = "blue", lwd = 2)

##normallik testi
shapiro.test(NICKEL_LOOCV$finalModel$residuals)

varImp(NICKEL_LOOCV$finalModel)
NICKEL_LOOCV$finalModel$anova

##training-test-predicted
##COBALT_LOOCV_for_Cobalt_pred <- predict(COBALT_LOOCV, data = LR_HEAVY_METALS_COBALT)

##goof(observed = LR_HEAVY_METALS_COBALT$Co, predicted = COBALT_LOOCV_for_Cobalt_pred, plot.it = TRUE)

###Mapping
CNR_Mean <- raster("D:/Sudan_heavy_metals/LANDSAT_8_9_Indexes/INDEX_Means_for_extent/CNR_Mean.tif")
SAGA_MRRRTF <- raster("D:/Sudan_heavy_metals/Topographic-variables/Topographic_variables_for_extent/SAGA_MRRTF.tif")
SAGA_ProfileCurvature <- raster("D:/Sudan_heavy_metals/Topographic-variables/Topographic_variables_for_extent/SAGA_ProfileCurvature.tif")
SAGA_ConvergenceIndex <- raster("D:/Sudan_heavy_metals/Topographic-variables/Topographic_variables_for_extent/SAGA_ConvergenceIndex.tif")
SAGA_Topographic_Position_Index <- raster("D:/Sudan_heavy_metals/Topographic-variables/Topographic_variables_for_extent/SAGA_Topographic_Position_Index.tif")
SAGA_Topographic_Wetness_Index <- raster("D:/Sudan_heavy_metals/Topographic-variables/Topographic_variables_for_extent/SAGA_Topographic_Wetness_Index.tif")
STANDART_Topographic_Wetness_Index <- raster("D:/Sudan_heavy_metals/Topographic-variables/Topographic_variables_for_extent/STANDART_Topographic_Wetness_Index.tif")


NDMI_Mean <- raster("D:/Sudan_heavy_metals/LANDSAT_8_9_Indexes/INDEX_Means_for_extent/NDMI_Mean.tif")
INR_Mean <- raster("D:/Sudan_heavy_metals/LANDSAT_8_9_Indexes/INDEX_Means_for_extent/INR_Mean.tif")
RONR_Mean <- raster("D:/Sudan_heavy_metals/LANDSAT_8_9_Indexes/INDEX_Means_for_extent/RONR_Mean.tif")


covs_Saudia_arabia <- stack(SAGA_MRRRTF, INR_Mean)
compareRaster(STANDART_Topographic_Wetness_Index, RONR_Mean) ##extent karsilastirmak için kullanisli
proj4string(covs_Saudia_arabia) <- CRS("+init=epsg:32637")


Nickel_Stepwise_Mapping <- predict(covs_Saudia_arabia, NICKEL_LOOCV$finalModel, "Nickel_Stepwise_Mapping.tif",
                                      format = "GTiff", datatype = "FLT4S", overwrite = TRUE)
par(mfrow = c(1,1))
plot(Nickel_Stepwise_Mapping,
     main = "Nickel_0-30 cm")


