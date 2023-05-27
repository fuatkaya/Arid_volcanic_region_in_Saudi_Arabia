library(randomForest)
library(ithir)
library(MASS)
library(caret)
library(ggplot2)
library(rasterVis)
library(lattice)
library(latticeExtra)

LR_HEAVY_METALS_CHROMIUM <-read.csv(file.choose(),header =T, sep = ",") 
set.seed(2022)
str(LR_HEAVY_METALS_CHROMIUM)
par(mfrow = c(1,1))
###Non-Transformed - Co
##hist(LR_HEAVY_METALS_rfe$Co,  xlab = "Co - Nontransformed")
###qqnorm(LR_HEAVY_METALS_rfe$Co, plot.it = TRUE, pch = 4, cex =  0.7, xlab = "Co - Nontransformed")
##qqline(LR_HEAVY_METALS_rfe$Co, col = "blue", lwd = 2)




########################

ctrl_lm <- trainControl(method= "LOOCV")


CHROMIUM_LOOCV <- train(Cr ~ SAGA_ProfileCurvature + SAGA_ConvergenceIndex + SAGA_FlowAccumulation +
                          SAGA_MassBalanceIndex + SAGA_Slope_Percent +
                          SAGA_Topographic_Position_Index  + SAGA_Topographic_Wetness_Index + STANDART_Topographic_Wetness_Index + 
                        RONR_Mean + NDMI_Mean + FNR_Mean + INR_Mean, 
                      method ="lmStepAIC", 
                      data = LR_HEAVY_METALS_CHROMIUM, 
                      trainControl=ctrl_lm)


CHROMIUM_LOOCV
CHROMIUM_LOOCV$results
summary(CHROMIUM_LOOCV)

par(mfrow = c(2,1))
hist(CHROMIUM_LOOCV$finalModel$residuals, col= "blue", xlab = "Cr_Residual", main="Histogram Plot")
qqnorm (CHROMIUM_LOOCV$finalModel$residuals, plot.it = TRUE, pch = 4, cex =  0.7)
qqline(CHROMIUM_LOOCV$finalModel$residuals, col = "blue", lwd = 2)

##normallik testi
shapiro.test(CHROMIUM_LOOCV$finalModel$residuals)

varImp(CHROMIUM_LOOCV$finalModel)
CHROMIUM_LOOCV$finalModel$anova

##training-test-predicted
##COBALT_LOOCV_for_Cobalt_pred <- predict(COBALT_LOOCV, data = LR_HEAVY_METALS_COBALT)

##goof(observed = LR_HEAVY_METALS_COBALT$Co, predicted = COBALT_LOOCV_for_Cobalt_pred, plot.it = TRUE)

###Mapping
##CNR_Mean <- raster("D:/Sudan_heavy_metals/LANDSAT_8_9_Indexes/INDEX_Means_for_extent/CNR_Mean.tif")
##SAGA_MRRRTF <- raster("D:/Sudan_heavy_metals/Topographic-variables/Topographic_variables_for_extent/SAGA_MRRTF.tif")
SAGA_ProfileCurvature <- raster("D:/Sudan_heavy_metals/Topographic-variables/Topographic_variables_for_extent/SAGA_ProfileCurvature.tif")
SAGA_ConvergenceIndex <- raster("D:/Sudan_heavy_metals/Topographic-variables/Topographic_variables_for_extent/SAGA_ConvergenceIndex.tif")
SAGA_Topographic_Position_Index <- raster("D:/Sudan_heavy_metals/Topographic-variables/Topographic_variables_for_extent/SAGA_Topographic_Position_Index.tif")
SAGA_Topographic_Wetness_Index <- raster("D:/Sudan_heavy_metals/Topographic-variables/Topographic_variables_for_extent/SAGA_Topographic_Wetness_Index.tif")


NDMI_Mean <- raster("D:/Sudan_heavy_metals/LANDSAT_8_9_Indexes/INDEX_Means_for_extent/NDMI_Mean.tif")
INR_Mean <- raster("D:/Sudan_heavy_metals/LANDSAT_8_9_Indexes/INDEX_Means_for_extent/INR_Mean.tif")
RONR_Mean <- raster("D:/Sudan_heavy_metals/LANDSAT_8_9_Indexes/INDEX_Means_for_extent/RONR_Mean.tif")


covs_Saudia_arabia <- stack(SAGA_ProfileCurvature,SAGA_ConvergenceIndex, SAGA_Topographic_Position_Index, SAGA_Topographic_Wetness_Index, 
                            NDMI_Mean, INR_Mean, RONR_Mean)
compareRaster(SAGA_ProfileCurvature, INR_Mean) ##extent karsilastirmak için kullanisli
proj4string(covs_Saudia_arabia) <- CRS("+init=epsg:32637")


Chromium_Stepwise_Mapping <- predict(covs_Saudia_arabia, CHROMIUM_LOOCV$finalModel, "Chromium_Stepwise_Mapping.tif",
                                   format = "GTiff", datatype = "FLT4S", overwrite = TRUE)
par(mfrow = c(1,1))
plot(Chromium_Stepwise_Mapping,
     main = "Cobalt_0-30 cm")


