library(randomForest)
library(ithir)
library(MASS)
library(caret)
library(ggplot2)
library(rasterVis)
library(lattice)
library(latticeExtra)

LR_HEAVY_METALS_MANGANESE <-read.csv(file.choose(),header =T, sep = ",") 
set.seed(2022)
str(LR_HEAVY_METALS_MANGANESE)
par(mfrow = c(1,1))
###Non-Transformed - Co
##hist(LR_HEAVY_METALS_rfe$Co,  xlab = "Co - Nontransformed")
###qqnorm(LR_HEAVY_METALS_rfe$Co, plot.it = TRUE, pch = 4, cex =  0.7, xlab = "Co - Nontransformed")
##qqline(LR_HEAVY_METALS_rfe$Co, col = "blue", lwd = 2)




########################

ctrl_lm <- trainControl(method= "LOOCV")


MANGANESE_LOOCV <- train(Mn ~ SAGA_MassBalanceIndex + SAGA_ProfileCurvature + SAGA_Valley_Depth +
                           CLNR_Mean + RONR_Mean, 
                    method ="lmStepAIC", 
                    data = LR_HEAVY_METALS_MANGANESE, 
                    trainControl=ctrl_lm)

MANGANESE_LOOCV
MANGANESE_LOOCV$finalModel
MANGANESE_LOOCV$results
summary(MANGANESE_LOOCV)

par(mfrow = c(2,1))
hist(MANGANESE_LOOCV$finalModel$residuals, col= "blue", xlab = "Mn_Residual", main="Histogram Plot")
qqnorm (MANGANESE_LOOCV$finalModel$residuals, plot.it = TRUE, pch = 4, cex =  0.7)
qqline(MANGANESE_LOOCV$finalModel$residuals, col = "blue", lwd = 2)

##normallik testi
shapiro.test(MANGANESE_LOOCV$finalModel$residuals)

varImp(MANGANESE_LOOCV$finalModel)
MANGANESE_LOOCV$finalModel$anova

##training-test-predicted
##COBALT_LOOCV_for_Cobalt_pred <- predict(COBALT_LOOCV, data = LR_HEAVY_METALS_COBALT)

##goof(observed = LR_HEAVY_METALS_COBALT$Co, predicted = COBALT_LOOCV_for_Cobalt_pred, plot.it = TRUE)

###Mapping
CNR_Mean <- raster("D:/Sudan_heavy_metals/LANDSAT_8_9_Indexes/INDEX_Means_for_extent/CNR_Mean.tif")
##SAGA_MRRRTF <- raster("D:/Sudan_heavy_metals/Topographic-variables/Topographic_variables_for_extent/SAGA_MRRTF.tif")
SAGA_ProfileCurvature <- raster("D:/Sudan_heavy_metals/Topographic-variables/Topographic_variables_for_extent/SAGA_ProfileCurvature.tif")
SAGA_ConvergenceIndex <- raster("D:/Sudan_heavy_metals/Topographic-variables/Topographic_variables_for_extent/SAGA_ConvergenceIndex.tif")
SAGA_Topographic_Position_Index <- raster("D:/Sudan_heavy_metals/Topographic-variables/Topographic_variables_for_extent/SAGA_Topographic_Position_Index.tif")
SAGA_Topographic_Wetness_Index <- raster("D:/Sudan_heavy_metals/Topographic-variables/Topographic_variables_for_extent/SAGA_Topographic_Wetness_Index.tif")
STANDART_Topographic_Wetness_Index <- raster("D:/Sudan_heavy_metals/Topographic-variables/Topographic_variables_for_extent/STANDART_Topographic_Wetness_Index.tif")


NDMI_Mean <- raster("D:/Sudan_heavy_metals/LANDSAT_8_9_Indexes/INDEX_Means_for_extent/NDMI_Mean.tif")
INR_Mean <- raster("D:/Sudan_heavy_metals/LANDSAT_8_9_Indexes/INDEX_Means_for_extent/INR_Mean.tif")
RONR_Mean <- raster("D:/Sudan_heavy_metals/LANDSAT_8_9_Indexes/INDEX_Means_for_extent/RONR_Mean.tif")


covs_Saudia_arabia <- stack(RONR_Mean)
compareRaster(STANDART_Topographic_Wetness_Index, RONR_Mean) ##extent karsilastirmak için kullanisli
proj4string(covs_Saudia_arabia) <- CRS("+init=epsg:32637")


Manganese_Stepwise_Mapping <- predict(covs_Saudia_arabia, MANGANESE_LOOCV$finalModel, "Manganese_Stepwise_Mapping.tif",
                                 format = "GTiff", datatype = "FLT4S", overwrite = TRUE)
par(mfrow = c(1,1))
plot(Manganese_Stepwise_Mapping,
     main = "Copper_0-30 cm")


