library(randomForest)
library(ithir)
library(MASS)
library(caret)
library(ggplot2)
library(rasterVis)
library(lattice)
library(latticeExtra)

LR_HEAVY_METALS_COBALT <-read.csv(file.choose(),header =T, sep = ",") 
set.seed(2022)
str(LR_HEAVY_METALS_COBALT)
par(mfrow = c(1,1))
###Non-Transformed - Co
##hist(LR_HEAVY_METALS_rfe$Co,  xlab = "Co - Nontransformed")
###qqnorm(LR_HEAVY_METALS_rfe$Co, plot.it = TRUE, pch = 4, cex =  0.7, xlab = "Co - Nontransformed")
##qqline(LR_HEAVY_METALS_rfe$Co, col = "blue", lwd = 2)




########################
par(mfrow = c(1,1))
View(LR_HEAVY_METALS_COBALT)
LR_HEAVY_METALS_selected_covariates_for_Cobalt <- lm(Co  ~ 
                                                       CNR_Mean + 
                                                       SAGA_MRRTF,  data = LR_HEAVY_METALS_COBALT, y = TRUE, x = TRUE)
LR_HEAVY_METALS_selected_covariates_for_Cobalt$coefficients
summary(LR_HEAVY_METALS_selected_covariates_for_Cobalt)


##stepwise
step_LR_HEAVY_METALS_selected_covariates_for_Cobalt <- step(LR_HEAVY_METALS_selected_covariates_for_Cobalt, trace = 0, direction = "both")
step_LR_HEAVY_METALS_selected_covariates_for_Cobalt$coefficients
summary(step_LR_HEAVY_METALS_selected_covariates_for_Cobalt)
hist(step_LR_HEAVY_METALS_selected_covariates_for_Cobalt$residuals, col= "blue", xlab = "Co_Residual", main="Histogram Plot")
qqnorm (step_LR_HEAVY_METALS_selected_covariates_for_Cobalt$residuals, plot.it = TRUE, pch = 4, cex =  0.7)
qqline(step_LR_HEAVY_METALS_selected_covariates_for_Cobalt$residuals, col = "blue", lwd = 2)

##normallik testi
shapiro.test(step_LR_HEAVY_METALS_selected_covariates_for_Cobalt$residuals)



###AIC_Anova
step_LR_HEAVY_METALS_selected_covariates_for_Cobalt$anova

#importance
varImp(step_LR_HEAVY_METALS_selected_covariates_for_Cobalt)
library(PerformanceAnalytics)
library(MLmetrics)
##training-test-predicted
step_step_LR_HEAVY_METALS_selected_covariates_for_Cobalt_pred <- predict(step_LR_HEAVY_METALS_selected_covariates_for_Cobalt, data = LR_HEAVY_METALS_COBALT)

varImp(step_LR_HEAVY_METALS_selected_covariates_for_Cobalt)
goof(observed = LR_HEAVY_METALS_COBALT$Co, predicted = step_step_LR_HEAVY_METALS_selected_covariates_for_Cobalt_pred, plot.it = TRUE)

library(ldsr)
library(hydroGOF) 

#training
nrmse(step_step_LR_HEAVY_METALS_selected_covariates_for_Cobalt_pred, LR_HEAVY_METALS_COBALT$Co)

ctrl_lm <- trainControl(method= "LOOCV")


COBALT_LOOCV <- train(Co ~ SAGA_Profile.Curvature + 
                          SAGA_Valley.Depth + 
                          CNR_Mean + 
                          CLNR_Mean + 
                          SAGA_Aspect + 
                          SAGA_MRRTF , 
                        method ="lmStepAIC", 
                        data = LR_HEAVY_METALS_COBALT, 
                        trainControl=ctrl_lm)



COBALT_LOOCV
summary(COBALT_LOOCV)

##training-test-predicted
##COBALT_LOOCV_for_Cobalt_pred <- predict(COBALT_LOOCV, data = LR_HEAVY_METALS_COBALT)

##goof(observed = LR_HEAVY_METALS_COBALT$Co, predicted = COBALT_LOOCV_for_Cobalt_pred, plot.it = TRUE)

###Mapping
CNR_Mean <- raster("D:/Sudan_heavy_metals/LANDSAT_8_9_Indexes/INDEX_Means_for_extent/CNR_Mean.tif")
SAGA_MRRRTF <- raster("D:/Sudan_heavy_metals/Topographic-variables/Topographic_variables_for_extent/SAGA_MRRTF.tif")

covs_Saudia_arabia <- stack(CNR_Mean,SAGA_MRRRTF)
compareRaster(CNR_Mean, SAGA_MRRRTF) ##extent karsilastirmak için kullanisli
proj4string(covs_Saudia_arabia) <- CRS("+init=epsg:32637")


Cobalt_Stepwise_Mapping <- predict(covs_Saudia_arabia, COBALT_LOOCV$finalModel, "Cobalt_Stepwise_Mapping.tif",
                             format = "GTiff", datatype = "FLT4S", overwrite = TRUE)
par(mfrow = c(1,1))
plot(Cobalt_Stepwise_Mapping,
     main = "Cobalt_0-30 cm")

par(mfrow = c(2,1))
hist(COBALT_LOOCV$finalModel$residuals, col= "blue", xlab = "Co_Residual", main="Histogram Plot")
qqnorm (COBALT_LOOCV$finalModel$residuals, plot.it = TRUE, pch = 4, cex =  0.7)
qqline(COBALT_LOOCV$finalModel$residuals, col = "blue", lwd = 2)

##normallik testi
shapiro.test(step_LR_HEAVY_METALS_selected_covariates_for_Cobalt$residuals)

varImp(COBALT_LOOCV$finalModel)
COBALT_LOOCV$finalModel$anova
