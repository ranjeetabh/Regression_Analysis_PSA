## Author: Ranjeeta Bhattacharya
## Project Description: Analyze and develop a model for PSA to be used for inferential purposes.
## Data Description: A university medical center urology group was interested in the association
## between prostate specific antigen (PSA) and a number of prognostic clinical measurements in
## men with advanced prostate cancer. Data were collected on 97 men who were about to undergo 
## radical prostectomies. Each line of data set provides information on 8 other variables for each person.  

######################################################################################################

## Importing necessary libraries

library(corrplot)
library(ggplot2)
library(lattice)


## Reading input file

prostate = read.table("prostate_data.txt", header = TRUE)

## Exploring data ##

dim(prostate)
names(prostate)
prostate
summary(prostate)

attach(prostate)

# Preview prostate data
head(prostate,3)

## Pairplot of all variables
pairs(prostate)

## Plotting correlation coefficients between different variables
## Good positive correlation exhibited between following pairs:
## cavol-cp, cavol - svi, cavol - psa, svi - cp, svi - psa, cp - psa 

cor_prostate <- cor(prostate)
corrplot(cor_prostate, method = "circle")
corrplot(cor_prostate, method = "number") # Display the correlation coefficient

## Checking for multicollinearity between explanatory variables
## No apparent presence of multicollinearity

library(car)
vif(lm(psa ~ cavol + weight + age + bph + svi + cp + gleason))

## Plotting each explanatory variable individually with response variable to analyze relationship 

par(mfrow = c(1, 2))
plot(cavol, psa, xlab = "Cancer Volume", ylab = "PSA Level", pch = 19, cex = 0.5)
plot(weight, psa, xlab = "Weight", ylab = "PSA Level", pch = 19, cex = 0.5)
plot(age, psa, xlab = "Age", ylab = "PSA Level", pch = 19, cex = 0.5)
plot(bph, psa, xlab = "Benign Prostatic Hyperplasia ", ylab = "PSA Level", pch = 19, cex = 0.5)
plot(svi, psa, xlab = "Seminal Vesicle Invasion", ylab = "PSA Level", pch = 19, cex = 0.5)
plot(cp, psa, xlab = "Capsular Penetration", ylab = "PSA Level", pch = 19, cex = 0.5)
plot(gleason, psa, xlab = "Gleason Score", ylab = "PSA Level", pch = 19, cex = 0.5)


## Plotting histogram for explanatoty variables

ggplot(data = prostate, aes(cavol)) + geom_histogram()
ggplot(data = prostate, aes(weight)) + geom_histogram()
ggplot(data = prostate, aes(age)) + geom_histogram()
ggplot(data = prostate, aes(bph)) + geom_histogram()
ggplot(data = prostate, aes(svi)) + geom_histogram()
ggplot(data = prostate, aes(cp)) + geom_histogram()
ggplot(data = prostate, aes(gleason)) + geom_histogram()

## Generating simple linear models for assessment based on graphs generated before

model_cavol = lm(psa ~ cavol)
summ_cavol = summary(model_cavol)
summ_cavol

model_weight = lm(psa ~ weight)
summ_weight = summary(model_weight)
summ_weight

model_age = lm(psa ~ age)
summ_age = summary(model_age)
summ_age

model_bph = lm(psa ~ bph)
summ_bph = summary(model_bph)
summ_bph

model_svi = lm(psa ~ svi)
summ_svi = summary(model_svi)
summ_svi

model_cp = lm(psa ~ cp)
summ_cp = summary(model_cp)
summ_cp

model_gleason = lm(psa ~ gleason)
summ_gleason = summary(model_gleason)
summ_gleason


## Printing model summaries to analyze 
## p-value is significant for cavol, svi, cp, gleason
## Adjusted R-Squared value is best for cavol

summ_cavol
summ_weight
summ_age
summ_bph
summ_svi
summ_cp
summ_gleason

## Residual plots for cavol, weight, cpi
## Plots indicates presence of Heteroscedasticity 

par(mfrow = c(1, 2))
#plot(cavol, psa, xlab = "Cancer Volume", ylab = "PSA Level", pch = 19, cex = 0.5)
plot(cavol, model_cavol$residuals, xlab = "Cancer Volume", ylab = "Residual Value", main = "Cavol vs Residual Value of Cavol")

#plot(weight, psa, xlab = "Weight", ylab = "PSA Level", pch = 19, cex = 0.5)
plot(weight, model_weight$residuals, xlab = "Weight", ylab = "Residual Value", main = "Weight vs Residual Value of Weight")

#plot(cp, psa, xlab = "Capsular Penetration", ylab = "PSA Level", pch = 19, cex = 0.5)
plot(cp, model_cp$residuals, xlab = "Capsular Penetration", ylab = "Residual Value", main = "CP vs Residual Value of CP")

## Trying different transformations to address Heteroscedasticity for cavol, weight, cpi

par(mfrow = c(2, 4))
plot(sqrt(cavol), log(psa), xlab = "sqrt(cavol)", ylab = "log(psa)", main = "(1)", pch = 19, cex = 0.5)
plot(log(cavol), sqrt(psa), xlab = "log(cavol)", ylab = "sqrt(psa)", main = "(2)", pch = 19, cex = 0.5)
plot(log(cavol), log(psa), xlab = "log(cavol)", ylab = "log(psa)", main = "(3)", pch = 19, cex = 0.5)
plot(sqrt(cavol), sqrt(psa), xlab = "sqrt(cavol)", ylab = "sqrt(psa)", main = "(4)", pch = 19, cex = 0.5)

plot(sqrt(weight), log(psa), xlab = "sqrt(weight)", ylab = "log(psa)", main = "(1)", pch = 19, cex = 0.5)
plot(log(weight), sqrt(psa), xlab = "log(weight)", ylab = "sqrt(psa)", main = "(2)", pch = 19, cex = 0.5)
plot(log(weight), log(psa), xlab = "log(weight)", ylab = "log(psa)", main = "(3)", pch = 19, cex = 0.5)
plot(sqrt(weight), sqrt(psa), xlab = "sqrt(weight)", ylab = "sqrt(psa)", main = "(4)", pch = 19, cex = 0.5)

plot(sqrt(cp), log(psa), xlab = "sqrt(cp)", ylab = "log(psa)", main = "(1)", pch = 19, cex = 0.5)
plot(log(cp), sqrt(psa), xlab = "log(cp)", ylab = "sqrt(psa)", main = "(2)", pch = 19, cex = 0.5)
plot(log(cp), log(psa), xlab = "log(cp)", ylab = "log(psa)", main = "(3)", pch = 19, cex = 0.5)
plot(sqrt(cp), sqrt(psa), xlab = "sqrt(cp)", ylab = "sqrt(psa)", main = "(4)", pch = 19, cex = 0.5)


## Trying polynomial transformation for variables age and bph and cp
## Both these variables didn't have significant p-value

#model_age = lm(psa ~ age)
#summ_age = summary(model_age)
summ_age  = summary(model_age) ## Adjusted R-squared:  -0.01025, p-value: 0.8725
summ_age

summ_age_1 = summary(lm(log(psa)~age))
summ_age_1  ## Adjusted R-squared:  0.01854, p-value: 0.09677

summ_age_2 = summary(lm(log(psa)~age + I(age^2)))
summ_age_2  ## Adjusted R-squared:  0.01941 , p-value: 0.148

summ_age_3 = summary(lm(log(psa)~age + I(age^2) + I(age^3)))
summ_age_3  ## Adjusted R-squared:  0.0144 , p-value: 0.2285

summ_age_4 = summary(lm(log(psa)~age + I(age^2) + I(age^3) + I(age^4)))
summ_age_4  ## Adjusted R-squared:  0.003854 , p-value: 0.3648


summ_bph_1 = summary(lm(log(psa)~bph))
summ_bph_1  ## Adjusted R-squared:  0.01345, p-value: 0.132

summ_bph_2 = summary(lm(log(psa)~bph + I(bph^2)))
summ_bph_2  ## Adjusted R-squared:  0.004058 , p-value: 0.3071

summ_bph_3 = summary(lm(log(psa)~bph + I(bph^2) + I(bph^3)))
summ_bph_3  ## Adjusted R-squared:  0.008516 , p-value: 0.2876

summ_bph_4 = summary(lm(log(psa)~bph + I(bph^2) + I(bph^3) + I(bph^4)))
summ_bph_4  ## Adjusted R-squared:  0.001603 , p-value: 0.3917


summ_cp_1 = summary(lm(log(psa)~cp))
summ_cp_1  ## Adjusted R-squared:  0.2559, p-value: 7.536e-08

summ_cp_2 = summary(lm(log(psa)~cp + I(cp^2)))
summ_cp_2  ## Adjusted R-squared:  0.2561 , p-value: 3.405e-07

summ_cp_3 = summary(lm(log(psa)~cp + I(cp^2) + I(cp^3)))
summ_cp_3  ## Adjusted R-squared:  0.2804 , p-value: 2.26e-07

summ_cp_4 = summary(lm(log(psa)~cp + I(cp^2) + I(cp^3) + I(cp^4)))
summ_cp_4  ## Adjusted R-squared:  0.2762 , p-value: 7.421e-07


xx = 0:865/10

y2 = summ_age_2$coe[1,1] + summ_age_2$coe[2,1]*xx + summ_age_2$coe[3,1]*xx^2
y3 = summ_age_3$coe[1,1] + summ_age_3$coe[2,1]*xx + summ_age_3$coe[3,1]*xx^2 + summ_age_3$coe[4,1]*xx^3
y4 = summ_age_4$coe[1,1] + summ_age_4$coe[2,1]*xx + summ_age_4$coe[3,1]*xx^2 + summ_age_4$coe[4,1]*xx^3 + summ_age_4$coe[5,1]*xx^4

plot(age, psa, xlab = "Age", ylab = "PSA", pch = 19, cex = 0.5)

points(xx, y2, type = "l", col = 3)
points(xx, y3, type = "l", col = 2)
points(xx, y4, type = "l", col = 4)

legend("topleft",lty=c(1, 1, 1), lwd=c(0.5, 0.5, 0.5), col = c(3, 2, 4), legend=c("Quadratic", "Cubic", "Quartic"), cex=1)


xx = 0:865/10

y2 = summ_bph_2$coe[1,1] + summ_bph_2$coe[2,1]*xx + summ_bph_2$coe[3,1]*xx^2
y3 = summ_bph_3$coe[1,1] + summ_bph_3$coe[2,1]*xx + summ_bph_3$coe[3,1]*xx^2 + summ_bph_3$coe[4,1]*xx^3
y4 = summ_bph_4$coe[1,1] + summ_bph_4$coe[2,1]*xx + summ_bph_4$coe[3,1]*xx^2 + summ_bph_4$coe[4,1]*xx^3 + summ_bph_4$coe[5,1]*xx^4

plot(bph, psa, xlab = "bph", ylab = "PSA", pch = 19, cex = 0.5)

points(xx, y2, type = "l", col = 3)
points(xx, y3, type = "l", col = 2)
points(xx, y4, type = "l", col = 4)

legend("topleft",lty=c(1, 1, 1), lwd=c(0.5, 0.5, 0.5), col = c(3, 2, 4), legend=c("Quadratic", "Cubic", "Quartic"), cex=1)



xx = 0:865/10

y2 = summ_cp_2$coe[1,1] + summ_cp_2$coe[2,1]*xx + summ_cp_2$coe[3,1]*xx^2
y3 = summ_cp_3$coe[1,1] + summ_cp_3$coe[2,1]*xx + summ_cp_3$coe[3,1]*xx^2 + summ_cp_3$coe[4,1]*xx^3
y4 = summ_cp_4$coe[1,1] + summ_cp_4$coe[2,1]*xx + summ_cp_4$coe[3,1]*xx^2 + summ_cp_4$coe[4,1]*xx^3 + summ_cp_4$coe[5,1]*xx^4

plot(cp, psa, xlab = "cp", ylab = "PSA", pch = 19, cex = 0.5)

points(xx, y2, type = "l", col = 3)
points(xx, y3, type = "l", col = 2)
points(xx, y4, type = "l", col = 4)

legend("topleft",lty=c(1, 1, 1), lwd=c(0.5, 0.5, 0.5), col = c(3, 2, 4), legend=c("Quadratic", "Cubic", "Quartic"), cex=1)


boxplot(psa~svi,data=prostate, xlab="svi", ylab="psa")
boxplot(psa~gleason,data=prostate, xlab="gleason", ylab="psa")


## Building initial multiple regression model with all parameters

model_aggregated = lm(psa ~ cavol + weight + age + bph + svi + cp + gleason)
summ_aggregated = summary(model_aggregated)
summ_aggregated


## Refining model by keeping only those parameters for which initial individual plots 
## showed some kind of linear relationship with explanatory variable

model_refined = lm(psa ~ cavol + weight + svi + cp + gleason)
summ_refined = summary(model_refined)
summ_refined


## We can see that the adjusted R-squared value improved slightly after removing some parameters
## Adjusted R-Squared value indicates that the model below explains around 42.2% variability of 
## explanatory variable

model = lm(psa ~ cavol + svi + cp + gleason)
refined = summary(model)
refined


## Modelling using tranformation of variables

model_transform = lm(log(psa) ~ log(cavol) + log(weight) + svi + cp + I(cp^2) + I(cp^3) + gleason)

refined_transform = summary(model_transform)
refined_transform

## Best combination of parameters after refining list of explanatory variables

refined_transform
refined_transform$adj.r.squared

########################################################################################

## Plotting residual and qq plot for the tranformed model
## Heteroskedasticity is corrected and qq plot also looks good

par(mfrow = c(1, 2))
plot(model_transform$fit, model_transform$res, xlab = "Fitted Values", ylab = "Residuals", pch = 19, cex = 0.5)
qqnorm(model_transform$res, pch = 19, cex = 0.5)
qqline(model_transform$res)

## Plotting residuals for individual variable in the transformed model

par(mfrow = c(2, 3))
plot(log(cavol), model_transform$residuals, xlab = "log(cavol)", ylab = "Residuals", pch = 19, cex = 0.5)
plot(log(weight), model_transform$residuals, xlab = "log(weight)", ylab = "Residuals", pch = 19, cex = 0.5)
plot(cp, model_transform$residuals, xlab = "cp", ylab = "Residuals", pch = 19, cex = 0.5)
plot(svi, model_transform$residuals, xlab = "svi", ylab = "Residuals", pch = 19, cex = 0.5)
plot(gleason, model_transform$residuals, xlab = "gleason", ylab = "Residuals", pch = 19, cex = 0.5)


####################################################################

## Outlier detection
## Plotting variables included in building final model 

## First the leverage ##
## It is useful for investigating whether one or more observations are outlying with regard to their X values, 
## and therefore might be excessively influencing the regression results.

par(mfrow = c(1, 2))
lev = hatvalues(model_transform)
plot(lev, ylab = "Leverage Value", pch = 19, cex = 0.5)
identify(lev)


## Since there are now several explanatory variables it makes the most sense to plot the leverages versus ##
## their record number (i.e. index). ##
## Now let's at the change in coefficients ##

dif_betas = dfbeta(model_transform)


## First the intercept $$

plot(dif_betas[,1], ylab = "Change in Intercept Value", pch = 19, cex = 0.5)
identify(dif_betas[,1])


## Next the slopes ##
par(mfrow = c(1, 2))
plot(dif_betas[,2], ylab = "Change in Cavol Slope Value", pch = 19, cex = 0.5)
identify(dif_betas[,2])

plot(dif_betas[,3], ylab = "Change in Weight Slope Value", pch = 19, cex = 0.5)
identify(dif_betas[,3])

plot(dif_betas[,4], ylab = "Change in cp Slope Value", pch = 19, cex = 0.5)
identify(dif_betas[,4])

plot(dif_betas[,5], ylab = "Change in svi Slope Value", pch = 19, cex = 0.5)
identify(dif_betas[,5])

plot(dif_betas[,5], ylab = "Change in gleason Slope Value", pch = 19, cex = 0.5)
identify(dif_betas[,5])


## Now let's look at the change in the fitted values ##
dif_fits = dffits(model)

plot(dif_fits, ylab = "Change in Fitted Value", pch = 19, cex = 0.5)
identify(dif_fits)


## Next, look at Cook's Distances ##
cooks = cooks.distance(model)

plot(cooks, ylab = "Cook's Distances", pch = 19, cex = 0.5)
identify(cooks)

## Lastly, use the influence function to get the estimates of the residual standard error after deleting the ith ## ## observations ##

infl = influence(model)
plot(infl$sigma, ylab = "Residual Standard Error Estimate", pch = 19, cex = 0.5)
identify(infl$sigma)


## It seems the observations 32, 47, 95, 96, 97  are repeated in the plots
## Lets analyze these observations futher

prostate[32,]   ## Weight is 449.25. Invalid data.
prostate[47,]   ## Nothing abnormal
prostate[55,]   ## PSA is high because of high cavol. Nothing abnormal
prostate[95,]   ## Nothing Abnormal. Valid data.
prostate[96,]   ## Nothing Abnormal. Valid data.
prostate[97,]   ## Nothing Abnormal. Valid data.

## Observation 32 was removed and model was fitted again 

model_transform_outlier = lm(log(psa[-32]) ~ log(cavol[-32]) + log(weight[-32]) + svi[-32] + cp[-32] + I(cp[-32]^2) + I(cp[-32]^3) + gleason[-32])

refined_transform_outlier = summary(model_transform_outlier)
refined_transform_outlier

###############################################################################

## Using AIC and BIC for doing comparative analysis of all models generated
## Larger difference in either AIC or BIC indicates stronger evidence for one 
## model over the other (the lower the better)

AIC(model_aggregated)   ## Score 953.0024
AIC(model_refined)      ## Score 950.4859

## Model refined_transform, score 218.7478
AIC(lm(log(psa) ~ log(cavol) + log(weight) + svi + cp + I(cp^2) + I(cp^3) + gleason))

## Model refined_transform_outlier, score 215.2477
AIC(lm(log(psa[-32]) ~ log(cavol[-32]) + log(weight[-32]) + svi[-32] + cp[-32] + I(cp[-32]^2) + I(cp[-32]^3) + gleason[-32]))


BIC(model_aggregated)   ## Score 976.1748
BIC(model_refined)      ## Score 968.5089

## Model refined_transform, score 241.9202
BIC(lm(log(psa) ~ log(cavol) + log(weight) + svi + cp + I(cp^2) + I(cp^3) + gleason))

## Model refined_transform_outlier, score 238.3268
BIC(lm(log(psa[-32]) ~ log(cavol[-32]) + log(weight[-32]) + svi[-32] + cp[-32] + I(cp[-32]^2) + I(cp[-32]^3) + gleason[-32]))


## Correlation coefficient between fitted value of psa vs response psa
## Strong correlation 0.8102334
cor(log(psa[-32]), model_transform_outlier$fitted.values)

## Residual and QQ Plots for final transformed model after removing outlier
## Residual plots look consistent with Homoscedasticity assumption and QQ plot also looks normal.

par(mfrow = c(1, 2))
plot(model_transform_outlier$fit, model_transform_outlier$res, xlab = "Fitted Values", ylab = "Residuals", pch = 19, cex = 0.5)
qqnorm(model_transform_outlier$res, pch = 19, cex = 0.5)
qqline(model_transform_outlier$res)

## Plotting residuals for individual variable in the transformed model

par(mfrow = c(2, 3))
plot(log(cavol[-32]), model_transform_outlier$residuals, xlab = "log(cavol)", ylab = "Residuals", pch = 19, cex = 0.5)
plot(log(weight[-32]), model_transform_outlier$residuals, xlab = "log(weight)", ylab = "Residuals", pch = 19, cex = 0.5)
plot(cp[-32], model_transform_outlier$residuals, xlab = "cp", ylab = "Residuals", pch = 19, cex = 0.5)
plot(svi[-32], model_transform_outlier$residuals, xlab = "svi", ylab = "Residuals", pch = 19, cex = 0.5)
plot(gleason[-32], model_transform_outlier$residuals, xlab = "gleason", ylab = "Residuals", pch = 19, cex = 0.5)


###########################################################################################################################




