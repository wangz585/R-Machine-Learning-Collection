# package install
install.packages('ggplot2')
install.packages('tidyverse')
install.packages('forcats')
install.packages("magrittr") 
install.packages("dplyr")
install.packages("MASS")
install.packages("car")
install.packages("pairsD3")

# package library
library(ggplot2)
library(tidyverse)
library(forcats)
library(dplyr) 
library("magrittr")
library(MASS)
library(car)
library(pairsD3)

# Import data
data = read.csv('FP_dataset.csv')

# Deal with categorical variables more than 2 categories
data$CONTROL_2 = ifelse(data$CONTROL == 2, 1, 0)
data$CONTROL_3 = ifelse(data$CONTROL == 3, 1, 0)
data$REGION_2 = ifelse(data$REGION == 2, 1, 0)
data$REGION_3 = ifelse(data$REGION == 3, 1, 0)
data$REGION_4 = ifelse(data$REGION == 4, 1, 0)
data$REGION_5 = ifelse(data$REGION == 5, 1, 0)
data$REGION_6 = ifelse(data$REGION == 6, 1, 0)
data$REGION_7 = ifelse(data$REGION == 7, 1, 0)
data$REGION_8 = ifelse(data$REGION == 8, 1, 0)
data$REGION_9 = ifelse(data$REGION == 9, 1, 0)

# Add transformation to the dataset, I know about these transformations later
data$logAVGFACSAL = log(data$AVGFACSAL)
data$logPCT_BORN_US = log(data$PCT_BORN_US)
data$logPCT_WHITE = log(data$PCT_WHITE)
data$sqrtNUMBRANCH = sqrt(data$NUMBRANCH)
data$poweredPCT_WHITE = (data$PCT_WHITE)^4
data$poweredPCT_BORN_US = (data$PCT_BORN_US)^8.77
data$sqrtCOSTT4_A = (data$COSTT4_A)^0.5
data$sqrtPAR_ED_PCT_1STGEN = (data$PAR_ED_PCT_1STGEN)^0.5

# Store variables in a vector
all.variables <- c('NUMBRANCH', 'COSTT4_A', 'AVGFACSAL', 'PFTFAC', 'PCTPELL', 'UG25ABV', 'INC_PCT_LO', 'PAR_ED_PCT_1STGEN', 'FEMALE','MD_FAMINC', 'PCT_WHITE', 'PCT_BLACK', 'PCT_ASIAN', 'PCT_HISPANIC', 'PCT_BA','PCT_GRAD_PROF', 'PCT_BORN_US', 'POVERTY_RATE', 'UNEMP_RATE', 'HBCU', 'PBI', 'TRIBAL', 'HSI', 'WOMENONLY','REGION_2', 'REGION_3', 'REGION_4', 'REGION_5', 'REGION_6', 'REGION_7', 'REGION_8', 'REGION_9', 'CONTROL_2', 'CONTROL_3')
numeric.variables <- c('NUMBRANCH', 'COSTT4_A', 'AVGFACSAL', 'PFTFAC', 'PCTPELL', 'UG25ABV', 'INC_PCT_LO', 'PAR_ED_PCT_1STGEN', 'FEMALE','MD_FAMINC', 'PCT_WHITE', 'PCT_BLACK', 'PCT_ASIAN', 'PCT_HISPANIC', 'PCT_BA','PCT_GRAD_PROF', 'PCT_BORN_US', 'POVERTY_RATE', 'UNEMP_RATE')

# Data Description (Summary)
stats.summary = do.call(cbind, lapply(data[,all.variables], median ))

num_predictor = length(all.variables)
num_observation = count(data)[1,1]

# Split Data into Training and Testing in R 
sample_size = floor(0.5*nrow(data))
set.seed(58)

# randomly split data in r
picked = sample(seq_len(nrow(data)),size = sample_size)
training.data = data[picked,]
testing.data =data[-picked,]

# Plot Histogram for each variable
par(mfrow=c(4,5))
for (val in all.variables)
{
  hist(data[, val], xlab=val, main = val)
}

# Plot BoxPlot for each variable
for (val in all.variables)
{
  boxplot(data[, val], xlab=val, main = val)
}

# Check first basic condition, this allows the audience to interact with 
shinypairs(training.data[,all.variables])

# Using Control2 as a factor variable to demonstatrate its importance
# Same, but with different colors and add regression lines
ggplot(data, aes(x=data$AVGFACSAL, y=data$COSTT4_A, color=as.factor(CONTROL_2))) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE)    # Don't add shaded confidence region

# Check the second condition 
formula.all.variables <- as.formula(paste("ADM_RATE ~ ", paste(all.variables, collapse= "+")))
model.all.variables <- lm(formula.all.variables, data=training.data)
plot((training.data$ADM_RATE) ~ fitted(model.all.variables), xlab="Fitted Values", ylab="Admited Rate")
abline(a = 0, b = 1, lty = 3)
lines(lowess(model.all.variables$fitted.values, training.data$ADM_RATE))
legend("topleft", legend=c("Identity", "Smooth Fit"), lty=c(2, 1))

# For the sake of transformation, add 0.5 for each column with zero in it
zero.variable <- c('ADM_RATE')
for (col in all.variables)
{
  if (any(training.data[, col] == 0)){
    training.data[, col] = training.data[, col] + 0.5
    zero.variable <- c(zero.variable, col)
  }
}
training.data$ADM_RATE = training.data$ADM_RATE + 0.5

# Apply box-cox transformation to check if condition 2 gets better
bc <- powerTransform(lm(cbind(training.data$ADM_RATE, training.data$NUMBRANCH, training.data$COSTT4_A, training.data$AVGFACSAL, training.data$PFTFAC, training.data$UG25ABV, training.data$PCTPELL, training.data$INC_PCT_LO, training.data$PAR_ED_PCT_1STGEN, training.data$FEMALE, training.data$MD_FAMINC, training.data$poweredPCT_WHITE, training.data$PCT_BLACK, training.data$PCT_BA, training.data$PCT_GRAD_PROF, training.data$PCT_BORN_US, training.data$POVERTY_RATE, training.data$UNEMP_RATE)~1))

# Store variables in a vector
all.variables <- c('sqrtNUMBRANCH', 'COSTT4_A', 'AVGFACSAL', 'PFTFAC', 'PCTPELL', 'UG25ABV', 'INC_PCT_LO', 'sqrtPAR_ED_PCT_1STGEN', 'FEMALE','MD_FAMINC', 'poweredPCT_WHITE', 'PCT_BLACK', 'PCT_ASIAN', 'PCT_HISPANIC', 'PCT_BA','PCT_GRAD_PROF', 'PCT_BORN_US', 'POVERTY_RATE', 'UNEMP_RATE', 'HBCU', 'PBI', 'TRIBAL', 'HSI', 'WOMENONLY','REGION_2', 'REGION_3', 'REGION_4', 'REGION_5', 'REGION_6', 'REGION_7', 'REGION_8', 'REGION_9', 'CONTROL_2', 'CONTROL_3')

# Check the second condition gets better with this transformation, not good, so, we give up on this approach
formula.all.variables <- as.formula(paste("ADM_RATE ~ ", paste(all.variables, collapse= "+")))
model.all.variables <- lm(formula.all.variables, data=data)
plot((data$ADM_RATE) ~ fitted(model.all.variables), xlab="Fitted Values", ylab="Admited Rate")
abline(a = 0, b = 1, lty = 3)
lines(lowess(model.all.variables$fitted.values, data$ADM_RATE))
legend("topleft", legend=c("Identity", "Smooth Fit"), lty=c(2, 1))

# Minus 0.5 for each column in zero.variables
for (col in all.variables)
{
  if (any(training.data[, col] == 0)){
    training.data[, col] = training.data[, col] - 0.5
    zero.variable <- c(zero.variable, col)
  }
}

# Verify Four Basic Conditions
plot(model.all.variables)

# Verify Assumptions for all predictors
for (val in all.variables)
{
  plot(sqrt(abs(rstandard(model.all.variables))) ~ training.data[, val], xlab=val, ylab="Residuals")
  abline(lm(sqrt(abs(rstandard(model.all.variables))) ~ training.data[, val]),col="red") 
}

# Check multicolinearity
vif(model.all.variables)

# Modify multicolinearity
multicolinearitycheck.variables <- c('sqrtNUMBRANCH', 'COSTT4_A', 'AVGFACSAL', 'PFTFAC',  'UG25ABV', 'sqrtPAR_ED_PCT_1STGEN', 'FEMALE', 'poweredPCT_WHITE', 'PCT_BORN_US','POVERTY_RATE', 'MD_FAMINC', 'HBCU', 'PBI', 'TRIBAL', 'HSI', 'WOMENONLY','REGION_2', 'REGION_3', 'REGION_4', 'REGION_5', 'REGION_6', 'REGION_7', 'REGION_8', 'CONTROL_2', 'CONTROL_3')
model.multicolinearitycheck.formula <- as.formula(paste("ADM_RATE ~ ", paste(multicolinearitycheck.variables, collapse= "+")))
model.multicolinearitycheck.variables <- lm(model.multicolinearitycheck.formula, data = training.data)
vif(model.multicolinearitycheck.variables)

# Test if dropping variables because of multicolinearity
# Cannot drop UNEMP_RATE or POVERTY_RATE, partial-f test fails
anova(model.all.variables,model.multicolinearitycheck.variables)

# Check Leverage Points: cooks
num_predictor = length(all.variables)
num_observation = count(training.data)[1,1]
D <- cooks.distance(model.multicolinearitycheck.variables)
cutoff <- qf(0.5, num_predictor, num_observation-num_predictor-1, lower.tail=T)
cooks.outlier <- which(D > cutoff)

# Check Leverage Points: dffits
fits <- dffits(model.all.variables)
cutoff <- 2*sqrt(num_predictor/num_observation)
dffits.outlier <- which(abs(fits) > cutoff)

# Use Automated Variable stepwise Selection (AIC and BIC)
variables.automated <- c(multicolinearitycheck.variables,'COSTT4_A:CONTROL_2','poweredPCT_WHITE:CONTROL_2')
formula.automated.variables <- as.formula(paste("ADM_RATE ~ ", paste(variables.automated, collapse= "+")))
model.automated <- lm(formula.automated.variables, data=training.data)
result.AIC <- stepAIC(model.automated,direction = "both", k = 2)
result.BIC <- stepAIC(model.automated,direction = "both", k = log(num_observation))

# Summarize AIC, BIC selection results
summary(result.AIC)
summary(result.BIC) 

# Use Partial-F test to check if dropping interation term is approraite
BIC.withinteractionterm <- lm(formula = ADM_RATE ~ sqrtNUMBRANCH + AVGFACSAL + sqrtPAR_ED_PCT_1STGEN + FEMALE + poweredPCT_WHITE + MD_FAMINC + CONTROL_2 +  poweredPCT_WHITE:CONTROL_2, data = training.data)
anova(result.BIC, BIC.withinteractionterm)

# Use Partial-F test to check if dropping interation term is approraite
BIC.withoutCONTROL <- lm(formula = ADM_RATE ~ sqrtNUMBRANCH + AVGFACSAL + sqrtPAR_ED_PCT_1STGEN + FEMALE + poweredPCT_WHITE + MD_FAMINC  +  poweredPCT_WHITE:CONTROL_2, data = training.data)
anova( result.BIC, BIC.withoutCONTROL)

# Model Validation
Validation1 <- lm(formula = ADM_RATE  ~ sqrtNUMBRANCH + COSTT4_A + AVGFACSAL + 
                    PFTFAC + sqrtPAR_ED_PCT_1STGEN + FEMALE + poweredPCT_WHITE + 
                    PCT_BORN_US + UNEMP_RATE + POVERTY_RATE + MD_FAMINC + HBCU + 
                    TRIBAL + HSI + CONTROL_2 + poweredPCT_WHITE:CONTROL_2, data = testing.data)

Validation2 <- lm(ADM_RATE ~ sqrtNUMBRANCH + AVGFACSAL + sqrtPAR_ED_PCT_1STGEN + 
                    FEMALE + poweredPCT_WHITE + MD_FAMINC  + poweredPCT_WHITE:CONTROL_2, data = 
                    testing.data)

# Also Preform model diagnostic on the plot for validation dataset
plot(Validation2)
```