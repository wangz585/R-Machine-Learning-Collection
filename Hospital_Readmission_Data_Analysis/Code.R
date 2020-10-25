library(readr)
raw.df <- read_csv("diabetes_update.csv")
View(raw.df)

# Explore the data
install.packages("DataExplorer")
library(DataExplorer)

# Plot missing values
plot_missing(raw.df)

# From explore analysis, we want to remove payer_code, weight
raw.df <- subset(raw.df, select = -c(payer_code, weight, X1, patient_nbr))

# Combine readmitted column
raw.df$readmitted <- ifelse(raw.df$readmitted == 'NO', "NO", "YES") 

# Plot all missing observations
frame_intro <- introduce(raw.df)
plot_intro(raw.df)

# Discrete Variables
plot_bar(raw.df)

# Continuous Variables
plot_histogram(raw.df)

# View the missing variable
library("dplyr")
barplot(table(raw.df$medical_specialty), main="Distribution")
as.data.frame(table(raw.df$medical_specialty))

# I want to replace this variable's missing value with 'missing' value, 
# compare its performance with data imputation or something else

# View some observation
temp <- raw.df %>% filter(raw.df$number_emergency == 0 )
median(raw.df$'Length of Stay')

# Group encounters by patients
groupeddf <- raw.df %>% group_by(patient_nbr)

# Now, create the dataframe for modelling

# To do: select the first encounter of each patient

# Get all patient Ids
patient.id <- unique(raw.df$patient_nbr)
patient.df <- raw.df %>% filter(raw.df$patient_nbr == 0)
for (id in patient.id)
{
  temp <- (raw.df %>% filter(raw.df$patient_nbr == id ))[1,]
  patient.df <- rbind(patient.df, id = temp)
}
# Save it for the sake of saving running time
patient.df <- read_csv("patient_df.csv")

# To do: delete unnecessary variables

# For simplicity, replace white space
colnames(patient.df) <- gsub(" ","",colnames(patient.df))
colnames(patient.df) <- gsub(":0","",colnames(patient.df))
patient.df <- subset(patient.df, select = -c(Unnamed, X1 ))

# Drop Columns with missing values
plot_missing(patient.df)
patient.df <- subset(patient.df, select = -c(weight,payer_code))

# Keep Medical_Speciality
library(forcats) 
patient.df$medical_specialty<-fct_explicit_na(patient.df$medical_specialty, na_level = "None")

# Drop Meaningless columns
patient.df <- subset(patient.df, select 
                     = -c(encounter_id, admission_source_id, encounter_num))

# 'Inbalanced Columns'
plot_intro(patient.df)
plot_bar(patient.df)
plot_histogram(patient.df)

# Drop columns With No value more than
library(tidyverse)
column_names <- patient.df %>% select_if(negate(is.numeric))
column_names <- subset(column_names, select = -c(race, gender, age, 
                                                 max_glu_serum, A1Cresult))

proption.of.no <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(proption.of.no) <- c("Column Name", "Number", "Proportion")
for (i in names(column_names)){
  temp <- column_names %>% filter(column_names[[i]] == 'No')
  num <- nrow(temp)
  proption.of.no <- rbind(proption.of.no, c(i,num,num/71518))
}

patient.df <- subset(patient.df, select = -c(chlorpropamide, acetohexamide,
                                             tolbutamide,miglitol,troglitazone,
                                             tolazamide,glipizide.metformin,examide,
                                             citoglipton,glimepiride.pioglitazone,
                                             metformin.pioglitazone,metformin.rosiglitazone))


# To do: recategorized complicated variables

# readmitted 
patient.df$readmitted <- ifelse(patient.df$readmitted == 'NO', "No", "Yes") 

# Other variables with four categories, No contains most of them except insulin variable
column_names <- subset(column_names, select = -c( insulin,chlorpropamide, acetohexamide,
                                                  tolbutamide,miglitol,troglitazone,
                                                  tolazamide,glipizide.metformin,examide,
                                                  citoglipton,glimepiride.pioglitazone,
                                                  metformin.pioglitazone,medical_specialty,
                                                  metformin.rosiglitazone
                                                  ))


for (i in names(column_names)){
  patient.df[[i]] <- ifelse(patient.df[[i]] == "No", "No", "Yes") 
}



# Take a look at medical care
sort(table(patient.df$medical_specialty))

# This is too many, we only None, InternalMedicine, 
# Family/GeneralPractice, Emergency/Trauma, Cardiology and others

patient.df$medical_specialty[!patient.df$medical_specialty == 'None'&
                               !patient.df$medical_specialty == 'InternalMedicine'&
                               !patient.df$medical_specialty == 'Family/GeneralPractice'&
                               !patient.df$medical_specialty == "Emergency/Trauma"&
                               !patient.df$medical_specialty == "Cardiology"] <- 'Others'
patient.df$medical_specialty<-fct_explicit_na(patient.df$medical_specialty, na_level = "Others")
patient.df$medical_specialty <- factor(patient.df$medical_specialty)

# Check all na values
na <- patient.df[rowSums(is.na(patient.df)) > 0,]


# Modify fake continous variable into categorical
names <- c('admission_type_id' ,'discharge_disposition_id','readmitted',
           'race','gender','age','insulin','max_glu_serum','A1Cresult')
categorical <- colnames(column_names)
patient.df[,names] <- lapply(patient.df[,names] , factor)
patient.df[,categorical] <- lapply(patient.df[,categorical] , factor)
patient.df$readmitted<- factor(patient.df$readmitted)

str(patient.df)
patient.df <- subset(patient.df, select = -c(patient_nbr))

# Warning message
patient.df <- subset(patient.df, select = -c(discharge_disposition_id))

write.csv(patient.df,"cleandf.csv", row.names = FALSE)
# To do: Split into training and testing datasets
# Notice that since we only select one encounter per patient, so every patient is an observation
table(patient.df$readmitted)

set.seed(1005120012)
test_ind <- sample(seq_len(nrow(patient.df)), size = 20000)

test <- patient.df[test_ind, ]
train <- patient.df[-test_ind, ]

# To do: Model Diagnostic
# Build the full model
dat <- na.omit(train)

logit.mi <- glm(readmitted ~ ., data = dat, family = binomial(link = logit)) 


# To do: Use AIC, BIC, Elastic-net method to select model

AIC.variable <- step(logit.mi, trace = 0)
BIC.variable <- step(logit.mi, trace = 0, k = log(50087))
x.matrix <- model.matrix(logit.mi)[,-1]
library(glmnet)
cvfit <- cv.glmnet(x.matrix, dat$readmitted, family = "binomial", type.measure = "class")
coef(cvfit)
summary(cvfit)
load("AIC.Rdata")
save(AIC.variable, file="AIC.Rdata")
save(BIC.variable, file='BIC.Rdata')
# To do: Compare AIC, BIC coefficents
AIC(logit.mi)
BIC(logit.mi)

summary(AIC.variable)
summary(BIC.variable)

BIC.model <- glm(formula = readmitted ~ race + age + admission_type_id + 
                   LengthofStay  + num_procedures + number_outpatient + 
                   number_emergency + number_inpatient + number_diagnoses + 
                   metformin + insulin + diabetesMed, family = binomial(link = logit), 
                 data = dat)
AIC.model <- glm(formula = readmitted ~ race + gender + age  + 
                   LengthofStay + medical_specialty + num_procedures + 
                   number_outpatient + number_emergency + number_inpatient + 
                   number_diagnoses + A1Cresult + change
                   rosiglitazone + acarbose + insulin + diabetesMed, family = binomial(link = logit), 
                 data = dat)

elasticnet.model <- glm(formula = readmitted ~ race + gender + age + LengthofStay 
                        + medical_specialty + number_outpatient + number_emergency
                        + number_inpatient + number_diagnoses + A1Cresult + glipizide
                        + pioglitazone + rosiglitazone + acarbose + change + diabetesMed
                        , family = binomial(link = logit), 
                        data = dat)
summary(AIC.model)

# Create a table to compare AIC, BIC
AIC <- c(AIC(AIC.model), AIC(BIC.model), AIC(elasticnet.model))
BIC <- c(BIC(AIC.model), BIC(BIC.model), BIC(elasticnet.model))
ResidualDeviance <- c(64890,64982,65205)
DegreeofFreedom <- c(50041,50050,50053)
NumVariables <- c(21, 13, 16)
Model.comparison <- data.frame(AIC, BIC, ResidualDeviance, DegreeofFreedom, NumVariables)
rownames(Model.comparison) <- c('AIC Criteria', 'BIC Criteria', 'Elastic-Net Criteria')

summary(BIC.model)
# Thus we choose BIC criteria, but we need to lower number of variables

# likelihood ratio test
removed.model <- glm(formula = readmitted ~   age + race LengthofStay+
                       num_procedures + number_outpatient + 
                        number_inpatient + number_diagnoses + number_emergency +
                         metformin + insulin + diabetesMed, family = binomial(link = logit), 
                     data = train)
summary(removed.model)
# First, remove medical_specialty and do LRTlrtest, it fails
install.packages('lmtest')
library(lmtest)
lrtest(removed.model, BIC.model)

# singular information matrix in lrm.fit (rank= 19 ).  Offending variable(s):
#  gender=Unknown/Invalid 



# We cannot remove any variables, so, consider about combing with interaction terms

interaction.model <- glm(formula = readmitted ~  age + gender + admission_type_id + 
                           LengthofStay + medical_specialty + num_procedures + number_outpatient + 
                           number_emergency + number_inpatient + number_diagnoses + 
                           metformin +gender*insulin  + diabetesMed, family = binomial(link = logit), 
                         data = dat)
summary(interaction.model)
# ROC

# The calibration plot

# To do: Add interaction terms and use F test likelihood ratio test

# To do: Model Comparison: ROC curve
library(rms)
lrm.final <- lrm(readmitted ~   age  + 
                   LengthofStay + + num_procedures + number_outpatient + 
                   number_emergency + number_inpatient + number_diagnoses + 
                   metformin + insulin + diabetesMed, data=dat,  
                 x =TRUE, y = TRUE, model= T)
library(pROC)
p <- predict(lrm.final, type = "fitted")
roc_logit <- roc(dat$readmitted ~ p)
## The True Positive Rate ##
TPR <- roc_logit$sensitivities
## The False Positive Rate ##
FPR <- 1 - roc_logit$specificities
plot(FPR, TPR, xlim = c(0,1), ylim = c(0,1), type = 'l', lty = 1, lwd = 2,col = 'red')
abline(a = 0, b = 1, lty = 2, col = 'blue')
text(0.7,0.4,label = paste("AUC = ", round(auc(roc_logit),2)))
# No big difference between training and testing
# To do: Validation use test data set

# Calibration Plot
lrm.final.all <- lrm(readmitted ~ age  + 
                       LengthofStay + + num_procedures + number_outpatient + 
                       number_emergency + number_inpatient + number_diagnoses + 
                       metformin + insulin + diabetesMed, data=patient.df,  
                 x =TRUE, y = TRUE, model= T)

# Residual
res.dev <- residuals(lrm.final.all, type = "deviance")
par(family = 'serif')
plot(patient.df$readmitted, res.dev, xlab='Weight gained by mother', 
     ylab='Deviance Residuals')
lines(lowess(lbw$gained, res.dev), lwd=2, col='blue')
abline(h=0, lty='dotted')

# cross validation
lrm.final.all <- lrm(readmitted ~ age  + 
                       LengthofStay + + num_procedures + number_outpatient + 
                       number_emergency + number_inpatient + number_diagnoses + 
                       metformin + insulin + diabetesMed, data=train,  
                     x =TRUE, y = TRUE, model= T)
cross.calib <- calibrate(lrm.final.all, method="crossvalidation", B=10) # model calibration
par(family = 'serif')
plot(cross.calib, las=1, xlab = "Predicted Probability")



## Plot the dfbetas ##
library(epiDisplay)
library(openintro)
par(family = 'serif')
lrm.final.all <- lrm(readmitted ~ ge  + 
                       LengthofStay +  num_procedures + number_outpatient + 
                       number_emergency + number_inpatient + number_diagnoses + 
                       metformin + insulin + diabetesMed, data=train,  
                     x =TRUE, y = TRUE, model= T)
df.final <- dfbetas(lrm.final.all)
plot(patient.df$readmitted, df.final[,3], xlab='Weight gained by mother', 
     ylab='dfbeta')
lines(lowess(lbw$gained, df.final[,3]), lwd=2, col='blue')
abline(h=0, lty='dotted')
abline(h=-2/sqrt(nrow(df.final)), lty='dotted')
abline(h=2/sqrt(nrow(df.final)), lty='dotted')

