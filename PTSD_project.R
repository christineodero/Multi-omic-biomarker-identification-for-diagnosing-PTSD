#Clear global environment
#rm(list=ls())
#load libraries
library(dplyr)
library(Matrix)
library(readxl)
library(stats)
library(base)
library(utils)
library(class)
library(caret)# for general data preparation and model fitting
library(ggplot2)
library(lattice)
library(randomForest)
library(DataExplorer)
library(mlbench)
library(miceadds)


#set working directory
getwd()
setwd("/home/christineodero/Documents/project")
#upload data
PTSD_OLink_FCC_data<-read_excel("PTSD_OLink_FCC.xlsx")

#check datatypes & show data structure
#str(PTSD_OLink_FCC_data)
head(PTSD_OLink_FCC_data)
dim(PTSD_OLink_FCC_data)

# Delete columns that are not needed and save data in a new variable
PTSD_OLink_FCC_data1 <- PTSD_OLink_FCC_data[, -c(1,2,4,5,6,11:17)]

# view the new data structure
head(PTSD_OLink_FCC_data1)
dim(PTSD_OLink_FCC_data1)
ncol(PTSD_OLink_FCC_data1)
nrow(PTSD_OLink_FCC_data1)

#finding rows with missing records
sum(is.na(PTSD_OLink_FCC_data1))
PTSD_OLink_FCC_data1[!complete.cases(PTSD_OLink_FCC_data1),]
#finding number of rows with missing data
nrow(PTSD_OLink_FCC_data1[!complete.cases(PTSD_OLink_FCC_data1),])

#find unique in Gender and filter for Male then inspect the column
unique(PTSD_OLink_FCC_data1$`Gender`)
length(which(PTSD_OLink_FCC_data1$Gender == "Male"))
PTSD_OLink_FCC_data1_filt <-filter(PTSD_OLink_FCC_data1,`Gender` == "Male")

#find unique in FCC phase and filter for T3 then inspect the column
unique(PTSD_OLink_FCC_data1_filt$`FCC phase`)
length(which(PTSD_OLink_FCC_data1_filt$`FCC phase` == "T3"))
PTSD_OLink_FCC_data1_filt<-filter(PTSD_OLink_FCC_data1_filt,`FCC phase` == "T3")
unique(PTSD_OLink_FCC_data1_filt$`FCC phase`)

#Use mice R package after filtering for T3 and Male using pmm (predictive mean matching)
library(mice)
data_input<-PTSD_OLink_FCC_data1_filt[,c(6:465)]
data_input_mice <- mice(data_input,method='pmm',m=1,maxit=50 ,seed=123)
summary(data_input_mice)
data_imput_mice_complete <- complete(data_input_mice, 1)
#join the feature variable and the imputed variables
PTSD_OLink_FCC_data1_filt1<-cbind(PTSD_OLink_FCC_data1_filt[1:5],data_imput_mice_complete[1:460])

#inspect the new dataset
sum(is.na(PTSD_OLink_FCC_data1_filt1))
nrow(PTSD_OLink_FCC_data1_filt1)
head(PTSD_OLink_FCC_data1_filt1)
dim(PTSD_OLink_FCC_data1_filt1)

#creating a data frame of positive and negative from PTSD status, 
length(PTSD_OLink_FCC_data1_filt1$PTSD_status)
unique(PTSD_OLink_FCC_data1_filt1$PTSD_status)
df_PTSD_Status<- PTSD_OLink_FCC_data1_filt1[, -c(1,3:5)]
df_PTSD_Status$PTSD_status <- as.factor(df_PTSD_Status$PTSD_status)
length(which(df_PTSD_Status$PTSD_status == "Positive"))
#inspect the new dataset
unique(df_PTSD_Status$PTSD_status) 


#creating a data frame of G1 G2 from Patient biotype
length(PTSD_OLink_FCC_data1_filt1$`Patient bioty;e`)
unique(PTSD_OLink_FCC_data1_filt1$`Patient bioty;e`)
df_patient_biotype<- PTSD_OLink_FCC_data1_filt1[, -c(1:3,5)]
df_patient_biotype <- df_patient_biotype %>% filter(`Patient bioty;e` %in% c("G1","G2"))
df_patient_biotype$`Patient biotype`<-as.factor(df_patient_biotype$`Patient biotype`)

#inspect the new dataset
head(df_patient_biotype)
unique(df_patient_biotype$`Patient biotype` )
dim(df_patient_biotype)
length(which(df_patient_biotype$Patient_biotype == "G2"))
sum(is.na(df_patient_biotype)) 
#colnames((df_patient_biotype)[1] <- "Patient_biotype"

#creating a data frame of NYU Group PTSD,HRG,LRG
length(PTSD_OLink_FCC_data1_filt1$`NYU group`)
unique(PTSD_OLink_FCC_data1_filt1$`NYU group`)

df_NYU_group<-PTSD_OLink_FCC_data1_filt1[-c(1:4)]
df_NYU_group<-df_NYU_group %>%filter(`NYU group`%in% c("PTSD","HRG","LRG"))
length(which(df_NYU_group$`NYU group` == "LRG"))
df_NYU_group$`NYU group`<-as.factor(df_NYU_group$`NYU group`)
#inspect the new dataset
head(df_NYU_group)
unique(df_NYU_group$`NYU group`)


#creating a data frame of NYU Group PTSD,HRG,
PTSD_HRG<-PTSD_OLink_FCC_data1_filt1[-c(1:4)]
PTSD_HRG<-PTSD_HRG %>%filter(`NYU group`%in% c("PTSD","HRG"))
PTSD_HRG$`NYU group`<-as.factor(PTSD_HRG$`NYU group`)
head(PTSD_HRG)
unique(PTSD_HRG$`NYU group`)
sum(is.na(PTSD_HRG))

####### ########### RANDOM FOREST #### Reverse Feature Elimination(df_PTSD_Status,
#df_patient_biotype, df_NYU_group and PTSD_HRG )
#Set the seed for reproducibility
#createDataPartition() function from the caret package

###############df_PTSD_Status######################
#split data into training (80%) and testing set (20%)
set.seed(123)
training = createDataPartition(df_PTSD_Status$PTSD_status,p = 0.8, list=FALSE)
train = df_PTSD_Status[training, ]
test = df_PTSD_Status[-training, ]
# Create a RFE selector using the random forest model
set.seed(123)
rfFuncs$summary <- twoClassSummary
rfeControl <- rfeControl(functions = rfFuncs, method = "repeatedcv", repeats = 5, number=10)
#Perform RFE 
results_rfe_PS <- rfe(PTSD_status~., data = df_PTSD_Status,sizes = c(3:25), rfeControl = rfeControl,metric="ROC")
results_rfe_PS_all <- results_rfe_PS
results_rfe_PS_all
predictors(results_rfe_PS_all)
#on the training data 80%
#result_rfe<-rfe(`PTSD_status`~., train,sizes = c(3:25),rfeControl = rfeControl)#training data 80%
#Summarize the results of the RFE model
#result_rfe
#predictors(result_rfe)
#on test data 20%
#result_rfes<-rfe(`PTSD_status`~., test,sizes = c(3:25),rfeControl = rfeControl) testing data 20%
#result_rfes
#predictors(result_rfes)


###################df_patient_biotype###########
#split data into training (80%) and testing set (20%)
set.seed(123)
trainings = createDataPartition(df_patient_biotype$Patient_biotype, p = 0.8, list = FALSE)
trains = df_patient_biotype[trainings, ]
tests = df_patient_biotype[-trainings, ]
# Create a RFE selector using the random forest model
set.seed(123)
rfFuncs$summary <- twoClassSummary
rfeCtrl <- rfeControl(functions = rfFuncs, method = "repeatedcv", repeats = 5, number=10)
# Performing RFE
results_rfe_PB <- rfe(Patient_biotype~., data = df_patient_biotype, sizes = c(3:25), rfeControl = rfeCtrl,metric="ROC")
results_rfe_PB_G2G1 <- results_rfe_PB
results_rfe_PB_G2G1
predictors(results_rfe_PB_G2G1)
#on the training data 80%
#reslt_rfe = rfe(`Patient_biotype`~., trains,sizes = c(3:25),rfeControl = rfeCtrl)
#reslt_rfe
#on test data 20%
#reslts_rfe = rfe(`Patient_biotype`~., test,sizes = c(3:25),rfeControl = rfeCtrl)
#reslts_rfe



##############df_NYU_group  ################## 3 LEVELS
#split data into training (80%) and testing set (20%)
set.seed(123)
trainin = createDataPartition(df_NYU_group$`NYU group`, p = 0.8, list = FALSE)
trainn = df_NYU_group[trainin, ]
testss =df_NYU_group[-trainin, ]
set.seed(123)
# Create a RFE selector using the random forest model
rfe_Control <- rfeControl(functions = rfFuncs, method = "repeatedcv", repeats = 5, number=10)
# Performing RFE
results_rfe_NYU <- rfe(`NYU group`~., data = df_NYU_group, sizes = c(3:25), rfeControl = rfe_Control)
results_rfe_NYU_all <- results_rfe_NYU
results_rfe_NYU_all
predictors(results_rfe_NYU_all)
#on the training data 80%
#results_rfe_tr = rfe(`NYU group`~., trainn,sizes = c(3:25),rfeControl = rfe_Control)
#results_rfe_tr
#predictors(results_rfe_tr)
#on test data 20%
# Performing RFE
#set.seed(123)
#reslt_rfes_ts = rfe(`NYU group`~., testss,sizes = c(3:25),rfeControl = rfe_Control)
#reslt_rfes_ts


###############PTSD_HRG####################
# createDataPartition() function from the caret package  
#split data into training (80%) and testing set (20%)
set.seed(123)
training_ph = createDataPartition(PTSD_HRG$`NYU group`, p = 0.8, list = FALSE)
train_ph = PTSD_HRG[training_ph, ]
test_ph = PTSD_HRG[-training_ph, ]
# Create a RFE selector using the random forest model
set.seed(123)
rfFuncs$summary <- twoClassSummary
rfecontrl <- rfeControl(functions = rfFuncs, method = "repeatedcv", repeats = 5, number=10)
# Performing RFE
results_rfe_PH <- rfe(`NYU group`~., data = PTSD_HRG,sizes = c(3:25),rfeControl = rfecontrl,metric="ROC")
results_rfe_PH_all<-results_rfe_PH
results_rfe_PH_all
predictors(results_rfe_PH_all)
#on the training data 80%
#reslt = rfe(`NYU group`~., train_ph,sizes = c(3:25),rfeControl = rfecontrl)
#reslt
#on test data 20%
#reslts = rfe(`NYU group`~., test_ph,sizes = c(3:25),rfeControl = rfecontrl)
#reslts

#####G2 | HRG########Dataframes
#create a DF then filter  for G2|HRG,
library(tidyr)
biotype<-PTSD_OLink_FCC_data1_filt1[-c(1:3,5)]
NYUgroup<-PTSD_OLink_FCC_data1_filt1[-c(1:3,4)]
colnames(biotype)[1] <- "Patient_biotype"
biotypeG2 <- filter(biotype, Patient_biotype == "G2")
colnames(NYUgroup)[1] <- "NYU_group"
NYUgroupHRG <- filter(NYUgroup, NYU_group == "HRG")
colnames(biotypeG2)[1] <- "Status"
colnames(NYUgroupHRG)[1] <- "Status"
G2vsHRG <- full_join(biotypeG2, NYUgroupHRG)
View(G2vsHRG)
G2vsHRG$Status <- as.factor(G2vsHRG$Status)

#######RFE####G2VSHRG
set.seed(123)
training_gh = createDataPartition(G2vsHRG$Status, p = 0.8, list = FALSE)
train_gh = G2vsHRG[training_gh, ]
test_gh = G2vsHRG[-training_gh, ]
# Create a RFE selector using the random forest model
set.seed(123)
rfFuncs$summary <- twoClassSummary
rfecontrls <- rfeControl(functions = rfFuncs, method = "repeatedcv", repeats = 5, number=10)
# Performing RFE
results_G2HRG <- rfe(`Status`~., data = G2vsHRG,sizes = c(3:25),rfeControl = rfecontrls,metric="ROC")
results_G2HRG_all<-results_G2HRG
results_G2HRG_all
predictors(results_G2HRG_all)
#on the training data 80%
#resl_t = rfe(`Status`~., train_gh,sizes = c(3:25),rfeControl = rfecontrls)
#resl_t
#on test data 20%
#resl_ts = rfe(`Status`~., test_gh,sizes = c(3:25),rfeControl = rfecontrls)
#resl_ts 


########G1|HRG Dataframes
#create a DF then filter  for G1|HRG
biotypeG1 <- filter(biotype, Patient_biotype == "G1")
colnames(biotypeG1)[1] <- "Status"
G1vsHRG <- full_join(biotypeG1, NYUgroupHRG)
View(G1vsHRG)
G1vsHRG$Status <- as.factor(G1vsHRG$Status)

#######RFE####G1VSHRG
set.seed(123)
training_gh1 = createDataPartition(G1vsHRG$Status, p = 0.8, list = FALSE)
train_gh1 = G1vsHRG[training_gh1, ]
test_gh1 = G1vsHRG[-training_gh1, ]
# Create a RFE selector using the random forest model
set.seed(123)
rfFuncs$summary <- twoClassSummary
rfecontrl_s <- rfeControl(functions = rfFuncs, method = "repeatedcv", repeats = 5, number=10)
# Performing RFE
results_G1HRG <- rfe(`Status`~., data = G1vsHRG,sizes = c(3:25),rfeControl = rfecontrl_s,metric="ROC")
results_G1HRG_all<-results_G1HRG
results_G1HRG_all
predictors(results_G1HRG_all)
#on the training data 80%
#r_eslt = rfe(`Status`~., train_gh1,sizes = c(3:25),rfeControl = rfecontrl_s)
#r_eslt
#on test data 20%
#re_slts = rfe(`Status`~., test_gh1,sizes = c(3:25),rfeControl = rfecontrl_s)
#re_slts 

####G1 NEG#########
#DF for Negative
Neg_pos<-PTSD_OLink_FCC_data1_filt1[-c(1,3:5)]
Negative <- filter(Neg_pos, PTSD_status == "Negative")
colnames(Negative)[1] <- "Status"

#df for G1
G1_NEG <- full_join(biotypeG1, Negative)
G1_NEG$Status <- as.factor(G1_NEG$Status)
#######RFE####G1 NEG
set.seed(123)
training_GN = createDataPartition(G1_NEG$Status, p = 0.8, list = FALSE)
train_GN = G1_NEG[training_GN, ]
test_GN = G1_NEG[-training_GN, ]
# Create a RFE selector using the random forest model
set.seed(123)
rfFuncs$summary <- twoClassSummary
rfecon_trls <- rfeControl(functions = rfFuncs, method = "repeatedcv", repeats = 5, number=10)
# Performing RFE
results_G1_NEG <- rfe(`Status`~., data = G1_NEG,sizes = c(3:25),rfeControl = rfecon_trls,metric="ROC")
results_G1_NEG_all<-results_G1_NEG
results_G1_NEG_all
predictors(results_G1_NEG_all)
#on the training data 80%
#r_eslt = rfe(`Status`~., train_GN,sizes = c(3:25),rfeControl = rfecon_trls)
#r_eslt
#on test data 20%
#re = rfe(`Status`~., test_GN,sizes = c(3:25),rfeControl = rfecon_trls)
#re 

#####G2 NEG######                      
G2_NEG <- full_join(biotypeG2, Negative)
G2_NEG$Status <- as.factor(G2_NEG$Status)
length(which(G2_NEG$Status == "Negative"))
#######RFE####G2 NEG
set.seed(123)
training_GNe = createDataPartition(G2_NEG$Status, p = 0.8, list = FALSE)
train_GNe = G2_NEG[training_GNe, ]
test_GNe = G2_NEG[-training_GNe, ]
# Create a RFE selector using the random forest model
set.seed(123)
rfFuncs$summary <- twoClassSummary
rfecon_trl <- rfeControl(functions = rfFuncs, method = "repeatedcv", repeats = 5, number=10)
# Performing RFE
results_G2_NEG <- rfe(`Status`~., data = G2_NEG,sizes = c(3:25),rfeControl = rfecon_trl, metric="ROC")
results_G2_NEG_all<-results_G2_NEG
results_G2_NEG_all
predictors(results_G2_NEG_all)
#on the training data 80%
#r_eslt = rfe(`Status`~., train_GNe,sizes = c(3:25),rfeControl = rfecon_trls)
#r_eslt
#on test data 20%
#re = rfe(`Status`~., test_GNe,sizes = c(3:25),rfeControl = rfecon_trls)
#re 













