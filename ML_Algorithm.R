######## Random Forest ML Algorithm - Project 2 ##########

############### Set the working directory ################
getwd()
setwd("C:/Users/jadea/Documents/Project 2/Datasets")
workingdir = "."
getwd() # This is just to check the change worked. 

################## Load in R Packages ###################
# Load R Packages
library(randomForest)
library(rfUtilities) #tests model significance
library(caret) # to get leave-one-out cross-validation accuracies and also contains the nearZeroVar function 
library(plyr) # for the "arrange" function
library(e1071)
library(dplyr) #Subsetting data
library(tibble) #Organising data frames
## To install its: install.packages("Package_Name")

################# Load in datasets #######################
# Load files
testingdataset <- read.csv("Lactamase_Testing_Dataset.csv")
metadata <- read.csv("Lactamase_Development_Dataset.csv")

dim(metadata) #658 13
dim(testingdataset) #379 13

############## Removing isolates with no SNP.address ###########
metadata <- metadata[-which(metadata$SNP.address == ""), ]
testingdataset <- testingdataset[-which(testingdataset$SNP.address == ""), ]

################ Subsetting Development Dataset #########
# Subsetting data into SNP address and presence/absence of AMR gene
SNP_address <- metadata %>% select(Reference.No., X250, X100, X50, X25, X10, X5, X0)
AMR_Presence <- metadata %>% select(Reference.No., `Î².Lactamases`, AMR)  

################ Rearranging the data ####################

# Formatting SNP address table
SNP_address_transpose <- as.data.frame(t(SNP_address), stringsAsFactors = FALSE)
SNP_address_transpose[] <- lapply(SNP_address_transpose, type.convert, as.is = TRUE)
colnames(SNP_address_transpose) <- c(SNP_address$Reference.No.)
SNP_address_final <- SNP_address_transpose[-c(1), ]

# Formatting AMR table
rownames(AMR_Presence) <- c(AMR_Presence$Reference.No.)
AMR_Presence_final <- AMR_Presence %>% select(AMR)

################ Dataset Details ######################
# Check data #no. of rows, no. of columns
dim(SNP_address_final) #7 650
dim(AMR_Presence_final) #650 1

# Check summaries of data
str(SNP_address_final)
summary(SNP_address_final)

################# Running the model ###################
# Prep input tables for classification of state. 
SNP_adress_final_AMR <- data.frame(t(SNP_address_final))  
SNP_adress_final_AMR$AMR <- AMR_Presence[rownames(SNP_adress_final_AMR), "AMR"]  

#Set random seed
set.seed(151)

# Classify inflamed and control samples. 
SNP_adress_final_AMR$AMR = factor(SNP_adress_final_AMR$AMR)
RF_AMR_classify <- randomForest( x=SNP_adress_final_AMR[,1:(ncol(SNP_adress_final_AMR)-1)] , y=SNP_adress_final_AMR[ , ncol(SNP_adress_final_AMR)] , ntree=501, importance=TRUE, proximities=TRUE)
RF_AMR_classify

# Running the sig test with 1,000 permutations. 
RF_AMR_classify_sig <- rf.significance( x=RF_AMR_classify,  xdata=SNP_adress_final_AMR[,1:(ncol(SNP_adress_final_AMR)-1)] , nperm=1000 , ntree=501 ) # p-Value = 0

################ Accuracy Estimated by Cross-Validation ################
# Systematically portion the data into training and test sets and repeatedly see how the model performs. 
# Simplest = leave-one-out cross-validation. 
fit_control <- trainControl( method = "LOOCV")

RF_AMR_classify_loocv <- train( SNP_adress_final_AMR[,1:(ncol(SNP_adress_final_AMR)-1)] , y=SNP_adress_final_AMR[, ncol(SNP_adress_final_AMR)] , method="rf", ntree=501 , tuneGrid=data.frame( mtry=25 ) , trControl=fit_control )

# View performance metrics
RF_AMR_classify_loocv$results #mtry = 25 Accuracy = 0.7246154 #Kappa = -0.003069124

############## Identifying Important Features ##############
par(mfrow=c(1,2))
RF_AMR_classify_imp <- as.data.frame( RF_AMR_classify$importance )
RF_AMR_classify_imp$features <- rownames( RF_AMR_classify_imp )
RF_AMR_classify_imp_sorted <- arrange( RF_AMR_classify_imp  , desc(MeanDecreaseAccuracy)  )
barplot(RF_AMR_classify_imp_sorted$MeanDecreaseAccuracy, ylab="Mean Decrease in Accuracy (Variable Importance)", main="RF Classification Variable Importance Distribution")

# Usually more informative to look at a subset of top features.
# E.g. The top 10 features:
barplot(RF_AMR_classify_imp_sorted[1:10,"MeanDecreaseAccuracy"], names.arg=RF_AMR_classify_imp_sorted[1:10,"features"] , ylab="Mean Decrease in Accuracy (Variable Importance)", las=2, ylim=c(0,0.02), main="Classification RF")  

############## Saving Files for Future Use #################
saveRDS( file = "RF_AMR_model.rda" , RF_AMR_classify )

############## Code to read back in the files that are saved above ############
setwd("/Path/to/my/RF_tutorial/")    
RF_AMR_model <- readRDS("RF_AMR_model.rda") 
