######## Random Forest Tutorial ######################

# Set working directory
getwd()
setwd("C:/Users/jadea/Documents/Project 2/Random Forest Tutorial")
workingdir = "."
getwd()

#Load packages
library(randomForest)
library(rfUtilities) #tests model significance
library(caret) # to get leave-one-out cross-validation accuracies and also contains the nearZeroVar function 
library(plyr) # for the "arrange" function
library(e1071)

# RF models are run when researchers want to classify samples into classes.
# Usually done based on multiple variables. 
# E.g. Taxa, functional categories --> ALSO KNOWN AS FEATURES. 

# RF can also be used to regress features against quantitative data.

# RF models are based on "decision trees". 
# Used for classification of a discrete variable or regression of a continuous variable. 
# CART trees. 

# loading in files 
otu_table <- read.table("otu_table_RF_tutorial.txt", sep = "\t", header = T, row.names = 1, stringsAsFactors = TRUE, comment.char = "")
metadata <- read.table("metadata_RF_tutorial.txt", sep = "\t", header = T, row.names = 1, stringsAsFactors = TRUE, comment.char = "")
#otu = snp address
#metadata = presence/absence 


# Check data #no. of rows, no. of columns
dim(otu_table) #1000 40
dim(metadata) #40 2

# Check summaries of data
str(metadata)
summary(metadata)

############# Pre-Processing ##########################
# One of the most important steps in ML. 
# The performance of the model isn't based on the choice of algorithm, but how the data is processed. 

# RF doesn't make any assumptions about how the data is distributed. 
# So not necessary to transform the data. 
# However, reducing noise input will improve model performance. 
# Aim to throw out features that are rare or have very low variance across samples. 
# Any cut-offs used in this step are abitary are would be based on the dataset. 

################ Removing Rare Features #################
# A basic way to decide which features are unlikely to be informative is if they are non-zero in only a small number of samples. 
# A good idea to look at the distribution for all features to help choose a reasonable cut-off. 

otu_nonzero_counts <- apply(otu_table, 1, function(y) sum(length(which(y > 0))))
hist(otu_nonzero_counts, breaks = 100, col = "grey", main = "", ylab = "Number of OTUs", xlab = "Number of Non-Zero Values")

# Typically researchers discard OTUs that are zero in >75-90% of samples. 
# Cut-off could potentially be optimised if you have an independent dataset or partition your data so that you have a validation set. 

# Removing features that are non-zero less than a specified proportion of the time. 
remove_rare <- function( table , cutoff_pro ) {
  row2keep <- c()
  cutoff <- ceiling( cutoff_pro * ncol(table) )  
  for ( i in 1:nrow(table) ) {
    row_nonzero <- length( which( table[ i , ]  > 0 ) ) 
    if ( row_nonzero > cutoff ) {
      row2keep <- c( row2keep , i)
    }
  }
  return( table [ row2keep , , drop=F ])
}

# Removing OTUs that have non-zero samples in <-20% of samples.
otu_table_rare_removed <- remove_rare(table = otu_table, cutoff_pro = 0.2)

# Seeing how many OTUs are left
dim(otu_table_rare_removed) #647 40
# 35% of OTUs were excluded.

# Renormalize the data so that each sample's column sums to 100. 
otu_table_rare_removed_norm <- sweep(otu_table_rare_removed, 2, colSums(otu_table_rare_removed) , '/')*100

# Sanity check.
colSums(otu_table_rare_removed_norm)

# Can also exclude OTUs that show low variance through samples. 
# Although not normally different from excluding based on no. of non-zero values.
# Uses nearZeroVar() from caret. 

############# Transforming the data ###################
# RF makes no assumption so not really necessary. 
# Certain transformations can make the features more comparable across samples. 
# Mainly when using relative abundance data. 

# ONLY APPLICABLE TO MICROBIOME SEQUENCING DATA AND DOES NOT REPRESENT STANDARD APPROACHES IN RF. 

# One approach is to standardise the data by subtracting each sample's mean then dividing by the sample's standard deviation. 
# subtract centre divide by scale. 
# Converts values into a Z score. 
otu_table_scaled <- scale(otu_table_rare_removed_norm, center = TRUE, scale = TRUE)

# Another method is to take the iverse hyperbolic sine and then to mean the center by sample. 
otu_table_asinh_mean_centred <- scale(asinh(otu_table), center = TRUE, scale = FALSE)

# Will not be the best transformation for all datasets. 
# Will the transformation help the model's performance without detracting too much from the interpretability. 

# Centred log-ratio (CLR) transformation.
# One of the most common transformations for read counts in each sample. 
# TYPICALLY PERFORMED ON READ COUNTS AND NOT RELATIVE ABUNDANCE. 
#A pseudocount needs to be added to deal with 0 values. Either 0.5 or 1. 
# Done using the clr function in the Compositions package. 

####################### Running the Model #################
# Need to get the input tables into the correct format. 

# Prep input tables for classification of state. 
otu_table_scaled_state <- data.frame(t(otu_table_scaled))  
otu_table_scaled_state$state <- metadata[rownames(otu_table_scaled_state), "state"]  

# Prep input tables for regression of inflammation score (IS)
otu_table_scaled_IS <- data.frame(t(otu_table_scaled))
otu_table_scaled_IS$IS <- metadata[rownames(otu_table_scaled_IS), "IS"]

# The 2 parameters for a RF model are the number of trees in the forest (ntree) & the number of features randomly sampled at each node in a tree (mtry).
# The more trees run in the forest the better the model will converge. 
# ~10,001 trees = a robust model. # Depending on computational time. 
# Aim for odd number of tress to ensure there are never any ties for binary classification models. 
# Better to stick with default values.

# Set a random seed. 
set.seed(151)

# Classify inflamed and control samples. 
RF_state_classify <- randomForest( x=otu_table_scaled_state[,1:(ncol(otu_table_scaled_state)-1)] , y=otu_table_scaled_state[ , ncol(otu_table_scaled_state)] , ntree=501, importance=TRUE, proximities=TRUE)

# Run RF to regress OTUs against inflammation score (IS)
RF_IS_regress <- randomForest( x=otu_table_scaled_IS[,1:(ncol(otu_table_scaled_IS)-1)] , y=otu_table_scaled_IS[ , ncol(otu_table_scaled_IS)] , ntree=501, importance=TRUE, proximities=TRUE )  

############# Assessing Model Fit ###########################
# In this example there is only a training set due to the small sample size. 
# Using a test set would allow us to validate the results. 
# Looking at the model summary.
RF_state_classify
RF_IS_regress

# 0% error rate, suggesting that this is a good model.

####################### Permutation Test ##################
# One way to assess model significance is testing whether the model's performance is more extreme than expected by chance. 

# Running the sig test with 1,000 permutations. 
RF_state_classify_sig <- rf.significance( x=RF_state_classify ,  xdata=otu_table_scaled_state[,1:(ncol(otu_table_scaled_state)-1)] , nperm=1000 , ntree=501 )  
RF_IS_regress_sig <- rf.significance( x=RF_IS_regress ,  xdata=otu_table_scaled_IS[,1:(ncol(otu_table_scaled_IS)-1)] , nperm=1000 , ntree=501 )  

################ Accuracy Estimated by Cross-Validation ################
# Systematically portion the data into training and test sets and repeatedly see how the model performs. 
# Simplest = leave-one-out cross-validation. 
fit_control <- trainControl( method = "LOOCV")

RF_state_classify_loocv <- train( otu_table_scaled_state[,1:(ncol(otu_table_scaled_state)-1)] , y=otu_table_scaled_state[, ncol(otu_table_scaled_state)] , method="rf", ntree=501 , tuneGrid=data.frame( mtry=25 ) , trControl=fit_control )
RF_IS_regress_loocv <- train( otu_table_scaled_IS[,1:(ncol(otu_table_scaled_IS)-1)] , y=otu_table_scaled_IS[, ncol(otu_table_scaled_IS)] , method="rf", ntree=501 , tuneGrid=data.frame( mtry=215 ) , trControl=fit_control )

# View performance metrics
RF_state_classify_loocv$results
RF_IS_regress_loocv$results

############## Identifying Important Features ##############
par(mfrow=c(1,2))
RF_state_classify_imp <- as.data.frame( RF_state_classify$importance )
RF_state_classify_imp$features <- rownames( RF_state_classify_imp )
RF_state_classify_imp_sorted <- arrange( RF_state_classify_imp  , desc(MeanDecreaseAccuracy)  )
barplot(RF_state_classify_imp_sorted$MeanDecreaseAccuracy, ylab="Mean Decrease in Accuracy (Variable Importance)", main="RF Classification Variable Importance Distribution")

RF_IS_regress_imp <- as.data.frame( RF_IS_regress$importance )
RF_IS_regress_imp$features <- rownames( RF_IS_regress_imp )
RF_IS_regress_imp_sorted <- arrange( RF_IS_regress_imp  , desc(`%IncMSE`)  )
barplot(RF_IS_regress_imp_sorted$`%IncMSE`, ylab="% Increase in Mean Squared Error (Variable Importance)", main="RF Regression Variable Importance Distribution")

# Usually more informative to look at a subset of top features.
# E.g. The top 10 features:
barplot(RF_state_classify_imp_sorted[1:10,"MeanDecreaseAccuracy"], names.arg=RF_state_classify_imp_sorted[1:10,"features"] , ylab="Mean Decrease in Accuracy (Variable Importance)", las=2, ylim=c(0,0.02), main="Classification RF")  
barplot(RF_IS_regress_imp_sorted[1:10,"%IncMSE"], names.arg=RF_IS_regress_imp_sorted[1:10,"features"] , ylab="% Increase in Mean Squared Error (Variable Importance)", las=2, ylim=c(0,0.012), main="Regression RF")  

############## Saving Files for Future Use #################
saveRDS( file = "RF_state_model.rda" , RF_state_classify )
saveRDS( file = "RF_IS_model.rda" , RF_IS_regress )

############## Code to read back in the files that are saved above ############
setwd("/Path/to/my/RF_tutorial/")    
RF_state_model <- readRDS("RF_state_model.rda")     
RF_IS_model <- readRDS("RF_IS_model.rda")  
