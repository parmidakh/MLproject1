### This line reads the CSV file named "pml-training.csv" and stores its contents in the variable training.
training <- read.csv(file='pml-training.csv',sep=',',header=T,na.strings=c("NA","NaN", "#DIV/0!")) 
### This line reads the CSV file named "pml-testing.csv" and stores its contents in the variable testing. Similar to the previous line, it uses the read.csv function with the same parameters.
testing<- read.csv(file='pml-testing.csv',sep=',',header=T,na.strings=c("NA","NaN", "#DIV/0!"))

### This line uses the createDataPartition function from the "caret" package to partition the training data into two subsets: one for training and one for testing. 
testIndex = createDataPartition(training$classe, p = 0.30,list=FALSE)
###  This line creates the mytrain dataset by excluding the rows specified by the testIndex vector from the training dataset. In other words, it contains the data that will be used for model training.
mytrain = training[-testIndex,]
mytest = training[testIndex,]


### ommitting sparse columns
### This line creates a logical matrix mask that has the same dimensions as the mytrain dataset. Each element of the mask matrix is TRUE if the corresponding element in the mytrain dataset is a missing value (NA), and FALSE otherwise. This matrix effectively identifies the missing values in the mytrain dataset.
mask <- is.na(mytrain)
###  This line calculates the column-wise mean of the logical mask matrix. Since TRUE is treated as 1 and FALSE as 0 in R, the resulting clM vector represents the proportion of missing values in each column of the mytrain dataset. Higher values indicate more missing values in a column.
clM<-colMeans(mask)
thres <- min(clM[clM>0])
ii <- (colMeans(mask) < thres)
summary(ii) ### 60 columns remaining
mytrain_red <- mytrain[,ii]

### ommitting properties that mustn't be relevant for a decision
### this part of the code creates a new dataset named mytrain_red2 that is a modified version of the previously reduced dataset mytrain_red. It excludes specific columns (listed in drops) that are not considered useful for analysis or modeling. This step can help streamline the dataset by removing irrelevant or redundant information.
drops <-  c("X","user_name","raw_timestamp_part_1","raw_timestamp_part_2","cvtd_timestamp" ,"new_window","num_window")
mytrain_red2 <- mytrain_red[,!(names(mytrain_red) %in% drops)]

library(randomForest)
rfr2<-randomForest(classe ~ .,data=mytrain_red2)
#OOB estimate of  error rate: 0.52%


### trying to reduce noise and prevent overfitting
### since total values are separate properties 3D data add only 2dof but noise
### reducing 3D data wherever they are in the least important quantile
drp <- c(names(mytrain_red2[grepl('accel_belt_',names(mytrain_red2))]))
drp <- c(drp,names(mytrain_red2[grepl('gyros_belt_',names(mytrain_red2))]))
drp <- c(drp,names(mytrain_red2[grepl('magnet_belt_',names(mytrain_red2))]))
drp <- c(drp,names(mytrain_red2[grepl('gyros_arm_',names(mytrain_red2))]))
drp <- c(drp,names(mytrain_red2[grepl('accel_arm_',names(mytrain_red2))]))
drp <- c(drp,names(mytrain_red2[grepl('magnet_arm_',names(mytrain_red2))]))
drp <- c(drp,names(mytrain_red2[grepl('gyros_dumbbell_',names(mytrain_red2))]))
drp <- c(drp,names(mytrain_red2[grepl('accel_dumbbell_',names(mytrain_red2))]))
drp <- c(drp,names(mytrain_red2[grepl('magnet_dumbbell_',names(mytrain_red2))]))
drp <- c(drp,names(mytrain_red2[grepl('accel_forearm_',names(mytrain_red2))]))
drp <- c(drp,names(mytrain_red2[grepl('magnet_forearm_',names(mytrain_red2))]))
mytrain_red3 <- mytrain_red2[,!(names(mytrain_red2) %in% drp)]### dim 20
rfr3<-randomForest(classe ~ .,data=mytrain_red3)
#OOB estimate of  error rate: 0.77%


### omitting all 3D data
drp <- c(names(mytrain_red2[grepl('_x',names(mytrain_red2))]))
drp <- c(drp,names(mytrain_red2[grepl('_y',names(mytrain_red2))]))
drp <- c(drp,names(mytrain_red2[grepl('_z',names(mytrain_red2))]))
mytrain_red4 <- mytrain_red2[,!(names(mytrain_red2) %in% drp)]### dim 17
#OOB estimate of  error rate: 0.97%


prd<-predict(rfr4,mytest)
cm<-confusionMatrix(prd,mytest$classe)
aa<-(cm$table+1)#/sum(cm$table+1)
for (i in seq(5)) aa[,i] <- aa[,i]/colMeans(aa)[i]/5
ggplot(melt(aa), aes(Reference,Prediction, fill=value)) + geom_raster()+ scale_fill_gradient( trans = 'log',name="log(Pr)")

library(caret)
modelrf4 <- train(classe ~ ., method='rf',data=mytrain_red4)
#OOB estimate of  error rate: 0.96%
prd<-predict(modelrf4,mytest)
#Accuracy : 0.9868 ## out-of-sample
aa<-(cm$table+1)#/sum(cm$table+1)
for (i in seq(5)) aa[,i] <- aa[,i]/colMeans(aa)[i]/5
ggplot(melt(aa), aes(Reference,Prediction, fill=value)) + geom_raster()+ scale_fill_gradient( trans = 'log',name="log(Pr)")


#modelFit <- train(classe ~ ., method='ada',data=mytrain) ##Currently this procedure can not directly handle > 2 class response
#modelgbm <- train(classe ~ ., method='gbm',data=mytrain) 



### try without X and without username
## rf involves bootstrapping




### sec 5.1: 17 parameters:
## with summary(training[grepl('dumbbell',names(training))])
#'pitch_forearm',
### no clear matches:
#maximum and minimum of the gyro   vs   gyros_forearm_x   gyros_forearm_y     gyros_forearm_z
