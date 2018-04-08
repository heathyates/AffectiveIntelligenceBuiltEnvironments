#################################################################################################
#Author: hlyates Yates                                                                            #
#Date: 03/14/2018                                                                               # 
#Description: Random Forest                                                                     # 
#################################################################################################
library(e1071)
library(openxlsx)
library(ROCR)
library(pROC)

#Section 1: Helper Files 
RemoveAndCreateDirectories <- function(exportDirectoryLoocv) {
    #Make Levels Equal 
    #
    #Args: 
    #   exportDirectory: 
    #
    #Returns:
    #   None 

    #Step 0: Detect i
    if (file.exists(exportDirectoryLoocv)) {

        #Step 0.1: Remove the files in the directory 
        do.call(file.remove, list(list.files(exportDirectoryLoocv, full.names = TRUE)))

        #Step 0.2: Create the directory again 
        dir.create(exportDirectoryLoocv)

    } else {

        dir.create(exportDirectoryLoocv)
    }




}





MakeLevelsEqual <- function(cvData) {
    #Make Levels Equal 
    #
    #Args: 
    #   dataParentDirectory: File path to the parent directory of the data for training and testing  
    #
    #Returns:
    #   cvData: data with the proper levels 

    #Step 0: Load master data and find levels for data 
    masterData <- read.csv(file.path("C:/Users/heath/OneDrive/Documents/Research/Current/Dissertation/3. WAM/10. Appendix - Normalized Cleaned Data/normalized master data", "all_participants_master_with_normalized.csv"))

    #Step 1: Remove NAs and make binary annotation a factor
    masterData[is.na(masterData)] <- 0
    masterData$BINARYANNOTATION <- as.factor(masterData$BINARYANNOTATION)

    #Step 2: Make training data levels equal to masterData levels
    levels(cvData$BINARYANNOTATION) <- levels(masterData$BINARYANNOTATION)
    levels(cvData$ZONE) <- levels(masterData$ZONE)
    levels(cvData$GENDER) <- levels(masterData$GENDER)
    levels(cvData$BODYSHAPE) <- levels(masterData$BODYSHAPE)
    levels(cvData$URBANORIGIN) <- levels(masterData$URBANORIGIN)
    levels(cvData$URBANPREFERENCE) <- levels(masterData$URBANPREFERENCE)
    levels(cvData$STUDYAREAFAMILIARITY) <- levels(masterData$STUDYAREAFAMILIARITY)
    levels(cvData$EXERCISE) <- levels(masterData$EXERCISE)
    levels(cvData$HRZONENORM) <- levels(masterData$HRZONENORM)
    levels(cvData$EDAZONENORM) <- levels(masterData$EDAZONENORM)
    levels(cvData$EXERCISEDPASTTHREEHOURS) <- levels(masterData$EXERCISEDPASTTHREEHOURS)

    return(cvData)

}

CalculateModelPerformanceMetrics <- function(fold, fit, aic, pR2, chiSquarePvalue, testData) {

    #Calculate the model performance metrics for the cross validation fit 
    #
    #Args: 
    #   dataParentDirectory: File path to the parent directory of the data for training and testing  
    #
    #Returns:
    #   cvData: data with the proper levels 

    #Step 0: Calculate the prediction values 
    fitted.results <- predict(fit, testData, type = "response")
    predict <- ifelse(fitted.results > 0.49, 1, 0)

    #Step 1: Calculate the missclassification error and accuracy 
    missclassificationError <- mean(predict != testData$BINARYANNOTATION)
    accuracy <- 1 - missclassificationError

    #Step 2: Calculate the AUC performance 
    #https://stackoverflow.com/questions/30366143/how-to-compute-roc-and-auc-under-roc-after-training-using-caret-in-r
    #https://stackoverflow.com/questions/4903092/calculate-auc-in-r
    auc <- 0 #By default, we set it to zero 

    #Step 2.1: Create the prediction object
    predictions <- as.vector(as.numeric(predict))
    labels <- as.vector(as.numeric(testData$BINARYANNOTATION))

    #Step 2.2: In order to do AUC we both need to have levels be the same (HINT: Ask about this on stackoverflow) 
    if (length(unique(predictions)) > 1 & length(unique(labels)) > 1) {
        pred <- prediction(predictions, labels)

        #Step 2.2: Find the AUC by doing a performance 
        perfAuc <- performance(pred, "auc")
        auc <- perfAuc@y.values[[1]]
    } else if (length(unique(predictions)) == 1) {
        auc <- 0.5
    }
    else {
        auc <- as.numeric(pROC::auc(predictions, labels)) #See: https://stackoverflow.com/questions/4903092/calculate-auc-in-r
    }

    #Step 3: Create a row and return the values 
    row <- cbind(FOLD = fold, ACCURACY = accuracy, AUC = auc, AIC = aic, PR2 = pR2, CHISQPValue = chiSquarePvalue)

    #Step 4: Return the row 
    return(row)

}

RunAlgorithm<- function(dataParentDirectory, exportDirectoryLoocv) {
    #Random Forest 
    #
    #Args: 
    #   dataParentDirectory: File path to the parent directory of the data for training and testing  
    #
    #Returns:
    #   None (but does write/output training and testing data sets) 

    print("Beginning script for running random forest")
    set.seed(415)

    #Step 0: Obtain the path for training and testing files 
    trainFilePath <- file.path(dataParentDirectory, "Train")
    testFilePath <- file.path(dataParentDirectory, "Test")

    #Step 1: Obtain the files in train and test folders 
    trainFiles <- list.files(trainFilePath)
    testFiles <- list.files(testFilePath)

    #Step 2: Create model performance data frame with will be output to exportdirectory  
    modelPerformanceColumns <- c("FOLD", "ACCURACY", "AUC", "AIC", "PR2", "CHISQPValue")
    modelPerformance <- data.frame(x = matrix("", nrow = 0, ncol = length(modelPerformanceColumns)))
    colnames(modelPerformance) <- list(modelPerformanceColumns)

    #Step 2: Learn and then verify on each fold 
    for (i in 1:length(trainFiles)) {

        #Debug: Use the following for debug purposes  
        #i <- 1

        print(paste0("Starting scripts for fold: ", i))


        #Step 2.1: Load the training data  
        trainingFilePath <- file.path(trainFilePath, paste0("train_", i, ".csv"))
        trainData <- read.csv(trainingFilePath, header = TRUE)

        #Step 2.2 Load the testing data  
        testingFilePath <- file.path(testFilePath, paste0("test_", i, ".csv"))
        testData <- read.csv(testingFilePath, header = TRUE)

        #Step 2.3 Process data in train and testing to replace NA with 0s
        #See: https://stackoverflow.com/questions/18562680/replacing-nas-with-0s-in-r-dataframe
        trainData[is.na(trainData)] <- 0
        testData[is.na(testData)] <- 0
        trainData$BINARYANNOTATION <- as.factor(trainData$BINARYANNOTATION)
        testData$BINARYANNOTATION <- as.factor(testData$BINARYANNOTATION)

        #Step 2.4: Make sure that the data has the proper levels 
        trainData <- MakeLevelsEqual(trainData)
        testData <- MakeLevelsEqual(testData)

        #Step 2.4: Train the data 
        print(paste0("Fitting for fold: ", i))
        fit <- glm(BINARYANNOTATION ~ WALKABILITY + factor(NUMLIGHTS) + factor(NUMTREES)
               + NUMINFLINE + NUMINFPOIN + factor(NUMGRASS),
                 family = binomial(link = 'logit'),
                 data = trainData)

        #Step 2.6 : Model selection criteria 
        #See: https://stats.stackexchange.com/questions/8511/how-to-calculate-pseudo-r2-from-rs-logistic-regression 
        #See: https://statisticalhorizons.com/hosmer-lemeshow
        #See: https://stats.stackexchange.com/questions/82105/mcfaddens-pseudo-r2-interpretation/99615 
        aic <- as.numeric(summary(fit)[5])
        pR2 <- 1 - fit$deviance / fit$null.deviance

        #Step 2.7 : Test H0: Null Model is good fit (vs Ha: not a good fit) 
        #See: https://stats.stackexchange.com/questions/141177/test-glm-model-using-null-and-model-deviances/141179#141179
        #the model as a whole is no better than the null model
        #pValue <- 1 - pchisq(fit$deviance, fit$df.residual)
        chiSquarePvalue <- 1 - pchisq(fit$null.deviance - fit$deviance, df = (fit$df.null - fit$df.residual))


        #See: https://stats.stackexchange.com/questions/59879/logistic-regression-anova-chi-square-test-vs-significance-of-coefficients-ano
        #anova(fit, test="Chisq")
        #anova(fit, test = "LRT")

        #Step 2.8: Calculate the model metrics 
        print(paste0("Conducting model performance for fold: ", i))
        rowOutput <- CalculateModelPerformanceMetrics(i, fit, aic, pR2, chiSquarePvalue, testData)

        #Step 2.9: Add this to the output
        modelPerformance <- rbind(modelPerformance, rowOutput)

    }

    #Step 3: Output the cross fold model performance to an export directory 
    print("Writing model performance to file")
    modelPerformanceFilePath <- file.path(exportDirectoryLoocv, "log_environmental_loocv.csv")
    write.csv(modelPerformance, modelPerformanceFilePath)


}




Main <- function() {
    #Random Forest 
    #
    #Args: 
    #   None 
    #
    #Returns:
    #   None (but does write/output training and testing data sets) 
    print("Starting algorithm training script.")

    #Step 0: Define the input parent directory with the data that contains training and testing as subdirectories 
    dataParentDirectory <- "C:/Users/heath/OneDrive/Documents/Research/Current/Dissertation/3. WAM/12. Appendix - ST Training and Test Data/LOOCV"

    #Step 1: Define the directory files 
    exportDirectory <- "C:/Users/heath/OneDrive/Documents/Research/Current/Dissertation/3. WAM/5. Results - Spatiotemporal"
    exportDirectoryForModel <- file.path(exportDirectory, "LOGEnvironmentalLOOCV")


    #Step 2: Create directory for random forest loocv (remove if there) 
    RemoveAndCreateDirectories(exportDirectoryForModel)

    #Step 3: Define the method to run the parent directory 
    RunAlgorithm(dataParentDirectory, exportDirectoryForModel)

    print("Finished algorithm script")

}
Main()