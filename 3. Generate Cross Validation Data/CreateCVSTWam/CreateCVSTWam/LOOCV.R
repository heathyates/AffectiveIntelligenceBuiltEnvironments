#################################################################################################
#Author: Heath Yates                                                                            #
#Date: 03/14/2018                                                                               # 
#Description: This conducts leave one out cross validation into test and validation sets        # 
#################################################################################################
library(openxlsx)
library(plyr)

#Section 1: Primary Functions 
CreateLOOCVTrainingAndTestingData <- function(masterData, LOOCVTrainingPath, LOOCVTestPath) {
    #Create LOOCV training and testing data sets
    #
    #Args: 
    #   masterData: The normalized or aggregated master data 
    #
    #Returns:
    #   None (but does write/output training and testing data sets) 

    #Step 0: Obtain the number of unique participants in the data 
    participantsInData <- unique(masterData$PARTICIPANTID)

    #Step 1: Start creating the folds by leaving out on participant at a time 
    #For example, train1 is all participants except participant 1, and test1 is just participant 1
    #For another, train2 is all participants except participant 2, and test2 is just participant 2 
    for (i in 1:length(participantsInData)) {

        print(paste0("Working on LOOCV training and testing fold: ", i))

        #Step 1.1: Obtain the training data set 
        train <- masterData[masterData$PARTICIPANTID != i,]

        #Step 1.2 Obtain the testing data set 
        test <- masterData[masterData$PARTICIPANTID == i,]

        #Step 1.3 Output training sets 
        trainFileName <- paste0("train_", i,".csv")
        write.csv(train, file.path(LOOCVTrainingPath, trainFileName))

        #Step 1.2: Output the test sets 
        testFileName <- paste0("test_", i,".csv")
        write.csv(test, file.path(LOOCVTestPath, testFileName))

        print(paste0("Files output for LOOCV training and testing fold: ", i))

    }

    print(paste0("LOOCV complete"))


}

#Section 2: Main function 
LOOCV <- function() {
    #Does leave one out on the training data given to it
    #
    #Args: 
    #   masterDataFilePath: Path to the file to read 
    #   
    #
    #Returns:
    #   None

    masterDataFilePath <- "C:/Users/heath/OneDrive/Documents/Research/Current/Dissertation/3. WAM/10. Appendix - Normalized Cleaned Data/normalized master data/all_participants_master_with_normalized.csv"
    exportDirectory <- "C:/Users/heath/OneDrive/Documents/Research/Current/Dissertation/3. WAM/12. Appendix - ST Training and Test Data"

    #Step 0: Define the parent, training, and testing paths 
    LOOCVParentPath <- file.path(exportDirectory, "LOOCV")
    LOOCVTrainingPath <- file.path(LOOCVParentPath, "Train")
    LOOCVTestPath <- file.path(LOOCVParentPath, "Test")

    #Step 1: Create the paths 
    dir.create(LOOCVParentPath)
    dir.create(LOOCVTrainingPath)
    dir.create(LOOCVTestPath)

    #Step 2: Load the master data 
    #Step 2: Load the master data 
    masterData <- read.csv(masterDataFilePath, header = TRUE)
    masterData <- arrange(masterData, masterData$PARTICIPANTID, masterData$TIME)
    masterData <- masterData[masterData$PARTICIPANTID != 19,]

    #Step 3: Create training and testing data for LOOCV 
    CreateLOOCVTrainingAndTestingData(masterData, LOOCVTrainingPath, LOOCVTestPath)

    
}

LOOCV()