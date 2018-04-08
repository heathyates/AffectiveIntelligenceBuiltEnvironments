#################################################################################################
#Author: Heath Yates                                                                            #
#Date: 03/14/2018                                                                               # 
#Description: This conducts two fold cross validation into test and validation sets             #
#################################################################################################
library(openxlsx)
library(plyr)


#Section 1: Primary Function 
CreateTWOFCVTrainingAndTestingData <- function(masterData, TWOFCVTrainingPath, TWOFCVTestPath) {
    #Create two fold cross validation 
    #
    #Args: 
    #   masterData: The normalized or aggregated master data 
    #
    #Returns:
    #   None (but does write/output training and testing data sets) 

    #Step 0: Obtain the number of unique participants in the data 
    participantsInData <- unique(masterData$PARTICIPANTID)
    validationLength <- length(participantsInData)/2

    #Step 1: 
    #For example, train1 is all participants except participant 1 and 2 , and test1 is just participant 1 and 2 
    #For another, train2 is all participants except participant 5 and 6, and test2 is just participant 5 and 6
    for (i in 1:validationLength) {

        print(paste0("Working on TWOFCV training and testing fold: ", i))

        #Step 1.1: Obtain the training data set 
        train <- masterData[masterData$PARTICIPANTID != i & masterData$PARTICIPANTID != (i+1),]

        #Step 1.2 Obtain the testing data set 
        test <- masterData[masterData$PARTICIPANTID == i | masterData$PARTICIPANTID == (i + 1),]

        #Step 1.3 Output training sets 
        trainFileName <- paste0("train_", i, ".csv")
        write.csv(train, file.path(TWOFCVTrainingPath, trainFileName))

        #Step 1.2: Output the test sets 
        testFileName <- paste0("test_", i, ".csv")
        write.csv(test, file.path(TWOFCVTestPath, testFileName))

        print(paste0("Files output for TWOFCV training and testing fold: ", i))

    }

    print(paste0("TWOFCV complete"))

}


#Section 2: Main function
TWOFCV <- function() {
    #Does leave one out on the training data given to it
    #
    #Args: 
    #   masterDataFilePath: Path to the file to read 
    #   exportDirectory: Path to file to export 
    #
    #Returns:
    #   None


    masterDataFilePath <- "C:/Users/heath/OneDrive/Documents/Research/Current/Dissertation/3. WAM/10. Appendix - Normalized Cleaned Data/normalized master data/all_participants_master_with_normalized.csv"
    exportDirectory <- "C:/Users/heath/OneDrive/Documents/Research/Current/Dissertation/3. WAM/12. Appendix - ST Training and Test Data"


    #Step 0: Define the parent, training, and testing paths 
    TWOFCVParentPath <- file.path(exportDirectory, "TWOFCV")
    TWOFCVTrainingPath <- file.path(TWOFCVParentPath, "Train")
    TWOFCVTestPath <- file.path(TWOFCVParentPath, "Test")

    #Step 1: Create the paths 
    dir.create(TWOFCVParentPath)
    dir.create(TWOFCVTrainingPath)
    dir.create(TWOFCVTestPath)

    #Step 2: Load the master data 
    masterData <- read.csv(masterDataFilePath, header = TRUE)
    masterData <- arrange(masterData, masterData$PARTICIPANTID, masterData$TIME)
    masterData <- masterData[masterData$PARTICIPANTID != 19,]

    #Step 3: Create training and testing data for TWOFCV
    CreateTWOFCVTrainingAndTestingData(masterData, TWOFCVTrainingPath, TWOFCVTestPath)


}
TWOFCV()