#################################################################################################
#Author: Heath Yates                                                                            #
#Date: 03/14/2018                                                                               # 
#Description: This conducts three fold cross validation into test and validation sets           #
#################################################################################################
library(openxlsx)
library(plyr)

#Section 1: Primary Function 
CreateTHREEFCVTrainingAndTestingData <- function(masterData, THREEFCVTrainingPath, THREEFCVTestPath) {
    #Create two fold cross validation 
    #
    #Args: 
    #   masterData: The normalized or aggregated master data 
    #
    #Returns:
    #   None (but does write/output training and testing data sets) 

    #Step 0: Obtain the number of unique participants in the data 
    participantsInData <- unique(masterData$PARTICIPANTID)
    validationLength <- length(participantsInData) / 3

    #Step 1: 
    #For example, train1 is all participants except participant 1,2 and 3. test1 is just participant 1,2, and 3. 
    #For another, train2 is all participants except participant 5,6, and 7. test2 is just participant 5,6, and 7  
    for (i in 1:validationLength) {

        print(paste0("Working on THREEFCV training and testing fold: ", i))

        #Step 1.1: Obtain the training data set 
        train <- masterData[masterData$PARTICIPANTID != i & masterData$PARTICIPANTID != (i + 1) & masterData$PARTICIPANTID != (i + 2),]

        #Step 1.2 Obtain the testing data set 
        test <- masterData[masterData$PARTICIPANTID == i | masterData$PARTICIPANTID == (i + 1) | masterData$PARTICIPANTID == (i + 2),]

        #Step 1.3 Output training sets 
        trainFileName <- paste0("train_", i, ".csv")
        write.csv(train, file.path(THREEFCVTrainingPath, trainFileName))

        #Step 1.2: Output the test sets 
        testFileName <- paste0("test_", i, ".csv")
        write.csv(test, file.path(THREEFCVTestPath, testFileName))

        print(paste0("Files output for THREEFCV training and testing fold: ", i))

    }

    print(paste0("THREEFCV complete"))

}


#Section 2: Main function
THREEFCV <- function() {
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
    THREEFCVParentPath <- file.path(exportDirectory, "THREEFCV")
    THREEFCVTrainingPath <- file.path(THREEFCVParentPath, "Train")
    THREEFCVTestPath <- file.path(THREEFCVParentPath, "Test")

    #Step 1: Create the paths 
    dir.create(THREEFCVParentPath)
    dir.create(THREEFCVTrainingPath)
    dir.create(THREEFCVTestPath)

    #Step 2: Load the master data 
    masterData <- read.csv(masterDataFilePath, header = TRUE)
    masterData <- read.csv(masterDataFilePath, header = TRUE)
    masterData <- arrange(masterData, masterData$PARTICIPANTID, masterData$TIME)
    masterData <- masterData[masterData$PARTICIPANTID != 19,]

    #Step 3: Create training and testing data for THREEFCV
    CreateTHREEFCVTrainingAndTestingData(masterData, THREEFCVTrainingPath, THREEFCVTestPath)


}
THREEFCV()