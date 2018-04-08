#################################################################################################
#Author: Heath Yates                                                                            #
#Date: 01/20/2018                                                                               #
#Description: Accepts individual argument to set current working directory. Once in the working #
#directory, it will iterate through each participant file. It will clean and process each indiv-#
#ual empatica and polar data, merging them into a single master file for that participant. In   #
#addition to this, it will also create an experimental master file will all the data appended.  #
#################################################################################################


#Section 0: Library dependencies and global variables
#library(xlsx) #rJava is a dependency which requires JAVA_HOME a 64 bit 
library(openxlsx)
library(lubridate)
library(plyr)

masterSubDirectoryFolderName <- "processed participants data"
qualtricsSubDirectoryFolderName <- "processed qualtrics data"
polarParentDirectory <- "raw joined data/participants"
empaticaParentDirectory <- "raw joined data/participants"
 
participants <- ""


#Section 1: Helper Functions 
RawExcelTimeToYMDHMS <- function(rawQualtricsDataNonNulls) {
    #Process dates in the raw qualtrics data properly  
    #
    #Args: 
    #   rawQualtricsData:   Location of the raw qualtrics data file 
    #
    #Returns:
    #   rawQualtricsProcessedDateDate: Processed and qualtrics data 

    #Step 0: Process the start date 
    processedStartDate <- as.POSIXct(rawQualtricsDataNonNulls$STARTDATE * 24 * 3600 + as.POSIXct("1899-12-30 00:00")) #https://theweiluo.wordpress.com/2011/08/30/convert-date-time-in-excel-to-posixct/
    rawQualtricsDataNonNulls["STARTDATE"] <- processedStartDate

    #Step 1: Process the end date 
    processedEndDate <- as.POSIXct(rawQualtricsDataNonNulls$ENDDATE * 24 * 3600 + as.POSIXct("1899-12-30 00:00")) #https://theweiluo.wordpress.com/2011/08/30/convert-date-time-in-excel-to-posixct/
    rawQualtricsDataNonNulls["ENDDATE"] <- processedEndDate

    #Step 2: Return the processed data set with the proper dates 
    rawQualtricsProcessedDateData <- rawQualtricsDataNonNulls
    return(rawQualtricsDataNonNulls)

}



RemoveAllFilesOrCreateNewDirectory <- function(exportDirectory) {
    #Invokes the scripts that cleans the directory with the processed raw data 
    #
    #Args: 
    #  exportDirectory: Path to the export directory for processed data  
    #
    #Returns:
    #  None 

    #Step 0: Define desired subdirectory name and path 
    subDirectory <- masterSubDirectoryFolderName
    directoryPath <- file.path(exportDirectory, subDirectory)

    

    #Step 1: Check if path exists, and if it does, delete all files in the directory, otherwise, create it 
    if (dir.exists(directoryPath)) {

        #Remove all the files in the directory 
        #https://stackoverflow.com/questions/9296377/automatically-delete-files-folders-in-r
        #do.call(file.remove, list(list.files(path = directoryPath, full.names = TRUE)))
        unlink(directoryPath, recursive = TRUE) # https://stackoverflow.com/questions/28097035/how-to-remove-a-directory-in-r
        dir.create(directoryPath)


    } else {

        dir.create(directoryPath)

    }

}






#Section 2: Primary core functions for the preprocessing of raw qualtrics data 
LoadProcessAndCleanQualtricsData <- function(rawQualtricsDataFilePath) {
    #Invokes the scripts that clean and process the data from Parker's built environment experiment 
    #
    #Args: 
    #   rawQualtricsData:   Location of the raw qualtrics data file 
    #
    #Returns:
    #   processedAndCleanedQualtricsData: Processed and qualtrics data 

    #Step 0: Load the raw qualtrics data
    rawQualtricsData <- read.xlsx(rawQualtricsDataFilePath, sheet = 1, colNames = TRUE, detectDates = TRUE)

    #Step 1: Remove age and race since not enough data to use
    colnames(rawQualtricsData)
    #rawQualtricsDataWithoutAgeAndRace <- subset(rawQualtricsData, select = c("PARTICIPANTID", "ORIGINALPARTICIPANTIDNUM", "STARTDATE", "ENDDATE", "GENDER", "BODYSHAPE", "Q33", "Q34", "Q36", "Q37", "Q38", "Q39", "Q40", "Q41", "Q42", "Q43", "Q44", "Q45", "Q46", "Q47", "Q48", "Q49"))

    #Step 2: The following qualtric zones need to be selected and renamed 
    #Q33 -> Z1
    #Q34 -> Z2
    #Q41 -> Z3
    #Q36 -> Z4
    #Q37 -> Z5
    #Q38 -> Z6
    #Q39 -> Z7
    #Q40 -> Z8
    #Q42 -> Z9
    #Q43 -> Z10
    #Q44 -> Z11
    #Q45 -> Z12
    rawQualtricsDataWithoutAgeAndRace <- subset(rawQualtricsData, select = c("PARTICIPANTID", "ORIGINALPARTICIPANTIDNUM", "STARTDATE", "ENDDATE", "GENDER", "BODYSHAPE", "URBANORIGIN", "URBANPREFERENCE", "STUDYAREAFAMILIARITY", "EXERCISE", "EXERCISEDPASTTHREEHOURS",
                                         "Q33", "Q34", "Q36", "Q37", "Q38", "Q39", "Q40", "Q41", "Q42", "Q43", "Q44", "Q45"))

    colnames(rawQualtricsDataWithoutAgeAndRace)  <- c("PARTICIPANTID", "ORIGINALPARTICIPANTIDNUM", "STARTDATE", "ENDDATE", "GENDER", "BODYSHAPE", "URBANORIGIN", "URBANPREFERENCE", "STUDYAREAFAMILIARITY", "EXERCISE", "EXERCISEDPASTTHREEHOURS", 
                                                    "Z1", "Z2", "Z4", "Z5", "Z6", "Z7", "Z8", "Z3", "Z9", "Z10", "Z11", "Z12")

    #Step 2: Remove rows will NA values 
    rawQualtricsDataNonNull <- na.omit(rawQualtricsDataWithoutAgeAndRace)

    #Step 3: Process dates correctly 
    rawQualtricsDataProperDates <- RawExcelTimeToYMDHMS(rawQualtricsDataNonNull)

    #Step 4: Count the participants in the file and zones 
    participantsID <- seq(1:length(rawQualtricsDataProperDates$PARTICIPANTID))
    rawQualtricsDataProperDates["PARTICIPANTID"] <- participantsID

    #Step 5: Eumerate the participantes 1 through n 
    #regPattern <- "([Q][0-9]*)"
    #rawQualtricsColnames <- colnames(rawQualtricsDataProperDates)
    #regExMatch <- regexpr(pattern = regPattern, text = rawQualtricsColnames)
    #rawQualtricsZones <- regmatches(x = rawQualtricsColnames, regExMatch)
    #zoneID <- paste0("Z", rep(1:length(rawQualtricsZones)))
    #practiceZone <- rawQualtricsDataProperDates
    #colnames(rawQualtricsDataProperDates)[7:(7 + length(rawQualtricsZones) - 1)] <- zoneID

    #Step 6: Rename the columns with appropriate zone names 
    processedAndCleanedQualtricsData <- rawQualtricsDataProperDates
    return(processedAndCleanedQualtricsData)

}


CreateDirectoriesForParticipants <- function(processedAndCleanedQualtricsData, exportDirectory) {
    #Invokes the scripts that clean and process the data from Parker's built environment experiment 
    #
    #Args: 
    #   workingDirectory: directory to the raw data for each participant  
    #
    #Returns:
    #     None: It creates participant subdirectories each with empatica and polar folders 
    #processedAndCleanedQualtricsData <- processedAndCleanedQualtricsData

    #Loop through each participant row
    for (i in 1:nrow(processedAndCleanedQualtricsData)) {

        #Step 0: Create a subdirectory name and path to particpant 
        participantID <- processedAndCleanedQualtricsData$PARTICIPANTID[i]
        subDirectory <- paste0("participant_", participantID)
        pathToParticipantFolder <- file.path(file.path(exportDirectory, masterSubDirectoryFolderName), subDirectory)

        #Step 1: Create a folder name based on participant 
        dir.create(pathToParticipantFolder)

        #Step 2: Create subdirectory for empatica and polar 
        dir.create(file.path(pathToParticipantFolder, "empatica"))
        dir.create(file.path(pathToParticipantFolder, "polar"))
        
    }


}

CopyAndMoveEmpaticaFiles <- function(sourceDirectory, exportDirectory) {
    #This script finds the raw empatica files, copies them, and then moves them to a new directory 
    #
    #Args: 
    #   exportDirectory: directory to the raw data for each participant  
    #
    #Returns:
    #   None 

    #Step 0: 
    empaticaFileDirectory <- file.path(sourceDirectory, empaticaParentDirectory)

    #Step 1: List all files in the subdirectory
    #filesInDirectory <- list.files(path = empaticaFileDirectory, full.names = TRUE)
    #participantsInDirectory <- filesInDirectory[2:length(filesInDirectory)]
    participantsInDirectory <- list.files(path = empaticaFileDirectory, full.names = TRUE)

    #Step 2: Loop through participants in the directory 
    for (i in 1:length(participantsInDirectory)) {
    
        #Step 2.1: Find participant empatica file directory and define the new path 
        oldEmpaticaFilePath <- file.path(participantsInDirectory[i], "empatica")
        subDirectory <- paste0("participant_", i)
        newEmpaticaFilePath <- file.path(file.path(file.path(exportDirectory, masterSubDirectoryFolderName), subDirectory), "empatica")

        #Step 2.2: Find the files in empatica file directory and copy them 
        empaticaFiles <- list.files(oldEmpaticaFilePath)

        #Step 2.3: Loop through these files and copy them 
        for (j in 1:length(empaticaFiles)) {

            #Step 2.3.1: Copy the old file to the new folder 
            file.copy(file.path(oldEmpaticaFilePath, empaticaFiles[j]), newEmpaticaFilePath)

        }

    }

}



ProcessPolarFilesAndMoveToNewDirectory <- function(sourceDirectory, exportDirectory) {
    #This script finds the raw polar files, refactors them, and then moves them to a new directory 
    #
    #Args: 
    #   exportDirectory: directory to the raw data for each participant  
    #
    #Returns:
    #   None 

    #Step 0: Define the parent directory for the participant with polar data 
    polarFileDirectory <- file.path(sourceDirectory, polarParentDirectory)


    #Step 1: List the participants in the directory 
    participantsInDirectory <- list.files(path = polarFileDirectory, full.names = TRUE)

    #Step 2: Loop through each participant sub folders and process polar file
    for (i in 1:length(participantsInDirectory)) {


        #Step 2.1: Find participant empatica file directory and define the new path 
        oldPolarFilePath <- file.path(participantsInDirectory[i], "polar")
        subDirectory <- paste0("participant_", i)
        newPolarFilePath <- file.path(file.path(file.path(exportDirectory, masterSubDirectoryFolderName), subDirectory), "polar")


        #Step 2.2: Open the excel file 
        polarExcelFile <- list.files(oldPolarFilePath)
        #polarParticipatnData <- read.xlsx(file.path(oldPolarFilePath,polarExcelFile), sheet = 1, colNames = TRUE)
        polarParticipantData <- read.csv(file.path(oldPolarFilePath, polarExcelFile), header = TRUE)


        #Step 2.2: Fix the dates data in the polar data 



        #Step 2.3: Fix the participant name 
        polarParticipantData["PARTICIPANTID"] <- i

        #Step 2.4: Rename the zones in the column appropriately 
        #c("Q33" = "Z1", "Q34" = "Z2", "Q36" = "Z3", "Q37" = "Z4", "Q38" = "Z5", "Q39" = "Z6", "Q40" = "Z7", "Q41" = "Z8", "Q42" = "Z9", "Q43" = "Z10", "Q44" = "Z11", "Q45" = "Z12")
        newZoneNames <- c("Q33" = "Z1", "Q34" = "Z2", "Q41" = "Z3", "Q36" = "Z4", "Q37" = "Z5", "Q38" = "Z6", "Q39" = "Z7", "Q40" = "Z8", "Q42" = "Z9", "Q43" = "Z10", "Q44" = "Z11", "Q45" = "Z12")
        polarParticipantData["QuestionNu"] <- revalue(polarParticipantData$QuestionNu, newZoneNames)

        #Step 2.5: Ouput to new location
        write.csv(polarParticipantData, file.path(newPolarFilePath, paste0(i, ".csv")))
    }



}


StoreProcessedRawQualtricsData <- function(processedRawQualtricsData, exportDirectory) {
    #Invokes the scripts that clean and process the data from Parker's built environment experiment 
    #
    #Args: 
    #   processedRawQualtricsData: directory to the raw data for each participant  
    #
    #Returns:
    #   None

    #Step 0: Check if directory exists, if so delete it and contents, otherwise create new directory
    directoryPath <- file.path(exportDirectory, qualtricsSubDirectoryFolderName)

    #Step 1: Check if path exists, and if it does, delete all files in the directory, otherwise, create it 
    if (dir.exists(directoryPath)) {

        #Remove all the files in the directory 
        #https://stackoverflow.com/questions/9296377/automatically-delete-files-folders-in-r
        #do.call(file.remove, list(list.files(path = directoryPath, full.names = TRUE)))
        unlink(directoryPath, recursive = TRUE) # https://stackoverflow.com/questions/28097035/how-to-remove-a-directory-in-r
        dir.create(directoryPath)


    } else {

        dir.create(directoryPath)

    }

    #Step 2: Export the processed raw qualtrics data 
    write.xlsx(processedRawQualtricsData, file.path(directoryPath, "processedqualtricsdata.xlsx"), colnames = TRUE) 

}


Main <- function(rawQualtricsDataFileDirectory, sourceDirectory, exportDirectory) {
    #Invokes the scripts that clean and process the data from Parker's built environment experiment 
    #
    #Args: 
    #   workingDirectory: directory to the raw data for each participant  
    #
    #Returns:
    #   participant_xx_master.xlsx : Cleaned, processed, and fused empatica and polar data by participant 
    #   master.xlsx : Cleaned, processed, and fused empatica and polar data for entire experiment 

    #Invokes the scripts that clean and process the data from Parker's built environment experiment 
    #
    #Args: 
    #   rawQualtricsDataFileDirectory:   Location of the raw qualtrics data file 
    #
    #Returns:
    #   participant_xx_master.xlsx : Cleaned, processed, and fused empatica and polar data by participant 
    #   master.xlsx : Cleaned, processed, and fused empatica and polar data for entire experiment 


    rawQualtricsDataFileDirectory <- "C:/Users/heath/OneDrive/Documents/Research/Current/Dissertation/3. WAM/6. Appendix - Raw Data/raw joined data/qualtric data/completeparticipantsqualtricsdata.xlsx"
    sourceDirectory <- "C:/Users/heath/OneDrive/Documents/Research/Current/Dissertation/3. WAM/6. Appendix - Raw Data"
    exportDirectory <- "C:/Users/heath/OneDrive/Documents/Research/Current/Dissertation/3. WAM/7. Appendix - Processed Data"

    #Step 0: Make sure the raw file directory, if exists, is completely empty 
    RemoveAllFilesOrCreateNewDirectory(exportDirectory)

    #Step 1: Load, process and clean master file 
    processedAndCleanedQualtricsData <- LoadProcessAndCleanQualtricsData(rawQualtricsDataFileDirectory)

    #Step 2: Create a new directory for raw data, with subdirectories for each participant (including subdirectores for empatica and polar) 
    CreateDirectoriesForParticipants(processedAndCleanedQualtricsData, exportDirectory)


    #Step 3: Process empatica data per participant and store it
    CopyAndMoveEmpaticaFiles(sourceDirectory, exportDirectory)

    #Step 4: Process polar data by participant and store
    ProcessPolarFilesAndMoveToNewDirectory(sourceDirectory, exportDirectory)

    #Step 5: Store process raw data 
    StoreProcessedRawQualtricsData(processedAndCleanedQualtricsData, exportDirectory)
}

args <- commandArgs(TRUE)
Main(args)