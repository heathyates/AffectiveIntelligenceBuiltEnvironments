#################################################################################################
#Author: Heath Yates                                                                            #
#Date: 01/20/2018                                                                               #
#Description: Accepts individual argument to set current working directory. Once in the working #
#directory, it will iterate through each participant file. It will clean and process each indiv-#
#ual empatica and polar data, merging them into a single master file for that participant. In   #
#addition to this, it will also create an experimental master file will all the data appended.  #
#Note that 7 is very safe and 1 is very unsafe on likert scale 
#################################################################################################

library(stringr)
library(openxlsx)


#Section 1: Helper functions 
LoadHelperRFunctions <- function(sourceDirectory) {
    #Invokes the scripts that clean and process the data from Parker's built environment experiment 
    #
    #Args: 
    #  sourceDirectory: Directory source where the R helper functions are   
    #
    #Returns:
    #  None

    #Check to make sure the source files exist and if not, load them 
    if (!exists("CleanAndProcessPolarData", mode = "function") | !exists("CleanAndProcessEmpaticaData", mode = "function")) {


        empaticaScriptPath <- file.path(sourceDirectory, "CleanAndProcessEmpaticaDataRefactored.R")
        #empaticaScriptPath <- file.path(sourceDirectory, "CleanAndProcessEmpaticaData.R")
        #polarScriptPath <- file.path(sourceDirectory, "CleanAndProcessPolarData.R")
        polarScriptPath <- file.path(sourceDirectory, "CleanAndProcessPolarDataRefactored.R")
        print(empaticaScriptPath)

        #source(empaticaScriptPath)
        source(empaticaScriptPath)
        source(polarScriptPath)

    } else {
        print("Source files already loaded")
    }




}


LoadDemographicAndAnnotationData <- function(qualtricsDirectory) {
    #Function loads demographic and annotation data from the qualtrics survey  
    #
    #Args: 
    #  exportDirectory: Path to the export directory for processed data  
    #
    #Returns:
    #  None

    #Step 0: Load the qualtrics data 
    qualtricsFile <- list.files(qualtricsDirectory)
    qualtricsData <- read.xlsx(file.path(qualtricsDirectory, qualtricsFile), sheet = 1, startRow = 1, colNames = TRUE)

    #Step 1: Provide a subset of relevant demographics and zone data 
    qualtricsDataSubset <- subset(qualtricsData, select = c("PARTICIPANTID", "GENDER", "BODYSHAPE", "URBANORIGIN", "URBANPREFERENCE", "STUDYAREAFAMILIARITY", "EXERCISE", "EXERCISEDPASTTHREEHOURS",
                                                    "Z1", "Z2", "Z4", "Z5", "Z6", "Z7", "Z8", "Z3", "Z9", "Z10", "Z11", "Z12"))
    #Step 2: Return the qualtrics data 
    return(qualtricsDataSubset)
}



RemoveAllFilesOrCreateNewDirectory <- function(exportDirectory) {
    #Invokes the scripts that clean and process the data from Parker's built environment experiment 
    #
    #Args: 
    #  exportDirectory: Path to the export directory for processed data  
    #
    #Returns:
    #  None

    subDirectory <- "master csv"
    subDirectoryWithAnnotatedData <- "master annotated csv"
    directoryPath <- file.path(exportDirectory, subDirectory)
    directoryPathWithAnnotation <- file.path(exportDirectory, subDirectoryWithAnnotatedData)

    if (dir.exists(directoryPath) & dir.exists(directoryPathWithAnnotation)) {

        #Remove all the files in the directory 
        #https://stackoverflow.com/questions/9296377/automatically-delete-files-folders-in-r
        do.call(file.remove, list(list.files(path = directoryPath, full.names = TRUE)))
        do.call(file.remove, list(list.files(path = directoryPathWithAnnotation, full.names = TRUE)))
    }
    else {

        dir.create(file.path(exportDirectory, subDirectory))
        dir.create(file.path(exportDirectory, subDirectoryWithAnnotatedData))

    }

}

labelAnnotation <- function(x) {
    #Reads zone and annotates with proper participant Demographi  
    #
    #Args: 
    #  mergedData: The merged data of empatica and polar data by participant
    #  participantDemographicAndAnnotationData: The demographic and annotation data of participant
    #
    #Returns:
    #  annotatedMergedData: The merged data of empatica and polar data by participant with annotation 

}

AnnotateData <- function(mergedData, participantDemographicAnnotationData) {
    #Reads the zone data and creates a new column of data with annotated responses  
    #
    #Args: 
    #  mergedData: The merged data of empatica and polar data by participant
    #  participantDemographicAndAnnotationData: The demographic and annotation data of participant
    #
    #Returns:
    #  annotatedMergedData: The merged data of empatica and polar data by participant with annotation  

    #Step 0: cast zone as character vector 
    zone <- as.character(mergedData$ZONE)

    #Step 1: Create an annotation variable based on zones  
    annotation <- lapply(zone, function(x)
        if (x == "Z1") {
            participantDemographicAnnotationData$Z1
        } else if (x == "Z2") {
            participantDemographicAnnotationData$Z2
        } else if (x == "Z3") {
            participantDemographicAnnotationData$Z3
        } else if (x == "Z4") {
            participantDemographicAnnotationData$Z4
        } else if (x == "Z5") {
            participantDemographicAnnotationData$Z5
        } else if (x == "Z6") {
            participantDemographicAnnotationData$Z6
        } else if (x == "Z7") {
            participantDemographicAnnotationData$Z7
        } else if (x == "Z8") {
            participantDemographicAnnotationData$Z8
        } else if (x == "Z9") {
            participantDemographicAnnotationData$Z9
        } else if (x == "Z10") {
            participantDemographicAnnotationData$Z10
        } else if (x == "Z11") {
            participantDemographicAnnotationData$Z11
        } else if (x == "Z12") {
            participantDemographicAnnotationData$Z12
        } else {
            NA
        }
    )

    #Step 1: Add new annotation data to merged data 
    numericAnnotation <- as.numeric(unlist(annotation))
    mergedData["ANNOTATION"] <- numericAnnotation


    #Step 2: Add new column of binary annotation data to merged data 
    binaryAnnotation <- lapply(numericAnnotation, function(x)
        if (!is.na(x)) {
            if (x < 5) {
                1 #Feels unsafe, aka 4 or below is unsafe 
            } else {
                0 #Feels safe 
            }
        } else {
            NA
        }
    )
    numericBinaryAnnotation <- as.numeric(unlist(binaryAnnotation))
    mergedData["BINARYANNOTATION"] <- numericBinaryAnnotation

    #Step 2: Return the data with the numerical annotation 
    annotatedMergedData <- mergedData
    return(mergedData)
}


#Section 2: Primary Functions 
CleanAndProcessAllParticipantData <- function(exportDirectory, demographicAndAnnotationData) {
    #Invokes the scripts that clean and process the data from Parker's built environment experiment 
    #
    #Args: 
    #   workingDirectory: directory to the raw data for each participant  
    #
    #Returns:
    #   participant_xx_master.xlsx : Cleaned, processed, and fused empatica and polar data by participant 
    #   master.xlsx : Cleaned, processed, and fused empatica and polar data for entire experiment 


    #Step 0: Check if the export directory is created or not. 
    RemoveAllFilesOrCreateNewDirectory(exportDirectory = exportDirectory)

    #Step 1: List all folders in current directory
    # In order for this script to work, the directory format must be as follows: 
    # participant_01
    #   empatica 
    #   polar 
    #
    # particpant_02
    #   empatica 
    #   polar 
    #
    # ...
    filesInDirectory <- list.files()


    #Step 2 : Define empty dataframe with column names 

    #Step 2.0a: Define for master columns without annotation information 
    allParticipantsMasterColumns <- c("TIME", "BVPMIN", "BVPMAX", "BVPAVG", "EDAMIN", "EDAMAX", "EDAAVG", "HRMIN", "HRMAX", "HRAVG", "TEMPMIN", "TEMPMAX", "TEMPAVG",
      "LAT", "LON", "ELEVATION", "PIDSEC", "WALKABILITY", "NUMLIGHTS", "NUMTREES", "NUMINFLINE", "NUMINFPOIN", "NUMGRASS", "NUMSCRUB",
      "ZONE", "PARTICIPANTID")
    allParticipantsMaster <- data.frame(x = matrix("", nrow = 0, ncol = length(allParticipantsMasterColumns)))
    colnames(allParticipantsMaster) <- list(allParticipantsMasterColumns)

   
    #Step 2.0b: Define for master columns with annotation information 
    allParticipantsWithAnnotationMasterColumns <- c("TIME", "BVPMIN", "BVPMAX", "BVPAVG", "EDAMIN", "EDAMAX", "EDAAVG", "HRMIN", "HRMAX", "HRAVG", "TEMPMIN", "TEMPMAX", "TEMPAVG",
      "LAT", "LON", "ELEVATION", "PIDSEC", "WALKABILITY", "NUMLIGHTS", "NUMTREES", "NUMINFLINE", "NUMINFPOIN", "NUMGRASS", "NUMSCRUB",
      "ZONE", "PARTICIPANTID", "GENDER", "BODYSHAPE", "URBANORIGIN", "URBANPREFERENCE", "STUDYAREAFAMILIARITY", "EXERCISE", "EXERCISEDPASTTHREEHOURS",
      "ANNOTATION", "BINARYANNOTATION")
    allParticipantsWithAnnotationMaster <- data.frame(x = matrix("", nrow = 0, ncol = length(allParticipantsWithAnnotationMasterColumns)))
    colnames(allParticipantsWithAnnotationMaster) <- list(allParticipantsWithAnnotationMasterColumns)



    #Step 3: Loop through each participant in the directory 
    for (i in 1:length(filesInDirectory)) {

        print(paste0("Processing data in folder: ", filesInDirectory[i]))

        #Step 3.0:  Get path to empatica and polar directory 
        participantDirectories <- list.dirs(filesInDirectory[i])
        participantEmpaticaDirectory <- participantDirectories[2]
        participantPolarDirectory <- participantDirectories[3]

        #Step 3.2:  Clean and process empatica raw data 
        empaticaPath <- file.path(getwd(), participantEmpaticaDirectory)
        cleanEmpaticaData <- CleanAndProcessEmpaticaData(empaticaPath)

        #Step 3.3: Clean and process polar raw data 
        polarPath <- file.path(getwd(), participantPolarDirectory)
        cleanPolarData <- CleanAndProcessPolarData(polarPath)
        
        #Step 3.4:  Merge processed and clean polar data, verify that Start and End still exists 
        mergedData <- merge(cleanEmpaticaData, cleanPolarData, by.x = "TIME", by.y = "TIME")

        startExists <- "Start" %in% unique(mergedData$ZONE)
        if (!startExists) {
            startRow <- as.numeric(rownames(head(mergedData, 1)))
            mergedData[startRow, ]$ZONE <- "Start"
        }

        endExists <- "End" %in% unique(mergedData$ZONE)
        if (!endExists) {
            endRow <- as.numeric(rownames(head(mergedData, 1)))
            mergedData[endRow, ]$ZONE <- "End"
        }


        #Step 3.5: Change time data to GMT to avoid excel bug when script outputs to excel 
        #https://codedump.io/share/uNsbh0PMmkp7/1/timestamp-changes-when-writing-a-r-dataframe-to-an-excel-file-depending-upon-utc-offset
        mergedData$TIME <- force_tz(mergedData$TIME, tzone = "GMT")
        
        #Step 3.6:  Add participant to the merged data (maybe merge this into the demographic data and have that in each folder) 
        #regPatternForParticipant <- "_[0-9]*"
        #regExMatch <- regexpr(pattern = regPatternForParticipant, text = filesInDirectory[i])
        #participantIdRaw<-regmatches(x = filesInDirectory[i], regExMatch)
        #participantId <- str_replace_all(participantIdRaw, "[[:punct:]]", "")
        #mergedData["PARTICIPANTID"] <- participantId

        #Step 3.6:  Add Demographic data to the merged data 
        #TO DO: TBD and maybe add tag data   

        #Step 3.7:  Save the data to export folder 
        participantId <- mergedData$PARTICIPANTID[1]
        print(paste0("Writing cleaned and processed data for participant ", participantId, " to file."))
        exportFileName <- paste0(filesInDirectory[i], "_master.csv")
        exportFilePath = paste0(exportDirectory, "/master csv/", exportFileName)
        #write.xlsx2(x = mergedData, file = exportFilePath, sheetName = "Master", col.names = TRUE)
        #write.xlsx(mergedData, exportFilePath, sheetName = "Master", col.names = TRUE)
        write.csv(mergedData, exportFilePath)

        #Step 3.8: Write this merge data to the participants master file 
        allParticipantsMaster <- rbind(allParticipantsMaster, mergedData)


        #Step 3.9: Obtain demographic and annotation data for participant  
        participantDemographicAnnotationData <- demographicAndAnnotationData[demographicAndAnnotationData$PARTICIPANTID == participantId,]

        #Step 3.10: Add demographic data 
        mergedData["GENDER"] <- participantDemographicAnnotationData$GENDER
        mergedData["BODYSHAPE"] <- participantDemographicAnnotationData$BODYSHAPE
        mergedData["URBANORIGIN"] <- participantDemographicAnnotationData$URBANORIGIN
        mergedData["URBANPREFERENCE"] <- participantDemographicAnnotationData$URBANPREFERENCE
        mergedData["STUDYAREAFAMILIARITY"] <- participantDemographicAnnotationData$STUDYAREAFAMILIARITY
        mergedData["EXERCISE"] <- participantDemographicAnnotationData$EXERCISE
        mergedData["EXERCISEDPASTTHREEHOURS"] <- participantDemographicAnnotationData$EXERCISEDPASTTHREEHOURS

        #Step 3.11: Add annotation data and binary annotation data 
        annotatedMergedData <- AnnotateData(mergedData, participantDemographicAnnotationData)

        #Step 3.12




        #Step 3.13: Save the annotated data to the export folder 
        print(paste0("Writing annotated, cleaned, and processed data for participant ", participantId, " to file."))
        annotatedExportFileName <- paste0(filesInDirectory[i], "_annotated_master.csv")
        annotatedExportFilePath <- paste0(exportDirectory, "/master annotated csv/", annotatedExportFileName)
        #write.xlsx(annotatedMergedData, annotatedExportFilePath, sheetName="Master", col.names = TRUE)
        write.csv(annotatedMergedData, annotatedExportFilePath)

        
        #Step 3.14:  If no master object, create one and save data. Otherwise, append data. Close the file. 
        allParticipantsWithAnnotationMaster <-rbind(allParticipantsWithAnnotationMaster, annotatedMergedData)


        #Step 3.15:  Clean memory before next loop 
        rawBVPData <- ""
        rawEDAData <- ""
        rawHRData <- ""
        rawPolarData <- ""
        rawTempData <- ""

        rm(list = c("participantDirectories", "participantEmpaticaDirectory", "participantPolarDirectory", "empaticaPath", "cleanEmpaticaData", "polarPath", "cleanPolarData", "mergedData", "exportFileName", "exportFilePath",
            "participantDemographicAnnotationData", "annotatedMergedData", "annotatedExportFileName", "annotatedExportFilePath"))


    }


    #Step 4: Output all participants files to a master file without annotation 
    masterExportFileName <- "all_participants_master.csv"
    masterExportFilePath = paste0(exportDirectory, "/master csv/", masterExportFileName)
    #write.xlsx(allParticipantsMaster, masterExportFilePath, sheetName = "Master", col.names = TRUE)
    write.csv(allParticipantsMaster, masterExportFilePath)

    #Step 5: Output all participants files to a master file with annotation 
    masterAnnotationExportFileName <- "all_participants_with_annotation_master.csv"
    masterAnnotationExportFilePath = paste0(exportDirectory, "/master annotated csv/", masterAnnotationExportFileName)
    #write.xlsx(allParticipantsWithAnnotationMaster, masterAnnotationExportFilePath, sheetName = "Master", col.names = TRUE)
    write.csv(allParticipantsMaster, masterAnnotationExportFilePath)




}



Main <- function(sourceDirectory, workingDirectory, qualtricsDirectory, exportDirectory) {
    #Invokes the scripts that clean and process the data from Parker's built environment experiment 
    #
    #Args: 
    #   workingDirectory: directory to the raw data for each participant  
    #
    #Returns:
    #   participant_xx_master.xlsx : Cleaned, processed, and fused empatica and polar data by participant 
    #   master.xlsx : Cleaned, processed, and fused empatica and polar data for entire experiment 

    print("Beginning cleaning and processing of raw participant empatica data and processed polar data.")

    #Step 0: Setup the current working directory 


    sourceDirectory <- "C:/Users/heath/OneDrive/Documents/Research/Current/Dissertation/3. WAM/1. Clean and Process Data/CleanAndProcessWam/CleanAndProcessWam"
    workingDirectory <- "C:/Users/heath/OneDrive/Documents/Research/Current/Dissertation/3. WAM/7. Appendix - Processed Data/processed participants data"

    qualtricsDirectory <- "C:/Users/heath/OneDrive/Documents/Research/Current/Dissertation/3. WAM/7. Appendix - Processed Data/processed qualtrics data"
    exportDirectory <- "C:/Users/heath/OneDrive/Documents/Research/Current/Dissertation/3. WAM/8. Appendix - Cleaned and Processed Data"

    setwd(workingDirectory)

    #Step 1: Load the functions required to clean and process data
    #See: https://stackoverflow.com/questions/6456501/how-to-include-source-r-script-in-other-scripts
    LoadHelperRFunctions(sourceDirectory = sourceDirectory)

    #Step 3: Load the qualtrics data 
    demographicAndAnnotationData <- LoadDemographicAndAnnotationData(qualtricsDirectory)
    
    #Step 2: Clean and process data for all participants 
    CleanAndProcessAllParticipantData(exportDirectory, demographicAndAnnotationData)


    print("Script has ended.")

}

args <- commandArgs(TRUE)
Main(args)