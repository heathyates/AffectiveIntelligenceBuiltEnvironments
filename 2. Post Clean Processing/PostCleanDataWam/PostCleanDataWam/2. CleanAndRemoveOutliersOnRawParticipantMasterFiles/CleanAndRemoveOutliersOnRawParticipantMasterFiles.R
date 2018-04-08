#################################################################################################
#Author: Heath Yates                                                                            #
#Date: 03/08/2018                                                                               #
#Description: Clean and remove outliers on raw participant master files                         #
#################################################################################################




#Section 1: Helper functions 
ClearAndCreateExportDirectory <- function(exportDirectory) {
    #This function examines if subdirectory paths exist in the export path, remove what is there and creates them
    #
    #Args: 
    #  exportDirectory 
    #
    #Returns:
    #  exportSubDirectory : Exports the path where the raw master participant data for plots should be sent :14


    #Step 0: Create the directory and export sub directory paths
    subDirectory <- "master annotated cleaned"
    directoryPath <- file.path(exportDirectory, subDirectory)

    #Step 1: If directory path exists, remove directory and all files in subdirectories 
    if (dir.exists(directoryPath)) {

        #Remove all the files in the directory 
        #https://stackoverflow.com/questions/22122968/how-to-remove-the-file-when-permisson-denied-in-r
        unlink(directoryPath, recursive = TRUE)

        #Then create the directories again 
        dir.create(directoryPath)
    }
    else {

        #Create the needed directories 
        dir.create(directoryPath)
    }


}

ClearAndCreateExportPlotDirectory <- function(plotExportDirectory) {
    #This function examines if subdirectory paths exist in the export path, remove what is there and creates them
    #
    #Args: 
    #   plotExportDirectory : Directory to output the cleaned plots 
    #
    #Returns:
    #   None


    #Step 0: Create the directory and export sub directory paths
    subDirectory <- "plots with cleaning"
    directoryPath <- file.path(plotExportDirectory, subDirectory)


    #Step 1: If directory path exists, remove directory and all files in subdirectories 
    if (dir.exists(directoryPath)) {

        #Remove all the files in the directory 
        #https://stackoverflow.com/questions/22122968/how-to-remove-the-file-when-permisson-denied-in-r
        unlink(directoryPath, recursive = TRUE)

        #Then create the directories again 
        dir.create(directoryPath)
    }
    else {

        #Create the needed directories 
        dir.create(directoryPath)
    }

}

LoadHelperRFunctions <- function(sourceDirectory) {
    #Invokes custom scripts required to clean the participants 
    #
    #Args: 
    #  sourceDirectory: Directory source where the R helper functions are   
    #
    #Returns:
    #  None

    #Check to make sure the source files exist and if not, load them 
    if (!exists("CleanAndRemoveOutlierOnParticipant", mode = "function")) {

        cleanParticipantScriptPath <- file.path(sourceDirectory, "CleanAndRemoveOutlierOnParticipant.R")
        source(cleanParticipantScriptPath)
    } else {
        print("Source files already loaded.")
    }


}


#Section 2: Primary Functions
CleanAllParticipants <- function(workingDirectory, exportDirectory, plotExportDirectory) {
    #Read all the files and either remove outliers or clean `
    #
    #Args: 
    #   workingDirectory: Location of the files to input
    #   exportDirectory: Location of 
    #Returns: 
    #   None 

    #Step 0: Find all the participants in the directory 
    participantsInDirectory <- list.files(workingDirectory)

    #Step 1: Remove the all_participants_with_annotation_master.csv from consideration 
    participantsFileNames <- participantsInDirectory[-1]

    #Step 2: Clean all the participants 
    # Inputs: hasOutliers, outlierCutOff, participantFilePath, exportDirectory, plotExportDirectory
    cleanedDataExportDirectoryPath <- file.path(exportDirectory, "master annotated cleaned")
    plotDataExportDirectoryPath <- file.path(plotExportDirectory, "plots with cleaning")

    #Step 3: Create Master Data format 
    cleanedMasterDataColumns <- c("TIME", "BVPMIN", "BVPMAX", "BVPAVG",
                                  "EDAMIN", "EDAMAX", "EDAAVG", "HRMIN",
                                  "HRMAX", "HRAVG", "TEMPMIN", "TEMPMAX",
                                  "TEMPAVG", "LAT", "LON", "ELEVATION",
                                  "PIDSEC", "WALKABILITY", "NUMLIGHTS", "NUMTREES",
                                  "NUMINFLINE", "NUMINFPOIN", "NUMGRASS", "NUMSCRUB",
                                  "ZONE", "PARTICIPANTID", "GENDER", "BODYSHAPE",
                                  "URBANORIGIN", "URBANPREFERENCE", "STUDYAREAFAMILIARITY", "EXERCISE",
                                  "EXERCISEDPASTTHREEHOURS", "ANNOTATION", "BINARYANNOTATION")
    allParticipantsCleanedMaster <- data.frame(x = matrix("", nrow = 0, ncol = length(cleanedMasterDataColumns)))
    colnames(allParticipantsCleanedMaster) <- list(cleanedMasterDataColumns)

    #Step 4: Process the data that has outliers 
    print(paste0("Cleaning data for file: ", participantsFileNames[1]))
    file.path(workingDirectory, participantsFileNames[1])
    participant1MasterDataCleaned <- CleanAndRemoveOutlierOnParticipant(TRUE, 1143, file.path(workingDirectory,participantsFileNames[1]), cleanedDataExportDirectoryPath, plotDataExportDirectoryPath)
    allParticipantsCleanedMaster <- rbind(allParticipantsCleanedMaster, participant1MasterDataCleaned)

    #Step 5: Process the data that does not have outliers 
    participantsWithoutOutliersFileNames <- participantsFileNames[-1]
    for (i in 1:length(participantsWithoutOutliersFileNames)) {

        print(paste0("Cleaning data for file: ", participantsWithoutOutliersFileNames[i]))
        #Step 5.1: Obtain the participantMasterDataCleaned 
        participantMasterDataCleaned <- CleanAndRemoveOutlierOnParticipant(FALSE, 0, file.path(workingDirectory, participantsWithoutOutliersFileNames[i]), cleanedDataExportDirectoryPath, plotDataExportDirectoryPath)

        #Step 5.2 Add it to the allParticipantsCleanedMaster 
        allParticipantsCleanedMaster <- rbind(allParticipantsCleanedMaster, participantMasterDataCleaned)

    }

    #Step 5: Output the master file in the export directory as well 
    masterExportFileName <- "all_participants_master_cleaned.csv"
    masterExportFilePath = file.path(cleanedDataExportDirectoryPath, masterExportFileName)
    write.csv(allParticipantsCleanedMaster, masterExportFilePath)

}

#Section 3: Main Function 
CleanAndRemoveOutliersOnRawParticipantMasterFiles <- function(workingDirectory, exportDirectory, sourceDirectory, plotExportDirectory) {
    #Main function for cleaning and removing outliers on raw participant files 
    #
    #Args: 
    #   workingDirectory: This is where to obtain the input files of the participants the script needs 
    #   exportDirectory: This is where to export the cleaned and processed data 
    #   sourceDirectory: This is where the R script can find the modules needed to make this script work 
    #   plotExportDirectory: This is where to export the finished plot 
    #
    #Returns: 
    #   None 

    print("Beginning Cleaning and Removing Outliers Script on Participant Master CSV Files")

    #Step 0: Define the directories needed to invoke this script 
    workingDirectory <- "C:/Users/heath/OneDrive/Documents/Research/Current/Dissertation/3. WAM/8. Appendix - Cleaned and Processed Data/master annotated csv"
    exportDirectory <- "C:/Users/heath/OneDrive/Documents/Research/Current/Dissertation/3. WAM/9. Appendix - Cleaned Data"
    sourceDirectory <- "C:/Users/heath/OneDrive/Documents/Research/Current/Dissertation/3. WAM/2. Post Clean Processing/PostCleanDataWam/PostCleanDataWam/2. CleanAndRemoveOutliersOnRawParticipantMasterFiles"
    plotExportDirectory <- "C:/Users/heath/OneDrive/Documents/Research/Current/Dissertation/3. WAM/5. Results/exploratory data analysis"

    #Step 1: Create the export directory for the cleaned data 
    ClearAndCreateExportDirectory(exportDirectory)
    ClearAndCreateExportPlotDirectory(plotExportDirectory)

    #Step 2: Load the helper function 
    LoadHelperRFunctions(sourceDirectory)

    #Step 3: Clean the participant data 
    CleanAllParticipants(workingDirectory, exportDirectory, plotExportDirectory)

    print("Script finished.")
}

args <- commandArgs(TRUE)
CleanAndRemoveOutliersOnRawParticipantMasterFiles(args)