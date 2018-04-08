#################################################################################################
#Author: Heath Yates                                                                            #
#Date: 03/08/2018                                                                               #
#Description: This runs all the custom cleaning scripts for all the participants                # 
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
    #  exportDirectory 
    #
    #Returns:
    #  exportSubDirectory : Exports the path where the raw master participant data for plots should be sent :14


    #Step 0: Create the directory and export sub directory paths
    subDirectory <- "cleaned master plots"
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
    if (!exists("CleanParticipant", mode = "function")) {

        cleanParticipantScriptPath <- file.path(sourceDirectory, "CleanParticipant.R")
        source(cleanParticipantScriptPath)
    } else {
        print("Source files already loaded.")
    }


}



#Section 2: Primary Functions 
CleanParticipantsMasterFiles <- function(workingDirectory) {
    #CleanParticipantData
    #
    #Args: 
    #  None 
    #
    #Returns:

    #Step 0: Define the outlier cutoff values 


    #Step 1: 
    participantsInDirectory <- list.files(workingDirectory)



}


#Section 3: Main function 
CleanAllParticipants <- function() {
    #Clean file for participant based on plot data during exploratory data analysis 
    #
    #Args: 
    #  None 
    #
    #Returns:

    workingDirectory <- "C:/Users/heath/OneDrive/Documents/Research/Current/Dissertation/2. WAMK/WAMK/6. Appendix - Processed Data/yates master/master annotated csv"
    exportDirectory <- "C:/Users/heath/OneDrive/Documents/Research/Current/Dissertation/2. WAMK/WAMK/6. Appendix - Processed Data/yates master"
    sourceDirectory <- "C:/Users/heath/OneDrive/Documents/Research/Current/Dissertation/2. WAMK/WAMK/3. Analysis/ExploratoryDataAnalysis/ExploratoryDataAnalysis/CleanParticipantMasterFilesIndividually"

    plotExportDirectory <- "C:/Users/heath/OneDrive/Documents/Research/Current/Dissertation/2. WAMK/WAMK/4. Results/exploratory data analysis"


    #Step 0: Clear the directory and then create it 
    ClearAndCreateExportDirectory(exportDirectory)
    ClearAndCreateExportPlotDirectory(plotExportDirectory)

    #Step 1: Load the helper functions to clean the participants data 
    LoadHelperRFunctions(sourceDirectory)

    #Step 2: 





}
