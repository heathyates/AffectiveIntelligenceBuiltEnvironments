#################################################################################################
#Author: Heath Yates                                                                            #
#Date: 03/08/2018                                                                               #
#Description: Script will read through each participant master file and create plot for the     #
#biometric data such as heart, eda, and  temp                                                   #
#################################################################################################

library(openxlsx)

exploratoryDataAnalysisSubdirectory <- "exploratory data analysis"
rawMasterPlotSubdirectory <- "plots without cleaning"

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
    directoryPath <- file.path(exportDirectory, exploratoryDataAnalysisSubdirectory)
    exportSubDirectory <- file.path(file.path(exportDirectory, exploratoryDataAnalysisSubdirectory), rawMasterPlotSubdirectory)


    #Step 1: If directory path exists, remove directory and all files in subdirectories 
    if (dir.exists(directoryPath)) {

        #Remove all the files in the directory 
        #https://stackoverflow.com/questions/22122968/how-to-remove-the-file-when-permisson-denied-in-r
        unlink(directoryPath, recursive = TRUE)

        #Then create the directories again 
        dir.create(directoryPath)
        dir.create(exportSubDirectory)
    }
    else {

        #Create the needed directories 
        dir.create(directoryPath)
        dir.create(exportSubDirectory)
    }

    #Step 2: Return exportSubDirectory
    return(exportSubDirectory)



}


SetTimeForMasterData <- function(participantData){
    #Correct the time for master annotation data 
    #
    #Args: 
    #   masterAnnotationData: master annotation data file  
    #
    #Returns:
    #   masterAnnotationTimeCorrectedData: master annotation data file with time corrected 

    #Step 0: 
    #See: https://stackoverflow.com/questions/19172632/converting-excel-datetime-serial-number-to-r-datetime
    participantData["TIME"] <- as.POSIXct(participantData$TIME * (60 * 60 * 24), origin = "1899-12-30", tz = "GMT")

    #Step 1: Return the corrected data 
    participantDataTimeCorrectedData <- participantData
    return(participantDataTimeCorrectedData)

}





#Section 2: Primary Functions  
CreateBiometricPlots <- function(workingDirectory, exportDirectory) {
    #Invokes the script for creating plots 
    #
    #Args: 
    #  workingDirectory 
    #
    #Returns:
    #   all_participantes_aggregated_master.xlsx 

    #Step 0: Remove any files in the export directory and create path to export biometric plots 
    exportSubDirectory <- ClearAndCreateExportDirectory(exportDirectory)

    #Step 1: list the files we with to read 
    filesInDirectory <- list.files(workingDirectory)

    #Step 2: Remove all_participants_with_annotation_master.xlsx and obtain present working directory
    participantsInDirectory <- filesInDirectory[-1]
    oldwd <- getwd()

    #Step 3: Loop through each participant and create biometric plots and set temporary wd
    setwd(workingDirectory)
    for (i in 1:length(participantsInDirectory)) {

        #Step 3.1: file and create directory
        participantFileName <- participantsInDirectory[i]

        #Step 3.2: Read file 
        participantData <- read.csv(participantFileName, header = TRUE)

        #Step 3.3: Obtain participant name and convert the time  
        participantID <- participantData$PARTICIPANTID[1]
        participantData$TIME <- as.POSIXct(participantData$TIME)

        #Step 3.4: Create observations so we can plot them versus biometric signal 
        observations <- as.numeric(rownames(participantData))

        #Step 3.5: Create plot for heart rate, eda, and temp 
        #See: https://www.r-bloggers.com/plotting-time-series-data-using-ggplot2/
        hrPlot <- ggplot(participantData, aes(observations, participantData$HRAVG)) + geom_line() + xlab("Observations(1 per sec)") + ylab("HR") + ggtitle(paste0("Participant ", participantID, " HR from E4"))
        edaPlot <- ggplot(participantData, aes(observations, participantData$EDAAVG)) + geom_line() + xlab("Observations(1 per sec)") + ylab("EDA") + ggtitle(paste0("Participant ", participantID, " EDA from E4"))
        tempPlot <- ggplot(participantData, aes(observations, participantData$TEMPAVG)) + geom_line() + xlab("Observations(1 per sec)") + ylab("Temp") + ggtitle(paste0("Participant ", participantID, " TEMP from E4"))

        #Step 3.6: Create a directory to store the pictures in
        plotExportDirectory <- file.path(exportSubDirectory, paste0("participant_", participantID))
        dir.create(plotExportDirectory)

        #Step 3.7: Save the plots to a directory 
        #ggsave(temp_plot, file = paste0("plot_", i, ".png"), width = 14, height = 10, units = "cm")
        ggsave(hrPlot, file = file.path(plotExportDirectory, paste0("participant_", participantID, "_hr_plot", ".png")))
        ggsave(edaPlot, file = file.path(plotExportDirectory, paste0("participant_", participantID,"_eda_plot",".png")))
        ggsave(tempPlot, file = file.path(plotExportDirectory, paste0("participant_", participantID,"_temp_plot",".png")))


    }

    #Step 4: Change working directory back 
    setwd(oldwd)

}

#Section 3: 
Main <- function(workingDirectory, exportDirectory) {
    #Invokes the script for creating plots 
    #
    #Args: 
    #   workingDirectory: Where to obtain the data to create the plots
    #   exportDirectory: Location where the plots will be located 
    #
    #Returns:
    #   all_participantes_aggregated_master.xlsx 

    #Step 0: Define the working directories for the cleaned and processed master files 
    #workingDirectory <- "C:/Users/heath/OneDrive/Documents/Research/Current/Dissertation/2. WAMK/WAMK/6. Appendix - Processed Data/yates master/master annotated"
    workingDirectory <- "C:/Users/heath/OneDrive/Documents/Research/Current/Dissertation/3. WAM/8. Appendix - Cleaned and Processed Data/master annotated csv"
    exportDirectory <- "C:/Users/heath/OneDrive/Documents/Research/Current/Dissertation/3. WAM/5. Results"

    #Step 1: Create plots for each participant for heart rate, eda, and temp 
    CreateBiometricPlots(workingDirectory, exportDirectory)




    
}

args <- commandArgs(TRUE)
Main(args)