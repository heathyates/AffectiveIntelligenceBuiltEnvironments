#################################################################################################
#Author: Heath Yates                                                                            #
#Date: 03/08/2018                                                                               #
#Description: This file cleans and if necessary, removes outliers in a participant              # 
#################################################################################################

#Section 0: Dependencies 
library(ggplot2)
library(openxlsx)

#Section 1: Helper Functions 

#Section 2: Primary Functions
RemoveOutliers <- function(outlierCutOff, participantMasterData, exportDirectory, plotExportDirectory) {
    #This script removes outliers after a certain cutoff point usuall associated with with 
    #the empatica being taken off before the end zone 
    #
    #Args: 
    #   participantMasterData: The master annotation csv file for the participante
    #   exportDirectory: This is the destination of the cleaned data for the user 
    #   plotExportDirectory: The destination to store the cleaned plot 
    #
    #Returns:
    #   participantMasterClean: The data cleaned with start to end 


    #Step 0: Remove the outliers 
    participantMasterDataWithoutOutliers <- participantMasterData[1:outlierCutOff - 1,]
    participantID <- participantMasterDataWithoutOutliers$PARTICIPANTID[1]

    #Step 1: Check to add end 
    zones <- unique(participantMasterDataWithoutOutliers$ZONE)
    endInZones <- "End" %in% zones
    if (!endInZones) {
        lastRow <- as.numeric(rownames(tail(participantMasterDataWithoutOutliers, 1)))
        participantMasterDataWithoutOutliers[lastRow,]$ZONE = "End"
    }

    #Step 2: Create the plots 
    observations <- as.numeric(rownames(participantMasterDataWithoutOutliers))
    hrPlot <- ggplot(participantMasterDataWithoutOutliers, aes(observations, participantMasterDataWithoutOutliers$HRAVG)) + geom_line() + xlab("Observations(1 per sec)") + ylab("HR") + ggtitle(paste0("Participant ", participantID, " HR from E4"))
    edaPlot <- ggplot(participantMasterDataWithoutOutliers, aes(observations, participantMasterDataWithoutOutliers$EDAAVG)) + geom_line() + xlab("Observations(1 per sec)") + ylab("EDA") + ggtitle(paste0("Participant ", participantID, " EDA from E4"))
    tempPlot <- ggplot(participantMasterDataWithoutOutliers, aes(observations, participantMasterDataWithoutOutliers$TEMPAVG)) + geom_line() + xlab("Observations(1 per sec)") + ylab("TEMP") + ggtitle(paste0("Participant ", participantID, " TEMP from E4"))

    #Step 3: Save the plots 
    participantPlotExportDirectory <- file.path(plotExportDirectory, paste0("participant_", participantID))
    dir.create(participantPlotExportDirectory)

    ggsave(hrPlot, file = file.path(participantPlotExportDirectory, paste0("participant_", participantID, "_hr_plot", ".png")))
    ggsave(edaPlot, file = file.path(participantPlotExportDirectory, paste0("participant_", participantID, "_eda_plot", ".png")))
    ggsave(tempPlot, file = file.path(participantPlotExportDirectory, paste0("participant_", participantID, "_temp_plot", ".png")))

    #Step 4: Save the cleaned data
    participantMasterDataCleaned <- subset(participantMasterDataWithoutOutliers, select = -X)

    fileName <- paste0("participant_", participantID, "_annotated_master_clean.csv")
    exportFilePath <- file.path(exportDirectory, fileName)
    write.csv(participantMasterDataCleaned, exportFilePath)
    

    #Step 5: export the cleaned data 
    return(participantMasterDataCleaned)

}

CleanData <- function(participantMasterData, exportDirectory, plotExportDirectory) {
    #This function cleans the script by using data only between start and end 
    #
    #Args: 
    #   participantMasterData: The master annotation csv file for the participante
    #   plotExportDirectory: The destination to store the cleaned plot 
    #
    #Returns:
    #   participantMasterClean: The data cleaned with start to end  

    #Step 0: Remove data before start and after end 
    participantID <- participantMasterData$PARTICIPANTID[1]
    startRow <- as.numeric(rownames(participantMasterData[participantMasterData$ZONE == "Start",]))
    endRow <- as.numeric(rownames(participantMasterData[participantMasterData$ZONE == "End",]))
    participantMasterStartToEndData <- participantMasterData[startRow:endRow,]

    #Step 1: Create the plots
    observations <- as.numeric(rownames(participantMasterStartToEndData))
    hrPlot <- ggplot(participantMasterStartToEndData, aes(observations, participantMasterStartToEndData$HRAVG)) + geom_line() + xlab("Observations(1 per sec)") + ylab("HR") + ggtitle(paste0("Participant ", participantID, " HR from E4"))
    edaPlot <- ggplot(participantMasterStartToEndData, aes(observations, participantMasterStartToEndData$EDAAVG)) + geom_line() + xlab("Observations(1 per sec)") + ylab("EDA") + ggtitle(paste0("Participant ", participantID, " EDA from E4"))
    tempPlot <- ggplot(participantMasterStartToEndData, aes(observations, participantMasterStartToEndData$TEMPAVG)) + geom_line() + xlab("Observations(1 per sec)") + ylab("TEMP") + ggtitle(paste0("Participant ", participantID, " TEMP from E4"))

    #Step 2: Save the plots 
    participantPlotExportDirectory <- file.path(plotExportDirectory, paste0("participant_", participantID))
    dir.create(participantPlotExportDirectory)

    ggsave(hrPlot, file = file.path(participantPlotExportDirectory, paste0("participant_", participantID, "_hr_plot", ".png")))
    ggsave(edaPlot, file = file.path(participantPlotExportDirectory, paste0("participant_", participantID, "_eda_plot", ".png")))
    ggsave(tempPlot, file = file.path(participantPlotExportDirectory, paste0("participant_", participantID, "_temp_plot", ".png")))

    #Step 3: Save the cleaned data
    participantMasterDataCleaned <- subset(participantMasterStartToEndData, select = -X)

    fileName <- paste0("participant_", participantID, "_annotated_master_clean.csv")
    exportFilePath <- file.path(exportDirectory, fileName)
    write.csv(participantMasterDataCleaned, exportFilePath)

    #Step 4: export the cleaned data 
    return(participantMasterDataCleaned)


    
}

#Section 3: Main Function 
CleanAndRemoveOutlierOnParticipant <- function(hasOutliers, outlierCutOff, participantFilePath, exportDirectory, plotExportDirectory) {
    #This main function removes outliers or cleans the file 
    #
    #Args: 
    #   hasOutliers: Boolean variable used to determine if outliers need done on participant data 
    #   participantFilePath: Path to the master annotated csv file data for participant 
    #
    #Returns:
    #   cleanedParticipantMasterData: The cleaned particpipant master data 
   

    #Step 0: Read the data 
    participantMasterData <- read.csv(participantFilePath, header = TRUE)

    #Step 0: If it has outliers, clean the outliers, export the cleaned data and plots. Otherwise, clean and export the plots 
    cleanedParticipantMasterData <- ""
    if (hasOutliers) {
        #Step 0.1 Remove the outliers 
        cleanedParticipantMasterData <- RemoveOutliers(outlierCutOff, participantMasterData, exportDirectory, plotExportDirectory)
    } else {
        #Step 0.2: Clean 
        cleanedParticipantMasterData <- CleanData(participantMasterData, exportDirectory, plotExportDirectory)

    }

    #Step 1: Return the cleaned participant master data 
    return(cleanedParticipantMasterData)



}