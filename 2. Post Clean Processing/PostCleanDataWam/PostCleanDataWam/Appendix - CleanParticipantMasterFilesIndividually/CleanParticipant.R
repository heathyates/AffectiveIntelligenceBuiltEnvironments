#################################################################################################
#Author: Heath Yates                                                                            #
#Date: 03/08/2018                                                                               #
#Description: Script will read through each participant master file and create plot for the     #
#biometric data such as heart, eda, and  temp                                                   #
#################################################################################################

#Step 1: Based on plot, it looks like eda level above 0.20 and above observation 1000 is an issue 
#rowsWithOutliers <- as.numeric(rownames(participantMasterData[participantMasterData$EDAAVG > 0.20 & participantMasterData$X >= 1000,]))

##Step 2: Remove the outliers 
#masterDataWithoutOutliers <- participantMasterData[-rowsWithOutliers,]

##Logic to make sure that end still exists 


##Logic to check plot to make sure things look okay now 
#participantID <- masterDataWithoutOutliers$PARTICIPANTID[1]
#observations <- as.numeric(rownames(masterDataWithoutOutliers))
#ggplot(masterDataWithoutOutliers, aes(observations, masterDataWithoutOutliers$EDAAVG)) + geom_line() + xlab("Observations(1 per sec)") + ylab("EDA") + ggtitle(paste0("Participant ", participantID, " EDA from E4"))

##Step 3: Add end to the last observation 
#lastRow <- as.numeric(rownames(tail(masterDataWithoutOutliers, 1)))

library(ggplot2)
library(openxlsx)

RemoveOutliersinMasterData <- function(participantMasterData, exportDirectory, outlierCutOff) {
    #Clean file for participant based on plot data during exploratory data analysis 
    #
    #Args: 
    #  participantMasterData: The data for participant 
    #
    #Returns: 
    #     participantMasterDataCleaned: Returns data without outliers in EDA 

    #Step 0: Find outliers, choose the smallest first observation, and participants 
    #rowsWithOutliers <- as.numeric(rownames(participantMasterData[participantMasterData$EDAAVG > 0.24 & participantMasterData$X >= outlier,]))
    #outlierCutOff <- min(rowsWithOutliers)
    #participantID <-participantMasterData$PARTICIPANTID[1]

    #Step 0: Remove the outliers 
    outlierCutOff <- 1143
    participantMasterDataWithoutOutliers <- participantMasterData[1:outlierCutOff - 1,]

    #Step 1: Create the file path to export
    participantID <- participantMasterDataWithoutOutliers$PARTICIPANTID[1]
    fileName <- paste0("participant_", participantID, "_annotated_master_clean.csv") 
    exportFilePath <- file.path(exportDirectory, fileName)

    #Step 2: Check if end is in the file, if not, create it on last line 
    zones <- unique(participantMasterDataWithoutOutliers$ZONE)
    endInZones <- "End" %in% zones
    if (!endInZones) {

        lastRow <- as.numeric(rownames(tail(participantMasterDataWithoutOutliers, 1)))
        participantMasterDataWithoutOutliers[lastRow, ]$ZONE = "End"
    }

    #Step 3: Remove the 'X' column 
    participantMasterDataCleaned <- subset(participantMasterDataWithoutOutliers, select = -X)


    #Step 4: Save the data without outliers 
    write.csv(participantMasterDataCleaned, exportFilePath)

    #Step 5: Return the participant master data without outliers .
    return(participantMasterDataCleaned)

    
}


#MAYBE NOT NEEDED DAMMIT!
PlotDataWithoutOutliers <- function(plotExportDirectory, participantMasterDataCleaned) {
    #Plots the eda biometric data with the outliers being removed 
    #
    #Args: 
    #   participantMasterDataCleaned: Participant data with outliers removed 
    #
    #Returns: 
    #   None


    #Step 0: Create variables to be used for the plots 
    participantID <- participantMasterDataCleaned$PARTICIPANTID[1]
    observations <- as.numeric(rownames(participantMasterDataCleaned))

    #Step 1: Create the plots 
    hrPlot <- ggplot(participantMasterDataCleaned, aes(observations, participantMasterDataCleaned$HRAVG)) + geom_line() + xlab("Observations(1 per sec)") + ylab("HR") + ggtitle(paste0("Participant ", participantID, " HR from E4"))
    edaPlot <- ggplot(participantMasterDataCleaned, aes(observations, participantMasterDataCleaned$EDAAVG)) + geom_line() + xlab("Observations(1 per sec)") + ylab("EDA") + ggtitle(paste0("Participant ", participantID, " EDA from E4"))
    tempPlot <- ggplot(participantMasterDataCleaned, aes(observations, participantMasterDataCleaned$TEMPAVG)) + geom_line() + xlab("Observations(1 per sec)") + ylab("TEMP") + ggtitle(paste0("Participant ", participantID, " TEMP from E4"))

    #Step 2: Create output directory for participant 
    participantPlotExportDirectory <- file.path(plotExportDirectory, paste0("participant_",participantID))
    dir.create(participantPlotExportDirectory)


    #Step 3: Output the file 
    ggsave(hrPlot, file = file.path(participantPlotExportDirectory, paste0("participant_", participantID, "_hr_plot", ".png")))
    ggsave(edaPlot, file = file.path(participantPlotExportDirectory, paste0("participant_", participantID, "_eda_plot", ".png")))
    ggsave(tempPlot, file = file.path(participantPlotExportDirectory, paste0("participant_", participantID, "_temp_plot", ".png")))

}


CleanParticipant <- function(participantFilePath, outlierCutOff) {
    #Clean file for participant based on plot data during exploratory data analysis 
    #
    #Args: 
    #   None 
    #
    #Returns: 
    #   None

    #Step 0: Defining working directories 
    workingDirectory <- "C:/Users/heath/OneDrive/Documents/Research/Current/Dissertation/2. WAMK/WAMK/6. Appendix - Processed Data/yates master/master annotated csv"

    exportDirectory <- "C:/Users/heath/OneDrive/Documents/Research/Current/Dissertation/2. WAMK/WAMK/6. Appendix - Processed Data/yates master/master annotated cleaned"

    plotExportDirectory <- "C:/Users/heath/OneDrive/Documents/Research/Current/Dissertation/2. WAMK/WAMK/4. Results/exploratory data analysis/cleaned master plots"

    #Demo Script 
    participantFilePath <- "C:/Users/heath/OneDrive/Documents/Research/Current/Dissertation/2. WAMK/WAMK/6. Appendix - Processed Data/yates master/master annotated csv/participant_1_annotated_master.csv"
    outlierCutOff <- 1134


    #Step 3: Load the data 
    participantMasterData <- read.csv(participantFilePath, header = TRUE)

    #Step 3: Remove the outliers in the master data and save the results 
    participantMasterDataCleaned <- RemoveOutliersinMasterData(participantMasterData, exportDirectory, outlierCutOff)

    #Step 4: Plot the corrected data 
    PlotDataWithoutOutliers(plotExportDirectory, participantMasterDataCleaned)


    #Step 5: Return back the corrected plot
    return(participantMasterDataCleaned)


} 
