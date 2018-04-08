#################################################################################################
#Author: Heath Yates                                                                            #
#Date: 02/28/2018                                                                               #
#Description: Process raw joined/combined qualtrics data from Parker and Taylor experiments     # 
#################################################################################################



#Section 3: Main function for the script 
PreProcessRawQualtricsData <- function(rawQualtricsDataFileDirectory) {
    #Invokes the scripts that clean and process the data from Parker's built environment experiment 
    #
    #Args: 
    #   rawQualtricsDataFileDirectory:   Location of the raw qualtrics data file 
    #
    #Returns:
    #   participant_xx_master.xlsx : Cleaned, processed, and fused empatica and polar data by participant 
    #   master.xlsx : Cleaned, processed, and fused empatica and polar data for entire experiment 

    
    #rawQualtricsDataFileDirectory <- "C:/Users/heath/OneDrive/Documents/Research/Current/Dissertation/2. WAMK/WAMK/5. Appendix - Raw Data/raw qualtrics data/rawqualtricsdata.xlsx"
    #rawQualtricsDataFileDirectory <- "D:/OneDrive/Documents/Research/Current/Dissertation/2. WAMK/WAMK/5. Appendix - Raw Data/raw qualtrics data/rawqualtricsdata.xlsx"
    rawQualtricsDataFileDirectory <- "C:/Users/heath/OneDrive/Documents/Research/Current/Dissertation/2. WAMK/WAMK/5. Appendix - Raw Data/raw joined data/complete participants/completeparticipantsqualtricsdata.xlsx"

    #Step 1: Load, process and clean master file 
    processedAndCleanedQualtricsData <- LoadProcessAndCleanQualtricsData(rawQualtricsDataFileDirectory)

    #Step 2: Create a new directory for raw data, with subdirectories for each participant (including subdirectores for empatica and polar) 

    CreateDirectoryForRawData()

    #Step 2: Save rawqualtricsdata that has been processed and cleaned 
    

    return()
    
}