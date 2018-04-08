#################################################################################################
#Author: Heath Yates                                                                            #
#Date: 01/20/2018                                                                               #
#Description: Read the raw file for Whitaker Affect Manhattan empatica data and produce xlsx   #
#data that combines by timestamp the following by participant:                                  #
#   - HR (Heart Rate)                                                                           #
#   - Temp (Temperature)                                                                        #  
#   - Min EDA                                                                                   #
#   - Max EDA                                                                                   #
#   - Avg EDA                                                                                   # 
#   - Min BVP                                                                                   #
#   - Max BVP                                                                                   #
#   - Min Temp                                                                                  #
#   - Max Temp                                                                                  # 
#   - Avg Temp                                                                                  #
#################################################################################################


#Section 0: Define the necessary libraries required and global variables 
library(plyr)
library(chron)
library(lubridate)
#library(xlsx)
library(openxlsx)



#workingDirectoryPath <- "C:/Users/heath/OneDrive/Documents/Research/Current/Dissertation/WAMK/Data/Whitaker Affect Manhattan Data/raw/participant_09/empatica"
#workingDirectoryPath <- "C:/Users/hlyates/OneDrive/Documents/Research/Current/Dissertation/WAMK/Data/Whitaker Affect Manhattan Data/raw/participant_09/empatica"
workingDirectoryPath <- "D:/OneDrive/Documents/Research/Current/Dissertation/WAMK/Data/Whitaker Affect Manhattan Data/raw/participant_09/empatica"

rawBVPData <- ""
rawEDAData <- ""
rawHRData <- ""
rawTempData <- ""


#Section 1: Define the helper functions 
CreateSampleIndex <- function(rawCleanData, samplingRate) {
    #Invokes the scripts that clean and process the data from Parker's built environment experiment 
    #
    #Args: 
    #   RawCleanData: Dataframe of raw empatica data for either ACC, BVP, EDA, HR, or TEMP
    #   SamplingRate: Sampling rate for the raw data to be used in creating sample index 
    #
    #Returns:
    #  RawCleanDataWithSampleRate: Dataframe of raw empatica data and sample index for grouped by each 64 sample rates
    #  For example, if the sample rate is 2 for EDA 
    #  EDA SampleIndexRatee 
    #  2.0 1 
    #  2.5 1
    #  1.0 2 
    #  1.7 2 
    #  .   . 
    #  .   . 
    #  .   .
    #  Thus preparing the data set for subsequent processing 

    # Determine the total length or number of biometric observations for data set 
    totalLength <- nrow(rawCleanData)

    # Determine if there are any extra observations above the sampling rate (this can happen if sensor turned off in the middle of a second)
    extraObservations <- totalLength %% samplingRate
    multiplicity <- (totalLength - extraObservations) / samplingRate

    #If the sample has extra observations, count them as well
    SampleRateIndex <- 0
    if (extraObservations > 0) {

        SampleRateIndex <- c(rep(1:multiplicity, each = samplingRate), rep(multiplicity + 1, each = extraObservations))

    } else {
        #The samples are even and so no need for extra observations to be counted
        SampleRateIndex <- rep(1:multiplicity, each = samplingRate)
    }


    # Creates a data frame with a new sample rate index 
    rawCleanDataWithSampleRate <- cbind(rawCleanData, SampleRateIndex)

    #Return the processed data with the sample rate index 
    return(rawCleanDataWithSampleRate)



}


#Clean and process the raw data and return the process and clean data in a dataframe
CleanAndProcessRawData <- function(rawData) {
    #Invokes the scripts that clean and process the data from Parker's built environment experiment 
    #
    #Args: 
    #   rawData: dataframe of raw data 
    #   biometricSignalName: string of biometric signal name, for example, "HR"
    #
    #Returns:
    #  cleanedAndProcessedData: dataframe of cleaned and processed raw data 

    #rawData <- rawHRData #Debug purposes only 
    #rawData <- rawEDAData

    #Step 1: Obtain the unix timestamp and utc and sampling rate 
    unixTimeStampUtcValue <- rawData[1,]
    samplingRate <- rawData[2,]
    biometricSignalName <- colnames(rawData)[1]


    #Step 2: Clean and parse the raw data just to have contain actual biometric signals (first two rows are time and sampling rate) 
    rawProcessedData <- as.data.frame(rawData[3:length(rawData[, 1]),])
    colnames(rawProcessedData) <- "BIOSIGNAL" #This generic name is enforced so we can aggregate by min, max, avg soon 

    #Step 3: Create index by each sampling index. For example, if we have 3 seconds of data, with sampling rate of 2 we have the following: 
    #   SampleRateIndex 
    #   1
    #   1
    #   2 
    #   2
    #   3
    #   3 
    rawProcessedDataWithSampleIndex <- CreateSampleIndex(rawProcessedData, samplingRate)

    #Step 4: Report the min, max, and average by all sampling observations per second
    rawProcessedDataWithAggregates <- ddply(rawProcessedDataWithSampleIndex, .(SampleRateIndex), summarize, MIN = min(BIOSIGNAL), MAX = max(BIOSIGNAL), AVG = mean(BIOSIGNAL))

    #Step 5: Create timestamp and increment timestamps based on the SampleRateIndex
    startingTimeStamp <- as.POSIXct(unixTimeStampUtcValue, origin = "1970-01-01")
    processedData <- ddply(rawProcessedDataWithAggregates, .(SampleRateIndex), plyr::here(mutate), TIME = startingTimeStamp + lubridate::seconds(SampleRateIndex - 1))


    processedAndCleanedData <- processedData[, c("MIN", "MAX", "AVG", "TIME")]

    #Step 7 : Rename the variables in the dataframe to the specific biosignal
    bioMin <- paste0(biometricSignalName, "MIN")
    bioMax <- paste0(biometricSignalName, "MAX")
    bioAvg <- paste0(biometricSignalName, "AVG")
    colnames(processedAndCleanedData) <- c(bioMin, bioMax, bioAvg, "TIME")


    #Step 8: Return the cleaned and process data set 
    return(processedAndCleanedData)


}


#Setup the working directory and the files 
SetUpScriptEnvironmentAndLoadDataForEmpatica <- function(empaticaFileDirectory) {
    #Invokes the scripts that clean and process the data from Parker's built environment experiment 
    #
    #Args: 
    #   None 
    #
    #Returns:
    #  None (but defines data to be put in ACC, BVP, EDA, HR, and Temp )

    #empaticaFileDirectory <- empaticaFilePath
    #print(empaticaFileDirectory)

    #Step 1.1: Define the raw empatica data (ACC, BVP, EDA, HR, IBI, and TEMP)
    rawBVPDataFile <- "BVP.csv"
    rawHRDataFile <- "HR.csv"
    rawEDADataFile <- "EDA.csv"
    rawTEMPDataFile <- "TEMP.csv"

    #Step 1.2: Set the working directory path 
    bvpDataFile <- file.path(empaticaFileDirectory, rawBVPDataFile)
    hrDataFile <- file.path(empaticaFileDirectory, rawHRDataFile)
    edaDataFile <- file.path(empaticaFileDirectory, rawEDADataFile)
    tempDataFile <- file.path(empaticaFileDirectory, rawTEMPDataFile)



    #Step 1.3: Load the raw empatica data (ACC, BVP, EDA, HR, IBI, and TEMP)
    bvpData <- read.csv(file = bvpDataFile, header = FALSE, col.names = c("BVP"))
    hrData <- read.csv(file = hrDataFile, header = FALSE, col.names = c("HR"))
    edaData <- read.csv(file = edaDataFile, header = FALSE, col.names = c("EDA"))
    tempData <- read.csv(file = tempDataFile, header = FALSE, col.names = c("TEMP"))

    assign("rawBVPData", bvpData, envir = .GlobalEnv)
    assign("rawHRData", hrData, envir = .GlobalEnv)
    assign("rawEDAData", edaData, envir = .GlobalEnv)
    assign("rawTempData", tempData, envir = .GlobalEnv)

}



#Section 2: The main function to process helper functions and script 
CleanAndProcessEmpaticaData <- function(empaticaFilePath) {
    #Invokes the scripts that clean and process the data from Parker's built environment experiment 
    #
    #Args: 
    #    empaticaFilePath: Path to participants raw empatica data 
    #
    #Returns:
    #   mergedData: Returns cleaned and processed empatica data  

    #empaticaFilePath <- "D:/OneDrive/Documents/Research/Current/Dissertation/2. WAMK/WAMK/5. Appendix - Raw Data/raw practice data/participant_08/empatica"
    #empaticaFilePath <- "C:/Users/heath/OneDrive/Documents/Research/Current/Dissertation/2. WAMK/WAMK/5. Appendix - Raw Data/raw practice data/participant_1/empatica"
    #empaticaFilePath <- "C:/Users/heath/OneDrive/Documents/Research/Current/Dissertation/2. WAMK/WAMK/5. Appendix - Raw Data/raw joined data/complete participants/t09/empatica"
    print(paste0("Processing and cleaning data found in: ", empaticaFilePath))



    #times(format(localBVPTimestamp, "%H:%M:%S")) Do this to the final timestamp of the data 
    #Step 0: Set up the script environment 
    SetUpScriptEnvironmentAndLoadDataForEmpatica(empaticaFilePath)


    #Step 1: Clean and process the raw data 
    bvpData <- CleanAndProcessRawData(rawBVPData)
    edaData <- CleanAndProcessRawData(rawEDAData) #This doesn't work 
    hrData <- CleanAndProcessRawData(rawHRData)
    tempData <- CleanAndProcessRawData(rawTempData)

    #Step 2: Merge the data 
    #https://stackoverflow.com/questions/13778267/how-can-i-merge-multiple-dataframes-with-the-same-column-names
    mergedData <- Reduce(function(x, y) { merge(x, y, by.x = "TIME", by.y = "TIME") }, list(bvpData, edaData, hrData, tempData))


    #Step 3: Write the data to the excel file 
    #write.xlsx(mergedData, file = "participant_09_empatica.xlsx", col.names=TRUE)
    return(mergedData)

}
