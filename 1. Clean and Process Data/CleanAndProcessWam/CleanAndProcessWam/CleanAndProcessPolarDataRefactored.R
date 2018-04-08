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


#Section 0: Define the library dependencies and global variables 
library(openxlsx)
library(ggplot2)
#rawPolarData <- ""




#Section 1: Define helper functions 
SetUpScriptEnvironmentAndLoadDataForPolar <- function(polarFileDirectory) {
    #Invokes the scripts that clean and process the data from Parker's built environment experiment 
    #
    #Args: 
    #   polarFileDirectory: Location to the csv joined polar file for participant 
    #
    #Returns:
    #   rawPolarData: Returns the data for raw polar data
    #polarFileDirectory <- polarFilePath


    #Step 0: Read the appropriate file 
    rawPolarDataFile <- list.files(path = polarFileDirectory)

    #Step 1: Load the data file with raw polar data 
    rawPolarData <- read.csv(file.path(polarFileDirectory, rawPolarDataFile), header = TRUE)
    #To do: use rawPolarDataFile to add participant id

    #Step 3: Assign raw data to a global variable
    #assign("rawPolarData", rawPolarData, envir = .GlobalEnv)
    return(rawPolarData)

}


#TO DO: Process start and end 
FilterOutStartEnd <- function(rawPolarData) {
    #Filters out the start and end in the polar data 
    #
    #Args: 
    #   rawData: dataframe of raw polar data
    #
    #Returns:
    #   rawFilteredData: Polar data with start and end removed 

    #Step 0: Find the first and the last 
    rawPolarData$QuestionNu <- as.character(rawPolarData$QuestionNu)
    rowIndexes <- which(rawPolarData$QuestionNu == "Start/ End Zone")
    firstStartEndIndex <- min(rowIndexes)
    lastStartEndIndex <- max(rowIndexes)

    #Step 1: Remove the extra tags 
    rawPolarData[rowIndexes, ]$QuestionNu <- ""

    #Step 2: Add the start and end tag 
    rawPolarData[firstStartEndIndex,]$QuestionNu <- "Start"
    rawPolarData[lastStartEndIndex,]$QuestionNu <- "End"

    #Step 3: Recast as levels 
    rawPolarData$QuestionNu <- as.factor(rawPolarData$QuestionNu)

    #Step 3: Return the filtered polar data out 
    rawPolarDataFiltered <- rawPolarData
    return(rawPolarDataFiltered)

}


#Section 2: Primary Functions 
CleanAndProcessRawPolarData <- function(rawPolarData) {
    #Cleans and processes the raw polar data and returns a datafram of clean data 
    #
    #Args: 
    #   rawData: dataframe of raw polar data
    #
    #Returns:
    #   cleanAndProcessedPolarData: dataframe of cleaned and processed raw data 

    #Step 0: Process the UTC variable for timestamp 
    rawPolarData["TIME"] <- ymd_hms(rawPolarData$Time, tz = "America/Chicago")

    #Step 1: Filter out the start/end in the data set 
    cleanAndProcessedPolarData <- FilterOutStartEnd(rawPolarData)

    #Step 2: Return the clean and processed data set 
    return(cleanAndProcessedPolarData)

}

#CleanAndProcessRawPolarData <- function() {
    ##Cleans and processes the raw polar data and returns a datafram of clean data 
    ##
    ##Args: 
    ##   rawData: dataframe of raw polar data
    ##
    ##Returns:
    ##   cleanAndProcessedPolarData: dataframe of cleaned and processed raw data 

    ##Step 0: Process the UTC variable for timestamp 
    #rawPolarData["TIMEUTC"] <- parse_date_time(rawPolarData$UTC, "Ymd HMS", tz = "UTC")
    #rawPolarData["TIMECST"] <- as.POSIXct(format(rawPolarData$TIME, tz = "America/Chicago", usetz = TRUE))



    ##Step 1: Subset the variables to output 
    #cleanAndProcessedPolarData <- subset(x = rawPolarData, select = c("Speed..mi.h.", "Distances..ft.", "Lat", "Lon", "TIMECST"))

    ##Step 2: Rename the variables in the datafram to the specific biosignal 
    #colnames(cleanAndProcessedPolarData) <- c("Speed(mi/h)", "Distances(ft)", "Lat", "Lon", "TIME")

    ##Step 3: Return the clean and processed data set 
    #return(cleanAndProcessedPolarData)

#}



#TO DO
#0. Only read in row 3 to onward and header is true, parse first row of data to get starting time though
#1. Create a timestamp for 15:23:43
#2. Convert walking time into timestamps 
#3. Subset for time, speed, and distance


#ParseFirstRowAndReturnTimeStamp <- function(firstRowDataSet, rawPolarFileName) {
    ##Invokes the scripts that clean and process the data from Parker's built environment experiment 
    ##
    ##Args: 
    ##   None 
    ##
    ##Returns:
    ##  None (but defines data to be put in ACC, BVP, EDA, HR, and Temp )

    ##For dev purposes only, delete this later 
    #rawPolarDataFile <- "Fit+Lab_V800+159_2017-12-07_15-25-43.csv"
    #firstRowPolarData <- read.csv(file = rawPolarDataFile, nrows = 1, skip = 0)

    #firstRowDataSet <- firstRowPolarData
    #rawPolarFileName <- rawPolarDataFile


    ##Step 0: Extract the date from the datafile name. For example, 2016-02-24
    ##See: https://stackoverflow.com/questions/2192316/extract-a-regular-expression-match-in-r-version-2-10
    #regPattern <- "([0-9]*)-([0-9]*)-([0-9]*)"
    #regExMatch <- regexpr(pattern = regPattern, text = rawPolarFileName)
    #yearMonthDay <- regmatches(x = rawPolarFileName, regExMatch)

    ##Step 1: Extract the hour, minute, and second. For example, 19:38:53
    #hourMinuteSecond <- firstRowDataSet$Start.time

    ##Step 2: Create timestamp from polar data 
    #combinedYearMonthDayHourMinuteSecond <- paste0(yearMonthDay, " ", hourMinuteSecond)
    #startTime <- as.POSIXct(strptime(combinedYearMonthDayHourMinuteSecond, "%Y-%m-%d %H:%M:%S"))

    ##Step 3: 
    #return(startTime)


#}

#Section 3: Main Function 
CleanAndProcessPolarData <- function(polarFilePath) {
    #Invokes the scripts that clean and process the data from Parker's built environment experiment 
    #
    #Args: 
    #   polarFilePath: Path to the polar csv file  
    #
    #Returns:
    #  cleanProcessedFilteredPolarData: Cleaned and processed polar data 

    #polarFilePath <- "D:/OneDrive/Documents/Research/Current/Dissertation/2. WAMK/WAMK/5. Appendix - Raw Data/raw practice data/participant_08/polar"
    #polarFilePath <- "C:/Users/heath/OneDrive/Documents/Research/Current/Dissertation/2. WAMK/WAMK/5. Appendix - Raw Data/processed raw data/participant_1/polar"
    #polarFilePath <-  "C:/Users/heath/OneDrive/Documents/Research/Current/Dissertation/2. WAMK/WAMK/5. Appendix - Raw Data/raw practice data/participant_1/polar"

    print(paste0("Processing and cleaning data found in: ", polarFilePath))

    #Step 0: Set up the script environment 
    rawPolarData <- SetUpScriptEnvironmentAndLoadDataForPolar(polarFilePath)

    #Step 1: Clean and process the raw polar data 
    cleanAndProcessedPolarData <- CleanAndProcessRawPolarData(rawPolarData)

    #Step 2: Send a subset of the variables to be merged with empatica 
    cleanAndProcessedPolarDataSubset <- subset(cleanAndProcessedPolarData, select = c("lat", "lon", "elevation", "PIDSec", "Walkabilit", "NumLights", "NumTrees", "NumInfLine", "NumInfPoin", "NumGrass", "NumShrub", "QuestionNu", "PARTICIPANTID", "TIME"))
    colnames(cleanAndProcessedPolarDataSubset) <- c("LAT", "LON", "ELEVATION", "PIDSEC", "WALKABILITY", "NUMLIGHTS", "NUMTREES", "NUMINFLINE", "NUMINFPOIN", "NUMGRASS", "NUMSCRUB", "ZONE", "PARTICIPANTID", "TIME")

    #Step 3: Export the data fram to the excel file 
    return(cleanAndProcessedPolarDataSubset)


}