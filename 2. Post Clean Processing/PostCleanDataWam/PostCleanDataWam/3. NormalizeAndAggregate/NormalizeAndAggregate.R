#################################################################################################
#Author: Heath Yates                                                                            #
#Date: 03/08/2018                                                                               # 
#Description: This script reads in the annotated master file and aggregates the data by         #
#participant and zone for environmental, biometrics variables                                   #
#################################################################################################

library(openxlsx)
library(lubridate)
library(plyr)

aggregatedSubdirectory <- "normalized master data"

#Section 1: Helper Functions 

ClearAndCreateExportDirectory <- function(exportDirectory) {
    #Clear and createx export directory for the aggregated master file 
    #
    #Args: 
    #   exportDirectory: Path to the export file 
    #
    #Returns:
    #   all_participantes_aggregated_master.xlsx 

    directoryPath <- file.path(exportDirectory, aggregatedSubdirectory)

    if (dir.exists(directoryPath)) {

        #Remove all the files in the directory 
        #https://stackoverflow.com/questions/9296377/automatically-delete-files-folders-in-r
        do.call(file.remove, list(list.files(path = directoryPath, full.names = TRUE)))
    }
    else {

        dir.create(file.path(exportDirectory, aggregatedSubdirectory))
    }

}


SetTimeForMasterData <- function(masterAnnotationData) {
    #Correct the time for master annotation data 
    #
    #Args: 
    #   masterAnnotationData: master annotation data file  
    #
    #Returns:
    #   masterAnnotationTimeCorrectedData: master annotation data file with time corrected 

    #Step 0: 
    #See: https://stackoverflow.com/questions/19172632/converting-excel-datetime-serial-number-to-r-datetime
    masterAnnotationData["TIME"] <- as.POSIXct(masterAnnotationData$TIME * (60 * 60 * 24), origin = "1899-12-30", tz = "GMT")

    #Step 1: Return the corrected data 
    masterAnnotationDataTimeCorrectedData <- masterAnnotationData
    return(masterAnnotationDataTimeCorrectedData)

}


RemoveDataNotInZone <- function(masterAnnotationData) {
    #Removes the data not in a zone 
    #
    #Args: 
    #   masterAnnotationData: The data with zones that have both na and non-nulls 
    #
    #Returns:
    #   masterAnnotationZoneOnly: master annotation data file with just the zones 

    #Step 0: Obtain all the rows where zones are NA 
    #See: https://stackoverflow.com/questions/11254524/omit-rows-containing-specific-column-of-na
    masterAnnotationNonNullZones <- subset(masterAnnotationData, !is.na(masterAnnotationData$ZONE))

    #Step 1: Remove the rows that have Zone values of Start and End 
    rowsWithStart <- as.numeric(rownames(masterAnnotationNonNullZones[masterAnnotationNonNullZones$ZONE == "Start",]))
    masterWithNoStart <- masterAnnotationNonNullZones[masterAnnotationNonNullZones$ZONE != "Start",]
    masterWithNoEnd <- masterWithNoStart[masterWithNoStart$ZONE != "End",]

    #Step 2: Return data with zones only 
    masterAnnotationZoneOnly <- masterWithNoEnd
    return(masterAnnotationZoneOnly)

}


#Section 2: Primary Functions

FindGlobalNormForBiometrics <- function(masterAnnotationDataCorrectedTime) {
    #This finds the biometric mean for EDA, HR, and TEMP and variance 
    #
    #Args: 
    #   masterAnnotationDataTimeCorrectedData: The master data with time corrected 
    #
    #Returns:
    #   masterAnnotationWithNorm: Returns the data set with global mean and variance for biometrics 

    #Step 0: Find mean HR and variance HR 
    meanHR <- mean(masterAnnotationDataCorrectedTime$HRAVG)
    varHR <- mean(masterAnnotationDataCorrectedTime$HRAVG)

    #Step 1: Find mean EDA and variance EDA 
    meanEDA <- mean(masterAnnotationDataCorrectedTime$EDAAVG)
    varEDA <- var(masterAnnotationDataCorrectedTime$EDAAVG)

    #Step 2: Find mean Temp and variance TEMP 
    meanTemp <- mean(masterAnnotationDataCorrectedTime$TEMPAVG)
    varTemp <- var(masterAnnotationDataCorrectedTime$TEMPAVG)

    #Step 3: Calculate normalHR, normalEDA, and normalTemp 
    normHR <- (masterAnnotationDataCorrectedTime$HRAVG - meanHR)/sqrt(varHR)
    normEDA <- (masterAnnotationDataCorrectedTime$EDAAVG - meanEDA) / sqrt(varEDA)
    normTemp <- (masterAnnotationDataCorrectedTime$TEMPAVG - meanTemp) / sqrt(varTemp)

    #Step 4: Add the normalization to the master data 
    masterAnnotationDataCorrectedTime["NORMHR"] <- normHR
    masterAnnotationDataCorrectedTime["NORMEDA"] <- normEDA
    masterAnnotationDataCorrectedTime["NORMTEMP"] <- normTemp

    masterAnnotationWithNorm <- masterAnnotationDataCorrectedTime
    return(masterAnnotationWithNorm)

}

FindSpecificNormalization <- function(masterAnnotationWithNorm) {
    #This finds the biometric mean for EDA, HR, and TEMP and variance 
    #
    #Args: 
    #   masterAnnotationDataTimeCorrectedData: The master data with time corrected 
    #
    #Returns:
    #   masterDataNormalized: The data has had new zone normalization variables added and contains only observations in the zones 

    #Step 0: Find global quantils by participant   
    masterAnnotationQuartiles <- ddply(masterAnnotationWithNorm, .(PARTICIPANTID), mutate,
                  BOTTOMHRQUART = quantile(HRAVG, probs = c(.10)), TOPHRQUART = quantile(HRAVG, probs = c(.90)),
                  BOTTOMEDAQUART = quantile(EDAAVG, probs = c(.10)), TOPEDAQUART = quantile(EDAAVG, probs = c(.90))
                  )


    #TO DO
    #Step 1:  Obtain all observations below or above a quartile cutoff
    bottomHRValues <- ddply(masterAnnotationQuartiles, .(PARTICIPANTID), subset, HRAVG <= BOTTOMHRQUART)
    topHRValues <- ddply(masterAnnotationQuartiles, .(PARTICIPANTID), subset, HRAVG >= TOPHRQUART)

    bottomEDAValues <- ddply(masterAnnotationQuartiles, .(PARTICIPANTID), subset, EDAAVG <= BOTTOMEDAQUART)
    topEDAValues <- ddply(masterAnnotationQuartiles, .(PARTICIPANTID), subset, EDAAVG >= TOPEDAQUART)


    #Step 2:  Find median for each participant and add to the equation 
    bottomHR <- ddply(bottomHRValues, .(PARTICIPANTID), summarize, BOTTOMHR = median(HRAVG))
    topHR <- ddply(topHRValues, .(PARTICIPANTID), summarize, TOPHR = median(HRAVG))

    bottomEDA <- ddply(bottomEDAValues, .(PARTICIPANTID), summarize, BOTTOMEDA = median(EDAAVG))
    topEDA <- ddply(topEDAValues, .(PARTICIPANTID), summarize, TOPEDA = median(EDAAVG))


    #Step 3: Merge this with data 
    masterAnnotationWithTopBottomBiometrics <- merge(masterAnnotationWithNorm, bottomHR, by.x = "PARTICIPANTID", by.y = "PARTICIPANTID")
    masterAnnotationWithTopBottomBiometrics <- merge(masterAnnotationWithTopBottomBiometrics, topHR, by.x = "PARTICIPANTID", by.y = "PARTICIPANTID")
    masterAnnotationWithTopBottomBiometrics <- merge(masterAnnotationWithTopBottomBiometrics, bottomEDA, by.x = "PARTICIPANTID", by.y = "PARTICIPANTID")
    masterAnnotationWithTopBottomBiometrics <- merge(masterAnnotationWithTopBottomBiometrics, topEDA, by.x = "PARTICIPANTID", by.y = "PARTICIPANTID")

    #Step 4: Remove all data except the zones 
    masterDataWithZonesOnly <- RemoveDataNotInZone(masterAnnotationWithTopBottomBiometrics)

    #Step 5: Calculate the specific normalization as specified by Healey (biometric - min)/(top - min) 
    masterDataWithZoneMin <- ddply(masterDataWithZonesOnly, .(ZONE, PARTICIPANTID), mutate, HRMINZONE = min(HRAVG), EDAMINZONE = min(EDAAVG))
    masterDataWithZoneNorm <- ddply(masterDataWithZoneMin, .(ZONE, PARTICIPANTID),
                                 mutate,
                                 HRZONENORM = (HRAVG - HRMINZONE) / (TOPHR - BOTTOMHR),
                                 EDAZONENORM = (EDAAVG - EDAMINZONE) / (TOPEDA - BOTTOMEDA))

    #Step 6: Subset only variables required for analysis and names of variables 
    masterDataNormalized <- subset(masterDataWithZoneNorm, select =  c("PARTICIPANTID","TIME","EDAAVG","HRAVG", "TEMPAVG", "LAT", "LON", "ELEVATION",
     "PIDSEC", "WALKABILITY", "NUMLIGHTS", "NUMTREES", "NUMINFLINE", "NUMINFPOIN",
     "NUMGRASS", "NUMSCRUB", "ZONE", "GENDER", "BODYSHAPE", "URBANORIGIN",
     "URBANPREFERENCE", "STUDYAREAFAMILIARITY", "EXERCISE", "EXERCISEDPASTTHREEHOURS", "ANNOTATION",
     "BINARYANNOTATION", "NORMHR", "NORMEDA", "NORMTEMP", "HRZONENORM", "EDAZONENORM"))

    #Step 7: Polish the names of the variables  
    colnames(masterDataNormalized) <- c("PARTICIPANTID", "TIME", "EDA", "HR", "TEMP", "LAT", "LON", "ELEVATION",
     "PIDSEC", "WALKABILITY", "NUMLIGHTS", "NUMTREES", "NUMINFLINE", "NUMINFPOIN",
     "NUMGRASS", "NUMSCRUB", "ZONE", "GENDER", "BODYSHAPE", "URBANORIGIN",
     "URBANPREFERENCE", "STUDYAREAFAMILIARITY", "EXERCISE", "EXERCISEDPASTTHREEHOURS", "ANNOTATION",
     "BINARYANNOTATION", "NORMHR", "NORMEDA", "NORMTEMP", "HRZONENORM", "EDAZONENORM")
    
    #Step 8: Return the annotated data with the new zone normalized data and data only specific to zones 
    return(masterDataNormalized)

}

ProcessAndAggregateData <- function(masterDataNormalized) {
    #Processes the data and aggregates it to prepare for analysis 
    #
    #Args: 
    #   masterAnnotationDataTimeCorrectedData: The master data with time corrected 
    #
    #Returns:
    #   aggregatedMasterDataSorted: Data ready for analysis 

    #Step 0: Select the demographic and annotation data by zone and participant 
    masterDataDemographicAndAnnotation <- subset(masterDataNormalized,
                                                 select = c("PARTICIPANTID", "ZONE", "GENDER", "BODYSHAPE", "URBANORIGIN",
                                                            "URBANPREFERENCE", "STUDYAREAFAMILIARITY", "EXERCISE", "EXERCISEDPASTTHREEHOURS",
                                                            "BINARYANNOTATION"))

    masterDemographicAndAnnotationByZoneAndParticipant <- ddply(masterDataDemographicAndAnnotation, .(ZONE, PARTICIPANTID), function(x) head(x, 1))


    #Step 1: Obtain the biometric and environmental variables to obtain means on 
    masterDataBiometricAndEnvironmental <- subset(masterDataNormalized, select = c("PARTICIPANTID", "EDA", "HR", "TEMP", "ELEVATION", "PIDSEC", "WALKABILITY", "NUMLIGHTS", "NUMTREES",
                                            "NUMINFLINE", "NUMINFPOIN", "NUMGRASS", "NUMSCRUB", "ZONE", "NORMHR", "NORMEDA", "NORMTEMP", "HRZONENORM",
                                            "EDAZONENORM"))

    masterBiometricAndEnvironmentalByZoneAndParticipant <- ddply(masterDataBiometricAndEnvironmental, .(ZONE, PARTICIPANTID), summarize,
                                                                 EDA = mean(EDA), HR = mean(HR), TEMP = mean(TEMP), ELEVATION = mean(ELEVATION), PIDSEC = mean(PIDSEC), WALKABILITY = mean(as.numeric(WALKABILITY)), NUMTREES = mean(as.numeric(NUMTREES)), NUMLIGHTS = mean(as.numeric(NUMLIGHTS)), NUMINFLINE = mean(as.numeric(NUMINFLINE)), NUMINFPOIN = mean(as.numeric(NUMINFPOIN)),
                                                                 NUMGRASS = mean(as.numeric(NUMGRASS)), NUMSCRUB = mean(as.numeric(NUMSCRUB)), NORMHR = mean(NORMHR), NORMEDA = mean(NORMEDA), NORMTEMP = mean(NORMTEMP), HRZONENORM = mean(HRZONENORM),
                                                                 EDAZONENORM = mean(EDAZONENORM))



    #Step 2: Merge with variables by zone and participant 
    aggregatedMasterData <- merge(masterDemographicAndAnnotationByZoneAndParticipant, masterBiometricAndEnvironmentalByZoneAndParticipant, by = c("ZONE", "PARTICIPANTID"))
    aggregatedMasterDataSorted <- arrange(aggregatedMasterData, ZONE, PARTICIPANTID)


    #Step 3: Return the data 
    return(aggregatedMasterDataSorted)
}





Main <- function(pathToMasterFile, exportDirectory) {
    #Invokes the script the aggregates the wamk data 
    #
    #Args: 
    #   pathToMasterFile: Path to the master file where the annotate data file is located 
    #   exportDirectory: 
    #
    #Returns:
    #   all_participantes_aggregated_master.xlsx 
    pathToMasterFile <- "C:/Users/heath/OneDrive/Documents/Research/Current/Dissertation/3. WAM/9. Appendix - Cleaned Data/master annotated cleaned/all_participants_master_cleaned.csv"
    exportDirectory <- "C:/Users/heath/OneDrive/Documents/Research/Current/Dissertation/3. WAM/10. Appendix - Normalized Cleaned Data"

    #Step 0: Creates export directory and if it already exists, removes it and then creates it again 
    ClearAndCreateExportDirectory(exportDirectory)

    #Step 2: Load the master file 
    masterAnnotationData <- read.csv(pathToMasterFile, header = TRUE, na.strings = c(""))

    #Step 3: Normalize the data for biometrics  
    masterAnnotationWithNorm <- FindGlobalNormForBiometrics(masterAnnotationData)

    #Step 4: Caculate a specific normalization defined by us (biometric - min(biometric by zone) / top - bottom #        where top is median of 10% of top values and bottom is median of bottom 10% of values masterDataNormalized <- FindSpecificNormalization(masterAnnotationWithNorm) #Fix this step with column names 
    masterDataNormalized <- FindSpecificNormalization(masterAnnotationWithNorm)


    #Step 5: Write the output of the normalized data 
    exportDirectoryPathNormalizedData <- file.path(file.path(exportDirectory, aggregatedSubdirectory), "all_participants_master_with_normalized.csv")
    write.csv(masterDataNormalized, exportDirectoryPathNormalizedData)

    #Step 6: Aggregate data just by means and output this to specific folder 
    masterDataAggregated <- ProcessAndAggregateData(masterDataNormalized)

    #Step 7: Write the output of the aggregated data    
    exportDirectoryPathAggregatedData <- file.path(file.path(exportDirectory, aggregatedSubdirectory), "all_participants_master_with_aggregated.csv")
    write.csv(masterDataAggregated, exportDirectoryPathAggregatedData)


}

args <- commandArgs(TRUE)
Main(args) 