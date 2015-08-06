addNewData <- function(runName, date, retest=FALSE) {
        
        ##open files - new data and old lab results
        file <- paste("M:/1. Data/1. Raw Data/Under5_PfLDH/01052015-1/", runName, "/", runName, " results.csv", sep="")
        newData <- read.csv(file)
        colnames(newData) <- c("Well", "Fluor", "Content", "Sample", "Cq", "SQ", "Override")
        options(stringsAsFactors=TRUE)
        labResults <- read.csv("M:/1. Data/2. Data Set Processing/Under5_PfLDH/Lab Results.csv", as.is=TRUE)
        labResults <- labResults[,2:ncol(labResults)]
        labResults$Date <- as.Date(as.character(labResults$Date), format="%Y-%m-%d")
        
        #create output data frame and sort data by sample
        numRows = (nrow(newData)-24)/4
        output <- data.frame(Date=character(numRows), Sample=character(numRows), Retest=numeric(numRows), Pf_Ct1=numeric(numRows), Pf_Ct2=numeric(numRows), Pf_MeanCt=numeric(numRows), SQ1=numeric(numRows), SQ2=numeric(numRows), MeanSQ=numeric(numRows), Hum_Ct1=numeric(numRows), Override_Ct1=numeric(numRows), Hum_Ct2=numeric(numRows), Override_Ct2=numeric(numRows), Hum_Result1=numeric(numRows), Hum_Result2=numeric(numRows), Result=numeric(numRows))
        newData <- newData[order(newData$Content, newData$Fluor, newData$Well),]
        
        output$Sample <- as.character(output$Sample)
        newData$Sample <- as.character(newData$Sample)
        newData$Cq <- as.numeric(as.character(newData$Cq))
        newData$SQ <- as.numeric(as.character(newData$SQ))
        newData$Override <- as.numeric(as.character(newData$Override))
        output$Date <- as.Date(as.character(output$Date), format="%m/%d/%Y")
        rowCount = 1

        ##human result
        newData["hum_result"] <- NA
        for (row in 1:(nrow(newData))) {
                if (newData$Fluor[row] == "VIC") {
                        if ((!is.na(newData$Cq[row]) && (newData$Cq[row] <35))) {
                                newData$hum_result[row] = 1
                        }
                        else {
                                newData$hum_result[row] = 0
                        }
                }
        }

        ##transform data so there's one line per sample
        for (row in 1:(nrow(newData)-24)) { ##number of rows except NTC and standards
                if (row %% 4 == 1) {
                        output$Sample[rowCount] <- newData$Sample[row+24]
                        output$Pf_Ct1[rowCount] <- newData$Cq[row+24]
                        output$SQ1[rowCount] <- newData$SQ[row+24]
                }
                if (row %% 4 == 2) {
                        output$Pf_Ct2[rowCount] = newData$Cq[row+24]
                        output$SQ2[rowCount] = newData$SQ[row+24]
                }
                if (row %% 4 == 3) {
                        output$Hum_Ct1[rowCount] = newData$Cq[row+24]
                        output$Hum_Result1[rowCount] = newData$hum_result[row+24]
                        output$Override_Ct1[rowCount] = newData$Override[row+24]
                }
                if (row %% 4 == 0) {
                        output$Hum_Ct2[rowCount] = newData$Cq[row+24]
                        output$Hum_Result2[rowCount] = newData$hum_result[row+24]
                        output$Override_Ct2[rowCount] = newData$Override[row+24]
                        rowCount = rowCount + 1
                }                
        }
        output$Date <- as.Date(as.character(output$Date), format="%m/%d/%Y")

        ##determine malaria positive/negative & calculate means & add date
        for (row in 1:nrow(output)) {
                output$Date[row] <- as.Date(as.character(date), format="%m/%d/%Y")
                if (retest==TRUE) {
                        output$Retest[row] = 1
                }
                #indeterminates
                if (output$Hum_Result1[row] == 0 && output$Hum_Result2[row] == 0) {
                        output$Result[row] = 99
                }
                #only one human sample amplified --> other sample determines Pf +/-
                else if (output$Hum_Result1[row] == 0 && output$Hum_Result2[row] == 1) {
                        if (is.na(output$Pf_Ct2[row])) {
                                output$Result[row] = 0
                        }
                        else {
                                output$Result[row] = 1
                                output$Pf_MeanCt[row] = output$Pf_Ct2[row]
                                output$MeanSQ[row] = output$SQ2[row]
                        }
                }
                else if (output$Hum_Result1[row] == 1 && output$Hum_Result2[row] == 0) {
                        if (is.na(output$Pf_Ct1[row])) {
                                output$Result[row] = 0
                        }
                        else {
                                output$Result[row] = 1
                                output$Pf_MeanCt[row] = output$Pf_Ct1[row]
                                output$MeanSQ = output$SQ1
                        }
                }
                #both human amplify --> Pfs must both amplify or have one under 38 to be +
                else {
                        if (!is.na(output$Pf_Ct1[row]) && !is.na(output$Pf_Ct2[row])) {
                                output$Result[row] = 1
                                output$Pf_MeanCt[row] = (output$Pf_Ct1[row] + output$Pf_Ct2[row])/2
                                output$MeanSQ[row] = (output$SQ1[row] + output$SQ2[row])/2
                        }
                        else if (is.na(output$Pf_Ct1[row]) && is.na(output$Pf_Ct2[row])) {
                                output$Result[row] = 0
                        }
                        else if ((!is.na(output$Pf_Ct1[row]) && (output$Pf_Ct1[row]<38))) {
                                output$Pf_MeanCt[row] = output$Pf_Ct1[row]
                                output$MeanSQ[row] = output$SQ1[row]
                                output$Result[row] = 1
                        }
                        else if ((!is.na(output$Pf_Ct2[row]) && (output$Pf_Ct2[row]<38))) {
                                output$Pf_MeanCt[row] = output$Pf_Ct2[row]
                                output$MeanSQ[row] = output$SQ2[row]
                                output$Result[row] = 1
                        }
                        else {
                                output$Result[row] = 0
                        }
                }
        }
        
        ##add to lab results - switch first line for second for very first time
#         labResults = output       
        labResults <- rbind.data.frame(labResults, output)
        write.csv(labResults, "M:/1. Data/2. Data Set Processing/Under5_PfLDH/Lab Results.csv")
}