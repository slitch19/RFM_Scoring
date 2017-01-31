
# REFERENCE FOR THE getRFMvalues function (row 12- 77)
# http://www.dataapple.net/wp-content/uploads/2013/12/RFM_Analysis_R_Source_Codes_V1.3.txt


library(readr)
df <- as.data.frame(read_csv("superstore.csv",
                             col_types = cols(Order_Date = col_date(format = "%m/%d/%Y"))))

startDate =  min(df$Order_Date) 
endDate = max(df$Order_Date)

################################################################################
# Function
# 	getRFMvalues(df,startDate,endDate, tIDColName ="",tDateColName="",tAmountColName="")
#
# Description
#	Process the input data frame of transaction records so that the data frame can be ready for RFM scoring.
#	  A.Remove the duplicate records with the same customer ID
#	  B.Find the most recent date for each ID and calculate the days to the endDate, to get the Recency data
#	  C.Calculate the quantity of translations of a customer, to get the Frequency data
#	  D.Sum the amount of money a customer spent and divide it by Frequency, to get the average amount per transaction, that is the Monetary data.
#
# Arguments
#	  df - A data frame of transaction records with customer ID, dates, and the amount of money of each transation
#	  startDate - the start date of transaction, the records that happened after the start date will be kepted
#	  endDate - the end date of transaction, the records that happed after the end date will be removed. It works with the start date to set a time scope
#	  tIDColName - the column name which contains customer IDs in the input data frame
#	  tDateColName - the column name which contains transaction dates in the input data frame
#	  tAmountColName - the column name which contains the amount of money of each transaction in the input data frame
#
# Return Value
#	Returns a new data frame with three new columns:
#   Recency - quantity of days from the # #most recent transaction of a customer to the endDate;
#   Frequency - quantity of transactions of a customer during the period from # #startDate to endDate;
#   Monetary - the average amount of money per transaction of a customer during that period.
#################################################################################


getRFMvalues = function(df, startDate, endDate, tIDColName, tDateColName, tAmountColName){
  
  #order the dataframe by date descendingly
  df = df[order(df[,tDateColName],decreasing = TRUE),]
  
  #remove the record before the start data and after the end Date
  df = df[df[,tDateColName]>= startDate,]
  df = df[df[,tDateColName]<= endDate,]
  
  #remove the rows with the duplicated IDs, and assign the df to a new df.
  newdf = df[!duplicated(df[,tIDColName]),]
  
  # caculate the Recency(days) to the endDate, the smaller days value means more recent
  Recency = as.numeric(difftime(endDate,newdf[,tDateColName],units="days"))
  
  # add the Days column to the newdf data frame
  newdf = cbind(newdf,Recency)
  
  #order the dataframe by ID to fit the return order of table() and tapply()
  newdf =  newdf[order(newdf[,tIDColName]),]
  
  # caculate the frequency
  freq = as.data.frame(table(df[,tIDColName]))
  Frequency = freq[,2]
  newdf = cbind(newdf,Frequency)
  
  #caculate the Money per deal
  money <- as.data.frame(tapply(df[,tAmountColName],df[,tIDColName],sum))
    # sum(df[which(df$Customer_ID == "AA-10315"),]$Amount) for testing
  Monetary <- money[,1]/Frequency
  newdf <- cbind(newdf,Monetary)
  
  return(newdf)
  
} # end of function getRFMvalues


# Call the function with the given input arguments
dfwithRFMvalues = getRFMvalues(df,startDate,endDate,tIDColName ="Customer_ID",tDateColName="Order_Date",tAmountColName="Amount")


################################################################################
# Function
# 	getRFMscores(dfwithRFMvalues)
#
# Description
#	Process the input data frame of transaction records merged with the RFM values and derive the following:
#   1. Recency Score 
#   2. Frequency Score
#   3. Monetary Score
#   4. RFM Score
#
# Arguments
#	dfwithRFMvalues - data frame of transaction records merged with the RFM values such as Recency, Frequency and MOnetary
#
# Return Value
#	Returns a new data frame with four (4) new columns:
#  Recency_Score
#  Frequency_Score
#  Monetary_Score
#  RFM_Score
#
#################################################################################

getRFMscores = function(dfwithRFMvalues){
  
  
  # Allocate Recency Scores based on Recency value percentiles (highest values gets a score of 1)
  Recency_Score = as.data.frame(matrix(0,nrow(dfwithRFMvalues),1))
  colnames(Recency_Score) = "Recency_Score"
  Recency_Quantile = as.data.frame(quantile(dfwithRFMvalues$Recency,probs = seq(0.2,1,0.20)))
  
  for (i in 1: nrow(dfwithRFMvalues)){
    
    if (dfwithRFMvalues$Recency[i] <= Recency_Quantile[1,1]) {
        Recency_Score[i,1] = 5
      
      } else if (dfwithRFMvalues$Recency[i] <= Recency_Quantile[2,1]){
        Recency_Score[i,1] = 4
     
      } else if (dfwithRFMvalues$Recency[i] <= Recency_Quantile[3,1]){
        Recency_Score[i,1] = 3
        
      } else if (dfwithRFMvalues$Recency[i] <= Recency_Quantile[4,1]) {
        Recency_Score[i,1] = 2
        
      } else {
        Recency_Score[i,1] = 1
        
      } 
  }
  
  
  # Allocate Frequency Scores based on Frequency value percentiles (highest values gets a score of 5)
  Frequency_Score = as.data.frame(matrix(0,nrow(dfwithRFMvalues),1))
  colnames(Frequency_Score) = "Frequency_Score"
  Frequency_Quantile = as.data.frame(quantile(dfwithRFMvalues$Frequency,probs = seq(0.2,1,0.20)))
 
   for (i in 1: nrow(dfwithRFMvalues)){
    
    if (dfwithRFMvalues$Frequency[i] <= Frequency_Quantile[1,1]) {
      Frequency_Score[i,1] = 1
      
    } else if (dfwithRFMvalues$Frequency[i] <= Frequency_Quantile[2,1]){
      Frequency_Score[i,1] = 2
      
    } else if (dfwithRFMvalues$Frequency[i] <= Frequency_Quantile[3,1]){
      Frequency_Score[i,1] = 3
      
    } else if (dfwithRFMvalues$Frequency[i] <= Frequency_Quantile[4,1]) {
      Frequency_Score[i,1] = 4
      
    } else {
      Frequency_Score[i,1] = 5
      
    } 
    
  }
  
  
  # Allocate Monetary Scores based on Monetary value percentiles (highest values gets a score of 5)
  Monetary_Score = as.data.frame(matrix(0,nrow(dfwithRFMvalues),1))
  colnames(Monetary_Score) = "Monetary_Score"
  Monetary_Quantile = as.data.frame(quantile(dfwithRFMvalues$Monetary,probs = seq(0.2,1,0.20)))
  
  for (i in 1: nrow(dfwithRFMvalues)){
    
    if (dfwithRFMvalues$Monetary[i] <= Monetary_Quantile[1,1]) {
      Monetary_Score[i,1] = 1
      
    } else if (dfwithRFMvalues$Monetary[i] <= Monetary_Quantile[2,1]){
      Monetary_Score[i,1] = 2
      
    } else if (dfwithRFMvalues$Monetary[i] <= Monetary_Quantile[3,1]){
      Monetary_Score[i,1] = 3
      
    } else if (dfwithRFMvalues$Monetary[i] <= Monetary_Quantile[4,1]) {
      Monetary_Score[i,1] = 4
      
    } else {
      Monetary_Score[i,1] = 5
      
    } 
    
  }
  
  
  # Consolidate Recency, Frequency and Monetary Scores in one data frame
  Scores = cbind(Recency_Score, Frequency_Score, Monetary_Score)
  colnames(Scores) = c("Recency_Score", "Frequency_Score", "Monetary_Score")
  
  return(Scores)
  
}

# Call getRFMscores function given the input data frame to collect R, F and M score and assign it to the data frame "Scores"
Scores = getRFMscores(dfwithRFMvalues)

# Create separate Columns for Recency, Frequency, Monetary Score
Recency_Score = as.data.frame(Scores["Recency_Score"])
Frequency_Score = as.data.frame(Scores["Frequency_Score"])
Monetary_Score = as.data.frame(Scores["Monetary_Score"])

# Create RFM_score column
RFM_Score = as.data.frame(matrix(0,nrow(dfwithRFMvalues),1))
colnames(RFM_Score) = "RFM_Score"

# Assign RFM scores per customer or per row.
for (i in 1: nrow(dfwithRFMvalues)){
  RFM_Score[i,1] = Recency_Score[i,1]*100 + Frequency_Score[i,1]*10 + Monetary_Score[i,1]*1
}


# Create master table which includes the input df + RFM values columns + RFM scores columns
Master_table = cbind(dfwithRFMvalues,Recency_Score,Frequency_Score,Monetary_Score,RFM_Score)

  
  
 
