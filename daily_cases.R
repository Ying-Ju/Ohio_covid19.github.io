# load necessary packages
library(data.table)


df <- read.csv("https://coronavirus.ohio.gov/static/COVIDSummaryData.csv")
df <- df[,-6]
colnames(df) <- c("County", "Sex", "Age_Range", "Onset_Date",         
                  "Date_Of_Death", "Case_Count",        
                  "Death_Count", "Hospitalized_Count")

# remove the last row that shows the total count and make sure the type of each variable is correct                 
df <- as.data.frame(df[1:(nrow(df)-1),])
df[,1:3] <- lapply(df[,1:3], factor)
df[,4:5] <- lapply(df[,4:5], function(x)  as.Date(x, "%m/%d/%Y"))
df[,6:8] <- lapply(df[,6:8], as.numeric)

date_sum <- table(df$Onset_Date, df$Case_Count)
daily_cases <- apply(date_sum, 1, function(x) sum(x*as.numeric(colnames(date_sum))))


path <- "C:/Users/Tessa/Documents/GitHub/Ohio_covid19.github.io"
df_previous <- read.csv(paste0(path, "/confirmed cases.csv"))
colnames(df_previous) <- c("Date", "Confirmed", "Diff_Confirmed", "Death", "Diff_Death")
add_confirmed <- sum(daily_cases)-df_previous$Confirmed[nrow(df_previous)]
add_death <- sum(df$Death_Count) - df_previous$Death[nrow(df_previous)]
myDF <- data.frame(date=format(Sys.Date(), "%m/%d/%Y"), Confirmed=sum(daily_cases), Diff_Confirmed=add_confirmed, Death=sum(df$Death_Count), Diff_Death=add_death)
write.table(myDF, paste0(path, "/confirmed cases.csv"), sep = ",", col.names = F, row.names = F, append = T)

myDF <- data.frame(date=format(Sys.Date()-1, "%m/%d/%Y"), Confirmed=105426, Diff_Confirmed=1178, Death=3755, Diff_Death=21)

#====================================================================================================================================

library(data.table)

#Data from The COVID Tracking Project.
#https://covidtracking.com/about-data

df_states <- fread("https://covidtracking.com/api/v1/states/daily.csv")
colnames(df_states)
df_states$date <- as.Date(as.character(df_states$date), "%Y%m%d")

table(df_states$state)
OH_index <- which(df_states$state=="OH")
df_OH <- df_states[OH_index,]

Tested <- data.frame(Date=df_OH$date, Tested=df_OH$totalTestResultsIncrease)
Tested
str(df_OH$date)

str(df_states$date)
index_today <- which(df_states$date=="2020-04-25")

df_previous$Date <- as.Date(df_previous$Date, format="%m/%d/%Y")
A <- merge(Tested, df_previous, by="Date")

df_NY <- df_states %>% filter(state=="NY")
df_date <- df_states %>% group_by(date) %>% summarise(average_case=mean(positiveIncrease, na.rm = TRUE))
