library(dplyr)
library(tidyr)
library(xts)

# Edit Wages_FakeSample to have multiple workers per firm and have workers continue in multiple quarters
load("~/Downloads/FakeWageSample.Rdata")
# write.csv(Wages_FakeSample, "~/Downloads/Edit_FakeWageSample.csv")
Data <- read.csv("/Users/zhangww/Dropbox (MIT)/research_projects/predictive_scheduling/Edit_FakeWageSample.csv")

# Read quarters as date variable 
Data$Quarter <- as.yearqtr(gsub('(.{4})', '\\1 ', as.character(Data$Quarter)), format = "%Y %q")

# Generate dummy for if the worker was at the firm 1 year ago 
Lag_Data <- Data %>% select(c("Quarter", "Sif_key", "SSN"))
Lag_Data$Quarter_tplus1 <- Lag_Data$Quarter + 0.25
Lag_Data$retention_flag <- 1
Data <- merge(Data, Lag_Data, by.x = c("Quarter", "Sif_key", "SSN"), by.y = c("Quarter_tplus1", "Sif_key", "SSN"), all.x = TRUE)
Data$retention_flag[is.na(Data$retention_flag)] <- 0

# Create Ind <- variable for 2 digit NAICS 
Data$Ind <- floor(Data$NAICS/10000)

# Calculate hourly wage 
Data$Hr_Wage <- Data$WAGE_AMT / Data$WEEKS_HO

# Calculate firm size and number of retained work
Firm_Size <- Data %>% group_by(Quarter, Sif_key) %>% summarise(Firm_Size = length(SSN))
Firm_Size$Firm_Size <- ifelse(Firm_Size$Firm_Size < 2, 1, 2) # TODO: Change to what we want the cutoffs to actually be
Data <- merge(Data, Firm_Size, by = c("Quarter", "Sif_key"), all.x = TRUE)

# Worker average wages, hours, retention
worker_avg <- Data %>% group_by(Ind, Firm_Size, Quarter) %>% summarise_at(vars(Hr_Wage, WEEKS_HO, retention_flag), mean) %>% rename("Avg_Worker_Wage" = "Hr_Wage", "Avg_Worker_Ho" = "WEEKS_HO", "Avg_Worker_Retention" = "retention_flag") 

# Firm average wages, hours, retention, and firm variance in hours
firm_avg <- Data %>% group_by(Ind, Firm_Size, Quarter, Sif_key) %>% summarise(Avg_Worker_Wage = mean(Hr_Wage), Avg_Worker_Ho = mean(WEEKS_HO), Var_Worker_Ho = var(WEEKS_HO), Var_Worker_Wage = var(Hr_Wage), Avg_Worker_Retention = mean(retention_flag))
firm_avg <- firm_avg %>% group_by(Ind, Firm_Size, Quarter) %>% summarise(Avg_Firm_Wage = mean(Avg_Worker_Wage), Avg_Firm_Ho = mean(Avg_Worker_Ho), Var_Firm_Ho = mean(Var_Worker_Ho), Var_Firm_Wage = var(Var_Worker_Wage), Avg_Firm_Retention = mean(Avg_Worker_Retention))

# Combine Firm and Worker averages 
output <- merge(worker_avg, firm_avg, by = c("Ind", "Firm_Size", "Quarter"))

# Export output