library(dplyr)
library(tidyr)
library(xts)
library(data.table)

########## LOAD DATA
# Edit Wages_FakeSample to have multiple workers per firm and have workers continue in multiple quarters
# load("~/Downloads/FakeWageSample.Rdata")
# write.csv(Wages_FakeSample, "~/Downloads/Edit_FakeWageSample.csv")
Data <- read.csv("/Users/zhangww/Dropbox (MIT)/research_projects/predictive_scheduling/Edit_FakeWageSample.csv")
# Data <- read.csv("C:/Users/Nathan/Dropbox/oregonps/Edit_FakeWageSample.csv")
#####################

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

# Calculate firm size
Firm_Size <- Data %>% group_by(Quarter, Sif_key) %>% summarise(Firm_Size = length(SSN))
Firm_Size$Firm_Size <- case_when(
  Firm_Size$Firm_Size < 50 ~ "< 50",
  Firm_Size$Firm_Size >= 50 & Firm_Size$Firm_Size < 100 ~ "50-99",
  Firm_Size$Firm_Size >= 100 & Firm_Size$Firm_Size < 300 ~ "100-299", 
  Firm_Size$Firm_Size >= 300 & Firm_Size$Firm_Size < 500 ~ "300-499", 
  Firm_Size$Firm_Size >= 500 & Firm_Size$Firm_Size < 1000 ~ "500-999", 
  Firm_Size$Firm_Size >= 1000 ~ ">= 1000"
)
Data <- merge(Data, Firm_Size, by = c("Quarter", "Sif_key"), all.x = TRUE)

# Worker average wages, hours, retention
worker_avg <- Data %>% group_by(Ind, Firm_Size, Quarter) %>% summarise_at(vars(Hr_Wage, WEEKS_HO, retention_flag), mean) %>% rename("Avg_Worker_Wage" = "Hr_Wage", "Avg_Worker_Ho" = "WEEKS_HO", "Avg_Worker_Retention" = "retention_flag")

# Firm average wages, hours, retention, and firm variance in hours
firm_avg <- Data %>% group_by(Ind, Firm_Size, Quarter, Sif_key) %>% summarise(Avg_Worker_Wage = mean(Hr_Wage), Avg_Worker_Ho = mean(WEEKS_HO), Var_Worker_Ho = var(WEEKS_HO), Var_Worker_Wage = var(Hr_Wage))

setDT(
  Data
)[
  ,
  .(n_retained = sum(retention_flag)),
  by = .(Sif_key, Quarter)
]

retention_outcomes =
  Data[
    ,
    .(n_workers = .N, n_retained = sum(retention_flag)),
    by = .(Sif_key, Quarter)
  ]
setkey(retention_outcomes, Sif_key, Quarter)
retention_outcomes[
  ,
  n_workers_lagged := shift(n_workers),
  Sif_key
][
  ,
  Avg_Worker_Retention := n_retained / n_workers_lagged
]

firm_avg = merge(firm_avg, retention_outcomes, by = c("Sif_key", "Quarter"))

firm_avg <- firm_avg %>% group_by(Ind, Firm_Size, Quarter) %>% summarise(Avg_Firm_Wage = mean(Avg_Worker_Wage), Avg_Firm_Ho = mean(Avg_Worker_Ho), Var_Firm_Ho = mean(Var_Worker_Ho), Var_Firm_Wage = var(Var_Worker_Wage), Avg_Firm_Retention = mean(Avg_Worker_Retention))

# Combine Firm and Worker averages
output <- merge(worker_avg, firm_avg, by = c("Ind", "Firm_Size", "Quarter"))

# Export output
