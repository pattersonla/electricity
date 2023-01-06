##------------------------------------------------------------------------------
##
## Script name: electric_rates_calculator.R
##
## Purpose of script: 
##  Calculate electricity rates for utilities using the OPEN EIA database: https://openei.org/wiki/Utility_Rate_Database for fixed rates
##  AND EIA data to get the average cost per kwh from here: https://www.eia.gov/electricity/sales_revenue_price/ (updates each October)
##
## Author: Lauren Patterson
##
## Date Created: May 2022
##
##
##------------------------------------------------------------------------------

remove(list = ls())  # clear all workspace variables
cat("\014")          # clear command line

## load packages
library(here)
library(tidyverse)
library(readxl)
library(stringr)
library(sf)

`%notin%` = function(x,y) !(x %in% y); #function to get what is not in the list


#read in electricity data - must download eia data
res.data <- read.csv(here::here('data','usurdb_2021.csv'))
length(unique(res.data$eiaid))

#convert dates
res.data <- res.data %>% filter(sector == "Residential") %>%
  mutate(startdate = as.Date(substr(startdate,1,10), format="%Y-%m-%d"), enddate = as.Date(substr(enddate,1,10), format="%Y-%m-%d"))
length(unique(res.data$eiaid))
table(res.data$fixedchargeunits, useNA="ifany")
fixed.rates <- res.data %>% group_by(eiaid, utility, fixedchargeunits) %>% summarize(fixed_charge = round(mean(fixedchargefirstmeter, na.rm=TRUE),2), .groups="drop")
fixed.rates <- fixed.rates %>% mutate(fixed_charge = ifelse(fixedchargeunits=="$/day", round(fixed_charge*30.25, 2), fixed_charge))
summary(fixed.rates)


#read in EIA data
eia <- read_excel(here::here('data','eia','Sales_Ult_Cust_2020.xlsx'), sheet="States", range="A3:P2834")
colnames(eia) <- c("year", "eiaid","name","delete","part","service_type","data_type","state","ownership","BA_code",
                   "revenue_thousands","megawatthrs","customers","cost_per_kwh","bill_500_kwh","average_monthly_kwh")
  
eia <- eia %>% select(-year, -delete, -part, -data_type) %>% 
  mutate(revenue_thousands = as.numeric(revenue_thousands), megawatthrs = as.numeric(megawatthrs), customers = as.numeric(customers)) %>% 
  filter(eiaid %notin% c("88888","99999")) %>% filter(is.na(revenue_thousands)==FALSE)

#clean up zeros and other missing data
eia <- eia %>% filter(revenue_thousands > 1 & customers > 100 & megawatthrs > 1)
summary(eia)  

#calculate cost per hour
kwh.charge <- eia %>% mutate(revenue = revenue_thousands*1000, kwh = megawatthrs*1000, ave_cost = round(revenue/kwh,4))

#add in fixed charge
#group fixed charge by eia - taking average
fixed.rates2 <- fixed.rates %>% group_by(eiaid) %>% summarize(fixed_cost = mean(fixed_charge, na.rm=TRUE))


kwh.charge <- merge(kwh.charge, fixed.rates2, by="eiaid", all.x=TRUE) 
kwh.charge <- kwh.charge %>% mutate(fixed_cost = ifelse(is.na(fixed_cost)==TRUE, 0, fixed_cost))
#remove those that are more than $100 - assume an error (get negative revenues)
kwh.charge <- kwh.charge %>% mutate(fixed_cost = ifelse(fixed_cost>100, 0, fixed_cost))
kwh.charge <- kwh.charge %>% mutate(new_revenue = revenue - (fixed_cost*12)*customers) %>% 
  mutate(new_cost = round(new_revenue/kwh,4))

kwh.charge <- kwh.charge %>% group_by(eiaid, name) %>% summarize(fixed_cost = mean(fixed_cost, na.rm=TRUE),
                                                                 ave_cost = mean(new_cost, na.rm=TRUE), .groups="drop")

#Loop through and calculate utility charge 
kwh_use <- seq(0,1600,100)  
#loop through and add columns
i=1
for(i in 1:length(kwh_use)){
  sel_use <- kwh_use[i]
  kwh.charge[,paste0("kwh_cost_",sel_use)] <- kwh.charge$ave_cost*sel_use
  kwh.charge[,paste0("total_cost_",sel_use)] <- kwh.charge[,paste0("kwh_cost_",sel_use)] + kwh.charge$fixed_cost
}

#transform into a long table
results <- kwh.charge %>% gather(key="variable", value="total_cost", -c("eiaid","name","fixed_cost","ave_cost"))
results <- results %>% mutate(hh_use = as.numeric(str_extract_all(variable, "[0-9]+")), 
                              column = ifelse(substr(variable,1,3)=="kwh", "energy_cost", "total_cost"))  
results <- results %>% pivot_wider(id_cols = c("eiaid","name","fixed_cost","ave_cost","hh_use"), 
                                   names_from = c(column), values_from = total_cost)

#save out bills
write.csv(results, here::here("results","estimated_energy_bills.csv"))

gis.sys <- e.sys %>% filter(ID %in% results$eiaid) %>% select(ID, NAME, CUSTOMERS, YEAR)



#LETS SEE WHAT TIERS Look like
table(res.data$energyratestructure.period0.tier0max, useNA="ifany")
table(res.data$energyratestructure.period0.tier1max, useNA="ifany")
  
#read in most recent rate
sel.rates <- subset(res.data, eiaid=="5416")
sel.rates <- sel.rates %>% group_by(eiaid, utility, sector) %>% filter(startdate >= max(startdate))

#Can use this to get the fixed charge and then combine with average kwh from table 6 or try to calculate

fixed.rates <- sel.rates %>% group_by(eiaid, utility, fixedchargeunits) %>% summarize(fixed_charge = round(mean(fixedchargefirstmeter, na.rm=TRUE),2), .groups="drop")


kwh = 1000

#energyratestructure.period0.tier0max = max kilowatt hrs in tier
#energyratesructure.period0.tier0rate = cost per kwh in tier
#energyratestructure.period0.tier0adj = changes to cost
period0 <- sel.rates %>% select(starts_with("energyratestructure.period0")) %>% select(-ends_with("sell")) %>% 
  




sel.rates$fixedchargefirstmeter
sel.rates %>% select(name, startdate, enddate, fixedchargefirstmeter) %>% as.data.frame()
zt <- res.data %>% filter(eiaid =="3046")

