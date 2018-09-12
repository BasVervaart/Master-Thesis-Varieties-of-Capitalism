####################
library(readxl)
library(dplyr)

#Set Working directory
setwd("C:/Users/Bas Vervaart/Documents/Thesis clustering/Data thesis/analysis")
#The imputed data is all data after imputation has been done in MICE, see imputation file
imp <-read_excel("imputed.xlsx",sheet=1)
#The impavg, has all imputed data averaged over 10 year periods, NZL has been removed from the analysis
impavg <- read_excel("imputed.xlsx",sheet=2)
#The impavg, has two additional overlapping 10 year periods, NZL has been removed from the analysis
impavg.2 <- read_excel("imputed.xlsx",sheet=3)
#Merge the both imputation files
impavg <- bind_rows(impavg, impavg.2)
impavg <- impavg[with(impavg,order(code, year)),]
rm(impavg.2)

#Create summary table
sum_epr_v1 <- summary(imp$epr_v1)
sum_epr_v1["Std.Dev."] <- round(sd(imp$epr_v1),3) # add the new value

sum_ept_v1 <- summary(round(imp$ept_v1),3)
sum_ept_v1["Std.Dev."] <- round(sd(imp$ept_v1),3) # add the new value

sum_share_rights <- summary(round(imp$share_rights),3)
sum_share_rights["Std.Dev."] <- round(sd(imp$share_rights),3) # add the new value

sum_min_share <- summary(round(imp$min_share),3)
sum_min_share["Std.Dev."] <- round(sd(imp$min_share),3) # add the new value

sum_sec_edu <- summary(round(imp$sec_edu),3)
sum_sec_edu["Std.Dev."] <- round(sd(imp$sec_edu),3) # add the new value

sum_ter_edu <- summary(round(imp$ter_edu),3)
sum_ter_edu["Std.Dev."] <- round(sd(imp$ter_edu),3) # add the new value

sum_type <- summary(round(imp$type),3)
sum_type["Std.Dev."] <- round(sd(imp$type),3) # add the new value

sum_coord <- summary(round(imp$coord),3)
sum_coord["Std.Dev."] <- round(sd(imp$coord),3) # add the new value

sum_training <- summary(round(imp$training),3)
sum_training["Std.Dev."] <- round(sd(imp$training),3) # add the new value

sum_tenure <- summary(round(imp$tenure),3)
sum_tenure["Std.Dev."] <- round(sd(imp$tenure),3) # add the new value

sum_stock <- summary(round(imp$stock),3)
sum_stock["Std.Dev."] <- round(sd(imp$stock),3) # add the new value

sum_all <- rbind(sum_epr_v1,sum_ept_v1,sum_share_rights,sum_min_share,sum_sec_edu,
                 sum_ter_edu, sum_type, sum_coord, sum_training, sum_tenure, sum_stock)

sum_all <- as.data.frame(sum_all[,c(1,6,3:4,7)])
rownames(sum_all) <- c("epr_v1","ept_v1","share_rights","min_share","sec_edu","ter_edu",
                       "type","coord","training","tenure","stock")
sum_all$Min. <- round(sum_all$Min.,3)
sum_all$Mean <- round(sum_all$Mean,3)


#Setwd
setwd("C:/Users/Bas Vervaart/Documents/Thesis clustering/Data thesis/analysis/Figures")
write.csv(sum_all, "descriptive statistics.csv")

sum_all<-read.csv("descriptive statistics.csv")
