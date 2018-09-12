setwd("C:/Users/Bas Vervaart/Documents/Thesis clustering/Data thesis/analysis")
library(tidyverse)
#Load the data
imputation <- read.csv("raw.csv",stringsAsFactors = F)
imputation <- imputation %>% select(-X) %>% filter(!code %in% "NZL")
#Define classes
dfMiss <- imputation %>%
  dplyr::mutate_at(vars(type,coord,training,wc,wc_rights),
                   funs(as.factor(.))) %>% 
  mutate_at(vars(epr_v1,ept_v1,share_rights,min_share,sec_edu,ter_edu,tenure,stock),
            funs(as.numeric(.)))

#Overview of missing data
library(VIM)
pattern <- dfMiss %>% select(-code,-country,-year,-wc,-wc_rights, -emp_perm)
missing_na <- aggr(pattern, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(merge), 
                   cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

sum(is.na(pattern))/prod(dim(pattern))


# Mice imputation
library(mice)
impMice <- mice(dfMiss, method='cart', seed=1234)

imputedMice <- mice::complete(impMice)
for (var in colnames(imputedMice)) {
  
  attr(imputedMice[[deparse(as.name(var))]], "ATT_1") <- NULL
  
}

#
write.csv(imputedMice, "imputed.csv")



