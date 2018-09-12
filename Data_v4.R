w#Packages
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

##########
#Analysis#
##########


# We investigate the dimensions between Industrial Relations vs Training Systems (train)
# And Industrial Relations vs Corporate governance, let's use some easier objectnames (corp)
# The variable Year refers to the last year in the 10 year period as before
# So 1993 = 1984-1993, 1998 = 1989-1998 etc. 

train93 <- impavg %>% select(code,year,epr_v1,ept_v1,type,coord,tenure,sec_edu,ter_edu,training) %>% filter(year==1993)
train98 <- impavg %>% select(code,year,epr_v1,ept_v1,type,coord,tenure,sec_edu,ter_edu,training) %>% filter(year==1998)
train03 <- impavg %>% select(code,year,epr_v1,ept_v1,type,coord,tenure,sec_edu,ter_edu,training) %>% filter(year==2003)
train08 <- impavg %>% select(code,year,epr_v1,ept_v1,type,coord,tenure,sec_edu,ter_edu,training) %>% filter(year==2008)
train13 <- impavg %>% select(code,year,epr_v1,ept_v1,type,coord,tenure,sec_edu,ter_edu,training) %>% filter(year==2013)

corp93 <- impavg %>% select(code,year,epr_v1,ept_v1,type,coord,tenure,share_rights,min_share,stock) %>% filter(year==1993)
corp98 <- impavg %>% select(code,year,epr_v1,ept_v1,type,coord,tenure,share_rights,min_share,stock) %>% filter(year==1998)
corp03 <- impavg %>% select(code,year,epr_v1,ept_v1,type,coord,tenure,share_rights,min_share,stock) %>% filter(year==2003)
corp08 <- impavg %>% select(code,year,epr_v1,ept_v1,type,coord,tenure,share_rights,min_share,stock) %>% filter(year==2008)
corp13 <- impavg %>% select(code,year,epr_v1,ept_v1,type,coord,tenure,share_rights,min_share,stock) %>% filter(year==2013)

#And set the rownames 
rownames <- c("AUS","AUT","BEL",
              "CAN","CHE", "DEU",
              "DNK","ESP","FIN",
              "FRA","GBR", "GRC",
              "IRL", "ITA","JPN",
              "NLD", "NOR","PRT", 
              "SWE","USA")
rownames(train93)<- rownames
rownames(train98) <- rownames
rownames(train03)<- rownames 
rownames(train08)<- rownames
rownames(train13)<- rownames
rownames(corp93)<- rownames
rownames(corp98)<- rownames
rownames(corp03)<- rownames
rownames(corp08)<- rownames
rownames(corp13)<- rownames

#Take the principal components
#PCA for training sytem
pca.train93 <- prcomp(cbind(train93$sec_edu,train93$ter_edu, train93$training), retx=T, scale=T)
pca.train98 <- prcomp(cbind(train98$sec_edu,train98$ter_edu, train98$training), retx=T, scale=T) 
pca.train03 <- prcomp(cbind(train03$sec_edu,train03$ter_edu, train03$training), retx=T, scale=T)
pca.train08 <- prcomp(cbind(train08$sec_edu,train08$ter_edu, train08$training), retx=T, scale=T)
pca.train13 <- prcomp(cbind(train13$sec_edu,train13$ter_edu, train13$training), retx=T, scale=T)
#PCA for Industrial Relations
pca.ir93 <- prcomp(cbind(train93$type,train93$coord,train93$tenure, train93$epr_v1, train93$ept_v1),retx=T,scale=T)
pca.ir98 <- prcomp(cbind(train98$type,train98$coord,train98$tenure, train98$epr_v1, train98$ept_v1),retx=T,scale=T)
pca.ir03 <- prcomp(cbind(train03$type,train03$coord,train03$tenure, train03$epr_v1, train03$ept_v1),retx=T,scale=T)
pca.ir08 <- prcomp(cbind(train08$type,train08$coord,train08$tenure, train08$epr_v1, train08$ept_v1),retx=T,scale=T)
pca.ir13 <- prcomp(cbind(train13$type,train13$coord,train13$tenure, train13$epr_v1, train13$ept_v1),retx=T,scale=T)
#PCA for Corporate Governance
pca.corp93 <- prcomp(cbind(corp93$share_rights, corp93$min_share, corp93$stock),retx=T,scale=T)
pca.corp98 <- prcomp(cbind(corp98$share_rights, corp98$min_share, corp98$stock),retx=T,scale=T)
pca.corp03 <- prcomp(cbind(corp03$share_rights, corp03$min_share, corp03$stock),retx=T,scale=T)
pca.corp08 <- prcomp(cbind(corp08$share_rights, corp08$min_share, corp08$stock),retx=T,scale=T)
pca.corp13 <- prcomp(cbind(corp13$share_rights, corp13$min_share, corp13$stock),retx=T,scale=T)

#Load packages
library(factoextra)
library(mclust)
library(clustvarsel)

#Select PCs for clustering

select<-clustvarsel(pca.train93$x, G=1:8) #PC1 #PC2 #PC3
select<-clustvarsel(pca.train98$x, G=1:8) #PC1 #PC2 #PC3
select<-clustvarsel(pca.train03$x, G=1:8) #PC1 #PC2 #PC3
select<-clustvarsel(pca.train08$x, G=1:8) #PC1 #PC2 #PC3
select<-clustvarsel(pca.train13$x, G=1:8) #PC1 #PC2 #PC3

select<-clustvarsel(pca.ir93$x, G=1:8) #PC1 #PC4
select<-clustvarsel(pca.ir98$x, G=1:8) #PC4 #PC5 #PC2
select<-clustvarsel(pca.ir03$x, G=1:8) #PC5 #PC1 #PC2
select<-clustvarsel(pca.ir08$x, G=1:8) #PC1 #PC5 #PC2 #PC4
select<-clustvarsel(pca.ir13$x, G=1:8) #PC1 #PC2 #PC3

select<-clustvarsel(pca.corp93$x, G=1:8) #PC1 #PC2 #PC3
select<-clustvarsel(pca.corp98$x, G=1:8) #PC2 #PC3
select<-clustvarsel(pca.corp03$x, G=1:8) #PC2 #PC3
select<-clustvarsel(pca.corp08$x, G=1:8) #PC2 #PC3
select<-clustvarsel(pca.corp13$x, G=1:8) #PC1 #PC2 #PC3


#Take the selected principal components and bind them into a new list

#List Industrial Relations vs Training Systems, 1984-1993
irtrain93<-cbind(pca.ir93$x[,c(1,4)],pca.train93$x[,c(1:3)])
colnames(irtrain93)<-c("PC1 ir","PC4 ir", "PC1 train", "PC2 train","PC3 train")
#List Industrial Relations vs Training Systems, 1989-1998
irtrain98<-cbind(pca.ir98$x[,c(4,5,2)],pca.train98$x[,c(1:3)])
colnames(irtrain98)<-c("PC4 ir","PC5 ir", "PC2 ir","PC1 train", "PC2 train","PC3 train")
#List Industrial Relations vs Training Systems, 1994-2003
irtrain03<-cbind(pca.ir03$x[,c(5,1,2)],pca.train03$x[,c(1:3)])
colnames(irtrain03)<-c("PC5 ir","PC1 ir","PC2 ir", "PC1 train", "PC2 train","PC3 train")
#List Industrial Relations vs Training Systems, 1999-2008
irtrain08<-cbind(pca.ir08$x[,c(1,5,2,4)],pca.train08$x[,c(1:3)])
colnames(irtrain08)<-c("PC1 ir","PC5 ir","PC2 ir","PC4 ir", "PC1 train", "PC2 train","PC3 train")
#List Industrial Relations vs Training Systems, 2004-2013
irtrain13<-cbind(pca.ir13$x[,c(1,2,3)],pca.train13$x[,c(1:3)])
colnames(irtrain13)<-c("PC1 ir","PC2 ir","PC3 ir", "PC1 train", "PC2 train","PC3 train")


#List Industrial Relations vs Corporate Governance, 1984-1993
ircorp93<-cbind(pca.ir93$x[,c(1,4)],pca.corp93$x[,c(1:3)])
colnames(ircorp93)<-c("PC1 ir","PC4 ir", "PC1 corp", "PC2 corp","PC3 corp")
#List Industrial Relations vs Corporate Governance, 1989-1998
ircorp98<-cbind(pca.ir98$x[,c(4,5,2)],pca.corp98$x[,c(2,3)])
colnames(ircorp98)<-c("PC4 ir","PC5 ir", "PC2 ir","PC2 corp","PC3 corp")
#List Industrial Relations vs Corporate Governance, 1994-2003
ircorp03<-cbind(pca.ir03$x[,c(5,1,2)],pca.corp03$x[,c(2,3)])
colnames(ircorp03)<-c("PC5 ir","PC1 ir","PC2 ir", "PC2 corp","PC3 corp")
#List Industrial Relations vs Corporate Governance, 1999-2008
ircorp08<-cbind(pca.ir08$x[,c(1,5,2,4)],pca.corp08$x[,c(2,3)])
colnames(ircorp08)<-c("PC1 ir","PC5 ir","PC2 ir","PC4 ir", "PC2 corp","PC3 corp")
#List Industrial Relations vs Corporate Governance, 2004-2013
ircorp13<-cbind(pca.ir13$x[,c(1,2,3)],pca.corp13$x[,c(1:3)])
colnames(ircorp13)<-c("PC1 ir","PC2 ir","PC3 ir", "PC1 corp","PC2 corp","PC3 corp")



#Run the MBC model on the principal components, with prior
mbc.irtrain93<-Mclust(irtrain93,G=1:8,warn=T,prior=priorControl())
mbc.irtrain98<-Mclust(irtrain98,G=1:8,warn=T,prior=priorControl())
mbc.irtrain03<-Mclust(irtrain03,G=1:8,warn=T,prior=priorControl())
mbc.irtrain08<-Mclust(irtrain08,G=1:8,warn=T,prior=priorControl())
mbc.irtrain13<-Mclust(irtrain13,G=1:8,warn=T,prior=priorControl())


mbc.ircorp93<-Mclust(ircorp93,G=1:8,warn=T,prior=priorControl())
mbc.ircorp98<-Mclust(ircorp98,G=1:8,warn=T,prior=priorControl())
mbc.ircorp03<-Mclust(ircorp03,G=1:8,warn=T,prior=priorControl())
mbc.ircorp08<-Mclust(ircorp08,G=1:8,warn=T,prior=priorControl())
mbc.ircorp13<-Mclust(ircorp13,G=1:8,warn=T,prior=priorControl())


##Cluster classifications
#Industrial relations vs Training Systems
summary(mbc.irtrain93)
mbc.irtrain93$z
class_irtrain_93<-cbind(train93$code, mbc.irtrain93$classification)

summary(mbc.irtrain98)
mbc.irtrain98$z
class_irtrain_98<-cbind(train98$code, mbc.irtrain98$classification)

summary(mbc.irtrain03)
mbc.irtrain03$z
class_irtrain_03<-cbind(train03$code, mbc.irtrain03$classification)

summary(mbc.irtrain08)
mbc.irtrain08$z
class_irtrain_08<-cbind(train08$code, mbc.irtrain08$classification)

summary(mbc.irtrain13)
mbc.irtrain13$z
class_irtrain_13<-cbind(train13$code, mbc.irtrain13$classification)

#Industrial relations vs Corporate Governance
summary(mbc.ircorp93)
mbc.ircorp93$z
class_ircorp_93<-cbind(corp93$code, mbc.ircorp93$classification)

summary(mbc.ircorp98)
mbc.ircorp98$z
class_ircorp_98<-cbind(corp98$code, mbc.ircorp98$classification)

summary(mbc.ircorp03)
mbc.ircorp03$z
class_ircorp_03<-cbind(corp03$code, mbc.ircorp03$classification)

summary(mbc.ircorp08)
mbc.ircorp08$z
class_ircorp_08<-cbind(corp08$code, mbc.ircorp08$classification)

summary(mbc.ircorp13)
mbc.ircorp13$z
class_ircorp_13<-cbind(corp13$code, mbc.ircorp13$classification)


#Check BIC differences for analysis!
mbc.irtrain93$BIC #Diff ~ 3.8 for VII 4 and VII 3
mbc.irtrain98$BIC #Diff ~ 0.9 for VII 8 and VII 3
mbc.irtrain03$BIC #Diff ~ 5.5 for VII 3 and VEI 3
mbc.irtrain08$BIC #Diff ~ 1.1 for VII 8 and VII 5
mbc.irtrain13$BIC #Diff ~ 0.9 for VII 3 and VII 4

mbc.ircorp93$BIC #Diff ~ 1.6 for VII 3 and VII 2
mbc.ircorp98$BIC #Diff = 0 for EII 1 and VII 1 
mbc.ircorp03$BIC #Diff = 0 for EEI 1, EVI 1 and VEI 1
mbc.ircorp08$BIC #Diff ~ 1 for VII 5 an VII 4
mbc.ircorp13$BIC #Diff ~ 2.6 for EII 2 and VII 3 

#Create table for classification Industrial relations vs Training Systems
class_all_irtrain <- as.data.frame(cbind(train93$code,mbc.irtrain93$classification,
                                 mbc.irtrain98$classification, mbc.irtrain03$classification,
                                 mbc.irtrain08$classification, mbc.irtrain13$classification))
model_irtrain <- as.character(c("","VII","VII","VII","VII","VII",""))
VoC <- c("LME","CME","CME","LME","CME","CME", "CME", "MED", 
         "CME","MED","LME","MED","LME","MED","CME","CME",
         "CME","MED","CME","LME")
class_all_irtrain <- cbind(class_all_irtrain,VoC)
bic_irtrain <- c("",-284,-307,-346,-405,-361,"")
clusters_irtrain<- c("",4,8,3,8,3,"")
#Merge
indx <- sapply(class_all_irtrain, is.factor)
class_all_irtrain[indx] <- lapply(class_all_irtrain[indx], as.character)
class_all_irtrain <- rbind(class_all_irtrain,clusters_irtrain,model_irtrain,bic_irtrain)
class_all_irtrain[c(21:23),1] <- c("No. Clusters","Model","BIC")
colnames(class_all_irtrain) <- c("Country","84-93","89-98","94-03","99-08","04-13","VoC")


##Create table for classification Industrial relations vs Corporate governance

class_all_ircorp<- as.data.frame(cbind(corp93$code,mbc.ircorp93$classification,
                                         mbc.ircorp98$classification, mbc.ircorp03$classification,
                                         mbc.ircorp08$classification, mbc.ircorp13$classification))
class_all_ircorp <- cbind(class_all_ircorp,VoC)
model_ircorp <- as.character(c("","VII","EII","EEI","VII","EII",""))
bic_ircorp <- c("",-304,-262,-287,-318,-373,"")
clusters_ircorp<- c("",3,1,1,5,2,"")
# Merge
indx <- sapply(class_all_ircorp, is.factor)
class_all_ircorp[indx] <- lapply(class_all_ircorp[indx], as.character)
class_all_ircorp <- rbind(class_all_ircorp,clusters_ircorp,model_ircorp,bic_ircorp)
class_all_ircorp[c(21:23),1] <- c("No. Clusters","Model","BIC")
colnames(class_all_ircorp) <- c("Country","84-93","89-98","94-03","99-08","04-13","VoC")

#Set wd for where the pdfs end up
setwd("C:/Users/Bas Vervaart/Documents/Thesis clustering/Data thesis/analysis/figures")

#Create PDF for Industrial Relations vs Training systems BIC plots
pdf("Figure_BIC_IRvsTrain_v3.pdf",height = 5, width = 14, onefile = T,paper = "special") 
par(mfrow=c(1,5),mar=c(4.6, 4.1, 3.6, 0.6))
#BIC Plot
plot.mclustBIC(mbc.irtrain93$BIC,legendArgs = list(x="bottomleft", ncol=4,cex=.75), ylim=c(-380,-280))
title(main="BIC 84-93")
abline(h=mbc.irtrain93$bic-10, lty=2, lwd=2)
plot.mclustBIC(mbc.irtrain98$BIC,legendArgs = list(x="bottomleft", ncol=4,cex=.75), ylim=c(-400,-300))
title(main="BIC 89-98")
abline(h=mbc.irtrain98$bic-10, lty=2, lwd=2)
plot.mclustBIC(mbc.irtrain03$BIC,legendArgs = list(x="bottomleft", ncol=4,cex=.75), ylim=c(-440,-340))
title(main="BIC 94-03")
abline(h=mbc.irtrain03$bic-10, lty=2, lwd=2)
plot.mclustBIC(mbc.irtrain08$BIC,legendArgs = list(x="bottomleft", ncol=4,cex=.75), ylim=c(-500,-400))
title(main="BIC 99-08")
abline(h=mbc.irtrain08$bic-10, lty=2, lwd=2)
plot.mclustBIC(mbc.irtrain13$BIC,legendArgs = list(x="bottomleft", ncol=4,cex=.75), ylim=c(-460,-360))
title(main="BIC 04-13")
abline(h=mbc.irtrain13$bic-10, lty=2, lwd=2)
dev.off()

#Create PDF for Industrial Relations vs Coporate Governance BIC plots
pdf("Figure_BIC_IRvsCORP.pdf",height = 5, width = 14, onefile = T,paper = "special") 
par(mfrow=c(1,5),mar=c(4.6, 4.1, 3.6, 0.6))
#BIC Plot
plot.mclustBIC(mbc.ircorp93$BIC,legendArgs = list(x="bottomleft", ncol=4,cex=.75), ylim=c(-400,-300))
title(main="BIC 84-93")
abline(h=mbc.ircorp93$bic-10, lty=2, lwd=2)
plot.mclustBIC(mbc.ircorp98$BIC,legendArgs = list(x="bottomleft", ncol=4,cex=.75), ylim=c(-360,-260))
title(main="BIC 89-98")
abline(h=mbc.ircorp98$bic-10, lty=2, lwd=2)
plot.mclustBIC(mbc.ircorp03$BIC,legendArgs = list(x="bottomleft", ncol=4,cex=.75), ylim=c(-380,-280))
title(main="BIC 94-03")
abline(h=mbc.ircorp03$bic-10, lty=2, lwd=2)
plot.mclustBIC(mbc.ircorp08$BIC,legendArgs = list(x="bottomleft", ncol=4,cex=.75), ylim=c(-410,-310))
title(main="BIC 99-08")
abline(h=mbc.ircorp08$bic-10, lty=2, lwd=2)
plot.mclustBIC(mbc.ircorp13$BIC,legendArgs = list(x="bottomleft", ncol=4,cex=.75), ylim=c(-470,-370))
title(main="BIC 04-13")
abline(h=mbc.ircorp13$bic-10, lty=2, lwd=2)
dev.off()

#ICL criterion for robustness

#ICL Criterium

irtrain93_ICL<-mclustICL(irtrain93,G=1:8) #EVI 2
irtrain98_ICL<-mclustICL(irtrain98,G=1:8) #VII 3
irtrain03_ICL<-mclustICL(irtrain03,G=1:8) #VII 3
irtrain08_ICL<-mclustICL(irtrain08,G=1:8) #EEI 1
irtrain13_ICL<-mclustICL(irtrain13,G=1:8) #EEV 2

ircorp93_ICL<-mclustICL(ircorp93,G=1:8) #VII 3
ircorp98_ICL<-mclustICL(ircorp98,G=1:8) #EEV 4
ircorp03_ICL<-mclustICL(ircorp03,G=1:8) #VEI 2
ircorp08_ICL<-mclustICL(ircorp08,G=1:8) #VEI 2
ircorp13_ICL<-mclustICL(ircorp13,G=1:8) #VEI 3

#ICL Table Multiple dimensions

model_icl <- c("EVI","VII","VII","EEI","EEV","VII","EEV","VEI","VEI","VEI")
clusters_icl <- c(2,3,3,1,2,3,4,2,2,3)
icl_icl <- c(-278,-304,-342,-395,-346,-301,-185,-278,-318,-366)
icl_table <- rbind(clusters_icl,model_icl,icl_icl)
colnames(icl_table) <- c("IRTRAIN93","IRTRAIN98","IRTRAIN03","IRTRAIN08","IRTRAIN13",
                          "IRCORP93","IRCORP98","IRCORP03","IRCORP08","IRCORP13")
rownames(icl_table) <- c("No. clusters","Model","ICL")
icl_table <- as.data.frame(icl_table)

#####################################################################
#####################################################################
#####################################################################
# Perform the analysis on the first principal components

#Screeplot

screeplot(pca.ir93)
screeplot(pca.ir98)
screeplot(pca.ir03)
screeplot(pca.ir08)
screeplot(pca.ir13)

screeplot(pca.train93)
screeplot(pca.train98)
screeplot(pca.train03)
screeplot(pca.train08)
screeplot(pca.train13)

screeplot(pca.corp93)
screeplot(pca.corp98)
screeplot(pca.corp03)
screeplot(pca.corp08)
screeplot(pca.corp13)


#Take first Principal Component of the different dimensions and bind them into a new list
#List Industrial Relations vs Training Systems, 1984-1993
irtrain93<-cbind(pca.ir93$x[,1],pca.train93$x[,1])
colnames(irtrain93)<-c("Industrial Relations","Training Systems")
#List Industrial Relations vs Training Systems, 1989-1998
irtrain98<-cbind(pca.ir98$x[,1],pca.train98$x[,1])
colnames(irtrain98)<-c("Industrial Relations","Training Systems")
#List Industrial Relations vs Training Systems, 1994-2003
irtrain03<-cbind(pca.ir03$x[,1],pca.train03$x[,1])
colnames(irtrain03)<-c("Industrial Relations","Training Systems")
#List Industrial Relations vs Training Systems, 1999-2008
irtrain08<-cbind(pca.ir08$x[,1],pca.train08$x[,1])
colnames(irtrain08)<-c("Industrial Relations","Training Systems")
#List Industrial Relations vs Training Systems, 2004-2013
irtrain13<-cbind(pca.ir13$x[,1],pca.train13$x[,1])
colnames(irtrain13)<-c("Industrial Relations","Training Systems")


#Run the MMBC model
#Using a prior, singular covariance for all models, up to 8 models
mbc.irtrain93<-Mclust(irtrain93,G=1:8,warn=T,prior=priorControl())
mbc.irtrain98<-Mclust(irtrain98,G=1:8,warn=T,prior=priorControl())
mbc.irtrain03<-Mclust(irtrain03,G=1:8,warn=T,prior=priorControl())
mbc.irtrain08<-Mclust(irtrain08,G=1:8,warn=T,prior=priorControl())
mbc.irtrain13<-Mclust(irtrain13,G=1:8,warn=T,prior=priorControl())

#Classification countries
mbc.irtrain93$z
mbc.irtrain98$z
mbc.irtrain03$z
mbc.irtrain08$z
mbc.irtrain13$z

#BIC values
mbc.irtrain93$BIC #Diff ~ 0.6 for EVI 2 and EEI 2 
mbc.irtrain98$BIC #Diff = 0 for EEE 1 and EEV 1
mbc.irtrain03$BIC #Diff ~ 0.5 for EEE 7 and EEI 3
mbc.irtrain08$BIC #Diff ~ 0.1 for EEI 3 and EEI 7
mbc.irtrain13$BIC #Diff ~ 0.9 for EII 3 and EII 4


#Set wd for where the pdfs end up
setwd("C:/Users/Bas Vervaart/Documents/Thesis clustering/Data thesis/analysis/figures")

#Create PDF for Industrial Relations vs Training systems 1984-1993
pdf("Figure_irtrain93_v2.pdf",height = 5, width = 14, onefile = T,paper = "special") 
par(mfrow=c(1,3),mar=c(4.6, 4.1, 3.6, 0.6))
#2D plot of country classification 
mclust2Dplot(irtrain93,parameters=mbc.irtrain93$parameters,z=mbc.irtrain93$z,
             what="classification", colors="white")
text(irtrain93[,1], irtrain93[,2],rownames(train93),col=mbc.irtrain93$classification,font=1)
title(main="Classification 2D plot 1984-1993")
#Uncertainty plot
coordProj(irtrain93,parameters=mbc.irtrain93$parameters,z=mbc.irtrain93$z,
          what="uncertainty", uncertainty=mbc.irtrain93$uncertainty)
text(irtrain93[,1], irtrain93[,2],rownames(train93),font=1)
title(main="Uncertainty classifications 1984-1993")
#BIC Plot
plot.mclustBIC(mbc.irtrain93$BIC,legendArgs = list(x="bottomleft", ncol=4,cex=.75), ylim=c(-220,-145))
title(main="BIC model comparison 1984-1993")
abline(h=mbc.irtrain93$bic-10, lty=2, lwd=2)
dev.off()

#Create PDF for Industrial Relations vs Training systems 1989-1998
pdf("Figure_irtrain98_v2.pdf",height = 5, width = 14, onefile = T,paper = "special") 
par(mfrow=c(1,3),mar=c(4.6, 4.1, 3.6, 0.6))
#2D plot of country classification 
mclust2Dplot(irtrain98,parameters=mbc.irtrain98$parameters,z=mbc.irtrain98$z,
             what="classification", colors="white")
text(irtrain98[,1], irtrain98[,2],rownames(train98),col=mbc.irtrain98$classification,font=1)
title(main="Classification 2D plot 1989-1998")
#BIC Plot
plot.mclustBIC(mbc.irtrain98$BIC,legendArgs = list(x="bottomleft", ncol=4,cex=.75), ylim=c(-220,-150))
title(main="BIC model comparison 1989-1998")
abline(h=mbc.irtrain98$bic-10, lty=2, lwd=2)
dev.off()

#Create PDF for Industrial Relations vs Training systems 1994-2003
pdf("Figure_irtrain03_v2.pdf",height = 5, width = 14, onefile = T,paper = "special") 
par(mfrow=c(1,3),mar=c(4.6, 4.1, 3.6, 0.6))
#2D plot of country classification 
mclust2Dplot(irtrain03,parameters=mbc.irtrain03$parameters,z=mbc.irtrain03$z,
             what="classification", colors="white")
text(irtrain03[,1], irtrain03[,2],rownames(train03),col=mbc.irtrain03$classification,font=1)
title(main="Classification 2D plot 1994-2003")
#Uncertainty plot
coordProj(irtrain03,parameters=mbc.irtrain03$parameters,z=mbc.irtrain03$z,
          what="uncertainty", uncertainty=mbc.irtrain03$uncertainty)
text(irtrain03[,1], irtrain03[,2],rownames(train03),font=1)
title(main="Uncertainty classifications 1994-2003")
#BIC Plot
plot.mclustBIC(mbc.irtrain03$BIC,legendArgs = list(x="bottomleft", ncol=4,cex=.75), ylim=c(-200,-150))
title(main="BIC model comparison 1994-2003")
abline(h=mbc.irtrain03$bic-10, lty=2, lwd=2)
dev.off()

#Create PDF for Industrial Relations vs Training systems 1999-2008
pdf("Figure_irtrain08_v2.pdf",height = 5, width = 14, onefile = T,paper = "special") 
par(mfrow=c(1,3),mar=c(4.6, 4.1, 3.6, 0.6))
#2D plot of country classification 
mclust2Dplot(irtrain08,parameters=mbc.irtrain08$parameters,z=mbc.irtrain08$z,
             what="classification", colors="white")
text(irtrain08[,1], irtrain08[,2],rownames(train08),col=mbc.irtrain08$classification,font=1)
title(main="Classification 2D plot 1999-2008")
#Uncertainty plot
coordProj(irtrain08,parameters=mbc.irtrain08$parameters,z=mbc.irtrain08$z,
          what="uncertainty", uncertainty=mbc.irtrain08$uncertainty)
text(irtrain08[,1], irtrain08[,2],rownames(train08),font=1)
title(main="Uncertainty classifications 1999-2008")
#BIC Plot
plot.mclustBIC(mbc.irtrain08$BIC,legendArgs = list(x="bottomleft", ncol=4,cex=.75), ylim=c(-210,-150))
title(main="BIC model comparison 1999-2008")
abline(h=mbc.irtrain08$bic-10, lty=2, lwd=2)
dev.off()

#Create PDF for Industrial Relations vs Training systems 2004-2013
pdf("Figure_irtrain13_v2.pdf",height = 5, width = 14, onefile = T,paper = "special") 
par(mfrow=c(1,3),mar=c(4.6, 4.1, 3.6, 0.6))
#2D plot of country classification 
mclust2Dplot(irtrain13,parameters=mbc.irtrain13$parameters,z=mbc.irtrain08$z,
             what="classification", colors="white")
text(irtrain13[,1], irtrain13[,2],rownames(train13),col=mbc.irtrain13$classification,font=1)
title(main="Classification 2D plot 2004-2013")
#Uncertainty plot
coordProj(irtrain13,parameters=mbc.irtrain13$parameters,z=mbc.irtrain13$z,
          what="uncertainty", uncertainty=mbc.irtrain13$uncertainty)
text(irtrain13[,1], irtrain13[,2],rownames(train13),font=1)
title(main="Uncertainty classifications 2004-2013")
#BIC Plot
plot.mclustBIC(mbc.irtrain13$BIC,legendArgs = list(x="bottomleft", ncol=4,cex=.75), ylim=c(-210,-140))
title(main="BIC model comparison 2004-2013")
abline(h=mbc.irtrain13$bic-10, lty=2, lwd=2)
dev.off()


#Create table for two dimensions
class_all_twodimensions <- as.data.frame(cbind(train93$code,mbc.irtrain93$classification,
                                         mbc.irtrain98$classification, mbc.irtrain03$classification,
                                         mbc.irtrain08$classification, mbc.irtrain13$classification))
model_twodimensions <- as.character(c("","EVI","EEE","EEE","EEI","EII",""))
bic_twodimensions <- c("",-148, -152, -151, -151, -146,"")
optimal_twodimensions <- c("",2, 1, 7, 3, 3,"")
VoC <- c("LME","CME","CME","LME","CME","CME", "CME", "MED", 
         "CME","MED","LME","MED","LME","MED","CME","CME",
         "CME","MED","CME","LME")
class_all_twodimensions<- cbind(class_all_twodimensions,VoC)
indx <- sapply(class_all_twodimensions, is.factor)
class_all_twodimensions[indx] <- lapply(class_all_twodimensions[indx], as.character)
class_all_twodimensions <- rbind(class_all_twodimensions,model_twodimensions)
class_all_twodimensions <- rbind(class_all_twodimensions, optimal_twodimensions, bic_twodimensions)
class_all_twodimensions[c(21:23),1] <- c("Model","No. Clusters","BIC")
colnames(class_all_twodimensions) <- c("country","84-93","89-98","94-03","99-08","04-13","VoC")

library(mclust)

#ICL Criterium
mclustICL(irtrain93,G=1:8) #EVI 2
mclustICL(irtrain98,G=1:8) #EEE 1
mclustICL(irtrain03,G=1:8) #VEV 7
mclustICL(irtrain08,G=1:8) #EEI 7
mclustICL(irtrain13,G=1:8) #EII 3


#Write csv
setwd("C:/Users/Bas Vervaart/Documents/Thesis clustering/Data thesis/analysis/figures")
write.csv(class_all_irtrain, "classification_irtrain.csv")
write.csv(class_all_ircorp, "classification_ircorp.csv")

print(xtable(class_all_irtrain, type = "latex"), file = "classification_irtrain.tex")
print(xtable(class_all_ircorp, type = "latex"), file = "classification_ircorp.tex")
print(xtable(icl_table, type = "latex"), file = "icl_table.tex")

tabel1<-read.csv("classification_irtrain.csv")
tabel1 <- tabel1 %>% select(-X)
colnames(tabel1) <-c("country","84-93","89-98","94-03","99-08",
                                                         "04-13","VoC")
print(xtable(tabel1, type = "latex"), file = "tabel1.tex")
library(xtable)
