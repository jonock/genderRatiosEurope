#Package install
#install.packages('ggplot2')
library(ggplot2)
library(kableExtra)
library(knitr)
library(stargazer)
library(tidyverse)
library(dplyr)

#Data Import


data = read.csv('07d6926180836158.csv')
data2 = read.csv('b70f8c00619a58f5.csv')
data = merge(data, data2, by='DirectorID')
data <- subset(data, HOCountryName != 'Netherlands Antilles')

nednow <- subset(ned, AnnualReportDate >= 20181201)
nedold <- subset(ned, AnnualReportDate <= 20101201)

nedComp <- subset(nednow, Salary > 0 | Bonus > 0 | Other > 0 | TotalCompensation > 0)
nedComp <- subset(nednow, TotalCompensation > 1)
nedCompU <- distinct(nedComp, BoardID, DirectorID, .keep_all = TRUE)
nedCompUF <-  distinct(nedComp, BoardID, .keep_all = TRUE) #alle firmen

nedUtl <- distinct(ned, BoardID, DirectorID, .keep_all = TRUE)
nedUFtl <- distinct(ned, BoardID, .keep_all = TRUE)
nedUExectl <- subset(nedUtl, NED == 'No')
nedUSuptl <-  subset(nedUtl, NED =='Yes')

nedU <- distinct(nednow, BoardID, DirectorID, .keep_all = TRUE)
nedUF <- distinct(nednow, BoardID, .keep_all = TRUE)
nedUExec <- subset(nedU, NED == 'No')
nedUSup <-  subset(nedU, NED =='Yes')


#DataGenerator
dataCaller <- function(){
  countries <- unique(data$HOCountryName)
  for (country in countries){
    dataGenerator(paste(country), paste(tolower(substr(country,1,3))))
  }
}




dataGenerator <- function(countrystring, countrycode, env = globalenv()){
  strings = c('Utl', 'UFtl', 'UExectl', 'USuptl')
  assign(countrycode, subset(data, HOCountryName == countrystring), envir = env)
  assign(paste(countrycode, strings[1], sep=""), distinct(get(countrycode), BoardID, DirectorID, .keep_all = TRUE), envir = env)
  assign(paste(countrycode, strings[2], sep=""), distinct(get(countrycode), BoardID, .keep_all = TRUE), envir = env)
  assign(paste(countrycode, strings[3], sep=""), subset(get(paste0(countrycode, strings[1])), NED=='No'), envir = env)
  assign(paste(countrycode, strings[4], sep=""), subset(get(paste0(countrycode, strings[1])), NED=='Yes'), envir = env)
  AnalyseGenderRatios(get(paste0(countrycode, 'USuptl')),countrystring_long = countrystring, countrystring_short = countrycode, bodystring_long = 'Supervisory', bodystring_short = 'Sup')
  AnalyseGenderRatios(get(paste0(countrycode, 'UExectl')),countrystring_long = countrystring, countrystring_short = countrycode, bodystring_long = 'Executive', bodystring_short = 'Exec')
}
dataCaller()

#Data before 2011
nedOldU <- distinct(nedold, BoardID, DirectorID, .keep_all = TRUE)
nedOldUF <- distinct(nedold, BoardID, .keep_all = TRUE)
nedOldUExec <- subset(nedOldU, NED == 'No')
nedOldUSup <-  subset(nedOldU, NED =='Yes')

#Histogram of GenderRatio

#Automated Data Observation of GenderRatio
AnalyseGenderRatios <- function(dataset, countrystring_long, countrystring_short,bodystring_long, bodystring_short){
  GenderRatios <- data.frame()
  ReportsComplete <- data.frame()
  for (year in 2010:2019){
    t1 <- sprintf('%d0101',year)
    t2 <- sprintf('%d1231',year)
    
    temp <- subset(dataset, AnnualReportDate >= t1 & AnnualReportDate <= t2)
    if (nrow(temp)>0){
      Reports <- distinct(temp, BoardID, Ticker, AnnualReportDate, .keep_all = TRUE)
      Reports$FemaleRatio = 1-Reports$GenderRatio
      Reports$year = year
      png(file=sprintf('outputs/%s_Hist_%s_%d.png',countrystring_short,bodystring_short,year),width = 9, height = 6, units = 'in', res = 300)
      hist(Reports$FemaleRatio, breaks = 30,xlim = c(0,1),col='darkmagenta',xlab = 'Ratio of female board members', 
           main = sprintf('%s Boards in %s %d (n=%d)',bodystring_long,countrystring_long ,year, nrow(Reports)) )
      dev.off()
      
      rep <- na.omit(Reports$FemaleRatio)
      x <- summary(rep)
      tempdf <- data.frame(x=matrix(x),row.names=names(x))
      colnames(tempdf) <- c(sprintf('%d',year))
      tempdf <- t(tempdf)
      GenderRatios <- rbind(GenderRatios, tempdf)
      rm(tempdf,rep,x)
      
      ReportsComplete <- rbind(ReportsComplete, Reports)
    }
  }
  write.csv(GenderRatios, sprintf('outputs/%s_GenderRatios_%s.csv',countrystring_short,bodystring_short))
  if (!is.null(ReportsComplete$FemaleRatio)){
    png(file=sprintf('outputs/%s_box_%s_%d.png',countrystring_short,bodystring_short,year),width = 9, height = 6, units = 'in', res = 300)
    boxplot(ReportsComplete$FemaleRatio ~ ReportsComplete$year, 
            ylab = 'Ratio of female board members', 
            xlab = 'Year',
            main = sprintf('%s Boards in %s (n=%d)',bodystring_long,countrystring_long, nrow(ReportsComplete))
    )
    dev.off()
    
    png(file=sprintf('%s_boxNat_%s_%d.png',countrystring_short,bodystring_short,year),width = 9, height = 6, units = 'in', res = 300)
    boxplot(ReportsComplete$NationalityMix ~ ReportsComplete$year, 
            ylab = 'NationalityMix', 
            xlab = 'Year',
            main = sprintf('%s Boards in %s (n=%d)',bodystring_long,countrystring_long, nrow(ReportsComplete))
    )
    dev.off()
    
    png(file=sprintf('%s_boxSize_%s_%d.png',countrystring_short,bodystring_short,year),width = 9, height = 6, units = 'in', res = 300)
    boxplot(ReportsComplete$NumberDirectors ~ ReportsComplete$year, 
            ylab = 'Number of Directors', 
            xlab = 'Year',
            main = sprintf('%s Boards in %s (n=%d)',bodystring_long,countrystring_long, nrow(ReportsComplete))
    )
    dev.off()
    return(ReportsComplete)
  }
  rm(ReportsComplete)
}

#Gender development over time

Gender <- data.frame('Gender' = c('F', 'M'))
for (year in 2011:2020){
  t1 <- sprintf('%d0101',year)
  t2 <- sprintf('%d1231',year)
  temp <- subset(ned, AnnualReportDate >= t1 & AnnualReportDate <= t2)
  tempappend <- as.data.frame(table(temp$Gender, dnn = list('Gender')), responseName = year)
  Gender <- cbind(Gender,tempappend[2])
}


genderPlots <- function(){
  Gendertemp <- Gender[,-1]
  rownames(Gendertemp) <- Gender[,1]
  Gender <- Gendertemp
  rm(Gendertemp)
  Gender <- rbind(Gender, SumOfMembers = colSums(Gender))
  Gender <- rbind(Gender, FemaleRatio = Gender[1,] / Gender[3,])
  
  png(file=sprintf('Ratio_development.png'),width=700, height=500)
  plot(2011:2020, Gender[4,], xlab = 'Year', ylab = 'Ratio of female board members in total', ylim = c(0,1), )
  dev.off()
  
  genderNowSup <- as.data.frame(table(nedUSup$Gender, dnn = list('Gender')), responseName = '2019')
  genderOldSup <- as.data.frame(table(nedOldUSup$Gender, dnn = list('Gender')), responseName = '2010')
  genderOverview <- merge(genderOldSup, genderNowSup, by='Gender')
  genderOverview1 <- genderOverview[,-1]
  rownames(genderOverview1) <- genderOverview[,1]
  genderOverview <- genderOverview1
  rm(genderOverview1)
  genderOverview <-  rbind(genderOverview, data.frame(X2010 = sum(genderOverview$X2010), X2019 = sum(genderOverview$X2019)))
  genderOverview[4,1] <- (genderOverview[1,1]/genderOverview[3,1])
  genderOverview[4,2] <- (genderOverview[1,2]/genderOverview[3,2])
  
}
temp <- dataGenerator('Netherlands', 'net')
summary(temp$FemaleRatio)

# AnalyseGenderRatios(nedUSuptl,countrystring_long = 'Netherlands', countrystring_short = 'NL', bodystring_long = 'Supervisory', bodystring_short = 'Sup')
# AnalyseGenderRatios(nedUExectl,countrystring_long = 'Netherlands', countrystring_short = 'NL', bodystring_long = 'Executive', bodystring_short = 'Exec')
# AnalyseGenderRatios(gerUSuptl,countrystring_long = 'Germany', countrystring_short = 'GE', bodystring_long = 'Supervisory', bodystring_short = 'Sup')
# AnalyseGenderRatios(gerUExectl,countrystring_long = 'Germany', countrystring_short = 'GE', bodystring_long = 'Executive', bodystring_short = 'Exec')
# AnalyseGenderRatios(itaUExectl,countrystring_long = 'Italy', countrystring_short = 'IT', bodystring_long = 'Executive', bodystring_short = 'Exec')
AnalyseGenderRatios(norO,countrystring_long = 'Norway', countrystring_short = 'NorO', bodystring_long = 'Supervisory', bodystring_short = 'Sup')

#dataCaller()
#genderPlots()



