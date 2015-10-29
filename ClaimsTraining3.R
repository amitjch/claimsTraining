#################################################################################################################

###     Claims Analytics Training
#       Exercise 3

#       aj, 28.10.2010
 
#################################################################################################################

## Set Working directory, if needed


## Exercise 1 - Using the read.table function and accessing variables from a data frame with epidemiological data

bf <- read.table("BirdFlu.csv",header = TRUE, sep = ";")
names(bf)[1] <- "Country"

names(data)
str(data)

bf$TotalCases <- bf$cases2003 + bf$cases2004+  bf$cases2005 +  bf$cases2006 + bf$cases2007+  bf$cases2008 
bf$TotalDeaths <- bf$deaths2003 + bf$deaths2004+  bf$deaths2005 +  bf$deaths2006 + bf$deaths2007+  bf$deaths2008 

#Numbers of bird flu cases in 2003

sum(bf$cases2003)

#Numbers of bird flu cases in 2003 and 2005
sum(rbind(bf$cases2003,bf$cases2005))

#Country with the most cases
bf[bf$TotalCases == max(bf$TotalCases),c("Country", "TotalCases")]

#Country with the least deaths
bf[bf$TotalDeaths == min(bf$TotalDeaths),c("Country", "TotalDeaths")]

#Total Cases per Country

bf[, c("Country", "TotalCases")]

#Total Number of cases per year
sapply(bf[,c("cases2003", "cases2004","cases2005","cases2006","cases2007","cases2008")], sum)

## Exercise 2 - Using the read.table function and accessing variables from a data frame with epidemiological data

isit <- read.table("ISIT.txt",header = TRUE, sep = "\t") #File from http://www.highstat.com/Book3/ISIT.txt

#Number of oberservations at station 1

nrow(isit[isit$Station == 1,]) #38

#Min, Median, Mean, Max of SampleDepth @ Station 1
min(isit[isit$Station == 1,"SampleDepth"])
median(isit[isit$Station == 1,"SampleDepth"])
mean(isit[isit$Station == 1,"SampleDepth"])
max(isit[isit$Station == 1,"SampleDepth"])

#Min, Median, Mean, Max of Sample.Depth @ Station 2
min(isit[isit$Station == 2,"SampleDepth"])
median(isit[isit$Station == 2,"SampleDepth"])
mean(isit[isit$Station == 2,"SampleDepth"])
max(isit[isit$Station == 2,"SampleDepth"])

#Min, Median, Mean, Max of Sample.Depth @ Station 3
min(isit[isit$Station == 3,"SampleDepth"])
median(isit[isit$Station == 3,"SampleDepth"])
mean(isit[isit$Station == 3,"SampleDepth"])
max(isit[isit$Station == 3,"SampleDepth"])

#Stations with fewer Obervatations than Station 1

isit$Station <- as.factor(isit$Station)

a <- as.data.frame(aggregate(isit$SampleDepth, by=list(isit$Station), FUN=length)) #Get No of Observation per Station
names(a) <- c("Station", "nObs") #Reduce to one less than Station 1
fewObsStation <- a[a$nObs < nrow(isit[isit$Station == 1,]),] #New Data.frame

#Data from 2002

isit[isit$Year == 2002,]

#Data from April

isit[isit$Month == 4,]

#Data from SampleDepth > 2000, ordered

samDepth2000 <- isit[isit$SampleDepth > 2000,]
samDepth2000[order(samDepth2000$SampleDepth),]

#SampleDepth > 2000m and Data from April

isit[isit$SampleDepth > 2000 & isit$Month == 4,]

## Exercise 3 - Export to an asci-file

export <- isit[isit$SampleDepth > 2000 & isit$Month == 4,]
write.table(x = export, file="Isit200_April.csv",sep = ";",row.names = FALSE,col.names = TRUE)


## Exercise 4 - Use factor function

#Generate reference table for the month. By using the data.frame function the variable are converted into factors already.
#Therefore no need to use the factor-function.

ref1 <- data.frame(1:5, "April", "2001")
names(ref1) <- c("Station", "newMonth", "newYear")
ref2 <- data.frame(6:11, "August", "2001")
names(ref2) <- c("Station", "newMonth", "newYear")
ref3 <- data.frame(12:15, "March", "2002")
names(ref3) <- c("Station", "newMonth", "newYear")
ref4 <- data.frame(16:19, "October", "2002")
names(ref4) <- c("Station", "newMonth", "newYear")
refTable <- rbind(ref1, ref2, ref3, ref4)

# Join them
newIsit <- merge(x = isit, y = refTable, by = "Station")
