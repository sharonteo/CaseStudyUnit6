
install.packages("repmis")

install.packages("ggplot2")

install.packages("downloader")
install.packages("dplyr")
install.packages("RCurl")
install.packages("bitops")
library(repmis)
library(ggplot2)
library(downloader)
library(RCurl)
library(dplyr)



site="https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv" 
download.file(site,destfile="./getdata%2Fdata%2FGDP.csv")

site="https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv" 
download.file(site,destfile="./getdata%2Fdata%2FEDSTATS_Country.csv")

GDP <- read.csv("getdata%2Fdata%2FGDP.csv",header=TRUE)
GDP <- read.csv("getdata%2Fdata%2FGDP.csv", stringsAsFactors=FALSE, header=TRUE)
GDPraw <- GDP
GDPraw[6:10] <-list(NULL)
GDPraw$X.1 <- NULL
GDPdata <- GDPraw[5:330,]
head(GDPdata)
names(GDPdata) <- c("CountryCode", "Ranking", "Short.Name", "GDP")
GDPdata$Ranking <- as.numeric(GDPdata$Ranking)
GDPdata$GDP <- gsub(",","",GDPdata$GDP)
GDPdata$GDP <- as.numeric(GDPdata$GDP)
EDU <- read.csv("getdata%2Fdata%2FEDSTATS_Country.csv", header=FALSE)
str(EDU)
head(EDU)

EDUraw[4:30] <-list(NULL)

head(EDUraw)
EDUdata <- EDUraw[2:240,]
head(EDUdata)
names(EDUdata) <- c("CountryCode", "Short.Name", "Income.Group")
head(EDUdata,5)

GDPEDU <- merge(GDPdata,EDUdata,by="CountryCode")
head(GDPEDU)
summary(GDPEDU)
#sort by GDP (ascending)
NEWGE <- GDPEDU[order(GDPEDU$Ranking),] 
head(NEWGE,5)
summary(NEWGE)
#We See There is 14 blank Income Groups
NEWGE=subset(NEWGE, NEWGE$Income.Group!="")
summary(GDPEDU)
nrow(GDP)
nrow(EDU)
NEWGE[1:13,1:5]
table(NEWGE$Income.group)
HOECD=subset(NEWGE,NEWGE$Income.Group=='High income: OECD')
HNOECD=subset(NEWGE,NEWGE$Income.Group=='High income: nonOECD')
summary(HOECD)
summary(HNOECD)

plot(NEWGE$Ranking,log(NEWGE$GDP),xlab="Rank",ylab="GDP",main="GDP Across Country Rankings")

qplot(as.numeric(NEWGE$Ranking), data=NEWGE, geom="bar", weight=log(NEWGE$GDP), ylab="GDP",xlab="Ranking",main = "GDP According to Country Rank",fill=NEWGE$Income.Group)+
theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

g = ggplot(NEWGE, aes(x = as.numeric(NEWGE$Ranking), y=log(NEWGE$GDP)))  + geom_point()+
aes(colour = NEWGE$Income.Group)+
labs(x="Rank",y="Natural Log GDP")+ 

theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))#,legend.position="none")

g
ggplot(data = GDPEduclean, aes(x = rank_num, y = log10(GDP2012))) + geom_point(aes(col = IncomeGroup)) + labs(title = "GDP as Function of Country Ranking", x = "GDP ranking", y = "log base 10 GDP")+ scale_fill_manual(breaks = 50, 100, 150, 200, 250)
summary(NEWGE)
head(NEWGE)
NEWGE$quantile <- ntile(NEWGE$GDP, 5)
head(NEWGE)
table(NEWGE$quantile,NEWGE$Income.Group)
