library(devtools)
install_github('fickse/mesowest')
library(mesowest)

### This works for downloading a .csv of all hourly submissions for a single RAWS station with the MesoWest API and then loading it into R
path = "C:/Users/andrew/Documents/R/RAWS/RAWS_Output.csv"  #path to save .csv
start = "201710010000"
end = "201810010000"
stid ="RVDC1"
file = curl::curl_download(paste("http://api.mesowest.net/v2/stations/timeseries?token=fa7799bb71314938921f8ec2b4b65c1d&stid=",stid,"&start=",start,"&end=",end,"&output=csv&units=english"),destfile = path)

#open .csv and remove the separate unit row, need to concatonate this somehow to title
RAWSstation = read.csv(path,  header = T,sep = ",",quote = "",comment.char = "#")
RAWSstation = RAWSstation[-1,]
###


### This works for loading the data directly from the mesowest API, .csv is not saved locally
path = "C:/Users/andrew/Documents/R/RAWS/RAWS_Output.csv"  #path to save .csv
start = "201710010000"
#Get current year: 
CurrentYear = format(Sys.Date(), "%Y")

#Get Current Month
CurrentMonth = format(Sys.Date(), "%m")

end=c(CurrentYear,CurrentMonth,'010000')
stid ="BUFC1"
file = curl::curl(paste("http://api.mesowest.net/v2/stations/timeseries?token=fa7799bb71314938921f8ec2b4b65c1d&stid=",stid,"&start=",start,"&end=",end,"&output=csv&units=english"))
#Station = read.csv(file,  header = T,sep = ",",quote = "",comment.char = "#")
#Station = Station[-1,]



#####
##

#Get current year: 
CurrentYear = format(Sys.Date(), "%Y")

#Get Current Month
CurrentMonth = format(Sys.Date(), "%m")

requestToken(apikey = "KzsQxP3c0dI4YX2Rs1wpYyz1hHKcvZujmBb")

#Using functions from MWfunctions_modification.R in C:\Offline\RAWS\Mesowest_modification
#Ravendale - Missing 6/2011- 8/2011
RVDC1_01_11 = mw(service = 'precipitation',stid='RVDC1',start='200107010000',end='201106010000',units='english',pmode="intervals", interval="month")
RVDC1_01_11_DF = as.data.frame(RVDC1_01_11$STATION$OBSERVATIONS$precipitation)
RVDC1_01_11_DF$Station <- 'RVDC1'

RVDC1_11_18 = mw(service = 'precipitation',stid='RVDC1',start='201109010000',end=paste(CurrentYear,CurrentMonth,'010000',sep = ""),units='english',pmode="intervals", interval="month")
RVDC1_11_18_DF = as.data.frame(RVDC1_11_18$STATION$OBSERVATIONS$precipitation)
RVDC1_11_18_DF$Station <- 'RVDC1'

#Honey Lake - Missing 
HLKC1_00_06 = mw(service = 'precipitation',stid='HLKC1',start='200111010000',end='200610010000',units='english',pmode="intervals", interval="month")
HLKC1_00_06_DF = as.data.frame(HLKC1_00_06$STATION$OBSERVATIONS$precipitation)
HLKC1_00_06_DF$Station <- 'HLKC1'

HLKC1_06_18 = mw(service = 'precipitation',stid='HLKC1',start='200611010000',end=paste(CurrentYear,CurrentMonth,'010000',sep = ""),units='english',pmode="intervals", interval="month")
HLKC1_06_18_DF = as.data.frame(HLKC1_06_18$STATION$OBSERVATIONS$precipitation)
HLKC1_06_18_DF$Station <- 'HLKC1'


#Hidden Valley - 
HDVC1_07_10 = mw(service = 'precipitation',stid='HDVC1',start='200711010000',end='201010010000',units='english',pmode="intervals", interval="month")
HDVC1_07_10_DF = as.data.frame(HDVC1_07_10$STATION$OBSERVATIONS$precipitation)
HDVC1_07_10_DF$Station <- 'HDVC1'

HDVC1_11_18 = mw(service = 'precipitation',stid='HDVC1',start='201109010000',end=paste(CurrentYear,CurrentMonth,'010000',sep = ""),units='english',pmode="intervals", interval="month")
HDVC1_11_18_DF = as.data.frame(HDVC1_11_18$STATION$OBSERVATIONS$precipitation)
HDVC1_11_18_DF$Station <- 'HDVC1'



#Bull Flat - 
BUFC1All = mw(service = 'precipitation',stid='BUFC1',start='200106010000',end=paste(CurrentYear,CurrentMonth,'010000',sep = ""),units='english',pmode="intervals", interval="month")
BUFC1 = as.data.frame(BUFC1All$STATION$OBSERVATIONS$precipitation)
BUFC1$Station <- 'BUFC1'



#Doyle - 
DYLC1_00_06= mw(service = 'precipitation',stid='DYLC1',start='200011010000',end='200612010000',units='english',pmode="intervals", interval="month")
DYLC1_00_06_DF = as.data.frame(DYLC1_00_06$STATION$OBSERVATIONS$precipitation)
DYLC1_00_06_DF$Station <- 'DYLC1'

DYLC1_08_18= mw(service = 'precipitation',stid='DYLC1',start='200802010000',end=paste(CurrentYear,CurrentMonth,'010000',sep = ""),units='english',pmode="intervals", interval="month")
DYLC1_08_18_DF = as.data.frame(DYLC1_08_18$STATION$OBSERVATIONS$precipitation)
DYLC1_08_18_DF$Station <- 'DYLC1'

#Buffalo Creek
BUFN2All = mw(service = 'precipitation',stid='BUFN2',start='200106010000',end=paste(CurrentYear,CurrentMonth,'010000',sep = ""),units='english',pmode="intervals", interval="month")
BUFN2 = as.data.frame(BUFN2All$STATION$OBSERVATIONS$precipitation)
BUFN2$Station <- 'BUFN2'

#Grasshopper
GRSC1All= mw(service = 'precipitation',stid='GRSC1',start='200106010000',end=paste(CurrentYear,CurrentMonth,'010000',sep = ""),units='english',pmode="intervals", interval="month")
GRSC1 = as.data.frame(GRSC1All$STATION$OBSERVATIONS$precipitation)
GRSC1$Station <- 'GRSC1'


Test <-rbind(BUFC1,BUFN2,DYLC1_08_18_DF,DYLC1_00_06_DF,GRSC1,HDVC1_11_18_DF,HDVC1_07_10_DF,HLKC1_00_06_DF,HLKC1_06_18_DF,RVDC1_01_11_DF,RVDC1_11_18_DF)
tmpSplit <- strsplit(Test$first_report, "-")

Test$year <- sapply(tmpSplit, "[", 1)
Test$month <- sapply(tmpSplit, "[", 2)
#Water Year Function
wtr_yr <- function(dates, start_month = 10){ 
  # Convert possible character vector into date
  d1 = as.Date(dates)
  # Year offset
  offset = ifelse(as.integer(format(d1, "%m")) < start_month, 0, 1)
  # Water year
  adj.year = as.integer(format(d1, "%Y")) + offset
  # Return the water year
  return(adj.year)
}                      
Test2 <- wtr_yr(Test$first_report)
Test$waterYear <- Test2



                 

                  


library(dplyr)
TestSummary = Test %>% 
  group_by_(.dots=c("Station","waterYear")) %>% 
  summarize(x=sum(total))


library(ggplot2)
# line with each station and all years
p <- ggplot(data = TestSummary,
            mapping = aes(x = waterYear, color = Station))+ xlab("Water Year")
p + geom_line(mapping = aes(y = x),size = 1.5)+ ylab("Precipitation in Inches") 

#Chart for each Station stacked vertically
d <- ggplot(data = TestSummary,
            mapping = aes(x = waterYear, fill = Station ))
d + geom_bar(position = "dodge",stat="identity",
             mapping = aes(y = x))+
             facet_grid(Station ~ .  )+ ylab("Precipitation in Inches") + xlab("Water Year")+
             theme( axis.text.y=element_text(angle=45)) + scale_x_continuous(breaks=seq(2000, 2019, 2))+ggtitle(paste("Annual Precipitation(in) for RAWS Stations from 2000-", CurrentYear,sep = ""))

## Chart for each year 
c <- ggplot(data = TestSummary,
            mapping = aes(x = factor(waterYear), fill = Station))+
            geom_bar(position = "dodge",stat="identity",
            mapping = aes(y = x))+
            facet_wrap(~ waterYear, ncol = 5, scales='free_x')+ ylab("Precipitation in Inches") + xlab("RAWS Station ID")+
            theme( axis.text.y=element_text(angle=45))+ggtitle(paste("Annual Precipitation(in) for Water Year (Oct-Nov)"))



## individual chart for 2019 one color pallet
f <- ggplot(data = TestSummary[TestSummary$waterYear == 2019,],
            mapping = aes(x = Station, fill = Station))
f + geom_bar(position = "dodge",stat="identity",
             mapping = aes(y = x))+ ylab("Precipitation in Inches") + xlab("RAWS Station ID") + scale_fill_brewer(palette="Reds")  # "Reds" is palette name

## individual chart for 2012 many colors
g <- ggplot(data = TestSummary[TestSummary$waterYear == 2012,],
            mapping = aes(x = Station, fill = Station))
g + geom_bar(position = "dodge",stat="identity",
             mapping = aes(y = x)) + ylab("Precipitation in Inches") + xlab("RAWS Station ID") +ggtitle(paste(2012,": Annual Precipitation (in) for Water Year (Oct-Nov)"))


test = RVDC1[first_report>="2011-09-30" & first_report<="2012-11-01"]
sum(RVDC1$total)



nstauffer/aim.analysis
