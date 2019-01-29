library(devtools)
#Download fickse's mesowest package through github
install_github('fickse/mesowest')
library(mesowest)

### Script for loading data directly from the mesowest API, .csv is not saved locally

# Sign up for Mesowest and recieve an API Key here: https://developers.synopticdata.com/signup/
requestToken(apikey = "KzsQxP3c0dI4YX2Rs1wpYyz1hHKcvZujmBb")

#Get current year: 
CurrentYear = format(Sys.Date(), "%Y")

#Get Current Month
CurrentMonth = format(Sys.Date(), "%m")

CurrentEndDate = paste(CurrentYear,CurrentMonth,'310000',sep = "")

#Using functions from mesowest package: install_github('fickse/mesowest')
#Ravendale - Missing 6/2011- 8/2011
RVDC1_01_11 = mw(service = 'precipitation',stid='RVDC1',start='200107010000',end='201106010000',units='english',pmode="intervals", interval="month")
RVDC1_01_11_DF = as.data.frame(RVDC1_01_11$STATION$OBSERVATIONS$precipitation)
RVDC1_01_11_DF$Station <- 'RVDC1'
RVDC1_01_11_DF$StationName <- RVDC1_01_11$STATION$NAME

RVDC1_11_18 = mw(service = 'precipitation',stid='RVDC1',start='201109010000',end= CurrentEndDate,units='english',pmode="intervals", interval="month")
RVDC1_11_18_DF = as.data.frame(RVDC1_11_18$STATION$OBSERVATIONS$precipitation)
RVDC1_11_18_DF$Station <- 'RVDC1'
RVDC1_11_18_DF$StationName <- RVDC1_11_18$STATION$NAME


#Honey Lake - Missing 10/2006 - 11/2006
HLKC1_00_06 = mw(service = 'precipitation',stid='HLKC1',start='200111010000',end='200610010000',units='english',pmode="intervals", interval="month")
HLKC1_00_06_DF = as.data.frame(HLKC1_00_06$STATION$OBSERVATIONS$precipitation)
HLKC1_00_06_DF$Station <- 'HLKC1'
HLKC1_00_06_DF$StationName <- HLKC1_00_06$STATION$NAME

HLKC1_06_18 = mw(service = 'precipitation',stid='HLKC1',start='200611010000',end= CurrentEndDate,units='english',pmode="intervals", interval="month")
HLKC1_06_18_DF = as.data.frame(HLKC1_06_18$STATION$OBSERVATIONS$precipitation)
HLKC1_06_18_DF$Station <- 'HLKC1'
HLKC1_06_18_DF$StationName <- HLKC1_06_18$STATION$NAME

#Hidden Valley - missing 10/2010 - 09/2011
HDVC1_07_10 = mw(service = 'precipitation',stid='HDVC1',start='200711010000',end='201010010000',units='english',pmode="intervals", interval="month")
HDVC1_07_10_DF = as.data.frame(HDVC1_07_10$STATION$OBSERVATIONS$precipitation)
HDVC1_07_10_DF$Station <- 'HDVC1'
HDVC1_07_10_DF$StationName <- HDVC1_07_10$STATION$NAME

HDVC1_11_18 = mw(service = 'precipitation',stid='HDVC1',start='201109010000',end=CurrentEndDate ,units='english',pmode="intervals", interval="month")
HDVC1_11_18_DF = as.data.frame(HDVC1_11_18$STATION$OBSERVATIONS$precipitation)
HDVC1_11_18_DF$Station <- 'HDVC1'
HDVC1_11_18_DF$StationName <- HDVC1_11_18$STATION$NAME


#Bull Flat - 
BUFC1All = mw(service = 'precipitation',stid='BUFC1',start='200106010000',end=CurrentEndDate ,units='english',pmode="intervals", interval="month")
BUFC1 = as.data.frame(BUFC1All$STATION$OBSERVATIONS$precipitation)
BUFC1$Station <- 'BUFC1'
BUFC1$StationName <- BUFC1All$STATION$NAME



#Doyle - Missing 12/2006 - 02/2008
DYLC1_00_06= mw(service = 'precipitation',stid='DYLC1',start='200011010000',end='200612010000',units='english',pmode="intervals", interval="month")
DYLC1_00_06_DF = as.data.frame(DYLC1_00_06$STATION$OBSERVATIONS$precipitation)
DYLC1_00_06_DF$Station <- 'DYLC1'
DYLC1_00_06_DF$StationName <- DYLC1_00_06$STATION$NAME

DYLC1_08_18= mw(service = 'precipitation',stid='DYLC1',start='200802010000',end=CurrentEndDate ,units='english',pmode="intervals", interval="month")
DYLC1_08_18_DF = as.data.frame(DYLC1_08_18$STATION$OBSERVATIONS$precipitation)
DYLC1_08_18_DF$Station <- 'DYLC1'
DYLC1_08_18_DF$StationName <- DYLC1_08_18$STATION$NAME

#Buffalo Creek
BUFN2All = mw(service = 'precipitation',stid='BUFN2',start='200106010000',end=CurrentEndDate ,units='english',pmode="intervals", interval="month")
BUFN2 = as.data.frame(BUFN2All$STATION$OBSERVATIONS$precipitation)
BUFN2$Station <- 'BUFN2'
BUFN2$StationName <- BUFN2All$STATION$NAME

#Grasshopper
GRSC1All= mw(service = 'precipitation',stid='GRSC1',start='200106010000',end=CurrentEndDate ,units='english',pmode="intervals", interval="month")
GRSC1 = as.data.frame(GRSC1All$STATION$OBSERVATIONS$precipitation)
GRSC1$Station <- 'GRSC1'
GRSC1$StationName <- GRSC1All$STATION$NAME

#Merge all station data into single dataframe
AllStations <-rbind(BUFC1,BUFN2,DYLC1_08_18_DF,DYLC1_00_06_DF,GRSC1,HDVC1_11_18_DF,HDVC1_07_10_DF,HLKC1_00_06_DF,HLKC1_06_18_DF,RVDC1_01_11_DF,RVDC1_11_18_DF)

#Split out year and month from the first report date
AllStationsSplit <- strsplit(AllStations$first_report, "-")

AllStations$year <- sapply(AllStationsSplit, "[", 1)
AllStations$month <- sapply(AllStationsSplit, "[", 2)
AllStations$monthabb <- month.abb[month(as.Date(AllStations$first_report))]

AllStations <- AllStations[complete.cases(AllStations),]


##Water Year summary and charts
#Water Year Function calculates water year for summary purposes
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
AllStationsWaterYear <- wtr_yr(AllStations$first_report)
AllStations$waterYear <- AllStationsWaterYear


library(dplyr)
#Summarize by water year
WaterYearSummary = AllStations %>% 
  group_by_(.dots=c("StationName","waterYear")) %>% 
  summarize(x=sum(total))

#Average Water Year Precip
AnnualAvg = WaterYearSummary[WaterYearSummary$waterYear !=2001 & WaterYearSummary$waterYear !=2019,] %>% 
  group_by_("StationName") %>% 
  summarize(Avg=mean(x))

WaterYearSummary <- left_join(WaterYearSummary,AnnualAvg,"StationName") 

# get list of years excluding 2001 
yearlist = WaterYearSummary[WaterYearSummary$waterYear !=2001,]
yearList = unique(yearlist$waterYear)


library(ggplot2)
# line with each station and all years
a <- ggplot(data = WaterYearSummary,
            mapping = aes(x = waterYear, color = StationName))+ xlab("Water Year")
a + geom_line(mapping = aes(y = x),size = 1.5)+ ylab("Precipitation in Inches") 



#Chart for each Station stacked vertically
b <- ggplot(data = WaterYearSummary[WaterYearSummary$waterYear != 2001,],
            mapping = aes(x = waterYear, fill = StationName ))
b + geom_bar(position = "dodge",stat="identity",
             mapping = aes(y = x))+
            facet_grid(StationName ~ .  )+
            geom_hline(aes(yintercept = Avg), colour="Black", lty=3) +
            ylab("Precipitation in Inches") +
            xlab("Water Year") + 
            theme( axis.text.y=element_text(angle=45))+
            scale_x_continuous(breaks=seq(2002, 2019, 2)) +
            ggtitle(paste("Annual Water Year Precipitation(in) for RAWS Stations from 2000-", CurrentYear,sep = "")) +
            theme(strip.text.y = element_text(size = 6,  angle = 90))

            
#Chart for each Station stacked vertically - Free y axis - slightly misleading 
b <- ggplot(data = WaterYearSummary,
            mapping = aes(x = waterYear, fill = StationName ))
b + geom_bar(position = "dodge",stat="identity",
             mapping = aes(y = x))+
  facet_grid(StationName ~ . , scales = "free_y" )+
  geom_hline(aes(yintercept = Avg), colour="Black", lty=3) +
  ylab("Precipitation in Inches") +
  xlab("Water Year") + 
  theme( axis.text.y=element_text(angle=45))+
  scale_x_continuous(breaks=seq(2000, 2019, 2)) +
  ggtitle(paste("Annual Precipitation(in) for RAWS Stations from 2000-", CurrentYear,sep = ""))

#Chart for each Station stacked vertically limited to 2015 - ** Added from update
c <- ggplot(data = AllStations[AllStations$year > 2016,],mapping = aes(x = month, fill = StationName))
c + geom_bar(position = "dodge",stat="identity",
            mapping = aes(y = total))+
            facet_grid(StationName ~ year ) + 
            ylab("Precipitation in Inches") + 
            xlab("Month")+
            theme( axis.text.y=element_text(angle=45), axis.text.x=element_text(angle=45))+
            ggtitle(paste("Monthly Precipitation(in) for RAWS Stations from 2017-2018"))+ 
            theme(strip.text.y = element_text(size = 6,  angle = 90))


## individual chart for 2019 one color pallet
d <- ggplot(data = WaterYearSummary[WaterYearSummary$waterYear == 2019,],
            mapping = aes(x = StationName, fill = StationName))
d + geom_bar(position = "dodge",stat="identity",
             mapping = aes(y = x))+ 
             ylab("Precipitation in Inches") + 
             xlab("RAWS Station") + 
             scale_fill_brewer(palette="Reds") + # "Reds" is palette name
             ggtitle(paste("Annual Precipitation (in) for Water Year (Oct-Nov)",2019))

## individual chart for 2012 many colors
e <- ggplot(data = WaterYearSummary[WaterYearSummary$waterYear == 2012,],
            mapping = aes(x = StationName, fill = StationName))
e + geom_bar(position = "dodge",stat="identity", mapping = aes(y = x)) + 
             ylab("Precipitation in Inches") + 
             xlab("RAWS Station") +
             ggtitle(paste("Annual Precipitation (in) for Water Year (Oct-Nov)",2012))


## Loop through plots - Creates one chart for each year that has the water year summary for all stations  ** Added from update
for(i in yearList){
  plot <-ggplot(data = WaterYearSummary[WaterYearSummary$waterYear == i,],
              mapping = aes(x = StationName, fill = StationName)) + geom_bar(position = "dodge",stat="identity",
              mapping = aes(y = x))+ ylab("Precipitation in Inches") + xlab("RAWS Station ID") + scale_fill_brewer(palette="Reds") + ggtitle(paste(i,": Annual Precipitation (in) for Water Year (Oct-Nov)"))
  print(plot)
  
}


#Monthly Summary
MonthlySummary = AllStations %>% 
  group_by_(.dots=c("StationName","year","month")) %>% 
  summarize(x=sum(total)) 
  

MonthlyAvg = MonthlySummary %>% 
  group_by_(.dots=c("StationName")) %>% 
  summarize(MonthAvg=mean(x)) 

MonthlySummary = left_join(MonthlySummary,MonthlyAvg,"StationName")

library(zoo)
MonthlySummary$YearMonth <- as.yearmon(paste(MonthlySummary$year, MonthlySummary$month), "%Y %m")



#Chart for each Station stacked vertically
z <- ggplot(data = MonthlySummary,
            mapping = aes(x = YearMonth, fill = StationName ))
z + geom_bar(position = "dodge",stat="identity",
             mapping = aes(y = x))+
            facet_grid(StationName ~ .,scales = "free_y"  )+ 
            ylab("Precipitation in Inches") + 
            xlab("Year and Month")+
            theme( axis.text.y=element_text(angle=45)) + 
            scale_x_continuous(breaks=seq(2001, 2019, 2))+
            ggtitle(paste("Monthly Precipitation(in) for RAWS Stations from 2000-", CurrentYear,sep = ""))+
            geom_hline(aes(yintercept = MonthAvg), colour="Black", lty=3)+
            theme(strip.text.y = element_text(size = 6,  angle = 90))

InputStationName = "RAVENDALE"
YearGreater = 2016

#Chart for individual Stations and flexible dates
y <- ggplot(data = MonthlySummary[which( MonthlySummary$StationName == InputStationName & MonthlySummary$year>YearGreater),],
            mapping = aes(x = as.factor(as.yearmon(YearMonth)), fill = StationName ))
y + geom_bar(position = "dodge",stat="identity", mapping = aes(y = x))+
            geom_hline(aes(yintercept = MonthAvg), colour="Black", lty=3)+
            ylab("Precipitation in Inches") + 
            xlab("Year and Month") +
            theme( axis.text.x=element_text(angle=45), axis.text.y=element_text(angle=45)) +
            scale_x_discrete()+ggtitle(paste("Annual Precipitation(in) for ",InputStationName," RAWS Stations from ",(YearGreater + 1)," - ", CurrentYear,sep = ""))
            
library(stringr)
#Chart for individual Stations and single month for example only Jan at Ravendale"
x <- ggplot(data = MonthlySummary[which( MonthlySummary$StationName == InputStationName & MonthlySummary$month == "01"),],
            mapping = aes(x = as.factor(as.yearmon(YearMonth)), fill = StationName ))
x + geom_bar(position = "dodge",stat="identity",
             mapping = aes(y = x))+
  ylab("Precipitation in Inches") + xlab("Year and Month") +
  theme( axis.text.x=element_text(angle=45), axis.text.y=element_text(angle=45)) + scale_x_discrete()+
  ggtitle(paste("Jannuary Monthly Precipitation(in) for ",str_to_title(InputStationName)," RAWS Stations from ",2002," - ", 2019,sep = ""))

#Chart for individual Station all months by year - this one is not very helpful
w <- ggplot(data = MonthlySummary[which( MonthlySummary$StationName == InputStationName ),],
            mapping = aes(x = as.factor(year), fill = StationName ))
w + geom_bar(position = "dodge",stat="identity",
             mapping = aes(y = x))+
  facet_grid(month ~ .  )+ ylab("Precipitation in Inches") + xlab("Year and Month") +
  theme( axis.text.x=element_text(angle=45), axis.text.y=element_text(angle=45)) + scale_x_discrete()+ggtitle(paste("Annual Precipitation(in) for ",InputStationName," RAWS Stations from ",(YearGreater + 1)," - ", CurrentYear,sep = ""))



### workflow for downloading a .csv of all hourly submissions for a single RAWS station with the MesoWest API and then loading it into R
path = "C:/Users/andrew/Documents/R/RAWS/RAWS_Output.csv"  #path to save .csv
start = "201710010000"
end = "201810010000"
stid ="RVDC1"
file = curl::curl_download(paste("http://api.mesowest.net/v2/stations/timeseries?token=fa7799bb71314938921f8ec2b4b65c1d&stid=",stid,"&start=",start,"&end=",end,"&output=csv&units=english"),destfile = path)

#open .csv and remove the separate unit row, need to concatonate this somehow to title
RAWSstation = read.csv(path,  header = T,sep = ",",quote = "",comment.char = "#")
RAWSstation = RAWSstation[-1,]
