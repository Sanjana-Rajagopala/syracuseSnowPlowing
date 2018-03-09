#Read the data from the excel file
library(data.table)
march_2017_data  = data.frame(fread("C:/Users/sanja/Downloads/Snowplow_Data_March_14_2017.csv", verbose=TRUE))


filter_data = data.frame(march_2017_data)
filter_data$repair_type = as.factor(filter_data$activity_type)


#Conversion into Proper Date format
#sample_1 = filter_data[filter_data$truck_name==73,]
sample_1 = filter_data
DateTime = sample_1$date_fixed[1]
DateTime = gsub("T"," ", DateTime)
DateTime = substr(DateTime, 1,19)

DateTime=as.POSIXlt(DateTime,tz=Sys.timezone())
DateTime


for(i in 1:length(sample_1$date_fixed))
{
  sample_1$date_fixed[i] = gsub("T"," ", sample_1$date_fixed[i])
  sample_1$date_fixed[i] = substr(sample_1$date_fixed[i], 1,19)
  
}
sample_1$date_fixed=as.POSIXlt(sample_1$date_fixed,tz=Sys.timezone())


sample_1 = sample_1[order(sample_1$date_fixed),]

x = sample_1$date_fixed[800]
x = x[1]$hour*3600+x[1]$min*60+x[1]$sec

latenightstart = as.POSIXlt("2017-03-14 00:00:00",tz=Sys.timezone())
latenightend = as.POSIXlt("2017-03-14 04:00:00",tz=Sys.timezone())


createTime <- function(li)
{
  return (li[1]$hour*3600+li[1]$min*60+li[1]$sec)
}


earlymornstart=as.POSIXlt("2017-03-14 04:00:00",tz=Sys.timezone())
earlymornend=as.POSIXlt("2017-03-14 07:00:00",tz=Sys.timezone())
mornstart=as.POSIXlt("2017-03-14 07:00:00",tz=Sys.timezone())
mornend=as.POSIXlt("2017-03-14 12:00:00",tz=Sys.timezone())
noonstart=as.POSIXlt("2017-03-14 12:00:00",tz=Sys.timezone())
noonend=as.POSIXlt("2017-03-14 16:00:00",tz=Sys.timezone())
evestart=as.POSIXlt("2017-03-14 16:00:00",tz=Sys.timezone())
eveend=as.POSIXlt("2017-03-14 19:00:00",tz=Sys.timezone())
nightstart=as.POSIXlt("2017-03-14 19:00:00",tz=Sys.timezone())
nightend=as.POSIXlt("2017-03-14 23:59:59",tz=Sys.timezone())


latenightseq = seq(createTime(latenightstart), createTime(latenightend), by=1)
earlymornseq = seq(createTime(earlymornstart), createTime(earlymornend), by=1)
mornseq = seq(createTime(mornstart), createTime(mornend),by=1)
noonseq = seq(createTime(noonstart), createTime(noonend), by=1)
eveseq = seq(createTime(evestart), createTime(eveend), by=1)
nightseq = seq(createTime(nightstart), createTime(nightend))



#Tag the groups of time and remove the idle activity
sample_1$TimeGroup <- NA
for(i in 1:length(sample_1$X))
{
  tempTime = createTime(sample_1$date_fixed[i])
  if(tempTime  %in%  latenightseq)
  {
    sample_1$TimeGroup[i] = "Late Night"
  }
  else if(tempTime  %in%  earlymornseq)
  {
    sample_1$TimeGroup[i] = "Early Morning"
  }
  else if(tempTime  %in%  mornseq)
  {
    sample_1$TimeGroup[i] = "Morning"
  }
  else if(tempTime  %in%  noonseq)
  {
    sample_1$TimeGroup[i] = "Noon"
  }
  else if(tempTime  %in%  eveseq)
  {
    sample_1$TimeGroup[i] = "Evening"
    print(tempTime)
  }
  else if(tempTime  %in%  nightseq)
  {
    sample_1$TimeGroup[i] = "Night"
  }
  
  
}

revisedsSample = sample_1
for(i in 2:length(revisedsSample$X))
{
  if(revisedsSample$activity_type[i]== "")
  {
    revisedsSample$activity_type[i] =  revisedsSample$activity_type[i-1]
  }
}

tempData = revisedsSample
inactiveStates = c("Ignition Off", "Spreader Off Stop Moving", "Stop Moving")
for(i in 22:length(tempData$X))
{
  if(tempData$activity_type[i] %in% inactiveStates)
  {
    tempData= tempData[-i,]
  }
  
  #else if(tempData$activity_type[i] == inactiveStates[2])
  #{
   # tempData= tempData[-i,]
  #}
  #else if (tempData$activity_type[i] == inactiveStates[3])
  #{
   # tempData= tempData[-i,]
  #}
}

finalData <- tempData
write.csv(finalData,'snowPlowing-mar-14.csv')
