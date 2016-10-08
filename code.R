library(dbscan)
rm(list = ls())

getwd()#see the workind directory

setwd('C:/Users/Shahadat Iqbal/Documents/BSM') #set the workind directory



#read data file
data<-read.csv('BSM_Trans_100%.csv')

#subset the data field which I need
data <- subset(data, select=c(PSN,localtime,spd,x,y))
data<-unique(data)

#sort the data 
data <- data[order(-data$PSN,data$localtime),]

data2<-NULL
n1<-nrow(data)
d<-data[1,]
data2<-d
rslt<-NULL

#run a for loop to analyse by ID
for(i in 2:n1) 
{
  if(data[i,1] == d[1,1]){
    d<-data[i,]
    data2<-rbind(data2,d)
  }
  else {
    t1<-(max(data2$localtime)-min(data2$localtime))
    
    if (t1<1500){
      n2<-nrow(data2)
      x1<-round(runif(1,min(data2$localtime),(min(data2$localtime)+30)),1)
      d30<-data2[data2$localtime==x1,]
      
      while (x1<max(data2$localtime)){
        x1<-x1+30
        if(nrow(data2[data2$localtime==x1,])>0){
          d30<-rbind(d30,data2[data2$localtime==x1,])
        }
        else{
          if (nrow(data2[data2$localtime==(x1+1),])>0){
            d30<-rbind(d30,data2[data2$localtime==(x1+1),])
          }
          else{
            if (nrow(data2[data2$localtime==(x1-1),])>0){
              d30<-rbind(d30,data2[data2$localtime==(x1-1),])
            }
            else{
              print("error to find data")
            }
          }
        }
      }
      n3<-nrow(d30)
      x2 <- sample(1:n3, 1)
      data3<-NULL
      data4<-NULL
      data3<-d30[1:x2,]
      data4<-d30[x2:n3,]
      tt1<-(max(data3$localtime)-min(data3$localtime))
      tt2<-(max(data4$localtime)-min(data4$localtime))
      d1<-(max(data3$y)-min(data3$y))
      d2<-(max(data3$y)-min(data3$y))
      r1<-NULL
      r1<-cbind(tt1,d1)
      rslt<-rbind(rslt,r1)
      r2<-NULL
      r2<-cbind(tt2,d2)
      rslt<-rbind(rslt,r2)
    }
  }
}


