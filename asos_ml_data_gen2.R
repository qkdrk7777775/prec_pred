library(readr)
library(dplyr)
#save(df,file='asos.rda')
#save(df2,file='aws.rda')
#load("~/R/git/kma/aws.rda")
load("/home/ducj/R/git/kma/asos.rda")
#df$date=as.POSIXct(df$date)
#df2$date=as.POSIXct(df2$date)
#df=df[df$date>as.POSIXct('2014-12-31'),]
#df2=df2[df2$date>as.POSIXct('2014-12-31'),]
library(zoo)
library(forecast)
station1=sort(unique(df$station))
#station2=sort(unique(df2$station))

df$pred=df$temp
#station=244부터 
for(i in station1[-96]){
  if(!c(paste0('asos_',i,'.csv'))%in%list.files('/home/ducj/R/git/kma/asos_temp/')){
    write.csv(NULL,paste0('/home/ducj/R/git/kma/asos_temp/asos_',i,'.csv'))
    list1=list()
    message(i, '/',max(station1))
    list1[[i]]=df[df$station==i,]
    #결측 없는 데이터 생성
    for(j in which(is.na(list1[[i]]$pred))){
      if(j %in%1:3){
        list1[[i]]$pred[j]=mean(list1[[i]]$pred[j:(j+4)],na.rm=T)
      }else if(j<2000){
        list1[[i]]$pred[j]=data.frame(forecast(auto.arima(ts(list1[[i]]$pred[1:(j-1)]))))[1,1]
      }else{
        list1[[i]]$pred[j]=data.frame(forecast(auto.arima(ts(list1[[i]]$pred[(j-1999):(j-1)]))))[1,1]
      }
    }
    n=(nrow(list1[[i]])%/%40+2)
    for(k in 1:(nrow(list1[[i]])%/%40+2)){
      message(paste0('forecasting_data_generate ',k,' : ',n))
      try(silent = T,{
        list1[[i]][2160:2200+(k-1)*40,paste0('X',1:72)]=
          rollapply(list1[[i]]$pred[(1:2200)+(k-1)*40], 2160, function(x){data.frame(forecast(auto.arima(x),72))[,1]})
      })
      # del=rollapply(list1[[i]]$pred[(1:2200)+(k-1)*40], 2160, function(x){data.frame(forecast(auto.arima(x),72))[,1]})
      # dim(list1[[i]][2160:2200+(k-1)*40,paste0('X',1:72)])
      # dim(del)
    }
    list1[[i]]$date=as.character(list1[[i]]$date)
    write.csv(list1[[i]],paste0('/home/ducj/R/git/kma/asos_temp/asos_',i,'.csv'))
  }  
}


