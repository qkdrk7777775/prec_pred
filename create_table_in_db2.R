library('RMariaDB')
con <- dbConnect(
  drv = RMariaDB::MariaDB(), 
  username = 'ducj',
  password = 'whckdwp1!@', 
  host = '203.128.184.77' ,
  port = 3307,
  db = 'db'
)
dbListTables(con)
df=dbGetQuery(con,'select * from asos')
#last=dbGetQuery(con,'select date from WS_data1 order by date desc LIMIT 1;')

#WS_data<<-dbGetQuery(con,'select * from WS_data1')
dir= './asos2019_2010/'
ls=list.files(dir)
station=read_csv('./META_관측지점정보_20201001223353.csv',locale=locale(encoding='cp949',tz='Asia/Seoul'))
station$시작일=as.POSIXct(station$시작일)
station$종료일=as.POSIXct(station$종료일)
colnames(station)[c(1:5,7:9)]=c('station','st','ed','id','add','lat','lon','alt')
station=station[c(1:5,7:9)]

library(readr)
library(dplyr)
library(microbenchmark)
library(data.table)
list=list()
colnames(list[[i]])
for(i in ls){
list[[i]]=read_csv(paste0(dir,i),locale=locale(encoding='cp949',tz='Asia/Seoul'))
list[[i]]$일시=as.POSIXct(list[[i]]$일시)
for(j in c(5,7,9,11,13,17,19,21)){
list[[i]][which(list[[i]][[j]]==1),j-1]=NA}
list[[i]]=list[[i]][,c(1:3,4,6,8,12,10,20,16,18)]
colnames(list[[i]])=c('station','id','date','temp','prec','ws','rh','wd','sunshine','LocalPressure','BarometricPressure')
list_=list()
for(st in unique(list[[i]]$station)){
  df_=list[[i]][list[[i]]$station==st,]
  st_=station[station$station==st,]

  if(nrow(st_)%in%c(0,1)){
    ls_=left_join(df_,st_)
    list_[[paste(st)]]=ls_
  }else{
    ls2_=list()
    for(k in 1:nrow(st_)){
      ls2_[[k]]=left_join(df_,st_[k,])
    }
    list_[[paste(st)]]=bind_rows(ls2_)
  }
}
list[[i]]=bind_rows(list_)
}

length(list)
df=as.data.frame(bind_rows(list))
df
df$date=as.character(df$date)
# summary(df)
# save(df,file='asos2.rda')
#dbWriteTable(con,'asos',df,overwrite=T)

######aws
dir= './aws2019_2010/'
ls=list.files(dir)

library(readr)
library(dplyr)
list=list()
colnames(list[[i]])
for(i in ls){
  list[[i]]=read_csv(paste0(dir,i),locale=locale(encoding='cp949'))
  list[[i]]$일시=as.POSIXct(as.character(list[[i]]$일시))
  list[[i]]=list[[i]][,c(1:3,4,7,6,10,6)]
  colnames(list[[i]])=c('station','id','date','temp','prec','ws','rh','wd')
}
df2=as.data.frame(bind_rows(list))
df2$date=as.character(df2$date)

# save(df2,file='aws.rda')
#dbWriteTable(con,'aws',df2,overwrite=T)

dbListTables(con)
dbDisconnect(con)
load(file='asos.rda')
head(df)
head(df2)
df=df[df$date>as.POSIXct('2015-06-01'),]
df2=df2[df2$date>as.POSIXct('2015-06-01'),]
library(forecast)
station1=unique(df$station)
station2=unique(df2$station)

df$pred=df$temp
list1=list()
#station=244부터 
for(i in station1){
  message(i, '/',max(station1))
  list1[[i]]=df[df$station==i,]
  for(j in which(is.na(list1[[i]]$pred))){
    if(j %in%1:3){
      list1[[i]]$pred[j]=mean(list1[[i]]$pred[j:(j+4)],na.rm=T)
    }else if(j<2000){
      list1[[i]]$pred[j]=data.frame(forecast(auto.arima(ts(list1[[i]]$pred[1:(j-1)]))))[1,1]
    }else{
    list1[[i]]$pred[j]=data.frame(forecast(auto.arima(ts(list1[[i]]$pred[(j-1999):(j-1)]))))[1,1]}
  }
}
#save(list1,file='임시.rda')
df=bind_rows(list1)
range(df$date)
# save(df,file='asos_temp_보간.rda')
# load(file='asos_temp_보간.rda')
list1=list()
for(i in station1){
  list1[[i]]=df[df$station==i,]
  list1[[i]][,c(paste0('X',1:72))]=NA
  for(j in (2000):nrow(list1[[i]])){
    message(i, '/',max(station1),' ',j,' : ',nrow(list1[[i]]))
  list1[[i]][j,paste0('X',1:72)]=data.frame(forecast(auto.arima(ts(list1[[i]]$pred[(j-1999):(j)])),72))[1:72,1]
  }
}
df=bind_rows(list1)

