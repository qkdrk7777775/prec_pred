dir= '/hdd/data/KMA/asos2019_2010/'
ls=list.files(dir)
library(readr)
library(dplyr)
createAsos=function(ls){
  list=list()
  for(i in ls){
    message(i)
    list[[i]]=suppressWarnings(read_csv(paste0(dir,i),locale=locale(encoding='cp949',tz='Asia/Seoul'),col_types = cols()))
    for(j in grep('플래그',colnames(list[[i]]))){
      list[[i]][which(list[[i]][[j]]==1),j-1]=NA}
      list[[i]]=list[[i]][,-grep('플래그|운|적설|지면상태|현상번호',colnames(list[[i]]))]
  }
  df=bind_rows(list)
  station1=unique(df$지점)
  
  station=read_csv('/hdd/data/KMA/META_관측지점정보_20201001223353.csv',locale=locale(encoding='cp949',tz='Asia/Seoul'))
  station$시작일=as.POSIXct(station$시작일)
  station$종료일=as.POSIXct(station$종료일)
  colnames(station)[c(1:5,7:9)]=c('지점','st','ed','id','add','lat','lon','alt')
  station=station[c(1:5,7:9)]
  list_=list()
  for(st in unique(df$지점)){
    df_=df[df$지점==st,]
    st_=station[station$지점==st,]
    if(nrow(st_)%in%c(0,1)){
      ls_=left_join(df_,st_)
      list_[[paste(st)]]=ls_
    }else{
      ls2_=list()
      for(k in 1:nrow(st_)){
        ls2_[[k]]=suppressMessages(left_join(df_,st_[k,]))
      }
      list_[[paste(st)]]=bind_rows(ls2_)
    }
  }
  df=bind_rows(list_)
return(df)}

