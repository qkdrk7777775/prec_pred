library(readr)
library(rgdal)
library(sp)
library(geoR)
library(ranger)
library(raster)
library(mlr)
library(tuneRanger)
#demo(meuse)
#proj4string(meuse)
GADM=raster::getData('GADM', country='KOR', level=1)
srtm1 <- raster::getData ( 'SRTM', lon = 128, lat = 36)
srtm2 <- raster::getData ( 'SRTM', lon = 128, lat = 34)
srtm3=merge(srtm1,srtm2)
df=read_csv('G:/HDD1/data/국립해양조사원/koofs_all_station_lonlat_and_QC.csv',locale=locale(encoding='cp949',tz='Asia/Seoul'))
df2=read_csv('G:/HDD1/data/국립해양조사원/kma_buoy_data_lonlat_and_QC.csv',locale=locale(encoding = 'utf-8',tz='Asia/Seoul'))
df3=read_csv('G:/HDD1/data/국립해양조사원/kma_cw_buoy_data_lonlat_and_QC.csv',locale=locale(encoding = 'utf-8',tz='Asia/Seoul'))
df4=read_csv('G:/HDD1/data/국립해양조사원/kma_lb_data_lonlat_and_QC.csv',locale=locale(encoding = 'utf-8',tz='Asia/Seoul'))
colnames(df)
df=df[,c(2:21)]
colnames(df)[c(1,15,14,5,11,13,8,19,20,7)]=c('times','a_press','temp','w_temp','ws','wd','유의파주기','lat','lon','유의파고(m)')
df=df[c('times','lat','lon','a_press','temp','w_temp','ws','wd','유의파주기','유의파고(m)')]
#df=df[c('times','lat','lon','유의파고(m)')]
# colnames(df)=c('times','lat','lon','target')
df2=df2[,c(2:17)]
colnames(df2)
colnames(df2)[c(2,3,4,7,8,9,11,12,13,14,15,16)]=c('times','ws','wd','rh','temp','w_temp','유의파고','평균파고','파주기','파향','lat','lon')
# df2=df2[c('times','lat','lon','유의파고')]
# colnames(df2)=c('times','lat','lon','target')
colnames(df3)
#colnames(df3)[c(3:10)]=c('times','w_temp','최대파고','유의파고','평균파고','파주기','lat','lon')
#df3=df3[c('times','lat','lon','target')]
colnames(df4)[3:19]=c('times','ws','wd','최대순간풍향','최대순간풍속','해면기압','rh','temp','일최저기온','일최고기온','w_temp','최대파고','target','파주기','수위','lat','lon')
col=c('times','lat','lon','a_press','h_press','temp','w_temp','ws','rh','유의파주기','유의파고')
colnames(df)[10]='유의파고'
colnames(df2)[6]='h_press'
colnames(df3)
colnames(df3)=c('X1','station','times','w_temp','최대파고','유의파고','평균파고','파주기','lat','lon')
colnames(df4)[c(8,15)]=c('h_press','유의파고')
cols=setdiff(col,colnames(df))
df[,cols]=NA
cols=setdiff(col,colnames(df2))
df2[,cols]=NA
cols=setdiff(col,colnames(df3))
df3[,cols]=NA
cols=setdiff(col,colnames(df4))
df4[,cols]=NA
library(dplyr)
all_df=bind_rows(df[,col],df2[,col],df3[,col],df4[,col])

# df4=df4[c('times','lat','lon','target')]

all_df=rbind(df,df2,df3,df4)



all_df$hour=as.POSIXct(substr(all_df$times,1,13),'%Y-%m-%d %H',tz='Asia/Seoul')
#all_df_temp=all_df
all_df=all_df_temp
all_df2=all_df[which(all_df$times==all_df$hour),]
# all_df=data.table::setDT(all_df)[,.(mean=mean(target,na.rm=T)),c('hour','lat','lon')]
# colnames(all_df)=c('times','lat','lon','target')
save(all_df2,file='C:/Users/cj/Desktop/imputeTSRFsp/all_df2.rda')
#save(all_df,st)
#########################
library(readr)
library(rgdal)
library(sp)
library(geoR)
library(ranger)
library(raster)
library(mlr)
library(tuneRanger)
all_df2
load('C:/Users/cj/Desktop/imputeTSRFsp/all_df2.rda')
load('C:/Users/cj/Desktop/imputeTSRFsp/asos2.rda')
df=df[,c('date','temp','prec','ws','rh','LocalPressure','BarometricPressure','lat','lon')]
colnames(df)
all_df2

colnames(all_df2)=c('date','lat','lon','LocalPressure','BarometricPressure','temp','w_temp','ws','rh','유의파주기','유의파고','hour')
unique(all_df2[,c('lat','lon')])
colnames(all_df2)
colnames(df)
df[,c('w_temp','유의파주기','유의파고')]=NA
library(dplyr)
df$date=as.POSIXct(df$date)

full_df=bind_rows(all_df2[,c('date','lat','lon','LocalPressure','BarometricPressure','temp','w_temp','ws','rh','유의파주기','유의파고')]
,df[,c('date','lat','lon','LocalPressure','BarometricPressure','temp','w_temp','ws','rh','유의파주기','유의파고')])
# save(full_df,file='C:/Users/cj/Desktop/imputeTSRFsp/full_df.rda')

station=read_csv('./koofs_station.csv',locale=locale(encoding='cp949'))[,-1]
station2=read_csv('./CWBuoy_station.csv',locale=locale(encoding='cp949'))
station3=read_csv('./buoy_station.csv',locale=locale(encoding='cp949'))
station4=read_csv('./LB_station.csv',locale=locale(encoding='cp949'))
library(readr)
kma_station=read_csv('./META_관측지점정보_20201001223353.csv',locale=locale(encoding='cp949'))[,-1]
kma_station=kma_station[,c('위도','경도')]
station=station[,-1]
station=station[c('lat','lon')]
station2=station2[c('위도','경도')]
station3=station3[c('위도','경도')]
station4=station4[c('위도','경도')]
colnames(kma_station)=colnames(station2)=colnames(station3)=colnames(station4)=c('lat','lon')

#install.packages('ncdf4')

st=rbind(kma_station,station,station2,station3,station4)
st$del=1
coordinates(st)=~lon+lat
from_crs = CRS("+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=500000 +ellps=bessel +units=m")
sp::proj4string(st) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +units=m")
df.grid0=expand.grid(seq(min(st@bbox[1,1]),max(st@bbox[1,2]),len=500),
                     seq(min(st@bbox[2,1]),max(st@bbox[2,2]),len=500))
colnames(df.grid0)=c('lon','lat')
df_pixel0<- sp::SpatialPixelsDataFrame(points = df.grid0[,c('lon','lat')], data = df.grid0)
# save(df_pixel0,file='df_pixel0.rda')
st <- spTransform(st, from_crs)
df.grid1=expand.grid(seq(min(st@bbox[1,1]),max(st@bbox[1,2]),len=500),
                     seq(min(st@bbox[2,1]),max(st@bbox[2,2]),len=500))
colnames(df.grid1)=c('lon','lat')
df_pixel1<- sp::SpatialPixelsDataFrame(points = df.grid1[,c('lon','lat')], data = df.grid1)
sp::proj4string(df_pixel1) <- from_crs
grid.dist=GSIF::buffer.dist(st,df_pixel1,classes=as.factor(1:nrow(st)))
#save(df.grid0,df_pixel0,df_pixel1,grid.dist,file='C:/Users/cj/Desktop/imputeTSRFsp/grid_df.rda')
######################
library(readr)
library(rgdal)
library(sp)
library(geoR)
library(ranger)
library(raster)
library(mlr)
library(tuneRanger)

load(file='./poster/grid_df.rda')
load(file='./poster/full_df.rda')
st=unique(full_df[,c('lon','lat')])
full_df$station=NA

for(i in 1:nrow(st)){
  message(i)
  full_df[intersect(which(full_df$lon==st[i,][['lon']]),which(full_df$lat==st[i,][['lat']])),'station']=i
}
full_df=full_df[!is.na(full_df$lat),]
#등표(cw)  독도 
c(131.8694, 37.2372)
#부이 서귀포 
c(127.0228, 33.1281)
#서해170     
c(124.0569, 36.1333)
#2,125,280
full_df=full_df[-which(full_df$lon%in%unlist(st[c(2,125,280),'lon'])),]
sp::coordinates(full_df)=~lon+lat
full_df@data

imputeTSRFsp=function(full_df, grid.dist,station_unique_name=c('alt','station')){
  #imputation Kalman
  full_df_temp=list();full_df_temp2=list()
  for(stat in unique(full_df$station)){
    stat=as.character(stat)
    message(stat)
    unique_name=colnames(full_df@coords)
    unique_name=c(unique_name,station_unique_name)
    full_df_temp[[stat]]=data.frame(full_df[full_df$station==stat,])
    na_df=data.frame(date=seq(min(full_df_temp[[stat]]$date),max(full_df_temp[[stat]]$date),3600))
    for(i in 2:ncol(full_df_temp[[stat]])){
      na_df[,colnames(full_df_temp[[stat]])[i]]=NA
    }
    for(i in unique_name){
      na_df[,i]=full_df_temp[[stat]][1,i]
    }
    full_df_temp[[stat]]=dplyr::bind_rows(full_df_temp[[stat]],na_df)

    #full_df_temp[['full_data']][[stat]]
    numeric_col=names(which(sapply(full_df_temp[[stat]],is.numeric)))
    full_df_temp2[[stat]]=full_df_temp[[stat]]
    for(i in numeric_col){
      #message(paste0(i,' impute using Kalman'))
      idx=is.na(full_df_temp[[stat]][,i])
      if(sum(idx)!=0){
        tryCatch({
          full_df_temp2[[stat]][,i]<-imputeTS::na_kalman(full_df_temp[[stat]][,i])
        }
        ,error=function(x){
          full_df_temp2[[stat]][idx,i]<-mean(full_df_temp[[stat]][-idx,i],na.rm=T)
        })
      }
    }
  }
  raw_df2 =dplyr::bind_rows(full_df_temp)
  # save(raw_df2,file='./raw_df2.rda')
  raw_df2$optional=NULL
  full_df_temp2$optional=NULL
  full_df_temp2=dplyr::bind_rows(full_df_temp2)
  # save(full_df_temp2,file='./full_df_temp2.rda')
  # load(file='./full_df_temp2.rda')
  full_df_temp2$optional=NULL
  full_df_temp2$date=as.character(full_df_temp2$date)
  ################
  if(any(is.na(full_df_temp2))){
    # save(full_df_temp2,grid.dist,file='./temp_.rda')
    # load(file='./temp_.rda')
    na_count=apply(is.na(full_df_temp2),2,sum)
    # colnames(full_df_temp2)
    for(target_temp in names(na_count[na_count!=0])){
      train_=full_df_temp2[!is.na(full_df_temp2[,target_temp]),-ncol(full_df_temp2)]
      test_ =full_df_temp2[is.na(full_df_temp2[,target_temp]),-ncol(full_df_temp2)]
      date_list=as.character(unique(test_$date))
      for(date_temp in date_list){
        message(paste0(target_temp,' ', date_temp))
        train2=train_[train_$date==date_temp,]
        test2=test_[test_$date==date_temp,]
        if(all(c(nrow(train2)!=0,nrow(test2)!=0))){
          sp::coordinates(train2)=~lon+lat
          sp::coordinates(test2 )=~lon+lat
          from_crs = sp::CRS("+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=500000 +ellps=bessel +units=m")
          sp::proj4string(train2) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +units=m")
          sp::proj4string(test2 ) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +units=m")
          train2 <- sp::spTransform(train2, from_crs)
          test2  <- sp::spTransform(test2, from_crs)
          dn0 <- paste(names(grid.dist), collapse="+")
          fm0 <- as.formula(paste(target_temp," ~ ", dn0))
          proj4string(train2)=proj4string(grid.dist)
          proj4string(test2)=proj4string(grid.dist)
          ov.zinc_=sp::over(train2, grid.dist)
          rm.zinc_=cbind(train2@data,ov.zinc_)
  
          proj4string(test2)=proj4string(grid.dist)
          m.zinc <- ranger::ranger(fm0, rm.zinc_,  num.trees=150, seed=1)
          full_df_temp2[intersect(which(is.na(full_df_temp2[,target_temp]))
                                  ,which(full_df_temp2$date==date_temp)),target_temp]=
            predict(m.zinc,cbind(test2@data,sp::over(test2,grid.dist)))$prediction
        }
      }
    }
  }
  load(file='./full_df_temp2.rda')
  # summary(full_df_temp2)
  
  ################
  
  load(file='./poster/grid_df.rda')
  load(file='./poster/full_df.rda')
  load(file='./full_df_temp2.rda')
  load(file='./raw_df2.rda')
  full_df2=full_df_temp2
  full_df2=full_df2[which(substr(full_df2$date,1,4)=='2019'),]
  raw_df2=raw_df2[which(substr(raw_df2$date,1,4)=='2019'),]
  pred.grid=grid.dist
  for(i in colnames(pred.grid@data)){
    pred.grid@data[,i]<-NULL
  }

  #Spatial RandomForest model fit
  output=list()
  for(target in names(which(sort(apply(is.na(raw_df2),2,sum),decreasing = T)!=0))[1:8]){
    train=full_df2[!is.na(raw_df2[,target]),]
    test=full_df2[is.na(raw_df2[,target]),]
    upper=mean(train[,target],na.rm=T)+1.96*sd(train[,target],na.rm=T)
    lower=mean(train[,target],na.rm=T)-1.96*sd(train[,target],na.rm=T)
    
    #temp_df2$station=NULL
    sp::coordinates(train)=~lon+lat
    sp::coordinates(test )=~lon+lat
    from_crs = sp::CRS("+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=500000 +ellps=bessel +units=m")
    sp::proj4string(train) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +units=m")
    sp::proj4string(test ) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +units=m")
    train <- sp::spTransform(train, from_crs)
    test  <- sp::spTransform(test, from_crs)
    dn0 <- paste(names(grid.dist), collapse="+")

    fm0 <- as.formula(paste(target," ~ ", dn0,'+',
                            paste0(setdiff(setdiff(names(which(sapply(train@data,is.numeric))),'station'),target),collapse = '+')))
    sp::proj4string(grid.dist)=from_crs
    ov.zinc <- sp::over(train, grid.dist)
    rm.zinc=cbind(train@data,ov.zinc)

    date_list=sort(as.character(unique(test@data$date)))
    for(date_temp in date_list){
      message(paste0(target,' ', date_temp,' model_1'))
      train2=rm.zinc[rm.zinc$date==date_temp,]
      if(nrow(train2)!=0){
        test2=test[test@data$date==date_temp,]
        
        m.zinc <- ranger::ranger(fm0, train2, quantreg=TRUE, num.trees=150, seed=1)
        full_df2[intersect(which(is.na(raw_df2[,target])),which(full_df2$date==date_temp)),target]=
          predict(m.zinc,cbind(test2@data,sp::over(test2,grid.dist)))$prediction
      }else{
        if(sum((test2@data[,target]>upper)|(test2@data[,target]<lower))){
          message('data outlier')
          break
        }
        
        print('train data nrow 0')
      }
    }
  }
  
  #save(full_df2,file='imputation_data_2019.rda')
  load(file='imputation_data_2019.rda')
  load(file='./poster/grid_df.rda')
  load(file='df_pixel0.rda')
  # load(file='./poster/full_df.rda')
  # load(file='./full_df_temp2.rda')
  # load(file='./raw_df2.rda')
  # 

  summary(full_df2)
    full_df3=full_df2
    from_crs = sp::CRS("+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=500000 +ellps=bessel +units=m")

    pred_list=list()
    date_list2=sort(as.character(unique(full_df2$date)))
    sp::proj4string(grid.dist)=from_crs
    pred.grid=grid.dist
    for(i in colnames(pred.grid@data)){
      pred.grid@data[,i]<-NULL
    }
    dn0 <- paste(names(grid.dist), collapse="+")
    pred.grid@coords=sp::spTransform(pred.grid,sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +units=m"))@coords
    library(raster)
    # srtm3@data@values[!is.na(srtm3@data@values)]
    
    # plot(srtm3,add=T)
    # pred.grid@data=dplyr::bind_cols(pred_list)
    # plot(raster(pred.grid[,'2019-12-31 14:00:00']))
    col_names=colnames(full_df3)[4:11]
    
    for(target in col_names[c(8,1:2,4:7)]){
      pred_list=list()
      for(date_temp in date_list2){
        message(paste0(target,' ', date_temp,' model_2'))
        train_=full_df3[full_df3$date==date_temp,]
        sp::coordinates(train_)=~lon+lat
        from_crs = sp::CRS("+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=500000 +ellps=bessel +units=m")
        sp::proj4string(train_) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +units=m")
        train_ <- sp::spTransform(train_, from_crs)
        ov.zinc3_ <- sp::over(train_, grid.dist)
        rm.zinc3_=cbind(train_@data,ov.zinc3_)
        
        fm1 <- as.formula(paste(target," ~ ", dn0))
        
        train3=rm.zinc3_[rm.zinc3_$date==date_temp,]
        m.zinc2 <- ranger::ranger(fm1, train3, quantreg=TRUE, num.trees=150, seed=1)
        # pred_list[[date_temp]]=
        #           predict(m.zinc2,dplyr::bind_rows(grid.dist@data[which.min((df_pixel0@coords[,1]-c(131.8694))^2+(df_pixel0@coords[,2]-c(37.2372))^2),],
        #           grid.dist@data[which.min((df_pixel0@coords[,1]-c(127.0228))^2+(df_pixel0@coords[,2]-c(33.1281))^2),],
        #           grid.dist@data[which.min((df_pixel0@coords[,1]-c(124.0569))^2+(df_pixel0@coords[,2]-c(36.1333))^2),]))$prediction
        pred_list[[date_temp]]=predict(m.zinc2,grid.dist@data)$prediction
      }
      # save(pred_list,file=paste0('./',target,'_pred_list_points.rda'))
    save(pred_list,file=paste0('./',target,'_pred_list.rda'))
    }
    output[[target]]=pred.grid
  # plot(raster(pred.grid[,1]))

  return(list(Points=full_df2,Grid=output,kalmanPoints=full_df_temp2))
}

output=imputeTSRFsp(full_df,grid.dist)
















for(i in date_list){
  day=gsub(':','',gsub('-| ','_',as.POSIXct(i,origin=as.POSIXct('1970-01-01 9:00:00'))))
  print(as.POSIXct(i,origin=as.POSIXct('1970-01-01 9:00:00')))

  if(!paste0(day,'.csv')%in%list.files('Y:/임시업무/공간랜덤포래스트/새 폴더 (2)/')){
    all_df2=all_df[all_df$times==as.POSIXct(i,origin=as.POSIXct('1970-01-01 9:00:00')),]
    sp::coordinates(all_df2)=~lon+lat
    sp::proj4string(all_df2) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +units=m")
    all_df2_=all_df2
    all_df2 <- spTransform(all_df2, from_crs)
    dn0 <- paste(names(grid.dist0), collapse="+")
    fm0 <- as.formula(paste("target ~ ", dn0))
    ov.zinc <- over(all_df2["target"], grid.dist0)
    ## Get a good estimate of "mtry" (fine-tuning):
    rm.zinc <- cbind(all_df2@data["target"], ov.zinc)
    rm.zinc=rm.zinc[complete.cases(rm.zinc),]
    rt.zinc <- makeRegrTask(data = rm.zinc, target = "target", check.data=FALSE)
    set.seed(1)
    #t.zinc <- tuneRanger(rt.zinc, num.trees = 150, build.final.model = FALSE, parameters = list(replace = FALSE),show.info=F)
    #pars.zinc = list(mtry=t.zinc$recommended.pars$mtry, min.node.size=t.zinc$recommended.pars$min.node.size, sample.fraction=t.zinc$recommended.pars$sample.fraction, num.trees=150, seed=1)
    set.seed(1)
    m.zinc <- ranger(fm0, rm.zinc, quantreg=TRUE, num.trees=150, seed=1)
    df_pixel0$rf = predict(m.zinc, grid.dist0@data)$predictions


    # df_points=df.grid@data
    # sp::coordinates(df_points)=~lon+lat
    # sp::proj4string(df_points)<-from_crs
    # df_points=spTransform(df_points,sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +units=m"))
    #plot(raster(df_pixel0["rf"]), col=leg, main="Random Forest (RF), buffers", axes=TRUE, box=FALSE)
    #plot(GADM,add=T,margin=F)
    #points(all_df2_, pch=16)
    library(RColorBrewer)
    day=gsub(':','',gsub('-| ','_',as.POSIXct(i,origin=as.POSIXct('1970-01-01 9:00:00'))))
    png(width=1000,height=1000,filename = paste0('Y:/임시업무/공간랜덤포래스트/새 폴더/',day,'.png'),type='cairo',family='malgun')
    par(mfrow=c(1,1))
    leg = c("#0000ff", "#0028d7", "#0050af", "#007986", "#00a15e", "#00ca35", "#00f20d", "#1aff00", "#43ff00", "#6bff00", "#94ff00", "#bcff00", "#e5ff00", "#fff200", "#ffca00", "#ffa100", "#ff7900", "#ff5000", "#ff2800", "#ff0000")
    cuts=seq(0,10,.1)
    pal <- colorRampPalette(leg)
    plot(raster(df_pixel0["rf"]), breaks=cuts, col = pal(30),main=day,cex=1.5,cex.main=3,cex.axis=2) #plot with de
    save(df_pixel0,all_df2_,file=paste0('Y:/임시업무/공간랜덤포래스트/새 폴더/',day,'.rda'))
    plot(GADM,add=T,margin=F)
    points(all_df2_, pch=16,col=pal(30)[cut(all_df2_$target,breaks = 30)],cex=2)
    points(all_df2_,pch=1,cex=2)
    dev.off()

    write_csv(data.frame(time=as.POSIXct(i,origin=as.POSIXct('1970-01-01 9:00:00')),df_pixel0@data),paste0('Y:/임시업무/공간랜덤포래스트/새 폴더 (2)/',day,'.csv'))
  }
}

dim(grid.dist@data)
dim(rm.zinc)
##################

coordinates(df.grid)<-~lon+lat
from_crs = CRS("+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=500000 +ellps=bessel +units=m")
to_crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

proj4string(df.grid) <- crs(to_crs)
df.grid <- spTransform(df.grid, from_crs)
bbox=df.grid@bbox
bbox[,'min']=bbox[,'min']-4000
bbox[,'max']=bbox[,'max']+4000
df.grid=plotKML::vect2rast(df.grid,cell.size=2000,bbox=bbox)
df.grid$mask=1
df.grid=SpatialPixelsDataFrame(points=df.grid,data=df.grid)
df.grid=as(df.grid['mask'],'SpatialPixelsDataFrame')

de2km=as(de2km['mask'],'SpatialPixelsDataFrame')
de.dist1=GSIF::buffer.dist(sic.val['value'],de2km,as.factor(1:nrow(sic.val@data)))


de.dist0=GSIF::buffer.dist(df2['w_temp'],df.grid,as.factor(1:nrow(df2@data)))

dist=GSIF::buffer.dist(df2['w_temp'],df.grid,factor(1:nrow(df2@data)))



###############
m=SpatialPixelsDataFrame(points=df.grid,data=df.grid)


bbox=sic.val@bbox
bbox[,'min']=bbox[,'min']-4000
bbox[,'max']=bbox[,'max']+4000
de2km=plotKML::vect2rast(sic.val,cell.size=200,bbox=bbox)
summary(sic.val@data)
de2km@grid@cells.dim
de2km$mask=1
de2km=as(de2km['mask'],'SpatialPixelsDataFrame')
de.dist0=GSIF::buffer.dist(sic.val['joker'],de2km,as.factor(1:nrow(sic.val@data)))

# library(GSIF)
# library("spacetime")
# library("sirad")
# library("semiArtificial")
# require("raster", quietly = TRUE)
requireNamespace('raser',quietly=T)
#install.packages(c('sirad'))
dim(meuse['zinc']@data)
#유의파고 QC 스크립트
grid.dist0=GSIF::buffer.dist(meuse['zinc'],meuse.grid[1],as.factor(1:nrow(meuse)))
dn0=paste(names(grid.dist0),collapse = '+')
fm0=as.formula(paste0('zinc~',dn0))
ov.zinc=over(meuse['zinc'],grid.dist0)
rm.zinc=cbind(meuse@data['zinc'],ov.zinc)
m.zinc=ranger(fm0,rm.zinc,quantreg=T,num.trees=150,seed=1,importance = 'impurity')
#sort(ranger::importance(m.zinc))
zinc.rfd=predict(m.zinc,grid.dist0@data,type='quantiles',quantiles=c(0.159,.5,.841))$predictions
str(zinc.rfd)
meuse.grid$zinc_rfd=zinc.rfd[,2]
meuse.grid$zinc_rfd_range=(zinc.rfd[,3]-zinc.rfd[,3])/2

zinc.geo=as.geodata(meuse['zinc'])
ini.v=c(var(log1p(zinc.geo$data)),500)
zinc.vgm=likfit(zinc.geo,lambda=0,ini=ini.v,cov.model = 'exponential')
locs=meuse.grid@coords
zinc.ok=krige.conv(zinc.geo,location=locs,krig=krige.control(obj.model = zinc.vgm))
meuse.grid$zinc_ok=zinc.ok$predict
meuse.grid$zinc_rfd_range=sqrt(zinc.ok$krige.var)
plot(meuse.grid['zinc_rfd_range'])
plot(meuse.grid['zinc_rfd'],zlim=c(0,700))
summary(df)
grep(colnames(df),)
df[,-c(18)]

#install.packages('intamap')
library(intamap)
library(gstat)
data(sic2004)
coordinates(sic.val)<-~x+y
sic.val$value=sic.val$joker
coordinates(sic.test)<-~x+y
#pred.sic2004=interpolate(sic.val,sic.test,maximumTime =  90)
#timeModels=generateTimeModels()

bbox=sic.val@bbox
bbox[,'min']=bbox[,'min']-4000
bbox[,'max']=bbox[,'max']+4000
bbox[,'min']
bbox[,'max']

de2km=plotKML::vect2rast(sic.val,cell.size=2000,bbox=bbox)
de2km$mask=1
de2km=as(de2km['mask'],'SpatialPixelsDataFrame')
de.dist1=GSIF::buffer.dist(sic.val['value'],de2km,as.factor(1:nrow(sic.val@data)))

df.grid=plotKML::vect2rast(df.grid,cell.size=2000,bbox=bbox)
df.grid=SpatialPixelsDataFrame(points=df.grid,data=df.grid)

ov.de=over(sic.val['joker'],de.dist0)
de.dn0=paste(names(de.dist0),collapse='+')
de.fm1=as.formula(paste('joker ~',de.dn0))
de.rm=do.call(cbind,list(sic.val@data['joker'],ov.de))
m1.gamma=ranger(de.fm1,de.rm[complete.cases(de.rm),],mtry=1)
m1.gamma
de2km$gamma_rfd1=predict(m1.gamma,de.dist0@data)$predictions
ov.test=over(sic.test,de2km['gamma_rfd1'])
sd(sic.test$joker-ov.test$gamma_rfd1,na.rm=T)


head(df)


ls=list()
date=sort(unique(df$관측시간))
for(i in as.character(date[1])){
  print(i)
  ls[[i]]=df[df$관측시간==i]
}


data(meuse.grid)
m = SpatialPixelsDataFrame(points = meuse.grid[c("x", "y")], data = meuse.grid)
