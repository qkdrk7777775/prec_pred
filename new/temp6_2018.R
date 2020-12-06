library(readr)
library(rgdal)
library(sp)
library(geoR)
library(ranger)
library(raster)
library(mlr)
library(tuneRanger)
library('fasttime')
library(data.table)
library(dplyr)


GADM=raster::getData('GADM', country='KOR', level=1)
srtm1 <- raster::getData ( 'SRTM', lon = 128, lat = 36)
srtm2 <- raster::getData ( 'SRTM', lon = 128, lat = 34)
srtm3=merge(srtm1,srtm2)

#file load
df1=read_csv('/hdd/data/국립해양조사원/koofs_all_station_lonlat_and_QC.csv',locale=locale(encoding='cp949',tz='Asia/Seoul'))
df2=read_csv('/hdd/data/KMA/kma_buoy_data_lonlat_and_QC.csv',locale=locale(encoding = 'utf-8',tz='Asia/Seoul'))
df3=read_csv('/hdd/data/KMA/kma_lb_data_lonlat_and_QC.csv',locale=locale(encoding = 'utf-8',tz='Asia/Seoul'))

#column extract

columnExtract=function(df,col){
  columnSet=function(df,pattern='X1'){
    idx=grep(pattern=pattern,colnames(df))  
    ifelse(length(idx)!=0,return(df[,-idx]),return(df))
  }
  df=columnSet(df)
  colList=c()
  for(temp in col){
    #print(temp)
    message(grep(temp,colnames(df),value=T))
    colList=c(colList,grep(temp,colnames(df)))
  }
  return(df[,colList])
}

#풍속, 풍향, 기온, 수온, 기압, 시정
col=c('시간','풍속','풍향\\(deg\\)','기온','수온','기압','시정','위도','경도')
df1_=columnExtract(df1,col)
colnames(df1_)=c('times','ws','wd','temp','w_temp','press','vis','lat','lon')

col=c('일시','풍속','풍향\\(deg','습도','기온','수온','기압','위도','경도')
df2_=columnExtract(df2,col)
df2_=df2_[,-grep('GUST',colnames(df2_))]
colnames(df2_)=c('times','ws','wd','rh','temp','w_temp','press','lat','lon')

colnames(df3)
col=c('일시','풍속','풍향\\(deg','습도','기온','수온','기압','위도','경도')
df3_=columnExtract(df3,col)
df3_=df3_[,-grep('최',colnames(df3_))]
colnames(df3_)=c('times','ws','wd','rh','temp','w_temp','press','lat','lon')

bindData=function(df1,df2,df3){
  
  hourExtract=function(df){
    df=as.data.table(df)
    df=df[minute(df$times)==0,]
    df=data.frame(df)
    return(df)
  }
  
  df1=hourExtract(df1)
  df2=hourExtract(df2)
  df3=hourExtract(df3)
  colList=union(union(colnames(df1),colnames(df2)),colnames(df3))
  df1[,setdiff(colList,colnames(df1))]=NA
  df2[,setdiff(colList,colnames(df2))]=NA
  df3[,setdiff(colList,colnames(df3))]=NA
  all_df=bind_rows(df1[,colList],df2[,colList],df3[,colList])
  return(all_df)
}
all_df=bindData(df1_,df2_,df3_)

# read asos
source('/home/ducj/jupyter/prec_pred/new/create_table_asos.R')
df=createAsos(ls)
colnames(df)[c(1:15)]=c('station','stationName','times','temp','prec','ws','wd','rh','vPress','dewT','press','seaPress','sunshine','solar','vis')
df=df[,c('times','temp','prec','wd','ws','rh','dewT','press','vis','lat','lon','alt')]

colList=union(colnames(all_df),colnames(df))
all_df[,setdiff(colList,colnames(all_df))]=NA
df[,setdiff(colList,colnames(df))]=NA
train=bind_rows(df,all_df)

#########################

station1=read_csv('/hdd/data/KMA/koofs_station.csv',locale=locale(encoding='cp949'))[,-1]
station2=read_csv('/hdd/data/KMA/buoy_station.csv',locale=locale(encoding='cp949'))
station3=read_csv('/hdd/data/KMA/LB_station.csv',locale=locale(encoding='cp949'))
kma_station=read_csv('/hdd/data/KMA/META_관측지점정보_20201001223353.csv',locale=locale(encoding='cp949'))[,-1]
kma_station=kma_station[,c('위도','경도')]
station1=station1[,-1]
station1=station1[c('lat','lon')]
station2=station2[c('위도','경도')]
station3=station3[c('위도','경도')]
colnames(kma_station)=colnames(station2)=colnames(station3)=c('lat','lon')
st=unique(rbind(kma_station,station1,station2,station3));st$del=1
coordinates(st)=~lon+lat
from_crs = CRS("+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=500000 +ellps=bessel +units=m")
sp::proj4string(st) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +units=m")
range(seq(min(st@bbox[1,1]),max(st@bbox[1,2]),len=1200))
range(seq(min(st@bbox[2,1]),max(st@bbox[2,2]),len=1200))
lonSep=0.1102957;latSep=0.1077978
#500m 간격
lonLan=length(seq(min(st@bbox[1,1]),max(st@bbox[1,2]),by=lonSep/20))
latLan=length(seq(min(st@bbox[2,1]),max(st@bbox[2,2]),by=latSep/20))
df.grid0=expand.grid(seq(min(st@bbox[1,1]),max(st@bbox[1,2]),by=lonSep/20),
                     seq(min(st@bbox[2,1]),max(st@bbox[2,2]),by=latSep/20))
colnames(df.grid0)=c('lon','lat')
df_pixel0<- sp::SpatialPixelsDataFrame(points = df.grid0[,c('lon','lat')], data = df.grid0)

st <- spTransform(st, from_crs)
df.grid1=expand.grid(seq(min(st@bbox[1,1]),max(st@bbox[1,2]),len=lonLan),
                     seq(min(st@bbox[2,1]),max(st@bbox[2,2]),len=latLan))
colnames(df.grid1)=c('lon','lat')
df_pixel1<- sp::SpatialPixelsDataFrame(points = df.grid1[,c('lon','lat')], data = df.grid1)
sp::proj4string(df_pixel1) <- from_crs
grid.dist=GSIF::buffer.dist(st,df_pixel1,classes=as.factor(1:nrow(st)))
#save(grid.dist,file='/home/ducj/jupyter/prec_pred/new/grid_dist.rda')
dim(grid.dist@data)

prFit=prcomp(grid.dist@data,scale=T)
PRC=data.frame(as.matrix(grid.dist@data)%*%prFit$rotation)[,1:3]
grid.dist@data=PRC

#####################
library(readr)
library(rgdal)
library(sp)
library(geoR)
library(ranger)
library(raster)
library(mlr)
library(tuneRanger)
library('fasttime')
library(data.table)
library(dplyr)

#save(grid.dist,train,file='/home/ducj/jupyter/prec_pred/new/temp.rda')
load(file='/home/ducj/jupyter/prec_pred/new/temp.rda')

######################
st=unique(train[,c('lon','lat')])
train=data.frame(train)
train$station=NA
train=train[year(train$times)==2018,]

for(i in 1:nrow(st)){
  message(i,'/',nrow(st))
  train[intersect(which(train$lon==st[i,][['lon']]),which(train$lat==st[i,][['lat']])),'station']=i
}
train=train[!is.na(train$lat),]
train[is.na(train$alt),'alt']=0
station_unique_name=c('lon','lat','alt','station')
imputeTSRFsp=function(train, grid.dist,station_unique_name=c('lon','lat','alt','station')){
  #imputation Kalman
  full_df_temp=list();full_df_temp2=list()
  for(stat in unique(train$station)){
    stat=as.character(stat)
    message(stat)
    unique_name=c(station_unique_name)
    tempDf=train[train$station==stat,]
    #alt
    na_df=data.frame(times=seq(min(tempDf$times),max(tempDf$times),3600))
    na_df=na_df[as.character(na_df$times)%in%setdiff(as.character(na_df$times),as.character(tempDf$times)),,drop=F]
    if(nrow(na_df)!=0){
      na_df[,colnames(tempDf)[2:ncol(tempDf)]]=NA
      for(i in unique_name){
        na_df[,i]=tempDf[1,i]
      }
      tempDf=dplyr::bind_rows(tempDf,na_df)
    }
    tempDf=tempDf[order(tempDf$times),]
    full_df_temp[[stat]]=tempDf
    numeric_col=names(which(sapply(full_df_temp[[stat]],is.numeric)))
    full_df_temp2[[stat]]=full_df_temp[[stat]]
    for(i in numeric_col){
      message(paste0(i,' impute using Kalman'))
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
  full_df_temp2=dplyr::bind_rows(full_df_temp2)
  # save(raw_df2,full_df_temp2,file='/home/ducj/jupyter/prec_pred/new/temp2_2018.rda')
  load(file='/home/ducj/jupyter/prec_pred/new/temp2_2018.rda')
  
  ################
  if(any(is.na(full_df_temp2))){
    na_count=apply(is.na(full_df_temp2),2,sum)
    
    cnt=0
    for(target_temp in names(na_count[na_count!=0])){
      cnt=cnt+1
      dropIdx=grep('station',colnames(full_df_temp2))
      train_=full_df_temp2[!is.na(full_df_temp2[,target_temp]),-dropIdx]
      test_ =full_df_temp2[is.na(full_df_temp2[,target_temp]),-dropIdx]
      date_list=as.character(unique(test_$times))
      train_$times=as.character(train_$times)
      test_$times=as.character(test_$times)
      
      for(date_temp in date_list){
        message(paste0(cnt,'/',length(na_count),' ',target_temp,' ', date_temp))
        train2=train_[train_$times==date_temp,]
        test2=test_[test_$times==date_temp,]
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
              ,which(full_df_temp2$times==as.POSIXct(date_temp))),target_temp]=
              predict(m.zinc,cbind(test2@data,sp::over(test2,grid.dist)))$prediction
        }
      }
      
      save(full_df_temp2,file=paste0('/home/ducj/jupyter/prec_pred/new/',cnt,'_',length(na_count),target_temp,'_2018.rda'))
    }
  }
  summary(full_df_temp2)
  c('temp','prec','wd','ws','rh','dewT','press','alt','w_temp')
  
  
  load(file='/home/ducj/jupyter/prec_pred/new/9_14w_temp_2018.rda')
  
  ################
  full_df2=full_df_temp2
  range(full_df2$times)
  range(raw_df2$times)
  #save(raw_df2,full_df2,df_pixel0,grid.dist,file='/home/ducj/jupyter/prec_pred/new/temp3.rda')
  
  library(readr)
  library(rgdal)
  library(sp)
  library(geoR)
  library(ranger)
  library(raster)
  library(mlr)
  library(tuneRanger)
  library('fasttime')
  library(data.table)
  library(dplyr)
  
  
  load(file='/home/ducj/jupyter/prec_pred/new/temp3.rda')
  full_df2=full_df2[,-grep('vis',colnames(full_df2))]
  raw_df2=raw_df2[,-grep('vis',colnames(raw_df2))]
  pred.grid=df_pixel0
  dim(df_pixel0@data)
  for(i in colnames(pred.grid@data)){
    pred.grid@data[,i]<-NULL
  }

  #Spatial RandomForest model fit
  output=list()
  for(target in names(which(sort(apply(is.na(raw_df2),2,sum),decreasing = T)!=0))){
    train=full_df2[!is.na(raw_df2[,target]),]
    test=full_df2[is.na(raw_df2[,target]),]
    upper=mean(train[,target],na.rm=T)+1.96*sd(train[,target],na.rm=T)
    lower=mean(train[,target],na.rm=T)-1.96*sd(train[,target],na.rm=T)
    
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

    date_list=sort(as.character(unique(test@data$times)))
    for(date_temp in date_list){
      message(paste0(target,' ', date_temp,' model_1'))
      train2=rm.zinc[rm.zinc$times==date_temp,]
      if(nrow(train2)!=0){
        test2=test[test@data$times==date_temp,]
        
        m.zinc <- ranger::ranger(fm0, train2, quantreg=TRUE, num.trees=150, seed=1)
        full_df2[intersect(which(is.na(raw_df2[,target])),which(full_df2$times==date_temp)),target]=
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
  
  library(readr)
  library(rgdal)
  library(sp)
  library(geoR)
  library(ranger)
  library(raster)
  library(mlr)
  library(tuneRanger)
  library('fasttime')
  library(data.table)
  library(dplyr)
  
  
  #save(full_df2,file='/home/ducj/jupyter/prec_pred/new/imputation_data_2019.rda')
  #save(raw_df2,full_df2,df_pixel0,grid.dist,file='/home/ducj/jupyter/prec_pred/new/temp4.rda')
  load(file='/home/ducj/jupyter/prec_pred/new/temp4.rda')
  
#  write_csv(data.frame(df_pixel0@coords),file=paste0('/hdd/temp/lonlat.csv'))
    summary(full_df2)
    full_df3=full_df2
    from_crs = sp::CRS("+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=500000 +ellps=bessel +units=m")

    pred_list=list()
    date_list2=sort(as.character(unique(full_df2$times)))
    sp::proj4string(grid.dist)=from_crs
    pred.grid=df_pixel0
    for(i in colnames(pred.grid@data)){
      pred.grid@data[,i]<-NULL
    }
    dn0 <- paste(names(grid.dist), collapse="+")
    #pred.grid@coords=sp::spTransform(pred.grid,sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +units=m"))@coords
    library(raster)
    # srtm3@data@values[!is.na(srtm3@data@values)]
    
    # plot(srtm3,add=T)
    # pred.grid@data=dplyr::bind_cols(pred_list)
    # plot(raster(pred.grid[,'2019-12-31 14:00:00']))
    col_names=colnames(full_df3)[c(2:8,12)]
    fileList=list.files('/hdd/temp/')
    for(target in col_names){
      pred_list=list()
      for(date_temp in date_list2){
        message(paste0(target,' ', date_temp,' model_2'))
        if(!paste0(target,date_temp,'.csv')%in%fileList){
          train_=full_df3[full_df3$times==date_temp,]
          sp::coordinates(train_)=~lon+lat
          from_crs = sp::CRS("+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=500000 +ellps=bessel +units=m")
          sp::proj4string(train_) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +units=m")
          train_ <- sp::spTransform(train_, from_crs)
          ov.zinc3_ <- sp::over(train_, grid.dist)
          rm.zinc3_=cbind(train_@data,ov.zinc3_)
          
          fm1 <- as.formula(paste(target," ~ ", dn0))
          
          train3=rm.zinc3_[rm.zinc3_$times==date_temp,]
          m.zinc2 <- ranger::ranger(fm1, train3, quantreg=TRUE, num.trees=150, seed=1)
          write_csv(data.frame(predict(m.zinc2,grid.dist@data)$prediction),file=paste0('/hdd/temp/',target,date_temp,'.csv'))
        }
      }
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
