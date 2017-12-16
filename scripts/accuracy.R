# accuracy.R
# Version 1.0
# Accuracy Assessment
#
# Project: NRT_Compare
# By xjtang
# Created On: 12/5/2016
# Last Update: 12/18/2016
#
# Version 1.0 - 12/18/2016
#   Calculate accuracy when map is different from stratification
#
# -------------------------------------------------------

# AA
# calclate accuracy
mPath <- '/Users/xjtang/Applications/Dropbox/'
sfile <- paste(mPath,'NRT/Analysis/accuracy/strata.csv',sep='')
rFile <- paste(mPath,'NRT/Analysis/accuracy/mc.csv',sep='')
testS <- paste(mPath,'NRT/Analysis/accuracy/test_strata.csv',sep='')
testR <- paste(mPath,'NRT/Analysis/accuracy/test_ref.csv',sep='')
AA <- function(sta,ref,verbose=T){

  # read input data
  sta2 <- read.table(sta,sep=',',stringsAsFactors=F)
  if(typeof(ref)=='character'){
    ref2 <- read.table(ref,sep=',',stringsAsFactors=F,header=T)
  }else{
    ref2 <- ref
  }

  # get information
  mapCls <- sort(unique(ref2[,'MAP']))
  staCls <- sta2[,1]
  staCnt <- sta2[,2]
  n <- sum(staCnt)
  nClass <- length(mapCls)
  nStra <- length(staCls)
  nRef <- nrow(ref2)
  nh <- rep(0,nStra)
  for(i in 1:nStra){
    nh[i] <- sum(ref2[,'STA']==staCls[i])
  }

  # initialize coefficients
  y_All <- rep(0,nRef)
  y_User <- matrix(0,nRef,nClass)
  x_User <- matrix(0,nRef,nClass)
  y_Prod <- matrix(0,nRef,nClass)
  x_Prod <- matrix(0,nRef,nClass)
  y_Area <- matrix(0,nRef,nClass)
  y_Err <- array(0,c(nClass,nClass,nRef))

  # initialize coefficient means
  yh_All <- rep(0,nStra)
  yh_User <- matrix(0,nClass,nStra)
  xh_User <- matrix(0,nClass,nStra)
  yh_Prod <- matrix(0,nClass,nStra)
  xh_Prod <- matrix(0,nClass,nStra)
  yh_Area <- matrix(0,nClass,nStra)
  yh_Err <- array(0,c(nClass,nClass,nStra))
  
  # initialize coefficient variances and covariances
  yv_All <- rep(0,nStra)
  yv_User <- matrix(0,nClass,nStra)
  xv_User <- matrix(0,nClass,nStra)
  co_User <- matrix(0,nClass,nStra)
  yv_Prod <- matrix(0,nClass,nStra)
  xv_Prod <- matrix(0,nClass,nStra)
  co_Prod <- matrix(0,nClass,nStra)
  yv_Area <- matrix(0,nClass,nStra)
  
  # initialize accuracies
  X_User <- rep(0,nClass)
  X_Prod <- rep(0,nClass)
  conf <- matrix(0,nClass,nClass)
  a_User <- rep(0,nClass)
  a_Prod <- rep(0,nClass)
  a_All <- 0
  area <- rep(0,nClass)
  
  # initialize standard error
  v_Area <- rep(0,nClass)
  v_User <- rep(0,nClass)
  v_Prod <- rep(0,nClass)
  v_All <- 0
  se_Area <- rep(0,nClass)
  se_User <- rep(0,nClass)
  se_Prod <- rep(0,nClass)
  se_All <- 0
  
  # calculation coefficients
  for(i in 1:nRef){
    y_All[i] <- (ref2[i,'MAP']==ref2[i,'REF'])
    y_User[i,] <- (ref2[i,'MAP']==ref2[i,'REF'])&(mapCls==ref2[i,'MAP'])
    x_User[i,] <- (mapCls==ref2[i,'MAP'])
    y_Prod[i,] <- (ref2[i,'MAP']==ref2[i,'REF'])&(mapCls==ref2[i,'REF'])
    x_Prod[i,] <- (mapCls==ref2[i,'REF'])
    y_Area[i,] <- (mapCls==ref2[i,'REF'])
    y_Err[which(mapCls==ref2[i,'MAP']),which(mapCls==ref2[i,'REF']),i] <- 1
  }

  # calculate coefficients means
  for(i in 1:nStra){
    yh_All[i] <- mean(y_All[ref2[,'STA']==staCls[i]])
    yh_User[,i] <- colMeans(y_User[ref2[,'STA']==staCls[i],])
    xh_User[,i] <- colMeans(x_User[ref2[,'STA']==staCls[i],])
    yh_Prod[,i] <- colMeans(y_Prod[ref2[,'STA']==staCls[i],])
    xh_Prod[,i] <- colMeans(x_Prod[ref2[,'STA']==staCls[i],])
    yh_Area[,i] <- colMeans(y_Area[ref2[,'STA']==staCls[i],])
    yh_Err[,,i] <- apply(y_Err[,,ref2[,'STA']==staCls[i]],c(1,2),mean)
  }
  
  # calculate coefficients variance
  for(i in 1:nStra){
    yv_All[i] <- var(y_All[ref2[,'STA']==staCls[i]])
    for(j in 1:nClass){
      yv_User[j,i] <- var(y_User[ref2[,'STA']==staCls[i],j])
      xv_User[j,i] <- var(x_User[ref2[,'STA']==staCls[i],j])
      yv_Prod[j,i] <- var(y_Prod[ref2[,'STA']==staCls[i],j])
      xv_Prod[j,i] <- var(x_Prod[ref2[,'STA']==staCls[i],j])
      yv_Area[j,i] <- var(y_Area[ref2[,'STA']==staCls[i],j])
    }
  }

  # calculate coefficients covariance
  for(i in 1:nStra){
    for(j in 1:nClass){
      co_User[j,i] <- var(y_User[ref2[,'STA']==staCls[i],j],x_User[ref2[,'STA']==staCls[i],j])
      co_Prod[j,i] <- var(y_Prod[ref2[,'STA']==staCls[i],j],x_Prod[ref2[,'STA']==staCls[i],j])
    }
  }
  
  # calculate accuracies
  a_All <- (yh_All%*%staCnt)/n
  for(i in 1:nClass){
    X_User[i] <- (xh_User[i,]%*%staCnt)
    X_Prod[i] <- (xh_Prod[i,]%*%staCnt)
    a_User[i] <- (yh_User[i,]%*%staCnt)/(xh_User[i,]%*%staCnt)
    a_Prod[i] <- (yh_Prod[i,]%*%staCnt)/(xh_Prod[i,]%*%staCnt)
    area[i] <- (yh_Area[i,]%*%staCnt)/n
    for(j in 1:nClass){
      conf[i,j] <- (yh_Err[i,j,]%*%staCnt)/n
    }
  }
  
  # calculate standard errors
  v_All <- (1/n^2)*(((staCnt^2)*(1-nh/staCnt))%*%(yv_All/nh))
  se_All <- sqrt(v_All)
  for(i in 1:nClass){
    v_Area[i] <- (1/n^2)*(((staCnt^2)*(1-nh/staCnt))%*%(yv_Area[i,]/nh))
    se_Area[i] <- sqrt(v_Area[i])
    v_User[i] <- (1/X_User[i]^2)*(((staCnt^2)*(1-nh/staCnt))%*%((yv_User[i,]+a_User[i]^2*xv_User[i,]-2*a_User[i]*co_User[i,])/nh))
    se_User[i] <- sqrt(v_User[i])
    v_Prod[i] <- (1/X_Prod[i]^2)*(((staCnt^2)*(1-nh/staCnt))%*%((yv_Prod[i,]+a_Prod[i]^2*xv_Prod[i,]-2*a_Prod[i]*co_Prod[i,])/nh))
    se_Prod[i] <- sqrt(v_Prod[i])
  }

  # export results
  if(verbose){
    print('Overall Accuracy: ')
    print(a_All)
    print("User's Accuracy: ")
    print(a_User)
    print("Producer's Accuracy: ")
    print(a_Prod)
    print('Area Proportion: ')
    print(area)
    print('Confusion Matrix: ')
    print(conf)
  }

  # done
  return(rbind(a_Prod,a_Prod+se_Prod*1.96,a_Prod-se_Prod*1.96))

}

strata <- paste(mPath,'NRT/Analysis/accuracy2/strata.csv',sep='')
sFile <- paste(mPath,'NRT/Analysis/accuracy2/all.csv',sep='')
eFile <- paste(mPath,'NRT/Analysis/accuracy2/events.csv',sep='')
oPath <- paste(mPath,'NRT/Analysis/accuracy2/date/',sep='')
oPath <- paste(mPath,'NRT/Analysis/accuracy2/nob/',sep='')
dPath <- paste(mPath,'NRT/Analysis/',sep='')
dPath2 <- paste(mPath,'NRT/Analysis/observation/',sep='')
events <- read.table(eFile,sep=',',stringsAsFactors=F,header=T)
samples <- read.table(sFile,sep=',',stringsAsFactors=F,header=T)

cal_accuracy <- function(d,dataPath,pct,model,size,samples,sta,m,alpha=-1){
  
  d$CDATE <- 0
  for(i in 1:nrow(d)){
    if(d[i,'D_EVENT']>0){
      d[i,'CDATE'] <- d[i,'D_EVENT']
    }else{
      d[i,'CDATE'] <- d[i,'D_FIRST_NF']
    }
  }
  
  d2 <- d[d['CDATE']<2013000,]
  for(i in 1:nrow(d2)){
    samples[samples[,'ID']==d2[i,'ID'],'REF'] <- 0
  }
  d <- d[d['CDATE']>=2013000,]
  
  if(m=='lag'){
    lags <- cal_detect_rate(d,dataPath,pct,model,size)
  }else{
    lags <- cal_detect_rate2(d,dataPath,pct,model,size)
  }
  r <- cal_den(lags[,2])
  samples$MAP <- 0
  r <- cbind(r,rep(0,nrow(r)),rep(0,nrow(r)),rep(0,nrow(r)))
  
  #samples[(samples[,'STA']==alpha)&(samples[,'REF']==1),'MAP'] <- 1
  
  for(i in 1:nrow(r)){
    lags2 <- lags[lags[,2]<=r[i,1],,drop=F]
    #lags2 <- lags[lags[,2]<=100,,drop=F]
    for(j in 1:nrow(lags2)){
      samples[samples[,'ID']==lags2[j,1],'MAP'] <- 1
    }
    accu <- AA(sta,samples,F)
    r[i,3] <- accu[1,2]
    r[i,4] <- accu[2,2]
    r[i,5] <- accu[3,2]
  }
  
  return(cbind(r[,1],r[,3],r[,4],r[,5]))
}

cal_detect_rate <- function(d,dataPath,pct,model,size,fd=F){
  
  # initialize output
  if(fd){
    d <- d[find_dup(d[,'AREA2'])==0,]
    d$CDATE <- 0
    for(i in 1:nrow(d)){
      if(d[i,'D_EVENT']>0){
        d[i,'CDATE'] <- d[i,'D_EVENT']
      }else{
        d[i,'CDATE'] <- d[i,'D_FIRST_NF']
      }
    }
    d <- d[d['CDATE']>=2013000,]
  }
  if(length(size)==2){
    d <- d[d[,'AREA2']>=size[1],]
    d <- d[d[,'AREA2']<=size[2],]
  }
  
  cat(paste('Total number of events: ',nrow(d),'\n',sep=''))
  r <- rep(9999,nrow(d))
  #r <- cbind(r,r)
  r <- cbind(r,r,r)
  
  # loop through events
  for(i in 1:nrow(d)){
    # grab info
    pid <- d[i,'PID']
    r[i,1] <- pid
    if(d[i,'D_EVENT']>0){
      baseDate <- d[i,'D_EVENT']
    }else{
      baseDate <- d[i,'D_FIRST_NF']
    }
    r[i,3] <- get_doy(d[i,'CDATE'])
    # read file
    eventFile <- paste(dataPath,model,'/event_',pid,'.csv',sep='')
    if(!file.exists(eventFile)){
      # cat(paste(pid,' file not exist.\n',sep=''))
      next
    }
    e <- read.table(eventFile,sep=',',stringsAsFactors=F,header=T)
    # calculate date
    lag <- sub_doy(e[,'DATE'],baseDate)
    # find lag time for percentile
    for(k in 1:nrow(e)){
      if(e[k,'PROP']>=pct){
        r[i,2] <- lag[k]
        break
      }
    }
  }
  
  # done
  return(r)
}

cal_detect_rate2 <- function(d,dataPath,pct,model,size,fd=F){
  
  # initialize output
  if(fd){
    d <- d[find_dup(d[,'AREA2'])==0,]
    d$CDATE <- 0
    for(i in 1:nrow(d)){
      if(d[i,'D_EVENT']>0){
        d[i,'CDATE'] <- d[i,'D_EVENT']
      }else{
        d[i,'CDATE'] <- d[i,'D_FIRST_NF']
      }
    }
    d <- d[d['CDATE']>=2013000,]
  }
  if(length(size)==2){
    d <- d[d[,'AREA2']>=size[1],]
    d <- d[d[,'AREA2']<=size[2],]
  }
  
  # initialize output
  cat(paste('Total number of events: ',nrow(d),'\n',sep=''))
  r <- rep(9999,nrow(d))
  r <- cbind(r,r)
  
  # loop through events
  for(i in 1:nrow(d)){
    # grab info
    pid <- d[i,'PID']
    r[i,1] <- pid
    # read file
    eventFile <- paste(dataPath,model,'/event_',pid,'.csv',sep='')
    if(!file.exists(eventFile)){
      # cat(paste(pid,' file not exist.\n',sep=''))
      next
    }
    e <- read.table(eventFile,sep=',',stringsAsFactors=F,header=T)
    # calculate date
    lag <- e[,'DATE']
    # find lag time for percentile
    for(k in 1:nrow(e)){
      if(e[k,'PROP']>=pct){
        r[i,2] <- lag[k]
        break
      }
    }
  }
  
  # done
  return(r)
}

plot_event <- function(outPath){
  png(file=paste(outPath,'test.png',sep=''),width=1000,height=1000,pointsize=20)
  a=cal_detect_rate(events,dPath,0.1,'fu',0,T)
  plot(a[,3],a[,2],main='Detection Rate',ylab='Detection Rate',xlab='Lag Time',xlim=c(0,400),ylim=c(0,200),bty='n',pch=16,col='blue')
  box(col='black',lwd=1)
  
  abline(h=50,col='grey',lwd=1)
  abline(h=100,col='grey',lwd=1)
  abline(h=150,col='grey',lwd=1)
  abline(h=200,col='grey',lwd=1)
  abline(h=0,col='grey',lwd=1)
  #abline(h=300,col='grey',lwd=1)
  #abline(h=350,col='grey',lwd=1)

  abline(v=0,col='grey',lwd=1)
  abline(v=100,col='grey',lwd=1)
  abline(v=200,col='grey',lwd=1)
  abline(v=300,col='grey',lwd=1)
  abline(v=400,col='grey',lwd=1)
  
  
  abline(v=152,col='red',lwd=1)
  abline(v=304,col='red',lwd=1)
  
  dev.off()
  
}

plot_detect_rate <- function(outPath){
  
  # all events three sites 
  png(file=paste(outPath,'test.png',sep=''),width=1000,height=1000,pointsize=20)
  plot(0,-1,main='Detection Rate',ylab='Detection Rate',xlab='Lag Time',xlim=c(-50,200),ylim=c(0,1),lwd=8,bty='n')
  a <- cal_accuracy(events,dPath,0.1,'fu',0,samples,strata,'lag')
  #a2 <- cal_accuracy(events,dPath,0.05,'fu',0,samples,strata,'lag')
  #a3 <- cal_accuracy(events,dPath,0.1,'fu',0,samples,strata,'lag')
  #a4 <- cal_accuracy(events,dPath,0.2,'fu',0,samples,strata,'lag')
  #a5 <- cal_accuracy(events,dPath,0.5,'fu',0,samples,strata,'lag')
  b <- cal_accuracy(events,dPath,0.1,'mc',0,samples,strata,'lag')
  c <- cal_accuracy(events,dPath,0.1,'ti',0,samples,strata,'lag')
  #a2 <- cal_accuracy(events,dPath2,0.1,'fu',0,samples,strata,'nob')
  #b <- cal_accuracy(events,dPath2,0.1,'mc',0,samples,strata,'nob')
  #c <- cal_accuracy(events,dPath2,0.1,'ti',0,samples,strata,'nob')
  
  mps <- 231.656
  #a3 <- cal_den(cal_detect_rate(events,dPath,0.05,'fu',c(0,3*mps*mps),T)[,2])
  #a2 <- cal_den(cal_detect_rate(events,dPath,0.05,'fu',c(3*mps*mps,15*mps*mps),T)[,2])
  #a <- cal_den(cal_detect_rate(events,dPath,0.05,'fu',c(15*mps*mps,1000*mps*mps),T)[,2])

  #a <- cal_den(cal_detect_rate(events[events[,'SCENE']=='P227R065',],dPath,0.05,'fu',0,T)[,2])
  #a2 <- cal_den(cal_detect_rate(events[events[,'SCENE']=='P232R066',],dPath,0.05,'fu',0,T)[,2])
  #a3 <- cal_den(cal_detect_rate(events[events[,'SCENE']=='P007R059',],dPath,0.05,'fu',0,T)[,2])

  #a <- cal_den(cal_detect_rate(events,dPath,0.05,'fu',0,T)[,2])
  #b <- cal_den(cal_detect_rate(events,dPath,0.05,'mc',0,T)[,2])
  #c <- cal_den(cal_detect_rate(events,dPath,0.05,'ti',0,T)[,2])
  

  box(col='black',lwd=1)
  

  
  polygon(c(a[,1],rev(a[,1])),c(a[,3],rev(a[,4])),col='grey96',border=NA) 
  polygon(c(b[,1],rev(b[,1])),c(b[,3],rev(b[,4])),col='grey93',border=NA) 
  polygon(c(c[,1],rev(c[,1])),c(c[,3],rev(c[,4])),col='grey90',border=NA) 
  
  lines(a[,1],a[,2],col='red',lwd=2)
  lines(a[,1],a[,3],col='red',lwd=1,lty='dashed')
  lines(a[,1],a[,4],col='red',lwd=1,lty='dashed')
  #lines(a2[,1],a2[,2],col='red',lwd=2)
  #lines(a3[,1],a3[,2],col='green',lwd=2)
  #lines(a4[,1],a4[,2],col='black',lwd=2)
  #lines(a5[,1],a5[,2],col='green',lwd=2)
  lines(b[,1],b[,2],col='blue',lwd=2)
  lines(b[,1],b[,3],col='blue',lwd=1,lty='dashed')
  lines(b[,1],b[,4],col='blue',lwd=1,lty='dashed')
  lines(c[,1],c[,2],col='green',lwd=2)
  lines(c[,1],c[,3],col='green',lwd=1,lty='dashed')
  lines(c[,1],c[,4],col='green',lwd=1,lty='dashed')
  
  abline(h=0.2,col='grey',lwd=1)
  abline(h=0.4,col='grey',lwd=1)
  abline(h=0.6,col='grey',lwd=1)
  abline(h=0.8,col='grey',lwd=1)
  abline(v=0,col='grey',lwd=1)
  abline(v=50,col='grey',lwd=1)
  abline(v=100,col='grey',lwd=1)
  abline(v=150,col='grey',lwd=1)
  
  dev.off()
  
  # done
  return(0)
}

# substract doy
sub_doy <- function(x,y){
  return((floor(x/1000)-floor(y/1000))*365+((x-floor(x/1000)*1000)-(y-floor(y/1000)*1000)))
}

# doy to decimal year
doy2dy <- function(x){
  return(floor(x/1000)+(x-floor(x/1000)*1000)/365)
}

# calculate density
cal_den <- function(x){
  n <- length(x)
  x2 <- sort(unique(x))
  y <- rep(0,length(x2))
  for(i in 1:length(x2)){
    y[i] <- sum(x<=x2[i])/n
  }
  return(cbind(x2,y))
}

cal_strata_accu <- function(d,dataPath,pct,model,size,samples,lt=100){
  
  d2 <- d[(pmax(d[,'D_FIRST_NF'],d[,'D_EVENT'])<2013000),]
  for(i in 1:nrow(d2)){
    samples[samples[,'ID']==d2[i,'ID'],'REF'] <- 0
  }
  d <- d[(d[,'D_FIRST_NF']>=2013000)|(d[,'D_EVENT']>=2013000),]
  lags <- cal_detect_rate(d,dataPath,pct,model,size)
  samples$MAP <- 0
  
  lags2 <- lags[lags[,2]<=lt,,drop=F]
  for(j in 1:nrow(lags2)){
    samples[samples[,'ID']==lags2[j,1],'MAP'] <- 1
  }
  
  for(k in sort(unique(samples[,'STA']))){
    print(paste('Conf for sta = ',k,sep=''))
    accu <- conf_mat(samples[samples['STA']==k,])
  }

  return(0)
}

md = 'ma2'
pFile <- paste(mPath,'VNRT/analysis2/date/',md,'_pieces.csv',sep='')
oPath2 <- paste(mPath,'VNRT/analysis2/date/',md,'/',sep='')
oFile2 <- paste(mPath,'VNRT/analysis2/date/',md,'_result.csv',sep='')
sum_dates <-function(events,pieceFile,outPath,outFile){
  
  # read input file
  pieces <- read.table(pieceFile,sep=',',stringsAsFactors=F,header=T)
  
  # initilize overall output file
  rall <- matrix(0,nrow(events),13)
  colnames(rall) <- c('PID','EAREA','DAREA','PROP','DLASTF','DFSTNF','DEXPD','DEVENT','DCLEAR','D25','D50','D75','LAG')
  
  # loop through all events
  for(i in 1:nrow(events)){
    
    # get information
    rall[i,'PID'] <- events[i,'PID']
    rall[i,'EAREA'] <- events[i,'AREA2']
    rall[i,'DLASTF'] <- events[i,'D_LAST_F']
    rall[i,'DFSTNF'] <- events[i,'D_FIRST_NF']
    rall[i,'DEXPD'] <- events[i,'D_EXPAND']
    rall[i,'DEVENT'] <- events[i,'D_EVENT']
    rall[i,'DCLEAR'] <- events[i,'D_CLEAR']
    event_pieces <- pieces[pieces[,'PID']==rall[i,'PID'],]
    if(nrow(event_pieces)==0){
      next
    }else{
      event_dates <- sort(unique(event_pieces[,'DDATE']))
    }
    
    # initialize results
    r <- matrix(0,length(event_dates),6)
    colnames(r) <- c('PID','EAREA','DATE','DAREA','CAREA','PROP')
    r[,'PID'] <- rall[i,'PID']
    r[,'EAREA'] <- rall[i,'EAREA']
    areasum <- 0
    
    # calculate areas and proportions
    for(j in 1:length(event_dates)){
      date_pieces <- event_pieces[event_pieces[,'DDATE']==event_dates[j],]
      r[j,'DATE'] <- event_dates[j]
      r[j,'DAREA'] <- sum(date_pieces[,'AREA3'])
      areasum <- areasum+r[j,'DAREA']
      r[j,'CAREA'] <- areasum
      r[j,'PROP'] <- r[j,'CAREA']/rall[i,'EAREA']
      
      # update overall results
      if((rall[i,'D25']==0)&(r[j,'PROP']>=0.25)){
        rall[i,'D25'] <- r[j,'DATE']
      }
      if((rall[i,'D50']==0)&(r[j,'PROP']>=0.5)){
        rall[i,'D50'] <- r[j,'DATE']
      }
      if((rall[i,'D75']==0)&(r[j,'PROP']>=0.75)){
        rall[i,'D75'] <- r[j,'DATE']
      }
      
    }
    
    # update overall results
    rall[i,'DAREA'] <- r[nrow(r),'CAREA']
    rall[i,'PROP'] <- r[nrow(r),'PROP']
    if(rall[i,'DEVENT']==0){
      rall[i,'LAG'] <- sub_doy(rall[i,'D25'],rall[i,'DFSTNF'])
    }else{
      rall[i,'LAG'] <- sub_doy(rall[i,'D25'],rall[i,'DEVENT'])
    }
    
    # export result
    write.table(r,paste(outPath,'event_',rall[i,'PID'],'.csv',sep=''),sep=',',
                row.names=F,col.names=T)
  }
  
  # export overall results
  write.table(rall,outFile,sep=',',row.names=F,col.names=T)
  
  # done
  return(0)
  
}


conf_mat <- function(samples){
  
  # get data
  res_vec <- samples[,'MAP']
  ref_vec <- samples[,'REF']
  
  # initialize result
  res_class <- sort(unique(res_vec))
  ref_class <- sort(unique(ref_vec))
  r <- matrix(0,length(res_class),length(ref_class))
  
  # caculate matrix
  for(i in 1:length(ref_vec)){
    r[res_class==res_vec[i],ref_class==ref_vec[i]] <- r[res_class==res_vec[i],ref_class==ref_vec[i]] + 1
  }
  rownames(r)=res_class
  colnames(r)=ref_class
  
  # export result
  print(r)
  
  # done
  return(r)
  
}

# get day of year
get_doy <- function(x){
  return((x/1000-floor(x/1000))*1000)
}

# find duplicate
find_dup <- function(x){
  y <- rep(0,length(x))
  for(i in 1:length(x)){
    if(sum(x==x[i])>1){
      if(y[i]==0){
        y[x==x[i]] <- 1
        y[i] <- 0
      }
    }
  }
  return(y)
}