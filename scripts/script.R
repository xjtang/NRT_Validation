# script.R
# Version 1.4
# Processing Tools
#
# Project: NRT_Compare
# By xjtang
# Created On: 5/27/2016
# Last Update: 10/16/2016
#
# Version 1.0 - 5/27/2016
#   Script created for analysing the reference dataset
#
# Updates of Version 1.1 - 7/1/2016
#   1.Added new function to analyse change dates.
#
# Updates of Version 1.2 - 8/14/2016
#   1.Bug fix.
#   2.Added new function to make plots of group events.
#   3.Optimized code.
#
# Updates of Version 1.3 - 9/11/2016
#   1.Added new function to make plots of percentiles.
#   2.Added new function to find start and end of detection time.
#   3.Improved the look of group plots.
#   4.Added percent filtering for event plots.
#   5.Plot lines instead of points
#   6.Adde function to plot density
#   7.Bug fix.
#
# Updates of Version 1.4 - 10/16/2016
#   1.Added new function to plot detecion rate against event size.
#   2.Changed terminology.
#   3.Bug fix.
#
# -------------------------------------------------------

# library
library(png)

# conf_mat
# create confusion matrix
mPath <- '/Users/xjtang/Applications/Dropbox/'
eFile <- paste(mPath,'NRT/Analysis/event_join2.csv',sep='')
oFile <- paste(mPath,'NRT/Analysis/ti_conf.csv',sep='')
conf_mat <- function(file,res,ref,output){

  # read table
  main <- read.table(file,sep=',',stringsAsFactors=F,header=T)

  # get data
  res_vec <- main[,res]
  ref_vec <- main[,ref]

  # initialize result
  res_class <- sort(unique(res_vec))
  ref_class <- sort(unique(ref_vec))
  r <- matrix(0,length(ref_class),length(res_class))

  # caculate matrix
  for(i in 1:length(res_vec)){
    r[ref_class==ref_vec[i],res_class==res_vec[i]] <- r[ref_class==ref_vec[i],res_class==res_vec[i]] + 1
  }
  rownames(r)=ref_class
  colnames(r)=res_class

  # export result
  write.table(r,output,sep=',',col.names=NA)

  # done

}

# sum_dates
# summarize date information
eFile2 <- paste(mPath,'NRT/Analysis/event_join3.csv',sep='')
pFile <- paste(mPath,'NRT/Analysis/mc.csv',sep='')
oPath <- paste(mPath,'NRT/Analysis/mc/',sep='')
oFile2 <- paste(mPath,'NRT/Analysis/mc_result.csv',sep='')
sum_dates <-function(eventFile,pieceFile,outPath,outFile){

  # read input file
  events <- read.table(eventFile,sep=',',stringsAsFactors=F,header=T)
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
    if(max((event_pieces[,'DATE']>=2013000)&(event_pieces[,'DATE']<2016000))){
      event_dates <- sort(unique(event_pieces[(event_pieces[,'DATE']>=2013000)&(event_pieces[,'DATE']<2016000),'DATE']))
    }else{
      next
    }

    # initialize results
    r <- matrix(0,length(event_dates),6)
    colnames(r) <- c('PID','EAREA','DATE','DAREA','CAREA','PROP')
    r[,'PID'] <- rall[i,'PID']
    r[,'EAREA'] <- rall[i,'EAREA']
    areasum <- 0

    # calculate areas and proportions
    for(j in 1:length(event_dates)){
      date_pieces <- event_pieces[event_pieces[,'DATE']==event_dates[j],]
      r[j,'DATE'] <- event_dates[j]
      r[j,'DAREA'] <- sum(date_pieces[,'PAREA'])
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

# plot_alert_pct
# generate plots if alert percentage as a function of time
rPath <- paste(mPath,'NRT/Analysis/',sep='')
oPath2 <- paste(mPath,'NRT/Analysis/alert_pct/',sep='')
plot_alert_pct <- function(eventFile,resultPath,outPath,s){

  # read event file
  events <- read.table(eventFile,sep=',',stringsAsFactors=F,header=T)
  lwidth <- 6
  
  # loop through all events
  for(i in 1:nrow(events)){

    # read input file
    totalfile <- 0
    if(file.exists(paste(resultPath,'fu/event_',events[i,'PID'],'.csv',sep=''))){
      r1 <- read.table(paste(resultPath,'fu/event_',events[i,'PID'],'.csv',sep=''),sep=',',stringsAsFactors=F,header=T)
      if(s>0){
        r1 <- r1[pct_inc(r1[,'PROP'],s)==1,]
      }
      totalfile <- totalfile+1
    }else{
      r1 <- matrix(0,2,2)
      colnames(r1) <- c('DATE','PROP')
    }
    if(file.exists(paste(resultPath,'mc/event_',events[i,'PID'],'.csv',sep=''))){
      r2 <- read.table(paste(resultPath,'mc/event_',events[i,'PID'],'.csv',sep=''),sep=',',stringsAsFactors=F,header=T)
      if(s>0){
        r2 <- r2[pct_inc(r2[,'PROP'],s)==1,]
      }
      totalfile <- totalfile+1
    }else{
      r2 <- matrix(0,2,2)
      colnames(r2) <- c('DATE','PROP')
    }
    if(file.exists(paste(resultPath,'ti/event_',events[i,'PID'],'.csv',sep=''))){
      r3 <- read.table(paste(resultPath,'ti/event_',events[i,'PID'],'.csv',sep=''),sep=',',stringsAsFactors=F,header=T)
      if(s>0){
        r3 <- r3[pct_inc(r3[,'PROP'],s)==1,]
      }
      totalfile <- totalfile+1
    }else{
      r3 <- matrix(0,2,2)
      colnames(r3) <- c('DATE','PROP')
    }
    if(totalfile==0){next}

    # initialize plot file
    png(file=paste(outPath,'event_',events[i,'PID'],'.png',sep=''),width=2000,height=2400,pointsize=20)
    cPar <- par(mfrow=c(3,1))

    # make plot
    x <- doy2dy(r1[,'DATE'])
    plot(x,r1[,'PROP'],type='l',col='black',pch=16,
         main='Fusion',ylab='Alert Percentage',xlab='Date of Detection',
         xlim=c(2013,2016),ylim=c(0,1),xaxt='n',bty='n',lwd=lwidth
    )
    box(col='black',lwd=2)
    axis(1,at=c(2013,2014,2015,2016))
    if(events[i,'D_EVENT']>0){
      abline(v=doy2dy(events[i,'D_EVENT']),col='red',lwd=lwidth)
    }else{
      abline(v=doy2dy(events[i,'D_FIRST_NF']),col='red',lwd=lwidth)
    }
    if(events[i,'D_CLEAR']>0){
      abline(v=doy2dy(events[i,'D_CLEAR']),col='blue',lwd=lwidth)
    }
    if(events[i,'D_EXPAND']>0){
      abline(v=doy2dy(events[i,'D_EXPAND']),col='green',lwd=lwidth)
    }
    x <- doy2dy(r2[,'DATE'])
    plot(x,r2[,'PROP'],type='l',col='black',pch=16,
         main='MCCDC',ylab='Alert Percentage',xlab='Date of Detection',
         xlim=c(2013,2016),ylim=c(0,1),xaxt='n',bty='n',lwd=lwidth
    )
    box(col='black',lwd=2)
    axis(1,at=c(2013,2014,2015,2016))
    if(events[i,'D_EVENT']>0){
      abline(v=doy2dy(events[i,'D_EVENT']),col='red',lwd=lwidth)
    }else{
      abline(v=doy2dy(events[i,'D_FIRST_NF']),col='red',lwd=lwidth)
    }
    if(events[i,'D_CLEAR']>0){
      abline(v=doy2dy(events[i,'D_CLEAR']),col='blue',lwd=lwidth)
    }
    if(events[i,'D_EXPAND']>0){
      abline(v=doy2dy(events[i,'D_EXPAND']),col='green',lwd=lwidth)
    }
    x <- doy2dy(r3[,'DATE'])
    plot(x,r3[,'PROP'],type='l',col='black',pch=16,
         main='Terra-i',ylab='Alert Percentage',xlab='Date of Detection',
         xlim=c(2013,2016),ylim=c(0,1),xaxt='n',bty='n',lwd=lwidth
    )
    box(col='black',lwd=2)
    axis(1,at=c(2013,2014,2015,2016))
    if(events[i,'D_EVENT']>0){
      abline(v=doy2dy(events[i,'D_EVENT']),col='red',lwd=lwidth)
    }else{
      abline(v=doy2dy(events[i,'D_FIRST_NF']),col='red',lwd=lwidth)
    }
    if(events[i,'D_CLEAR']>0){
      abline(v=doy2dy(events[i,'D_CLEAR']),col='blue',lwd=lwidth)
    }
    if(events[i,'D_EXPAND']>0){
      abline(v=doy2dy(events[i,'D_EXPAND']),col='green',lwd=lwidth)
    }

    # close plot
    par(cPar)
    dev.off()
  }

  # done
  return(0)
}

# alert_pct_grp
# make plots of alert percentage for group of events
dPath <- paste(mPath,'NRT/Analysis/',sep='')
oPath3 <- paste(mPath,'NRT/Analysis/alert_pct_grp/',sep='')
data <- read.table(eFile2,sep=',',stringsAsFactors=F,header=T)
alert_pct_grp <- function(d,dataPath,outPath,outName,s){

  # initialize plot
  png(file=paste(outPath,outName,'.png',sep=''),width=2000,height=2400,pointsize=20)
  cPar <- par(mfrow=c(3,1))
  lwidth <- 1

  # loop through datasets
  for(i in 1:3){
    # initialize plot
    if(i==1){
      model <- 'Fusion'
      model2 <- 'fu'
    }else if(i==2){
      model <- 'MCCDC'
      model2 <- 'mc'
    }else{
      model <- 'Terra_i'
      model2 <- 'ti'
    }
    plot(0,-1,main=model,ylab='Alert Percentage',xlab='Lag Time',
         xlim=c(-400,400),ylim=c(0,1),bty='n')
    box(col='black',lwd=2)
    # loop through events
    for(j in 1:nrow(d)){
      # grab info
      pid <- d[j,'PID']
      if(d[j,'D_EVENT']>0){
        baseDate <- d[j,'D_EVENT']
      }else{
        baseDate <- d[j,'D_FIRST_NF']
      }
      if(d[j,'SCENE']=='P227R065'){
        pCol <- 'red'
      }else if(d[j,'SCENE']=='P232R066'){
        pCol <- 'blue'
      }else{
        pCol <- 'green'
      }
      # read file
      eventFile <- paste(dataPath,model2,'/event_',pid,'.csv',sep='')
      if(!file.exists(eventFile)){next}
      e <- read.table(eventFile,sep=',',stringsAsFactors=F,header=T)
      # calculate date
      lag <- sub_doy(e[,'DATE'],baseDate)
      # data filtering
      if(s>0){
        f <- (pct_inc(e[,'PROP'],s)==1)
        points(lag[f],e[f,'PROP'],type='l',col=pCol,pch=16,lwd=lwidth)
      }else{
        points(lag,e[,'PROP'],type='l',col=pCol,pch=16,lwd=lwidth)
      }
      # plot

    }
  }

  # complete plot
  par(cPar)
  dev.off()

  # done
  return(0)
}

# alert_pct_hist
# make histogram of specific alert percentage as a function of lag time
oPath4 <- paste(mPath,'NRT/Analysis/alert_pct_hist/',sep='')
alert_pct_hist <- function(d,dataPath,outPath,outName,pct){

  # initialize output
  r <- matrix(0,nrow(d),3)
  png(file=paste(outPath,outName,'.png',sep=''),width=2000,height=1500,pointsize=20)
  cPar <- par(mfrow=c(3,1))

  # loop through datasets
  for(i in 1:3){
    # initialize plot
    if(i==1){
      model <- 'Fusion'
      model2 <- 'fu'
    }else if(i==2){
      model <- 'MCCDC'
      model2 <- 'mc'
    }else{
      model <- 'Terra_i'
      model2 <- 'ti'
    }

    # loop through events
    for(j in 1:nrow(d)){
      # grab info
      pid <- d[j,'PID']
      if(d[j,'D_EVENT']>0){
        baseDate <- d[j,'D_EVENT']
      }else{
        baseDate <- d[j,'D_FIRST_NF']
      }
      # read file
      eventFile <- paste(dataPath,model2,'/event_',pid,'.csv',sep='')
      if(!file.exists(eventFile)){next}
      e <- read.table(eventFile,sep=',',stringsAsFactors=F,header=T)
      # calculate date
      lag <- sub_doy(e[,'DATE'],baseDate)
      # find lag time for percentile
      for(k in 1:nrow(e)){
        if(e[k,'PROP']>=pct){
          r[j,i] <- lag[k]
          break
        }
      }
    }

    # make hist
    r[r[,i]>800,i] <- 800
    r[r[,i]<(-200),i] <- -200
    hist(r[r[,i]!=0,i],seq(-200,800,20),main=model,xlim=c(-200,800),ylim=c(0,60),xlab='Lag Time')

  }

  # complete plot
  par(cPar)
  dev.off()

  # done
  return(0)
}

# cal_detect_rate
# calculate detection rate as a function of lag time
cal_detect_rate <- function(d,dataPath,pct,model,size){

  # initialize output
  d <- d[find_dup(d[,'AREA2'])==0,]
  if(length(size)==2){
    d <- d[d[,'AREA2']>=size[1],]
    d <- d[d[,'AREA2']<=size[2],]
  }
  d <- d[(d[,'D_FIRST_NF']>=2013000)|(d[,'D_EVENT']>=2013000),]
  cat(paste('Total number of events: ',nrow(d),'\n',sep=''))
  r <- rep(9999,nrow(d))

  # loop through events
  for(i in 1:nrow(d)){
    # grab info
    pid <- d[i,'PID']
    if(d[i,'D_EVENT']>0){
      baseDate <- d[i,'D_EVENT']
    }else{
      baseDate <- d[i,'D_FIRST_NF']
    }
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
        r[i] <- lag[k]
        break
      }
    }
    if(r[i]==9999){
      # cat(paste(pid,' never reach goal.\n',sep=''))
    }
  }

  # done
  return(cal_den(r))
}

# plot_detect_rate
# plot detection rate as a function of lag time
oPath5 <- paste(mPath,'NRT/Analysis/detect_rate/',sep='')
plot_detect_rate <- function(outPath,md='fu'){
  
  # all events three models
  png(file=paste(outPath,'all_bymodel.png',sep=''),width=1000,height=1000,pointsize=20)
  plot(0,-1,main='Detection Rate',ylab='Detection Rate',xlab='Lag Time',xlim=c(-50,200),ylim=c(0,1))
  a <- cal_detect_rate(data,dPath,0.1,'fu',0)
  lines(a[,1],a[,2],col='red',lwd=3)
  a <- cal_detect_rate(data,dPath,0.1,'mc',0)
  lines(a[,1],a[,2],col='blue',lwd=3)
  a <- cal_detect_rate(data,dPath,0.1,'ti',0)
  lines(a[,1],a[,2],col='green',lwd=3)
  dev.off()
  
  
  # all events three sites 
  png(file=paste(outPath,'all_bysite_',md,'.png',sep=''),width=1000,height=1000,pointsize=20)
  plot(0,-1,main='Detection Rate',ylab='Detection Rate',xlab='Lag Time',xlim=c(-50,200),ylim=c(0,1))
  a <- cal_detect_rate(data[data[,'SCENE']=='P227R065',],dPath,0.1,md,0)
  lines(a[,1],a[,2],col='red',lwd=2)
  a <- cal_detect_rate(data[data[,'SCENE']=='P232R066',],dPath,0.1,md,0)
  lines(a[,1],a[,2],col='blue',lwd=2)
  a <- cal_detect_rate(data[data[,'SCENE']=='P007R059',],dPath,0.1,md,0)
  lines(a[,1],a[,2],col='green',lwd=2)
  dev.off()

  # all events 5 percentile
  png(file=paste(outPath,'all_bypct.png',sep=''),width=1000,height=1000,pointsize=20)
  plot(0,-1,main='Detection Rate',ylab='Detection Rate',xlab='Lag Time',xlim=c(-50,200),ylim=c(0,1))
  a <- cal_detect_rate(data,dPath,0.05,'fu',0)
  lines(a[,1],a[,2],lwd=2)
  a <- cal_detect_rate(data,dPath,0.1,'fu',0)
  lines(a[,1],a[,2],lwd=2)
  a <- cal_detect_rate(data,dPath,0.2,'fu',0)
  lines(a[,1],a[,2],lwd=2)
  a <- cal_detect_rate(data,dPath,0.3,'fu',0)
  lines(a[,1],a[,2],lwd=2)
  a <- cal_detect_rate(data,dPath,0.5,'fu',0)
  lines(a[,1],a[,2],lwd=2)
  dev.off()

  # all events 3 sizes
  mps <- 231.656
  png(file=paste(outPath,'all_bysize_',md,'.png',sep=''),width=1000,height=1000,pointsize=20)
  plot(0,-1,main='Detection Rate',ylab='Detection Rate',xlab='Lag Time',xlim=c(-50,200),ylim=c(0,1))
  a <- cal_detect_rate(data,dPath,0.1,md,c(0,3*mps*mps))
  lines(a[,1],a[,2],col='green',lwd=2)
  a <- cal_detect_rate(data,dPath,0.1,md,c(3*mps*mps,15*mps*mps))
  lines(a[,1],a[,2],col='blue',lwd=2)
  a <- cal_detect_rate(data,dPath,0.1,md,c(15*mps*mps,1000*mps*mps))
  lines(a[,1],a[,2],col='red',lwd=2)
  dev.off()

  # done
  return(0)
}

# rate_size
# calculate detection rate as a function of event size
rate_size <- function(d,dataPath,lt,pct,model){

  # initialize output
  d <- d[find_dup(d[,'AREA2'])==0,]
  d <- d[(d[,'D_FIRST_NF']>=2013000)|(d[,'D_EVENT']>=2013000),]
  cat(paste('Total number of events: ',nrow(d),'\n',sep=''))
  r <- matrix(0,nrow(d),2)

  # loop through events
  for(i in 1:nrow(d)){
    # grab info
    pid <- d[i,'PID']
    r[i,1] <- d[i,'AREA2']
    if(d[i,'D_EVENT']>0){
      baseDate <- d[i,'D_EVENT']
    }else{
      baseDate <- d[i,'D_FIRST_NF']
    }
    # read file
    eventFile <- paste(dataPath,model,'/event_',pid,'.csv',sep='')
    if(!file.exists(eventFile)){
      # cat(paste(pid,' file not exist.\n',sep=''))
      next
    }
    e <- read.table(eventFile,sep=',',stringsAsFactors=F,header=T)
    # calculate date
    lag <- sub_doy(e[,'DATE'],baseDate)
    # check if this event is detected
    if(max(e[lag<=lt,'PROP'])>=pct){
      r[i,2] <- 1
    }else{
      r[i,2] <- 0
    }
  }

  # calculate detection rate as a function of size
  r2 <- matrix(9999,nrow(d),2)
  sizeList <- sort(r[,1])
  for(i in 1:length(sizeList)){
    r2[i,1] <- sum(sizeList[1:i])/i
    r2[i,2] <- sum(r[r[,1]<=sizeList[i],2])/i
  }

  # done
  return(r2)
}

# plot_rate_size
# plot detection rate as a function of event size
oPath6 <- paste(mPath,'NRT/Analysis/event_size/',sep='')
plot_rate_size <- function(outPath,lt,pct){
  # make plot
  png(file=paste(outPath,'rate_size.png',sep=''),width=1000,height=1000,pointsize=20)
  plot(0,-1,main='Detection Rate / Event Size',
       ylab='Detection Rate',xlab='Event Size (ha)',xlim=c(0,150),ylim=c(0,1))
  a <- rate_size(data,dPath,lt,pct,'fu')
  lines(a[,1]/(100*100),a[,2],col='red',lwd=3)
  a <- rate_size(data,dPath,lt,pct,'mc')
  lines(a[,1]/(100*100),a[,2],col='blue',lwd=3)
  a <- rate_size(data,dPath,lt,pct,'ti')
  lines(a[,1]/(100*100),a[,2],col='green',lwd=3)
  dev.off()
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

# percent increment
pct_inc <- function(x,s){
  c <- 0
  y <- rep(0,length(x))
  for(i in 1:length(x)){
    if(x[i]>=c){
      y[i] <- 1
      c <- ceiling(x[i]/s)*s
    }
  }
  return(y)
}

# substract doy
sub_doy <- function(x,y){
  return((floor(x/1000)-floor(y/1000))*365+((x-floor(x/1000)*1000)-(y-floor(y/1000)*1000)))
}

# doy to decimal year
doy2dy <- function(x){
  return(floor(x/1000)+(x-floor(x/1000)*1000)/365)
}
