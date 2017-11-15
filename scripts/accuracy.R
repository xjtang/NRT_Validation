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
AA <- function(sta,ref){

  # read input data
  sta2 <- read.table(sta,sep=',',stringsAsFactors=F)
  ref2 <- read.table(ref,sep=',',stringsAsFactors=F,header=T)

  # get information
  mapCls <- sort(unique(ref2[,'MAP']))
  staCls <- sta2[,1]
  staCnt <- sta2[,2]
  n <- sum(staCnt)
  nClass <- length(mapCls)
  nStra <- length(staCls)
  nRef <- nrow(ref2)

  # initialize coefficients
  y_All <- rep(0,nRef)
  y_User <- matrix(0,nRef,nClass)
  x_User <- matrix(0,nRef,nClass)
  y_Prod <- matrix(0,nRef,nClass)
  x_Prod <- matrix(0,nRef,nClass)
  y_Area <- matrix(0,nRef,nClass)
  y_Err <- array(0,c(nClass,nClass,nRef))

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

  # initialize coefficient means
  yh_All <- rep(0,nStra)
  yh_User <- matrix(0,nClass,nStra)
  xh_User <- matrix(0,nClass,nStra)
  yh_Prod <- matrix(0,nClass,nStra)
  xh_Prod <- matrix(0,nClass,nStra)
  yh_Area <- matrix(0,nClass,nStra)
  yh_Err <- array(0,c(nClass,nClass,nStra))

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

  # initialize accuracies
  conf <- matrix(0,nClass,nClass)
  a_User <- rep(0,nClass)
  a_Prod <- rep(0,nClass)
  a_All <- 0
  area <- rep(0,nClass)

  # calculate accuracies
  a_All <- (yh_All%*%staCnt)/n
  for(i in 1:nClass){
    a_User[i] <- (yh_User[i,]%*%staCnt)/(xh_User[i,]%*%staCnt)
    a_Prod[i] <- (yh_Prod[i,]%*%staCnt)/(xh_Prod[i,]%*%staCnt)
    area[i] <- (yh_Area[i,]%*%staCnt)/n
    for(j in 1:nClass){
      conf[i,j] <- (yh_Err[i,j,]%*%staCnt)/n
    }
  }

  # export results
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

  # done
  return(0)

}


mPath <- 'D:/Users/HotDog/Documents/Dropbox/'
strata <- paste(mPath,'NRT/Analysis/accuracy2/strata.csv',sep='')
sFile <- paste(mPath,'VNRT/analysis/date/event_join4.csv',sep='')
eFile <- paste(mPath,'VNRT/analysis/observation/event_join4.csv',sep='')
oPath <- paste(mPath,'NRT/Analysis/accuracy2/date',sep='')
oFile <- paste(mPath,'NRT/Analysis/accuracy2/fu_date_result.csv',sep='')
dPath <- paste(mPath,'NRT/Analysis/',sep='')
events <- read.table(eFile,sep=',',stringsAsFactors=F,header=T)
samples <- read.table(sFile,sep=',',stringsAsFactors=F,header=T)


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


