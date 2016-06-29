# script.R
# Version 1.1
# Processing Tools
#
# Project: NRT_Compare
# By xjtang
# Created On: 5/27/2016
# Last Update: 6/27/2016
#
# Version 1.0 - 5/27/2016
#   Script created for analysing the reference dataset
#
# Updates of Version 1.1 -6/27/2016
#   1.Added new function to analyse change dates.   
#
# -------------------------------------------------------

# conf_mat
# create confusion matrix
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
eFile <- 'C:/Users/HotDog/Desktop/NRT/Analysis/Date/CSV/event.csv'
pFile <- 'C:/Users/HotDog/Desktop/NRT/Analysis/Date/CSV/fu.csv'
oPath <- 'C:/Users/HotDog/Desktop/NRT/Analysis/Date/CSV/fu/'
oFile <- 'C:/Users/HotDog/Desktop/NRT/Analysis/Date/CSV/fu_result.csv'
sum_dates <-function(eventFile,pieceFile,outPath,outFile){
  
  # read input file
  events <- read.table(eventFile,sep=',',stringsAsFactors=F,header=T)
  pieces <- read.table(pieceFile,sep=',',stringsAsFactors=F,header=T)
  
  # initilize overall output file
  rall <- matrix(0,nrow(events),10)
  
  # loop through all events
  for(i in 1:nrow(events)){
    
    # get information
    pid <- events[i,'PID']
    earea <- events[i,'AREA2']
    event_pieces <- pieces[pieces[,'PID']==pid,]
    if(max((event_pieces[,'DATE']>=2013000)&(event_pieces[,'DATE']<2016000))){
      event_dates <- sort(unique(event_pieces[(event_pieces[,'DATE']>=2013000)&(event_pieces[,'DATE']<2016000),'DATE']))
    }else{
      next  
    }
    
    # initialize results
    r <- matrix(0,length(event_dates),6)
    r[,1] <- pid
    r[,2] <- earea
    areasum <- 0
        
    # calculate areas and proportions
    for(j in 1:length(event_dates)){
      date_pieces <- event_pieces[event_pieces[,'DATE']==event_dates[j],]
      r[j,3] <- event_dates[j]
      r[j,4] <- sum(date_pieces[,'PAREA'])
      areasum <- areasum+r[j,4]
      r[j,5] <- areasum
      r[j,6] <- r[j,5]/earea
    }
    
    # export result
    colnames(r) <- c('PID','EAREA','DATE','DAREA','CAREA','PROP')
    write.table(r,paste(outPath,'event_',pid,'.csv',sep=''),sep=',',
                row.names=F,col.names=T)
    
    # make plot
    
    
  }
  
  # export overall results
  colnames(rall) <- c('PID','EAREA','DAREA','PROP','DLASTF','DFSTNF','DEVENT','D25','D50','D75','LAG')
  write.table(rall,outFile,sep=',',
              row.names=F,col.names=T)
  
  # done
  
}

