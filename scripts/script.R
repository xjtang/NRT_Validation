# script.R
# Version 1.0
# Processing Tools
#
# Project: NRT_Compare
# By xjtang
# Created On: 5/27/2016
# Last Update: 5/27/2016
#
# Version 1.0 - 5/27/2016
#   Script created for analysing the reference dataset
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
