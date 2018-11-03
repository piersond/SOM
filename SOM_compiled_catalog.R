# Catalog for compiled SOM data
##############################################

#Bring in compiled SOM data
data <- read.csv("E:/Box/Box Sync/SOM_tarball/tarball_10-18-2018.csv")

#See full list of data column names
colnames(data)

#Catalog numeric columns
num.catalog <- data.frame(name="START",n=0,numeric= TRUE,mean=0,median=0,min=0,max=0)
for (i in 1:ncol(data)) {

  name <- colnames(data[i])
  n <- length(na.omit(data[,i]))
  
  if(is.numeric(na.omit(data[,i]))) {
    numeric <- TRUE
    minm <- round(min(na.omit(data[,i])),3)
    maxm <- round(max(na.omit(data[,i])),1)
    mn <- round(mean(na.omit(data[,i])),2)
    mdn <- round(median(na.omit(data[,i])),1)
    
  } else {
    numeric <- FALSE
    minm <- NA
    maxm <- NA
    mn <- NA
    mdn <- NA
  }
  
  df <- data.frame(name=name,n=n,numeric=numeric,mean=mn,median=mdn,min=minm,max=maxm)

  num.catalog <- rbind(num.catalog,df)
}

#Clean up first two rows
num.catalog <- num.catalog[-c(1,2),]

#Export catalog
write.csv(num.catalog, paste0("SOM Compiled Data Catalog_",format(Sys.Date(), format="%B-%d-%Y"),".csv"))




