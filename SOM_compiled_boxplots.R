library(ggplot2)

#Bring in compiled SOM data
data <- read.csv("E:/Box/Box Sync/SOM_tarball/tarball_10-18-2018.csv")

#See full list of data column names
#colnames(data)

for (i in 2:ncol(data)) {
  df <- NULL
  plot <- NULL
  if(is.numeric(na.omit(data[,i]))) {
    df <- data[!is.na(data[,i]),]
    plot <- ggplot(df, aes(y=df[,i],x=location_name)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90)) + ylab(colnames(data[i]))
    ggsave(plot,filename=paste0(colnames(data[i]),"_boxplot.pdf"), width = 14, height = 8, path=paste0(getwd(),"/temp/"), device='pdf')
  }
  print(paste0("completed ",colnames(data[i])))
}