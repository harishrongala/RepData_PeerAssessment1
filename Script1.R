dataset_url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip";
if(!file.exists(paste(getwd(),"/activity.zip",sep="")))
        {
        if(!file.exists(paste(getwd(),"/activity.csv",sep="")))
                {
                download.file(dataset_url,paste(getwd(),"/activity.zip",sep=""));
                unzip("activity.zip");
        }
}
dataset<-read.csv("activity.csv",na.strings = "NA",header = TRUE);
dataset<-data.frame(dataset);
print(tail(dataset));
print(mean(dataset$steps,na.rm = TRUE));
print(median(dataset$steps,na.rm = TRUE));
print(summary(dataset$steps));
hist(dataset$steps);
abline(v=mean(dataset$steps,na.rm = TRUE));
print(head(dataset[dataset$steps==0]));
weekend_data<-dataset[weekdays(as.POSIXct(dataset$date))=="Sunday" | weekdays(as.POSIXct(dataset$date))=="Saturday"]