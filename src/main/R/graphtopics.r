#make sure the following two libraries are installed first
#install.packages("RColorBrewer")
#install.packages("gplots")
library(RColorBrewer)
library(gplots)

#read in data
data <- read.csv("../../../out_all_75.csv")
K <- length(data) - 1
colnames(data) = c("week", 1:K)

#weeks <- data[,1]
#topicVectors <- as.list(data.frame(t(data[,c(2:101)])))

#data <- data.frame(weeks, topicVectors)

countAboveThreshold <- function(threshold) {
  function(...){
    fields <- c(...)
    sum(fields > threshold)
  }
}

#plot number of articles collected per week
countPerWeek <- aggregate(. ~ week, data, length)[,c(1,2)]
barplot(countPerWeek[,2], names.arg=countPerWeek[,1], ylab="number of articles", las=2, main="Number of articles per week")

meanTopics <- aggregate(. ~ week, data, mean)
countTopics <- aggregate(. ~ week, data, countAboveThreshold(.40))

#TODO: add xlab="year-week"
topic <- 59
barplot(countTopics[,topic+1], names.arg=countTopics[,1], ylab="number of articles", las=2)

#get main topics for each week
mainTopics <- max.col(t(countTopicsM))

#plot a heatmap of topic distributions
countTopicsM <- t(data.matrix(countTopics[,c(2:(K+1))]))
breaks <- seq(0,50,1)
#breaks <- append(breaks, 0, 0)
mycol <- colorpanel(n=length(breaks)-1,low="blue",mid="green", high="red")
#mycol <- append(mycol, "#000088", 0)
heatmap.2(countTopicsM, col=mycol, Colv=NA, Rowv=NA, breaks=breaks, trace='none', labCol=countTopics[,1], density.info='none', symkey=FALSE)


#plot a heatmap of main topics
breaks <- seq(0,0.1,0.01)
mycol <- colorpanel(n=length(breaks)-1,low="blue",mid="green", high="red")
meanTopicsM <- t(data.matrix(meanTopics[,c(2:(K+1))]))
heatmap.2(meanTopicsM, col=mycol, breaks=breaks, Colv=NA, Rowv=NA, trace='none', labCol=meanTopics[,1], symkey=FALSE)