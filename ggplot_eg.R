library('ggplot2')
filepath<-"http://bit.ly/wBBTQO"
#read in the tab delimited text file using the url() function
#myData<-read.table(file=url(filepath),header=T,sep="\t")
#write.table(myData,file='ggplot_egdata.csv',sep=',')
myData<-read.table(file='ggplot_egdata.csv',header=T,sep=",")
summary(myData)
str(myData)
table(myData)
#Hist
qplot(data=myData,x=Tribe,main="Hist")
#Scatter

qplot(data=myData,x=BM,y=var1,log="xy",color=Tribe)
qplot(data=myData,x=log(BM),y=log(var1),color=Tribe)
#box
qplot(data=myData,x=Hab,y=var1,geom="boxplot")
qplot(data=myData,x=Hab,y=var1,geom="jitter")

#Bar - not working
ggplot(myData,aes(Hab,fill=cut)) + geom_bar(position='dodge')

#facets
qplot(data=myData,x=BM,y=var1,log="xy",color=Tribe,facets = Hab~Tribe)
qplot(data=myData,x=BM,y=var1,log="xy",color=Tribe,facets = ~Tribe)
#layers and trendlines
myGG = qplot(data=myData,x=BM,y=var1,log="xy",color=Tribe,facets = ~Tribe)
myGG = myGG + stat_smooth(method='lm')
myGG
ggsave('exampleGGPlot2Graphs.jpg')



head(diamonds)
