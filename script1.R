# try this https://compgenomr.github.io/book/software-information-and-conventions.html

file_path <- system.file()

chr <- c("chr1", "chr1", "chr2", "chr2")
strand <- c("-","-","+","+")
start<- c(200,4000,100,400)
end<-c(250,410,200,450)
mydata <- data.frame(chr,start,end,strand)
#change column names
names(mydata) <- c("chr","start","end","strand")
mydata # OR this will work too

mydata[2:3, 2:4] # how rows #2-3, with columns #2-4

w <- list(name="Fred",
          mynumbers=c(1,2,3),
          mymatrix=matrix(1:4,ncol=2),
          age=5.3)
w$mymatrix

features=c("promoter","exon","intron")
f.feat=factor(features)
f.feat

2L # integer

enhancerFilePath=system.file("extdata",
                             "subset.enhancers.hg18.bed",
                             package="compGenomRData")
cpgiFilePath=system.file("extdata",
                         "subset.cpgi.hg18.bed",
                         package="compGenomRData")
# read enhancer marker BED file
enh.df <- read.table(enhancerFilePath, header = FALSE) 

# read CpG island BED file
cpgi.df <- read.table(cpgiFilePath, header = FALSE) 

# check first lines to see how the data looks like
head(enh.df)

head(cpgi.df)

write.table(cpgi.df,file="cpgi.txt",quote=FALSE,
            row.names=FALSE,col.names=FALSE,sep="\t")

save(cpgi.df,enh.df,file="mydata.RData")
load("mydata.RData")
# saveRDS() can save one object at a type
saveRDS(cpgi.df,file="cpgi.rds")
x=readRDS("cpgi.rds")
head(x)

# sample 50 values from normal distribution
# and store them in vector x
x <- rnorm(50)
hist(x) # plot the histogram of those values

hist(x,main="Hello histogram!!!",col="red")

y <- rnorm(50)
plot(x,y,main="scatterplot of random samples",
     ylab="y values",xlab="x values")

boxplot(x,y,main="boxplots of random samples")

perc=c(50,70,35,25)
barplot(height=perc,
        names.arg=c("CpGi","exon","CpGi","exon"),
        ylab="percentages",main="imagine %s",
        col=c("red","red","blue","blue"))
legend("topright",legend=c("test","control"),
       fill=c("red","blue"))

# display multiple plots in one area
par(mfrow=c(1,2)) # 

# make the plots
hist(x,main="Hello histogram!!!",col="red")
plot(x,y,main="scatterplot",
     ylab="y values",xlab="x values")

# save plots as an image must have a graphics device (PDF)
pdf("mygraphs/myplot.pdf",width=5,height=5)
plot(x,y)
dev.off()

library(ggplot2)
myData=data.frame(col1=x,col2=y)

# the data is myData and I’m using col1 and col2 
# columns on x and y axes
ggplot(myData, aes(x=col1, y=col2)) +
  geom_point() # map x and y as points

ggplot(myData, aes(x=col1)) +
  geom_histogram() + # map x and y as points
  labs(title="Histogram for a random variable", x="my variable", y="Count")

# data frame with group column showing which 
# groups the vector x and y belong
myData2=rbind(data.frame(values=x,group="x"),
              data.frame(values=y,group="y"))

# x-axis will be group and y-axis will be values
ggplot(myData2, aes(x=group,y=values)) + 
  geom_boxplot()

ggplot(myData2, aes(x=values)) + 
  geom_histogram() +facet_grid(.~group)


# combine both geom histograms and present with scatter plot
library(cowplot)
# histogram
p1 <- ggplot(myData2, aes(x=values,fill=group)) + 
  geom_histogram()
# scatterplot
p2 <- ggplot(myData, aes(x=col1, y=col2)) +
  geom_point() 

# plot two plots in a grid and label them as A and B
plot_grid(p1, p2, labels = c('A', 'B'), label_size = 12)


# Functions
sqSum<-function(x,y){
  result=x^2+y^2
  return(result)
}
# now try the function out
sqSum(2,3)

sqSumPrint<-function(x,y){
  result=x^2+y^2
  cat("here is the result:",result,"\n")
}
# now try the function out
sqSumPrint(2,3)

mat=cbind(c(3,0,3,3),c(3,0,0,0),c(3,0,0,3),c(1,1,0,0),c(1,1,1,0),c(1,1,1,0))
result<-apply(mat,1,sum)
result