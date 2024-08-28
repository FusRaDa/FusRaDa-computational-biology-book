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
result<-apply(mat,1,sum) # second paramter indicates start of index
result

input=c(1,2,3)
lapply(input,function(x) x^2)

Xs=0:5
Ys=c(2,2,2,3,3,3)
result<-mapply(function(x,y) sum(x,y),Xs,Ys)
result

result=Xs+Ys
result

colSums(mat)
rowSums(mat)


# Exercies Ch2
2 + 3
sqrt(36)
log10(1000)
log2(32)
x <- 2 + 3 + 4
abs(5-145)


x <- sqrt(625) / 5
x

x <- x * 10000
x

vec <- c(1, 2, 3, 5, 10)
vec

length(vec)

vec <- c(2:15)
vec

rep(x=4, times=10)

c(TRUE, FALSE, TRUE, FALSE)

genes = c("PAX6", "ZIC2", "OCT4", "SOX2")
genes[c(1, 2)]

myvec=1:5
# the length of the logical vector 
# should be equal to length(myvec) 
myvec[c(TRUE,TRUE,FALSE,FALSE,FALSE)] 
myvec[c(TRUE,FALSE,FALSE,FALSE,TRUE)]

myvec > 3
myvec == 4
myvec <= 2
myvec != 4

logicals <- myvec > 2
myvec[logicals]

matrix(1:15, nrow = 5, ncol = 3)
mat <- matrix(1:15, nrow = 5, ncol = 3, byrow = TRUE)
mat[1:3, 1:3]
mat2 <- mat[4:5, ]
cols <- mat[, 1:2]
class(cols)

df <- data.frame(col1 = 1:5, col2 = c("a", "b", "c", "e", "f"), col3 = 5:1)
df

df[1:2, 1:2]
df[4:5, ]

df$col2
df[, 2]

rows <- df[, 1] > 3
rows1 <- df$col1[rows]
df[rows1, ]

rows <- df[, 1] >= 3
rows1 <- df$col1[rows]
df[rows1, ]

as.matrix(df) # turns values into strings

mylist <- list(name = "Fred", surname = "Rada", status = "Married", gender = "Male")

mylist$name

mylist$gender

mylist[1]
mylist[4]

myfactor <- factor(c("a", "b", "c", "d", "e"))

myc <- c("a", "b", "c", "d")

myfact <- as.factor(myc)
as.character(myfact)

cpgtFilePath=system.file("extdata",
                         "CpGi.table.hg18.txt",
                         package="compGenomRData")
cpgtFilePath
cpgiSepComma=read.table(cpgtFilePath,header=TRUE,sep=",", stringsAsFactors = FALSE, nrows = 10)
head(cpgiSepComma)

cpgiSepComma


cpgtFilePath=system.file("extdata",
                         "CpGi.table.hg18.txt",
                         package="compGenomRData")
cpgtFilePath
cpgiSepComma=read.table(cpgtFilePath,header=FALSE,sep=",")

write.table(cpgiSepComma,file="my.cpgi.file2.txt",
            row.names=FALSE,col.names=FALSE, quote = FALSE, sep = "\t")

tab <- read.table("my.cpgi.file.txt", header=TRUE, row.names=NULL)
tab2 <- read.table("my.cpgi.file2.txt", header=TRUE, row.names=NULL)

tab2[1:10,]
tab2[, 1:3]

chr1s <- tab2[tab2$row.names == "chr1", ]


cpgtFilePath1=system.file("extdata",
                         "rn4.refseq.bed",
                         package="compGenomRData")

cpgtFilePath2=system.file("extdata",
                          "rn4.refseq2name.txt",
                          package="compGenomRData")

ref1 <- read.table(cpgtFilePath1,header=FALSE)
ref2 <- read.table(cpgtFilePath2,header=FALSE)

head(ref1)
head(ref2)

new.df <- merge(x = ref1, y = ref2, by.x = "V4", by.y = "V1")
head(new.df) # CORRECT ANS
