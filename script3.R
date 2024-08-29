#create 10 random numbers from uniform distribution 
x=runif(10)
# calculate mean
mean(x)

# calculate median
median(x)

# min and max values
range(x)

# variation
x=rnorm(20,mean=6,sd=0.7)
var(x)

# standard deviation
sd(x)

x=rnorm(20,mean=6,sd=0.7)
IQR(x)
quantile(x)

boxplot(x,horizontal = T)

# get the value of probability density function when X= -2,
# where mean=0 and sd=2
dnorm(-2, mean=0, sd=2)

# get the probability of P(X =< -2) where mean=0 and sd=2
pnorm(-2, mean=0, sd=2)

# get the probability of P(X > -2) where mean=0 and sd=2
pnorm(-2, mean=0, sd=2,lower.tail = FALSE)

# get 5 random numbers from normal dist with  mean=0 and sd=2
rnorm(5, mean=0 , sd=2)

# get y value corresponding to P(X > y) = 0.15 with  mean=0 and sd=2
qnorm( 0.15, mean=0 , sd=2)

library(mosaic)
set.seed(21)
sample1= rnorm(50,20,5) # simulate a sample

# do bootstrap resampling, sampling with replacement
boot.means=do(1000) * mean(resample(sample1))

# get percentiles from the bootstrap means
q=quantile(boot.means[,1],p=c(0.025,0.975))

# plot the histogram
hist(boot.means[,1],col="cornflowerblue",border="white",
     xlab="sample means")
abline(v=c(q[1], q[2] ),col="red")
text(x=q[1],y=200,round(q[1],3),adj=c(1,0))
text(x=q[2],y=200,round(q[2],3),adj=c(0,0))

alpha=0.05
sd=5
n=50
mean(sample1)+qnorm(c(alpha/2,1-alpha/2))*sd/sqrt(n)

set.seed(100)
gene1=rnorm(30,mean=4,sd=2)
gene2=rnorm(30,mean=2,sd=2)
org.diff=mean(gene1)-mean(gene2)
gene.df=data.frame(exp=c(gene1,gene2),
                   group=c( rep("test",30),rep("control",30) ) )


exp.null <- do(1000) * diff(mosaic::mean(exp ~ shuffle(group), data=gene.df))
hist(exp.null[,1],xlab="null distribution | no difference in samples",
     main=expression(paste(H[0]," :no difference in means") ),
     xlim=c(-2,2),col="cornflowerblue",border="white")
abline(v=quantile(exp.null[,1],0.95),col="red" )
abline(v=org.diff,col="blue" )
text(x=quantile(exp.null[,1],0.95),y=200,"0.05",adj=c(1,0),col="red")
text(x=org.diff,y=200,"org. diff.",adj=c(1,0),col="blue")

p.val=sum(exp.null[,1]>org.diff)/length(exp.null[,1])
p.val

# Welch's t-test
stats::t.test(gene1,gene2)

# t-test with equal variance assumption
stats::t.test(gene1,gene2,var.equal=TRUE)


library(qvalue)
data(hedenfalk)

qvalues <- qvalue(hedenfalk$p)$q
bonf.pval=p.adjust(hedenfalk$p,method ="bonferroni")
fdr.adj.pval=p.adjust(hedenfalk$p,method ="fdr")

plot(hedenfalk$p,qvalues,pch=19,ylim=c(0,1),
     xlab="raw P-values",ylab="adjusted P-values")
points(hedenfalk$p,bonf.pval,pch=19,col="red")
points(hedenfalk$p,fdr.adj.pval,pch=19,col="blue")
legend("bottomright",legend=c("q-value","FDR (BH)","Bonferroni"),
       fill=c("black","blue","red"))


set.seed(100)

#sample data matrix from normal distribution

gset=rnorm(3000,mean=200,sd=70)
data=matrix(gset,ncol=6)

# set groups
group1=1:3
group2=4:6
n1=3
n2=3
dx=rowMeans(data[,group1])-rowMeans(data[,group2])

require(matrixStats)

# get the esimate of pooled variance 
stderr = sqrt( (rowVars(data[,group1])*(n1-1) + 
                  rowVars(data[,group2])*(n2-1)) / (n1+n2-2) * ( 1/n1 + 1/n2 ))

# do the shrinking towards median
mod.stderr = (stderr + median(stderr)) / 2 # moderation in variation

# esimate t statistic with moderated variance
t.mod <- dx / mod.stderr

# calculate P-value of rejecting null 
p.mod = 2*pt( -abs(t.mod), n1+n2-2 )

# esimate t statistic without moderated variance
t = dx / stderr

# calculate P-value of rejecting null 
p = 2*pt( -abs(t), n1+n2-2 )

par(mfrow=c(1,2))
hist(p,col="cornflowerblue",border="white",main="",xlab="P-values t-test")
mtext(paste("signifcant tests:",sum(p<0.05))  )
hist(p.mod,col="cornflowerblue",border="white",main="",
     xlab="P-values mod. t-test")
mtext(paste("signifcant tests:",sum(p.mod<0.05))  )


# set random number seed, so that the random numbers from the text
# is the same when you run the code.
set.seed(32)

# get 50 X values between 1 and 100
x = runif(50,1,100)

# set b0,b1 and variance (sigma)
b0 = 10
b1 = 2
sigma = 20
# simulate error terms from normal distribution
eps = rnorm(50,0,sigma)
# get y values from the linear equation and addition of error terms
y = b0 + b1*x+ eps

mod1=lm(y~x)

# plot the data points
plot(x,y,pch=20,
     ylab="Gene Expression",xlab="Histone modification score")
# plot the linear fit
abline(mod1,col="blue")

mod1=lm(y~x)
summary(mod1)

# get confidence intervals 
confint(mod1)

# pull out coefficients from the model
coef(mod1)

set.seed(100)
gene1=rnorm(30,mean=4,sd=2)
gene2=rnorm(30,mean=2,sd=2)
gene.df=data.frame(exp=c(gene1,gene2),
                   group=c( rep(1,30),rep(0,30) ) )

mod2=lm(exp~group,data=gene.df)
summary(mod2)

require(mosaic)
plotModel(mod2)

gene.df=data.frame(exp=c(gene1,gene2,gene2),
                   group=c( rep("A",30),rep("B",30),rep("C",30) ) 
)

mod3=lm(exp~group,data=gene.df)
summary(mod3)

set.seed(100)

#sample data matrix from normal distribution
gset=rnorm(600,mean=200,sd=70)
data=matrix(gset,ncol=6)

require(matrixStats)
means <- rowMeans(data)
vars <- rowVars(data)

hist(means)
boxplot(means)

hist(vars)
boxplot(vars)

sd(data)

set.seed(100)

# create data
mean_sets <- list()
for (x in 1:1000) {
  set <- rpois(30, lambda = 25)
  mean_set <- mean(set)
  mean_sets <- append(mean_sets, mean_set)
}

library(mosaic)
set.seed(21)
sample1= rnorm(1:30) # simulate a sample

# do bootstrap resampling, sampling with replacement
boot.means=do(1000) * mean(resample(sample1))

# get percentiles from the bootstrap means
q=quantile(boot.means[,1],p=c(0.025,0.975))

# plot the histogram
hist(boot.means[,1],col="cornflowerblue",border="white",
     xlab="sample means")
abline(v=c(q[1], q[2] ),col="red")
text(x=q[1],y=200,round(q[1],3),adj=c(1,0))
text(x=q[2],y=200,round(q[2],3),adj=c(0,0))


#HINT
set.seed(100)

#sample 30 values from poisson dist with lamda paramater =30
pois1=rpois(30,lambda=5)

require(mosaic)
quantile((do(1000)*mean(rpois(30,lambda=5)))[,1],probs=c(0.025,0.975)) # regular method
t.test(pois1)

# get 2.5 and 97.5 percentile
quantile((do(1000)*mean(sample(pois1,30,replace=T)))[,1],probs=c(0.025,0.975)) # mosaic method

