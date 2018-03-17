#2.1
1+2
q()

#2.2
mean(c(148, 160, 159, 153, 151, 140, 158, 137, 149, 160)) # 変数(ベクトル)の平均値
height <- c(148, 160, 159, 153, 151, 140, 158, 137, 149, 160) # 代入
height  # height を確認
mean(height)   # heightの平均を計算

#2.3
sex <- c("F","M","M","F","F","F","M","F","M","F")
tapply(height, sex, mean)     #グループごとの平均を求める
mydata <- data.frame(height,sex)
mydata
mydata$height
mean(mydata$height)
# exdata1 <- read.table(file="c:/tmp/example1.dat", header=T)
exdata1 <- read.table(file="example1.dat", header=T)
ls()
rm(height, sex)

attach(exdata1)
mean(height)
var(height)
sd(height)

table(sex)
table(height)
table(cut(height,seq(135,165,by=5)))
summary(exdata1)


#2.4
hist(height)
hist(height,breaks=3)
hist(height,br=c(136,140,144,148,152,156,160,170))
op <- par(mfrow=c(2,1)) #複数グラフモード、opにグラフパラメータを保存
hist(height)
hist(height, br=6,col="lightblue", border="pink")
par(op)                 #グラフパラメータを元に戻す
str(hist(height,plot=F))  
h <- hist(height,plot=F)
h$counts
stem(height)

par(mfrow=c(1,2)) #複数グラフモード
boxplot(height, main="height")
boxplot(height~sex, main="height~sex")

plot(height)
plot(seq(1,length(height)),height,type="b",xlab="") 
plot(height,weight)  #散布図

plot(exdata1)  #データフレームをプロットする
par(mfrow=c(1,2))
plot(exdata1$sex)  #データフレーム内の1変数をプロットする
plot(weight~height, data=exdata1)　#データフレーム内の変数のプロット

curve(sin,0,2*pi)
curve(cos, add= TRUE, col="red")

dnorm(2,mean=0,sd=1)
pnorm(2,0,1)
qnorm(0.975)

#2.6
t.test(height~sex,data=mydata)

#2.7
bicycle <- read.table("bicycle.csv", header=T, sep=",")
lm(separation ~ travel, data=bicycle)
bicycle.lm <- lm(separation ~ travel, data=bicycle)
summary(bicycle.lm)
coef(bicycle.lm)
plot(bicycle)
names(bicycle.lm)
abline(bicycle.lm$coef)

## 3
#3.1
demo()
demo(graphics)
help(plot)
apropos(plot)
help.search("plot")

#3.2
1:5     #seq(1,5)
5:1     #seq(5,1)
seq(2,10,3)  #seq(from=2,to=10,by=3)
rep("F",3)
rep(1:3,c(2,3,4))
uriage <-c(243,224,253,335,314)
(kokusei <- uriage/sum(uriage))
(ruisekihi<- cumsum(uriage)/sum(uriage))

height
height[3:5]
height[c(-1,-3,-7)]
weight>45
height[weight>45]
height[sex=="F"]

c(height,weight)
(in1 <- list(height,weight) )
(in2 <- array(c(height,weight), dim=c(30, 2)) )
(in3 <- matrix(c(height,weight), ncol=2) )
in1[[1]]
in1[[1]][3]
names(in1) <- c("身長","体重")
in1$身長
in1$身長[5]

cov(in2)
eigen(cov(in2))

#3.3
library(MASS)
library(help="MASS")
install.packages("UsingR")
library(UsingR)

library(scatterplot3d)
scatterplot3d(iris[,1:3])
scatterplot3d(iris[,c(1,2,4)])

library(car)
?scatterplot

library(JGR)
data(Cars93)
attach(Cars93)
Cars93[1:5,]
dim(Cars93)
names(Cars93)
imosaic(AirBags,Cylinders,Origin)
imosaic(AirBags,Cylinders,Origin,type="mul")
iplot(MPG.city,RPM)
ipcp(Cars93)

#3.4
in4 <- scan("example2.dat")
in4
(in5 <- scan("example2.dat", list(0,0)) )
read.table(file=file.choose())

#3.5

tmean <- function (x) 
{
    (sum(x)-max(x)-min(x))/(length(x)-2)
}
tmean2<-tmean
edit(tmean2)
height-median(height)  #中央値からの偏差
sum(abs(height-median(height))) #中央値からの絶対偏差の和
sum((height-median(height))^2) #中央値からの偏差の2乗和
(height-median(height))%*%(height-median(height)) #内積で計算

#4.1
objects()
objects (pat="we*") #weから始まるオブジェクト名
rm("xname")

sink("071222.log")
objects()
table(height)
mean(height)
sink()


コマンドプロンプト
#4.2
cd "C:\Program Files\R\R-2.6.1\bin"
C:\Program Files\R\R-3.2.2\bin>Rgui.exe --help

######
## 自作関数
piplot=function(n){
  x=runif(n)
  y=runif(n)
  nin=0
  curve(sqrt(1-x^2),xlim=c(0,1),ylim=c(0,1))
  for (i in 1:n){
    if(x[i]^2+y[i]^2<1){
      nin=nin+1
      points(x[i],y[i],col="red")
    }else{
      points(x[i],y[i],col="blue")
    }
  }
  return(4* nin/n)
}
piplot(1000)


# Rstudio
data(iris)
summary(iris)
plot(iris)
plot(iris[,1:2])

#　グラフ
plot(iris[,1:2],type="n")
points(iris[iris$Species=="setosa",1:2])
points(iris[iris$Species=="versicolor",1:2],col="red")
points(iris[iris$Species=="virginica",1:2],col="blue")
plot(Sepal.Width~Sepal.Length,data=iris)
plot(Sepal.Width~Sepal.Length,data=iris,col=Species)
legend("topright",legend=levels(iris$Species),pch=1,col=1:3)
plot(Sepal.Width~Sepal.Length,data=iris,col=Species)
legend(x=6.8,y=4.5,legend=levels(iris$Species),pch=1,col=1:3)
par(mfrow=c(2,2))
plot(iris[iris$Species=="setosa",1:2],main="setosa")
plot(iris[iris$Species=="versicolor",1:2],main="versicolor")
plot(iris[iris$Species=="virginica",1:2],main="virginica")
par(mfrow=c(1,1))
plot(Sepal.Length~Species,data=iris)

# lattice graph
xyplot(Sepal.Width~Sepal.Length,data=iris)
xyplot(Sepal.Width~Sepal.Length|Species,data=iris)
xyplot(Sepal.Width~Sepal.Length,group=Species,data=iris)
xyplot(Sepal.Width~Sepal.Length,group=Species,data=iris,auto.key=list(title="Species",column=3))
xyp.iris2 <- xyplot(Sepal.Width~Sepal.Length,group=Species,data=iris)
update(xyp.iris2,auto.key=list(title="Species",column=3))
histogram(~Sepal.Length,data=iris)
histogram(~Sepal.Length|Species,data=iris)
histogram(~Sepal.Length|Species,data=iris,layout=c(1,3))
densityplot(~Sepal.Length,data=iris)
densityplot(~Sepal.Length|Species,data=iris,plot.points="rug",layout=c(1,3))
densityplot(~Sepal.Length,group=Species,data=iris,plot.points="rug",auto.key=list(column=3))
bwplot(Species~Sepal.Length,data=iris)
bwplot(Sepal.Length~Species,data=iris)
stripplot(Sepal.Length~Species,data=iris)
stripplot(Sepal.Length~Species,data=iris,jitter.data=TRUE)

#データの抽出利用
data(Cars93, package="MASS")
names(Cars93)
xyplot(MPG.city~Horsepower,data=Cars93)
xyplot(MPG.city~Horsepower|Type,data=Cars93)
Cars93[Cars93$Type=="Midsize",c(7,13)]
plot(Cars93[Cars93$Type=="Midsize",c(7,13)])
subset(Cars93,Type=="Midsize",c(Horsepower,MPG.city))
with(subset(Cars93,Type=="Midsize",c(Horsepower,MPG.city)),
     plot(MPG.city~Horsepower))
with(subset(Cars93,Type=="Midsize",c(Horsepower,MPG.city)),
     {plot(MPG.city~Horsepower)
      lm.mid<-lm(MPG.city~Horsepower)
      summary(lm.mid)
      })
Cars93mid<-subset(Cars93,Type=="Midsize",c(Horsepower,MPG.city))
plot(MPG.city~Horsepower,data=Cars93mid)
lm.mid<-lm(MPG.city~Horsepower,data=Cars93mid)
summary(lm.mid)

# ggplot2 グラフ
ggplot(Cars93, aes(x=Horsepower,y=MPG.city))+geom_point()
gp<-ggplot(Cars93, aes(x=Horsepower,y=MPG.city))
gp+geom_point()
gp+geom_point()+stat_smooth(method=lm)
gp+geom_point()+stat_smooth(method=lm,se=FALSE)
ggplot(Cars93, aes(x=Horsepower,y=MPG.city,colour=Type))+geom_point()
gpType<-ggplot(Cars93, aes(x=Horsepower,y=MPG.city,colour=Type))
gpType+geom_point()+stat_smooth(method=lm,se=FALSE)
gpType2<-ggplot(Cars93, aes(x=Horsepower,y=MPG.city))
gpType2+geom_point()+stat_smooth(method=lm,se=FALSE)+facet_grid(Type~.)
gpType2+geom_point()+stat_smooth(method=lm,se=FALSE)+facet_wrap(~Type)
gpType2+geom_point()+stat_smooth(method=lm,se=FALSE)+facet_wrap(~Type,ncol=2)
ggplot(Cars93,aes(x=Horsepower,colour=Type,fill=Type))+geom_histogram()
ggplot(Cars93,aes(x=Horsepower,colour=Type,fill=Type))+geom_histogram(position="identity",alpha=0.4)
ggplot(Cars93,aes(x=Horsepower,colour=Type,fill=Type))+geom_density(alpha=0.4)
ggplot(Cars93,aes(x=Horsepower))+geom_histogram()+facet_grid(Type~.)
ggplot(Cars93,aes(x=Horsepower))+geom_density()+facet_grid(Type~.)


