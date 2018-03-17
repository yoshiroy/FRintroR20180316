1+2
pi
sin(pi/2)
exp(1)
e
e<-exp(1)
e^2
1:10                # 1,2,3,...

# questdata.xlsx の読み込み
# 右の Environment で Import Dataset / From Excel
summary(questdata) #要約
names(questdata)   #変数名の表示
plot(questdata)  #質的変数がエラーとなる
str(questdata)

questdata$携帯電話 <- as.factor(questdata$携帯電話)
questdata$血液型 <- as.factor(questdata$血液型)
questdata$アルバイト <- as.factor(questdata$アルバイト)
questdata$サークル <- as.factor(questdata$サークル)
questdata$自宅<- as.factor(questdata$自宅) 
str(questdata)
plot(questdata)

#　グラフ描画
hist(questdata$通学時間) 
boxplot(questdata$通学時間)
boxplot(通学時間~自宅,data=questdata)
# plot関数はデータの方により振る舞いが変わる
plot(questdata) #データフレームのプロット
plot(questdata$通学時間) #量的変数(インデックスプロット)
plot(questdata$血液型) #質的変数（棒グラフ）
plot(睡眠時間~通学時間,data=questdata) #量的2変数
plot(jitter(睡眠時間)~通学時間,data=questdata) #量的2変数 jitter誤差付
plot(携帯料金~携帯電話,data=questdata) #量的変数 vs 質的変数
plot(携帯電話~携帯料金,data=questdata) #質的変数 vs 量的変数 
plot(睡眠時間~通学時間,col=自宅,data=questdata) #量的2変数|質的変数
plot(jitter(睡眠時間)~通学時間,col=自宅,data=questdata) #量的2変数|質的変数
plot(通学時間~.,data=questdata) #量的変数 vs 他の変数
plot(携帯電話~血液型,data=questdata)  #質的2変数
plot(血液型~携帯電話,data=questdata)  #質的2変数

#回帰分析（線形モデル）
lm(睡眠時間~通学時間,data=questdata)
questdata.lm <- lm(睡眠時間~通学時間,data=questdata)
summary(questdata.lm)
plot(questdata.lm)
plot(睡眠時間~通学時間,data=questdata)
abline(questdata.lm)  #回帰直線を追加
# 重回帰分析
questdata.lm2 <- lm(睡眠時間~通学時間+自宅,data=questdata)
summary(questdata.lm2)
plot(睡眠時間~通学時間,col=自宅,data=questdata)

# Rコマンダーの起動
library(Rcmdr)  # もしくは Package タブでRcmdr をクリック


# ggplot2 グラフ
library(ggplot2)
data(Cars93,package="MASS")
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

#dplyrによるデータの抽出
library(dplyr)
dplyr::select(Cars93,Horsepower,MPG.city,Type)
dplyr::select(Cars93,Type:MPG.city)
dplyr::filter(Cars93,Type=="Midsize")
Cars93 %>%
  dplyr::select(Horsepower,MPG.city,Type) %>%
  dplyr::filter(Type=="Midsize") %>%
  ggplot(aes(x=Horsepower,y=MPG.city))+geom_point()+stat_smooth(method=lm)
Cars93 %>%
  dplyr::select(Horsepower,MPG.city,Type) %>%
  dplyr::filter(Type=="Midsize") -> Cars93mid
ggplot(Cars93mid,aes(x=Horsepower,y=MPG.city))+geom_point()+stat_smooth(method=lm)
lm.Mid<-lm(MPG.city~Horsepower,data=Cars93mid)
summary(lm.Mid)
head(mutate(Cars93mid,var1=MPG.city/Horsepower))
head(arrange(Cars93mid,Horsepower)) #desc(Horsepower)
summarize(Cars93mid,n=n(),m.Hp=mean(Horsepower),s.Hp=sd(Horsepower))
Cars93 %>%
  group_by(Type) %>%
  summarize(ケース数=n(),Hpの平均=mean(Horsepower),Hpの標準偏差=sd(Horsepower))

## baseball example
hitting <- read.csv("NPB年度別打撃結果.csv")
names(hitting)
summary(hitting)
hitting$年度 <- factor(hitting$年度)
table(hitting$年度)
names(table(hitting$年度))
levels(hitting$年度)
hitting[hitting$年度=="2016",]
table(hitting[hitting$年度=="2016",]$球団)
hitting[hitting$年度=="2016" & hitting$球団=="読売",]

#library(dplyr)
#library(ggplot2)
table(hitting$年度)
hitting %>% 
  dplyr::filter(年度 %in% 2011:2016) %>%
  dplyr::filter(打席数>=400) -> hittingR5
plot(本塁打~三振,data=hittingR5)
names(hittingR5)
plot(本塁打~三振,data=hittingR5)
ggplot(hittingR5, aes(x=三振,y=本塁打))+geom_point()
ggplot(hittingR5, aes(x=三振,y=本塁打))+geom_point()+facet_grid(球団~.)

hitting %>% 
  dplyr::filter(年度 %in% 2011:2016) %>%
  dplyr::group_by(年度,球団) %>%
  dplyr::summarise(球団本塁打数=sum(本塁打))

hitting %>% 
  dplyr::filter(年度 %in% 2011:2016) %>%
  dplyr::group_by(年度,球団) %>%
  dplyr::summarise(球団本塁打数=sum(本塁打)) %>%
  ggplot(., aes(x=年度,y=球団本塁打数,group=球団,colour=球団))+geom_line()
