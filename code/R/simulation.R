# Source: http://home.ewha.ac.kr/~josong/BCRF/simulation2.r
## real data performance
library(randomForest)
#1. Boston housing

house<-read.table("housing.data.txt")
colnames(house)[14]<-"y"

p<-0.7
n<-nrow(house)
sim.n<-1000
res.house<-matrix(0,nrow=sim.n,ncol=6)
for (i in 1:sim.n){
	shuf.ind<-sample(n)
	train.ind<-shuf.ind[1:floor(n*p)]
	train.data<-house[train.ind,]
	test.data<-house[-train.ind,]

	rf1<-randomForest(y~., data=train.data)
	rf2<-randomForest(y~., data=train.data,corr.bias=TRUE)
	res0<-predict(rf2,test.data) 
	res1<-predict.BC1(rf1,train.data,test.data)
	res2<-predict.BC.SLR(rf1,train.data,test.data)
	res3<-predict.res.rot(rf1,train.data,test.data)
	res4<-predict.res.rot.seq(rf1,train.data,test.data)

	res.house[i,1]<-mean((res1[,1]-test.data$y)^2)	#naive RF
	res.house[i,2]<-mean((res2-test.data$y)^2)		#RF with built-in bias correction
	res.house[i,3]<-mean((res1[,2]-test.data$y)^2)	#BC1
	res.house[i,4]<-mean((res2[,2]-test.data$y)^2)	#BC with SLR
	res.house[i,5]<-mean((res3-test.data$y)^2)		#BC with rot
	res.house[i,6]<-mean((res4-test.data$y)^2)		#BC with rot.seq
}
colMeans(res.house)
apply(res.house,2,sd)

#2. servo

servo<-read.table("servo.data.txt",sep=",")
colnames(servo)[5]<-"y"

p<-0.7
n<-nrow(servo)
sim.n<-1000
res.servo<-matrix(0,nrow=sim.n,ncol=6)
for (i in 1:sim.n){
	shuf.ind<-sample(n)
	train.ind<-shuf.ind[1:floor(n*p)]
	train.data<-servo[train.ind,]
	test.data<-servo[-train.ind,]

	rf1<-randomForest(y~., data=train.data)
	rf2<-randomForest(y~., data=train.data,corr.bias=TRUE)
	res0<-predict(rf2,test.data) 
	res1<-predict.BC1(rf1,train.data,test.data)
	res2<-predict.BC.SLR(rf1,train.data,test.data)
	res3<-predict.res.rot(rf1,train.data,test.data)
	res4<-predict.res.rot.seq(rf1,train.data,test.data)

	res.servo[i,1]<-mean((res1[,1]-test.data$y)^2)	#naive RF
	res.servo[i,2]<-mean((res2-test.data$y)^2)		#RF with built-in bias correction
	res.servo[i,3]<-mean((res1[,2]-test.data$y)^2)	#BC1
	res.servo[i,4]<-mean((res2[,2]-test.data$y)^2)	#BC with SLR
	res.servo[i,5]<-mean((res3-test.data$y)^2)		#BC with rot
	res.servo[i,6]<-mean((res4-test.data$y)^2)		#BC with rot.seq
}
colMeans(res.servo)
apply(res.servo,2,sd)


#3. Abalone

abalone<-read.table("abalone.data.txt",sep=",")
colnames(abalone)[9]<-"y"

p<-0.7
n<-nrow(abalone)
sim.n<-100
res.abalone<-matrix(0,nrow=sim.n,ncol=6)
for (i in 1:sim.n){
	shuf.ind<-sample(n)
	train.ind<-shuf.ind[1:floor(n*p)]
	train.data<-abalone[train.ind,]
	test.data<-abalone[-train.ind,]

	rf1<-randomForest(y~., data=train.data)
	rf2<-randomForest(y~., data=train.data,corr.bias=TRUE)
	res0<-predict(rf2,test.data) 
	res1<-predict.BC1(rf1,train.data,test.data)
	res2<-predict.BC.SLR(rf1,train.data,test.data)
	res3<-predict.res.rot(rf1,train.data,test.data)
	res4<-predict.res.rot.seq(rf1,train.data,test.data)

	res.abalone[i,1]<-mean((res1[,1]-test.data$y)^2)	#naive RF
	res.abalone[i,2]<-mean((res2-test.data$y)^2)		#RF with built-in bias correction
	res.abalone[i,3]<-mean((res1[,2]-test.data$y)^2)	#BC1
	res.abalone[i,4]<-mean((res2[,2]-test.data$y)^2)	#BC with SLR
	res.abalone[i,5]<-mean((res3-test.data$y)^2)		#BC with rot
	res.abalone[i,6]<-mean((res4-test.data$y)^2)		#BC with rot.seq
}
colMeans(res.abalone)
apply(res.abalone,2,sd)


#4. Auto-mpg

auto<-read.table("auto-mpg.data.txt")
auto<-auto[,-9]
auto[,4]<-as.numeric(auto[,4])
colnames(auto)[1]<-"y"

p<-0.7
n<-nrow(auto)
sim.n<-1000
res.auto<-matrix(0,nrow=sim.n,ncol=6)
for (i in 1:sim.n){
	shuf.ind<-sample(n)
	train.ind<-shuf.ind[1:floor(n*p)]
	train.data<-auto[train.ind,]
	test.data<-auto[-train.ind,]

	rf1<-randomForest(y~., data=train.data)
	rf2<-randomForest(y~., data=train.data,corr.bias=TRUE)
	res0<-predict(rf2,test.data) 
	res1<-predict.BC1(rf1,train.data,test.data)
	res2<-predict.BC.SLR(rf1,train.data,test.data)
	res3<-predict.res.rot(rf1,train.data,test.data)
	res4<-predict.res.rot.seq(rf1,train.data,test.data)

	res.auto[i,1]<-mean((res1[,1]-test.data$y)^2)	#naive RF
	res.auto[i,2]<-mean((res2-test.data$y)^2)		#RF with built-in bias correction
	res.auto[i,3]<-mean((res1[,2]-test.data$y)^2)	#BC1
	res.auto[i,4]<-mean((res2[,2]-test.data$y)^2)	#BC with SLR
	res.auto[i,5]<-mean((res3-test.data$y)^2)		#BC with rot
	res.auto[i,6]<-mean((res4-test.data$y)^2)		#BC with rot.seq
}
colMeans(res.auto)
apply(res.auto,2,sd)




#5. Concrete compressive

concrete<-read.table("Concrete_Data.csv",sep=",")
colnames(concrete)[9]<-"y"

p<-0.7
n<-nrow(concrete)
sim.n<-100
res.concrete<-matrix(0,nrow=sim.n,ncol=6)
for (i in 1:sim.n){
	shuf.ind<-sample(n)
	train.ind<-shuf.ind[1:floor(n*p)]
	train.data<-concrete[train.ind,]
	test.data<-concrete[-train.ind,]

	rf1<-randomForest(y~., data=train.data)
	rf2<-randomForest(y~., data=train.data,corr.bias=TRUE)
	res0<-predict(rf2,test.data) 
	res1<-predict.BC1(rf1,train.data,test.data)
	res2<-predict.BC.SLR(rf1,train.data,test.data)
	res3<-predict.res.rot(rf1,train.data,test.data)
	res4<-predict.res.rot.seq(rf1,train.data,test.data)

	res.concrete[i,1]<-mean((res1[,1]-test.data$y)^2)	#naive RF
	res.concrete[i,2]<-mean((res2-test.data$y)^2)		#RF with built-in bias correction
	res.concrete[i,3]<-mean((res1[,2]-test.data$y)^2)	#BC1
	res.concrete[i,4]<-mean((res2[,2]-test.data$y)^2)	#BC with SLR
	res.concrete[i,5]<-mean((res3-test.data$y)^2)		#BC with rot
	res.concrete[i,6]<-mean((res4-test.data$y)^2)		#BC with rot.seq
}
colMeans(res.concrete)
apply(res.concrete,2,sd)


par(mfrow=c(2,3))
plot(test.data$y,res1[,1],main="RF")
abline(0,1,col=2)
plot(test.data$y,res2[,2],main="BC1")
abline(0,1,col=2)
plot(test.data$y,res1[,2],main="SLR")
abline(0,1,col=2)
plot(test.data$y,res2[,2],main="SLR")
abline(0,1,col=2)
plot(test.data$y,res3,main="Rotation")
abline(0,1,col=2)
plot(test.data$y,res4,main="Rot.seq")
abline(0,1,col=2)

postscript("rf.bias.ps",horizontal=F)
par(mfrow=c(1,1))
plot(test.data$y,res1[,1],main="Random Forest: Observed vs. Predicted",
xlab="Observed values", ylab="Predicted values")
abline(0,1)
dev.off()

