# Source: http://home.ewha.ac.kr/~josong/BCRF/all%20functions.r
## this file has all 4 BCRF methods
# 1. predict.BC1: BC using RF to estimate residuals (from Zhang and Lu) 
# 2. predict.BC.SLR : BC using SLR
# 3. predict.res.rot: BC using rotation fitted residuals
# 4. predict.res.rot.seq: BC using best angle rotation

predict.BC1<-function(rfobj,train.data,new.data=train.data){

	#this function predict RF regression with bias correction
	#it's from the paper "bc Rf for regression"
	#response Y must be the last column in a train.data
	
	rf1.pred<-predict(rfobj,train.data)
	rf1.orgy<-rf1$y
	resid1<-rf1.orgy-rf1.pred
	data1<-data.frame(resid1,train.data)
	
	#we will use this function for bias estimation
	lastcol<-ncol(train.data)
	rf.resid<-randomForest(x=train.data, y=resid1)
	#rf.resid<-randomForest(x=train.data[,-lastcol], y=resid1) #BC3
	
	
	#fitted value for new data
	newdata.pred<-predict(rf1,new.data)
	namey<-colnames(train.data)[lastcol]
	new.data[,lastcol]<-newdata.pred  
	colnames(new.data)[lastcol]<-namey #we create dataset with yhat and x.test
	
	newdata.resid1<-predict(rf.resid,new.data) #we use yhat and x from test data to estimate residuals
	newdata.bcpred<-newdata.pred+newdata.resid1
	
	res<-cbind(newdata.pred,newdata.bcpred)
	return(res)  #first col is orginal RF, second col is BCRF
}


predict.BC.SLR<-function(rfobj,train.data,test.data=train.data){

	#this function predict RF regression with bias correction
	#the idea of bias correction is simple.
	#we fit SLR for predicted y vs observed y
	#bias_corrected_yhat=a+b*yhat
	#so when new data (test data) comes, get the fitted value first and 
	#get the fitted value using SLR
	
	#first, fit yhat on y using SLR and get the coeffs
	rf1.pred<-predict(rfobj,train.data)
	rf1.orgy<-rf1$y
	lm1<-lm(rf1.orgy~rf1.pred)$coeff
	#print(lm1)
	
	#next, get the fitted value of RF for test data
	rf1testdata.pred<-predict(rfobj,test.data)
	rf1.testdata.bcpred<-lm1[1]+lm1[2]*rf1testdata.pred
	return(cbind(rf1testdata.pred,rf1.testdata.bcpred))#first col is original RF, second col is BCRF estimates
}


predict.res.rot<-function(rfobj,train.data,test.data=train.data){

	#this function predicts bias corrected RF regression using residual rotation
	#1. fit RF using original data
	#2. compute residuals
	#3. fit RF for Y=residuals, X=fitted Y
	#4. fit RF using test data. Get Yhat^test
	#5. Compute fitted residual^test using the model #3 using Yhat^test
	#6. fit SLR for Y=residual^test, X=Yhat^test
	#7. rotate the fitted line to the horizontal line, Y=0
	#8. Compute rhat^test.rot
	#9. Yhat^BC=Yhat^test+rhat^test.rot


	#1
	rf1.pred<-predict(rfobj,train.data)
	#rf1.y<-rf1$y
	
	#2
	rf1.res<-rf1$y-rf1.pred
	
	#3
	#rfres<-randomForest(x=rf1.y,y=rf1.res)
	rfres<-randomForest(y=rf1.res,x=train.data)

	#4
	rf1test.pred<-predict(rfobj,test.data)

	#5
	lastcol<-ncol(train.data)
	namey<-colnames(train.data)[lastcol]
	test.data[,lastcol]<-rf1test.pred
	colnames(test.data)[lastcol]<-namey
	rf1test.reshat<-predict(rfres,test.data) #check this. ok, it's checked.

	#6
	lm1<-lm(rf1test.reshat~rf1test.pred)

	#7
	#crosspt.y<-0
	crosspt.x<- -lm1$coeff[1]/lm1$coeff[2]
	slope1<-as.numeric(lm1$coeff[2])
	#theta1<-atan((1-slope1)/(1+slope1))
	theta1<-atan(slope1)
	#print(theta1)
	transf.m<-matrix(c(cos(theta1),sin(theta1),-sin(theta1),cos(theta1)),nrow=2)

	#8
	temp1<-cbind(rf1test.pred-crosspt.x,rf1test.reshat)
	temp2<-temp1%*%transf.m
	temp2[,1]<-temp2[,1]+crosspt.x+temp2[,2]

	return(temp2[,1])
}


predict.res.rot.seq<-function(rfobj,train.data,test.data=train.data,alpha=seq(-0.6,0.6,0.01)){

	#this function predicts bias corrected RF regression using best angle rotation
	#it finds the best angle rotation sequentially
	#1. fit RF using original data
	#2. compute residuals
	#3. fit RF for Y=residuals, X=fitted Y
	#4. Find the best rotation angle that minimizes RSS
	
	#5. fit RF using test data. Get Yhat^test
	#6. Compute fitted residual^test using the model #3 using Yhat^test
	#7. fit SLR for Y=residual^test, X=Yhat^test
	#8. rotate the fitted line using the best angle margin found in #4
	#8. Compute rhat^test.rot
	#9. Yhat^BC=Yhat^test+rhat^test.rot


	#1
	rf1.pred<-predict(rfobj,train.data)
	rf1.y<-rf1$y
	
	#2
	rf1.res<-rf1.y-rf1.pred
	
	#3
	#rfres<-randomForest(x=rf1.y,y=rf1.res)
	#rfres<-randomForest(rf1.res~rf1.y) #Fit RF using train.residual and train.Y
	rfres<-randomForest(x=train.data,y=rf1.res)


	#5
	rf1train.reshat<-predict(rfres,train.data) #estimated residuals for train set

	#6
	lm1<-lm(rf1train.reshat~rf1.pred)

	#7 find the best rotation angle
	alpha.len<-length(alpha)
	mse<-rep(0,alpha.len)
	#crosspt.y<-0
	crosspt.x<- -lm1$coeff[1]/lm1$coeff[2]
	for (i in 1:alpha.len){
		slope1<-as.numeric(lm1$coeff[2])
		theta1<-atan(slope1)+alpha[i]
		transf.m<-matrix(c(cos(theta1),sin(theta1),-sin(theta1),cos(theta1)),nrow=2)
		temp1<-cbind(rf1.pred-crosspt.x,rf1train.reshat)
		temp2<-temp1%*%transf.m
		res<-temp2[,1]+crosspt.x+temp2[,2]
		mse[i]<-mean((res-rf1$y)^2)
	}
	
	#8
	
	bestalpha.ind<-which.min(mse)
	
	
	#9
	rf1test.pred<-predict(rfobj,test.data)
	rf1.y<-rf1test.pred
	#rf1test.reshat<-predict(rfres,rf1.y) #estimated residuals for test set
	rf1test.reshat<-predict(rfres,test.data)
	lm1<-lm(rf1test.reshat~rf1test.pred)
	crosspt.x<- -lm1$coeff[1]/lm1$coeff[2]
	slope1<-as.numeric(lm1$coeff[2])
	theta1<-atan(slope1)+alpha[bestalpha.ind] #best angle rotation
	transf.m<-matrix(c(cos(theta1),sin(theta1),-sin(theta1),cos(theta1)),nrow=2)
	temp1<-cbind(rf1test.pred-crosspt.x,rf1test.reshat)
	temp2<-temp1%*%transf.m
	res<-temp2[,1]+crosspt.x+temp2[,2]
	
	return(res)
	
}

