
require(tcltk)

read.events=function(){
events.file=tclvalue(tkgetOpenFile())
events=read.csv(events.file)
return(events)
}

read.hauls=function(){
haul.data.file=tclvalue(tkgetOpenFile())
haul.data=read.csv(haul.data.file)
return(haul.data)
}

sor = function(haul.data,events,flag=12){
events$DTIME = as.POSIXct(strptime(events$DTIME, "%m/%d/%Y %H:%M:%OS"))
haul.list=unique(haul.data$HAUL)
for(i in haul.list){
 sgt=haul.data[haul.data$HAUL==i,]
 #sgt$DATE_TIME=sub('/12','/2012',sgt$DATE_TIME)
 sgt$DATE_TIME = as.POSIXct(strptime(sgt$DATE_TIME, "%m/%d/%Y %H:%M:%OS"))
 sgt=sgt[sgt$CABINET_SENSOR_FLAG==flag,]
 haul=sgt$HAUL[1]
 if(length(which(events$HAUL==haul))==2){
 on.bottom=events[events$HAUL==haul & events$EVENT==3,]$DTIME
 off.bottom=events[events$HAUL==haul & events$EVENT==7,]$DTIME
 yy=sgt$MEASUREMENT_VALUE[sgt$MEASUREMENT_VALUE<30 & sgt$DATUM_CODE==0]
 xx=cbind(sgt$RECORD_ID,sgt$DATE_TIME)[sgt$MEASUREMENT_VALUE<30 & sgt$DATUM_CODE==0,]
 point.density = as.data.frame(cbind(
  aa1=c(1000,1000,1000,xx[,2][4:(length(xx[,2]))]-xx[,2][1:(length(xx[,2])-3)]),
  aa2=c(1000,1000,xx[,2][3:(length(xx[,2]))]-xx[,2][1:(length(xx[,2])-2)]),
  aa3=c(1000,xx[,2][2:(length(xx[,2]))]-xx[,2][1:(length(xx[,2])-1)]),
  aa4=c(xx[,2][1:(length(xx[,2])-1)]-xx[,2][2:length(xx[,2])],-1000),
  aa5=c(xx[,2][1:(length(xx[,2])-2)]-xx[,2][3:length(xx[,2])],-1000,-1000),
  aa6=c(xx[,2][1:(length(xx[,2])-3)]-xx[,2][4:length(xx[,2])],-1000,-1000,-1000)
 ))
 idx.rej=(point.density$aa2<60 | point.density$aa5>-60 |
		point.density$aa3-point.density$aa4<60)
 xx2=xx[idx.rej,]
 yy2=yy[idx.rej]
 if(length(yy2)>10){
 par(mfrow=c(3,1),mar=c(2, 4, 1, 2))
 plot(xx[,2],yy,ylim=c(0,40),main=paste('Haul',haul,'all data'),
	xlim=c(on.bottom-360,off.bottom+360))
 md1=smooth.spline(yy~xx[,2],spar=.8) #ca: iteratively fits smooth splines for calc resids and figures (from model 3- md4 or md3? to calc resids); spline = 0.8 includes 80% of points; need to add params to Sean's code
 smooth=predict(md1)
 lines(smooth,col='red')}
 xx1=xx[xx[,2]>on.bottom & xx[,2]<off.bottom,]
 yy1=yy[xx[,2]>on.bottom & xx[,2]<off.bottom]
 if(length(yy1)>10){
 plot(xx1[,2],yy1,ylim=c(0,40),
	xlim=c(on.bottom-360,off.bottom+360))
 abline(v=on.bottom,col=2)
 abline(v=off.bottom,col=2)
 mtext("click here", side = 3, line=0)
 md2=smooth.spline(yy1~xx1[,2],spar=.8)
 smooth2=predict(md2)
 lines(smooth2,col='red')}
 xx3=xx2[xx2[,2]>on.bottom & xx2[,2]<off.bottom,]
 yy3=yy2[xx2[,2]>on.bottom & xx2[,2]<off.bottom]
 if(length(yy3)>10){
 plot(xx3[,2],yy3,ylim=c(0,40),
	xlim=c(on.bottom-360,off.bottom+360))
 abline(v=on.bottom,col=2)
 abline(v=off.bottom,col=2)
 mtext("sparse data removed", side = 3, line=0)
 md3=smooth.spline(yy3~xx3[,2],spar=.8)
 smooth3=predict(md3)
 lines(smooth3,col='red')
 a = locator(n=1)

 resids=yy3-smooth3$y
 max.dist = max(abs(resids))
 if(sd(resids)<5) { #ca: stop distance calc, based on sd of hauls; equation from 2011 paper; threshold stopping value (hard-coded); would need to add to sean's code
  stop.dist=-0.3034*sd(resids)^2 + 2.9428*sd(resids)-0.1112
  } else {stop.dist=7}
 idx=which(abs(resids)==max.dist)
 yy4=yy3
 xx4=xx3
 for(j in 1:length(yy3)){
  yy4=yy4[-idx]
  xx4=xx4[-idx,]
  #plot(xx4,yy4,ylim=c(0,40))
  md4=smooth.spline(yy4~xx4[,2],spar=.8)
  smooth=predict(md4)
  #lines(smooth,col='red')
  resids=yy4-smooth$y
  max.dist = max(abs(resids))
  if(max.dist>stop.dist){ #ca: stop rules- threshold (user spec in Sean's code); 
   idx=which(abs(resids)==max.dist)
   #a = locator(n=1)
   } else {
   print("stopping distance reached")
   #plot(xx4,yy4,ylim=c(0,40))
   md4=smooth.spline(yy4~xx4[,2],spar=.8)
   smooth=predict(md4)
   #lines(smooth,col='red')
   #mtext("data after SOR", side = 3)
   #mtext("click here to acknowledge plot review", side = 1, line=4)
   #a = locator(n=1)
   break
   } 
 }
 plot(xx4[,2],yy4,ylim=c(0,40),xlim=c(on.bottom-360,off.bottom+360),
	,main=paste('Haul',haul,'data after SOR'))
 lines(smooth,col='red')
 abline(v=on.bottom,col=2)
 abline(v=off.bottom,col=2)
 
 accept = readline("Do you accept SOR solution? (y or n)")
 if(accept=="y"){
  DATUM_CODE_N=data.frame(DATUM_CODE_N=rep.int(0,length(xx4[,2])))
  XX=data.frame(xx4)
  colnames(XX)[1]="RECORD_ID"
  newdata=cbind(XX,DATUM_CODE_N)
  newdata1 = merge(sgt,newdata,all=T)
  newdata1=newdata1[,-11]
  newdata1$DATE_TIME=format(newdata1$DATE_TIME, "%m/%d/%Y %H:%M:%S")
  newdata1$DATUM_CODE_N[is.na(newdata1$DATUM_CODE_N)]=10
  newdata1$DATUM_CODE_N[newdata1$DATUM_CODE!=0]=newdata1$DATUM_CODE[newdata1$DATUM_CODE!=0]
  newdata1$DATUM_CODE=newdata1$DATUM_CODE_N
  newdata1=newdata1[,-11]
  write.csv(newdata1,paste("sor_h",haul,".csv",sep=""), quote = F,row.names = F)
  } else {
  DATUM_CODE_N=data.frame(DATUM_CODE_N= rep.int(10,length(sgt[,1])))
  newdata1=cbind(sgt,DATUM_CODE_N)
  newdata1$DATE_TIME=format(newdata1$DATE_TIME, "%m/%d/%Y %H:%M:%S")
  newdata1$DATUM_CODE_N[newdata1$DATUM_CODE!=0]=newdata1$DATUM_CODE[newdata1$DATUM_CODE!=0]
  newdata1$DATUM_CODE=newdata1$DATUM_CODE_N
  newdata1=newdata1[,-11]
  write.csv(newdata1,paste("sor_h",haul,".csv",sep=""), quote = F,row.names = F)
  print("Spread data rejected")}
 } else {
  plot(0,0,pch="",main=paste('Haul',haul))
  text(0,0,"data not sufficient")
  a=locator(n=1)
 }
} else {
plot(0,0,pch="",main=paste('Haul',haul))
text(0,0,"no on bottom times for this haul")
a=locator(n=1)
}
}

}

#sor(haul.data,events)

output_sor_table=function(){
file.type = 'sor_h'
file.list=list.files(pattern=file.type)
sor.data=NA
for(i in 1:length(file.list)){
 sor.data1=read.csv(file.list[i])
 sor.data=rbind(sor.data,sor.data1)
}
sor.data=sor.data[-1,]
write.csv(sor.data,"sor_output.csv")
}



estimate.means = function(events){

events$DTIME = as.POSIXct(strptime(events$DTIME, "%m/%d/%Y %H:%M:%OS"))
haul.data = read.csv('sor_output.csv')
haul.list=unique(haul.data$HAUL)

Haul=NA
mean.spread=NA
N=NA
resid.sd=NA

for(i in 1:length(haul.list)){
 sgr=haul.data[haul.data$HAUL==haul.list[i],]
 sgr$DATE_TIME = as.POSIXct(strptime(sgr$DATE_TIME, "%m/%d/%Y %H:%M:%OS"))
 haul=sgr$HAUL[1]
 Haul[i]=haul
 if(length(which(events$HAUL==haul))==2){
  on.bottom=events[events$HAUL==haul & events$EVENT==3,]$DTIME
  off.bottom=events[events$HAUL==haul & events$EVENT==7,]$DTIME
  xx=sgr$DATE_TIME[sgr$DATUM_CODE==0]
  yy=sgr$MEASUREMENT_VALUE[sgr$DATUM_CODE==0]
  xx1=xx[xx>on.bottom & xx<off.bottom]
  yy1=yy[xx>on.bottom & xx<off.bottom]
  if(length(yy1)>10){
   plot(xx1,yy1,ylim=c(0,40),main=haul,
 	xlim=c(on.bottom-360,off.bottom+360))
   abline(v=on.bottom,col=2)
   abline(v=off.bottom,col=2)
   md2=smooth.spline(yy1~xx1,spar=.8)
   smooth2=predict(md2)
   lines(smooth2,col='red')
   preds=predict(md2,x=as.numeric(seq(on.bottom,off.bottom,length.out=100)))
   mean.spread[i]=mean(preds$y)
   N[i]=length(yy1)
   resid.sd[i]=sd(residuals(md2))
   mtext(paste('Mean =',round(mean.spread[i],2),'SD =',round(resid.sd[i],2)),
		 side = 3, line=0)
   a=locator(n=1)
   } else {
    plot(0,0,pch="",main=paste('Haul',haul))
    text(0,0,"data not sufficient")
    a=locator(n=1)
   }
   } else {
   plot(0,main="no on or off bottom times")
   mean.spread=mean(preds$y)
   a=locator(n=1)
  }
}
summary.spreads = as.data.frame(cbind(Haul=Haul,N=N,Mean=mean.spread,SD=resid.sd))
return(summary.spreads)
}

#estimate.means(events)

