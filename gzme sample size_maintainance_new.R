disc=0.2  #set to 0 for maintainance study first

chn_prp=110/1100

##set up the change in each arm 
tarm1=0.66
tarm2=0.46


alpha=0.05
pwr=0.80

n1=1100*7/11*2/3*0.66*(1-disc)
n2=1100*7/11*1/3*0.66*(1-disc)





###for China 10% evaluation
n1_chn=ceiling(n1*chn_prp)   #pooled GG917  25
n2_chn=ceiling(n2*chn_prp)   #pooled GG917  13


simnum=50000
res1=rep(0,simnum)
res2=rep(0,simnum)
res3=rep(0,simnum)
for(i in 1:simnum){
  set.seed(i*1000)
  arm1=rbinom(n1,1,tarm1)
  arm2=rbinom(n2,1,tarm2)
  
  
  dat1=c(arm1,arm2)
  supp_dat1=c(rep("arm1",length(arm1)),rep("arm2",length(arm2)))
  
  
  tab1=table(supp_dat1, dat1)
  
  
  p1 <- chisq.test(tab1)
  #p1 <- chisq.test(tab1,correct=FALSE)
  
  
  arm1_chn=sample(arm1,n1_chn,replace=FALSE)
  arm2_chn=sample(arm2,n2_chn,replace=FALSE)
  
  
  diff1=mean(arm1)-mean(arm2)
  
  
  diff1_chn=mean(arm1_chn)-mean(arm2_chn)
  
  if(p1$p.value<0.05){
    res1[i]=1
  }
  
  if((p1$p.value<0.05 & (abs(diff1_chn)>=abs(diff1)*0.5))){  #preserve 50% effect
    res2[i]=1
  }
  
  if((p1$p.value<0.05 & (diff1_chn>0))){  
    res3[i]=1
  }
}

mean(res1)  
mean(res2)  
mean(res3)  