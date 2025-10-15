##based on protocol, the sample size is 1300 with 2:2:3:3:3 to LY3537031 4 arms and 1 placebo arm. 200:200:300:300:300
##primary endpoint is percent change in HDD (heavy drinking days) and proportion of achieving WHO 2-level reduction
##key secondary endpoint is the response of no HDDs from week 25 to week 48

##assumptions for primary endpoint
trtdiff=0.2
sd1=0.25
disc=0.3

chn_prp=110/1100

##set up the change in each arm by myself, can evaluate the difference of adjusting each arm values
tarm1=-0.4
tarm2=-0.4
tarm3=-0.4
tarm4=-0.2

##set up the change in each arm for dual primary endpoint
tarm1_1=0.6
tarm2_1=0.6
tarm3_1=0.6
tarm4_1=0.4

alpha=0.05
pwr=0.90

n1=200*(1-disc)
n2=200*(1-disc)
n3=300*(1-disc)
n4=400*(1-disc)


###for China 10% evaluation
n1_chn=ceiling(200*(1-disc)*chn_prp)
n2_chn=ceiling(200*(1-disc)*chn_prp)
n3_chn=ceiling(300*(1-disc)*chn_prp)
n4_chn=ceiling(400*(1-disc)*chn_prp)

simnum=50000
res1=rep(0,simnum)
res2=rep(0,simnum)
res3=rep(0,simnum)
for(i in 1:simnum){
  set.seed(i*1000)
  arm1=rnorm(n1,tarm1,sd1)
  arm2=rnorm(n2,tarm2,sd1)
  arm3=rnorm(n3,tarm3,sd1)
  arm4=rnorm(n4,tarm4,sd1)
  
  ind1=sample(seq(1,n1,1),n1_chn,replace=FALSE)
  ind2=sample(seq(1,n2,1),n2_chn,replace=FALSE)
  ind3=sample(seq(1,n3,1),n3_chn,replace=FALSE)
  ind4=sample(seq(1,n4,1),n4_chn,replace=FALSE)
  
  arm1_chn=arm1[ind1]
  arm2_chn=arm2[ind2]
  arm3_chn=arm3[ind3]
  arm4_chn=arm4[ind4]
  
  p1=t.test(arm1,arm4,alternative = "two.sided")
  p2=t.test(arm2,arm4,alternative = "two.sided")
  p3=t.test(arm3,arm4,alternative = "two.sided")
  p4=t.test(arm4,arm4,alternative = "two.sided")
  
  diff1=mean(arm1)-mean(arm4)
  diff2=mean(arm2)-mean(arm4)
  diff3=mean(arm3)-mean(arm4)
  
  diff1_chn=mean(arm1_chn)-mean(arm4_chn)
  diff2_chn=mean(arm2_chn)-mean(arm4_chn)
  diff3_chn=mean(arm3_chn)-mean(arm4_chn)
  
  #p1_chn=t.test(arm1_chn,arm4_chn,alternative = "two.sided")
  #p2_chn=t.test(arm2_chn,arm4_chn,alternative = "two.sided")
  #p3_chn=t.test(arm3_chn,arm4_chn,alternative = "two.sided")
  
  ########second primary endpoint
  arm1=rbinom(n1,1,tarm1_1)
  arm2=rbinom(n2,1,tarm2_1)
  arm3=rbinom(n3,1,tarm3_1)
  arm4=rbinom(n4,1,tarm4_1)
  
  arm1_chn=arm1[ind1]
  arm2_chn=arm2[ind2]
  arm3_chn=arm3[ind3]
  arm4_chn=arm4[ind4]
  
  dat1=c(arm1,arm4)
  supp_dat1=c(rep("arm1",length(arm1)),rep("arm4",length(arm4)))
  dat2=c(arm2,arm4)
  supp_dat2=c(rep("arm2",length(arm1)),rep("arm4",length(arm4)))
  dat3=c(arm3,arm4)
  supp_dat3=c(rep("arm3",length(arm3)),rep("arm4",length(arm4)))

  
  tab1=table(supp_dat1, dat1)
  tab2=table(supp_dat2, dat2)
  tab3=table(supp_dat3, dat3)
  
  p1_1 <- chisq.test(tab1, correct = TRUE)
  p2_1 <- chisq.test(tab2, correct = TRUE)
  p3_1 <- chisq.test(tab3, correct = TRUE)
  
  diff1_1=mean(arm1)-mean(arm4)
  diff2_1=mean(arm2)-mean(arm4)
  diff3_1=mean(arm3)-mean(arm4)
  
  diff1_chn_1=mean(arm1_chn)-mean(arm4_chn)
  diff2_chn_1=mean(arm2_chn)-mean(arm4_chn)
  diff3_chn_1=mean(arm3_chn)-mean(arm4_chn)
  
  
  
  if(p1$p.value<(0.05/3/2) | p2$p.value<(0.05/3/2) | p3$p.value<(0.05/3/2) |p1_1$p.value<(0.05/3/2) | p2_1$p.value<(0.05/3/2) | p3_1$p.value<(0.05/3/2)){
    res1[i]=1
  }
  
  if((p1$p.value<0.05/3/2 & (abs(diff1_chn)>=abs(diff1)*0.5)) | (p2$p.value<0.05/3/2 & (abs(diff2_chn)>=abs(diff2)*0.5)) | (p3$p.value<0.05/3/2 & (abs(diff3_chn)>=abs(diff3)*0.5)) |  (p1_1$p.value<0.05/3/2 & (abs(diff1_chn_1)>=abs(diff1_1)*0.5)) | (p2_1$p.value<0.05/3/2 & (abs(diff2_chn_1)>=abs(diff2_1)*0.5)) | (p3_1$p.value<0.05/3/2 & (abs(diff3_chn_1)>=abs(diff3_1)*0.5)) ){  #preserve 50% effect
    res2[i]=1
  }
  
  if((p1$p.value<0.05/3/2 & (diff1_chn>0)) | (p2$p.value<0.05/3/2 & (diff2_chn<0)) | (p3$p.value<0.05/3/2 & (diff3_chn<0)) |  (p1_1$p.value<0.05/3/2 & (diff1_chn_1>0)) | (p2_1$p.value<0.05/3/2 & (diff2_chn_1>0)) | (p3_1$p.value<0.05/3/2 & (diff3_chn_1>0)) ){  #preserve 50% effect
    res3[i]=1
  }
}

mean(res1)  #1
mean(res2)  #0.99186
mean(res3)  #0.99996
