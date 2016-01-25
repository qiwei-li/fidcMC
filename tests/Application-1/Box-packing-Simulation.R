isGoodNumber<-function(X,n){
  ifelse(X==n, TRUE, FALSE)
}

stat<-function(X,n){
	t=list()
	len=length(X)
	for (i in 1:n){
		t[i]=length(X[isGoodNumber(X,i)])/len
	}
	return(t)
}
stat1<-function(X,n){
	t=rep(1,n)
	X=X[!is.na(X)]
	len=length(X)
	for (i in 1:n){
		t[i]=length(X[isGoodNumber(X,i)])/len
	}
	return(t)
}

Item_Box_Simulation<-function(n){ 
	c=1/(sum(1:10)/10)
	p=0.1*c*(1:10)
	wt_max=10

	itm_wt=sample(x=1:10,size=1,prob=p)
	bx_cnt=c(1,rep(NA,n-1))
	bx_crnt_wt=c(itm_wt,rep(0,n-1))
	bx_fnl_wt=rep(NA,n)
	bx_itm_num=rep(NA,n)
	frst_itm_wt=c(itm_wt,rep(NA,n))
	count=2
	itm_num=1
	while(count<=n){
		itm_wt=sample(x=1:10,size=1,prob=p)
		if ((itm_wt+bx_crnt_wt[count-1])>wt_max){
			bx_cnt[count]=1
			bx_crnt_wt[count]=itm_wt
			bx_fnl_wt[count-1]=bx_crnt_wt[count-1]
			bx_itm_num[count-1]=itm_num
			itm_num=1
			frst_itm_wt[count]=itm_wt
		}
		else{
			bx_crnt_wt[count]=itm_wt+bx_crnt_wt[count-1]
			itm_num=itm_num+1
		}
		count=count+1
	}
	cwt_sta_dis=stat(bx_crnt_wt,10)
	fwd_sta_dis=stat1(bx_fnl_wt,10)
	fiw_sta_dis=stat1(frst_itm_wt,10)
	bx_fnl_wt_av=mean(bx_fnl_wt,na.rm=T)
	bx_itm_num_av=mean(bx_itm_num,na.rm=T)
	frst_itm_wt_av=mean(frst_itm_wt,na.rm=T)

	#t<-list('current_weight_distribution'=cwt_sta_dis,'final_weight_distribution'=fwd_sta_dis,'First_Item_Weight'=fiw_sta_dis)
	t<-list('Box_final_Weight'=bx_fnl_wt_av,'Box_Item_Number'=bx_itm_num_av,'First_Item_Weight_Probobaility'=fiw_sta_dis)
	#t=list()
	#t['current_weight_distribution']=cwt_sta_dis
	#t['final_weight_distribution']=fwd_sta_dis
	#return(list(Box_Weight_Average=bx_fnl_wt_av,Box_Item_Number=bx_itm_num_av,First_Item_Weight=frst_itm_wt_av))
	return(t)
}
