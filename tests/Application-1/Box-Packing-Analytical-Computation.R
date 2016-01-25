############ ITEM WEIGHT & PROBABILITY ############
I<-c(1:10)/10*10/55
SUM<-sum(I)
#I<-c(1:10)
W_I<-c(1:10)*1.0
W_I_av<-sum(I*W_I)

################ TRANSITION MATRIX ################
pijdef<-function(I){
	n<-length(I)
	p<-diag(0,n)
	for (i in 1:n){
		for (j in 1:n){
			if (i>j){
				if ((i+j)>n) p[i,j]<-0+I[j]
				else p[i,j]<-0
			}
			if (i<j){
				if ((i+j)>n) p[i,j]<-I[j-i]+I[j]
				else p[i,j]<-I[j-i]
			}
			if (i==j & (i+j)>n) p[i,j]<-I[j]
		}
	}
	return(p)
}

P<-pijdef(I)

#################   MARKOV CHAIN  #################
fsd<-function(p) {
	n<-nrow(p)
	nwmtrx<-diag(n)-t(p)
	nwmtrx[n,]<-rep(1,n)
	rt<-c(rep(0,n-1),1)
	sol<-solve(nwmtrx,rt)
	return(sol)
}

PI<-fsd(P)

############# BOX WEIGHT & PROBABILITY ############
###### BOX PROBABILITY ######
P_box<-function(I,PI){
	n<-length(I)
	pbox=rep(1,length(I))
	for (i in 1:n){
		tot<-sum(I[(n-i+1):n])
		pbox[i]<-PI[i]*tot
	}
	return(pbox)
}

pbox<-P_box(I,PI)
pbox<-pbox/sum(pbox)

######## BOX WEIGHT ########
W_B<-c(1:length(I))*1.0
Box_Final_Weight_Average<-sum(pbox*W_B)

####### BOX ITEM NUM #######
ITEM_NUM<-Box_Final_Weight_Average/W_I_av

###### FIRST ITEM NUM ######
P_first_weight_item<-function(PI,I){
	n<-length(I)
	pfirstitem<-rep(1,n)
	for (i in 1:n){
		tot1<-0
		for (j in (n-i+1):n){
			tot1<-tot1+PI[j]*I[i]		
		}
		pfirstitem[i]<-tot1
	}
	return(pfirstitem)
}

first_item_probability<-P_first_weight_item(PI,I)
first_item_probability<-first_item_probability/sum(first_item_probability)
names(first_item_probability)<-c(1:length(I))
Weight_First_Item_average<-sum(first_item_probability*W_I)
