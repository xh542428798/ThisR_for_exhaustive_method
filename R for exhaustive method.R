


#Setting variables to be exhaustive
if(FALSE){
  
  #setting
  if(TRUE){
    ebvdb3m$tryweight=round(ebvdb3m$DOI)# the name of dataset is ebvdb3
    allvalues=sort(unique(ebvdb3m[,"tryweight"]))
    length(allvalues) 
    allvalues #display
    
    #setting pvalue
    set_pvalue01=0.05 
    set_pvalue12=0.05
    
    #setting prognosis
    ebvdb3m$msurv=Surv( time=as.double(ebvdb3m[,"deadtime"]),event=as.double(ebvdb3m[,"dead"]) )
  }
  
  #running
  if(TRUE){
    firstcut=0
    secondcut=0
    
    logstr=as.formula(paste0("msurv ~","tryweightcut"))
    thisresult=c()
    for(i in seq( 1,length(allvalues)-2,1 ) ){ #Starting from the first one, the first cut value should be exhausted to the penultimate one.
      j=i+1
      for(j in seq(j,length(allvalues)-1,1 ) ){#scanning
        firstcut=allvalues[i]
        secondcut=allvalues[j]
        ebvdb3m[,"tryweightcut"]=as.factor(ifelse(ebvdb3m[,"tryweight"]<=firstcut,0,ifelse(ebvdb3m[,"tryweight"]<=secondcut,1,2)));
        
        #if the process is wrong,print here
        # print(c(firstcut,secondcut))
        # print(table(ebvdb3m[,"tryweightcut"]))
        
        tempprint=survdiff(logstr,data=ebvdb3m[which(ebvdb3m[,"tryweightcut"]==0 | ebvdb3m[,"tryweightcut"]==1),]);
        pvalue01 <- 1 - pchisq(tempprint$chisq, length(tempprint$n) - 1)
        # print(pvalue01);
        # if(pvalue01<set_pvalue01){
        tempprint=survdiff(logstr,data=ebvdb3m[which(ebvdb3m[,"tryweightcut"]==1 | ebvdb3m[,"tryweightcut"]==2),]);
        pvalue12 <- 1 - pchisq(tempprint$chisq, length(tempprint$n) - 1)
        # print(pvalue12);
        # if(pvalue12<set_pvalue12){
        
        thisy=coxP("tryweightcut+age+sex+betel_liquid+pNstage+extranodal_extension",data=ebvdb3m,ireturn = "summary")
        multi_p1=as.numeric(thisy$coefficients[1,5])
        multi_p2=as.numeric(thisy$coefficients[2,5])
        
        thisy=coxP("tryweightcut+age+sex+betel_liquid+pNstage+extranodal_extension",data=ebvdb3m[which(ebvdb3m[,"tryweightcut"]!=0),],ireturn = "summary")
        multi_p23=as.numeric(thisy$coefficients[1,5])
        # multi_p2=as.numeric(thisy$coefficients[2,5])
        
        thisresult=rbind(thisresult,c(firstcut,secondcut,length(which(ebvdb3m[,"tryweightcut"]==0)),
                                      length(which(ebvdb3m[,"tryweightcut"]==1)),
                                      length(which(ebvdb3m[,"tryweightcut"]==2)),
                                      pvalue01,pvalue12,multi_p1,multi_p2,multi_p23) )
        
        
        
        # }
        
        # }
        
        
        #用于调试
        # break;
        # print(c(i,j))
        print(c(firstcut,secondcut))
      }
      # print(c(i,j))
      # print(thisresult)
    }
    #detail
    thisresult=as.data.frame(thisresult)
    
    thisresult$V3=as.integer(as.character(thisresult$V3))
    thisresult$V4=as.integer(as.character(thisresult$V4))
    thisresult$V5=as.integer(as.character(thisresult$V5))
    thisresult$V6=as.numeric(as.character(thisresult$V6))
    thisresult$V7=as.numeric(as.character(thisresult$V7))
    thisresult$V8=as.numeric(as.character(thisresult$V8))
    thisresult$V9=as.numeric(as.character(thisresult$V9))
    thisresult$V10=as.numeric(as.character(thisresult$V10))
    
    #sample size balance with R2 assessment
    thismean=mean(thisresult$V3+thisresult$V4+thisresult$V5)
    thisresult$myr2= (thisresult$V3-thismean)^2 + (thisresult$V4-thismean)^2 + (thisresult$V5-thismean)^2
    
    thisresult2=thisresult[which(thisresult$V6<0.05 & thisresult$V7<0.05),]
  }
  
  
  if(FALSE){
    write.csv(thisresult,file="./thisresult.csv")
  }
}