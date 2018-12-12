# 1、formula准备------------------------------------------
data=read.csv('data\\treat_two_hundred.csv',header = T)
{
  A91='70-80-81|70-71-72|61-71-72|61-62-63|52-53-54|52-62-63|43-44-45|43-53-54|34-44-45|34-35-36|25-35-36|25-26-27|16-17-18|16-26-27'
  A92='07-08-18|07-17-27|16-17-27|16-26-36|25-35-45|25-26-36|34-44-54|34-35-45|43-44-54|43-53-63|52-53-63|52-62-72|61-71-81|61-62-72'
  A9=cbind(A91,A92)
  B91='70-80-90|70-71-81|61-71-81|61-62-73|52-53-63|52-62-72|43-44-54|43-53-63|34-44-54|34-35-45|25-35-45|25-26-36|16-17-27|16-26-36'
  B92='07-08-09|07-17-18|16-17-18|16-26-73|25-35-36|25-26-27|34-44-45|34-35-36|43-44-45|43-53-54|52-53-54|52-62-63|61-71-72|61-62-63'
  B9=cbind(B91,B92)  
  A71='50-60-70|50-51-61|41-42-52|41-51-61|32-33-43|32-42-52|23-33-43|23-24-34|14-24-34|14-15-25'
  A72='05-06-07|05-15-16|14-24-25|14-15-16|23-33-34|23-24-25|32-33-34|32-42-43|41-42-43|41-51-52'
  A7=cbind(A71,A72)
  B71='50-60-61|50-51-52|41-42-43|41-51-52|32-33-34|32-42-43|23-33-34|23-24-25|14-24-25|14-15-16'
  B72='05-06-16|05-15-25|14-24-34|14-15-25|23-33-43|23-24-34|32-33-43|32-42-52|41-42-52|41-51-61'
  B7=cbind(B71,B72)
  A51='30-40-41|30-31-32|21-31-32|21-22-23|12-22-23|12-13-14'
  A52='03-04-14|03-13-23|12-13-23|12-22-32|21-22-32|21-31-41'
  A5=cbind(A51,A52)
  # A52是对A51的完全相反
  B51='30-40-50|30-40-41|21-31-41|21-22-32|12-22-32|12-13-23'
  # B51是对A51的第三项相反（反一反）
  B52='03-04-05|03-13-14|12-13-14|12-22-23|21-22-23|21-31-32'
  # B52是对B51的完全相反，也是对A52的第三项相反（反一反）
  B5=cbind(B51,B52)
  path=matrix(c(A9,A7,A5, A9,B7,A5, A9,A7,B5, A9,B7,B5, B9,A7,A5, B9,B7,A5, B9,A7,B5, B9,B7,B5),byrow = T,ncol = 6)
  colnames(path)=c('B_R9','P_R9','B_R7','P_R7','B_R5','P_R5')
  minus_path=matrix(c(B9,B7,B5, B9,A7,B5, B9,B7,A5, B9,A7,A5, A9,B7,B5, A9,A7,B5, A9,B7,A5, A9,A7,A5),byrow = T,ncol = 6)
  colnames(minus_path)=c('B_R9','P_R9','B_R7','P_R7','B_R5','P_R5')
}
c=c('data','path','minus_path')
a=ls()
rm(list=(setdiff(a,c)))
gc()
if(!require(stringr))install.packages('stringr')
library(stringr)
# 2、训练----------------------------------------------

new_quantize=function(path,real_comb,set){
  n=length(set)-8
  # 每一个数据子集均要尝试8种的路径
  r9=matrix(rep(-1,nrow(path)*n),ncol = nrow(path),nrow = n)
  for (j in 1:nrow(path)) {
    for (i in 1:n) {
      r9[i,j]=ifelse(set[i]=='B',ifelse(str_detect(real_comb[i,'R9'],path[j,'B_R9']),1,-1),ifelse(str_detect(real_comb[i,'R9'],path[j,'P_R9']),1,-1))
      
    }
  }
  r7=matrix(rep(-1,nrow(path)*n),ncol = nrow(path),nrow = n)
  for (j in 1:nrow(path)) {
    for (i in 1:n) {
      r7[i,j]=ifelse(set[i+2]=='B',ifelse(str_detect(real_comb[i,'R7'],path[j,'B_R7']),1,-1),ifelse(str_detect(real_comb[i,'R7'],path[j,'P_R7']),1,-1))
      
    }
  }
  r5=matrix(rep(-1,nrow(path)*n),ncol = nrow(path),nrow = n)
  for (j in 1:nrow(path)) {
    for (i in 1:n) {
      r5[i,j]=ifelse(set[i+4]=='B',ifelse(str_detect(real_comb[i,'R5'],path[j,'B_R5']),1,-1),ifelse(str_detect(real_comb[i,'R5'],path[j,'P_R5']),1,-1))
      
    }
  }
  return(list(r9,r7,r5))
}

train=function(data=data,sumtemp=sumtemp){
  n=nrow(data)-8
  optimal=list()
  for (i in 1:8){
    for (l in 1:(n-5)){
      if(sumtemp[l,i]<0&sumtemp[l+1,i]<0&sumtemp[l+2,i]<0&sumtemp[l+3,i]<0&sumtemp[l+4,i]<0&sumtemp[l+5,i]<0) break
      else if(l==(n-5)) optimal=append(optimal,i)
    }
  }
  if(length(optimal)!=0){
    return(matrix(unlist(optimal)))
  }
  else{
    return(matrix(0))
  }
  
}

enen_change=function(quantity,minus_quantity,type=T){
  even_final_result=quantity
  for (j in 1:ncol(quantity)) {
    if(type==T){
      for (i in seq(2,n,2)) {
        even_final_result[i,j]=ifelse(quantity[i-1,j]>0,quantity[i,j],minus_quantity[i,j])
      }
    }
    else{
      for (i in seq(2,n,2)) {
        even_final_result[i,j]=ifelse(quantity[i-1,j]<0,quantity[i,j],minus_quantity[i,j])
      }
    }
  }
  return(even_final_result)
}
n=nrow(data)-8
run=function(data=data,type=T){
  
  lis=NULL# 存放结果
  for (m in 1:length(data)) {
    {       
      final_comb=matrix(ncol = 3,nrow = n)
      #求数据集真实的'9R','7R','5R'
      for (i in 1:n) {
        t0=table(data[i:(i+8),m])
        result0=paste0(t0[1],t0[2])
        t1=table(data[i:(i+7),m])
        result1=paste0(t1[1],t1[2])
        t2=table(data[i:(i+6),m])
        result2=paste0(t2[1],t2[2])
        comb1=paste0(result2,'-',result1,'-',result0)
        
        t3=table(data[(i+2):(i+8),m])
        result3=paste0(t3[1],t3[2])
        t4=table(data[(i+2):(i+7),m])
        result4=paste0(t4[1],t4[2])
        t5=table(data[(i+2):(i+6),m])
        result5=paste0(t5[1],t5[2])
        comb2=paste0(result5,'-',result4,'-',result3)
        
        t6=table(data[(i+4):(i+8),m])
        result6=paste0(t6[1],t6[2])
        t7=table(data[(i+4):(i+7),m])
        result7=paste0(t7[1],t7[2])
        t8=table(data[(i+4):(i+6),m])
        result8=paste0(t8[1],t8[2])
        comb3=paste0(result8,'-',result7,'-',result6)
        
        final_comb[i,]=matrix(c(comb1,comb2,comb3),ncol = 3)
      }
      colnames(final_comb)=c('R9','R7','R5')
    }
    
    R9_quantity=new_quantize(path = path,real_comb=final_comb,set=data[,m])[[1]]
    minus_R9_quantity=new_quantize(path =minus_path,real_comb=final_comb,set=data[,m])[[1]]
    R9_quantity[seq(3,n,4),]=minus_R9_quantity[seq(3,n,4),]
    
    R7_quantity=new_quantize(path = path,real_comb=final_comb,set=data[,m])[[2]]
    minus_R7_quantity=new_quantize(path =minus_path,real_comb=final_comb,set=data[,m])[[2]]
    R7_quantity[seq(3,n,4),]=minus_R7_quantity[seq(3,n,4),]
    
    R5_quantity=new_quantize(path = path,real_comb=final_comb,set=data[,m])[[3]]
    minus_R5_quantity=new_quantize(path =minus_path,real_comb=final_comb,set=data[,m])[[3]]
    R5_quantity[seq(3,n,4),]=minus_R5_quantity[seq(3,n,4),]
    
    orginal=R9_quantity+R7_quantity+R5_quantity
    new=minus_R9_quantity+minus_R7_quantity+minus_R5_quantity
    sumtemp=enen_change(orginal,new,type = type)
    sat=matrix(train(data = data,sumtemp =sumtemp))
    lis=c(lis,sat)
    if (m%%10==0) print(m)
    
  }
  # 交集
  {
    t=table(lis)
    #或者存放成list，然后table(unlist(m))
    order_optimal=sort(t,decreasing = T)
    #opt=order_optimal[which(order_optimal==order_optimal[1])]
    
  }
  return(order_optimal)
}

run(data=data,type=F)# 对方版本的
run(data=data,type=T)# 我的版本的

