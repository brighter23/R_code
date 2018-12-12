# 1、formula准备------------------------------------------
data=read.csv('data\\treat_two_hundred.csv',header = T)
#data=read.csv('data\\new.csv',header = T)

{
  A91='70-80-81|70-71-72|61-71-72|61-62-63|52-53-54|52-62-63|43-44-45|43-53-54|34-44-45|34-35-36|25-35-36|25-26-27|16-17-18|16-26-27'
  A92='07-08-18|07-17-27|16-17-27|16-26-36|25-35-45|25-26-36|34-44-54|34-35-45|43-44-54|43-53-63|52-53-63|52-62-72|61-71-81|61-62-72'
  A9=cbind(A91,A92)
  B91='70-80-90|70-71-81|61-71-81|61-62-72|52-53-63|52-62-72|43-44-54|43-53-63|34-44-54|34-35-45|25-35-45|25-26-36|16-17-27|16-26-36'
  B92='07-08-09|07-17-18|16-17-18|16-26-27|25-35-36|25-26-27|34-44-45|34-35-36|43-44-45|43-53-54|52-53-54|52-62-63|61-71-72|61-62-63'
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
  B51='30-40-50|30-31-41|21-31-41|21-22-32|12-22-32|12-13-23'
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


# 2、训练并求最优----------------------------------------------

# 注意：plan7修改了量化函数！！！！！！！
new_quantize=function(path=path,minus_path=minus_path,real_comb,set,vers=-1){
  n=length(set)-8
  # 每一个数据子集均要尝试8种的路径
  r9=matrix(rep(0,nrow(path)*n),ncol = nrow(path),nrow = n)
  for (j in 1:nrow(path)) {
    # 奇数行结果
    for (i in seq(1,n,2)) {
      r9[i,j]=ifelse(set[i]=='B',ifelse(str_detect(real_comb[i,'R9'],path[j,'B_R9']),1,-1),ifelse(str_detect(real_comb[i,'R9'],path[j,'P_R9']),1,-1))
      
    }
    # 偶数行
    for (i in seq(2,n,2)) {
      # 在-1（我的版本）或者1（对方版本）下"反一反"
      # 奇数行为-1则反一下formula
      if (r9[i-1,j]==vers){
        r9[i,j]=ifelse(set[i]=='B',ifelse(str_detect(real_comb[i,'R9'],minus_path[j,'B_R9']),1,-1),ifelse(str_detect(real_comb[i,'R9'],minus_path[j,'P_R9']),1,-1))
      }
      else{r9[i,j]=ifelse(set[i]=='B',ifelse(str_detect(real_comb[i,'R9'],path[j,'B_R9']),1,-1),ifelse(str_detect(real_comb[i,'R9'],path[j,'P_R9']),1,-1))
}
      
      
    }
  }
  r7=matrix(rep(0,nrow(path)*n),ncol = nrow(path),nrow = n)
  for (j in 1:nrow(path)) {
    # 奇数行结果
    for (i in seq(1,n,2)) {
      r7[i,j]=ifelse(set[i+2]=='B',ifelse(str_detect(real_comb[i,'R7'],path[j,'B_R7']),1,-1),ifelse(str_detect(real_comb[i,'R7'],path[j,'P_R7']),1,-1))
      
    }
    # 偶数行
    for (i in seq(2,n,2)) {
      # 在-1（我的版本）或者1（对方版本）下"反一反"
      # 奇数行为-1则反一下formula
      if (r7[i-1,j]==vers){
        r7[i,j]=ifelse(set[i+2]=='B',ifelse(str_detect(real_comb[i,'R7'],minus_path[j,'B_R7']),1,-1),ifelse(str_detect(real_comb[i,'R7'],minus_path[j,'P_R7']),1,-1))
      }
      else{r7[i,j]=ifelse(set[i+2]=='B',ifelse(str_detect(real_comb[i,'R7'],path[j,'B_R7']),1,-1),ifelse(str_detect(real_comb[i,'R7'],path[j,'P_R7']),1,-1))
      }
      
      
    }
  }
  r5=matrix(rep(0,nrow(path)*n),ncol = nrow(path),nrow = n)
  for (j in 1:nrow(path)) {
    # 奇数行结果
    for (i in seq(1,n,2)) {
      r5[i,j]=ifelse(set[i+4]=='B',ifelse(str_detect(real_comb[i,'R5'],path[j,'B_R5']),1,-1),ifelse(str_detect(real_comb[i,'R5'],path[j,'P_R5']),1,-1))
      
    }
    # 偶数行
    for (i in seq(2,n,2)) {
      # 在-1（我的版本）或者1（对方版本）下"反一反"
      # 奇数行为-1则反一下formula
      if (r7[i-1,j]==vers){
        r5[i,j]=ifelse(set[i+4]=='B',ifelse(str_detect(real_comb[i,'R5'],minus_path[j,'B_R5']),1,-1),ifelse(str_detect(real_comb[i,'R5'],minus_path[j,'P_R5']),1,-1))
      }
      else{r5[i,j]=ifelse(set[i+4]=='B',ifelse(str_detect(real_comb[i,'R5'],path[j,'B_R5']),1,-1),ifelse(str_detect(real_comb[i,'R5'],path[j,'P_R5']),1,-1))
      }
      
      
    }
  }
  return(list(r9,r7,r5))
}
train=function(data=data,R9_final_quantity=R9_final_quantity,R7_final_quantity=R7_final_quantity,R5_final_quantity=R5_final_quantity){
  n=nrow(data)-8
  optimal=list()#用来存放9R，7R，5R对应的列数
  sumtemp=R9_final_quantity+R7_final_quantity+R5_final_quantity
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
enen_change=function(quantity,minus_quantity){
  even_final_result=quantity
  for (j in 1:ncol(quantity)) {
    for (i in seq(2,n,2)) {# 改变偶数行结果
      even_final_result[i,j]=ifelse(quantity[i-1,j]==1,quantity[i,j],minus_quantity[i,j])
    }
  }
  
  return(even_final_result)
}
n=nrow(data)-8 #最后final result的行数
# 封装成函数
run=function(seq){
  # 2、训练
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
    #seq=ifelse(rbinom(3,1,0.5)==0,-1,1)#随机产生也不太好
    R9_quantity=new_quantize(path = path,minus_path,real_comb=final_comb,set=data[,m],vers=seq[1])[[1]]
    #minus_R9_quantity=new_quantize(path =minus_path,real_comb=final_comb,set=data[,m],vers=-1)[[1]]
    
    
    R7_quantity=new_quantize(path = path,minus_path,real_comb=final_comb,set=data[,m],vers=seq[2])[[2]]
    #minus_R7_quantity=new_quantize(path =minus_path,real_comb=final_comb,set=data[,m],vers=-1)[[2]]
    
    R5_quantity=new_quantize(path = path,minus_path,real_comb=final_comb,set=data[,m],vers=seq[3])[[3]]
    #minus_R5_quantity=new_quantize(path =minus_path,real_comb=final_comb,set=data[,m],vers=-1)[[3]]
    
    
    #R9_final_quantity=enen_change(R9_quantity,minus_R9_quantity)
    #R7_final_quantity=enen_change(R7_quantity,minus_R7_quantity)
    #R5_final_quantity=enen_change(R5_quantity,minus_R5_quantity)
    sat=matrix(train(data = data ,R9_final_quantity =R9_quantity,R7_final_quantity = R7_quantity,R5_final_quantity = R5_quantity ))
    lis=c(lis,sat)
    #assign(paste0('optimal',m),matrix(train(data = data ,R9_final_quantity =R9_quantity,R7_final_quantity = R7_quantity,R5_final_quantity = R5_quantity )))
    if (m%%10==0) print(m)
    
  }
  # 3、交集----------------------------------------------
  {
    t=table(lis)
    #或者存放成list，然后table(unlist(m))
    order_optimal=sort(t,decreasing = T)
    opt=order_optimal[which(order_optimal==order_optimal[1])]
    
  }
  return(order_optimal)
}
run(seq = c(-1,-1,-1))


# 3、预测---------------------------------------------
# 法1：还会得到不满足的数据集的final_result
pred_unsatisfy7=function(index,data=data,path=path,minus_path=minus_path,seq){
  n=nrow(data)-8
  final_sum_5=matrix(nrow = n)
  final_sum_6=matrix(nrow = n)
  final_sum_7=matrix(nrow = n)
  final_sum_8=matrix(nrow = n)
  final_sum_9=matrix(nrow = n)
  final_sum_10=matrix(nrow = n)
  final_sum_11=matrix(nrow = n)
  final_sum_12=matrix(nrow = n)
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
    
    R9_quantity=new_quantize(path = path[index,,drop=F],minus_path=minus_path[index,,drop=F],real_comb=final_comb,set=data[,m],vers=seq[1])[[1]]

    R7_quantity=new_quantize(path = path[index,,drop=F],minus_path=minus_path[index,,drop=F],real_comb=final_comb,set=data[,m],vers=seq[2])[[2]]

    R5_quantity=new_quantize(path = path[index,,drop=F],minus_path=minus_path[index,,drop=F],real_comb=final_comb,set=data[,m],vers=seq[3])[[3]]
    
    sumtemp=R9_quantity+R7_quantity+ R5_quantity
    
    sumtemp=as.matrix(sumtemp)
    # 大于等于5的数据个数
    for (l in 1:(n-4)){
      if(sumtemp[l]<0&sumtemp[l+1]<0&sumtemp[l+2]<0&sumtemp[l+3]<0&sumtemp[l+4]<0){
        colnames(sumtemp)=c(paste0('data',m))
        final_sum_5=cbind(final_sum_5,sumtemp)
        break
      }
    }
    # 大于等于6的数据个数
    for (l in 1:(n-5)){
      if(sumtemp[l]<0&sumtemp[l+1]<0&sumtemp[l+2]<0&sumtemp[l+3]<0&sumtemp[l+4]<0&sumtemp[l+5]<0){
        colnames(sumtemp)=c(paste0('data',m))
        final_sum_6=cbind(final_sum_6,sumtemp)
        break
      }
    }
    
    for (l in 1:(n-6)){
      if(sumtemp[l]<0&sumtemp[l+1]<0&sumtemp[l+2]<0&sumtemp[l+3]<0&sumtemp[l+4]<0&sumtemp[l+5]<0&sumtemp[l+6]<0){
        colnames(sumtemp)=c(paste0('data',m))
        final_sum_7=cbind(final_sum_7,sumtemp)
        break
      }
    }
    
    for (l in 1:(n-7)){
      if(sumtemp[l]<0&sumtemp[l+1]<0&sumtemp[l+2]<0&sumtemp[l+3]<0&sumtemp[l+4]<0&sumtemp[l+5]<0&sumtemp[l+6]<0&sumtemp[l+7]<0){
        colnames(sumtemp)=c(paste0('data',m))
        final_sum_8=cbind(final_sum_8,sumtemp)
        break
      }
    }
    
    for (l in 1:(n-8)){
      if(sumtemp[l]<0&sumtemp[l+1]<0&sumtemp[l+2]<0&sumtemp[l+3]<0&sumtemp[l+4]<0&sumtemp[l+5]<0&sumtemp[l+6]<0&sumtemp[l+7]<0&sumtemp[l+8]<0){
        colnames(sumtemp)=c(paste0('data',m))
        final_sum_9=cbind(final_sum_9,sumtemp)
        break
      }
    }
    # 大于等于10的数据个数
    for (l in 1:(n-9)){
      if(sumtemp[l]<0&sumtemp[l+1]<0&sumtemp[l+2]<0&sumtemp[l+3]<0&sumtemp[l+4]<0&sumtemp[l+5]<0&sumtemp[l+6]<0&sumtemp[l+7]<0&sumtemp[l+8]<0&sumtemp[l+9]<0){
        colnames(sumtemp)=c(paste0('data',m))
        final_sum_10=cbind(final_sum_10,sumtemp)
        break
      }
    }
    # 大于等于11的数据个数
    for (l in 1:(n-10)){
      if(sumtemp[l]<0&sumtemp[l+1]<0&sumtemp[l+2]<0&sumtemp[l+3]<0&sumtemp[l+4]<0&sumtemp[l+5]<0&sumtemp[l+6]<0&sumtemp[l+7]<0&sumtemp[l+8]<0&sumtemp[l+9]<0&sumtemp[l+10]<0){
        colnames(sumtemp)=c(paste0('data',m))
        final_sum_11=cbind(final_sum_11,sumtemp)
        break
      }
    }
    # 大于等于12的数据个数
    for (l in 1:(n-11)){
      if(sumtemp[l]<0&sumtemp[l+1]<0&sumtemp[l+2]<0&sumtemp[l+3]<0&sumtemp[l+4]<0&sumtemp[l+5]<0&sumtemp[l+6]<0&sumtemp[l+7]<0&sumtemp[l+8]<0&sumtemp[l+9]<0&sumtemp[l+10]<0&sumtemp[l+11]<0){
        colnames(sumtemp)=c(paste0('data',m))
        final_sum_12=cbind(final_sum_12,sumtemp)
        break
      }
    }
  }
  return(list(final_sum_5[,-1,drop=F],final_sum_6[,-1,drop=F],final_sum_7[,-1,drop=F],final_sum_8[,-1,drop=F],final_sum_9[,-1,drop=F],final_sum_10[,-1,drop=F],final_sum_11[,-1,drop=F],final_sum_12[,-1,drop=F]))#drop=F，避免因为只有一个数据时自动变成向量而失去列名
}

error1=pred_unsatisfy7(8,data,path = path,minus_path = minus_path,seq = c(-1,-1,-1))
error2=pred_unsatisfy7(3,data,path = path,minus_path = minus_path,seq = c(-1,-1,-1))
dis_unsatisfy=function(formular_final_result){
  for (k in 2:8){
    num=ncol(formular_final_result[[k-1]])-ncol(formular_final_result[[k]])
    print(paste0('连续负的个数为',k+3,'的样本个数：',num))
  }
  num=ncol(formular_final_result[[8]])
  print(paste0('连续负的个数为超过12个的样本个数：',num))
}
dis_unsatisfy(error1)
dis_unsatisfy(error2)

# 法2 ：

pred_unsatisfy_num=function(index,data=data,path=path,minus_path=minus_path,seq){
  n=nrow(data)-8
  continue_minus_num=c()
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
    
    R9_quantity=new_quantize(path = path[index,,drop=F],minus_path=minus_path[index,,drop=F],real_comb=final_comb,set=data[,m],vers=seq[1])[[1]]
    
    R7_quantity=new_quantize(path = path[index,,drop=F],minus_path=minus_path[index,,drop=F],real_comb=final_comb,set=data[,m],vers=seq[2])[[2]]
    
    R5_quantity=new_quantize(path = path[index,,drop=F],minus_path=minus_path[index,,drop=F],real_comb=final_comb,set=data[,m],vers=seq[3])[[3]]
    
    sumtemp=R9_quantity+R7_quantity+ R5_quantity
    
    # 转化成字符串，查看”00000“这部分是否在字符串内
    strings=str_c(ifelse(sumtemp<0,0,1),collapse = '')
    for (k in 1:nrow(data)) {
      pattern=str_c(paste0(rep(0,k)),collapse = '')
      if(!str_detect(strings,pattern)){
        #print(paste0('最大连续为负个数',k-1))
        continue_minus_num[m]=k-1
        break
      }
    }
    
  }
  return(continue_minus_num)
}

unsatisfy_num=pred_unsatisfy_num(index=8,data,path = path,minus_path = minus_path,seq = c(-1,-1,-1))
new_dis_unsatisfy=function(unsatisfy_num){
  k=unsatisfy_num[which.max(unsatisfy_num)]
  for(i in 4:k){
    print(paste0('连续负为',i,'的数据集个数：',length(which(unsatisfy_num==i))))
  }
}
new_dis_unsatisfy(unsatisfy_num)


