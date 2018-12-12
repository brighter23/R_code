# 1、formula准备------------------------------------------
data=read.csv('data\\treat_two_hundred.csv',header = T)
data1=read.csv('data\\new.csv',header = T)
data1=data1[,-23]
data378=cbind(data,data1)
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
c=c('data','data1','data378','path','minus_path')
a=ls()
rm(list=(setdiff(a,c)))
gc()
if(!require(stringr))install.packages('stringr')
library(stringr)
# 2、训练----------------------------------------------

new_quantize=function(path,real_comb,set){
  n=length(set)-8
  # 每一个数据子集均要尝试8种的路径
  r9=matrix(rep(0,nrow(path)*n),ncol = nrow(path),nrow = n)
  for (j in 1:nrow(path)) {
    for (i in 1:n) {
      r9[i,j]=ifelse(set[i]=='B',ifelse(str_detect(real_comb[i,'R9'],path[j,'B_R9']),1,-1),ifelse(str_detect(real_comb[i,'R9'],path[j,'P_R9']),1,-1))
      
    }
  }
  r7=matrix(rep(0,nrow(path)*n),ncol = nrow(path),nrow = n)
  for (j in 1:nrow(path)) {
    for (i in 1:n) {
      r7[i,j]=ifelse(set[i+2]=='B',ifelse(str_detect(real_comb[i,'R7'],path[j,'B_R7']),1,-1),ifelse(str_detect(real_comb[i,'R7'],path[j,'P_R7']),1,-1))
      
    }
  }
  r5=matrix(rep(0,nrow(path)*n),ncol = nrow(path),nrow = n)
  for (j in 1:nrow(path)) {
    for (i in 1:n) {
      r5[i,j]=ifelse(set[i+4]=='B',ifelse(str_detect(real_comb[i,'R5'],path[j,'B_R5']),1,-1),ifelse(str_detect(real_comb[i,'R5'],path[j,'P_R5']),1,-1))
      
    }
  }
  return(list(r9,r7,r5))
}


train=function(sumtemp){
  n=nrow(data)
  optimal=list()#用来存放9R，7R，5R对应的列数
  for (i in 1:8){
    for (l in 1:(n-21)){
      if(sumtemp[l,i]<0&sumtemp[l+1,i]<0&sumtemp[l+2,i]<0&sumtemp[l+3,i]<0&sumtemp[l+4,i]<0&sumtemp[l+5,i]<0) break
      else if(l==(n-21)) optimal=append(optimal,i)
    }
  }
  if(length(optimal)!=0){
    return(matrix(unlist(optimal)))
  }
  else{
    return(matrix(0))
  }
  
}

# 对奇数行加入随机性
run=function(data=data,path=path,minus_path=minus_path,seq){
  #注意这里的n
  n=nrow(data)
  odd_data=data[seq(1,n,2),]
  even_data=data[seq(2,n,2),]
  lis=NULL# 存放结果
  for (m in 1:length(data)) {
    # 产生奇数行和偶数行fianl_comb
    { odd_n=nrow(odd_data)-8     
    odd_final_comb=matrix(ncol = 3,nrow = odd_n)
    for (i in 1:odd_n) {
      t0=table(odd_data[i:(i+8),m])
      result0=paste0(t0[1],t0[2])
      t1=table(odd_data[i:(i+7),m])
      result1=paste0(t1[1],t1[2])
      t2=table(odd_data[i:(i+6),m])
      result2=paste0(t2[1],t2[2])
      comb1=paste0(result2,'-',result1,'-',result0)
      
      t3=table(odd_data[(i+2):(i+8),m])
      result3=paste0(t3[1],t3[2])
      t4=table(odd_data[(i+2):(i+7),m])
      result4=paste0(t4[1],t4[2])
      t5=table(odd_data[(i+2):(i+6),m])
      result5=paste0(t5[1],t5[2])
      comb2=paste0(result5,'-',result4,'-',result3)
      
      t6=table(odd_data[(i+4):(i+8),m])
      result6=paste0(t6[1],t6[2])
      t7=table(odd_data[(i+4):(i+7),m])
      result7=paste0(t7[1],t7[2])
      t8=table(odd_data[(i+4):(i+6),m])
      result8=paste0(t8[1],t8[2])
      comb3=paste0(result8,'-',result7,'-',result6)
      
      odd_final_comb[i,]=matrix(c(comb1,comb2,comb3),ncol = 3)
    }
    colnames(odd_final_comb)=c('R9','R7','R5')
    }
    # 偶数
    { even_n=nrow(even_data)-8     
      even_final_comb=matrix(ncol = 3,nrow = even_n)
      #求'9R','7R','5R'
      for (i in 1:odd_n) {
        t0=table(even_data[i:(i+8),m])
        result0=paste0(t0[1],t0[2])
        t1=table(even_data[i:(i+7),m])
        result1=paste0(t1[1],t1[2])
        t2=table(even_data[i:(i+6),m])
        result2=paste0(t2[1],t2[2])
        comb1=paste0(result2,'-',result1,'-',result0)
        
        t3=table(even_data[(i+2):(i+8),m])
        result3=paste0(t3[1],t3[2])
        t4=table(even_data[(i+2):(i+7),m])
        result4=paste0(t4[1],t4[2])
        t5=table(even_data[(i+2):(i+6),m])
        result5=paste0(t5[1],t5[2])
        comb2=paste0(result5,'-',result4,'-',result3)
        
        t6=table(even_data[(i+4):(i+8),m])
        result6=paste0(t6[1],t6[2])
        t7=table(even_data[(i+4):(i+7),m])
        result7=paste0(t7[1],t7[2])
        t8=table(even_data[(i+4):(i+6),m])
        result8=paste0(t8[1],t8[2])
        comb3=paste0(result8,'-',result7,'-',result6)
        
        even_final_comb[i,]=matrix(c(comb1,comb2,comb3),ncol = 3)
      }
      colnames(even_final_comb)=c('R9','R7','R5')
    }
    # 计算量化后结果
    {
      #奇数行不用反，固定使用A
      odd=new_quantize(path = path,real_comb=odd_final_comb,set=odd_data[,m])
      odd_minus=new_quantize(path = minus_path,real_comb=odd_final_comb,set=odd_data[,m])
      even=new_quantize(path = path,real_comb=even_final_comb,set=even_data[,m])
      even_minus=new_quantize(path = minus_path,real_comb=even_final_comb,set=even_data[,m])
      
      odd_R9_quantity=odd[[1]]
      odd_minus_R9_quantity=odd_minus[[1]]
      even_R9_quantity=even[[1]]
      even_minus_R9_quantity=even_minus[[1]]
      
      odd_R7_quantity=odd[[2]]
      odd_minus_R7_quantity=odd_minus[[2]]
      even_R7_quantity=even[[2]]
      even_minus_R7_quantity=even_minus[[2]]
      
      odd_R5_quantity=odd[[3]]
      odd_minus_R5_quantity=odd_minus[[3]]
      even_R5_quantity=even[[3]]
      even_minus_R5_quantity=even_minus[[3]]
      
    }
    
    # 1：奇数轨奇数行；2：偶数轨偶数行；3：奇数轨偶数行；4：偶数轨偶数行
    odd_sumtempt=odd_R9_quantity+odd_R7_quantity+odd_R5_quantity
    odd_minus_sumtempt=odd_minus_R9_quantity+odd_minus_R7_quantity+odd_minus_R5_quantity
    even_sumtempt=even_R9_quantity+even_R7_quantity+even_R5_quantity
    even_minus_sumtempt=even_minus_R9_quantity+even_minus_R7_quantity+even_minus_R5_quantity
    
    
    # 1、奇数轨的奇数行必定是使用原始的formula产生的：
    odd_sumtempt_odd_row=odd_sumtempt[seq(1,odd_n,2),,drop=FALSE]
    # 2、偶数轨的奇数行
    if (seq[1]==0){
      # 保持不变
      even_sumtempt_odd_row=even_sumtempt[seq(1,even_n,2),,drop=FALSE]  
    }
    else if(seq[1]==1){
      # 根据奇数轨的奇数行决定
      even_sumtempt_odd_row=ifelse(odd_sumtempt_odd_row<0,even_sumtempt[seq(1,even_n,2),,drop=FALSE],even_minus_sumtempt[seq(1,even_n,2),,drop=FALSE])
    }
    else{
      print('seq[1] must be 0 or 1')
      break
    }
    # 2、奇数轨的偶数行
    if (seq[2]==0){
      # 保持不变
      odd_sumtempt_even_row=odd_sumtempt[seq(2,odd_n,2),,drop=FALSE]  
    }
    else if(seq[2]==1){
      # 根据奇数轨的奇数行决定,注意这里奇数行与偶数行一定要一样多
      odd_sumtempt_even_row=ifelse(odd_sumtempt_odd_row<0,odd_sumtempt[seq(2,odd_n,2),,drop=FALSE],odd_minus_sumtempt[seq(2,odd_n,2),,drop=FALSE])
    }
    else if(seq[2]==2){
      # 根据偶数轨的奇数行
      odd_sumtempt_even_row=ifelse(even_sumtempt_odd_row<0,odd_sumtempt[seq(2,odd_n,2),,drop=FALSE],odd_minus_sumtempt[seq(2,odd_n,2),,drop=FALSE])
    }
    else{
      print('seq[2] must be 0,1 or 2')
      break
    }
    # 3、偶数轨的偶数行
    if (seq[3]==0){
      # 保持不变
      even_sumtempt_even_row=even_sumtempt[seq(2,even_n,2),,drop=FALSE]  
    }
    else if(seq[3]==1){
      # 根据奇数轨的奇数行决定
      even_sumtempt_even_row=ifelse(odd_sumtempt_odd_row<0,even_sumtempt[seq(2,even_n,2),,drop=FALSE],even_minus_sumtempt[seq(2,even_n,2),,drop=FALSE])
    }
    else if(seq[3]==2){
      # 根据偶数轨的奇数行
      even_sumtempt_even_row=ifelse(even_sumtempt_odd_row<0,even_sumtempt[seq(2,even_n,2),,drop=FALSE],even_minus_sumtempt[seq(2,even_n,2),,drop=FALSE])
    }
    else if(seq[3]==3){
      # 根据奇数轨的偶数行
      even_sumtempt_even_row=ifelse(odd_sumtempt_even_row<0,even_sumtempt[seq(2,even_n,2),,drop=FALSE],even_minus_sumtempt[seq(2,even_n,2),,drop=FALSE])
    }
    else{
      print('seq[3] must be 0,1,2 or 3')
      break
    }
    
    
    # 最后合并成final_result
    sumtemp=matrix(nrow = n-16,ncol =8)
    sumtemp[seq(1,n-16,4),]=odd_sumtempt_odd_row
    sumtemp[seq(2,n-16,4),]=even_sumtempt_odd_row
    sumtemp[seq(3,n-16,4),]=odd_sumtempt_even_row
    sumtemp[seq(4,n-16,4),]=even_sumtempt_even_row
    sat=matrix(train(sumtemp =sumtemp))
    lis=c(lis,sat)
    if (m%%10==0) print(m)
  }
  # 交集
  t=table(lis)
  order_optimal=sort(t,decreasing = T)
  return(order_optimal)
}


# plan11,偶数轨偶数行根据偶数轨的奇数行对冲（与最初始的偶数轨偶数行）
run(data=data378,path=path,minus_path=minus_path,seq = c(1,1,2))
# 最优7：138+139=277

# plan 13
run(data=data378,path=path,minus_path=minus_path,seq = c(0,1,2))
# plan 14
run(data=data378,path=path,minus_path=minus_path,seq = c(1,2,1))
# plan 15
run(data=data378,path=path,minus_path=minus_path,seq = c(1,2,3))
# plan 16
run(data=data378,path=path,minus_path=minus_path,seq = c(0,2,1))

run(data=data378,path=path,minus_path=minus_path,seq = c(1,2,3))





# 3、预测---------------------------------------------
pred_unsatisfy_num=function(index,data=data,path=path,minus_path=minus_path,seq=c(1,1,2)){
  continue_minus_num=c()
  #注意这里的n
  n=nrow(data)
  odd_data=data[seq(1,n,2),]
  even_data=data[seq(2,n,2),]
  for (m in 1:length(data)) {
    # 产生奇数行和偶数行fianl_comb
    { odd_n=nrow(odd_data)-8     
    odd_final_comb=matrix(ncol = 3,nrow = odd_n)
    for (i in 1:odd_n) {
      t0=table(odd_data[i:(i+8),m])
      result0=paste0(t0[1],t0[2])
      t1=table(odd_data[i:(i+7),m])
      result1=paste0(t1[1],t1[2])
      t2=table(odd_data[i:(i+6),m])
      result2=paste0(t2[1],t2[2])
      comb1=paste0(result2,'-',result1,'-',result0)
      
      t3=table(odd_data[(i+2):(i+8),m])
      result3=paste0(t3[1],t3[2])
      t4=table(odd_data[(i+2):(i+7),m])
      result4=paste0(t4[1],t4[2])
      t5=table(odd_data[(i+2):(i+6),m])
      result5=paste0(t5[1],t5[2])
      comb2=paste0(result5,'-',result4,'-',result3)
      
      t6=table(odd_data[(i+4):(i+8),m])
      result6=paste0(t6[1],t6[2])
      t7=table(odd_data[(i+4):(i+7),m])
      result7=paste0(t7[1],t7[2])
      t8=table(odd_data[(i+4):(i+6),m])
      result8=paste0(t8[1],t8[2])
      comb3=paste0(result8,'-',result7,'-',result6)
      
      odd_final_comb[i,]=matrix(c(comb1,comb2,comb3),ncol = 3)
    }
    colnames(odd_final_comb)=c('R9','R7','R5')
    }
    # 偶数
    { even_n=nrow(even_data)-8     
      even_final_comb=matrix(ncol = 3,nrow = even_n)
      #求'9R','7R','5R'
      for (i in 1:odd_n) {
        t0=table(even_data[i:(i+8),m])
        result0=paste0(t0[1],t0[2])
        t1=table(even_data[i:(i+7),m])
        result1=paste0(t1[1],t1[2])
        t2=table(even_data[i:(i+6),m])
        result2=paste0(t2[1],t2[2])
        comb1=paste0(result2,'-',result1,'-',result0)
        
        t3=table(even_data[(i+2):(i+8),m])
        result3=paste0(t3[1],t3[2])
        t4=table(even_data[(i+2):(i+7),m])
        result4=paste0(t4[1],t4[2])
        t5=table(even_data[(i+2):(i+6),m])
        result5=paste0(t5[1],t5[2])
        comb2=paste0(result5,'-',result4,'-',result3)
        
        t6=table(even_data[(i+4):(i+8),m])
        result6=paste0(t6[1],t6[2])
        t7=table(even_data[(i+4):(i+7),m])
        result7=paste0(t7[1],t7[2])
        t8=table(even_data[(i+4):(i+6),m])
        result8=paste0(t8[1],t8[2])
        comb3=paste0(result8,'-',result7,'-',result6)
        
        even_final_comb[i,]=matrix(c(comb1,comb2,comb3),ncol = 3)
      }
      colnames(even_final_comb)=c('R9','R7','R5')
    }
    # 计算量化后结果
    {
      #奇数行不用反，固定使用A
      odd=new_quantize(path = path[index,,drop=F],real_comb=odd_final_comb,set=odd_data[,m])
      odd_minus=new_quantize(path = minus_path[index,,drop=F],real_comb=odd_final_comb,set=odd_data[,m])
      even=new_quantize(path = path[index,,drop=F],real_comb=even_final_comb,set=even_data[,m])
      even_minus=new_quantize(path = minus_path[index,,drop=F],real_comb=even_final_comb,set=even_data[,m])
      
      odd_R9_quantity=odd[[1]]
      odd_minus_R9_quantity=odd_minus[[1]]
      even_R9_quantity=even[[1]]
      even_minus_R9_quantity=even_minus[[1]]
      
      odd_R7_quantity=odd[[2]]
      odd_minus_R7_quantity=odd_minus[[2]]
      even_R7_quantity=even[[2]]
      even_minus_R7_quantity=even_minus[[2]]
      
      odd_R5_quantity=odd[[3]]
      odd_minus_R5_quantity=odd_minus[[3]]
      even_R5_quantity=even[[3]]
      even_minus_R5_quantity=even_minus[[3]]
      
    }
    
    # 1：奇数轨奇数行；2：偶数轨偶数行；3：奇数轨偶数行；4：偶数轨偶数行
    odd_sumtempt=odd_R9_quantity+odd_R7_quantity+odd_R5_quantity
    odd_minus_sumtempt=odd_minus_R9_quantity+odd_minus_R7_quantity+odd_minus_R5_quantity
    even_sumtempt=even_R9_quantity+even_R7_quantity+even_R5_quantity
    even_minus_sumtempt=even_minus_R9_quantity+even_minus_R7_quantity+even_minus_R5_quantity
    
    
    # 1、奇数轨的奇数行必定是使用原始的formula产生的：
    odd_sumtempt_odd_row=odd_sumtempt[seq(1,odd_n,2),,drop=FALSE]
    # 2、偶数轨的奇数行
    if (seq[1]==0){
      # 保持不变
      even_sumtempt_odd_row=even_sumtempt[seq(1,even_n,2),,drop=FALSE]  
    }
    else if(seq[1]==1){
      # 根据奇数轨的奇数行决定
      even_sumtempt_odd_row=ifelse(odd_sumtempt_odd_row<0,even_sumtempt[seq(1,even_n,2),,drop=FALSE],even_minus_sumtempt[seq(1,even_n,2),,drop=FALSE])
    }
    else{
      print('seq[1] must be 0 or 1')
      break
    }
    # 2、奇数轨的偶数行
    if (seq[2]==0){
      # 保持不变
      odd_sumtempt_even_row=odd_sumtempt[seq(2,odd_n,2),,drop=FALSE]  
    }
    else if(seq[2]==1){
      # 根据奇数轨的奇数行决定,注意这里奇数行与偶数行一定要一样多
      odd_sumtempt_even_row=ifelse(odd_sumtempt_odd_row<0,odd_sumtempt[seq(2,odd_n,2),,drop=FALSE],odd_minus_sumtempt[seq(2,odd_n,2),,drop=FALSE])
    }
    else if(seq[2]==2){
      # 根据偶数轨的奇数行
      odd_sumtempt_even_row=ifelse(even_sumtempt_odd_row<0,odd_sumtempt[seq(2,odd_n,2),,drop=FALSE],odd_minus_sumtempt[seq(2,odd_n,2),,drop=FALSE])
    }
    else{
      print('seq[2] must be 0,1 or 2')
      break
    }
    # 3、偶数轨的偶数行
    if (seq[3]==0){
      # 保持不变
      even_sumtempt_even_row=even_sumtempt[seq(2,even_n,2),,drop=FALSE]  
    }
    else if(seq[3]==1){
      # 根据奇数轨的奇数行决定
      even_sumtempt_even_row=ifelse(odd_sumtempt_odd_row<0,even_sumtempt[seq(2,even_n,2),,drop=FALSE],even_minus_sumtempt[seq(2,even_n,2),,drop=FALSE])
    }
    else if(seq[3]==2){
      # 根据偶数轨的奇数行
      even_sumtempt_even_row=ifelse(even_sumtempt_odd_row<0,even_sumtempt[seq(2,even_n,2),,drop=FALSE],even_minus_sumtempt[seq(2,even_n,2),,drop=FALSE])
    }
    else if(seq[3]==3){
      # 根据奇数轨的偶数行
      even_sumtempt_even_row=ifelse(odd_sumtempt_even_row<0,even_sumtempt[seq(2,even_n,2),,drop=FALSE],even_minus_sumtempt[seq(2,even_n,2),,drop=FALSE])
    }
    else{
      print('seq[3] must be 0,1,2 or 3')
      break
    }
    
    
    # 最后合并成final_result
    sumtemp=matrix(nrow = n-16,ncol =1)
    sumtemp[seq(1,n-16,4),]=odd_sumtempt_odd_row
    sumtemp[seq(2,n-16,4),]=even_sumtempt_odd_row
    sumtemp[seq(3,n-16,4),]=odd_sumtempt_even_row
    sumtemp[seq(4,n-16,4),]=even_sumtempt_even_row
    # 转化成字符串，查看多少个“0”在字符串内，例如”00000“这部分是否在字符串内
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

unsatisfy_num1=pred_unsatisfy_num(index=7,data=data378,path=path,minus_path=minus_path,seq=c(1,1,2))

new_dis_unsatisfy=function(unsatisfy_num){
  k=unsatisfy_num[which.max(unsatisfy_num)]
  for(i in 4:k){
    print(paste0('连续负为',i,'的数据集个数：',length(which(unsatisfy_num==i))))
  }
}

new_dis_unsatisfy(unsatisfy_num1)


