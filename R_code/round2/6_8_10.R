# 1、formula准备------------------------------------------
data=read.csv('data\\treat_two_hundred.csv',header = T)
data1=read.csv('data\\new.csv',header = T)
data2=read.csv('data\\new1.csv',header = T)
data3=read.csv('data\\new2.csv',header = T)
data1=data1[,-23]
all_data=cbind(data,data1,data2,data3)
{
  library(stringr)
  formula=read.table('data\\R6810.txt',header = T,stringsAsFactors = F)
  R6=formula[1:10,];R8=formula[12:25,];R10=formula[27:44,]
  R6=str_split(R6,pattern = ',',simplify = T)
  R6_1=apply(R6[,1:3], 1,function(x)str_c(x,collapse = '-'))
  R6_2=apply(R6[,4:6], 1,function(x)str_c(x,collapse = '-'))
  R61=str_c(R6_1,collapse = '|')
  R62=str_c(R6_2,collapse = '|')
  R8=str_split(R8,pattern = ',',simplify = T)
  R8_1=apply(R8[,1:3], 1,function(x)str_c(x,collapse = '-'))
  R8_2=apply(R8[,4:6], 1,function(x)str_c(x,collapse = '-'))
  R81=str_c(R8_1,collapse = '|')
  R82=str_c(R8_2,collapse = '|')
  R10=str_split(R10,pattern = ',',simplify = T)
  R10_1=apply(R10[,1:3], 1,function(x)str_c(x,collapse = '-'))
  R10_2=apply(R10[,4:6], 1,function(x)str_c(x,collapse = '-'))
  R101=str_c(R10_1,collapse = '|')
  R102=str_c(R10_2,collapse = '|')
  path=matrix(c(R61,R81,R101, R61,R82,R101, R61,R81,R102, R61,R82,R102, R62,R81,R101, R62,R82,R101, R62,R81,R102, R62,R82,R102),byrow = T,ncol = 3)
  colnames(path)=c('R6','R8','R10')
  minus_path=matrix(c(R62,R82,R102, R62,R81,R102, R62,R82,R101, R62,R81,R101, R61,R82,R102, R61,R81,R102, R61,R82,R101, R61,R81,R101),byrow = T,ncol = 3)
  colnames(minus_path)=c('R6','R8','R10')
}

c=c('data','data1','data2','data3','all_data','path','minus_path')
a=ls()
rm(list=(setdiff(a,c)))
gc()
library(stringr)

# 2、训练----------------------------------------------

new_quantize=function(path,real_comb,set){
  n=length(set)-9
  r10=matrix(rep(0,nrow(path)*n),ncol = nrow(path),nrow = n)
  for (j in 1:nrow(path)) {
    for (i in 1:n) {
      r10[i,j]=ifelse(str_detect(real_comb[i,'R10'],path[j,'R10']),1,-1)
      
    }
  }
  r8=matrix(rep(0,nrow(path)*n),ncol = nrow(path),nrow = n)
  for (j in 1:nrow(path)) {
    for (i in 1:n) {
      r8[i,j]=ifelse(str_detect(real_comb[i,'R8'],path[j,'R8']),1,-1)
      
    }
  }
  r6=matrix(rep(0,nrow(path)*n),ncol = nrow(path),nrow = n)
  for (j in 1:nrow(path)) {
    for (i in 1:n) {
      r6[i,j]=ifelse(str_detect(real_comb[i,'R6'],path[j,'R6']),1,-1)
      
    }
  }
  return(list(r10,r8,r6))
}


train=function(sumtemp,num){
  n=nrow(data)
  optimal=list()
  for (i in 1:8){
    for (l in 1:(n-num*2-5)){
      if(sumtemp[l,i]<0&sumtemp[l+1,i]<0&sumtemp[l+2,i]<0&sumtemp[l+3,i]<0&sumtemp[l+4,i]<0&sumtemp[l+5,i]<0) break
      else if(l==(n-num*2-5)) optimal=append(optimal,i)
    }
  }
  if(length(optimal)!=0){
    return(matrix(unlist(optimal)))
  }
  else{
    return(matrix(0))
  }
  
}


run=function(data=data,path=path,minus_path=minus_path,seq){
  #注意这里的n
  n=nrow(data)
  odd_data=data[seq(1,n,2),]
  even_data=data[seq(2,n,2),]
  lis=NULL# 存放满足连续负个数小于等于5的结果
  num=9
  for (m in 1:length(data)) {
    # 产生奇数行fianl_comb
    { odd_n=nrow(odd_data)-num     
    odd_final_comb=matrix(ncol = 3,nrow = odd_n)
    for (i in 1:odd_n) {
      t0=table(odd_data[i:(i+num),m])
      result0=paste0(t0[1],t0[2])
      t1=table(odd_data[i:(i+num-1),m])
      result1=paste0(t1[1],t1[2])
      t2=table(odd_data[i:(i+num-2),m])
      result2=paste0(t2[1],t2[2])
      comb1=paste0(result2,'-',result1,'-',result0)
      
      t3=table(odd_data[(i+2):(i+num),m])
      result3=paste0(t3[1],t3[2])
      t4=table(odd_data[(i+2):(i+num-1),m])
      result4=paste0(t4[1],t4[2])
      t5=table(odd_data[(i+2):(i+num-2),m])
      result5=paste0(t5[1],t5[2])
      comb2=paste0(result5,'-',result4,'-',result3)
      
      t6=table(odd_data[(i+4):(i+num),m])
      result6=paste0(t6[1],t6[2])
      t7=table(odd_data[(i+4):(i+num-1),m])
      result7=paste0(t7[1],t7[2])
      t8=table(odd_data[(i+4):(i+num-2),m])
      result8=paste0(t8[1],t8[2])
      comb3=paste0(result8,'-',result7,'-',result6)
      
      odd_final_comb[i,]=matrix(c(comb1,comb2,comb3),ncol = 3)
    }
    colnames(odd_final_comb)=c('R10','R8','R6')
    }
    # 偶数fianl_comb
    { even_n=nrow(even_data)-num    
      even_final_comb=matrix(ncol = 3,nrow = even_n)
      for (i in 1:even_n) {
        t0=table(even_data[i:(i+num),m])
        result0=paste0(t0[1],t0[2])
        t1=table(even_data[i:(i+num-1),m])
        result1=paste0(t1[1],t1[2])
        t2=table(even_data[i:(i+num-2),m])
        result2=paste0(t2[1],t2[2])
        comb1=paste0(result2,'-',result1,'-',result0)
        
        t3=table(even_data[(i+2):(i+num),m])
        result3=paste0(t3[1],t3[2])
        t4=table(even_data[(i+2):(i+num-1),m])
        result4=paste0(t4[1],t4[2])
        t5=table(even_data[(i+2):(i+num-2),m])
        result5=paste0(t5[1],t5[2])
        comb2=paste0(result5,'-',result4,'-',result3)
        
        t6=table(even_data[(i+4):(i+num),m])
        result6=paste0(t6[1],t6[2])
        t7=table(even_data[(i+4):(i+num-1),m])
        result7=paste0(t7[1],t7[2])
        t8=table(even_data[(i+4):(i+num-2),m])
        result8=paste0(t8[1],t8[2])
        comb3=paste0(result8,'-',result7,'-',result6)
        
        even_final_comb[i,]=matrix(c(comb1,comb2,comb3),ncol = 3)
      }
      colnames(even_final_comb)=c('R10','R8','R6')
    }
    # 计算【量化】后结果
    {
      #奇数行不用反，固定使用A
      odd=new_quantize(path = path,real_comb=odd_final_comb,set=odd_data[,m])
      odd_minus=new_quantize(path = minus_path,real_comb=odd_final_comb,set=odd_data[,m])
      even=new_quantize(path = path,real_comb=even_final_comb,set=even_data[,m])
      even_minus=new_quantize(path = minus_path,real_comb=even_final_comb,set=even_data[,m])
      
      # 【注】：往下R9就是R10，R7就是R8.
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
    # 求formulaA或者B下的final_result
    {
      # 1：奇数轨奇数行；2：偶数轨偶数行；3：奇数轨偶数行；4：偶数轨偶数行
      odd_sumtempt=odd_R9_quantity+odd_R7_quantity+odd_R5_quantity
      
      odd_minus_sumtempt=odd_minus_R9_quantity+odd_minus_R7_quantity+odd_minus_R5_quantity
      
      even_sumtempt=even_R9_quantity+even_R7_quantity+even_R5_quantity
      
      even_minus_sumtempt=even_minus_R9_quantity+even_minus_R7_quantity+even_minus_R5_quantity
      
    }
    
    # 当奇数轨、偶数轨个数为奇数时，都去掉最后一个保证奇数轨的奇数行偶数行一样多
  
    # 还需要在偶数轨的奇数行受到奇数轨奇数行影响时修改seq[1]=1
    if((nrow(odd_data)-num )%%2!=0){
      rest=rbind(odd_sumtempt[odd_n,,drop=FALSE],even_sumtempt[odd_n,,drop=FALSE])
      odd_n= odd_n-1
      even_n= even_n-1
      
    }
    # 1、奇数轨的奇数行必定是使用原始的formula产生的：
    odd_sumtempt_odd_row=odd_sumtempt[seq(1,odd_n,2),,drop=FALSE]
    # 2、偶数轨的奇数行
    {
      if (seq[1]==0){
        # 保持不变
        even_sumtempt_odd_row=even_sumtempt[seq(1,even_n,2),,drop=FALSE]  
      }
      else if(seq[1]==1){
        # 根据奇数轨的奇数行决定
        even_sumtempt_odd_row=ifelse(odd_sumtempt_odd_row<0,even_sumtempt[seq(1,even_n,2),,drop=FALSE],even_minus_sumtempt[seq(1,even_n,2),,drop=FALSE])
        rest[2,]=ifelse(rest[1,]<0,rest[2,],-rest[2,])
      }
      else{
        print('seq[1] must be 0 or 1')
        break
      }
    }
    
    # 3、奇数轨的偶数行
    {
      if (seq[2]==0){
        # 保持不变
        odd_sumtempt_even_row=odd_sumtempt[seq(2,odd_n,2),,drop=FALSE]
        
      }
      else if(seq[2]==1){
        # 根据奇数轨的奇数行决定,注意这里奇数行与偶数行一定要一样多
        odd_sumtempt_even_row=ifelse(odd_sumtempt_odd_row<0,odd_sumtempt[seq(2,odd_n,2),,drop=FALSE],odd_minus_sumtempt[seq(2,odd_n,2),,drop=FALSE])
      }
      else if(seq[2]==2){
        # 根据偶数轨的奇数行,同号时使用原始的,记为1，异号记为2
        if(seq[1]==1){
          odd_sumtempt_even_row=ifelse(even_sumtempt_odd_row*odd_sumtempt_odd_row>0,odd_sumtempt[seq(2,odd_n,2),,drop=FALSE],odd_minus_sumtempt[seq(2,odd_n,2),,drop=FALSE])
          # pattern由1、2号位同时决定：
          pattern=ifelse(even_sumtempt_odd_row*odd_sumtempt_odd_row>0,1,2)
          # sign用来记录odd_sumtempt_even_row的正负符号
          sign=ifelse(odd_sumtempt_even_row>0,1,-1)
          odd_newsum_even_row=pattern*sign
        }
        else if(seq[1]==0){
          odd_sumtempt_even_row=ifelse(even_sumtempt_odd_row<0,odd_sumtempt[seq(2,odd_n,2),,drop=FALSE],odd_minus_sumtempt[seq(2,odd_n,2),,drop=FALSE])
          
        }
        
      }
      else{
        print('seq[2] must be 0,1 or 2')
        break
      }
    }
   
    # 4、偶数轨的偶数行
    {
      if (seq[3]==0){
        # 保持不变
        even_sumtempt_even_row=even_sumtempt[seq(2,even_n,2),,drop=FALSE]
        
      }
      else if(seq[3]==1){
        # 根据奇数轨的奇数行决定
        even_sumtempt_even_row=ifelse(odd_sumtempt_odd_row<0,even_sumtempt[seq(2,even_n,2),,drop=FALSE],even_minus_sumtempt[seq(2,even_n,2),,drop=FALSE])
        
      }
      else if(seq[3]==2){
        if(seq[1]==1){
          even_sumtempt_even_row=ifelse(even_sumtempt_odd_row*odd_sumtempt_odd_row>0,even_sumtempt[seq(2,even_n,2),,drop=FALSE],even_minus_sumtempt[seq(2,even_n,2),,drop=FALSE])
          
        }
        else if(seq[1]==0){
          even_sumtempt_even_row=ifelse(even_sumtempt_odd_row<0,even_sumtempt[seq(2,even_n,2),,drop=FALSE],even_minus_sumtempt[seq(2,even_n,2),,drop=FALSE])
          
          
        }
      }
      else if(seq[3]==3){
        # 仅根据3
        if(seq[1]==0&seq[2]==0){
          even_sumtempt_even_row=ifelse(odd_sumtempt_even_row<0,even_sumtempt[seq(2,even_n,2),,drop=FALSE],even_minus_sumtempt[seq(2,even_n,2),,drop=FALSE]) 
          
          
        }
        # 根据1，3
        else if(seq[1]==0&seq[2]==1){
          even_sumtempt_even_row=ifelse(odd_sumtempt_even_row*odd_sumtempt_odd_row>0,even_sumtempt[seq(2,even_n,2),,drop=FALSE],even_minus_sumtempt[seq(2,even_n,2),,drop=FALSE]) 
          
        }
        # 根据2、3
        else if(seq[1]==0&seq[2]==2){
          even_sumtempt_even_row=ifelse(odd_sumtempt_even_row*even_sumtempt_odd_row>0,even_sumtempt[seq(2,even_n,2),,drop=FALSE],even_minus_sumtempt[seq(2,even_n,2),,drop=FALSE]) 
          
          
        }
        # 仅根据3来，因为3固定使用A：seq[2]=0
        else if(seq[1]==1&seq[2]==0){
          even_sumtempt_even_row=ifelse(odd_sumtempt_even_row<0,even_sumtempt[seq(2,even_n,2),,drop=FALSE],even_minus_sumtempt[seq(2,even_n,2),,drop=FALSE]) 
          
        }
        # 根据1，3，因为3只与1相关
        else if(seq[1]==1&seq[2]==1){
          even_sumtempt_even_row=ifelse(odd_sumtempt_even_row*odd_sumtempt_odd_row>0,even_sumtempt[seq(2,even_n,2),,drop=FALSE],even_minus_sumtempt[seq(2,even_n,2),,drop=FALSE]) 
          
          
        }
        # 根据1，2，3来
        else if(seq[1]==1&seq[2]==2){
          even_sumtempt_even_row=ifelse(odd_sumtempt_even_row*even_sumtempt_odd_row*odd_sumtempt_odd_row<0,even_sumtempt[seq(2,even_n,2),,drop=FALSE],even_minus_sumtempt[seq(2,even_n,2),,drop=FALSE]) 
          
        }
        
      }
      else{
        print('seq[3] must be 0,1,2 or 3')
        break
      }
    }
    
    
    
    # 最后合并成final_result
    sumtemp=matrix(nrow = n-18,ncol =nrow(path))
    sumtemp[seq(1,n-18,4),]=rbind(odd_sumtempt_odd_row,rest[1,,drop=F])
    sumtemp[seq(2,n-18,4),]=rbind(even_sumtempt_odd_row,rest[2,,drop=F])
    sumtemp[seq(3,n-18,4),]=odd_sumtempt_even_row
    sumtemp[seq(4,n-18,4),]=even_sumtempt_even_row
    
    sat=matrix(train(sumtemp =sumtemp,num))
    if(sat[1,1]==0) print(paste0('一个formula都不满足的',m))
    # 满足的小于等于5的formula
    lis=c(lis,sat)
    if (m%%10==0) print(m)
  }
 
  # 交集
  t=table(lis)
  order_optimal=sort(t,decreasing = T)
  return(order_optimal)
}

# plan3:
run(data=all_data,path=path,minus_path=minus_path,seq = c(1,0,3))

# plan11:
run(data=all_data,path=path,minus_path=minus_path,seq = c(1,1,2))

# plan13:双轨自己独立反一反
run(data=all_data,path=path,minus_path=minus_path,seq = c(0,1,2))
# 3、预测---------------------------------------------
pred_unsatisfy_num=function(index,data=data,path=path,minus_path=minus_path,seq){
  path = path[index,,drop=F]
  minus_path = minus_path[index,,drop=F]
  continue_minus_num=c()
  #注意这里的n
  n=nrow(data)
  odd_data=data[seq(1,n,2),]
  even_data=data[seq(2,n,2),]
  for (m in 1:length(data)) {
    if(m%%10==0)print(m)
# 产生奇数行fianl_comb
    { odd_n=nrow(odd_data)-num     
      odd_final_comb=matrix(ncol = 3,nrow = odd_n)
      for (i in 1:odd_n) {
        t0=table(odd_data[i:(i+num),m])
        result0=paste0(t0[1],t0[2])
        t1=table(odd_data[i:(i+num-1),m])
        result1=paste0(t1[1],t1[2])
        t2=table(odd_data[i:(i+num-2),m])
        result2=paste0(t2[1],t2[2])
        comb1=paste0(result2,'-',result1,'-',result0)
        
        t3=table(odd_data[(i+2):(i+num),m])
        result3=paste0(t3[1],t3[2])
        t4=table(odd_data[(i+2):(i+num-1),m])
        result4=paste0(t4[1],t4[2])
        t5=table(odd_data[(i+2):(i+num-2),m])
        result5=paste0(t5[1],t5[2])
        comb2=paste0(result5,'-',result4,'-',result3)
        
        t6=table(odd_data[(i+4):(i+num),m])
        result6=paste0(t6[1],t6[2])
        t7=table(odd_data[(i+4):(i+num-1),m])
        result7=paste0(t7[1],t7[2])
        t8=table(odd_data[(i+4):(i+num-2),m])
        result8=paste0(t8[1],t8[2])
        comb3=paste0(result8,'-',result7,'-',result6)
        
        odd_final_comb[i,]=matrix(c(comb1,comb2,comb3),ncol = 3)
      }
      colnames(odd_final_comb)=c('R10','R8','R6')
    }
# 偶数fianl_comb
    { even_n=nrow(even_data)-num    
      even_final_comb=matrix(ncol = 3,nrow = even_n)
      for (i in 1:even_n) {
        t0=table(even_data[i:(i+num),m])
        result0=paste0(t0[1],t0[2])
        t1=table(even_data[i:(i+num-1),m])
        result1=paste0(t1[1],t1[2])
        t2=table(even_data[i:(i+num-2),m])
        result2=paste0(t2[1],t2[2])
        comb1=paste0(result2,'-',result1,'-',result0)
        
        t3=table(even_data[(i+2):(i+num),m])
        result3=paste0(t3[1],t3[2])
        t4=table(even_data[(i+2):(i+num-1),m])
        result4=paste0(t4[1],t4[2])
        t5=table(even_data[(i+2):(i+num-2),m])
        result5=paste0(t5[1],t5[2])
        comb2=paste0(result5,'-',result4,'-',result3)
        
        t6=table(even_data[(i+4):(i+num),m])
        result6=paste0(t6[1],t6[2])
        t7=table(even_data[(i+4):(i+num-1),m])
        result7=paste0(t7[1],t7[2])
        t8=table(even_data[(i+4):(i+num-2),m])
        result8=paste0(t8[1],t8[2])
        comb3=paste0(result8,'-',result7,'-',result6)
        
        even_final_comb[i,]=matrix(c(comb1,comb2,comb3),ncol = 3)
      }
      colnames(even_final_comb)=c('R10','R8','R6')
    }
# 计算【量化】后结果
    {
      #奇数行不用反，固定使用A
      odd=new_quantize(path = path,real_comb=odd_final_comb,set=odd_data[,m])
      odd_minus=new_quantize(path = minus_path,real_comb=odd_final_comb,set=odd_data[,m])
      even=new_quantize(path = path,real_comb=even_final_comb,set=even_data[,m])
      even_minus=new_quantize(path = minus_path,real_comb=even_final_comb,set=even_data[,m])
      
      # 【注】：往下R9就是R10，R7就是R8.
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
# 求formulaA或者B下的final_result
    {
      # 1：奇数轨奇数行；2：偶数轨偶数行；3：奇数轨偶数行；4：偶数轨偶数行
      odd_sumtempt=odd_R9_quantity+odd_R7_quantity+odd_R5_quantity
      
      odd_minus_sumtempt=odd_minus_R9_quantity+odd_minus_R7_quantity+odd_minus_R5_quantity
      
      even_sumtempt=even_R9_quantity+even_R7_quantity+even_R5_quantity
      
      even_minus_sumtempt=even_minus_R9_quantity+even_minus_R7_quantity+even_minus_R5_quantity
      
    }
    
    # 当奇数轨、偶数轨个数为奇数时，都去掉最后一个保证奇数轨的奇数行偶数行一样多
    
    # 还需要在偶数轨的奇数行受到奇数轨奇数行影响时修改seq[1]=1
    if((nrow(odd_data)-num )%%2!=0){
      rest=rbind(odd_sumtempt[odd_n,,drop=FALSE],even_sumtempt[odd_n,,drop=FALSE])
      odd_n= odd_n-1
      even_n= even_n-1
      
    }
# 1、奇数轨的奇数行必定是使用原始的formula产生的：
    odd_sumtempt_odd_row=odd_sumtempt[seq(1,odd_n,2),,drop=FALSE]
# 2、偶数轨的奇数行
    {
      if (seq[1]==0){
        # 保持不变
        even_sumtempt_odd_row=even_sumtempt[seq(1,even_n,2),,drop=FALSE]  
      }
      else if(seq[1]==1){
        # 根据奇数轨的奇数行决定
        even_sumtempt_odd_row=ifelse(odd_sumtempt_odd_row<0,even_sumtempt[seq(1,even_n,2),,drop=FALSE],even_minus_sumtempt[seq(1,even_n,2),,drop=FALSE])
        rest[2,]=ifelse(rest[1,]<0,rest[2,],-rest[2,])
      }
      else{
        print('seq[1] must be 0 or 1')
        break
      }
    }
    
# 3、奇数轨的偶数行
    {
      if (seq[2]==0){
        # 保持不变
        odd_sumtempt_even_row=odd_sumtempt[seq(2,odd_n,2),,drop=FALSE]
        
      }
      else if(seq[2]==1){
        # 根据奇数轨的奇数行决定,注意这里奇数行与偶数行一定要一样多
        odd_sumtempt_even_row=ifelse(odd_sumtempt_odd_row<0,odd_sumtempt[seq(2,odd_n,2),,drop=FALSE],odd_minus_sumtempt[seq(2,odd_n,2),,drop=FALSE])
      }
      else if(seq[2]==2){
        # 根据偶数轨的奇数行,同号时使用原始的,记为1，异号记为2
        if(seq[1]==1){
          odd_sumtempt_even_row=ifelse(even_sumtempt_odd_row*odd_sumtempt_odd_row>0,odd_sumtempt[seq(2,odd_n,2),,drop=FALSE],odd_minus_sumtempt[seq(2,odd_n,2),,drop=FALSE])
          # pattern由1、2号位同时决定：
          pattern=ifelse(even_sumtempt_odd_row*odd_sumtempt_odd_row>0,1,2)
          # sign用来记录odd_sumtempt_even_row的正负符号
          sign=ifelse(odd_sumtempt_even_row>0,1,-1)
          odd_newsum_even_row=pattern*sign
        }
        else if(seq[1]==0){
          odd_sumtempt_even_row=ifelse(even_sumtempt_odd_row<0,odd_sumtempt[seq(2,odd_n,2),,drop=FALSE],odd_minus_sumtempt[seq(2,odd_n,2),,drop=FALSE])
          
        }
        
      }
      else{
        print('seq[2] must be 0,1 or 2')
        break
      }
    }
    
# 4、偶数轨的偶数行
    {
      if (seq[3]==0){
        # 保持不变
        even_sumtempt_even_row=even_sumtempt[seq(2,even_n,2),,drop=FALSE]
        
      }
      else if(seq[3]==1){
        # 根据奇数轨的奇数行决定
        even_sumtempt_even_row=ifelse(odd_sumtempt_odd_row<0,even_sumtempt[seq(2,even_n,2),,drop=FALSE],even_minus_sumtempt[seq(2,even_n,2),,drop=FALSE])
        
      }
      else if(seq[3]==2){
        if(seq[1]==1){
          even_sumtempt_even_row=ifelse(even_sumtempt_odd_row*odd_sumtempt_odd_row>0,even_sumtempt[seq(2,even_n,2),,drop=FALSE],even_minus_sumtempt[seq(2,even_n,2),,drop=FALSE])
          
        }
        else if(seq[1]==0){
          even_sumtempt_even_row=ifelse(even_sumtempt_odd_row<0,even_sumtempt[seq(2,even_n,2),,drop=FALSE],even_minus_sumtempt[seq(2,even_n,2),,drop=FALSE])
          
          
        }
      }
      else if(seq[3]==3){
        # 仅根据3
        if(seq[1]==0&seq[2]==0){
          even_sumtempt_even_row=ifelse(odd_sumtempt_even_row<0,even_sumtempt[seq(2,even_n,2),,drop=FALSE],even_minus_sumtempt[seq(2,even_n,2),,drop=FALSE]) 
          
          
        }
        # 根据1，3
        else if(seq[1]==0&seq[2]==1){
          even_sumtempt_even_row=ifelse(odd_sumtempt_even_row*odd_sumtempt_odd_row>0,even_sumtempt[seq(2,even_n,2),,drop=FALSE],even_minus_sumtempt[seq(2,even_n,2),,drop=FALSE]) 
          
        }
        # 根据2、3
        else if(seq[1]==0&seq[2]==2){
          even_sumtempt_even_row=ifelse(odd_sumtempt_even_row*even_sumtempt_odd_row>0,even_sumtempt[seq(2,even_n,2),,drop=FALSE],even_minus_sumtempt[seq(2,even_n,2),,drop=FALSE]) 
          
          
        }
        # 仅根据3来，因为3固定使用A：seq[2]=0
        else if(seq[1]==1&seq[2]==0){
          even_sumtempt_even_row=ifelse(odd_sumtempt_even_row<0,even_sumtempt[seq(2,even_n,2),,drop=FALSE],even_minus_sumtempt[seq(2,even_n,2),,drop=FALSE]) 
          
        }
        # 根据1，3，因为3只与1相关
        else if(seq[1]==1&seq[2]==1){
          even_sumtempt_even_row=ifelse(odd_sumtempt_even_row*odd_sumtempt_odd_row>0,even_sumtempt[seq(2,even_n,2),,drop=FALSE],even_minus_sumtempt[seq(2,even_n,2),,drop=FALSE]) 
          
          
        }
        # 根据1，2，3来
        else if(seq[1]==1&seq[2]==2){
          even_sumtempt_even_row=ifelse(odd_sumtempt_even_row*even_sumtempt_odd_row*odd_sumtempt_odd_row<0,even_sumtempt[seq(2,even_n,2),,drop=FALSE],even_minus_sumtempt[seq(2,even_n,2),,drop=FALSE]) 
          
        }
        
      }
      else{
        print('seq[3] must be 0,1,2 or 3')
        break
      }
    }
    
    # 最后合并成final_result
    sumtemp=matrix(nrow = n-18,ncol =nrow(path))
    sumtemp[seq(1,n-18,4),]=rbind(odd_sumtempt_odd_row,rest[1,,drop=F])
    sumtemp[seq(2,n-18,4),]=rbind(even_sumtempt_odd_row,rest[2,,drop=F])
    sumtemp[seq(3,n-18,4),]=odd_sumtempt_even_row
    sumtemp[seq(4,n-18,4),]=even_sumtempt_even_row
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

unsatisfy_num1=pred_unsatisfy_num(index=4,data=all_data,path=path,minus_path=minus_path,seq=c(1,1,2))

new_dis_unsatisfy=function(unsatisfy_num){
  k=unsatisfy_num[which.max(unsatisfy_num)]
  for(i in 4:k){
    print(paste0('连续负为',i,'的数据集个数：',length(which(unsatisfy_num==i))))
  }
}

new_dis_unsatisfy(unsatisfy_num1)



