# 1、formula准备------------------------------------------
data=read.csv('data\\treat_two_hundred.csv',header = T)
data1=read.csv('data\\new.csv',header = T)
data2=read.csv('data\\new1.csv',header = T)
data3=read.csv('data\\new2.csv',header = T)
data1=data1[,-23]
all_data=cbind(data,data1,data2,data3)
{
  library(stringr)
  formula=read.table('data\\head_tail.txt',header = F,stringsAsFactors = F)
  R5_diff=formula[1:4,];R5_same=formula[6:13,]
  A5_diff=str_c(apply(str_split(R5_diff,',',simplify = T)[,1:3],1,function(x)str_c(x,collapse = '-')),collapse = '|')
  A5_same=str_c(apply(str_split(R5_same,',',simplify = T)[,1:3],1,function(x)str_c(x,collapse = '-')),collapse = '|')
  B5_diff=str_c(apply(str_split(R5_diff,',',simplify = T)[,4:6],1,function(x)str_c(x,collapse = '-')),collapse = '|')
  B5_same=str_c(apply(str_split(R5_same,',',simplify = T)[,4:6],1,function(x)str_c(x,collapse = '-')),collapse = '|')
  A5=cbind(A5_diff,A5_same);B5=cbind(B5_diff,B5_same);
  
  R7_diff=formula[16:23,];R7_same=formula[25:36,]
  A7_diff=str_c(apply(str_split(R7_diff,',',simplify = T)[,1:3],1,function(x)str_c(x,collapse = '-')),collapse = '|')
  A7_same=str_c(apply(str_split(R7_same,',',simplify = T)[,1:3],1,function(x)str_c(x,collapse = '-')),collapse = '|')
  B7_diff=str_c(apply(str_split(R7_diff,',',simplify = T)[,4:6],1,function(x)str_c(x,collapse = '-')),collapse = '|')
  B7_same=str_c(apply(str_split(R7_same,',',simplify = T)[,4:6],1,function(x)str_c(x,collapse = '-')),collapse = '|')
  A7=cbind(A7_diff,A7_same);B7=cbind(B7_diff,B7_same);
  
  R9_diff=formula[39:50,];R9_same=formula[52:67,]
  A9_diff=str_c(apply(str_split(R9_diff,',',simplify = T)[,1:3],1,function(x)str_c(x,collapse = '-')),collapse = '|')
  A9_same=str_c(apply(str_split(R9_same,',',simplify = T)[,1:3],1,function(x)str_c(x,collapse = '-')),collapse = '|')
  B9_diff=str_c(apply(str_split(R9_diff,',',simplify = T)[,4:6],1,function(x)str_c(x,collapse = '-')),collapse = '|')
  B9_same=str_c(apply(str_split(R9_same,',',simplify = T)[,4:6],1,function(x)str_c(x,collapse = '-')),collapse = '|')
  A9=cbind(A9_diff,A9_same);B9=cbind(B9_diff,B9_same);
  
  path=matrix(c(A9,A7,A5, A9,B7,A5, A9,A7,B5, A9,B7,B5, B9,A7,A5, B9,B7,A5, B9,A7,B5, B9,B7,B5),byrow = T,ncol = 6)
  colnames(path)=c('B_R9','P_R9','B_R7','P_R7','B_R5','P_R5')
  minus_path=matrix(c(B9,B7,B5, B9,A7,B5, B9,B7,A5, B9,A7,A5, A9,B7,B5, A9,A7,B5, A9,B7,A5, A9,A7,A5),byrow = T,ncol = 6)
  colnames(minus_path)=c('B_R9','P_R9','B_R7','P_R7','B_R5','P_R5')
}
# B表示首尾不同，P表示首尾相同
c=c('data','data1','data2','data3','all_data','path','minus_path')
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
      r9[i,j]=ifelse(set[i]!=set[i+6],ifelse(str_detect(real_comb[i,'R9'],path[j,'B_R9']),1,-1),ifelse(str_detect(real_comb[i,'R9'],path[j,'P_R9']),1,-1))
      
    }
  }
  r7=matrix(rep(0,nrow(path)*n),ncol = nrow(path),nrow = n)
  for (j in 1:nrow(path)) {
    for (i in 1:n) {
      r7[i,j]=ifelse(set[i+2]!=set[i+6],ifelse(str_detect(real_comb[i,'R7'],path[j,'B_R7']),1,-1),ifelse(str_detect(real_comb[i,'R7'],path[j,'P_R7']),1,-1))
      
    }
  }
  r5=matrix(rep(0,nrow(path)*n),ncol = nrow(path),nrow = n)
  for (j in 1:nrow(path)) {
    for (i in 1:n) {
      r5[i,j]=ifelse(set[i+4]!=set[i+6],ifelse(str_detect(real_comb[i,'R5'],path[j,'B_R5']),1,-1),ifelse(str_detect(real_comb[i,'R5'],path[j,'P_R5']),1,-1))
      
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


run=function(data=data,path=path,minus_path=minus_path,seq){
  #注意这里的n
  n=nrow(data)
  odd_data=data[seq(1,n,2),]
  even_data=data[seq(2,n,2),]
  lis=NULL# 存放满足连续负个数小于等于5的结果
  newsum_index=NULL# 存放8个formula下的新指标值
  for (m in 1:length(data)) {
    # 产生奇数行fianl_comb
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
    # 偶数fianl_comb
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
    # 计算【量化】后结果
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
    # 求formulaA或者B下的final_result
    {
      # 1：奇数轨奇数行；2：偶数轨偶数行；3：奇数轨偶数行；4：偶数轨偶数行
      odd_sumtempt=odd_R9_quantity+odd_R7_quantity+odd_R5_quantity
      
      odd_minus_sumtempt=odd_minus_R9_quantity+odd_minus_R7_quantity+odd_minus_R5_quantity
      
      even_sumtempt=even_R9_quantity+even_R7_quantity+even_R5_quantity
      
      even_minus_sumtempt=even_minus_R9_quantity+even_minus_R7_quantity+even_minus_R5_quantity
      
    }
    
    # 1、奇数轨的奇数行必定是使用原始的formula产生的：
    odd_sumtempt_odd_row=odd_sumtempt[seq(1,odd_n,2),,drop=FALSE]
    # 因为固定使用A，所以都是1：
    pattern=matrix(1,nrow =odd_n/2 ,ncol =nrow(path))
    # sign用来记录odd_sumtempt_odd_row的正负符号
    sign=ifelse(odd_sumtempt_odd_row>0,1,-1)
    odd_newsum_odd_row=pattern*sign
    # 2、偶数轨的奇数行
    {
      if (seq[1]==0){
        # 保持不变
        even_sumtempt_odd_row=even_sumtempt[seq(1,even_n,2),,drop=FALSE]  
        # 因为固定使用A，所以都是1：
        pattern=matrix(1,nrow =even_n/2 ,ncol =nrow(path))
        # sign用来记录even_sumtempt_odd_row的正负符号
        sign=ifelse(even_sumtempt_odd_row>0,1,-1)
        even_newsum_odd_row=pattern*sign
      }
      else if(seq[1]==1){
        # 根据奇数轨的奇数行决定
        even_sumtempt_odd_row=ifelse(odd_sumtempt_odd_row<0,even_sumtempt[seq(1,even_n,2),,drop=FALSE],even_minus_sumtempt[seq(1,even_n,2),,drop=FALSE])
        # 因为根据1号位的odd_sumtempt_odd_row来，若小于0说明路径就是B所以2号位应该使用A也就是记为1：
        pattern=ifelse(odd_sumtempt_odd_row<0,1,2)
        # sign用来记录even_sumtempt_odd_row的正负符号
        sign=ifelse(even_sumtempt_odd_row>0,1,-1)
        even_newsum_odd_row=pattern*sign
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
        # 因为固定使用A，所以都是1：
        pattern=matrix(1,nrow =odd_n/2 ,ncol =nrow(path))
        # sign用来记录odd_sumtempt_even_row的正负符号
        sign=ifelse(odd_sumtempt_even_row>0,1,-1)
        odd_newsum_even_row=pattern*sign
      }
      else if(seq[2]==1){
        # 根据奇数轨的奇数行决定,注意这里奇数行与偶数行一定要一样多
        odd_sumtempt_even_row=ifelse(odd_sumtempt_odd_row<0,odd_sumtempt[seq(2,odd_n,2),,drop=FALSE],odd_minus_sumtempt[seq(2,odd_n,2),,drop=FALSE])
        # 因为根据1号位的odd_sumtempt_odd_row来，若小于0说明路径就是B所以2号位应该使用A也就是记为1：
        pattern=ifelse(odd_sumtempt_odd_row<0,1,2)
        # sign用来记录odd_sumtempt_even_row的正负符号
        sign=ifelse(odd_sumtempt_even_row>0,1,-1)
        odd_newsum_even_row=pattern*sign
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
          # pattern仅由2号位同时决定：
          pattern=ifelse(even_sumtempt_odd_row<0,1,2)
          # sign用来记录odd_sumtempt_even_row的正负符号
          sign=ifelse(odd_sumtempt_even_row>0,1,-1)
          odd_newsum_even_row=pattern*sign
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
        # 因为固定使用A，所以都是1：
        pattern=matrix(1,nrow =even_n/2 ,ncol =nrow(path))
        # sign用来记录even_sumtempt_even_row的正负符号
        sign=ifelse(even_sumtempt_even_row>0,1,-1)
        even_newsum_even_row=pattern*sign
      }
      else if(seq[3]==1){
        # 根据奇数轨的奇数行决定
        even_sumtempt_even_row=ifelse(odd_sumtempt_odd_row<0,even_sumtempt[seq(2,even_n,2),,drop=FALSE],even_minus_sumtempt[seq(2,even_n,2),,drop=FALSE])
        # 因为根据1号位：
        pattern=ifelse(odd_sumtempt_odd_row<0,1,2)
        # sign用来记录even_sumtempt_even_row的正负符号
        sign=ifelse(even_sumtempt_even_row>0,1,-1)
        even_newsum_even_row=pattern*sign
      }
      else if(seq[3]==2){
        if(seq[1]==1){
          even_sumtempt_even_row=ifelse(even_sumtempt_odd_row*odd_sumtempt_odd_row>0,even_sumtempt[seq(2,even_n,2),,drop=FALSE],even_minus_sumtempt[seq(2,even_n,2),,drop=FALSE])
          
          pattern=ifelse(even_sumtempt_odd_row*odd_sumtempt_odd_row>0,1,2)
          # sign用来记录even_sumtempt_even_row的正负符号
          sign=ifelse(even_sumtempt_even_row>0,1,-1)
          even_newsum_even_row=pattern*sign
        }
        else if(seq[1]==0){
          even_sumtempt_even_row=ifelse(even_sumtempt_odd_row<0,even_sumtempt[seq(2,even_n,2),,drop=FALSE],even_minus_sumtempt[seq(2,even_n,2),,drop=FALSE])
          
          pattern=ifelse(even_sumtempt_odd_row<0,1,2)
          # sign用来记录even_sumtempt_even_row的正负符号
          sign=ifelse(even_sumtempt_even_row>0,1,-1)
          even_newsum_even_row=pattern*sign
        }
      }
      else if(seq[3]==3){
        # 仅根据3
        if(seq[1]==0&seq[2]==0){
          even_sumtempt_even_row=ifelse(odd_sumtempt_even_row<0,even_sumtempt[seq(2,even_n,2),,drop=FALSE],even_minus_sumtempt[seq(2,even_n,2),,drop=FALSE]) 
          
          pattern=ifelse(odd_sumtempt_even_row<0,1,2)
          # sign用来记录even_sumtempt_even_row的正负符号
          sign=ifelse(even_sumtempt_even_row>0,1,-1)
          even_newsum_even_row=pattern*sign
        }
        # 根据1，3
        else if(seq[1]==0&seq[2]==1){
          even_sumtempt_even_row=ifelse(odd_sumtempt_even_row*odd_sumtempt_odd_row>0,even_sumtempt[seq(2,even_n,2),,drop=FALSE],even_minus_sumtempt[seq(2,even_n,2),,drop=FALSE]) 
          
          pattern=ifelse(odd_sumtempt_even_row*odd_sumtempt_odd_row>0,1,2)
          # sign用来记录even_sumtempt_even_row的正负符号
          sign=ifelse(even_sumtempt_even_row>0,1,-1)
          even_newsum_even_row=pattern*sign
        }
        # 根据2、3
        else if(seq[1]==0&seq[2]==2){
          even_sumtempt_even_row=ifelse(odd_sumtempt_even_row*even_sumtempt_odd_row>0,even_sumtempt[seq(2,even_n,2),,drop=FALSE],even_minus_sumtempt[seq(2,even_n,2),,drop=FALSE]) 
          
          pattern=ifelse(odd_sumtempt_even_row*even_sumtempt_odd_row>0,1,2)
          # sign用来记录even_sumtempt_even_row的正负符号
          sign=ifelse(even_sumtempt_even_row>0,1,-1)
          even_newsum_even_row=pattern*sign
        }
        # 仅根据3来，因为3固定使用A：seq[2]=0
        else if(seq[1]==1&seq[2]==0){
          even_sumtempt_even_row=ifelse(odd_sumtempt_even_row<0,even_sumtempt[seq(2,even_n,2),,drop=FALSE],even_minus_sumtempt[seq(2,even_n,2),,drop=FALSE]) 
          
          pattern=ifelse(odd_sumtempt_even_row<0,1,2)
          # sign用来记录even_sumtempt_even_row的正负符号
          sign=ifelse(even_sumtempt_even_row>0,1,-1)
          even_newsum_even_row=pattern*sign
        }
        # 根据1，3，因为3只与1相关
        else if(seq[1]==1&seq[2]==1){
          even_sumtempt_even_row=ifelse(odd_sumtempt_even_row*odd_sumtempt_odd_row>0,even_sumtempt[seq(2,even_n,2),,drop=FALSE],even_minus_sumtempt[seq(2,even_n,2),,drop=FALSE]) 
          
          pattern=ifelse(odd_sumtempt_even_row*odd_sumtempt_odd_row>0,1,2)
          # sign用来记录even_sumtempt_even_row的正负符号
          sign=ifelse(even_sumtempt_even_row>0,1,-1)
          even_newsum_even_row=pattern*sign
        }
        # 根据1，2，3来
        else if(seq[1]==1&seq[2]==2){
          even_sumtempt_even_row=ifelse(odd_sumtempt_even_row*even_sumtempt_odd_row*odd_sumtempt_odd_row<0,even_sumtempt[seq(2,even_n,2),,drop=FALSE],even_minus_sumtempt[seq(2,even_n,2),,drop=FALSE]) 
          
          pattern=ifelse(odd_sumtempt_even_row*even_sumtempt_odd_row*odd_sumtempt_odd_row<0,1,2)
          # sign用来记录even_sumtempt_even_row的正负符号
          sign=ifelse(even_sumtempt_even_row>0,1,-1)
          even_newsum_even_row=pattern*sign
        }
        
      }
      else{
        print('seq[3] must be 0,1,2 or 3')
        break
      }
    }
    
    # 最后合并成final_result
    sumtemp=matrix(nrow = n-16,ncol =nrow(path))
    sumtemp[seq(1,n-16,4),]=odd_sumtempt_odd_row
    sumtemp[seq(2,n-16,4),]=even_sumtempt_odd_row
    sumtemp[seq(3,n-16,4),]=odd_sumtempt_even_row
    sumtemp[seq(4,n-16,4),]=even_sumtempt_even_row
    
    newsum=matrix(nrow = n-16,ncol =nrow(path))
    newsum[seq(1,n-16,4),]=odd_newsum_odd_row
    newsum[seq(2,n-16,4),]=even_newsum_odd_row
    newsum[seq(3,n-16,4),]=odd_newsum_even_row
    newsum[seq(4,n-16,4),]=even_newsum_even_row
    #新指标
    newsum_index=c(newsum_index,colSums(newsum))
    
    sat=matrix(train(sumtemp =sumtemp))
    sat=sat+16
    # 满足的小于等于5的formula
    lis=c(lis,sat)
    if (m%%10==0) print(m)
  }
  final_newsum_index=matrix(newsum_index,byrow = T,ncol = nrow(path))
  colnames(final_newsum_index)=c(paste0('formula',17:24))
  # 交集
  t=table(lis)
  order_optimal=sort(t,decreasing = T)
  return(list(order_optimal,final_newsum_index))
}


# plan11,偶数轨偶数行根据偶数轨的奇数行对冲（与最初始的偶数轨偶数行）
res=run(data=all_data,path=path,minus_path=minus_path,seq = c(1,1,2))
# 查看下formula8的结果： 最优7：558
res[[2]][1:10,8]
# 计算负的个数
res[[1]]
for (i in 1:8) {
  print(paste0('formula ',i,' 的新指标中负的个数:',sum(res[[2]][,i]<0)))
}

# plan 13
res1=run(data=all_data,path=path,minus_path=minus_path,seq = c(0,1,2))
# 查看下formula8的结果：
res1[[2]][1:10,3]
# 计算负的个数
res1[[1]]
for (i in 1:8) {
  print(paste0('formula ',i,' 的新指标中负的个数:',sum(res1[[2]][,i]<0)))
}

# plan 3：
res2=run(data=all_data,path=path,minus_path=minus_path,seq = c(1,0,3))
# 计算负的个数
res2[[1]]
for (i in 1:8) {
  print(paste0('formula ',i,' 的新指标中负的个数:',sum(res2[[2]][,i]<0)))
}

# 没有反一反的：
res3=run(data=all_data,path=path,minus_path=minus_path,seq = c(0,0,0))
# 计算负的个数
res3[[1]]
for (i in 1:8) {
  print(paste0('formula ',i,' 的新指标中负的个数:',sum(res3[[2]][,i]<0)))
}

# 3、预测---------------------------------------------
pred_unsatisfy_num=function(index,data=data,path=path,minus_path=minus_path,seq=c(1,1,2)){
  index=index-16
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
    # 偶数fianl_comb
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
    # 计算【量化】后结果
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
    # 求formulaA或者B下的final_result
    {
      # 1：奇数轨奇数行；2：偶数轨偶数行；3：奇数轨偶数行；4：偶数轨偶数行
      odd_sumtempt=odd_R9_quantity+odd_R7_quantity+odd_R5_quantity
      
      odd_minus_sumtempt=odd_minus_R9_quantity+odd_minus_R7_quantity+odd_minus_R5_quantity
      
      even_sumtempt=even_R9_quantity+even_R7_quantity+even_R5_quantity
      
      even_minus_sumtempt=even_minus_R9_quantity+even_minus_R7_quantity+even_minus_R5_quantity
      
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
    sumtemp=matrix(nrow = n-16,ncol =nrow(path))
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

unsatisfy_num1=pred_unsatisfy_num(index=21,data=all_data,path=path,minus_path=minus_path,seq=c(1,0,3))

new_dis_unsatisfy=function(unsatisfy_num){
  k=unsatisfy_num[which.max(unsatisfy_num)]
  for(i in 4:k){
    print(paste0('连续负为',i,'的数据集个数：',length(which(unsatisfy_num==i))))
  }
}

new_dis_unsatisfy(unsatisfy_num1)


