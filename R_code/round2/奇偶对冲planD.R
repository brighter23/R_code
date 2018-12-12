# 1、产生formula的排列--------------------------------------------

{
  #定义函数
  arrang=function(x){
    #a是行数，b是列数,表示有几个备选
    a=dim(x)[1];b=dim(x)[2]
    result=matrix(nrow =a,ncol =b^a  )
    for (i in 1:a) {
      result[i,]= rep(rep(x[i,],b^(i-1)),c(rep(b^(a-i),b^i)))
    }
    return(result)
  }
  #9R
  a11='70-80-90|07-08-09';a13='70-80-81|07-08-18'
  a21='70-71-72|07-17-27';a23='70-71-81|07-17-18'   
  a31='61-62-72|16-26-27';a33='61-62-63|16-26-36'
  a41='61-71-81|16-17-18';a43='61-71-72|16-17-27'
  a51='52-53-54|25-35-45';a53='52-53-63|25-35-36'
  a61='52-62-72|25-26-27';a63='52-62-63|25-26-36'
  a71='43-44-45|34-44-54';a73='43-44-54|34-44-45'
  a81='43-53-54|34-35-45';a83='43-53-63|34-35-36'
  
  a=matrix(c(a11,a21,a31,a41,a51,a61,a71,a81,
             a13,a23,a33,a43,a53,a63,a73,a83),nrow = 8)
  
  new_a=matrix(c(a13,a23,a33,a43,a53,a63,a73,a83,
                 a11,a21,a31,a41,a51,a61,a71,a81),nrow = 8)
  #7R
  b11='50-60-70|05-06-07';b13='50-60-61|05-06-16'
  b21='50-51-61|05-15-16';b23='50-51-52|05-15-25'
  b31='41-42-52|14-24-25';b33='41-42-43|14-24-34'
  b41='41-51-61|14-15-16';b43='41-51-52|14-15-25'
  b51='32-33-43|23-33-34';b53='32-33-34|23-33-43'
  b61='32-42-52|23-24-25';b63='32-42-43|23-24-34'
  b=matrix(c(b11,b21,b31,b41,b51,b61,
             b13,b23,b33,b43,b53,b63),nrow = 6)
  
  new_b=matrix(c(b13,b23,b33,b43,b53,b63,
                 b11,b21,b31,b41,b51,b61),nrow = 6)
  #5R
  c11='30-31-32|03-13-23';c13='30-31-41|03-13-14'
  c21='30-40-41|03-04-14';c23='30-40-50|03-04-05'
  c31='21-22-32|12-22-23';c33='21-22-23|12-22-32'
  c41='21-31-41|12-13-14';c43='21-31-32|12-13-23'
  c=matrix(c(c11,c21,c31,c41,
             c13,c23,c33,c43),nrow = 4)
  
  new_c=matrix(c(c13,c23,c33,c43,
                 c11,c21,c31,c41),nrow = 4)
  
  pinjie=function(x){
    return(str_c(x,collapse = "|"))
  }
  
  f9_final_formular=apply(arrang(a),2,pinjie)
  f7_final_formular=apply(arrang(b),2,pinjie)
  f5_final_formular=apply(arrang(c),2,pinjie)
  
  new_f9_final_formular=apply(arrang(new_a),2,pinjie)
  new_f7_final_formular=apply(arrang(new_b),2,pinjie)
  new_f5_final_formular=apply(arrang(new_c),2,pinjie)
}
#结果放在a_final_formular,b_final_formular,c_final_formular中
c=c('f5_final_formular','new_f5_final_formular','f7_final_formular','new_f7_final_formular','f9_final_formular','new_f9_final_formular')
a=ls()
rm(list=(setdiff(a,c)))
gc()

# 2、原始数据集分开：
if(!require(stringr))install.packages('stringr')
library(stringr)
data=read.csv('data\\treat_two_hundred.csv',header = T)
n=nrow(data)
odd_data=data[seq(1,n,2),]
even_data=data[seq(2,n,2),]
# 量化函数
quantize=function(x,y,n=n){
  r=matrix(rep(-1,length(x)*n),ncol = length(x),nrow = n)
  for (i in 1:length(x)) {
    r[which(str_detect(y,x[i])),i]=1
  }
  return(r)
}


train=function(m){
  optimal=list()#用来存放9R，7R，5R对应的列数
  n=nrow(data)-16
  #例如1，2，3则表示是9R的第一个组合加7R的第2个组合，加5R的第3个组合
  for (i in 1:16) {
    for (j in 1:64) {
      for (k in 1:256) {
        sumtemp=R9_final_quantity[,k]+R7_final_quantity[,j]+R5_final_quantity[,i]
        #检验偶数行是否准确修改sumtemp[seq(2,n,2)]==even_final_result[seq(2,n,2),,drop=F]
        for (l in 1:(n-5)){
          if(sumtemp[l]<0&sumtemp[l+1]<0&sumtemp[l+2]<0&sumtemp[l+3]<0&sumtemp[l+4]<0&sumtemp[l+5]<0) break
          else if(l==(n-5)) optimal=append(optimal,paste0(i,'-',j,'-',k))
        }
      }
    }
  }
  return(matrix(unlist(optimal)))
}

# 注意这里偶数行根据奇数行结果决定是否使用formulaA或者B
even_change=function(odd_quantity,quantity,minus_quantity){
  even_final_result=quantity
  for (j in 1:ncol(quantity)) {
    for (i in 1:even_n) {
      even_final_result[i,j]=ifelse(odd_quantity[i,j]==-1,quantity[i,j],minus_quantity[i,j])
    }
  }
  return(even_final_result)
}

for (m in 1:length(data)) {
  # 产生奇数行和偶数行fianl_comb
  { odd_n=nrow(odd_data)-8     
    odd_final_comb=matrix(ncol = 3,nrow = odd_n)
    #求'9R','7R','5R'
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
    odd_R9_quantity=quantize(f9_final_formular,odd_final_comb[,1],odd_n)
    #odd_minus_R9_quantity=quantize(new_f9_final_formular,odd_final_comb[,1],odd_n)
    
    even_R9_quantity=quantize(f9_final_formular,even_final_comb[,1],even_n)
    even_minus_R9_quantity=quantize(new_f9_final_formular,even_final_comb[,1],even_n)
    
    odd_R7_quantity=quantize(f7_final_formular,odd_final_comb[,2],odd_n)
    #odd_minus_R7_quantity=quantize(new_f7_final_formular,odd_final_comb[,2],odd_n)
    
    even_R7_quantity=quantize(f7_final_formular,even_final_comb[,2],even_n)
    even_minus_R7_quantity=quantize(new_f7_final_formular,even_final_comb[,2],even_n)
    
    odd_R5_quantity=quantize(f5_final_formular,odd_final_comb[,3],odd_n)
    #odd_minus_R5_quantity=quantize(new_f5_final_formular,odd_final_comb[,3],odd_n)
    
    even_R5_quantity=quantize(f5_final_formular,even_final_comb[,3],even_n)
    even_minus_R5_quantity=quantize(new_f5_final_formular,even_final_comb[,3],even_n)
    
  }
  R9_final_quantity=matrix(nrow = n-16,ncol =256)
  R7_final_quantity=matrix(nrow = n-16,ncol =64)
  R5_final_quantity=matrix(nrow = n-16,ncol =16)
  
  R9_final_quantity[seq(1,n-16,2),]=odd_R9_quantity
  R9_final_quantity[seq(2,n-16,2),]=even_change(odd_R9_quantity,even_R9_quantity,even_minus_R9_quantity)
  
  R7_final_quantity[seq(1,n-16,2),]=odd_R7_quantity
  R7_final_quantity[seq(2,n-16,2),]=even_change(odd_R7_quantity,even_R7_quantity,even_minus_R7_quantity)
  
  R5_final_quantity[seq(1,n-16,2),]=odd_R5_quantity
  R5_final_quantity[seq(2,n-16,2),]=even_change(odd_R5_quantity,even_R5_quantity,even_minus_R5_quantity)
  
  
  # 代入训练函数
  assign(paste0('optimal',m),matrix(train(m)))
  print(m)
}


## 预测-----------------------------------------------------------

op_formula=read.csv('result\\op_formula_190_data_final_E.csv',stringsAsFactors = F,header = F)
library(stringr)
opsite_formula=function(formula){
  o1=new_f5_final_formular[which(f5_final_formular==formula[,1])]
  o2=new_f7_final_formular[which(f7_final_formular==formula[,2])]
  o3=new_f9_final_formular[which(f9_final_formular==formula[,3])]
  opsite_op_formula=data.frame(o1,o2,o3,stringsAsFactors = F)
  return(opsite_op_formula)
} 
odd_data=data[seq(1,68,2),]
even_data=data[seq(2,68,2),]
pred_unsatisfyD=function(index,data=data,old_formula,new_formula){
  n=nrow(data)-16
  final_sum_5=matrix(nrow = n)
  final_sum_6=matrix(nrow = n)
  final_sum_7=matrix(nrow = n)
  final_sum_8=matrix(nrow = n)
  final_sum_9=matrix(nrow = n)
  final_sum_10=matrix(nrow = n)
  for (m in 1:length(data)) {
    # 产生奇数行和偶数行fianl_comb
    { odd_n=nrow(odd_data)-8     
    odd_final_comb=matrix(ncol = 3,nrow = odd_n)
    #求'9R','7R','5R'
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
      odd_R9_quantity=quantize(old_formula[index,3],odd_final_comb[,1],odd_n)
      #odd_minus_R9_quantity=quantize(new_f9_final_formular,odd_final_comb[,1],odd_n)
      
      even_R9_quantity=quantize(old_formula[index,3],even_final_comb[,1],even_n)
      even_minus_R9_quantity=quantize(new_formula[index,3],even_final_comb[,1],even_n)
      
      odd_R7_quantity=quantize(old_formula[index,2],odd_final_comb[,2],odd_n)
      #odd_minus_R7_quantity=quantize(new_f7_final_formular,odd_final_comb[,2],odd_n)
      
      even_R7_quantity=quantize(old_formula[index,2],even_final_comb[,2],even_n)
      even_minus_R7_quantity=quantize(new_formula[index,2],even_final_comb[,2],even_n)
      
      odd_R5_quantity=quantize(old_formula[index,1],odd_final_comb[,3],odd_n)
      #odd_minus_R5_quantity=quantize(new_f5_final_formular,odd_final_comb[,3],odd_n)
      
      even_R5_quantity=quantize(old_formula[index,1],even_final_comb[,3],even_n)
      even_minus_R5_quantity=quantize(new_formula[index,1],even_final_comb[,3],even_n)
      
    }
    {
      R9_final_quantity=matrix(nrow = n,ncol =1)
      R7_final_quantity=matrix(nrow = n,ncol =1)
      R5_final_quantity=matrix(nrow = n,ncol =1)
      
      R9_final_quantity[seq(1,n,2),]=odd_R9_quantity
      R9_final_quantity[seq(2,n,2),]=even_change(odd_R9_quantity,even_R9_quantity,even_minus_R9_quantity)
      
      R7_final_quantity[seq(1,n,2),]=odd_R7_quantity
      R7_final_quantity[seq(2,n,2),]=even_change(odd_R7_quantity,even_R7_quantity,even_minus_R7_quantity)
      
      R5_final_quantity[seq(1,n,2),]=odd_R5_quantity
      R5_final_quantity[seq(2,n,2),]=even_change(odd_R5_quantity,even_R5_quantity,even_minus_R5_quantity)
    }
   
    sumtemp=R9_final_quantity+R7_final_quantity+ R5_final_quantity
    # 大于等于5
    for (l in 1:(n-4)){
      if(sumtemp[l]<0&sumtemp[l+1]<0&sumtemp[l+2]<0&sumtemp[l+3]<0&sumtemp[l+4]<0){
        colnames(sumtemp)=c(paste0('data',m))
        final_sum_5=cbind(final_sum_5,sumtemp)
        break
      }
    }
    # 大于等于6 的数据
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
    
    for (l in 1:(n-9)){
      if(sumtemp[l]<0&sumtemp[l+1]<0&sumtemp[l+2]<0&sumtemp[l+3]<0&sumtemp[l+4]<0&sumtemp[l+5]<0&sumtemp[l+6]<0&sumtemp[l+7]<0&sumtemp[l+8]<0&sumtemp[l+9]<0){
        colnames(sumtemp)=c(paste0('data',m))
        final_sum_10=cbind(final_sum_10,sumtemp)
        break
      }
    }
    
  }
  return(list(final_sum_5[,-1,drop=F],final_sum_6[,-1,drop=F],final_sum_7[,-1,drop=F],final_sum_8[,-1,drop=F],final_sum_9[,-1,drop=F],final_sum_10[,-1,drop=F]))#drop=F，避免因为只有一个数据时自动变成向量而失去列名
}
for (i in 1:nrow(op_formula)) {
  # 注意这里formula的循环已经体现在op_formula[i,]，因此不需要再index=i，而是恒定为1
  tempt=pred_unsatisfyD(1,data,op_formula[i,],opsite_formula(op_formula[i,]))
  assign(paste0('error_formular_',i,'_final_result'),list(tempt))
} 

# formula1的不满足分布：

dis_unsatisfy=function(formular_final_result){
  for (k in 2:6){
    num=ncol(formular_final_result[[k-1]])-ncol(formular_final_result[[k]])
    print(paste0('连续负的个数为',k+3,'的样本个数：',num))
  }
  num=ncol(formular_final_result[[6]])
  print(paste0('连续负的个数为超过10个的样本个数：',num))
}
dis_unsatisfy(error_formular_1_final_result[[1]])



























