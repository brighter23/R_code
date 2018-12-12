
#1、取前64个数据，得final_result有63个-----------------------------------
data=read.csv('data\\treat_two_hundred.csv',header = T)
#data=data[,31:51]
if(!require(stringr))install.packages('stringr')
library(stringr)
#2、产生formula的排列----------------------------------------------------

#定义排列组合函数
arrang=function(x){
  #a是行数，b是列数,表示有几个备选
  a=dim(x)[1];b=dim(x)[2]
  result=matrix(nrow =a,ncol =b^a  )
  for (i in 1:a) {
    result[i,]= rep(rep(x[i,],b^(i-1)),c(rep(b^(a-i),b^i)))
  }
  return(result)
}
# 5 7 9R-----------------------------------------------------------------
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
#7R
b11='50-60-70|05-06-07';b13='50-60-61|05-06-16'
b21='50-51-61|05-15-16';b23='50-51-52|05-15-25'
b31='41-42-52|14-24-25';b33='41-42-43|14-24-34'
b41='41-51-61|14-15-16';b43='41-51-52|14-15-25'
b51='32-33-43|23-33-34';b53='32-33-34|23-33-43'
b61='32-42-52|23-24-25';b63='32-42-43|23-24-34'
b=matrix(c(b11,b21,b31,b41,b51,b61,
           b13,b23,b33,b43,b53,b63),nrow = 6)
#5R
c11='30-31-32|03-13-23';c13='30-31-41|03-13-14'
c21='30-40-41|03-04-14';c23='30-40-50|03-04-05'
c31='21-22-32|12-22-23';c33='21-22-23|12-22-32'
c41='21-31-41|12-13-14';c43='21-31-32|12-13-23'
c=matrix(c(c11,c21,c31,c41,
           c13,c23,c33,c43),nrow = 4)

pinjie=function(x){
  return(str_c(x,collapse = "|"))
}

f9_final_formular=apply(arrang(a),2,pinjie)
f7_final_formular=apply(arrang(b),2,pinjie)
f5_final_formular=apply(arrang(c),2,pinjie)

#结果放在f9_final_formular,f7_final_formular,f5_final_formular中
# 6 8 10 R----------------------------------------------------------------

#10R
a11='80-90-100|08-09-010';a12='80-90-91|08-09-19'
a21='80-81-82|08-18-28';a22='80-81-91|08-18-19'   
a31='71-72-82|17-27-28';a32='71-72-73|17-27-37'
a41='71-81-91|17-18-19';a42='71-81-82|17-18-28'
a51='62-63-64|26-36-46';a52='62-63-73|26-36-37'
a61='62-72-82|26-27-28';a62='62-72-73|26-27-37'
a71='53-54-55|35-45-55';a72='53-54-64|35-45-46'
a81='53-63-64|35-36-46';a82='53-63-73|35-36-37'
a91='44-54-55|44-45-55';a92='44-54-64|44-45-46'

a=matrix(c(a11,a21,a31,a41,a51,a61,a71,a81,a91,
           a12,a22,a32,a42,a52,a62,a72,a82,a92),nrow = 9)
#8R
b11='60-70-80|06-07-08';b12='60-70-71|06-07-17'
b21='60-61-71|06-16-17';b22='60-61-62|06-16-26'
b31='51-61-71|15-16-17';b32='51-61-62|15-16-26'
b41='51-52-53|15-25-35';b42='51-52-62|15-25-26'
b51='42-43-44|24-34-44';b52='42-43-53|24-34-35'
b61='42-52-62|24-25-26';b62='42-52-53|24-25-35'
b71='33-43-44|33-34-44';b72='33-43-53|33-34-35'
b=matrix(c(b11,b21,b31,b41,b51,b61,b71,
           b12,b22,b32,b42,b52,b62,b72),nrow = 7)
#6R
c11='40-50-60|04-05-06';c12='40-50-51|04-05-15'
c21='40-41-42|04-14-24';c22='40-41-51|04-14-15'
c31='31-32-33|13-23-33';c32='31-32-42|13-23-24'
c41='31-41-51|13-14-15';c42='31-41-42|13-14-24'
c51='22-32-33|22-23-33';c52='22-32-42|22-23-24'

c=matrix(c(c11,c21,c31,c41,c51,
           c12,c22,c32,c42,c52),nrow = 5)


f10_final_formular=apply(arrang(a),2,pinjie)
f8_final_formular=apply(arrang(b),2,pinjie)
f6_final_formular=apply(arrang(c),2,pinjie)
#str_c(paste0('\'','f',5:10,'_final_formular','\''),collapse = ',')
# 回收内存-------------------------------------------------------------
c=c('data','f5_final_formular','f6_final_formular','f7_final_formular','f8_final_formular','f9_final_formular','f10_final_formular')
a=ls()
rm(list=(setdiff(a,c)))
gc()


#3、找出各个样本满足约束得最优组合---------------------------------------

#定义量化函数:从formulary到正负1
quantize=function(x,y,n=n){
  r=matrix(rep(-1,length(x)*n),ncol = length(x),nrow = n)
  for (i in 1:length(x)) {
    r[which(str_detect(y,x[i])),i]=1
  }
  return(r)
}
n=nrow(data)-8
#每个数据集都得到一个最优得组合，例如190个就有190张表
# 奇数行-------------------------------------------------------------------

for (m in 1:length(data)) {
  #将'7~9R','5~7R',‘3~5R’合并为3个公式:分别以'**-**-**'的格式
  #每个数据集都要计算一次final_comb和对应fianl_quantity
  { 
    final_comb=matrix(ncol = 3,nrow = n)
    #求奇数行的'9R','7R','5R'
    for (i in seq(1,n,2)) {
      #使用table时注意只有因子的统计在频数为0时会显示
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
    #求偶数行的'10R','8R','6R'
    for (i in seq(2,n,2)) {
      t0=table(data[(i-1):(i+8),m])
      result0=paste0(t0[1],t0[2])
      t1=table(data[(i-1):(i+7),m])
      result1=paste0(t1[1],t1[2])
      t2=table(data[(i-1):(i+6),m])
      result2=paste0(t2[1],t2[2])
      comb1=paste0(result2,'-',result1,'-',result0)
      
      t3=table(data[(i+1):(i+8),m])
      result3=paste0(t3[1],t3[2])
      t4=table(data[(i+1):(i+7),m])
      result4=paste0(t4[1],t4[2])
      t5=table(data[(i+1):(i+6),m])
      result5=paste0(t5[1],t5[2])
      comb2=paste0(result5,'-',result4,'-',result3)
      
      t6=table(data[(i+3):(i+8),m])
      result6=paste0(t6[1],t6[2])
      t7=table(data[(i+3):(i+7),m])
      result7=paste0(t7[1],t7[2])
      t8=table(data[(i+3):(i+6),m])
      result8=paste0(t8[1],t8[2])
      comb3=paste0(result8,'-',result7,'-',result6)
      
      final_comb[i,]=matrix(c(comb1,comb2,comb3),ncol = 3)
    }
    colnames(final_comb)=c('9R','7R','5R')
  }
  #结果放在final_comb中 
  #根据final_comb量化formular
  f9_final_quantity=quantize(f9_final_formular,final_comb[seq(1,n,2),1],length(seq(1,n,2)))
  f7_final_quantity=quantize(f7_final_formular,final_comb[seq(1,n,2),2],length(seq(1,n,2)))
  f5_final_quantity=quantize(f5_final_formular,final_comb[seq(1,n,2),3],length(seq(1,n,2)))
  #4、开始训练出满足的组合
  train=function(m,n){
    h=length(seq(1,n,2))
    optimal=list()#用来存放9R，7R，5R对应的列数
    #例如1，2，3则表示是9R的第一个组合加7R的第2个组合，加5R的第3个组合
    for (i in 1:16) {
      for (j in 1:64) {
        for (k in 1:256) {
          sumtemp=f9_final_quantity[,k]+f7_final_quantity[,j]+f5_final_quantity[,i]
          for (l in 1:(h-2)){
            if(sumtemp[l]<0&sumtemp[l+1]<0&sumtemp[l+2]<0) break
            else if(l==(h-2)) optimal=append(optimal,paste0(i,'-',j,'-',k))
          }
        }
      }
    }
    return(matrix(unlist(optimal)))
  }
  assign(paste0('optimal',m),matrix(train(m,n)))
  print(m)
}

 

#4、取交集---------------------------------------------------------------
#把所有数据集的最优组合放在一起，求组合的频数,最高的即为最优的
# 奇数-------------------------------------------------------------------
{
  #不可以存放成矩阵，虽然使用table结果一样，但是矩阵要求长度一致
  #str_c(paste0("optimal",1:190),collapse = ',')
  m=c(optimal1,optimal2,optimal3,optimal4,optimal5,optimal6,optimal7,optimal8,optimal9,optimal10,optimal11,optimal12,optimal13,optimal14,optimal15,optimal16,optimal17,optimal18,optimal19,optimal20,optimal21,optimal22,optimal23,optimal24,optimal25,optimal26,optimal27,optimal28,optimal29,optimal30,optimal31,optimal32,optimal33,optimal34,optimal35,optimal36,optimal37,optimal38,optimal39,optimal40,optimal41,optimal42,optimal43,optimal44,optimal45,optimal46,optimal47,optimal48,optimal49,optimal50,optimal51,optimal52,optimal53,optimal54,optimal55,optimal56,optimal57,optimal58,optimal59,optimal60,optimal61,optimal62,optimal63,optimal64,optimal65,optimal66,optimal67,optimal68,optimal69,optimal70,optimal71,optimal72,optimal73,optimal74,optimal75,optimal76,optimal77,optimal78,optimal79,optimal80,optimal81,optimal82,optimal83,optimal84,optimal85,optimal86,optimal87,optimal88,optimal89,optimal90,optimal91,optimal92,optimal93,optimal94,optimal95,optimal96,optimal97,optimal98,optimal99,optimal100,optimal101,optimal102,optimal103,optimal104,optimal105,optimal106,optimal107,optimal108,optimal109,optimal110,optimal111,optimal112,optimal113,optimal114,optimal115,optimal116,optimal117,optimal118,optimal119,optimal120,optimal121,optimal122,optimal123,optimal124,optimal125,optimal126,optimal127,optimal128,optimal129,optimal130,optimal131,optimal132,optimal133,optimal134,optimal135,optimal136,optimal137,optimal138,optimal139,optimal140,optimal141,optimal142,optimal143,optimal144,optimal145,optimal146,optimal147,optimal148,optimal149,optimal150,optimal151,optimal152,optimal153,optimal154,optimal155,optimal156,optimal157,optimal158,optimal159,optimal160,optimal161,optimal162,optimal163,optimal164,optimal165,optimal166,optimal167,optimal168,optimal169,optimal170,optimal171,optimal172,optimal173,optimal174,optimal175,optimal176,optimal177,optimal178,optimal179,optimal180,optimal181,optimal182,optimal183,optimal184,optimal185,optimal186,optimal187,optimal188,optimal189,optimal190)
  t=table(m)
  #或者存放成list，然后table(unlist(m))
  order_optimal=sort(t,decreasing = T)
  opt=order_optimal[which(order_optimal==order_optimal[1])]
  formular=str_split(names(opt),pattern = '-')
  formular_index=lapply(formular, function(x)as.integer(x))
  op_formular=matrix(ncol = 3)
  for (i in formular_index) {
    op_formular=rbind(op_formular,c(f5_final_formular[i[1]],f7_final_formular[i[2]],f9_final_formular[i[3]]))
  }
  assign('op_formula_190_data_odd',data.frame(op_formular[-1,]))
  colnames(op_formula_190_data_odd)=c("第3列组合","第2列组合","第1列组合")
  write.csv(op_formula_190_data_odd,'result\\op_formula_190_data_odd.csv',row.names = F)
}
# 偶数-------------------------------------------------------------------
{ #str_c(paste0('optimal_even',1:length(data)),collapse = ',')
  m=c(optimal_even1,optimal_even2,optimal_even3,optimal_even4,optimal_even5,optimal_even6)
  t=table(m)
  order_optimal=sort(t,decreasing = T)
  opt=order_optimal[which(order_optimal==order_optimal[1])]
  formular=str_split(names(opt),pattern = '-')
  formular_index=lapply(formular, function(x)as.integer(x))
  op_formular=matrix(ncol = 3)
  for (i in formular_index) {
    op_formular=rbind(op_formular,c(f6_final_formular[i[1]],f8_final_formular[i[2]],f10_final_formular[i[3]]))
  }
  assign('op_formula_30_data_even',data.frame(op_formular[-1,]))
  colnames(op_formula_30_data_even)=c("第3列组合","第2列组合","第1列组合")
  write.csv(op_formula_30_data_even,'result\\op_formula_30_data_even.csv',row.names = F)
}

#5、诊断:将最优组合带入求final_result-----------------------------------------------------------------

rm(list=ls())
gc()
data=read.csv('data\\treat_two_hundred.csv',header = T)
if(!require(stringr))install.packages('stringr')
library(stringr)

op_formula_data_odd=read.csv('result\\op_formula_six_data_odd.csv',stringsAsFactors = F)
op_formula_data_even=read.csv('result\\op_formula_six_data_even.csv',stringsAsFactors = F)

n=nrow(data)-8
quantize=function(x,y,n=n){
  r=matrix(rep(-1,length(x)*n),ncol = length(x),nrow = n)
  for (i in 1:length(x)) {
    r[which(str_detect(y,x[i])),i]=1
  }
  return(r)
}

# 求所有的final_result
pred_all_odd=function(index,data=data,formula){
  n=nrow(data)-8
  h=ifelse(n%%2==0,n/2,(n-1)/2)
  h=h+1
  final_sum=matrix(nrow = h)
  for (m in 1:length(data)) {
    { 
      final_comb=matrix(ncol = 3,nrow = n)
      #求奇数行的'9R','7R','5R'
      for (i in seq(1,n,2)) {
        #使用table时注意只有因子的统计在频数为0时会显示
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
      #求偶数行的'10R','8R','6R'
      for (i in seq(2,n,2)) {
        t0=table(data[(i-1):(i+8),m])
        result0=paste0(t0[1],t0[2])
        t1=table(data[(i-1):(i+7),m])
        result1=paste0(t1[1],t1[2])
        t2=table(data[(i-1):(i+6),m])
        result2=paste0(t2[1],t2[2])
        comb1=paste0(result2,'-',result1,'-',result0)
        
        t3=table(data[(i+1):(i+8),m])
        result3=paste0(t3[1],t3[2])
        t4=table(data[(i+1):(i+7),m])
        result4=paste0(t4[1],t4[2])
        t5=table(data[(i+1):(i+6),m])
        result5=paste0(t5[1],t5[2])
        comb2=paste0(result5,'-',result4,'-',result3)
        
        t6=table(data[(i+3):(i+8),m])
        result6=paste0(t6[1],t6[2])
        t7=table(data[(i+3):(i+7),m])
        result7=paste0(t7[1],t7[2])
        t8=table(data[(i+3):(i+6),m])
        result8=paste0(t8[1],t8[2])
        comb3=paste0(result8,'-',result7,'-',result6)
        
        final_comb[i,]=matrix(c(comb1,comb2,comb3),ncol = 3)
      }
      colnames(final_comb)=c('9R','7R','5R')
    }
    f9_final_quantity=quantize(formula[index,3],final_comb[seq(1,n,2),1],length(seq(1,n,2)))
    f7_final_quantity=quantize(formula[index,2],final_comb[seq(1,n,2),2],length(seq(1,n,2)))
    f5_final_quantity=quantize(formula[index,1],final_comb[seq(1,n,2),3],length(seq(1,n,2)))
    sumtemp=f9_final_quantity+ f7_final_quantity+ f5_final_quantity
    final_sum=cbind(final_sum,sumtemp)
  }
    return(final_sum[,-1])
}

# 奇数行不满足——————————————————————————————————————————————————————----
odd_pred_unsatisfy=function(index,data=data,formula){
  n=nrow(data)-8
  h=length(seq(1,n,2))
  final_sum=matrix(nrow = h)
  for (m in 1:length(data)) {
    { 
      final_comb=matrix(ncol = 3,nrow = n)
      #求奇数行的'9R','7R','5R'
      for (i in seq(1,n,2)) {
        #使用table时注意只有因子的统计在频数为0时会显示
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
      #求偶数行的'10R','8R','6R'
      for (i in seq(2,n,2)) {
        t0=table(data[(i-1):(i+8),m])
        result0=paste0(t0[1],t0[2])
        t1=table(data[(i-1):(i+7),m])
        result1=paste0(t1[1],t1[2])
        t2=table(data[(i-1):(i+6),m])
        result2=paste0(t2[1],t2[2])
        comb1=paste0(result2,'-',result1,'-',result0)
        
        t3=table(data[(i+1):(i+8),m])
        result3=paste0(t3[1],t3[2])
        t4=table(data[(i+1):(i+7),m])
        result4=paste0(t4[1],t4[2])
        t5=table(data[(i+1):(i+6),m])
        result5=paste0(t5[1],t5[2])
        comb2=paste0(result5,'-',result4,'-',result3)
        
        t6=table(data[(i+3):(i+8),m])
        result6=paste0(t6[1],t6[2])
        t7=table(data[(i+3):(i+7),m])
        result7=paste0(t7[1],t7[2])
        t8=table(data[(i+3):(i+6),m])
        result8=paste0(t8[1],t8[2])
        comb3=paste0(result8,'-',result7,'-',result6)
        
        final_comb[i,]=matrix(c(comb1,comb2,comb3),ncol = 3)
      }
      colnames(final_comb)=c('9R','7R','5R')
    }
    f9_final_quantity=quantize(formula[index,3],final_comb[seq(1,n,2),1],length(seq(1,n,2)))
    f7_final_quantity=quantize(formula[index,2],final_comb[seq(1,n,2),2],length(seq(1,n,2)))
    f5_final_quantity=quantize(formula[index,1],final_comb[seq(1,n,2),3],length(seq(1,n,2)))
    sumtemp=f9_final_quantity+ f7_final_quantity+ f5_final_quantity
    for (l in 1:(h-2)){
      if(sumtemp[l]<0&sumtemp[l+1]<0&sumtemp[l+2]<0){
        colnames(sumtemp)=c(paste0('data',m))
        final_sum=cbind(final_sum,sumtemp)
        break
      }
    }
  }
  return(list(final_sum[,-1,drop=F],data_name=colnames(final_sum[,-1,drop=F])))#drop=F，避免因为只有一个数据时自动变成向量而失去列名
}
odd_unsatisfy_set_names=list(NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL)
for (i in 1:nrow(op_formula_data_odd)) {
  tempt=odd_pred_unsatisfy(i,data,op_formula_data_odd)
  odd_unsatisfy_set_names[i]=list(tempt[[2]])
  assign(paste0('odd_error_formular_',i,'_final_result'),data.frame(tempt[[1]]))
}

unique_names=unique(odd_unsatisfy_set_names)
final_unsatisfy=unique_names[[1]]
for (i in unique_names) {
  final_unsatisfy=intersect(final_unsatisfy,i)
}


# 偶数行不满足————————————————————————————————————————————————-----
even_pred_unsatisfy=function(index,data=data,formula){
  n=nrow(data)-8
  h=ifelse(n%%2==0,n/2,(n-1)/2)
  final_sum=matrix(nrow = h)
  for (m in 1:length(data)) {
    { 
      final_comb=matrix(ncol = 3,nrow = n)
      #求奇数行的'9R','7R','5R'
      for (i in seq(1,n,2)) {
        #使用table时注意只有因子的统计在频数为0时会显示
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
      #求偶数行的'10R','8R','6R'
      for (i in seq(2,n,2)) {
        t0=table(data[(i-1):(i+8),m])
        result0=paste0(t0[1],t0[2])
        t1=table(data[(i-1):(i+7),m])
        result1=paste0(t1[1],t1[2])
        t2=table(data[(i-1):(i+6),m])
        result2=paste0(t2[1],t2[2])
        comb1=paste0(result2,'-',result1,'-',result0)
        
        t3=table(data[(i+1):(i+8),m])
        result3=paste0(t3[1],t3[2])
        t4=table(data[(i+1):(i+7),m])
        result4=paste0(t4[1],t4[2])
        t5=table(data[(i+1):(i+6),m])
        result5=paste0(t5[1],t5[2])
        comb2=paste0(result5,'-',result4,'-',result3)
        
        t6=table(data[(i+3):(i+8),m])
        result6=paste0(t6[1],t6[2])
        t7=table(data[(i+3):(i+7),m])
        result7=paste0(t7[1],t7[2])
        t8=table(data[(i+3):(i+6),m])
        result8=paste0(t8[1],t8[2])
        comb3=paste0(result8,'-',result7,'-',result6)
        
        final_comb[i,]=matrix(c(comb1,comb2,comb3),ncol = 3)
      }
      colnames(final_comb)=c('9R','7R','5R')
    }
    f9_final_quantity=quantize(formula[index,3],final_comb[seq(2,n,2),1],length(seq(2,n,2)))
    f7_final_quantity=quantize(formula[index,2],final_comb[seq(2,n,2),2],length(seq(2,n,2)))
    f5_final_quantity=quantize(formula[index,1],final_comb[seq(2,n,2),3],length(seq(2,n,2)))
    sumtemp=f9_final_quantity+ f7_final_quantity+ f5_final_quantity
    for (l in 1:(h-2)){
      if(sumtemp[l]<0&sumtemp[l+1]<0&sumtemp[l+2]<0){
        colnames(sumtemp)=c(paste0('data',m))
        final_sum=cbind(final_sum,sumtemp)
        break
      }
    }
  }
  return(list(final_sum[,-1,drop=F],data_name=colnames(final_sum[,-1,drop=F])))#drop=F，避免因为只有一个数据时自动变成向量而失去列名
}
even_unsatisfy_set_names=list(NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                              NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL)
for (i in 1:nrow(op_formula_data_even)) {
  tempt=even_pred_unsatisfy(i,data,op_formula_data_even)
  even_unsatisfy_set_names=list(tempt[[2]])
  assign(paste0('even_error_formular_',i,'_final_result'),data.frame(tempt[[1]]))
}

even_unique_names=unique(even_unsatisfy_set_names)
intersect(unique_names[[1]],unique_names[[2]])












#将不满足最多连续5个负数的提取出来-----------------------
pred_unsatisfy_even=function(index,data=data,formula){
  n=nrow(data)-8
  h=ifelse(n%%2==0,n/2,(n-1)/2)
  final_sum=matrix(nrow = h)
  for (m in 1:length(data)) {
    #将'7~9R','5~7R',‘3~5R’合并为3个公式:分别以'**-**-**'的格式
    #每个数据集都要计算一次final_comb和对应fianl_quantity
    { 
      final_comb=matrix(ncol = 3,nrow = h)
      for (i in 1:h) {
        t0=table(data[(i-1):(i+8),m])
        result0=paste0(t0[1],t0[2])
        t1=table(data[(i-1):(i+7),m])
        result1=paste0(t1[1],t1[2])
        t2=table(data[(i-1):(i+6),m])
        result2=paste0(t2[1],t2[2])
        comb1=paste0(result2,'-',result1,'-',result0)
        
        t3=table(data[(i+1):(i+8),m])
        result3=paste0(t3[1],t3[2])
        t4=table(data[(i+1):(i+7),m])
        result4=paste0(t4[1],t4[2])
        t5=table(data[(i+1):(i+6),m])
        result5=paste0(t5[1],t5[2])
        comb2=paste0(result5,'-',result4,'-',result3)
        
        t6=table(data[(i+3):(i+8),m])
        result6=paste0(t6[1],t6[2])
        t7=table(data[(i+3):(i+7),m])
        result7=paste0(t7[1],t7[2])
        t8=table(data[(i+3):(i+6),m])
        result8=paste0(t8[1],t8[2])
        comb3=paste0(result8,'-',result7,'-',result6)
        
        final_comb[i,]=matrix(c(comb1,comb2,comb3),ncol = 3)
      }
      colnames(final_comb)=c('9R','7R','5R')
    }
    f9_final_quantity=quantize(formula[index,3],final_comb[,1],h)
    f7_final_quantity=quantize(formula[index,2],final_comb[,2],h)
    f5_final_quantity=quantize(formula[index,1],final_comb[,3],h)
    
    sumtemp=f9_final_quantity+ f7_final_quantity+ f5_final_quantity
    for (l in 1:(n-5)){
      if(sumtemp[l]<0&sumtemp[l+1]<0&sumtemp[l+2]<0&sumtemp[l+3]<0&sumtemp[l+4]<0&sumtemp[l+5]<0){
        colnames(sumtemp)=paste0('data',m)
        final_sum=cbind(final_sum,sumtemp)
        break
      }
    }
  }
  return(final_sum[,-1])
}

for (i in 1:nrow(op_formula_six_data_odd)) {
  assign(paste0('error_formular_',i,'_final_result_odd'),data.frame(pred_unsatisfy_odd(i,data,op_formula_six_data_odd)))
}





#使用27个数据集预测得到score1
data_27=read.csv('term2//data_27.csv')
max_satisfied=function(seq,data=data){
  acc=c()
  n=nrow(data)-8
  #定义量化函数
  for (j in seq) {
    #final_sum=matrix(nrow = n)
    accurate=0#符合条件的个数
    for (m in 1:length(data)) {
      #将'7~9R','5~7R',‘3~5R’合并为3个公式:以'**-**-**'的格式
      {       
        #data0=as.data.frame(data[,m])
        final_comb=matrix(ncol = 3)
        #求'9R','7R','5R'
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
          
          comb=matrix(c(comb1,comb2,comb3),ncol = 3)
          final_comb=rbind(final_comb,comb)
        }
        final_comb=final_comb[-1,]
        colnames(final_comb)=c('9R','7R','5R')
      }
      #结果放在final_comb中 
      #根据final_comb量化formular
      
      a_final_quantity=quantize(merge_[j,3],final_comb[,1],n)
      b_final_quantity=quantize(merge_[j,2],final_comb[,2],n)
      c_final_quantity=quantize(merge_[j,1],final_comb[,3],n)
      sumtemp=a_final_quantity+b_final_quantity+c_final_quantity
      for (l in 1:(n-5)){
        if(sumtemp[l]<0&sumtemp[l+1]<0&sumtemp[l+2]<0&sumtemp[l+3]<0&sumtemp[l+4]<0&sumtemp[l+5]<0) break
        else if(l==(n-5)) accurate=accurate+1 
      }
      
      #final_sum=cbind(final_sum,sumtemp)
    }
    print(paste0('第',j,'个组合符合条件个数：',accurate))
    acc[j]=accurate
    #assign(paste0('formular_',j,'_final_result'),data.frame(final_sum[,-1]))
  }
  return(acc)
}
k=max_satisfied(1:16,data_27)
score1=k/2
#根据47个预测错误中满足最多连续7个负的比例得score2
pred3=function(seq,data=data,merge_){
  c=c()
  n=nrow(data)-8
  for (j in seq) {
    final_sum=matrix(nrow = n)
    for (m in 1:length(data)) {
      #将'7~9R','5~7R',‘3~5R’合并为3个公式:以'**-**-**'的格式
      {       
        #data0=as.data.frame(data[,m])
        final_comb=matrix(ncol = 3)
        #求'9R','7R','5R'
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
          
          comb=matrix(c(comb1,comb2,comb3),ncol = 3)
          final_comb=rbind(final_comb,comb)
        }
        final_comb=final_comb[-1,]
        colnames(final_comb)=c('9R','7R','5R')
      }
      #结果放在final_comb中 
      #根据final_comb量化formular
      a_final_quantity=quantize(merge_[j,3],final_comb[,1],n)
      b_final_quantity=quantize(merge_[j,2],final_comb[,2],n)
      c_final_quantity=quantize(merge_[j,1],final_comb[,3],n)
      sumtemp=a_final_quantity+b_final_quantity+c_final_quantity
      for (l in 1:(n-7)){
        if(sumtemp[l]<0&sumtemp[l+1]<0&sumtemp[l+2]<0&sumtemp[l+3]<0&sumtemp[l+4]<0&sumtemp[l+5]<0&sumtemp[l+6]<0&sumtemp[l+7]<0){
          colnames(sumtemp)=paste0('data',m)
          final_sum=cbind(final_sum,sumtemp)
          break
        }
      }
    }
    c[j]=ncol(final_sum[,-1])
  } 
  return(c)
}
k2=pred3(1:16,data,merge_)
#k1=pred3(1:16,merge_)#这是47个中连续7个以上为负的得数据集个数
score2=(47-k2)/47
score=score1+score2
op=merge_[which(score==score[which.max(score)]),]



#6、带入新数据集-----------------------------------------------------------


new=read.csv('term2\\tempt.csv',header = F,stringsAsFactors = F)
new=lapply(new, function(x)str_replace(x,pattern = '-',''))
R9=matrix(unlist(new[1:3]),ncol = 3)
R9=apply(R9, 1, function(x) str_c(x,collapse ='-'))
R7=matrix(unlist(new[4:6]),ncol = 3)
R7=apply(R7, 1, function(x) str_c(x,collapse ='-'))
R5=matrix(unlist(new[7:9]),ncol = 3)
R5=apply(R5, 1, function(x) str_c(x,collapse ='-'))

final_comb=data.frame(R9=R9,R7=R7,R5=R5)

hR9=matrix(unlist(new[10:12]),ncol = 3)
hR9=apply(hR9, 1, function(x) str_c(x,collapse ='-'))
hR7=matrix(unlist(new[13:15]),ncol = 3)
hR7=apply(hR7, 1, function(x) str_c(x,collapse ='-'))
hR5=matrix(unlist(new[16:18]),ncol = 3)
hR5=apply(hR5, 1, function(x) str_c(x,collapse ='-'))

hfinal_comb=data.frame(R9=hR9,R7=hR7,R5=hR5)

op=data.frame(c("30-31-41|03-04-14|30-40-50|03-04-05|21-22-32|12-22-23|21-31-32|12-13-23",
                "30-31-41|03-04-14|30-40-50|03-04-05|21-22-32|12-22-23|21-31-32|12-13-23")
              ,c('50-60-61|05-06-16|50-51-61|05-15-16|41-42-43|14-24-34|41-51-52|14-15-25|32-33-43|23-33-34|32-42-43|23-24-34',
                 '50-60-61|05-06-16|50-51-61|05-15-16|41-42-43|14-24-34|41-51-52|14-15-25|32-33-43|23-33-34|32-42-43|23-24-34')
              ,c('70-80-81|07-08-18|70-71-72|07-17-27|61-62-63|16-26-36|61-71-81|16-17-18|52-53-54|25-35-45|52-62-72|25-26-27|43-44-45|34-44-54|43-53-63|34-35-36',
                 '70-80-90|07-08-09|70-71-72|07-17-27|61-62-63|16-26-36|61-71-81|16-17-18|52-53-54|25-35-45|52-62-72|25-26-27|43-44-45|34-44-54|43-53-63|34-35-36')
              ,stringsAsFactors = F)
op=read.csv('term2\\op_formula_3.5.csv',stringsAsFactors = F)


#组合1
a_final_quantity=quantize(op[1,3],final_comb[,1],512)
b_final_quantity=quantize(op[1,2],final_comb[,2],512)
c_final_quantity=quantize(op[1,1],final_comb[,3],512)
sumtemp1=a_final_quantity+b_final_quantity+c_final_quantity
write.csv(sumtemp1,'term2\\sumtemp1-1888.csv')

a_final_quantity=quantize(op[1,3],hfinal_comb[,1],512)
b_final_quantity=quantize(op[1,2],hfinal_comb[,2],512)
c_final_quantity=quantize(op[1,1],hfinal_comb[,3],512)
hsumtemp1=a_final_quantity+b_final_quantity+c_final_quantity
write.csv(hsumtemp1,'term2\\hsumtemp1-1888.csv')

#组合2
a_final_quantity=quantize(op[2,3],final_comb[,1],512)
b_final_quantity=quantize(op[2,2],final_comb[,2],512)
c_final_quantity=quantize(op[2,1],final_comb[,3],512)
sumtemp2=a_final_quantity+b_final_quantity+c_final_quantity
write.csv(sumtemp2,'term2\\sumtemp2-1889.csv')

a_final_quantity=quantize(op[2,3],hfinal_comb[,1],512)
b_final_quantity=quantize(op[2,2],hfinal_comb[,2],512)
c_final_quantity=quantize(op[2,1],hfinal_comb[,3],512)
hsumtemp2=a_final_quantity+b_final_quantity+c_final_quantity
write.csv(hsumtemp2,'term2\\hsumtemp2-1889.csv')

