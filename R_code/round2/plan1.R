# 1、formula准备------------------------------------------
data=read.csv('data\\treat_two_hundred.csv',header = T)
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
train=function(R9_quantity=R9_quantity,R7_quantity=R7_quantity,R5_quantity=R5_quantity){
  optimal=list()
  sumtemp=R9_quantity+R7_quantity+R5_quantity
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

n=nrow(data)-8
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
  #minus_R9_quantity=minus_quantize(f9_final_formular,final_comb[,1],n)
  
  R7_quantity=new_quantize(path = path,real_comb=final_comb,set=data[,m])[[2]]
  #minus_R7_quantity=minus_quantize(f7_final_formular,final_comb[,2],n)
  
  R5_quantity=new_quantize(path = path,real_comb=final_comb,set=data[,m])[[3]]
  #minus_R5_quantity=minus_quantize(f5_final_formular,final_comb[,3],n)
  
  
  # R9_final_quantity=enen_change(R9_quantity,minus_R9_quantity)
  # R7_final_quantity=enen_change(R7_quantity,minus_R7_quantity)
  # R5_final_quantity=enen_change(R5_quantity,minus_R5_quantity)
  
  # 代入训练函数
  assign(paste0('optimal',m),matrix(train(R9_quantity = R9_quantity ,R7_quantity = R7_quantity,R5_quantity = R5_quantity )))
  print(m)
}

# 3、交集----------------------------------------------
{
  {
    m=c(optimal1,optimal2,optimal3,optimal4,optimal5,optimal6,optimal7,optimal8,optimal9,optimal10,optimal11,optimal12,optimal13,optimal14,optimal15,optimal16,optimal17,optimal18,optimal19,optimal20,optimal21,optimal22,optimal23,optimal24,optimal25,optimal26,optimal27,optimal28,optimal29,optimal30,optimal31,optimal32,optimal33,optimal34,optimal35,optimal36,optimal37,optimal38,optimal39,optimal40,optimal41,optimal42,optimal43,optimal44,optimal45,optimal46,optimal47,optimal48,optimal49,optimal50,optimal51,optimal52,optimal53,optimal54,optimal55,optimal56,optimal57,optimal58,optimal59,optimal60,optimal61,optimal62,optimal63,optimal64,optimal65,optimal66,optimal67,optimal68,optimal69,optimal70,optimal71,optimal72,optimal73,optimal74,optimal75,optimal76,optimal77,optimal78,optimal79,optimal80,optimal81,optimal82,optimal83,optimal84,optimal85,optimal86,optimal87,optimal88,optimal89,optimal90,optimal91,optimal92,optimal93,optimal94,optimal95,optimal96,optimal97,optimal98,optimal99,optimal100,optimal101,optimal102,optimal103,optimal104,optimal105,optimal106,optimal107,optimal108,optimal109,optimal110,optimal111,optimal112,optimal113,optimal114,optimal115,optimal116,optimal117,optimal118,optimal119,optimal120,optimal121,optimal122,optimal123,optimal124,optimal125,optimal126,optimal127,optimal128,optimal129,optimal130,optimal131,optimal132,optimal133,optimal134,optimal135,optimal136,optimal137,optimal138,optimal139,optimal140,optimal141,optimal142,optimal143,optimal144,optimal145,optimal146,optimal147,optimal148,optimal149,optimal150,optimal151,optimal152,optimal153,optimal154,optimal155,optimal156,optimal157,optimal158,optimal159,optimal160,optimal161,optimal162,optimal163,optimal164,optimal165,optimal166,optimal167,optimal168,optimal169,optimal170,optimal171,optimal172,optimal173,optimal174,optimal175,optimal176,optimal177,optimal178,optimal179,optimal180,optimal181,optimal182,optimal183,optimal184,optimal185,optimal186,optimal187,optimal188,optimal189)
    t=table(m)
  }
  #或者存放成list，然后table(unlist(m))
  order_optimal=sort(t,decreasing = T)
  opt=order_optimal[which(order_optimal==order_optimal[1])]
  path[6,]# B9,B7,A5
}

pred_unsatisfy1=function(index,data=data,path){
  n=nrow(data)-8
  final_sum_5=matrix(nrow = n)
  final_sum_6=matrix(nrow = n)
  final_sum_7=matrix(nrow = n)
  final_sum_8=matrix(nrow = n)
  final_sum_9=matrix(nrow = n)
  final_sum_10=matrix(nrow = n)
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
    R9_final_quantity=R9_quantity[,index]
    
    
    R7_quantity=new_quantize(path = path,real_comb=final_comb,set=data[,m])[[2]]
    R7_final_quantity=R7_quantity[,index]
    
    R5_quantity=new_quantize(path = path,real_comb=final_comb,set=data[,m])[[3]]
    R5_final_quantity=R5_quantity[,index]
    
    sumtemp=R9_final_quantity+R7_final_quantity+ R5_final_quantity
    sumtemp=as.matrix(sumtemp)
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
        names(sumtemp)=c(paste0('data',m))
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

error1=pred_unsatisfy1(3,data,path = path )
dis_unsatisfy=function(formular_final_result){
  for (k in 2:6){
    num=ncol(formular_final_result[[k-1]])-ncol(formular_final_result[[k]])
    print(paste0('连续负的个数为',k+3,'的样本个数：',num))
  }
  num=ncol(formular_final_result[[6]])
  print(paste0('连续负的个数为超过9个的样本个数：',num))
}
dis_unsatisfy(error1)
