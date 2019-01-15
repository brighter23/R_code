# 1、formula准备------------------------------------------
data1024=read.csv('data\\data1024pices.csv',header = F)
library(stringr)

{
  library(stringr)
  formula=read.csv2('data\\YAY5791113R.csv',header = F,stringsAsFactors = F)
  
  R5A=formula[2:7,];R5B=formula[9:14,]
  R7A=formula[16:47,];R7B=formula[49:80,]
  R9A=formula[82:209,];R9B=formula[211:338,]
  R11A=formula[340:851,];R11B=formula[853:1364,]
  R13A=formula[1366:3413,];R13B=formula[3415:5462,]
  Formula=function(R,c1,c2){
    R_=str_split(R,pattern = ',',simplify = T)
    R_B=apply(R_[,c1], 1, function(x)str_c(x,collapse = '-'))
    R_P=apply(R_[,c2], 1, function(x)str_c(x,collapse = '-'))
    return(cbind(str_c(R_B,collapse = '|'), str_c(R_P,collapse = '|')))
  }
  
  R5A=Formula(R5A,c1 = 1:3,c2=4:6);R5B=Formula(R5B,c1 = 1:3,c2=4:6)
  R7A=Formula(R7A,c1 = 1:6,c2=7:12);R7B=Formula(R7B,c1 = 1:6,c2=7:12)
  R9A=Formula(R9A,c1 = 1:8,c2=9:16);R9B=Formula(R9B,c1 = 1:8,c2=9:16)
  R11A=Formula(R11A,c1 = 1:10,c2=11:20);R11B=Formula(R11B,c1 = 1:10,c2=11:20)
  R13A=Formula(R13A,c1 = 1:12,c2=13:24);R13B=Formula(R13B,c1 = 1:12,c2=13:24)
  
  path_BP_YAY579=matrix(c(R5A,R7A,R9A, R5A,R7B,R9A, R5A,R7A,R9B, R5A,R7B,R9B, R5B,R7A,R9A, R5B,R7B,R9A, R5B,R7A,R9B, R5B,R7B,R9B),byrow = T,ncol = 6)
  colnames(path_BP_YAY579)=c('B_R5','P_R5','B_R7','P_R7','B_R9','P_R9')
  path_BP_YAY7911=matrix(c(R7A,R9A,R11A, R7A,R9B,R11A, R7A,R9A,R11B, R7A,R9B,R11B, R7B,R9A,R11A, R7B,R9B,R11A, R7B,R9A,R11B, R7B,R9B,R11B),byrow = T,ncol = 6)
  colnames(path_BP_YAY7911)=c('B_R7','P_R7','B_R9','P_R9','B_R11','P_R11')
  path_BP_YAY91113=matrix(c(R9A,R11A,R13A, R9A,R11B,R13A, R9A,R11A,R13B, R9A,R11B,R13B, R9B,R11A,R13A, R9B,R11B,R13A, R9B,R11A,R13B, R9B,R11B,R13B),byrow = T,ncol = 6)
  colnames(path_BP_YAY91113)=c('B_R9','P_R9','B_R11','P_R11','B_R13','P_R13')
}
{
  library(stringr)
  formula=read.csv2('data\\668810R.csv',header = F,stringsAsFactors = F)
  
  R66A=formula[2:17,];R66B=formula[19:34,]
  R88A=formula[36:99,];R88B=formula[101:164,]
  R10A=formula[166:421,];R10B=formula[423:678,]
  Formula=function(R,c1,c2){
    R_=str_split(R,pattern = ',',simplify = T)
    R_B=apply(R_[,c1], 1, function(x)str_c(x,collapse = '-'))
    R_P=apply(R_[,c2], 1, function(x)str_c(x,collapse = '-'))
    return(cbind(str_c(R_B,collapse = '|'), str_c(R_P,collapse = '|')))
  }
  
  R66A=Formula(R66A,c1 = 1:5,c2=6:10);R66B=Formula(R66B,c1 = 1:5,c2=6:10)
  R88A=Formula(R88A,c1 = 1:7,c2=8:14);R88B=Formula(R88B,c1 = 1:7,c2=8:14)
  R10A=Formula(R10A,c1 = 1:9,c2=10:18);R10B=Formula(R10B,c1 = 1:9,c2=10:18)
  
  path_BP_YAY668810=matrix(c(R66A,R88A,R10A, R66A,R88B,R10A, R66A,R88A,R10B, R66A,R88B,R10B, R66B,R88A,R10A, R66B,R88B,R10A, R66B,R88A,R10B, R66B,R88B,R10B),byrow = T,ncol = 6)
  colnames(path_BP_YAY668810)=c('B_R66','P_R66','B_R88','P_R88','B_R10','P_R10')
}
library(stringr)
# 2、一个片段在YAY579或668810下的8种结果=========================================
true_path579_0=function(dataset){
  #9R:8个
  t19=table(dataset[1:9])
  result19=paste(t19[1],t19[2],sep = ':')
  t18=table(dataset[1:8])
  result18=paste(t18[1],t18[2],sep = ':')
  t17=table(dataset[1:7])
  result17=paste(t17[1],t17[2],sep = ':')
  t16=table(dataset[1:6])
  result16=paste(t16[1],t16[2],sep = ':')
  t15=table(dataset[1:5])
  result15=paste(t15[1],t15[2],sep = ':')
  t14=table(dataset[1:4])
  result14=paste(t14[1],t14[2],sep = ':')
  t13=table(dataset[1:3])
  result13=paste(t13[1],t13[2],sep = ':')
  t12=table(dataset[1:2])
  result12=paste(t12[1],t12[2],sep = ':')
  comb1=paste0(result12,'-',result13,'-',result14,'-',result15,'-',result16,'-',result17,'-',result18,'-',result19)
  #7R：6个
  t39=table(dataset[3:9])
  result39=paste(t39[1],t39[2],sep = ':')
  t38=table(dataset[3:8])
  result38=paste(t38[1],t38[2],sep = ':')
  t37=table(dataset[3:7])
  result37=paste(t37[1],t37[2],sep = ':')
  t36=table(dataset[3:6])
  result36=paste(t36[1],t36[2],sep = ':')
  t35=table(dataset[3:5])
  result35=paste(t35[1],t35[2],sep = ':')
  t34=table(dataset[3:4])
  result34=paste(t34[1],t34[2],sep = ':')
  comb2=paste0(result34,'-',result35,'-',result36,'-',result37,'-',result38,'-',result39)
  #5R：3个，因为不是从02或20或11开始
  t59=table(dataset[5:9])
  result59=paste(t59[1],t59[2],sep = ':')
  t58=table(dataset[5:8])
  result58=paste(t58[1],t58[2],sep = ':')
  t57=table(dataset[5:7])
  result57=paste(t57[1],t57[2],sep = ':')
  comb3=paste0(result57,'-',result58,'-',result59)
  return(matrix(c(comb1,comb2,comb3),ncol = 3))
  
} # 顺序，5-7-9R的结果
true_path668810_0=function(dataset){
  #10R:
  t110=table(dataset[1:10])
  result110=paste(t110[1],t110[2],sep = ':')
  t19=table(dataset[1:9])
  result19=paste(t19[1],t19[2],sep = ':')
  t18=table(dataset[1:8])
  result18=paste(t18[1],t18[2],sep = ':')
  t17=table(dataset[1:7])
  result17=paste(t17[1],t17[2],sep = ':')
  t16=table(dataset[1:6])
  result16=paste(t16[1],t16[2],sep = ':')
  t15=table(dataset[1:5])
  result15=paste(t15[1],t15[2],sep = ':')
  t14=table(dataset[1:4])
  result14=paste(t14[1],t14[2],sep = ':')
  t13=table(dataset[1:3])
  result13=paste(t13[1],t13[2],sep = ':')
  t12=table(dataset[1:2])
  result12=paste(t12[1],t12[2],sep = ':')
  comb1=paste0(result12,'-',result13,'-',result14,'-',result15,'-',result16,'-',result17,'-',result18,'-',result19,'-',result110)
  #88R：
  t310=table(dataset[3:10])
  result310=paste(t310[1],t310[2],sep = ':')
  t39=table(dataset[3:9])
  result39=paste(t39[1],t39[2],sep = ':')
  t38=table(dataset[3:8])
  result38=paste(t38[1],t38[2],sep = ':')
  t37=table(dataset[3:7])
  result37=paste(t37[1],t37[2],sep = ':')
  t36=table(dataset[3:6])
  result36=paste(t36[1],t36[2],sep = ':')
  t35=table(dataset[3:5])
  result35=paste(t35[1],t35[2],sep = ':')
  t34=table(dataset[3:4])
  result34=paste(t34[1],t34[2],sep = ':')
  comb2=paste0(result34,'-',result35,'-',result36,'-',result37,'-',result38,'-',result39,'-',result310)
  #66R：
  t510=table(dataset[5:10])
  result510=paste(t510[1],t510[2],sep = ':')
  t59=table(dataset[5:9])
  result59=paste(t59[1],t59[2],sep = ':')
  t58=table(dataset[5:8])
  result58=paste(t58[1],t58[2],sep = ':')
  t57=table(dataset[5:7])
  result57=paste(t57[1],t57[2],sep = ':')
  t56=table(dataset[5:6])
  result56=paste(t56[1],t56[2],sep = ':')
  comb3=paste0(result56,'-',result57,'-',result58,'-',result59,'-',result510)
  return(matrix(c(comb1,comb2,comb3),ncol = 3))
  
} # 顺序，66-88-10R的结果
new_quantize579=function(set,order=T,connect=T,path=path_BP_YAY579){
  if(order){
    # 顺序9R：12345678+9，7R：345678+9，5R：5678+9
    # 都是使用true_path**_0，BP依据set[1],set[3],set[5]
    real_comb=true_path579_0(set)
    r9=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      r9[1,j]=ifelse(set[1]=='B'|set[1]==1,ifelse(str_detect(real_comb[1,1],path[j,'B_R9']),1,-1),ifelse(str_detect(real_comb[1,1],path[j,'P_R9']),1,-1))
    }
    r7=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      r7[1,j]=ifelse(set[3]=='B'|set[3]==1,ifelse(str_detect(real_comb[1,2],path[j,'B_R7']),1,-1),ifelse(str_detect(real_comb[1,2],path[j,'P_R7']),1,-1))
    }
    r5=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      r5[1,j]=ifelse(set[5]=='B'|set[5]==1,ifelse(str_detect(real_comb[1,3],path[j,'B_R5']),1,-1),ifelse(str_detect(real_comb[1,3],path[j,'P_R5']),1,-1))
    }
  }
  else{
    # 逆序
    # 逆序+连续87654321+9，7R：876543+9，5R：8765+9
    if(connect)real_comb=true_path579_1(set)
    # 逆序+断开87654321+9，7R：654321+9，5R：4321+9
    else real_comb=true_path579_0(set) # 本质是在逆序时使用了顺序的计算方式
    
    r9=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      sparate_condition=ifelse(connect,set[1],set[1])
      r9[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,1],path[j,'B_R9']),1,-1),ifelse(str_detect(real_comb[1,1],path[j,'P_R9']),1,-1))
    }
    r7=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      sparate_condition=ifelse(connect,set[1],set[3])
      r7[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,2],path[j,'B_R7']),1,-1),ifelse(str_detect(real_comb[1,2],path[j,'P_R7']),1,-1))
    }
    r5=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      sparate_condition=ifelse(connect,set[1],set[5])
      r5[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,3],path[j,'B_R5']),1,-1),ifelse(str_detect(real_comb[1,3],path[j,'P_R5']),1,-1))
    }
  }
  return(r5+r7+r9)
} 
# new_quantize579(set=data[1:9,1],order=F,connect=T)
new_quantize668810=function(set,order=T,connect=T,path=path_BP_YAY668810){
  if(order){
    # 顺序10R：123456789+10，88R：3456789+10，66R：56789+10
    # 都是使用true_path**_0，BP依据set[1],set[3],set[5]
    real_comb=true_path668810_0(set)
    r10=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      r10[1,j]=ifelse(set[1]=='B'|set[1]==1,ifelse(str_detect(real_comb[1,1],path[j,'B_R10']),1,-1),ifelse(str_detect(real_comb[1,1],path[j,'P_R10']),1,-1))
    }
    r8=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      r8[1,j]=ifelse(set[3]=='B'|set[3]==1,ifelse(str_detect(real_comb[1,2],path[j,'B_R88']),1,-1),ifelse(str_detect(real_comb[1,2],path[j,'P_R88']),1,-1))
    }
    r6=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      r6[1,j]=ifelse(set[5]=='B'|set[5]==1,ifelse(str_detect(real_comb[1,3],path[j,'B_R66']),1,-1),ifelse(str_detect(real_comb[1,3],path[j,'P_R66']),1,-1))
    }
  }
  else{
    # 逆序
    # 逆序+连续987654321+10，88R：9876543+10，66R：98765+10
    if(connect)real_comb=true_path668810_1(set)
    # 逆序+断开987654321+10，88R：7654321+10，66R：54321+10
    else real_comb=true_path668810_0(set) # 本质是在逆序时使用了顺序的计算方式
    
    r10=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      sparate_condition=ifelse(connect,set[1],set[1])
      r10[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,1],path[j,'B_R10']),1,-1),ifelse(str_detect(real_comb[1,1],path[j,'P_R10']),1,-1))
    }
    r8=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      sparate_condition=ifelse(connect,set[1],set[3])
      r8[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,2],path[j,'B_R88']),1,-1),ifelse(str_detect(real_comb[1,2],path[j,'P_R88']),1,-1))
    }
    r6=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      sparate_condition=ifelse(connect,set[1],set[5])
      r6[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,3],path[j,'B_R66']),1,-1),ifelse(str_detect(real_comb[1,3],path[j,'P_R66']),1,-1))
    }
  }
  return(r6+r8+r10)
} 
# 对于10个的片段579取2-10，6810取1-10的数据输入
factorize=function(set){
  return(factor(str_split(set,pattern = ',',simplify = T),levels = c('B','P')))
}
new_quantize579(factorize(data1024[1,])[2:10])
new_quantize668810(factorize(data1024[1,])[1:10])
# 8种结果排列组合得到64种结果
couple=read.csv('data\\formula_couple64.csv') # 64种组合结果的索引
couple_copute=function(set,couple,path_BP_YAY579=path_BP_YAY579,path_BP_YAY668810=path_BP_YAY668810){
  final_result579=new_quantize579(factorize(set)[2:10])
  final_result6810=new_quantize668810(factorize(set)[1:10])
  tempt=apply(couple,1,function(x){final_result579[x[1]]+final_result6810[x[2]]})
  return(str_c(tempt,collapse = ','))
}
# 对整个data1024进行相同的运算
couple_copute(data1024[1,],couple)
result=apply(data1024,1,function(x){couple_copute(set=x,couple = couple)})

write.csv(as.data.frame(result),'C:\\Users\\Think\\Desktop\\1024reslut.csv',row.names = F)
