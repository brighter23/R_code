# 1、formula准备------------------------------------------
data=read.csv('data\\treat_two_hundred.csv',header = T)
data1=read.csv('data\\new.csv',header = T)
data2=read.csv('data\\new1.csv',header = T)
data3=read.csv('data\\new2.csv',header = T)
data1=data1[,-23]
all_data=cbind(data,data1,data2,data3)
row.names(data)=1:68
row.names(all_data)=1:68
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

c=c('data','all_data','path_BP_YAY579','path_BP_YAY7911','path_BP_YAY91113')
a=ls()
rm(list=(setdiff(a,c)))
gc()
library(stringr)
# 当有新的formula时需要修改true_path、new_quantize、取数据的手数
# 2、训练==================================================
# 当有新的formula时需要修改true_path、new_quantize、取数据的手数(取7手计算final result的都一样，取9手的与9手的一样)

# 2.1 定义计算每9个真实数据下的真实路径final_comb:
true_path0=function(dataset){
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
true_path1=function(dataset){
  #9R:8个
  t19=table(dataset[c(1:8,9)])
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
  t39=table(dataset[c(1:6,9)])
  result39=paste(t39[1],t39[2],sep = ':')
  t38=table(dataset[1:6])
  result38=paste(t38[1],t38[2],sep = ':')
  t37=table(dataset[1:5])
  result37=paste(t37[1],t37[2],sep = ':')
  t36=table(dataset[1:4])
  result36=paste(t36[1],t36[2],sep = ':')
  t35=table(dataset[1:3])
  result35=paste(t35[1],t35[2],sep = ':')
  t34=table(dataset[1:2])
  result34=paste(t34[1],t34[2],sep = ':')
  comb2=paste0(result34,'-',result35,'-',result36,'-',result37,'-',result38,'-',result39)
  #5R：3个，因为不是从02或20或11开始
  t59=table(dataset[c(1:4,9)])
  result59=paste(t59[1],t59[2],sep = ':')
  t58=table(dataset[1:4])
  result58=paste(t58[1],t58[2],sep = ':')
  t57=table(dataset[1:3])
  result57=paste(t57[1],t57[2],sep = ':')
  comb3=paste0(result57,'-',result58,'-',result59)
  return(matrix(c(comb1,comb2,comb3),ncol = 3))
  
} # 注：这里输入的时已经逆序处理的数据了
# 1266的逆序不同了，本质是和顺序的计算方式一样，即数据是逆序的输入，计算使用true_path0
true_path79110=function(dataset){
  #11R:
  t111=table(dataset[1:11])
  result111=paste(t111[1],t111[2],sep = ':')
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
  comb1=paste0(result12,'-',result13,'-',result14,'-',result15,'-',result16,'-',result17,'-',result18,'-',
               result19,'-',result110,'-',result111)
  #9R
  t311=table(dataset[3:11])
  result311=paste(t311[1],t311[2],sep = ':')
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
  comb2=paste0(result34,'-',result35,'-',result36,'-',result37,'-',result38,'-',result39,'-',result310,'-',result311)
  #7R：
  t511=table(dataset[5:11])
  result511=paste(t511[1],t511[2],sep = ':')
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
  comb3=paste0(result56,'-',result57,'-',result58,'-',result59,'-',result510,'-',result511)
  return(matrix(c(comb1,comb2,comb3),ncol = 3))
  
} # 顺序7-9-11R的结果
true_path79111=function(dataset){
  #11R
  t111=table(dataset[c(1:10,11)])
  result111=paste(t111[1],t111[2],sep = ':')
  t110=table(dataset[1:10])
  result110=paste(t110[1],t110[2],sep = ':')
  t19=table(dataset[c(1:9)])
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
  comb1=paste0(result12,'-',result13,'-',result14,'-',result15,'-',result16,'-',result17,'-',result18,'-',
               result19,'-',result110,'-',result111)
  #9R
  t311=table(dataset[c(1:8,11)])
  result311=paste(t311[1],t311[2],sep = ':')
  t310=table(dataset[1:8])
  result310=paste(t310[1],t310[2],sep = ':')
  t39=table(dataset[c(1:7)])
  result39=paste(t39[1],t39[2],sep = ':')
  t38=table(dataset[1:6])
  result38=paste(t38[1],t38[2],sep = ':')
  t37=table(dataset[1:5])
  result37=paste(t37[1],t37[2],sep = ':')
  t36=table(dataset[1:4])
  result36=paste(t36[1],t36[2],sep = ':')
  t35=table(dataset[1:3])
  result35=paste(t35[1],t35[2],sep = ':')
  t34=table(dataset[1:2])
  result34=paste(t34[1],t34[2],sep = ':')
  comb2=paste0(result34,'-',result35,'-',result36,'-',result37,'-',result38,'-',result39,'-',result310,'-',result311)
  #7R：
  t511=table(dataset[c(1:6,11)])
  result511=paste(t511[1],t511[2],sep = ':')
  t510=table(dataset[1:6])
  result510=paste(t510[1],t510[2],sep = ':')
  t59=table(dataset[c(1:5)])
  result59=paste(t59[1],t59[2],sep = ':')
  t58=table(dataset[1:4])
  result58=paste(t58[1],t58[2],sep = ':')
  t57=table(dataset[1:3])
  result57=paste(t57[1],t57[2],sep = ':')
  t56=table(dataset[1:2])
  result56=paste(t56[1],t56[2],sep = ':')
  comb3=paste0(result56,'-',result57,'-',result58,'-',result59,'-',result510,'-',result511)
  return(matrix(c(comb1,comb2,comb3),ncol = 3))
  
} # 注：这里输入的时已经逆序处理的数据了

true_path911130=function(dataset){
  #13R:
  t113=table(dataset[1:13])
  result113=paste(t113[1],t113[2],sep = ':')
  t112=table(dataset[1:12])
  result112=paste(t112[1],t112[2],sep = ':')
  t111=table(dataset[1:11])
  result111=paste(t111[1],t111[2],sep = ':')
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
  comb1=paste0(result12,'-',result13,'-',result14,'-',result15,'-',result16,'-',result17,'-',result18,'-',
               result19,'-',result110,'-',result111,'-',result112,'-',result113)
  #11R
  t313=table(dataset[3:13])
  result313=paste(t313[1],t313[2],sep = ':')
  t312=table(dataset[3:12])
  result312=paste(t312[1],t312[2],sep = ':')
  t311=table(dataset[3:11])
  result311=paste(t311[1],t311[2],sep = ':')
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
  comb2=paste0(result34,'-',result35,'-',result36,'-',result37,'-',result38,'-',
               result39,'-',result310,'-',result311,'-',result312,'-',result313)
  #9R：
  t513=table(dataset[5:13])
  result513=paste(t513[1],t513[2],sep = ':')
  t512=table(dataset[5:12])
  result512=paste(t512[1],t512[2],sep = ':')
  t511=table(dataset[5:11])
  result511=paste(t511[1],t511[2],sep = ':')
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
  comb3=paste0(result56,'-',result57,'-',result58,'-',result59,'-',result510,'-',result511
               ,'-',result512,'-',result513)
  return(matrix(c(comb1,comb2,comb3),ncol = 3))
  
} # 顺序9-11-13R的结果
true_path911131=function(dataset){
  #13R:
  t113=table(dataset[c(1:12,13)])
  result113=paste(t113[1],t113[2],sep = ':')
  t112=table(dataset[1:12])
  result112=paste(t112[1],t112[2],sep = ':')
  t111=table(dataset[c(1:11)])
  result111=paste(t111[1],t111[2],sep = ':')
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
  comb1=paste0(result12,'-',result13,'-',result14,'-',result15,'-',result16,'-',result17,'-',result18,'-',
               result19,'-',result110,'-',result111,'-',result112,'-',result113)
  #11R
  t313=table(dataset[c(1:10,13)])
  result313=paste(t313[1],t313[2],sep = ':')
  t312=table(dataset[1:10])
  result312=paste(t312[1],t312[2],sep = ':')
  t311=table(dataset[1:9])
  result311=paste(t311[1],t311[2],sep = ':')
  t310=table(dataset[1:8])
  result310=paste(t310[1],t310[2],sep = ':')
  t39=table(dataset[c(1:7)])
  result39=paste(t39[1],t39[2],sep = ':')
  t38=table(dataset[1:6])
  result38=paste(t38[1],t38[2],sep = ':')
  t37=table(dataset[1:5])
  result37=paste(t37[1],t37[2],sep = ':')
  t36=table(dataset[1:4])
  result36=paste(t36[1],t36[2],sep = ':')
  t35=table(dataset[1:3])
  result35=paste(t35[1],t35[2],sep = ':')
  t34=table(dataset[1:2])
  result34=paste(t34[1],t34[2],sep = ':')
  comb2=paste0(result34,'-',result35,'-',result36,'-',result37,'-',result38,'-',
               result39,'-',result310,'-',result311,'-',result312,'-',result313)
  #9R：
  t513=table(dataset[c(1:8,13)])
  result513=paste(t513[1],t513[2],sep = ':')
  t512=table(dataset[1:8])
  result512=paste(t512[1],t512[2],sep = ':')
  t511=table(dataset[1:7])
  result511=paste(t511[1],t511[2],sep = ':')
  t510=table(dataset[1:6])
  result510=paste(t510[1],t510[2],sep = ':')
  t59=table(dataset[c(1:5)])
  result59=paste(t59[1],t59[2],sep = ':')
  t58=table(dataset[1:4])
  result58=paste(t58[1],t58[2],sep = ':')
  t57=table(dataset[1:3])
  result57=paste(t57[1],t57[2],sep = ':')
  t56=table(dataset[1:2])
  result56=paste(t56[1],t56[2],sep = ':')
  comb3=paste0(result56,'-',result57,'-',result58,'-',result59,'-',result510,'-',result511
               ,'-',result512,'-',result513)
  return(matrix(c(comb1,comb2,comb3),ncol = 3))
  
} # 注：这里输入的时已经逆序处理的数据了

true_path90=function(dataset){
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
  return(matrix(c(comb1),ncol = 1))
  
} # 顺序，仅9R
true_path91=function(dataset){
  #9R:8个
  t19=table(dataset[c(1:8,9)])
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
  return(matrix(c(comb1),ncol = 1))
  
} 
true_path70=function(dataset){
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
  return(matrix(c(comb2),ncol = 1))
  
} # 顺序,仅7R
true_path71=function(dataset){
  #7R：6个
  t39=table(dataset[c(1:6,9)])
  result39=paste(t39[1],t39[2],sep = ':')
  t38=table(dataset[1:6])
  result38=paste(t38[1],t38[2],sep = ':')
  t37=table(dataset[1:5])
  result37=paste(t37[1],t37[2],sep = ':')
  t36=table(dataset[1:4])
  result36=paste(t36[1],t36[2],sep = ':')
  t35=table(dataset[1:3])
  result35=paste(t35[1],t35[2],sep = ':')
  t34=table(dataset[1:2])
  result34=paste(t34[1],t34[2],sep = ':')
  comb2=paste0(result34,'-',result35,'-',result36,'-',result37,'-',result38,'-',result39)
  return(matrix(c(comb2),ncol = 1))
  
} 
true_path50=function(dataset){
  #5R：3个，因为不是从02或20或11开始
  t59=table(dataset[5:9])
  result59=paste(t59[1],t59[2],sep = ':')
  t58=table(dataset[5:8])
  result58=paste(t58[1],t58[2],sep = ':')
  t57=table(dataset[5:7])
  result57=paste(t57[1],t57[2],sep = ':')
  comb3=paste0(result57,'-',result58,'-',result59)
  return(matrix(c(comb3),ncol = 1))
  
} # 顺序，仅5R
true_path51=function(dataset){
  #5R：3个，因为不是从02或20或11开始
  t59=table(dataset[c(1:4,9)])
  result59=paste(t59[1],t59[2],sep = ':')
  t58=table(dataset[1:4])
  result58=paste(t58[1],t58[2],sep = ':')
  t57=table(dataset[1:3])
  result57=paste(t57[1],t57[2],sep = ':')
  comb3=paste0(result57,'-',result58,'-',result59)
  return(matrix(c(comb3),ncol = 1))
  
}
# 2.2 定义7个真实数据下根据真实路径下的量化值-1或1:
# path=path_BP为分Bp的：
# Inverse有2种形式F,T，代表不同的取数据计算真实路径的方式，对应true_path0~1
new_quantize=function(path,set,Inverse,R){
  # 分BP的path有6列：
  if(R==9){
    if(Inverse)real_comb=true_path91(set)# 逆序的量化
    else real_comb=true_path90(set) # 顺序的方式进行量化
    r9=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      sparate_condition=ifelse(Inverse,set[1],set[1])
      r9[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,1],path[j,'B_R9']),1,-1),ifelse(str_detect(real_comb[1,1],path[j,'P_R9']),1,-1))
    }
    return(r9)
  }
  else if(R==7){
    if(Inverse)real_comb=true_path71(set)# 逆序的量化
    else real_comb=true_path70(set) # 顺序的方式进行量化
    r7=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      sparate_condition=ifelse(Inverse,set[1],set[3])
      r7[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,1],path[j,'B_R7']),1,-1),ifelse(str_detect(real_comb[1,1],path[j,'P_R7']),1,-1))
    }
    return(r7)
  }
  else if(R==5){
    if(Inverse)real_comb=true_path51(set)# 逆序的量化
    else real_comb=true_path50(set) # 顺序的方式进行量化
    r5=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      sparate_condition=ifelse(Inverse,set[1],set[5])
      r5[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,1],path[j,'B_R5']),1,-1),ifelse(str_detect(real_comb[1,1],path[j,'P_R5']),1,-1))
    }
    return(r5)
  }
  else{
    if(Inverse)real_comb=true_path1(set)# 逆序的量化
    else real_comb=true_path0(set) # 顺序的方式进行量化
    r9=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      sparate_condition=ifelse(Inverse,set[1],set[1])
      r9[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,1],path[j,'B_R9']),1,-1),ifelse(str_detect(real_comb[1,1],path[j,'P_R9']),1,-1))
    }
    r7=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      sparate_condition=ifelse(Inverse,set[1],set[3])
      r7[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,2],path[j,'B_R7']),1,-1),ifelse(str_detect(real_comb[1,2],path[j,'P_R7']),1,-1))
    }
    r5=matrix(0,ncol = nrow(path),nrow = 1)
    for (j in 1:nrow(path)) {
      sparate_condition=ifelse(Inverse,set[1],set[5])
      r5[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,3],path[j,'B_R5']),1,-1),ifelse(str_detect(real_comb[1,3],path[j,'P_R5']),1,-1))
    }
    return(r7+r5+r9)
  }
} 
new_quantize1266=function(path,set,Inverse){
  if(Inverse)real_comb=true_path0(set)# 逆序的数据，顺序的量化
  else real_comb=true_path0(set) # 顺序的方式进行量化
  r9=matrix(0,ncol = nrow(path),nrow = 1)
  for (j in 1:nrow(path)) {
    sparate_condition=ifelse(Inverse,set[1],set[1])
    r9[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,1],path[j,'B_R9']),1,-1),ifelse(str_detect(real_comb[1,1],path[j,'P_R9']),1,-1))
  }
  r7=matrix(0,ncol = nrow(path),nrow = 1)
  for (j in 1:nrow(path)) {
    sparate_condition=ifelse(Inverse,set[3],set[3])
    r7[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,2],path[j,'B_R7']),1,-1),ifelse(str_detect(real_comb[1,2],path[j,'P_R7']),1,-1))
  }
  r5=matrix(0,ncol = nrow(path),nrow = 1)
  for (j in 1:nrow(path)) {
    sparate_condition=ifelse(Inverse,set[5],set[5])
    r5[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,3],path[j,'B_R5']),1,-1),ifelse(str_detect(real_comb[1,3],path[j,'P_R5']),1,-1))
  }
  return(r7+r5+r9)
} 
new_quantize7911=function(path,set,Inverse){
  if(Inverse)real_comb=true_path79111(set)# 逆序的量化
  else real_comb=true_path79110(set) # 顺序的方式进行量化
  r11=matrix(0,ncol = nrow(path),nrow = 1)
  for (j in 1:nrow(path)) {
    sparate_condition=ifelse(Inverse,set[1],set[1])
    r11[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,1],path[j,'B_R11']),1,-1),ifelse(str_detect(real_comb[1,1],path[j,'P_R11']),1,-1))
  }
  r9=matrix(0,ncol = nrow(path),nrow = 1)
  for (j in 1:nrow(path)) {
    sparate_condition=ifelse(Inverse,set[1],set[3])
    r9[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,2],path[j,'B_R9']),1,-1),ifelse(str_detect(real_comb[1,2],path[j,'P_R9']),1,-1))
  }
  r7=matrix(0,ncol = nrow(path),nrow = 1)
  for (j in 1:nrow(path)) {
    sparate_condition=ifelse(Inverse,set[1],set[5])
    r7[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,3],path[j,'B_R7']),1,-1),ifelse(str_detect(real_comb[1,3],path[j,'P_R7']),1,-1))
  }
  return(r7+r9+r11)
} 
new_quantize91113=function(path,set,Inverse){
  if(Inverse)real_comb=true_path911131(set)# 逆序的量化
  else real_comb=true_path911130(set) # 顺序的方式进行量化
  r13=matrix(0,ncol = nrow(path),nrow = 1)
  for (j in 1:nrow(path)) {
    sparate_condition=ifelse(Inverse,set[1],set[1])
    r13[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,1],path[j,'B_R13']),1,-1),ifelse(str_detect(real_comb[1,1],path[j,'P_R13']),1,-1))
  }
  r11=matrix(0,ncol = nrow(path),nrow = 1)
  for (j in 1:nrow(path)) {
    sparate_condition=ifelse(Inverse,set[1],set[3])
    r11[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,2],path[j,'B_R11']),1,-1),ifelse(str_detect(real_comb[1,2],path[j,'P_R11']),1,-1))
  }
  r9=matrix(0,ncol = nrow(path),nrow = 1)
  for (j in 1:nrow(path)) {
    sparate_condition=ifelse(Inverse,set[1],set[5])
    r9[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,3],path[j,'B_R9']),1,-1),ifelse(str_detect(real_comb[1,3],path[j,'P_R9']),1,-1))
  }
  return(r9+r11+r13)
} 
# new_quantize(path = path_BP_YAY579,set=data[1:9,1],Inverse=T,R=9) # 测试应该是正负相对
# 2.3 定义诊断连续负的长度：
detect_continue_minus=function(sumtemp){
  continue_minus_num=c()
  for (i in 1:ncol(sumtemp)) {
    # 转化成字符串，查看多少个“0”在字符串内，例如”00000“这部分是否在字符串内
    strings=str_c(ifelse(sumtemp[,i]<0,0,1),collapse = '')
    for (k in 1:nrow(sumtemp)) {
      pattern=str_c(paste0(rep(0,k)),collapse = '')
      if(!str_detect(strings,pattern)){
        #print(paste0('最大连续为负个数',k-1))
        continue_minus_num[i]=k-1
        break
      }
    }
  }
  return(continue_minus_num)
}
# 取数据：并生成预测值
{
  # 单轨，轨迹数据，2个一组
  data_sort_sequence_inverse_one=function(data,m,choice,path=path){
    n=nrow(data)
    final_result=matrix(nrow = n,ncol = nrow(path)) # final_result
    row.names(final_result)=1:n
    pre=data.frame(matrix(nrow = n,ncol = nrow(path)),row.names = 1:n) # 奇数轨预测值
    for(i in 1:ncol(pre)){
      pre[,i]=factor(pre[,i],levels = c('B','P'))
    }    
    # 1、先计算使用原始数据的9-16手
    # 奇数手：顺
    for (i in seq(9,15,2)) {
        # 计算final result
        final_result[i,]=new_quantize(path,set = data[c(seq(i-8,i-1,1),i),m],Inverse = F,R)
        # 计算预测值
        pre[i,]=ifelse(final_result[i,]>0,ifelse(data[i,m]=='B','B','P'),ifelse(data[i,m]=='B','P','B'))
      }
    # 偶数手：逆
    for (i in seq(10,16,2)) {
        final_tempt=new_quantize(path,set = data[c(seq(i-1,i-8,-1),i),m],Inverse = T,R)
        if(choice==12111){final_result[i,]=ifelse(final_result[i-1,]>0,-final_tempt,final_tempt)}
        else if(choice==12112){final_result[i,]=ifelse(final_result[i-1,]>0,final_tempt,-final_tempt)}
        else if(choice==12119){final_result[i,]=-final_tempt}
        pre[i,]=ifelse(final_result[i,]>0,ifelse(data[i,m]=='B','B','P'),ifelse(data[i,m]=='B','P','B'))
      }
    # 2、合再一起做，因为互相用到数据
    for(i in 17:n){
      # final result替换数据从重新计算
      # 奇数手（第1手）固定使用original formula:
      if (i %in% seq(17,n,2)){
        final_result[i,]=diag(sapply(rbind(pre[seq(i-8,i-1,1),],data.frame(matrix(rep(data[i,m],8),ncol=8),row.names = i))
                                     ,function(x){new_quantize(path,set=x,Inverse = F,R)}))
        # 计算预测值
        pre[i,]=ifelse(final_result[i,]>0,ifelse(data[i,m]=='B','B','P'),ifelse(data[i,m]=='B','P','B'))
      }
      # 偶数手（第2手，逆序:需要改seq索引和inverse=T）根据第1手的结果
      else{
        # 先计算8个formula下对应8条路径的的结果结果矩阵，1列表示1条路径下8个formula的结果
        temptMatrix_final_result=sapply(rbind(pre[seq(i-1,i-8,-1),],data.frame(matrix(rep(data[i,m],8),ncol=8),row.names = i))
                                       ,function(x){new_quantize(path,set=x,Inverse = T,R)})
        # 不同的plan
        if(choice==12111){
          # 根据第1手结果against
          for(j in 1:8){
            # 计算每个final result:根据第一手的final result against,正时使用相反formula，负的时候使用相同formula
            if(final_result[i-1,j]>0)final_result[i,j]=temptMatrix_final_result[abs(j-9),j]
            else final_result[i,j]=temptMatrix_final_result[j,j]
          }
        }
        else if(choice==12112){
          # 根据第一手final result follow，plan12112
          # 正时使用相同formula，负的时候使用相反formula
          for(j in 1:8){
            # 计算每个final result:根据第一手的final result against,正时使用相反formula，负的时候使用相同formula
            if(final_result[i-1,j]<0)final_result[i,j]=temptMatrix_final_result[abs(j-9),j]
            else final_result[i,j]=temptMatrix_final_result[j,j]
          }
        }
        else if(choice==12119){
          # 根据第一手final result follow，plan12112
          # 正时使用相同formula，负的时候使用相反formula
          for(j in 1:8){
            # 计算每个final result:根据第一手的final result against,正时使用相反formula，负的时候使用相同formula
            final_result[i,j]=temptMatrix_final_result[abs(j-9),j]
          }
        }
        # 计算预测值,只与final result和真实数据有关
        pre[i,]=ifelse(final_result[i,]>0,ifelse(data[i,m]=='B','B','P'),ifelse(data[i,m]=='B','P','B'))
      }
    }
    # 3、去除无法计算final result的：
    final_result=final_result[-c(1:8),]
    pre=pre[-c(1:8),]
    colnames(final_result)=c(paste0('trace',1:nrow(path)))
    colnames(pre)=c(paste0('trace',1:nrow(path)))
    return(list(final_result,pre))
  }
  data_sort_1226_one=function(data,m,choice,path=path){
    n=nrow(data)
    final_result=matrix(nrow = n,ncol = nrow(path)) # final_result
    row.names(final_result)=1:n
    pre=data.frame(matrix(nrow = n,ncol = nrow(path)),row.names = 1:n) # 奇数轨预测值
    for(i in 1:ncol(pre)){
      pre[,i]=factor(pre[,i],levels = c('B','P'))
    }    
    # 1、先计算使用原始数据的9-16手
    # 奇数手：逆
    for (i in seq(9,15,2)) {
        # 计算final result
        final_result[i,]=new_quantize1266(path,set = data[c(seq(i-1,i-8,-1),i),m],Inverse = T)
        # 计算预测值
        pre[i,]=ifelse(final_result[i,]>0,ifelse(data[i,m]=='B','B','P'),ifelse(data[i,m]=='B','P','B'))
      }
    # 偶数手：顺
    for (i in seq(10,16,2)) {
        final_tempt=new_quantize1266(path,set = data[c(seq(i-8,i-1,1),i),m],Inverse = F)
        if(choice==12261){final_result[i,]=ifelse(final_result[i-1,]>0,-final_tempt,final_tempt)}
        else if(choice==12262){final_result[i,]=ifelse(final_result[i-1,]>0,final_tempt,-final_tempt)}
        else if(choice==12269){final_result[i,]=-final_tempt}
        pre[i,]=ifelse(final_result[i,]>0,ifelse(data[i,m]=='B','B','P'),ifelse(data[i,m]=='B','P','B'))
      }
    # 2、合再一起做，因为互相用到数据
    for(i in 17:n){
      # final result替换数据从重新计算
      # 奇数手（第1手逆序）固定使用original formula:
      if (i %in% seq(17,n,2)){
        final_result[i,]=diag(sapply(rbind(pre[seq(i-1,i-8,-1),],data.frame(matrix(rep(data[i,m],8),ncol=8),row.names = i))
                                     ,function(x){new_quantize1266(path,set=x,Inverse = F)}))
        # 计算预测值
        pre[i,]=ifelse(final_result[i,]>0,ifelse(data[i,m]=='B','B','P'),ifelse(data[i,m]=='B','P','B'))
      }
      # 偶数手（第2手，顺序:需要改seq索引和inverse=T）根据第1手的结果
      else{
        # 先计算8个formula下对应8条路径的的结果结果矩阵，1列表示1条路径下8个formula的结果
        temptMatrix_final_result=sapply(rbind(pre[seq(i-8,i-1,1),],data.frame(matrix(rep(data[i,m],8),ncol=8),row.names = i))
                                        ,function(x){new_quantize1266(path,set=x,Inverse = T)})
        # 不同的plan
        if(choice==12261){
          # 根据第1手结果against
          for(j in 1:8){
            # 计算每个final result:根据第一手的final result against,正时使用相反formula，负的时候使用相同formula
            if(final_result[i-1,j]>0)final_result[i,j]=temptMatrix_final_result[abs(j-9),j]
            else final_result[i,j]=temptMatrix_final_result[j,j]
          }
        }
        else if(choice==12262){
          # 根据第一手final result follow，plan12112
          # 正时使用相同formula，负的时候使用相反formula
          for(j in 1:8){
            # 计算每个final result:根据第一手的final result against,正时使用相反formula，负的时候使用相同formula
            if(final_result[i-1,j]<0)final_result[i,j]=temptMatrix_final_result[abs(j-9),j]
            else final_result[i,j]=temptMatrix_final_result[j,j]
          }
        }
        else if(choice==12269){
          # 根据第一手final result follow，plan12112
          # 正时使用相同formula，负的时候使用相反formula
          for(j in 1:8){
            # 计算每个final result:根据第一手的final result against,正时使用相反formula，负的时候使用相同formula
            final_result[i,j]=temptMatrix_final_result[abs(j-9),j]
          }
        }
        # 计算预测值,只与final result和真实数据有关
        pre[i,]=ifelse(final_result[i,]>0,ifelse(data[i,m]=='B','B','P'),ifelse(data[i,m]=='B','P','B'))
      }
    }
    # 3、去除无法计算final result的：
    final_result=final_result[-c(1:8),]
    pre=pre[-c(1:8),]
    colnames(final_result)=c(paste0('trace',1:nrow(path)))
    colnames(pre)=c(paste0('trace',1:nrow(path)))
    return(list(final_result,pre))
  }
  data_sort_sequence_inverse_one7911=function(data,m,choice,path=path){
    n=nrow(data)
    final_result=matrix(nrow = n,ncol = nrow(path)) # final_result
    row.names(final_result)=1:n
    pre=data.frame(matrix(nrow = n,ncol = nrow(path)),row.names = 1:n) # 奇数轨预测值
    for(i in 1:ncol(pre)){
      pre[,i]=factor(pre[,i],levels = c('B','P'))
    }    
    # 1、先计算使用原始数据的9-16手
    # 奇数手：顺
    for (i in seq(11,19,2)) {
        # 计算final result
        final_result[i,]=new_quantize7911(path,set = data[c(seq(i-10,i-1,1),i),m],Inverse = F)
        # 计算预测值
        pre[i,]=ifelse(final_result[i,]>0,ifelse(data[i,m]=='B','B','P'),ifelse(data[i,m]=='B','P','B'))
      }
    # 偶数手：逆
    for (i in seq(12,20,2)) {
        final_tempt=new_quantize7911(path,set = data[c(seq(i-1,i-10,-1),i),m],Inverse = T)
        if(choice==12111){final_result[i,]=ifelse(final_result[i-1,]>0,-final_tempt,final_tempt)}
        else if(choice==12112){final_result[i,]=ifelse(final_result[i-1,]>0,final_tempt,-final_tempt)}
        else if(choice==12119){final_result[i,]=-final_tempt}
        pre[i,]=ifelse(final_result[i,]>0,ifelse(data[i,m]=='B','B','P'),ifelse(data[i,m]=='B','P','B'))
      }
    # 2、合再一起做，因为互相用到数据
    for(i in 21:n){
      # final result替换数据从重新计算
      # 奇数手（第1手）固定使用original formula:
      if (i %in% seq(21,n,2)){
        final_result[i,]=diag(sapply(rbind(pre[seq(i-10,i-1,1),],data.frame(matrix(rep(data[i,m],8),ncol=8),row.names = i))
                                     ,function(x){new_quantize7911(path,set=x,Inverse = F)}))
        # 计算预测值
        pre[i,]=ifelse(final_result[i,]>0,ifelse(data[i,m]=='B','B','P'),ifelse(data[i,m]=='B','P','B'))
      }
      # 偶数手（第2手，逆序:需要改seq索引和inverse=T）根据第1手的结果
      else{
        # 先计算8个formula下对应8条路径的的结果结果矩阵，1列表示1条路径下8个formula的结果
        temptMatrix_final_result=sapply(rbind(pre[seq(i-1,i-10,-1),],data.frame(matrix(rep(data[i,m],8),ncol=8),row.names = i))
                                       ,function(x){new_quantize7911(path,set=x,Inverse = T)})
        # 不同的plan
        if(choice==12111){
          # 根据第1手结果against
          for(j in 1:8){
            # 计算每个final result:根据第一手的final result against,正时使用相反formula，负的时候使用相同formula
            if(final_result[i-1,j]>0)final_result[i,j]=temptMatrix_final_result[abs(j-9),j]
            else final_result[i,j]=temptMatrix_final_result[j,j]
          }
        }
        else if(choice==12112){
          # 根据第一手final result follow，plan12112
          # 正时使用相同formula，负的时候使用相反formula
          for(j in 1:8){
            # 计算每个final result:根据第一手的final result against,正时使用相反formula，负的时候使用相同formula
            if(final_result[i-1,j]<0)final_result[i,j]=temptMatrix_final_result[abs(j-9),j]
            else final_result[i,j]=temptMatrix_final_result[j,j]
          }
        }
        else if(choice==12119){
          # 根据第一手final result follow，plan12112
          # 正时使用相同formula，负的时候使用相反formula
          for(j in 1:8){
            # 计算每个final result:根据第一手的final result against,正时使用相反formula，负的时候使用相同formula
            final_result[i,j]=temptMatrix_final_result[abs(j-9),j]
          }
        }
        # 计算预测值,只与final result和真实数据有关
        pre[i,]=ifelse(final_result[i,]>0,ifelse(data[i,m]=='B','B','P'),ifelse(data[i,m]=='B','P','B'))
      }
    }
    # 3、去除无法计算final result的：
    final_result=final_result[-c(1:10),]
    pre=pre[-c(1:10),]
    colnames(final_result)=c(paste0('trace',1:nrow(path)))
    colnames(pre)=c(paste0('trace',1:nrow(path)))
    return(list(final_result,pre))
  }
  data_sort_sequence_inverse_one91113=function(data,m,choice,path=path){
    n=nrow(data)
    final_result=matrix(nrow = n,ncol = nrow(path)) # final_result
    row.names(final_result)=1:n
    pre=data.frame(matrix(nrow = n,ncol = nrow(path)),row.names = 1:n) # 奇数轨预测值
    for(i in 1:ncol(pre)){
      pre[,i]=factor(pre[,i],levels = c('B','P'))
    }    
    # 1、先计算使用原始数据的9-16手
    # 奇数手：顺
    for (i in seq(13,23,2)) {
        # 计算final result
        final_result[i,]=new_quantize91113(path,set = data[c(seq(i-12,i-1,1),i),m],Inverse = F)
        # 计算预测值
        pre[i,]=ifelse(final_result[i,]>0,ifelse(data[i,m]=='B','B','P'),ifelse(data[i,m]=='B','P','B'))
      }
    # 偶数手：逆
    for (i in seq(14,24,2)) {
        final_tempt=new_quantize91113(path,set = data[c(seq(i-1,i-12,-1),i),m],Inverse = T)
        if(choice==12111){final_result[i,]=ifelse(final_result[i-1,]>0,-final_tempt,final_tempt)}
        else if(choice==12112){final_result[i,]=ifelse(final_result[i-1,]>0,final_tempt,-final_tempt)}
        else if(choice==12119){final_result[i,]=-final_tempt}
        pre[i,]=ifelse(final_result[i,]>0,ifelse(data[i,m]=='B','B','P'),ifelse(data[i,m]=='B','P','B'))
      }
    # 2、合再一起做，因为互相用到数据
    for(i in 25:n){
      # final result替换数据从重新计算
      # 奇数手（第1手）固定使用original formula:
      if (i %in% seq(25,n,2)){
        final_result[i,]=diag(sapply(rbind(pre[seq(i-12,i-1,1),],data.frame(matrix(rep(data[i,m],8),ncol=8),row.names = i))
                                     ,function(x){new_quantize91113(path,set=x,Inverse = F)}))
        # 计算预测值
        pre[i,]=ifelse(final_result[i,]>0,ifelse(data[i,m]=='B','B','P'),ifelse(data[i,m]=='B','P','B'))
      }
      # 偶数手（第2手，逆序:需要改seq索引和inverse=T）根据第1手的结果
      else{
        # 先计算8个formula下对应8条路径的的结果结果矩阵，1列表示1条路径下8个formula的结果
        temptMatrix_final_result=sapply(rbind(pre[seq(i-1,i-12,-1),],data.frame(matrix(rep(data[i,m],8),ncol=8),row.names = i))
                                       ,function(x){new_quantize91113(path,set=x,Inverse = T)})
        # 不同的plan
        if(choice==12111){
          # 根据第1手结果against
          for(j in 1:8){
            # 计算每个final result:根据第一手的final result against,正时使用相反formula，负的时候使用相同formula
            if(final_result[i-1,j]>0)final_result[i,j]=temptMatrix_final_result[abs(j-9),j]
            else final_result[i,j]=temptMatrix_final_result[j,j]
          }
        }
        else if(choice==12112){
          # 根据第一手final result follow，plan12112
          # 正时使用相同formula，负的时候使用相反formula
          for(j in 1:8){
            # 计算每个final result:根据第一手的final result against,正时使用相反formula，负的时候使用相同formula
            if(final_result[i-1,j]<0)final_result[i,j]=temptMatrix_final_result[abs(j-9),j]
            else final_result[i,j]=temptMatrix_final_result[j,j]
          }
        }
        else if(choice==12119){
          # 根据第一手final result follow，plan12112
          # 正时使用相同formula，负的时候使用相反formula
          for(j in 1:8){
            # 计算每个final result:根据第一手的final result against,正时使用相反formula，负的时候使用相同formula
            final_result[i,j]=temptMatrix_final_result[abs(j-9),j]
          }
        }
        # 计算预测值,只与final result和真实数据有关
        pre[i,]=ifelse(final_result[i,]>0,ifelse(data[i,m]=='B','B','P'),ifelse(data[i,m]=='B','P','B'))
      }
    }
    # 3、去除无法计算final result的：
    final_result=final_result[-c(1:12),]
    pre=pre[-c(1:12),]
    colnames(final_result)=c(paste0('trace',1:nrow(path)))
    colnames(pre)=c(paste0('trace',1:nrow(path)))
    return(list(final_result,pre))
  }
  # data_sort_sequence_inverse_one(data,1,choice = 12111,path = path_BP_YAY579,R=5)
  # 单轨，轨迹数据，4个一组
  data_sort_sequence_inverse_two=function(data,m,choice,path=path,R){
    n=nrow(data)
    final_result=matrix(nrow = n,ncol = 8) # final_result
    row.names(final_result)=1:n
    pre=data.frame(matrix(nrow = n,ncol = 8),row.names = 1:n) # 奇数轨预测值
    for(i in 1:ncol(pre)){
      pre[,i]=factor(pre[,i],levels = c('B','P'))
    }
    # 1、先计算使用原始数据的9-16手
    # 第一手数据，奇数手：顺,使用original formula
    for (i in seq(9,13,4)) {
        # 计算final result
        final_result[i,]=new_quantize(path,set = data[c(seq(i-8,i-1,1),i),m],Inverse = F,R)
        # 计算预测值
        pre[i,]=ifelse(final_result[i,]>0,ifelse(data[i,m]=='B','B','P'),ifelse(data[i,m]=='B','P','B'))
      }
    # 第二手，偶数手：逆，根据第一手follow或者against
    for (i in seq(10,14,4)) {
        final_tempt=new_quantize(path,set = data[c(seq(i-2,i-9,-1),i),m],Inverse = T,R)
        if(choice==12113|choice==12115|choice==12117){
          # against
          final_result[i,]=ifelse(final_result[i-1,]>0,-final_tempt,final_tempt)}
        else if(choice==12114|choice==12116|choice==12118){
          # follow
          final_result[i,]=ifelse(final_result[i-1,]>0,final_tempt,-final_tempt)}
        pre[i,]=ifelse(final_result[i,]>0,ifelse(data[i,m]=='B','B','P'),ifelse(data[i,m]=='B','P','B'))
    }
    # 第三手数据，奇数手：顺,使用 固定相反 formula、follow第一手或者against第一手
    for (i in seq(11,15,4)) {
      # 计算final result
      final_tempt=new_quantize(path,set = data[c(seq(i-8,i-1,1),i),m],Inverse = F,R)
      if(choice==12113|choice==12114){
        # 固定的相反
        final_result[i,]=-final_tempt
      }
      else if(choice==12115|choice==12116){
        # against第一手
        final_result[i,]=ifelse(final_result[i-2,]>0,-final_tempt,final_tempt)
      }
      else if(choice==12117|choice==12118){
        # follow 第一手
        final_result[i,]=ifelse(final_result[i-2,]>0,final_tempt,-final_tempt)
      }
      # 计算预测值
      pre[i,]=ifelse(final_result[i,]>0,ifelse(data[i,m]=='B','B','P'),ifelse(data[i,m]=='B','P','B'))
    }
    # 第四手，偶数手：逆，根据第三手follow或者against，注意在第三手非固定使用formula时，也受到第一手影响
    for (i in seq(12,16,4)) {
      final_tempt=new_quantize(path,set = data[c(seq(i-2,i-9,-1),i),m],Inverse = T,R)
      if(choice==12113){
        # against
        final_result[i,]=ifelse(final_result[i-1,]>0,-final_tempt,final_tempt)}
      else if(choice==12114){
        # follow
        final_result[i,]=ifelse(final_result[i-1,]>0,final_tempt,-final_tempt)}
      else if(choice==12115){
        # against 根据1、3手结果，3手against第1手
        final_result[i,]=ifelse(final_result[i-1,]*final_result[i-3,]>0,final_tempt,-final_tempt)}
      else if(choice==12116){
        # follow 根据1、3手结果，3手against第1手
        final_result[i,]=ifelse(final_result[i-1,]*final_result[i-3,]<0,final_tempt,-final_tempt)}
      else if(choice==12117){
        # against 根据1、3手结果，3手follow第1手
        final_result[i,]=ifelse(final_result[i-1,]*final_result[i-3,]<0,final_tempt,-final_tempt)}
      else if(choice==12118){
        # follow 根据1、3手结果，3手follow第1手
        final_result[i,]=ifelse(final_result[i-1,]*final_result[i-3,]>0,final_tempt,-final_tempt)}
      # 计算预测值
      pre[i,]=ifelse(final_result[i,]>0,ifelse(data[i,m]=='B','B','P'),ifelse(data[i,m]=='B','P','B'))
      }
    # 2、合再一起做，因为互相用到数据
    for(i in 17:n){
      # 1、计算final result替换数据从重新计算
      # 奇数手（第1手顺）固定使用original formula:
      if (i %in% seq(17,n,4)){
        final_result[i,]=diag(sapply(rbind(pre[seq(i-8,i-1,1),],data.frame(matrix(rep(data[i,m],8),ncol=8),row.names = i))
                                     ,function(x){new_quantize(path,set=x,Inverse = F,R)}))
      }
      # 偶数数手（第2手逆）根据第一手的against或follow:
      else if(i %in% seq(18,n,4)){
        temptMatrix_final_result=sapply(rbind(pre[seq(i-2,i-9,-1),],data.frame(matrix(rep(data[i,m],8),ncol=8),row.names = i))
                                        ,function(x){new_quantize(path,set=x,Inverse = T,R)})
        # 不同的plan
        if(choice==12113|choice==12115|choice==12117){
          # 根据第1手结果against
          for(j in 1:8){
            # 计算每个final result:根据第一手的final result against,正时使用相反formula，负的时候使用相同formula
            if(final_result[i-1,j]>0)final_result[i,j]=temptMatrix_final_result[abs(j-9),j]
            else final_result[i,j]=temptMatrix_final_result[j,j]
            }
          }
        else if(choice==12114|choice==12116|choice==12118){
          # follow
          for(j in 1:8){
            # 计算每个final result:根据第一手的final result against,正时使用相反formula，负的时候使用相同formula
            if(final_result[i-1,j]<0)final_result[i,j]=temptMatrix_final_result[abs(j-9),j]
            else final_result[i,j]=temptMatrix_final_result[j,j]
            }
          }
      }
      # 奇数手（第3手顺）固定相反formula或根据 第一手 的against或follow:
      else if(i %in% seq(19,n,4)){
        temptMatrix_final_result=sapply(rbind(pre[seq(i-8,i-1,1),],data.frame(matrix(rep(data[i,m],8),ncol=8),row.names = i))
                                     ,function(x){new_quantize(path,set=x,Inverse = F,R)})
        if(choice==12113|choice==12114){
          for(j in 1:8){
            # 计算每个final result:使用相反formula,次对角元素
            final_result[i,j]=temptMatrix_final_result[abs(j-9),j]
          }
        }
        else if(choice==12115|choice==12116){
          # against第一手
          for(j in 1:8){
            # 计算每个final result:根据第一手的final result against,正时使用相反formula，负的时候使用相同formula
            if(final_result[i-2,j]>0)final_result[i,j]=temptMatrix_final_result[abs(j-9),j]
            else final_result[i,j]=temptMatrix_final_result[j,j]
          }
        }
        else if(choice==12117|choice==12118){
          # follow 第一手
          for(j in 1:8){
            # 计算每个final result:根据第一手的final result against,正时使用相反formula，负的时候使用相同formula
            if(final_result[i-2,j]<0)final_result[i,j]=temptMatrix_final_result[abs(j-9),j]
            else final_result[i,j]=temptMatrix_final_result[j,j]
          }
        }
      }
      # 偶数手（第4手逆）第1、3手的结果against或follow
      else{
        # 先计算8个formula下对应8条路径的的结果结果矩阵，1列表示1条路径下8个formula的结果
        temptMatrix_final_result=sapply(rbind(pre[seq(i-2,i-9,-1),],data.frame(matrix(rep(data[i,m],8),ncol=8),row.names = i))
                                       ,function(x){new_quantize(path,set=x,Inverse = T,R)})
        # 不同的plan
        if(choice==12113){
          # 简单against第三手
          for(j in 1:8){
            # 计算每个final result:根据第三手的final result against,正时使用相反formula，负的时候使用相同formula
            if(final_result[i-1,j]>0)final_result[i,j]=temptMatrix_final_result[abs(j-9),j]
            else final_result[i,j]=temptMatrix_final_result[j,j]
            }
          }
        else if(choice==12114){
          # 简单follow第三手
          for(j in 1:8){
            # 计算每个final result:根据第一手的final result against,正时使用相反formula，负的时候使用相同formula
            if(final_result[i-1,j]<0)final_result[i,j]=temptMatrix_final_result[abs(j-9),j]
            else final_result[i,j]=temptMatrix_final_result[j,j]
            }
          }
        else if(choice==12115){
          # against 根据1、3手结果，3手against第1手
          for(j in 1:8){
            if(final_result[i-1,j]*final_result[i-3,j]<0)final_result[i,j]=temptMatrix_final_result[abs(j-9),j]
            else final_result[i,j]=temptMatrix_final_result[j,j]
            }
          }
        else if(choice==12116){
          # follow 根据1、3手结果，3手against第1手
          for(j in 1:8){
            if(final_result[i-1,j]*final_result[i-3,j]>0)final_result[i,j]=temptMatrix_final_result[abs(j-9),j]
            else final_result[i,j]=temptMatrix_final_result[j,j]
            }
          }
        else if(choice==12117){
          # against 根据1、3手结果，3手follow第1手
          for(j in 1:8){
            if(final_result[i-1,j]*final_result[i-3,j]>0)final_result[i,j]=temptMatrix_final_result[abs(j-9),j]
            else final_result[i,j]=temptMatrix_final_result[j,j]
            }
          }
        else if(choice==12118){
          # follow 根据1、3手结果，3手follow第1手
          for(j in 1:8){
            if(final_result[i-1,j]*final_result[i-3,j]<0)final_result[i,j]=temptMatrix_final_result[abs(j-9),j]
            else final_result[i,j]=temptMatrix_final_result[j,j]
            }
          }
      }
      # 2、计算预测值,只与final result和真实数据有关
      pre[i,]=ifelse(final_result[i,]>0,ifelse(data[i,m]=='B','B','P'),ifelse(data[i,m]=='B','P','B'))
    }
    # 3、去除无法计算final result的：
    final_result=final_result[-c(1:8),]
    pre=pre[-c(1:8),]
    colnames(final_result)=c(paste0('trace',1:nrow(path)))
    colnames(pre)=c(paste0('trace',1:nrow(path)))
    return(list(final_result,pre))
  }
  # 单轨，原始数据，2个一组
  data_sort_sequence_inverse_origin_one=function(data,m,choice,path=path,R){
    n=nrow(data)
    final_result=matrix(nrow = n,ncol = nrow(path)) # final_result
    row.names(final_result)=1:n
    # 1、先计算使用原始数据的9-16手
    # 奇数手：顺
    for (i in seq(9,n,2)) {
        # 计算final result
        final_result[i,]=new_quantize(path,set = data[c(seq(i-8,i-1,1),i),m],Inverse = F,R)
      }
    # 偶数手：逆
    for (i in seq(10,n,2)) {
        final_tempt=new_quantize(path,set = data[c(seq(i-1,i-8,-1),i),m],Inverse = T,R)
        if(choice==121110){final_result[i,]=ifelse(final_result[i-1,]>0,-final_tempt,final_tempt)}
        else if(choice==121190){final_result[i,]=-final_tempt}
      }
    # 2、去除无法计算final result的：
    final_result=final_result[-c(1:8),]
    colnames(final_result)=c(paste0('trace',1:nrow(path)))
    return(list(final_result))
  }
  data_sort_1226_origin_one=function(data,m,choice,path=path){
    n=nrow(data)
    final_result=matrix(nrow = n,ncol = nrow(path)) # final_result
    row.names(final_result)=1:n
    # 奇数手：逆
    for (i in seq(9,n,2)) {
      # 计算final result
      final_result[i,]=new_quantize1266(path,set = data[c(seq(i-1,i-8,-1),i),m],Inverse = T)
    }
    # 偶数手：顺
    for (i in seq(10,n,2)) {
      final_tempt=new_quantize1266(path,set = data[c(seq(i-8,i-1,1),i),m],Inverse = F)
      if(choice==122610){final_result[i,]=ifelse(final_result[i-1,]>0,-final_tempt,final_tempt)}
      else if(choice==122620){final_result[i,]=ifelse(final_result[i-1,]>0,final_tempt,-final_tempt)}
      else if(choice==122690){final_result[i,]=-final_tempt}
    }
    # 2、去除无法计算final result的：
    final_result=final_result[-c(1:8),]
    colnames(final_result)=c(paste0('trace',1:nrow(path)))
    return(list(final_result))
  }
  data_sort_sequence_inverse_origin_one7911=function(data,m,choice,path=path){
    n=nrow(data)
    final_result=matrix(nrow = n,ncol = nrow(path)) # final_result
    row.names(final_result)=1:n
    # 1、先计算使用原始数据的9-16手
    # 奇数手：顺
    for (i in seq(11,n,2)) {
        # 计算final result
        final_result[i,]=new_quantize7911(path,set = data[c(seq(i-10,i-1,1),i),m],Inverse = F)
      }
    # 偶数手：逆
    for (i in seq(12,n,2)) {
        final_tempt=new_quantize7911(path,set = data[c(seq(i-1,i-10,-1),i),m],Inverse = T)
        if(choice==121110){final_result[i,]=ifelse(final_result[i-1,]>0,-final_tempt,final_tempt)}
        else if(choice==121190){final_result[i,]=-final_tempt}
      }
    # 2、去除无法计算final result的：
    final_result=final_result[-c(1:10),]
    colnames(final_result)=c(paste0('trace',1:nrow(path)))
    return(list(final_result))
  }
  data_sort_sequence_inverse_origin_one91113=function(data,m,choice,path=path){
    n=nrow(data)
    final_result=matrix(nrow = n,ncol = nrow(path)) # final_result
    row.names(final_result)=1:n
    # 1、先计算使用原始数据的9-16手
    # 奇数手：顺
    for (i in seq(13,n,2)) {
        # 计算final result
        final_result[i,]=new_quantize91113(path,set = data[c(seq(i-12,i-1,1),i),m],Inverse = F)
      }
    # 偶数手：逆
    for (i in seq(14,n,2)) {
        final_tempt=new_quantize91113(path,set = data[c(seq(i-1,i-12,-1),i),m],Inverse = T)
        if(choice==121110){final_result[i,]=ifelse(final_result[i-1,]>0,-final_tempt,final_tempt)}
        else if(choice==121190){final_result[i,]=-final_tempt}
      }
    # 2、去除无法计算final result的：
    final_result=final_result[-c(1:12),]
    colnames(final_result)=c(paste0('trace',1:nrow(path)))
    return(list(final_result))
  }
  # data_sort_sequence_inverse_origin_one(data,1,choice = 121110,path = path_BP_YAY579,R=9)
  # 单轨，原始数据，4个一组
  data_sort_sequence_inverse_origin_two=function(data,m,choice,path=path,R){
    n=nrow(data)
    final_result=matrix(nrow = n,ncol = 8) # final_result
    row.names(final_result)=1:n
    # 1、先计算使用原始数据的9-16手
    # 第一手数据，奇数手：顺,使用original formula
    for (i in seq(9,n,4)) {
      # 计算final result
      final_result[i,]=new_quantize(path,set = data[c(seq(i-8,i-1,1),i),m],Inverse = F,R)
    }
    # 第二手，偶数手：逆，根据第一手follow或者against
    for (i in seq(10,n,4)) {
      final_tempt=new_quantize(path,set = data[c(seq(i-2,i-9,-1),i),m],Inverse = T,R)
      if(choice==121130|choice==121150|choice==121170){
        # against
        final_result[i,]=ifelse(final_result[i-1,]>0,-final_tempt,final_tempt)}
      else if(choice==121140|choice==121160|choice==121180){
        # follow
        final_result[i,]=ifelse(final_result[i-1,]>0,final_tempt,-final_tempt)}
    }
    # 第三手数据，奇数手：顺,使用 固定相反 formula、follow第一手或者against第一手
    for (i in seq(11,n,4)) {
      # 计算final result
      final_tempt=new_quantize(path,set = data[c(seq(i-8,i-1,1),i),m],Inverse = F,R)
      if(choice==121130|choice==121140){
        # 固定的相反
        final_result[i,]=-final_tempt
      }
      else if(choice==121150|choice==121160){
        # against第一手
        final_result[i,]=ifelse(final_result[i-2,]>0,-final_tempt,final_tempt)
      }
      else if(choice==121170|choice==121180){
        # follow 第一手
        final_result[i,]=ifelse(final_result[i-2,]>0,final_tempt,-final_tempt)
      }
    }
    # 第四手，偶数手：逆，根据第三手follow或者against，注意在第三手非固定使用formula时，也受到第一手影响
    for (i in seq(12,n,4)) {
      final_tempt=new_quantize(path,set = data[c(seq(i-2,i-9,-1),i),m],Inverse = T,R)
      if(choice==121130){
        # against
        final_result[i,]=ifelse(final_result[i-1,]>0,-final_tempt,final_tempt)}
      else if(choice==121140){
        # follow
        final_result[i,]=ifelse(final_result[i-1,]>0,final_tempt,-final_tempt)}
      else if(choice==121150){
        # against 根据1、3手结果，3手against第1手
        final_result[i,]=ifelse(final_result[i-1,]*final_result[i-3,]>0,final_tempt,-final_tempt)}
      else if(choice==121160){
        # follow 根据1、3手结果，3手against第1手
        final_result[i,]=ifelse(final_result[i-1,]*final_result[i-3,]<0,final_tempt,-final_tempt)}
      else if(choice==121170){
        # against 根据1、3手结果，3手follow第1手
        final_result[i,]=ifelse(final_result[i-1,]*final_result[i-3,]<0,final_tempt,-final_tempt)}
      else if(choice==121180){
        # follow 根据1、3手结果，3手follow第1手
        final_result[i,]=ifelse(final_result[i-1,]*final_result[i-3,]>0,final_tempt,-final_tempt)}
    }
    # 2、去除无法计算final result的：
    final_result=final_result[-c(1:8),]
    colnames(final_result)=c(paste0('trace',1:8))
    return(list(final_result))
  }
}
# data_test=read.csv('data//data_test.csv',header = F)
# data_sort_sequence_inverse_origin_one(data=data_test,m=1,choice = 121110,path = path_R11,R9=F,R11=T,R13=F)
run=function(data=data,path=path,choice,R=0){
  library(stringr)
  n_path=nrow(path)
  minus_result=matrix(nrow =length(data),ncol = n_path)
  plus=matrix(nrow =length(data),ncol = n_path)
  minus=matrix(nrow =length(data),ncol = n_path)
  new_indexE1=matrix(nrow =length(data),ncol = n_path)
  new_indexE2=matrix(nrow =length(data),ncol = n_path)
  for (m in 1:length(data)) {
    if(m%%50==0)print(m)
    if(choice %in% c(12111,12112,12119)){
      temp=data_sort_sequence_inverse_one(data,m,choice,path,R)
    }
    else if(choice %in% c(121110,121120,121190)){
      temp=data_sort_sequence_inverse_origin_one(data,m,choice,path,R)
    }
    else if(choice %in% c(12261,12262,12269)){
      temp=data_sort_1226_one(data,m,choice,path)
    }
    else if(choice %in% c(122610,122620,122690)){
      temp=data_sort_1226_origin_one(data,m,choice,path)
    }
    else if(choice %in% c(12113,12114,12115,12116,12117,12118)){
      temp=data_sort_sequence_inverse_two(data,m,choice,path,R)
    }
    else if(choice %in% c(121130,121140,121150,121160,121170,121180)){
      temp=data_sort_sequence_inverse_origin_two(data,m,choice,path,R)
    }
    # 1）初始化变量，以记录指标：
    {
      sumtemp=temp[[1]]
      n_sumtemp=nrow(sumtemp)
    }
    
    # 2）每个数据集m计算多个评价指标：
    {
      # 计算连续负 “最大” 的手数
      minus_result[m,]=detect_continue_minus(sumtemp)
      # 计算新指标:评价指标
      # 1、正负比例
      plus[m,]=apply(sumtemp, 2, function(x)sum(x>0)) 
      minus[m,]=n_sumtemp-plus[m,]# 数据m下结果负的手数
      symbol=ifelse(sumtemp>0,1,-1)
      patternE1=matrix(rep(c(rep(c(1,2),(n_sumtemp)%/%2),c(1,2)[0:((n_sumtemp)%%2)]),8),ncol = 8)
      new_indexE1[m,]=colSums(patternE1*symbol)
      patternE2=matrix(rep(c(rep(c(2,1),(n_sumtemp)%/%2),c(2,1)[0:((n_sumtemp)%%2)]),8),ncol = 8)
      new_indexE2[m,]=colSums(patternE2*symbol)
    } 
    
  }
  
  colnames(minus_result)=c(paste0('formula',1:n_path))
  # result存放最大连续负个数
  result=matrix(nrow =max(minus_result) ,ncol = n_path)
  for (i in 1:(max(minus_result))){
    result[i,]=apply(minus_result,2,function(x)sum(x==i))
  }
  colnames(result)=c(paste0('formula',1:n_path))
  
  # satisfy为某一个formula满足小于等于5的数据集个数：
  satisfy=apply(result[1:5,], 2, sum)
  return(list(minus_result,result,satisfy,plus,minus,
              new_indexE1,new_indexE2))
}
run7911=function(data=data,path=path,choice){
  library(stringr)
  n_path=nrow(path)
  minus_result=matrix(nrow =length(data),ncol = n_path)
  plus=matrix(nrow =length(data),ncol = n_path)
  minus=matrix(nrow =length(data),ncol = n_path)
  new_indexE1=matrix(nrow =length(data),ncol = n_path)
  new_indexE2=matrix(nrow =length(data),ncol = n_path)
  for (m in 1:length(data)) {
    if(m%%50==0)print(m)
    if(choice %in% c(12111,12112,12119)){
      temp=data_sort_sequence_inverse_one7911(data,m,choice,path)
    }
    else if(choice %in% c(121110,121120,121190)){
      temp=data_sort_sequence_inverse_origin_one7911(data,m,choice,path)
    }
    else if(choice %in% c(12113,12114,12115,12116,12117,12118)){
      temp=data_sort_sequence_inverse_two(data,m,choice,path,R)
    }
    else if(choice %in% c(121130,121140,121150,121160,121170,121180)){
      temp=data_sort_sequence_inverse_origin_two(data,m,choice,path,R)
    }
    # 1）初始化变量，以记录指标：
    {
      sumtemp=temp[[1]]
      n_sumtemp=nrow(sumtemp)
    }
    
    # 2）每个数据集m计算多个评价指标：
    {
      # 计算连续负 “最大” 的手数
      minus_result[m,]=detect_continue_minus(sumtemp)
      # 计算新指标:评价指标
      # 1、正负比例
      plus[m,]=apply(sumtemp, 2, function(x)sum(x>0)) 
      minus[m,]=n_sumtemp-plus[m,]# 数据m下结果负的手数
      symbol=ifelse(sumtemp>0,1,-1)
      patternE1=matrix(rep(c(rep(c(1,2),(n_sumtemp)%/%2),c(1,2)[0:((n_sumtemp)%%2)]),8),ncol = 8)
      new_indexE1[m,]=colSums(patternE1*symbol)
      patternE2=matrix(rep(c(rep(c(2,1),(n_sumtemp)%/%2),c(2,1)[0:((n_sumtemp)%%2)]),8),ncol = 8)
      new_indexE2[m,]=colSums(patternE2*symbol)
    } 
    
  }
  
  colnames(minus_result)=c(paste0('formula',1:n_path))
  # result存放最大连续负个数
  result=matrix(nrow =max(minus_result) ,ncol = n_path)
  for (i in 1:(max(minus_result))){
    result[i,]=apply(minus_result,2,function(x)sum(x==i))
  }
  colnames(result)=c(paste0('formula',1:n_path))
  
  # satisfy为某一个formula满足小于等于5的数据集个数：
  satisfy=apply(result[1:5,], 2, sum)
  return(list(minus_result,result,satisfy,plus,minus,
              new_indexE1,new_indexE2))
}
run91113=function(data=data,path=path,choice){
  library(stringr)
  n_path=nrow(path)
  minus_result=matrix(nrow =length(data),ncol = n_path)
  plus=matrix(nrow =length(data),ncol = n_path)
  minus=matrix(nrow =length(data),ncol = n_path)
  new_indexE1=matrix(nrow =length(data),ncol = n_path)
  new_indexE2=matrix(nrow =length(data),ncol = n_path)
  for (m in 1:length(data)) {
    if(m%%50==0)print(m)
    if(choice %in% c(12111,12112,12119)){
      temp=data_sort_sequence_inverse_one91113(data,m,choice,path)
    }
    else if(choice %in% c(121110,121120,121190)){
      temp=data_sort_sequence_inverse_origin_one91113(data,m,choice,path)
    }
    else if(choice %in% c(12113,12114,12115,12116,12117,12118)){
      temp=data_sort_sequence_inverse_two(data,m,choice,path,R)
    }
    else if(choice %in% c(121130,121140,121150,121160,121170,121180)){
      temp=data_sort_sequence_inverse_origin_two(data,m,choice,path,R)
    }
    # 1）初始化变量，以记录指标：
    {
      sumtemp=temp[[1]]
      n_sumtemp=nrow(sumtemp)
    }
    
    # 2）每个数据集m计算多个评价指标：
    {
      # 计算连续负 “最大” 的手数
      minus_result[m,]=detect_continue_minus(sumtemp)
      # 计算新指标:评价指标
      # 1、正负比例
      plus[m,]=apply(sumtemp, 2, function(x)sum(x>0)) 
      minus[m,]=n_sumtemp-plus[m,]# 数据m下结果负的手数
      symbol=ifelse(sumtemp>0,1,-1)
      patternE1=matrix(rep(c(rep(c(1,2),(n_sumtemp)%/%2),c(1,2)[0:((n_sumtemp)%%2)]),8),ncol = 8)
      new_indexE1[m,]=colSums(patternE1*symbol)
      patternE2=matrix(rep(c(rep(c(2,1),(n_sumtemp)%/%2),c(2,1)[0:((n_sumtemp)%%2)]),8),ncol = 8)
      new_indexE2[m,]=colSums(patternE2*symbol)
    } 
    
  }
  
  colnames(minus_result)=c(paste0('formula',1:n_path))
  # result存放最大连续负个数
  result=matrix(nrow =max(minus_result) ,ncol = n_path)
  for (i in 1:(max(minus_result))){
    result[i,]=apply(minus_result,2,function(x)sum(x==i))
  }
  colnames(result)=c(paste0('formula',1:n_path))
  
  # satisfy为某一个formula满足小于等于5的数据集个数：
  satisfy=apply(result[1:5,], 2, sum)
  return(list(minus_result,result,satisfy,plus,minus,
              new_indexE1,new_indexE2))
}
# 2.6 展示函数
present=function(result){
  library(stringr)
  print(result[[2]])
  cat('\n')
  final_result=matrix(round(colSums(result[[4]])/colSums(result[[5]]),3),nrow = 1)
  row.names(final_result)=c('plus/minus')
  colnames(final_result)=c(paste0('formula',1:8))
  final_result1=rbind(result[[3]],apply(result[[6]], 2, sum))
  final_result1=rbind(final_result1,apply(result[[7]], 2, sum))
  final_result1=rbind(final_result1,colSums(result[[4]]))
  final_result1=rbind(final_result1,colSums(result[[5]]))
  row.names(final_result1)=c('quantity','E1_sum','E2_sum','plus','minus')
  
  first5_e1=apply(result[[6]], 2, function(x)str_c(sort(x,decreasing = T)[1:5],collapse = ' '))
  last5_e1=apply(result[[6]], 2, function(x)str_c(sort(x,decreasing = F)[1:5],collapse = ' '))
  first5_e2=apply(result[[7]], 2, function(x)str_c(sort(x,decreasing = T)[1:5],collapse = ' '))
  last5_e2=apply(result[[7]], 2, function(x)str_c(sort(x,decreasing = F)[1:5],collapse = ' '))
  
  e1=str_c(first5_e1,last5_e1,sep  = ';')
  e2=str_c(first5_e2,last5_e2,sep  = ';')
  ss=data.frame(E1=e1,E2=e2)
  row.names(ss)=c(paste0('formula',1:8))
  colnames(ss)=c('first&last5 of index E1','first&last5 of index E2')
  print(ss)
  cat('\n')
  print(final_result)
  print(final_result1)
}

# write.csv(as.data.frame(sumtemp[,8]),'C:\\Users\\Think\\Desktop\\1.csv',row.names = F)
# 91113===========================
plan1211_1_YAY91113=run7913(data = all_data,path = path_BP_YAY91113,choice=12111)
present(plan1211_1_YAY91113)
plan1211_2_YAY91113=run91113(data = all_data,path = path_BP_YAY91113,choice=12112)
present(plan1211_2_YAY91113)
plan1211_9_YAY91113=run91113(data = all_data,path = path_BP_YAY91113,choice=12119)
present(plan1211_9_YAY91113)

plan1211_1_YAY91113_original=run91113(data = all_data,path = path_BP_YAY91113,choice=121110)
present(plan1211_1_YAY91113_original)
plan1211_2_YAY91113_original=run91113(data = all_data,path = path_BP_YAY91113,choice=121120)
present(plan1211_2_YAY91113_original)
plan1211_9_YAY91113_original=run91113(data = all_data,path = path_BP_YAY91113,choice=121190)
present(plan1211_9_YAY91113_original)
# 7911===============================================
plan1211_1_YAY7911=run7911(data = all_data,path = path_BP_YAY7911,choice=12111)
present(plan1211_1_YAY7911)
plan1211_2_YAY7911=run7911(data = all_data,path = path_BP_YAY7911,choice=12112)
present(plan1211_2_YAY7911)
plan1211_9_YAY7911=run7911(data = all_data,path = path_BP_YAY7911,choice=12119)
present(plan1211_9_YAY7911)

plan1211_1_YAY7911_original=run7911(data = all_data,path = path_BP_YAY7911,choice=121110)
present(plan1211_1_YAY7911_original)
plan1211_2_YAY7911_original=run7911(data = all_data,path = path_BP_YAY7911,choice=121120)
present(plan1211_2_YAY7911_original)
plan1211_9_YAY7911_original=run7911(data = all_data,path = path_BP_YAY7911,choice=121190)
present(plan1211_9_YAY7911_original)
# plan1211=======================================================
plan1211_1_YAY579=run(data = all_data,path = path_BP_YAY579,choice=12111)
present(plan1211_1_YAY579)
plan1211_2_YAY579=run(data = all_data,path = path_BP_YAY579,choice=12112)
present(plan1211_2_YAY579)
plan1211_3_YAY579=run(data = all_data,path = path_BP_YAY579,choice=12113)
present(plan1211_3_YAY579)
plan1211_4_YAY579=run(data = all_data,path = path_BP_YAY579,choice=12114)
present(plan1211_4_YAY579)
plan1211_5_YAY579=run(data = all_data,path = path_BP_YAY579,choice=12115)
present(plan1211_5_YAY579)
plan1211_6_YAY579=run(data = all_data,path = path_BP_YAY579,choice=12116)
present(plan1211_6_YAY579)
plan1211_7_YAY579=run(data = all_data,path = path_BP_YAY579,choice=12117)
present(plan1211_7_YAY579)
plan1211_8_YAY579=run(data = all_data,path = path_BP_YAY579,choice=12118)
present(plan1211_8_YAY579)
plan1211_9_YAY579=run(data = all_data,path = path_BP_YAY579,choice=12119)
present(plan1211_9_YAY579)

# plan1266===========================================================
plan1226_1_YAY579=run(data = all_data,path = path_BP_YAY579,choice=12261)
present(plan1226_1_YAY579)
plan1226_2_YAY579=run(data = all_data,path = path_BP_YAY579,choice=12262)
present(plan1226_2_YAY579)

plan1226_1_YAY579_original=run(data = all_data,path = path_BP_YAY579,choice=122610)
present(plan1226_1_YAY579_original)
plan1226_2_YAY579_original=run(data = all_data,path = path_BP_YAY579,choice=122620)
present(plan1226_2_YAY579_original)
# 使用原始数据======================================================
plan1211_1_YAY579_original=run(data = all_data,path = path_BP_YAY579,choice=121110)
present(plan1211_1_YAY579_original)
plan1211_2_YAY579_original=run(data = all_data,path = path_BP_YAY579,choice=121120)
present(plan1211_2_YAY579_original)
plan1211_3_YAY579_original=run(data = all_data,path = path_BP_YAY579,choice=121130)
present(plan1211_3_YAY579_original)
plan1211_4_YAY579_original=run(data = all_data,path = path_BP_YAY579,choice=121140)
present(plan1211_4_YAY579_original)
plan1211_5_YAY579_original=run(data = all_data,path = path_BP_YAY579,choice=121150)
present(plan1211_5_YAY579_original)
plan1211_6_YAY579_original=run(data = all_data,path = path_BP_YAY579,choice=121160)
present(plan1211_6_YAY579_original)
plan1211_7_YAY579_original=run(data = all_data,path = path_BP_YAY579,choice=121170)
present(plan1211_7_YAY579_original)
plan1211_8_YAY579_original=run(data = all_data,path = path_BP_YAY579,choice=121180)
present(plan1211_8_YAY579_original)
plan1211_9_YAY579_original=run(data = all_data,path = path_BP_YAY579,choice=121190)
present(plan1211_9_YAY579_original)
## 单纯的R9======================================================
# 使用轨迹数据
plan1211_1_YAY579_R9=run(data = all_data,path = path_BP_YAY579,choice=12111,R=9)
present(plan1211_1_YAY579_R9)
plan1211_2_YAY579_R9=run(data = all_data,path = path_BP_YAY579,choice=12112,R=9)
present(plan1211_2_YAY579_R9)
plan1211_3_YAY579_R9=run(data = all_data,path = path_BP_YAY579,choice=12113,R=9)
present(plan1211_3_YAY579_R9)
plan1211_4_YAY579_R9=run(data = all_data,path = path_BP_YAY579,choice=12114,R=9)
present(plan1211_4_YAY579_R9)
# 原始数据
plan1211_1_YAY579_R9_original=run(data = all_data,path = path_BP_YAY579,choice=121110,R=9)
present(plan1211_1_YAY579_R9_original)
plan1211_2_YAY579_R9_original=run(data = all_data,path = path_BP_YAY579,choice=121120,R=9)
present(plan1211_2_YAY579_R9_original)
plan1211_3_YAY579_R9_original=run(data = all_data,path = path_BP_YAY579,choice=121130,R=9)
present(plan1211_3_YAY579_R9_original)
plan1211_4_YAY579_R9_original=run(data = all_data,path = path_BP_YAY579,choice=121140,R=9)
present(plan1211_4_YAY579_R9_original)

## 单纯的R7======================================================
# 使用轨迹数据
plan1211_1_YAY579_R7=run(data = all_data,path = path_BP_YAY579,choice=12111,R=7)
present(plan1211_1_YAY579_R7)
plan1211_2_YAY579_R7=run(data = all_data,path = path_BP_YAY579,choice=12112,R=7)
present(plan1211_2_YAY579_R7)
# 原始数据
plan1211_1_YAY579_R7_original=run(data = all_data,path = path_BP_YAY579,choice=121110,R=7)
present(plan1211_1_YAY579_R7_original)
plan1211_2_YAY579_R7_original=run(data = all_data,path = path_BP_YAY579,choice=121120,R=7)
present(plan1211_2_YAY579_R7_original)

## 单纯的R5======================================================
# 使用轨迹数据
plan1211_1_YAY579_R5=run(data = all_data,path = path_BP_YAY579,choice=12111,R=5)
present(plan1211_1_YAY579_R5)
plan1211_2_YAY579_R5=run(data = all_data,path = path_BP_YAY579,choice=12112,R=5)
present(plan1211_2_YAY579_R5)
# 原始数据
plan1211_1_YAY579_R5_original=run(data = all_data,path = path_BP_YAY579,choice=121110,R=5)
present(plan1211_1_YAY579_R5_original)
plan1211_2_YAY579_R5_original=run(data = all_data,path = path_BP_YAY579,choice=121120,R=5)
present(plan1211_2_YAY579_R5_original)


