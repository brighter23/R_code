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
c=c('data','all_data','path_BP_YAY579','path_BP_YAY668810')
a=ls()
rm(list=(setdiff(a,c)))
gc()
library(stringr)
# 当有新的formula时需要修改true_path、new_quantize、取数据的手数
# 2、训练==================================================
# 当有新的formula时需要修改true_path、new_quantize、取数据的手数(取7手计算final result的都一样，取9手的与9手的一样)

# 2.1 定义计算每9个真实数据下的真实路径final_comb:
{
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
  true_path579_1=function(dataset){
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
    
  }  # 注：这里输入的时已经逆序处理的数据了
  
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
  true_path668810_2=function(dataset){
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
    t310=table(dataset[c(1,4:10)])
    result310=paste(t310[1],t310[2],sep = ':')
    t39=table(dataset[c(1,4:9)])
    result39=paste(t39[1],t39[2],sep = ':')
    t38=table(dataset[c(1,4:8)])
    result38=paste(t38[1],t38[2],sep = ':')
    t37=table(dataset[c(1,4:7)])
    result37=paste(t37[1],t37[2],sep = ':')
    t36=table(dataset[c(1,4:6)])
    result36=paste(t36[1],t36[2],sep = ':')
    t35=table(dataset[c(1,4:5)])
    result35=paste(t35[1],t35[2],sep = ':')
    t34=table(dataset[c(1,4:4)])
    result34=paste(t34[1],t34[2],sep = ':')
    comb2=paste0(result34,'-',result35,'-',result36,'-',result37,'-',result38,'-',result39,'-',result310)
    #66R：
    t510=table(dataset[c(1,6:10)])
    result510=paste(t510[1],t510[2],sep = ':')
    t59=table(dataset[c(1,6:9)])
    result59=paste(t59[1],t59[2],sep = ':')
    t58=table(dataset[c(1,6:8)])
    result58=paste(t58[1],t58[2],sep = ':')
    t57=table(dataset[c(1,6:7)])
    result57=paste(t57[1],t57[2],sep = ':')
    t56=table(dataset[c(1,6:6)])
    result56=paste(t56[1],t56[2],sep = ':')
    comb3=paste0(result56,'-',result57,'-',result58,'-',result59,'-',result510)
    return(matrix(c(comb1,comb2,comb3),ncol = 3))
    
  } # 顺序，66-88-10R的“断开拼接”结果：输入9 +1 2 3 4 5 6 7 8 +10
  true_path668810_1=function(dataset){
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
    t310=table(dataset[c(1:7,10)])
    result310=paste(t310[1],t310[2],sep = ':')
    t39=table(dataset[1:7])
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
    comb2=paste0(result34,'-',result35,'-',result36,'-',result37,'-',result38,'-',result39,'-',result310)
    #66R：
    t510=table(dataset[c(1:5,6)])
    result510=paste(t510[1],t510[2],sep = ':')
    t59=table(dataset[1:5])
    result59=paste(t59[1],t59[2],sep = ':')
    t58=table(dataset[1:4])
    result58=paste(t58[1],t58[2],sep = ':')
    t57=table(dataset[1:3])
    result57=paste(t57[1],t57[2],sep = ':')
    t56=table(dataset[1:2])
    result56=paste(t56[1],t56[2],sep = ':')
    comb3=paste0(result56,'-',result57,'-',result58,'-',result59,'-',result510)
    return(matrix(c(comb1,comb2,comb3),ncol = 3))
    
  } # 逆序，66-88-10R的结果
  
  # 1266的逆序不同了，本质是和顺序的计算方式一样，即数据是逆序的输入
  true_path7911_0=function(dataset){
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
  true_path7911_1=function(dataset){
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
  
  true_path91113_0=function(dataset){
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
  true_path91113_1=function(dataset){
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
}

# 2.2 定义n个真实数据下根据真实路径下的量化值-1或1:
# order有2种形式T,F，代表不同的取数据计算真实路径的方式，对应true_path**_0、true_path**_1
{
  # 在顺序下是不分断开或连续的，因为都是连续的取数据true_path**_0，且都是BP依据set[1],set[3],set[5]
  # 逆序：分为断开取数据和不断开
    # 逆+断开：true_path**_0,BP依据set[1],set[3],set[5]
    # 逆+不断：true_path**_1，判断BP的依据都是set[1]
  # 总结：使用true_path**_0的BP依据set[1],set[3],set[5]，true_path**_1都是set[1]
  new_quantize579=function(set,order,connect,path=path_BP_YAY579){
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
  new_quantize668810=function(set,order,connect,separate=FALSE,path=path_BP_YAY668810){
    if(order){
      # 顺序10R：123456789+10，88R：3456789+10，66R：56789+10
        # 都是使用true_path**_0，BP依据set[1],set[3],set[5]
      if(separate)real_comb=true_path668810_2(set)else real_comb=true_path668810_0(set)
      r10=matrix(0,ncol = nrow(path),nrow = 1)
      for (j in 1:nrow(path)) {
        sparate_condition=ifelse(separate,set[1],set[1])
        r10[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,1],path[j,'B_R10']),1,-1),ifelse(str_detect(real_comb[1,1],path[j,'P_R10']),1,-1))
      }
      r8=matrix(0,ncol = nrow(path),nrow = 1)
      for (j in 1:nrow(path)) {
        sparate_condition=ifelse(separate,set[1],set[3])
        r8[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,2],path[j,'B_R88']),1,-1),ifelse(str_detect(real_comb[1,2],path[j,'P_R88']),1,-1))
      }
      r6=matrix(0,ncol = nrow(path),nrow = 1)
      for (j in 1:nrow(path)) {
        sparate_condition=ifelse(separate,set[1],set[5])
        r6[1,j]=ifelse(sparate_condition=='B'|sparate_condition==1,ifelse(str_detect(real_comb[1,3],path[j,'B_R66']),1,-1),ifelse(str_detect(real_comb[1,3],path[j,'P_R66']),1,-1))
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
  # new_quantize668810(set=data[1:10,1],order=T,connect=T,separate=T)
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
}

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
  data_sort_1226_one=function(data,m,choice,connect){
    n=nrow(data)
    final_result=matrix(nrow = n,ncol = 8) # final_result
    row.names(final_result)=1:n
    pre=data.frame(matrix(nrow = n,ncol = 8),row.names = 1:n) # 奇数轨预测值
    for(i in 1:ncol(pre)){
      pre[,i]=factor(pre[,i],levels = c('B','P'))
    }    
    # 1、先计算使用原始数据的9-16手
    # 奇数手：逆,path_BP_YAY579
    for (i in seq(9,15,2)) {
      # 计算final result
      final_result[i,]=new_quantize579(set = data[c(seq(i-1,i-8,-1),i),m],order =F ,connect = connect)
      # 计算预测值
      pre[i,]=ifelse(final_result[i,]>0,ifelse(data[i,m]=='B','B','P'),ifelse(data[i,m]=='B','P','B'))
    }
    # 偶数手：顺
    for (i in seq(10,16,2)) {
      final_tempt=new_quantize579(set = data[c(seq(i-8,i-1,1),i),m],order =T ,connect = connect)
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
                                     ,function(x){new_quantize579(set=x,order =F ,connect = connect)}))
        # 计算预测值
        pre[i,]=ifelse(final_result[i,]>0,ifelse(data[i,m]=='B','B','P'),ifelse(data[i,m]=='B','P','B'))
      }
      # 偶数手（第2手，顺序:需要改seq索引和inverse=T）根据第1手的结果
      else{
        # 先计算8个formula下对应8条路径的的结果结果矩阵，1列表示1条路径下8个formula的结果
        temptMatrix_final_result=sapply(rbind(pre[seq(i-8,i-1,1),],data.frame(matrix(rep(data[i,m],8),ncol=8),row.names = i))
                                        ,function(x){new_quantize579(set=x,order =T ,connect = connect)})
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
    colnames(final_result)=c(paste0('trace',1:8))
    colnames(pre)=c(paste0('trace',1:8))
    return(list(final_result,pre))
  }
  data_sort_107_one=function(data,m,choice,connect){
    n=nrow(data)
    final_result=matrix(nrow = n,ncol = 8) # final_result
    row.names(final_result)=1:n
    pre=data.frame(matrix(nrow = n,ncol = 8),row.names = 1:n) # 奇数轨预测值
    for(i in 1:ncol(pre)){
      pre[,i]=factor(pre[,i],levels = c('B','P'))
    }    
    # 1、先计算使用原始数据的9-16手
    # 奇数手：逆,path_BP_YAY579
    for (i in seq(9,15,2)) {
      # 计算final result
      final_result[i,]=new_quantize579(set = data[c(seq(i-1,i-8,-1),i),m],order =F ,connect = connect)
      # 计算预测值
      pre[i,]=ifelse(final_result[i,]>0,ifelse(data[i,m]=='B','B','P'),ifelse(data[i,m]=='B','P','B'))
    }
    # 偶数手：顺
    for (i in seq(10,16,2)) {
      final_tempt=new_quantize579(set = data[c(seq(i-9,i-2,1),i),m],order =T ,connect = connect)
      if(choice==1071){final_result[i,]=ifelse(final_result[i-1,]>0,-final_tempt,final_tempt)}
      else if(choice==1072){final_result[i,]=ifelse(final_result[i-1,]>0,final_tempt,-final_tempt)}
      else if(choice==1079){final_result[i,]=-final_tempt}
      pre[i,]=ifelse(final_result[i,]>0,ifelse(data[i,m]=='B','B','P'),ifelse(data[i,m]=='B','P','B'))
    }
    # 2、合再一起做，因为互相用到数据
    for(i in 17:n){
      # final result替换数据从重新计算
      # 奇数手（第1手逆序）固定使用original formula:
      if (i %in% seq(17,n,2)){
        final_result[i,]=diag(sapply(rbind(pre[seq(i-1,i-8,-1),],data.frame(matrix(rep(data[i,m],8),ncol=8),row.names = i))
                                     ,function(x){new_quantize579(set=x,order =F ,connect = connect)}))
        # 计算预测值
        pre[i,]=ifelse(final_result[i,]>0,ifelse(data[i,m]=='B','B','P'),ifelse(data[i,m]=='B','P','B'))
      }
      # 偶数手（第2手，顺序:需要改seq索引和inverse=T）根据第1手的结果
      else{
        # 先计算8个formula下对应8条路径的的结果结果矩阵，1列表示1条路径下8个formula的结果
        temptMatrix_final_result=sapply(rbind(pre[seq(i-9,i-2,1),],data.frame(matrix(rep(data[i,m],8),ncol=8),row.names = i))
                                        ,function(x){new_quantize579(set=x,order =T ,connect = connect)})
        # 不同的plan
        if(choice==1071){
          # 根据第1手结果against
          for(j in 1:8){
            # 计算每个final result:根据第一手的final result against,正时使用相反formula，负的时候使用相同formula
            if(final_result[i-1,j]>0)final_result[i,j]=temptMatrix_final_result[abs(j-9),j]
            else final_result[i,j]=temptMatrix_final_result[j,j]
          }
        }
        else if(choice==1072){
          # 根据第一手final result follow，plan12112
          # 正时使用相同formula，负的时候使用相反formula
          for(j in 1:8){
            # 计算每个final result:根据第一手的final result against,正时使用相反formula，负的时候使用相同formula
            if(final_result[i-1,j]<0)final_result[i,j]=temptMatrix_final_result[abs(j-9),j]
            else final_result[i,j]=temptMatrix_final_result[j,j]
          }
        }
        else if(choice==1079){
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
    colnames(final_result)=c(paste0('trace',1:8))
    colnames(pre)=c(paste0('trace',1:8))
    return(list(final_result,pre))
  }
  data_sort_1227_one=function(data,m,choice,connect){
    n=nrow(data)
    final_result=matrix(nrow = n,ncol = 8) # final_result
    row.names(final_result)=1:n
    pre=data.frame(matrix(nrow = n,ncol = 8),row.names = 1:n) # 奇数轨预测值
    for(i in 1:ncol(pre)){
      pre[,i]=factor(pre[,i],levels = c('B','P'))
    }    
    # 1、先计算使用原始数据的9-16手
    # 奇数手：逆,path_BP_YAY579
    for (i in seq(9,15,2)) {
      # 计算final result
      final_result[i,]=new_quantize579(set = data[c(seq(i-1,i-8,-1),i),m],order =F ,connect = connect)
      # 计算预测值
      pre[i,]=ifelse(final_result[i,]>0,ifelse(data[i,m]=='B','B','P'),ifelse(data[i,m]=='B','P','B'))
    }
    # 偶数手：顺,path_BP_YAY668810，且是10手
    for (i in seq(10,16,2)) {
      final_tempt=new_quantize668810(set = data[c(seq(i-9,i-1,1),i),m],order =T ,connect = connect)
      if(choice==12271){final_result[i,]=ifelse(final_result[i-1,]>0,-final_tempt,final_tempt)}
      else if(choice==12272){final_result[i,]=ifelse(final_result[i-1,]>0,final_tempt,-final_tempt)}
      else if(choice==12279){final_result[i,]=-final_tempt}
      pre[i,]=ifelse(final_result[i,]>0,ifelse(data[i,m]=='B','B','P'),ifelse(data[i,m]=='B','P','B'))
    }
    # 2、合再一起做，因为互相用到数据
    for(i in 17:n){
      # final result替换数据从重新计算
      # 奇数手（第1手逆序）固定使用original formula:
      if (i %in% seq(17,n,2)){
        final_result[i,]=diag(sapply(rbind(pre[seq(i-1,i-8,-1),],data.frame(matrix(rep(data[i,m],8),ncol=8),row.names = i))
                                     ,function(x){new_quantize579(set=x,order =F ,connect = connect)}))
        # 计算预测值
        pre[i,]=ifelse(final_result[i,]>0,ifelse(data[i,m]=='B','B','P'),ifelse(data[i,m]=='B','P','B'))
      }
      # 偶数手（第2手，顺序:需要改seq索引和inverse=T）根据第1手的结果
      else{
        # 先计算8个formula下对应8条路径的的结果结果矩阵，1列表示1条路径下8个formula的结果
        temptMatrix_final_result=sapply(rbind(pre[seq(i-9,i-1,1),],data.frame(matrix(rep(data[i,m],8),ncol=8),row.names = i))
                                        ,function(x){new_quantize668810(set=x,order =T ,connect = connect)})
        # 不同的plan
        if(choice==12271){
          # 根据第1手结果against
          for(j in 1:8){
            # 计算每个final result:根据第一手的final result against,正时使用相反formula，负的时候使用相同formula
            if(final_result[i-1,j]>0)final_result[i,j]=temptMatrix_final_result[abs(j-9),j]
            else final_result[i,j]=temptMatrix_final_result[j,j]
          }
        }
        else if(choice==12272){
          # 根据第一手final result follow，plan12112
          # 正时使用相同formula，负的时候使用相反formula
          for(j in 1:8){
            # 计算每个final result:根据第一手的final result against,正时使用相反formula，负的时候使用相同formula
            if(final_result[i-1,j]<0)final_result[i,j]=temptMatrix_final_result[abs(j-9),j]
            else final_result[i,j]=temptMatrix_final_result[j,j]
          }
        }
        else if(choice==12279){
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
    colnames(final_result)=c(paste0('trace',1:8))
    colnames(pre)=c(paste0('trace',1:8))
    return(list(final_result,pre))
  }
  data_sort_106_one=function(data,m,choice,connect){
    n=nrow(data)
    final_result=matrix(nrow = n,ncol = 8) # final_result
    row.names(final_result)=1:n
    pre=data.frame(matrix(nrow = n,ncol = 8),row.names = 1:n) # 奇数轨预测值
    for(i in 1:ncol(pre)){
      pre[,i]=factor(pre[,i],levels = c('B','P'))
    }    
    # 1、先计算使用原始数据的9-16手
    # 奇数手：逆,path_BP_YAY579
    for (i in seq(9,15,2)) {
      # 计算final result
      final_result[i,]=new_quantize579(set = data[c(seq(i-1,i-8,-1),i),m],order =F ,connect = connect)
      # 计算预测值
      pre[i,]=ifelse(final_result[i,]>0,ifelse(data[i,m]=='B','B','P'),ifelse(data[i,m]=='B','P','B'))
    }
    # 偶数手：顺,path_BP_YAY668810，且是10手
    for (i in seq(10,16,2)) {
      final_tempt=new_quantize668810(set = data[c(i-1,seq(i-9,i-2,1),i),m],order =T ,connect = connect,separate=T)
      if(choice==1061){final_result[i,]=ifelse(final_result[i-1,]>0,-final_tempt,final_tempt)}
      else if(choice==1062){final_result[i,]=ifelse(final_result[i-1,]>0,final_tempt,-final_tempt)}
      else if(choice==1069){final_result[i,]=-final_tempt}
      pre[i,]=ifelse(final_result[i,]>0,ifelse(data[i,m]=='B','B','P'),ifelse(data[i,m]=='B','P','B'))
    }
    # 2、合再一起做，因为互相用到数据
    for(i in 17:n){
      # final result替换数据从重新计算
      # 奇数手（第1手逆序）固定使用original formula:
      if (i %in% seq(17,n,2)){
        final_result[i,]=diag(sapply(rbind(pre[seq(i-1,i-8,-1),],data.frame(matrix(rep(data[i,m],8),ncol=8),row.names = i))
                                     ,function(x){new_quantize579(set=x,order =F ,connect = connect)}))
        # 计算预测值
        pre[i,]=ifelse(final_result[i,]>0,ifelse(data[i,m]=='B','B','P'),ifelse(data[i,m]=='B','P','B'))
      }
      # 偶数手（第2手，顺序:需要改seq索引和inverse=T）根据第1手的结果
      else{
        # 先计算8个formula下对应8条路径的的结果结果矩阵，1列表示1条路径下8个formula的结果
        temptMatrix_final_result=sapply(rbind(pre[c(i-1,seq(i-9,i-2,1)),],data.frame(matrix(rep(data[i,m],8),ncol=8),row.names = i))
                                        ,function(x){new_quantize668810(set=x,order =T ,connect = connect,separate=T)})
        # 不同的plan
        if(choice==1061){
          # 根据第1手结果against
          for(j in 1:8){
            # 计算每个final result:根据第一手的final result against,正时使用相反formula，负的时候使用相同formula
            if(final_result[i-1,j]>0)final_result[i,j]=temptMatrix_final_result[abs(j-9),j]
            else final_result[i,j]=temptMatrix_final_result[j,j]
          }
        }
        else if(choice==1062){
          # 根据第一手final result follow，plan12112
          # 正时使用相同formula，负的时候使用相反formula
          for(j in 1:8){
            # 计算每个final result:根据第一手的final result against,正时使用相反formula，负的时候使用相同formula
            if(final_result[i-1,j]<0)final_result[i,j]=temptMatrix_final_result[abs(j-9),j]
            else final_result[i,j]=temptMatrix_final_result[j,j]
          }
        }
        else if(choice==1069){
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
    colnames(final_result)=c(paste0('trace',1:8))
    colnames(pre)=c(paste0('trace',1:8))
    return(list(final_result,pre))
  }
  data_sort_1213_one=function(data,m,choice,connect){
    n=nrow(data)
    final_result=matrix(nrow = n,ncol = 8) # final_result
    row.names(final_result)=1:n
    pre=data.frame(matrix(nrow = n,ncol = 8),row.names = 1:n) # 奇数轨预测值
    for(i in 1:ncol(pre)){
      pre[,i]=factor(pre[,i],levels = c('B','P'))
    }    
    # 1、先计算使用原始数据的9-16手
    # 奇数手：顺,path_BP_YAY579
    for (i in seq(9,15,2)) {
      # 计算final result
      final_result[i,]=new_quantize579(set = data[c(seq(i-8,i-1,1),i),m],order = T,connect = connect)
      # 计算预测值
      pre[i,]=ifelse(final_result[i,]>0,ifelse(data[i,m]=='B','B','P'),ifelse(data[i,m]=='B','P','B'))
    }
    # 偶数手：逆,path_BP_YAY668810，且是10手
    for (i in seq(10,16,2)) {
      final_tempt=new_quantize668810(set = data[c(seq(i-1,i-9,-1),i),m],order = F,connect = connect)
      if(choice==12131){final_result[i,]=ifelse(final_result[i-1,]>0,-final_tempt,final_tempt)}
      else if(choice==12132){final_result[i,]=ifelse(final_result[i-1,]>0,final_tempt,-final_tempt)}
      else if(choice==12139){final_result[i,]=-final_tempt}
      pre[i,]=ifelse(final_result[i,]>0,ifelse(data[i,m]=='B','B','P'),ifelse(data[i,m]=='B','P','B'))
    }
    # 2、合再一起做，因为互相用到数据
    for(i in 17:n){
      # final result替换数据从重新计算
      # 奇数手（第1手逆序）固定使用original formula:
      if (i %in% seq(17,n,2)){
        final_result[i,]=diag(sapply(rbind(pre[seq(i-8,i-1,1),],data.frame(matrix(rep(data[i,m],8),ncol=8),row.names = i))
                                     ,function(x){new_quantize579(set=x,order = T,connect = connect)}))
        # 计算预测值
        pre[i,]=ifelse(final_result[i,]>0,ifelse(data[i,m]=='B','B','P'),ifelse(data[i,m]=='B','P','B'))
      }
      # 偶数手（第2手，顺序:需要改seq索引和inverse=T）根据第1手的结果
      else{
        # 先计算8个formula下对应8条路径的的结果结果矩阵，1列表示1条路径下8个formula的结果
        temptMatrix_final_result=sapply(rbind(pre[seq(i-1,i-9,-1),],data.frame(matrix(rep(data[i,m],8),ncol=8),row.names = i))
                                        ,function(x){new_quantize668810(set=x,order = F,connect = connect)})
        # 不同的plan
        if(choice==12131){
          # 根据第1手结果against
          for(j in 1:8){
            # 计算每个final result:根据第一手的final result against,正时使用相反formula，负的时候使用相同formula
            if(final_result[i-1,j]>0)final_result[i,j]=temptMatrix_final_result[abs(j-9),j]
            else final_result[i,j]=temptMatrix_final_result[j,j]
          }
        }
        else if(choice==12132){
          # 根据第一手final result follow，plan12112
          # 正时使用相同formula，负的时候使用相反formula
          for(j in 1:8){
            # 计算每个final result:根据第一手的final result against,正时使用相反formula，负的时候使用相同formula
            if(final_result[i-1,j]<0)final_result[i,j]=temptMatrix_final_result[abs(j-9),j]
            else final_result[i,j]=temptMatrix_final_result[j,j]
          }
        }
        else if(choice==12139){
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
    colnames(final_result)=c(paste0('trace',1:8))
    colnames(pre)=c(paste0('trace',1:8))
    return(list(final_result,pre))
  }
  
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
  data_sort_1226_origin_one=function(data,m,choice,connect){
    n=nrow(data)
    final_result=matrix(nrow = n,ncol = 8) # final_result
    row.names(final_result)=1:n
    # 1、先计算使用原始数据的9-16手
    # 奇数手：逆
    for (i in seq(9,n,2)) {
      # 计算final result
      final_result[i,]=new_quantize579(set = data[c(seq(i-1,i-8,-1),i),m],order =F ,connect = connect)
    }
    # 偶数手：顺
    for (i in seq(10,n,2)) {
      final_tempt=new_quantize579(set = data[c(seq(i-8,i-1,1),i),m],order =T ,connect = connect)
      if(choice==122610){final_result[i,]=ifelse(final_result[i-1,]>0,-final_tempt,final_tempt)}
      else if(choice==122620){final_result[i,]=ifelse(final_result[i-1,]>0,final_tempt,-final_tempt)}
      else if(choice==122690){final_result[i,]=-final_tempt}
    }
    # 3、去除无法计算final result的：
    final_result=final_result[-c(1:8),]
    colnames(final_result)=c(paste0('trace',1:8))
    return(list(final_result))
  }
  data_sort_107_origin_one=function(data,m,choice,connect){
    n=nrow(data)
    final_result=matrix(nrow = n,ncol = 8) # final_result
    row.names(final_result)=1:n
    # 1、先计算使用原始数据的9-16手
    # 奇数手：逆
    for (i in seq(9,n,2)) {
      # 计算final result
      final_result[i,]=new_quantize579(set = data[c(seq(i-1,i-8,-1),i),m],order =F ,connect = connect)
    }
    # 偶数手：顺
    for (i in seq(10,n,2)) {
      final_tempt=new_quantize579(set = data[c(seq(i-9,i-2,1),i),m],order =T ,connect = connect)
      if(choice==10710){final_result[i,]=ifelse(final_result[i-1,]>0,-final_tempt,final_tempt)}
      else if(choice==10720){final_result[i,]=ifelse(final_result[i-1,]>0,final_tempt,-final_tempt)}
      else if(choice==10790){final_result[i,]=-final_tempt}
    }
    # 3、去除无法计算final result的：
    final_result=final_result[-c(1:8),]
    colnames(final_result)=c(paste0('trace',1:8))
    return(list(final_result))
  }
  data_sort_1227_origin_one=function(data,m,choice,connect){
    n=nrow(data)
    final_result=matrix(nrow = n,ncol = 8) # final_result
    row.names(final_result)=1:n
    # 1、先计算使用原始数据的9-16手
    # 奇数手：逆,path_BP_YAY579
    for (i in seq(9,n,2)) {
      # 计算final result
      final_result[i,]=new_quantize579(set = data[c(seq(i-1,i-8,-1),i),m],order =F ,connect = connect)
    }
    # 偶数手：顺,path_BP_YAY668810，且是10手
    for (i in seq(10,n,2)) {
      final_tempt=new_quantize668810(set = data[c(seq(i-9,i-1,1),i),m],order =T ,connect = connect)
      if(choice==122710){final_result[i,]=ifelse(final_result[i-1,]>0,-final_tempt,final_tempt)}
      else if(choice==122720){final_result[i,]=ifelse(final_result[i-1,]>0,final_tempt,-final_tempt)}
      else if(choice==122790){final_result[i,]=-final_tempt}
    }
    
    # 3、去除无法计算final result的：
    final_result=final_result[-c(1:8),]
    colnames(final_result)=c(paste0('trace',1:8))
    return(list(final_result))
  }
  data_sort_1213_origin_one=function(data,m,choice,connect){
    n=nrow(data)
    final_result=matrix(nrow = n,ncol = 8) # final_result
    row.names(final_result)=1:n
    # 1、先计算使用原始数据的9-16手
    # 奇数手：顺,path_BP_YAY579
    for (i in seq(9,n,2)) {
      # 计算final result
      final_result[i,]=new_quantize579(set = data[c(seq(i-8,i-1,1),i),m],order = T,connect = connect)
    }
    # 偶数手：逆,path_BP_YAY668810，且是10手
    for (i in seq(10,n,2)) {
      final_tempt=new_quantize668810(set = data[c(seq(i-1,i-9,-1),i),m],order = F,connect = connect)
      if(choice==121310){final_result[i,]=ifelse(final_result[i-1,]>0,-final_tempt,final_tempt)}
      else if(choice==121320){final_result[i,]=ifelse(final_result[i-1,]>0,final_tempt,-final_tempt)}
      else if(choice==121390){final_result[i,]=-final_tempt}
    }
    # 2、合再一起做，因为互相用到数据
    # 3、去除无法计算final result的：
    final_result=final_result[-c(1:8),]
    colnames(final_result)=c(paste0('trace',1:8))
    return(list(final_result))
  }
  data_sort_106_origin_one=function(data,m,choice,connect){
    n=nrow(data)
    final_result=matrix(nrow = n,ncol = 8) # final_result
    row.names(final_result)=1:n
    # 1、先计算使用原始数据的9-16手
    # 奇数手：逆,path_BP_YAY579
    for (i in seq(9,n,2)) {
      # 计算final result
      final_result[i,]=new_quantize579(set = data[c(seq(i-1,i-8,-1),i),m],order =F ,connect = connect)
    }
    # 偶数手：顺,path_BP_YAY668810，且是10手
    for (i in seq(10,n,2)) {
      final_tempt=new_quantize668810(set = data[c(i-1,seq(i-9,i-2,1),i),m],order =T ,connect = connect,separate=T)
      if(choice==10610){final_result[i,]=ifelse(final_result[i-1,]>0,-final_tempt,final_tempt)}
      else if(choice==10620){final_result[i,]=ifelse(final_result[i-1,]>0,final_tempt,-final_tempt)}
      else if(choice==10690){final_result[i,]=-final_tempt}
    }
    # 3、去除无法计算final result的：
    final_result=final_result[-c(1:8),]
    colnames(final_result)=c(paste0('trace',1:8))
    return(list(final_result))
  }
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
run=function(data=data,choice,connect){
  library(stringr)
  n_path=8
  minus_result=matrix(nrow =length(data),ncol = n_path)
  plus=matrix(nrow =length(data),ncol = n_path)
  minus=matrix(nrow =length(data),ncol = n_path)
  new_indexE1=matrix(nrow =length(data),ncol = n_path)
  new_indexE2=matrix(nrow =length(data),ncol = n_path)
  for (m in 1:length(data)) {
    if(m%%50==0)print(m)
    if(choice %in% c(12271,12272,12279)){
      temp=data_sort_1227_one(data,m,choice,connect = connect)
    }
    else if(choice %in% c(122710,122720,122790)){
      temp=data_sort_1227_origin_one(data,m,choice,connect = connect)
    }
    else if(choice %in% c(12131,12132,12139)){
      temp=data_sort_1213_one(data,m,choice,connect = connect)
    }
    else if(choice %in% c(121310,121320,121390)){
      temp=data_sort_1213_origin_one(data,m,choice,connect = connect)
    }
    else if(choice %in% c(12261,12262,12269)){
      temp=data_sort_1226_one(data,m,choice,connect = connect)
    }
    else if(choice %in% c(122610,122620,122690)){
      temp=data_sort_1226_origin_one(data,m,choice,connect = connect)
    }
    else if(choice %in% c(1061,1062,1069)){
      temp=data_sort_106_one(data,m,choice,connect = connect)
    }
    else if(choice %in% c(10610,10620,10690)){
      temp=data_sort_106_origin_one(data,m,choice,connect = connect)
    }
    else if(choice %in% c(1071,1072,1079)){
      temp=data_sort_107_one(data,m,choice,connect = connect)
    }
    else if(choice %in% c(10710,10720,10790)){
      temp=data_sort_107_origin_one(data,m,choice,connect = connect)
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

# write.csv(as.data.frame((patternE1*symbol)[,c(1,8)]),'C:\\Users\\Think\\Desktop\\1.csv',row.names = F)
# plan1227===========================================================
plan1227_1_disconnect=run(data = all_data,choice=12271,connect=F)
present(plan1227_1_disconnect)
plan1227_2_disconnect=run(data = all_data,choice=12272,connect=F)
present(plan1227_2_disconnect)

plan1227_1_original_disconnect=run(data = all_data,choice=122710,connect=F)
present(plan1227_1_original_disconnect)
plan1227_2_original_disconnect=run(data = all_data,choice=122720,connect=F)
present(plan1227_2_original_disconnect)


plan1227_1_connect=run(data = all_data,choice=12271,connect=T)
present(plan1227_1_connect)
plan1227_2_connect=run(data = all_data,choice=12272,connect=T)
present(plan1227_2_connect)

plan1227_1_original_connect=run(data = all_data,choice=122710,connect=T)
present(plan1227_1_original_connect)
plan1227_2_original_connect=run(data = all_data,choice=122720,connect=T)
present(plan1227_2_original_connect)
# plan1213===========================================================
plan1213_1_disconnect=run(data = all_data,choice=12131,connect=F)
present(plan1213_1_disconnect)
plan1213_2_disconnect=run(data = all_data,choice=12132,connect=F)
present(plan1213_2_disconnect)

plan1213_1_original_disconnect=run(data = all_data,choice=121310,connect=F)
present(plan1213_1_original_disconnect)
plan1213_2_original_disconnect=run(data = all_data,choice=121320,connect=F)
present(plan1213_2_original_disconnect)


plan1213_1_connect=run(data = all_data,choice=12131,connect=T)
present(plan1213_1_connect)
plan1213_2_connect=run(data = all_data,choice=12132,connect=T)
present(plan1213_2_connect)

plan1213_1_original_connect=run(data = all_data,choice=121310,connect=T)
present(plan1213_1_original_connect)
plan1213_2_original_connect=run(data = all_data,choice=121320,connect=T)
present(plan1213_2_original_connect)
# plan1226===========================================================
plan1226_1_disconnect=run(data = all_data,choice=12261,connect=F)
present(plan1226_1_disconnect)
plan1226_2_disconnect=run(data = all_data,choice=12262,connect=F)
present(plan1226_2_disconnect)

plan1226_1_original_disconnect=run(data = all_data,choice=122610,connect=F)
present(plan1226_1_original_disconnect)
plan1226_2_original_disconnect=run(data = all_data,choice=122620,connect=F)
present(plan1226_2_original_disconnect)

# 不断开取数据
plan1226_1_connect=run(data = all_data,choice=12261,connect=T)
present(plan1226_1_connect)
plan1226_2_connect=run(data = all_data,choice=12262,connect=T)
present(plan1226_2_connect)

plan1226_1_original_connect=run(data = all_data,choice=122610,connect=T)
present(plan1226_1_original_connect)
plan1226_2_original_connect=run(data = all_data,choice=122620,connect=T)
present(plan1226_2_original_connect)
# plan106===========================================================
plan106_1_disconnect=run(data = all_data,choice=1061,connect=F)
present(plan106_1_disconnect)
plan106_2_disconnect=run(data = all_data,choice=1062,connect=F)
present(plan106_2_disconnect)

plan106_1_original_disconnect=run(data = all_data,choice=10610,connect=F)
present(plan106_1_original_disconnect)
plan106_2_original_disconnect=run(data = all_data,choice=10620,connect=F)
present(plan106_2_original_disconnect)

plan106_1_connect=run(data = all_data,choice=1061,connect=T)
present(plan106_1_connect)
plan106_2_connect=run(data = all_data,choice=1062,connect=T)
present(plan106_2_connect)

plan106_1_original_connect=run(data = all_data,choice=10610,connect=T)
present(plan106_1_original_connect)
plan106_2_original_connect=run(data = all_data,choice=10620,connect=T)
present(plan106_2_original_connect)


# plan107===========================================================
plan106_1_disconnect=run(data = all_data,choice=1071,connect=F)
present(plan107_1_disconnect)
plan107_2_disconnect=run(data = all_data,choice=1072,connect=F)
present(plan107_2_disconnect)

plan107_1_original_disconnect=run(data = all_data,choice=10710,connect=F)
present(plan107_1_original_disconnect)
plan107_2_original_disconnect=run(data = all_data,choice=10720,connect=F)
present(plan107_2_original_disconnect)

plan107_1_connect=run(data = all_data,choice=1071,connect=T)
present(plan107_1_connect)
plan107_2_connect=run(data = all_data,choice=1072,connect=T)
present(plan107_2_connect)

plan107_1_original_connect=run(data = all_data,choice=10710,connect=T)
present(plan107_1_original_connect)
plan107_2_original_connect=run(data = all_data,choice=10720,connect=T)
present(plan107_2_original_connect)

