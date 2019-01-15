# 在桌面创建“sas_data”的文件夹，把sas文件放里面，就会自动生成CSV文件在这个文件夹
# 记得修改你的桌面路径： "C:/Users/Think/Desktop/sas_data"
library(haven)
# 若上句报错运行：
# install.packages('haven')
filenames <- dir("C:/Users/Think/Desktop/sas_data", full.names = T)
for(files in filenames){
  dataset <- read_sas(files)
  write.csv(x = dataset,file = paste0(paste0("C:/Users/Think/Desktop/sas_data/",
                                      str_split(files,pattern = '/')[[1]][6]),'.csv'))
}
