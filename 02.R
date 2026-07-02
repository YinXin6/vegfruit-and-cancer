library(openxlsx)
library(stringr)
library(data.table)
library(dplyr)
library(tidyr)
data <- fread('xxx')

data <- data%>%filter(
  (Sex==1&(能量>=800&能量<=4200))|
    (Sex==2&(能量>=500&能量<=3500))
)

data <- data%>%filter(
  (新鲜蔬菜<=1000&水果<=800)
)


library(tidyverse)
missing_report<-data %>%
  summarise(across(everything(),~sum(is.na(.)))) %>%
  pivot_longer(everything(),names_to = "变量",values_to = "缺失数") 
n1 <- nrow(data)
data <- data %>% rename(height=hight)
data <- data%>%filter(!is.na(height)&!is.na(weight)&!is.na(yaowei)&!is.na(调查日期))
n2 <- nrow(data)
print(c(n1,n1-n2,n2))

data <- data %>% mutate(
  height=ifelse(height<=120|height>=200,
                mean(height[height>100&height<200],
                     na.rm=T),height)
)
data <- data %>% mutate(
  weight=ifelse(weight<=30|weight>=160,
                mean(weight[weight>30&weight<160],
                     na.rm=T),weight)
)
data$BMI <- data$weight/((data$height/100)^2)
data <- data %>%
  mutate(
    肥胖 = case_when(
      BMI < 18.5 ~ "1消瘦",
      BMI >= 18.5 & BMI < 24 ~ "2正常",
      BMI >=24 & BMI < 28 ~ "3超重",
      BMI >=28 ~ "4肥胖"
    ) )
data <- data %>% mutate(
  yaowei=ifelse(yaowei<=50|yaowei>=120,
                mean(yaowei[yaowei>50&yaowei<120],
                     na.rm=T),yaowei)
)
data <- data %>% mutate(
  腰围=case_when(Sex==1&yaowei>=90~"3肥胖",
               Sex==1&yaowei>=85&yaowei<90~"2肥胖前期",
               Sex==2&yaowei>=85~"3肥胖",
               Sex==2&yaowei>=80&yaowei<85~"2肥胖前期",
               TRUE~"1正常")
)
data <- data %>% mutate(
  education=case_when(
    education %in% c(1,2,3)~"1小学及以下",
    education %in% c(4,5,6,7)~"2初高中",
    education %in% c(8,9)~"3大专及以上"
  )
)

data <- data %>% mutate(
  smoking=case_when(
    是否吸烟==1&戒烟==1~"2已戒烟",
    是否吸烟==1&戒烟!=1~"3现在吸烟",
    是否吸烟==2~"1不吸烟"
  )
)
data <- data %>% mutate(
  alcohol=case_when(
    喝酒==2~"1不喝酒",
    喝酒==1&(!is.na(几岁停止喝酒)&几岁停止喝酒!=0)~"2已戒酒",
    喝酒==1&(is.na(几岁停止喝酒)|几岁停止喝酒==0)~"3现在喝酒"
  )
)
data <- data %>% mutate(
  tea=case_when(
    喝茶==2~"1不喝茶",
    喝茶==1&现在喝茶==2~"2过去喝茶",
    喝茶==1&现在喝茶==1~"3现在喝茶"
  )
)

##体育锻炼
#时间格式转变
data$显著体锻时间 <- as.numeric(as.difftime(data$显著体锻时间,format="%H:%M:%S"),units="mins")
data$轻度体锻时间 <- as.numeric(as.difftime(data$轻度体锻时间,format="%H:%M:%S"),units="mins")
data$每周自行车时间 <- as.numeric(as.difftime(data$每周自行车时间,format="%H:%M:%S"),units="mins")
data$步行时间 <- as.numeric(as.difftime(data$步行时间,format="%H:%M:%S"),units="mins")
data$做家务时间 <- as.numeric(as.difftime(data$做家务时间,format="%H:%M:%S"),units="mins")
data$坐着靠着 <- as.numeric(as.difftime(data$坐着靠着,format="%H:%M:%S"),units="mins")
##异常值修改
data <- data %>% mutate(
  显著体锻时间=case_when(
    is.na(显著体锻时间)|显著体锻时间<10~0,
    显著体锻时间>180~180,
    TRUE~显著体锻时间),
  轻度体锻时间=case_when(
    is.na(轻度体锻时间)|轻度体锻时间<10~0,
    轻度体锻时间>180~180,
    TRUE~轻度体锻时间),
  每周自行车时间=case_when(
    is.na(每周自行车时间)|每周自行车时间<10~0,
    每周自行车时间>180~180,
    TRUE~每周自行车时间),
  步行时间=case_when(
    is.na(步行时间)|步行时间<10~0,
    步行时间>180~180,
    TRUE~步行时间),
  做家务时间=case_when(
    is.na(做家务时间)|做家务时间<10~0,
    做家务时间>180~180,
    TRUE~做家务时间),
  坐着靠着=case_when(
    is.na(坐着靠着)|坐着靠着<10~0,
    步行时间>180~180,
    TRUE~坐着靠着)
)
data <- data %>% mutate(
  显著体锻天数=ifelse(显著体锻时间==0,0,显著体锻天数),
  轻度体锻天数=ifelse(轻度体锻时间==0,0,轻度体锻天数),
  每周自行车=ifelse(每周自行车时间==0,0,每周自行车),
  每周步行=ifelse(步行时间==0,0,每周步行),
  做家务=ifelse(做家务时间==0,0,做家务),
)
data <- data %>% mutate(
  中等强度时间 =轻度体锻天数*轻度体锻时间+每周自行车*每周自行车时间+做家务*做家务时间,
  中等强度时间=ifelse(中等强度时间>=1260,1260,中等强度时间)
)

data <- data %>% mutate(
  体育锻炼水平=8*显著体锻天数*显著体锻时间+4*中等强度时间+3.3*每周步行*步行时间) %>% 
  mutate(活动水平=case_when(
    显著体锻天数>=3&体育锻炼水平>=1500~'3高',
    (显著体锻天数+轻度体锻天数+每周自行车+每周步行+做家务)>=7&体育锻炼水平>=3000~'3高',
    (显著体锻天数+轻度体锻天数+每周自行车+每周步行+做家务)>=5&体育锻炼水平>=600~'2中',
    显著体锻时间>=20&显著体锻天数>=3~"2中",
    (轻度体锻时间+每周自行车时间+步行时间+做家务时间)>=30&
      (轻度体锻天数+每周自行车+每周步行+做家务)>=5~"2中",
    TRUE~'1低'
  ))
table(data$活动水平)

data <- data %>% mutate(
  被动吸烟=ifelse(工作被动吸烟==1|家中被动吸烟==1,1,0)
)


data <- data %>% filter(恶性肿瘤!=1)
data%>%filter(!is.na(ICD10)&ICD10!="")%>%summarise(n=n())
data <- data%>%
  mutate(诊断日期=as.Date(诊断日期,format="%Y-%m-%d"),
         调查日期=as.Date(调查日期,format="%Y/%m/%d"))%>%
  filter(is.na(诊断日期)|诊断日期==""|调查日期<诊断日期)
data%>%filter(!is.na(ICD10)&ICD10!="")%>%summarise(n=n())
write.csv(data,'C:/group_jk226/mhcdc/yx/data0928.csv')

data <- data%>%mutate(ICD10=ifelse(
  !is.na(诊断日期)&(is.na(ICD10)|ICD10==""),"C0",ICD10))
cols <- c("PIX","Age","Sex","education",
          "smoking","被动吸烟","alcohol","tea","活动水平","肥胖","腰围",
          "高血压","糖尿病","冠心病","脑卒中","高脂血症",
          names(data)[c(61:67,73:135)],
          "死亡日期","死因代码","ICD10","诊断日期","调查日期")
final <- data[,..cols]
write.csv(final,'xxx',row.names = F)
