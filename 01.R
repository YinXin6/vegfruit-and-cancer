library(stringr)
library(data.table)
library(dplyr)
library(tidyr)

jd1 <- fread('xxxxx')
jd2 <- fread('xxx')
jddeath <- fread('xxxxx')

jd1clean <- jd1%>%
  mutate(诊断日期=as.Date(诊断日期,format="%Y/%m/%d"))%>%
  group_by(PIX)%>%
  arrange(诊断日期,.by_group=T)
  slice(1)%>%
  ungroup()
jd2clean <- jd2%>%
  mutate(诊断日期=as.Date(诊断日期,format="%Y/%m/%d"))%>%
  group_by(PIX)%>%
  arrange(诊断日期,.by_group=T)%>%
  slice(1)%>%
  ungroup()
jd1clean <-jd1clean%>%mutate(across(everything(),as.character))
jd2clean <-jd2clean%>%mutate(across(everything(),as.character))

jd <- bind_rows(jd1clean,jd2clean)%>%
  mutate(诊断日期=as.Date(诊断日期,format="%Y-%m-%d"))%>%
  group_by(PIX)%>%
  arrange(诊断日期,.by_group=T)%>%
  slice(1)%>%
  ungroup()


mh <- fread('xxx')
mhdeath <- fread('xxx')


ICD_MAP <- data.frame(
  诊断名称=c("胃","小肠","十二指肠","空肠","回肠","肝","直肠","甲状腺",
         "宫颈","结肠","前列腺","乳","肾","胰","肺","子宫","卵巢"),
  ICD10=c("C16","C17","C17","C17","C17","C22","C20","C73",
          "C53","C18","C61","C50","C64","C25","C34","C55","C56"),
  stringsAsFactors=F
)

mh$ICD10[mh$ICD10==""] <- NA
mh <- mh%>%
  mutate(ICD10=if_else(
    is.na(ICD10),
    sapply(诊断名称,function(x){
      match <- ICD_MAP$ICD10[grepl(paste(ICD_MAP$诊断名称,collapse = "|"),x)]
      if(length(match)>0)match[1] else NA}),
    ICD10
  ))

mh$ICD10 <- mh$ICD10 %>% replace_na("C99")

mh <- mh%>%
  mutate(诊断日期=as.Date(诊断日期,format="%Y/%m/%d"))%>%
  group_by(PIX)%>%
  arrange(诊断日期,.by_group=T)%>%
  slice(1)%>%
  ungroup()


sj <- fread('xxx')
sjdeath <- fread('xxx')


sj <- sj%>%
  mutate(诊断日期=as.Date(诊断日期,format="%Y/%m/%d"))%>%
  group_by(PIX)%>%
  arrange(诊断日期,.by_group=T)%>%
  slice(1)%>%
  ungroup()


xh <- fread('xxx')
xhdeath <- fread('xxx')
xh <- xh%>%
  mutate(诊断日期=as.Date(诊断日期,format="%Y/%m/%d"))%>%
  group_by(PIX)%>%
  arrange(诊断日期,.by_group=T)%>%
  slice(1)%>%
  ungroup()


sum(duplicated(sjdeath$PIX))
sjdeath <- sjdeath%>%group_by(PIX)%>%
  arrange(desc(is.na(死因代码)&死因代码!=""))%>%
  slice(1)%>%
  ungroup()

library(purrr)
death <- bind_rows(jddeath,mhdeath,xhdeath,sjdeath)%>%
  group_by(PIX)%>%
  summarise(
    死亡日期=coalesce(死亡日期[1],NA),
    死因代码=coalesce(死因代码[1],NA),
    .groups = "drop")
write.csv(death,'C:/group_jk226/mhcdc/yinx/death.csv',row.names = F)
xh <-xh%>%mutate(across(everything(),as.character))
sj <-sj%>%mutate(across(everything(),as.character))
mh <-mh%>%mutate(across(everything(),as.character))
jd <-jd%>%mutate(across(everything(),as.character))
zhongliu <- bind_rows(jd,mh[,2:4],xh,sj)

riqi <- fread('xxx')

zhongliu2 <- read.xlsx('xxx')
zhongliu2 <- zhongliu2[,c(2,7,1)]
zhongliu <- bind_rows(zhongliu,zhongliu2) %>% mutate(
  诊断日期=as.Date(诊断日期,format="%Y-%m-%d"))%>%
    group_by(PIX)%>%
    slice(1)%>%
    ungroup()
zhongliu <- zhongliu %>% filter(!is.na(PIX)&PIX!="")
write.csv(zhongliu,'C:/group_jk226/mhcdc/yinx/zhongliu.csv',row.names = F)

base <- fread('xxx')
base <- base%>%left_join(death,by="PIX")%>%
  left_join(zhongliu,by="PIX")%>%left_join(riqi[,2:3],by="PIX")
base%>%filter(!is.na(ICD10)&ICD10!="")%>%summarise(n=n())


