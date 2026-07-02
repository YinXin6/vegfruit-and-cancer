library(stringr)
library(data.table)
library(dplyr)
library(tidyr)
library(epiDisplay)
library(openxlsx)
data <- fread('xxx')

data <- data %>% slice(
  -which(is.na(诊断日期)&!is.na(死亡日期)&grepl("C",死因代码))
)

cancer_map <- list(
  肺癌=c("C34","D02.2"),结直肠癌=c("C18","C19","C20","C21","D0.10","D01.1","D01.2","D01.3"),
  胃癌=c("C16","D00.2"),肝癌=c("C22","D01.5"),食管癌=c("C15","D00.1"),
  前列腺癌=c("C61","D07.5"),乳腺癌=c("C50","D05"),甲状腺癌=c("C73","D09.3"),
  子宫颈癌=c("C53","D06"),胰腺癌=c("C25","D01.7"),膀胱癌=c("C67","D09.0"),
  肾癌及其他泌尿系统=c("C64","C65","C66","C68","D09.1")
)
data <- data%>%
  mutate(肿瘤=ifelse(!is.na(ICD10)&ICD10!="",1,0))

for (cancer in names(cancer_map)){
  data[[cancer]] <- ifelse(
    sapply(data$ICD10,function(x) any (startsWith(x,cancer_map[[cancer]]))),1,0
  )
}

all_cancer_codes <- unlist(cancer_map)
data <- data%>%mutate(
  其他肿瘤=ifelse(
    !is.na(ICD10)&ICD10!=""&!(sapply(ICD10,function(x) 
      any(startsWith(x,all_cancer_codes)))),1,0
  )
)
data <- data %>% mutate(
  饮食相关肿瘤=case_when(结直肠癌==1|食管癌==1|胃癌==1|胰腺癌==1|膀胱癌==1|肾癌及其他泌尿系统==1~1,T~0),
  其他肿瘤1=case_when(肝癌==1|前列腺癌==1|子宫颈癌==1|其他肿瘤==1~1,T~0),
  其他肿瘤2=case_when(肝癌==1|前列腺癌==1|子宫颈癌==1|甲状腺癌==1|乳腺癌==1|其他肿瘤==1~1,T~0)
)
data <- data %>% mutate(across(93:104,~replace(.,is.na(.),0)))

data <- data %>% mutate(
  慢病=case_when(高血压==1|糖尿病==1|冠心病==1|高脂血症==1|脑卒中==1~1,T~0)
) 


cancernames <- c(names(cancer_map),"其他肿瘤")
tumortable <- lapply(as.data.frame(data)[cancernames],table)
tumortable

data <- data %>% mutate(
  调查日期=as.Date(调查日期,format="%Y/%m/%d"),
  诊断日期=as.Date(诊断日期,format="%Y/%m/%d"),
  死亡日期=as.Date(死亡日期,format="%Y/%m/%d")) %>% 
  mutate(#定义随访终点
    enddate=case_when(
      !is.na(诊断日期)~诊断日期,
      is.na(诊断日期)&!is.na(死亡日期)~死亡日期,
      TRUE~as.Date('2024/6/30')
    ),
    followupyears=as.numeric(enddate-调查日期)/365.25
  )

data <- data %>% mutate(
  果蔬摄入=新鲜蔬菜+水果,
  果蔬摄入4=ntile(果蔬摄入,4),
  蔬菜摄入4=ntile(新鲜蔬菜,4),
  水果摄入4=ntile(水果,4),
  红肉摄入=猪肉+其他畜肉,
  鱼摄入=淡水鱼+海水鱼
)


data <- data %>% mutate(
  smoking=case_when(is.na(smoking)~"1不吸烟",T~smoking)
)
data$被动吸烟[is.na(data$被动吸烟)] <- 0

library(purrr)
gourp_vars <- c("果蔬摄入4","水果摄入4","蔬菜摄入4",
                "VAA","VB1A","VB2A","VB3A","VCA","VEA","铁A","锌A","硒A","D20-5A","D22-6A","D18-3A","D20-4A","D18-2A")

IR_list <- map(gourp_vars,function(g){
  data %>% group_by(across(all_of(g))) %>% 
     filter(Sex==2) %>% 
      summarise(
         across(
              c("肿瘤","肺癌","胃癌","结直肠癌","乳腺癌","甲状腺癌","饮食相关肿瘤","其他肿瘤1","其他肿瘤2"),
              list(
                 cases=~sum(.x,na.rm=T),
                  personyears=~sum(followupyears,na.rm = T),
                  IR=~sum(.x,na.rm=T)/sum(followupyears,na.rm=T)*100000),
              .names="{col}_{fn}"),
           .groups="drop"
         )})

IR <- bind_rows(IR_list)

library(survival)
for (i in names(data)[c(3:16,109,113:115)])
  data[[i]] <- factor(data[[i]])
cancer_cols <- c("肿瘤","肺癌","胃癌","饮食相关肿瘤","结直肠癌","乳腺癌","甲状腺癌","其他肿瘤1","其他肿瘤2")
cox_results <- list()
for(cancer in cancer_cols){
  surv_obj <- Surv(time = data$followupyears,event = data[[cancer]])
  cox_model <- coxph(surv_obj~Age+Sex+education+smoking+被动吸烟+alcohol+tea+活动水平
                     +肥胖+腰围+高血压+糖尿病+冠心病+脑卒中+高脂血症
                     +红肉摄入+鱼摄入+果蔬摄入4,data=data)
  cox_summary <- broom::tidy(cox_model) %>% 
    mutate(cancer=cancer)
  cox_results[[cancer]] <- cox_summary
}

cox_all <- bind_rows(cox_results)
cox_all <- cox_all %>% 
  mutate(
    HR=round(exp(estimate),2),
    lower=round(exp(estimate-1.96*std.error),2),
    upper=round(exp(estimate+1.96*std.error),2),
    HR_CI=paste0(HR,"(",lower,"-",upper,")")
  ) %>% 
  dplyr::select(cancer,term,HR,lower,upper,HR_CI,p.value)
write.xlsx(cox_all,'C:/group_jk226/mhcdc/yinx/结果/COX果蔬摄入.xlsx')


for(cancer in cancer_cols){
  surv_obj <- Surv(time = data$followupyears,event = data[[cancer]])
  cox_model <- coxph(surv_obj~Age+education+smoking+alcohol+tea+活动水平
                     +肥胖+腰围+高血压+糖尿病+冠心病+脑卒中+高脂血症
                     +红肉摄入+鱼摄入+果蔬摄入4,data=data,subset=(Sex==1))
  cox_summary <- broom::tidy(cox_model) %>% 
    mutate(cancer=cancer)
  cox_results[[cancer]] <- cox_summary
}

cox_all <- bind_rows(cox_results)
cox_all <- cox_all %>% 
  mutate(
    HR=round(exp(estimate),2),
    lower=round(exp(estimate-1.96*std.error),2),
    upper=round(exp(estimate+1.96*std.error),2),
    HR_CI=paste0(HR,"(",lower,"-",upper,")")
  ) %>% 
  dplyr::select(cancer,term,HR,lower,upper,HR_CI,p.value)


library(epiDisplay)
for (i in names(data)[c(3:16,109,113:115)])
  data[[i]] <- factor(data[[i]])
for (i in names(data)[c(92:108,118:141)])
  data[[i]] <- factor(data[[i]])
table1 <- tableStack(vars=c(2:19,30:37,43:45,74:77,79:86,92:109,112:141),##顺序可能乱了
                     by=果蔬摄入4,total.column=T,dataFrame=as.data.frame(data))

##RCS
library(survival)
library(rms)
library(ggplot2)

dd <- datadist(data)
options(datadist='dd')
fit <- cph(Surv(followupyears,饮食相关肿瘤)~rcs(水果,3)+Age+Sex+education+
             smoking+alcohol+活动水平+肥胖+红肉摄入+鱼摄入+
             高血压+糖尿病+冠心病+脑卒中+高脂血症+新鲜蔬菜,
           data=data,x=T,y=T,surv=T)

AIC(fit)
an <- anova(fit)
an
HR <- Predict(fit,水果,fun=exp,ref.zero = T)
P <- ggplot()+geom_line(data=HR,aes(水果,yhat),linetype="solid",linewidth=1,alpha=0.7,colour="darkblue")+
  geom_ribbon(data=HR,aes(水果,ymin=lower,ymax=upper),alpha=0.1,fill="darkblue")
P
P <- P+theme_classic()+geom_hline(yintercept = 1,linetype=2,size=0.5)+
  geom_vline(xintercept = 98,linetype=2,size=0.5)+geom_vline(xintercept = 316,linetype=2,size=0.5)+labs(x="",y="HR(95%CI)")+
  scale_x_continuous(expand=expansion(mult=c(0,0.02)))




