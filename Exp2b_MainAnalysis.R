## ---------------------------
##
## Script name: Exp2b_MainAnalysis
##
## Purpose of script: Experiment 2b data cleaning and EDA and analysis
##
## Author: Chuanxiuyue (Carol) He
##
## Date Created: 2021-05-21
##
## Email: carol.hcxy@gmail.com
## 
## Content: 
##      1. Load Experiment 2b Data
##      2. Experiment 2b Data Cleaning
##      3. Accuracy Descriptive Statistics
##      4. Response Time Descriptive Statistics
##      5. d' Calculation
##      6. d' Descriptive Statistics
##      7. Bias Calculation
##      8. Bias Descriptive Statistics
##      9. Capacity Calculation
##      10. Capacity Descriptive Statistics
##      11. d' ANOVA
## ---------------------------


## ---------------------------

## set working directory for Mac and PC

setwd("~/CarolHe/")      # Carol's working directory (mac)
setwd("C:/Users/carolhe/")    # if using PC

## ---------------------------

## load up the packages
## use install.packages() to install new packages
library(tidyverse)
library(reshape2)
library(psych) # descriptive statistics
library(effectsize)


## ---------------------------

#####----------Load Experiment 2b-1 Data----------#####

## read csv files
csv_files_ls = list.files(path=getwd(), pattern = "*.csv")
csv_files_df <- lapply(csv_files_ls, 
                       function(x) 
                         { 
                         tmp <- try(read.csv(
                           file = x, 
                           header = TRUE))
                         if (!inherits(tmp, 
                                       'try-error')) 
                           tmp
                         }
                       )

## combine all txt files together
combined_df <- do.call("rbind", 
                       lapply(csv_files_df, 
                              as.data.frame)
                       )

#####----------Experiment 2b-1 Data Cleaning----------#####

## delete rows for instructions
combined_df <- combined_df[is.na(combined_df$instr_Loop.thisRepN),]
## delete rows for between-block slides
combined_df <- combined_df[is.na(combined_df$selectBlock.thisRepN),]
#delete practice trials
combined_df <- combined_df[str_count(combined_df$stim1, "practice")==0,]

#add subject number
subject <- NULL
for (i in seq(1:6)){
  subject =append(subject,rep(i,192))
}

#add 3D and connectivity info
combined_df$dimensionality <- recode(str_count(combined_df$stim1, "Cube"),'1'='3D','0'='2D')

combined_df$connected <- recode(str_count(combined_df$stim1, "6u_s_(.)_s")+str_count(combined_df$stim1, "6u_s_(..)_s"),'1'='dc','0'='c')

#select useful fields
combined_df <- combined_df%>%
  mutate(subject = subject)%>%
  select(subject,age,sex..female.male.,order,struc_judge_respond.corr,struc_judge_respond.rt,dual_respond.corr,dual_respond.rt,trials.thisTrialN,trials.thisIndex,stim1,stim2,isAnyChange,connected,dimensionality)

combined_df1 <- combined_df

#####----------Load Experiment 2b-2 Data----------#####

## read csv files
csv_files_ls = list.files(path=getwd(), pattern = "*.csv")
csv_files_df <- lapply(csv_files_ls, 
                       function(x) 
                       { 
                         tmp <- try(read.csv(
                           file = x, 
                           header = TRUE))
                         if (!inherits(tmp, 
                                       'try-error')) 
                           tmp
                       }
)

## combine all txt files together
combined_df <- do.call("rbind", 
                       lapply(csv_files_df, 
                              as.data.frame)
)


#####----------Experiment 2b-2 Data Cleaning----------#####

## delete rows for instructions
combined_df <- combined_df[is.na(combined_df$instr_Loop.thisRepN),]
## delete rows for between-block slides
combined_df <- combined_df[is.na(combined_df$selectBlock.thisRepN),]
#delete practice trials
combined_df <- combined_df[str_count(combined_df$stim1, "practice")==0,]

#add subject number
subject <- NULL
for (i in seq(7,25,1)){
  subject =append(subject,rep(i,192))
}

#add 3D and connectivity info
combined_df$dimensionality <- recode(str_count(combined_df$stim1, "Cube"),'1'='3D','0'='2D')

combined_df$connected <- recode(str_count(combined_df$stim1, "6u_s_(.)_s")+str_count(combined_df$stim1, "6u_s_(..)_s"),'1'='dc','0'='c')

#select useful fields
combined_df <- combined_df%>%
  mutate(subject = subject)%>%
  select(subject,age,sex..female.male.,order,struc_judge_respond.corr,struc_judge_respond.rt,dual_respond.corr,dual_respond.rt,trials.thisTrialN,trials.thisIndex,stim1,stim2,isAnyChange,connected,dimensionality)

#####----------Experiment 2b All Data Cleaning----------#####

combined_df <- rbind(combined_df1,combined_df)

## performance on the concurrent verbal task:
v_perf <- aggregate(combined_df$dual_respond.corr, 
                    list(subject=combined_df$subject), 
                    mean)
names(v_perf) [2] <- "VerbTaskAcc"


## performance on the change detection task:
Exp2_cdt <- aggregate(combined_df$struc_judge_respond.corr,
                      list(subject=combined_df$subject,
                           dim = combined_df$dimensionality,
                           conn = combined_df$connected,
                           change=combined_df$isAnyChange), 
                      mean)

names(Exp2_cdt)[5] <- "acc"
## poor performance in the concurrent verbal task
v_perf <- v_perf[v_perf$VerbTaskAcc>=0.8,]

gender <- combined_df%>%group_by(subject,sex..female.male.)%>%
  summarise(
    n=n()
  )

## exclude low verb_acc participants 
Exp2_cdt <- Exp2_cdt[Exp2_cdt$subject %in% v_perf$subject,]

## exclude participants with low acc in change detection task

c_perf <- aggregate(Exp2_cdt$acc, 
                    list(subject=Exp2_cdt$subject), 
                    mean)
names(c_perf) [2] <- "CDTAcc"
c_perf <- c_perf[c_perf$CDTAcc>0.5,]

Exp2_cdt <- Exp2_cdt[Exp2_cdt$subject %in% c_perf$subject,]

#####------Accuracy Descriptive Statistics-----#####

Exp2_descrp <- Exp2_cdt%>%
  group_by(dim,conn,change) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(acc, na.rm = TRUE),
    se = sd(acc, na.rm = TRUE)/sqrt(count)
  )

#####----------Signal Detection Theory----------#####

## recast Exp1 Structure Change Detection data
re_Exp2_cdt<- 
  recast(Exp2_cdt, 
         subject ~ dim + conn + change, 
         id.var = c("subject", "dim",  "conn", "change"))

names(re_Exp2_cdt) <- c("subject","d2_c_s","d2_c_d","d2_dc_s","d2_dc_d","d3_c_s","d3_c_d","d3_dc_s","d3_dc_d")

Exp2_fh <- re_Exp2_cdt %>%
  mutate_at(c("d2_c_s","d2_dc_s",
              "d3_c_s","d3_dc_s"),
            list(f=~ifelse(.==1,1/48,1-.)))%>%
  mutate_at(vars(ends_with('_d')),
            list(h=~ifelse(.==1,1-(1/48),ifelse(.==0,1/48,.))))%>%
  mutate_at(vars(contains('_f'),contains('_h')),
            list(z=~qnorm(.)))

#####----------Bias Calculation----------#####

Exp2_bias <- Exp2_fh %>%
  mutate(d2.c.b=exp(-1*(d2_c_d_h_z^2-d2_c_s_f_z^2)*0.5))%>%
  mutate(d2.dc.b=exp(-1*(d2_dc_d_h_z^2-d2_dc_s_f_z^2)*0.5))%>%
  mutate(d3.c.b=exp(-1*(d3_c_d_h_z^2-d3_c_s_f_z^2)*0.5))%>%
  mutate(d3.dc.b=exp(-1*(d3_dc_d_h_z^2-d3_dc_s_f_z^2)*0.5))%>%
  select(subject,contains('b'))

## wide to long
Exp2_bias_long <- reshape(Exp2_bias,
                          direction = 'long',
                          idvar = 'subject',
                          varying = c(2:5),
                          timevar='dim',
                          times=c('squares','cubes'),
                          v.names=c('c','dc'))
Exp2_bias_long <- gather(Exp2_bias_long,
                         conn,
                         bias,
                         c:dc,
                         factor_key = T)

Exp2_bias_long$subject <- as.factor(Exp2_bias_long$subject)

#####----------Bias Descriptive Statistics----------#####

indivi_bias <- Exp2_bias_long%>%
  group_by(subject) %>%
  dplyr::summarise(
    mean = mean(bias, na.rm = TRUE),
  )

t.test(indivi_bias$mean,mu=1)

#####----------d' Calculation----------#####
Exp2_fh <- Exp2_fh%>%
  mutate(d2.c.dp=d2_c_d_h_z-d2_c_s_f_z)%>%
  mutate(d2.dc.dp=d2_dc_d_h_z-d2_dc_s_f_z)%>%
  mutate(d3.c.dp=d3_c_d_h_z-d3_c_s_f_z)%>%
  mutate(d3.dc.dp=d3_dc_d_h_z-d3_dc_s_f_z)

Exp2_dpr <- Exp2_fh%>%
  select(subject,contains('dp'))

## wide to long
Exp2_dpr_long <- reshape(Exp2_dpr,
                         direction = 'long',
                         idvar = 'subject',
                         varying = c(2:5),
                         timevar='dim',
                         times=c('squares','cubes'),
                         v.names=c('connected', 'disconncted'))

Exp2_dpr_long <- gather(Exp2_dpr_long,
                        connectivity,
                        dp,
                        connected:disconncted,
                        factor_key = T)

Exp2_dpr_long$dim <- as.factor(Exp2_dpr_long$dim)
Exp2_dpr_long$subject <- as.factor(Exp2_dpr_long$subject)


#####--------d' Descriptive Statistics--------#####

Exp2_dpr_summary <- Exp2_dpr_long%>%
  group_by(dim, connectivity) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(dp, na.rm = TRUE),
    se = sd(dp, na.rm = TRUE)/sqrt(count)
  )

## visualization - bar graph with error bars
#jpeg("Exp2a_dpr.jpeg", width = 8, height = 4.5, units = 'in', res = 300)
ggplot(Exp2_dpr_summary,aes(x=connectivity,y=mean,fill = dim))+
  geom_bar(stat = "identity",position = position_dodge())+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5,position=position_dodge(0.9),size=.8,linetype="solid")+
  ylim(0,3)+
  ylab('d\'')+
  scale_fill_manual(values=c('#999999','#E69F00'))+
  theme_classic(base_size = 20)+
  labs(fill="dimensionality")
#dev.off()

#####----------Dprime ANOVA----------#####
aov2 <- aov(dp~dim*connectivity+Error(subject),data=Exp2_dpr_long)
summary(aov2)
eta_squared(aov2)
tapply(Exp2_dpr_long$dp, list(Exp2_dpr_long$dim,Exp2_dpr_long$connectivity), mean)


#####------Response Time Descriptive Statistics-----#####

## remove time-out trials
combined_df<- combined_df[combined_df$subject %in% c_perf$subject,]
combined_df_noTimeOut <- combined_df[complete.cases(combined_df[, "time"]),]

TIME <- aggregate(combined_df_noTimeOut$time,
                  list(subject=combined_df_noTimeOut$subject,
                       dim=combined_df_noTimeOut$dimensionality,
                       nUs=combined_df_noTimeOut$nUnits,
                       change=combined_df_noTimeOut$change), 
                  mean)

names(TIME)[5] <- "RT"

TIME$change <-recode(TIME$change,
                     '0' = 'no change',
                     '1' = 'change')
## descriptive statistics
Exp1_RT_summary <- TIME%>%
  group_by(dim,nUs,change) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(RT, na.rm = TRUE),
    sd = sd(RT, na.rm = TRUE),
    se = sd/sqrt(count)
  )
## visualization - line graph with error bars
#jpeg("Exp1_rt_line.jpeg", width = 8, height = 4.5, units = 'in', res = 300)
ggplot(Exp1_RT_summary,aes(x=nUs,y=mean,color=factor(dim),shape=factor(dim)))+
  geom_point(size=3)+
  geom_line()+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=1,position=position_dodge(0.2))+
  ylim(0.5,1.5)+
  ylab('response time(s)')+
  scale_x_continuous(name ="nUs",breaks=c(4,6,8),limits=c(3,9))+
  facet_wrap(~change)+
  theme_classic(base_size = 20)+
  scale_color_manual(values=c('#999999','#E69F00','#E69FA8'))
#dev.off()










