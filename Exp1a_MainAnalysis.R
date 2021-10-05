## ---------------------------
##
## Script name: Exp1a_MainAnalysis
##
## Purpose of script: Experiment 1a data cleaning and EDA and analysis
##
## Author: Chuanxiuyue (Carol) He
##
## Date Created: 2021-05-20
##
## Email: carol.hcxy@gmail.com
## 
## Content: 
##      1. Load Experiment 1a Data
##      2. Experiment 1a Data Cleaning
##      3. Accuracy Descriptive Statistics
##      4. Bias Calculation
##      5. Bias Descriptive Statistics
##      6. d' Calculation
##      7. d' Descriptive Statistics
##      8. d' ANOVA
##      9. d' Bayes Factor
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
library(BayesFactor)

## ---------------------------

#####----------Load Experiment 1a Data----------#####

## read txt files
txt_files_ls <-  list.files(
  path=getwd(), 
  pattern = "*.txt")

txt_files_df <- lapply(txt_files_ls, 
                       function(x) 
                         { 
                         tmp <- try(read.table(
                           file = x, 
                           header = TRUE, 
                           sep=";")) 
                         if (!inherits(tmp, 
                                       'try-error')) 
                           tmp
                         }
                       )

## combine all txt files together
combined_df <- do.call("rbind", 
                       lapply(txt_files_df, 
                              as.data.frame)
                       )


#####----------Experiment 1a Data Cleaning----------#####

## finds -99999 values in the accuracy column and sets them to NA so that we can omit them and saving a new data frame
combined_df$accuracy[combined_df$accuracy == -99999] <- 0

## finds timed-out trials and sets them to NA
combined_df$time[combined_df$time == -1] <- NA

## no dual task trials set as NA
combined_df$dualTaskAcc[combined_df$dualTaskAcc == -99] <- NA

## performance on the concurrent verbal task:
v_df <- na.omit(combined_df)  
v_perf <- aggregate(v_df$dualTaskAcc, 
                    list(subject=v_df$subject), 
                    mean)
names(v_perf) [2] <- "VerbTaskAcc"


## performance on the change detection task:
Exp1_cdt <- aggregate(combined_df$accuracy,
                  list(subject=combined_df$subject,
                       dim = combined_df$dimensionality,
                       nUs = combined_df$nUnits,
                       change=combined_df$change), 
                  mean)

names(Exp1_cdt)[5] <- "acc"


## poor performance in the concurrent verbal task
v_perf <- v_perf[v_perf$VerbTaskAcc>=0.8,]

## exclude low verb_acc participants 
Exp1_cdt <- Exp1_cdt[Exp1_cdt$subject %in% v_perf$subject,]

## exclude participants with low acc in change detection task

c_perf <- aggregate(Exp1_cdt$acc, 
                    list(subject=Exp1_cdt$subject), 
                    mean)
names(c_perf) [2] <- "CDTAcc"
c_perf <- c_perf[c_perf$CDTAcc>0.5,]

Exp1_cdt <- Exp1_cdt[Exp1_cdt$subject %in% c_perf$subject,]

#####------Accuracy Descriptive Statistics-----#####

Exp1_descrp <- Exp1_cdt%>%
  group_by(dim,nUs,change) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(acc, na.rm = TRUE),
    se = sd(acc, na.rm = TRUE)/sqrt(count)
  )

#####----------Signal Detection Theory----------#####

## recast Exp1 Structure Change Detection data
re_Exp1_cdt<- 
  recast(Exp1_cdt, 
         subject ~ dim + nUs + change, 
         id.var = c("subject", "dim",  "nUs", "change"))

names(re_Exp1_cdt) <- c("subject","d1_n4_s","d1_n4_d","d1_n6_s","d1_n6_d","d1_n8_s","d1_n8_d","d2_n4_s","d2_n4_d","d2_n6_s","d2_n6_d","d2_n8_s","d2_n8_d","d3_n4_s","d3_n4_d","d3_n6_s","d3_n6_d","d3_n8_s","d3_n8_d")

Exp1_fh <- re_Exp1_cdt %>%
  mutate_at(c("d1_n4_s","d1_n6_s",
              "d1_n8_s","d2_n4_s","d2_n6_s",
              "d2_n8_s","d3_n4_s","d3_n6_s",
              "d3_n8_s"),
            list(f=~ifelse(.==1,1/16,1-.)))%>%
  mutate_at(vars(contains('_d')),
            list(h=~ifelse(.==1,1-(1/16),ifelse(.==0,1/16,.))))%>%
  mutate_at(vars(contains('_f'),contains('_h')),
            list(z=~qnorm(.)))

#####----------Bias Calculation----------#####

Exp1_bias <- Exp1_fh %>%
  mutate(d1.n4.b=exp(-1*(d1_n4_d_h_z^2-d1_n4_s_f_z^2)*0.5))%>%
  mutate(d1.n6.b=exp(-1*(d1_n6_d_h_z^2-d1_n6_s_f_z^2)*0.5))%>%
  mutate(d1.n8.b=exp(-1*(d1_n8_d_h_z^2-d1_n8_s_f_z^2)*0.5))%>%
  mutate(d2.n4.b=exp(-1*(d2_n4_d_h_z^2-d2_n4_s_f_z^2)*0.5))%>%
  mutate(d2.n6.b=exp(-1*(d2_n6_d_h_z^2-d2_n6_s_f_z^2)*0.5))%>%
  mutate(d2.n8.b=exp(-1*(d2_n8_d_h_z^2-d2_n8_s_f_z^2)*0.5))%>%
  mutate(d3.n4.b=exp(-1*(d3_n4_d_h_z^2-d3_n4_s_f_z^2)*0.5))%>%
  mutate(d3.n6.b=exp(-1*(d3_n6_d_h_z^2-d3_n6_s_f_z^2)*0.5))%>%
  mutate(d3.n8.b=exp(-1*(d3_n8_d_h_z^2-d3_n8_s_f_z^2)*0.5))%>%
  dplyr::select(subject,contains('b'))

## wide to long
Exp1_bias_long <- reshape(Exp1_bias,
                          direction = 'long',
                          idvar = 'subject',
                          varying = c(2:10),
                          timevar='dim',
                          times=c('1D', '2D','3D'),
                          v.names=c('n4', 'n6','n8'))
Exp1_bias_long <- gather(Exp1_bias_long,
                         nUs,
                         bias,
                         n4:n8,
                         factor_key = T)

Exp1_bias_long$subject <- as.factor(Exp1_bias_long$subject)

#####----------Bias Descriptive Statistics----------#####

Exp1_bias_long%>%
  group_by(dim, nUs) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(bias, na.rm = TRUE),
    se = sd(bias, na.rm = TRUE)/sqrt(count)
  )
indivi_bias <- Exp1_bias_long%>%
  group_by(subject) %>%
  dplyr::summarise(
    mean = mean(bias, na.rm = TRUE),
  )

t.test(indivi_bias$mean,mu=1)


#####----------d' Descriptive Statistics----------#####
Exp1_fh <- Exp1_fh%>%
  mutate(d1.n4.dp=d1_n4_d_h_z-d1_n4_s_f_z)%>%
  mutate(d1.n6.dp=d1_n6_d_h_z-d1_n6_s_f_z)%>%
  mutate(d1.n8.dp=d1_n8_d_h_z-d1_n8_s_f_z)%>%
  mutate(d2.n4.dp=d2_n4_d_h_z-d2_n4_s_f_z)%>%
  mutate(d2.n6.dp=d2_n6_d_h_z-d2_n6_s_f_z)%>%
  mutate(d2.n8.dp=d2_n8_d_h_z-d2_n8_s_f_z)%>%
  mutate(d3.n4.dp=d3_n4_d_h_z-d3_n4_s_f_z)%>%
  mutate(d3.n6.dp=d3_n6_d_h_z-d3_n6_s_f_z)%>%
  mutate(d3.n8.dp=d3_n8_d_h_z-d3_n8_s_f_z)

Exp1_dpr <- Exp1_fh%>%
  dplyr::select(subject,contains('dp'))

## wide to long
Exp1_dpr_long <- reshape(Exp1_dpr,
                         direction = 'long',
                         idvar = 'subject',
                         varying = c(2:10),
                         timevar='dim',
                         times=c('1D','2D','3D'),
                         v.names=c('n4', 'n6','n8'))

Exp1_dpr_long <- gather(Exp1_dpr_long,
                        nUs,
                        dp,
                        n4:n8,
                        factor_key = T)

Exp1_dpr_long$dim <- as.factor(Exp1_dpr_long$dim)
Exp1_dpr_long$subject <- as.factor(Exp1_dpr_long$subject)

Exp1_dpr_long <-Exp1_dpr_long%>%
  mutate(nUs_num = dplyr::case_when(
    nUs == 'n4'  ~ 4,
    nUs == 'n6'  ~ 6,
    nUs == 'n8'  ~ 8))

#####--------d' Descriptive Statistics--------#####

Exp1_dpr_summary <- Exp1_dpr_long%>%
  group_by(dim, nUs) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(dp, na.rm = TRUE),
    se = sd(dp, na.rm = TRUE)/sqrt(count),
    nUs_num = mean(nUs_num)
  )

## visualization - line graph with error bars
##jpeg("Exp1_dpr_line.jpeg", width = 9, height = 4.5, units = 'in', res = 300)
ggplot(Exp1_dpr_summary,aes(x=nUs_num,y=mean,color=dim,linetype=dim))+
  geom_point(size=3)+
  geom_line(size=1)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=1,position=position_dodge(0),size=.8,linetype="solid")+
  ylim(0,4)+
  ylab('d\'')+
  scale_x_continuous(name ="Units",breaks=c(4,6,8),limits=c(3,9))+
  theme_classic(base_size = 20)+
  scale_color_manual(values=c('#999999','#555555','#000000'))+
  labs(colour=" structural \n dimensionality",linetype=" structural \n dimensionality")
##dev.off()

#####----------d' ANOVA----------#####
aov1 <- aov(dp~dim*nUs+Error(subject),data=Exp1_dpr_long)
summary(aov1)
eta_squared(aov1)

#####----------d' Bayes Factor----------#####
bf = anovaBF(dp ~ dim*nUs+subject, data=Exp1_dpr_long, whichRandom="subject")
bf
plot(bf)


