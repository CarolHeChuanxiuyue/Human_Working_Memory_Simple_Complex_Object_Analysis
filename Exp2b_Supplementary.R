## ---------------------------
##
## Script name: Exp2a_Supplementary
##
## Purpose of script: Experiment 2a Supplementary Analysis: can only be used after running main analysis (complete data cleaning first)
##
## Author: Chuanxiuyue (Carol) He
##
## Date Created: 2021-05-27
##
## Email: carol.hcxy@gmail.com
## 
## Content: 
##      1. Response Time Descriptive Statistics
##      2. Response Time ANOVA
##      3. Capacity Calculatioon
##      4. Capacity Descriptive Statistics
##      5. Capacity ANOVA
## ---------------------------



#####------Response Time Descriptive Statistics-----#####

## only include valid participants
combined_df <- combined_df[combined_df$subject %in% v_perf$subject,]
combined_df <- combined_df[combined_df$subject %in% c_perf$subject,]

## remove time-out trials
combined_df_noTimeOut <- combined_df[complete.cases(combined_df[, "struc_judge_respond.rt"]),]
length(combined_df_noTimeOut$struc_judge_respond.corr)/length(combined_df$struc_judge_respond.corr)

## remove incorrectly-answered trials
combined_df_c <- combined_df_noTimeOut[combined_df_noTimeOut$struc_judge_respond.corr==1,]
length(combined_df_c$struc_judge_respond.corr)/length(combined_df$struc_judge_respond.corr)

TIME <- aggregate(combined_df_c$struc_judge_respond.rt,
                  list(subject=combined_df_c$subject,
                       dim = combined_df_c$dimensionality,
                       conn = combined_df_c$connected,
                       change=combined_df_c$isAnyChange), 
                  mean)

names(TIME)[5] <- "RT"

TIME$change <-recode(TIME$change,
                     '0' = 'no change',
                     '1' = 'change')
## descriptive statistics
Exp2_RT_summary <- TIME%>%
  group_by(dim,conn,change) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(RT, na.rm = TRUE),
    sd = sd(RT, na.rm = TRUE),
    se = sd/sqrt(count)
  )

#####------Response Time ANOVA-----#####
TIME[,c(1:4)] <- lapply(TIME[,c(1:4)],factor)
aov_s4 <- aov(RT~dim*conn+change+Error(subject),data=TIME)
summary(aov_s4)
eta_squared(aov_s4)

#####----------Capacity Calculation----------#####
Exp2_k <- Exp2_fh %>%
  mutate(d2.c.k=6*(d2_c_d_h-d2_c_s_f)/(1-d2_c_s_f))%>%
  mutate(d2.dc.k=6*(d2_dc_d_h-d2_dc_s_f)/(1-d2_dc_s_f))%>%
  mutate(d3.c.k=6*(d3_c_d_h-d3_c_s_f)/(1-d3_c_s_f))%>%
  mutate(d3.dc.k=6*(d3_dc_d_h-d3_dc_s_f)/(1-d3_dc_s_f))%>%
  select(subject,contains('k'))

## wide to long
Exp2_k_long <- reshape(Exp2_k,
                       direction = 'long',
                       idvar = 'subject',
                       varying = c(2:5),
                       timevar='dim',
                       times=c('squares','cubes'),
                       v.names=c('c','dc'))

Exp2_k_long <- gather(Exp2_k_long,
                      conn,
                      k,
                      c:dc,
                      factor_key = T)

Exp2_k_long$subject <- as.factor(Exp2_k_long$subject)

#####----------Capacity Descriptive Statistics----------#####
indivi_k <- Exp2_k_long%>%
  group_by(subject) %>%
  dplyr::summarise(
    mean = mean(k, na.rm = TRUE),
  )

t.test(indivi_k$mean)
sd(indivi_k$mean)/sqrt(length(indivi_k$mean))

Exp2_k_summary <- Exp2_k_long%>%
  group_by(dim, conn) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(k, na.rm = TRUE),
    se = sd(k, na.rm = TRUE)/sqrt(count)
  )

#####----------Capacity ANOVA----------#####

aov <- aov(k~dim*conn+Error(subject),data=Exp2_k_long)
summary(aov)
