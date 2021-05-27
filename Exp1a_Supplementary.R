## ---------------------------
##
## Script name: Exp1a_Supplementary
##
## Purpose of script: Experiment 1a Supplementary Analysis: can only be used after running main analysis (complete data cleaning first)
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
combined_df<- combined_df[combined_df$subject %in% c_perf$subject,]
combined_df_noTimeOut <- combined_df[complete.cases(combined_df[, "time"]),]
## percent of time-out trial:
length(combined_df_noTimeOut$accuracy)/length(combined_df$accuracy)
## remove incorrectly answered trials
combined_df_c <- combined_df_noTimeOut[combined_df_noTimeOut$accuracy==1,]
length(combined_df_c$accuracy)/length(combined_df$accuracy)

TIME <- aggregate(combined_df_c$time,
                  list(subject=combined_df_c$subject,
                       dim=combined_df_c$dimensionality,
                       nUs=combined_df_c$nUnits,
                       change=combined_df_c$change), 
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

#####------Response Time ANOVA-----#####
TIME[,c(1:4)] <- lapply(TIME[,c(1:4)],factor)
aov_s1 <- aov(RT~dim*nUs+change+Error(subject),data=TIME)
summary(aov_s1)
eta_squared(aov_s1)

#####----------Capacity Calculation----------#####
Exp1_k <- Exp1_fh %>%
  mutate(d1.n4.k=4*(d1_n4_d_h-d1_n4_s_f)/(1-d1_n4_s_f))%>%
  mutate(d1.n6.k=6*(d1_n6_d_h-d1_n6_s_f)/(1-d1_n6_s_f))%>%
  mutate(d1.n8.k=8*(d1_n8_d_h-d1_n8_s_f)/(1-d1_n8_s_f))%>%
  mutate(d2.n4.k=4*(d2_n4_d_h-d2_n4_s_f)/(1-d2_n4_s_f))%>%
  mutate(d2.n6.k=6*(d2_n6_d_h-d2_n6_s_f)/(1-d2_n6_s_f))%>%
  mutate(d2.n8.k=8*(d2_n8_d_h-d2_n8_s_f)/(1-d2_n8_s_f))%>%
  mutate(d3.n4.k=4*(d3_n4_d_h-d3_n4_s_f)/(1-d3_n4_s_f))%>%
  mutate(d3.n6.k=6*(d3_n6_d_h-d3_n6_s_f)/(1-d3_n6_s_f))%>%
  mutate(d3.n8.k=8*(d3_n8_d_h-d3_n8_s_f)/(1-d3_n8_s_f))%>%
  select(subject,contains('k'))

## wide to long
Exp1_k_long <- reshape(Exp1_k,
                       direction = 'long',
                       idvar = 'subject',
                       varying = c(2:10),
                       timevar='dim',
                       times=c('1D', '2D','3D'),
                       v.names=c('n4', 'n6','n8'))
Exp1_k_long <- gather(Exp1_k_long,
                      nUs,
                      k,
                      n4:n8,
                      factor_key = T)

Exp1_k_long$subject <- as.factor(Exp1_k_long$subject)

#####----------Capacity Descriptive Statistics----------#####
Exp1_k_long <-Exp1_k_long%>%
  mutate(nUs_num = dplyr::case_when(
    nUs == 'n4'  ~ 4,
    nUs == 'n6'  ~ 6,
    nUs == 'n8'  ~ 8))
Exp1_k_summary <- Exp1_k_long%>%
  group_by(dim, nUs) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(k, na.rm = TRUE),
    se = sd(k, na.rm = TRUE)/sqrt(count),
    nUs_num = mean(nUs_num)
  )

psych::describe(Exp1_k_long$k)
t.test(Exp1_k_long$k,conf.level = .95)

ggplot(Exp1_k_summary,aes(x=nUs_num,y=mean,color=dim,linetype=dim))+
  geom_point(size=3)+
  geom_line(size=1)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=1,position=position_dodge(0),size=.8,linetype="solid",alpha=0.5)+
  ylim(2,5)+
  ylab('Capacity')+
  scale_x_continuous(name ="Units",breaks=c(4,6,8),limits=c(3,9))+
  theme_classic(base_size = 20)+
  scale_color_manual(values=c('#999999','#E69F00','#E69FA8'))+
  labs(colour="dimensionality",linetype="dimensionality")

#####----------Capacity ANOVA----------#####

## recast Exp1 Structure Change Detection data
Exp1_cdt_ac_dim <- aggregate(combined_df$accuracy,
                             list(subject=combined_df$subject,
                                  nUs = combined_df$nUnits,
                                  change=combined_df$change), 
                             mean)
cdt_Units<- 
  recast(Exp1_cdt_ac_dim, 
         subject ~ nUs + change, 
         id.var = c("subject","nUs", "change"))

names(cdt_Units) <- c("subject","n4_s","n4_d","n6_s","n6_d","n8_s","n8_d")

units_fh <- cdt_Units %>%
  mutate_at(c("n4_s","n6_s",
              "n8_s"),
            list(f=~ifelse(.==1,1/48,1-.)))%>%
  mutate_at(vars(contains('_d')),
            list(h=~ifelse(.==1,1-(1/48),ifelse(.==0,1/48,.))))%>%
  mutate_at(vars(contains('_f'),contains('_h')),
            list(z=~qnorm(.)))

units_k <- units_fh %>%
  mutate(n4.k=4*(n4_d_h-n4_s_f)/(1-n4_s_f))%>%
  mutate(n6.k=6*(n6_d_h-n6_s_f)/(1-n6_s_f))%>%
  mutate(n8.k=8*(n8_d_h-n8_s_f)/(1-n8_s_f))%>%
  select(subject,contains('k'))

cor(units_k[,-1])

units_k_long <- reshape(units_k,
                        direction = 'long',
                        idvar = 'subject',
                        varying = c(2:4),
                        timevar='units',
                        times=c('n4', 'n6','n8'),
                        v.names=c('k'))
units_k_long$subject <- as.factor(units_k_long$subject)
units_k_long$units <- as.factor(units_k_long$units)

aov <- aov(k~units+Error(subject),data=units_k_long)
summary(aov)

tapply(units_k_long$k,units_k_long$units,mean)
t.test(units_k$n4.k,mu=3.52)
t.test(units_k$n6.k,mu=3.52)
t.test(units_k$n8.k,mu=3.52)