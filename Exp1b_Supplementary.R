## ---------------------------
##
## Script name: Exp1b_Supplementary
##
## Purpose of script: Experiment 1b Supplementary Analysis: can only be used after running main analysis (complete data cleaning first)
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
                       type=combined_df_c$setName,
                       nUs=combined_df_c$nUnits,
                       change=combined_df_c$change), 
                  mean)

names(TIME)[5] <- "RT"

TIME$change <-recode(TIME$change,
                     '0' = 'no change',
                     '1' = 'change')
## descriptive statistics
Exp1_RT_summary <- TIME%>%
  group_by(type,nUs,change) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(RT, na.rm = TRUE),
    sd = sd(RT, na.rm = TRUE),
    se = sd/sqrt(count)
  )
## visualization - line graph with error bars
#jpeg("Exp1_rt_line.jpeg", width = 8, height = 4.5, units = 'in', res = 300)
ggplot(Exp1_RT_summary,aes(x=nUs,y=mean,color=factor(type),shape=factor(type)))+
  geom_point(size=3)+
  geom_line()+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=1,position=position_dodge(0.2))+
  ylim(0.5,1.5)+
  ylab('response time(s)')+
  scale_x_continuous(name ="nUs",breaks=c(4,6,8),limits=c(3,9))+
  facet_wrap(~change)+
  theme_classic(base_size = 20)+
  scale_color_manual(values=c('#999999','#E69F00'))
#dev.off()

#####------Response Time ANOVA-----#####
TIME[,c(1:4)] <- lapply(TIME[,c(1:4)],factor)
aov_s2 <- aov(RT~type*nUs+change+Error(subject),data=TIME)
summary(aov_s2)
eta_squared(aov_s2)

TIME%>%
  group_by(nUs) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(RT, na.rm = TRUE),
    sd = sd(RT, na.rm = TRUE),
    se = sd/sqrt(count)
  )
TIME%>%
  group_by(type) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(RT, na.rm = TRUE),
    sd = sd(RT, na.rm = TRUE),
    se = sd/sqrt(count)
  )
TIME%>%
  group_by(change) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(RT, na.rm = TRUE),
    sd = sd(RT, na.rm = TRUE),
    se = sd/sqrt(count)
  )

#####----------Capacity Calculation----------#####
Exp1_k <- Exp1_fh %>%
  mutate(cubes.n4.k=4*(cubes_n4_d_h-cubes_n4_s_f)/(1-cubes_n4_s_f))%>%
  mutate(cubes.n6.k=6*(cubes_n6_d_h-cubes_n6_s_f)/(1-cubes_n6_s_f))%>%
  mutate(cubes.n8.k=8*(cubes_n8_d_h-cubes_n8_s_f)/(1-cubes_n8_s_f))%>%
  mutate(squares.n4.k=4*(squares_n4_d_h-squares_n4_s_f)/(1-squares_n4_s_f))%>%
  mutate(squares.n6.k=6*(squares_n6_d_h-squares_n6_s_f)/(1-squares_n6_s_f))%>%
  mutate(squares.n8.k=8*(squares_n8_d_h-squares_n8_s_f)/(1-squares_n8_s_f))%>%
  select(subject,contains('.k'))

## wide to long
Exp1_k_long <- reshape(Exp1_k,
                       direction = 'long',
                       idvar = 'subject',
                       varying = c(2:7),
                       timevar='dim',
                       times=c('cubes','squares'),
                       v.names=c('n4', 'n6','n8'))

Exp1_k_long <- gather(Exp1_k_long,
                      nUs,
                      k,
                      n4:n8,
                      factor_key = T)

Exp1_k_long$subject <- as.factor(Exp1_k_long$subject)

#####----------Capacity Descriptive Statistics----------#####
indivi_k <- Exp1_k_long%>%
  group_by(subject) %>%
  dplyr::summarise(
    mean = mean(k, na.rm = TRUE),
  )

t.test(indivi_k$mean)

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

ggplot(Exp1_k_summary,aes(x=nUs_num,y=mean,color=dim,linetype=dim))+
  geom_point(size=3)+
  geom_line(size=1)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=1,position=position_dodge(0),size=.8,linetype="solid",alpha=0.5)+
  ylim(2,6)+
  ylab('Capacity')+
  scale_x_continuous(name ="Units",breaks=c(4,6,8),limits=c(3,9))+
  theme_classic(base_size = 20)+
  scale_color_manual(values=c('#999999','#E69F00'))+
  labs(colour="dimensionality",linetype="dimensionality")

#####----------Capacity ANOVA----------#####

aov <- aov(k~dim*nUs+Error(subject),data=Exp1_k_long)
summary(aov)
