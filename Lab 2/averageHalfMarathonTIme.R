library(tidyverse)
library(textreadr)
library(rvest)
library(lubridate)
#library(RSelenium)
library(here)
library(hrbrthemes)
library(viridis)
library(AMR)
library(infer)

### 5k
cb<-rvest::read_html(here("data/","5k", "Seattle Cherry Blossom Run Results.html"))
cb_tab<-cb%>% minimal_html()%>%
  html_node("table") %>% 
  html_table(header=1)
cb_tab<-cb_tab%>%mutate(
  ClockTime_min=hms(ClockTime),
  ClockTime_min=hour(ClockTime_min)*60 + minute(ClockTime_min)+second(ClockTime_min)/100,
  ChipTime_min=hms(ChipTime),
  ChipTime_min=hour(ChipTime_min)*60 + minute(ChipTime_min)+second(ChipTime_min)/100,
)

cb_tab%>%ggplot(aes(y=ChipTime_min,x=Age,group=Gender,color=Gender))+
  geom_point()+
  geom_line()+
  theme_bw()+
  ggtitle("Run Time By Age of Half Marathon Runners")+
  xlab("Age (Years)")+
  ylab("Time (Minutes)")

### 5k

## Half Marathon
cb<-rvest::read_html(here("data/","half", "Seattle Cherry Blossom Run Results.html"))


cb_tab<-cb%>% minimal_html()%>%
  html_node("table") %>% 
  html_table(header=1)

cb_tab<-cb_tab%>%mutate(
  ChipTime_min=hms(ChipTime),
  ChipTime_min=hour(ChipTime_min)*60 + minute(ChipTime_min)+second(ChipTime_min)/100,
  Name=substr(str_remove_all(str_remove_all(Name,"\\n"),"\\t"),2,1000000L)
)

glimpse(cb_tab)
cb_tab$age_groups<-AMR::age_groups(cb_tab$Age,"fives")

## Counts by age
cb_tab%>%group_by(age_groups)%>%summarise(n=n())

## Counts by age and gender
cb_tab%>%group_by(age_groups,Gender)%>%summarise(n=n())
cb_tab_mf<-cb_tab%>%filter(Gender%in%c("F","M"))
cb_tab_mf%>%group_by(age_groups,Gender)%>%summarise(n=n())


stats_overall<-cb_tab_mf%>%summarise(
  Total=n(),
  AvgTime=mean(ChipTime_min),
  se=sd(ChipTime_min)/Total,
  med=median(ChipTime_min),
  q5=quantile(ChipTime_min,.05),
  q95=quantile(ChipTime_min,.95)
)

stats_gender<-cb_tab_mf%>%group_by(Gender)%>%summarise(
  Total=n(),
  AvgTime=mean(ChipTime_min),
  se=sd(ChipTime_min)/Total,
  med=median(ChipTime_min),
  q5=quantile(ChipTime_min,.05),
  q95=quantile(ChipTime_min,.95)
)

##################
## Statistical test
##################
dplyr::glimpse(cb_tab_mf)

# calculate the observed statistic
observed_statistic <- cb_tab_mf %>%
  specify(response = ChipTime_min) %>%
  calculate(stat = "mean")

# generate the null distribution
null_dist_1_sample <- cb_tab_mf %>%
  specify(response = ChipTime_min) %>%
  hypothesize(null = "point", mu = 140) %>% ## 140 Min Average Female Time in Minutes for 20-24 year olds
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean")

# visualize the null distribution and test statistic!
null_dist_1_sample %>%
  visualize() + 
  shade_p_value(observed_statistic,
                direction = "two-sided")+
  theme_bw()

# calculate the p value from the test statistic and null distribution
p_value_1_sample <- null_dist_1_sample %>%
  get_p_value(obs_stat = observed_statistic,
              direction = "two-sided")

p_value_1_sample

cb_tab_mf%>%t_test(response =ChipTime_min , mu = 140)


# calculate the observed statistic
observed_statistic <- cb_tab_mf %>%
  specify(response = ChipTime_min) %>%
  hypothesize(null = "point", mu = 40) %>%
  calculate(stat = "t") %>%
  dplyr::pull()

# calculate 2-tail t-test
pt(observed_statistic, df = nrow(gss) - 1, lower.tail = FALSE)*2

########
#Paired t-Test
#######

# calculate the observed statistic
observed_statistic <-  cb_tab_mf  %>%
  specify( ChipTime_min ~ Gender) %>%
  calculate(stat = "diff in means", order = c("F", "M"))

observed_statistic


# generate the null distribution with randomization
null_dist_2_sample <- cb_tab_mf %>%
  specify(ChipTime_min ~ Gender) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("F", "M"))

null_dist_2_sample %>%
  visualize() + 
  shade_p_value(observed_statistic,
                direction = "two-sided")+
  theme_bw()

## P-value
p_value_2_sample <- null_dist_2_sample %>%
  get_p_value(obs_stat = observed_statistic,
              direction = "two-sided")

p_value_2_sample

## t-test
cb_tab_mf%>%t_test( 
       formula = ChipTime_min ~ Gender, 
       order = c("F", "M"),
       alternative = "two-sided")

# calculate the observed statistic
observed_statistic <- cb_tab_mf %>%
  specify(ChipTime_min ~ Gender,) %>%
  hypothesize(null = "point", mu = 0) %>%
  calculate(stat = "t", order = c("F", "M")) %>%
  dplyr::pull()

observed_statistic

pt(observed_statistic, df = nrow(gss) - 2, lower.tail = FALSE)*2

############################
## Let's try young versus old (Males)
############################

# calculate the observed statistic
cb_tab_mf<-cb_tab_mf%>%mutate(
  youngOld = age_groups(Age,35),
  youngOld= recode_factor(youngOld, `0-34`="Young",
                          `35+`="Old")
)
levels(cb_tab_mf$youngOld)



observed_statistic <-  cb_tab_mf%>%filter(Gender=="M")  %>%
  specify(ChipTime_min ~ youngOld) %>%
  calculate(stat = "diff in means", order = c("Old","Young"))

observed_statistic


# generate the null distribution with randomization
null_dist_2_sample <- cb_tab_mf%>%filter(Gender=="M") %>%
  specify(ChipTime_min ~ youngOld) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("Old","Young"))

null_dist_2_sample %>%
  visualize() + 
  shade_p_value(observed_statistic,
                direction = "two-sided")+
  theme_bw()

## P-value
p_value_2_sample <- null_dist_2_sample %>%
  get_p_value(obs_stat = observed_statistic,
              direction = "two-sided")

p_value_2_sample

## t-test
cb_tab_mf%>%filter(Gender=="M")%>%t_test( 
  formula = ChipTime_min ~ youngOld, 
  order = c("Old","Young"),
  alternative = "two-sided")


#cb_tab_mf%>%mutate(City=tolower(City))%>%group_by(City)%>%summarise(n=n())%>%arrange(desc(n))

###########
## SLR
###########

cb_tab_mf%>%ggplot(aes(y=ChipTime_min,x=Age,color=Gender))+
  geom_point()+
  #stat_summary(fun.data= mean_cl_normal) + 
  geom_smooth(method='lm')+
  theme_bw()+
  xlab("Age")+
  ylab("Time (Minutes)")
 
models<-list(
  lm(ChipTime_min~1,data=cb_tab_mf),
  lm(ChipTime_min~Age,data=cb_tab_mf),
lm(ChipTime_min~Age+Gender,data=cb_tab_mf)
)

r2<-models%>%purrr::map(function(x){summary(x)$r.squared})%>%unlist()
ar2<-models%>%purrr::map(function(x){summary(x)$adj.r.squared})%>%unlist()
data.frame(model=1:3,r2,ar2)

list(broom::tidy(m0),broom::tidy(m1),broom::tidy(m2))%>%bind_rows(.id="models")


##################
## Statistical test
##################
stats<-cb_tab_mf%>%group_by(age_groups,Gender)%>%summarise(
  Total=n(),
  AvgTime=mean(ChipTime_min),
  se=sd(ChipTime_min)/Total,
  med=median(ChipTime_min),
  q5=quantile(ChipTime_min,.05),
  q95=quantile(ChipTime_min,.95)
  )

stats%>%ggplot(aes(y=AvgTime,x=age_groups,group=Gender,color=Gender))+
  geom_point()+
  geom_errorbar(aes(ymin=AvgTime-2*se, ymax=AvgTime+2*se), width=.2)+
  ylim(95,205)+
  theme_bw()

stats%>%ggplot(aes(y=med,x=age_groups,group=Gender,color=Gender))+
  geom_point()+
  geom_errorbar(aes(ymin=q5, ymax=q95), width=.1)+
  #ylim(95,205)+
  theme_bw()


cb_tab%>%
  ggplot( aes(x=age_groups, y=ChipTime_min, fill=Gender)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A boxplot with jitter") +
  xlab("")

cb_tab%>%ggplot(aes(x=ChipTime_min, color=Gender,fill=Gender)) +
  geom_histogram(alpha=0.5, position="identity")+
  theme_bw()

cb_tab%>%ggplot(aes(y=ChipTime_min,x=Age,group=Gender,color=Gender))+
  geom_point()+
  geom_line()+
  theme_bw()+
  ggtitle("Run Time By Age of Half Marathon Runners")+
  xlab("Age (Years)")+
  ylab("Time (Minutes)")

## Average Half Marathon times
hm_times<-rvest::read_html("https://runninforsweets.com/average-half-marathon-time/")

ahm_time<-hm_times%>% minimal_html()%>%
  html_node("table") %>% 
  html_table(header=1)

hms(Time.Training)        # format to 'hours:minutes:seconds'
hour(res)*60 + minute(res)       # convert hours to minutes, and add minutes

ahm_update<-ahm_time%>%mutate(
  Male=hm(Male),
  Male_min=hour(Male)*60 + minute(Male),
  Female=hm(Female),
  Female_min=hour(Female)*60 + minute(Female),
  Age_lower = parse_number(Age) 
  )


ahm_long<-ahm_update%>%pivot_longer(cols = c("Male_min","Female_min"),
                          names_to="Gender",
                          values_to="RunTime")%>%select(Gender,Age_lower,RunTime)




ahm_long%>%ggplot(aes(y=RunTime,x=Age_lower,group=Gender,color=Gender))+
          geom_point()+
          geom_line()+
          theme_bw()+
          ggtitle("Average Run Time By Age of Half Marathon Runners")+
          xlab("Age (Years)")+
          ylab("Time (Minutes)")

