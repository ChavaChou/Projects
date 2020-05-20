library(data.table)
library(lubridate)
library(ggplot2)
library(rddtools)
library(rdrobust)
library(rdd)

new_d1 <- read.csv('/Users/miana/Downloads/new_d1.csv',header = T)
new_d2 <- read.csv('/Users/miana/Downloads/new_d2.csv',header = T)
avg_d <- merge(new_d1,new_d2, by = c("X","State","year","month"))
d <- fread('/Users/miana/Downloads/US_Accidents_Dec19.csv',stringsAsFactors = F)

me <- d[d$State == 'ME',]


me$month <- month(me$Start_Time)
me$year <- year(me$Start_Time)
view(me)

me_crash <- me %>%
  select(year,month,State)%>%
  group_by(year,month)%>%
  count()

avg_me <- avg_d[avg_d$State =="ME",]
me <- merge(avg_me,me_crash, by = c("year","month"))

ri <- d[d$State == 'RI',]
ri$month <- month(ri$Start_Time)
ri$year <- year(ri$Start_Time)
ri_crash <- ri %>%
  select(year,month,State)%>%
  group_by(year,month)%>%
  count()
avg_ri<- avg_d[avg_d$State =="RI",]
ri <- merge(avg_ri,ri_crash, by = c("year","month"))

f <- rbind(me,ri)
colnames(f)
f<-f %>%
  select(X,State,year,month,n,avg_severity:major_astro)%>%
  arrange(X,State,year,month,n)

write.csv(f,'/Users/miana/Downloads/me_ri_crashes_weather.csv')

f <- read.csv('/Users/miana/Downloads/me_ri_crashes_weather.csv',header = T)
# differences in difference

f$y_m <- ymd(paste(f$year,f$month,1,sep=" "))
colnames(f)
f$after <- ifelse(f$y_m>=as.POSIXct("2016-11-1"),1,0)
f$legalization <- ifelse(f$State=='ME',1,0)

filter_f <- f %>%
  filter(y_m <as.POSIXct("2017-7-1"))
filter_f$week <- c(1:2,4:16,1:16)

ggplot(filter_f, aes(x = week, y = n,group = State, color = factor(State))) + 
  geom_line()+
  geom_vline(xintercept = 9)



view(f)
did_basic = lm(log(n+1) ~ legalization + after + legalization*after +as.factor(major_weather_condition), data=filter_f)
summary(did_basic)

library(plm)
did_sfe_tfe = plm(log(n+1) ~ legalization + after + legalization*after + as.factor(major_weather_condition), data = filter_f, index=c("State", "week"), effect="twoway", model="within")
summary(did_sfe_tfe)



# dynamic failed
did_dyn_sfe_tfe <- lm(log(n+1) ~ legalization + as.factor(week) + legalization*as.factor(week)+as.factor(major_weather_condition), data = filter_f)
summary(did_dyn_sfe_tfe)

# regression disconuity 
# the legalization date is not set randomly


