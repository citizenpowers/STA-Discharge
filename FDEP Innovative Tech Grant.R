#the object of this project is to see look at the flow, nutrient loads, and water quality of the C-7 canal. 

library(ggplot2)
library(readr)
library(readxl)
library(tidyr)
library(stringr)
library(lubridate)
library(scales)
library(RColorBrewer)
library(viridis)
library(Hmisc)
library(ggpmisc)
library(ggrepel)
library(zoo)
library(dbhydroR)
library(tidyverse)
library(broom)
library(scales)

#Import Data

S27_S <- get_hydro(raw=T,dbkey ="91470",date_min = "2010-01-01", date_max = "2023-12-31")

LR06_WQ_Data <- read_excel("Data/LR06.xlsx")

# Tidy Data ---------------------------------------------------------------


LR06_WQ_Data_Tidy <- LR06_WQ_Data %>%
mutate(Date=ymd(`Act Date`))  %>%
filter(Characteristic %in% c("Phosphorus as PO4","Nitrogen, Kjeldahl"),Date>"2011-03-06",Date<"2017-01-10") %>%
mutate(`Value`=as.numeric(`Result Value`))  %>%
select(Date,Characteristic,`Value`) %>%
mutate(`Label Date`=ISOdate(2000,month(Date),mday(Date)))

LR06_WQ_Data_Tidy_Wide <- LR06_WQ_Data_Tidy %>%
pivot_wider(names_from="Characteristic",values_from="Value")  

S27_Q_Tidy<- S27_S %>%
select(21:26) %>%
rename(Date="date",Flow="data.value")  %>%
filter(Date>"2011-03-06",Date<"2017-01-10") %>%
select(Date,Flow) 


# Nutrient budget ----------------------------------------------------------------

Nutrient_budget_S7 <- as.data.frame(seq(from=as.Date("2011-03-06"),to=as.Date("2017-01-10"),by = "days")) %>% 
setNames("Date") %>%
mutate(`Label Date`=ISOdate(2000,month(Date),mday(Date)))%>%
left_join(S27_Q_Tidy,by ="Date") %>%
left_join(select(LR06_WQ_Data_Tidy_Wide,-`Label Date`),by ="Date") %>% 
mutate(`Interpolated TP`=na.approx(`Phosphorus as PO4`, na.rm=FALSE)) %>%   #Interpolate Between Values
mutate(`Interpolated TKN`=na.approx(`Nitrogen, Kjeldahl`, na.rm=FALSE)) %>%
mutate(`Daily P Load (kg)`=`Interpolated TP`*Flow*2446575.5808/1000000) %>%
mutate(`Daily TN Load (kg)`=`Interpolated TKN`*Flow*2446575.5808/1000000) %>%
mutate(`HRT for 1000 cubic ft treatment`=if_else(Flow!=0,1000/Flow,0.000)) %>%
mutate(`HRT for 10000 cubic ft treatment`=if_else(Flow!=0,10000/Flow,0.000)) %>%
mutate(`HRT for 100000 cubic ft treatment`=if_else(Flow!=0,100000/Flow,0.0000)) 


# Figures -----------------------------------------------------------------


#S27 flow histogram
ggplot(S27_Q_Tidy,aes(`Flow`))+geom_histogram(bins=60)+theme_bw()+coord_cartesian(xlim=c(0,1600))+
xlab("Daily Average Flow (cfs)")+scale_x_continuous(breaks=seq(0,1600,200))

ggsave(plot = last_plot(),filename="./Figures/Daily Average Flow of the Little River.jpeg",width =8, height =6, units = "in")


#Flow quantiles
Flow_percents <- S27_Q_Tidy %>% summarise(Percentile = scales::percent(c(0.05,0.10,0.25, 0.5, 0.75,0.95,0.99,1)),`Flow cfs` = quantile(`Flow`, c(0.05,0.10,0.25, 0.5, 0.75,0.95,0.99,1),na.rm=TRUE))

write.csv(Flow_percents, "./Data/Flow_percents.csv",row.names=F)


#Concentrations
ggplot(LR06_WQ_Data_Tidy,aes(Value))+geom_histogram()+facet_wrap(~Characteristic,scales="free")+theme_bw()

TP_percents <- LR06_WQ_Data_Tidy %>% filter(Characteristic=="Phosphorus as PO4") %>% summarise(Percentile = scales::percent(c(0.01,0.05,0.10,0.25, 0.5, 0.75,0.95,0.99,1)),`TP mg/l` = quantile(`Value`, c(0.01,0.05,0.10,0.25, 0.5, 0.75,0.95,0.99,1),na.rm=TRUE))
TN_percents <- LR06_WQ_Data_Tidy %>% filter(Characteristic=="Nitrogen, Kjeldahl") %>% summarise(Percentile = scales::percent(c(0.01,0.05,0.10,0.25, 0.5, 0.75,0.95,0.99,1)),`TN mg/l` = quantile(`Value`, c(0.01,0.05,0.10,0.25, 0.5, 0.75,0.95,0.99,1),na.rm=TRUE))
Concentration_percents <- left_join(TP_percents,TN_percents,by="Percentile")

write.csv(Concentration_percents, "./Data/Concentration_percents.csv",row.names=F)

#Seasonal Trends
ggplot(LR06_WQ_Data_Tidy ,aes(`Label Date`,Value))+geom_point(shape=21,size=2,color="grey70")+
  facet_wrap(~Characteristic,scales="free",nrow=2)+
  scale_x_datetime(date_labels = "%b",date_breaks="month")+
  geom_ribbon(stat='smooth', method = "loess", se=TRUE, alpha=0.3) +theme_bw() +  geom_line(stat='smooth', method = "loess")  + theme(legend.position = "bottom")+
  ylab(expression(mg~L^-1))+xlab("Month")+guides(fill="none")

ggsave(plot = last_plot(),filename="./Figures/Seasonal Trends in TP and TN.jpeg",width =8, height =6, units = "in")

#flow vs TP Concentration 
ggplot(Nutrient_budget_S7,aes(Flow,`Phosphorus as PO4`))+geom_point(shape=21,size=2,fill="blue")+theme_bw()+
coord_cartesian(xlim=c(0,500))+geom_smooth(method = "loess")

ggsave(plot = last_plot(),filename="./Figures/flow vs TP Concentration.jpeg",width =8, height =6, units = "in")

#flow vs TN Concentration 
ggplot(Nutrient_budget_S7,aes(Flow,`Nitrogen, Kjeldahl`))+geom_point(shape=21,size=2,fill="blue")+theme_bw()+
coord_cartesian(xlim=c(0,500))+geom_smooth(method = "loess")

ggsave(plot = last_plot(),filename="./Figures/flow vs TN Concentration.jpeg",width =8, height =6, units = "in")

#Interpolated TP
ggplot(Nutrient_budget_S7,aes(Date,`Interpolated TP`))+geom_line()+theme_bw()

#Interpolated TN
ggplot(Nutrient_budget_S7,aes(Date,`Interpolated TKN`))+geom_line()+theme_bw()

#Seasonal P Load
ggplot(Nutrient_budget_S7 ,aes(`Label Date`,`Daily P Load (kg)`))+geom_point(shape=21,size=2,color="grey70")+
scale_x_datetime(date_labels = "%b",date_breaks="month")+
geom_ribbon(stat='smooth', method = "loess", se=TRUE, alpha=0.3) +theme_bw() +  geom_line(stat='smooth', method = "loess")  + theme(legend.position = "bottom")+
ylab(expression(Phosphorus~(kg)))+xlab("Month")+guides(fill="none")

ggsave(plot = last_plot(),filename="./Figures/Seasonal P Load.jpeg",width =8, height =6, units = "in")

#Seasonal TN Load
ggplot(Nutrient_budget_S7 ,aes(`Label Date`,`Daily TN Load (kg)`))+geom_point(shape=21,size=2,color="grey70")+
scale_x_datetime(date_labels = "%b",date_breaks="month")+
geom_ribbon(stat='smooth', method = "loess", se=TRUE, alpha=0.3) +theme_bw() +  geom_line(stat='smooth', method = "loess")  + theme(legend.position = "bottom")+
ylab(expression(Nitrogen~(kg)))+xlab("Month")+guides(fill="none")

ggsave(plot = last_plot(),filename="./Figures/Seasonal TN Load.jpeg",width =8, height =6, units = "in")

#Nutrient loads summary
Nutrient_load_summary <- Nutrient_budget_S7 %>% summarise(Percentile = scales::percent(c(0.05,0.25, 0.5, 0.75,0.95,0.99,1)),`P (kg)` = quantile(`Daily P Load (kg)`, c(0.05,0.25, 0.5, 0.75,0.95,0.99,1),na.rm=TRUE),
`N (kg)` = quantile(`Daily TN Load (kg)`, c(0.05,0.25, 0.5, 0.75,0.95,0.99,1),na.rm=TRUE))

write.csv(Nutrient_load_summary, "./Data/Nutrient_load_summary.csv",row.names=F)

#HRT vs flow
ggplot(pivot_longer(Nutrient_budget_S7,names_to="HRT",values_to="Seconds",10:12),aes(Flow,Seconds,fill=HRT))+geom_point(shape=21,size=2)+
scale_y_log10(breaks=c(100000,10000,1000,100,10,1),labels = label_comma())+theme_bw()+ theme(legend.position = "bottom")

ggsave(plot = last_plot(),filename="./Figures/HRT vs Flow.jpeg",width =8, height =6, units = "in")

#HRT vs load
ggplot(pivot_longer(Nutrient_budget_S7,names_to="HRT",values_to="Seconds",10:12),aes(`Daily P Load (kg)`,Seconds,fill=HRT))+geom_point(shape=21,size=2)+
scale_y_log10(breaks=c(100000,10000,1000,100,10,1),labels = label_comma())+theme_bw()+ theme(legend.position = "bottom")

ggsave(plot = last_plot(),filename="./Figures/HRT vs P load.jpeg",width =8, height =6, units = "in")


#HRT Histogram
ggplot(pivot_longer(Nutrient_budget_S7,names_to="HRT",values_to="Seconds",10:12),aes(Seconds,fill=HRT))+geom_histogram(bins=60)+theme_bw()+facet_wrap(~HRT,scales="free")+
scale_x_log10(labels = label_comma())+ theme(legend.position = "bottom")

ggsave(plot = last_plot(),filename="./Figures/HRT histogram.jpeg",width =8, height =6, units = "in")

#HRT  summary
HRT_summary <- Nutrient_budget_S7 %>% summarise(Percentile = scales::percent(c(0.05,0.25, 0.5, 0.75,0.95,0.99,1)),`HRT 1000` = quantile(`HRT for 1000 cubic ft treatment`, c(0.05,0.25, 0.5, 0.75,0.95,0.99,1),na.rm=TRUE),
                                                `HRT 10000` = quantile(`HRT for 10000 cubic ft treatment`, c(0.05,0.25, 0.5, 0.75,0.95,0.99,1),na.rm=TRUE),
                                                `HRT 100000` = quantile(`HRT for 100000 cubic ft treatment`, c(0.05,0.25, 0.5, 0.75,0.95,0.99,1),na.rm=TRUE))     

write.csv(HRT_summary, "./Data/HRT_summary.csv",row.names=F)
 
