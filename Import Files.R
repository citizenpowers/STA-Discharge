#the object of this project is to see if Total phosphorus can be modeled by analytes that can be measured using sensors. 
# this script will import data


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

#Import Data discharge data
STA_WQ_Discharge_Data <- get_wq(raw=T, station_id = c("G436","G310","G344A","S7","S362"),date_min = "2010-01-01", date_max = "2023-12-31", test_name = c("PHOSPHATE, DISSOLVED AS P", "TOTAL SUSPENDED SOLIDS","PHOSPHATE, TOTAL AS P","PHOSPHATE, ORTHO AS P","TURBIDITY"))

#Import STA inflow data
STA_WQ_Inflow_Data <- get_wq(raw=T, station_id = c( "S6","S319","G302","G370","G342A"),date_min = "2010-01-01", date_max = "2023-12-31", test_name = c("PHOSPHATE, DISSOLVED AS P", "TOTAL SUSPENDED SOLIDS","PHOSPHATE, TOTAL AS P","PHOSPHATE, ORTHO AS P","TURBIDITY"))

# Tidy Data ---------------------------------------------------------------

STA_WQ_Data_tidy <- mutate(STA_WQ_Discharge_Data,Position="Discharge") %>%
bind_rows(mutate(STA_WQ_Inflow_Data,Position="Inflow")) %>%
filter(Collection.Method=="G", Sample.Type.New=="SAMP",is.na(Flag)) %>%
mutate(Date=as.Date(dmy_hm(Collection_Date))) %>%
mutate(STA=case_when(Station.ID %in% c("S319","S362")~"STA-1E",
                     Station.ID %in% c("G302","G310")~"STA-1W",
                     Station.ID %in% c("S6","G436")~"STA-2",
                     Station.ID %in% c("S7","G370")~"STA-3/4",
                     Station.ID %in% c("G342A","G344A")~"STA-5/6")) %>%
select(STA,Position,Station.ID,Date,Test.Name,Value) 


STA_WQ_Data_tidy_Wide <- STA_WQ_Data_tidy%>%
pivot_wider(names_from="Test.Name",values_from="Value",values_fn = mean) %>%
rowwise() %>%
mutate(`PHOSPHATE, PARTICULATE`=ifelse(is.numeric(`PHOSPHATE, TOTAL AS P`) & is.numeric(`PHOSPHATE, DISSOLVED AS P`),`PHOSPHATE, TOTAL AS P`-`PHOSPHATE, DISSOLVED AS P`,NA)) %>%
mutate(`PHOSPHATE, DISSOVED ORGANIC`=ifelse(is.numeric(`PHOSPHATE, ORTHO AS P`) & is.numeric(`PHOSPHATE, DISSOLVED AS P`),`PHOSPHATE, DISSOLVED AS P`-`PHOSPHATE, ORTHO AS P`,NA)) %>%
mutate(`OP/TP Ratio`=ifelse(is.numeric(`PHOSPHATE, ORTHO AS P`) & is.numeric(`PHOSPHATE, TOTAL AS P`),`PHOSPHATE, ORTHO AS P`/`PHOSPHATE, TOTAL AS P`,NA)) %>%
mutate(`DOP/TP Ratio`=ifelse(is.numeric(`PHOSPHATE, DISSOVED ORGANIC`) & is.numeric(`PHOSPHATE, TOTAL AS P`),`PHOSPHATE, DISSOVED ORGANIC`/`PHOSPHATE, TOTAL AS P`,NA)) %>%
mutate(`PP/TP Ratio`=ifelse(is.numeric(`PHOSPHATE, PARTICULATE`) & is.numeric(`PHOSPHATE, TOTAL AS P`),`PHOSPHATE, PARTICULATE`/`PHOSPHATE, TOTAL AS P`,NA)) 

P_species <- STA_WQ_Data_tidy_Wide %>% 
pivot_longer(names_to="Test.Name",values_to="Value",5:11) %>%
filter(str_detect(Test.Name,"PHOSPHATE"))%>%
filter(Test.Name !="PHOSPHATE, TOTAL AS P") %>%
mutate(Month=month(Date,label = T, abbr = TRUE),`Label Date`=ISOdate(2000,month(Date),mday(Date)))


# Create Models -----------------------------------------------------------

G310_Mod <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `PHOSPHATE, ORTHO AS P`+ `TOTAL SUSPENDED SOLIDS`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="G310" ))
S7_Mod <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `PHOSPHATE, ORTHO AS P`+ `TOTAL SUSPENDED SOLIDS`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="S7" ))
G436_Mod <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `PHOSPHATE, ORTHO AS P`+ `TOTAL SUSPENDED SOLIDS`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="G436" ))
G344A_Mod <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `PHOSPHATE, ORTHO AS P`+ `TOTAL SUSPENDED SOLIDS`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="G344A" ))
S362_Mod <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `PHOSPHATE, ORTHO AS P`+ `TOTAL SUSPENDED SOLIDS`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="S362" ))

G310_Mod_OP <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `PHOSPHATE, ORTHO AS P`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="G310" ))
S7_Mod_OP <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `PHOSPHATE, ORTHO AS P`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="S7" ))
G436_Mod_OP <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `PHOSPHATE, ORTHO AS P`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="G436" ))
S362_Mod_OP <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `PHOSPHATE, ORTHO AS P`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="S362" ))
G344A_Mod_OP <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `PHOSPHATE, ORTHO AS P`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="G344A" ))

G310_Mod_TSS <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `TOTAL SUSPENDED SOLIDS`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="G310" ))
S7_Mod_TSS <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `TOTAL SUSPENDED SOLIDS`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="S7" ))
S362_Mod_TSS <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `TOTAL SUSPENDED SOLIDS`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="S362" ))
G436_Mod_TSS <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `TOTAL SUSPENDED SOLIDS`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="G436" ))
G344A_Mod_TSS <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `TOTAL SUSPENDED SOLIDS`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="G344A" ))

G302_Mod <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `PHOSPHATE, ORTHO AS P`+ `TOTAL SUSPENDED SOLIDS`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="G302" ))
S319_Mod <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `PHOSPHATE, ORTHO AS P`+ `TOTAL SUSPENDED SOLIDS`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="S319" ))
S6_Mod <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `PHOSPHATE, ORTHO AS P`+ `TOTAL SUSPENDED SOLIDS`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="S6" ))
G370_Mod <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `PHOSPHATE, ORTHO AS P`+ `TOTAL SUSPENDED SOLIDS`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="G370" ))
G342A_Mod <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `PHOSPHATE, ORTHO AS P`+ `TOTAL SUSPENDED SOLIDS`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="G342A" ))

G302_Mod_OP <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `PHOSPHATE, ORTHO AS P`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="G302" ))
S319_Mod_OP <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `PHOSPHATE, ORTHO AS P`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="S319" ))
S6_Mod_OP <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `PHOSPHATE, ORTHO AS P`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="S6" ))
G370_Mod_OP <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `PHOSPHATE, ORTHO AS P`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="G370" ))
G342A_Mod_OP <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `PHOSPHATE, ORTHO AS P`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="G342A" ))

G302_Mod_TSS <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `TOTAL SUSPENDED SOLIDS`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="G302" ))
S319_Mod_TSS <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `TOTAL SUSPENDED SOLIDS`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="S319" ))
S6_Mod_TSS <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `TOTAL SUSPENDED SOLIDS`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="S6" ))
G370_Mod_TSS <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `TOTAL SUSPENDED SOLIDS`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="G370" ))
G342A_Mod_TSS <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `TOTAL SUSPENDED SOLIDS`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="G342A" ))

Model_fit <- mutate(glance(G310_Mod),Model="G310 TP~OP+TSS",Position="Discharge")  %>%
bind_rows(mutate(glance(S7_Mod),Model="S7 TP~OP+TSS",Position="Discharge"))  %>%
bind_rows(mutate(glance(G436_Mod),Model="G436 TP~OP+TSS",Position="Discharge")) %>%
bind_rows(mutate(glance(G344A_Mod),Model="G344A TP~OP+TSS",Position="Discharge")) %>%
bind_rows(mutate(glance(S362_Mod),Model="S362 TP~OP+TSS",Position="Discharge")) %>%
  
bind_rows(mutate(glance(G436_Mod_OP),Model="G436 TP~OP",Position="Discharge")) %>%
bind_rows(mutate(glance(S7_Mod_OP),Model="S7 TP~OP",Position="Discharge")) %>%
bind_rows(mutate(glance(G310_Mod_OP),Model="G310 TP~OP",Position="Discharge")) %>%
bind_rows(mutate(glance(G344A_Mod_OP),Model="G344A TP~OP",Position="Discharge")) %>%
bind_rows(mutate(glance(S362_Mod_OP),Model="S362 TP~OP",Position="Discharge")) %>%
  
bind_rows(mutate(glance(G310_Mod_TSS),Model="G310 TP~TSS",Position="Discharge")) %>%
bind_rows(mutate(glance(S7_Mod_TSS),Model="S7 TP~TSS",Position="Discharge")) %>%
bind_rows(mutate(glance(G436_Mod_TSS),Model="G436 TP~TSS",Position="Discharge")) %>%
bind_rows(mutate(glance(G344A_Mod_TSS),Model="G344A TP~TSS",Position="Discharge")) %>%
bind_rows(mutate(glance(S362_Mod_TSS),Model="S362 TP~TSS",Position="Discharge")) %>%
  
bind_rows(mutate(glance(G302_Mod),Model="G302 TP~OP+TSS",Position="Inflow"))  %>%
bind_rows(mutate(glance(G370_Mod),Model="G370 TP~OP+TSS",Position="Inflow"))  %>%
bind_rows(mutate(glance(S6_Mod),Model="S6 TP~OP+TSS",Position="Inflow")) %>%
bind_rows(mutate(glance(G342A_Mod),Model="G342A TP~OP+TSS",Position="Inflow")) %>%
bind_rows(mutate(glance(S319_Mod),Model="S319 TP~OP+TSS",Position="Inflow")) %>%
  
bind_rows(mutate(glance(S6_Mod_OP),Model="S6 TP~OP",Position="Inflow")) %>%
bind_rows(mutate(glance(G370_Mod_OP),Model="G370 TP~OP",Position="Inflow")) %>%
bind_rows(mutate(glance(G302_Mod_OP),Model="G302 TP~OP",Position="Inflow")) %>%
bind_rows(mutate(glance(G342A_Mod_OP),Model="G342A TP~OP",Position="Inflow")) %>%
bind_rows(mutate(glance(S319_Mod_OP),Model="S319 TP~OP",Position="Inflow")) %>%
  
bind_rows(mutate(glance(G302_Mod_TSS),Model="G302 TP~TSS",Position="Inflow")) %>%
bind_rows(mutate(glance(G370_Mod_TSS),Model="G370 TP~TSS",Position="Inflow")) %>%
bind_rows(mutate(glance(S6_Mod_TSS),Model="S6 TP~TSS",Position="Inflow")) %>%
bind_rows(mutate(glance(G342A_Mod_TSS),Model="G342A TP~TSS",Position="Inflow")) %>%
bind_rows(mutate(glance(S319_Mod_TSS),Model="S319 TP~TSS",Position="Inflow")) %>%  
mutate(across(Model, ~factor(., levels=c("G310 TP~TSS","G310 TP~OP","G310 TP~OP+TSS","S362 TP~TSS","S362 TP~OP","S362 TP~OP+TSS","G436 TP~TSS","G436 TP~OP","G436 TP~OP+TSS","S7 TP~TSS","S7 TP~OP","S7 TP~OP+TSS","G344A TP~TSS","G344A TP~OP","G344A TP~OP+TSS",
"G302 TP~TSS","G302 TP~OP","G302 TP~OP+TSS","G342A TP~TSS","G342A TP~OP","G342A TP~OP+TSS","S319 TP~TSS","S319 TP~OP","S319 TP~OP+TSS","G370 TP~TSS","G370 TP~OP","G370 TP~OP+TSS","S6 TP~TSS","S6 TP~OP","S6 TP~OP+TSS"))))

write.csv(Model_fit %>% arrange(desc(r.squared)), "./Data/Model_fit.csv")

glance.lm(G370_Mod)


Model_residuals <-mutate(augment(G310_Mod),Model="G310 TP~OP+TSS",Position="Discharge") %>%
bind_rows(mutate(augment(S7_Mod),Model="S7 TP~OP+TSS",Position="Discharge"))  %>%
bind_rows(mutate(augment(G436_Mod),Model="G436 TP~OP+TSS",Position="Discharge"))  %>%
bind_rows(mutate(augment(G344A_Mod),Model="G344A TP~OP+TSS",Position="Discharge"))  %>%
bind_rows(mutate(augment(S362_Mod),Model="S362 TP~OP+TSS",Position="Discharge"))  %>%
  
bind_rows(mutate(augment(G436_Mod_OP),Model="G436 TP~OP",Position="Discharge")) %>%
bind_rows(mutate(augment(S7_Mod_OP),Model="S7 TP~OP",Position="Discharge")) %>%
bind_rows(mutate(augment(G310_Mod_OP),Model="G310 TP~OP",Position="Discharge")) %>%
bind_rows(mutate(augment(G344A_Mod_OP),Model="G344A TP~OP",Position="Discharge")) %>%
bind_rows(mutate(augment(S362_Mod_OP),Model="S362 TP~OP",Position="Discharge")) %>%
  
bind_rows(mutate(augment(G310_Mod_TSS),Model="G310 TP~TSS",Position="Discharge")) %>%
bind_rows(mutate(augment(S7_Mod_TSS),Model="S7 TP~TSS",Position="Discharge")) %>%
bind_rows(mutate(augment(G436_Mod_TSS),Model="G436 TP~TSS",Position="Discharge")) %>%
bind_rows(mutate(augment(G344A_Mod_TSS),Model="G344A TP~TSS",Position="Discharge")) %>%
bind_rows(mutate(augment(S362_Mod_TSS),Model="S362 TP~TSS",Position="Discharge")) %>%

bind_rows(mutate(augment(G302_Mod),Model="G302 TP~OP+TSS",Position="Inflow")) %>%
bind_rows(mutate(augment(G370_Mod),Model="G370 TP~OP+TSS",Position="Inflow"))  %>%
bind_rows(mutate(augment(S6_Mod),Model="S6 TP~OP+TSS",Position="Inflow"))  %>%
bind_rows(mutate(augment(G342A_Mod),Model="G342A TP~OP+TSS",Position="Inflow"))  %>%
bind_rows(mutate(augment(S319_Mod),Model="S319 TP~OP+TSS",Position="Inflow"))  %>%
  
bind_rows(mutate(augment(G302_Mod_OP),Model="G302 TP~OP",Position="Inflow")) %>%
bind_rows(mutate(augment(G370_Mod_OP),Model="G370 TP~OP",Position="Inflow")) %>%
bind_rows(mutate(augment(S6_Mod_OP),Model="S6 TP~OP",Position="Inflow")) %>%
bind_rows(mutate(augment(G342A_Mod_OP),Model="G342A TP~OP",Position="Inflow")) %>%
bind_rows(mutate(augment(S319_Mod_OP),Model="S319 TP~OP",Position="Inflow")) %>%
 
bind_rows(mutate(augment(G302_Mod_TSS),Model="G302 TP~TSS",Position="Inflow")) %>%
bind_rows(mutate(augment(G370_Mod_TSS),Model="G370 TP~TSS",Position="Inflow")) %>%
bind_rows(mutate(augment(S6_Mod_TSS),Model="S6 TP~TSS",Position="Inflow")) %>%
bind_rows(mutate(augment(G342A_Mod_TSS),Model="G342A TP~TSS",Position="Inflow")) %>%
bind_rows(mutate(augment(S319_Mod_TSS),Model="S319 TP~TSS",Position="Inflow")) 


Only_OP_TSS_Mods <- Model_fit %>%
filter(str_detect(Model,"OP\\+\\TSS")) %>%
mutate(Station.ID=str_sub(Model,start=1,end=str_locate(Model,"TP")[,1]-2))

write.csv(Only_OP_TSS_Mods %>% arrange((sigma)), "./Data/Only_OP_TSS_Mods.csv")


Ratio_Summary <- STA_WQ_Data_tidy_Wide %>%
group_by(Station.ID) %>%
summarise(`Mean DOP/TP Ratio`=mean(`DOP/TP Ratio`,na.rm=T),`Standard Deviation DOP/TP Ratio`=sd(`DOP/TP Ratio`,na.rm=T),
          `Mean OP/TP Ratio`=mean(`OP/TP Ratio`,na.rm=T),`Standard Deviation OP/TP Ratio`=sd(`OP/TP Ratio`,na.rm=T),
          `Mean PP/TP Ratio`=mean(`PP/TP Ratio`,na.rm=T),`Standard Deviation PP/TP Ratio`=sd(`PP/TP Ratio`,na.rm=T))  %>%
right_join(Only_OP_TSS_Mods, by="Station.ID")


# Visualize ---------------------------------------------------------------
#Parameters over time
ggplot(STA_WQ_Data_tidy,aes(Date,Value))+geom_point(shape=21,size=2,color="grey70")+
facet_wrap(~Test.Name,scales="free")+
geom_ribbon(stat='smooth', method = "loess", se=TRUE, alpha=0.3) +
geom_line(stat='smooth', method = "loess")  + 
ylab(expression(TP~(mu~g~L^-1)))+xlab("Outflow (cfs)")+guides(fill="none",color="none")

#P species by month
#STA label position
dat_text <- data.frame(
  label = c("G436","G310","G344A","S7","S362","S6","S319","G302","G370","G342A"),
  Position   = c("Discharge","Discharge","Discharge","Discharge","Discharge","Inflow","Inflow","Inflow","Inflow","Inflow"),
  STA  =c("STA-2","STA-1W","STA-5/6","STA-3/4","STA-1E","STA-2","STA-1E","STA-1W","STA-3/4","STA-5/6"))

ggplot(filter(P_species,Test.Name!="PHOSPHATE, DISSOLVED AS P"),aes(`Label Date`,Value*1000,color=Test.Name))+geom_point(shape=21,size=2,alpha=.5)+
facet_grid(fct_relevel(Position,"Inflow","Discharge")~STA,scales="free")+coord_cartesian(ylim=c(0,100))+
scale_x_datetime(date_labels = "%b",date_breaks="month")+scale_y_continuous( breaks=pretty_breaks(n=10))+
geom_ribbon(stat='smooth', method = "loess", se=TRUE, alpha=0.3) +theme_bw() +  geom_line(stat='smooth', method = "loess")  + theme(legend.position = "bottom")+
ylab(expression(P~(mu~g~L^-1)))+xlab("Month")+guides(fill="none")+
geom_text(data= dat_text,mapping = aes(x = as.POSIXct("2000-01-01 12:00:00"), y = Inf, label = label),hjust   = -0.1,vjust   = 2,color="black")

ggsave(plot = last_plot(),filename="./Figures/Seasonal Trends in P Species.jpeg",width =16, height =8, units = "in")


#TSS vs Turbidity
ggplot(STA_WQ_Data_tidy_Wide,aes(`TOTAL SUSPENDED SOLIDS`,`PHOSPHATE, PARTICULATE`))+geom_point(shape=21,size=3,fill="violet")+scale_x_continuous(limits=c(-4,50))+
facet_wrap(~Station.ID,scales="free")+ geom_smooth(method="lm")+stat_poly_eq(formula = y~x, aes(label = paste( ..rr.label..),color="red"),parse = TRUE)+theme_bw()

#Model validation
#Predicted vs fitted
ggplot(Model_residuals,aes(.fitted,`PHOSPHATE, TOTAL AS P`))+geom_point(shape=21,size=2)+geom_smooth(method="lm")+facet_wrap(~Model,scales="free",ncol=3)+theme_bw()+
stat_poly_eq(formula = y~x, aes(label = paste( ..rr.label..),color="red"),parse = TRUE)+labs(x= expression(Predicted~TP~mu~L^-1),y=expression(Measured~TP~mu~L^-1), title = "Model Fit")

ggsave(plot = last_plot(),filename="./Figures/Fitted vs Predicted.jpeg",width =8, height =16, units = "in")

#Residuals
ggplot(Model_residuals,aes(`PHOSPHATE, TOTAL AS P`,.resid))+geom_point(shape=21,size=2)+facet_wrap(~Model,scales="free",ncol=3)+theme_bw()+
labs(x= expression(Measured~TP~mu~L^-1),y=expression(Residuals~TP~mu~L^-1), title = "Model Residuals")

ggsave(plot = last_plot(),filename="./Figures/All model residuals.jpeg",width =8, height =11.5, units = "in")


#DOP to TP Ratio
ggplot(Ratio_Summary,aes(`Mean DOP/TP Ratio`,`r.squared`,label=Station.ID,fill=Position))+geom_point(shape=21,size=2)+theme_bw()+geom_label_repel(fill="white")

ggsave(plot = last_plot(),filename="./Figures/Mean DOP to TP Ratio.jpeg",width =8, height =6, units = "in")

#SD of DOP to TP Ratio
ggplot(Ratio_Summary,aes(`Standard Deviation DOP/TP Ratio`,`r.squared`,label=Station.ID,fill=Position))+geom_point(shape=21,size=2)+theme_bw()+geom_label_repel(fill="white")

ggsave(plot = last_plot(),filename="./Figures/SD DOP to TP Ratio.jpeg",width =8, height =6, units = "in")

#OP to TP Ratio
ggplot(Ratio_Summary,aes(`Mean OP/TP Ratio`,`r.squared`,label=Station.ID,fill=Position))+geom_point(shape=21,size=2)+theme_bw()+geom_label_repel(fill="white")

ggsave(plot = last_plot(),filename="./Figures/Mean OP to TP Ratio.jpeg",width =8, height =6, units = "in")

#SD of OP to TP Ratio
ggplot(Ratio_Summary,aes(`Standard Deviation OP/TP Ratio`,`r.squared`,label=Station.ID,fill=Position))+geom_point(shape=21,size=2)+theme_bw()+geom_label_repel(fill="white")

ggsave(plot = last_plot(),filename="./Figures/SD OP to TP Ratio.jpeg",width =8, height =6, units = "in")

#PP to TP Ratio
ggplot(Ratio_Summary,aes(`Mean PP/TP Ratio`,`r.squared`,label=Station.ID,fill=Position))+geom_point(shape=21,size=2)+theme_bw()+geom_label_repel(fill="white")

ggsave(plot = last_plot(),filename="./Figures/Mean PP to TP Ratio.jpeg",width =8, height =6, units = "in")

#SD of PP to TP Ratio
ggplot(Ratio_Summary,aes(`Standard Deviation PP/TP Ratio`,`r.squared`,label=Station.ID,fill=Position))+geom_point(shape=21,size=2)+theme_bw()+geom_label_repel(fill="white")

ggsave(plot = last_plot(),filename="./Figures/SD PP to TP Ratio.jpeg",width =8, height =6, units = "in")

