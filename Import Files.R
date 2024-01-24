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
#Import Data
STA_WQ_Data <- get_wq(raw=T, station_id = c( "S7","S6","G310","G344A","G376B","S362"),date_min = "2010-01-01", date_max = as.character(today()), test_name = c("PHOSPHATE, DISSOLVED AS P", "TOTAL SUSPENDED SOLIDS","PHOSPHATE, TOTAL AS P","PHOSPHATE, ORTHO AS P","TURBIDITY"))



# Tidy Data ---------------------------------------------------------------

STA_WQ_Data_tidy <- STA_WQ_Data %>%
filter(Collection.Method=="G", Sample.Type.New=="SAMP",is.na(Flag)) %>%
mutate(Date=as.Date(dmy_hm(Collection_Date))) %>%
select(Date,Station.ID,Test.Name,Value)


STA_WQ_Data_tidy_Wide <- STA_WQ_Data_tidy%>%
pivot_wider(names_from="Test.Name",values_from="Value",values_fn = mean) %>%
mutate(`Particulate P`=ifelse(is.finite(`PHOSPHATE, TOTAL AS P`) & is.finite(`PHOSPHATE, DISSOLVED AS P`),`PHOSPHATE, TOTAL AS P`-`PHOSPHATE, DISSOLVED AS P`,NA)) %>%
mutate(`PP/OP Ratio`=ifelse(is.finite(`PHOSPHATE, ORTHO AS P`) & is.finite(`Particulate P`),`Particulate P`/`PHOSPHATE, ORTHO AS P`,NA)) 




# Create Models -----------------------------------------------------------

G310_Mod <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `PHOSPHATE, ORTHO AS P`+ `TOTAL SUSPENDED SOLIDS`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="G310" ))
S7_Mod <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `PHOSPHATE, ORTHO AS P`+ `TOTAL SUSPENDED SOLIDS`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="S7" ))
S6_Mod <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `PHOSPHATE, ORTHO AS P`+ `TOTAL SUSPENDED SOLIDS`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="S6" ))
G344A_Mod <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `PHOSPHATE, ORTHO AS P`+ `TOTAL SUSPENDED SOLIDS`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="G344A" ))
G376B_Mod <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `PHOSPHATE, ORTHO AS P`+ `TOTAL SUSPENDED SOLIDS`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="G376B" ))
S362_Mod <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `PHOSPHATE, ORTHO AS P`+ `TOTAL SUSPENDED SOLIDS`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="S362" ))
G310_Mod_OP <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `PHOSPHATE, ORTHO AS P`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="G310" ))
S7_Mod_OP <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `PHOSPHATE, ORTHO AS P`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="S7" ))
S6_Mod_OP <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `PHOSPHATE, ORTHO AS P`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="S6" ))
S362_Mod_OP <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `PHOSPHATE, ORTHO AS P`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="S362" ))
G344A_Mod_OP <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `PHOSPHATE, ORTHO AS P`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="G344A" ))
G376B_Mod_OP <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `PHOSPHATE, ORTHO AS P`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="G376B" ))
G310_Mod_TSS <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `TOTAL SUSPENDED SOLIDS`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="G310" ))
S7_Mod_TSS <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `TOTAL SUSPENDED SOLIDS`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="S7" ))
S362_Mod_TSS <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `TOTAL SUSPENDED SOLIDS`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="S362" ))
S6_Mod_TSS <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `TOTAL SUSPENDED SOLIDS`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="S6" ))
G344A_Mod_TSS <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `TOTAL SUSPENDED SOLIDS`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="G344A" ))
G376B_Mod_TSS <-  lm(`PHOSPHATE, TOTAL AS P`  ~ `TOTAL SUSPENDED SOLIDS`, filter(STA_WQ_Data_tidy_Wide,Station.ID=="G376B" ))


Model_fit <- mutate(glance(G310_Mod),Model="G310 TP~OP+TSS")  %>%
bind_rows(mutate(glance(S7_Mod),Model="S7 TP~OP+TSS"))  %>%
bind_rows(mutate(glance(S6_Mod),Model="S6 TP~OP+TSS")) %>%
  bind_rows(mutate(glance(G344A_Mod),Model="G344A TP~OP+TSS")) %>%
  bind_rows(mutate(glance(G376B_Mod),Model="G376B TP~OP+TSS")) %>%
  bind_rows(mutate(glance(S362_Mod),Model="S362 TP~OP+TSS")) %>%
bind_rows(mutate(glance(S6_Mod_OP),Model="S6 TP~OP")) %>%
bind_rows(mutate(glance(S7_Mod_OP),Model="S7 TP~OP")) %>%
bind_rows(mutate(glance(G310_Mod_OP),Model="G310 TP~OP")) %>%
  bind_rows(mutate(glance(G344A_Mod_OP),Model="G344A TP~OP")) %>%
  bind_rows(mutate(glance(G376B_Mod_OP),Model="G376B TP~OP")) %>%
  bind_rows(mutate(glance(S362_Mod_OP),Model="S362 TP~OP")) %>%
bind_rows(mutate(glance(G310_Mod_TSS),Model="G310 TP~TSS")) %>%
bind_rows(mutate(glance(S7_Mod_TSS),Model="S7 TP~TSS")) %>%
bind_rows(mutate(glance(S6_Mod_TSS),Model="S6 TP~TSS")) %>%
  bind_rows(mutate(glance(G344A_Mod_TSS),Model="G344A TP~TSS")) %>%
  bind_rows(mutate(glance(G376B_Mod_TSS),Model="G376B TP~TSS")) %>%
  bind_rows(mutate(glance(S362_Mod_TSS),Model="S362 TP~TSS")) 

Model_residuals <-mutate(augment(G310_Mod),Model="G310 TP~OP+TSS") %>%
bind_rows(mutate(augment(S7_Mod),Model="S7 TP~OP+TSS"))  %>%
bind_rows(mutate(augment(S6_Mod),Model="S6 TP~OP+TSS"))  %>%
  bind_rows(mutate(augment(G344A_Mod),Model="G344A TP~OP+TSS"))  %>%
  bind_rows(mutate(augment(G376B_Mod),Model="G376B TP~OP+TSS"))  %>%
  bind_rows(mutate(augment(S362_Mod),Model="S362 TP~OP+TSS"))  %>%
bind_rows(mutate(augment(S6_Mod_OP),Model="S6 TP~OP")) %>%
bind_rows(mutate(augment(S7_Mod_OP),Model="S7 TP~OP")) %>%
bind_rows(mutate(augment(G310_Mod_OP),Model="G310 TP~OP")) %>%
  bind_rows(mutate(augment(G344A_Mod_OP),Model="G344A TP~OP")) %>%
  bind_rows(mutate(augment(G376B_Mod_OP),Model="G376B TP~OP")) %>%
  bind_rows(mutate(augment(S362_Mod_OP),Model="G362 TP~OP")) %>%
bind_rows(mutate(augment(G310_Mod_TSS),Model="G310 TP~TSS")) %>%
bind_rows(mutate(augment(S7_Mod_TSS),Model="S7 TP~TSS")) %>%
bind_rows(mutate(augment(S6_Mod_TSS),Model="S6 TP~TSS")) %>%
  bind_rows(mutate(augment(G344A_Mod_TSS),Model="G344A TP~TSS")) %>%
  bind_rows(mutate(augment(G376B_Mod_TSS),Model="G376B TP~TSS")) %>%
  bind_rows(mutate(augment(S362_Mod_TSS),Model="S362 TP~TSS")) 

# Visualize ---------------------------------------------------------------
#Parameters over time
ggplot(STA_WQ_Data_tidy,aes(Date,Value))+geom_point(shape=21,size=2,color="grey70")+
facet_wrap(~Test.Name,scales="free")+
geom_ribbon(stat='smooth', method = "loess", se=TRUE, alpha=0.3) +
geom_line(stat='smooth', method = "loess")  + 
ylab(expression(TP~(mu~g~L^-1)))+xlab("Outflow (cfs)")+guides(fill="none",color="none")

#TSS vs Turbidity
ggplot(STA_WQ_Data_tidy_Wide,aes(`TOTAL SUSPENDED SOLIDS`,`TURBIDITY`))+geom_point(shape=21,size=3,fill="violet")+scale_x_continuous(limits=c(-4,50))

#Model validation
#Predicted vs fitted
ggplot(Model_residuals,aes(.fitted,`PHOSPHATE, TOTAL AS P`))+geom_point(shape=21,size=2)+geom_smooth(method="lm")+facet_wrap(~Model,scales="free",ncol=3)+theme_bw()+
stat_poly_eq(formula = y~x, aes(label = paste( ..rr.label..),color="red"),parse = TRUE)+labs(x= expression(Predicted~TP~mu~L^-1),y=expression(Measured~TP~mu~L^-1), title = "Model Fit")


#Residuals
ggplot(Model_residuals,aes(`PHOSPHATE, TOTAL AS P`,.resid))+geom_point(shape=21,size=2)+facet_wrap(~Model,scales="free",ncol=3)+theme_bw()+
labs(x= expression(Predicted~TP~mu~L^-1),y=expression(Measured~TP~mu~L^-1), title = "Model Residuals")
