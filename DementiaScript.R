dementia <- read.csv("your path for hlth_cd_asdr2_1_Data.csv ",skip=0,stringsAsFactors = FALSE)
dementia$Value = as.numeric(as.character(dementia$Value))
dementia$Flag.and.Footnotes <- NULL
dementia$UNIT <- NULL
dementia$ICD10 <- NULL


dementia[dementia$GEO=='Germany (until 1990 former territory of the FRG)',]$GEO = as.character('Germany')
dementia[dementia$GEO=='European Union - 28 countries (2013-2020)',]$GEO = as.character('EU(2013-2020)')
dementia[dementia$GEO=='Czechia',]$GEO = as.character('Czech Rep.')


library(rgeos)
library(data.table)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggridges)
library(viridis)
library(hrbrthemes)
dementia2 = subset(dementia,  GEO == "Greece" & SEX== "Total" & AGE == "Total")

dementia3 = subset(dementia,  GEO == "Greece" & SEX== "Total")

dementiaTotals = subset(dementia,SEX == "Total" & AGE == "Total")
dementiaTotals = subset(dementiaTotals,GEO != "Poland" & GEO != "Romania" & GEO != "Turkey" & GEO != "Liechtenstein" & GEO != "Slovenia" & GEO != "Bulgaria")

LessThan65 = subset(dementia, SEX== "Total" & AGE == "Less than 65 years"&
                      (GEO == "Greece" | GEO == "Italy" | GEO == "Portugal" | GEO == "Spain" | GEO == "Cyprus" |
                         GEO == "Austria" | GEO == "Greece" | GEO == "Croatia" | GEO == "Czechia" | GEO == "Greece"))

Male_Female = subset(dementia, GEO == "Greece" & (SEX=="Males" | SEX=="Females") &
AGE == "Total")

BoxplotChart = subset(dementia, SEX=="Total" & AGE == "65 years or over" & +
(GEO == "Cyprus"|GEO =="Greece"|GEO =="Italy"|GEO =="Portugal"|GEO =="Spain"|
   GEO == "Austria" | GEO == "Greece" | GEO == "Croatia" | GEO == "Czechia"))

DumbellChart = subset(dementia, SEX=="Total" & AGE == "Total" & (TIME == 2011 | TIME == 2019) &
(GEO == "Greece" | GEO == "Italy" | GEO == "Portugal" | GEO == "Spain" |GEO == "Austria" | 
   GEO == "Greece" | GEO == "Croatia" | GEO == "Czechia"| GEO == 'Germany' | GEO == 'Belgium'|
   GEO == 'Denmark'| GEO == "Hungary" ))

EU_MEAN_VALUES = subset(dementia, SEX=="Total" & AGE == "Total" & 
(GEO == "Greece"| GEO == "Italy" | GEO == "Portugal" | GEO == "Spain" | GEO == "Cyprus" |
   GEO == "Austria"| GEO == "Croatia" | GEO == "Czechia"))

ValuesOf2019= subset(dementia,SEX=="Total" & AGE == "Total" & TIME=='2019' & Value!=0
                     &(GEO!="Norway" & GEO!="Switzerland" &GEO!="Norway" &
                       GEO!="Iceland" &GEO!="Serbia" &GEO!="Liechtenstein" &GEO!="Turkey"))

Under_Over_65 = subset(dementia,SEX=='Total' & GEO =='Greece' & AGE!='Total')


#### Plot 1

png(filename="1.png", res=135,width=1440  , height=810)

ggplot(dementia2,aes(TIME,Value))+geom_point(color = "#0099f9",size = 3.5)+ geom_line(linetype = "dashed",color = "#0099f9",size = 1.3)+
  theme(
    plot.title = element_text(color = "#0099f9", size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.caption = element_text(face = "italic"))+
  labs(
    #title = "Dementia Death Rate Trendline in Greece",
    subtitle = "Data from 2011 to 2019",
    caption = "Source: Eurostat"
  )+
  geom_label(aes(label = Value),
             nudge_x = 0,
             nudge_y = 0.5,
             size = 4)+
  scale_x_continuous(breaks = dementia2$TIME)+
  scale_y_continuous(breaks = c(0:14))+
  labs(y="Rate",x='Year')

dev.off()

####  Plot 2

png(filename="2.png", res=135,width=1440  , height=810)

ggplot(BoxplotChart, aes(Value,GEO))+
geom_boxplot(aes(GEO,Value, fill = GEO),alpha=0.5)+
  theme(
    plot.title = element_text(color = "#636363", size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.caption = element_text(face = "italic"))+
  labs(
    #title = "Dementia Death Rate Distribution of Data",
    subtitle = "Similar Demographics to Greece (2011 to 2019)",
    caption = "Source: Eurostat"
  )+
  geom_hline(aes(yintercept = mean(count)))+
labs(y= "Rate", x = "Country",caption="Source: Eurostat")
  
dev.off()

####  Plot 3


df<-as.data.frame(DumbellChart)

df = df[order(df$GEO),]

df %>%
  group_by(TIME) %>%
  summarise(mean = mean(Value),
            SE = sd(Value)) %>%
  mutate(meanpos = mean + 1 *SE,
         meanneg = mean - 1 *SE)-> stats
stats_2011 <- stats %>%
  filter(TIME == "2011")
stats_2019 <- stats %>%
  filter(TIME == "2019")
head(stats)
       
df$paired = rep(1:(20/2),each=2)

library(ggtext)

png(filename="3.png", res=135,width=1440  , height=810)

ggplot(df,aes(x= Value, y= reorder(GEO,log(Value))))+
  geom_line(aes(group = paired),color="grey")+
  geom_point(aes(color=factor(TIME)), size=5) +
  scale_color_manual(values = c("#762a83","#009688"))+
  labs(y="Country",x="Rate")+
  labs(
    #title = "Dementia Death Rate in EU Countries",
    subtitle = "<span style = 'color: #762a83;'>**2011**</span> to <span style = 'color: #009688;'>**2019**</span>",
    caption = "Source: Eurostat",
    color = "")+
  geom_vline(xintercept = stats_2011$mean, linetype = "solid", size = .5, alpha = .8, color = "#762a83")+
  geom_vline(xintercept = stats_2019$mean, color = "#009688", linetype = "solid",  size = .5, alpha = .8)+
  geom_text(x = stats_2011$mean -2, y = 'Denmark', label = "MEAN", angle = 90, size = 3.5, color = "#762a83")+
  geom_text(x = stats_2019$meanpos - 16, y = 'Denmark', label = "MEAN", angle = 90, size = 3.5, color = "#009688")+
  theme_minimal()+
  theme(plot.subtitle = element_markdown(size = 14, hjust = .5),
        plot.title = element_markdown(size = 14,hjust =.5),
        plot.caption = element_text(face = "italic"))

dev.off()  

####  Plot 4


world_map <- ne_countries(scale = 50, returnclass = 'sf')

Test=subset(dementia,subset= TIME==2017 & AGE=='Total' & SEX=='Total')

european_union <- c("Albania","Austria","Belgium","Bulgaria","Croatia","Cyprus",
                    "Czech Rep.","Denmark","Estonia","Finland","France",
                    "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                    "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                    "Portugal","Romania","Slovakia","Slovenia","Spain",
                    "Sweden","United Kingdom","Switzerland","Montenegro",
                    "Serbia","Bosnia and Herz.","Macedonia","Kosovo","Norway","Turkey")

european_union_map <- 
  world_map %>% 
  filter(name %in% european_union)



bbox_europe <- st_bbox(c(xmin = -10, ymin = 20, xmax = 60, ymax = 70), crs = st_crs(european_union_map))

european_union_map_cropped <- st_crop(european_union_map, bbox_europe)

df <- 
  tibble(country = european_union,
         some_value = runif(length(european_union)))

map <- 
  european_union_map_cropped %>% 
  left_join(Test, by = c("name" = "GEO"))

png(filename="4.png", res=135,width=1440  , height=810)
ggplot(data = map) +
  theme_minimal()+
  geom_sf(mapping = aes(fill = Value,colour='')) +
  scale_fill_gradient(name = "Rate",na.value='grey',high='#1c9099',low='#ece7f2') +
  labs(#title = "Dementia death rate in 2017",
       caption = "Source: Eurostat",y='Latitude',x='Longitude') +
  scale_x_continuous(limits = c(-15, 50))+
  theme(plot.title.position = "plot",
        plot.title=element_text(size= 20,hjust=0.5),axis.text=element_text(size=12),
        legend.position = c(0.78, 0.75),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(1, 1, 1, 1),
        plot.caption = element_text(face = "italic"))+
  scale_colour_manual(values=NA)+
  guides(colour=guide_legend("No data", override.aes=list(fill="grey")))
dev.off()


####  Plot 5

png(filename="5.png", res=135,width=1440  , height=810)

ggplot(Male_Female,aes(factor(TIME),Value,colour=SEX,fill=SEX))+
geom_bar(aes(group=SEX),size=1,stat='identity', position='dodge',width=0.65)+
geom_text(aes(label=Value),position=position_dodge(width=0.85), vjust=-0.5)+
theme(legend.position = c(0.2, 0.8))+
labs(#title = "Death Rate of Dementia Cases in Greece",
     subtitle = "Based on Gender(2011 to 2019)",y= "Rate", x = "Year",caption="Source: Eurostat")+
theme(plot.title = element_text(hjust = 0.5))+
theme(plot.subtitle = element_text(hjust=0.5),
plot.caption = element_text(face = "italic"))

dev.off()

###  Plot 6

png(filename="6.png", res=135,width=1440  , height=810)

ggplot(LessThan65, aes(x = Value, y =factor(TIME), fill = stat(x),height = ..density..)) +
  geom_density_ridges_gradient(stat='density',scale = 0.6, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Rate", option = "G") +
  labs(#title = 'Rate of Less Than 65 (2011-2019)',
       caption="Source: Eurostat",x='Rate',y='Year')+
  coord_cartesian(clip = "off")+
  theme_minimal()+
  labs(
    title = "Dementia Death Rate Worldwide",
    subtitle = "Less than 65(2011 to 2019)",
    caption = "Source: Eurostat"
  )+
  theme(plot.subtitle = element_text(hjust=0.5),plot.title = element_text(hjust=0.5),
        plot.caption = element_text(face = "italic"))

dev.off()

#### Plot 7

png(filename="7.png", res=135,width=1440  , height=810)

ggplot(ValuesOf2019,aes(x=reorder(GEO,Value),y=Value))+
geom_bar(stat="identity",fill=ifelse(ValuesOf2019$GEO == "Greece",'Blue', '#9ecae1'))+
geom_hline(yintercept = mean(ValuesOf2019$Value), linetype = "dashed", size = 0.8, alpha = 0.8, color = "#1c9099")+
geom_text(x ='Bulgaria', y = mean(ValuesOf2019$Value)-1.5 , label = "MEAN", angle = 90, size = 3.5, color = "#1c9099")+
geom_text(aes(label=Value),hjust=-0.1,nudge_x=0.1)+
theme_minimal()+
labs(#title = "Dementia Standarised Death Rate of EU",
     y= "Rate", x = "Country",caption="Source: Eurostat",
     subtitle = "Year 2019")+
theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust=0.5),
      plot.caption = element_text(face = "italic"))+
coord_flip()

dev.off()
####  Plot 8

png(filename="8.png", res=135,width=1440  , height=810)

ggplot(data=dementiaTotals, aes(x=TIME,y=Value)) +
  geom_line(color = "lightblue", size = 1)+
  geom_point(color = "steelblue") +
  geom_area( fill="#69b3a2", alpha=0.4)+
  facet_wrap(~GEO)+
  theme_minimal()+
  labs(y="Rate",x='Year',
       subtitle = "<span style = 'color: #636363;'>**Lowest**</span> - <span style = 'color: #000000;'>**Highest**</span> (2011-2019)",)+
  theme(plot.title = element_text(hjust = 0.5,colour='#1c9099'),
        plot.subtitle = element_markdown(hjust=0.5,size=20),
        plot.caption = element_text(face = "italic"))+
scale_x_continuous(breaks = c(2012,2014,2016,2018),expand = expansion(mult = 0.1))+
scale_y_continuous(breaks = c(0,20,40,60,80))+
geom_text(data = . %>% group_by(GEO) %>% filter(Value == max(Value)),aes(label=sprintf('%0.2f',Value),vjust="inward",hjust='inward'),colour='black')+
geom_text(data = . %>% group_by(GEO) %>% filter(Value == min(Value)),aes(label=sprintf('%0.2f',Value),vjust="inward",hjust='inward'),colour='#636363')+
labs(
  #title = "Dementia Death Rate Trendline in EU Countries",
  caption = "Source: Eurostat"
)
  
dev.off()

#### Plot 9

png(filename="9.png", res=135,width=1440  , height=810)

ggplot(Under_Over_65,aes(factor(TIME),log(Value),colour=AGE)) +geom_line(aes(group=AGE),size=1) +geom_point()+
  geom_text(aes(label=Value), vjust=-0.6)+
  labs(#title = "Death Rate of Dementia Cases in Greece based on Age"
    y= "Log(Value)", x = "Year",caption="Source: Eurostat")+
  theme(plot.title = element_text(hjust = 0.5),legend.position='top',
        plot.caption = element_text(face = "italic"))

dev.off() 

#### Plot 10


FacetChart = subset(dementia,(SEX == "Males" | SEX == "Females") & (AGE == "65 years or over" )
                    & (GEO == "Greece"| GEO == "Italy"| GEO == "Cyprus" |
                         GEO == "Greece"))

testFemales=FacetChart %>% filter(SEX == "Females")
testMales=FacetChart %>% filter(SEX == "Males")
  
png(filename="10.png", res=135,width=1440  , height=810)

ggplot(FacetChart,aes(x=factor(TIME), y=Value))+
  geom_point(aes(x = factor(TIME), y = Value, colour = GEO,group=GEO,size=0.4),show.legend=FALSE) +
  geom_line(aes(x=factor(TIME),y=Value,group=GEO,colour=GEO))+
  geom_hline(data = testFemales,aes(yintercept=mean(Value)),linetype='dashed',size = .5, alpha = .4, color = "#762a83")+
  geom_hline(data = testMales,aes(yintercept=mean(Value)),linetype='dashed',size = .5, alpha = .4, color = "#762a83")+
  geom_text(y = mean(testFemales$Value) +2, x = '2019', label = "MEAN", size = 3.5, color = "black")+
  labs(
    subtitle = "Dementia Rate Trendlines (2011-2019)",
    caption = "Source:Eurostat",x = "Year",y = "Rate",colour = "Country")+
  facet_grid(SEX~AGE)+
  theme(plot.title = element_text(hjust = 0.5),legend.position='top',
        plot.subtitle = element_text(hjust = 0.5),
        legend.text = element_text(size = 15),
        plot.caption = element_text(face = "italic"))+
  labs(color = "")+
  scale_color_manual(values = c("Greece" = "steelblue",
                                "Italy" = "#7fcdbb",
                                "Cyprus" = "#fdbb84"))
dev.off()
