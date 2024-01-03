## Figures for thesis presentation
## written by Joe Endris

#libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggfortify)
library(ggtext)
library(ggimage)
library(gridExtra)
library(gridGraphics)
library(lubridate)
library(readxl)

# # # # # # # # # # # #
## data preparation ----
# # # # # # # # # # # #

#read in raw data for LT50 values
outputs<-read_excel("data/LT50 master.xlsx")

#create column for julian date
outputs$julian_date <- yday(outputs$Date)

#create column for month
outputs <- mutate(outputs, month=month(outputs$Date))

#create column for year
outputs <- mutate(outputs, year=year(outputs$Date))

#read in NOAA Climate Data data
TN<-read.csv("data/Tennessee_climate.csv")

#omit NA in temperature recordings 
TN<-TN[complete.cases(TN[,10]),]

#create column for year
TN <- mutate(TN, year=year(TN$DATE))

#create column for month
TN <- mutate(TN, month=month(TN$DATE))

# create column for julian date
TN$julian_date <- yday(TN$DATE)

#filter for 1980+ only
TN1980 <- TN %>%
  filter(year>1979)

#read in phenology observations
phenology<-read_excel("data/phenology_check.xlsx")

#create column for year
phenology <- mutate(phenology, year=year(date))

#create column for julian date
phenology$julian_date <- yday(phenology$date)

#filter out 2021 data since there is no corresponding LT50 data for 2021
phenology <- filter(phenology, year > "2021")

#omit any blank spots in the mean_phenology column
phenology <- phenology[complete.cases(phenology[,4]),]

#read in data sets
tcrit <- read_excel("data/crit_values_final.xlsx")
leaf_max_temp <- read_excel("data/leaf_temperatures.xlsx", sheet =2)

#filter just TN data
tcrit<-tcrit[which(tcrit$state=="TN"),]

#create column for julian date
tcrit$julian_date <- yday(tcrit$date)

#create column for month
tcrit <- mutate(tcrit, month=month(tcrit$date))

#create column for year
tcrit <- mutate(tcrit, year=year(tcrit$date))

#add column for image location - needed for plotting leaf image
leaf_max_temp$location<-'leaf_image2.jpg'#change this filename to whatever .jpg or .png - can only use jpg or png
#If using a different .jpg, make sure the file size is really small (<10 KB). Otherwise the plotting
#takes forever and you end up with a huge file size for the ggplot. Not sure why it does this.

#Load NOAA Climate Data Online data
tenn_clim<-read.csv("data/Tennessee_climate.csv")

#create column for year
tenn_clim <- mutate(tenn_clim, year=year(tenn_clim$DATE))

#create column for month
tenn_clim <- mutate(tenn_clim, month=month(tenn_clim$DATE))

## create column for julian date##
tenn_clim$julian_date <- yday(tenn_clim$DATE)

#omit NA in TMAX/TMIN recordings 
tenn_clim<-tenn_clim[complete.cases(tenn_clim[,9]),]

#filter for 1980-present
tenn1980 <- tenn_clim %>%
  filter(year>1979)

# # # # # # # # # # # 
## days below -2 ----
# # # # # # # # # # # 

#determine number of spring days below -2
neg_2_days <- TN1980 %>%
  group_by(year) %>%
  summarise(total_days=sum(TMIN < -2))

mean(neg_2_days$total_days)

#plot Number of Days Below -2 since 1980
TN_freeze_plot <- neg_2_days %>%
  ggplot(aes(x = year, y = total_days)) +
  geom_point(color="black") +
  geom_smooth(method="lm")+
  labs(y= "Number of Days below -2°C",
       x= "Year") + 
  theme_bw(base_size = 10)+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.background = element_blank())

TN_freeze_plot

ggsave(filename="Figure1.tiff", plot=TN_freeze_plot, device= "tiff", units="cm", width = 8.23, height = 5.49, dpi=600)

# # # # # # # # # # # 
## Last -2 by year----
# # # # # # # # # # # 

#calculate last day below -2 for each year since 1980
last_freeze <- tenn1980%>%
  filter(TMIN< -2)%>%
  filter(julian_date<180)%>%
  group_by(year)%>%
  filter(row_number()==n())

#plot last -2 day
last_neg_2 <- ggplot(last_freeze, aes(x=year, y=julian_date))+
  geom_point()+
  geom_hline(yintercept = 83.26, color= 'red', size=1.25)+ #average last freeze date
  geom_smooth(method="lm")+
  scale_y_continuous(limits = c(40, 125),
                     breaks=seq(40, 125,by=10))+
  labs(y= "Julian Date of last -2°C",
       x= "Year") + 
  theme_bw(base_size = 18)+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

last_neg_2


# # # # # # # # # # # 
## Phenology plot----
# # # # # # # # # # # 

#calculate mean phenology by julian date
phenology <- phenology%>%
  group_by(species, year, julian_date) %>%
  dplyr::mutate(mean_phenology=mean(phenology))

#calculate SD for phenology
phenology <- phenology%>%
  group_by(species, year, julian_date) %>%
  mutate(pheno_sd = sd(phenology, na.rm=TRUE))

phenology[,10][phenology[,10]==0] <- NA

maple_phenology<-ggplot(data=subset(phenology, species=="Acer saccharum"), aes(x = julian_date, y=mean_phenology, color=factor(year))) +
  geom_point()+
  geom_errorbar(aes(y = mean_phenology, ymin = mean_phenology - pheno_sd, ymax = mean_phenology + pheno_sd), alpha = .2) +
  geom_line()+
  labs(x="", y="", colour = "Year")+
  scale_color_manual(values = c("2022" = "darkorange", "2023" = "darkgreen"))+
  ylim(-1, 5)+
  theme_bw(base_size = 10)+
  theme(axis.title.x = element_markdown())+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        text=element_text(size=10),
        legend.background = element_blank(),
        legend.box.background = element_blank(),legend.spacing.y = unit(0, "cm"),
        legend.position=c("0.08","0.5"),
        legend.title = element_text(size = 4), 
        legend.text = element_text(size = 4))+
  annotate("text", x=40,y=4.5,label= expression("Acer saccharum"), hjust=0, size=3)

maple_phenology

beech_phenology<-ggplot(data=subset(phenology, species=="Fagus grandifolia"), aes(x = julian_date, y=mean_phenology, color=factor(year))) +
  geom_point()+
  geom_errorbar(aes(y = mean_phenology, ymin = mean_phenology - pheno_sd, ymax = mean_phenology + pheno_sd), alpha = .2) +
  geom_line()+
  labs(x="", y="Phenology Code", colour = "Year")+
  scale_color_manual(values = c("2022" = "darkorange", "2023" = "darkgreen"))+
  ylim(-1, 5)+
  theme_bw(base_size = 10)+
  theme(legend.position="none")+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        text=element_text(size=10))+
  annotate("text", x=40,y=4,label= expression("Fagus grandifolia"), hjust=0, size=3)

beech_phenology

poplar_phenology<-ggplot(data=subset(phenology, species=="Liriodendron tulipifera"), aes(x = julian_date, y=mean_phenology, color=factor(year))) +
  geom_point()+
  geom_errorbar(aes(y = mean_phenology, ymin = mean_phenology - pheno_sd, ymax = mean_phenology + pheno_sd), alpha = .2) +
  geom_line()+
  labs(x="Julian Date", y="", colour = "Year")+
  scale_color_manual(values = c("2022" = "darkorange", "2023" = "darkgreen"))+
  ylim(-1, 5)+
  theme_bw(base_size = 10)+
  theme(legend.position="none")+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        text=element_text(size=10))+
  annotate("text", x=40,y=4.5,label= expression("Liriodendron tulipifera"), hjust=0, size=3)

poplar_phenology

Figure2 <- grid.arrange(maple_phenology, beech_phenology, poplar_phenology, nrow=3)

ggsave(filename="Figure2.tiff", plot=Figure2, device= "tiff", units="cm", width = 12.5, height = 9.37, dpi=600)

# # # # # # # # # # # #
## LT50 plots series----
# # # # # # # # # # # #
dbl_panel <- filter(outputs, State == "TN")

dbl_panel <- dbl_panel%>%
  group_by(year, Species, julian_date) %>%
  dplyr::summarise(LT15.m=mean(LT15), LT50mod=mean(LT50), LT95.m=mean(LT95),
                   LT50mod_sd=sd(LT50),
                   LT50mod_se=sd(LT50)/sqrt(6))

jdate_TMIN <- TN %>%
  filter(year >1979) %>%
  group_by(julian_date) %>%
  summarise(absol_TMIN = min(TMIN)) 

TMIN_2022 <- TN %>%
  filter(year==2022) %>%
  mutate(absol_TMIN = TMIN) %>%
  select(julian_date, absol_TMIN)

TMIN_2023 <- TN %>%
  filter(year==2023) %>%
  mutate(absol_TMIN = TMIN) %>%
  select(julian_date, absol_TMIN)

#stack jdate_TMIN and TMIN_2022 into a single dataframe
jdate_TMIN$year="1980"
TMIN_2022$year="2022"
TMIN_2023$year="2023"
new<-rbind(jdate_TMIN,TMIN_2022,TMIN_2023)

plot2022 <-ggplot() +
  geom_point(data=subset(dbl_panel, year=="2022"), aes(x = julian_date, y=LT50mod, color= Species), position = position_dodge(width = 2))+
  geom_errorbar(data=subset(dbl_panel, year=="2022"), aes(x= julian_date, ymax=LT50mod+LT50mod_se,ymin=LT50mod-LT50mod_se, color= Species), width= 2, position = position_dodge(width = 2))+
  geom_line(data=subset(new,year!="2023"), aes(x=julian_date, y=absol_TMIN, group=year,linetype=year))+
  scale_color_manual(values = c("Acer saccharum" = "red", "Liriodendron tulipifera" = "blue", "Fagus grandifolia" = "black"))+
  scale_linetype_manual("Minimum temperature",values = c("2022"=1,"1980"=2),labels=c("Since 1980","2022"))+
  xlim(40,130) +
  ylim(-20,10)+
  labs(y=expression("LT"["50"]/"Temperature (°C)"))+
  xlab("Julian Date")+
  theme_bw(base_size = 10)+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        legend.position = 'none')+
  ggtitle(2022)

plot2023 <-ggplot() +
  geom_point(data=subset(dbl_panel, year=="2023"), aes(x = julian_date, y=LT50mod, color= Species), position = position_dodge(width = 2))+
  geom_errorbar(data=subset(dbl_panel, year=="2023"), aes(x= julian_date, ymax=LT50mod+LT50mod_se,ymin=LT50mod-LT50mod_se, color= Species), width = 2, position = position_dodge(width = 2))+
  geom_line(data=subset(new,year!="2022"), aes(x=julian_date, y=absol_TMIN, group=year,linetype=year))+
  scale_linetype_manual("Minimum temperature",values = c("2023"=1,"1980"=2),labels=c("Since 1980","2023"))+
  scale_color_manual(values = c("Acer saccharum" = "red", "Liriodendron tulipifera" = "blue", "Fagus grandifolia" = "black"),guide="none")+
  xlim(40,130) +
  ylim(-20,10)+
  labs(y=expression("LT"["50"]/"Temperature (°C)"))+
  xlab("Julian Date")+
  theme_bw(base_size = 10)+
  theme(axis.title.x = element_markdown())+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = 'none')+
  ggtitle(2023)

Figure3 <- grid.arrange(plot2022, plot2023,nrow=2)

ggsave(filename="Figure3.tiff", plot=Figure3, device= "tiff", units="cm", width = 12.5, height = 9.37, dpi=600)


##Plots for presentations
plot22 <-ggplot()+
  geom_line(data=subset(new,year!="2023"), aes(x=julian_date, y=absol_TMIN, group=year,linetype=year))+
  scale_color_manual(values = c("Acer saccharum" = "red", "Liriodendron tulipifera" = "blue", "Fagus grandifolia" = "black"))+
  scale_linetype_manual("Minimum temperature",values = c("2022"=1,"1980"=2),labels=c("Since 1980","2022"))+
  xlim(40,130) +
  ylim(-20,10)+
  labs(y=expression("LT"["50"]/"Temperature (°C)"))+
  xlab("Julian Date")+
  theme_bw(base_size = 18)+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        legend.position = 'none')+
   ggtitle(2022)
plot22

plot23 <-ggplot() +
  geom_line(data=subset(new,year!="2022"), aes(x=julian_date, y=absol_TMIN, group=year,linetype=year))+
  scale_linetype_manual("Minimum temperature",values = c("2023"=1,"1980"=2),labels=c("Since 1980","2023"))+
  scale_color_manual(values = c("Acer saccharum" = "red", "Liriodendron tulipifera" = "blue", "Fagus grandifolia" = "black"),guide="none")+
  xlim(40,130) +
  ylim(-20,10)+
  labs(y=expression("LT"["50"]/"Temperature (°C)"))+
  xlab("Julian Date")+
  theme_bw(base_size = 18)+
  theme(axis.title.x = element_markdown())+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = 'none')+
  ggtitle(2023)
plot23

layer1 <- grid.arrange(plot22, plot23,nrow=2)

plot22b <-ggplot() +
  geom_point(data=subset(dbl_panel, year=="2022"), aes(x = julian_date, y=LT50mod, color= Species), position = position_dodge(width = 2))+
  geom_errorbar(data=subset(dbl_panel, year=="2022"), aes(x= julian_date, ymax=LT50mod+LT50mod_se,ymin=LT50mod-LT50mod_se, color= Species), width= 2, position = position_dodge(width = 2))+
  geom_line(data=subset(new,year!="2023"), aes(x=julian_date, y=absol_TMIN, group=year,linetype=year))+
  scale_color_manual(values = c("Acer saccharum" = "red", "Liriodendron tulipifera" = "blue", "Fagus grandifolia" = "black"))+
  scale_linetype_manual("Minimum temperature",values = c("2022"=1,"1980"=2),labels=c("Since 1980","2022"))+
  xlim(40,130) +
  ylim(-20,10)+
  labs(y=expression("LT"["50"]/"Temperature (°C)"))+
  xlab("Julian Date")+
  theme_bw(base_size = 18)+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        legend.position = 'none')+
  ggtitle(2022)
plot22b

plot23b <-ggplot() +
  geom_point(data=subset(dbl_panel, year=="2023"), aes(x = julian_date, y=LT50mod, color= Species), position = position_dodge(width = 2))+
  geom_errorbar(data=subset(dbl_panel, year=="2023"), aes(x= julian_date, ymax=LT50mod+LT50mod_se,ymin=LT50mod-LT50mod_se, color= Species), width = 2, position = position_dodge(width = 2))+
  geom_line(data=subset(new,year!="2022"), aes(x=julian_date, y=absol_TMIN, group=year,linetype=year))+
  scale_linetype_manual("Minimum temperature",values = c("2023"=1,"1980"=2),labels=c("Since 1980","2023"))+
  scale_color_manual(values = c("Acer saccharum" = "red", "Liriodendron tulipifera" = "blue", "Fagus grandifolia" = "black"),guide="none")+
  xlim(40,130) +
  ylim(-20,10)+
  labs(y=expression("LT"["50"]/"Temperature (°C)"))+
  xlab("Julian Date")+
  theme_bw(base_size = 18)+
  theme(axis.title.x = element_markdown())+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = 'none')+
  ggtitle(2023)
plot23b

layer2 <- grid.arrange(plot22b, plot23b,nrow=2)

plot22c <-ggplot() +
  geom_point(data=subset(dbl_panel, year=="2022"), aes(x = julian_date, y=LT50mod, color= Species), position = position_dodge(width = 2))+
  geom_errorbar(data=subset(dbl_panel, year=="2022"), aes(x= julian_date, ymax=LT50mod+LT50mod_se,ymin=LT50mod-LT50mod_se, color= Species), width= 2, position = position_dodge(width = 2))+
  geom_line(data=subset(new,year!="2023"), aes(x=julian_date, y=absol_TMIN, group=year,linetype=year))+
  geom_vline(xintercept = 97, color = 'blue', linewidth=1.25)+
  geom_vline(xintercept = 108.75, color = 'red', linewidth=1.25)+
  geom_vline(xintercept = 109.25, color = 'black', linewidth=1.25)+
  scale_color_manual(values = c("Acer saccharum" = "red", "Liriodendron tulipifera" = "blue", "Fagus grandifolia" = "black"))+
  scale_linetype_manual("Minimum temperature",values = c("2022"=1,"1980"=2),labels=c("Since 1980","2022"))+
  xlim(40,130) +
  ylim(-20,10)+
  labs(y=expression("LT"["50"]/"Temperature (°C)"))+
  xlab("Julian Date")+
  theme_bw(base_size = 18)+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        legend.position = 'none')+
  ggtitle(2022)
plot22c

plot23c <-ggplot() +
  geom_point(data=subset(dbl_panel, year=="2023"), aes(x = julian_date, y=LT50mod, color= Species), position = position_dodge(width = 2))+
  geom_errorbar(data=subset(dbl_panel, year=="2023"), aes(x= julian_date, ymax=LT50mod+LT50mod_se,ymin=LT50mod-LT50mod_se, color= Species), width = 2, position = position_dodge(width = 2))+
  geom_line(data=subset(new,year!="2022"), aes(x=julian_date, y=absol_TMIN, group=year,linetype=year))+
  geom_vline(xintercept = 76, color = 'blue', linewidth=1.25)+
  geom_vline(xintercept = 82.75, color = 'red', linewidth=1.25)+
  geom_vline(xintercept = 83.25, color = 'black', linewidth=1.25)+
  scale_linetype_manual("Minimum temperature",values = c("2023"=1,"1980"=2),labels=c("Since 1980","2023"))+
  scale_color_manual(values = c("Acer saccharum" = "red", "Liriodendron tulipifera" = "blue", "Fagus grandifolia" = "black"),guide="none")+
  xlim(40,130) +
  ylim(-20,10)+
  labs(y=expression("LT"["50"]/"Temperature (°C)"))+
  xlab("Julian Date")+
  theme_bw(base_size = 18)+
  theme(axis.title.x = element_markdown())+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = 'none')+
  ggtitle(2023)
plot23c

layer1c <- grid.arrange(plot22c, plot23c,nrow=2)

plot22d <-ggplot()+
  geom_line(data=subset(new,year=="2022"), aes(x=julian_date, y=absol_TMIN, group=year,linetype=year))+
  xlim(40,130) +
  ylim(-20,10)+
  labs(y=expression("LT"["50"]/"Temperature (°C)"))+
  xlab("Julian Date")+
  theme_bw(base_size = 18)+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        legend.position = 'none')+
  ggtitle(2022)
plot22d

plot23d <-ggplot() +
  geom_line(data=subset(new,year=="2023"), aes(x=julian_date, y=absol_TMIN, group=year,linetype=year))+
  xlim(40,130) +
  ylim(-20,10)+
  labs(y=expression("LT"["50"]/"Temperature (°C)"))+
  xlab("Julian Date")+
  theme_bw(base_size = 18)+
  theme(axis.title.x = element_markdown())+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = 'none')+
  ggtitle(2023)
plot23d

layer1a <- grid.arrange(plot22d, plot23d,nrow=2)
# # # # # # # # # # # # #
## hottest day of year----
# # # # # # # # # # # # #

#determine hottest day by year
TN_TMAX <- tenn1980 %>%
  group_by(year) %>%
  summarise(abs_TMAX = max(TMAX))

mean_TMAX <- tenn1980 %>%
  filter(julian_date > 120) %>%
  filter(julian_date < 274) %>%
  group_by(julian_date) %>%
  summarise(abs_TMAX = max(TMAX))

mean(mean_TMAX$abs_TMAX)

#create plot for record high by year
record_TMAX_plot <- ggplot(TN_TMAX, aes(x=year, y= abs_TMAX))+
  geom_point()+
  #geom_hline(yintercept = 36.58824, color= "red", linewidth = 1.25)+ #mean summer TMAX
  scale_y_continuous(limits = c(33, 44),
                     breaks=seq(32,45,by=2),
                     minor_breaks = seq(32, 45, 1)) +
  labs(
    y= "Highest Temperature °C",
    x= "Year")+
  theme_bw(base_size = 18)+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background  = element_blank(),
        axis.line = element_line(colour = "black"),
        #axis.title.x = element_blank(),
        axis.text.x=element_text(angle = 45, hjust = 1))

record_TMAX_plot

# # # # # # # # # # # # # # # # 
## number of days above 32.2----
# # # # # # # # # # # # # # # #

#number of days above 32.2C (90F)
days_32 <- tenn1980 %>%
  group_by(year) %>%
  summarise(number=sum(TMAX>32.19))

#plot number of days above 32.2C
days_32_plot <- ggplot(days_32, aes(x=year, y=number ))+
  geom_point() +
  #geom_hline(yintercept = 54.1, color= "red", linewidth = 1.25)+ #mean number of days above 32.2
  theme_bw(base_size = 18)+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x=element_text(angle = 45, hjust = 1))+
  labs(y= "Number of Days >32.2°C",
    x= "Year")

days_32_plot


# # # # # # # # # # # # # # 
## June 2022 Tcrit plots----
# # # # # # # # # # # # # # 

Junea <- ggplot(tcrit%>%
                     filter(year==2022,month==6), aes(y= Tcrit.mn, x= id)) +
  coord_flip()+
  #geom_image has to come first so it is drawn behind everything else, that way if white space is in image,
  #it doesn't show up
  scale_x_discrete(limit=rev)+
  ylab("Critical Temperature (°C)")+
  xlab("Species")+
  ylim(30, 55)+
  ggtitle("June 2022")+
  theme_bw(base_size = 18)+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position='none')
Junea

Juneb <- ggplot(tcrit%>%
                  filter(year==2022,month==6), aes(y= Tcrit.mn, x= id)) +
  coord_flip()+
  geom_point()+
  scale_x_discrete(limit=rev)+
  geom_errorbar(aes(ymax=Tcrit.uci,ymin=Tcrit.lci),width=0.5)+
  ylab("Critical Temperature (°C)")+
  xlab("Species")+
  ylim(30, 55)+
  ggtitle("June 2022")+
  theme_bw(base_size = 18)+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

Juneb

Junec <- ggplot(tcrit%>%
                  filter(year==2022,month==6), aes(y= Tcrit.mn, x= id)) +
  coord_flip()+
  geom_point()+
  scale_x_discrete(limit=rev)+
  geom_errorbar(aes(ymax=Tcrit.uci,ymin=Tcrit.lci),width=0.5)+
  geom_hline(yintercept = 38.3, linetype = 3, color= "blue")+ #highest temp June 2022
  annotate("text", x=5.75, y=37.5, label="Highest temp in June 2022", angle='90')+
  ylab("Critical Temperature (°C)")+
  xlab("Species")+
  ylim(30, 55)+
  ggtitle("June 2022")+
  theme_bw(base_size = 18)+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
Junec

Juned <- ggplot(tcrit%>%
                  filter(year==2022,month==6), aes(y= Tcrit.mn, x= id)) +
  coord_flip()+
  geom_point()+
  scale_x_discrete(limit=rev)+
  geom_errorbar(aes(ymax=Tcrit.uci,ymin=Tcrit.lci),width=0.5)+
  geom_hline(yintercept = 42.8, linetype = 2, color= "orange")+ #record june high temp
  annotate("text", x=5.75, y=42.3, label="Record June temp", angle='90')+
  ylab("Critical Temperature (°C)")+
  xlab("Species")+
  ylim(30, 55)+
  ggtitle("June 2022")+
  theme_bw(base_size = 18)+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
Juned

Junee <- ggplot(tcrit%>%
                  filter(year==2022,month==6), aes(y= Tcrit.mn, x= id)) +
  coord_flip()+
  geom_point()+
  scale_x_discrete(limit=rev)+
  geom_errorbar(aes(ymax=Tcrit.uci,ymin=Tcrit.lci),width=0.5)+
  geom_hline(yintercept = 44.4, color= "red")+ #record high temp
  ylab("Critical Temperature (°C)")+
  xlab("Species")+
  ylim(30, 55)+
  ggtitle("June 2022")+
  theme_bw(base_size = 18)+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
Junee

June2022 <- ggplot(tcrit%>%
                     filter(year==2022,month==6), aes(y= Tcrit.mn, x= id)) +
  coord_flip()+
  #geom_image has to come first so it is drawn behind everything else, that way if white space is in image,
  #it doesn't show up
  geom_image(data=subset(leaf_max_temp, year=="2022"), aes(y=leaf_temp, x=id,image=location),size=0.05)+
  geom_point()+
  scale_x_discrete(limit=rev)+
  geom_errorbar(aes(ymax=Tcrit.uci,ymin=Tcrit.lci),width=0.5)+
  ylab("Critical Temperature (°C)")+
  xlab("Species")+
  ylim(30, 55)+
  ggtitle("June 2022")+
  theme_bw(base_size = 18)+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
June2022


# # # # # # # # # # # # # # 
## four month Tcrit plot----
# # # # # # # # # # # # # # 

#June 2022
June2022 <- ggplot(tcrit%>%
                     filter(year==2022,month==6), aes(y= Tcrit.mn, x= id)) +
  coord_flip()+
  #geom_image has to come first so it is drawn behind everything else, that way if white space is in image,
  #it doesn't show up
  geom_image(data=subset(leaf_max_temp, year=="2022"), aes(y=leaf_temp, x=id,image=location),size=0.05)+
  geom_point()+
  scale_x_discrete(limit=rev)+
  geom_errorbar(aes(ymax=Tcrit.uci,ymin=Tcrit.lci),width=0.5)+
  ylab("Critical Temperature (°C)")+
  xlab("Species")+
  ylim(30, 55)+
  ggtitle("June 2022")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
June2022

July2022 <- ggplot(tcrit%>%
                     filter(year==2022,month==7), aes(y= Tcrit.mn, x= id)) +
  coord_flip()+
  geom_image(data=subset(leaf_max_temp, year=="2022"), aes(y=leaf_temp, x=id,image=location),size=0.05)+
  geom_point()+
  scale_x_discrete(limit=rev)+
  geom_errorbar(aes(ymax=Tcrit.uci,ymin=Tcrit.lci),width=0.5)+
  ylab("Critical Temperature (°C)")+
  xlab("Species")+
  ylim(30, 55)+
  ggtitle("July 2022")+
  theme(legend.position="none")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
July2022

June2023 <- ggplot(tcrit%>%
                     filter(year==2023,month==6), aes(y= Tcrit.mn, x= id)) +
  coord_flip()+
  geom_image(data=subset(leaf_max_temp, year=="2023"), aes(y=leaf_temp, x=id,image=location),size=0.05)+
  geom_point()+
  scale_x_discrete(limit=rev)+
  geom_errorbar(aes(ymax=Tcrit.uci,ymin=Tcrit.lci),width=0.5)+
  ylab("Critical Temperature (°C)")+
  xlab("Species")+
  ylim(30, 55)+
  ggtitle("June 2023")+
  theme(legend.position="none")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
June2023

July2023 <- ggplot(tcrit%>%
                     filter(year==2023,month==7), aes(y= Tcrit.mn, x= id)) +
  coord_flip()+
  geom_image(data=subset(leaf_max_temp, year=="2023"), aes(y=leaf_temp, x=id,image=location),size=0.05)+
  geom_point()+
  geom_errorbar(aes(ymax=Tcrit.uci,ymin=Tcrit.lci),width=0.5)+
  scale_x_discrete(limit=rev)+
  ylab("Critical Temperature (°C)")+
  xlab("Species")+
  ylim(30, 55)+
  ggtitle("July 2023")+
  theme(legend.position="none")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
July2023

grid.arrange(June2022,July2022, June2023,July2023,ncol=2)
