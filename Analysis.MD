# BC Historical Forest Fires

## Preamble

The relationship between forests and fires is complicated. Fires are a natural part of the lifecycle of forests and can even [benefit some trees](https://en.wikipedia.org/wiki/Sequoiadendron_giganteum#Ecology). When old or diseased trees burn the canopy is thinned allowing more sunlight for young and healthy trees (which can often survive fires). As forests go up in flames, their nutrients are brought to the forest floor, enriching the soil. These allow forests to stay healthy and diverese, which in turn allows the wildlife that inhabits them to stay healthy. However, if forest fires are too intense they can burn even healthy trees, damage the soil, create smoke which can harm wildlife, and destroy homes or communities. 

With the sun obscured and skies darkened on what should be a sunny summer day (see picture below, taken in 2017), I decided to look at the history of British Columbia's forest fires. This data is from the BC online data catalogue and runs from 1950-2017. As of writing this, 2018 has surpassed 2017 to become BC's [worst ever forest fire seasons](https://www.cbc.ca/news/canada/british-columbia/state-emergency-bc-wildfires-1.4803546) but as the season is not over the data is thus incomplete and won't be used.

![water_smoke_sun](https://raw.githubusercontent.com/atomaszewicz/forest_fires/master/Images/photo%20(16).jpg)
**Figure 1:**  English Bay as seen from Third Beach, summer 2017. 

The dataset has a huge range of sizes, fires as small as 0.0025 acres (ac) to as enormous as 1,300,000 ac (BC spans 235 million acres). This is a large range and I'm not sure what constitutes a major fire. Thankfully the National Wildfire Coordination Group has created a [classification system](https://www.nwcg.gov/term/glossary/size-class-of-fire%C2%A0)) for fire sizes.

|Class|1|2|3|4|5|6|7|
|---|---|---|---|---|---|---|---|
|Size (Ac)|X<0.25|0.25<=X<10|10$\le$X<100| 100<=X<300| 300<=X<1000| 1000<=X<3000| 3000<=X |

## Out of the Frying Pan and Into the Fire

Every year stories come out of [people starting forest fires with cigarettes](https://www.telegraph.co.uk/news/2017/07/17/cigarette-butt-thought-have-caused-major-forest-fire-southern/). These highlight the fragility of our forests and how responsible we have to be. I was hoping to analyze the specific ignition sources for fires, but the data set

has the 'cause' as three broad categories: person, ligthining, and unknown.

![cause_bar](https://raw.githubusercontent.com/atomaszewicz/forest_fires/master/Images/cause_bar_labels.png)
**Figure 2:** Number of fires by class and cause

There are vastly more small fires than large fires (96% are smaller than 100 acres), but the largest 1% of fires account for over 90% of the total area burned. How has the amount of forest burned annual changed over time?

Over the 67 years this data covers, over 20 million acres of forest have burned. The yearly amount of burning ranges from thousands to millions of acres; the median is 86,000 acres. 


![year_class_bar](https://raw.githubusercontent.com/atomaszewicz/forest_fires/master/Images/year_class_bar.png)
**Figure 3:** Total area burned by year and class

2017 is certainly the worst year on record with over 3 million acres of forest fires. There is no clear pattern to the data, and a discussion of the effects of global warming on forest fires is beyond the scope of this project.




# Code

Let's start by loading our data CSV into RStudio.

```
setwd("C:/Users/hobbo/Downloads/BC_forest_fire_data_csv_zip/")
fire_raw<-read.csv("H_FIRE_PNT.csv")
require(reshape2)
require(ggplot2)
```

I prefer to keep my imported dataframes unedited, so let's create a new one to work with. 

```
#Make our working dataframe, with demand that size is non-zero and not a duplicate
fires<-subset(fire_raw,fire_raw$SIZE_HA>0 & fire_raw$FIRE_TYPE!="Duplicate")

#Delete some columns we don't need
fires$FIRE_NO<-NULL
fires$GEO_DESC<-NULL
fires$SHAPE<-NULL
```
```
#Since the NWCG classifications are in acres we will create a column with the sizes in acres
ha2ac<-2.471
fires$SIZE_AC<-fires$SIZE_HA*ha2ac

#Now we create a new column for the class and fill it with the appropriate classification

fires$CLASS_NUM<-with(fires,ifelse(fires$SIZE_AC<0.25,fires$CLASS<-1,
                         ifelse(fires$SIZE_AC>=0.25 & fires$SIZE_AC<10, fires$CLASS_NUM<-2,
                         ifelse(fires$SIZE_AC>=10 & fires$SIZE_AC<100,fires$CLASS_NUM<-3,
                         ifelse(fires$SIZE_AC>=100 & fires$SIZE_AC<300, fires$CLASS_NUM<-4,
                         ifelse(fires$SIZE_AC>=300 & fires$SIZE_AC<1000, fires$CLASS_NUM<-5,
                         ifelse(fires$SIZE_AC>=1000 & fires$SIZE_AC<3000, fires$CLASS_NUM<-6,
                                fires$CLASS_NUM<-7))))))
                   )
```

Let's look more at the breakdown of causes by class. Really used an ugly method to create because kept getting weird errors.

```
#Create a new dataframe

classes<-data.frame("class"=c(1:7),"num"=c(1:7),"cause_ppl"=c(1:7),"cause_lgt"=c(1:7),"cause_uk"=c(1:7))

#Fill the dataframe

for(i in 1:7){
     classes$class  
     classes$num[i]<-nrow(subset(fires,fires$CLASS_NUM==i))
     classes$cause_ppl[i]<-nrow(subset(fires,fires$CLASS_NUM==i & fires$FIRE_CAUSE=="Person"))
     classes$cause_lgt[i]<-nrow(subset(fires,fires$CLASS_NUM==i & fires$FIRE_CAUSE=="Lightning"))
     classes$cause_uk[i]<-nrow(subset(fires,fires$CLASS_NUM==i & fires$FIRE_CAUSE==""))
}
```

```
#Create a new dataframe by reshaping the old one. This is to make it easier to creat eour plot
classes_melt<-melt(classes,id=c("class","num"))

#Create a stacked bar chart
cause_bar<-ggplot(data=classes_melt,aes(x=class,y=value,fill=variable))+geom_bar(stat="identity")+xlab("Fire Class")+scale_x_continuous(breaks=c(1,2,3,4,5,6,7))+theme(panel.grid.minor.x=element_blank())+ylab("Number of Fires")+scale_y_continuous(labels=c("0","25,000","50,000","75,000","100,000"))+ggtitle("Third Class Burns",subtitle="B.C. Forest Fires, 1950-2017")+guides(fill=guide_legend(title="Fire Cause"))
```
```
#Make another dataframe, this time to study the area burned by year
annual_class<-data.frame("year"<-(1950:2017),"num_fires"<-(1950:2017),"burned_ac_total"<-(1950:2017),"burned_ac_1"<-(1950:2017),"burned_ac_2"<-(1950:2017),"burned_ac_3"<-(1950:2017),"burned_ac_4"<-(1950:2017),"burned_ac_5"<-(1950:2017),"burned_ac_6"<-(1950:2017),"burned_ac_7"<-(1950:2017))

colnames(annual_class)<-c("year","num_fires","burned_ac_total","burned_ac_1","burned_ac_2","burned_ac_3","burned_ac_4","burned_ac_5","burned_ac_6","burned_ac_7")

#Fill up our years dataframe

for(i in 1950:2017){
    annual_class$year[i-1949]<-i
    annual_class$num_fires[i-1949]<-nrow(subset(fires,as.numeric(fires$FIRE_YEAR)==i))
    annual_class$burned_ac_total[i-1949]<-sum(subset(fires$SIZE_AC,as.numeric(fires$FIRE_YEAR)==i))
    annual_class$burned_ac_1[i-1949]<-sum(subset(fires$SIZE_AC,as.numeric(fires$FIRE_YEAR==i) & as.numeric(fires$CLASS_NUM == 1)))
    annual_class$burned_ac_2[i-1949]<-sum(subset(fires$SIZE_AC,as.numeric(fires$FIRE_YEAR==i) & as.numeric(fires$CLASS_NUM == 2)))
    annual_class$burned_ac_3[i-1949]<-sum(subset(fires$SIZE_AC,as.numeric(fires$FIRE_YEAR==i) & as.numeric(fires$CLASS_NUM == 3)))
    annual_class$burned_ac_4[i-1949]<-sum(subset(fires$SIZE_AC,as.numeric(fires$FIRE_YEAR==i) & as.numeric(fires$CLASS_NUM == 4)))
    annual_class$burned_ac_5[i-1949]<-sum(subset(fires$SIZE_AC,as.numeric(fires$FIRE_YEAR==i) & as.numeric(fires$CLASS_NUM == 5)))
    annual_class$burned_ac_6[i-1949]<-sum(subset(fires$SIZE_AC,as.numeric(fires$FIRE_YEAR==i) & as.numeric(fires$CLASS_NUM == 6)))
    annual_class$burned_ac_7[i-1949]<-sum(subset(fires$SIZE_AC,as.numeric(fires$FIRE_YEAR==i) & as.numeric(fires$CLASS_NUM == 7)))
}

```

```
#Reshape this dataframe to create our plot
annual_class_melt<-melt(annual_class,id=c("year","num_fires","burned_ac_total"))

annual_class_bar<-ggplot(data=annual_class_melt,aes(x=year,y=value,fill=rev(variable)))+geom_bar(stat="identity")+xlab("Year")+scale_x_continuous(breaks=c(1950,1960,1970,1980,1990,2000,2010,2017))+ylab("Acres Burned")+scale_y_continuous(breaks=c(0,500000,1000000,2000000,3000000),labels=c("0","500,000","1,000,000","2,000,000","3,000,000"))+ggtitle("The Forests They Are a-Burnin'", subtitle="B.C. Forest Fires")+scale_fill_discrete(name = "Fire Class", labels = c("7", "6","5","4","3","2","1"))
```
