library("readxl")
Assay_1 <- read.csv("data/Assay 1.csv")
Assay_2 <- read.csv("data/Assay 2.csv")
Assay_3 <- read.csv("data/Assay 3.csv")
names(Assay_1) <- c("Treatments","Replication","Survivals","AdultNO",      
                   "Adult","Mortality")
Assay_2
names(Assay_2) <- c("Treatments","No.of.earthworms","Earthworm.weight.g","No.of.adult",
                    "Date","Group","Adults","Mortality")                
Assay_3
names(Assay_3) <- c("Treatments","Survivals","AdultsNO","Date","DayCount",
                 "Adults","Mortality","PMortality")


Assay_2 <- Assay_2[, 1:which(names(Assay_2)=="Mortality")]

hist(Assay_1$Mortality)
hist(Assay_2$Mortality)
hist(Assay_3$Mortality)

#ASSAY 1
hist(Assay_1$Mortality)
library(ggplot2)
g <- ggplot(Assay_1, aes(x = Treatments, y = Mortality, 
                      fill = Treatments))

g + geom_boxplot()

Assay_1$TMortality <- asin(sqrt(Assay_1$Mortality))

Assay_1$TMortality
#hist(Assay_1$TMortality)

ks.test(Assay_1$TMortality, "pnorm" )
#Transformation did not make mortality data normal

Anova.Assay1M <- aov(TMortality~Treatments, data = Assay_1)
ks.test(Anova.Assay1M$residuals, "pnorm")
#Transformation did not make residuals normal

Tukey1 <- TukeyHSD(Anova.Assay1M, conf.level = 0.95)
plot(Tukey1)
Tukey1
library(agricolae)
hsd <- agricolae::HSD.test(Anova.Assay1M, "Treatments")

levels(factor(Assay_1$Treatments))
hsd
plot(hsd)



#Despite transforming the mortality data the article reported graphics of the untransformed data
dataA1 <- Assay_1 %>% select(Treatments, Mortality) 
my_sumA1 <- dataA1 %>%
  group_by(Treatments) %>%
  summarise( 
    n=n(),
  Mean.Mortality=mean(Mortality),
    sd=sd(Mortality)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

ggplot(my_sumA1) +
  geom_bar( aes(x=Treatments, y=Mean.Mortality), stat="identity", fill="midnightblue", alpha=0.5) +
  geom_errorbar( aes(x=Treatments, ymin=Mean.Mortality-se, ymax=Mean.Mortality+se), width=0.4, colour="orange", alpha=0.9, size=1) +
  ggtitle("Mortality of Pheritimoid Earthworms in Assay 1 (+/- Standard Error)")



#ASSAY 2
Assay_2$TMortality <- asin(sqrt(Assay_2$Mortality))
hist(Assay_2$TMortality)
g <- ggplot(Assay_2, aes(x = Treatments, y = Mortality, 
                         fill = Treatments))

g + geom_boxplot()

ks.test(Assay_2$TMortality, "pnorm" )
#transformation did not make the mortality data normal

Anova.Assay2M <- aov(TMortality~Treatments, data = Assay_2)
ks.test(Anova.Assay2M$residuals, "pnorm")
#Transformation failed in making residuals normal

Tukey2 <- TukeyHSD(Anova.Assay2M, conf.level = 0.95)
plot(Tukey2)

library(agricolae)
hsd2 <- agricolae::HSD.test(Anova.Assay2M, "Treatments")
hsd2
levels(factor(Assay_2$Treatments))
hsd2
plot(hsd2)

#Data reported in paper was the untransformed data 
dataA2 <- Assay_2 %>% select(Treatments, Mortality) 
my_sumA2 <- dataA2 %>%
  group_by(Treatments) %>%
  summarise( 
    n=n(),
    Mean.Mortality=mean(Mortality),
    sd=sd(Mortality)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

#Graph of mortality in Assay2 using data with no transformed as in the paper.
ggplot(my_sumA2) +
  geom_bar( aes(x=Treatments, y=Mean.Mortality), stat="identity", fill="firebrick4", alpha=0.5) +
  geom_errorbar( aes(x=Treatments, ymin=Mean.Mortality-se, ymax=Mean.Mortality+se), width=0.4, colour="orange", alpha=0.9, size=1) +
  ggtitle("Mortality of Pheritimoid Earthworms in Assay 2 (+/- Standard Error)")


#ASSAY3
Assay_3$TMortality <- asin(sqrt(Assay_3$PMortality))
hist(Assay_3$TMortality)

ks.test(Assay_3$TMortality, "pnorm" )
#Transformation did not make the data normal.

Anova.Assay3M <- aov(TMortality~Treatments, data = Assay_3)
ks.test(Anova.Assay3M$residuals, "pnorm")
#Transformation failed in making residuals normal

Tukey3 <- TukeyHSD(Anova.Assay3M, conf.level = 0.95)
plot(Tukey3)

library(agricolae)
hsd3 <- agricolae::HSD.test(Anova.Assay3M, "Treatments")
hsd3
levels(factor(Assay_3$Treatments))
hsd3
plot(hsd3)

#Data reported in paper was the untransformed data 
dataA3 <- Assay_3 %>% select(Treatments, PMortality) 
my_sumA3 <- dataA3 %>%
  group_by(Treatments) %>%
  summarise( 
    n=n(),
    Mean.Mortality=mean(PMortality),
    sd=sd(PMortality)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

#Graph of mortality in Assay3 using data with no transformed as in the paper.
ggplot(my_sumA3) +
  geom_bar(aes(x=Treatments, y=Mean.Mortality), stat="identity", fill="red4", alpha=0.5) +
  geom_errorbar(aes(x=Treatments, ymin=Mean.Mortality-se, ymax=Mean.Mortality+se), width=0.4, colour="orange", alpha=0.9, size=1) +
  ggtitle("Mortality of Pheritimoid Earthworms in Assay 3 (+/- Standard Error)")


#THE MORTALITY DATA IN ALL ASSAYS FAILED TO BECOME NORMALLY DISTRIBUTED AFTER THE TRANSFORMATION 
#HENCE THE MOST APPROPRAITE TEST THAT SHOULD HAVE BEEN USED TO ANALYSE THIS DATA
#IS A NONE PARAMETRIC TEST LIKE THE KRUSKAL WALLIS TEST

#NON PARAMETRIC TEST ASSAY 1
KAssay1 <- kruskal.test(Mortality ~ Treatments, data = Assay_1)
KAssay1 <- agricolae::kruskal(Assay_1$Mortality, Assay_1$Treatments)
letters.ordered <- KAssay1$groups$groups[order(row.names(KAssay1$groups))]
letters.ordered
plot(KAssay1)
#There is no difference between results obtained with the arcsine transformed data
#and those obtained using the kruskal wallis test in Assay 1.

#NON PARAMETRIC ASSAY 2
KAssay2 <- kruskal.test(Mortality ~ Treatments, data = Assay_2)
KAssay2 <- agricolae::kruskal(Assay_2$Mortality, Assay_2$Treatments)
letters.ordered <- KAssay2$groups$groups[order(row.names(KAssay2$groups))]
letters.ordered
plot(KAssay2)
#The Kruskal wallis test shows the differences between the various treatments
#these differencesare not visible using the tukey HSD.

#NON PARAMETRIC ASSAY 3
KAssay3 <- kruskal.test(PMortality ~ Treatments, data = Assay_3)
KAssay3 <- agricolae::kruskal(Assay_3$PMortality, Assay_3$Treatments)
letters.ordered <- KAssay3$groups$groups[order(row.names(KAssay1$groups))]
letters.ordered
plot(KAssay3)
# For Assay3, the tukey HSD shows more differences between the means of the
#various treatments than the kruskal wallis test.




