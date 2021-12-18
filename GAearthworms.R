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

#ASSAY 2
Assay_2$TMortality <- asin(sqrt(Assay_2$Mortality))
hist(Assay_2$TMortality)
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
#THE MORTALITY DATA IN ALL ASSAYS FAILED TO BECOME NORMALLY AFTER THE TRANSFORMATION 
#HENCE THE MOST APPROPRAITE TEST THAT SHOULD HAVE BEEN USED TO ANALYSE THIS DATA
#IS A NONE PARAMETRIC TEST LIKE THE KRUSKAL WALLIS TEST