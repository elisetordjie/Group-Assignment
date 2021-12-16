library("readxl")
Assay_1 <- read.csv("data/Assay 1.csv")
Assay_2 <- read.csv("data/Assay 2.csv")
Assay_3 <- read.csv("data/Assay 3.csv")
names(Assay_1) <- c("Treatments","Replication","Survivals","AdultNO",      
                   "Adult","Mortality")
                  


Assay_2 <- Assay_2[, 1:which(names(Assay_2)=="Mortality")]

hist(Assay_1$Mortality)
hist(Assay_2$Mortality)
hist(Assay_3$Mortality)

Assay_1$TMortality <- asin(sqrt(Assay_1$Mortality))

Assay_1$TMortality
#hist(Assay_1$TMortality)

ks.test(Assay_1$TMortality, "pnorm" )

Anova.Assay1M <- aov(TMortality~Treatments, data = Assay_1)
ks.test(Anova.Assay1M$residuals, "pnorm")

Tukey1 <- TukeyHSD(Anova.Assay1M, conf.level = 0.95)
plot(Tukey1)
Tukey1
library(agricolae)
hsd <- agricolae::HSD.test(Anova.Assay1M, "Treatments")

letters.ordered <- hsd$groups$groups[order(row.names(hsd$groups))]

levels(factor(Assay_1$Treatments))
agricolae::HSD.test()

