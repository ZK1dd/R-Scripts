library(gam)
cars04 <- read.csv("~/Downloads/cars04.csv")
head(cars04)

cars04$Hybrid<-ifelse(cars04$Hybrid==1,1,2)
pairs(cars04[3:13],dataset = cars04, col = cars04$Hybrid)

car <- cars04[,2:13]
View(car)

car.lm <- lm(SuggestedRetailPrice~.,data = car)
summary(car.lm)

gam.lm <- gam(SuggestedRetailPrice ~ DealerCost + Cylinders + s(CityMPG,4) + s(HighwayMPG,4) + s(Weight,3) + s(WheelBase,3) + s(EngineSize,3) + Width,data = car)
summary(gam.lm)
plot(gam.lm,se = TRUE)
preplot(gam.lm)

anova(car.lm,gam.lm,test = "Chisq")
