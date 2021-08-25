#Stage 1 Data Preparation
#library we may use
library(dplyr)
library(tidyr) 
library(ggplot2)
library(epiDisplay) 
library(splines2) 
library(gam) 
library(readxl)
library(GeneCycle)
library(scales)
library(tidyverse)
library(sqldf)
library(GGally)
df$Accident_date <- as.Date(df$Accident_date)
Num_accident <- count(df, vars = df$Accident_date)
sum(is.na(df))
sum(is.na(df$no_of_vehicles_involved))
##Road type
## splitting and count data by factor
rt1 <- sqldf('select Accident_date, COUNT(Road_type) as rt1 from df where Road_type=1 group by Accident_date')
rt2 <- sqldf('select Accident_date, COUNT(Road_type) as rt2 from df where Road_type=2 group by Accident_date')
rt3 <- sqldf('select Accident_date, COUNT(Road_type) as rt3 from df where Road_type=3 group by Accident_date')

##merge counted data
rt <- merge(rt1,rt2,by='Accident_date', all=TRUE)
rt <- merge(rt,rt3,by='Accident_date',all=TRUE)

##We define null variable to zero
rt[is.na(rt)] <- 0

## calculate the index
for(i in 1:nrow(rt)){
  rt$rtindex[i] <- (rt$rt1[i] * 1 + rt$rt2[i] * 2 + rt$rt3[i] * 3)/(rt$rt1[i]+rt$rt2[i]+rt$rt3[i])
}
##Weather condition
wc0 <- sqldf('select Accident_date, COUNT(Weather_Status) as wc0 from df where Weather_Status=0 group by Accident_date')
wc1 <- sqldf('select Accident_date, COUNT(Weather_Status) as wc1 from df where Weather_Status=1 group by Accident_date')
wc2 <- sqldf('select Accident_date, COUNT(Weather_Status) as wc2 from df where Weather_Status=2 group by Accident_date')
wc3 <- sqldf('select Accident_date, COUNT(Weather_Status) as wc3 from df where Weather_Status=3 group by Accident_date')
wc4 <- sqldf('select Accident_date, COUNT(Weather_Status) as wc4 from df where Weather_Status=4 group by Accident_date')
wc5 <- sqldf('select Accident_date, COUNT(Weather_Status) as wc5 from df where Weather_Status=5 group by Accident_date')

##merge counted data
wc <- merge(wc0,wc1,by='Accident_date', all=TRUE)
wc <- merge(wc,wc2,by='Accident_date', all=TRUE)
wc <- merge(wc,wc3,by='Accident_date', all=TRUE)
wc <- merge(wc,wc4,by='Accident_date', all=TRUE)
wc <- merge(wc,wc5,by='Accident_date', all=TRUE)

wc[is.na(wc)] <- 0

## calculate the index
for(i in 1:nrow(wc)){
  wc$wcindex[i] <- (wc$wc0[i] * 0 + wc$wc1[i] * 1 + wc$wc2[i] * 2 + wc$wc3[i] * 3 + wc$wc4[i] * 4 + wc$wc5[i] * 5)/(wc$wc0[i]+wc$wc1[i]+wc$wc2[i]+wc$wc3[i]+wc$wc4[i]+wc$wc5[i])
}
## road surface condition
rsc0 <- sqldf('select Accident_date, COUNT(road_surface_condition) as rsc0 from df where road_surface_condition=0 group by Accident_date')
rsc1 <- sqldf('select Accident_date, COUNT(road_surface_condition) as rsc1 from df where road_surface_condition=1 group by Accident_date')
rsc2 <- sqldf('select Accident_date, COUNT(road_surface_condition) as rsc2 from df where road_surface_condition=2 group by Accident_date')
rsc3 <- sqldf('select Accident_date, COUNT(road_surface_condition) as rsc3 from df where road_surface_condition=3 group by Accident_date')
rsc4 <- sqldf('select Accident_date, COUNT(road_surface_condition) as rsc4 from df where road_surface_condition=4 group by Accident_date')
rsc5 <- sqldf('select Accident_date, COUNT(road_surface_condition) as rsc5 from df where road_surface_condition=5 group by Accident_date')
rsc6 <- sqldf('select Accident_date, COUNT(road_surface_condition) as rsc6 from df where road_surface_condition=6 group by Accident_date')
rsc7 <- sqldf('select Accident_date, COUNT(road_surface_condition) as rsc7 from df where road_surface_condition=7 group by Accident_date')

##merge counted data
rsc <- merge(rsc0,rsc1,by='Accident_date', all=TRUE)
rsc <- merge(rsc,rsc2,by='Accident_date',all=TRUE)
rsc <- merge(rsc,rsc3,by='Accident_date',all=TRUE)
rsc <- merge(rsc,rsc4,by='Accident_date',all=TRUE)
rsc <- merge(rsc,rsc5,by='Accident_date',all=TRUE)
rsc <- merge(rsc,rsc6,by='Accident_date',all=TRUE)
rsc <- merge(rsc,rsc7,by='Accident_date',all=TRUE)
rsc[is.na(rsc)] <- 0
## calculate the index
for(i in 1:nrow(rsc)){
  rsc$rscindex[i] <- (rsc$rsc0[i] * 0 + rsc$rsc1[i] * 1 + rsc$rsc2[i] * 2 + rsc$rsc3[i] * 3 + + rsc$rsc4[i] * 4 + rsc$rsc5[i] * 5 + rsc$rsc6[i] * 6 + rsc$rsc7[i] * 7)/(rsc$rsc0[i]+rsc$rsc1[i]+rsc$rsc2[i]+rsc$rsc3[i]+rsc$rsc4[i]+rsc$rsc5[i]+rsc$rsc6[i]+rsc$rsc7[i])
}
## On-scene Road markings
Osrm1 <- sqldf('select Accident_date, COUNT(Road_markings) as Osrm1 from df where Road_markings=1 group by Accident_date')
Osrm0 <- sqldf('select Accident_date, COUNT(Road_markings) as Osrm0 from df where Road_markings=0 group by Accident_date')

##merge counted data
Osrm <- merge(Osrm1,Osrm0,by='Accident_date', all=TRUE)

Osrm[is.na(Osrm)] <- 0
## calculate the index
for(i in 1:nrow(Osrm)){
  Osrm$Osrmindex[i] <- (Osrm$Osrm1[i] * 1 + Osrm$Osrm0[i] * 0)/(Osrm$Osrm1[i]+Osrm$Osrm0[i])
}
## Road_catseyes
Rc1 <- sqldf('select Accident_date, COUNT(Road_catseyes) as Rc1 from df where Road_catseyes=1 group by Accident_date')
Rc0 <- sqldf('select Accident_date, COUNT(Road_catseyes) as Rc0 from df where Road_catseyes=0 group by Accident_date')

##merge counted data
Rc <- merge(Rc1,Rc0,by='Accident_date', all=TRUE)

Rc[is.na(Rc)] <- 0
## calculate the index
for(i in 1:nrow(Rc)){
  Rc$Rcindex[i] <- (Rc$Rc1[i] * 1 + Rc$Rc0[i] * 0)/(Rc$Rc1[i]+Rc$Rc0[i])
}
## Road geometric details
rgd0 <- sqldf('select Accident_date, COUNT(Road_geometric_details) as rgd0 from df where Road_geometric_details=0 group by Accident_date')
rgd1 <- sqldf('select Accident_date, COUNT(Road_geometric_details) as rgd1 from df where Road_geometric_details=1 group by Accident_date')
rgd2 <- sqldf('select Accident_date, COUNT(Road_geometric_details) as rgd2 from df where Road_geometric_details=2 group by Accident_date')
rgd3 <- sqldf('select Accident_date, COUNT(Road_geometric_details) as rgd3 from df where Road_geometric_details=3 group by Accident_date')
rgd4 <- sqldf('select Accident_date, COUNT(Road_geometric_details) as rgd4 from df where Road_geometric_details=4 group by Accident_date')
rgd <- merge(rgd0,rgd1,by='Accident_date', all=TRUE)
rgd <- merge(rgd,rgd2,by='Accident_date',all=TRUE)
rgd <- merge(rgd,rgd3,by='Accident_date',all=TRUE)
rgd <- merge(rgd,rgd4,by='Accident_date',all=TRUE)
rgd[is.na(rgd)] <- 0
## calculate the index
for(i in 1:nrow(rgd)){
  rgd$rgdindex[i] <- (rgd$rgd0[i] * 0 + rgd$rgd1[i] * 1 + rgd$rgd2[i] * 2  +  rgd$rgd3[i] * 3 +  rgd$rgd4[i] * 4)/(rgd$rgd0[i]+rgd$rgd1[i]+rgd$rgd2[i]+rgd$rgd3[i]+rgd$rgd4[i])
}
num.ac <- Num_accident$n
Roadmark <- Osrm$Osrmindex
cateyes <- Rc$Rcindex
road.geom <- rgd$rgdindex
r.surface <- rsc$rscindex
rt<- rt$rtindex
weather <- wc$wcindex
## calculate the index
index_df <- data.frame(num.ac, Roadmark, cateyes, road.geom, r.surface, rt, weather)#put all the index in one data frame
#head(index_df, n=10)
write.csv(index_df,"E:\\FYP\\index_df.csv")
#Stage 2 Fit distribution 
library(MASS)
library(COMPoissonReg)
library(fitdistrplus)
library(mpcmp)
library(kableExtra)
library(glm2)

## read data
d = data.frame(read.csv("E:\\FYP\\index_df.csv")) 
d = d[, c(2:12)]

## fit the distribution
pois.fit<-fitdist(d$num.ac,"pois")
fit.pois=rbind(as.numeric(pois.fit$estimate),as.numeric(pois.fit$sd),NA,NA,pois.fit$loglik,pois.fit$aic,pois.fit$bic)

nb.fit<-fitdist(d$num.ac,"nbinom")
fit.nb=rbind(as.numeric(nb.fit$estimate[2]),as.numeric(nb.fit$sd[2]),as.numeric(nb.fit$estimate[1]),as.numeric(nb.fit$sd[1]),nb.fit$loglik,nb.fit$aic,nb.fit$bic)

comp.fit<-glm.cmp(d$num.ac ~ 1)
sum.comp <- summary(comp.fit)
fit.comp=rbind(summary(comp.fit)$coefficients[1],summary(comp.fit)$coefficients[2],as.numeric(summary(comp.fit)$nu),NA,NA,sum.comp$aic,sum.comp$bic)


results=data.frame(cbind(fit.pois,fit.nb, fit.comp))
rownames(results)=c("par1","SE.par1","par2","SE.par2","Loklik","AIC","BIC")
colnames(results)=c("Poisson","Negative Binomial", "COM-Poisson")
kable(results, caption = "Goodness of fit based on the simulated COM-Poisson distributed data set")
comp.fit.12<-glm.cmp(d$num.ac ~ d$ac.cause+d$ac.type)
fit.comp.12=rbind(NA,NA,sum.comp$aic,sum.comp$bic)


results=data.frame(cbind(fit.comp.12))
rownames(results)=c("R^2","Loklik","AIC","BIC")
colnames(results)=c("COM-Poisson")
kable(results, caption = "COM-Poisson")


#Stage 3 All Possible Regression
install.packages("stringr")
install.packages("COMPoissonReg")
install.packages("dplyr")

library(stringr)
library(COMPoissonReg)
library(dplyr)

# read the data

d = data.frame(read.csv("C:\\Users\\HWLOK\\Desktop\\OU sem4\\FYP_new\\index_df.csv"))
d = d[, c(2, 6:10, 12)]
colnames(d) = c("Y", str_c("X", 1:6)) # rename the column names

# generate a data frame containing all the possible predictor combinations.


predictors = str_c("X", 1:6)
m = matrix(NA, nrow=6, ncol=1)
for (a in 1:6) {
  temp = as.matrix(combn(predictors, a))
  if (nrow(temp) < 6) {
    dummy = matrix(NA, nrow = 6 - nrow(temp), ncol = ncol(temp))
    temp = rbind(temp, dummy)
  }
  m = cbind(m, temp)
}
m = as.data.frame(t(m[,-1]))
print(dim(m))
all(duplicated(m)==FALSE) # there is no duplicated record => all the predictor combinations are distinct
# write.csv(m,"C:\\Users\\HWLOK\\Desktop\\OU sem4\\FYP_new\\predictors.csv", row.names = FALSE)

options(COMPoissonReg.optim.method = "SANN")

# fit all possible regression

m2 = matrix(NA, nrow=63, ncol=7)
for(i in 1:nrow(m)) {
  cat("fitting model", i, "\n")
  f = m[i, ]
  f = f[!is.na(f)]
  cmp.out = glm.cmp(reformulate(f, response = "Y"), data=d)
  m2[i, 1] = length(coef(cmp.out)) - 2
  m2[i, 2] = toString(reformulate(f, response = "Y"))
  # y.hat = predict(cmp.out, newdata=d)
  # m2[i, 3] = (sum((y.hat-mean(d$Y))^2)/sum(((d$Y-mean(d$Y))^2)))
  # m2[i, 4] = 1 - (1-as.numeric(m2[i, 3]) * (nrow(d)-1)) / (nrow(d) - as.numeric(m2[i, 1]) - 1)
  s = summary(cmp.out)
  m2[i, 5] = s$aic
  m2[i, 6] = s$bic
  m2[i, 7] = cmp.out$loglik
}

colnames(m2) = c("p", "formula", "R-Squared", "Adj. R-Squared", "AIC", "BIC", "Log Likelihood")

# write.csv(m2, "C:\\Users\\HWLOK\\Desktop\\OU sem4\FYP_new\\SANN.csv", row.names = FALSE) 

# model selection

model = data.frame(read.csv("C:\\Users\\HWLOK\\Desktop\\OU sem4\\FYP_new\\SANN.csv"))

which(model$AIC == min(model$AIC))
which(model$BIC == min(model$BIC))
which(model$Log.Likelihood == max(model$Log.Likelihood))
#Stage 4 Descriptive and Exploratory Analysis
library(dplyr)
library(tidyr) 
library(ggplot2)
library(epiDisplay) 
library(splines2) 
library(gam) 
library(readxl)
library(GeneCycle)
library(scales)
library(tidyverse)
library(sqldf)
library(GGally)

index_df= data.frame(read.csv("E:\\FYP\\index_df.csv"))

## Correlogram
cor.plot = ggpairs(index_df, title= "Correlogram")
cor.plot

##Boxplot
ggplot()+
  geom_boxplot(aes(x=index_df$num.ac))+
  xlab("Number of accident")

ggplot()+
  geom_boxplot(aes(x=index_df$r.surface))+
  xlab("Road surface")
ggplot()+
  geom_boxplot(aes(x=index_df$rt))+
  xlab("Road type")
hist(index_df$road.geom,xlab = "Road Geometric", main = "Frequency of Index road Geometric detail",col = "red")
hist(index_df$weather,xlab = "Weather", main = "Frequency of Index Weather",col = "red")
hist(index_df$Roadmark,xlab = "On-Scene Roadmark", main = "Frequency of Index On Scene Roadmark",col = "red")
hist(index_df$cateyes,xlab = "Road Cat eyes", main = "Frequency of Index Road Cat eyes",col = "red") ##scatter plot
ggplot()+
  geom_point(aes(y=index_df$num.ac,x=index_df$ac.cause))+
  ylab("Number of accident")+
  xlab("Accident Cause")
ggplot()+
  geom_point(aes(y=index_df$num.ac,x=index_df$ac.type))+
  ylab("Number of accident")+
  xlab("Accident type")
ggplot()+
  geom_point(aes(y=index_df$num.ac,x=index_df$dam.RT))+
  ylab("Number of accident")+
  xlab("Damage of road type")
ggplot()+
  geom_point(aes(y=index_df$num.ac,x=index_df$Roadmark))+
  ylab("Number of accident")+
  xlab("On-secene Road Markings")
ggplot()+
  geom_point(aes(y=index_df$num.ac,x=index_df$cateyes))+
  ylab("Number of accident")+
  xlab("Road catseyes")
ggplot()+
  geom_point(aes(y=index_df$num.ac,x=index_df$road.geom))+
  ylab("Number of accident")+
  xlab("Road geomtric details")
ggplot()+
  geom_point(aes(y=index_df$num.ac,x=index_df$r.surface))+
  ylab("Number of accident")+
  xlab("Road surface")
ggplot()+
  geom_point(aes(y=index_df$num.ac,x=index_df$rt))+
  ylab("Number of accident")+
  xlab("Road type")
ggplot()+
  geom_point(aes(y=index_df$num.ac,x=index_df$Vehicle.t))+
  ylab("Number of accident")+
  xlab("Vehicle type")
ggplot()+
  geom_point(aes(y=index_df$num.ac,x=index_df$weather))+
  ylab("Number of accident")+
  xlab("Weather")

