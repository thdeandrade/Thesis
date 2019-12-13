#### DATA ######

# BEFORE START, DO NOT FORGET TO CHANGE THE SEARCH PATH #

setwd("C:\\[A]Aerogeofisico\\#ARTICLE_RF_POP [in preparation]\\RandomForest_WPP\\TESTES5")

pop<-read.table("pop7.txt", head=T)
attach(pop)
head(pop)

#### CLAY PREDICTION ######

###### RANDOM FOREST ###

#To install the ithir package, it is recommended to use the following function:#
#install.packages("devtools")
#library(devtools)
#install_bitbucket("brendo1001/ithir/pkg")

library(randomForest)
library(ithir)
library(raster)

set.seed(123)
training <- sample(nrow(pop), 0.7 * nrow(pop))

# fit the model
pop.RF.Exp <- randomForest(Clay ~ Soil+Geo+Aeromag+Precip+
                             TWI+Elev,
                           data = pop[training, ],
                           importance = TRUE, ntree = 1000)
#str(pop.RF.Exp)
#treesize(pop.RF.Exp)

print(pop.RF.Exp)
#importance(pop.RF.Exp)

varImpPlot(pop.RF.Exp)
#varImpPlot(pop.RF.Exp, type=2)

# Internal validation
RF.pred.C <- predict(pop.RF.Exp, newdata = pop[training, ])
goof(observed = pop$Clay[training], predicted = RF.pred.C)

#write.table(RF.pred.C,"RF.IV.txt",quote=F,row.names = T)

# External validation
RF.pred.V <- predict(pop.RF.Exp, newdata = pop[-training, ])
goof(observed = pop$Clay[-training], predicted = RF.pred.V)

#write.table(RF.pred.V,"RF.EV.txt",quote=F,row.names = T)

##### CREATING A MAP #####

map.RF <- predict(pop.RF.Exp, newdata = pop)
mat<-cbind(data.frame(pop$x,pop$y, map.RF))
#write.table(mat,"mat.txt",quote=F,row.names = F)

# Variographic analysis
library(sp)
library(maptools)
library(raster)
library(gstat)
library(graphics)
library(lattice)

mat
names(mat)
coordinates(mat)=~ pop.x+pop.y
form<-map.RF~1
v.arg.RF<-variogram(form,data=mat)
#x11()
plot(v.arg.RF,pl=F,pch=16,col=1)

# Modeling
m.arg.RF <- fit.variogram(v.arg.RF,vgm(6000,"Sph",150000,2000))
m.arg.RF
sqr<-attr(m.arg.RF, "SSErr")
sqr
#x11()
plot(v.arg.RF,model=m.arg.RF, col=1,pl=F,pch=16,main ="Clay variogram by Random Forest
     \n esf(1869.688;5052.787;183279.3;2.219)")
          #(nug+psill; sph+psill; sph+range; sqr)

# Creating GRID
x<-mat$pop.x
y<-mat$pop.y
dis <- (max(x)-min(x))/109 #Dist?ncia entre pontos
grid <- expand.grid(X=seq(min(x),max(x),dis), Y=seq(min(y),max(y),dis))
gridded(grid) = ~ X + Y
#plot(grid)

# Creating CONTOUR
contorno<-read.table("contorno.pop.txt", head=T)

p = Polygon(contorno)
ps = Polygons(list(p),1)
contorno = SpatialPolygons(list(ps))
#plot(contorno)

# Ordinary Kriging (nsim=0)
ko.arg.RF<-krige(formula=form, mat, grid, model=m.arg.RF,
                 nmin=5,
                 nmax=20,
                 block=c(0,0),
                 nsim=0,
                 na.action=na.pass,
                 debug.level=-1)

# Making kriging file conversions
ko.arg.RF <- as.data.frame(ko.arg.RF)

# ATENTION FOR THIS LINE!!
#write.table(ko.arg.RF,"ko.arg.RF.txt",quote=F,row.names = F)
# This is important for do de error map

head(ko.arg.RF)
coordinates(ko.arg.RF)=~X+Y #conversion
gridded(ko.arg.RF)=TRUE #conversion
ko.arg.RF <- raster(ko.arg.RF) #conversion
head(ko.arg.RF)

# Printing the map of values predicted by RF
ko.arg.RF <- mask(ko.arg.RF, contorno, inverse=FALSE)
#spplot(ko.arg.RF)
#x11()
plot(ko.arg.RF,main="Predicted Clay by Random Forest",
     xlab="Latitude",ylab="Longitude")
plot(contorno, add=T)
contour(ko.arg.RF, add=T, nlevels = 6)

#### CREATING AN ERROR MAP ####

Error<-read.table("ko.arg.RF.txt", head=T)
Error<-Error[,c(1:2,4)]
head(Error)

ko.arg.RF.Error <- as.data.frame(Error)
head(ko.arg.RF.Error)
coordinates(ko.arg.RF.Error)=~X+Y #conversion
gridded(ko.arg.RF.Error)=TRUE #conversion
ko.arg.RF.Error <- raster(ko.arg.RF.Error) #conversion
head(ko.arg.RF.Error)

# Printing the Error map
ko.arg.RF.Error <- mask(ko.arg.RF.Error, contorno, inverse=FALSE)
#spplot(ko.arg.RF.Error)
#x11()
plot(ko.arg.RF.Error,main="Variance Map",
     xlab="Latitude",ylab="Longitude")
plot(contorno, add=T)
contour(ko.arg.RF.Error, add=T, nlevels = 6)

#### STEPWISE ####

pop.MLR.Full <- lm(Clay ~ Soil+Geo+Aeromag+Precip+TWI,
                   data = pop)

summary(pop.MLR.Full)

pop.MLR.Step <- step(pop.MLR.Full, trace = 0, direction="both")
summary(pop.MLR.Step)

##### VIF ####

#install.packages("olsrr")
library(olsrr)
model <- lm(Clay ~ Soil+Geo+Aeromag+Precip+TWI,
            data = pop)
ols_vif_tol(model)

# END #
