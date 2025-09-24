# read one file
d1<-read.csv(file.choose())
d1
t<-d1[,1]      #year
prd_b<-d1[,2]
dc_b<-d1[,3]
exp_b<-d1[,4]
GDP_b<-d1[,5]
ppl_b<-d1[,6]


#To plot time-series graphs
plot(t, prd_b, main="Line diagram showing coffee production of Brazil over the years 2000-
2019", xlab="Years", ylab="Production(data in thousand 60kg bags)",type="o")

#To plot multiple bar diagram showing coffee production, domestic consumption and export
barplot(height=rbind(production,consumption,export),
beside = TRUE,names.arg=t,
main="Multiple bar diagram showing coffee production, consumption and export of Brazil
over the years 2000-2019",
xlab="Year",ylab="Data in thousand 60kg bags",
col=c("yellow","purple","cyan"),
legend.text = c("Production" , "Domestic consumption" , "Export"),
args.legend = list(x = "topleft"))

# To plot component bar diagram showing coffee domestic consumption and export
d<-rbind(d1[1,],d1[6,],d1[11,],d1[16,],d1[20,]) #d1 contains the dataset for BRAZIL
t1<-d[,1] #t1 contains year
barplot(height=rbind(d$dc_b,d$exp_b),
names.arg=t1,
main="Component bar diagram showing coffee consumption and export of Brazil over the
years 2000-2019",
xlab="Year",ylab="Data in thousand 60kg bags",
col=c("lavender","lightblue"),
legend.text = c("Domestic consumption" , "Export"),
args.legend = list(x = "topleft"))

#fitting trend equation
fit<-lm(exp_b~t)
plot(t, exp_b, main="Graph showing Brazil's coffee export and fitted trend line",
xlab="Years", ylab="Coffee export", type="o")
abline(fit, col='red')
legend("topleft", legend = c("Observed value", "Trend line"),
pch=15,bty="n" , col=c("black", "red"))
axis(1,d1$Year)

#To plot Coffee export-Coffee production scatter plot
plot(prd_b, exp_b, main="Coffee export-Coffee production scatter plot", xlab="Coffee
production", ylab="Coffee export")

#Fitting linear regression
fit_sr<-lm(exp_b~prd_b)
summary(fit_sr)

#To plot multiple line diagram showing observed and fitted values
p<-predict(fit_sr, newdata = data.frame(prd_b=prd_b)) #storing the fitted values
plot(t, p, main="Multiple line diagram showing observed and fitted values of India's coffee
export", xlab="Years", ylab="Coffee export", col="blue", type="o")
lines(t, exp_b, col="red", type = "o")
legend("topleft", legend = c("Observed value","Fitted value"),
pch=15,bty="n" , col=c("red","blue"))
axis(1,t)

#Fitting second order polynomial regression
fit_pr<-lm(exp_v~poly(prd_v,2,raw=TRUE))

#Fitting multiple linear regressionfit_mlr<-lm(GDP_v~ppl_v+exp_v+dc_v)
summary(fit_mlr)

#Checking multicollinearity
library(mctest)
imcdiag(fit_mlr)

#Finding principal components using PCA
ds<-d1[,2:6] #ds contain a subset of the dataset
head(ds)
d_nm<-scale(ds) #standardizing ds
head(d_nm)
dpca<-prcomp(d_nm) #to find PCs
dpca #shows the PCs
summary(dpca) #shows variance and cumulative proportion of the PCs

#To plot scree plot
dpca$sdev # Compute standard deviation
dpca.var <- dpca$sdev ^ 2 # Compute variance
propve <- dpca.var / sum(dpca.var) # Proportion of variance for a scree plot
plot(propve, xlab = "Principal component",
ylab = "Proportion of Variance Explained",
ylim = c(0, 1), type = "b",
main = "Scree Plot")