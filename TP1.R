ozone=read.table("ozone.txt",header=T)

plot(ozone$T12,ozone$maxO3)

cor(ozone$T12, ozone$maxO3)

reg<-lm(maxO3~T12,data=ozone)

summary(reg)

lm(formula = maxO3 ~ T12, data = ozone)

abline(reg,col="red")

confint(reg)

par(mfrow=c(2,2))

plot(reg)

residus=residuals(reg)

shapiro.test(residus)
