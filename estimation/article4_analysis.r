
### SEM model to determine the validity of the construct we have chosen for internal and external
### We add the lambdas from this to the next chunk to build weighted indexes

model <- "

external =~   + Q16a + Q21c +Q21e +Q21d + Q19e
internal =~  + Q18b  + Q25 + Q25a + Q25c +Q26d + Q26f

###internal ~ external
spinoff ~ external + internal + lnalder + lnintfou + lnsize

"



fit <- sem(model, data = spindata, ordered="spinoff")
summary(fit,fit.measures=TRUE)



### Get lamda from CFA to build new variable for regression

semres <- parameterEstimates(fit)
extparam <- with(semres, semres[ lhs == 'external' & op == '=~',])$est
intparam <- with(semres, semres[ lhs == 'internal' & op == '=~',])$est
extvar <- with(semres, semres[ lhs == 'external' & op == '=~',])$rhs
intvar <- with(semres, semres[ lhs == 'internal' & op == '=~',])$rhs

extdata <- spindata[,extvar]
intdata <- spindata[,intvar]

spindata$ext <- rowMeans(data.frame(mapply(`*`,extdata,extparam)),na.rm=TRUE)
spindata$int <- rowMeans(data.frame(mapply(`*`,intdata,intparam)),na.rm=TRUE)
spindata$lext <- log(spindata$ext)
spindata$lint <- log(spindata$int)
spindata$colcust <- ifelse(spindata$Q24a >3,1,0)
spindata$colsup <- ifelse(spindata$Q24b >3,1,0)
spindata$coluni <- ifelse(spindata$Q24c >3,1,0)


### Making some tables with the variables we use in our constructs
exttab <- qnames[which(qnames$q %in% extvar),]
exttab$type <- "external"
inttab <- qnames[which(qnames$q %in% intvar),]
inttab$type <- "internal"
ivtab <- rbind(exttab,inttab)



### Base regression models

mod1 <- glm(spinoff~ ext + int + lnalder + lnsize + lnintfou + factor(sic) + factor(reg), data=spindata, family="poisson");summary(mod1)
mod2 <- glm(spinoff~ ext + int + lnalder + lnsize + lnintfou + hcap + factor(sic) + factor(reg), data=spindata, family="poisson");summary(mod2)
mod3 <- glm(spinoff~ ext + int + orgdummy + lnalder + lnsize + lnintfou + hcap  + factor(sic) + factor(reg), data=spindata, family="poisson");summary(mod3)
mod4 <- glm(spinoff~ ext + int + orgdummy + fousam + lnalder + lnsize + lnintfou + hcap + factor(sic) + factor(reg), data=spindata, family="poisson");summary(mod3)
mod5 <- glm(spinoff~ ext + int*orgdummy + lnalder + lnsize + lnintfou + hcap +fousam +factor(sic) + factor(reg) , data=spindata, family="poisson");summary(mod5)
mod6 <- glm(spinoff~ ext*orgdummy + int + lnalder + lnsize + lnintfou + hcap +fousam +factor(sic) + factor(reg), data=spindata, family="poisson");summary(mod6)
mod7 <- glm(spinoff~ (ext + int)*fousam + orgdummy + lnalder + lnsize + lnintfou +hcap + factor(sic) + factor(reg), data=spindata, family="poisson");summary(mod7)



modlist1 <- list(mod1,mod2,mod3,mod4,mod5,mod6,mod7)

r1 <- round(PseudoR2(mod1)[4],3)
r2 <- round(PseudoR2(mod2)[4],3)
r3 <- round(PseudoR2(mod3)[4],3)
r4 <- round(PseudoR2(mod4)[4],3)
r5 <- round(PseudoR2(mod5)[4],3)
r6 <- round(PseudoR2(mod6)[4],3)
r7 <- round(PseudoR2(mod7)[4],3)



### summ function explored
### https://cran.r-project.org/web/packages/jtools/vignettes/summ.html

##### TESTING DIFFERENTIATED RD COLLABORATION



### Wrapper to test different cut offs
wrapres <- function(cutoff){
spindata$colcust <- ifelse(spindata$Q24a >cutoff,1,0)
spindata$colsup <- ifelse(spindata$Q24b >cutoff,1,0)
spindata$coluni <- ifelse(spindata$Q24c >cutoff,1,0)



modA <- glm(spinoff~ (ext + int) + orgdummy + lnalder + lnsize + lnintfou +hcap +colsup + factor(sic) + factor(reg), data=spindata, family="poisson")
modB <- glm(spinoff~ (ext + int) + orgdummy + lnalder + lnsize + lnintfou +hcap +colsup + coluni+ factor(sic) + factor(reg), data=spindata, family="poisson")
modC <- glm(spinoff~ ext + int+ orgdummy + lnalder + lnsize + lnintfou +hcap + coluni+ factor(sic) + factor(reg), data=spindata, family="poisson")
modD <- glm(spinoff~ ext*colsup + int + orgdummy + lnalder + lnsize + lnintfou +hcap +colsup +factor(sic) + factor(reg), data=spindata, family="poisson")
modE <- glm(spinoff~ ext*coluni + int + orgdummy + lnalder + lnsize + lnintfou +hcap  + coluni+ factor(sic) + factor(reg), data=spindata, family="poisson")
modF <- glm(spinoff~ ext + int*colsup + orgdummy + lnalder + lnsize + lnintfou +hcap + colsup+ factor(sic) + factor(reg), data=spindata, family="poisson")
modG <- glm(spinoff~ ext + int*coluni + orgdummy + lnalder + lnsize + lnintfou +hcap + coluni+ factor(sic) + factor(reg), data=spindata, family="poisson")
modH <- glm(spinoff~ (ext+int)*coluni  + orgdummy + lnalder + lnsize + lnintfou +hcap +colsup + coluni+ factor(sic) + factor(reg), data=spindata, family="poisson")
modI <- glm(spinoff~ (ext+int)*colsup  + orgdummy + lnalder + lnsize + lnintfou +hcap +colsup + coluni+ factor(sic) + factor(reg), data=spindata, family="poisson")
modJ <- glm(spinoff~ (ext+int)*colsup  + (ext+int)*coluni + orgdummy + lnalder + lnsize + lnintfou +hcap +colsup + coluni+ factor(sic) + factor(reg), data=spindata, family="poisson")

modlistres <-list(modA,modB,modC,modD,modE,modF,modG,modH,modI,modJ)
r2list <- list(r1s = round(PseudoR2(modA)[4],3),
               r2s = round(PseudoR2(modB)[4],3),
               r3s = round(PseudoR2(modC)[4],3),
               r4s = round(PseudoR2(modD)[4],3),
               r5s = round(PseudoR2(modE)[4],3),
               r6s = round(PseudoR2(modF)[4],3),
               r7s = round(PseudoR2(modG)[4],3),
               r8s = round(PseudoR2(modH)[4],3),
               r9s = round(PseudoR2(modI)[4],3),
               r10s = round(PseudoR2(modJ)[4],3))

return(list(modlistres,r2list))
}

modlist2a <- wrapres(3)
modlist2b <- wrapres(4)
modlist2c <- wrapres(5)

ml1 <- modlist2a[[1]]
r1 <- as.numeric(unlist(modlist2a[[2]]))

ml2 <- modlist2b[[1]]
r2 <- as.numeric(unlist(modlist2b[[2]]))

ml3 <- modlist2c[[1]]
r3 <- as.numeric(unlist(modlist2c[[2]]))

### What if we use the whole panel of projects


sfall <-  merge(sfmain, zw0, by.x=c("Organisasjonsnummer"), by.y=c("orgnr"), all.x=TRUE)
sfall <- merge(sfall, spindata, by.x=c("Organisasjonsnummer"), by.y=c("orgnr"), all.x=TRUE)
sfall$sambb <- ifelse(sfall$sambb==1,1,0)
sfall$sambf <- ifelse(sfall$sambf==1,1,0)
sfall$sambb[is.na(sfall$sambb)] <- 0
sfall$sambf[is.na(sfall$sambf)] <- 0

foo <- subset(sfall,!is.na(sfall$ext))
foo$spin <- foo$Videreringspinoff

nrow(foo)
foo$sam <- ifelse(foo$Har.FoU.samarbeid=="Ja",1,0)

mod1fa <- glm(spin ~ ext + int +  orgdummy.x + lnalder + lnsize.y + lnintfou +hcap + factor(sic) + factor(reg), data=foo, family="binomial")

mod1fb <- glm(spin ~ ext + int +  orgdummy.x*sambf + sambb + lnalder + lnsize.y + lnintfou +hcap + factor(sic) + factor(reg), data=foo, family="binomial")

mod1fc <- glm(spin ~ ext + int +  orgdummy.x*sambb + sambf + lnalder + lnsize.y + lnintfou +hcap + factor(sic) + factor(reg), data=foo, family="binomial")

mod1fd <- glm(spin ~ ext + int +  orgdummy.x*(sambb + sambf) + lnalder + lnsize.y + lnintfou +hcap + factor(sic) + factor(reg), data=foo, family="binomial")




mod2fa <- glm(spin ~ ext + int + sambb + sambf +  orgdummy.x  + lnalder + lnsize.y + lnintfou +hcap + factor(sic) + factor(reg) , data=foo, family="binomial")

mod2fb <- glm(spin ~ ext + int*sambb + sambf +  orgdummy.x  + lnalder + lnsize.y + lnintfou +hcap + factor(sic) + factor(reg) , data=foo, family="binomial")

mod2fc <- glm(spin ~ ext + int*sambf + sambb +  orgdummy.x  + lnalder + lnsize.y + lnintfou +hcap + factor(sic) + factor(reg) , data=foo, family="binomial")

mod2fd <- glm(spin ~ ext + int*(sambb + sambf) +  orgdummy.x  + lnalder + lnsize.y + lnintfou +hcap + factor(sic) + factor(reg) , data=foo, family="binomial")


mod3fa <- glm(spin ~ ext*sambb + sambf + int +  orgdummy.x  + lnalder + lnsize.y + lnintfou +hcap + factor(sic) + factor(reg), data=foo, family="binomial")

mod3fb <- glm(spin ~ ext*sambf + sambb + int +  orgdummy.x  + lnalder + lnsize.y + lnintfou +hcap + factor(sic) + factor(reg), data=foo, family="binomial")

mod3fc <- glm(spin ~ ext*(sambb + sambf) + int +  orgdummy.x  + lnalder + lnsize.y + lnintfou +hcap + factor(sic) + factor(reg), data=foo, family="binomial")


modlistf1 <- list(mod1fa,mod1fb,mod1fc,mod1fd)
modlistf2 <- list(mod2fa,mod2fb,mod2fc,mod2fd,mod3fa,mod3fb,mod3fc)


r2listf1 <- list(round(PseudoR2(mod1fa)[4],3),
                 round(PseudoR2(mod1fb)[4],3),
                 round(PseudoR2(mod1fc)[4],3),
                 round(PseudoR2(mod1fd)[4],3))

r2listf2 <- list(round(PseudoR2(mod2fa)[4],3),
                 round(PseudoR2(mod2fb)[4],3),
                 round(PseudoR2(mod2fc)[4],3),
                 round(PseudoR2(mod2fd)[4],3),
                 round(PseudoR2(mod3fa)[4],3),
                 round(PseudoR2(mod3fb)[4],3),
                 round(PseudoR2(mod3fc)[4],3))











mod1 <- glm(spin ~ ext*(colsup + coluni) + int +  orgdummy.x + lnalder + lnsize.y + lnintfou +hcap , data=foo, family="binomial");summary(mod1)
mod1 <- glm(spin ~ ext+(colsup + coluni)*int +  orgdummy.x + lnalder + lnsize.y + lnintfou +hcap + factor(sic) + factor(reg) , data=foo, family="binomial");summary(mod1)

mod1 <- glm(spin ~ (ext + int)*orgdummy.x + lnalder + lnsize.y + lnintfou +hcap + factor(sic) + factor(reg), data=foo, family="binomial");summary(mod1)


mod1 <- glm(spin ~ ext + int*sam + orgdummy.x + lnalder + lnsize.y + lnintfou +hcap+ factor(sic) + factor(reg), data=foo, family="binomial");summary(mod1)

library(lme4)

mod1 <- glmer(spin ~ ext + int+ orgdummy.x*(sambb + sambf) + lnalder + lnsize.y + lnintfou +hcap+ (1|sic) + (1|reg) +  (1|Organisasjonsnummer), data=foo);summary(mod1)
mod1 <- glmer(spin ~ int + ext*sam + orgdummy.x + lnalder + lnsize.y + lnintfou +hcap+ (1|sic) + (1|reg), data=foo);summary(mod1)
mod1 <- glmer(spin ~ (ext + int)*sam + (1|sic) + (1|reg), family="binomial", data=foo);summary(mod1)



### RUN UNTIL THIS POINT


mod8 <- glm(spinoff~ ext+colcust*int + orgdummy + alder + lnsize + lnintfou +hcap + factor(sic) + factor(reg), data=spindata, family="poisson");summary(mod8)

mod9 <- glm(spinoff~ ext + colsup*int + orgdummy + alder + lnsize + lnintfou +hcap + factor(sic) + factor(reg), data=spindata, family="poisson");summary(mod9)

mod10 <- glm(spinoff~ ext + coluni*int + orgdummy + alder + lnsize + lnintfou +hcap + factor(sic) + factor(reg), data=spindata, family="poisson");summary(mod10)

mod11 <- glm(spinoff~ ext*colcust + int + orgdummy + alder + lnsize + lnintfou +hcap + factor(sic) + factor(reg), data=spindata, family="poisson");summary(mod11)

mod12 <- glm(spinoff~ ext*colsup + int + orgdummy + alder + lnsize + lnintfou +hcap + factor(sic) + factor(reg), data=spindata, family="poisson");summary(mod12)

mod13 <- glm(spinoff~ ext*coluni + int + orgdummy + alder + lnsize + lnintfou +hcap + factor(sic) + factor(reg), data=spindata, family="poisson");summary(mod13)




par(mfrow=c(2,3))

meplot(model=mod8,var1="int",var2="colcust", xlab="R&D with customers", ylab="Effect of Internal opportunities")
meplot(model=mod9,var1="int",var2="colsup",xlab="R&D with suppliers", ylab="Effect of Internal opportunities")
meplot(model=mod10,var1="int",var2="coluni",xlab="R&D with universities", ylab="Internal opportunities")

meplot(model=mod11,var1="ext",var2="colcust", xlab="R&D with customers", ylab="Effect of External opportunities n.s")
meplot(model=mod12,var1="ext",var2="colsup",xlab="R&D with suppliers", ylab="Effect of External opportunities n.s")
meplot(model=mod13,var1="ext",var2="coluni",xlab="R&D with universities", ylab="Effect of External opportunities n.s")








### plotting marginal effet




## pl1 <- interplot(m = mod8, var1 = "int", var2 = "colcust",ylim = c(0,1))
## pl2 <- interplot(m = mod9, var1 = "int", var2 = "colsup")
## pl3 <- interplot(m = mod10, var1 = "int", var2 = "coluni")
## grid.arrange(pl1,pl2,pl3, ncol=3)

## sl1 <- pl1[2]/pl1[1]-1
## sl1

## pl1 <- meplot(model=mod8,var1="int",var2="colcust",rnum=TRUE)
## pl2 <- meplot(model=mod9,var1="int",var2="colsup",rnum=TRUE)
## pl3 <- meplot(model=mod10,var1="int",var2="coluni",rnum=TRUE)



## index(pl1)





## lines(meplot(model=mod9,var1="int",var2="colsup"))


## head(pl2)

## ### FINDINGS> RD collaboration level variabel not working

## mod1 <- glm(spinoff~  Q28b, data=spindata, family="poisson");summary(mod1)



## RD centralization is more human capital / more rd workers are expensive
## Importance of RD to the firm
## Level of where RD is performance *organizational level RD / the higher level the more important

## If yu are high level you can translate these into more projects so more you can generalize more oppostyintes fro the external environment and hence makes it more likely to genreate internal opportunities. Are these firms in one location?
## y

## SOmething about the org structure ??  in skattefunn
## Multiple locations / if only one location





## ### orgdummy=1 means they have their own RD unit (centralized), 0 means the RD is in the business unit / part of operations


## #### NEED TO EXPLORE MISSING







