library(foreign)
library(stringr)
library(Hmisc)
library(reshape2)
library(dplyr)
library(broom)
library(tableHTML)
library(condformat)
library(factoextra)
library(stargazer)
library(xtable)
library(pixiedust)
library(RColorBrewer)
library(glmnet)
library(mice)
library(basicTrendline)


require(lavaan)
require(semPlot)

library(GPArotation)
library(psych)
library(polycor)
require(gridExtra)
require(BaylorEdPsych)


#This takes a long time to run / about 10 minutes
                                        source("/Users/larshovdanmolden/Documents/gitrep/script/skattefunn_data.r")
                                        #save.image("art4.RData")
load("/Users/larshovdanmolden/Documents/Data/skattefunn/sf/art4.RData")
                                        #save.image("/Users/larshovdanmolden/Documents/Data/skattefunn/sf/art4base.RData")
#load("/Users/larshovdanmolden/Documents/Data/skattefunn/sf/art4findata.RData")
#save.image("/Users/larshovdanmolden/Documents/Data/skattefunn/sf/art4findata.RData")

Sys.setlocale("LC_ALL", 'en_US.UTF-8')
#Sys.setenv(LC_ALL="en_US.UTF-8")
setwd("/Users/larshovdanmolden/Documents/Data/Article_4")

qnames <- data.frame(read.csv("qnames.csv", sep=";", stringsAsFactors=FALSE, header=FALSE));colnames(qnames) <- c("q","desc")



setwd("/Users/larshovdanmolden/Documents/Data/Article_3")


# Importing and cleaning email address and the project number
regdataraw = read.spss("regdata_tommy.sav", to.data.frame=TRUE, labels=F)
regdatazw <- regdataraw[,c("epost","prosjektnummer")] #could add ny_epost too
regdatazw$epost <- str_trim(as.character(regdatazw$epost))
names(regdatazw) <- c("email","pnr")

# Importing Skattefunn data // UNDER DEVELOPMENT
regdatasf <- sfmain[,c("Prosjektnummmer","Organisasjonsnummer")]
names(regdatasf) <- c("pnr","orgnr") #orgnr is a unique identifier for each firm / same for pnr at project level

datazw = read.csv("skattefunn_10_clean.csv", sep=";",dec=",", header=T)
#datazw = read.delim("skattefunn_10_year_test.dat", header=T)
#head(datazw)

#datazw = read.spss("skattefunn_10_year_test.sav", to.data.frame=TRUE, use.value.labels=T)
md <- datazw[c("Resp","dc_t0","dc_t1")]
md$Resp <- str_trim(as.character(md$Resp))
colnames(md) <- c("email","dc0","dc1")

# NB / regdf comes from skattefunn_data.r contains all projects, not only endreport

matchvec <- match(md$email,regdf$email)
regdata <- cbind(md,regdf[matchvec,])
datazw <- cbind(datazw,regdata)

#Splitting datazw in first wave and second wave
zw0 <- data.frame(orgnr=datazw$orgnr);zw0$yr <- 2004
zw1 <- data.frame(orgnr=datazw$orgnr);zw1$yr <- 2014

zw0[,3:117] <- datazw[,2:116]
zw1[,3:70] <- datazw[,117:184];zw1 <- zw1[1:263,]


### ADD PROJECT RESULTS EACH YEAR - PRODUCT LAUNCHES PR YEAR PR FIRM

#sfmain has the end reports
#datazw has the register information and the DC data
year <- c(seq(2002,2015,1)) ; org <- unique(datazw$orgnr)

#This dataset contains a panel of projects for each firm laid in time. Financial data to be added to this dataset
sfp <- expand.grid(orgnr=org, year = year)

sfp <- merge(sfp, sfmain, by.x=c("orgnr","year"), by.y=c("Organisasjonsnummer","ar_til"), all.x=TRUE)


regnskapadj <- regnskap[ , !(names(regnskap) %in% c("ansatte"))]
sfp <- merge(sfp, regnskapadj, by.x=c("orgnr","year"), by.y=c("orgnr","yr"), all.x=TRUE)

sfp$timepnr <- paste(sfp$orgnr,sfp$Prosjektnummmer,sfp$year)
sfp <- sfp[!duplicated(sfp$timepnr),]
sfp <- merge(sfp, zw0, by.x=c("orgnr","year"), by.y=c("orgnr","yr"), all.x=TRUE)
sfp$fousam <- ifelse(sfp$Har.FoU.samarbeid=="Ja",1,0)


print("resulting data frame is the skattefunn panel - sfp")





### Making cross sections based on the first DC obs

exvec <- c("Videreringspinoff",
           "Nyeprodtjenoppn",
           "Nyeprodtjenforv",
           "Nyeprosoppn",
           "Nyeprosforv",
           "Patoppn",
           "Patforv",
           "phdoppn")

namvec <- c("spinoff",
           "npra",
           "npre",
           "npoa",
           "npoe",
           "pata",
           "pate",
           "phda")

################ NBNBNBNB THIS WAS SUM BEFORE

extsfres <- function(var,nam){

    res <- data.frame(sfp  %>% dplyr::group_by(orgnr) %>% dplyr::summarise(sum(!! var,na.rm=TRUE)))
    colnames(res) <- c("orgnr",nam)

    return(res)
    }



extres <- list()
for(i in 1:length(exvec)){

    extres[[i]] <- extsfres(quo(get(exvec[i])),namvec[i])
}

newprod <- do.call("cbind",extres)
newprod <- newprod[c(1,2,4,6,8,10,12,14,16)]
newprod <- merge(newprod, zw0,by="orgnr")
newprod[newprod == -1] <- NA

### READY FOR ANALYSIS
## making large correlation matrix

npracor <- cbind(newprod[,13:124], newprod$npra)
nprecor <- cbind(newprod[,13:124], newprod$npre)
npoacor <- cbind(newprod[,13:124], newprod$npoa)
npoecor <- cbind(newprod[,13:124], newprod$npoe)
patacor <- cbind(newprod[,13:124], newprod$pata)
patecor <- cbind(newprod[,13:124], newprod$pate)
spincor <- cbind(newprod[,13:124], newprod$spinoff)

npracor <- cbind(newprod$orgnr, newprod$npra,newprod[,13:124])
colnames(npracor)[1:2] <- c("orgnr","npra")
nprecor <- cbind(newprod$orgnr, newprod$npre,newprod[,13:124])
colnames(nprecor)[1:2] <- c("orgnr","npre")
npoacor <- cbind(newprod$orgnr, newprod$npoa,newprod[,13:124])
colnames(npoacor)[1:2] <- c("orgnr","npoa")
npoecor <- cbind(newprod$orgnr, newprod$npoe,newprod[,13:124])
colnames(npoecor)[1:2] <- c("orgnr","npoe")
patacor <- cbind(newprod$orgnr, newprod$pata,newprod[,13:124])
colnames(patacor)[1:2] <- c("orgnr","pata")
patecor <- cbind(newprod$orgnr, newprod$pate,newprod[,13:124])
colnames(patecor)[1:2] <- c("orgnr","pate")
spincor <- cbind(newprod$orgnr, newprod$spinoff,newprod[,13:124])
colnames(spincor)[1:2] <- c("orgnr","spin")


dfnpra <- melt(data.frame(npracor),id.vars = "npra")
dfnpre <- melt(data.frame(nprecor),id.vars = "npre")
dfnpoa <- melt(data.frame(npoacor),id.vars = "npoa")
dfnpoe <- melt(data.frame(npoecor),id.vars = "npoe")
dfpata <- melt(data.frame(patacor),id.vars = "pata")
dfpate <- melt(data.frame(patecor),id.vars = "pate")
dfspin <- melt(data.frame(spincor),id.vars = "spin")


resnpra <- dfnpra %>% group_by(variable) %>% do(tidy(glm(npra ~ value, data=.,family="poisson")))
resnpre <- dfnpre %>% group_by(variable) %>% do(tidy(glm(npre ~ value, data=.,family="poisson")))
resnpoa <- dfnpoa %>% group_by(variable) %>% do(tidy(glm(npoa ~ value, data=.,family="poisson")))
resnpoe <- dfnpoe %>% group_by(variable) %>% do(tidy(glm(npoe ~ value, data=.,family="poisson")))
respata <- dfpata %>% group_by(variable) %>% do(tidy(glm(pata ~ value, data=.,family="poisson")))
respate <- dfpate %>% group_by(variable) %>% do(tidy(glm(pate ~ value, data=.,family="poisson")))
resspin <- dfspin %>% group_by(variable) %>% do(tidy(glm(spin ~ value, data=.,family="poisson")))


cleanres <- function(df){
    tokeep <- which(df$term=="value")
    df2 <- data.frame(df[tokeep,])
    return(df2)
    }


stackres <- function(ls,sign){

    names <- names(ls)
    templist1 <- list()
    templist2 <- list()

    for(i in 1:length(ls)){
        templist1[[i]] <- ls[[i]][,c(1,3,6)]
        templist2[[i]] <- ls[[i]][,c(1,3)]
    }

    res <- data.frame(do.call("cbind",templist1))
    estv <- grep("estimate",colnames(res))
    pv <- grep("p.value",colnames(res))

    beta <- res[,estv];colnames(beta) <- paste("coef.",names, sep="")
    sig <- res[,pv];colnames(sig) <- paste("p.",names, sep="")
    q <- res[,1]

    finres1 <- cbind(q,beta,sig)
    finres2 <- cbind(q,beta)

    if(sign){return(finres1)} else {return(finres2)}

 }

modlist <- list(resnpra,resnpre,resnpoa,resnpoe,respata,respate,resspin)
namvec <- c("npra",
            "npre",
            "npoa",
            "npoe",
            "pata",
            "pate",
            "spin")
names(modlist) <- namvec



modlist1 <- lapply(modlist, FUN=cleanres)
finalres <- stackres(modlist1, sign=FALSE) #DONT INCLUDE SIG IN THIS PART
is.num <- sapply(finalres, is.numeric)
finalres[is.num] <- lapply(finalres[is.num], round, 4)
finalres <- finalres[-1,]

qmatch <- as.character(finalres$q)
qnames$q <- as.character(qnames$q)

qnames <- qnames[,1:2]

finalresexp <- merge(qnames,finalres, by="q",all.y=TRUE)
#finalresexp$q <- qnames
finalresexp <- subset(finalresexp, !is.na(finalresexp$desc))


result <- condformat(finalresexp) %>%
rule_fill_discrete(coef.npra, expression = coef.npra < 0, colours=c("TRUE"="red", "FALSE"="green")) %>%
rule_fill_discrete(coef.npre, expression = coef.npre < 0, colours=c("TRUE"="red", "FALSE"="green"))%>%
rule_fill_discrete(coef.npoa, expression = coef.npoa < 0, colours=c("TRUE"="red", "FALSE"="green"))%>%
rule_fill_discrete(coef.npoe, expression = coef.npoe < 0, colours=c("TRUE"="red", "FALSE"="green"))%>%
rule_fill_discrete(coef.pata, expression = coef.pata < 0, colours=c("TRUE"="red", "FALSE"="green"))%>%
rule_fill_discrete(coef.pate, expression = coef.pate < 0, colours=c("TRUE"="red", "FALSE"="green"))%>%
rule_fill_discrete(coef.spin, expression = coef.spin < 0, colours=c("TRUE"="red", "FALSE"="green"))


resall <- condformat2html(result)
#write.table(resall,
           # file='martinres.html',
           # quote = FALSE,
           # col.names = FALSE,
           # row.names = FALSE)


####### DEBUGGING
summary(newprod)


foo <- glm(spinoff ~ Q16a, data=newprod, family="poisson");summary(foo)
food <- newprod[,c("spinoff","Q16")]

head(food)
food

### ADDING FOR MARTIN ANALYSIS
finalres <- stackres(modlist1, sign=TRUE) #DONT INCLUDE SIG IN THIS PART
is.num <- sapply(finalres, is.numeric)
finalres[is.num] <- lapply(finalres[is.num], round, 4)
finalres <- finalres[-1,]
finalresexp <- merge(qnames,finalres, by="q",all.y=TRUE)
finalresexp <- subset(finalresexp, !is.na(finalresexp$desc))


#Getting adjusted datasets
spinadj <- spincor[,c(1,2,c(26:99))]
finalresadj <- finalresexp[,c("q","coef.spin","p.spin")]
fixedf <- sfp[,c("orgnr","Fylke","Sektor","ansatte")]
fixedf <- fixedf[complete.cases(fixedf), ]

fixedf <- regnskap[,c("orgnr","ansatte","nace","kommune","fylke","yr")]

fixedf <- regnskap[!duplicated(fixedf$orgnr),]



spinadjest <- merge(spinadj, fixedf, by="orgnr",all=FALSE, all.x=TRUE)
spinadj <- spinadjest[,c(2:76)]
spinadj$ind <- factor(spinadjest$nace)
spinadj$geo <- factor(spinadjest$fylke)
spinadj$emp <- spinadjest$ansatte



aiv <- spinadj[ , !(names(spinadj) %in% c("ind","geo","emp"))]
ind <- spinadj[ , !(names(spinadj) %in% c("geo","emp"))]
emp <- spinadj[ , !(names(spinadj) %in% c("geo"))]
geo <- spinadj[ , !(names(spinadj) %in% c("ind"))]

po <- function(df){


    mod <- glm(spin~., data=df, family="poisson")
    ext <- grep("Q",names(mod$coefficients))
    res.coef <- mod$coefficients[ext]
    res.p <- summary(mod)$coefficients[ext,4]

    return(list(coef=res.coef,p=res.p))
    }


reslist <- list()
reslist[["q"]] <- finalresexp$q
reslist[["desc"]] <- finalresexp$desc

reslist[["base"]]  <- finalresadj$coef.spin
reslist[["base.p"]]  <- finalresadj$p.spin
reslist[["aiv"]] <- po(aiv)$coef
reslist[["aiv.p"]] <- po(aiv)$p
reslist[["ind"]] <- po(ind)$coef
reslist[["ind.p"]] <- po(ind)$p
reslist[["emp"]] <- po(emp)$coef
reslist[["emp.p"]] <- po(emp)$p
reslist[["geo"]] <- po(geo)$coef
reslist[["geo.p"]] <- po(geo)$p


resdf <- data.frame(reslist)
is.num <- sapply(resdf, is.numeric)
resdf[is.num] <- lapply(resdf[is.num], round, 4)


result2 <- condformat(resdf) %>%


    rule_fill_discrete(aiv, expression = aiv < 0, colours=c("TRUE"="red", "FALSE"="green")) %>%
    rule_fill_discrete(ind, expression = ind < 0, colours=c("TRUE"="red", "FALSE"="green"))%>%
    rule_fill_discrete(base, expression = base < 0, colours=c("TRUE"="red", "FALSE"="green"))%>%
    rule_fill_discrete(emp, expression = emp < 0, colours=c("TRUE"="red", "FALSE"="green"))%>%
    rule_fill_discrete(geo, expression = geo < 0, colours=c("TRUE"="red", "FALSE"="green"))%>%

    rule_text_bold(emp, expression = emp.p < 0.05 )%>%
    rule_text_bold(geo, expression = geo.p < 0.05 )%>%
    rule_text_bold(aiv, expression = aiv.p < 0.05 )%>%
    rule_text_bold(ind, expression = ind.p < 0.05 )%>%
    rule_text_bold(base, expression = base.p < 0.05)




setwd("/Users/larshovdanmolden/Documents/Data/Article_4")

allinone <- condformat2html(result2)
#allinone
#write.table(allinone,
#            file='allinone.html',
#            quote = FALSE,
#            col.names = FALSE,
#            row.names = FALSE)


### PCA

## spinpca <-na.omit(spinadjest[,c(3:76)])

## pca1 <- prcomp(spinpca,scores=TRUE)
## summary(pca1)
## pca_viz<- data.frame(pca1$rotation[,1:10])


## head(unclass(pca1$rotation)[, 1:4])



## result3 <- condformat(pca_viz) %>%


##     #rule_fill_discrete(pca_viz, expression = abs(pca_viz) >  0.2, colours=c("TRUE"="red", "FALSE"="green"))


##     rule_fill_gradient2(PC1)  %>%
##     rule_fill_gradient2(PC2)  %>%
##     rule_fill_gradient2(PC3)  %>%
##     rule_fill_gradient2(PC4)  %>%
##     rule_fill_gradient2(PC5)  %>%
##     rule_fill_gradient2(PC6)  %>%
##     rule_fill_gradient2(PC7)  %>%
##     rule_fill_gradient2(PC8)  %>%
##     rule_fill_gradient2(PC9)  %>%
##     rule_fill_gradient2(PC10)


## pca_viz_res<- condformat2html(result3)



## write.table(pca_viz_res,
##             file='pca.html',
##             quote = FALSE,
##             col.names = FALSE,
##             row.names = FALSE)



## #factors to extract
## eig.val <- get_eigenvalue(pca1)
## eig.val



#Correlations between them. Most are grouped
### fviz_pca_var(pca1)

#18c Top management generally believes that the firms environment should be explored gradually and with cautios
# 17c We relate to our competitors by avoiding as little direct competition as possible. We prefer that we all leave eachother alone



#Variables factor map
## fviz_pca_var(pca1, col.var="contrib") +
## scale_color_gradient2(low="white", mid="blue",
##       high="red", midpoint=5) + theme_minimal()

## #none is really sticking out

### LASSSO

#install.packages("mice", repos = "http://cran.us.r-project.org")


### ALL COMPLETE OBS
lassoraw <- spinadjest[,c(2:76)]
lassodf <- na.omit(spinadjest[,c(2:76)])
nrow(lassodf)

y <- c(lassodf$spin)
x <- as.matrix(lassodf[ , !(names(lassodf) %in% c("spin"))])



lasso <- glmnet(x,y)
#plot(lasso)
#print(lasso)
c1 <- coef(lasso,s=0.05)


#### IMPUTED TO BE IMPLEMENTED LATER

##### IMPUTED MISSING VALUES
                                        #https://www.r-bloggers.com/imputing-missing-data-with-r-mice-package/
#https://cran.r-project.org/web/packages/mice/mice.pdf

## lassoraw2 <- lassoraw[,1:10]
## #impLassorf <- impLasso

## #impLasso <- mice(lassoraw,m=5,maxit=25,meth='lda',seed=500)

## ###summary(impLasso)

## implassodf <- complete(impLasso,1)

## y <- c(implassodf$spin)
## x <- as.matrix(implassodf[ , !(names(implassodf) %in% c("spin"))])

## mod1 <- lm(y ~ x);summary(mod1)
## mod1 <- lm(spin ~., data=lassoraw2);summary(mod1)

## lassoimp <- glmnet(x,y)
## plot(lassoimp)
## print(lassoimp)
## c2 <- coef(lassoimp,s=0.05)


## lassores <- data.frame("q"=rownames(c1)[-1],"full"=c1[-1,1], "imp"=c2[-1,1])
## is.num <- sapply(lassores, is.numeric)
## lassores[is.num] <- lapply(lassores[is.num], round, 4)
## lassores[lassores==0.00000]<-NA



## result4 <- condformat(lassores) %>%


##     #rule_fill_discrete(pca_viz, expression = abs(pca_viz) >  0.2, colours=c("TRUE"="red", "FALSE"="green"))


##     rule_fill_discrete(full, expression = full < 0, colours=c("TRUE"="red", "FALSE"="green")) %>%
##     rule_fill_discrete(imp, expression = imp < 0, colours=c("TRUE"="red", "FALSE"="green"))


## lasso_viz<- condformat2html(result4)
## lasso_viz


## write.table(lasso_viz,
##             file='lasso.html',
##             quote = FALSE,
##             col.names = FALSE,
##             row.names = FALSE)


### ADDING ONLY VARIABLES FROM THE LASSO
tmp_coeffs<-  coef(lasso,s=0.05,allCoef=TRUE)
tmp_coeffs <- data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)
lassovar <- tmp_coeffs$name[-1]
lassotmp <- spinadjest[ , (names(spinadjest) %in% lassovar)]
lassotmp$spin <- spinadjest$spin
lassotmp$orgnr <- spinadjest$orgnr

lassotmp2 <- lassotmp[ , !(names(lassotmp) %in% c("Q22","Q22b","Q28b"))]
lassotmp3 <- lassotmp[ , !(names(lassotmp) %in% c("Q28b"))]


mod1 <- glm(spin ~ ., data=lassotmp, family="poisson");summary(mod1)
mod2 <-  glm(spin ~ ., data=lassotmp2, family="poisson");summary(mod2)
mod3 <-  glm(spin ~ ., data=lassotmp3, family="poisson");summary(mod3)


addvarres <- list(mod1,mod2,mod3)





### MARTINS GROUPING  - PUT INTO A SEM FRAMEWORK



###### ADD CONTROL VARIABLES / extract from SFP tax credit projects
spindata <- newprod  #### TRTY TO USE THIS DATASET GOING FORWARD
spindata <- subset(spindata, spinoff<11)
sfp$sic <- factor(sfp$Sektor)
sfp$reg <- factor(sfp$Fylke)

exvec2 <- c("size", ### size of asset side
            "alder",
            "internfou",
            "eksternfou",
            "sales",
            "lonnskost",
            "fousam",
            "orgdummy",
            "adjorgdummy")



namvec2 <- exvec2




extres <- list()
for(i in 1:length(exvec2)){

    extres[[i]] <- extsfresm(quo(get(exvec2[i])),namvec2[i])
}


ctrlvar <- do.call("cbind",extres)
ctrlvar <- ctrlvar[,c(1,seq(2,length(names(ctrlvar)), by=2))]
head(ctrlvar)

### adding dummies for industry and region from SFP
dum <- sfp[,c("orgnr","sic","reg")]
dum <- dum[!is.na(dum$sic),]
dum <- dum[!is.na(dum$reg),]




dum <- dum %>%
       group_by(orgnr) %>%
       slice(1)

ctrlvar <- merge(ctrlvar,dum,by="orgnr", all.x=TRUE)


seq(2,length(names(ctrlvar)), by=2)
spindata2 <- merge(ctrlvar, spindata,by="orgnr")

spintemp <- spindata2
spindata <- spindata2
spindata$hcap <- pmin((spindata$lonnskost+1)/(spindata$sales+1),1) ### human capital as salary over sales
spindata$lnsales <- log(max(spindata$sales,1))
spindata$lnintfou <- log(spindata$internfou+1)
spindata$spindum <-  ifelse(spindata$spinoff==0,0,1 )
spindata$lnsize <- log(spindata$size+1)
spindataadj <- subset(spindata, hcap<1)
spindata$lnalder <- log(spindata$alder)
spindata$spinoffdum <- ifelse(spindata$spinoff==0,0,1)


external <- c("Q16a","Q20c","Q19d","Q21d","Q22a","Q22d","Q23","Q23b","Q24c","Q25","Q27g","Q28","Q28b","Q19b","Q27a")
internal <- c("Q18d", "Q19" ,"Q20e","Q22c","Q25c","Q26f")

external1 <- c("Q16a","Q20c","Q19d","Q23","Q23b","Q24c","Q25","Q27g","Q27a")
internal <- c( "Q19" ,"Q19b","Q20e","Q22c","Q25c","Q26f")

external2 <- c("Q22a","Q28","Q28b")



spinext1 <- spindata[,external1]
spinext2 <- spindata[,external2]


spinint <- spindata[,internal]

               # for hetcor()


pcint <- hetcor(spinint, ML=TRUE)
pcext1 <- hetcor(spinext1, ML=TRUE)
pcext2 <- hetcor(spinext2, ML=TRUE)

parext1 <- fa.parallel(spinext1, fm = 'minres', fa = 'fa')
parext2 <- fa.parallel(spinext2, fm = 'minres', fa = 'fa')
parint <- fa.parallel(spinint, fm = 'minres', fa = 'fa')



intfac <- fa(r=pcint$correlations,nfactors = 1,rotate = "oblimin",fm="minres")
print(intfac)
print(intfac$loadings,cutoff = 0.4)

extfac1 <- fa(r=pcext1$correlations,nfactors = 1,rotate = "oblimin",fm="minres")
print(extfac1)
print(extfac1$loadings,cutoff = 0.4)

extfac2 <- fa(r=pcext2$correlations,nfactors = 1,rotate = "oblimin",fm="minres")
print(extfac2)
print(extfac2$loadings,cutoff = 0.4)






## external =~  Q16a + Q20c + Q19d + Q23 + Q23b + Q24c + Q25 + Q27g
## internal =~  Q19b +  Q19  + Q20e   + Q25c + Q26f

## ordered <- c("Q16a","Q20", "Q19d","Q23","Q23b","Q24c","Q25","Q27g",  "Q19b","Q19","Q20e","Q25c","Q26f","spinoff")
## external ~ internal



model <- "

external =~   + Q16a + Q21c +Q21e +Q21d + Q19e
internal =~  + Q18b  + Q25 + Q25a + Q25c +Q26d + Q26f

###internal ~ external
spinoff ~ external + internal + lnalder + lnintfou + lnsize

"



fit <- sem(model, data = spindata, ordered="spinoff")
summary(fit,fit.measures=TRUE)








######################### RUNS HERE / saved here ###############################################


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

### Making some tables

exttab <- qnames[which(qnames$q %in% extvar),]
exttab$type <- "external"
inttab <- qnames[which(qnames$q %in% intvar),]
inttab$type <- "internal"
ivtab <- rbind(exttab,inttab)



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


spindata$colcust <- ifelse(spindata$Q24a >4,1,0)
spindata$colsup <- ifelse(spindata$Q24b >3,1,0)
spindata$coluni <- ifelse(spindata$Q24c >3,1,0)



mod8 <- glm(spinoff~ (ext + int) + orgdummy + lnalder + lnsize + lnintfou +hcap +colsup + factor(sic) + factor(reg), data=spindata, family="poisson");summary(mod8)

mod8b <- glm(spinoff~ (ext + int) + orgdummy + lnalder + lnsize + lnintfou +hcap +colsup + coluni+ factor(sic) + factor(reg), data=spindata, family="poisson");summary(mod8b)


mod9 <- glm(spinoff~ ext + int+ orgdummy + lnalder + lnsize + lnintfou +hcap + coluni+ factor(sic) + factor(reg), data=spindata, family="poisson");summary(mod9)

mod10 <- glm(spinoff~ ext*colsup + int + orgdummy + lnalder + lnsize + lnintfou +hcap +colsup +factor(sic) + factor(reg), data=spindata, family="poisson");summary(mod10)

mod11 <- glm(spinoff~ ext*coluni + int + orgdummy + lnalder + lnsize + lnintfou +hcap  + coluni+ factor(sic) + factor(reg), data=spindata, family="poisson");summary(mod11)


mod13 <- glm(spinoff~ ext + int*colsup + orgdummy + lnalder + lnsize + lnintfou +hcap + colsup+ factor(sic) + factor(reg), data=spindata, family="poisson");summary(mod13)

mod14 <- glm(spinoff~ ext + int*coluni + orgdummy + lnalder + lnsize + lnintfou +hcap + coluni+ factor(sic) + factor(reg), data=spindata, family="poisson");summary(mod14)

mod15 <- glm(spinoff~ (ext+int)*coluni  + orgdummy + lnalder + lnsize + lnintfou +hcap +colsup + coluni+ factor(sic) + factor(reg), data=spindata, family="poisson");summary(mod15)

mod16 <- glm(spinoff~ (ext+int)*colsup  + orgdummy + lnalder + lnsize + lnintfou +hcap +colsup + coluni+ factor(sic) + factor(reg), data=spindata, family="poisson");summary(mod16)

mod17 <- glm(spinoff~ (ext+int)*colsup  + (ext+int)*coluni + orgdummy + lnalder + lnsize + lnintfou +hcap +colsup + coluni+ factor(sic) + factor(reg), data=spindata, family="poisson");summary(mod17)


modlist2 <-list(mod8,mod9,mod8b,mod10,mod11,mod13,mod14,mod15,mod16,mod17)

r1s <- round(PseudoR2(mod8)[4],3)
r2s <- round(PseudoR2(mod9)[4],3)
r3s <- round(PseudoR2(mod10)[4],3)
r4s <- round(PseudoR2(mod11)[4],3)
r5s <- round(PseudoR2(mod13)[4],3)
r6s <- round(PseudoR2(mod14)[4],3)
r7s <- round(PseudoR2(mod15)[4],3)


cor(spindata$colsup, spindata$colcust, use="complete.obs")





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

##library(interplot)


pl1 <- interplot(m = mod8, var1 = "int", var2 = "colcust",ylim = c(0,1))
pl2 <- interplot(m = mod9, var1 = "int", var2 = "colsup")
pl3 <- interplot(m = mod10, var1 = "int", var2 = "coluni")
grid.arrange(pl1,pl2,pl3, ncol=3)

sl1 <- pl1[2]/pl1[1]-1
sl1

pl1 <- meplot(model=mod8,var1="int",var2="colcust",rnum=TRUE)
pl2 <- meplot(model=mod9,var1="int",var2="colsup",rnum=TRUE)
pl3 <- meplot(model=mod10,var1="int",var2="coluni",rnum=TRUE)



index(pl1)





lines(meplot(model=mod9,var1="int",var2="colsup"))


head(pl2)

### FINDINGS> RD collaboration level variabel not working

mod1 <- glm(spinoff~  Q28b, data=spindata, family="poisson");summary(mod1)



RD centralization is more human capital / more rd workers are expensive
Importance of RD to the firm
Level of where RD is performance *organizational level RD / the higher level the more important

If yu are high level you can translate these into more projects so more you can generalize more oppostyintes fro the external environment and hence makes it more likely to genreate internal opportunities. Are these firms in one location?
y

SOmething about the org structure ??  in skattefunn
Multiple locations / if only one location





### orgdummy=1 means they have their own RD unit (centralized), 0 means the RD is in the business unit / part of operations


#### NEED TO EXPLORE MISSING





meplot <- function(model,var1,var2,ci=.95,
                   xlab=var2,ylab=paste("Marginal Effect of",var1),
                   main="Marginal Effect Plot",
                   me_lty=1,me_lwd=3,me_col="black",
                   ci_lty=1,ci_lwd=1,ci_col="black",
                   yint_lty=2,yint_lwd=1,yint_col="black", rnum=FALSE){
  alpha <- 1-ci
  z <- qnorm(1-alpha/2)
  beta.hat <- coef(model)
  cov <- vcov(model)
  z0 <- seq(min(model$model[,var2],na.rm=T),max(model$model[,var2],na.rm=T),length.out=1000)
  dy.dx <- beta.hat[var1] + beta.hat[length(beta.hat)]*z0
  se.dy.dx <- sqrt(cov[var1,var1] + z0^2*cov[nrow(cov),ncol(cov)] + 2*z0*cov[var1,ncol(cov)])
  upr <- dy.dx + z*se.dy.dx
  lwr <- dy.dx - z*se.dy.dx
  plot(x=z0, y=dy.dx,type="n",xlim=c(min(z0),max(z0)),
                                         ylim=c(min(lwr),max(upr)),
      # ylim=c(-0.25,0),
       xlab = xlab,
       ylab = ylab,
       main = main)
  lines(z0, dy.dx, lwd = me_lwd, lty = me_lty, col = me_col)
 # lines(z0, lwr, lwd = ci_lwd, lty = ci_lty, col = ci_col)
 # lines(z0, upr, lwd = ci_lwd, lty = ci_lty, col = ci_col)
  abline(h=0,lty=yint_lty,lwd=yint_lwd,col=yint_col)

  if(rnum){return(dy.dx)}
}












## Standardized Root Mean Square Residual:

##   SRMR                                           0.091       0.091

## Parameter Estimates:

##   Information                                 Expected
##   Information saturated (h1) model        Unstructured
##   Standard Errors                           Robust.sem

## Latent Variables:
##                    Estimate   Std.Err  z-value  P(>|z|)
##   external =~
##     Q16a               1.000
##     Q21c               1.613    0.139   11.618    0.000
##     Q21e               1.626    0.135   12.017    0.000
##     Q21d               1.607    0.140   11.483    0.000
##     Q19e               1.805    0.154   11.726    0.000
##   internal =~
##     Q18b               1.000
##     Q25                1.466    0.159    9.204    0.000
##     Q25a               1.581    0.167    9.497    0.000
##     Q25c               1.112    0.134    8.313    0.000
##     Q26d               1.858    0.203    9.172    0.000
##     Q26f               1.478    0.149    9.918    0.000

## Regressions:
##                    Estimate   Std.Err  z-value  P(>|z|)
##   internal ~
##     external           0.778    0.093    8.327    0.000
##   spinoff ~
##     external           0.735    0.196    3.752    0.000
##     internal          -0.493    0.189   -2.614    0.009
##     alder              0.001    0.000    5.127    0.000
##     lnsize             0.155    0.018    8.763    0.000
##     internfou          0.012    0.001   11.920    0.000

## Intercepts:
##                    Estimate   Std.Err  z-value  P(>|z|)
##    .Q16a               5.831    0.081   71.949    0.000
##    .Q21c               5.294    0.092   57.435    0.000
##    .Q21e               5.000    0.085   59.009    0.000
##    .Q21d               4.906    0.087   56.210    0.000
##    .Q19e               4.923    0.097   50.735    0.000
##    .Q18b               3.668    0.091   40.402    0.000
##    .Q25                5.463    0.076   72.315    0.000
##    .Q25a               5.189    0.089   58.184    0.000
##    .Q25c               3.929    0.095   41.267    0.000
##    .Q26d               4.742    0.115   41.379    0.000
##    .Q26f               6.033    0.091   66.368    0.000
##    .spinoff            0.000
##     external           0.000
##    .internal           0.000

## Thresholds:
##                    Estimate   Std.Err  z-value  P(>|z|)
##     spinoff|t1         2.050    0.191   10.720    0.000
##     spinoff|t2         2.770    0.199   13.942    0.000
##     spinoff|t3         3.406    0.207   16.425    0.000
##     spinoff|t4         3.982    0.220   18.117    0.000
##     spinoff|t5         4.441    0.246   18.020    0.000
##     spinoff|t6         4.606    0.257   17.919    0.000
##     spinoff|t7         4.833    0.286   16.927    0.000
##     spinoff|t8         4.999    0.315   15.866    0.000
##     spinoff|t9         5.510    0.320   17.232    0.000

## Variances:
##                    Estimate   Std.Err  z-value  P(>|z|)
##    .Q16a               1.161    0.052   22.398    0.000
##    .Q21c               1.301    0.061   21.293    0.000
##    .Q21e               1.092    0.046   23.930    0.000
##    .Q21d               1.317    0.059   22.390    0.000
##    .Q19e               1.490    0.083   17.985    0.000
##    .Q18b               1.964    0.101   19.535    0.000
##    .Q25                0.822    0.043   19.206    0.000
##    .Q25a               1.103    0.050   21.903    0.000
##    .Q25c               2.006    0.101   19.866    0.000
##    .Q26d               2.062    0.108   19.134    0.000
##    .Q26f               1.007    0.048   21.035    0.000
##    .spinoff            0.931
##     external           0.310    0.045    6.966    0.000
##    .internal           0.124    0.025    5.020    0.000

## Scales y*:
##                    Estimate   Std.Err  z-value  P(>|z|)
##     spinoff            1.000

## >





























## ### ARE SPIN OFFS DEVLINING OVER TIME
## ## FIRST CRUDE COUNT ALL OVER THE BOARD


## spin_t <- data.frame(sfmain  %>% dplyr::group_by(ar_til) %>% dplyr::summarise(sum(Videreringspinoff,na.rm=TRUE)))

## spin_t <- data.frame(sfmain  %>% dplyr::group_by(ar_fra) %>% dplyr::summarise(sum(Videreringspinoff,na.rm=TRUE)))
## colnames(spin_t) <- c("yr","spin")
## spin_t

## spin_t <- data.frame(spinadjest  %>% dplyr::group_by(yr) %>% dplyr::summarise(sum(spin,na.rm=TRUE)))
## colnames(spin_t) <- c("yr","spin")

## test <- spin_t %>% group_by() %>% mutate(cumsum = cumsum(spin))


## plot(test$cumsum, type="l")
## plot(spin_t, type="l")

## foo <- cbind(spin_t,foo2$n)
## foo3 <- foo$spin
## foo4 <- foo2$n
## foo4

## test <- foo3/foo4
## foo3
## plot(test)

## test

## foo2 <- sfmain %>% dplyr::group_by(ar_til) %>% tally()

## df2 <- count(spin_t, c("spin",'yr'))

## spin_nr <- data.frame(sfmain  %>% dplyr::group_by(ar_til) %>% dplyr::summarise(count(Prosjektnummmer,na.rm=TRUE)))


## ### making SPINOFF PANEL PR FIRM

## spin_time <- data.frame(sfmain  %>% dplyr::group_by(Organisasjonsnummer,ar_til) %>% dplyr::summarise(sum(Videreringspinoff,na.rm=TRUE)));colnames(spin_time) <- c("orgnr","yr","spin")

## test <- data.frame(spin_time%>% dplyr::group_by(yr) %>% dplyr::summarise(sum(spin,na.rm=TRUE)))
## colnames(test) <- c("yr","spin")
## plot(test)

## spinoff.panel <- merge(spin_time, lassotmp,by="orgnr", all.x=TRUE)
## spinoff.panel$t <-spinoff.panel$yr - 2002
## spinoff.panel$spin <- spinoff.panel$spin.x
## spinoff.panel$spinbin <- ifelse(spinoff.panel$spin>0,1,0)
## spinoff.panel$t2 <- spinoff.panel$t^2

## foo <- subset(spinoff.panel, spin<2)

## mod3 <-  glm(spin.x ~ Q20*t, data=spinoff.panel, family="poisson");summary(mod3)
## mod3 <-  glm(spin.x ~ Q17 + Q17:t, data=spinoff.panel, family="poisson");summary(mod3)
## mod3 <-  glm(spin.x ~ Q17, data=spinoff.panel, family="poisson");summary(mod3)
## mod3 <-  glm(spin.x ~ t, data=spinoff.panel, family="poisson");summary(mod3)
## mod3 <-  glm(spinbin ~ t, data=spinoff.panel, family="binomial");summary(mod3)

## mod3 <-  glm(spin ~ t + t2, data=foo, family="poisson");summary(mod3)
## mod3 <-  lm(spin ~ t, data=foo);summary(mod3)


## dfspinp <- melt(data.frame(spinoff.panel),id.vars = c("spin.x","t"))
## dfspinp$value2 <- dfspinp$value
## dfspinp$value <- dfspinp$value*dfspinp$t
## dfspinp$spinalt <- ifelse(dfspinp$spin.x>0,1,0)


## mod3 <-  glm(spin.x ~ Q17, data=dfspinp, family="poisson");summary(mod3)


## # models for both count - where number of spin offs can be more than one yearly, and as binomial
## spinp <- dfspinp %>% group_by(variable) %>% do(tidy(glm(spin.x~ value, data=.,family="poisson")))
## spinpalt <- dfspinp %>% group_by(variable) %>% do(tidy(glm(spinalt~ value, data=.,family="binomial")))

## modlistspin <- list(spinp=spinp,spinpalt=spinpalt)
## modlistspin <- lapply(modlistspin, FUN=cleanres)

## spintime <- stackres(modlistspin, sign=TRUE) #DONT INCLUDE SIG IN THIS PART
## head(spintime)
## is.num <- sapply(spintime, is.numeric)
## spintime[is.num] <- lapply(spintime[is.num], round, 4)
## spintime <- spintime[grep("Q",spintime$q),] #removing orgnr and yr regressions

## result5 <- condformat(spintime) %>%


##     rule_fill_discrete(coef.spinp, expression = coef.spinp < 0, colours=c("TRUE"="red", "FALSE"="green")) %>%
##     rule_fill_discrete(coef.spinpalt, expression = coef.spinpalt < 0, colours=c("TRUE"="red", "FALSE"="green"))%>%
##     rule_text_bold(coef.spinp, expression =p.spinp < 0.05 )%>%
##     rule_text_bold(coef.spinpalt, expression = p.spinpalt < 0.05 )



## timeint <- condformat2html(result5)
## timeintlat <- condformat2latex(result5)



## ######### New plots
## # yearly plots in number of projects

## yearpro <- sfmain %>% group_by(ar_fra) %>% dplyr::count()
## yearcost <- sfmain %>% group_by(ar_fra) %>% dplyr::summarise(sum(SR_Sum_Total_finansiering,na.rm=TRUE));colnames(yearcost) <- c("ar","cost")
## yearother <-  sfmain %>% group_by(ar_fra) %>% dplyr::summarise(sum(SR_Annen.offentlig.finansiering,na.rm=TRUE));colnames(yearother) <- c("ar","othercost")

## yrvec <- yearcost$ar
## avgyearcost <- yearcost$cost/yearpro$n
## avgyearcostother <- yearother$othercost/yearpro$n
## yearcostsummary <- data.frame(cbind(yrvec,avgyearcost,avgyearcostother))
## yearcostsummary


## o





## f1 <- yearcostsummary[1:9,2]
## f2 <- yearcostsummary[1:9,3]
## e1 <- yearcostsummary[10:15,2]
## e2 <- yearcostsummary[10:15,3]


## cor(f1,f2)



## res <- colSums(sfmain$size==0)/nrow(sfmain$size)*100


## sum(sfmain$size == 0,na.rm=TRUE)


## plot(yearcostsummary$yrvec,yearcostsummary$avgyearcost,type="l",
##      ylim=c(0,1.4),
##      ylab="million NOK - average",
##      col="blue")

## lines(yearcostsummary$yrvec,yearcostsummary$avgyearcostother, col="red")
## title(main="Average budgeted tax credit pr firm")
## legend(2007, 0.2, legend=c("Avg budgeted tax credit", "Avg budget other grants"),
##        col=c("blue", "red"), lty=1:1)



## foo <- trendline(yearcostsummary$yrvec,yearcostsummary$avgyearcost,model="line2P",summary=TRUE,eDigit=10)
## trendline(yearcostsummary$yrvec[10:15],yearcostsummary$avgyearcost[10:15],model="line2P",summary=TRUE,eDigit=10)

## trendline(yearcostsummary$yrvec[1:9],yearcostsummary$avgyearcostother[1:9],model="line2P",summary=TRUE,eDigit=10)
## trendline(yearcostsummary$yrvec[10:15],yearcostsummary$avgyearcostother[10:15],model="line2P",summary=TRUE,eDigit=10)
## #yearpro is sum of projects
