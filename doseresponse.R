folder <- "D:\\SCS ON D DRIVE\\SCSdata\\Ransom, Corey\\"
subj <- "Utah Sweetvetch and Tall Fescue"
ext <- ".csv"
gfname <-paste0(folder,subj," ","greenhouse"," ","Data","2",ext)
#   print(fname)
ffname <- paste0(folder,subj," ","field"," ","Data","2",ext)

greenhouse2 <- read.csv(gfname); str(greenhouse2)
greenhouse2[greenhouse2$Rate==0,]$Rate <-1
field2 <-read.csv(ffname); str(field2)
field2[field2$Rate==0,]$Rate <- 1

library(data.table)
library(drc)

############################################################
## This function compares various Dose-Response models by
## AIC/BIC for each response of interest in a given dataset
## location is the char string indicating where the data are
## collected. From and to are integer indicating which col-
## omns in the dataset to be analyzed. 
############################################################
modelselect <- function(.dsn, start, end){
#   fname <-paste0(folder,subj," ",loc," ","Data","2",ext)
#   .dsn <- read.csv(fname)
#   .dsn[.dsn$Rate==0,]$Rate <-1; str(.dsn); cat("\n")
  colseq <- seq(start, end)
  for (i in colseq){
    my.formula <- formula(paste(names(.dsn)[i],"~","Rate"))
    LL4est <- drm(my.formula, Species, data=.dsn, fct=LL.4())
#     LL4est <- drm(formula(paste(names(.dsn)[i],"~","Rate")), Species, data=.dsn, fct=LL.4())
#     .dsn$"resp" <- .dsn[,col]
#     LL4est <- drm(resp~Rate, data = .dsn, fct=LL.4())
    print(names(.dsn)[i])
    print(modelFit(LL4est))
    cat("\n")
    ms <- mselect(LL4est, 
                  list(LL.3(), LL.5(),
                       W1.4(), W1.3(),
                       W2.4(), W2.3()),
                  icfct = AIC)
    return(ms)
    cat("\n\n")
  }
}
names(.dsn)
modelselect(greenhouse2, 5,7)
names(field2)
modelselect(field2,5,14 )

for (col in colseq){
  resp <- greenhouse2[,col]
  W2.3est <- drm(resp~Rate, 
                 Species,
                 data = greenhouse2,
                 fct = W2.3())
  summary(W2.3est)
  plot(W2.3est, ylab = names(resp))
  ED(W2.3est, 50, "delta")
  ED(W2.3est, 15, "delta")
  compParm(W2.3est,"e","/")
  cat("\n\n")
}

W2.3est <- drm(resp~Rate, 
               Species,
               data = greenhouse2,
               fct = W2.3())
summary(W2.3est)
plot(W2.3est, ylab = names(resp))
ED(W2.3est, 50, "delta")
ED(W2.3est, 15, "delta")
compParm(W2.3est,"e","/")
comped(c(1626,495),c(141,19), log=F, operator = "/")
