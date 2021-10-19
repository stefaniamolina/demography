library(LexisPlotR)
library(plyr)
library(dplyr)

#change data to Age-specific fertility rates --> year, age, cohort

setwd("~/Desktop/Demography R")
FR <- read.csv("France_ASFR.csv")
head(FR)

FR <- FR %>%
  mutate(Code = "FR") %>%  # include code
  mutate(ID = paste(Code, Year, Age, Cohort, sep = "")) %>% # include ID
  select(ID, Code, everything()) # rearrange columns

FR[FR == "."] <- NA 
FR <- na.omit(FR) #remove NAs

FR$Cohort <- as.integer(FR$Cohort)

LexTriCoords <- function(x){
  ID <- x["ID"]
  X <- as.integer(unlist(x[c("Year","Age","Cohort")]))
  # lower
  if (diff(c(X[2],X[1]))==X[3]){
    xcoord <- c(X[1],X[1]+1,X[1]+1)
    ycoord <- c(X[2],X[2],X[2]+1)
  } else { #upper
    xcoord <- c(X[1],X[1]+1,X[1])
    ycoord <- c(X[2],X[2]+1,X[2]+1)
  }
  data.frame(ID=ID,Year=xcoord,Age=ycoord,check.rows=FALSE)
}

POS <- rbind.fill(apply(FR,1,LexTriCoords))
ASFR <- data.frame(ID = as.factor(FR$ID),ASFR = as.numeric(FR$ASFR)) 
datapoly <- merge(ASFR, POS, by=c("ID"))
datapoly <- datapoly[datapoly$ASFR > 0,]

a <- -rev(unique(FR$Cohort)[unique(FR$Cohort)%%5==0])
# we pick out the cohorts and remember that a is the 'y' intercept
b <- rep(1,length(a))
FR <- data.frame(a,b)
# I'd prefer not to do it this way, but the abline function wants a data.frame later.


# my dorky HCL ramp function, useful pretty much only for this kind of plot.
# assuming you have clear values between which you want specific colors to interpolate, this thing will set things up right:
myfxHCLramp <- function(H,C=95,L,N=5){
  # H and L must be of equal length
  colsi <- c()
  for (i in 1:(length(H)-1)){
    Hi <- seq(H[i],H[i+1],length=(N+1))
    Hi[Hi<0] <- Hi[Hi<0]+360
    colsi <- c(colsi,hcl(h=Hi,c=C,l=seq(L[i],L[i+1],length=(N+1)))[ifelse(i==1,1,2):(N+1)])
  }
  colsi
}

H <- seq(255,-60,length=6)
L <- seq(75,15,length=6) # L is luminance
L[1] <- 99 # we blend in for near-white
# cols = colors, brks = breaks, labs = labels
cols <- myfxHCLramp(H=H,L=L,N=2)
brks <- seq(0,.25,length.out=11)
labs <- c(">0","0.025", "0.050", "0.075", "0.100", "0.125", "0.150", "0.175", "0.200", "0.225", "0.250")

#ASFR -- age specific fertility rate 

library(ggplot2)
dev.new(width=14,height=6)
FRplot <- (LexFx <-  ggplot(datapoly, aes(x=Year, y=Age)) + geom_polygon(aes(fill=ASFR, group=ID)) +
             scale_fill_gradientn(colors= cols,limits=c(0,.25),breaks=brks,labels=labs,space="Lab") + 
             theme_bw() +
             coord_equal(ratio = 1) + 
             scale_x_continuous(expand=c(0,0),breaks=seq(1946,2018,by=10),labels=seq(1946,2018,by=10),limits=c(1946,2018)) + 
             scale_y_continuous(expand=c(0,0),breaks=seq(15,55,by=10),labels=seq(15,55,by=10),limits=c(12,56)) +
             geom_vline(xintercept=seq(1946,2018,by=10),colour="#80808030") +
             geom_hline(yintercept=seq(15,55,by=10),colour="#80808030") +
             geom_abline(aes(intercept=a, slope=b),data=FR,colour="#80808030") +
             ggtitle("France")
)
