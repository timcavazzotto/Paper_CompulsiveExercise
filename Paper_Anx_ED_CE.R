## Create by Timothy Cavazzotto, Ph.D.
## 11/09/2021


# PACKAGES
library(metafor) 
library(meta)


# Load Data
load("Meta1.RData") #dataBF1
load("Meta2.RData") #dataBF2
load("Meta3.RData") #dataBF3
load("Meta4.RData") #dataBF4

dataBF4 <- dataBF4[-c(6,7), ] #Excluding studies with men

##### meta #####

meta1<-metacont(n.e = dataBF1$n_CE, 
                mean.e = dataBF1$ANS_m_CE, 
                sd.e = dataBF1$ANS_SD_CE,
                n.c = dataBF1$n_NCE,
                mean.c = dataBF1$ANS_m_NCE,
                sd.c = dataBF1$ANS_SD_NCE,
                studlab = dataBF1$author,
                method.tau = "REML",
                sm= "SMD")

summary(meta1) #significance of heterogeneity
funnel(meta1,
       comb.random=T,
       comb.fixed=T,
       studlab=F,
       xlab="Standardized Mean Difference",
       ylab="Standard Error",
       xlim=c(-2.5,2.5)) #Funel plot
metabias(meta1, method.bias = "linreg",k.min = 5)

forest(meta1,
       sortvar = TE,
       rightlabs = c("SMD","95% CI", "Weight"),
       text.random = "Random Effect",
       smlab = "SMD",
       digits.sd = 2,
       leftlabs=c("", "n", "M","SD", "n", "M", "SD"),
       lab.e = "CE",
       lab.c = "NCE",
       col.diamond="black", 
       col.diamond.lines="black",
       col.label.left = "black", 
       col.by = "black",
       bylab = "",
       hetlab = "Heterogeneity: ",
       label.left = "NCE",
       label.right = "CE",
       weight.study = "random"
  )


##### meta ANS_TA #####

meta2<-metacont(n.e = dataBF2$n_CE, 
                mean.e = dataBF2$ANS_m_CE, 
                sd.e = dataBF2$ANS_SD_CE,
                n.c = dataBF2$n_NCE,
                mean.c = dataBF2$ANS_m_NCE,
                sd.c = dataBF2$ANS_SD_NCE,
                byvar = dataBF2$TA,
                studlab = dataBF2$author,
                method.tau = "REML",
                sm= "SMD")

summary(meta2) #significance of heterogeneity
funnel(meta2,comb.random=T,comb.fixed=T,studlab=F,xlim=c(-2.5,2.5)) #Funel plot
metabias(meta2, method.bias = "linreg",k.min = 5)

forest(meta2,  
       sortvar = TE,
       rightlabs = c("SMD","95% CI", "Weight"),
       text.random = "Random Effect",
       smlab = "SMD",
       digits.sd = 2,
       leftlabs=c("", "n", "M","SD", "n", "M", "SD"),
       lab.e = "CE",
       lab.c = "NCE",
       col.diamond="black", 
       col.diamond.lines="black",
       col.label.left = "black", 
       col.by = "black",
       bylab = "",
       hetlab = "Heterogeneity: ",
       label.left = "NCE",
       label.right = "CE",
       weight.study = "random"
       
)

#### meta diagnostico ####

meta3<-metacont(n.e = dataBF3$n_CE, 
                mean.e = dataBF3$ANS_m_CE, 
                sd.e = dataBF3$ANS_SD_CE,
                n.c = dataBF3$n_NCE,
                mean.c = dataBF3$ANS_m_NCE,
                sd.c = dataBF3$ANS_SD_NCE,
                studlab = dataBF3$author,
                method.tau = "REML",
                sm= "SMD")

summary(meta3) #significance of heterogeneity
funnel(meta3,comb.random=T,comb.fixed=T,studlab=F,xlim=c(-2.5,2.5)) #Funel plot
metabias(meta3, method.bias = "linreg",k.min = 5)

forest(meta3,  
       sortvar = TE,
       rightlabs = c("SMD","95% CI", "Weight"),
       text.random = "Random Effect",
       smlab = "SMD",
       digits.sd = 2,
       leftlabs=c("", "n", "M","SD", "n", "M", "SD"),
       lab.e = "CE",
       lab.c = "NCE",
       col.diamond="black", 
       col.diamond.lines="black",
       col.label.left = "black", 
       col.by = "black",
       bylab = "",
       hetlab = "Heterogeneity: ",
       label.left = "NCE",
       label.right = "CE",
       weight.study = "random"
       )

#### meta fem ####

meta4<-metacont(n.e = dataBF4$n_CE, 
                mean.e = dataBF4$ANS_m_CE, 
                sd.e = dataBF4$ANS_SD_CE,
                n.c = dataBF4$n_NCE,
                mean.c = dataBF4$ANS_m_NCE,
                sd.c = dataBF4$ANS_SD_NCE,
                studlab = dataBF4$author,
                method.tau = "REML",
                sm= "SMD")

summary(meta4) #significance of heterogeneity
funnel(meta4,comb.random=T,comb.fixed=T,studlab=F,xlim=c(-2.5,2.5)) #Funel plot
metabias(meta4, method.bias = "linreg",k.min = 5)

forest(meta4,  
       sortvar = TE,
       rightlabs = c("SMD","95% CI", "Weight"),
       text.random = "Random Effect",
       smlab = "SMD",
       digits.sd = 2,
       leftlabs=c("", "n", "M","SD", "n", "M", "SD"),
       lab.e = "CE",
       lab.c = "NCE",
       col.diamond="black", 
       col.diamond.lines="black",
       col.label.left = "black", 
       col.by = "black",
       bylab = "",
       hetlab = "Heterogeneity: ",
       label.left = "NCE",
       label.right = "CE",
       weight.study = "random"
       
)


