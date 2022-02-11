rmANOVA = function(fileLocation = "", outputfile = "", outputfile2 = "", alf = 0.05){
  library(xlsx)
  library(dplyr)
  library(reshape)
  rawData = read.xlsx(fileLocation, sheetName = "Sheet1", colIndex = 1:90)
  nc = ncol(rawData)
  
  timeGoup = c("Child depression", "Child anxiety", "Child focus on schoolwork", "Child Grief", "Child Growth",
               "Mother internalizing behavior", "Mother externalizing behavior", "Father internalizing behavior", "Father externalizing behavior",
               "Mother anxiety/depression", "Mother withdrawn", "Mother somatic", "Mother rule-breaking", "Mother aggressive beh",
               "Father anxiety/depression", "Father withdrawn", "Father somatic", "Father rule-breaking", "Father aggressive beh")

  efftimeAge = vector()
  
  
  
  b = rawData
#  dataN = list()
  for(i in 0:18){
    u = i + 1
    cat("#####################################################################################################", "\n", "\n",file = outputfile,append = TRUE)
    cat(timeGoup[i+1], ":", "\n", "\n",file = outputfile,append = TRUE)
    temp= cbind(b[,c(1:8)], b[,c((8+i*4+1):(8+i*4+4))])
    temp[temp == "#NULL!"] <- NA
    temp2 = na.omit(temp)
#    dataN[[i+1]] = temp2
    
    cat("Age with Time", "\n", "\n",file = outputfile,append = TRUE)
    mdata1 <- melt(temp2[,c(1,3,9:12)], id=c("CHILDID","CAgeGrp")) 
    colnames(mdata1) = c("CHILDID","CAgeGrp","Time","value")
    mdata1$CAgeGrp <- as.factor(mdata1$CAgeGrp)
    mdata1$Time <- as.factor(mdata1$Time)
    mdata1$value <- as.numeric(mdata1$value)
    mod1 <- lm(value ~ CAgeGrp + Time + CAgeGrp:Time, data = mdata1)
    ANOVA1 <- summary(aov(mod1))
    capture.output(ANOVA1, file = outputfile, append = TRUE)
    
    FCAgeGrp <-  ANOVA1[[1]]$"F value"[1]   
    FTime <-  ANOVA1[[1]]$"F value"[2]
    FinteractCT <-  ANOVA1[[1]]$"F value"[3]
    
    nreps <- 2000 
    FC <- numeric(nreps)    #Set up space to store F values as calculated.
    FT <- numeric(nreps)  
    FCT <- numeric(nreps)
    FC[1] <- FCAgeGrp          # The first F of our 5000 
    FT[1] <- FTime
    FCT[1] <- FinteractCT
    
    nl = nrow(mdata1)
    
    for (i in 2:nreps) {
      set.seed(123+i*10)
      newdata <- mdata1[sample(nrow(mdata1), (nl-5)), ]
      mod1t <- lm(value ~ CAgeGrp + Time + CAgeGrp:Time, data = newdata)
      ts <- summary(aov(mod1t))
      FC[i] <- ts[[1]]$"F value"[1]
      FT[i] <- ts[[1]]$"F value"[2]
      FCT[i] <- ts[[1]]$"F value"[3]
    }
    probC <- length(FC[FC >= FCAgeGrp + .Machine$double.eps ^0.5])/nreps
    probT <- length(FT[FT >= FTime+ .Machine$double.eps ^0.5])/nreps       
    probCT  <-  length(FCT[FCT >= FinteractCT + .Machine$double.eps ^0.5])/nreps
    
    efftimeAge = append(efftimeAge, probC)
    
    cat("\n", file = outputfile, append = TRUE)
    cat(" The probability for the effect of interaction between Age and Time is ",probCT, "\n", file = outputfile,append = TRUE)
    cat(" The probability for the effect of Age is ", probC, "\n", file = outputfile, append = TRUE)
    cat(" The probability for the effect of Time is ", probT, "\n", file = outputfile,append = TRUE)
    cat("\n", file = outputfile,append = TRUE)
    
    
    ########################################################

    cat("Sex with time", "\n", "\n",file = outputfile,append = TRUE)
    mdata2 <- melt(temp2[,c(1,4,9:12)], id=c("CHILDID","GENDERC")) 
    colnames(mdata2) = c("CHILDID","GENDERC","Time","value")
    mdata2$GENDERC <- as.factor(mdata2$GENDERC)
    mdata2$Time <- as.factor(mdata2$Time)
    mdata2$value <- as.numeric(mdata2$value)
    mod2 <- lm(value ~ GENDERC + Time + GENDERC:Time, data = mdata2)
    ANOVA2 <- summary(aov(mod2))
    capture.output(ANOVA2, file = outputfile,append = TRUE)
    
    FGENDERC <-  ANOVA2[[1]]$"F value"[1]   
    FTime2 <-  ANOVA2[[1]]$"F value"[2]
    FinteractGT <-  ANOVA2[[1]]$"F value"[3]
    
    nreps <- 2000 
    FG <- numeric(nreps)    #Set up space to store F values as calculated.
    FT2 <- numeric(nreps)  
    FGT <- numeric(nreps)
    FG[1] <- FGENDERC          # The first F of our 5000 
    FT2[1] <- FTime2
    FGT[1] <- FinteractGT
    
    nl = nrow(mdata2)
    
    for (i in 2:nreps) {
      set.seed(123+i*11)
      newdata <- mdata2[sample(nrow(mdata2), (nl-5)), ]
      mod2t <- lm(value ~ GENDERC + Time + GENDERC:Time, data = newdata)
      ts <- summary(aov(mod2t))
      FG[i] <- ts[[1]]$"F value"[1]
      FT2[i] <- ts[[1]]$"F value"[2]
      FGT[i] <- ts[[1]]$"F value"[3]
    }
    probG <- length(FG[FG >= FGENDERC + .Machine$double.eps ^0.5])/nreps
    probT2 <- length(FT2[FT2 >= FTime2+ .Machine$double.eps ^0.5])/nreps       
    probGT  <-  length(FGT[FGT >= FinteractGT + .Machine$double.eps ^0.5])/nreps
    
    cat("\n", file = outputfile,append = TRUE)
    cat(" The probability for the effect of interaction between Sex and Time is ",probGT, "\n", file = outputfile,append = TRUE)
    cat(" The probability for the effect of Sex is ", probG, "\n", file = outputfile, append = TRUE)
    cat(" The probability for the effect of Time is ", probT2, "\n", file = outputfile,append = TRUE)
    cat("\n", file = outputfile,append = TRUE)
    ########################################################
    cat("Race with time", "\n", "\n",file = outputfile, append = TRUE)
    mdata3 <- melt(temp2[,c(1,5,9:12)], id=c("CHILDID","childrace3grp")) 
    colnames(mdata3) = c("CHILDID","childrace3grp","Time","value")

    mdata3$childrace3grp <- as.factor(mdata3$childrace3grp)
    mdata3$Time <- as.factor(mdata3$Time)
    mdata3$value <- as.numeric(mdata3$value)
    mod3 <- lm(value ~ childrace3grp + Time + childrace3grp:Time, data = mdata3)
    ANOVA3 <- summary(aov(mod3))
    capture.output(ANOVA3, file = outputfile, append = TRUE)
    
    Fchildrace3grp <-  ANOVA3[[1]]$"F value"[1]   
    FTime <-  ANOVA3[[1]]$"F value"[2]
    FinteractChT <-  ANOVA3[[1]]$"F value"[3]
    
    nreps <- 2000 
    FCh <- numeric(nreps)    #Set up space to store F values as calculated.
    FT <- numeric(nreps)  
    FChT <- numeric(nreps)
    FCh[1] <- Fchildrace3grp          # The first F of our 5000 
    FT[1] <- FTime
    FChT[1] <- FinteractChT
    
    nl = nrow(mdata3)
    
    for (i in 2:nreps) {
      set.seed(123+i*12)
      newdata <- mdata3[sample(nrow(mdata3), (nl-5)), ]
      mod3t <- lm(value ~ childrace3grp + Time + childrace3grp:Time, data = newdata)
      ts <- summary(aov(mod3t))
      FCh[i] <- ts[[1]]$"F value"[1]
      FT[i] <- ts[[1]]$"F value"[2]
      FChT[i] <- ts[[1]]$"F value"[3]
    }
    probCh <- length(FCh[FCh >= Fchildrace3grp + .Machine$double.eps ^0.5])/nreps
    probT <- length(FT[FT >= FTime+ .Machine$double.eps ^0.5])/nreps       
    probChT  <-  length(FChT[FChT >= FinteractChT + .Machine$double.eps ^0.5])/nreps
    
    cat("\n", file = outputfile,append = TRUE)
    cat(" The probability for the effect of interaction between Race and Time is ",probChT, "\n", file = outputfile,append = TRUE)
    cat(" The probability for the effect of Race is ", probCh, "\n", file = outputfile, append = TRUE)
    cat(" The probability for the effect of Time is ", probT, "\n", file = outputfile,append = TRUE)
    
    cat("\n", "\n", file = outputfile,append = TRUE)
    
    ###################################################################################################
    
    signiT1 = ANOVA1[[1]][["Pr(>F)"]][2]
    signiT2 = ANOVA2[[1]][["Pr(>F)"]][2]
    signiT3 = ANOVA3[[1]][["Pr(>F)"]][2]
    signi = min(c(signiT1, signiT2, signiT3))
    
    if(signi < alf){
      cat("#####################################################################################################", "\n", "\n",file = outputfile2,append = TRUE)
      cat(timeGoup[u], ":", "\n", "\n", file = outputfile2,append = TRUE)
      cat("Main effect of time:", "\n", "\n",file = outputfile2, append = TRUE)
      mdataT <- melt(temp2[,c(1,9:12)], id=c("CHILDID")) 
      colnames(mdataT) = c("CHILDID","Time","value")
      
      mdataT$CHILDID <- as.factor(mdataT$CHILDID)
      mdataT$Time <- as.factor(mdataT$Time)
      mdataT$value <- as.numeric(mdataT$value)
      options(contrasts=c("contr.sum","contr.poly"))
      modT <- aov(value ~ Time + Error(CHILDID), data = mdataT)
      ANOVAt <- summary(modT)
      capture.output(ANOVAt, file = outputfile2, append = TRUE)
      
      FFTime <-  ANOVAt$"Error: Within"[[1]][[4]][1]
      
      nreps <- 2000
      Fsamp <- numeric(nreps)
      counter <- 0
#      FTT[1] <- FFTime
      
      nl = nrow(mdataT)
      
      for (i in 1:nreps) {
        set.seed(123+i*15)
        newdata <- mdataT[sample(nrow(mdataT), (nl-5)), ]
        modTT <- aov(value ~ Time + Error(CHILDID), data = newdata)
        ts <- summary(modTT)
        Fsamp[i] <- ts$"Error: Within"[[1]][[4]][1]
        
        if (Fsamp[i] > FFTime) {
          counter = counter + 1
        }
 
      }
      
      p <- counter/nreps
      
      cat("The probability of sampled F greater than obtained F is = ", p, '\n','\n', file = outputfile2,append = TRUE)
      cat("\n", "\n", file = outputfile2,append = TRUE)
      
      #############################################################
      #The following is pair-wised t test
      #############################################################
      
      cat("*****************************", "\n", "\n",file = outputfile2,append = TRUE)
      cat("Pairwise t test:", "\n", "\n",file = outputfile2, append = TRUE)
      
      p = pairwise.t.test(mdataT$value, mdataT$Time, p.adjust = "none", pool.sd = FALSE)
      capture.output(p, file = outputfile2, append = TRUE)
      cat("\n", "\n", file = outputfile2,append = TRUE)
      
    }
    
    signR = ANOVA3[[1]][["Pr(>F)"]][1]
    
    if(signR < alf){
#      cat("#####################################################################################################", "\n", "\n",file = outputfile2,append = TRUE)
#      cat(timeGoup[u], ":", "\n", "\n", file = outputfile2,append = TRUE)
      cat("Main effect of Race:", "\n", "\n",file = outputfile2, append = TRUE)
#      mdataT <- melt(temp2[,c(1,9:12)], id=c("CHILDID")) 
#      colnames(mdataT) = c("CHILDID","Time","value")
      
      mdata3$childrace3grp <- as.factor(mdata3$childrace3grp)
      mdata3$value <- as.numeric(mdata3$value)
      modR <- aov(value ~ childrace3grp, data = mdata3)
      ANOVAR <- summary(modR)
      capture.output(ANOVAR, file = outputfile2, append = TRUE)
      
      rTime <-  ANOVAR[[1]][["Pr(>F)"]][1]
      
      nreps <- 2000
      FsampRace <- numeric(nreps)
      counter2 <- 0
      
      nl = nrow(mdata3)
      
      for (i in 1:nreps) {
        set.seed(123+i*12)
        newdata <- mdata3[sample(nrow(mdata3), (nl-5)), ]
        modrr <- aov(value ~ childrace3grp, data = newdata)
        rs <- summary(modrr)
        FsampRace[i] <- rs[[1]][["Pr(>F)"]][1]
        
        if (FsampRace[i] > rTime) {
          counter2 = counter2 + 1
        }
        
      }
      
      pr <- counter2/nreps
      
      cat("The probability of sampled F greater than obtained F is = ", pr, '\n','\n', file = outputfile2, append = TRUE)
      cat("\n", "\n", file = outputfile2,append = TRUE)
      
      #############################################################
      #The following is pair-wised t test
      #############################################################
      
      cat("*****************************", "\n", "\n",file = outputfile2,append = TRUE)
      cat("Pairwise t test:", "\n", "\n",file = outputfile2, append = TRUE)
      
      pr = pairwise.t.test(mdata3$value, mdata3$childrace3grp, p.adjust = "none", pool.sd = FALSE)
      capture.output(pr, file = outputfile2, append = TRUE)
      cat("\n", "\n", file = outputfile2,append = TRUE)
      
    }
    
  }
  
  out = cbind(timeGoup,efftimeAge)

  return(out)
}

Mauchlytest = function(fileLocation = "", outputfile = ""){
  library(xlsx)
  library(dplyr)
  library(reshape)
  library(car)
  rawData = read.xlsx(fileLocation, sheetName = "Sheet1", colIndex = 1:90)
  nc = ncol(rawData)
  
  timeGoup = c("Child depression", "Child anxiety", "Child focus on schoolwork", "Child Grief", "Child Growth",
               "Mother internalizing behavior", "Mother externalizing behavior", "Father internalizing behavior", "Father externalizing behavior",
               "Mother anxiety/depression", "Mother withdrawn", "Mother somatic", "Mother rule-breaking", "Mother aggressive beh",
               "Father anxiety/depression", "Father withdrawn", "Father somatic", "Father rule-breaking", "Father aggressive beh")
  
#  efftimeAge = vector()
  
  
  b = rawData
  #  dataN = list()
  for(i in 0:18){
    cat("#####################################################################################################", "\n", "\n",file = outputfile,append = TRUE)
    cat(timeGoup[i+1], ":", "\n", "\n",file = outputfile,append = TRUE)
    temp= cbind(b[,c(1:8)], b[,c((8+i*4+1):(8+i*4+4))])
    temp[temp == "#NULL!"] <- NA
    temp2 = na.omit(temp)
    
    TimeMatrix = data.matrix(temp2[,9:12])
    model = lm(TimeMatrix ~ 1)
    mdata <- melt(temp2[,c(1,9:12)], id=c("CHILDID"))
    Time = levels(factor(mdata[,2]))
    
    options(contrasts=c("contr.sum", "contr.poly"))
    
    results<-Anova(model, idata=data.frame(Time), idesign=~Time, type="III")
    re = summary(results, multivariate=F)
    
    capture.output(re, file = outputfile, append = TRUE)
    
    cat("\n", "\n", file = outputfile,append = TRUE)
    
  }
}
  