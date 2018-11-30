getGDD <- function(avgT, Tbase, Tcutoff){
  if(is.na(avgT) == FALSE){
    if(avgT>Tbase && avgT<Tcutoff){
      GDDval = avgT - Tbase
    } else if (avgT >= Tcutoff){
      GDDval = Tcutoff - Tbase
    } else if (avgT<= Tbase){
      GDDval = 0
    } else {GDDval = NaN}
  } else {GDDval = NaN}
  
  return(GDDval)
}

convertF2C <- function(F){return((F-32)*(5/9))
}

getGDDvec <- function(data, idStart, tbase, tcutoff){
  vecGDD = matrix(0, nrow = nrow(data), ncol = 1)
  for(i in 1:nrow(data)){
    if(i<idStart){
      vecGDD[i] = 0
    } else{
      if (is.nan(data$avg[i])==FALSE || is.na(data$avg[i])==FALSE){
        vecGDD [i] = getGDD(data$avg[i], tbase, tcutoff)
      } else{
        vecGDD [i] = NaN
      }
      
    }
  }
  return(vecGDD)
}

accumulateGDD<- function(vecGDD, idStart){
  vecAc = matrix(0, nrow = length(vecGDD), ncol = 1)
  for(i in 1:length(vecGDD)){
    if(i<idStart){
      vecAc[i] = 0
    } else if (i== idStart){
      vecAc[i] = vecGDD[i]
    }
     else{
      vecAc[i]  = vecAc[i-1] + vecGDD[i]
    }
  }
  return(vecAc)
}

analyzeData <- function(tempSet, Nyears, idyearStart, tbase, tcutoff, threshAc){
  yearVec = format(as.Date(tempSet$date, format="%d/%m/%Y"),"%Y")
  data = list()
  ncycle = 5;
  dayGDD = matrix(NaN, nrow=Nyears, ncol = (ncycle+1))
  for(i in 1:(Nyears)){
    print(paste("Analyzing the year ", idyearStart, sep = ''))
    idyear = idyearStart 
    data1 = tempSet[which(yearVec== as.character(idyear)),]
    dateStart = as.Date(paste(idyear, 4,20,sep = "-" ))
    idStart = which(data1$date==dateStart)
    vecGDD <- getGDDvec(data1, idStart, tbase, tcutoff)
    data1$GDD = vecGDD
    vecAcGDD = accumulateGDD(vecGDD, idStart, threshAc)
    data1$AcGDD = vecAcGDD
    dayofYear = which(vecAcGDD>=threshAc)
    nSetGDD = length(dayofYear)
    dayGDD[i, 1] =idyear
    if(nSetGDD<=ncycle){
      if(nSetGDD!=0){
        dateofYear = data1$date[dayofYear]
        data = rbind(data, data1)
        for(j in 1:nSetGDD){
          dayGDD[i, j+1] = dayofYear[j]
        }
        #SdayGDD = rbind(dayGDD, dayofYear[1:3])
        
      }
    }
    idyearStart = idyear + 1
  }
  dataList = list(data= data, dayofYear=dayGDD)
  return(dataList)
}


analyzeData1 <- function(tempSet, Nyears, idyearStart, tbase, tcutoff, threshAc){
  yearVec = format(as.Date(tempSet$date, format="%d/%m/%Y"),"%Y")
  data = list()
  ncycle = length(threshAc);
  dayGDD = matrix(NaN, nrow=Nyears, ncol = (ncycle+1))
  for(i in 1:(Nyears)){
    print(paste("Analyzing the year ", idyearStart, sep = ''))
    idyear = idyearStart 
    dayGDD[i, 1] = idyear 
    data1 = tempSet[which(yearVec== as.character(idyear)),]
    dateStart = as.Date(paste(idyear, 1,1,sep = "-" ))
    idStart = which(data1$date==dateStart)
    vecGDD <- getGDDvec(data1, idStart, tbase, tcutoff)
    data1$GDD = vecGDD
    GDDdateStart = as.Date(paste(idyear, 1,1,sep = "-" ))
    idStart = which(data1$date==GDDdateStart)  
    for(j in 1:ncycle){
      vecAcGDD = accumulateGDD(vecGDD, idStart)
      idThresholds = which(vecAcGDD>=threshAc[j])
      if (length(idThresholds)!=0){
        dayofYear = idThresholds[1]
        idStart = dayofYear + 1
      } else {dayofYear = NaN}
      
      dayGDD[i, j+1] = dayofYear
    }
    
    # data1$AcGDD = vecAcGDD
    idyearStart = idyear + 1
  }
  dataList = list(data= data, dayofYear=dayGDD)
  return(dataList)
}


analyzeData2 <- function(tempSet, Nyears, idyearStart, tbase, tcutoff, threshAc){
  yearVec = format(as.Date(tempSet$date, format="%d/%m/%Y"),"%Y")
  data = list()
  ncycle = length(threshAc);
  dayGDD = matrix(NaN, nrow=Nyears, ncol = (ncycle+1))
  for(i in 1:(Nyears)){
    print(paste("Analyzing the year ", idyearStart, sep = ''))
    idyear = idyearStart 
    dayGDD[i, 1] = idyear 
    data1 = tempSet[which(yearVec== as.character(idyear)),]
    dateStart = as.Date(paste(idyear, 4,20,sep = "-" ))
    idStart = which(data1$date==dateStart)
    vecGDD <- getGDDvec(data1, idStart, tbase, tcutoff)
    data1$GDD = vecGDD
    GDDdateStart = as.Date(paste(idyear, 4,20,sep = "-" ))
    idStart = which(data1$date==GDDdateStart)  
    for(j in 2:ncycle){
      vecAcGDD = accumulateGDD(vecGDD, idStart)
      idThresholds = which(vecAcGDD>=threshAc[j])
      if (length(idThresholds)!=0){
        dayofYear = idThresholds[1]
        idStart = dayofYear + 1
      } else {dayofYear = NaN}
      
      dayGDD[i, j+1] = dayofYear
    }
    
    # data1$AcGDD = vecAcGDD
    idyearStart = idyear + 1
  }
  dataList = list(data= data, dayofYear=dayGDD)
  return(dataList)
}

setDat <- function(minTemp, maxTemp, idSet){
  nRecords = nrow(minTemp)
  idyearStart = minTemp[1,1]
  idyearEnd = minTemp[nRecords,1]
  Nyears = idyearEnd - idyearStart + 1
  dateVec = as.Date(paste(minTemp[,1],minTemp[,2],minTemp[,3],sep = "-" ))
  avgTemp = (minTemp[,idSet] + maxTemp[,idSet])/2
  tempRcP45 = data.frame(date=dateVec, min=minTemp[,idSet], max=maxTemp[,idSet], avg=avgTemp)
  setLists = list(set = tempRcP45, idyearStart = idyearStart, Nyears = Nyears)
  return(setLists)
}

list2mat <- function(inlist){
  ndim = ncol(inlist)
  ndata = nrow(inlist)
  matlist = matrix(0, nrow = ndata, ncol = ndim)
  for(i in 1:ndim){
    unlistI = unlist(inlist[,i])
    for(j in 1:ndata){
      matlist[j, i] = unlistI[j]
    }
  }
  return(matlist)
} 

avgData <- function(dataX, tau){
  ndata = nrow(dataX)
  X = dataX$X
  Y = dataX$Y
  avgData = matrix(NaN, nrow = (ndata-tau+1), ncol = 2)
  for(i in 1:(ndata-tau+1)){
      Y1 = Y[i:(i+tau-1)]
      avgData[i,1] = X[i]
      avgData[i,2] = round(mean(Y1, na.rm = TRUE))
  }
  return(avgData)
}

# getStats<- function(data)

dirName <- function(methodName, cropName, siteName){
  dir0 = 'Output'
  if (dir.exists(dir0)==FALSE){dir.create(dir0)}
  dir2Save = paste(dir0, '/', cropName, sep = '')
  if (dir.exists(dir2Save)==FALSE){dir.create(dir2Save)}
  dir2Save = paste(dir2Save, '/', siteName, sep = '')
  if (dir.exists(dir2Save)==FALSE){dir.create(dir2Save)}
  dir2Save = paste(dir2Save, '/', methodName, sep = '')
  if (dir.exists(dir2Save)==FALSE){dir.create(dir2Save)}
  return(dir2Save)
}

day2month<-function(data){
  ndata = length(data)
  if(ndata%%4 == 0){
    ndaysVec = c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  } else{
    ndaysVec = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  }
  cumDays = cumsum(ndaysVec)
  cumData = cumsum(data)
  cumDataMonth = cumData[cumDays]
  monthData = matrix(NaN, nrow = 1, ncol = 12)
  monthData[1] = cumDataMonth[1]
  for(i in 2:12){
    monthData[i] = cumDataMonth[i]-cumDataMonth[i-1]
  }
  return(monthData/ndaysVec)
  
}

analyzePlot<- function(siteName, methodName, cropName, threshAc, countyName, latVal, longVal){
  fileMin = paste('Data', siteName, 'tasmin.csv', sep = '/')
  fileMax = paste('Data', siteName, 'tasmax.csv', sep = '/')
  minTemp = read.csv(fileMin, header = F, sep = ",")
  maxTemp = read.csv(fileMax, header = F, sep = ",")
  dir2Save = dirName(methodName, cropName, siteName)
  Tbase = 54.86
  Tcutoff = 95
  tbase = convertF2C(Tbase)
  tcutoff = convertF2C(Tcutoff)
  threshAc = convertF2C(threshAc)
  
  tempRcP45 <- setDat(minTemp, maxTemp, 4)
  if(methodName==1){
    result.RCP45 = analyzeData1 (tempRcP45$set, tempRcP45$Nyears, tempRcP45$idyearStart, tbase, tcutoff, threshAc)
  } else {
    result.RCP45 = analyzeData2 (tempRcP45$set, tempRcP45$Nyears, tempRcP45$idyearStart, tbase, tcutoff, threshAc)
  }
  calcSetRcP45  <- result.RCP45$data
  dayofYearRCP45 <- result.RCP45$dayofYear
  colnames(dayofYearRCP45) <- c('Year', 'First', 'Second', 'Third', 'Fourth', 'Fifth', 'Sixth')
  fileOut.csv = paste(dir2Save, '/RCP45Year.csv', sep = '')
  write.csv(dayofYearRCP45, fileOut.csv) 
  tempRcP85 <- setDat(minTemp, maxTemp, 5)
  if(methodName==1){
    result.RCP85 = analyzeData1 (tempRcP85$set, tempRcP85$Nyears, tempRcP85$idyearStart, tbase, tcutoff, threshAc)
  } else {
    result.RCP85 = analyzeData2 (tempRcP85$set, tempRcP85$Nyears, tempRcP85$idyearStart, tbase, tcutoff, threshAc)
  }
  calcSetRcP85  <- result.RCP85$data
  dayofYearRCP85 = result.RCP85$dayofYear
  colnames(dayofYearRCP85) <- c('Year', 'First', 'Second', 'Third', 'Fourth', 'Fifth', 'Sixth')
  fileOut.csv = paste(dir2Save, '/RCP85Year.csv', sep = '')
  write.csv(dayofYearRCP85, fileOut.csv)  
  idyearStart = tempRcP45$idyearStart
  Nyears = tempRcP45$Nyears
  idyearEnd = idyearStart + Nyears-1
  yearVec = idyearStart:idyearEnd
  cycleList = c('First', 'Second', 'Third', 'Fourth', 'Fifth', 'Sixth')
  X = yearVec
  idhistYearEnd = which(X==2005)
  for(i in 1:6){
    fileName = paste(dir2Save, "/Figure", i, ".png", sep = '')
    Xhis = X[1:idhistYearEnd]
    Y = dayofYearRCP45[,i+1]
    Y1 = dayofYearRCP85[,i+1]
    meanObs = mean(Y[1:idhistYearEnd], na.rm = TRUE)
    Yhis = Y
    Yhis[(idhistYearEnd+1):Nyears]= "NaN"
    Xtrend = X[(idhistYearEnd+1):Nyears]
    YR45 =Y
    YR45[1:idhistYearEnd] = "NaN"
    meanR45 = mean(Y[(idhistYearEnd+1):Nyears], na.rm = TRUE)
    meanR85 = mean(Y1[(idhistYearEnd+1):Nyears], na.rm = TRUE)
    textH = paste('Mean (Historical) = ', sprintf("%04.3f", meanObs), sep = "")
    text45 = paste('Mean (R4.5) = ', sprintf("%04.3f", meanR45), sep = "")
    text85 = paste('Mean (R8.5) = ', sprintf("%04.3f", meanR85), sep = "")
    lengNan <- length(which(Y=="NaN"))
    lengNan1 <- length(which(Y1=="NaN"))
    if(meanObs != "NaN" || meanR45 !="NaN" || meanR85 != "NaN"){
      YR85 = Y1
      YR85[1:idhistYearEnd] = "NaN"
      graphTitle = paste(cycleList[i], " Cycle for ", countyName, ' (', latVal, ', ', longVal, ')')
      png(filename = fileName, width = 1000, height = 800)
      leg.text = c("Historical", "RCP4.5", "RCP8.5")
      plot(X, Yhis, type = 'l', col = "black", lwd = 2,
           main = graphTitle, xlab = "Year", ylab =  'Days to Matuarity', xaxs ='i', xaxt='n',
           xlim = c(yearVec[1], yearVec[length(yearVec)]), ylim = c(min(Y, Y1, na.rm = TRUE)*0.9, max(Y, Y1, na.rm = TRUE)*1.1), 
           cex.lab=1.5, cex.axis=1.5, cex.main=2)
      axis(1, at = c(seq(yearVec[1], yearVec[length(yearVec)], by=10),yearVec[length(yearVec)]), cex.axis=1.5)
      lines(X, YR45, col="blue", lty=1, cex=1, lwd = 2)
      lines(X, YR85, col="red", lty=1, cex=1, lwd = 2)
      op <- par(cex = 2)
      legend('bottomleft',  legend = leg.text, horiz = F, 
             col = c("black", "blue", "red"), lty = 1, 
             lwd =  2, cex = 1.1, box.lty = 0, pch=c(NA,NA) , bty = "n")
      dev.off()
      #}
      
    }
  }
  
  dataX = data.frame(X = dayofYearRCP45[,1], Y= dayofYearRCP45[,2])
  avg1RCP45 = avgData(dataX,10)
  dataX = data.frame(X = dayofYearRCP45[,1], Y= dayofYearRCP45[,3])
  avg2RCP45 = avgData(dataX,10)
  dataX = data.frame(X = dayofYearRCP45[,1], Y= dayofYearRCP45[,4])
  avg3RCP45 = avgData(dataX,10)
  dataX = data.frame(X = dayofYearRCP45[,1], Y= dayofYearRCP45[,5])
  avg4RCP45 = avgData(dataX,10)
  df.RCP45 = data.frame(decade = avg1RCP45[,1], cGDD1 = avg1RCP45[,2], cGDD2 = avg2RCP45[,2], 
                        cGDD3 = avg3RCP45[,2], cGDD4 = avg4RCP45[,2])
  fileOut.csv = paste(dir2Save, '/RCP45Decade.csv', sep = '')
  write.csv(df.RCP45, fileOut.csv)
  
  dataX = data.frame(X = dayofYearRCP85[,1], Y= dayofYearRCP85[,2])
  avg1RCP85 = avgData(dataX,10)
  dataX = data.frame(X = dayofYearRCP85[,1], Y= dayofYearRCP85[,3])
  avg2RCP85 = avgData(dataX,10)
  dataX = data.frame(X = dayofYearRCP85[,1], Y= dayofYearRCP85[,5])
  avg3RCP85 = avgData(dataX,10)
  dataX = data.frame(X = dayofYearRCP85[,1], Y= dayofYearRCP85[,5])
  avg4RCP85 = avgData(dataX,10)
  df.RCP85 = data.frame(decade = avg1RCP85[,1], cGDD1 = avg1RCP85[,2], cGDD2 = avg2RCP85[,2], 
                        cGDD3 = avg3RCP85[,2], cGDD4 = avg4RCP85[,2])
  fileOut.csv = paste(dir2Save, '/RCP85Decade.csv', sep = '')
  write.csv(df.RCP85, fileOut.csv)
  return(list(RCP45 = dayofYearRCP45, RCP85=dayofYearRCP85))
}

interpretSet<-function(locName, modelList, cropName, siteName, methodName, idscen){
  dataSet1 = data.frame(matrix(NaN, nrow=150, ncol=(1+length(modelList))))
  dataSet2 = data.frame(matrix(NaN, nrow=150, ncol=(1+length(modelList))))
  dataSet3 = data.frame(matrix(NaN, nrow=150, ncol=(1+length(modelList))))
  dataSet4 = data.frame(matrix(NaN, nrow=150, ncol=(1+length(modelList))))
  dataSet5 = data.frame(matrix(NaN, nrow=150, ncol=(1+length(modelList))))
  colnames(dataSet1) = c('Year', modelList)
  colnames(dataSet2) = c('Year', modelList)
  colnames(dataSet3) = c('Year', modelList)
  colnames(dataSet4) = c('Year', modelList)
  colnames(dataSet5) = c('Year', modelList)
  if(idscen==1){
    fileName2Read1 = 'RCP45Year'
  }else{
    fileName2Read1 = 'RCP85Year'
  }
  for(idModel in 1:length(modelList)){
    siteName = paste(locName, modelList[idModel], sep = '')
    dir2Read = paste('Output/', cropName, '/', siteName, '/', methodName, sep = '')
    fileName2Read = paste(dir2Read, '/', fileName2Read1, '.csv', sep = '')
    dataRCP = read.csv(fileName2Read)
    dataSet1[,1] = dataRCP$Year
    dataSet2[,1] = dataRCP$Year
    dataSet3[,1] = dataRCP$Year
    dataSet4[,1] = dataRCP$Year
    dataSet5[,1] = dataRCP$Year
    dataSet1[,idModel+1] = dataRCP$First
    dataSet2[,idModel+1] = dataRCP$Second
    dataSet3[,idModel+1] = dataRCP$Third
    dataSet4[,idModel+1] = dataRCP$Fourth
    dataSet5[,idModel+1] = dataRCP$Fifth
  }
  idHist = which(dataSet1$Year>2005)
  dataSet1$avgHist=rowMeans(dataSet1[, 2:ncol(dataSet1)])
  dataSet1$avgHist[idHist] = NaN
  dataSet2$avgHist=rowMeans(dataSet2[, 2:ncol(dataSet2)])
  dataSet2$avgHist[idHist] = NaN
  dataSet3$avgHist=rowMeans(dataSet3[, 2:ncol(dataSet3)])
  dataSet3$avgHist[idHist] = NaN
  dataSet4$avgHist=rowMeans(dataSet4[, 2:ncol(dataSet4)])
  dataSet4$avgHist[idHist] = NaN
  dataSet5$avgHist=rowMeans(dataSet5[, 2:ncol(dataSet5)])
  dataSet5$avgHist[idHist] = NaN  
  idProj = which(dataSet1$Year<=2005)
  dataSet1[idProj, 2:(ncol(dataSet1)-1)]=NaN
  dataSet2[idProj, 2:(ncol(dataSet2)-1)]=NaN
  dataSet3[idProj, 2:(ncol(dataSet3)-1)]=NaN
  dataSet4[idProj, 2:(ncol(dataSet4)-1)]=NaN
  dataSet5[idProj, 2:(ncol(dataSet5)-1)]=NaN
  dataSet1$avgProj =rowMeans(dataSet1[, 2:(ncol(dataSet1)-2)], na.rm = TRUE)
  dataSet2$avgProj =rowMeans(dataSet2[, 2:(ncol(dataSet2)-2)], na.rm = TRUE)
  dataSet3$avgProj =rowMeans(dataSet3[, 2:(ncol(dataSet3)-2)], na.rm = TRUE)
  dataSet4$avgProj =rowMeans(dataSet4[, 2:(ncol(dataSet4)-2)], na.rm = TRUE)
  dataSet5$avgProj =rowMeans(dataSet5[, 2:(ncol(dataSet5)-2)], na.rm = TRUE)
  
  dir2Save = paste('Output/', methodName, locName, cropName, fileName2Read1, sep = '')
  dir.create(dir2Save)
  fileName2Image = paste(dir2Save, '/', fileName2Read1, 'Figure1.png', sep = '')
  newDataSet1 = ggSet(dataSet1)
  dd = melt(newDataSet1, id=c("Year"))
  Figure1<-ggplot(dd) + geom_line(aes(x=Year, y=value, colour=variable)) +
    labs(title = 'First Cycle',x='Year', y = 'Days to mature', color = 'Legend\n')+
    scale_colour_manual(name='Legend', values=c('black', 'cyan', 'blue', 'orange', 'yellow', 'green', 
                                                'red', 'brown', 'gray', 
                                                'tomato', 'salmon', 'magenta'))+
    theme_bw()
  ggsave(fileName2Image, plot = Figure1, width = 8, height = 4, units = 'in', dpi = 300)
  
  fileName2Image = paste(dir2Save, '/', fileName2Read1, 'Figure2.png', sep = '')
  newDataSet2 = ggSet(dataSet2)
  dd = melt(newDataSet2, id=c("Year"))
  Figure2<-ggplot(dd) + geom_line(aes(x=Year, y=value, colour=variable)) +
    labs(title = 'Second Cycle',x='Year', y = 'Days to mature', color = 'Legend\n')+
    scale_colour_manual(name='Legend', values=c('black', 'cyan', 'blue', 'orange', 'yellow', 'green', 
                                                'red', 'brown', 'gray', 
                                                'tomato', 'salmon', 'magenta')) +
    theme_bw() 
  ggsave(fileName2Image, plot = Figure2, width = 8, height = 4, units = 'in', dpi = 300)
  
  fileName2Image = paste(dir2Save, '/', fileName2Read1, 'Figure3.png', sep = '')
  newDataSet3 = ggSet(dataSet3)
  dd = melt(newDataSet3, id=c("Year"))
  Figure3<-ggplot(dd) + geom_line(aes(x=Year, y=value, colour=variable)) +
    labs(title = 'Third Cycle',x='Year', y = 'Days to mature', color = 'Legend\n')+
    scale_colour_manual(name='Legend', values=c('black', 'cyan', 'blue', 'orange', 'yellow', 'green', 
                                                'red', 'brown', 'gray', 
                                                'tomato', 'salmon', 'magenta')) +
    theme_bw()
  ggsave(fileName2Image, plot = Figure3, width = 8, height = 4, units = 'in', dpi = 300)
  
  fileName2Image = paste(dir2Save, '/', fileName2Read1, 'Figure4.png', sep = '')
  newDataSet4 = ggSet(dataSet4)
  dd = melt(newDataSet4, id=c("Year"))
  Figure4<-ggplot(dd) + geom_line(aes(x=Year, y=value, colour=variable)) +
    labs(title = 'Fourth Cycle',x='Year', y = 'Days to mature', color = 'Legend\n')+
    scale_colour_manual(name='Legend', values=c('black', 'cyan', 'blue', 'orange', 'yellow', 'green', 
                                                'red', 'brown', 'gray', 
                                                'tomato', 'salmon', 'magenta')) +
    theme_bw()
  ggsave(fileName2Image, plot = Figure4, width = 8, height = 4, units = 'in', dpi = 300)
  
  fileName2Image = paste(dir2Save, '/', fileName2Read1, 'Figure5.png', sep = '')
  
  newDataSet5 = ggSet(dataSet5)
  dd = melt(newDataSet5, id=c("Year"))
  Figure5<-ggplot(dd) + geom_line(aes(x=Year, y=value, colour=variable)) +
    labs(title = 'Fifth Cycle',x='Year', y = 'Days to mature', color = 'Legend\n')+
    scale_colour_manual(name='Legend', values=c('black', 'cyan', 'blue', 'orange', 'yellow', 'green', 
                                                'red', 'brown', 'gray', 
                                                'tomato', 'salmon', 'magenta')) +
    theme_bw()
  ggsave(fileName2Image, plot = Figure5, width = 8, height = 4, units = 'in', dpi = 300)
  return(list(dataFirst = newDataSet1, dataSecond = newDataSet2, 
              dataThird = newDataSet3, dataFourth = newDataSet4, dataFifth = newDataSet5, 
              Fig1 = Figure1, Fig1 = Figure2, Fig1 = Figure3, Fig1 = Figure4, Fig1 = Figure5))
  
}

interpretSet2<-function(locName, modelList, cropName, siteName, methodName, ModelType){
  dataSet1 = data.frame(matrix(NaN, nrow=150, ncol=(1+length(modelList))))
  dataSet2 = data.frame(matrix(NaN, nrow=150, ncol=(1+length(modelList))))
  dataSet3 = data.frame(matrix(NaN, nrow=150, ncol=(1+length(modelList))))
  dataSet4 = data.frame(matrix(NaN, nrow=150, ncol=(1+length(modelList))))
  dataSet5 = data.frame(matrix(NaN, nrow=150, ncol=(1+length(modelList))))
  colnames(dataSet1) = c('Year', modelList)
  colnames(dataSet2) = c('Year', modelList)
  colnames(dataSet3) = c('Year', modelList)
  colnames(dataSet4) = c('Year', modelList)
  colnames(dataSet5) = c('Year', modelList)
  fileName2Read1 = paste(ModelType, 'Year', sep = '')
  for(idModel in 1:length(modelList)){
    siteName = paste(locName, modelList[idModel], sep = '')
    dir2Read = paste('Output/', cropName, '/', siteName, '/', methodName, sep = '')
    fileName2Read = paste(dir2Read, '/', fileName2Read1, '.csv', sep = '')
    dataRCP = read.csv(fileName2Read)
    dataSet1[,1] = dataRCP$Year
    dataSet2[,1] = dataRCP$Year
    dataSet3[,1] = dataRCP$Year
    dataSet4[,1] = dataRCP$Year
    dataSet5[,1] = dataRCP$Year
    dataSet1[,idModel+1] = dataRCP$First
    dataSet2[,idModel+1] = dataRCP$Second
    dataSet3[,idModel+1] = dataRCP$Third
    dataSet4[,idModel+1] = dataRCP$Fourth
    dataSet5[,idModel+1] = dataRCP$Fifth
  }
  idHist = which(dataSet1$Year>2005)
  dataSet1$avgHist=rowMeans(dataSet1[, 2:ncol(dataSet1)])
  dataSet1$avgHist[idHist] = NaN
  dataSet2$avgHist=rowMeans(dataSet2[, 2:ncol(dataSet2)])
  dataSet2$avgHist[idHist] = NaN
  dataSet3$avgHist=rowMeans(dataSet3[, 2:ncol(dataSet3)])
  dataSet3$avgHist[idHist] = NaN
  dataSet4$avgHist=rowMeans(dataSet4[, 2:ncol(dataSet4)])
  dataSet4$avgHist[idHist] = NaN
  dataSet5$avgHist=rowMeans(dataSet5[, 2:ncol(dataSet5)])
  dataSet5$avgHist[idHist] = NaN  
  idProj = which(dataSet1$Year<=2005)
  dataSet1[idProj, 2:(ncol(dataSet1)-1)]=NaN
  dataSet2[idProj, 2:(ncol(dataSet2)-1)]=NaN
  dataSet3[idProj, 2:(ncol(dataSet3)-1)]=NaN
  dataSet4[idProj, 2:(ncol(dataSet4)-1)]=NaN
  dataSet5[idProj, 2:(ncol(dataSet5)-1)]=NaN
  dataSet1$avgProj =rowMeans(dataSet1[, 2:(ncol(dataSet1)-2)], na.rm = TRUE)
  dataSet2$avgProj =rowMeans(dataSet2[, 2:(ncol(dataSet2)-2)], na.rm = TRUE)
  dataSet3$avgProj =rowMeans(dataSet3[, 2:(ncol(dataSet3)-2)], na.rm = TRUE)
  dataSet4$avgProj =rowMeans(dataSet4[, 2:(ncol(dataSet4)-2)], na.rm = TRUE)
  dataSet5$avgProj =rowMeans(dataSet5[, 2:(ncol(dataSet5)-2)], na.rm = TRUE)
  
  return(list(dataFirst = newDataSet1, dataSecond = newDataSet2, 
              dataThird = newDataSet3, dataFourth = newDataSet4, dataFifth = newDataSet5))
  
}

ggSet <- function(dataSet){
  newDataSet = data.frame(Year = dataSet$Year, HISTORICAL = dataSet$avgHist, 
                          ACCESS1 = dataSet$ACCESS1, CANESM21 = dataSet$CANESM21,
                          CAESM1BGC = dataSet$CAESM1BGC, CCSM4 = dataSet$CCSM4,   
                          CMCC_CMS = dataSet$CMCC_CMS,  CNRCCM5 = dataSet$CNRCCM5,  
                          GFDL_CM3 = dataSet$GFDL_CM3, HADGECC = dataSet$HADGECC,  
                          HADGEES = dataSet$HADGEES,  MICRO5 = dataSet$MICRO5, 
                          AVERAGE = dataSet$avgProj)
  return(newDataSet)
}

getStats <- function(X, Y){
  if(length(which(is.nan(Y))) ==length(Y)){
    statVal = c(NaN, NaN, NaN, NaN, NaN)
  } else{
    mu = mean(Y, na.rm = TRUE)
    std= sd(Y, na.rm = TRUE)
    med = round(median(Y, na.rm = TRUE))
    set.na = detectNA(X, Y)
    X.na = set.na$X
    Y.na = set.na$Y
    if (length(Y.na)!=0){
      modfit = lm(Y.na~X.na)
      slope = modfit$coefficients[2]
      intercept = modfit$coefficients[1]
    } else{
      modfit = NaN
      slope = NaN
      intercept = NaN
    }
    
    statVal = c(mu, std, med, slope, intercept)
    
  }
  
  return(statVal)
}
detectNA <- function(X, Y){
  idNNA <- which(is.na(Y)==FALSE & is.nan(Y)==FALSE )
  X = X[idNNA]
  Y = Y[idNNA]
  return(list(X=X, Y=Y))
}
getStatSummary <- function(df.Input){
  statsArray = data.frame(matrix(NaN, nrow = 5, ncol = (ncol(df.Input)-1) ))
  colnames(statsArray) = colnames(df.Input)[2:ncol(df.Input)]
  rownames(statsArray) = c('Mean', 'Std', 'Median', 'Slope', 'Intercept')
  statsArray[, 1] = getStats(df.Input$Year, df.Input$HISTORICAL)
  statsArray[, 2] = getStats(df.Input$Year, df.Input$ACCESS1)
  statsArray[, 3] = getStats(df.Input$Year, df.Input$CANESM21)
  statsArray[, 4] = getStats(df.Input$Year, df.Input$CAESM1BGC)
  statsArray[, 5] = getStats(df.Input$Year, df.Input$CCSM4)
  statsArray[, 6] = getStats(df.Input$Year, df.Input$CMCC_CMS)
  statsArray[, 7] = getStats(df.Input$Year, df.Input$CNRCCM5)
  statsArray[, 8] = getStats(df.Input$Year, df.Input$GFDL_CM3)
  statsArray[, 9] = getStats(df.Input$Year, df.Input$HADGECC)
  statsArray[, 10] = getStats(df.Input$Year, df.Input$HADGEES)
  statsArray[, 11] = getStats(df.Input$Year, df.Input$MICRO5)
  statsArray[, 12] = getStats(df.Input$Year, df.Input$AVERAGE)
  return(statsArray)
}

