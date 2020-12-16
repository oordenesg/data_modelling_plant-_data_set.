
############# Auxiliar Functions for the code of part 1 and 2 #################
library(dplyr)
library(fpc)
library(cluster)
library(lpSolve)
# Function plot.hist creates groups of histograms.
#Parameters:
#df :Data frame
#from: The first column to use.
#to: The last column to use.
#v: Number of vertical histograms.
#h: Number of horizontal histograms.
plot.hist<-function(df,from,to,v,h){
  color.mean<-"red"
  color.median<-"green"
  par(mfrow=c(v,h))
  group<-colnames(df)[from:to]
  for(i in 1:length(group)){
    hist(df[,group[i]],main = NULL,xlab = gsub('.',' ',group[i],fixed = TRUE),col = "grey66",cex.lab=1.6, cex.axis=1.6,cex.sub=1.6)
    abline(v=mean(df[,group[i]], na.rm = TRUE), col= color.mean, lty = 2,lwd = 2)
    abline(v=median(df[,group[i]],na.rm = TRUE), col = color.median, lty = 2,lwd = 2)
    title(main = paste("Histogram",gsub('.',' ',group[i],fixed = TRUE), sep = " "), line = 2.5, cex.main = 1.6)
    legend("top", c("mean","median"), col = c(color.mean,color.median), lty=2, lwd = 2,bg="transparent",cex = 1.3,pt.cex = 1.3, bty = "n",inset=c(0,-0.1),xpd = TRUE, horiz = TRUE)
  }
  par(mfrow=c(1,1))
}


# This function allows to know in summary the most correlated variables (over 70%) with the rest of the attributes.
# Parameters:
# df: correlation matrix
# over: Consider all the correlation over a certain number between 0 and 1.
multicollinearity<-function(df,over){
  df.aux <- data.frame(df)
  return(sort((rowSums(((df.aux>=over | df.aux<=-over)*1))-1)/ncol(df.aux),decreasing = TRUE))
  
}


# Function plot.boxplot creates groups of boxplots
#df :Data frame
#from: The first column to use.
#to: The last column to use.
#v: Number of vertical histograms.
#h: Number of horizontal histograms.
plot.boxplot<-function(df,from,to,v,h){
  par(mfrow=c(v,h))
  df.aux<-df
  df.aux$Class<-"Total"
  df.boxplot<-rbind(df,df.aux)
  group<-colnames(df.boxplot)[from:to]
  for(i in group){
    boxplot(df.boxplot[,i] ~ df.boxplot[,ncol(df.boxplot)], ylab = i, xlab = "Class",main = paste("Boxplot",gsub('.',' ',i,fixed = TRUE), sep = " "),col = "grey66", cex.lab=1.4,cex.axis=1, cex.main=1.4,outcex=1.8)
  }
}




# This rep.missing.values function allows to replace the missing values with zero, the mean or the median.
# Parameters
#df :Data frame 
#Operation: replace missing values with mean, median, or zero.
rep.missing.values<-function(df,operation){
  if (operation == "0"){
    df[is.na(df)]<-0
    return(df)
  }
  else if(operation == "mean"){
    for (i in 1:ncol(df[,-ncol(df)])){
      df[is.na(df[,i]),i]<-mean(df[,i], na.rm = TRUE)
    }
    return(df)
  }
  else if (operation == "median"){
    for (i in 1:ncol(df[,-ncol(df)])){
      df[is.na(df[,i]),i]<-median(df[,i], na.rm = TRUE)
    }
    return(df)
  }
  else{
    print("Enter the correct operation")
  }
}




# The boxplot.replace.mvalues function allows to create boxplots for missing value replacement techniques.
# Parameters
#df :Data frame 
#nco: Number of the column to analyse.

boxplot.replace.mvalues<-function(df,nco){
  par(mfrow=c(1,4))
  dfs.list<-list("With NA's" = df, "NA's = 0"= rep.missing.values(df,"0"), "NA's = Mean" = rep.missing.values(df,"mean"), "NA's = Median" = rep.missing.values(df,"median"))
  for (i in 1:4){
    boxplot(unlist(lapply(dfs.list, '[', nco)[i]),ylab = names(dfs.list[[i]])[nco], main = paste(names(dfs.list[[i]])[nco],names(dfs.list[[i]])[nco], sep = " ") ,cex.lab=1.6, cex.axis=1.6,cex.sub=1.6, cex.main=1.6,outcex=1.6)
    points(mean(unlist(dfs.list[[i]][nco]), na.rm = TRUE), pch = 22, col = "red", lwd = 3)
    text(mean(unlist(dfs.list[[i]][nco]),na.rm = TRUE)-15, labels = round(mean(unlist(dfs.list[[i]][nco]),na.rm = TRUE),2),cex = 1.6)
    text(y = round(boxplot.stats(unlist(dfs.list[[i]][nco]))$stats,2), labels = round(boxplot.stats(unlist(dfs.list[[i]][nco]))$stats,2), x = 1.38, cex = 1.4)
    legend("topleft", c("Mean"), col = c("red"), pch = 20,bg="transparent",cex = 1.4,pt.cex = 1.4, bty = "n")
  }
  par(mfrow=c(1,1))
}


# Normalising function normalises the values between zero and one.
# Parameters
#df: Data frame
normalising<-function(df,from,to){
  for(j in from:to){
    for(i in 1:nrow(df)){
      df[i,j]<- (df[i,j] - min(df[,j]))/(max(df[,j]) - min(df[,j]))
    }
  }
  return(df)
}



# The missing.values.rows function provides a summary of the number of records grouped by the number of missing values.
# Parameters
# df: Data frame
missing.values.rows<-function(df){
  data_rows_na<-cbind(df, empty_values = rowSums(is.na(df)))
  results<-data.frame(row.missing.values= 1:length(table(data_rows_na$empty_values)))
  rownames(results)<-sort(unique(data_rows_na$empty_values))
  for(i in 1:length(table(data_rows_na$empty_values))){
    results[i,1]<-table(data_rows_na$empty_values)[i]
    results[i,"prc.total"]<-round((results[i,1]/nrow(df))*100,1)
    results[,"cum.prc"]<-cumsum(results[,2])
  }
  return(results)
}





#The matrix.and.metrics function allows to sort the confusion matrix and get the external metrics.
# Parameters
# df: Data frame
matrix.and.metrics<-function(df){
  df2<-as.matrix(df)
  lp<-lp.assign(cost.mat = df2, direction = "max")$solution # Using the function lp.assign from lpSolve
  vector.solution<-as.vector(round(lp %*% seq_len(ncol(df2))))
  df2<-df2[,vector.solution]
  colnames(df2)<-vector.solution
  colnames(df2)<-rownames(df2)
  
  # Accuracy
  accuracy<-sum(diag(df2))/sum(df2)
  
  #Precision
  precision<- data.frame(precision=1:ncol(df2), sum_row=NA, pond=NA)
  for(i in 1:ncol(df2)){
    precision[i,1]<-df2[i,i]/sum(df2[,i])
    precision[i,2]<-sum(df2[i,])
    precision[i,3]<-(precision[i,1]*precision[i,2])/sum(df2)
  }
  precision<-sum(precision[,3])
  #Recall
  recall<- data.frame(recall=1:ncol(df2), sum_row=NA, pond=NA)
  for(i in 1:ncol(df2)){
    recall[i,1]<-df2[i,i]/sum(df2[i,])
    recall[i,2]<-sum(df2[i,])
    recall[i,3]<-(recall[i,1]*recall[i,2])/sum(df2)
  }
  recall<-sum(recall[,3])
  #F1
  f1<-2*((precision*recall)/(precision+recall))
  
  # Final output
  return(list(Cf.matrix = df2,Accuracy = accuracy, Precision = precision,Recall = recall, F1 = f1))
  
}





