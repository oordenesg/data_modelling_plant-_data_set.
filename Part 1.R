

#############################################################################################
########################  Data modelling ###########################################
#############################################################################################

# Libraries and dataset
library(dplyr)
source("extra_functions.R")


getwd()  
data<-read.csv("cw_dataset.csv",header = TRUE,sep = ",")  # Save the dataset in the working directory.

## 0.0 Summary data
str(data)
table(data$Class)/nrow(data)
## 0.0 Replace empty strings with NA values to avoid data problems.
is.na(data)<-data==''

############## 1.1 Explore the data ##############

## 1.1.i Summary of the data
dispersion.measures<-summary(data[,2:19]) # All numeric attributes without considering the Sample_ID variable
dispersion.measures

percentage.na.values<-round(colSums(is.na(data[,2:19]))/nrow(data),2) 
percentage.na.values

## 1.1.ii Histograms the attributes

plot.hist(data,2,4,1,3) # Centroid X, Centroid Y, Mass 
plot.hist(data,5,7,1,3) # Width, Depth, Orientation 0
plot.hist(data,8,10,1,3) # Orientation 1, Orientation 2, Orientation 3
plot.hist(data,11,13,1,3) # Orientation 4, Orientation 5, Orientation 6
plot.hist(data,14,16,1,3) # Orientation 7, Orientation 8, Orientation 9
plot.hist(data,17,19,1,3)#  Leaf Weight, Leaf Area, Leaf Hue.




############## 1.2 Explore the relationships between the attributes, and between the class and the attributes ############## 

# Correlation matrix
correlation.matrix<-cor(data[unlist(lapply(data,is.numeric))][-1], use = "pairwise.complete.obs")
correlation.matrix

# Additional Function to check the multicollinearity
multicollinearity(correlation.matrix,0.70)

## 1.2.i. Calculate the correlations and produce scatterplots for the variables: orientation 4 and orientation 7. What does this correlation tell you about the relationships of these variables?

corr.or4.or7<-correlation.matrix["Orientation..4","Orientation..7"]
corr.or4.or7

# Scatterplot

plot(data$Orientation..4, data$Orientation..7, main="Orientation 4 vs Orientation 7",xlab="Orientation 4", ylab="Orientation 7", pch=18, bty="n",cex.lab=1.5, cex.axis=1.5,cex.sub=1.5, cex.main=1.5)
abline(lm(data$Orientation..7~data$Orientation..4), col="blue", lwd=2, lty=2) # regression line (y~x)
legend("bottom", c("Linear Trend"), col = c("blue"), lty=2, lwd = 2,bg="transparent",cex = 1.4,pt.cex = 1.4, bty = "n",seg.len = 1.5)
box()


## 1.2.ii. Produce scatterplots between the class variable and orientation 4, orientation 6 and area variables.

par(mfrow=c(1,3))
plot(as.numeric(data$Class), data$Orientation..4,main = "Class vs Orientation 4",ylab = "Orientation 4", xlab = "Class",xaxt = "n", col = data$Class, cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5,  cex = 1.5)
axis(1, at=1:5,labels=unique(data$Class), cex.axis = 1.5)
plot(as.numeric(data$Class), data$Orientation..6,main = "Class vs Orientation 6",ylab = "Orientation 6",xlab = "Class",xaxt = "n", col = data$Class, cex.lab = 1.5, cex.axis = 1.5,cex.main = 1.5, cex = 1.5)
axis(1, at=1:5,labels=unique(data$Class), cex.axis = 1.5)
plot(as.numeric(data$Class), data$Leaf.Area,main = "Class vs Area",ylab = "Area",xlab = "Class",xaxt = "n", col = data$Class, cex.lab = 1.5, cex.axis = 1.5,cex.main = 1.5,cex = 1.5)
axis(1, at=1:5,labels=unique(data$Class), cex.axis = 1.5)


## 1.2.iii. Produce boxplots for all of the appropriate attributes in the dataset. Group each variable according to the class attribute.

plot.boxplot(data,2,4,1,3) # Centroid X, Centroid Y, Mass 
plot.boxplot(data,5,7,1,3) # Width, Depth, Orientation 0
plot.boxplot(data,8,10,1,3) # Orientation 1, Orientation 2, Orientation 3
plot.boxplot(data,11,13,1,3) # Orientation 4, Orientation 5, Orientation 6
plot.boxplot(data,14,16,1,3) # Orientation 7, Orientation 8, Orientation 9
plot.boxplot(data,17,19,1,3) #  Leaf Weight, Leaf Area, Leaf Hue.



############## 1.4 Dealing with missing values in R############## 


# 1.4.i. Replace missing values in the dataset using three strategies: replacement with 0, mean and median.

data.mvalues.0<-rep.missing.values(data,"0")  # Replace with zero
data.mvalues.mean<-rep.missing.values(data,"mean") # Repace with the mean
data.mvalues.median<-rep.missing.values(data,"median") # Replace with the median

# 1.4.ii. Define, compare and contrast these approaches and its effects on the data.
# Replace with 0 Example: Width and Depth
boxplot.replace.mvalues(data,6)  #Boxplot Depth variable
#boxplot.replace.mvalues(data,5)  #Boxplot Width variable


############## 1.5. Attribute transformation ############## 

# Transformation techniques: Mean centering, Normalisation and Standardisation) 
# Mean Centering 
data.mvalues.0.MC<-data.mvalues.0 %>% mutate_at(c(2:19), funs(c(scale(., scale = FALSE))))
data.mvalues.mean.MC<-data.mvalues.mean %>% mutate_at(c(2:19), funs(c(scale(., scale = FALSE))))
data.mvalues.median.MC<-data.mvalues.median %>% mutate_at(c(2:19), funs(c(scale(., scale = FALSE))))

# Normalising between [0,1]:
data.mvalues.0.NM<-normalising(data.mvalues.0,2,19)
data.mvalues.mean.NM<-normalising(data.mvalues.mean,2,19)
data.mvalues.median.NM<-normalising(data.mvalues.median,2,19)

# Standardising 
data.mvalues.0.ST<-data.mvalues.0 %>% mutate_at(c(2:19), funs(c(scale(.))))
data.mvalues.mean.ST<-data.mvalues.mean %>% mutate_at(c(2:19), funs(c(scale(.))))
data.mvalues.median.ST<-data.mvalues.median %>% mutate_at(c(2:19), funs(c(scale(.))))


# Define, compare and contrast these approaches and its effects on the data.
# Boxplot Example


par(mfrow=c(1,4))
boxplot(data[,7],ylab = "Orientation 0", main = "Orientation 0",col = "grey66", cex.lab=1.6, cex.axis=1.6,cex.sub=1.6, cex.main=1.6,outcex=1.8)
boxplot(data.mvalues.mean.NM[,7],ylab = "Orientation 0",main = "Or.0: Normalisation", ylab = "Orientation 0",col = "grey66", cex.lab=1.6, cex.axis=1.6,cex.sub=1.6, cex.main=1.6,outcex=1.8)
boxplot(data.mvalues.mean.MC[,7],ylab = "Orientation 0",main = "Or.0: Mean centering", col = "grey66", cex.lab=1.6, cex.axis=1.6,cex.sub=1.6, cex.main=1.6,outcex=1.8)
boxplot(data.mvalues.mean.ST[,7],ylab = "Orientation 0",main = "Or.0: Standardization", col = "grey66", cex.lab=1.6, cex.axis=1.6,cex.sub=1.6, cex.main=1.6,outcex=1.8)

############## 1.6. Attribute / instance selection ############## 


#1.6.i Starting again from the raw data, consider attribute and instance deletion strategies to deal with missing values

data.attribute.instance.deletion<-data[,-17] # Remove the variable Leaf Weight. It has 50% of missing values
missing.values.rows(data.attribute.instance.deletion)  #it is possible to remove all those instances with more than 3 missing values
data.attribute.instance.deletion<-cbind(data.attribute.instance.deletion, empty_values = rowSums(is.na(data.attribute.instance.deletion))) #Auxiliar column
data.attribute.instance.deletion<-data.attribute.instance.deletion[data.attribute.instance.deletion$empty_values <= 2,] # Delete all those instances with more than 3 missing values
data.attribute.instance.deletion<-data.attribute.instance.deletion[,-20] # Delete the auxiliar column

#1.6.ii Start from the raw data, use correlations between attributes to reduce the number of attributes.
#       Try to reduce the dataset to contain only uncorrelated attributes and no missing values. Explain
#       your choices and its effects on the dataset.

data.without.missing.values<-cbind(data, empty_values = rowSums(is.na(data))) # Create an auxiliar column
data.without.missing.values<- data.without.missing.values[data.without.missing.values$empty_values == 0,] #Delete all those instaces with 1 or more missing values
data.without.missing.values.uncorrelated<-data.without.missing.values[,c(1:5,7,17:20)] # Select the uncorrelated attributes

cor(data.without.missing.values.uncorrelated[,2:9]) # The correlation matrix with uncorrelated variables.

#1.6.iii Starting from an appropriate version of the dataset, use Principal Component Analysis to create a
#         data set with eight attributes. Explain the process and the result obtained.

# My approtiate version considers
data.pca<-data[,-17] # Remove the Leaf Weight variable
data.pca<-cbind(data.pca, empty_values = rowSums(is.na(data.pca)))  # Auxiliar column Number of attributes without information
data.pca<-data.pca[data.pca$empty_values <=2,] # Deleting all those which have 3 o more missing values
data.pca<-data.frame(data.pca %>% mutate_each(funs(replace(., which(is.na(.)),mean(., na.rm=TRUE))))) # Replacing by the mean
data.pca<-data.pca[,-20]  # Deleting the auxiliar column
data.pca<-data.pca %>% mutate_at(c(2:18), funs(c(scale(.))))  # standarizing data before PCA
pca <- prcomp(data.pca[,c(2:18)],scale=T)  # PCA without considering the Class attribute

summary.pca<-summary(pca)

summary.pca

input.all.pcs<-data.frame(pca$x)
input.all.pcs$Class<-data.pca$Class


############## Final Datasets ############## 


dim(data.mvalues.0.MC) # Missing values replaced by 0 and Mean Centered
dim(data.mvalues.mean.MC)# Missing values replaced by the mean and Mean Centered
dim(data.mvalues.median.MC)# Missing values replaced by the median and Mean Centered

dim(data.mvalues.0.NM) # Missing values replaced by 0 and Normalisation
dim(data.mvalues.mean.NM) # Missing values replaced by the mean and Normalisation
dim(data.mvalues.median.NM) # Missing values replaced by the median and Normalisation

dim(data.mvalues.0.ST) # Missing values replaced by 0 / Standardisation
dim(data.mvalues.mean.ST) # Missing values replaced by the mean / Standardisation
dim(data.mvalues.median.ST) # Missing values replaced by the median / Standardisation
 
dim(data.attribute.instance.deletion) # Without Leaf Weight variable/ Instances with a maximum of 2 missing values.
dim(data.without.missing.values.uncorrelated) # Uncorrelated variables and without missing values
