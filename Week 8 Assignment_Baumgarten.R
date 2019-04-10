
# Assignment 8

my.path <- '/Users/corybaumgarten/Documents/Northwestern/2017SU_PREDICT_410-DL_SEC55/Data/';
my.file <- paste(my.path,'European_Employment.csv',sep=''); 
my.data <- read.csv(my.file,header=TRUE);
str(my.data)
head(my.data)
summary(my.data)


# Part 2: Initial Exploratory Data Analysis
# Pairwise scatterplot
pairs(my.data[,-c(2)])


# Part 3: Visualizing the Data with Labelled Scatterplots

eu.df <- subset(my.data,Group=='EU');
efta.df <- subset(my.data,Group=='EFTA');
eastern.df <- subset(my.data,Group=='Eastern');
other.df <- subset(my.data,Group=='Other');

# Plot of FIN versus SER;
plot(my.data$SER,my.data$FIN,xlab='Services',ylab='Finance',xlim=c(0,27),ylim=c(0,17))
text(eu.df$SER,eu.df$FIN,labels=eu.df$Country,cex=0.75,pos=4,col='green')
text(efta.df$SER,efta.df$FIN,labels=efta.df$Country,cex=0.75,pos=4,col='blue')
text(eastern.df$SER,eastern.df$FIN,labels=eastern.df$Country,cex=0.75,pos=4,col='red')
text(other.df$SER,other.df$FIN,labels=other.df$Country,cex=0.75,pos=4,col='grey')

# Now let’s make the same plot for MAN versus SER. Plot MAN versus SER;
plot(my.data$MAN,my.data$FIN,xlab='Manufacturing',ylab='Finance',xlim=c(0,32),ylim=c(0,17))
text(eu.df$MAN,eu.df$FIN,labels=eu.df$Country,cex=0.75,pos=4,col='green')
text(efta.df$MAN,efta.df$FIN,labels=efta.df$Country,cex=0.75,pos=4,col='blue')
text(eastern.df$MAN,eastern.df$FIN,labels=eastern.df$Country,cex=0.75,pos=4,col='red')
text(other.df$MAN,other.df$FIN,labels=other.df$Country,cex=0.75,pos=4,col='grey')


# Part 4: Creating a 2D Projection Using Principal Components Analysis
apply(my.data[,-c(1,2)],MARGIN=1,FUN=sum)
pca.out <- princomp(x=my.data[,-c(1,2)],cor=FALSE);
names(pca.out)

pc.1 <- pca.out$scores[,1];
pc.2 <- pca.out$scores[,2];

my.pca <- as.data.frame(list(Country=my.data$Country,Group=my.data$Group,pc1=pc.1,pc2=pc.2));

# Do we know why I used list() instead of cbind()?;

eu.pca <- subset(my.pca,Group=='EU');
efta.pca <- subset(my.pca,Group=='EFTA');
eastern.pca <- subset(my.pca,Group=='Eastern');
other.pca <- subset(my.pca,Group=='Other');

plot(eu.pca$pc1,eu.pca$pc2,xlab='Principal Component 1',ylab='Principal Component 2', xlim=c(-60,25),ylim=c(-25,30))
points(efta.pca$pc1,efta.pca$pc2)
points(eastern.pca$pc1,eastern.pca$pc2)
points(other.pca$pc1,other.pca$pc2)
text(eu.pca$pc1,eu.pca$pc2,labels=eu.pca$Country,cex=0.75,col='green',pos=4)
text(efta.pca$pc1,efta.pca$pc2,labels=efta.pca$Country,cex=0.75,col='blue',pos=1)
text(eastern.pca$pc1,eastern.pca$pc2,labels=eastern.pca$Country,cex=0.75,col='red',pos=1)
text(other.pca$pc1,other.pca$pc2,labels=other.pca$Country,cex=0.75,col='grey',pos=3)


# Part 5: Hierarchical Clustering Analysis

# Drop the 'Other' Category;
label.data <- subset(my.data,Group != 'Other');

# Cluster in FIN*SER view;
fin.ser <- hclust(d=dist(label.data[,c('FIN','SER')]),method='complete');
plot(fin.ser,labels=label.data[,1],xlab='Hierarchical Clustering FIN vs SER 2D View',sub='');



fin.ser.3 <- cutree(fin.ser,k=3);
finser.3df <- as.data.frame(list(Country=label.data[,1],Group=label.data[,2],Cluster=fin.ser.3))
finser.t3 <- table(finser.3df$Group,finser.3df$Cluster)
finser.comp3 <- t(finser.t3[1:3,])*(1/apply(finser.t3[1:3,],FUN=sum,MARGIN=2))
finser.accuracy3 <- sum(apply(finser.t3[1:3,],FUN=max,MARGIN=2))/sum(apply(finser.t3[1:3,],FUN=sum,MARGIN=2));

fin.ser.6 <- cutree(fin.ser,k=6);
finser.6df <- as.data.frame(list(Country=label.data[,1],Group=label.data[,2],Cluster=fin.ser.6))
finser.t6 <- table(finser.6df$Group,finser.6df$Cluster)
finser.comp6 <- t(finser.t6[1:3,])*(1/apply(finser.t6[1:3,],FUN=sum,MARGIN=2))
finser.accuracy6 <- sum(apply(finser.t6[1:3,],FUN=max,MARGIN=2))/sum(apply(finser.t6[1:3,],FUN=sum,MARGIN=2));


# Cluster in PC1*PC2 view;
pca.out <- princomp(x=label.data[,-c(1,2)],cor=FALSE);
my.pca <- as.data.frame(list(Country=label.data$Country,Group=label.data$Group,pc1=pca.out$scores[,1],pc2=pca.out$scores[,2]));

eu.pca <- subset(my.pca,Group=='EU');
efta.pca <- subset(my.pca,Group=='EFTA');
eastern.pca <- subset(my.pca,Group=='Eastern');
other.pca <- subset(my.pca,Group=='Other');

pc1.pc2 <- hclust(d=dist(my.pca[,c('pc1','pc2')]),method='complete');
plot(pc1.pc2,labels=my.pca[,1],xlab='Hierarchical Clustering PC1 vs PC2 2D View',sub='');

pca.3 <- cutree(pc1.pc2,k=3);
pca.3df <- as.data.frame(list(Country=my.pca[,1],Group=my.pca[,2],Cluster=pca.3))
pca.t3 <- table(pca.3df$Group,pca.3df$Cluster)
pca.comp3 <- t(pca.t3[1:3,])*(1/apply(pca.t3[1:3,],FUN=sum,MARGIN=2))
pca.accuracy3 <- sum(apply(pca.t3[1:3,],FUN=max,MARGIN=2))/sum(apply(pca.t3[1:3,],FUN=sum,MARGIN=2));

pca.6 <- cutree(pc1.pc2,k=6);
pca.6df <- as.data.frame(list(Country=my.pca[,1],Group=my.pca[,2],Cluster=pca.6))
pca.t6 <- table(pca.6df$Group,pca.6df$Cluster)
pca.comp6 <- t(pca.t6[1:3,])*(1/apply(pca.t6[1:3,],FUN=sum,MARGIN=2))
pca.accuracy6 <- sum(apply(pca.t6[1:3,],FUN=max,MARGIN=2))/sum(apply(pca.t6[1:3,],FUN=sum,MARGIN=2));


# Part 6: k-Means Clustering Analysis

# Cluster in FIN*SER view;
# Specify 3 Clusters;
finser.k3 <- kmeans(x=label.data[,c('FIN','SER')],centers=3);
names(finser.k3)

finser.k3df <- as.data.frame(list(Country=label.data[,1],Group=label.data[,2],Cluster=finser.k3$cluster, FIN=label.data$FIN,SER=label.data$SER));
finser.k3tab <- table(finser.k3df$Group,finser.k3df$Cluster);
finser.k3ac <- sum(apply(finser.k3tab[1:3,],FUN=max,MARGIN=2))/sum(apply(finser.k3tab[1:3,],FUN=sum,MARGIN=2));

# Plot the cluster centers;
plot(label.data$SER,label.data$FIN,xlab='Services',ylab='Finance',xlim=c(0,27),ylim=c(0,17),col='white')
text(eu.df$SER,eu.df$FIN,labels=eu.df$Country,cex=0.75,pos=4,col='green')
text(efta.df$SER,efta.df$FIN,labels=efta.df$Country,cex=0.75,pos=4,col='blue')
text(eastern.df$SER,eastern.df$FIN,labels=eastern.df$Country,cex=0.75,pos=4,col='red')
text(other.df$SER,other.df$FIN,labels=other.df$Country,cex=0.75,pos=4,col='grey')
text(finser.k3$centers[,2],finser.k3$centers[,1],labels=seq(1,3,1),col='black',cex=1)
points(finser.k3$centers[,2],finser.k3$centers[,1],col='black',cex=2.5)
text(finser.k3df$SER,finser.k3df$FIN,labels=finser.k3df$Cluster,col='grey',cex=0.75);
title('k-Means with 3 Clusters')


# Specify 6 Clusters;
finser.k6 <- kmeans(x=label.data[,c('FIN','SER')],centers=6);
names(finser.k6)

finser.k6df <- as.data.frame(list(Country=label.data[,1],Group=label.data[,2],Cluster=finser.k6$cluster, FIN=label.data$FIN,SER=label.data$SER));
finser.k6tab <- table(finser.k6df$Group,finser.k6df$Cluster);
finser.k6ac <- sum(apply(finser.k6tab[1:3,],FUN=max,MARGIN=2))/sum(apply(finser.k6tab[1:3,],FUN=sum,MARGIN=2));

# Plot the cluster centers;
plot(label.data$SER,label.data$FIN,xlab='Services',ylab='Finance',xlim=c(0,27),ylim=c(0,17),col='white')
text(eu.df$SER,eu.df$FIN,labels=eu.df$Country,cex=0.75,pos=4,col='green')
text(efta.df$SER,efta.df$FIN,labels=efta.df$Country,cex=0.75,pos=4,col='blue')
text(eastern.df$SER,eastern.df$FIN,labels=eastern.df$Country,cex=0.75,pos=4,col='red')
text(other.df$SER,other.df$FIN,labels=other.df$Country,cex=0.75,pos=4,col='grey')
text(finser.k6$centers[,2],finser.k6$centers[,1],labels=seq(1,6,1),col='black',cex=1)
points(finser.k6$centers[,2],finser.k6$centers[,1],col='black',cex=2.5)
text(finser.k6df$SER,finser.k6df$FIN,labels=finser.k6df$Cluster,col='grey',cex=0.75);
title('k-Means with 6 Clusters')



# Cluster in PC1*PC2 view;
# Specify 3 Clusters;
pca.k3 <- kmeans(x=my.pca[,-c(1,2)],centers=3);
names(pca.k3)

pca.k3df <- as.data.frame(list(Country=my.pca[,1],Group=my.pca[,2],Cluster=pca.k3$cluster,pc1=my.pca$pc1,pc2=my.pca$ pc2));
pca.k3tab <- table(pca.k3df$Group,pca.k3df$Cluster);
pca.k3ac <- sum(apply(pca.k3tab[1:3,],FUN=max,MARGIN=2))/sum(apply(pca.k3tab[1:3,],FUN=sum,MARGIN=2));

# Plot the cluster centers;
plot(my.pca$pc1,my.pca$pc2,xlab='Principal Component 1',ylab='Principal Component 2', xlim=c(-60,20),ylim=c(-22,25),col='white')
text(eu.pca$pc1,eu.pca$pc2,labels=eu.pca$Country,cex=0.75,pos=4,col='green')
text(efta.pca$pc1,efta.pca$pc2,labels=efta.pca$Country,cex=0.75,pos=4,col='blue')
text(eastern.pca$pc1,eastern.pca$pc2,labels=eastern.pca$Country,cex=0.75,pos=4,col='red')
text(other.pca$pc1,other.pca$pc2,labels=other.df$Country,cex=0.75,pos=4,col='grey')
text(pca.k3$centers[,1],pca.k3$centers[,2],labels=seq(1,3,1),col='black',cex=1)
points(pca.k3$centers[,1],pca.k3$centers[,2],col='black',cex=2.5)
text(pca.k3df$pc1,pca.k3df$pc2,labels=pca.k3df$Cluster,col='grey',cex=0.75);
title('k-Means with 3 Clusters')

# Specify 6 Clusters;
pca.k6 <- kmeans(x=my.pca[,-c(1,2)],centers=6); names(pca.k6)
pca.k6df <- as.data.frame(list(Country=my.pca[,1],Group=my.pca[,2],Cluster=pca.k6$cluster,pc1=my.pca$pc1,pc2=my.pca$ pc2));
pca.k6tab <- table(pca.k6df$Group,pca.k6df$Cluster);
pca.k6ac <- sum(apply(pca.k6tab[1:3,],FUN=max,MARGIN=2))/sum(apply(pca.k6tab[1:3,],FUN=sum,MARGIN=2));

# Plot the cluster centers;
plot(my.pca$pc1,my.pca$pc2,xlab='Principal Component 1',ylab='Principal Component 2',xlim=c(-60,25),ylim=c(-25,30),col='white')
text(eu.pca$pc1,eu.pca$pc2,labels=eu.pca$Country,cex=0.75,pos=4,col='green')
text(efta.pca$pc1,efta.pca$pc2,labels=efta.pca$Country,cex=0.75,pos=4,col='blue')
text(eastern.pca$pc1,eastern.pca$pc2,labels=eastern.pca$Country,cex=0.75,pos=4,col='red')
text(other.pca$pc1,other.pca$pc2,labels=other.df$Country,cex=0.75,pos=4,col='grey')
text(pca.k6$centers[,1],pca.k6$centers[,2],labels=seq(1,6,1),col='black',cex=1)
points(pca.k6$centers[,1],pca.k6$centers[,2],col='black',cex=2.5)
text(pca.k6df$pc1,pca.k6df$pc2,labels=pca.k6df$Cluster,col='grey',cex=0.75);
title('k-Means with 6 Clusters')



# Loop through 1-20 clusters using all dimensions;
# Compute the accuracy for each cluster, store, and plot; 
# Set the maximum number of clusters to consider;

k.max <- 20;

# Initialize the accuracy arrays for storage;
accuracy.hier <- rep(NA,k.max);
accuracy.kmeans <- rep(NA,k.max);

# Fit the hierarchical clustering model outside of the loop for efficiency;
all.h <- hclust(d=dist(label.data[,-c(1,2)]),method='complete');

# Loop through different cluster sizes and compute classification accuracy;
for (j in 1:k.max){
  
# Fit hierarchical cluster model of size j;
hier.j <- cutree(all.h,k=j);
hier.df <- as.data.frame(list(Country=label.data[,1],Group=label.data[,2],Cluster=hier.j));
hier.table <- table(hier.df$Group,hier.df$Cluster);

# Cannot use apply() on a vector;
if (j==1){
  accuracy.hier[j] <- max(hier.table[1:3,])/sum(hier.table[1:3,]);
  }else{
    accuracy.hier[j] <- sum(apply(hier.table[1:3,],FUN=max,MARGIN=2))/sum(apply(hier.table[1:3,],FUN=sum,MARGIN=2)); 
    }#end if-else;

# Fit k-means clustering model of size j;
kmeans.j <- kmeans(x=label.data[,-c(1,2)],centers=j);
kmeans.df <- as.data.frame(list(Country=label.data[,1],Group=label.data[,2],Cluster=kmeans.j$cluster));
kmeans.table <- table(kmeans.df$Group,kmeans.df$Cluster);

# Cannot use apply() on a vector;
if (j==1){
  accuracy.kmeans[j] <- max(kmeans.table[1:3,])/sum(kmeans.table[1:3,]);
  }else{
    accuracy.kmeans[j] <- sum(apply(kmeans.table[1:3,],FUN=max,MARGIN=2))/sum(apply(kmeans.table[1:3,],FUN=sum,MARGIN=2));
    }#end if-else;
} #end j loop;

plot(seq(1,k.max,1),accuracy.hier,ylim=c(0,1),xlab='# Clusters',ylab='Accuracy',cex.axis=1,type='l',lwd=2,col='red')
points(seq(1,k.max,1),accuracy.hier,ylim=c(0,1),cex=1.5,type='p',col='red',pch=19)
points(seq(1,k.max,1),accuracy.kmeans,ylim=c(0,1),type='l',lwd=2,col='blue')
points(seq(1,k.max,1),accuracy.kmeans,ylim=c(0,1),cex=1.5,type='p',col='blue')
title('Classification Accuracy')
legend(1,0.2,legend=c('Hierarchical','k-Means'),col=c('red','blue'),lwd=2)



