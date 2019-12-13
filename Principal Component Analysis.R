dataset <- mtcars

# features that need to be removed?
dataset <- cbind(dataset[,1:7], dataset[, 10:11])

pca <- prcomp(dataset, scale. = T)
pca
biplot(pca, scale = 0)

pca$rotation[,2]

pr.out = pca
pr.var<-pr.out$sdev^2
pr.var
pve<-pr.var/sum(pr.var)
pve

par(mfrow=c(1,2))
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained",
     main="Scree Plot", ylim=c(0,1),type= 'b')
plot(cumsum(pve), xlab="Principal Component",
     ylab="Cumulative Proportion of Variance Explained",
     main="Cumulative Proportion", ylim=c(0,1),type="b")
