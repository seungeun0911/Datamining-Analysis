#################��?�?��� ��?��################
# ������ ������

install.packages("ggplot2")

# cross table (�?��? ���)
install.packages("gmodels")


# confusionMatrix
install.packages("caret") 


# ���?� ������
install.packages("e1071")

# cart �?����� ����
install.packages("rpart")

# �?� ���� ?�� �?��� �?���
install.packages("rattle")

# �?���� ?�� ���� ������?� �?�
install.packages("rpart.plot")


# bagging
install.packages("adabag") 


# ���� ��? ��?��
install.packages("arules")


{
  library(class)
  library(ggvis)
  library(gmodels) 
  library(caret) 
  library(e1071)
  library(rpart)
  library(rattle)
  library(rpart.plot)
  library(adabag)
  library(rpart) 
  library(arules)
}

### ������ ���� ###


#�������� �?����� 
dong_data <- read.csv("��_������_����(400).csv", header=TRUE)

#�������� ����?
normalize <- function(x){
  num <- x-min(x)
  denom <- max(x)-min(x)
  return(num/denom)
}

norm_dong <- dong_data
norm_dong[,c(-1,-2)] <- as.data.frame(lapply(norm_dong[,c(-1,-2)], normalize))
norm_dong$��<-NULL
norm_dong$label<-as.factor(norm_dong$label)


#����?2 : norm_dong_data (cart tree�� �?�)
norm_dong_data_all <- norm_dong
norm_dong_data_all[,c(-1,-2)]<-as.data.frame(lapply(norm_dong_data_all[,c(-1,-2)], normalize))
set.seed(1234) #��?: ���� ������ � ���?��� �?� �?� ���� ���� ������ ���� 
##1. ���� ����(7:3)
ind <- sample(2, nrow(norm_dong_data_all), replace=TRUE, prob=c(0.7, 0.3))
##2. �?�������, �?��� ������
subway.training <- norm_dong_data_all[ind==1,]
subway.test <- norm_dong_data_all[ind==2,]
subway.trainLabels <- norm_dong_data_all[ind==1,2] #label ���� ���� (label ������ 2�� �?��� ����)
subway.testLabels <- norm_dong_data_all[ind==2,2]



######################## k-means clustering ########################
######## �������� ?�����?�  
#1. k�� ���?� - elbow point
wss<-0
for(i in 1:15){
  wss[i] <-sum(kmeans(norm_dong[,-1], centers = i) $ withinss)
}
plot(1:15, wss, type = "b", xlab = "Clusters #", ylab = "Within group sum of squares")
#2. k-means ���� 
dong.kmeans<-kmeans(norm_dong[,-1], centers = 4)
#3. k���� ���� �??� (���?��?�) 
dong.label <- order(dong.kmeans$cluster)
names(dong_data)[2]<-c("label")
dong_data$label<-as.factor(dong_data$label)

######## �������� ?�����?�
#1. �������� �?����� + ����? 
gu_data <- read.csv("�����_��?����_������.csv", header=TRUE)
norm_gu <- gu_data
norm_gu[,c(-1,-2)] <- as.data.frame(lapply(norm_gu[,c(-1,-2)], normalize))

gu_data2 <- gu_data
gu_data2[,-1] <- as.data.frame(lapply(gu_data2[,-1], normalize))
gu_kmeans22 <- kmeans(gu_data[,-1], centers=5)
gu_kmeans22$cluster
#2. k�� ���?� - elbow point 
wss2 <- 0
for(i in 1:15){
  wss2[i] <- sum(kmeans(norm_gu[,-1], centers = i)$withinss)
}
plot(1:15, wss2, type="b", xlab="Clusters #", ylab="Within group sum of squares")
#3. k-means ���� 
gu.kmeans <- kmeans(norm_gu[,10], centers = 5)
#4. k�� ���� �??� (���?��?�)
gu.label <- order(gu.kmeans$cluster)
names(gu_data)[2] <- c("label")
gu_data$label <- as.factor(gu_data$label)


# ?�����?� 1�� ��� �?�����
clustered_1st_raw_data<-read.csv("?�����?�1��_���.csv",header=TRUE)
# dong������ �??�
normalize <- function(x){
  num <- x-min(x)
  denom <- max(x)-min(x)
  return(num/denom)
}

dong.training<-read.csv("��_������_����(325).csv",header=TRUE)
dong.training.norm<-dong.training
dong.training.norm[,c(-1,-2)]<-as.data.frame(lapply(dong.training[,c(-1,-2)], normalize))
dong.training.norm$��<-NULL
dong.testing<-read.csv("��_������_����(75).csv",header=TRUE)
dong.testing.norm<-dong.testing
dong.testing.norm[,c(-1,-2)]<-as.data.frame(lapply(dong.testing[,c(-1,-2)], normalize))
dong.testing.norm$��<-NULL
dong.training.norm$label<-as.factor(dong.training.norm$label)
dong.testing.norm$label<-as.factor(dong.testing.norm$label)

######################## �??� �?� ���� ########################
#train&test ������ �?����� 
dong_train <- read.csv("��_������_����(325).csv",header=TRUE)
dong_test <- read.csv("��_������_����(75).csv", header=TRUE)

#����?
norm_dong_train <- dong_train
norm_dong_train[,c(-1,-2)] <- as.data.frame(lapply(norm_dong_train[,c(-1,-2)], normalize))
norm_dong_train$��<-NULL
norm_dong_train$label<-as.factor(norm_dong_train$label)

norm_dong_test <- dong_test 
norm_dong_test[,c(-1,-2)] <- as.data.frame(lapply(norm_dong_test[,c(-1,-2)], normalize))
norm_dong_test$��<-NULL
norm_dong_test$label<-as.factor(norm_dong_test$label)

####ORIGINAL_DONG_DATA_SAMPLE####

# ?������ ��� (��������)
dong_clustering<-read.csv("����_��_?�����?�.csv",header=TRUE)

# ��������
original_dong_data<-read.csv("����_��_������_����.csv",header=TRUE)
str(original_dong_data)

#  �� �?�(���� 5)
original_dong_data<-cbind(original_dong_data,dong_clustering[,5]) 
names(original_dong_data)[19]<-c("label")

#  factor ���� 
original_dong_data$label<-as.factor(original_dong_data$label)

# ������ ����
ORIGINAL_DONG_DATA<-original_dong_data
str(ORIGINAL_DONG_DATA)

#####ORIGINAL_DONG_DATA_ALLn#####

# ����?  ORIGINAL_DONG_DATA
ORIGINAL_SAMPLE_DATA[,c(-1,-2,-19)]<-as.data.frame(lapply(ORIGINAL_SAMPLE_DATA[,c(-1,-2,-19)], normalize))

#cor(original_dong_data[,c(-1,-2,-21)])
NORM_SAMPLE_DATA<-ORIGINAL_SAMPLE_DATA

#####��_������_����(400)#####
rawData<-read.csv("��_������_����(325).csv",header=TRUE)
str(rawData)

# �� �?�
names(rawData)[2]<-c("label")

#  factor ���� 
rawData$label<-as.factor(rawData$label)

# ������ copy
ORIGINAL_DONG_DATA_ALL<-rawData
NORM_DONG_DATA_ALL<-ORIGINAL_DONG_DATA_ALL

# ����? : norm_dong_data
NORM_DONG_DATA_ALL[,c(-1,-2)]<-as.data.frame(lapply(NORM_DONG_DATA_ALL[,c(-1,-2)], normalize))

NORM_DONG_DATA_ALL$��<-NULL
ORIGINAL_DONG_DATA_ALL$��<-NULL

JUNGGYE_DONG_DATA_ALL<-read.csv("��_������_����(�?�).csv",header=TRUE)
JUNGGYE_DONG_DATA_ALL$��<-NULL

######################## 1.���?� �������� ########################
install.packages("e1071")
library(e1071)

#�� ���� 
nb_model <- naiveBayes(norm_dong_train, norm_dong_train$label, laplace = 1) 
nb_pred <- predict(nb_model, norm_dong_test, type="class") 
nb_pred2 <- naiveBayes.predict(nb_mode, norm_dong_test, type="class")

#���(1) - ���?� 
nb_result<-cbind(norm_dong_test, norm_dong_test$label, nb_pred)
acc<-prop.table(table(nb_pred, norm_dong_test$label))
acc

#���(2) - Cross Table
install.packages("gmodels")
library(gmodels)
CrossTable(x=norm_dong_test$label, y=nb_pred, prop.chisq=FALSE)

#���(3) - �?��? ���?�? �� 
nb_pred
label_n <- data.frame(nb_pred)
label_n[,1] <- dong_test$�� 
label_n[,2] <- data.frame(nb_pred)
#csv���?� ���� 
write.csv(label_n, "D:/01 ����/2021-1/�����?��?�/��?/03 r�?�/label_naiveBayes(75).csv")



######################## 2.adaboost ########################
install.packages("adabag")
library(adabag)

subway.adaboost <- boosting(label~.,data=norm_dong_train[], mfinal=10, control=rpart.control(maxdepth=10))
subway.predboosting <- predict.boosting(subway.adaboost, newdata=norm_dong_test[]) 

#���(1) - ���?� 
1-subway.predboosting$error

#���(2) - �?��? ���?�? �� 
label_boo <- data.frame(subway.predboosting$class)
label_boo[,1] <- dong_test$�� 
label_boo[,2] <- data.frame(subway.predboosting$class)
#csv���?� ���� 
write.csv(label_boo, "D:/01 ����/2021-1/�����?��?�/��?/03 r�?�/label_boosting(75).csv")

#for�� �?� 
result_boo<-0
for(i in 1:30)
{
  subway.adaboost <- boosting(label~.,data=norm_dong_train[], mfinal=10, control=rpart.control(maxdepth=i))
  subway.predboosting <- predict.boosting(subway.adaboost, newdata=norm_dong_test[])
  result_boo[i]<-1-subway.predboosting$error
}
result_boo
max(result_boo)



######################## 3.bagging ########################
library(adabag)

subway.bagging <- bagging(label~., data=norm_dong_train[], mfinal=10, control=rpart.control(maxdepth=13))
subway.predbagging <- predict.bagging(subway.bagging, newdata=norm_dong_test[])

#���(1) - ���?� 
1-subway.predbagging$error

#���(2) - �?��? ���?�? �� 
label_bag <- data.frame(subway.predbagging$class)
label_bag[,1] <- dong_test$�� 
label_bag[,2] <- data.frame(subway.predbagging$class)
#csv���?� ���� 
write.csv(label_bag, "D:/01 ����/2021-1/�����?��?�/��?/03 r�?�/label_bagging(75).csv")

#for�� �?�
result_bag<-0
for (i in 1:30)
{
  subway.bagging <- bagging(label~., data=norm_dong_train[], mfinal=10, control=rpart.control(maxdepth=i))
  subway.predbagging <- predict.bagging(subway.bagging, newdata=norm_dong_test[])
  result_bag[i]<-1-subway.predbagging$error
}
result_bag
max(result_bag)



######################## 4.cart tree ########################
#cart �?����� ����
install.packages("rpart") #���� �?� No ?�� 
library(rpart)
#�?���� ?�� �?��� �?���
install.packages("rattle")
library(rattle)

#cart �� ����
cart_model <- rpart(norm_dong_train$label~.,data = norm_dong_train[-1],
                    method = "class", control = rpart.control(cp=0.001,minsplit = 2))

#��� (��?��� ������ �?���)
plot(cart_model)
text(cart_model)

#cp�� ���
plotcp(cart_model)    #cp�� ���� ���� ���� 0.065�� ����
printcp(cart_model)

#����?�� ����
ptree <- prune(cart_model, cp=0.01)
plot(ptree)
text(ptree, cex=0.55) #cex: ����?�� 
print(ptree)

ptree2 <- prune(cart_model, cp=0.1712)
plot(ptree2)
text(ptree2)

ptree3 <- prune(cart_model, cp=0.0277)
plot(ptree3)
text(ptree3)


#�����?� ���� (test data�� �?��?� ��?�� ?��)
#21~31�?� ���� �?� 
pdata<-predict(ptree,subway.test,type = "class")
pdata2<-predict(ptree2,subway.test,type = "class")
pdata3<-predict(ptree3,subway.test,type = "class")
pdata2

#��?�� ���� - �� 0 ����.. 
acc <- sum(subway.testLabels==pdata)/length(pdata)
acc2 <- sum(subway.testLabels==pdata2)/length(pdata2)
acc3 <- sum(subway.testLabels==pdata3)/length(pdata3)
acc
table(pdata, subway.testLabels)

#�?���� -> �?����� ����?
#�?���� ?�� ���� ������?� �?�
install.packages("rpart.plot")
library(rpart.plot)

prp(cart_model, type=4, extra=2, digits = 3)
fancyRpartPlot(cart_model)



######################## ?�����?� �?� (�?� �� �???�) ########################
#������ ������ ���??�� 
install.packages("ggvis")
library(ggvis)

install.packages("ggplot2")
library(ggplot2)

#����? �?� (3-4�� ����)
dong_data <- read.csv("��_������_����(400).csv", header=TRUE)
norm_dong <- dong_data
norm_dong[,c(-1,-2)] <- as.data.frame(lapply(norm_dong[,c(-1,-2)], normalize))

ggplot(data=norm_dong, aes(x=��, y=��?��?�?�, color=label))+geom_point() + ggtitle("���� ��?��?�?�") 
ggplot(data=norm_dong, aes(x=��, y=����, color=label)) + geom_point() + ggtitle("���� ����")
ggplot(norm_dong, aes(x=��, y=������, color=label)) + geom_point() + ggtitle("���� ������")
ggplot(norm_dong, aes(x=��, y=�?�, color=label)) + geom_point()  + ggtitle("���� �?�")

ggplot(norm_dong, aes(x=��, y=�?��?�, color=label)) + geom_point()  + ggtitle("���� �?��?�")
ggplot(norm_dong, aes(x=��, y=����, color=label)) + geom_point()  + ggtitle("���� ����")
ggplot(norm_dong, aes(x=��, y=�����?�.08., color=label)) + geom_point()  + ggtitle("���� �����?�(8��)")
ggplot(norm_dong, aes(x=��, y=�����?�.19., color=label)) + geom_point()  + ggtitle("���� �����?�(19��)")
ggplot(norm_dong, aes(x=��, y=����, color=label)) + geom_point()  + ggtitle("���� ����")
ggplot(norm_dong, aes(x=��, y=������, color=label)) + geom_point()  + ggtitle("���� ������")


# ������ ������ 
clustered_1st_raw_data<-read.csv("?�����?�1��_���.csv",header=TRUE)

p<-ggvis(data = clustered_1st_raw_data,
         x=~��,y=~?������,fill=~?������)
layer_points(p)




