#################설치해야할 패키지################
# 데이터 산점도

install.packages("ggplot2")

# cross table (분류기에 사용)
install.packages("gmodels")


# confusionMatrix
install.packages("caret") 


# 나이브 베이즈
install.packages("e1071")

# cart 알고리즘 지원
install.packages("rpart")

# 의사 결정 트리 그래프 그리기
install.packages("rattle")

# 의사결정 트리 모델을 만들어주는 함수
install.packages("rpart.plot")


# bagging
install.packages("adabag") 


# 연관 규칙 패키지
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

### 데이터 정리 ###


#원데이터 불러오기 
dong_data <- read.csv("동_데이터_종합(400).csv", header=TRUE)

#원데이터 정규화
normalize <- function(x){
  num <- x-min(x)
  denom <- max(x)-min(x)
  return(num/denom)
}

norm_dong <- dong_data
norm_dong[,c(-1,-2)] <- as.data.frame(lapply(norm_dong[,c(-1,-2)], normalize))
norm_dong$동<-NULL
norm_dong$label<-as.factor(norm_dong$label)


#정규화2 : norm_dong_data (cart tree에 필요)
norm_dong_data_all <- norm_dong
norm_dong_data_all[,c(-1,-2)]<-as.data.frame(lapply(norm_dong_data_all[,c(-1,-2)], normalize))
set.seed(1234) #시드값: 난수 생성시 처음 시작값을 주어 매번 같은 값이 나오게 만듦 
##1. 랜덤 샘플(7:3)
ind <- sample(2, nrow(norm_dong_data_all), replace=TRUE, prob=c(0.7, 0.3))
##2. 학습데이터, 테스팅 데이터
subway.training <- norm_dong_data_all[ind==1,]
subway.test <- norm_dong_data_all[ind==2,]
subway.trainLabels <- norm_dong_data_all[ind==1,2] #label 정보 지정 (label 정보는 2번 속성에 있음)
subway.testLabels <- norm_dong_data_all[ind==2,2]



######################## k-means clustering ########################
######## 동데이터 클러스터링  
#1. k값 구하기 - elbow point
wss<-0
for(i in 1:15){
  wss[i] <-sum(kmeans(norm_dong[,-1], centers = i) $ withinss)
}
plot(1:15, wss, type = "b", xlab = "Clusters #", ylab = "Within group sum of squares")
#2. k-means 실행 
dong.kmeans<-kmeans(norm_dong[,-1], centers = 4)
#3. k값을 토대로 라벨링 (라벨추가작업) 
dong.label <- order(dong.kmeans$cluster)
names(dong_data)[2]<-c("label")
dong_data$label<-as.factor(dong_data$label)

######## 구데이터 클러스터링
#1. 구데이터 불러오기 + 정규화 
gu_data <- read.csv("서울시_자치구별_데이터.csv", header=TRUE)
norm_gu <- gu_data
norm_gu[,c(-1,-2)] <- as.data.frame(lapply(norm_gu[,c(-1,-2)], normalize))

gu_data2 <- gu_data
gu_data2[,-1] <- as.data.frame(lapply(gu_data2[,-1], normalize))
gu_kmeans22 <- kmeans(gu_data[,-1], centers=5)
gu_kmeans22$cluster
#2. k값 구하기 - elbow point 
wss2 <- 0
for(i in 1:15){
  wss2[i] <- sum(kmeans(norm_gu[,-1], centers = i)$withinss)
}
plot(1:15, wss2, type="b", xlab="Clusters #", ylab="Within group sum of squares")
#3. k-means 실행 
gu.kmeans <- kmeans(norm_gu[,10], centers = 5)
#4. k값 토대로 라벨링 (라벨추가작업)
gu.label <- order(gu.kmeans$cluster)
names(gu_data)[2] <- c("label")
gu_data$label <- as.factor(gu_data$label)


# 클러스터링 1차 결과 불러오기
clustered_1st_raw_data<-read.csv("클러스터링1차_결과.csv",header=TRUE)
# dong데이터 라벨링
normalize <- function(x){
  num <- x-min(x)
  denom <- max(x)-min(x)
  return(num/denom)
}

dong.training<-read.csv("동_데이터_종합(325).csv",header=TRUE)
dong.training.norm<-dong.training
dong.training.norm[,c(-1,-2)]<-as.data.frame(lapply(dong.training[,c(-1,-2)], normalize))
dong.training.norm$동<-NULL
dong.testing<-read.csv("동_데이터_종합(75).csv",header=TRUE)
dong.testing.norm<-dong.testing
dong.testing.norm[,c(-1,-2)]<-as.data.frame(lapply(dong.testing[,c(-1,-2)], normalize))
dong.testing.norm$동<-NULL
dong.training.norm$label<-as.factor(dong.training.norm$label)
dong.testing.norm$label<-as.factor(dong.testing.norm$label)

######################## 라벨링 작업 이후 ########################
#train&test 데이터 불러오기 
dong_train <- read.csv("동_데이터_종합(325).csv",header=TRUE)
dong_test <- read.csv("동_데이터_종합(75).csv", header=TRUE)

#정규화
norm_dong_train <- dong_train
norm_dong_train[,c(-1,-2)] <- as.data.frame(lapply(norm_dong_train[,c(-1,-2)], normalize))
norm_dong_train$동<-NULL
norm_dong_train$label<-as.factor(norm_dong_train$label)

norm_dong_test <- dong_test 
norm_dong_test[,c(-1,-2)] <- as.data.frame(lapply(norm_dong_test[,c(-1,-2)], normalize))
norm_dong_test$동<-NULL
norm_dong_test$label<-as.factor(norm_dong_test$label)

####ORIGINAL_DONG_DATA_SAMPLE####

# 클러스터 결과 (동데이터)
dong_clustering<-read.csv("샘플_동_클러스터링.csv",header=TRUE)

# 동데이터
original_dong_data<-read.csv("샘플_동_데이터_종합.csv",header=TRUE)
str(original_dong_data)

#  라벨 추가(라벨은 5)
original_dong_data<-cbind(original_dong_data,dong_clustering[,5]) 
names(original_dong_data)[19]<-c("label")

#  factor 지정 
original_dong_data$label<-as.factor(original_dong_data$label)

# 데이터 복사
ORIGINAL_DONG_DATA<-original_dong_data
str(ORIGINAL_DONG_DATA)

#####ORIGINAL_DONG_DATA_ALLn#####

# 정규화  ORIGINAL_DONG_DATA
ORIGINAL_SAMPLE_DATA[,c(-1,-2,-19)]<-as.data.frame(lapply(ORIGINAL_SAMPLE_DATA[,c(-1,-2,-19)], normalize))

#cor(original_dong_data[,c(-1,-2,-21)])
NORM_SAMPLE_DATA<-ORIGINAL_SAMPLE_DATA

#####동_데이터_종합(400)#####
rawData<-read.csv("동_데이터_종합(325).csv",header=TRUE)
str(rawData)

# 라벨 추가
names(rawData)[2]<-c("label")

#  factor 지정 
rawData$label<-as.factor(rawData$label)

# 데이터 copy
ORIGINAL_DONG_DATA_ALL<-rawData
NORM_DONG_DATA_ALL<-ORIGINAL_DONG_DATA_ALL

# 정규화 : norm_dong_data
NORM_DONG_DATA_ALL[,c(-1,-2)]<-as.data.frame(lapply(NORM_DONG_DATA_ALL[,c(-1,-2)], normalize))

NORM_DONG_DATA_ALL$동<-NULL
ORIGINAL_DONG_DATA_ALL$동<-NULL

JUNGGYE_DONG_DATA_ALL<-read.csv("동_데이터_종합(판교).csv",header=TRUE)
JUNGGYE_DONG_DATA_ALL$동<-NULL

######################## 1.나이브 베이지안 ########################
install.packages("e1071")
library(e1071)

#모델 생성 
nb_model <- naiveBayes(norm_dong_train, norm_dong_train$label, laplace = 1) 
nb_pred <- predict(nb_model, norm_dong_test, type="class") 
nb_pred2 <- naiveBayes.predict(nb_mode, norm_dong_test, type="class")

#결과(1) - 적합도 
nb_result<-cbind(norm_dong_test, norm_dong_test$label, nb_pred)
acc<-prop.table(table(nb_pred, norm_dong_test$label))
acc

#결과(2) - Cross Table
install.packages("gmodels")
library(gmodels)
CrossTable(x=norm_dong_test$label, y=nb_pred, prop.chisq=FALSE)

#결과(3) - 분류기에 적합시킨 라벨 
nb_pred
label_n <- data.frame(nb_pred)
label_n[,1] <- dong_test$동 
label_n[,2] <- data.frame(nb_pred)
#csv파일로 저장 
write.csv(label_n, "D:/01 윤주/2021-1/데이터마이닝/발표/03 r코드/label_naiveBayes(75).csv")



######################## 2.adaboost ########################
install.packages("adabag")
library(adabag)

subway.adaboost <- boosting(label~.,data=norm_dong_train[], mfinal=10, control=rpart.control(maxdepth=10))
subway.predboosting <- predict.boosting(subway.adaboost, newdata=norm_dong_test[]) 

#결과(1) - 적합도 
1-subway.predboosting$error

#결과(2) - 분류기에 적합시킨 라벨 
label_boo <- data.frame(subway.predboosting$class)
label_boo[,1] <- dong_test$동 
label_boo[,2] <- data.frame(subway.predboosting$class)
#csv파일로 저장 
write.csv(label_boo, "D:/01 윤주/2021-1/데이터마이닝/발표/03 r코드/label_boosting(75).csv")

#for문 이용 
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

#결과(1) - 적합도 
1-subway.predbagging$error

#결과(2) - 분류기에 적합시킨 라벨 
label_bag <- data.frame(subway.predbagging$class)
label_bag[,1] <- dong_test$동 
label_bag[,2] <- data.frame(subway.predbagging$class)
#csv파일로 저장 
write.csv(label_bag, "D:/01 윤주/2021-1/데이터마이닝/발표/03 r코드/label_bagging(75).csv")

#for문 이용
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
#cart 알고리즘 지원
install.packages("rpart") #에러 뜨면 No 클릭 
library(rpart)
#의사결정 트리 그래프 그리기
install.packages("rattle")
library(rattle)

#cart 모델 생성
cart_model <- rpart(norm_dong_train$label~.,data = norm_dong_train[-1],
                    method = "class", control = rpart.control(cp=0.001,minsplit = 2))

#출력 (징그럽게 나오는 그래프)
plot(cart_model)
text(cart_model)

#cp값 출력
plotcp(cart_model)    #cp가 가장 낮은 값이 0.065로 나옴
printcp(cart_model)

#가지치기 진행
ptree <- prune(cart_model, cp=0.01)
plot(ptree)
text(ptree, cex=0.55) #cex: 글자크기 
print(ptree)

ptree2 <- prune(cart_model, cp=0.1712)
plot(ptree2)
text(ptree2)

ptree3 <- prune(cart_model, cp=0.0277)
plot(ptree3)
text(ptree3)


#예측하기 진행 (test data를 이용하여 정확도 확인)
#21~31코드 실행 필요 
pdata<-predict(ptree,subway.test,type = "class")
pdata2<-predict(ptree2,subway.test,type = "class")
pdata3<-predict(ptree3,subway.test,type = "class")
pdata2

#정확도 측정 - 다 0 나옴.. 
acc <- sum(subway.testLabels==pdata)/length(pdata)
acc2 <- sum(subway.testLabels==pdata2)/length(pdata2)
acc3 <- sum(subway.testLabels==pdata3)/length(pdata3)
acc
table(pdata, subway.testLabels)

#분류기법 -> 그림으로 도식화
#의사결정 트리 모델을 만들어주는 함수
install.packages("rpart.plot")
library(rpart.plot)

prp(cart_model, type=4, extra=2, digits = 3)
fancyRpartPlot(cart_model)



######################## 클러스터링 분석 (속성 간 비교분석) ########################
#데이터 산점도 라이브러리 
install.packages("ggvis")
library(ggvis)

install.packages("ggplot2")
library(ggplot2)

#정규화 다시 (3-4줄 제외)
dong_data <- read.csv("동_데이터_종합(400).csv", header=TRUE)
norm_dong <- dong_data
norm_dong[,c(-1,-2)] <- as.data.frame(lapply(norm_dong[,c(-1,-2)], normalize))

ggplot(data=norm_dong, aes(x=동, y=상권월소득금액, color=label))+geom_point() + ggtitle("동별 상권월소득금액") 
ggplot(data=norm_dong, aes(x=동, y=면적, color=label)) + geom_point() + ggtitle("동별 면적")
ggplot(norm_dong, aes(x=동, y=가구수, color=label)) + geom_point() + ggtitle("동별 가구수")
ggplot(norm_dong, aes(x=동, y=인구, color=label)) + geom_point()  + ggtitle("동별 인구")

ggplot(norm_dong, aes(x=동, y=인구밀도, color=label)) + geom_point()  + ggtitle("동별 인구밀도")
ggplot(norm_dong, aes(x=동, y=사업체, color=label)) + geom_point()  + ggtitle("동별 사업체")
ggplot(norm_dong, aes(x=동, y=유동인구.08., color=label)) + geom_point()  + ggtitle("동별 유동인구(8시)")
ggplot(norm_dong, aes(x=동, y=유동인구.19., color=label)) + geom_point()  + ggtitle("동별 유동인구(19시)")
ggplot(norm_dong, aes(x=동, y=은행, color=label)) + geom_point()  + ggtitle("동별 은행")
ggplot(norm_dong, aes(x=동, y=주차장, color=label)) + geom_point()  + ggtitle("동별 주차장")


# 데이터 산점도 
clustered_1st_raw_data<-read.csv("클러스터링1차_결과.csv",header=TRUE)

p<-ggvis(data = clustered_1st_raw_data,
         x=~구,y=~클러스터,fill=~클러스터)
layer_points(p)




