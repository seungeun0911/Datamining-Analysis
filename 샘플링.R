set.seed(1234) 
###1.샘플링###
# (1) 랜덤 샘플(7:3)
ind <- sample(2, nrow(NORM_DONG_DATA_ALL), replace=TRUE, prob=c(0.7, 0.3))



# (2) 라벨 정렬 후 샘플링

# [ train , test 데이터 수 계산 ]
#train data의 수 (전체데이터의70%) : 423 * 0.7 =296
#라벨 분포 계산 label(0) : label(1) : label(2) =141:156:126 [sum(original_data$label==0)]
#라벨 0과 1에서 각각 296의 1/3인 98씩 뽑았다. 

train <- c(sample(1:142,98), sample(143:298,98), sample(299:424,98))

###2. training데이터, test데이터###
subway.training <- NORM_DONG_DATA_ALL[ind==1,] 
subway.test <- NORM_DONG_DATA_ALL[ind==2,]
subway.trainLabels <- NORM_DONG_DATA_ALL[ind==1,2] #label 정보 지정 (label 정보는 2번 속성에 있음)
subway.testLabels <- NORM_DONG_DATA_ALL[ind==2,2]
str(NORM_DONG_DATA_ALL)