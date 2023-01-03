# 설치해야하는 패키지
# knn
install.packages("class")

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