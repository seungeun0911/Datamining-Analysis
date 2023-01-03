raw_data<-read.csv("서울시_자치구별_데이터2.csv",header=TRUE)

raw_data<-raw_data[,-c(73:80)]
### 데이터 copy
original_data<-raw_data

### 라벨 추가
original_data<-cbind(original_data,clustered_1st_raw_data[,5])
names(original_data)[83]<-c("label")

### factor 지정 
original_data$label<-as.factor(original_data$label)

### 데이터 copy
norm_data<-original_data[-83]

### 정규화 : norm_data
norm_data[,-1]<-as.data.frame(lapply(norm_data[,-1], normalize))


###########################################

copy_data<-read.csv("서울시_자치구별_데이터.csv",header=TRUE,stringsAsFactors=FALSE)
copy_data[,-1] <- as.data.frame(lapply(copy_data[,-1], normalize))


### header 이름 변경하기
#1번
names(copy_data)[1]<-"자치구"

#2번
names(copy_data)[names(copy_data)=="자치구"]<-"district"

#3번
names(copy_data)[c(1,2)]<-c("a","b")

###
str(names(copy_data))
summary(names(copy_data))
mode(names(raw_data))

####구조, 통계 정보
str(copy_data)

###na 확인
#전체 na 수 계산
table(is.na(copy_data))
#각행마다 na 수 계산
colSums(is.na(copy_data))
#특정행에 false의 수를 모두 더해서 0인 열만 가져옴 =>na 값이 전혀 없는 애들
test1.nona <- copy_data[ , colSums(is.na(copy_data)) == 0]
copy_data$X10.11.인구변화[copy_data$X10.11.인구변화==NA]<-0
TEST_DATA<-ORIGINAL_DONG_DATA
#각행마다 na 수 계산
colSums(is.na(TEST_DATA))

###for문 이용

for(i in 1: length(TEST_DATA[,5]))
{
  
  if(TEST_DATA[i,5]==NA){
    
    # 클러스터 그룹 추출
    clusterNum<-subset(dong_clustering, dong_clustering$동==TEST_DATA[i,2])[2]
    
    # 해당 클러스터에 속하는 동 이름 추출
    nameList<-subset(dong_clustering, dong_clustering$클러스터==clusterNum)[1]
    
    sum<-0
    for(j in 1: length(nameList)){
      sum+=subset(TEST_DATA, TEST_DATA[,2]==nameList[j])[5]
    }
    
    # data<-sum(subset(TEST_DATA, TEST_DATA[,2]==nameList)[5])
    
    print(data)
    
  }
  
}