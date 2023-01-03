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
ORIGINAL_SAMPLE_DATA<-original_dong_data
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