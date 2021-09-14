library(ggplot2)
library(dplyr)
library(sqldf)


for (i in 1:length(accident$사고일시)){
  accident$연도[i] <- date_names[[i]][1]
}

accident$연도 <- as.factor(gsub("년","",accident$연도))

accident_trend <- sqldf('select 연도, count(*) as 사고건수 from accident group by 연도')

ggplot(accident_trend, aes(x=연도, y=사고건수))+
  geom_bar(stat='identity', width=0.7)+
  xlab("년도")+
  ylab("교통사고 건수")+
  labs(title="연도별 교통사고 추이")+
  theme(plot.title = element_text(hjust = 0.5))




## 수원 지역 내 사고 다발 지역 ggmap 
accident_spot <- read.csv("data\\지자체별_사고다발_지역.csv")

accident_spot <- accident_spot %>% 
  filter(지점명 %in% c("경기도 수원시 팔달구 인계동(인계사거리 인근)",
                    "경기도 수원시 팔달구 인계동(나혜석거리 인근)",
                    "경기도 수원시 팔달구 인계동(나혜석거리 인근)",
                    "경기도 수원시 팔달구 인계동(인계사거리 인근)",
                    "경기도 수원시 팔달구 인계동(나혜석거리 인근)",
                    "경기도 수원시 권선구 권선동(권선사거리 인근)"))

accident_spot <- accident_spot[-c(1,2),] 

library(ggmap)

register_google("AIzaSyBKgybLTpGXJUkdZP2DFwolc_SaOwopigM")
ingye <- get_map("kwonsun il dong suwon",zoom=14,maptype="roadmap")
ggmap(ingye)+
  geom_point(data=accident_spot, mapping = aes(x=경도,y=위도),color="red",alpha=0.5,size=5)


## 20대후반 ~ 30대 가해 운전자들 특성 시각화 

law <- sqldf('select 법규위반, count(*) as 사고건수 from dangerous_age where 법규위반 != "안전운전불이행" group by 법규위반')

ggplot(data=law, aes(x=reorder(법규위반,-사고건수), y=사고건수))+
  geom_bar(stat='identity')+
  labs(title="법규위반 내용 별 사고건수",x="",y="")+
  theme(axis.text.x=element_text(angle=45, hjust=1,size=10))+
  theme(plot.title = element_text(hjust = 0.5))


accident_type <- sqldf('select 사고유형, count(*) as 사고건수 from dangerous_age group by 사고유형')

ggplot(data=accident_type, aes(x=reorder(사고유형,-사고건수), y=사고건수))+
  geom_bar(stat='identity')+
  labs(title="사고유형 별 사고건수",x="",y="")+
  theme(axis.text.x=element_text(angle=45, hjust=1,size=10))+
  theme(plot.title = element_text(hjust = 0.5))


accident_road <- sqldf('select 도로형태, count(*) as 사고건수 from dangerous_age group by 도로형태')

ggplot(data=accident_road, aes(x=reorder(도로형태,-사고건수), y=사고건수))+
  geom_bar(stat='identity')+
  labs(title="도로형태 별 사고건수",x="",y="")+
  theme(axis.text.x=element_text(angle=45, hjust=1,size=10))+
  theme(plot.title = element_text(hjust = 0.5))

road_type_percent <- dangerous_age %>% 
  group_by(도로형태) %>% 
  mutate(사고건수 = n()) %>% 
  select(도로형태, 사고건수)

road_type_percent <- distinct(road_type_percent,도로형태,사고건수)
road_type_percent$사고건수 <- road_type_percent$사고건수/sum(road_type_percent$사고건수)*100


## DNN 모델 돌릴 파일 

suwon_dnn <- suwon_accident %>% 
  select(-사고번호, -사고일시,-시군구, -사망자수, 
         -중상자수, -경상자수, -부상신고자수,-피해운전자.차종, 
         -피해운전자.성별, -피해운전자.연령,-피해운전자.상해정도,-가해운전자.상해정도,
         -사고유형,-si) %>% 
  rename(가해운전자_차종 = 가해운전자.차종,
         가해운전자_성별 = 가해운전자.성별, 
         가해운전자_연령 = 가해운전자.연령,
         행정동 = dong,
         발생월 = month,
         발생시간 = 발생.시간)

suwon_dnn <- na.omit(suwon_dnn)

suwon_dnn$사고내용 <- ifelse(suwon_dnn$사고내용 %in% c("사망사고","중상사고"),1,0)

write.csv(suwon_dnn,"data/suwon_dnn.csv")






