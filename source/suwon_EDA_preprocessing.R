library(dplyr)
library(ggplot2)
library(rgdal)
library(sqldf)

unique(accident$사고내용) ## 총 4가지 유형

unique(accident$사고유형) ## 크게 차대차, 차대사람, 차량단독, 철길건널목 4가지, 세부항목 21가지 

unique(accident$법규위반) ## 총 11가지 유형

unique(accident$노면상태) ## 포장, 비포장 대분류2개, 세부항목 (젖음, 습기,적설...) 16가지 

unique(accident$기상상태) ## 맑음, 안개, 기타, 흐림, 눈, 비 

unique(accident$도로형태) ## 단일로, 주차장, 교차로, ... 등등 14가지 소분류 

unique(accident$가해운전자.차종) ## 12가지 (승용, 화물, 승합, 이륜...)

unique(accident$가해운전자.상해정도) ## 상해없음, 경상, 부상신고, 사망, 중상, 기타불명 


## 경기도 전체 vs 수원시 비교 
suwon_accident <- accident %>% 
  filter(si == "수원시")

## (1) 사고 내용 별 사고건수 비교 -- 비슷

kyunggi_count <- sqldf("select 사고내용, count(*) as 사고건수 from accident group by 사고내용 order by 사고건수 desc")

ggplot(kyunggi_count, aes(x=reorder(사고내용, -사고건수),y=사고건수))+
  geom_bar(stat="identity")+
  xlab("사고내용")+
  labs(title="경기도 - 사고 내용 별 사고건수")+
  theme(plot.title = element_text(hjust = 0.5))

suwon_count <- sqldf("select 사고내용, count(*) as 사고건수 from suwon_accident group by 사고내용 order by 사고건수 desc")

ggplot(suwon_count, aes(x=reorder(사고내용, -사고건수),y=사고건수))+
  geom_bar(stat="identity")+
  xlab("사고내용")+
  labs(title="수원시 - 사고 내용 별 사고건수")+
  theme(plot.title = element_text(hjust = 0.5))

## (2) 사고 유형 별 사고건수 비교 -- 비슷 

kyunggi_count <- sqldf("select 사고유형, count(*) as 사고건수 from accident group by 사고유형 order by 사고건수 desc")

ggplot(kyunggi_count, aes(x=reorder(사고유형, -사고건수),y=사고건수))+
  geom_bar(stat="identity")+
  xlab("사고내용")+
  labs(title="경기도 - 사고 유형 별 사고건수")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90))


suwon_count <- sqldf("select 사고유형, count(*) as 사고건수 from suwon_accident group by 사고유형 order by 사고건수 desc")

ggplot(suwon_count, aes(x=reorder(사고유형, -사고건수),y=사고건수))+
  geom_bar(stat="identity")+
  xlab("사고내용")+
  labs(title="수원시 - 사고 유형 별 사고건수")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90))


## (3) 법규위반 내용 별 사고건수 비교 -- 비슷

kyunggi_count <- sqldf("select 법규위반, count(*) as 사고건수 from accident group by 법규위반 order by 사고건수 desc")

ggplot(kyunggi_count, aes(x=reorder(법규위반, -사고건수),y=사고건수))+
  geom_bar(stat="identity")+
  xlab("법규위반")+
  labs(title="경기도 - 법규위반 별 사고건수")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90))


suwon_count <- sqldf("select 법규위반, count(*) as 사고건수 from suwon_accident group by 법규위반 order by 사고건수 desc")

ggplot(suwon_count, aes(x=reorder(법규위반, -사고건수),y=사고건수))+
  geom_bar(stat="identity")+
  xlab("법규위반")+
  labs(title="수원시 - 법규위반 별 사고건수")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90))

## (4) 노면 상태 별 사고건수 비교 -- 비슷

kyunggi_count <- sqldf("select 노면상태, count(*) as 사고건수 from accident group by 노면상태 order by 사고건수 desc")

ggplot(kyunggi_count, aes(x=reorder(노면상태, -사고건수),y=사고건수))+
  geom_bar(stat="identity")+
  xlab("노면상태")+
  labs(title="경기도 - 노면상태 별 사고건수")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90))


suwon_count <- sqldf("select 노면상태, count(*) as 사고건수 from suwon_accident group by 노면상태 order by 사고건수 desc")

ggplot(suwon_count, aes(x=reorder(노면상태, -사고건수),y=사고건수))+
  geom_bar(stat="identity")+
  xlab("노면상태")+
  labs(title="수원시 - 노면상태 별 사고건수")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90))


## (5) 기상상태 별 사고건수 비교 -- 비슷 (맑음이 압도적 1위)

kyunggi_count <- sqldf("select 기상상태, count(*) as 사고건수 from accident group by 기상상태 order by 사고건수 desc")

ggplot(kyunggi_count, aes(x=reorder(기상상태, -사고건수),y=사고건수))+
  geom_bar(stat="identity")+
  xlab("기상상태")+
  labs(title="경기도 - 기상상태 별 사고건수")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90))


suwon_count <- sqldf("select 기상상태, count(*) as 사고건수 from suwon_accident group by 기상상태 order by 사고건수 desc")

ggplot(suwon_count, aes(x=reorder(기상상태, -사고건수),y=사고건수))+
  geom_bar(stat="identity")+
  xlab("기상상태")+
  labs(title="수원시 - 기상상태 별 사고건수")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90))


## (6) 도로형태 별 사고건수 비교 

kyunggi_count <- sqldf("select 도로형태, count(*) as 사고건수 from accident group by 도로형태 order by 사고건수 desc")

ggplot(kyunggi_count, aes(x=reorder(도로형태, -사고건수),y=사고건수))+
  geom_bar(stat="identity")+
  xlab("도로형태")+
  labs(title="경기도 - 도로형태 별 사고건수")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90))


suwon_count <- sqldf("select 도로형태, count(*) as 사고건수 from suwon_accident group by 도로형태 order by 사고건수 desc")

ggplot(suwon_count, aes(x=reorder(도로형태, -사고건수),y=사고건수))+
  geom_bar(stat="identity")+
  xlab("도로형태")+
  labs(title="수원시 - 도로형태 별 사고건수")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90))


## (7) 가해운전자.차종 별 사고건수 비교 -- 비슷 (승용차가 압도적으로 높음)

kyunggi_count <- accident %>% 
  group_by(가해운전자.차종) %>% 
  mutate(사고건수 = n()) %>% 
  select(가해운전자.차종, 사고건수) %>% 
  distinct(가해운전자.차종,사고건수)

ggplot(kyunggi_count, aes(x=reorder(가해운전자.차종, -사고건수),y=사고건수))+
  geom_bar(stat="identity")+
  xlab("가해운전자 차종")+
  labs(title="경기도 - 가해운전자 차종 별 사고건수")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90))


suwon_count <- suwon_accident %>% 
  group_by(가해운전자.차종) %>% 
  mutate(사고건수 = n()) %>% 
  select(가해운전자.차종, 사고건수) %>% 
  distinct(가해운전자.차종,사고건수)

ggplot(suwon_count, aes(x=reorder(가해운전자.차종, -사고건수),y=사고건수))+
  geom_bar(stat="identity")+
  xlab("가해운전자 차종")+
  labs(title="수원시 - 가해운전자 차종 별 사고건수")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90))


## (8) 요일 별 사고건수 비교 

kyunggi_count <- sqldf("select 요일, count(*) as 사고건수 from accident group by 요일 order by 사고건수 desc")

ggplot(kyunggi_count, aes(x=reorder(요일, -사고건수),y=사고건수))+
  geom_bar(stat="identity")+
  xlab("요일")+
  labs(title="경기도 - 요일 별 사고건수")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90))


suwon_count <- sqldf("select 요일, count(*) as 사고건수 from suwon_accident group by 요일 order by 사고건수 desc")

ggplot(suwon_count, aes(x=reorder(요일, -사고건수),y=사고건수))+
  geom_bar(stat="identity")+
  xlab("요일")+
  labs(title="수원시 - 요일 별 사고건수")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90))


## (9) 가해 운전자 성별 사고건수 비교 -- 비슷 (남성이 압도적으로 높음)

kyunggi_count <- accident %>% 
  group_by(가해운전자.성별) %>% 
  mutate(사고건수 = n()) %>% 
  select(가해운전자.성별, 사고건수) %>% 
  distinct(가해운전자.성별,사고건수)

ggplot(kyunggi_count, aes(x=reorder(가해운전자.성별, -사고건수),y=사고건수))+
  geom_bar(stat="identity")+
  xlab("가해운전자 성별")+
  labs(title="경기도 - 가해운전자 성별 별 사고건수")


suwon_count <- suwon_accident %>% 
  group_by(가해운전자.성별) %>% 
  mutate(사고건수 = n()) %>% 
  select(가해운전자.성별, 사고건수) %>% 
  distinct(가해운전자.성별,사고건수)

ggplot(suwon_count, aes(x=reorder(가해운전자.성별, -사고건수),y=사고건수))+
  geom_bar(stat="identity")+
  xlab("가해운전자 성별")+
  labs(title="수원시 - 가해운전자 성별 별 사고건수")


## (10) 가해운전자 연령 별 사고건수 -- 거의 비슷 (50대가 가장 많이 나타남)

kyunggi_count <- accident %>% 
  group_by(가해운전자.연령) %>% 
  mutate(사고건수 = n()) %>% 
  select(가해운전자.연령, 사고건수) %>% 
  distinct(가해운전자.연령,사고건수)

ggplot(kyunggi_count, aes(x=가해운전자.연령,y=사고건수))+
  geom_line()+
  geom_point()+
  xlab("가해운전자 연령")+
  labs(title="경기도 - 가해운전자 연령 별 사고건수")


suwon_count <- suwon_accident %>% 
  group_by(가해운전자.연령) %>% 
  mutate(사고건수 = n()) %>% 
  select(가해운전자.연령, 사고건수) %>% 
  distinct(가해운전자.연령,사고건수)

ggplot(suwon_count, aes(x=가해운전자.연령,y=사고건수))+
  geom_line()+
  geom_point()+
  xlab("가해운전자 연령")+
  labs(title="수원시 - 가해운전자 연령 별 사고건수")


## (11) 피해자 상해정도 비교 -- 비슷 (경상이 압도적으로 높음)

kyunggi_count <- accident %>% 
  group_by(피해운전자.상해정도) %>% 
  mutate(사고건수 = n()) %>% 
  select(피해운전자.상해정도, 사고건수) %>% 
  distinct(피해운전자.상해정도,사고건수)

ggplot(kyunggi_count, aes(x=reorder(피해운전자.상해정도, -사고건수),y=사고건수))+
  geom_bar(stat="identity")+
  xlab("피해운전자 상해정도")+
  labs(title="경기도 - 피해운전자 상해정도 별 사고건수")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90))


suwon_count <- suwon_accident %>% 
  group_by(피해운전자.상해정도) %>% 
  mutate(사고건수 = n()) %>% 
  select(피해운전자.상해정도, 사고건수) %>% 
  distinct(피해운전자.상해정도,사고건수)

ggplot(suwon_count, aes(x=reorder(피해운전자.상해정도, -사고건수),y=사고건수))+
  geom_bar(stat="identity")+
  xlab("피해운전자 상해정도")+
  labs(title="수원시 -  피해운전자 상해정도 별 사고건수")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90))



## (12) 수원시 교통사고 통계 (행정동 별)
suwon_accident_ranking <- sqldf("select 시군구, count(*) as 사고건수 from suwon_accident group by 시군구 order by 사고건수 desc")
suwon_accident_ranking

