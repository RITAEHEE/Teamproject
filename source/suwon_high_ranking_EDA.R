library(ggplot2)
library(dplyr)
library(sqldf)

## 집중 동네 : 인계동, 권선동, 세류동, 매탄동

address <- c("경기도 수원시 팔달구 인계동",
             "경기도 수원시 권선구 권선동",
             "경기도 수원시 권선구 세류동",
             "경기도 수원시 영통구 매탄동")

suwon_high <- suwon_accident[suwon_accident$시군구 %in% address,]

## (1) 노면 상태 별 사고건수 비교 -- 비슷

suwon_count <- sqldf("select 노면상태, count(*) as 사고건수 from suwon_high group by 노면상태 order by 사고건수 desc")

ggplot(suwon_count, aes(x=reorder(노면상태, -사고건수),y=사고건수))+
  geom_bar(stat="identity")+
  xlab("노면상태")+
  labs(title="수원시 - 노면상태 별 사고건수")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90))



## (2) 도로형태 별 사고건수 비교 

suwon_count <- sqldf("select 도로형태, count(*) as 사고건수 from suwon_high group by 도로형태 order by 사고건수 desc")

ggplot(suwon_count, aes(x=reorder(도로형태, -사고건수),y=사고건수))+
  geom_bar(stat="identity")+
  xlab("도로형태")+
  labs(title="수원시 - 도로형태 별 사고건수")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90))

## (3) 요일별 사고건수

suwon_count <- sqldf("select 요일, count(*) as 사고건수 from suwon_high group by 요일 order by 사고건수 desc")

ggplot(suwon_count, aes(x=reorder(요일, -사고건수),y=사고건수))+
  geom_bar(stat="identity")+
  xlab("요일")+
  labs(title="수원시 - 요일 별 사고건수")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90))


## (4) 연령별 사고건수 

suwon_count <- suwon_high %>% 
  group_by(가해운전자.연령) %>% 
  mutate(사고건수 = n()) %>% 
  select(가해운전자.연령, 사고건수) %>% 
  distinct(가해운전자.연령,사고건수)

ggplot(suwon_count, aes(x=가해운전자.연령,y=사고건수))+
  geom_line()+
  geom_point()+
  xlab("가해운전자 연령")+
  labs(title="수원시 - 가해운전자 연령 별 사고건수")


## (5) 법규 위반별 사고건수 

suwon_count <- sqldf("select 법규위반, count(*) as 사고건수 from suwon_high group by 법규위반 order by 사고건수 desc")

ggplot(suwon_count, aes(x=reorder(법규위반, -사고건수),y=사고건수))+
  geom_bar(stat="identity")+
  xlab("법규위반")+
  labs(title="수원시 - 법규위반 별 사고건수")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90))


## (6) 가해 운전자 차종 별 사고건수 
suwon_count <- suwon_high %>% 
  group_by(가해운전자.차종) %>% 
  mutate(사고건수 = n()) %>% 
  select(가해운전자.차종, 사고건수) %>% 
  distinct(가해운전자.차종,사고건수)

ggplot(suwon_count, aes(x=reorder(가해운전자.차종, -사고건수),y=사고건수))+
  geom_bar(stat="identity")+
  xlab("가해운전자 차종")+
  labs(title="수원시 - 가해운전자 차종 별 사고건수")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90))

## (7) 월별 사고건수 

suwon_count <- sqldf("select month, count(*) as 사고건수 from suwon_high group by month order by 사고건수 desc")

ggplot(suwon_count, aes(x=reorder(month, -사고건수),y=사고건수))+
  geom_bar(stat="identity")+
  xlab("month")+
  labs(title="수원시 - month 별 사고건수")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90))

## (8) 시간대별 사고건수 

suwon_count <- sqldf("select 발생시간대, count(*) as 사고건수 from suwon_high_df group by 발생시간대 order by 사고건수 desc")

ggplot(suwon_count, aes(x=reorder(발생시간대, -사고건수),y=사고건수))+
  geom_bar(stat="identity")+
  xlab("month")+
  labs(title="수원시 - 발생시간대 별 사고건수")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90))


## (9) 피해.운전자연령 별 사고건수 (수원시 전체 vs 다발지역)
suwon_high_df <- na.omit(suwon_high_df)

suwon_count <- suwon_high_df %>% 
  group_by(피해운전자.연령) %>% 
  mutate(사고건수 = n()) %>% 
  select(피해운전자.연령,사고건수)

ggplot(suwon_count, aes(x=피해운전자.연령,y=사고건수))+
  geom_line()+
  xlab("피해운전자.연령")+
  labs(title="수원시 - 피해운전자.연령 별 사고건수")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90))


suwon_count <- suwon_high_df %>% 
  group_by(가해운전자.연령) %>% 
  mutate(사고건수 = n()) %>% 
  select(가해운전자.연령, 사고건수) %>% 
  distinct(가해운전자.연령,사고건수)

ggplot(suwon_count, aes(x=가해운전자.연령,y=사고건수))+
  geom_line()+
  xlab("가해운전자 연령")+
  labs(title="수원시 - 가해운전자 연령 별 사고건수")

