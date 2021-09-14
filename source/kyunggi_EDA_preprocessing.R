library(dplyr)
library(ggplot2)
library(rgdal)
library(sqldf)

## 데이터 로드 
df1 <- read.csv('data\\경기도_2016_2017.csv',encoding="utf-8")
df2 <- read.csv('data\\경기도_2018_2020.csv',encoding="utf-8")

accident <- rbind(df1,df2)

## 데이터 전처리 

accident$가해운전자.연령 <- gsub("세","",accident$가해운전자.연령)
accident$가해운전자.연령[accident$가해운전자.연령=="기타불명"] <- NA


accident$피해운전자.연령 <- gsub("세","",accident$피해운전자.연령)
accident$피해운전자.연령[accident$피해운전자.연령=="기타불명"] <- NA


Sys.setlocale(category = "LC_ALL", locale="us")

accident <- accident %>% 
  rename(gahe_age = 가해운전자.연령, pihe_age = 피해운전자.연령)

accident$gahe_age <- as.numeric(accident$gahe_age)
accident$pihe_age <- as.numeric(accident$pihe_age)


Sys.setlocale(category = "LC_ALL", locale="kor")

accident <- accident %>% 
  rename(가해운전자.연령 = gahe_age, 피해운전자.연령 = pihe_age)



## 사고 다발 지역 순위
accident_ranking <- sqldf("select 시군구, count(*) as 건수 from accident group by 시군구 order by 건수 desc")


## accident에 동이름 열 추가 
dong_names <- strsplit(accident$시군구,' ')

for (i in 1:length(accident$시군구)){
  accident$dong[i] <- dong_names[[i]][length(dong_names[[i]])]
  
}

## accident에 월 열 추가 
date_names <- strsplit(accident$사고일시,' ')

for (i in 1:length(accident$사고일시)){
  accident$발생.월[i] <- date_names[[i]][2]
  
}

accident$발생.월 <- gsub('월','',accident$발생.월)
accident$발생.월 <- as.numeric(accident$발생.월)

## accident에 시간 열 추가 
for (i in 1:length(accident$사고일시)){
  accident$발생.시간[i] <- date_names[[i]][4]
  
}

accident$발생.시간 <- gsub('시','',accident$발생.시간)
accident$발생.시간 <- as.numeric(accident$발생.시간)


## 사고 지역 지도시각화 -> 경향 파악 

map_shape <- readOGR('data\\경기동_읍면동_지도',layer='LSMD_ADM_SECT_UMD_41',encoding = 'utf-8')
map_shape <- spTransform(map_shape,CRS('+proj=longlat'))
map <- fortify(map_shape, region = "COL_ADM_SE") 


map_new <- fortify(map_shape, region = "EMD_NM")
map_new <- map_new %>% 
  group_by(id) %>% 
  summarise(lat = mean(lat),long = mean(long))

accident_map <- merge(accident[,c('사고번호','dong')],map_new,by.x='dong',by.y='id')
accident_map$사고번호 <- NULL
accident_map$dong <- NULL

for (i in 1:nrow(accident_map)){
  accident_map$lat[i] <- accident_map$lat[i]+ runif(1)/1000
  accident_map$long[i] <- accident_map$long[i] + runif(1)/1000
}


ggplot() + 
  geom_polygon(data = map, 
               aes(x = long, 
                   y = lat,
                   group = group), 
               color = 'white',
               fill='#109C23')+
  stat_density_2d(data=accident_map,geom="polygon",aes(x=long,y=lat,alpha=(..level..),fill="#F05013"))+
  
  scale_color_viridis_c()+
  theme_void()+
  theme(legend.position = 'none')

## 시별 사고건수 파악 
## 시 컬럼 추가

for (i in 1:length(accident$시군구)){
  accident$si[i] <- dong_names[[i]][2]
}

## 시별 사고건수
accident_ranking_si <- sqldf("select si, count(*) as 건수 from accident group by si order by 건수 desc")


## 사고 다발지역 수원시만 따로 필터링 
suwon_df <- accident %>% 
  filter(si=="수원시") %>% 
  select(-c(사고번호,사고일시,사망자수, 중상자수,경상자수,부상신고자수,시군구,가해운전자.상해정도,피해운전자.상해정도,dong,si)) %>% 
  rename(발생.월 = month, 발생.시간대 = 발생.시간)

suwon_df <- na.omit(suwon_df)
suwon_df$사고내용 <- ifelse(suwon_df$사고내용 %in% c("경상사고","부상사고"),0,1)

## 수원시 내 사고 다발 지역만 df로 

suwon_high_df <- suwon_high %>% 
  select(-c(사고번호,사고일시,사망자수, 중상자수,경상자수,부상신고자수,시군구,가해운전자.상해정도,피해운전자.상해정도,dong,si)) %>% 
  rename(발생.월 = month, 발생.시간대 = 발생.시간)

suwon_high_df$사고내용 <- ifelse(suwon_high_df$사고내용 %in% c("경상사고","부상사고"),0,1)

suwon_high_df <- suwon_high_df %>% 
  rename(가해운전자_성별 = 가해운전자.성별, 가해운전자_연령=가해운전자.연령,
         발생_월 = 발생.월, 발생_시간대 = 발생.시간대,
         피해운전자_성별 = 피해운전자.성별, 피해운전자_연령=피해운전자.연령,
         가해운전자_차종=가해운전자.차종, 피해운전자_차종=피해운전자.차종)

## 파이썬에서 쓸 csv 파일 출력 
write.csv(accident_new,"data/accident_new.csv")
write.csv(suwon_df,"data/suwon_df.csv")
write.csv(suwon_high_df,"data/suwon_high_df.csv")

