accident_df <- merge(accident, accident_map, by="사고번호")

accident_df <- accident_df %>% 
  rename(가해운전자_차종 = 가해운전자.차종,
         가해운전자_성별 = 가해운전자.성별,
         가해운전자_연령 = 가해운전자.연령,
         가해운전자_상해정도 = 가해운전자.상해정도,
         피해운전자_차종 = 피해운전자.차종,
         피해운전자_성별 = 피해운전자.성별,
         피해운전자_연령 = 피해운전자.연령,
         피해운전자_상해정도 = 피해운전자.상해정도,
         행정동명 = dong.x,
         행정시명 = si,
         발생_월 = month,
         발생_시간대 = 발생.시간,
         위도 = lat,
         경도 = long) %>% 
  select(-dong.y)

write.csv(accident_df,"accident_df.csv")


