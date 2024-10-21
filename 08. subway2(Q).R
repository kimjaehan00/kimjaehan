#실습에 필요한 packages를 라이브러리에 등록
library(dplyr)
library(ggplot2)

#CSV형식의 파일 불러와서 congestion객체에 입력하고 구조 확인
str(congestion)

#변수의 이상치와 결측치 확인하고 처리
summary(congestion)
#결측치 갯수 확인
is.na(congestion)
sum(is.na(congestion))
colSums(is.na(congestion))
#결측치가 있는 행을 제거한 새로운 데이터 프레임 생성
#6시 출발기차의 결측치를 제거
congestion1 <- congestion[!is.na(congestion$s0600),]
colSums(is.na(congestion1))
#23시 30분 출발기차의 결측치를 제거
congestion1 <- congestion1[!is.na(congestion1$s2330),]
colSums(is.na(congestion1))
#남은 결측치를 0으로 대체
congestion1[is.na(congestion1)] <- 0
colSums(is.na(congestion1))

#이상치 확인
ggplot(congestion1, aes(y=s0530))+
  geom_boxplot()
summary(congestion1$s0530)

#파생변수만들기
#1.지하철역의 하루 평균 혼잡도
congestion1$day_mean <- rowMeans(congestion1[,c('s0530','s0600','s0630','s0700','s0730','s0800','s0830','s0900','s0930','s1000','s1030','s1100','s1130','s1200','s1230','s1300','s1330','s1400','s1430','s1500','s1530','s1600','s1630','s1700','s1730','s1800','s1830','s1900','s1930','s2000','s2030','s2100','s2130','s2200','s2230','s2300','s2330')])

#데이터분석
#1.  수도권 지하철의 하루 평균 혼잡도
passenger_daymean <- congestion1 %>%
  group_by(station)%>%
  summarise(m=mean(day_mean))%>% 
  arrange(desc(m))%>%
  head(10)

ggplot(data=passenger_daymean, aes(x=reorder(station, m), y=m))+
  geom_col()+
  coord_flip()

#2. 호선별 하루평균혼잡도
line_pm <- congestion1 %>%
  group_by(line) %>%
  summarise(total=mean(day_mean))
line_pm8 <- line_pm %>%
  filter(line%in%c("1","2","3","4","5","6","7","8" ))
ggplot(data = line_pm8, aes(x=reorder(line,total),y=total))+
  geom_col()+
  coord_flip()+
  ggtitle("서울 지하철 호선별 하루평균 혼잡도")+
  xlab("노선")+
  ylab("하루평균혼잡도")

#2. 호선별 출근시간(07:00~09:00)의 혼잡도 평균
line_pm0700 <- congestion1 %>%
  group_by(line) %>%
  summarise(r=mean(s0700+s0730+s0800+s0830+s0900))
line_pm8_0700 <- line_pm0700 %>%
  filter(line%in%c("1","2","3","4","5","6","7","8" ))
ggplot(data = line_pm8_0700, aes(x=reorder(line,r),y=r))+
  geom_col()+
  coord_flip()+
  ggtitle("서울 지하철 호선별 출근시간 혼잡도")+
  xlab("노선")+
  ylab("출근시간혼잡도")

#2-1. 호선별 출근시간(07:00~09:00)의 기술통계
congestion_stats <- congestion1 %>%
  group_by(line) %>%
  summarise(
    mean = mean(s0700 + s0730 + s0800 + s0830 + s0900),
    median = median(s0700 + s0730 + s0800 + s0830 + s0900),
    min = min(s0700 + s0730 + s0800 + s0830 + s0900),
    max = max(s0700 + s0730 + s0800 + s0830 + s0900),
    q1 = quantile(s0700 + s0730 + s0800 + s0830 + s0900, 0.25),
    q3 = quantile(s0700 + s0730 + s0800 + s0830 + s0900, 0.75)
  ) %>%
  arrange(desc(mean))
congestion_stats

#2-2. 평균혼잡도가 가장 높은 출근 시간대를 막대그래프로 그리기
time_means <- data.frame(
  time = c("s0700", "s0730", "s0800", "s0830", "s0900"),
  mean_congestion = c(
    mean(congestion1$s0700),
    mean(congestion1$s0730),
    mean(congestion1$s0800),
    mean(congestion1$s0830),
    mean(congestion1$s0900)
  )
)

ggplot(data = time_means, aes(x = reorder(time, mean_congestion), y = mean_congestion)) +
  geom_col() +
  coord_flip() +
  ggtitle("07:00~09:00 평균 혼잡도") +
  xlab("시간대") +
  ylab("평균 혼잡도")

# 2-3. 출근시간 평균혼잡도가 가장 높은 호선에서 기여도가 높은 역
line_means <- congestion1 %>%
  group_by(line) %>%
  summarise(mean_congestion = mean(s0700 + s0730 + s0800 + s0830 + s0900)) %>%
  arrange(desc(mean_congestion))

station_contribution <- congestion1 %>%
  filter(line == line_means$line[1]) %>%
  group_by(station) %>%
  summarise(mean_congestion = mean(day_mean)) %>%
  arrange(desc(mean_congestion))

ggplot(data = station_contribution, aes(x = reorder(station, mean_congestion), y = mean_congestion)) +
  geom_col() +
  coord_flip() +
  ggtitle(paste(top_line, "호선에서 기여도가 높은 역")) +
  xlab("역") +
  ylab("평균 혼잡도")

#3.08시 지하철 혼잡도 범주화/범주별 빈도분석
congestion1 %>%
  mutate(s80_grade=ifelse(s0800<=80, "good", ifelse(s0800<=130, "normal", ifelse(s0800<=150, "caution", "bad"))))%>%
  group_by(s80_grade) %>%
  summarise(n=n())%>%
  mutate(total=sum(n), pct=round(n/total*100,1))%>%
  select(s80_grade,n,pct)%>%
  arrange(desc(n))

#3-1. 호선별로 08시 지하철 혼잡도 범주화
congestion1 %>%
  mutate(s80_grade=ifelse(s0800<=80, "good", ifelse(s0800<=130, "normal", ifelse(s0800<=150, "caution", "bad"))))%>%
  group_by(line, s80_grade) %>%
  summarise(n=n())%>%
  mutate(total=sum(n), pct=round(n/total*100,1))%>%
  filter(s80_grade=="caution")%>%
  select(line, s80_grade,n,pct)%>%
  arrange(desc(pct))%>%
  head(5)

#4. 호선별 퇴근시간(18:00~20:00)의 혼잡도 평균
line_pm1800 <- congestion1 %>%
  group_by(line) %>%
  summarise(r=mean(s1800+s1830+s1900+s1930+s2000))
line_pm8_1800 <- line_pm1800 %>%
  filter(line%in%c("1","2","3","4","5","6","7","8" ))
ggplot(data = line_pm8_1800, aes(x=reorder(line,r),y=r))+
  geom_col()+
  coord_flip()+
  ggtitle("서울 지하철 호선별 퇴근시간 혼잡도")+
  xlab("노선")+
  ylab("퇴근시간혼잡도")


#4-1. 호선별 퇴근시간(18:00~20:00)의 기술통계
congestion_stats2 <- congestion1 %>%
  group_by(line) %>%
  summarise(
    mean = mean(s1800+s1830+s1900+s1930+s2000),
    median = median(s1800+s1830+s1900+s1930+s2000),
    min = min(s1800+s1830+s1900+s1930+s2000),
    max = max(s1800+s1830+s1900+s1930+s2000),
    q1 = quantile(s1800+s1830+s1900+s1930+s2000, 0.25),
    q3 = quantile(s1800+s1830+s1900+s1930+s2000, 0.75)
  ) %>%
  arrange(desc(mean))
congestion_stats2

#4-2. 평균혼잡도가 가장 높은 퇴근 시간대를 막대그래프로 그리기
time_means2 <- data.frame(
  time = c("s1800", "s1830", "s1900", "s1930", "s2000"),
  mean_congestion = c(
    mean(congestion1$s1800),
    mean(congestion1$s1830),
    mean(congestion1$s1900),
    mean(congestion1$s1930),
    mean(congestion1$s2000)
  )
)

ggplot(data = time_means2, aes(x = reorder(time, mean_congestion), y = mean_congestion)) +
  geom_col() +
  coord_flip() +
  ggtitle("18:00~20:00 평균 혼잡도") +
  xlab("시간대") +
  ylab("평균 혼잡도")


#4-3. 퇴근시간 평균혼잡도가 가장 높은 호선에서 기여도가 높은 역
line_means2 <- congestion1 %>%
  group_by(line) %>%
  summarise(mean_congestion = mean(s1800 + s1830 + s1900 + s1930 + s2000)) %>%
  arrange(desc(mean_congestion))

station_contribution2 <- congestion1 %>%
  filter(line == line_means2$line[1]) %>%
  group_by(station) %>%
  summarise(mean_congestion = mean(day_mean)) %>%
  arrange(desc(mean_congestion))

ggplot(data = station_contribution2, aes(x = reorder(station, mean_congestion), y = mean_congestion)) +
  geom_col() +
  coord_flip() +
  ggtitle(paste(top_line, "호선에서 기여도가 높은 역")) +
  xlab("역") +
  ylab("평균 혼잡도")
