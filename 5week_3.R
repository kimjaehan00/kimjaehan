table(X2024STB_survey $Gender)
ECN <- table(X2024STB_survey $Gender)
prop.table(ECN)

table(X2024STB_survey $Gender, X2024STB_survey $Grade)

barplot(table(X2024STB_survey $Nationality))
  
library(RColorBrewer)
pal1 <- brewer.pal(5, 'Set3')
barplot(table(X2024STB_survey $`residential area`),col=pal1, xlab = "지역명", ylab = "명수", xlim=c(0,90), horiz=TRUE)

barplot(table(X2024STB_survey $Gender, X2024STB_survey $Grade))

pie(table(X2024STB_survey $Grade))

hist(X2024STB_survey $Age, main="나이분포", col = terrain.colors(12),freq= FALSE)

boxplot(Age ~ Grade, data = X2024STB_survey, main = "Comparison of Age by Grade", xlab = "Grade", ylab = "Age")

clean_data <- X2024STB_survey

# 산점도 생성
plot(x = clean_data$Grade, 
     y = clean_data$Age, 
     xlab = "grade", 
     ylab = "age", 
     main = "Comparison of Age by Grade")
