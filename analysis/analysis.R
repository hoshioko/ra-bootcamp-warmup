#分析
#(a)記述統計
# 1.「(d) Master Dataの作成」で作成したデータの、各列に含まれるNAの数を数えなさい。
num_na <- list()
for (i in 1:length(Master_Data)) {
  num_na[i] <- sum(is.na(Master_Data[i]))
}
print(num_na)

# 2.問題背景などを知る上で役に立つ記述統計を作成しなさい
library(tidyverse)
library(dplyr)
library(gt)
#必要なもの 全部、制度を変えなかったグループ、制度を変えたグループ
#セメスター制の割合、4年制の卒業者割合、
# ステータス変数の作成
Master_Data <- Master_Data %>%
  mutate(status = ifelse(is.na(after), "Never Switchers", "Switchers"))
Master_Data$semester <- as.numeric(Master_Data$semester)
summary_data <- Master_Data %>%
  group_by(status) %>%
  summarize(
    semester_rate = mean(semester, na.rm = TRUE),
    total_graduate_rate = mean(gradrate4yr, na.rm = TRUE),
    men_graduate_rate = mean(mengradrate4yr, na.rm = TRUE),
    women_graduate_rate = mean(womengradrate4yr, na.rm = TRUE)
  )
all_data <- list()
all_data$semester_rate <- mean(Master_Data$semester, na.rm = TRUE)
all_data$total_graduate_rate <- mean(Master_Data$gradrate4yr, na.rm = TRUE)
all_data$men_graduate_rate <- mean(Master_Data$mengradrate4yr, na.rm = TRUE)
all_data$women_graduate_rate <- mean(Master_Data$womengradrate4yr, na.rm = TRUE)
summary_data <- bind_rows(summary_data, all_data)
summary_data <- summary_data %>%
  mutate(across(where(is.numeric), ~ round(., 2)))
summary_data[3,1] <- "All"
summary_data <- summary_data %>%
  rename("Semester calendar" = 2, 
         "Four-year graduation rate" = 3,
         "Four-year men graduation rate" = 4, 
         "Four-year women graduation rate" = 5)
summary_data <- as.data.frame(summary_data)
rownames(summary_data) <- summary_data[, 1]
summary_data <- summary_data[,-1]
summary_data <- t(summary_data)
summary_data <- cbind(rownames(summary_data), summary_data)
summary_data <- as.data.frame(summary_data)
summary_data <- summary_data %>%
  rename(" " = 1)
summary_data %>% 
  gt() %>% 
  tab_header(title = "Table")

# 以下の問題への解答は同時に示す
# 3.4年卒業率の平均推移を計算し、図で示しなさい
# 4.semester導入率を計算し、図で示しなさい
library(ggplot2)
Master_Data$semester <- as.numeric(Master_Data$semester)
graph_data <- aggregate(cbind(gradrate4yr,semester) ~  year, Master_Data, mean)

par(oma = c(0, 0, 0, 0))
plot(x=graph_data$year, y=graph_data$semester, ylim=c(min(graph_data$semester), max(graph_data$semester)), 
     type='l', lwd=1.5,xlab='Year', 
     ylab='Fractions of schools on semesters', axes = F)
axis(2)
par(new=T)
plot(x=graph_data$year, y=graph_data$gradrate4yr, xlim=c(1990, max(graph_data$year)),
     ylim=c(min(graph_data$gradrate4yr), max(graph_data$gradrate4yr)),
     type='l', lty='dotted', lwd=1.5,
     xlab='', ylab='', axes = F)
mtext('4-year graduation rate', side = 4, line = 3)
axis(4)
axis(1)
box()
legend("bottomright", legend = c("Share on semesters", "Four-year graduation rate"),
       lty = c('solid', 'dotted'), lwd=1.5, cex=0.8)

# 5.以下の3つの変数を横軸、「4年卒業率」を縦軸にとった、散布図を作成しなさい。
#大学ごとにそれぞれの変数の期間内の平均値を求め、それから散布図を作成した。
#そのまま作成したものはデータ数が多すぎて、分布がわかりにくくなったからである。
scatter_plot <- function(data, x, y) {
  x <- enquo(x)
  y <- enquo(y)
  ggplot(data) + geom_point(aes(!!x, !!y))+ 
    xlab(quo_name(x)) + ylab("4-year graduation rate")
}

#女子学生比率
Master_Data <- mutate(Master_Data, w_cohortsize = as.numeric(w_cohortsize), totcohortsize = as.numeric(totcohortsize))
women_rate <- Master_Data$w_cohortsize / Master_Data$totcohortsize
plot_data <- aggregate(cbind(women_rate,gradrate4yr) ~  unitid, Master_Data, mean)
scatter_plot(plot_data, women_rate, gradrate4yr)

#白人学生割合
Master_Data <- mutate(Master_Data, white_cohortsize = as.numeric(white_cohortsize))
white_rate <- Master_Data$white_cohortsize / Master_Data$totcohortsize
plot_data2 <- aggregate(cbind(white_rate,gradrate4yr) ~  unitid, Master_Data, mean)
scatter_plot(plot_data2, white_rate, gradrate4yr)

#学費(instatetuition)
Master_Data <- mutate(Master_Data, instatetuition = as.numeric(instatetuition))
plot_data3 <- aggregate(cbind(instatetuition,gradrate4yr) ~  unitid, Master_Data, mean)
scatter_plot(plot_data3, instatetuition, gradrate4yr)

#(b)回帰分析
library(broom)
result <- lm(gradrate4yr~after , data=Master_Data) %>%
  tidy()
