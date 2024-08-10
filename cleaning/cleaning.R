#データ整理と変換
#(a)Semester Data の整形
# 1.生データを読み込みなさい (semester_dummy_1.csv, semester_dummy_2.csv)
data1 <- read.csv("semester_data_1.csv")
data2 <- read.csv("semester_data_2.csv")

# 2.semester_dummy_1.csvについては、1行目を列名としなさい
colnames(data1) <- data1[1,]

# 3.2つのデータを適切に結合しなさい
colnames(data2) <- data1[1,]
Semester_Data <- rbind(data1, data2)
Semester_Data <- Semester_Data[-1,]

# 4.’Y’列を削除しなさい
Semester_Data <- Semester_Data[,-6]

# 5.semester制が導入された年の列を作成しなさい。
library(dplyr)
Semester_Data <- Semester_Data %>%
  group_by(unitid) %>%
  mutate(yearofsem = year[which(quarter == 0 & semester == 1)[1]])
Semester_Data$yearofsem[Semester_Data$yearofsem == 1991] <- NA

# 6.5.を用いてsemester制導入後を示すダミー変数を作成しなさい
library(tidyverse)
Semester_Data$yearofsem <- parse_number(Semester_Data$yearofsem)
Semester_Data$year <- parse_number(Semester_Data$year)
Semester_Data$after <- as.integer(Semester_Data$yearofsem<=Semester_Data$year)

#(b)Gradrate Dataの整形
# 1.生データを読み込み、適切に結合しなさい
library(openxlsx)
file_paths1 <- paste0("/Users/nakamurayuuki/outcome/", 1991:1993, ".xlsx")
file_paths2 <- paste0("/Users/nakamurayuuki/outcome/", 1995:2016, ".xlsx")
file_paths <- c(file_paths1, file_paths2)
data_list <- list()
for (i in 1:length(file_paths)) {
  data_list[[i]] <- read.xlsx(file_paths[i])
}
Graduate_Data <- data_list[[1]]
for (i in 2:length(file_paths)){
  Graduate_Data <- rbind(Graduate_Data, data_list[[i]])
}

# 2.女子学生の4年卒業率に0.01をかけて、0から1のスケールに変更しなさい
Graduate_Data$womengradrate4yr <- Graduate_Data$women_gradrate_4yr * 0.01 

# 3.男女合計の4年卒業率と男子学生の4年卒業率を計算し、新たな列として追加しなさい
Graduate_Data <- mutate(Graduate_Data, tot4yrgrads = as.numeric(tot4yrgrads), totcohortsize = as.numeric(totcohortsize))
Graduate_Data$gradrate4yr <- Graduate_Data$tot4yrgrads / Graduate_Data$totcohortsize
Graduate_Data <- mutate(Graduate_Data, m_4yrgrads = as.numeric(m_4yrgrads), m_cohortsize = as.numeric(m_cohortsize))
Graduate_Data$mengradrate4yr <- Graduate_Data$m_4yrgrads / Graduate_Data$m_cohortsize

# 4.計算した卒業率を有効数字3桁に調整しなさい
Graduate_Data <- mutate(Graduate_Data, gradrate4yr = as.numeric(gradrate4yr))
Graduate_Data$gradrate4yr %>%
  round(digits = 3)

Graduate_Data <- mutate(Graduate_Data, mengradrate4yr = as.numeric(mengradrate4yr))
Graduate_Data$mengradrate4yr %>%
  round(digits = 3)

# 5.1991年から2010年までのデータフレームに変形しなさい
Graduate_Data <- Graduate_Data %>%
  mutate(year = as.numeric(year)) %>%
  filter(year <= 2010)

#(c)Covariates Dataの整形
# 1.生データを読み込みなさい (covariates.xlsx)
library(openxlsx)
Covariates_Data <- read.xlsx("covariates.xlsx")

# 2.’university_id’という列名を’unitid’に変更しなさい
library(dplyr)
Covariates_Data <- rename(.data = Covariates_Data, unitid　= university_id)

# 3.’unitid’に含まれる”aaaa”という文字を削除しなさい
library("stringr")
Covariates_Data <- mutate(Covariates_Data, unitid = as.character(unitid))
Covariates_Data$unitid <- str_replace_all(Covariates_Data$unitid, pattern="aaaa", replacement="")

# 4. ‘category’列に含まれる’instatetuition’, ‘costs’, ’faculty’, ’white_cohortsize’を別の列として追加しなさい(wide型に変更しなさい)
library(tidyr)
Covariates_Data <- Covariates_Data %>%
  pivot_wider(names_from = category, values_from = value)

# 5.outcomeやsemester_dummyに含まれる年を調べ、covariatesデータの期間を他のデータに揃えなさい
library(dplyr)
outcome <- unique(Graduate_Data$year)
semester_dummy <- unique(Semester_Data$year)
year_list <- as.vector(unique(outcome, semester_dummy))
Covariates_Data <- Covariates_Data %>%
  filter(year %in% year_list)
  
# 6.outcome_dataに含まれるunitidを特定し、covariatesに含まれるunitidをoutcomeデータに揃えなさい
outcome_unitid <- as.vector(unique(Graduate_Data$unitid))
Covariates_Data <- Covariates_Data %>%
  filter(unitid %in% outcome_unitid)

#(d)Master Dataの作成
# 1.結合に用いる変数を考え、semester_data, covariates_data, gradrate_dataを適切に結合しなさい
Graduate_Data <- Graduate_Data %>%
  mutate(unitid = as.double(unitid), year = as.numeric(year))
Semester_Data <- Semester_Data %>%
  mutate(unitid = as.double(unitid), year = as.numeric(year))
Covariates_Data <- Covariates_Data %>%
  mutate(unitid = as.double(unitid), year = as.numeric(year))
dup <- c("unitid", "year")
undup <- c("instnm", "semester", "quarter", "yearofsem", "after")
Master_Data <- left_join(Semester_Data, Graduate_Data, by = dup)　%>%
  left_join(Covariates_Data, by = dup)
