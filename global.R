require(openxlsx)
require(reshape2)
require(dplyr)
require(Dict)

last_update_time <- max(file.mtime(paste0("../", list.files("../"))))


##### 1)  Health Education Programs Datset #####
df_prog <- read.xlsx('./data/prog.xlsx')
df_target = read.xlsx('./data/target.xlsx')

df_prog$enrollment_number[is.na(df_prog$enrollment_number)] <- 0
df_prog$attendance_number[is.na(df_prog$attendance_number)] <- 0
df_prog$Month = as.character(format(as.Date(df_prog$sessiondate), "%m"))
df_prog$Quarter = ifelse(as.numeric(df_prog$Month) < 4, "Q4",
                          ifelse(as.numeric(df_prog$Month) < 7, "Q1",
                                 ifelse(as.numeric(df_prog$Month) < 10, "Q2", "Q3")))

month_levels = c("04","05","06","07","08","09","10","11","12","01","02","03")
choice_output_main = sort(unique(df_prog$output_main))
choice_output = sort(unique(df_prog$output))
choice_pcode = sort(unique(df_prog$subcode))


##### 2)  Health Screening Surveys Dataset
screen_pain = read.xlsx('./data/pain_screen.xlsx')
screen_sleep = read.xlsx('./data/sleep_screen.xlsx')
screen_cvd = read.xlsx('./data/cvd_screen.xlsx')

preprocess_func <- function(data) {
  
  ## define id for each date
  data$date = substr(data$create_date, 1, 10)
  date_id = as.data.frame(seq(min(as.Date(data$create_date)), max(as.Date(data$create_date)), by="days"))
  colnames(date_id) = "date"
  date_id$date = as.character(date_id$date)
  
  data = merge(data, date_id, by="date", all=T)
  
  data$year = substr(data$date, 1, 4)
  data$month = substr(data$date, 1, 7)
  
  data = data[, c(colnames(data)[!(colnames(data) %in% c("year","month","date"))], "year","month","date")]
  
  get_week <- function(d) {
    d <- as.Date(d)
    prev.days <- seq(d-6, d, by='day')
    subseq.days <- seq(d, d+6, by='day')
    wbound_low = prev.days[weekdays(prev.days)=='Monday']
    wbound_up = subseq.days[weekdays(subseq.days)=='Sunday']
    return(paste0(wbound_low, " to ", wbound_up))
  }
  data$week = sapply(data$date, get_week)
  
  return(data)
}

screen_pain = preprocess_func(screen_pain)
screen_sleep = preprocess_func(screen_sleep)
screen_cvd = preprocess_func(screen_cvd)

collector = read.xlsx('./data/collector.xlsx')
collector = subset(collector, !(is.na(SM_collector_id)))

screen_pain = merge(screen_pain, collector[,c("SM_collector_id","Description")], by.x="collector", by.y="SM_collector_id", all.x=T)
screen_sleep = merge(screen_sleep, collector[,c("SM_collector_id","Description")], by.x="collector", by.y="SM_collector_id", all.x=T)
screen_cvd = merge(screen_cvd, collector[,c("SM_collector_id","Description")], by.x="collector", by.y="SM_collector_id", all.x=T)
screen_pain$Description[is.na(screen_pain$Description)] <- "General"
screen_sleep$Description[is.na(screen_sleep$Description)] <- "General"
screen_cvd$Description[is.na(screen_cvd$Description)] <- "General"

screen_pain = screen_pain[!is.na(screen_pain$collector), ]
screen_sleep = screen_sleep[!is.na(screen_sleep$collector), ]
screen_cvd = screen_cvd[!is.na(screen_cvd$collector), ]

region_dict = list()
region_dict[['HK Island']] = c("南區","灣仔","中西區","東區")
region_dict[['KL East']] = c("觀塘","黃大仙")
region_dict[['KL West']] = c("深水埗","旺角","油麻地","尖沙咀","九龍城")
region_dict[['NT East']] = c("沙田","大圍","大埔","馬鞍山","將軍澳","西貢","北區")
region_dict[['NT West']] = c("屯門","天水圍","元朗","荃灣","葵青","離島")


##### 3)  Volunteer Work group Dataset #####
vol_1 = read.xlsx("./data/s1_data.xlsx", sheet=1)
vol_2 = read.xlsx("./data/s2_data.xlsx", sheet=1)
vol = dplyr::bind_rows(vol_1, vol_2)
vol = subset(vol, is.na(TERMINATION_DATE) | !(TERMINATION_DATE < '2020-04-01'))

school_opts = sort(unique(vol$SCHOOL))
dept_opts = sort(unique(vol$DEPARTMENT))
grade_opts = sort(unique(vol$GRADE))
yos_opts = sort(c("< 1 year", "1 year - < 3 years", "3 year - < 5 years", "5 years or above"))
term_opts = sort(unique(vol$TERM))
type_opts = sort(unique(vol$TYPE))
mon_opts = c(month.abb[4:12], month.abb[1:3])
yr_opts = c("2020-2021", "2021-2022", "2022-2023")

#### format date columns
for (c in colnames(vol)[grepl("DATE", colnames(vol)) == T | grepl("Date", colnames(vol)) == T]) {
  vol[,c] = as.Date(vol[,c], origin = "1899-12-30")
}
#### define periods
make_df_periods_start = seq(as.Date("2020-04-01"), length = 36, by = "months")
make_df_periods_end = seq(as.Date("2020-05-01"), length = 36, by = "months") - 1

yos_dat = list()
for (n in 1:length(make_df_periods_start)) {
  
  yos_time = make_df_periods_start[n] - vol$EMPLOYMENT_DATE
  yos_blank = ifelse(!is.na(vol$TERMINATION_DATE) & vol$TERMINATION_DATE < make_df_periods_start[n], 1, 0)
  yos_bounce = ifelse(vol$EMPLOYMENT_DATE >= make_df_periods_start[n] & vol$TERMINATION_DATE <= make_df_periods_end[n], 1, 0)
  yos_time = ifelse(yos_time >= 0, 
                   ifelse(yos_time < 365, "< 1 year", 
                          ifelse(yos_time < 365 * 3, "1 year - < 3 years",
                                 ifelse(yos_time < 365 * 5, "3 year - < 5 years", "5 years or above"))), NA)
  yos_time[yos_blank == 1] = NA
  yos_time[yos_bounce == 1] = "< 1 year"
  
  if (n == length(make_df_periods_start)) {
    yos_terminate = ifelse(vol$EMPLOYMENT_DATE >= make_df_periods_start[n] & is.na(vol$TERMINATION_DATE), 1, 0)
    yos_time[yos_terminate == 1] = "< 1 year"
  }
  yos_dat[[n]] = yos_time
}

yos_dat = do.call("cbind", yos_dat)
colnames(yos_dat) = paste0("As.of.", make_df_periods_start)
yos_dat = data.frame(yos_dat)


##### 4)  Engagement Dataset #####
outcomes_6 = read.xlsx("./data/outcomes_6.xlsx")
outcomes_2 = read.xlsx("./data/outcomes_2.xlsx")

o1 = outcomes_6[,colnames(outcomes_6)[grepl("community_connection_", colnames(outcomes_6)) == TRUE]]
o2 = outcomes_6[,colnames(outcomes_6)[grepl("public_support_", colnames(outcomes_6)) == TRUE]]
o3 = outcomes_6[,colnames(outcomes_6)[grepl("wellness_", colnames(outcomes_6)) == TRUE]]
o4 = outcomes_6[,colnames(outcomes_6)[grepl("recognition_", colnames(outcomes_6)) == TRUE]]
o5 = outcomes_6[,colnames(outcomes_6)[grepl("empowerment_", colnames(outcomes_6)) == TRUE]]
o6 = outcomes_6[,colnames(outcomes_6)[grepl("motivation_", colnames(outcomes_6)) == TRUE]]
o7 = outcomes_6[,colnames(outcomes_6)[grepl("social_inclusion_", colnames(outcomes_2)) == TRUE]]
o8 = outcomes_6[,colnames(outcomes_6)[grepl("community_belongingness_", colnames(outcomes_2)) == TRUE]]

olist = list(o1,o2,o3,o4,o5,o6,o7,o8)

oname = c("S1 - Community Connection",
          "S2 - Public Support",
          "S3 - Health Improvement or Alert",
          "S4 - Industry Recognition",
          "S5 - Empowerment",
          "S6 - Sustainable Motivation or Intention",
          "S7 - Social Inclusion",
          "S8 - Community Belongingness")

for (x in 1:length(olist)) {
  olist[[x]]$score = rowSums(olist[[x]])
  olist[[x]]$name = oname[x]
}
