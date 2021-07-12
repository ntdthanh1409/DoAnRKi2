setwd("C:\\Users\\PC\\Desktop\\COVID-19\\csse_covid_19_data\\csse_covid_19_daily_reports_us")
#install va update thu vien 

library('ggplot2')
library('data.table')
library('tibble')
library('tidyr')
library('dplyr')
library('readr')
library('tidyverse')
library('lubridate')
library('ggrepel')
library('forcats')
library(reshape2)

#Doc tat ca cac file vao mot dataframe
files <- list.files(pattern = ".csv")
temp <- lapply(files, fread, sep=",")
data <- rbindlist( temp, fill=TRUE)
names(data)
View(data)

noC <- data[Province_State=="North Carolina"]
Tx <- data[Province_State=="Texas"] 

noC$Case_Fatality_Ratio <- format(round(noC$Case_Fatality_Ratio, 2), nsmall = 2)
Tx$Case_Fatality_Ratio <- format(round(Tx$Case_Fatality_Ratio, 2), nsmall = 2)
#doc file du lieu ve covid cua cac bang/thanh pho
#tai My ngay 1/1/2021
df_1 <- read.csv("01-01-2021.csv", 
                 header = TRUE)
names(df_1)
View(df_1)

df_2 <- read.csv("06-01-2020.csv")
names(df_2)

#Chuyen sang so thap phan co 2 chu so sau dau phay
df_1$Case_Fatality_Ratio <- format(round(df_1$Case_Fatality_Ratio, 2), nsmall = 2)
df_1$Case_Fatality_Ratio

# Bieu do 1: 
# Outside bars

df <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv',
               header = TRUE)

viet_nam = df[275,5:ncol(df)]
head(viet_nam)

str(viet_nam)
so_ngay = ncol(viet_nam)
so_ca_mac <- c(1:ncol(viet_nam))


for (i in so_ca_mac){
  so_ca_mac[i] <- viet_nam[[i]]
}


so_ca_mac
so_ca_mac_10 = so_ca_mac[(so_ngay-9):so_ngay]
so_ca_mac_10
ngay = names(viet_nam)
ngay_10 = ngay[(so_ngay-9):so_ngay]
ngay_10

data_plot_10 = data.frame(ngay=ngay_10, so_ca_mac= so_ca_mac_10)
data_plot_10

data_plot_10$ngay <- factor(data_plot_10$ngay, levels = data_plot_10$ngay[order(data_plot_10$so_ca_mac)])
data_plot_10$ngay  # notice the changed order of factor levels

ggplot(data = data_plot_10, aes(x=ngay, y=so_ca_mac))+
  geom_bar(color ='black' ,fill = 'orange',stat="identity")+
  geom_text(aes(label=so_ca_mac), vjust=-0.3, size=3.5)+
  labs(title="BIEU DO CONG DON SO CA MAC TRONG 10 NGAY GAN NHAT CUA VIET NAM",
       x ="Ngay", y = "Tong so ca mac")
# Bieu do 2: 

ggplot(df_1[5:11,], aes(x='', y=Deaths, fill=Province_State)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() + labs(title="BIEU DO THE HIEN sO LUONG CA TU VONG BOI COVID 19 O MOT SO BANG/THANH PHO TAI MI NGAY 1-1-2021") +
  geom_text(aes(label = paste0(Deaths)), position = position_stack(vjust=0.5)) 

# Bieu do 3: 

ggplot(df_1, aes(x=Recovered, y=Province_State, fill= Recovered)) + 
  geom_point(aes(color=Recovered)) + 
  labs(title="BIEU DO THE HIEN SO LUONG CA HOI PHUC SAU KHI NHIEM COVID 19 O MOT SO BANG/THANH PHO TAI MI NGAY 1-1-2021", 
       x = "Recovered", y="Province")

#Bieu do 4: 

ggplot(df_1[5: 20,], aes(x='', y=Confirmed, fill=Province_State)) +
  geom_bar(stat="identity", width=1) +
  theme_void() +
  coord_polar("y", start=0) + 
  labs(title="BIEU DO THE HIEN SO CA NHIEM COVID 19 O MOT SO BANG/THANH PHO TAI MI NGAY 1-1-2021") +
  geom_text(aes(label = paste0(Confirmed)), position = position_stack(vjust=0.5))

# Bieu do 5: 
ggplot(df_1, aes(x=Deaths, color=Province_State)) +
  geom_col(aes(x=Deaths, y=Province_State, fill =Province_State)) + 
  theme_grey() +
  labs(title="BIEU DO THE HIEN SO LUONG CA TU VONG BOI COVID 19 O CAC BANG/THANH PHO TAI MI NGAY 1-1-2021")

# Bieu do 6: 

ggplot(df_1[10:25,], aes(x='', y=Case_Fatality_Ratio, fill=Province_State)) +
  geom_bar(stat="identity", width=1) +
  theme_void() + coord_polar("y", start=0) + 
  labs(title="BIEU DO THE HIEN TI LE CA TU VONG BOI COVID 19 O MOT SO BANG/THANH PHO TAI MI NGAY 1-1-2021")  +
  geom_text(aes(label = paste0(Case_Fatality_Ratio)), position = position_stack(vjust=0.5))

# Bieu do 7: 

ggplot(df_1[1:10,], aes(x='', y=Recovered, fill=Province_State)) +
  geom_bar(stat="identity", width=1) +
  theme_gray() +
  coord_polar("y", start=0) + 
  labs(title="BIEU DO THE HIEN SO LUONG CA HOI PHUC SAU KHI NHIEM COVID 19 O MOT SO BANG/THANH PHO TAI MI NGAY 1-1-2021") +
  geom_text(aes(label = paste0(Recovered)), position = position_stack(vjust=0.5)) 

# Bieu do 8: 
ggplot(noC, aes(x=Confirmed, y=Last_Update, fill = Confirmed)) + 
  geom_point(aes(colour = Confirmed)) +
  labs(title="BIEU DO THE HIEN SO CA NHIEM COVID 19 O North Carolina
                                    TU 04/2020 DEN 07/2021", x = "Confirmed", y="Last Update")

df_ghi_nhan <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv',
                        header = TRUE)
df_tu_vong <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv',
                       header = TRUE)
df_phuc_hoi <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv',
                        header = TRUE)

# xu ly du lieu so ca ghi nhan
viet_nam_ghi_nhan = df_ghi_nhan[275,5:ncol(df_ghi_nhan)]
head(viet_nam_ghi_nhan)
str(viet_nam_ghi_nhan)

so_ngay = ncol(viet_nam_ghi_nhan)
so_ca_mac <- c(2:ncol(viet_nam_ghi_nhan))

for (i in so_ca_mac){
  so_ca_mac[i] <- viet_nam_ghi_nhan[[i]]- viet_nam_ghi_nhan[[i-1]]
}

so_ca_mac
so_ca_mac_10 = so_ca_mac[(so_ngay-9):so_ngay]
so_ca_mac_10
ngay = names(viet_nam_ghi_nhan)
ngay_10 = ngay[(so_ngay-9):so_ngay]
ngay_10

data_plot_10 = data.frame(ngay=ngay_10, so_ca_mac= so_ca_mac_10)
data_plot_10

# Xu ly du lieu so ca tu vong
viet_nam_tu_vong = df_tu_vong[275,5:ncol(df_tu_vong)]
viet_nam_tu_vong

so_ca_tu_vong <- c(2:ncol(viet_nam_tu_vong))

for (i in so_ca_tu_vong){
  so_ca_tu_vong[i] <- viet_nam_tu_vong[[i]]- viet_nam_tu_vong[[i-1]]
}

so_ca_tu_vong_10 = so_ca_tu_vong[(so_ngay-9):so_ngay]
so_ca_tu_vong_10

data_plot_10_tu_vong = data.frame(ngay=ngay_10, so_ca_tu_vong= so_ca_tu_vong_10)
data_plot_10_tu_vong

# Xy ly du lieu so ca phuc hoi
viet_nam_phuc_hoi = df_phuc_hoi[260,5:ncol(df_phuc_hoi)]
viet_nam_phuc_hoi

so_ca_phuc_hoi <- c(2:ncol(viet_nam_phuc_hoi))

for (i in so_ca_phuc_hoi){
  so_ca_phuc_hoi[i] <- viet_nam_phuc_hoi[[i]]- viet_nam_phuc_hoi[[i-1]]
}

so_ca_phuc_hoi_10 = so_ca_phuc_hoi[(so_ngay-9):so_ngay]
so_ca_phuc_hoi_10

data_plot_10 = data.frame(ngay=ngay_10, so_ca_mac = so_ca_mac_10,
                          so_ca_tu_vong = so_ca_tu_vong_10,
                          so_ca_phuc_hoi = so_ca_phuc_hoi_10)
data_plot_10
data_plot <- as_tibble(data_plot_10)
Bieu do 9:
df_long <- data_plot %>%
  pivot_longer(
    so_ca_mac:so_ca_phuc_hoi,
    names_to = "variable", values_to = "value"
  )

df_long
ggplot(data=df_long, aes(x=ngay, y=value, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=value), vjust=-0.3, color="black",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()+
  labs(title="BIEU DO COT THE HIEN SO CA MAC, SO CA TU VONG, SO CA PHUC HOI\nTRONG 10 NGAY GAN NHAT",
       x ="So Luong", y = "Ngay")+
  theme(legend.position = "top", axis.text.x = element_text(angle = 45))

# Bieu do 10: bieu do non

df_2<-df_2[1:11,c('Province_State','Confirmed')]
df_2<-df_2%>%mutate(percent=Confirmed*100/sum(Confirmed))
df_2$percent <-  format(round(df_2$percent, 2), nsmall = 2)

ggplot(df_2, aes(x='', y=Confirmed, fill=Province_State)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() + labs(title="BIEU DO THE HIEN TI LE SO CA NHIEM COVID 19 O MOT SO BANG/THANH PHO TAI MI NGAY 6-1-2021") +
  geom_text(aes(label = paste0(percent)), position = position_stack(vjust=0.5))

# Bieu do 11: bieu do cot

ggplot(df_2[5:11,], aes(x=Deaths, color=Province_State)) +
  geom_col(aes(x=Deaths, y=Province_State, fill =Province_State)) + 
  theme_grey() +
  labs(title="BIEU DO THE HIEN SO LUONG CA TU VONG DO COVID 19 O BANG/THANH PHO TAI MI 6-1-2020")

# Bieu do 12: bieu do diem

ggplot(df_2, aes(x=Recovered, y=Province_State, fill= Recovered)) + 
  geom_point(aes(color=Recovered)) + 
  labs(title="BIEU DO THE HIEN SO LUONG CA HOI PHUC SAU KHI NHIEM COVID 19 O MOT SO BANG/THANH PHO TAI MI NGAY 6-1-2021", 
       x = "Recovered", y="Province")
# bieu do 13:
df <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv',
               header = TRUE)

viet_nam = df[275,5:ncol(df)]
head(viet_nam)

str(viet_nam)
so_ngay = ncol(viet_nam)
so_ca_tu_vong <- c(1:ncol(viet_nam))


for (i in so_ca_tu_vong){
  so_ca_tu_vong[i] <- viet_nam[[i]]
}


so_ca_tu_vong
so_ca_tu_vong_10 = so_ca_tu_vong[(so_ngay-9):so_ngay]
so_ca_tu_vong_10
ngay = names(viet_nam)
ngay_10 = ngay[(so_ngay-9):so_ngay]
ngay_10

data_plot = data.frame(ngay=ngay, so_ca_tu_vong= so_ca_tu_vong)
data_plot

data_plot$ngay <- factor(data_plot$ngay, levels = ngay)
data_plot$ngay  # notice the changed order of factor levels

plot5 <- ggplot(data_plot, aes(ngay, so_ca_tu_vong, group = 1)) +
  geom_line(color="red", size=1.2) +
  labs(x = "Ngay", y = "So ca mac", 
       title = "BIEU DO THE HIEN XU HUONG SO CA TU VONG COVID 19 CUA VIET NAM")
plot5
# Bieu do 14:
df_ghi_nhan <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv',
                        header = TRUE)
country = df_ghi_nhan$Country.Region
df_ghi_nhan = df_ghi_nhan[,5:ncol(df_ghi_nhan)]
data = data.frame(country,df_ghi_nhan)


data <- data.table(data)  # DF is your original data
data = data[, lapply(.SD, sum), by=country]
data
name = names(data)

n = length(name)
data = data.frame(data)
data[,n]

data_sort <-data[order(data[,n],decreasing = TRUE),]

data_plot = data.frame(country = data_sort[,1],ghi_nhan = data_sort[,ncol(data_sort)])
data_plot_5 = data_plot[1:5,]
data_plot_5

data_plot_5$country <- factor(data_plot_5$country, levels = data_plot_5$country)
data_plot_5$country  # notice the changed order of factor levels

ggplot(data = data_plot_5, aes(x=country, y= ghi_nhan))+
  geom_bar(color ='black' ,fill = 'orange',stat="identity")+
  geom_text(aes(label=ghi_nhan), vjust=-0.3, size=3.5)+
  labs(title="BIEU DO SO CA MAC TRONG NGAY GAN NHAT CUA CAC NUOC DAN DAU",
       x ="Country", y = "So ca mac moi")
# Bieu do 15: 
ggplot(noC, aes(y=Case_Fatality_Ratio, x=Last_Update, fill= Case_Fatality_Ratio)) + 
  geom_point(aes(color=Case_Fatality_Ratio)) + 
  labs(title="BIEU DO THE HIEN TI LE TU VONG DO COVID 19 O NORTH CAROLINA
                                    TU 04/2020 DEN 07/2021", x="Last Update", y="Case Fatality Ratio")
# Bieu do 16: Bieu do duong
ggplot(Tx) + 
  geom_line(aes(y=Case_Fatality_Ratio, x=Last_Update, color=Case_Fatality_Ratio)) +
  labs(title="Bi???u d??? th??? hi???n t??? l??? t??? vong gây ra b???i covid du???c xác nh???n t???i Alaska - M???
                                  t??? 04/2020 d???n 07/2021", x="Last Update", y="Case Fatality Ratio")

# Bieu do 17

theme_set(theme_minimal())

covid19_raw <- read_csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

covid19 <- covid19_raw %>%
  pivot_longer(-c(`Province/State`, `Country/Region`, Lat, Long),
               names_to = "date",
               values_to = "confirmed_n"
  ) %>%
  select(-c(Lat, Long)) %>%
  rename(
    province_state = `Province/State`,
    country_region = `Country/Region`
  ) %>%
  mutate(date = mdy(date)) %>%
  group_by(country_region, date) %>%
  summarise(confirmed_n = sum(confirmed_n)) %>%
  ungroup()

covid19 <- covid19 %>%
  arrange(date) %>%
  group_by(country_region) %>%
  mutate(new_cases_n = confirmed_n - lag(confirmed_n, default = 0)) %>%
  ungroup()

covid_global_cases %>%
  filter(country_region == "US") %>%
  ggplot(aes(x = date, y = confirmed_cases_n)) +
  geom_line() +
  scale_x_date(date_breaks = "2 week", date_labels = "%d %b") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Date", y = "Confirmed cases (n)", title = "BIEU DO VE COVID O MI NAM 2020")
 
# Bieu do 18: 

covid_global_cases %>%
  filter(country_region %in% c("US", "Australia")) %>%
  ggplot(aes(x = date, y = confirmed_cases_n, color = country_region)) +
  geom_line(show.legend = FALSE) +
  scale_x_date(date_breaks = "2 week", date_labels = "%d %b") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Date", y = "Confirmed cases (n)", title = "Number of confirmed COVID-19 cases in 2020") +
  facet_wrap(~country_region, scales = "free", ncol = 1)

# Bieu do 19: 

covid19 %>%
  filter(country_region %in% c("US","Japan","Australia","Vietnam")) %>%
  ggplot(aes(x = date, y = new_cases_n, color = country_region)) +
  geom_line(show.legend = FALSE) +
  scale_x_date(date_breaks = "6 weeks", date_labels = "%d %b") +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~country_region, ncol = 1, scales = "free_y") +
  labs(
    x = "Date", y = "New cases",
    title = "NHUNG CA MAC COVID MOI O Australia & United States & VietNam & Japan"
  )

covid19_raw <- covid19_raw[,1:150]


# Bieu do 20: 

corona_deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
data_deaths <- corona_deaths %>%
  filter(`Country/Region` == "Germany" | `Country/Region` == "Italy" |  `Country/Region` == "China"  |  `Country/Region` == "US" | `Country/Region` == "France" | `Country/Region` == "New Zealand") %>%
  filter(`Province/State` !="Diamond Princess" | is.na(`Province/State`)) %>%
  select(-`Province/State`) %>%
  select(-Lat,-Long) %>%
  group_by(`Country/Region`) %>%
  summarise_each(list(sum))

n <- data_deaths$`Country/Region`
data_deaths <- as.data.frame(t(data_deaths[,-1]))
colnames(data_deaths) <- n
data_deaths <- tibble::rownames_to_column(data_deaths, "Day")
data_deaths <- data_deaths %>%
  mutate(Day = as.Date(Day,"%m/%d/%y"))

d_deaths <- melt(data_deaths, id.vars="Day")

# plot
ggplot(d_deaths, aes(Day,value, col=variable)) + 
  geom_line() +
  ggtitle("TI LE TU VONG DO COVID 19 O MOT SO NUOC") + 
  xlab("Date") + 
  ylab("Deaths")