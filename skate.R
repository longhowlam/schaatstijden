#### libraries #####
library(dplyr)
library(readr)
library(stringr)
library(anytime)
library(lubridate)
library(ggplot2)

options(tibble.width = Inf) 

#### data import and prep #####
skater_data <- read_csv(
  "skater_data_full.csv",
  col_types = cols(time = col_character())
)

skater_data = skater_data %>% 
  mutate(
    gender     = str_sub(MN,1,1),
    event_date = dmy(str_replace(date_of_birth, "-\\d+", "")),
    Distance   = str_extract(category, "\\d+[m]"),
    minuten    = str_extract(time, "\\d+\\.") %>% str_replace("\\.",""), 
    minuten    = ifelse(is.na(minuten), 0, as.numeric(minuten) ),
    seconden   = str_extract(time, "\\d+\\,") %>% str_replace("\\,","") %>% as.numeric(),
    msecs      = str_extract(time, "\\,\\d+") %>% str_replace("\\,","") %>% as.numeric(),
    tijd_sec   = 60*minuten + seconden + msecs/100 ,
    DoB = dmy( paste(
        "1",
        str_sub(age, 1,3),
        str_extract(age, "\\d{4}")
      )
    ),
    Age        = interval(DoB, event_date)  %/% years(1)
  ) %>%
  filter(
    gender %in% c("M", "L"),
    tijd_sec <1250,
    Age < 75
  ) %>%
  select(
    fullname, gender, country,DoB,
    event, event_date, Distance,
    PR, SB, WR,
    Age,
    tijd_sec
  )
  
#### Some insights ####################################################

skater_data %>% filter(Distance == "1000m", tijd_sec< 300) %>%
ggplot(aes(tijd_sec)) + geom_histogram(col="black") + facet_wrap(~gender)

skater_data %>% filter(Distance == "10000m", tijd_sec < 1500) %>%
  ggplot(aes(tijd_sec)) + geom_histogram(col="black") + facet_wrap(~gender)

#### 10 KM met Sven ####
P = skater_data %>% 
  filter(Distance == "10000m", tijd_sec > 750, tijd_sec< 2000, gender == "M") %>%
  ggplot(aes(Age, tijd_sec)) + geom_point(alpha = 0.1) + 
  geom_smooth() +
  scale_x_continuous(breaks = 10*(1:7)) +
  scale_y_continuous(breaks = 60*(11:20), limits = c(750,1200))

sven = skater_data %>% 
  filter(
    str_detect(fullname, "Sven Kramer"),
    Distance == "10000m"
    )

P + 
  geom_point(data = sven , aes(Age, tijd_sec), col="Red") +
  geom_smooth(data = sven , aes(Age, tijd_sec), col="Red", size=3)


skater_data %>% group_by(gender) %>% summarise(n=n()) %>% arrange(desc(n))


###### Per land ################################################################

N=4
Top = skater_data %>%
  group_by(country) %>% 
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  slice(1:N)

skater_data %>% inner_join(Top) %>%
  filter(Distance == "10000m", tijd_sec > 750, tijd_sec< 2000, gender == "M") %>%
  ggplot(aes(Age, tijd_sec)) + geom_point(alpha = 0.1) + 
  geom_smooth() +
  facet_wrap(~country)

skater_data %>% inner_join(Top) %>%
  filter(
    Distance == "10000m",
    tijd_sec > 750,
    tijd_sec< 2000, gender == "M",
    Age < 40
    ) %>%
  ggplot(aes(Age, tijd_sec)) + geom_point(alpha = 0.1) + 
  geom_smooth(aes(col=country) ,se=FALSE, size=3)


##########  events landen ####################################################
tmp = skater_data %>% 
  group_by(event) %>% 
  summarise(
    n = n(),
    ncountry = n_distinct(country)
    ) 
hist(tmp$ncountry, nclass = 100)


skater_data2 = skater_data %>% 
  left_join(tmp) %>%
  mutate(
    NLanden = cut(ncountry,breaks=mb)
  )

mb = unique(quantile(skater_data2$ncountry, probs=0.2*(0:5)))


pp = skater_data2 %>% 
  filter(Distance == "10000m", tijd_sec > 750, tijd_sec< 2000, gender == "M") %>%
  ggplot(aes(Age, tijd_sec)) + geom_point(alpha = 0.1) + 
  geom_smooth() +
  scale_x_continuous(breaks = 10*(1:7)) +
  scale_y_continuous(breaks = 60*(11:20), limits = c(750,1200)) +
  facet_wrap(~NLanden ,ncol=3)
pp
table(skater_data2$NLanden)
hist(skater_data2$ncountry)
