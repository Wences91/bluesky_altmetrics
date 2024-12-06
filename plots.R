library(ggplot2)
library(tidyr)
library(dplyr)

# Nature
df_nature_rec <- read.csv('data/top.csv')
df_nature_rec <- df_nature_rec[which(df_nature_rec$primary_location.source.display_name=='Nature'),]
df_nature_rec$publication_date <- as.Date(paste0(substr(df_nature_rec$publication_date,1,8),'01'), format='%Y-%m-%d')
table(df_nature_rec$publication_date)

df_nature_men <- read.delim2('results/nature.tsv')
length(which(df_nature_men$x_mentions>0))
mean(df_nature_men[which(df_nature_men$x_mentions>0),'x_mentions'])
length(which(df_nature_men$bsky_mentions>0))
  
df_nature <- read.delim2('results/nature_bsky.tsv')
df_nature$indexedAt <- as.Date(paste0(substr(df_nature$indexedAt,1, 8),'01'), format='%Y-%m-%d')
length(unique(df_nature$doi))

unique(df_nature[,c('author_did', 'doi')]) %>%
  group_by(doi) %>%
  summarise(accounts = n()) %>%
  group_by() %>%
  summarise(accounts = mean(accounts))

ggplot()+
  geom_bar(data=df_nature, aes(x=indexedAt), fill='#6C757D')+
  geom_line(data=df_nature_rec, aes(x = publication_date, y = ..count..*10), stat = 'count', size = 1) +
  scale_y_continuous(
    name = 'Bluesky mentions',
    sec.axis = sec_axis(~./10, name = 'Papers published'))+
  scale_x_date(breaks = seq(as.Date('2024-01-01'), as.Date('2024-11-15'), by = "2 months"),
               date_labels = '%Y\n%b',
               limits = as.Date(c('2023-12-16','2024-11-15'), format='%Y-%m-%d'))+
  labs(x='Mention date', y='Bluesky mentions')+
  theme_light()+
  theme(legend.position = 'none',
        strip.background = element_rect(fill = 'black'),
        strip.text = element_text(face='bold', size=14),
        axis.title = element_text(size=16),
        axis.text = element_text(color='black', size=12))+
  facet_wrap(.~'Nature')


# Science
df_science_rec <- read.csv('data/top.csv')
df_science_rec <- df_nature_rec[which(df_science_rec$primary_location.source.display_name=='Science'),]
df_science_rec$publication_date <- as.Date(paste0(substr(df_science_rec$publication_date,1,8),'01'), format='%Y-%m-%d')
table(df_science_rec$publication_date)

df_science_men <- read.delim2('results/science.tsv')
length(which(df_science_men$x_mentions>0))
mean(df_science_men[which(df_science_men$x_mentions>0),'x_mentions'])
length(which(df_science_men$bsky_mentions>0))

df_science <- read.delim2('results/science_bsky.tsv')
df_science$indexedAt <- as.Date(paste0(substr(df_science$indexedAt,1, 8),'01'), format='%Y-%m-%d')
length(unique(df_science$doi))

unique(df_science[,c('author_did', 'doi')]) %>%
  group_by(doi) %>%
  summarise(accounts = n()) %>%
  group_by() %>%
  summarise(accounts = mean(accounts))

ggplot()+
  geom_bar(data=df_science, aes(x=indexedAt), fill='#D71920')+
  geom_line(data=df_science_rec, aes(x = publication_date, y = ..count..*50), stat = 'count', size = 1) +
  scale_y_continuous(
    name = 'Bluesky mentions',
    sec.axis = sec_axis(~./50, name = 'Papers published'))+
  scale_x_date(breaks = seq(as.Date('2024-01-01'), as.Date('2024-11-15'), by = "2 months"),
               date_labels = '%Y\n%b',
               limits = as.Date(c('2023-12-16','2024-11-15'), format='%Y-%m-%d'))+
  labs(x='Mention date', y='Bluesky mentions')+
  theme_light()+
  theme(legend.position = 'none',
        strip.background = element_rect(fill = 'black'),
        strip.text = element_text(face='bold', size=14),
        axis.title = element_text(size=16),
        axis.text = element_text(color='black', size=12))+
  facet_wrap(.~'Science')



# PNAS
df_pnas_rec <- read.csv('data/top.csv')
df_pnas_rec <- df_pnas_rec[which(df_pnas_rec$primary_location.source.display_name=='Proceedings of the National Academy of Sciences'),]
df_pnas_rec$publication_date <- as.Date(paste0(substr(df_pnas_rec$publication_date,1,8),'01'), format='%Y-%m-%d')
table(df_pnas_rec$publication_date)

df_pnas_men <- read.delim2('results/pnas.tsv')
length(which(df_pnas_men$x_mentions>0))
mean(df_pnas_men[which(df_pnas_men$x_mentions>0),'x_mentions'])
length(which(df_pnas_men$bsky_mentions>0))

df_pnas <- read.delim2('results/pnas_bsky.tsv')
df_pnas$indexedAt <- as.Date(paste0(substr(df_pnas$indexedAt,1, 8),'01'), format='%Y-%m-%d')
length(unique(df_pnas$doi))

unique(df_pnas[,c('author_did', 'doi')]) %>%
  group_by(doi) %>%
  summarise(accounts = n()) %>%
  group_by() %>%
  summarise(accounts = mean(accounts))

ggplot()+
  geom_bar(data=df_pnas, aes(x=indexedAt), fill='#1f75b9')+
  geom_line(data=df_pnas_rec, aes(x = publication_date, y = ..count..*1), stat = 'count', size = 1) +
  scale_y_continuous(
    name = 'Bluesky mentions',
    sec.axis = sec_axis(~./10, name = 'Papers published'))+
  scale_x_date(breaks = seq(as.Date('2024-01-01'), as.Date('2024-11-15'), by = "2 months"),
               date_labels = '%Y\n%b',
               limits = as.Date(c('2023-12-16','2024-11-15'), format='%Y-%m-%d'))+
  labs(x='Mention date', y='Bluesky mentions')+
  theme_light()+
  theme(legend.position = 'none',
        strip.background = element_rect(fill = 'black'),
        strip.text = element_text(face='bold', size=14),
        axis.title = element_text(size=16),
        axis.text = element_text(color='black', size=12))+
  facet_wrap(.~'PNAS')


# JASIST
df_jasist_rec <- read.csv('data/jasist.csv')
df_jasist_rec$publication_date <- as.Date(paste0(substr(df_jasist_rec$publication_date,1,8),'01'), format='%Y-%m-%d')
table(df_jasist_rec$publication_date)

df_jasist_men <- read.delim2('results/jasist.tsv')
length(which(df_jasist_men$x_mentions>0))
mean(df_jasist_men[which(df_jasist_men$x_mentions>0),'x_mentions'])
length(which(df_jasist_men$bsky_mentions>0))

df_jasist <- read.delim2('results/jasist_bsky.tsv')
df_jasist$indexedAt <- as.Date(paste0(substr(df_jasist$indexedAt,1, 8),'01'), format='%Y-%m-%d')
length(unique(df_jasist$doi))

unique(df_jasist[,c('author_did', 'doi')]) %>%
  group_by(doi) %>%
  summarise(accounts = n()) %>%
  group_by() %>%
  summarise(accounts = mean(accounts))

ggplot()+
  geom_bar(data=df_jasist, aes(x=indexedAt), fill='#cc0000')+
  geom_line(data=df_jasist_rec, aes(x = publication_date, y = ..count..*1), stat = 'count', size = 1) +
  scale_y_continuous(
    name = 'Bluesky mentions',
    sec.axis = sec_axis(~./1, name = 'Papers published'))+
  scale_x_date(breaks = seq(as.Date('2024-01-01'), as.Date('2024-11-15'), by = "2 months"),
               date_labels = '%Y\n%b',
               limits = as.Date(c('2023-12-16','2024-11-15'), format='%Y-%m-%d'))+
  labs(x='Mention date', y='Bluesky mentions')+
  theme_light()+
  theme(legend.position = 'none',
        strip.background = element_rect(fill = 'black'),
        strip.text = element_text(face='bold', size=14),
        axis.title = element_text(size=16),
        axis.text = element_text(color='black', size=12))+
  facet_wrap(.~'JASIST')



# JOI
df_joi_rec <- read.csv('data/joi.csv')
df_joi_rec$publication_date <- as.Date(paste0(substr(df_joi_rec$publication_date,1,8),'01'), format='%Y-%m-%d')
table(df_joi_rec$publication_date)

df_joi_men <- read.delim2('results/joi.tsv')
length(which(df_joi_men$x_mentions>0))
mean(df_joi_men[which(df_joi_men$x_mentions>0),'x_mentions'])
length(which(df_joi_men$bsky_mentions>0))

df_joi <- read.delim2('results/joi_bsky.tsv')
df_joi$indexedAt <- as.Date(paste0(substr(df_joi$indexedAt,1, 8),'01'), format='%Y-%m-%d')
length(unique(df_joi$doi))

unique(df_joi[,c('author_did', 'doi')]) %>%
  group_by(doi) %>%
  summarise(accounts = n()) %>%
  group_by() %>%
  summarise(accounts = mean(accounts))

ggplot()+
  geom_bar(data=df_joi, aes(x=indexedAt), fill='#227bc0')+
  geom_line(data=df_joi_rec, aes(x = publication_date, y = ..count../3), stat = 'count', size = 1) +
  scale_y_continuous(
    name = 'Bluesky mentions',
    sec.axis = sec_axis(~.*3, name = 'Papers published'))+
  scale_x_date(breaks = seq(as.Date('2024-01-01'), as.Date('2024-11-15'), by = "2 months"),
               date_labels = '%Y\n%b',
               limits = as.Date(c('2023-12-16','2024-11-15'), format='%Y-%m-%d'))+
  labs(x='Mention date', y='Bluesky mentions')+
  theme_light()+
  theme(legend.position = 'none',
        strip.background = element_rect(fill = 'black'),
        strip.text = element_text(face='bold', size=14),
        axis.title = element_text(size=16),
        axis.text = element_text(color='black', size=12))+
  facet_wrap(.~'Journal of Informetrics')



# Scientometrics
df_scientometrics_rec <- read.csv('data/scientometrics.csv')
df_scientometrics_rec$publication_date <- as.Date(paste0(substr(df_scientometrics_rec$publication_date,1,8),'01'), format='%Y-%m-%d')
table(df_scientometrics_rec$publication_date)

df_scientometrics_men <- read.delim2('results/scientometrics.tsv')
length(which(df_scientometrics_men$x_mentions>0))
mean(df_scientometrics_men[which(df_scientometrics_men$x_mentions>0),'x_mentions'])
length(which(df_scientometrics_men$bsky_mentions>0))

df_scientometrics <- read.delim2('results/scientometrics_bsky.tsv')
df_scientometrics$indexedAt <- as.Date(paste0(substr(df_scientometrics$indexedAt,1, 8),'01'), format='%Y-%m-%d')
length(unique(df_scientometrics$doi))

unique(df_scientometrics[,c('author_did', 'doi')]) %>%
  group_by(doi) %>%
  summarise(accounts = n()) %>%
  group_by() %>%
  summarise(accounts = mean(accounts))

ggplot()+
  geom_bar(data=df_scientometrics, aes(x=indexedAt), fill='#bb9237')+
  geom_line(data=df_scientometrics_rec, aes(x = publication_date, y = ..count..*0.5), stat = 'count', size = 1) +
  scale_y_continuous(
    name = 'Bluesky mentions',
    sec.axis = sec_axis(~./0.5, name = 'Papers published'))+
  scale_x_date(breaks = seq(as.Date('2024-01-01'), as.Date('2024-11-15'), by = "2 months"),
               date_labels = '%Y\n%b',
               limits = as.Date(c('2023-12-16','2024-11-15'), format='%Y-%m-%d'))+
  labs(x='Mention date', y='Bluesky mentions')+
  theme_light()+
  theme(legend.position = 'none',
        strip.background = element_rect(fill = 'black'),
        strip.text = element_text(face='bold', size=14),
        axis.title = element_text(size=16),
        axis.text = element_text(color='black', size=12))+
  facet_wrap(.~'Scientometrics')



# QSS
df_qss_rec <- read.csv('data/qss.csv')
df_qss_rec$publication_date <- as.Date(paste0(substr(df_qss_rec$publication_date,1,8),'01'), format='%Y-%m-%d')
table(df_qss_rec$publication_date)

df_qss_men <- read.delim2('results/qss.tsv')
length(which(df_qss_men$x_mentions>0))
mean(df_qss_men[which(df_qss_men$x_mentions>0),'x_mentions'])
length(which(df_qss_men$bsky_mentions>0))

df_qss <- read.delim2('results/qss_bsky.tsv')
df_qss$indexedAt <- as.Date(paste0(substr(df_qss$indexedAt,1, 8),'01'), format='%Y-%m-%d')
length(unique(df_qss$doi))

unique(df_qss[,c('author_did', 'doi')]) %>%
  group_by(doi) %>%
  summarise(accounts = n()) %>%
  group_by() %>%
  summarise(accounts = mean(accounts))

ggplot()+
  geom_bar(data=df_qss, aes(x=indexedAt), fill='grey70')+
  geom_line(data=df_qss_rec, aes(x = publication_date, y = ..count..*1), stat = 'count', size = 1) +
  scale_y_continuous(
    name = 'Bluesky mentions',
    sec.axis = sec_axis(~./1, name = 'Papers published'))+
  scale_x_date(breaks = seq(as.Date('2024-01-01'), as.Date('2024-11-15'), by = "2 months"),
               date_labels = '%Y\n%b',
               limits = as.Date(c('2023-12-16','2024-11-15'), format='%Y-%m-%d'))+
  labs(x='Mention date', y='Bluesky mentions')+
  theme_light()+
  theme(legend.position = 'none',
        strip.background = element_rect(fill = 'black'),
        strip.text = element_text(face='bold', size=14),
        axis.title = element_text(size=16),
        axis.text = element_text(color='black', size=12))+
  facet_wrap(.~'QSS')


ggplot()+
  geom_bar(data=df_qss, aes(x=indexedAt), fill='grey70')+
  geom_line(data=df_qss_rec, aes(x = publication_date, y = ..count..*1, color='Papers published'), stat = 'count', size = 1) +
  scale_color_manual(values = 'black')+
  scale_y_continuous(
    name = 'Bluesky mentions',
    sec.axis = sec_axis(~./1, name = 'Papers published'))+
  scale_x_date(breaks = seq(as.Date('2024-01-01'), as.Date('2024-11-15'), by = "2 months"),
               date_labels = '%Y\n%b',
               limits = as.Date(c('2023-12-16','2024-11-15'), format='%Y-%m-%d'))+
  labs(x='Mention date', y='Bluesky mentions')+
  theme_light()+
  theme(legend.position = 'bottom',
        legend.text = element_text(size=12),
        strip.background = element_rect(fill = 'black'),
        strip.text = element_text(face='bold', size=14),
        axis.title = element_text(size=16),
        axis.text = element_text(color='black', size=12))+
  facet_wrap(.~'QSS')



# Total

df <- rbind.data.frame(df_nature, df_science, stringsAsFactors = FALSE)
df <- rbind.data.frame(df, df_pnas, stringsAsFactors = FALSE)
df <- rbind.data.frame(df, df_jasist, stringsAsFactors = FALSE)
df <- rbind.data.frame(df, df_joi, stringsAsFactors = FALSE)
df <- rbind.data.frame(df, df_scientometrics, stringsAsFactors = FALSE)
df <- rbind.data.frame(df, df_qss, stringsAsFactors = FALSE)

unique(df[,c('author_did', 'doi')]) %>%
  group_by(doi) %>%
  summarise(accounts = n()) %>%
  group_by() %>%
  summarise(accounts = mean(accounts))



df_nature_men$source <- 'Nature'
df_science_men$source <- 'Science'
df_pnas_men$source <- 'PNAS'
df_jasist_men$source <- 'JASIST'
df_joi_men$source <- 'Journal of Informetrics'
df_scientometrics_men$source <- 'Scientometrics'
df_qss_men$source <- 'QSS'

df_men <- rbind.data.frame(df_nature_men[,c('id','doi','x_mentions','bsky_mentions','source')], df_science_men[,c('id','doi','x_mentions','bsky_mentions','source')], stringsAsFactors = FALSE)
df_men <- rbind.data.frame(df_men, df_pnas_men[,c('id','doi','x_mentions','bsky_mentions','source')], stringsAsFactors = FALSE)
df_men <- rbind.data.frame(df_men, df_jasist_men[,c('id','doi','x_mentions','bsky_mentions','source')], stringsAsFactors = FALSE)
df_men <- rbind.data.frame(df_men, df_joi_men[,c('id','doi','x_mentions','bsky_mentions','source')], stringsAsFactors = FALSE)
df_men <- rbind.data.frame(df_men, df_scientometrics_men[,c('id','doi','x_mentions','bsky_mentions','source')], stringsAsFactors = FALSE)
df_men <- rbind.data.frame(df_men, df_qss_men[,c('id','doi','x_mentions','bsky_mentions','source')], stringsAsFactors = FALSE)

length(which(df_men$x_mentions>0))
mean(df_men[which(df_men$x_mentions>0),'x_mentions'])
length(which(df_men$bsky_mentions>0))

df_men_plot <- df_men[,c('id','doi','x_mentions','source')]

df_men_plot <- df_men_plot %>%
  left_join(unique(df[,c('author_did', 'doi')]) %>%
  group_by(doi) %>%
  summarise(bsky_mentions = n()), by='doi') %>%
  replace_na(list(bsky_mentions = 0))

df_men_plot <- df_men_plot[which(df_men_plot$doi %in% df$doi[which(df$indexedAt==as.Date('2024-11-01'))]),]

df_men_plot <- df_men_plot %>%
  group_by(source) %>%
  summarise(x_mentions = sum(x_mentions, na.rm = TRUE), bsky_mentions = sum(bsky_mentions, na.rm = TRUE)) %>%
  mutate(total = x_mentions + bsky_mentions) %>%
  mutate(
    x_mentions_pct = x_mentions / total * 100,
    bsky_mentions_pct = bsky_mentions / total * 100
  ) %>%
  select(source, x_mentions_pct, bsky_mentions_pct)

df_men_plot <- pivot_longer(
  df_men_plot,
  cols = c("x_mentions_pct", "bsky_mentions_pct"),
  names_to = "platform",
  values_to = "percentage"
)

df_men_plot$source <- factor(df_men_plot$source, levels = c('QSS', 'JASIST', 'Journal of Informetrics', 'Scientometrics',
                                                            'Science', 'PNAS', 'Nature'), ordered = TRUE)

ggplot(df_men_plot, aes(x = percentage, y = source, fill = platform)) +
  geom_col() +
  geom_text(data=df_men_plot[which(df_men_plot$percentage>5),], aes(x = percentage, y = source, label=paste0(round(percentage),'%')), position = position_stack(vjust = 0.5), color='white')+
  scale_x_continuous(labels = scales::percent_format(scale = 1, accuracy = 1)) +
  scale_fill_manual(
    values = c("x_mentions_pct" = "black", "bsky_mentions_pct" = "#00ACED"),
    labels = c("x_mentions_pct" = "X", "bsky_mentions_pct" = "Bluesky")
  )+
  labs(
    x = "Percentage of users mentioning the papers",
    y = "Journal",
    fill = "Social media"
  ) +
  theme_minimal()+
  theme(legend.position = 'bottom',
        legend.text = element_text(size=14),
        legend.title = element_text(face='bold', size=14),
        strip.text = element_text(face='bold', size=14),
        axis.title = element_text(size=16),
        axis.text = element_text(color='black', size=12))
