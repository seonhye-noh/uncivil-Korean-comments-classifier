# This is replication code for Kernell and Noh's "The AI Referee" paper. 
library(tidyverse)
library(openxlsx)
library(patchwork)
library(tsibble)

#############################################################################
#DATA PREPARATION# (commented out to make it run faster)
CB1full <- read_csv("cb1_for_share.csv")
CB2full <- read_csv("cb2_for_share.csv")

# Use only data two months pre and post CB
firstDate1 <- min(CB1full$date,na.rm=TRUE)
lastDate1 <- max(CB1full$date,na.rm=TRUE)
firstDate2 <- min(CB2full$date,na.rm=TRUE)
lastDate2 <- max(CB2full$date,na.rm=TRUE)
CB1 <- CB1full[which(abs(CB1full$date2-CB1full$dateCB)<=60),]
CB2 <- CB2full[which(abs(CB2full$date2-CB2full$dateCB)<=60),]
# Indicators for pre or post (=TRUE or FALSE)
CB1$pre <-(CB1$date2<CB1$dateCB)
CB1$post <- (CB1$date2>CB1$dateCB)
CB2$pre <- (CB2$date2<CB2$dateCB)
CB2$post <- (CB2$date2>CB2$dateCB)

save(CB1,CB2,file='CBallMonths.RData')
rm(list=ls())

load("CBallMonths.RData")
#############################################################################
#ANALYSES#

# Figure 1 ----------------------------------------------------------------
aggregator <- read.xlsx("NewsConsumption.xlsx", sheet = "SearchEngineAndNewsAggregator")
website <- read.xlsx("NewsConsumption.xlsx",vsheet = "NewsWebsite")

all_countries <- sort(unique(c(aggregator$country, website$country)))
aggregator <- aggregator %>% 
  filter(country != "Average") %>%
  mutate(country = factor(country, levels = all_countries))
website <- website %>% 
  filter(country != "Average") %>%
  mutate(country = factor(country, levels = all_countries))

make_plot <- function(df) {
  df <- df %>% 
    mutate(country_label = ifelse(country == "Korea", "South Korea", as.character(country)))
  
  ggplot(df, aes(x = country_label, y = percent,
                 fill = ifelse(country == "Korea", "Korea", "Other"))) +
    geom_col(color = "grey30") +
    geom_text(aes(label = percent), vjust = -0.35, size = 3) +
    scale_fill_manual(values = c("Korea" = "grey26", "Other" = "grey65")) +
    expand_limits(y = c(0, 80)) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      #panel.grid.major.x = element_blank(),
      axis.text.x = element_text(angle = 90, size = 10, vjust = 0.5, hjust = 1),
      axis.text.y = element_text(size = 11),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none"
    )
}

fig1.1 <- make_plot(aggregator) +
  labs(title = "Digital News Consumption via Search Engine and News Aggregator")
fig1.2 <- make_plot(website) +
  labs(title = "Digital News Consumption via News Website")

fig1.1 / fig1.2

# Figure 2 ----------------------------------------------------------------
monthly <- read_csv("monthlyStats.csv") %>%
  mutate(yearmonth = make_yearmonth(year = year, month = month))

fig2.1 <- ggplot(monthly, aes(yearmonth, N.users))+
  geom_point()+
  geom_line()+
  scale_y_continuous(labels = c("800k", "600k",  "400k",  "200k"),
                     breaks = c(800000, 600000, 400000, 200000))+
  expand_limits(y = c(100000, 800000))+
  theme_bw()+
  theme(axis.text.x = element_text(size = 22),
        title = element_text(size = 15),
        axis.text.y = element_text(size = 22),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())+
  labs(x = "", y = "", title = "")

fig2.2 <- ggplot(monthly, aes(yearmonth, N.comments))+
  geom_point()+
  geom_line()+
  scale_y_continuous(breaks = c(1000000, 2000000, 3000000, 4000000, 5000000),
                     labels = c("1 million", "2 million", "3 million", "4 million", "5 million"))+
  theme_bw()+
  theme(axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 22),
        title = element_text(size = 15),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())+
  labs(x = "", y = "", title = "")

fig2.1 + fig2.2

# Figure 3 ----------------------------------------------------------------
## No code is needed

# Figure 4 ----------------------------------------------------------------
## See: https://github.com/seonhye-noh/uncivil-Korean-comments-classifier

# Figure 5 ----------------------------------------------------------------
ten.stat <- read_csv("tenYearStats.csv")

cb1 <- yearweek("2019-11-13")
cb2 <- yearweek("2020-06-18")

fig5 <- ggplot(ten.stat, aes(week, value))+
  # points
  geom_point(aes(color = type), size = 3, stroke = 0.3)+
  scale_color_manual(
    values = c("Cleanbot1" = "grey80", "Cleanbot2" = "grey35"),
    labels = c("Cleanbot1" = "Predicted by Cleanbot 1", "Cleanbot2" = "Predicted by Cleanbot 2"))+
  # scale
  scale_x_discrete(
    breaks = c("2012 W14", "2014 W14", "2016 W14", "2018 W14", "2020 W14"),
    labels = c("Apr 2012", "Apr 2014", "Apr 2016", "Apr 2018", "Apr 2020")
  )+
  # theme
  theme_bw()+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = "top",
        axis.title.y = element_text(size = 15),
        panel.grid.minor = element_blank())+
  labs(x = "", y = "Predicted Share of Comments Flagged")+
  # additional line and label
  geom_vline(xintercept = as.character(cb1), colour = "grey30", linetype = 2)+
  geom_text(aes(label = "Cleanbot 1 Launch"),
            size = 4.5,
            x = as.character(cb1), y = 8,
            angle = 90, vjust = -1,
            colour = "black")+
  geom_vline(xintercept = as.character(cb2), colour = "grey30", linetype = 2)+
  geom_text(aes(label = "Cleanbot 2 Launch"),
            size = 4.5,
            x = as.character(cb2), y = 8,
            angle = 90, vjust = -1,
            colour = "black")

fig5

# Figure 6 ------------------------------------------------------
## Percent predicted flagged per day (Cleanbot 1 and 2 +/2months).
dateCB1<-ymd(20191113)
dateCB2<-ymd(20200618)
daily1<-CB1 %>% group_by(date) %>% summarise(flagged = sum(cleanbot1_predicted), total = n()) 
daily1<- daily1 %>% mutate(percentFlagged = flagged/total,pre=date<dateCB1,post=date>dateCB1)
daily2<-CB2 %>% group_by(date) %>% summarise(flagged = sum(cleanbot2_predicted), total = n()) 
daily2<- daily2 %>% mutate(percentFlagged = flagged/total,pre=date<dateCB2,post=date>dateCB2)

fig6.1 <- ggplot(daily1, aes(x = date, y = percentFlagged)) +
  geom_point(shape = 1) +
  geom_vline(xintercept = dateCB1, linewidth = 0.3) +
  geom_segment(
    x = min(daily1$date), xend = dateCB1,
    y = mean(daily1$percentFlagged[daily1$pre]),
    yend = mean(daily1$percentFlagged[daily1$pre]),
    linetype = "dashed"
  ) +
  geom_segment(
    x = dateCB1, xend = max(daily1$date),
    y = mean(daily1$percentFlagged[daily1$post]),
    yend = mean(daily1$percentFlagged[daily1$post]),
    linetype = "dashed"
  ) +
  scale_y_continuous(
    limits = c(.05, .25),
    breaks = (1:5) * .05,
    labels = (1:5) * 5
  ) +
  scale_x_date(
    breaks = c(min(daily1$date), dateCB1-30, dateCB1, dateCB1+30, max(daily1$date)),
    labels = c("9/13/19","10/13/19","11/13/19","12/13/19","1/13/20")
  ) +
  labs(title = "Cleanbot 1",
       y = "Percent Predicted Flagged per Day",
       x = NULL) +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(r = 25),
        panel.grid.minor = element_blank())

fig6.2 <- ggplot(daily2, aes(x = date, y = percentFlagged)) +
  geom_point(shape = 1) +
  geom_vline(xintercept = dateCB2, linewidth = 0.3) +
  geom_segment(
    x = min(daily2$date), xend = dateCB2,
    y = mean(daily2$percentFlagged[daily2$pre]),
    yend = mean(daily2$percentFlagged[daily2$pre]),
    linetype = "dashed"
  ) +
  geom_segment(
    x = dateCB2, xend = max(daily2$date),
    y = mean(daily2$percentFlagged[daily2$post]),
    yend = mean(daily2$percentFlagged[daily2$post]),
    linetype = "dashed"
  ) +
  scale_y_continuous(
    limits = c(.05, .25),
    breaks = (1:5) * .05,
    labels = (1:5) * 5
  ) +
  scale_x_date(
    breaks = c(min(daily2$date), dateCB2-30, dateCB2, dateCB2+30, max(daily2$date)),
    labels = c("4/18/20","5/18/20","6/18/20","7/18/20","8/18/20")
  ) +
  labs(title = "Cleanbot 2",
       y = NULL,
       x = NULL) +
  theme_bw()+
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank())

fig6.1 + fig6.2

# Figure 7 ----------------------------------------------------------------
#Create a moving average line (centered).
daily1<-daily1 %>% mutate(total_ma = zoo::rollmean(total,k=7,fill=NA))
daily2<-daily2 %>% mutate(total_ma = zoo::rollmean(total,k=7,fill=NA))

range.min <- min(c(daily1$total, daily2$total))
range.max <- max(c(daily1$total, daily2$total))

fig7.1 <- ggplot(daily1, aes(x = date, y = total)) +
  geom_point(shape = 1) +
  geom_line(aes(y = total_ma)) +
  geom_vline(xintercept = dateCB1, linewidth = 0.3) +
  geom_segment(
    x = min(daily1$date), xend = dateCB1,
    y = mean(daily1$total[daily1$pre]),
    yend = mean(daily1$total[daily1$pre]),
    linetype = "dashed"
  ) +
  geom_segment(
    x = dateCB1, xend = max(daily1$date),
    y = mean(daily1$total[daily1$post]),
    yend = mean(daily1$total[daily1$post]),
    linetype = "dashed"
  ) +
  scale_y_continuous(
    limits = c(range.min, range.max),
    breaks = (3:9) * 20000,
    labels = (3:9) * 2
  ) +
  scale_x_date(
    breaks = c(min(daily1$date), dateCB1 - 30, dateCB1, dateCB1 + 30, max(daily1$date)),
    labels = c("9/13/19", "10/13/19", "11/13/19", "12/13/19", "1/13/20")
  ) +
  labs(
    title = "Cleanbot 1",
    y = "Daily Comments Posted (Tens of Thousands)",
    x = NULL
  ) +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(r = 25),
        panel.grid.minor = element_blank())

fig7.2 <- ggplot(daily2, aes(x = date, y = total)) +
  geom_point(shape = 1) +
  geom_line(aes(y = total_ma)) +
  geom_vline(xintercept = dateCB2, linewidth = 0.3) +
  geom_segment(
    x = min(daily2$date), xend = dateCB2,
    y = mean(daily2$total[daily2$pre]),
    yend = mean(daily2$total[daily2$pre]),
    linetype = "dashed"
  ) +
  geom_segment(
    x = dateCB2, xend = max(daily2$date),
    y = mean(daily2$total[daily2$post]),
    yend = mean(daily2$total[daily2$post]),
    linetype = "dashed"
  ) +
  scale_y_continuous(
    limits = c(range.min, range.max),
    breaks = (3:9) * 20000,
    labels = (3:9) * 2
  ) +
  scale_x_date(
    breaks = c(min(daily2$date), dateCB2 - 30, dateCB2, dateCB2 + 30, max(daily2$date)),
    labels = c("4/18/20", "5/18/20", "6/18/20", "7/18/20", "8/18/20")
  ) +
  labs(
    title = "Cleanbot 2",
    y = NULL,
    x = NULL
  ) +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank())

fig7.1 + fig7.2

# Figure 8 ----------------------------------------------------------------
# Data for percentage of flagged comments by section
sections <- c("Politics", "Society","Economy", "World", "Life/Culture", "IT/Science")

fig8.1_cb1_section <- CB1 %>%
  filter(section %in% sections) %>%
  group_by(section) %>%
  summarise(
    cb = "Cleanbot1",
    pre  = mean(cleanbot1_predicted[prepost == "pre"],  na.rm = TRUE),
    post = mean(cleanbot1_predicted[prepost == "post"], na.rm = TRUE),
    .groups = "drop"
  )
fig8.1_cb2_section <- CB2 %>%
  filter(section %in% sections) %>%
  group_by(section) %>%
  summarise(
    cb = "Cleanbot2",
    pre  = mean(cleanbot2_predicted[prepost == "pre"],  na.rm = TRUE),
    post = mean(cleanbot2_predicted[prepost == "post"], na.rm = TRUE),
    .groups = "drop"
  )
fig8.1 <- bind_rows(fig8.1_cb1_section, fig8.1_cb2_section) %>%
  mutate(P.pre = pre*100,
         P.post = post*100)

# Data for average number of daily comments
fig8.2_cb1_section <- CB1 %>%
  count(prepost, section, commenting_date, name = "N.comments") %>%
  filter(section %in% sections) %>%
  group_by(section) %>%
  summarise(
    cb = "Cleanbot1",
    pre  = mean(N.comments[prepost == "pre"],  na.rm = TRUE),
    post = mean(N.comments[prepost == "post"], na.rm = TRUE),
    .groups = "drop"
  )

fig8.2_cb2_section <- CB2 %>%
  count(prepost, section, commenting_date, name = "N.comments") %>%
  filter(section %in% sections) %>%
  group_by(section) %>%
  summarise(
    cb = "Cleanbot2",
    pre  = mean(N.comments[prepost == "pre"],  na.rm = TRUE),
    post = mean(N.comments[prepost == "post"], na.rm = TRUE),
    .groups = "drop"
  )

fig8.2 <- bind_rows(fig8.2_cb1_section, fig8.2_cb2_section)

# Function to plot figure 8
plot_pre_post_vertical_arrows <- function(df, yvar_pre, yvar_post, sectionvar, cleanbotvar, ylabel, title = "") {
  
  df <- df %>%
    mutate(section_numeric = as.numeric(factor(!!sym(sectionvar), levels = sections)),
           x_pos = ifelse(!!sym(cleanbotvar) == "Cleanbot1", section_numeric - 0.2, section_numeric + 0.2))
  
  ggplot(df) +
    # Arrows
    geom_segment(aes(x = x_pos, xend = x_pos, 
                     y = !!sym(yvar_pre), yend = !!sym(yvar_post),
                     color = !!sym(cleanbotvar)),
                 arrow = arrow(length = unit(0.35, "cm"), type = "open"),
                 linewidth = 0.8) +
    # Points
    geom_point(aes(x = x_pos, y = !!sym(yvar_pre), color = !!sym(cleanbotvar), shape = !!sym(cleanbotvar)),
               size = 3, stroke = 1) +
    # Scales and labels
    scale_color_manual(values = c("Cleanbot1" = "grey70", "Cleanbot2" = "black")) +
    scale_shape_manual(values = c("Cleanbot1" = 16, "Cleanbot2" = 16)) +
    scale_x_continuous(breaks = 1:6, labels = sections) +
    
    labs(x = "", y = ylabel, color = "", shape = "", title = title) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 15),
          axis.title = element_text(size = 14),
          legend.position = "top",
          legend.text = element_text(size = 14),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank())
}

# fig8.1: Percent of comments flagged
fig8.1 <- plot_pre_post_vertical_arrows(
  df = fig8.1,
  yvar_pre = "P.pre",
  yvar_post = "P.post",
  sectionvar = "section",
  cleanbotvar = "cb",
  ylabel = "Percent of Comments Predicted to be Flagged"
)

# fig8.2: Average daily comments
fig8.2 <- plot_pre_post_vertical_arrows(
  df = fig8.2,
  yvar_pre = "pre",
  yvar_post = "post",
  sectionvar = "section",
  cleanbotvar = "cb",
  ylabel = "Average Daily Number of Comments"
)

fig8.1 + fig8.2

# Figure 9 ----------------------------------------------------------------
# Data for replies
reply_CB1 <- CB1 %>%
  #filter(replyallcount >= 1) %>%
  mutate(
    period = ifelse(prepost == "pre", "Pre-Cleanbot", "Post-Cleanbot"),
    civility = ifelse(cleanbot1_predicted == 1, "Uncivil", "Civil")
  ) %>%
  group_by(period, civility) %>%
  summarise(
    y = mean(replyallcount, na.rm = TRUE), # mean_replies
    .groups = "drop"
  ) %>%
  mutate(CB = "Cleanbot1")

reply_CB2 <- CB2 %>%
  #filter(replyallcount >= 1) %>%
  mutate(
    period = ifelse(prepost == "pre", "Pre-Cleanbot", "Post-Cleanbot"),
    civility = ifelse(cleanbot2_predicted == 1, "Uncivil", "Civil")
  ) %>%
  group_by(period, civility) %>%
  summarise(
    y = mean(replyallcount, na.rm = TRUE), # mean_replies
    .groups = "drop"
  ) %>%
  mutate(CB = "Cleanbot2")

reply_all <- bind_rows(reply_CB1, reply_CB2)
reply_all <- reply_all %>%
  mutate(period = factor(period, levels = c("Pre-Cleanbot", "Post-Cleanbot")),
         civility = factor(civility, levels = c("Uncivil", "Civil")))

# Data for likes
like_CB1 <- CB1 %>%
  mutate(
    likeddifference = sympathycount - antipathycount,
    period = ifelse(prepost=="pre", "Pre-Cleanbot", "Post-Cleanbot"),
    civility = ifelse(cleanbot1_predicted == 1, "Uncivil", "Civil")
  ) %>%
  group_by(period, civility) %>%
  summarise(
    y = mean(likeddifference, na.rm = TRUE), # mean difference
    .groups = "drop"
  ) %>%
  mutate(CB = "Cleanbot1")

like_CB2 <- CB2 %>%
  mutate(
    likeddifference = sympathycount - antipathycount,
    period = ifelse(prepost=="pre", "Pre-Cleanbot", "Post-Cleanbot"),
    civility = ifelse(cleanbot2_predicted == 1, "Uncivil", "Civil")
  ) %>%
  group_by(period, civility) %>%
  summarise(
    y = mean(likeddifference, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(CB = "Cleanbot2")

fig9.1_replies <- make_panel(reply_all, "Average # Replies")
fig9.2_likes   <- make_panel(like_all,  "Average Like Difference")

fig9.1_replies + fig9.2_likes

###########################################################
#Data prep for Figures 10, 11, and 12. Create dataset at user level.

CB1date<-CB1$dateCB[[1]]
CB2date<-CB2$dateCB[[1]]

usersOneMonth1<-CB1 %>% filter((date2>CB1date-31)&(date2<CB1date+31)) %>% group_by(useridno, prepost) %>% summarise(totalFlagged = sum(cleanbot1_predicted), totalPosts = n(),percentFlagged=totalFlagged/totalPosts*100) 
usersOneMonth2<-CB2 %>% filter((date2>CB2date-31)&(date2<CB2date+31)) %>% group_by(useridno, prepost) %>% summarise(totalFlagged = sum(cleanbot2_predicted), totalPosts = n(),percentFlagged=totalFlagged/totalPosts*100) 

usersOneMonthPre1<-usersOneMonth1 %>% filter(prepost=='pre')
usersOneMonthPre2<-usersOneMonth2 %>% filter(prepost=='pre')
usersOneMonthPre1$cat<- cut(usersOneMonthPre1$totalPosts,breaks=c(0,2, 15,1000000),labels=c('infrequent', 'moderate', 'frequent'))
usersOneMonthPre2$cat<- cut(usersOneMonthPre2$totalPosts,breaks=c(0,2, 15,1000000),labels=c('infrequent', 'moderate', 'frequent'))

usersOneMonthPost1<-usersOneMonth1 %>% filter(prepost=='post')
usersOneMonthPost2<-usersOneMonth2 %>% filter(prepost=='post')
usersOneMonthPost1$cat<- cut(usersOneMonthPost1$totalPosts,breaks=c(0,2, 15,1000000),labels=c('infrequent', 'moderate', 'frequent'))
usersOneMonthPost2$cat<- cut(usersOneMonthPost2$totalPosts,breaks=c(0,2, 15,1000000),labels=c('infrequent', 'moderate', 'frequent'))

usersPreAndPost1<-full_join(usersOneMonthPre1,usersOneMonthPost1,by="useridno",suffix=c('Pre','Post'))
usersPreAndPost2<-full_join(usersOneMonthPre2,usersOneMonthPost2,by="useridno",suffix=c('Pre','Post'))

binCenter<-function(bins){
  n<-length(bins)
  binsbins<-c(bins,bins[2:(n-1)])
  orderedEndPoints<-sort(binsbins)
  temp<-tibble(g=sort(rep(c(1:((2*n-2)/2)),2)),ep=orderedEndPoints)
  centers<-temp %>% group_by(g) %>% summarise(mid=mean(ep))
  return(centers$mid)
}

###########################################################

# Figure 10 ---------------------------------------------------------------
# Flag percent by commenting frequency
# data
plotData<-usersPreAndPost1 %>% mutate(x=cut(totalPostsPre,breaks=c(c(0:10)*5,c(1:5)*10+50),labels=c(c(0:9)*5+2.5,c(0:4)*10+55))) %>% group_by(x) %>% 
  summarise(y=mean(percentFlaggedPre),n=n(),sd=sd(percentFlaggedPre),se=sd/sqrt(n),
            y2=mean(percentFlaggedPost,na.rm=TRUE),sd2=sd(percentFlaggedPost,na.rm=TRUE),se2=sd2/sqrt(n))
plotData<-plotData %>% mutate(change=(y2-y)/y*100)

plotData2<-usersPreAndPost2 %>% mutate(x=cut(totalPostsPre,breaks=c(c(0:10)*5,c(1:5)*10+50),labels=c(c(0:9)*5+2.5,c(0:4)*10+55))) %>% group_by(x) %>% 
  summarise(y=mean(percentFlaggedPre),n=n(),sd=sd(percentFlaggedPre),se=sd/sqrt(n),
            y2=mean(percentFlaggedPost,na.rm=TRUE),sd2=sd(percentFlaggedPost,na.rm=TRUE),se2=sd2/sqrt(n))
plotData2<-plotData2 %>% mutate(change=(y2-y)/y*100)

facetData1<-plotData %>% mutate(cb='Cleanbot 1')
facetData2<-plotData2 %>% mutate(cb='Cleanbot 2')
facetData<-bind_rows(facetData1,facetData2)
facetData$x<-as.numeric(as.character(facetData$x))

ann_text1 <- tribble(
  ~x, ~y2, ~cb,
  55, 10.8, 'Cleanbot 1'
)
ann_text2 <- tribble(
  ~x, ~y2, ~cb,
  44, 13.2, 'Cleanbot 1'
)
ann_text3 <- tribble(
  ~x, ~y2, ~cb,
  44, 21.9, 'Cleanbot 2'
)
ann_text4 <- tribble(
  ~x, ~y2, ~cb,
  55, 17.7, 'Cleanbot 2'
)
#quartz(width=12,height=6)

# plot
fig10 <- ggplot(facetData, aes(x = x)) +
  ## Before Cleanbot
  geom_line(aes(y = y), color = "grey60", linewidth = 0.6) +
  geom_point(aes(y = y), color = "grey30", size = 2) +
  geom_linerange(
    aes(ymin = y - se, ymax = y + se),
    color = "grey40",
    linewidth = 0.5
  ) +
  ## After Cleanbot
  geom_line(aes(y = y2), color = "grey60", linewidth = 0.6) +
  geom_point(
    aes(y = y2, color = change),
    size = 2.5
  ) +
  geom_linerange(
    aes(ymin = y2 - se2, ymax = y2 + se2, color = change),
    linewidth = 0.6
  ) +
  ## Scales
  scale_y_continuous(
    limits = c(7.5, 23),
    #name = "Percent flagged comments\n(mean ± SE)"
    name = "Percent Flagged Comments\n(± standard error)"
  ) +
  scale_x_continuous(
    name = "Total Comments Month Before Cleanbot"
  ) +
  scale_color_viridis_c(
    option = "C",
    end = 0.9,
    name = "Percent change\nin incivility"
  ) +
  ## Annotations
  geom_text(data = ann_text2, aes(y = y2), label = "Before Cleanbot",
            color = "black", size = 4) +
  geom_text(data = ann_text1, aes(y = y2), label = "After Cleanbot",
            color = "black", size = 4) +
  geom_text(data = ann_text3, aes(y = y2), label = "Before Cleanbot",
            color = "black", size = 4) +
  geom_text(data = ann_text4, aes(y = y2), label = "After Cleanbot",
            color = "black", size = 4) +
  
  facet_wrap(~cb) +
  ## Theme
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major.y = element_line(color = "grey90"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    
    strip.background = element_blank(),
    strip.text = element_text(size = 15),
    
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11),
    legend.position = "right"
  )

fig10

# Figure 11 ---------------------------------------------------------------
# Change in flagged by pre-intervention number of flags
A<-c(c(0:10)*5,50+c(1:23)*10)
B<-binCenter(A)

minPosts<-0

incivilityPlotData1 <- usersPreAndPost1 %>% filter(totalPostsPre>=minPosts) %>%
  mutate(x=cut(totalFlaggedPre,breaks=A,labels=B,include.lowest=TRUE)) %>% group_by(x) %>%
  summarise(y=mean((totalFlaggedPost-totalFlaggedPre),na.rm=TRUE),
            n=n(),sd=sd((totalFlaggedPost-totalFlaggedPre),na.rm=TRUE),se=sd/sqrt(n)) %>%
  mutate(cb='Cleanbot 1')

incivilityPlotData2 <- usersPreAndPost2 %>% filter(totalPostsPre>=minPosts) %>%
  mutate(x=cut(totalFlaggedPre,breaks=A,labels=B,include.lowest=TRUE)) %>% group_by(x) %>%
  summarise(y=mean((totalFlaggedPost-totalFlaggedPre),na.rm=TRUE),
            n=n(),sd=sd((totalFlaggedPost-totalFlaggedPre),na.rm=TRUE),
            se=sd/sqrt(n)) %>%  mutate(cb='Cleanbot 2')

incivilityPlotData <- bind_rows(incivilityPlotData1,incivilityPlotData2)
incivilityPlotData$x<-as.numeric(as.character(incivilityPlotData$x))

medianIncivilityPre1<-median((usersPreAndPost1 %>% filter(totalPostsPre>=minPosts,totalFlaggedPre>=0))$totalFlaggedPre)
medianIncivilityPre2<-median((usersPreAndPost2 %>% filter(totalPostsPre>=minPosts,totalFlaggedPre>=0))$totalFlaggedPre)

modelData1<-usersPreAndPost1 %>% filter(totalFlaggedPre<=500) %>% mutate(change=totalFlaggedPost-totalFlaggedPre)
modelData2<-usersPreAndPost2 %>% filter(totalFlaggedPre<=500) %>% mutate(change=totalFlaggedPost-totalFlaggedPre)
model1<-lm(change~0+totalFlaggedPre,modelData1)
model2<-lm(change~0+totalFlaggedPre,modelData2)
dim(modelData1)[1]/dim(usersPreAndPost1)[1]
dim(modelData2)[1]/dim(usersPreAndPost2)[1]

slopes<-tibble(m=c(model1$coefficients[[1]],model2$coefficients[[1]]),b=c(0,0),cb=c("Cleanbot 1","Cleanbot 2"))

medians<-tibble(medianIncivility=c(medianIncivilityPre1,medianIncivilityPre2),cb=c('Cleanbot 1','Cleanbot 2'))
#quartz(width=12,height=6)

incivilityPlotData$cb <- factor(incivilityPlotData$cb)

fig11 <- ggplot(incivilityPlotData, aes(x = x, y = y)) +
  ## Reference line
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.6) +
  ## Linear trend
  geom_abline(
    data = slopes,
    aes(slope = m, intercept = b),
    color = "red3",
    linewidth = 0.5,
    show.legend = FALSE
  ) +
  ## Estimates
  geom_linerange(
    aes(ymin = y - se, ymax = y + se),
    color = "grey40",
    linewidth = 0.6
  ) +
  geom_point(
    aes(size = n),
    shape = 21,
    fill = "white",
    color = "grey25",
    stroke = 0.8
  ) +
  ## Scales
  scale_size_continuous(
    range = c(2.5, 6)
  ) +
  scale_x_continuous(
    limits = c(0, 150),
    name = "Flagged Comments before Cleanbot"
  ) +
  scale_y_continuous(
    limits = c(-90, 15),
    name = "Change in Flagged Comments after Cleanbot\n(± standard error)"
  ) +
  facet_wrap(~cb) +
  ## Theme
  theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 15),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 13),
    legend.position = "none",
    panel.spacing.x = unit(1.5, "cm")
  )

fig11

# Figure 12 ---------------------------------------------------------------
# Percent change in comments by pre-intervention percent flagged
A<-c(0:20)*5
B<-binCenter(A)

freqData1<-usersPreAndPost1 %>% filter(!is.na(totalPostsPre)) %>%
  mutate(totalPostsPost = ifelse(is.na(totalPostsPost), 0, totalPostsPost)) %>%
  mutate(x=cut(percentFlaggedPre,breaks=A,labels=B,include.lowest=TRUE)) %>%
  group_by(x,catPre) %>% summarise(y=mean(100*(totalPostsPost-totalPostsPre)/totalPostsPre,na.rm=TRUE),
                                   n=n(),sd=sd(100*(totalPostsPost-totalPostsPre)/totalPostsPre,na.rm=TRUE),se=sd/sqrt(n)) %>%
  mutate(cb = 'Cleanbot 1')

freqData2<-usersPreAndPost2 %>% filter(!is.na(totalPostsPre)) %>% 
  mutate(totalPostsPost = ifelse(is.na(totalPostsPost), 0, totalPostsPost)) %>%
  mutate(x=cut(percentFlaggedPre,breaks=A,labels=B,include.lowest=TRUE)) %>%
  group_by(x,catPre) %>% summarise(y=mean(100*(totalPostsPost-totalPostsPre)/totalPostsPre,na.rm=TRUE),
                                   n=n(),sd=sd(100*(totalPostsPost-totalPostsPre)/totalPostsPre,na.rm=TRUE),se=sd/sqrt(n)) %>%
  mutate(cb = 'Cleanbot 2')

freqData<-rbind(freqData1,freqData2)
freqData$x<-as.numeric(as.character(freqData$x))
#quartz(width=12,height = 6)

fig12 <- ggplot(freqData, aes(x = x, y = y, color = catPre)) +
  ## Reference line
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.6) +
  ## Estimates
  geom_linerange(
    aes(ymin = y - se, ymax = y + se),
    position = position_dodge(width = 1.5),
    linewidth = 0.6
  ) +
  geom_point(
    size = 2.8,
    position = position_dodge(width = 1.5),
  ) +
  ## Scales
  scale_x_continuous(
    name = "Percent Flagged Comments before Cleanbot"
  ) +
  scale_y_continuous(
    name = "Percent Change in Total Comments after Cleanbot\n(± standard error)"
  ) +
  scale_color_viridis_d(
    option = "C",
    end = 0.85,
    labels = c("Infrequent", "Moderate", "Frequent"),
    name = "Pre-intervention\ncommenting frequency"
  ) +
  facet_wrap(~cb) +
  ## Theme
  theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    
    strip.background = element_blank(),
    strip.text = element_text(size = 15),
    
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 15),
    axis.text = element_text(size = 13),
    
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    legend.position = "right"
  )
fig12

