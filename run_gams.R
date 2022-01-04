library(tidyverse)
library(readxl)
library(mgcv)
library(ggplot2)
library(viridis)

# process effort data
ca_effort = readxl::read_xlsx("CA_effort.xlsx")
ca_effort = pivot_longer(ca_effort, cols = 2:9, names_to = "Month")
or_effort = readxl::read_xlsx("OR_effort.xlsx")
or_effort = pivot_longer(or_effort, cols = 2:12, names_to = "Month")
wa_effort = readxl::read_xlsx("WA_effort.xlsx")
wa_effort = pivot_longer(wa_effort, cols = 2:9, names_to = "Month")
effort = rbind(ca_effort, or_effort, wa_effort)
effort = dplyr::rename(effort, "Days" = value)

# process landings data
ca = readxl::read_xlsx("CA_landings.xlsx")
ca = pivot_longer(ca, cols = 2:9, names_to = "Month")
or = readxl::read_xlsx("OR_landings.xlsx")
or = pivot_longer(or, cols = 2:12, names_to = "Month")
wa = readxl::read_xlsx("WA_landings.xlsx")
wa = pivot_longer(wa, cols = 2:9, names_to = "Month")
landings = rbind(ca, or, wa)
landings = dplyr::rename(landings, "Landings" = value)

effort$Year = as.numeric(effort$Year)
effort$Days = as.numeric(effort$Days)
landings$Landings = as.numeric(landings$Landings)
data = dplyr::left_join(landings, effort) %>% 
  dplyr::filter(!is.na(Landings),!is.na(Days), Days > 0,
                Month!="Season")

# bring in spatial codes
codes = read.csv("area_code.csv") %>% 
  dplyr::rename(Port = Area)
data = dplyr::left_join(data, codes)

# recode month as numeric
data$MonthN = NA
data$MonthN[which(data$Month == "Apr.")] = 4
data$MonthN[which(data$Month == "May")] = 5
data$MonthN[which(data$Month == "June")] = 6
data$MonthN[which(data$Month == "July")] = 7
data$MonthN[which(data$Month == "Aug.")] = 8
data$MonthN[which(data$Month == "Sept.")] = 9
data$MonthN[which(data$Month == "Oct.")] = 10
data$MonthN[which(data$Month == "Nov.")] = 11
data$MonthN[which(data$Month == "Dec.")] = 12

# fit initial model - neg bin family, log(offset)
fit <- gam(Landings ~ s(Year) + s(Code) +s(MonthN,k=4) + offset(log(Days)), 
            family = nb(), data = data)

# That model does pretty good -- but we can maybe improve by addingn 2-way
# smooths across space/year/season
fit2 <- gam(Landings ~ s(Year) + s(Code) +s(MonthN,k=4) + 
              ti(Year,Code) + ti(Code,MonthN,k=4) + 
              ti(Year,MonthN,k=4) + offset(log(Days)), 
           family = nb(), data = data)

# make predictions
df = expand.grid(Code = unique(data$Code),
                 MonthN = 4:10,
                 Year = unique(data$Year),
                 Days = 10)
df$pred = predict(fit2, df, type="response")
df$cpue = df$pred / df$Days

df = dplyr::left_join(df, codes)
df$Port = factor(df$Port, levels = codes$Port)
# make plot of a few time slices across ports

jpeg("July_CPUE_byport.jpeg")
ggplot(dplyr::filter(df, MonthN == 7,Year %in% seq(1975,2020,5)), aes(Port, cpue, group=Year,col=Year)) + 
  geom_line() + 
  theme_bw() + 
  ylab("Predicted CPUE (July)") + 
  scale_color_viridis(end=0.8) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

jpeg("Seasonal_CPUE_byport.jpeg")
ggplot(dplyr::filter(df,Year %in% seq(1975,2020,5)), aes(MonthN, cpue, group=Year,col=Year)) + 
  geom_line() + 
  theme_bw() + 
  facet_wrap(~Port, scale="free_y") + 
  ylab("Predicted CPUE") + 
  xlab("Month") + 
  scale_color_viridis(end=0.8) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()


