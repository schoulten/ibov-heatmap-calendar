### This R code provides a heatmap as a calendar of the daily return (%) of IBOVESPA (B3).

# Created by Fernando da Silva (GitHub: @schoulten)


# Packages ----------------------------------------------------------------

# Install/load packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse", "lubridate", "ragg", "quantmod", "RColorBrewer")


# Disable scientific notation
options(scipen = 999)


# Data --------------------------------------------------------------------

# Get IBOVESPA data
getSymbols("^BVSP", src = "yahoo")


# Calculate daily return
return <- dailyReturn(BVSP$BVSP.Close, type = "log")


# Create object with retuns and dates to the heatmap calendar
ibov <- tibble(time   = as.Date(time(return)),
               return = round(return$daily.returns$daily.returns*100, 2)) %>%
  filter(time >= "2020-01-01" & time <= "2020-12-31") %>% 
  complete(time = seq(ymd("2020-01-01"), 
                      ymd("2020-12-31"), 
                      "day")) %>%
  mutate(weekday  = wday(time, label = T, week_start = 1), 
         month    = month(time, label = T, abbr = F),
         week     = isoweek(time),
         day      = day(time), 
         week     = case_when(month == "dezembro" & week == 1 ~ 53,
                              month == "janeiro" & week %in% 52:53 ~ 0,
                              TRUE ~ week),
         min_r = min(return, na.rm = TRUE),
         max_r = max(return, na.rm = TRUE),
         preturn  = cut(return, c(round((min_r[1])-1,0):(round((max_r[1])+1,0))))
         ) 


# Plot --------------------------------------------------------------------

# Set pallete colors
pubu <- RColorBrewer::brewer.pal(9, "RdYlGn")
col_p <- colorRampPalette(pubu)

# Create custom chart theme
theme_calendar <- function(){
  
  theme(aspect.ratio     = 1/2,
        axis.title       = element_blank(),
        axis.ticks       = element_blank(),
        axis.text.y      = element_blank(),
        axis.text.x = element_text(face = "bold", size = 12),
        panel.grid       = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text       = element_text(face = "bold", size = 15),
        legend.position  = "top",
        legend.text      = element_text(hjust = .5, face = "bold", size = 12),
        legend.title     = element_text(size = 12, hjust = 1, face = "bold"),
        plot.caption     =  element_text(hjust = 1, size = 12),
        panel.border     = element_rect(colour = "grey", fill=NA, size=1),
        plot.title       = element_text(hjust = .5,
                                        size = 26,
                                        face = "bold",
                                        margin = margin(0,0,0.5,0,unit = "cm")),
        plot.subtitle    = element_text(hjust = .5, size = 16)
  )
}


# Plot heatmap calendar
ibov %>%
  filter(!weekday %in% c("sáb", "dom")) %>%
  ggplot(aes(weekday, -week, fill = preturn)) +
  geom_tile(colour = "white", size = .4)  + 
  geom_text(aes(label = day, fontface = "bold")) +
  guides(fill = guide_colorsteps(barwidth = 40, 
                                 barheight = .4,
                                 title.position = "top")) +
  scale_fill_manual(values = c("white", col_p(30)),
                    na.value = "grey95", drop = FALSE) +
  scale_colour_manual(values = c("black", "white"), guide = FALSE) + 
  facet_wrap(~ month, nrow = 4, ncol = 3, scales = "free") +
  labs(title    = "Como foi o IBOVESPA em 2020?", 
       subtitle = "Retorno Diário",
       caption  = "Fonte: @schoulten com dados de Yahoo Finance",
       fill     = "Retorno em %") +
  theme_calendar()


# Save PNG
ggsave("ibov_calendar.png", height = 10, width = 8, device = agg_png(), dpi = 1200)
