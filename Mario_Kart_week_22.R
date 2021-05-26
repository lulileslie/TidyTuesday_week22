# Libraries 
library('tidyverse')
library('ggthemes')
library("scales")
library("ggtext")
library("tidytuesdayR")

#Importing data
tuesdata <- tidytuesdayR::tt_load(2021, week = 22)
records <- tuesdata$records

# Theme settings
theme_set(theme_minimal(base_size = 15))

theme_update(
  panel.grid.major = element_line(color = "grey92", size = .2),
  axis.title.x = element_text(color = "grey30", margin = margin(t = 7), family = "Muli Light", size = 9),
  axis.title.y = element_text(color = "grey30", margin = margin(r = 7), family = "Muli Light", size = 9),
  axis.text = element_text(color = "grey50", family = "Muli Light"),
  axis.ticks =  element_line(color = "grey92", size = .2),
  axis.ticks.length = unit(.5, "lines"),
  legend.position = "right",
  plot.title = element_text(hjust = 0, color = "black",
                            family = "Muli SemiBold",
                            size = 16, margin = margin(t = 10, b = 10)),
  plot.subtitle = element_text(hjust = 0, color = "grey30",
                               family = "Muli",
                               size = 12, margin = margin(0, 0, 15, 0)),
  plot.title.position = "plot",
  plot.caption = element_text(color = "grey50", size = 10, hjust = 1,
                              family = "Muli Light",
                              lineheight = 1.05, margin = margin(30, 0, 0, 0)),
  plot.caption.position = "plot",
  plot.margin = margin(rep(20, 4))
)

# Subset: Three lap records, no shortcut 
Variacao_simples_no <- records %>% 
  filter(type == "Three Lap") %>% 
  filter(shortcut == "No") %>% 
  group_by(track) %>%
  summarise(variou = max(time) - min(time),
            inicial = max(time),
            atual = min(time),
            max_duracao = max(record_duration)) 

# Plot
plot <- ggplot(Variacao_simples_no, aes(x = inicial, y = reorder (track, inicial), group = track), alpha = 0.9)+
  geom_segment(aes(x = inicial, xend = atual, y = reorder (track, inicial), yend = track), color = "lightgrey") +
  geom_point(color = "blue") +
  geom_point(aes(x = atual, y = track), color = "red") +
  xlim(80, 440)+
  labs(title = "Mario Kart 64 record time variation by track", 
       subtitle = "How word records varied between February 1997 and February 2021,\nnot counting shortcuts") +
  theme(axis.title.y=element_blank()) +
  xlab("Time in seconds") +
  annotate ("text", x = 415, y = 16.2, label = "First record", color = "blue", family = "Muli Light", size = 3.6 )+
  annotate ("text", x = 305, y = 16.2, label = "Current record", color = "red", family = "Muli Light", size = 3.6 )

ggsave("MarioKart.png", plot)


# TidyTuesday: https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-05-25/readme.md

