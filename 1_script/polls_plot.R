# Metadata ----------------------------------------------------------------
# Title: polls_plot.R
# Purpose: Build a plot of polls bias
# Author(s): @pablocal
# Date: 2020-11-08 08:32:47
#
# Comments ----------------------------------------------------------------
#
#
#
#
#
# Options and packages ----------------------------------------------------
library(tidyverse)
library(showtext)

font_add_google("Roboto Condensed", "RobotoC")
font_add_google("Roboto", "Roboto")
showtext_auto()

# 1. Prepare data ---------------------------------------------------------
d <- read_csv("2_data/usa_polls.csv")

# 1.1 Bar plot ------------------------------------------------------------
d_polls <- d %>%
  filter(pollster_sponsor != "result") %>% 
  group_by(ele, state) %>% 
  summarise(per_polls = mean(r)) %>% 
  ungroup() %>% 
  select(ele, state, per_polls)

d_results <- d %>%
  filter(pollster_sponsor == "result") %>% 
  select(ele, state, r) %>% 
  mutate(per_results = r) %>% 
  select(ele, state, per_results)

d_plot_bar_20 <- left_join(d_polls, d_results, by = c("ele", "state")) %>% 
  filter(ele == 2020) %>% 
  mutate(per = per_polls - per_results) 

# 1.2 h-lines ---------------------------------------------------------------
d_plot_hline_16 <- left_join(d_polls, d_results, by = c("ele", "state")) %>% 
  filter(ele == 2016) %>% 
  mutate(per = per_polls - per_results) 

# 1.3 scatter and line ----------------------------------------------------
d_plot_point <- left_join(d_polls, d_results, by = c("ele", "state")) %>%
  filter(ele == 2020) %>% 
  pivot_longer(cols = per_polls:per_results,
               names_to = "poll_result",
               values_to = "per") %>% 
  mutate(per = per-40,
         col = ifelse(poll_result == "per_polls", "tomato", "red"),
         alpha = ifelse(poll_result == "per_polls", .9, 1))

# 1.4 labels --------------------------------------------------------------
d_text <- tibble(
  state = c(.5, .5, .5, 12.4),
  per = c(4.5, 9.5, 14.5, -5.5),
  label = c("45%", "50%", "55%", "-5 p.p.")
)

d_labels <- tibble(
  state = c(2.5, 2.4, 3.2, 5),
  per = c(4.1, 15.3, -8.5, -6),
  label = c("Polls\n(% Trump)", "Results\n(% Trump)", "Deviation\n2016", "Poll average\ndeviation 2020"),
  color = c("tomato", "red", "gray20", "firebrick")
)

d_arrows <- tibble(x = c(2.1, 2.5, 3.5) , 
                   y = c(15.2, 4.7, -8.5), 
                   xend = c(2, 2.1, 3.8), 
                   yend = c(13.8, 6.2, -7.4))

# 2. Plot -----------------------------------------------------------------

ggplot() + 
  geom_hline(yintercept = 0) +
  geom_col(data = d_plot_bar_20, aes(x = reorder(state, per), y = per), fill = "firebrick", width = .7) +
  geom_line(data = d_plot_point, aes(x = state, y = per, group = state), col = "tomato") +
  geom_point(data = d_plot_point, aes(x = state, y = per, col = col, alpha = alpha), size = 5) +
  geom_errorbar(data = d_plot_hline_16, aes(x = state, y = per, ymin = per, ymax = per), linetype = 2) +
  geom_text(data = d_text, aes(x = state, y = per, label = label), color = "grey40", size = 3.5, family = "RobotoC") +
  geom_text(data = d_labels, aes(x = state, y = per, label = label, color = color), size = 5, fontface = "bold", family = "RobotoC") +
  geom_curve(data = d_arrows, aes(x = x, y = y, xend = xend, yend = yend), arrow = arrow(length = unit(0.07, "inches")), curvature = .3, size = .3, col = "gray40") +
  geom_curve(aes(x = 4.6, y = -6.2, xend = 4.1, yend = -4.95), arrow = arrow(length = unit(0.07, "inches")), curvature = -.5, angle = 90, size = .3, col = "gray40") +
  scale_alpha_identity() +
  scale_color_identity() +
  labs(title = "Trump's vote was underestimated in all key battleground states",
       caption = "Real Clear Politics 10/11/2020") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "gray"),
        panel.grid.major.x = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold", size = 20, family = "RobotoC"),
        plot.title = element_text(family = "RobotoC", face = "bold", size = 30),
        plot.caption = element_text(color = "grey10", size = 9, family = "RobotoC") 
        )



ggsave("plot.svg")
ggsave("plot.pdf")
ggsave("plot.jpg")

