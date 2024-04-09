# Reproducing the WSJ Russia’s real GDP forecast chart using R
# Воспроизведение графика прогноза реального ВВП России от WSJ с помощью R
# Source: https://www.wsj.com/articles/is-russias-economy-growing-or-shrinking-it-depends-on-the-forecaster-41e7af0c
library(tidyverse)
library(ragg)
library(cowplot)

# extrafont::loadfonts() #fix fonts

#loading data
GDP_forecast <- read_delim("GDP forecast.csv",
                           col_types = "cdf")

GDP_forecast$Organization <-
  stringr::str_wrap(GDP_forecast$Organization, 7)

#Checking
GDP_forecast
glimpse(GDP_forecast)

GDP_forecast %>% group_by(Year) %>%
  summarise(GDP_mean = round(mean(`GDP forecast`), 1),
            GDP_median = median(`GDP forecast`)) %>%
  arrange(Year)

#Calculate the average forecast by organization/year
GDP_mean <- GDP_forecast %>% group_by(Organization) %>%
  summarise(
    GDP_mean = round(mean(`GDP forecast`), 2),
    GDP_median = median(`GDP forecast`),
    GDP_2023 = `GDP forecast`[Year == 2023]
  ) %>%
  arrange(desc(GDP_2023)) #sorted by 2023 GDP values
GDP_mean

#coord for labels GDP percent
nudge_x <- c(-0.2, -0.1, 0.0,
             -0.2, -0.15, 0.0,
             -0.2, 0.2, 0.0,
             -0.2, 0.25, 0.0,
             -0.2, 0.35, 0.0,
             -0.2, 0.4, 0.0,
             -0.2, 0.4, 0.0
             )
nudge_y <- c(-0.4, 0.4, 0.4,
             -0.4, 0.4, 0.4,
             -0.4, -0.4, 0.45,
             -0.4, -0.3, 0.45,
             -0.4, -0.2, 0.45,
             -0.4, 0.0, 0.4,
             -0.4, 0.0, 0.3
             )

#main dataset
GDP_forecast <-
  GDP_forecast %>% mutate(Organization = factor(
    Organization,
    ordered = TRUE,
    levels = GDP_mean$Organization
  )) %>%
  mutate(
    id = as.numeric(Organization),
    x = as.numeric(Year),
    y = `GDP forecast`,
    GDP_perc = ifelse(Organization == "JPMORGAN",
                      sprintf("%.1f%%", y), y),
    Org_labels =
      ifelse(
        Organization != "JPMORGAN" & Year == 2022,
        as.character(Organization),
        ""
      ),
    nudge_x = nudge_x,
    nudge_y = nudge_y
  )

#Checking
glimpse(GDP_forecast)
levels(GDP_forecast$Organization)

#create approximated values for the graph and smooth gradient
approx_df <- GDP_forecast %>% group_by(id, Organization) %>%
  reframe(x_approx = approx(x, n = 1000)$x,
          y_approx = approx(y, n = 1000)$y) %>%
  mutate(x = x_approx, y = y_approx)

glimpse(approx_df)

#dataset for background gray lines
d_bg <- GDP_forecast %>% select(-Organization)

# minimal labels
year_lab <- c("2022", "’23", "’24")
wsj_title <- "Russia's real GDP forecast,\nby organization"
wsj_explainer <- "has the most\npositive outlook\nfor 2023."
wsj_caption <-
  "Note: Seasonal adjustments vary by organization.\nSource: the companies and organizations"

###################
# facet design for ggh4x::facet_manual()
design <- "
#1#
234
567
"
###################

p <- ggplot(approx_df) +
  geom_hline(yintercept = 0,
             linewidth = 0.4,
             color = "gray40") +
  geom_line(
    data = d_bg,
    aes(x, y, group = id),
    colour = "grey90",
    linewidth = 0.7
  ) +
  geom_point(
    data = d_bg,
    aes(x, y, group = id),
    colour = "grey90",
    size = 2,
    shape = 21,
    fill = "white",
    stroke = 0.75
  ) +
  geom_line(
    aes(x = x_approx, y = y_approx,
        colour = y_approx),
    linewidth = 1,
    lineend = "square"
  ) +
  geom_point(
    data = GDP_forecast,
    aes(x, y, color = y),
    size = 3,
    shape = 21,
    stroke = 1.25,
    fill = "white"
  ) +
  geom_text(
    data = GDP_forecast,
    aes(
      x = 0.85,
      y = 1.8,
      group = id,
      label = Org_labels
    ),
    hjust = 0,
    vjust = 0.75
  ) +
  geom_text(
    data = GDP_forecast,
    aes(
      x,
      y,
      group = id,
      label = GDP_perc,
      color = y,
      fontface = "bold",
      family = "PT Sans"
    ),
    size = 4.25, 
    nudge_x = GDP_forecast$nudge_x,
    nudge_y = GDP_forecast$nudge_y
  ) +
  scale_color_gradientn(guide = "none", 
                        breaks = (seq(-2.5,1.8,0.25)),
                        colours = c("#b0983b",
                                    "#187e80"),
                        values = 
                          scales::rescale(c(-2.5, 0, 1.8))) +
  ggh4x::facet_manual( ~ Organization,
                       design = design, scales = "free_x") +
  annotate("text",
           x = 1,
           y = 0.25,
           label = "0%") +
  labs(title = wsj_title,
       caption = wsj_caption) +
  theme_minimal(base_family = "PT Sans", base_size = 16) +
  theme(
    text = element_text(family = "PT Sans"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    panel.spacing.x = unit(3, "lines"),
    panel.spacing.y = unit(2, "lines"),
    strip.text = element_blank(),
    plot.title = element_text(family = "PT Sans", face = "bold", size = 16),
    plot.caption = element_text(
      hjust = 0,
      colour = "gray60",
      size = 12
    )
  ) +
  scale_x_continuous(
    breaks = c(1:3),
    labels = year_lab,
    expand = c(0, 0.15),
    limits = c(0.7, 3.25)
  )

ggdraw(p) +
  annotate(
    geom = "text",
    x = 0.05,
    y = 0.873,
    size = 4.5,
    fontface = "bold",
    family = "PT Sans",
    label = "JPMORGAN",
    hjust = 0
  ) +
  annotate(
    geom = "text",
    x = 0.05,
    y = 0.83,
    size = 4.5,
    family = "PT Sans",
    lineheight = 1,
    label = wsj_explainer,
    hjust = 0
  ) +
  geom_segment(aes(
    x = 0.18,
    y = 0.873,
    xend = 0.47,
    yend = 0.873
  ),
  arrow = arrow(length = unit(0.2, "cm"),
                type = "closed"))

ggsave(
  "WSJ_GDP_forecast.png",
  dpi = 600,
  bg = "white",
  scale = 1,
  width = 7,
  height = 10,
  device = agg_png
)

