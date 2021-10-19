library(tidyverse)
library(janitor)
library(showtext)
library(ggplot2)
library(ggpubr)


###--------------------------------------------------
### USA Female
###--------------------------------------------------

usa <- read_table(paste0("USA.Mx_1x1.txt"), skip = 2, na = ".") %>% clean_names()
usa$age <- as.integer(recode(usa$age, "110+" = "110"))

usa <- usa %>% mutate(ratio = male / female,
                          deciles = cut(ratio, breaks = quantile(ratio, probs = seq(0, 1, 0.1), na.rm = TRUE)),
                          pct_diff = ((male - female) / (male + female))*100)


pfemale <- ggplot(subset(usa, age < 101), aes(x = year, y = age, fill = ntile(female, 100)))
pfemale_out <- pfemale + geom_raster() +
  scale_fill_viridis_c(option = "F", direction = -1) +
  scale_x_continuous(breaks = seq(1933, 2019, by = 10)) +
  guides(fill = guide_legend(nrow = 1, title.position = "top", label.position = "bottom")) +
  ylim(c(0, 100)) +
  labs(x = "Year", y = "Age", fill = "Death Rate Percentile",
       title = "Female") +
  theme(legend.position = "top",
        legend.title = element_text(size = 8)) +
  theme_light()

pfemale_out

###--------------------------------------------------
### usa male
###--------------------------------------------------
pmale <- ggplot(data = subset(usa, age < 101), 
                mapping = aes(x = year, 
                              y = age, fill = ntile(male, 100)))
pmale_out <- pmale + geom_raster() +
  scale_fill_viridis_c(option = "F", direction = -1) +
  scale_x_continuous(breaks = seq(1933, 2019, by = 10)) +
  ylim(c(0, 100)) +
  guides(fill = FALSE) +
  labs(x = "Year", y = "Age", fill = "Death Rate Percentile",
       title = "Male") +
  theme_light()

pmale_out

usa_mortality <- ggarrange(pfemale_out, pmale_out,
                             ncol = 1, nrow = 2,
                             common.legend = TRUE,
                             legend = "bottom")
