library(tidyverse)
install.packages("janitor")
library(janitor)
install.packages("showtext")
library(showtext)
showtext_auto()
library(ggplot2)
library(ggpubr)

setwd("~/Desktop/Demography R/mortality/death_rates")
## Make a "figures" subdirectory in the working directory if one
## doesn't already exist

ifelse(!dir.exists(file.path("figures")),
       dir.create(file.path("figures")),
       FALSE)

###--------------------------------------------------
### France female 
###--------------------------------------------------

france <- read_table(paste0("FRATNP.Mx_1x1.txt"), skip = 2, na = ".") %>% clean_names()
france$age <- as.integer(recode(france$age, "110+" = "110"))

france <- france %>% mutate(ratio = male / female,
                            deciles = cut(ratio, breaks = quantile(ratio, probs = seq(0, 1, 0.1), na.rm = TRUE)),
                            pct_diff = ((male - female) / (male + female))*100,
                            bin_ratio = ntile(ratio, 100)) # binned by percentile


pfemale <- ggplot(subset(france, age < 101), aes(x = year, y = age, fill = ntile(female, 100)))
pfemale_out <- pfemale + geom_raster() +
  scale_fill_viridis_c(option = "F", direction = -1) +
  scale_x_continuous(breaks = seq(1820, 2015, by = 15)) +
  guides(fill = guide_legend(nrow = 1, title.position = "top", label.position = "bottom")) +
  ylim(c(0, 100)) +
  labs(x = "Year", y = "Age", fill = "Death Rate Percentile",
       title = "Female") +
  theme(legend.position = "top",
        legend.title = element_text(size = 8)) +
  theme_light()

pfemale_out
ggsave("figures/france_women.png", p_out, height = 8, width = 12)
ggsave("figures/france_women.pdf", p_out, height = 8, width = 12)

###--------------------------------------------------
### France male
###--------------------------------------------------
pmale <- ggplot(data = subset(france, age < 101), 
            mapping = aes(x = year, 
                          y = age, fill = ntile(male, 100)))
pmale_out <- pmale + geom_raster() +
  scale_fill_viridis_c(option = "F", direction = -1) +
  scale_x_continuous(breaks = seq(1820, 2015, by = 15)) +
  ylim(c(0, 100)) +
  guides(fill = FALSE) +
  labs(x = "Year", y = "Age", fill = "Male Death Rate Percentile",
       title = "Male") +
  theme_light()

pmale_out

France_mortality <- ggarrange(pfemale_out, pmale_out,
                              ncol = 1, nrow = 2,
                              common.legend = TRUE,
                              legend = "bottom")

