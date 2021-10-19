###--------------------------------------------------
### Germany 
###--------------------------------------------------

germany <- read_table(paste0("DEUTNP.Mx_1x1.txt"), skip = 2, na = ".") %>% clean_names()
germany$age <- as.integer(recode(germany$age, "110+" = "110"))

germany <- germany %>% mutate(ratio = male / female,
                            deciles = cut(ratio, breaks = quantile(ratio, probs = seq(0, 1, 0.1), na.rm = TRUE)),
                            pct_diff = ((male - female) / (male + female))*100)


pfemale <- ggplot(subset(germany, age < 101), aes(x = year, y = age, fill = ntile(female, 100)))
pfemale_out <- pfemale + geom_raster() +
  scale_fill_viridis_c(option = "F", direction = -1) +
  scale_x_continuous(breaks = seq(1990, 2017, by = 5)) +
  guides(fill = guide_legend(nrow = 1, title.position = "top", label.position = "bottom")) +
  ylim(c(0, 100)) +
  labs(x = "Year", y = "Age", fill = "Death Rate Percentile",
       title = "Female") +
  theme(legend.position = "top",
        legend.title = element_text(size = 8)) +
  theme_light()

pfemale_out

###--------------------------------------------------
### Germany male
###--------------------------------------------------
pmale <- ggplot(data = subset(germany, age < 101), 
                mapping = aes(x = year, 
                              y = age, fill = ntile(male, 100)))
pmale_out <- pmale + geom_raster() +
  scale_fill_viridis_c(option = "F", direction = -1) +
  scale_x_continuous(breaks = seq(1820, 2015, by = 5)) +
  ylim(c(0, 100)) +
  guides(fill = FALSE) +
  labs(x = "Year", y = "Age", fill = "Male Death Rate Percentile",
       title = "Male") +
  theme_light()

pmale_out

germany_mortality <- ggarrange(pfemale_out, pmale_out,
                              ncol = 1, nrow = 2,
                              common.legend = TRUE,
                              legend = "bottom")

