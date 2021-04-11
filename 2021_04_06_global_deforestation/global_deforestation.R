library(tidytuesdayR)
library(tidyverse)
library(scales)
library(rnaturalearth)
library(rnaturalearthdata)


tt <- tt_load("2021-04-06")
world <- ne_countries(scale = "medium", returnclass = "sf")

wmap <- world %>% select(iso_a3)


forest_area <- tt$forest_area %>%
  mutate(forest_area_bin = case_when(
    forest_area < 0.25 ~ "0.25%",
    forest_area < 0.5 ~ "0.5%",
    forest_area < 0.75 ~ "0.75%",
    forest_area < 1 ~ "1%",
    forest_area < 2.5 ~ "2.5%",
    forest_area < 5 ~ "5%",
    forest_area < 10 ~ "10%",
    forest_area > 10 ~ ">10%",
    TRUE ~ "0%")) %>%
  mutate(forest_area_bin = fct_relevel(forest_area_bin, "0.25%", "0.5%",
                                       "0.75%", "1%", "2.5%", "5%",
                                       "10%", ">10%"))

forest_area %>%
  filter(year == 2020, !is.na(code)) %>%
  full_join(wmap, by = c("code" = "iso_a3")) %>%
  na.omit() %>%
  ggplot() +
  geom_sf(aes(fill = forest_area_bin, geometry = geometry),
          color = "black") +
  scale_fill_manual(
    labels = c("0%-0.25%", "0.25%-0.50%", "0.50%-0.75%", "0.75%-1%", "1%-2.5%",
               "2.5%-5%", "5%-10%", ">10%"),
    values = c("#ffffe5", "#f7fcb9", "#d9f0a3", "#addd8e",
               "#78c679", "#41ab5d", "#238443", "#005a32")
  ) +
  labs(fill = "% Forest Area",
       title = "Share of Global Forest Area, 2020") +
  theme_map() +
  ggsave("global_forest_share.png", width = 10, height = 6)
