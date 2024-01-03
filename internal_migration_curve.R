
# Separating Tallinn and merging other municipalities into counties.
# creating a flow map to present internal migration in Estonia

# R version 4.0.5

library(dplyr)
library(sf)
library(ggplot2)
library(ggthemes)
library(pxweb)



# import data via px_web

px_query_list <- list("Aasta" = c("*"),
                      "Sugu" = c("*"),
                      "Lähtemaakond" = c("*"))


px_andmed_list1 <- pxweb_get(url = "https://andmed.stat.ee/api/v1/et/stat/RVR07",
                             query = px_query_list)


px_df_list1 <- as.data.frame(px_andmed_list1, column.name.type = "text", variable.value.type = "text")

# prepare the variables
data <- px_df_list1 %>% filter(Aasta == 2022 & Sugu == "Mehed ja naised",
                               Sihtmaakond == "..Tallinnasse") %>% 
  rename(Year = Aasta,
         Start = `Lähtemaakond`,
         number_of_people = `RVR07: MAAKONDADEVAHELINE RÄNNE`) %>% 
  mutate(Destination = "Tallinn",
         Start = str_replace(Start, "maakonnast", "maakond"), # replacind the word 'maakonnast' with 'maakond'
         Start = case_when(Start == "..Tallinnast" ~ "Tallinn",
                           TRUE ~ Start)) %>% 
  select(-c(Sugu, Sihtmaakond))


# Estonian municipalities as Omavalitsus SHP from https://geoportaal.maaamet.ee/est/Ruumiandmed/Haldus-ja-asustusjaotus-p119.html

municipalities <- st_read("kaart/omavalitsus_SHP/omavalitsus.shp")

# merge all the municipalieties into counties except for tallinn
counties_and_tallinn <- municipalities %>% 
  mutate(County = case_when(ONIMI == "Tallinn" ~ "Tallinn",
                          TRUE ~ MNIMI)) %>% 
  group_by(County) %>% 
  summarize(geometry = st_union(geometry))

# st_crs(counties_and_tallinn)
# 
# ggplot(counties_and_tallinn)+
#   geom_sf()
# 
# # to save it
# st_write(counties_and_tallinn, "counties_and_tallinn.shp")

# join data table and geometry
counties_and_tallinn2 <- counties_and_tallinn %>% left_join(data, by=c("County" = "Start"))



# coordinates for all the records (counties and tallinn)

coords <- counties_and_tallinn2 %>% 
  mutate(center = st_centroid(geometry)) %>%
  mutate(coordinates = st_coordinates(center)) %>%
  mutate(longitude = coordinates[, "X"],
         latitude = coordinates[, "Y"])


# filtering out Tallinn's coordinates 

tln_coords <- coords %>% 
  filter(County == "Tallinn") %>% 
  select(longitude, latitude) %>%
  unlist()


# finding coordinates for all the other counties

data_for_the_map <- coords %>% 
  filter(County != "Tallinn") %>% 
  mutate(xend = tln_coords[1], yend = tln_coords[2])


ggplot()+
  geom_sf(data = counties_and_tallinn2, fill="red")+ # map with Tallinn
  geom_sf(data = data_for_the_map, aes(fill=number_of_people), lwd = 0.4)+ #  line width = 0.4
  scale_fill_continuous(low = "#F7FFE5" , high ="#A0C49D", na.value="#F8F8F8",
                      trans = "log",
                      breaks = c(100, 500, 1000, 3000), labels = c(100, 500, 1000, 3000),
                      "Number of People")+ # legend title
  theme_map() +
  # curve adjustments
  geom_curve(data = data_for_the_map, aes(x=longitude, y=latitude, xend=xend, yend=yend), 
             color = "gray50",
             curvature = 0.2,
             arrow = arrow(length = unit(3, "pt"), type = "open"),
             alpha = 0.5,
             linewidth = 1)+
  # data labels
  geom_label(data = data_for_the_map, aes(x=longitude, y=latitude, label=number_of_people),
    # nudge_x=0.45, nudge_y=-0.5,
    # check_overlap=TRUE, # not supported by geom_label
    label.padding=unit(0.4, "lines"),
    # label.size=0.2,
    color="white",
    fill="#038225",
    alpha = 0.5,    # vjust = 1.5, hjust = 1.5, size = 3
    size = 4
  )+
  # title and caption
  labs(title = "Internal Migration to Tallinn from Estonian Counties 2022",
       caption = "Data source: Statistics Estonia")+
  # size and position of legend, and title and caption size
  theme(legend.position = c(1.0, 0.3),
        legend.key.size = unit(1, 'cm'),
        plot.title = element_text(size = 16),
        plot.caption = element_text(size = 10))




