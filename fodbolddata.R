install.packages("rjson",  dependencies = T)
library(jsonlite)
library(ggplot2)
library(dplyr)

file_path <- "/Users/Helenejensen/Documents/Dataanalyse/Fodbolddata/evt_5360157.json"
fbdata <- fromJSON(file_path, flatten = T)

fb <- as.data.frame(fbdata[["events"]])

optæl <- as.data.frame(table(fb$type.primary))
str(optæl)

colnames(optæl) <- c("Type", "Frequency")

ggplot(optæl, aes(x = reorder(Type, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity") +  # Brug "identity" for at bruge værdierne direkte
  theme_minimal() +                                 # Minimal tema for et rent layout
  labs(title = "Barplot af Type Primary",           # Tilføj en titel
       x = "Type Primary",                          # Label for x-aksen
       y = "Frekvens") +                            # Label for y-aksen
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


fb_pass <- fb %>% 
  filter(type.primary == "pass")

fb_shot <- fb %>% 
  filter(type.primary == "shot")

t <- fb_shot %>%
  filter(!is.na(shot.onTarget)) %>%  # Sørg for at der ikke er NA-værdier i shot.onTarget
  group_by(team.name, shot.onTarget) %>%
  tally(name = "shotcount", sort = TRUE)



passes <- fb$type.primary == "pass"






passes_on_opponent_half <- fb %>% filter(type.primary == "pass") %>% 
  group_by(period, team.name) %>% 
  summarise(totpassp)



<- fb %>%
  filter(type.primary == "pass", location.x > 50) %>%     # Filtrer afleveringer på modstanderhalvdelen
  group_by(team_name = team.name) %>%                    # Gruppér efter holdnavn
  summarise(num_passes = n(), .groups = "drop") %>%      # Optæl og fjern grupperingsattribut
  arrange(desc(num_passes)) 


