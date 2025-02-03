library(mongolite)
library(tidyverse)
library(ggplot2)
library(jsonlite)

con <- mongo(
 url = "mongodb://localhost",
 db = "soccer",
 collection = "matches"
)

dfmatches <- con$find(query = "{}",
         fields = "{}")

  # find home
dfmatches$home <- sub(" -.*", "", dfmatches$label)

  # away
dfmatches$away <- str_extract(dfmatches$label, "(?<=- )[^(,]+")

  # homegoal
dfmatches$homegoal <- str_extract(dfmatches$label, "(?<=, )\\d+")

  # awaygoal
dfmatches$awaygoal <- str_extract(dfmatches$label, "(?<=-)\\d+")

  # Find alle Ajax’ kampe
df_ajax <- dfmatches %>% filter(grepl("Ajax", label))

  # Herunder stats på sejre hjemme vs ude
  # ny variabel win, lose, draw
homestat <- df_ajax %>% mutate(
  result = case_when(
    homegoal > awaygoal ~ "win",    # Hjemmeholdet vinder
    homegoal < awaygoal ~ "lost",   # Udeholdet vinder
    homegoal == awaygoal ~ "draw"   # Uafgjort
  )
) %>%
  
  # Vælg kun de nødvendige kolonner
  select(label, homegoal, awaygoal, result)

  # pivot longer for grupperede søjler i plot
homestat_long <- homestat %>% 
  pivot_longer(cols = c(homegoal, awaygoal),
               names_to = "GoalType", values_to = "Goals") %>%
  mutate(Goals = as.numeric(Goals),  # Sikre at Goals er numerisk
         cat = case_when(result == "win" ~ "win",
                         result == "lost" ~ "lost",
                         result == "draw" ~ "draw")) %>% 
  group_by(cat, GoalType) %>% 
  summarise(TotalGoals = sum(Goals))

  # plot
ggplot(homestat_long, aes(x = cat, y = TotalGoals, fill = GoalType)) +
  geom_bar(stat = "identity", position = "dodge") +  # Grupperede søjler
  scale_fill_manual(values = c("homegoal" = "blue", "awaygoal" = "red")) +  # Farver
  labs(title = "Total mål for Ajax: Hjemme vs. Ude (Win, Draw, Lost)",
       x = "Resultat",
       y = "Total mål",
       fill = "Goal Type") +
  theme_minimal()


  # Lav en liste af matchid’s
cong <- mongo(
  url = "mongodb://localhost",
  db = "soccer",
  collection = "games"
)


idv=df_ajax[,'_id']
idvt=idv[1:3]
query = jsonlite::toJSON(list(`_id` = list(`$in` = idvt)), auto_unbox = TRUE)
result=cong$find(query, fields = '{}')

reslist <- result$events
resdf <- bind_rows(reslist)
resdf <- fromJSON(toJSON(resdf), flatten = T)

  # aggreger
df_pass <- resdf %>% 
  filter(type.primary == "pass")

ajax_pass <- df_pass %>% 
  filter(team.name == "Ajax")

pass_table <- as.data.frame(table(ajax_pass$pass.accurate))


ajax_pass <- ajax_pass %>%
  mutate(angle_category = case_when(
    pass.angle > 0 ~ "Fremad",
    pass.angle < 0 ~ "Bagud",
    TRUE ~ "Zero"
  ))

ajax_pass$angle_category

ajax_angle <- ajax_pass %>%
  group_by(matchId, angle_category) %>%
  summarise(count = n(), .groups = "drop")





