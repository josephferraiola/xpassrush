library(tidyverse)

pressures <- pffScoutingData %>% 
  left_join(df_plays, by = c("gameId", "playId")) %>% 
  select(gameId, playId, defensiveTeam, pressure_caused, is_penalty, down) %>% 
  filter(is_penalty == 0, down != 0) %>% 
  group_by(defensiveTeam, gameId, playId) %>% 
  summarize(pressure = max(pressure_caused, na.rm = T))

df_plays1 <- df_plays %>% 
  left_join(fastr_pbp, by = c("gameId", "playId")) %>% 
  left_join(pressures, by = c("gameId", "playId", "defensiveTeam"))

num_pass_rushers_table <- df_plays1 %>% 
  filter(down != 0, is_penalty == 0) %>% 
  select(defensiveTeam, 32:63) %>% 
  mutate(zone_blitz = ifelse(pff_passCoverageType == "Zone" & blitz == 1, 1, 0))

total_plays_team <- num_pass_rushers_table %>% 
  group_by(defensiveTeam) %>% 
  summarize(total_plays = n())

num_pass_rushers_table %>% 
  group_by(defensiveTeam, num_pass_rushers) %>% 
  summarize(n_plays = n(),
            epa = mean(epa),
            pressure_rate = mean(pressure, na.rm = T)) %>% 
  left_join(total_plays_team, by = c("defensiveTeam")) %>% 
  mutate(Pct = n_plays / total_plays * 100) %>% filter(defensiveTeam == "ATL")

#different table 
#slot blitz %, blitz look %, sim pressure%, overload look %, zone blitz

# remember to get pressure rate
num_pass_rushers_table %>%
  group_by(defensiveTeam) %>% 
  #filter(sim_pressure == 1) %>% 
  summarize(n_plays = n(),
            epa = mean(epa),
            pressure_rate = mean(pressure, na.rm = T),
            sim_pressure_rate = mean(sim_pressure) * 100,
            slot_blitz = mean(team_slot_blitz, na.rm = T) * 100) %>% 
  arrange(desc(slot_blitz)) %>% view()

# NFL Average
total_plays_nfl <- num_pass_rushers_table %>% 
  group_by() %>% 
  summarize(total_plays = n())

num_pass_rushers_table %>% 
  group_by(num_pass_rushers) %>% 
  summarize(n_plays = n(),
            epa = mean(epa),
            pressure_rate = mean(pressure, na.rm = T)) %>% 
  merge(total_plays_nfl) %>% 
  mutate(Pct = n_plays / total_plays * 100)


####### ATL SIM PRESSURE ANALYSIS

ATL_sim_pressures <- df_plays1 %>% 
  select(defensiveTeam, sim_pressure, epa, success, down, pressure, yardsToGo) %>% 
  filter(sim_pressure == 1,
         defensiveTeam == "ATL",
         down != 0) %>% 
  mutate(distance_bins = ifelse(yardsToGo >= 8, "Long (8+)",
                                ifelse(yardsToGo <= 7 & yardsToGo >= 3, "Medium (3-7)",
                                       ifelse(yardsToGo <= 2, "Short (<2)", NA)))) %>% 
  group_by(defensiveTeam, down, distance_bins) %>% 
  summarise(n_plays = n(),
            epa = mean(epa),
            pressure_rate = mean(pressure),
            success = mean(success)) %>% 
  arrange(desc(n_plays))

df_plays1 %>% 
  filter(down != 0, is_penalty == 0, sim_pressure == 1) %>% 
  group_by() %>% 
  summarize(n = n()) %>% 
  arrange(desc(n))

df_plays1 %>% 
  filter(down != 0, is_penalty == 0, sim_pressure == 1, defensiveTeam == "ATL") %>% 
  select(gameId, playId, defensiveTeam, sim_pressure, team_slot_blitz, epa, down, yardsToGo) %>% 
  view()

# sim pressure
simpressure <- df_plays %>% 
  select(gameId, playId, sim_pressure)

defenders_prior_to_snap_joined <- defenders_prior_to_snap_joined %>% 
  left_join(simpressure, by = c("gameId", "playId"))

defenders_prior_to_snap_joined %>%
  group_by(nflId, displayName, primary_position, pff_positionLinedUp, defensiveTeam, sim_pressure) %>% 
  filter(defensiveTeam == "ATL", displayName %in% c("Deion Jones", "Isaiah Oliver", "Foyesade Oluokun")) %>% 
  summarise(plays = n(),
            pass_rushes = sum(is_pass_rusher),
            pass_rush_pct = (pass_rushes / plays),
            avg_xPR = mean(xPassRush),
            exp_rushes = sum(xPassRush),
            passrushed_oe_sum = sum(PROE),
            passrushed_oe_mean = mean(PROE),
            mse = mean(mse),
            avg_crept_dis = mean(crept_dis),
            distFromLOS = mean(distFromLOS),
            abs_y_dis_from_ball = mean(absolute_y_dist_from_ball),
            dir = mean(dir),
            pressures = sum(pressure_caused)) %>% 
  filter(plays > 0) %>%  view()

defenders_prior_to_snap_joined %>%
  group_by(nflId, displayName, primary_position, defensiveTeam, sim_pressure, is_pass_rusher) %>% 
  filter(defensiveTeam == "ATL", primary_position == "EDGE", down != 0, is_penalty == 0) %>% 
  summarise(plays = n(),
            pass_rushes = sum(is_pass_rusher),
            pass_rush_pct = (pass_rushes / plays),
            avg_xPR = mean(xPassRush),
            exp_rushes = sum(xPassRush),
            passrushed_oe_sum = sum(PROE),
            passrushed_oe_mean = mean(PROE),
            mse = mean(mse),
            avg_crept_dis = mean(crept_dis),
            distFromLOS = mean(distFromLOS),
            abs_y_dis_from_ball = mean(absolute_y_dist_from_ball),
            dir = mean(dir),
            pressures = sum(pressure_caused)) %>% 
  filter(plays > 0) %>%  view()

blockerIds <- pffScoutingData %>% 
  select(gameId, playId, nflId, displayName, pff_positionLinedUp, pff_nflIdBlockedPlayer, blocked_player, blocked_player_posLinedUp) %>% 
  rename(blockerId = nflId, 
         blocker_name = displayName,
         blocker_position = pff_positionLinedUp,
         nflId = pff_nflIdBlockedPlayer, 
         displayName = blocked_player, 
         pff_positionLinedUp = blocked_player_posLinedUp)

defenders_prior_to_snap_joined1 <- defenders_prior_to_snap_joined %>% 
  select(gameId, playId, nflId, displayName, defensiveTeam, is_pass_rusher, xPassRush, mse, pressure_caused, sim_pressure) %>%
  left_join(blockerIds, by = c("gameId", "playId", "nflId", "displayName")) %>% 
  filter(defensiveTeam == "ATL", sim_pressure == 1, blocker_position %in% c("TE", "TE-L", "TE-R", "TE-oL", "TE-iL", "TE-oR", "TE-iR"))







