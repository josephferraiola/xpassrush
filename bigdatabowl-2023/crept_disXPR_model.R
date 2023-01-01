library(tidyverse)
library(tidymodels)
library(usemodels)
library(textrecipes)
library(vip)
library(nflfastR)
library(ggimage)
library(xgboost)
library(caret)
options(scipen = 999)


# processing the data

##Reading in the non tracking data
df_games <- read_csv("data/games.csv")

df_games <- df_games %>%
  select(gameId, season, week, homeTeamAbbr, visitorTeamAbbr)

df_players <- read_csv("data/players.csv")

df_players <- df_players %>%
  select(nflId, displayName, officialPosition)

df_tracking <- read_tracking_data() %>%
  process_tracking_data()

#create line of scrimmage using ball data
line_of_scrimmage <- df_tracking %>%
  filter(team == "football", frameId == 1) %>%
  select(gameId, playId, x) %>%
  rename(los = x)

df_tracking <- df_tracking %>%
  left_join(line_of_scrimmage, by = c("playId", "gameId"))

df_tracking <- df_tracking %>%
  mutate(distFromLOS = x - los,
         distFromMid = y - 26.65)

ball_df <- df_tracking %>% 
  filter(team == "football") %>% 
  select(gameId, playId, frameId, x, y) %>% 
  rename(ball_x = x,
         ball_y = y)

df_tracking <- df_tracking %>% 
  left_join(ball_df, by = c("playId", "gameId", "frameId"))

df_tracking <- df_tracking %>% 
  mutate(distFromBall = sqrt((x - ball_x) ^ 2 + (y - ball_y) ^ 2),
         y_dis_from_ball = ball_y - y)

df_plays <- read_csv("data/plays.csv") %>%
  process_plays_data()

pffScoutingData <- read_csv("data/pffScoutingData.csv")

# create a is pass rusher column to better summarize the data and have a binary variable for the expected pass rusher model
pffScoutingData <- pffScoutingData %>% 
  mutate(is_pass_rusher = ifelse(pff_role == "Pass Rush", 1,0),
         is_coverage = ifelse(pff_role == "Coverage", 1,0),
         pressure_caused = ifelse(pff_hit | pff_hurry | pff_sack == 1, 1, 0),
         pressure_allowed = ifelse(pff_hitAllowed | pff_hurryAllowed | pff_sackAllowed == 1, 1, 0))

# number of pass rushers column to later create is blitz column
num_pass_rushers_df <- pffScoutingData %>% 
  select(gameId, playId, is_pass_rusher, is_coverage) %>% 
  group_by(gameId, playId) %>% 
  summarise(num_pass_rushers = sum(is_pass_rusher),
            num_cov_players = sum(is_coverage))

df_plays <- df_plays %>% 
  left_join(num_pass_rushers_df, by = c("gameId", "playId"))

# create a blitz column (5 or more pass rushers) and 5-man pressure column
df_plays <- df_plays %>% 
  mutate(blitz = ifelse(num_pass_rushers >= 5, 1, 0),
         five_man_pressure = ifelse(num_pass_rushers == 5, 1, 0))

# get a player's highest played pff_positionLinedUp

pffScoutingData <- pffScoutingData %>% 
  left_join(df_players, by = c("nflId")) %>% 
  mutate(general_position = ifelse(pff_positionLinedUp %in% c("RLB", "RILB", "MLB", "LILB", "LLB"), "Off-Ball LB",
                                   ifelse(pff_positionLinedUp %in% c("FSR", "FS", "FSL", "SSR", "SS", "SSL"), "Safety",
                                          ifelse(pff_positionLinedUp %in% c("RCB", "LCB", "SCBR", "SCBoR", "SCBiR", "SCBL", "SCBoL", "SCBiL"), "CB",
                                                 ifelse(pff_positionLinedUp %in% c("ROLB", "LOLB", "REO", "LEO", "RE", "LE"), "EDGE",
                                                        ifelse(pff_positionLinedUp %in% c("DRT", "DLT", "NRT", "NT", "NLT"), "IDL", pff_positionLinedUp))))))
position_aligned <- pffScoutingData %>% 
  select(gameId, playId, nflId, displayName, general_position) %>% 
  group_by(nflId, displayName, general_position) %>% 
  add_count(general_position) %>% 
  rename(snaps_aligned_as = n) %>% 
  select(-gameId, -playId) %>% 
  distinct(nflId, displayName, .keep_all = TRUE)

total_plays <- pffScoutingData %>% 
  select(gameId, playId, nflId, displayName) %>% 
  group_by(nflId, displayName) %>% 
  summarise(snaps = n())

position_aligned <- position_aligned %>% 
  left_join(total_plays, by = c("nflId", "displayName")) %>% 
  mutate(aligned_as_pct = snaps_aligned_as / snaps * 100) %>% 
  group_by(nflId, displayName, general_position) %>% 
  filter(aligned_as_pct > 50) %>% 
  select(-snaps, -snaps_aligned_as) %>% 
  rename(primary_position = general_position)

pffScoutingData <- pffScoutingData %>% 
  left_join(position_aligned, by = c("nflId", "displayName"))

pffScoutingData <- pffScoutingData %>% 
  mutate(primary_position = ifelse(!is.na(primary_position), primary_position, officialPosition))

# some players need primary position to be done this tedious way
pffScoutingData$primary_position <- gsub("DT", "IDL", pffScoutingData$primary_position)
pffScoutingData$primary_position <- gsub("DE", "EDGE", pffScoutingData$primary_position)
pffScoutingData$primary_position <- gsub("OLB", "EDGE", pffScoutingData$primary_position)
pffScoutingData$primary_position <- gsub("FS", "Safety", pffScoutingData$primary_position)
pffScoutingData$primary_position <- gsub("SS", "Safety", pffScoutingData$primary_position)
pffScoutingData$primary_position <- gsub("MLB", "Off-Ball LB", pffScoutingData$primary_position)
pffScoutingData$primary_position <- gsub("ILB", "Off-Ball LB", pffScoutingData$primary_position)

# matchups for blocked players

pass_rushers <- pffScoutingData %>% 
  filter(pff_role == "Pass Rush") %>% 
  select(gameId, playId, nflId, displayName, pff_positionLinedUp, officialPosition) %>% 
  rename(pff_nflIdBlockedPlayer = nflId,
         blocked_player = displayName,
         blocked_player_posLinedUp = pff_positionLinedUp,
         blocked_player_official_pos = officialPosition)

pffScoutingData <- pffScoutingData %>% 
  left_join(pass_rushers, by = c("pff_nflIdBlockedPlayer", "gameId", "playId"))

# create a column for slot CB blitzes
cb_blitz <- pffScoutingData %>% 
  select(gameId, playId, nflId, is_pass_rusher, pff_positionLinedUp) %>% 
  left_join(df_players, by = c("nflId")) %>% 
  filter(officialPosition == "CB")

cb_blitz <- cb_blitz %>% 
  filter(is_pass_rusher == 1) %>% 
  mutate(cb_blitz_type = ifelse(pff_positionLinedUp %in% c("RCB", "LCB"), "wide blitz",
                                "slot blitz"))

cb_blitz <- cb_blitz %>% 
  mutate(slot_blitz = ifelse(cb_blitz_type == "slot blitz", 1, 0),
         wide_blitz = ifelse(cb_blitz_type == "wide blitz", 1, 0)) %>% 
  select(gameId, playId, nflId, cb_blitz_type, slot_blitz, wide_blitz)

# join cb blitzes to the individual player level in pffScoutingData
pffScoutingData <- pffScoutingData %>% 
  left_join(cb_blitz, by = c("gameId", "playId", "nflId"))

# get slot blitz and wide blitz column on a play level to later show percentage of time teams use these blitz type
play_level_cb_blitz <- cb_blitz %>% 
  select(gameId, playId, slot_blitz, wide_blitz) %>% 
  group_by(gameId, playId) %>% 
  summarise(num_slot_blitzers = sum(slot_blitz),
            num_wide_blitzers = sum(wide_blitz)) %>% 
  mutate(team_slot_blitz = ifelse(num_slot_blitzers >= 1, 1, 0),
         team_wide_blitz = ifelse(num_wide_blitzers >= 1, 1, 0))

df_plays <- df_plays %>% 
  left_join(play_level_cb_blitz, by = c("gameId", "playId"))

# change NAs to 0
df_plays <- df_plays %>% 
  mutate(num_slot_blitzers = ifelse(is.na(num_slot_blitzers), 0, num_slot_blitzers),
         num_wide_blitzers = ifelse(is.na(num_wide_blitzers), 0, num_wide_blitzers),
         team_slot_blitz = ifelse(is.na(team_slot_blitz), 0, team_slot_blitz),
         team_wide_blitz = ifelse(is.na(team_wide_blitz), 0, team_wide_blitz))

################################################################################

joined_df <- df_games %>% 
  left_join(df_plays, by = c("gameId"))

fastr_pbp <- readRDS("data/play_by_play_2021.rds") %>% 
  rename(playId = play_id, 
         gameId = old_game_id) %>% 
  filter(week <= 8) %>% 
  select(playId, gameId, epa, wp, wpa, xpass, success) %>% 
  mutate(gameId = as.numeric(gameId))


joined_df <- joined_df %>%
  left_join(fastr_pbp, by = c("playId", "gameId"))


joined_df <- joined_df %>% 
  left_join(df_tracking, by = c("gameId" = "gameId",
                                "playId" = "playId"))

joined_df <- joined_df %>% 
  left_join(pffScoutingData, by = c("nflId" = "nflId",
                                    "gameId" = "gameId",
                                    "playId" = "playId"))

joined_df <- joined_df %>%
  mutate(sideOfBall = ifelse(((team == homeTeamAbbr) &
                                (possessionTeam == homeTeamAbbr)
  ) |
    ((team == visitorTeamAbbr) &
       (possessionTeam == visitorTeamAbbr)
    ),
  "offense",
  "defense"))

# creating blitz look columns

blitz_look_data <- joined_df %>% 
  select(playDescription, gameId, playId, nflId, displayName, officialPosition, pff_role, is_pass_rusher,
         pff_positionLinedUp, frameId, event, x, y, o, a, dir, dis, los, distFromLOS, 
         num_pass_rushers, blitz, cb_blitz_type, slot_blitz, time, team)

blitz_look_snap <- blitz_look_data %>% 
  group_by(gameId, playId, nflId) %>%
  filter(cumsum(event %in% c("ball_snap")) == 0) %>% 
  filter(frameId == max(frameId)) %>% 
  mutate(near_los = ifelse(distFromLOS < 2.0, 1, 0),
         box_pos = ifelse(pff_positionLinedUp %in% c("REO", "RE", "DRT", "NRT", "NT", "NLT", "DLT", "LE", "LEO",
                                                     "ROLB", "RLB", "RILB", "MLB", "LILB", "LLB", "LOLB"), 1, 0),
         blitz_look = ifelse(near_los == 1 & box_pos == 1, 1, 0))

frameid_1 <- blitz_look_data %>% 
  filter(frameId == 1) %>% 
  select(gameId, playId, nflId, displayName, x) %>% 
  rename(frame1_x = x)

pass_forward <- blitz_look_data %>% 
  filter(event %in% c("pass_forward", "qb_sack", "qb_strip_sack", "run")) %>% 
  select(gameId, playId, nflId, displayName, x) %>%
  rename(moment_pass_or_sack_x = x)

blitz_look_dist <- blitz_look_snap %>% 
  left_join(frameid_1, by = c("gameId", "playId", "nflId", "displayName")) %>% 
  left_join(pass_forward, by = c("gameId", "playId", "nflId", "displayName"))

blitz_look_dist <- blitz_look_dist %>% 
  mutate(crept_dis = frame1_x - x,
         postsnap_depth = x - moment_pass_or_sack_x)

# for individual players distances covered prior to snap and post snap coverage depth
blitz_look_coverage_snaps <- blitz_look_dist %>% 
  filter(blitz_look == 1,
         pff_role == "Coverage")

blitz_summary <- blitz_look_coverage_snaps %>% 
  filter(blitz_look == 1) %>% 
  group_by(nflId, displayName) %>% 
  summarise(blitz_looks = sum(blitz_look),
            avg_crept_dis = mean(crept_dis),
            avg_postsnap_depth = mean(postsnap_depth))

# get blitz look column on a play level
play_level_blitz_looks <- blitz_look_dist %>% 
  select(gameId, playId, blitz_look) %>% 
  group_by(gameId, playId) %>% 
  summarise(num_players_blitz_look = sum(blitz_look)) %>% 
  mutate(team_blitz_look = ifelse(num_players_blitz_look >= 5, 1, 0))

df_plays <- df_plays %>% 
  left_join(play_level_blitz_looks, by = c("gameId", "playId"))

# creating sim pressure column 

los_players <- pffScoutingData %>% 
  filter(primary_position %in% c("EDGE", "IDL")) %>% 
  select(gameId, playId, is_pass_rusher, is_coverage) %>% 
  group_by(gameId, playId) %>% 
  summarise(los_pass_rushers = sum(is_pass_rusher),
            los_coverage = sum(is_coverage))

back_seven_players <- pffScoutingData %>% 
  filter(primary_position %in% c("CB", "Safety", "Off-Ball LB")) %>% 
  select(gameId, playId, is_pass_rusher, is_coverage) %>% 
  group_by(gameId, playId) %>% 
  summarise(back_seven_pass_rushers = sum(is_pass_rusher),
            back_seven_coverage = sum(is_coverage))

sim_pressures <- num_pass_rushers_df %>% 
  left_join(los_players, by = c("gameId", "playId")) %>% 
  left_join(back_seven_players, by = c("gameId", "playId"))

sim_pressures1 <- sim_pressures %>% 
  mutate(sim_pressure = ifelse(num_cov_players == 7 & los_coverage >= 1 & back_seven_pass_rushers >= 1, 1, 0))

df_plays <- df_plays %>% 
  left_join(sim_pressures1, by = c("gameId", "playId", "num_pass_rushers", "num_cov_players"))

# creating overload looks & overload rushes using side of center

# get center coordinates
C_coordinates <- joined_df %>%
  filter(pff_positionLinedUp == "C") %>%
  select(gameId, playId, x, y, frameId) %>%
  rename(xCenter = x,
         yCenter = y)

#join the C coordinates with joined_df
joined_df <- joined_df %>%
  left_join(C_coordinates, by = c("playId", "gameId", "frameId"))

# Overload Blitz and Overload Blitz Looks
left_right_of_center <- joined_df %>% 
  filter(event == "ball_snap") %>% 
  select(gameId, playId, nflId, displayName, officialPosition, primary_position, pff_positionLinedUp, is_pass_rusher, los, distFromLOS, distFromMid, x, y, yCenter, sideOfBall) %>% 
  mutate(sideOfCenter = ifelse(y > yCenter, "left", "right"),
         near_los = ifelse(distFromLOS < 2.0, 1, 0),
         box_pos = ifelse(pff_positionLinedUp %in% c("REO", "RE", "DRT", "DLT", "NRT", "NT", "NLT", "LE", "LEO",
                                                     "ROLB", "RLB", "RILB", "MLB", "LILB", "LLB", "LOLB"), 1, 0),
         is_big = ifelse(primary_position %in% c("EDGE", "IDL"), 1, 0),
         is_left_big = ifelse(is_big == 1 & sideOfCenter == "left", 1,0),
         is_right_big = ifelse(is_big == 1 & sideOfCenter == "right", 1,0),
         is_left_box_def = ifelse(sideOfCenter == "left" & box_pos == 1 & near_los == 1, 1, 0),
         is_right_box_def = ifelse(sideOfCenter == "right" & box_pos == 1 & near_los == 1, 1, 0),
         is_left_def = ifelse(sideOfCenter == "left" & pff_positionLinedUp %in% c("DRT", "NRT", "RE", "REO","RILB", "RLB", "ROLB",
                                                                                  "SCBR", "RCB", "SCBoR", "SCBiR"), 1, 0),
         is_right_def = ifelse(sideOfCenter == "right" & pff_positionLinedUp %in% c("DLT", "NLT", "LE", "LEO","LILB", "LLB", "LOLB",
                                                                                    "SCBL", "LCB", "SCBoL", "SCBiL"), 1, 0),
         is_left_rusher = ifelse(is_left_def == 1 & is_pass_rusher == 1, 1, 0),
         is_right_rusher = ifelse(is_right_def == 1 & is_pass_rusher == 1, 1, 0))

#number of pass rushers to left & right
num_pass_rushers_left_right <- left_right_of_center %>% 
  select(gameId, playId, is_left_big, is_right_big, is_left_def, is_right_def, is_left_box_def, is_right_box_def, is_left_rusher, is_right_rusher)  %>% 
  group_by(gameId, playId) %>% 
  summarise(num_left_box_def = sum(is_left_box_def),
            num_right_box_def = sum(is_right_box_def),
            num_left_rushers = sum(is_left_rusher),
            num_right_rushers = sum(is_right_rusher),
            num_left_bigs = sum(is_left_big),
            num_right_bigs = sum(is_right_big)) %>% 
  mutate(BOSS_look = ifelse(num_left_bigs >= 3 | num_right_bigs >= 3, 1, 0 & num_left_bigs != num_right_bigs),
         overload_look = ifelse(num_left_box_def >= 3 | num_right_box_def >= 3, 1, 0),
         overload_rush = ifelse(num_left_rushers >= 3 | num_right_rushers >= 3, 1, 0))

#join to plays 

df_plays <- df_plays %>% 
  left_join(num_pass_rushers_left_right, by = c("gameId", "playId"))

df_plays <- df_plays %>% 
  mutate(overload_look = ifelse(!is.na(overload_look), overload_look, 0),
         overload_rush = ifelse(!is.na(overload_rush), overload_rush, 0),
         BOSS_look = ifelse(!is.na(BOSS_look), BOSS_look, 0))


dist_crept <- blitz_look_dist %>% 
  select(gameId, playId, nflId, crept_dis, postsnap_depth)

joined_df <- joined_df %>% 
  left_join(dist_crept, by = c("gameId", "playId", "nflId"))

################################################################################

#### MODEL BUILDING
defenders_prior_to_snap <- joined_df %>% 
  group_by(gameId, playId) %>% 
  mutate(has_ball_snap=as.integer(as.logical(sum(event=="ball_snap")))) %>% 
  ungroup() %>% 
  filter(has_ball_snap==1) %>% 
  group_by(gameId, playId, nflId) %>%
  filter(cumsum(event %in% c("ball_snap")) == 0) %>% 
  filter(frameId == max(frameId)) %>% 
  ungroup() %>% 
  filter(sideOfBall == "defense",
         !is.na(is_pass_rusher),
         !is.na(defendersInBox)) %>% 
  mutate(
    yardlineNumber = if_else(
      possessionTeam == yardlineSide &
        yardlineNumber != 50,
      yardlineNumber,
      yardlineNumber + 50),
    defenseIsHome = if_else(defensiveTeam == homeTeamAbbr, 1, 0),
    score_dif = ifelse(defenseIsHome, preSnapHomeScore - preSnapVisitorScore, 
                       preSnapVisitorScore - preSnapHomeScore),
    offenseFormation = case_when(
      offenseFormation == "SHOTGUN" ~ 0,
      offenseFormation == "EMPTY" ~ 1,
      offenseFormation == "SINGLEBACK" ~ 2,
      offenseFormation == "I_FORM" ~ 3,
      offenseFormation == "JUMBO" ~ 4,
      offenseFormation == "PISTOL" ~ 5,
      offenseFormation == "WILDCAT" ~ 6
    )
  ) %>% 
  filter(!is.na(pff_positionLinedUp)) %>% 
  mutate(pff_positionLinedUp = as.factor(pff_positionLinedUp), officialPosition = as.factor(officialPosition)) %>% 
  mutate(absolute_y_dist_from_ball = abs(y_dis_from_ball))







