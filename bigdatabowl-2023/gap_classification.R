library(tidyverse)
library(tidymodels)
library(usemodels)
library(textrecipes)
library(vip)
library(nflfastR)
library(ggimage)
library(xgboost)
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
  mutate(distFromBall = sqrt((x - ball_x) ^ 2 + (y - ball_y) ^ 2))

df_plays <- read_csv("data/plays.csv") %>%
  process_plays_data()

pffScoutingData <- read_csv("data/pffScoutingData.csv")

# create a is pass rusher column to better summarize the data and have a binary variable for the expected pass rusher model
pffScoutingData <- pffScoutingData %>% 
  mutate(is_pass_rusher = ifelse(pff_role == "Pass Rush", 1,0),
         is_coverage = ifelse(pff_role == "Coverage", 1,0))

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



####################################### ADDED
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

#join all df together

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

#Fixing the TE problem for Gap Classification

left_tes <- joined_df %>% 
  filter(event == "ball_snap",
         pff_positionLinedUp %in% c("TE-iL", "TE-L", "TE-oL")) %>% 
  group_by(gameId, playId) %>% 
  summarize(num_left_tes=n())

right_tes <- joined_df %>% 
  filter(event == "ball_snap",
         pff_positionLinedUp %in% c("TE-iR", "TE-R", "TE-oR")) %>% 
  group_by(gameId, playId) %>% 
  summarize(num_right_tes=n())

joined_df <- 
  joined_df %>% 
  left_join(left_tes,
            by=c("gameId","playId")) %>% 
  left_join(right_tes,
            by=c("gameId","playId"))

# creating gap labels

LT_coordinates <- joined_df %>%
  filter(pff_positionLinedUp == "LT") %>%
  select(gameId, playId, x, y, frameId) %>%
  rename(xLT = x,
         yLT = y)

# create a distance from LG column
LG_coordinates <- joined_df %>%
  filter(pff_positionLinedUp == "LG") %>%
  select(gameId, playId, x, y, frameId) %>%
  rename(xLG = x,
         yLG = y)

# create a distance from RT column
RT_coordinates <- joined_df %>%
  filter(pff_positionLinedUp == "RT") %>%
  select(gameId, playId, x, y, frameId) %>%
  rename(xRT = x,
         yRT = y)

# create a distance from RG column
RG_coordinates <- joined_df %>%
  filter(pff_positionLinedUp == "RG") %>%
  select(gameId, playId, x, y, frameId) %>%
  rename(xRG = x,
         yRG = y)

# create a distance from Center column
C_coordinates <- joined_df %>%
  filter(pff_positionLinedUp == "C") %>%
  select(gameId, playId, x, y, frameId) %>%
  rename(xCenter = x,
         yCenter = y)

#join the dataframes
joined_df <- joined_df %>%
  left_join(LT_coordinates, by = c("playId", "gameId", "frameId")) %>%
  left_join(RT_coordinates, by = c("playId", "gameId", "frameId")) %>%
  left_join(RG_coordinates, by = c("playId", "gameId", "frameId")) %>%
  left_join(LG_coordinates, by = c("playId", "gameId", "frameId")) %>%
  left_join(C_coordinates, by = c("playId", "gameId", "frameId"))

#clean up environment
rm(C_coordinates,
   LT_coordinates,
   LG_coordinates,
   RT_coordinates,
   RG_coordinates)

# create a df for each of the different TE alignments
# TE-L, TE-R, TE-iL, TE-iR, TE-oL, TE-oR

# create a distance from Center column
TEL_coordinates <- joined_df %>%
  filter(pff_positionLinedUp == "TE-L") %>%
  select(gameId, playId, x, y, frameId) %>%
  rename(xTEL = x,
         yTEL = y)

TER_coordinates <- joined_df %>%
  filter(pff_positionLinedUp == "TE-R") %>%
  select(gameId, playId, x, y, frameId) %>%
  rename(xTER = x,
         yTER = y)

TEiL_coordinates <- joined_df %>%
  filter(pff_positionLinedUp == "TE-iL") %>%
  select(gameId, playId, x, y, frameId) %>%
  rename(xTEiL = x,
         yTEiL = y)

TEiR_coordinates <- joined_df %>%
  filter(pff_positionLinedUp == "TE-iR") %>%
  select(gameId, playId, x, y, frameId) %>%
  rename(xTEiR = x,
         yTEiR = y)

TEoL_coordinates <- joined_df %>%
  filter(pff_positionLinedUp == "TE-oL") %>%
  select(gameId, playId, x, y, frameId) %>%
  rename(xTEoL = x,
         yTEoL = y)

TEoR_coordinates <- joined_df %>%
  filter(pff_positionLinedUp == "TE-oR") %>%
  select(gameId, playId, x, y, frameId) %>%
  rename(xTEoR = x,
         yTEoR = y)

joined_df <- joined_df %>%
  left_join(TEL_coordinates, by = c("playId", "gameId", "frameId")) %>%
  left_join(TER_coordinates, by = c("playId", "gameId", "frameId")) %>%
  left_join(TEiL_coordinates, by = c("playId", "gameId", "frameId")) %>%
  left_join(TEiR_coordinates, by = c("playId", "gameId", "frameId")) %>%
  left_join(TEoL_coordinates, by = c("playId", "gameId", "frameId")) %>% 
  left_join(TEoR_coordinates, by = c("playId", "gameId", "frameId"))

joined_df <- joined_df %>% 
  mutate(num_left_tes = ifelse(is.na(num_left_tes), 0, num_left_tes),
         num_right_tes = ifelse(is.na(num_right_tes), 0, num_right_tes))


#trying to classify Gaps
pre_snap_gaps <- joined_df %>% 
  filter(event == "ball_snap",
         sideOfBall == "defense") %>% 
  select(gameId, playId, nflId, displayName, officialPosition, pff_positionLinedUp, los, distFromLOS, distFromMid, x, y,
         yLT, yLG, yCenter, yRG, yRT, sideOfBall, yTEL, yTER, yTEiL, yTEiR, yTEoL, yTEoR, num_right_tes, num_left_tes) %>% 
  mutate(pre_snap_gap =
           case_when(y >= yCenter & y <= yLG ~ "L-A",
                     y >= yLG & y <= yLT ~ "L-B",
                     num_left_tes == 0 & y >= yLT ~ "L-C",
                     y < yCenter & y >= yRG ~ "R-A",
                     y <= yRG & y >= yRT ~ "R-B",
                     num_right_tes == 0 & y <= yRT ~ "R-C",
                     num_left_tes == 1 & y >= yLT & y <= yTEL ~ "L-C",
                     num_left_tes == 1 & y >= yTEL ~ "L-D",
                     num_left_tes == 2 & y >= yLT & y <= yTEiL ~ "L-C",
                     num_left_tes == 2 & y >= yTEiL & y <= yTEoL ~ "L-D",
                     num_left_tes == 2 & y >= yTEoL ~ "L-E",
                     num_left_tes == 3 & y >= yLT & y <= yTEiL ~ "L-C", 
                     num_left_tes == 3 & y >= yTEiL & y <= yTEL ~ "L-D",
                     num_left_tes == 3 & y >= yTEL & y <= yTEoL ~ "L-E",
                     num_left_tes == 3 & y >= yTEoL ~ "L-F",
                     num_right_tes == 1 & y <= yRT & y >= yTER ~ "R-C",
                     num_right_tes == 1 & y <= yTER ~ "R-D",
                     num_right_tes == 2 & y <= yRT & y >= yTEiR ~ "R-C",
                     num_right_tes == 2 & y <= yTEiR & y >= yTEoR ~ "R-D",
                     num_right_tes == 2 & y <= yTEoR ~ "R-E",
                     num_right_tes == 3 & y <= yRT & y >= yTEiR ~ "R-C",
                     num_right_tes == 3 & y <= yTEiR & y >= yTER ~ "R-D",
                     num_right_tes == 3 & y <= yTER & y >= yTEoR ~ "R-E",
                     num_right_tes == 3 & y <= yTEoR ~ "R-F"))

pre_snap_gaps <- pre_snap_gaps %>% 
  select(gameId, playId, nflId, displayName, pre_snap_gap) %>% 
  filter(nflId != "NA")

saveRDS(pre_snap_gaps, "pre_snap_gaps.rds")

# Following code is pretty useless but if interested this is how I was attempted to get a "post-snap gap"
# %>% 
#   rename(presnap_yLT = yLT, 
#          presnap_yLG = yLG, 
#          presnap_yCenter = yCenter, 
#          presnap_yRG = yRG,
#          presnap_yRT = yRT,
#          presnap_yTEL = yTEL,
#          presnap_yTER = yTER,
#          presnap_yTEiL = yTEiL, 
#          presnap_yTEiR = yTEiR, 
#          presnap_yTEoL = yTEoL, 
#          presnap_yTEoR = yTEoR) %>% 
#   select(gameId, playId, nflId, displayName, pre_snap_gap, presnap_yLT, 
#          presnap_yLG, presnap_yCenter, presnap_yRG, presnap_yRT, presnap_yTEL, presnap_yTER, presnap_yTEiL, presnap_yTEiR, 
#          presnap_yTEoL, presnap_yTEoR, num_right_tes, num_left_tes)

# 
# post_snap_gaps <- joined_df %>% 
#   filter(sideOfBall == "defense", 
#          general_position %in% c("EDGE", "IDL")) %>%
#   group_by(gameId, playId, nflId, displayName) %>%
#   filter(cumsum(event %in% c("ball_snap")) > 0) %>%
#   filter(as.numeric(difftime(time, min(time), units = "secs")) <= 1.5) %>% 
#   filter(frameId == max(frameId)) %>% 
#   ungroup() %>% 
#   select(gameId, playId, nflId, displayName, y) %>% 
#   rename(post_snap_y = y)
# 
# 
# pre_post_snap_gaps_join <- pre_snap_gaps %>% 
#   left_join(post_snap_gaps, by = c("gameId", "playId", "nflId", "displayName"))
# 
# 
# pre_post_snap_gaps_join <- pre_post_snap_gaps_join %>% 
#   mutate(post_snap_gap =
#            case_when(post_snap_y >= presnap_yCenter & post_snap_y <= presnap_yLG ~ "L-A",
#                      post_snap_y >= presnap_yLG & post_snap_y <= presnap_yLT ~ "L-B",
#                      num_left_tes == 0 & post_snap_y >= presnap_yLT ~ "L-C",
#                      post_snap_y < presnap_yCenter & post_snap_y >= presnap_yRG ~ "R-A",
#                      post_snap_y <= presnap_yRG & post_snap_y >= presnap_yRT ~ "R-B",
#                      num_right_tes == 0 & post_snap_y <= presnap_yRT ~ "R-C",
#                      num_left_tes == 1 & post_snap_y >= presnap_yLT & post_snap_y <= presnap_yTEL ~ "L-C",
#                      num_left_tes == 1 & post_snap_y >= presnap_yTEL ~ "L-D",
#                      num_left_tes == 2 & post_snap_y >= presnap_yLT & post_snap_y <= presnap_yTEiL ~ "L-C",
#                      num_left_tes == 2 & post_snap_y >= presnap_yTEiL & post_snap_y <= presnap_yTEoL ~ "L-D",
#                      num_left_tes == 2 & post_snap_y >= presnap_yTEoL ~ "L-E",
#                      num_left_tes == 3 & post_snap_y >= presnap_yLT & post_snap_y <= presnap_yTEiL ~ "L-C", 
#                      num_left_tes == 3 & post_snap_y >= presnap_yTEiL & post_snap_y <= presnap_yTEL ~ "L-D",
#                      num_left_tes == 3 & post_snap_y >= presnap_yTEL & post_snap_y <= presnap_yTEoL ~ "L-E",
#                      num_left_tes == 3 & post_snap_y >= presnap_yTEoL ~ "L-F",
#                      num_right_tes == 1 & post_snap_y <= presnap_yRT & post_snap_y >= presnap_yTER ~ "R-C",
#                      num_right_tes == 1 & post_snap_y <= presnap_yTER ~ "R-D",
#                      num_right_tes == 2 & post_snap_y <= presnap_yRT & post_snap_y >= presnap_yTEiR ~ "R-C",
#                      num_right_tes == 2 & post_snap_y <= presnap_yTEiR & post_snap_y >= presnap_yTEoR ~ "R-D",
#                      num_right_tes == 2 & post_snap_y <= presnap_yTEoR ~ "R-E",
#                      num_right_tes == 3 & post_snap_y <= presnap_yRT & post_snap_y >= presnap_yTEiR ~ "R-C",
#                      num_right_tes == 3 & post_snap_y <= presnap_yTEiR & post_snap_y >= presnap_yTER ~ "R-D",
#                      num_right_tes == 3 & post_snap_y <= presnap_yTER & post_snap_y >= presnap_yTEoR ~ "R-E",
#                      num_right_tes == 3 & post_snap_y <= presnap_yTEoR ~ "R-F"))
# 
# 
# gap_changes <- pre_post_snap_gaps_join %>% 
#   mutate(gap_change = ifelse(pre_snap_gap == post_snap_gap, 0, 1)) %>% 
#   select(gameId, playId, nflId, displayName, pre_snap_gap, post_snap_gap, gap_change) 
# 
# gap_changes %>% 
#   group_by(gap_change) %>% 
#   summarise(n_plays = n())


matchups <- pffScoutingData %>% 
  select(gameId, playId, nflId, displayName, pff_positionLinedUp, blocked_player, blocked_player_posLinedUp, blocked_player_official_pos) %>% 
  filter(blocked_player_posLinedUp %in% c("ROLB", "LOLB", "REO", "LEO", "RE", "LE"),
         pff_positionLinedUp %in% c("RT", "LT"))





