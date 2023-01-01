library(tidyverse)
library(glue)
library(ggimage)
library(ggrepel)
library(ggthemes)
library(teamcolors)
library(ggthemes)
library(RCurl)
library(nflfastR)
library(nflreadr)
library(nflplotR)

#aspect ratio
asp_ratio = 1.618

#probability by each pff position // tracks
defenders_prior_to_snap_joined %>%
  group_by(primary_position) %>%
  summarise(
    xPassRush = mean(xPassRush),
    isPassRusher = mean(is_pass_rusher),
    PROE = mean(PROE),
    mse = mean(mse)) %>% 
  arrange(desc(xPassRush)) %>%
  View()

#player level stats summed
xpr_player_stats <- defenders_prior_to_snap_joined %>%
  group_by(team, nflId, displayName, primary_position) %>%
  summarize(
    rushes = sum(is_pass_rusher),
    exp_rushes = sum(xPassRush),
    passrushed_oe_sum = sum(PROE),
    passrushed_oe_mean = mean(PROE),
    mse = mean(mse),
    plays = n()
  )

xpr_player_stats %>% 
  group_by(primary_position) %>% 
  filter(plays >= 75) %>% 
  filter(mse == max(mse)) %>% 
  mutate(tendency = ifelse(passrushed_oe_mean > 0, "Blitzer", "Coverage")) %>% 
  View()

#total team plays
team_plays <- df_plays %>% 
  left_join(fastr_pbp, by = c("gameId", "playId")) %>% 
  group_by(defensiveTeam) %>% 
  summarise(total_plays = n(),
            team_epa = mean(epa))

# team stats summed
xpr_team_stats <- defenders_prior_to_snap_joined %>%
  group_by(defensiveTeam, gameId, playId) %>%
  summarise(rushes = sum(is_pass_rusher),
            exp_rushes = sum(xPassRush),
            passrushed_oe_sum = sum(PROE),
            passrushed_oe_mean = mean(PROE),
            mse = mean(mse),
            pressure = max(pressure_caused),
            epa = mean(epa),
            pass_rush_pct = mean(is_pass_rusher),
            expect_passrush = mean(xPassRush)) %>% 
  group_by(defensiveTeam) %>%
  summarise(rushes = sum(rushes),
            exp_rushes = sum(exp_rushes),
            passrushed_oe_sum = sum(passrushed_oe_sum),
            passrushed_oe_mean = mean(passrushed_oe_mean) * 100,
            mse = mean(mse),
            pressure = mean(pressure) * 100,
            epa = mean(epa),
            pass_rush_pct = mean(pass_rush_pct),
            expect_passrush = mean(expect_passrush),
            n_plays = n()) %>% 
  mutate(mse_percentile = quantile(mse))

# getting team colors and logos for plotting
teams_colors_logos <- teams_colors_logos

xpr_team_stats <- xpr_team_stats %>%
  left_join(teams_colors_logos, by = c("defensiveTeam" = "team_abbr"))

xpr_team_stats$defensiveTeam <- factor(xpr_team_stats$defensiveTeam, levels = xpr_team_stats$defensiveTeam)

teams_colors_logos <- teams_colors_logos %>% 
  filter(team_abbr %in% xpr_team_stats$defensiveTeam)

ggplot(data = xpr_team_stats, aes(x = mse, y = passrushed_oe_mean)) +
  geom_hline(yintercept = mean(xpr_team_stats$passrushed_oe_mean), color = "red", linetype = "dashed", alpha= 1) +
  geom_vline(xintercept =  mean(xpr_team_stats$mse), color = "red", linetype = "dashed", alpha=0.5) +
  geom_image(data = xpr_team_stats, aes(image = team_logo_espn), size = 0.095, by = "width", asp = asp_ratio) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme(aspect.ratio = 1/asp_ratio) +
  labs(title = "Team Defense MSE vs PROE - Unpredictable Pass Rush Schemes",
       subtitle = "MSE: mean squared error; measures how actual behavior deviates from model prediction, ie more unpredictable/versatile playcalling",
       x = "Team Defense Mean Squared Error (MSE)",
       y = "Pass Rush % Over Expectation (PROE)") +
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 20, face = "bold", margin = margin(5, 0, 20, 0)),
        axis.text.y = element_text(size = 20, face = "bold", margin = margin(0, 5, 0, 20)),
        plot.title = element_text(size = 25, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))

ggsave("teammseproe.png", height = 10, width = 10 * asp_ratio, dpi = "retina")

xpr_team_stats %>% 
  ggplot(aes(x = mse, y = fct_reorder(defensiveTeam, mse))) +
  geom_bar(aes(fill = team_color, color = team_color2), alpha = 0.9, stat = "identity") +
  geom_image(aes(image = team_logo_espn), asp = 16/9, size = 0.025) +
  xlim(0, 0.07) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  labs(x = "Team Mean Squared Error (MSE)",
       y = "",
       title = "Team Defense MSE - Unpredictable Pass Rush Schemes",
       subtitle = "MSE: mean squared error; measures how actual behavior deviates from model prediction, ie more unpredictable/versatile playcalling") +
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 16, face = "bold", margin = margin(5, 0, 20, 0)),
        axis.text.y = element_text(size = 14, face = "bold", margin = margin(0, 5, 0, 20)),
        plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))

ggsave("teamdefense.png", height = 10, width = 10 * asp_ratio, dpi = "retina")

# joining sim pressures and overloads into df
df_pressure_types <- df_plays %>% 
  select(gameId, playId, sim_pressure, overload_look, overload_rush)

defenders_prior_to_snap_joined <- defenders_prior_to_snap_joined %>% 
  left_join(df_pressure_types, by = c("gameId", "playId"))

# sim pressures
xpr_simpressure_stats <- defenders_prior_to_snap_joined %>%
  mutate(mse = (is_pass_rusher - xPassRush) ** 2) %>%
  group_by(sim_pressure) %>%
  summarize(
    rushes = sum(is_pass_rusher),
    exp_rushes = sum(xPassRush),
    passrushed_oe_sum = sum(PROE),
    passrushed_oe_mean = mean(PROE),
    mse = mean(mse),
    epa = mean(epa)
  )

#
defenders_prior_to_snap_joined %>%
  group_by(down) %>%
  summarize(
    rushes = sum(is_pass_rusher),
    exp_rushes = sum(xPassRush),
    passrushed_oe_sum = sum(PROE),
    passrushed_oe_mean = mean(PROE),
    mse = mean(mse),
    epa = mean(epa)
  ) %>% 
  arrange(desc(mse))

example_play <- defenders_prior_to_snap_joined %>% 
  select(gameId, playId, nflId, displayName, team, pff_positionLinedUp, is_pass_rusher,
         xPassRush, PROE, x, y)

df_sim_pressure <- df_plays %>% 
  select(gameId, playId, down, yardsToGo, defensiveTeam, sim_pressure) %>% 
  filter(sim_pressure == 1,
         defensiveTeam == "ATL")

df_plays %>% 
  group_by(defensiveTeam) %>% 
  summarise(sim_pressures = sum(sim_pressure)) %>% 
  arrange(desc(sim_pressures))

df_plays %>% 
  filter(sim_pressure == 1) %>% 
  group_by(sim_pressure) %>% 
  summarise(n_play = n()) %>% 
  arrange(-n_play)

# Expected Pass Rush Heat Map
defenders_prior_to_snap_joined %>%
  mutate(pff_role = ifelse(pff_role == "Pass Rush", "Pass Rusher", "Coverage Player")) %>% 
  filter(down != 0, is_penalty == 0) %>% 
  ggplot(aes(x = distFromLOS, y = y_dis_from_ball, color = xPassRush)) +
  scale_alpha_continuous(limits = c(0,0.2), breaks = seq(0,0.2, by = 0.025)) +
  geom_point(alpha = 0.03) +
  scale_color_gradient2(mid = "#FFFF00", high = "red",
                        midpoint = 0) +
  xlim(-3, 25) +
  ylim(-25, 25) +
  coord_flip() +
  facet_wrap(~ pff_role) +
  labs(title = "Expected Pass Rush Heat Map",
       subtitle = "2021 Regular Season Weeks 1-8",
       x = "Distance from Line of Scrimmage (Yds)",
       y = "Horizontal Distance from Ball (Yds; Left = Negative, Right = Positive)") +
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 16, face = "bold", margin = margin(5, 0, 20, 0)),
        axis.text.y = element_text(size = 16, face = "bold", margin = margin(0, 5, 0, 20)),
        plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))

ggsave("xPassRushHeat.png", height = 7, width = 7 * asp_ratio, dpi = "retina")

# sim pressure
pressurelooks <- df_plays %>% 
  select(gameId, playId, sim_pressure, overload_look, overload_rush)

defenders_prior_to_snap_joined <- defenders_prior_to_snap_joined %>% 
  left_join(pressurelooks, by = c("gameId", "playId"))

defenders_prior_to_snap_joined %>% 
  filter(defensiveTeam == "ATL", down != 0, is_penalty == 0) %>% 
  mutate(is_sim_pressure = ifelse(sim_pressure == 1, "Sim Pressure", "Other"),
         pff_role = ifelse(pff_role == "Pass Rush", "Pass Rusher", "Coverage Player")) %>% 
  ggplot(aes(x = distFromLOS, y = y_dis_from_ball, color = xPassRush)) +
  scale_alpha_continuous(limits = c(0,0.2), breaks = seq(0,0.2, by = 0.025)) +
  geom_point(alpha = 0.6) +
  scale_color_gradient2(low = "#d7191c", mid = "#ffff00", high = "red",
                        midpoint = 0) +
  xlim(-0.05, 25) +
  ylim(-25, 25) +
  coord_flip() +
  facet_wrap(~is_sim_pressure + pff_role) +
  labs(title = "ATL Falcons Expected Pass Rush Heat Map",
       subtitle = "by Sim Pressure & Role (Pass Rusher or Coverage)",
       x = "Distance from Line of Scrimmage (Yds)",
       y = "Horizontal Distance from Ball (Yds; Left = Negative, Right = Positive)") +
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12, face = "bold", margin = margin(5, 0, 20, 0)),
        axis.text.y = element_text(size = 12, face = "bold", margin = margin(0, 5, 0, 20)),
        plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))

ggsave("simpressurexPassRushHeat.png", height = 7, width = 7 * asp_ratio, dpi = "retina")



