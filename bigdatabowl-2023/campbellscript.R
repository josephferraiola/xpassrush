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

#load in presnap gap classification
pre_snap_gaps <- readRDS("pre_snap_gaps.rds")

defenders_prior_to_snap_joined <- defenders_prior_to_snap_joined %>% 
  left_join(pre_snap_gaps, by = c("gameId", "playId", "nflId", "displayName")) %>% 
  mutate(is_mugged = ifelse(pff_positionLinedUp %in% c("MLB", "RILB", "LILB") & distFromLOS <= 2, 1, 0))

# mugged looks, pre_snap_gap
defenders_prior_to_snap_joined %>% 
  filter(is_mugged == 1) %>% 
  group_by(nflId, displayName, primary_position) %>% 
  summarise(plays = n(),
            pass_rushes = sum(is_pass_rusher),
            pass_rush_pct = (pass_rushes / plays),
            avg_xPR = mean(xPassRush),
            exp_rushes = sum(xPassRush),
            passrushed_oe_sum = sum(PROE),
            passrushed_oe_mean = mean(PROE),
            mse = mean(mse),
            avg_crept_dis = mean(crept_dis),
            avg_post_snap_depth = mean(postsnap_depth),
            pressures = sum(pressure_caused),
            pressure_rate = mean(pressure_caused)) %>% 
  filter(plays > 0) %>%  view()

#De'Vondre Campbell mugged looks, pre_snap_gap
campbell_mugged <- defenders_prior_to_snap_joined %>% 
  filter(is_mugged == 1,
         displayName == "De'Vondre Campbell") %>% 
  select(gameId, playId, nflId, displayName, pff_positionLinedUp, primary_position, pff_role, is_pass_rusher, xPassRush,
         pre_snap_gap, is_mugged, PROE, pressure_caused, pff_role, distFromLOS, y_dis_from_ball, absolute_y_dist_from_ball, dir, crept_dis, postsnap_depth, mse) %>% 
  group_by(pff_role) %>% 
  summarise(plays = n(),
            pass_rushes = sum(is_pass_rusher),
            pass_rush_pct = (pass_rushes / plays),
            avg_xPR = mean(xPassRush) * 100,
            exp_rushes = sum(xPassRush),
            passrushed_oe_sum = sum(PROE),
            passrushed_oe_mean = mean(PROE) * 100,
            distFromLOS = mean(distFromLOS),
            absolute_y_dist_from_ball = mean(absolute_y_dist_from_ball),
            dir = mean(dir),
            mse = mean(mse),
            avg_crept_dis = mean(crept_dis),
            avg_post_snap_depth = mean(postsnap_depth),
            pressures = sum(pressure_caused),
            pressure_rate = mean(pressure_caused)
            )

campbell <- campbell_mugged %>%
  select(pff_role, avg_xPR, distFromLOS, absolute_y_dist_from_ball, avg_crept_dis, dir) %>% 
  gt() %>% 
  gt_theme_538(table.width = px(650)) %>%
  cols_align(align = "center") %>%
  fmt_number(columns = vars(avg_xPR, distFromLOS, absolute_y_dist_from_ball, dir),
             decimals = 1) %>%
  fmt_number(columns = vars(avg_crept_dis),
             decimals = 2) %>%
  cols_label(pff_role = "Role",
             avg_xPR = "xPassRush %",
             distFromLOS = "Dist from LOS",
             absolute_y_dist_from_ball = "Horz. Dist from Ball",
             avg_crept_dis = "Crept Dist",
             dir = "Direction") %>%
  tab_header(
    title = md("**De'Vondre Campbell Tendencies - Mugged Alignment by Role**")) %>% 
  opt_align_table_header(align = "left")

#save table
gtsave(campbell, "campbell_role.png")

defenders_prior_to_snap_joined %>% 
  filter(is_mugged == 1,
         displayName == "De'Vondre Campbell") %>% 
  select(gameId, playId, nflId, displayName, pff_positionLinedUp, primary_position, is_pass_rusher, xPassRush,
         pre_snap_gap, is_mugged, PROE, crept_dis, postsnap_depth, pressure_caused, pff_role, distFromLOS, y_dis_from_ball) %>% 
  ggplot(aes(x = distFromLOS, y = y_dis_from_ball, color = xPassRush)) +
  scale_alpha_continuous(limits = c(0,0.2), breaks = seq(0,0.2, by = 0.025)) +
  geom_point(alpha = 0.8) +
  scale_color_gradient2(low = "#d7191c", mid = "#ffff00", high = "red",
                        midpoint = 0) +
  xlim(-3, 25) +
  ylim(-25, 25) +
  coord_flip() +
  facet_wrap(~ pff_role) +
  labs(title = "De'Vondre Campbell Expected Pass Rush Heat Map",
       subtitle = "Aligned in Mugged Look",
       x = "Distance from Line of Scrimmage (Yds)",
       y = "Horizontal Distance from Ball (Left = Negative, Right = Positive)") +
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 14, face = "bold", margin = margin(5, 0, 20, 0)),
        axis.text.y = element_text(size = 14, face = "bold", margin = margin(0, 5, 0, 20)),
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))

ggsave("campebellheat.png", height = 4, width = 4 * asp_ratio, dpi = "retina")

################################################################################

# Overall; get top PROE and below
overall_PROE <- defenders_prior_to_snap_joined %>% 
  filter(is_mugged == 1) %>% 
  group_by(nflId, displayName, primary_position, team) %>% 
  summarise(plays = n(),
            pass_rushes = sum(is_pass_rusher),
            pass_rush_pct = (pass_rushes / plays) * 100,
            avg_xPR = mean(xPassRush) * 100,
            exp_rushes = sum(xPassRush),
            passrushed_oe_sum = sum(PROE),
            passrushed_oe_mean = mean(PROE) * 100,
            mse = mean(mse),
            avg_crept_dis = mean(crept_dis),
            avg_post_snap_depth = mean(postsnap_depth),
            pressures = sum(pressure_caused),
            pressure_rate = mean(pressure_caused)) %>% 
  filter(displayName == "De'Vondre Campbell")

pass_rushes <- defenders_prior_to_snap_joined %>% 
  filter(is_pass_rusher == 1, is_mugged == 1) %>% 
  group_by(nflId, displayName, primary_position, team) %>% 
  summarise(avg_crept_dis = mean(crept_dis),
            avg_post_snap_depth = mean(postsnap_depth)) %>% 
  rename(pr_crep_dis = avg_crept_dis,
         pr_post_snap_depth = avg_post_snap_depth)

coverage <- defenders_prior_to_snap_joined %>% 
  filter(is_pass_rusher == 0, is_mugged == 1) %>% 
  group_by(nflId, displayName, primary_position, team) %>% 
  summarise(avg_crept_dis = mean(crept_dis),
            avg_post_snap_depth = mean(postsnap_depth)) %>% 
  rename(cov_crep_dis = avg_crept_dis,
         cov_post_snap_depth = avg_post_snap_depth)

#highest PROE
overall_PROE <- overall_PROE %>% 
  left_join(pass_rushes, by = c("nflId", "displayName", "primary_position", "team")) %>% 
  left_join(coverage, by = c("nflId", "displayName", "primary_position", "team"))



campbell <- overall_PROE %>%
  filter(displayName == "De'Vondre Campbell") %>% 
  mutate(headshot = case_when(
    displayName == "De'Vondre Campbell" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/3040180.png&w=350&h=254")) %>%
  select(displayName, nflId, headshot, primary_position, exp_rushes, pass_rushes, avg_xPR, pass_rush_pct, passrushed_oe_mean, avg_crept_dis) %>% 
  ungroup() %>% 
  select(-nflId, -displayName, -headshot) %>% 
  gt() %>% 
  gt_theme_538(table.width = px(650)) %>%
  cols_align(align = "center") %>%
  fmt_number(columns = vars(exp_rushes),
             decimals = 0) %>% 
  fmt_number(columns = vars(pass_rush_pct, avg_xPR, passrushed_oe_mean),
             decimals = 1) %>%
  fmt_number(columns = vars(avg_crept_dis),
             decimals = 2) %>%
  cols_label(primary_position = "Primary Position",
             pass_rushes = "Pass Rushes (actual)",
             exp_rushes = "xPass Rushes",
             avg_xPR = "xPass Rush%",
             pass_rush_pct = "Actual Pass Rush%",
             passrushed_oe_mean = "PROE (%)",
             avg_crept_dis = "Crept Dist") %>%
  tab_header(
    title = md("**De'Vondre Campbell Tendencies - Mugged Alignment**"),
    subtitle = "") %>% 
  opt_align_table_header(align = "left")

#save table
gtsave(campbell, "campbell.png")









