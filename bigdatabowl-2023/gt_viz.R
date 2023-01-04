library(tidyverse)
library(gt)
library(webshot2)

#538 theme
gt_theme_538 <- function(data,...) {
  data %>%
    opt_all_caps()  %>%
    opt_table_font(
      font = list(
        google_font("Chivo"),
        default_fonts()
      )
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom", color = "transparent", weight = px(2)
      ),
      locations = cells_body(
        columns = TRUE,
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = nrow(data$`_data`)
      )
    )  %>% 
    tab_options(
      column_labels.background.color = "white",
      table.border.top.width = px(3),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      source_notes.font.size = 12,
      table.font.size = 16,
      heading.align = "left",
      ...
    ) 
}

primary_pos_summary <- defenders_prior_to_snap_joined %>%
  group_by(primary_position) %>%
  summarise(
    xPassRush = mean(xPassRush) * 100,
    isPassRusher = mean(is_pass_rusher) * 100) %>% 
  arrange(desc(xPassRush)) %>% 
gt() %>%
  gt_theme_538(table.width = px(650)) %>%
  cols_align(align = "center") %>%
  fmt_number(columns = vars(xPassRush, isPassRusher),
             decimals = 1) %>% 
  cols_label(primary_position = "Primary Position",
             xPassRush = "Expected Pass Rush %",
             isPassRusher = "Actual Pass Rush %") %>%
  tab_header(
    title = md("**xPass Rush Results by Primary Position**"),
    subtitle = "Weeks 1-8, 2021"
  ) %>% 
  opt_align_table_header(align = "center")
#save table
gtsave(primary_pos_summary, "primary_pos_summary.png")

################################################################################

# Overall; get top PROE and below
overall_PROE <- defenders_prior_to_snap_joined %>% 
  group_by(nflId, displayName, primary_position, team, down) %>% 
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
            pressure_rate = mean(pressure_caused)) 

pass_rushes <- defenders_prior_to_snap_joined %>% 
  filter(is_pass_rusher == 1) %>% 
  group_by(nflId, displayName, primary_position, team) %>% 
  summarise(avg_crept_dis = mean(crept_dis),
            avg_post_snap_depth = mean(postsnap_depth)) %>% 
  rename(pr_crep_dis = avg_crept_dis,
         pr_post_snap_depth = avg_post_snap_depth)

coverage <- defenders_prior_to_snap_joined %>% 
  filter(is_pass_rusher == 0) %>% 
  group_by(nflId, displayName, primary_position, team) %>% 
  summarise(avg_crept_dis = mean(crept_dis),
            avg_post_snap_depth = mean(postsnap_depth)) %>% 
  rename(cov_crep_dis = avg_crept_dis,
       cov_post_snap_depth = avg_post_snap_depth)

#highest PROE
overall_PROE <- overall_PROE %>% 
  left_join(pass_rushes, by = c("nflId", "displayName", "primary_position", "team")) %>% 
  left_join(coverage, by = c("nflId", "displayName", "primary_position", "team"))

highest_proe <- overall_PROE %>%
  mutate(headshot = case_when(
    displayName == "Sam Eguavoen" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/2577637.png&w=350&h=254",
    displayName == "Isaiah Oliver" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/3915437.png&w=350&h=254",
    displayName == "Elandon Roberts" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/2987743.png&w=350&h=254",
    displayName == "Micah Parsons" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/4361423.png&w=350&h=254",
    displayName == "Josh Sweat" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/3693166.png&w=350&h=254")) %>% 
  filter(plays >= 75) %>% 
  arrange(desc(passrushed_oe_mean)) %>% 
  head(5) %>% 
  select(displayName, headshot, primary_position, pass_rush_pct, avg_xPR, passrushed_oe_mean, pr_crep_dis, cov_crep_dis) %>% 
  ungroup() %>% 
  select(-nflId) %>% 
  gt() %>%
  gtExtras::gt_img_rows(headshot) %>% 
  gt_theme_538(table.width = px(650)) %>%
  cols_align(align = "center") %>%
  fmt_number(columns = vars(pass_rush_pct, avg_xPR, passrushed_oe_mean),
             decimals = 1) %>%
  fmt_number(columns = vars(pr_crep_dis, cov_crep_dis),
             decimals = 2) %>%
  cols_label(displayName = "Player",
             headshot = " ",
             primary_position = "Primary Position",
             avg_xPR = "xPass Rush %",
             pass_rush_pct = "Actual Pass Rush %",
             passrushed_oe_mean = "PROE",
             pr_crep_dis = "Crept Dist (Pass Rushes)",
             cov_crep_dis = "Crept Dist (Cov)") %>%
  tab_header(
    title = md("**Highest Pass Rush Over Expected - Blitzers**"),
    subtitle = "Weeks 1-8, 2021 | Min. 75 Plays") %>% 
  tab_source_note(source_note = "Positive Crept Distance (yds) is going toward the LOS; Negative is backwards"
  ) %>% 
  opt_align_table_header(align = "left")
  
#save table
gtsave(highest_proe, "highest_proe.png")

# lowest PROE
lowest_proe <- overall_PROE %>%
  mutate(headshot = case_when(
    displayName == "Benson Mayowa" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/16528.png&w=350&h=254",
    displayName == "Andrew Van Ginkel" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/3133487.png&w=350&h=254",
    displayName == "Tyus Bowser" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/3040037.png&w=350&h=254",
    displayName == "Lorenzo Carter" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/3128715.png&w=350&h=254",
    displayName == "Josh Uche" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/4046528.png&w=350&h=254")) %>% 
  filter(plays >= 75) %>% 
  arrange(passrushed_oe_mean) %>% 
  head(5) %>% 
  select(displayName, headshot, primary_position, pass_rush_pct, avg_xPR, passrushed_oe_mean, pr_crep_dis, cov_crep_dis) %>% 
  ungroup() %>% 
  select(-nflId) %>% 
  gt() %>% 
  gtExtras::gt_img_rows(headshot) %>% 
  gt_theme_538(table.width = px(650)) %>%
  cols_align(align = "center") %>%
  fmt_number(columns = vars(pass_rush_pct, avg_xPR, passrushed_oe_mean),
             decimals = 1) %>%
  fmt_number(columns = vars(pr_crep_dis, cov_crep_dis),
             decimals = 2) %>%
  cols_label(displayName = "Player",
             headshot = " ",
             primary_position = "Primary Position",
             avg_xPR = "xPass Rush %",
             pass_rush_pct = "Actual Pass Rush %",
             passrushed_oe_mean = "PROE",
             pr_crep_dis = "Crept Dist (Pass Rushes)",
             cov_crep_dis = "Crept Dist (Cov)") %>%
  tab_header(
    title = md("**Lowest Pass Rush Over Expected - Bluffers**"),
    subtitle = "Weeks 1-8, 2021 | Min. 75 Plays") %>% 
  tab_source_note(source_note = "Positive Crept Distance (yds) is going toward the LOS; Negative is backwards"
  ) %>% 
  opt_align_table_header(align = "left")

#save table
gtsave(lowest_proe, "lowest_proe.png")

overall_PROE %>% 
  mutate(crep_dis_dif = cov_crep_dis - pr_crep_dis) %>% view()

################################################################################

#player level stats summed
xpr_player_stats <- defenders_prior_to_snap_joined %>%
  group_by(team, nflId, displayName, primary_position) %>%
  summarize(plays = n(),
              pass_rushes = sum(is_pass_rusher),
              pass_rush_pct = (pass_rushes / plays) * 100,
              avg_xPR = mean(xPassRush) * 100,
              exp_rushes = sum(xPassRush),
              passrushed_oe_sum = sum(PROE),
              passrushed_oe_mean = mean(PROE) * 100,
              mse = mean(mse))

mse_highs <- xpr_player_stats %>% 
  mutate(headshot = case_when(
    displayName == "Brandon Jones" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/4039059.png&w=350&h=254",
    displayName == "Andrew Van Ginkel" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/3133487.png&w=350&h=254",
    displayName == "Sam Eguavoen" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/2577637.png&w=350&h=254",
    displayName == "Isaiah Oliver" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/3915437.png&w=350&h=254",
    displayName == "Vita Vea" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/3134362.png&w=350&h=254")) %>% 
  group_by(primary_position) %>% 
  filter(plays >= 75) %>% 
  filter(mse == max(mse)) %>% 
  mutate(tendency = ifelse(passrushed_oe_mean > 0, "Pass Rusher", "Coverage")) %>% 
  arrange(desc(mse)) %>% 
  select(displayName, headshot, primary_position, pass_rush_pct, avg_xPR, passrushed_oe_mean, mse, tendency) %>% 
  ungroup() %>% 
  gt() %>% 
  gtExtras::gt_img_rows(headshot) %>% 
  gt_theme_538(table.width = px(650)) %>%
  cols_align(align = "center") %>%
  fmt_number(columns = vars(pass_rush_pct, avg_xPR, passrushed_oe_mean),
             decimals = 1) %>%
  fmt_number(columns = vars(mse),
             decimals = 2) %>%
  cols_label(displayName = "Player",
             headshot = " ",
             primary_position = "Primary Position",
             avg_xPR = "xPass Rush %",
             pass_rush_pct = "Actual Pass Rush %",
             passrushed_oe_mean = "PROE",
             mse = "MSE",
             tendency = "Tendency") %>%
  tab_header(
    title = md("**Most Unpredictable Defenders - Highest MSE by Primary Position**"),
    subtitle = "Weeks 1-8, 2021 | Min. 75 Plays") %>% 
  tab_source_note(source_note = "PROE = Pass Rush % Over Expected | MSE = Mean Squared Error | Tendency label relative to expectation"
  ) %>% 
  opt_align_table_header(align = "left")

#save table
gtsave(mse_highs, "mse_highs.png")


# ATL FALCONS TABLES
atl_num_pass_rushers <- num_pass_rushers_table %>% 
  group_by(defensiveTeam, num_pass_rushers) %>% 
  summarize(n_plays = n(),
            epa = mean(epa),
            pressure_rate = mean(pressure, na.rm = T) * 100,
            success = mean(success, na.rm = T) * 100) %>% 
  left_join(total_plays_team, by = c("defensiveTeam")) %>% 
  mutate(Pct = n_plays / total_plays * 100) %>% 
  filter(defensiveTeam == "ATL") %>% 
  select(num_pass_rushers, n_plays, Pct, pressure_rate, epa, success) %>% 
  ungroup() %>% 
  select(-defensiveTeam) %>% 
  gt() %>% 
  gt_theme_538(table.width = px(650)) %>%
  cols_align(align = "center") %>%
  fmt_number(columns = vars(Pct, pressure_rate, success),
             decimals = 1) %>%
  fmt_number(columns = vars(epa),
             decimals = 3) %>%
  cols_label(num_pass_rushers = "# of Pass Rushers",
             n_plays = "# of Plays",
             Pct = "% of Plays",
             pressure_rate = "Pressure %",
             epa = "EPA",
             success = "Success %") %>%
  tab_header(
    title = md("**Atlanta Falcons Defense by Number of Pass Rushers**"),
    subtitle = "Weeks 1-8, 2021") %>% 
  tab_source_note(source_note = "NFL AVG (4-man rush) -- % of Plays: 70.6%, Pressure %: 33.3, EPA: 0.071"
  ) %>% 
  opt_align_table_header(align = "left") %>% 
  data_color(
  columns = c(3),
  colors = scales::col_numeric(
    paletteer::paletteer_d(
      palette = "RColorBrewer::Reds") %>% as.character(),
    domain = NULL
  )
)

#save table
gtsave(atl_num_pass_rushers, "atl_num_pass_rushers.png")

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
            pressure_rate = mean(pressure)*100,
            success = mean(success)*100) %>%
  select(down, distance_bins, n_plays, pressure_rate, epa, success) %>% 
  ungroup() %>% 
  select(-defensiveTeam) %>% 
  gt() %>% 
  gt_theme_538(table.width = px(650)) %>%
  cols_align(align = "center") %>%
  fmt_number(columns = vars(success, pressure_rate),
             decimals = 1) %>%
  fmt_number(columns = vars(epa),
             decimals = 3) %>%
  cols_label(down = "Down",
             distance_bins = "Yds to Go",
             n_plays = "# of Plays",
             pressure_rate = "Pressure %",
             epa = "EPA",
             success = "Success %") %>%
  tab_header(
    title = md("**Atlanta Falcons Simulated Pressures by Yards to Go**"),
    subtitle = "ATL: 15.5% Sim Pressure Usage (1st)") %>% 
  tab_source_note(source_note = "EPA color scale: darker = worse outcome for defense"
  ) %>% 
  opt_align_table_header(align = "left") %>% 
  data_color(
    columns = c(5),
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::Reds") %>% as.character(),
      domain = NULL
    )
  )

#save table
gtsave(ATL_sim_pressures, "ATL_sim_pressures.png")

second_third_level_sim_blitzers <- defenders_prior_to_snap_joined %>%
  group_by(nflId, displayName, primary_position, defensiveTeam, sim_pressure) %>% 
  filter(defensiveTeam == "ATL", displayName %in% c("Deion Jones", "Isaiah Oliver", "Foyesade Oluokun"), down != 0, is_penalty == 0) %>% 
  summarise(plays = n(),
            pass_rushes = sum(is_pass_rusher),
            pass_rush_pct = (pass_rushes / plays)*100,
            avg_xPR = mean(xPassRush)*100,
            exp_rushes = sum(xPassRush),
            passrushed_oe_sum = sum(PROE),
            passrushed_oe_mean = mean(PROE)*100,
            mse = mean(mse),
            avg_crept_dis = mean(crept_dis),
            distFromLOS = mean(distFromLOS),
            abs_y_dis_from_ball = mean(absolute_y_dist_from_ball),
            dir = mean(dir),
            pressures = sum(pressure_caused)) %>% 
  mutate(headshot = case_when(
    displayName == "Deion Jones" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/2976545.png&w=350&h=254",
    displayName == "Isaiah Oliver" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/3915437.png&w=350&h=254",
    displayName == "Foyesade Oluokun" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/3050073.png&w=350&h=254"),
    is_sim_pressure = ifelse(sim_pressure == 1, "Yes", "No")) %>%
  arrange(desc(displayName)) %>% 
  select(displayName, headshot, primary_position, is_sim_pressure, pass_rush_pct, avg_xPR, passrushed_oe_mean, distFromLOS, abs_y_dis_from_ball, avg_crept_dis, pressures) %>% 
  ungroup() %>% 
  select(-nflId, -defensiveTeam) %>% 
  gt() %>% 
  gtExtras::gt_img_rows(headshot) %>% 
  gt_theme_538(table.width = px(650)) %>%
  cols_align(align = "center") %>%
  fmt_number(columns = vars(pass_rush_pct, avg_xPR, passrushed_oe_mean, distFromLOS, abs_y_dis_from_ball),
             decimals = 1) %>%
  fmt_number(columns = vars(avg_crept_dis),
             decimals = 2) %>%
  cols_label(displayName = "Player",
             headshot = " ",
             primary_position = "Primary Position",
             is_sim_pressure = "Sim Pressure?",
             avg_xPR = "xPass Rush %",
             pass_rush_pct = "Actual Pass Rush %",
             passrushed_oe_mean = "PROE",
             distFromLOS = "Dist from LOS",
             abs_y_dis_from_ball = "Horz. Dist from Ball", 
             avg_crept_dis = "Crept Dist"
             ) %>%
  tab_header(
    title = md("**Falcons Core Second & Third Level Blitzers on Simulated Pressures**"),
    subtitle = "Pre-Snap Indicator Differences") %>% 
  tab_source_note(source_note = "Pressures: a sack, QB hit, or hurry"
  ) %>% 
  opt_align_table_header(align = "left") %>% 
  data_color(
    columns = c(7),
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::Reds") %>% as.character(),
      domain = NULL
    )
  )

#save table
gtsave(second_third_level_sim_blitzers, "second_third_level_sim_blitzers.png")
