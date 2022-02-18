
# SETUP -------------------------------------------------------------------

  library(tidyverse)


# DATA PREP ---------------------------------------------------------------

  # Load Datasets
  href_ss <- read.csv('./data/input/skater_stats.csv', stringsAsFactors = FALSE)
  href_stnd <- read.csv('./data/input/league_standings.csv', stringsAsFactors = FALSE)
  
  # Team Reference File
  team_ref <- read.csv('./ref/team_lookup.csv')
  
  # Skater Stats
  skater_stats <- href_ss %>%
    filter(
      Season >= 1922
    ) %>%
    select(
      Season, Game_Type, Player_ID, Player_Name, Age, Team_ID, Team_Name, Pos = Pos1, GP, G, A, PTS, TOI
    )
  
  # Remove TOT Seasons & arrange dataset for rank method purposes
  skater_stats_nontot <- skater_stats[!(skater_stats$Team_ID=="TOT"),]
  skater_stats_nontot <- skater_stats_nontot %>%
    group_by(Season, Game_Type, Player_ID, Player_Name, Pos, Team_ID, Team_Name) %>%
    summarise(
      GP = sum(GP),
      G = sum(G),
      A = sum(A),
      PTS = sum(PTS),
      TOI = sum(TOI)
    ) %>%
    arrange(
      Season,
      Game_Type,
      desc(G),
      desc(PTS),
      Player_Name
    ) %>%
    ungroup()
  
  # Standings
  leag_stand <- href_stnd %>%
    filter(
      Season >= 1922
    ) %>%
    select(
      Season, Team_ID, Team_Name, GP, GF, GA, PTS, Playoffs
    ) %>%
    rename(
      TEAM_GP = GP
    ) %>%
    group_by(
      Season
    ) %>%
    mutate(
      LEAG_GP = sum(TEAM_GP),
      LEAG_PTS = sum(PTS)
    ) %>%
    ungroup()
  
  # Create Position Reference Table
  pos_tbl <- data.frame(
    Pos = unique(skater_stats_nontot$Pos)
  )
  pos_tbl <- pos_tbl %>%
    mutate(
      Fwd_Def = if_else(
        Pos == "D", "D", "F"
      )
    )


# CALCULATE PLAYER SHARE OF TEAM STATS ------------------------------------

  # Create League PTS Table by Season
  leag_tbl <- leag_stand %>%
    select(
      Season, LEAG_PTS
    ) %>%
    group_by(
      Season
    ) %>%
    summarise(
      LEAG_PTS = unique(LEAG_PTS)
    )
  
  # Create Team Data to Calculate Player Share of Team Points
  team_dat <- skater_stats_nontot %>% 
    group_by(Season, Game_Type, Team_ID, Team_Name) %>%
    summarise(
      TMG = sum(G),
      TMA = sum(A),
      TMPTS = sum(PTS)
    ) %>%
    ungroup()
  
  # Calculate Player Share of Goals, Assists, and Points (Also Add F/D field based on pos ref table and Team data)
  player_share_team_offense <- skater_stats_nontot %>%
    left_join(
      pos_tbl,
      by = "Pos"
    ) %>%
    left_join(
      team_dat,
      by = c("Season", "Game_Type", "Team_ID", "Team_Name")
    ) %>%
    mutate(
      PCT_TMG = G/TMG,
      PCT_TMA = A/TMA,
      PCT_TMPTS = PTS/TMPTS
    ) %>%
    group_by(
      Season,
      Game_Type,
      Team_ID,
      Team_Name
    ) %>%
    arrange(
      desc(G),
      .by_group = TRUE
    ) %>%
    mutate(
      TMG_RNK = rank(desc(G), ties.method = "first"),
      PCT_TMG_CUMM = cumsum(PCT_TMG)
    ) %>%
    arrange(
      desc(A),
      .by_group = TRUE
    ) %>%
    mutate(
      TMA_RNK = rank(desc(A), ties.method = "first"),
      PCT_TMA_CUMM = cumsum(PCT_TMA)
    ) %>%
    arrange(
      desc(PTS),
      .by_group = TRUE
    ) %>%
    mutate(
      TMPTS_RNK = rank(desc(PTS), ties.method = "first"),
      PCT_TMPTS_CUMM = cumsum(PCT_TMPTS)
    ) %>%
    ungroup()
  

# ADD ADVANCED OFFENSIVE METRICS ------------------------------------------
  
  # Goals Created
  player_share_team_offense <- player_share_team_offense %>%
    mutate(
      GC = (G+0.5*A)*(TMG/(TMG+0.5*TMA)),
      PCT_GC = GC/TMG
    ) %>%
    group_by(
      Season,
      Game_Type,
      Team_ID,
      Team_Name
    ) %>%
    arrange(
      desc(GC),
      .by_group = TRUE
    ) %>%
    mutate(
      GC_RNK = rank(desc(GC), ties.method = "first"),
      PCT_GC_CUMM = cumsum(PCT_GC)
    ) %>%
    ungroup()
    
  # Offensive Point Shares
  player_share_team_offense <- player_share_team_offense %>%
    left_join(
      leag_tbl,
      by = "Season"
    ) %>%
    group_by(
      Season,
      Game_Type,
      Team_ID,
      Team_Name,
      Fwd_Def
    ) %>%
    mutate(
      MG = if_else(
        is.na(TOI) == TRUE | Season < 1999,
        GC-(7/12)*GP*((sum(GC)/sum(GP))),
        GC-(7/12)*TOI*((sum(GC)/sum(TOI)))
      )
    ) %>%
    group_by(
      Season,
      Game_Type
    ) %>%
    mutate(
      MGP = sum(G)/LEAG_PTS,
      OPS = MG/MGP
    ) %>%
    group_by(
      Season,
      Game_Type,
      Team_ID,
      Team_Name
    ) %>%
    mutate(
      PCT_OPS = OPS/sum(OPS)
    ) %>%
    arrange(
      desc(OPS),
      .by_group = TRUE
    ) %>%
    mutate(
      OPS_RNK = rank(desc(OPS), ties.method = "first"),
      PCT_OPS_CUMM = cumsum(PCT_OPS)
    ) %>%
    ungroup()


# REMOVE OPS & RELATED METRICS FROM POSTSEASON ----------------------------
  
  # This is not relevant for the postseason as this is a metric that captures how each player contributes
  # to points in the standings, i.e. what contribution they had to team getting 2-points after a win
  
  player_share_team_offense <- player_share_team_offense %>%
    mutate(
      LEAG_PTS = case_when(
        Game_Type == "POST" ~ NA_integer_,
        TRUE ~ LEAG_PTS
      ),
      MG = case_when(
        Game_Type == "POST" ~ NA_real_,
        TRUE ~ MG
      ),
      MGP = case_when(
        Game_Type == "POST" ~ NA_real_,
        TRUE ~ MGP
      ),
      OPS = case_when(
        Game_Type == "POST" ~ NA_real_,
        TRUE ~ OPS
      ),
      PCT_OPS = case_when(
        Game_Type == "POST" ~ NA_real_,
        TRUE ~ PCT_OPS
      ),
      OPS_RNK = case_when(
        Game_Type == "POST" ~ NA_integer_,
        TRUE ~ OPS_RNK
      ),
      PCT_OPS_CUMM = case_when(
        Game_Type == "POST" ~ NA_real_,
        TRUE ~ PCT_OPS_CUMM
      )
    )


# SAVE OUTPUT -------------------------------------------------------------
  
  write.csv(player_share_team_offense, "./data/output/player_share_team_offense.csv", row.names = FALSE)
  
  
  