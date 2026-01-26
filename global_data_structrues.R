#' Contains many data structures that will be used throughout mdbl_scoreboard_app


#### DT::datatable options #####################################################

# Options used for all DT::datatable tables
dt_datable_options <-
    list(
        lengthChange = FALSE,
        searching = FALSE,
        info = FALSE,
        paging = FALSE,
        scrollX = TRUE
    )


#### Making datatables from google sheets data #################################

# Creating table of the metadata of each session
sessions_df <- mdbl_scoreboard_raw_excel_filename %>%
    read_excel(sheet = "Sessions") %>%
    rename(
        session_id = `Session ID`,
        date = Date,
        weekday = Weekday,
        start_time = `Start Time`,
        end_time = `End Time`,
        season_num = `Season Number`,
        comp = Competition,
        matchweek = Matchweek,
        bowling_alley = `Bowling Alley`,
        num_lanes = `Number of Lanes`
    ) %>%
    mutate(
        across(c(session_id, weekday, comp, bowling_alley), as.character),
        across(c(season_num, matchweek, num_lanes), as.integer),
        date = as_date(date),
        across(c(start_time, end_time), as_datetime)
    )

# Creating table with the ID info of each bowler
bowlers_df <- mdbl_scoreboard_raw_excel_filename %>%
    read_excel(sheet = "Bowlers") %>%
    rename(
        bowler_id = `Bowler ID`,
        bowler = Bowler
    )

# Creating intermediate table of the scores from each session
scores_pre_separating_throws_and_frame <- mdbl_scoreboard_raw_excel_filename %>%
    read_excel(
        sheet = "Scores",
        
        # must assign names as such due to the irregular naming of columns in the
        # google sheets "Scores" sheet
        col_names =
            c(
                
                # identifying information for each row
                "session_id", "game_num", "lane_num", "row_unit",
                "bowler_id", "bowler",
                
                # each bowling frame-throw combination, which has the form "f#t#"
                expand_grid(
                    frame = 1:10,
                    throw = 1:3
                ) %>%
                    filter(!(frame %in% 1:9 & throw == 3)) %>%
                    arrange(frame, throw) %>%
                    mutate(frame_throw = str_c("f", frame, "t", throw)) %>%
                    .$frame_throw,
                
                # score info
                "game_score", "cum_session_score"
            )
    ) %>%
    
    # has column info
    slice(-(1:2)) %>%
    
    # no longer needed
    select(-cum_session_score) %>%
    
    # filling data downward (these cols are part of merged cells in the google sheet)
    fill(session_id, game_num, lane_num, bowler_id, bowler, game_score, .direction = "down") %>%
    
    
    mutate(
        across(c(session_id, row_unit, bowler_id, bowler, f1t1:f10t3), as.character),
        across(f1t1:f10t3, function(col) str_remove(col, "\\.0$")),
        across(c(game_num, lane_num, game_score, game_score), as.integer)
    ) %>%
    left_join(
        sessions_df %>%
            select(session_id, comp),
        by = join_by(session_id)
    ) %>%
    relocate(comp, .after = session_id)

# Creating table of scores for each throw
scores_per_throw_df <- scores_pre_separating_throws_and_frame %>%
    filter(row_unit == "Per Throw") %>%
    select(-c(row_unit, game_score))

# Creating table of scores for each frame
# Also includes game score
scores_per_frame_df <- scores_pre_separating_throws_and_frame %>%
    filter(row_unit == "Per Frame") %>%
    select(-row_unit) %>%
    
    # all frame data is in the col corresponding to the first throw of a frame
    select(
        session_id, comp, game_num, lane_num, bowler_id, bowler,
        ends_with("t1"), game_score
    ) %>%
    rename_with(function(col) str_remove(col, "t1$"), f1t1:f10t1) %>%
    mutate(across(f1:f10, as.integer))


# Not needed anymore
rm(scores_pre_separating_throws_and_frame)


#### Games cutoff for qualifying for "Average Score" standings points ##########

avg_score_games_qualify_cutoff <- scores_per_frame_df %>%
    filter(comp == "Regular Season") %>%
    distinct(session_id, game_num) %>%
    nrow() %>%
    `*`(AVG_SCORE_FRAC_QUALIFY_CUTOFF) %>%
    
    # rounding up to nearest integer
    ceiling()


#### Contains the labels in the correct order to identify dates and games ######
# Use the ordering of the cols for factor levels

date_game_plt_labels <- scores_per_frame_df %>%
    distinct(session_id, game_num) %>%
    left_join(
        sessions_df %>%
            select(session_id, date),
        by = join_by(session_id)
    ) %>%
    
    # ensuring correct ordering
    arrange(date) %>%
    
    # formatting with just date and date-game combo
    mutate(
        date_label = format(date, DATE_PLOT_FORMAT),
        date_game_num_label = str_c(date_label, game_num, sep = " - "),
        
        date_label =
            factor(
                date_label,
                levels = unique(date_label),
                ordered = TRUE
            ),
        date_game_num_label =
            factor(
                date_game_num_label,
                levels = unique(date_game_num_label),
                ordered = TRUE
            )
    )


#### `labeller()` object for per bowler facet plots ############################

per_bowler_labeller <- labeller(
    bowler_id = bowlers_df %>%
        pull(var = bowler, name = bowler_id)
)


#### `theme()` object to rotate x-axis labels 90 degrees #######################

rotate_x_axis_labels_90d <-
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
