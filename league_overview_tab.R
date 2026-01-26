#' Contains objects and functions for both the ui and server() of the
#' "League Overview" tab of mdbl_scorekeeper_app

# UI object for League Overview tab
league_overview_tab_ui <- 
    tabItem(
        tabName = "league_overview_tab",
        
        fluidRow(
            box(
                title =
                    HTML(
                        "League Standings",
                        "<font size='2'>",
                        as.character(
                            actionLink(
                                inputId = "league_standings_popup",
                                label = "",
                                icon = icon("info-circle"),
                                style = "color: #00274C"
                            )
                        ),
                        "</font>"
                    ),
                width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                
                # TODO: add info popup of how standings are calculated
                # TODO: do same for stats box
                # See barrels tab
                DTOutput("league_standings_table")
            ),
            
            box(
                title =
                    HTML(
                        "League Stats",
                        "<font size='2'>",
                        as.character(
                            actionLink(
                                inputId = "league_stats_popup",
                                label = "",
                                icon = icon("info-circle"),
                                style = "color: #00274C"
                            )
                        ),
                        "</font>"
                    ),
                    
                width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                
                column(
                    width = 12,
                    pickerInput(
                        inputId = "league_stats_comps_input",
                        label = "Competitions to Include",
                        choices = c("Preseason", "Regular Season"),
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE
                    )
                ),
                column(
                    width = 12,
                    actionButton(
                        inputId = "league_stats_input_button",
                        label = "Calculate League Stats",
                        width = "100%",
                        style = "color: #fff; background-color: #00274C;"
                    )
                ),
                
                # Container for table and plot output, so that there is not
                # empty space when no comps are selected
                uiOutput("league_stats_ui")
            )
        )
    )


# Outputting league standings as a DT::datatable object
league_standings_table_fn <- function() {
    
    # TODO: link each bowler to their page

    scores_per_frame_df %>%
        filter(comp == "Regular Season") %>%
        group_by(comp,  bowler_id) %>%
        summarize(
            num_sessions = length(unique(session_id)),
            num_games = n(),
            total_score = sum(game_score),
            avg_score = mean(game_score),
            .groups = "drop"
        ) %>%
        
        # TODO: do we want to do fraction game-wins / session-wins if tie?
        
        # bringing in game wins
        left_join(
            scores_per_frame_df %>%
                filter(comp == "Regular Season") %>%
                filter(game_score == max(game_score), .by = c(session_id, game_num)) %>%
                group_by(comp, bowler_id) %>%
                summarize(num_game_wins = n(), .groups = "drop"),
            by = join_by(comp, bowler_id)
        ) %>%
        
        # bringing in session wins
        left_join(
            scores_per_frame_df %>%
                filter(comp == "Regular Season") %>%
                filter(game_score == max(game_score), .by = session_id) %>%
                group_by(comp, bowler_id) %>%
                summarize(num_session_wins = n(), .groups = "drop"),
            by = join_by(comp, bowler_id)
        ) %>%
        
        mutate(
            # replacing NAs with 0
            across(c(num_game_wins, num_session_wins), function(col) if_else(is.na(col), 0L, col)),
            
            # duplicating `avg_score` col, but converting values to NA if the
            # bowler has not met the cutoff for number of games played
            avg_score_with_cutoff =
                if_else(num_games >= avg_score_games_qualify_cutoff, avg_score, NA_real_),
            
            # ranking `total_score` and `avg_score_with_cutoff` cols
            across(
                c(total_score, avg_score_with_cutoff),
                function(col) rank(-col, ties.method = "min"),
                .names = "{.col}_rank"
            ),
            
            # using triangle numbers to assign points accrued for `total_score`
            # and `avg_score_with_cutoff`
            across(
                c(total_score_rank, avg_score_with_cutoff_rank),
                function(col) {
                    case_when(
                        col == 1 ~ 15L,
                        col == 2 ~ 10L,
                        col == 3 ~ 6L,
                        col == 4 ~ 3L,
                        col == 5 ~ 1L,
                        TRUE ~ 0L
                    )
                },
                .names = '{str_remove(.col, "_rank")}_points'
            ),
            
            # calculating total points
            points = total_score_points + avg_score_with_cutoff_points + num_game_wins + 3 * num_session_wins
        ) %>%
        
        # bringing in names
        left_join(bowlers_df, by = join_by(bowler_id)) %>%
        
        # cleaning table
        select(
            bowler, num_sessions, num_games,
            total_score, avg_score, num_session_wins, num_game_wins, points
        ) %>%
        arrange(
            desc(points), desc(total_score), desc(avg_score),
            desc(num_session_wins), desc(num_game_wins),
            desc(num_sessions), desc(num_games),
            bowler
        ) %>%
        common_renames() %>%
    
        datatable(
            rownames = FALSE,
            options = list(pageLength = calculating_page_length("Regular Season")) %>%
                append(dt_datable_options)
        ) %>%
        formatRound(columns = "Avg Score", digits = 1) %>%
        formatStyle(
            columns = "Points",
            backgroundColor = "#FBE6A3"
        )
}


# Outputting UI wrapper for "League Stats" box content
league_stats_ui_fn <- function(league_stats_input_in) {
    
    require_comps(league_stats_input_in$comps)
    
    list(
        column(width = 12, br(), hr()),

        tabsetPanel(
            type = "tabs",
            tabPanel(
                "Summary Stats",
                DTOutput("league_stats_summary_table")
            ),
            tabPanel(
                "Avg Score Post-Frame",
                DTOutput("league_stats_avg_game_table")
            )
        ),
        
        column(width = 12, br(), hr()),
        
        tabsetPanel(
            type = "tabs",
            tabPanel(
                "Leaguewide Game Scores",
                plotOutput("league_stats_leaguewide_scores_plt", height = 1600) #%>%
                    # withLoader(loader = "loader3")
            ),
            tabPanel(
                "Per Bowler Game Scores",
                plotOutput("league_stats_per_bowler_scores_plt", height = 1600) #%>%
                    # withLoader(loader = "loader3")
            )
        )
    )
}


# Outputting league summary stats as a DT::datatable object
league_stats_summary_table_fn <- function(league_stats_input_in) {
    
    require_comps(league_stats_input_in$comps)
    
    # Summary stats calculated from `scores_per_frame_df`
    summary_stats_by_frame <- scores_per_frame_df %>%
        filter(comp %in% league_stats_input_in$comps) %>%
        duplicate_df_for_leaguewide() %>%
        group_by(bowler_id) %>%
        summarize(
            num_sessions = length(unique(session_id)),
            num_games = n(),
            total_score = sum(game_score),
            avg_score = mean(game_score),
            min_score = min(game_score),
            max_score = max(game_score),
            .groups = "drop"
        )
    
    # Intermediate table used for calculating stats from `scores_per_throw_df`
    scores_per_throw_df_long_clean <- scores_per_throw_df %>%
        filter(comp %in% league_stats_input_in$comps) %>%
        
        # making each row correspond to a throw
        pivot_longer(cols = f1t1:f10t3, names_to = "frame_throw", values_to = "score") %>%
        
        # separating `frame_throw` into a `frame` and `throw` col
        mutate(
            frame = as.integer(str_extract(frame_throw, "(?<=f)[0-9]+")),
            throw = as.integer(str_sub(frame_throw, start = -1)),
            .after = frame_throw
        ) %>%
        
        duplicate_df_for_leaguewide() %>%
        
        # getting previous throw's score for the purpose of calculating stats
        # including 10th frame
        mutate(
            prev_score = lag(score, n = 1, default = NA_character_),
            .by = c(session_id, comp, game_num, lane_num, bowler_id, frame)
        ) %>%
        
        # effective_throw accounts for a closed frame in the 10th frame
        mutate(
            effective_throw =
                case_when(
                    is.na(score) ~ NA_integer_,
                    frame %in% 1:9 ~ throw,
                    frame == 10 & throw == 1 ~ 1L,
                    frame == 10 & throw == 2 & prev_score == "X" ~ 1L,
                    frame == 10 & throw == 2 & prev_score != "X" ~ 2L,
                    frame == 10 & throw == 3 & prev_score %in% c("X", "/") ~ 1L,
                    frame == 10 & throw == 3 & !(prev_score %in% c("X", "/")) ~ 2L
                ),
            .after = throw
        )
    
    # Summary stats about strikes and spares
    summary_stats_closed_frames <- scores_per_throw_df_long_clean %>%
        
        # calculating frequency of opportunities and conversions for strikes and
        # spares (including different types of spares) 
        mutate(
            is_strike_opp = (!is.na(effective_throw) & effective_throw == 1),
            is_strike = (is_strike_opp & !is.na(score) & score == "X"),
            is_spare_opp = (!is.na(effective_throw) & effective_throw == 2),
            is_spare = (is_spare_opp & !is.na(score) & score == "/"),
            
            is_nonsplit_spare_opp =(is_spare_opp & !is.na(prev_score) & !str_detect(prev_score, "s$")),
            is_nonsplit_spare = (is_nonsplit_spare_opp & !is.na(score) & score == "/"),
            is_split_spare_opp = (is_spare_opp & !is.na(prev_score) & str_detect(prev_score, "s$")),
            is_split_spare = (is_split_spare_opp & !is.na(score) & score == "/"),
            
            is_single_pin_spare_opp = (is_spare_opp & !is.na(prev_score) & prev_score == "9"),
            is_single_pin_spare = (is_single_pin_spare_opp & !is.na(score) & score == "/"),
            is_multi_pin_spare_opp = (is_spare_opp & !is.na(prev_score) & prev_score != "9"),
            is_multi_pin_spare = (is_multi_pin_spare_opp & !is.na(score) & score == "/")
        ) %>%
        group_by(bowler_id) %>%
        summarize(
            across(
                c(
                    is_strike_opp, is_strike,
                    is_spare_opp, is_spare,
                    is_nonsplit_spare_opp, is_nonsplit_spare,
                    is_split_spare_opp, is_split_spare,
                    is_single_pin_spare_opp, is_single_pin_spare,
                    is_multi_pin_spare_opp, is_multi_pin_spare
                ),
                sum,
                .names = '{str_remove(.col, "^is_")}s'
            ),
            .groups = "drop"
        ) %>%
        
        # calculating success rate for strikes and spares (including different
        # types of spares) 
        mutate(
            strike_rate = strikes / strike_opps,
            spare_rate = spares / spare_opps,
            nonsplit_spare_rate = nonsplit_spares / nonsplit_spare_opps,
            split_spare_rate = split_spares / split_spare_opps,
            single_pin_spare_rate = single_pin_spares / single_pin_spare_opps,
            multi_pin_spare_rate = multi_pin_spares / multi_pin_spare_opps
        ) %>%
        
        # remove frequency and opportunity cols, just keeping rate cols
        select(
            bowler_id,
            strike_rate, spare_rate, 
            nonsplit_spare_rate, split_spare_rate,
            single_pin_spare_rate, multi_pin_spare_rate
        )
    
    # Summary stats about first throws per frame
    summary_stats_first_throws <- scores_per_throw_df_long_clean %>%
        filter(!is.na(effective_throw), effective_throw == 1) %>%
        
        # remove split markers, and converting strikes to 10
        # note that spares cant happen on the first throw of a frame
        mutate(
            score_int = map_int(score, function(score_in) {
                score_in %>%
                    str_remove("s$") %>%
                    str_replace("^X$", "10") %>%
                    as.integer()
            })
        ) %>%
        
        # calculating avg score on the first throw, as well as gutter rate
        group_by(bowler_id) %>%
        summarize(
            avg_first_throw = mean(score_int),
            first_throw_gutter_rate = mean(score_int == 0),
            .groups = "drop"
        )
    
    # CSS strings for formatting the outputted DT::datatable
    border_right_css <- "solid 1px"
    border_right_color_css <- "rgba(0, 0, 0, 0.3)"
    border_right_all_css = str_c("border-right: ", border_right_css, "; border-right-color: ", border_right_color_css, ";")
    
    # Joining together all calculated stats and converting to DT::datatable
    summary_stats_by_frame %>%
        
        # joining data together
        inner_join(summary_stats_closed_frames, by = join_by(bowler_id)) %>%
        inner_join(summary_stats_first_throws, by = join_by(bowler_id)) %>%
        
        adding_bowler_names() %>%
        common_renames() %>%
        datatable(
            rownames = FALSE,
            
            # # adding title
            # caption = tags$caption(
            #     style = "caption-side: top; text-align: left; color: black; font-size: 200%; text-decoration: underline;",
            #     "Summary Stats"
            # ),
            
            options =
                list(pageLength = calculating_page_length(league_stats_input_in$comps)) %>%
                append(dt_datable_options),
            
            # adding spanners above the col names, to group like cols together
            container =
                withTags(
                    table(
                        class = "display",
                        thead(
                            tr(
                                # bowler, # sessions, # wins cols
                                th(rowspan = 1),
                                th(rowspan = 1),
                                th(rowspan = 1, style = border_right_all_css),
                                
                                # from `summary_stats_by_frame`
                                th(colspan = 4, "Score Summary", style = border_right_all_css),
                                
                                # from `summary_stats_closed_frames`
                                th(colspan = 6, "Closed Frame Rates", style = border_right_all_css),
                                
                                # from `summary_stats_first_throws`
                                th(colspan = 2, "1st Throw")
                            ),
                            
                            # adding colnames back in
                            tr(
                                (.) %>%
                                    colnames() %>%
                                    
                                    # making sure col names at the end of spanners
                                    # have right border
                                    # cant use `map()`
                                    lapply(function(colname) {
                                        if (colname %in% c("# Games", "Max Score", "Multi-Pin Spare %")) {
                                            th(colname, style = border_right_all_css)
                                        } else {
                                            th(colname)
                                        }
                                    })
                            )
                        )
                    )
                )
        ) %>%
        
        # adding right border across entire cols at the end of spanners
        formatStyle(
            columns = c("# Games", "Max Score", "Multi-Pin Spare %"),
            `border-right` = border_right_css,
            `border-right-color` = border_right_color_css
        ) %>%
        
        formatRound(
            columns = c("Avg Score", "Avg First Throw"),
            digits = 1
        ) %>%
        formatPercentage(
            columns = (.) %>%
                .$x %>%
                attr("colnames") %>%
                str_subset(" %$"),
            digits = 1
        ) %>%
        formatStyle(
            columns = "Bowler",
            target = "row",
            backgroundColor = styleEqual("Leaguewide", "lightgray")
        )
}

# Outputting league avg game as a DT::datatable object
# TODO: this could also be graph
league_stats_avg_game_table_fn <- function(league_stats_input_in) {
    
    require_comps(league_stats_input_in$comps)
    
    scores_per_frame_df %>%
        filter(comp %in% league_stats_input_in$comps) %>%
        
        duplicate_df_for_leaguewide() %>%
        
        # calculating avg score at the end of each frame
        group_by(bowler_id) %>%
        summarize(across(f1:f10, mean)) %>%
        
        adding_bowler_names() %>%
        common_renames() %>%
        rename_with(function(col) str_replace(col, "f", "F")) %>%
        datatable(
            rownames = FALSE,
            # caption = tags$caption(
            #     style = "caption-side: top; text-align: left; color: black; font-size: 200%; text-decoration: underline;",
            #     "Average Score Post-Frame"
            # ),
            options = list(calculating_page_length(league_stats_input_in$comps)) %>%
                append(dt_datable_options)
        ) %>%
        formatRound(columns = str_c("F", 1:10), digits = 1) %>%
        formatStyle(
            columns = "Bowler",
            target = "row",
            backgroundColor = styleEqual("Leaguewide", "lightgray")
        )
}


# Plotting histogram of league scores 
league_stats_hist_scores_plt_fn <- function(league_stats_input_in, per_bowler) {
    
    require_comps(league_stats_input_in$comps)
    
    # # size of bins in outputted plot
    # bin_size <- 10L
    
    scores_per_frame_df %>%
        filter(comp %in% league_stats_input_in$comps) %>%
        
        # calculating `game_score_label`, which is the x-axis of the plot
        # groups `game_score` into bins of size `bin_size`
        mutate(
            game_score_label_num = floor(game_score / 10) * 10,
            game_score_label = str_c(game_score_label_num, "s"),
            game_score_label =
                factor(
                    game_score_label,
                    levels =
                        str_c(
                            seq(
                                min(game_score_label_num),
                                max(game_score_label_num),
                                by = 10
                            ),
                            "s"
                        ),
                    ordered = TRUE
                ),
        ) %>%
        
        # ensures faceted plot will be ordered by player name
        when(
            per_bowler ~ (.) %>%
                factor_bowler_id_by_bowler(),
            !per_bowler ~ (.)
        ) %>%
        
        ggplot(aes(x = game_score_label)) +
        
        # prefer barplot to histogram because it spaces the values and allows labels
        geom_bar() +
        
        # adding text to top inner part of bars
        geom_text(
            stat = "Count",
            aes(label = after_stat(count)),
            vjust = 1.5,
            color = ggthemes_data[["fivethirtyeight"]] %>%
                filter(name == "Light Gray") %>%
                .$value,
            size = ifelse(per_bowler, 5, 7),
            family = plot_font_name
        ) +
        
        # faceting plot
        {
            if (per_bowler) {
                list(
                    facet_wrap(~bowler_id, labeller = per_bowler_labeller),
                    rotate_x_axis_labels_90d
                )
            }
        } +
        
        # Ensure empty intermediate breaks arent dropped
        scale_x_discrete(drop = FALSE) + 
        
        # Ensuring y-axis labels are integers
        scale_y_continuous(breaks = integer_breaks) +
        
        labs(
            title =
                str_c(
                    "MDBL",
                    if_else(per_bowler, "per Bowler", "Leaguewide"),
                    "Game Scores",
                    sep = " "
                ),
            subtitle = generate_comps_subtitle(league_stats_input_in$comps),
            x = "Score",
            y = "Count"
        )
}

# Plotting lineplot of league scores 
league_stats_temporal_scores_plt_fn <- function(league_stats_input_in, per_bowler, per_game, remove_title = FALSE) {
    
    # TODO: if multiple sessions occur on single day, must edit this func
    
    require_comps(league_stats_input_in$comps)
    
    date_label_format <- "%m/%d/%y"
    
    scores_per_frame_df %>%
        filter(comp %in% league_stats_input_in$comps) %>%
        
        when(
            per_bowler & per_game ~ (.) %>%
                select(session_id, game_num, bowler_id, game_score),
            
            per_bowler & !per_game ~ (.) %>%
                group_by(session_id, bowler_id) %>%
                summarize(session_score = mean(game_score), .groups = "drop") %>%
                select(session_id, bowler_id, session_score),
            
            !per_bowler & per_game ~ (.) %>%
                group_by(session_id, game_num) %>%
                summarize(game_score = mean(game_score), .groups = "drop") %>%
                select(session_id, game_num, game_score),
            
            !per_bowler & !per_game ~ (.) %>%
                group_by(session_id) %>%
                summarize(session_score = mean(game_score), .groups = "drop") %>%
                select(session_id, session_score)
        ) %>%
        
        when(
            per_game ~ (.) %>%
               left_join(
                   date_game_plt_labels %>%
                       distinct(session_id, game_num, date_game_num_label),
                   by = join_by(session_id, game_num)
               ),
            !per_game ~ (.) %>%
                left_join(
                    date_game_plt_labels %>%
                        distinct(session_id, date_label),
                    by = join_by(session_id)
                )
        ) %>%
        
        # ensures faceted plot will be ordered by player name
        when(
            per_bowler ~ (.) %>%
                factor_bowler_id_by_bowler(),
            !per_bowler ~ (.)
        ) %>%
        
        rename(
            any_of(
                c(
                    "session_or_game_label" = "date_game_num_label",
                    "session_or_game_label" = "date_label",
                    "score" = "game_score",
                    "score" = "session_score"
                )
            )
        ) %>%
        
        ggplot(aes(x = session_or_game_label, y = score, group = 1)) +
        geom_line() +
        
        # faceting plot
        {
            if (per_bowler) {
                list(
                    facet_wrap(~bowler_id, labeller = per_bowler_labeller),
                    geom_point(),
                    rotate_x_axis_labels_90d,
                    
                    # looks weird to have when unfaceted, too much empty space
                    scale_y_continuous(limits = c(0, NA))
                )
            } else {
                geom_label(
                    aes(label = round(score, digits = 1)),
                    family = plot_font_name,
                    size = 7
                )
            }
        } +
        
        {
            if (remove_title) {
                labs(
                    x = if_else(per_game, "Game", "Date"),
                    y = "Score"
                )
            } else {
                labs(
                    title =
                        str_c(
                            "MDBL",
                            if_else(per_bowler, "per Bowler", "Leaguewide"),
                            if_else(per_game, "Game", "Session"),
                            "Scores",
                            sep = " "
                        ),
                    subtitle = generate_comps_subtitle(league_stats_input_in$comps),
                    x = if_else(per_game, "Game", "Date"),
                    y = "Score" 
                )
            }
        }
}

# Creating and combining histogram and lineplot of game scores
generate_league_scores_plots <- function(league_stats_input_in, per_bowler = FALSE) {
    
    plot_grid(
        league_stats_hist_scores_plt_fn(league_stats_input_in, per_bowler = per_bowler),
        league_stats_temporal_scores_plt_fn(league_stats_input_in, per_bowler = per_bowler, per_game = TRUE, remove_title = TRUE),
        ncol = 1,
        rel_heights = c(0.55, 0.45)
    )
}
