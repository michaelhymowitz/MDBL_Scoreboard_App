#' Contains objects and functions for both the ui and server() of the
#' "Bowler Profiles" tab of mdbl_scorekeeper_app

# UI object for Bowler Profiles tab
bowler_profiles_tab_ui <- 
    tabItem(
        tabName = "bowler_profiles_tab",
        
        fluidRow(
            box(
                title = "Inputs",
                width = 12, status = "success", solidHeader = TRUE, collapsible = FALSE,
                
                column(
                    width = 6,
                    pickerInput(
                        inputId = "bowler_profiles_bowler_id_input",
                        label = "Select Bowler",
                        choices = bowlers_df %>%
                            arrange(bowler) %>%
                            pull(bowler_id, name = bowler),
                        multiple = TRUE,
                        options = pickerOptions(maxOptions = 1)
                    )
                ),
                
                column(
                    width = 6,
                    pickerInput(
                        inputId = "bowler_profiles_comps_input",
                        label = "Competitions to Include",
                        choices = c("Friendly", "Regular Season", "Playoffs"),
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE
                    )
                ),
                
                column(
                    width = 12,
                    actionButton(
                        inputId = "bowler_profiles_input_button",
                        label = "Generate Bowler Profile",
                        width = "100%",
                        style = "color: #000000; background-color: #FBE6A3;"
                    )
                )
            ),
            
            # Container for table and plot output, so that there is not
            # empty space when no comps are selected
            uiOutput("bowler_profiles_output_ui")
        )
    )


# Outputting UI wrapper for selected bowler's box content
bowler_profiles_output_ui_fn <- function(bowler_profiles_input_in) {
    
    req(length(bowler_profiles_input_in$bowler_id) == 1)
    req(length(bowler_profiles_input_in$comps) > 0)
    
    # Requiring that the selected bowler has appeared in at least one of the
    # competitions selected
    req(
        scores_per_frame_df %>%
            filter(
                bowler_id == bowler_profiles_input_in$bowler_id,
                comp %in% bowler_profiles_input_in$comps
            ) %>%
            nrow() %>%
            `>`(0)
    )
    
    box(
        title =
            HTML(
                bowlers_df %>%
                    filter(bowler_id == bowler_profiles_input_in$bowler_id) %>%
                    .$bowler,
                "<font size='2'>",
                as.character(
                    actionLink(
                        inputId = "selected_bowler_popup",
                        label = "",
                        icon = icon("info-circle"),
                        style = "color: #00274C"
                    )
                ),
                "</font>"
            ),
        
        width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
        
        tabsetPanel(
            type = "tabs",
            tabPanel(
                "Summary Stats",
                DTOutput("bowler_profiles_summary_table")
            ),
            tabPanel(
                "Avg Score By Frame",
                DTOutput("bowler_profiles_avg_game_table")
            ),
            tabPanel(
                "Scoresheets",
                DTOutput("bowler_profile_scoresheets_table")
            ),
            # Removing for now because I don't think these graphs look good or are meaningful
            # tabPanel(
            #     "Stats Distribtution",
            #     plotOutput("bowler_profile_stats_distribution_plt")
            # ),
            tabPanel(
                "Stats per Game",
                br(),
                
                # If decide to bring stat distribution per-player plots, uncomment
                # here
                # fluidRow(
                #     column(
                #         width = 6,
                #         pickerInput(
                #             inputId = "bowler_profiles_stats_per_game_input",
                #             label = "Select Stats to Plot",
                #             choices =
                #                 common_renames_map[common_renames_map %in% per_game_stats],
                #             options = list(`actions-box` = TRUE),
                #             multiple = TRUE
                #         ),
                #     ),
                #     column(
                #         width = 6,
                #         pickerInput(
                #             inputId = "bowler_profiles_stats_plot_type_input",
                #             label = "Select Plot Type",
                #             choices = c(
                #                 "Selected Stats per Game",
                #                 "Disribution of Values of Selected Stats"
                #             ),
                #             multiple = TRUE,
                #             options = pickerOptions(maxOptions = 1)
                #         )
                #     )
                # ),
                
                pickerInput(
                    inputId = "bowler_profiles_stats_per_game_input",
                    label = "Select Stats to Plot",
                    choices =
                        common_renames_map[common_renames_map %in% per_game_stats],
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE
                ),
                actionButton(
                    inputId = "bowler_profiles_stats_per_game_input_button",
                    label = "Generate Plots",
                    width = "100%",
                    style = "color: #000000; background-color: #FBE6A3;"
                ),
                br(),
                
                # ensures box doesn't get larger before the stats are selected
                # also allowes for setting height of plot
                uiOutput("bowler_profile_stats_per_game_plt_wrapper")
            ),
        )
    )
}


# Outputting selected bowler's stats as a DT::datatable object
bowler_profiles_summary_table_fn <- function(bowler_profiles_input_in) {
    
    # number of games needed to qualify for qualified stat ranks
    num_games_needed_for_qualified_ranks <- scores_per_frame_df %>%
        filter(comp %in% bowler_profiles_input_in$comps) %>%
        distinct(session_id, game_num) %>%
        nrow() %>%
        `*`(AVG_SCORE_FRAC_QUALIFY_CUTOFF) %>%
        
        # rounding up to nearest integer
        ceiling()
    
    bowler_profiles_input_in %>%
        
        # calculating stats for each bowler
        league_stats_summary_table_fn(return_as_dt = FALSE) %>%
        
        filter(bowler_id != "__leaguewide__") %>%
        select(
            -c(
                strike_opps, spare_opps,
                nonsplit_spare_opps, split_spare_opps,
                single_pin_spare_opps, multi_pin_spare_opps
            )
        ) %>%
        
        # denoting if bowler qualifies for qualified stat ranks
        mutate(
            is_qualified = (num_games >= num_games_needed_for_qualified_ranks),
            .after = bowler_id
        ) %>%
        
        # pivoting longer to calculate ranks across all stats at once
        pivot_longer(
            cols = -c(bowler_id, is_qualified),
            names_to = "stat_name",
            values_to = "stat_value"
        ) %>%
        
        # calculating qualified and unqualified ranks
        mutate(
            # overall rankings, both for when higher vs lower is better
            stat_higher_better_rank = rank(-stat_value, na.last = "keep", ties.method = "min"),
            stat_lower_better_rank = rank(stat_value, na.last = "keep", ties.method = "min"),
            
            # making stats NA if the bowler is not qualified
            stat_value_qualified = if_else(is_qualified, stat_value, NA_real_),
            
            # qualified rankings, both for when higher vs lower is better
            stat_qualified_higher_better_rank = rank(-stat_value_qualified, na.last = "keep", ties.method = "min"),
            stat_qualified_lower_better_rank = rank(stat_value_qualified, na.last = "keep", ties.method = "min"),
            .by = stat_name
        ) %>%
        
        # calculating final overall and qualified ranks
        # note that "first_throw_gutter_rate" is the only stat where lower is better
        # also only considering if a player is qualified on rate stats
        mutate(
            stat_rank = if_else(stat_name == "first_throw_gutter_rate", stat_lower_better_rank, stat_higher_better_rank),
            stat_qualified_rank =
                case_when(
                    stat_name == "first_throw_gutter_rate" ~ stat_qualified_lower_better_rank,
                    
                    # these are the only rate stats
                    stat_name %in%
                        c(
                            "strike_rate", "spare_rate", "nonsplit_spare_rate", "split_spare_rate",
                            "single_pin_spare_rate", "multi_pin_spare_rate"
                        ) ~ stat_qualified_higher_better_rank,
                    
                    TRUE ~ stat_higher_better_rank
                )
        ) %>%
        
        # removing columns no longer needed
        select(bowler_id, stat_name, stat_value, stat_rank, stat_qualified_rank) %>%
        
        # denoting ties with prefixed "T"
        mutate(has_rank_tie = (n() >= 2), .by = c(stat_name, stat_rank)) %>%
        mutate(has_qualified_rank_tie = (n() >= 2), .by = c(stat_name, stat_qualified_rank)) %>%
        mutate(
            stat_rank = 
                if_else(
                    has_rank_tie,
                    str_c("T", stat_rank),
                    as.character(stat_rank)
                ),
            
            stat_qualified_rank = 
                if_else(
                    has_qualified_rank_tie,
                    str_c("T", stat_qualified_rank),
                    as.character(stat_qualified_rank)
                )
        ) %>%
        select(-has_rank_tie, -has_qualified_rank_tie) %>%
        
        # keeping only bowler selected
        filter(bowler_id == bowler_profiles_input_in$bowler_id) %>%
        select(-bowler_id) %>%
        
        mutate(
            
            # stat_category allows us to effectively group the output by like stats
            stat_category =
                case_when(
                    stat_name %in% c("num_sessions", "num_games") ~ "Appearances",
                    stat_name %in% c("total_score", "avg_score", "min_score", "max_score") ~ "Score Summary",
                    stat_name %in% c("strikes", "strike_rate", "spares", "spare_rate") ~ "Closed Frames",
                    stat_name %in% c("nonsplit_spares", "nonsplit_spare_rate", "split_spares", "split_spare_rate") ~ "Non-Split & Split Spares",
                    stat_name %in% c("single_pin_spares", "single_pin_spare_rate", "multi_pin_spares", "multi_pin_spare_rate") ~ "Single- & Multi-Pin Spares",
                    stat_name %in% c("avg_first_throw", "first_throw_gutter_rate") ~ "1st Throw"
                ),
            
            # cleaning the values here, because all values will be represented
            # in a single column as character-type
            stat_value =
                case_when(
                    stat_name  == "total_score" ~ comma(stat_value, accuracy = 1),
                    
                    stat_name %in%
                        c(
                            "num_sessions", "num_games", "min_score", "max_score",
                            "strikes", "spares", "nonsplit_spares", "split_spares",
                            "single_pin_spares", "multi_pin_spares"
                        ) ~ as.character(round(stat_value, digits = 0)),
                    
                    stat_name %in%
                        c(
                            "avg_score", "avg_first_throw"
                        ) ~ as.character(round(stat_value, digits = 1)),
                    
                    stat_name %in%
                        c(
                            "strike_rate", "spare_rate",
                            "nonsplit_spare_rate", "split_spare_rate",
                            "single_pin_spare_rate", "multi_pin_spare_rate",
                            "first_throw_gutter_rate"
                        ) ~ percent(stat_value, accuracy = 0.1)
                )
        ) %>%
        
        # cleaning, organizing, and prepping table for `datatable()` call
        order_and_clean_stat_names() %>%
        rename(
            Value = stat_value,
            `Leaguewide<br/>Rank` = stat_rank,
            `Qualified<br/>Leaguewide<br/>Rank` = stat_qualified_rank
        ) %>%
        column_to_rownames("clean_stat_name") %>%
        
        datatable(
            rownames = TRUE,
            
            # ensures <br/> happens in colnames
            escape = FALSE,
            
            # adding spanners above the col names, to group like cols together
            extensions = "RowGroup",
            
            options =
                list(
                    # TODO: check if need to add pageLength
                    
                    # preventing table from being ordered by a column
                    # looks weird with the row groups
                    ordering = FALSE,
                    
                    # grouping rows based on `stat_category`, then hiding col
                    # note these are 0-indexed
                    rowGroup = list(dataSrc = 4),
                    columnDefs = list(list(visible = FALSE, targets = 4))
                ) %>%
                append(dt_datable_options)
        )
}


# Outputting selected bowler's avg game as a DT::datatable object
bowler_profiles_avg_game_table_fn <- function(bowler_profiles_input_in) {
    
    bowler_profiles_input_in %>%
        
        # calculating avg score post- and per-frame
        league_stats_avg_frames_table_fn(return_as_dt = FALSE) %>%
        
        filter(bowler_id != "__leaguewide__") %>%
        
        # ranking bowlers by their score after each frame
        mutate(
            avg_score_post_frame_rank = rank(-avg_score_post_frame, ties.method = "min"),
            avg_pts_per_frame_rank = rank(-avg_pts_per_frame, ties.method = "min"),
            .by = frame
        ) %>%
        
        # orders rows better after `pivot_wider()`
        relocate(avg_score_post_frame_rank, .after = avg_score_post_frame) %>%
        
        # denoting ties with prefixed "T"
        mutate(has_post_frame_rank_tie = (n() >= 2), .by = c(frame, avg_score_post_frame_rank)) %>%
        mutate(has_per_frame_rank_tie = (n() >= 2), .by = c(frame, avg_pts_per_frame_rank)) %>%
        mutate(
            avg_score_post_frame_rank = 
                if_else(
                    has_post_frame_rank_tie,
                    str_c("T", avg_score_post_frame_rank),
                    as.character(avg_score_post_frame_rank)
                ),
            
            avg_pts_per_frame_rank = 
                if_else(
                    has_per_frame_rank_tie,
                    str_c("T", avg_pts_per_frame_rank),
                    as.character(avg_pts_per_frame_rank)
                )
        ) %>%
        select(-has_post_frame_rank_tie, -has_per_frame_rank_tie) %>%
        
        # keeping only bowler selected
        filter(bowler_id == bowler_profiles_input_in$bowler_id) %>%
        select(-bowler_id) %>%
        
        # cleaning values for eventual `datatable()`
        mutate(
            across(
                c(avg_score_post_frame, avg_pts_per_frame),
                function(col) as.character(round(col, digits = 1))
            )
        ) %>%
        
        # reorganizing table back to wide form
        pivot_longer(
            cols = -frame,
            names_to = "stat_name",
            values_to = "stat_value"
        ) %>%
        pivot_wider(
            names_from = frame,
            values_from = stat_value
        ) %>%
        
        # cleaning, organizing, and prepping table for `datatable()` call
        mutate(
            stat_name =
                case_when(
                    stat_name == "avg_score_post_frame" ~ "Avg Score Post-Frame",
                    stat_name == "avg_score_post_frame_rank" ~ "Avg Score Post-Frame Rank",
                    stat_name == "avg_pts_per_frame" ~ "Avg Score per-Frame",
                    stat_name == "avg_pts_per_frame_rank" ~ "Avg Score per-Frame Rank"
                )
        ) %>%
        rename_with(function(col) str_replace(col, "f", "F")) %>%
        column_to_rownames("stat_name") %>%
        
        datatable(
            rownames = TRUE,
            
            options =
                list(
                    # TODO: check if need to add pageLength
                    
                    # meaningless to sort
                    ordering = FALSE
                ) %>%
                append(dt_datable_options)
        )
}

# Outputting selected bowler's scoresheets as a DT::datatable object
bowler_profile_scoresheets_table_fn <- function(bowler_profiles_input_in) {
    
    # saving as table before plugging into `DT::datatable()` because i cant
    # access the table in the pipe all the ways it's needed
    bowler_scoresheets_pre_dt <-
        
        # binding scores per throw and scores per frame
        bind_rows(
            scores_per_throw_df %>%
                filter(bowler_id == bowler_profiles_input_in$bowler_id) %>%
                mutate(row_type = "per_throw", .before = everything()),
            
            scores_per_frame_df %>%
                filter(bowler_id == bowler_profiles_input_in$bowler_id) %>%
                
                # needed for binding with `scores_per_throw_df`
                mutate(across(f1:f10, as.character)) %>%
                rename_with(function(col) str_c(col, "t2"), .cols = f1:f10) %>%
                
                mutate(row_type = "per_frame", .before = everything())
        ) %>%
        filter(comp %in% bowler_profiles_input_in$comps) %>%
        arrange(session_id, game_num, desc(row_type == "per_throw")) %>%
        select(-comp, -bowler) %>%
        
        # bringing in session info
        left_join(
            sessions_df %>%
                select(session_id, date, season_num, comp, matchweek, bowling_alley),
            by = join_by(session_id)
        ) %>%
        relocate(date, season_num, comp, matchweek, bowling_alley, .after = session_id) %>%
        
        # fixing date for 2026-02-04 satellite session and formatting date properly
        # here, can't format like this in `DT::datatable()`
        mutate(
            date =
                as_date(
                    if_else(
                        session_id == "202602021" &
                            bowler_id %in% c(
                                "jacob_pavlawk_1","seth_howell_1",
                                "tommy_cummins_1", "liam_klaiman_1" 
                            ),
                        as_date("2026-02-04"),
                        date
                    )
                ),
            date = format(date, "%b %d, %Y")
        ) %>%
        select(-session_id, -bowler_id) %>%
        
        # removing duplicated data that clutters output
        mutate(
            across(
                c(date, season_num, comp, matchweek, bowling_alley, game_num, lane_num),
                function(col) if_else(row_type == "per_frame", NA, col)
            )
        ) %>%
        select(-row_type) %>%
        
        # renaming columns
        common_renames() %>%
        rename_with(
            function(col) {
                col %>%
                    str_replace("t", " t") %>%
                    str_to_upper()
            },
            f1t1:f10t3
        )
    
    bowler_scoresheets_pre_dt %>%
        datatable(
            rownames = FALSE,
            
            # removing striped rows
            class = list(stripe = FALSE),
            
            options = list(
                # preventing table from being ordered by a column
                ordering = FALSE,
                
                # preventing date column from going to multiple lines
                columnDefs = list(
                    list(
                        targets = "Date",
                        className = "dt-nowrap"
                    )
                )
            ) %>%
                append(dt_datable_options),
            
            # adding spanners above the col names, to group like cols together
            container =
                withTags(
                    table(
                        
                        # removing striped rows
                        class = "table table-striped",
                        
                        thead(
                            tr(
                                # date through lane_num
                                th(
                                    colspan = 7,
                                    "Game Information",
                                    style = border_right_solid_all_css
                                ),
                                
                                # frames 1-9
                                lapply(1:9, function(frame_num) {
                                    th(
                                        colspan = 2,
                                        str_c("Frame", frame_num, sep = " "),
                                        style =
                                            str_c(
                                                border_right_dotted_all_css,
                                                "white-space: nowrap;",
                                                sep = " "
                                            )
                                    )
                                }),
                                
                                # frame 10
                                th(
                                    colspan = 3,
                                    "Frame 10",
                                    style =
                                        str_c(
                                            border_right_solid_all_css,
                                            "white-space: nowrap;",
                                            sep = " "
                                        )
                                ),
                                
                                # final score
                                th(colspan = 1)
                            ),
                            
                            # adding colnames back in
                            tr(
                                bowler_scoresheets_pre_dt %>%
                                    colnames() %>%
                                    
                                    # making sure col names at the end of spanners
                                    # have right border
                                    # cant use `map()`
                                    lapply(function(colname) {
                                        if (colname %in% c("Lane #", "F10 T3")) {
                                            colname %>%
                                                str_remove("^F10 T") %>%
                                                th(style = border_right_solid_all_css)
                                        } else if (colname %in% str_c("F", 1:10, " T1")) {
                                            th("1")
                                        } else if (colname == "F10 T2") {
                                            th("2")
                                        } else if (colname %in% str_c("F", 1:9, " T2")) {
                                            th("2", style = border_right_dotted_all_css)
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
            columns = c("Lane #", "F10 T3"),
            `border-right` = border_right_solid_css,
            `border-right-color` = border_right_color_css
        ) %>%
        
        # adding right dotted border across entire cols after each frame
        formatStyle(
            columns = str_c("F", 1:9, " T2"),
            `border-right` = border_right_dotted_css,
            `border-right-color` = border_right_color_css
        ) %>%
        
        # every even game is gray
        formatStyle(
            columns = colnames(bowler_scoresheets_pre_dt),
            target = "row",
            backgroundColor = 
                styleRow(
                    keep(
                        1:nrow(bowler_scoresheets_pre_dt),
                        function(row_num) ceiling(row_num / 2) %% 2 == 0
                    ),
                    "lightgray"
                )
        ) %>%
        
        # every odd game is white
        formatStyle(
            columns = colnames(bowler_scoresheets_pre_dt),
            target = "row",
            backgroundColor = 
                styleRow(
                    keep(
                        1:nrow(bowler_scoresheets_pre_dt),
                        function(row_num) ceiling(row_num / 2) %% 2 == 1
                    ),
                    "white"
                )
        )
}


# Helper function for plots on Bowler Profiles tab which create the df passed to
# `ggplot()` commands
create_bowler_stats_df_for_plt <- function(bowler_profiles_input_in, bowler_profiles_stats_per_game_input_in = NULL) {
    
    req(length(bowler_profiles_stats_per_game_input_in$stats) >= 1)

    bowler_profiles_input_in %>%
        
        # calculating stats for each bowler
        league_stats_summary_table_fn(return_as_dt = FALSE, group_by_game = TRUE) %>%
        
        filter(bowler_id == bowler_profiles_input_in$bowler_id) %>%
        
        # bringing in game score
        left_join(
            scores_per_frame_df %>%
                select(session_id, game_num, bowler_id, game_score),
            by = join_by(session_id, game_num, bowler_id)
        ) %>%
        relocate(game_score, .after = bowler_id) %>%
        
        # replacing game_num with game into the season
        arrange(session_id, game_num) %>%
        select(-game_num, -bowler_id) %>%
        mutate(season_game_num = row_number(), .before = everything()) %>%
        
        when(
            !is.null(bowler_profiles_stats_per_game_input_in) ~ (.) %>%
                select(session_id, season_game_num, all_of(bowler_profiles_stats_per_game_input_in$stats)),
            
            TRUE ~ (.) %>%
                select(session_id, season_game_num, all_of(per_game_stats))
        ) %>%
        
        # `pivot_longer()` so that I can `facet_wrap()`
        pivot_longer(
            cols = -c(session_id, season_game_num),
            names_to = "stat_name",
            values_to = "stat_value"
        ) %>%
        
        # `stat_label` is needed for `geom_label()`
        mutate(
            stat_label =
                if_else(
                    str_detect(stat_name, "_rate$"),
                    percent(stat_value, accuracy = 1),
                    as.character(round(stat_value, digits = 1))
                )
        )
}


# Removing for now because I don't think these graphs look good or are meaningful
# bowler_profile_stats_distribution_plt_fn <- function(bowler_profiles_input_in) {
#     
#     bowler_profiles_input_in %>%
#         create_bowler_stats_df_for_plt() %>%
#         ggplot(aes(x = stat_value)) +
#         geom_histogram() +
#         facet_wrap(~stat_name, scales = "free")
# }


# Outputting ui for plot of selected stats for selected bowler to control flow
# Also setting height of plot
bowler_profile_stats_per_game_plt_wrapper_fn <- function(bowler_profiles_stats_per_game_input_in) {
    
    req(length(bowler_profiles_stats_per_game_input_in$stats) >= 1)
    
    column(
        width = 12,
        plotOutput(
            "bowler_profile_stats_per_game_plt",
            height = 600 * length(bowler_profiles_stats_per_game_input_in$stats)
        )
    )
}

# Plotting selected stats for selected bowler
bowler_profile_stats_per_game_plt_fn <- function(bowler_profiles_input_in, bowler_profiles_stats_per_game_input_in) {
    
    # bowler name needed for plot
    bowler <- bowlers_df %>%
        filter(bowler_id == bowler_profiles_input_in$bowler_id) %>%
        .$bowler
    
    bowler_profiles_input_in %>%
        
        # making each row correspond to a single game-stat instance per bowler
        create_bowler_stats_df_for_plt(bowler_profiles_stats_per_game_input_in) %>%
        
        # bringing in bowling_alley and competition
        left_join(
            sessions_df %>%
                select(session_id, comp, bowling_alley),
            by = join_by(session_id)
            
        ) %>%
        
        mutate(
            
            # adding linetype_style style, which is linetype style for border
            # dotted border vs solid border
            linetype_style =
                case_when(
                    comp == "Friendly" ~ "dotted",
                    comp == "Regular Season" ~ "solid",
                    comp == "Playoffs" ~ "longdash"
                ),
            
            # ensures facet plots are ordered as selected
            stat_name =
                factor(
                    stat_name,
                    levels = unname(common_renames_map),
                    ordered = TRUE
                )
        ) %>%
        
        ggplot(aes(x = season_game_num, y = stat_value)) +
        geom_point() +
        geom_line() +
        geom_label(
            aes(
                label = stat_label,
                color = bowling_alley,
                linetype = linetype_style
            ),
            family = plot_font_name,
            size = 7,
            linewidth = 1
        ) +
        
        # specifying linetype style and legend values for competition
        scale_linetype_manual(
            values = c("dotted" = "dotted", "solid" = "solid", "longdash" = "longdash"),
            labels = c("dotted" = "Friendly", "solid" = "Regular Season", "longdash" = "Playoffs")
        ) +
        
        facet_wrap(~stat_name, ncol = 1, scales = "free_y", labeller = per_stat_labeller) +
        
        # formatting y-axis labels on graphs representing rate stats as percentage
        {
            if (length(bowler_profiles_stats_per_game_input_in$stats) >= 2) {
                facetted_pos_scales(
                    y = list(
                        str_detect(stat_name, "_rate$") ~ scale_y_continuous(labels = percent),
                        TRUE ~ scale_y_continuous(labels = identity)
                    )
                ) 
            }
        } +
        
        scale_x_continuous(breaks = integer_breaks) +
        
        # making the label of the legend be a point, as opposed to default letter "a"
        guides(
            color = guide_legend(override.aes = list(label = "\u2022")),
            linetype = guide_legend(override.aes = list(label = "\u2022")),
        ) +
        
        labs(
            title =
                str_c(
                    bowler,
                    "'",
                    if_else(str_sub(bowler, start = -1) == "s", "", "s"),
                    " Selected Stats per Game"
                ),
            subtitle = generate_comps_subtitle(bowler_profiles_input_in$comps),
            x = "Season Game #",
            y = "Stat Value",
            color = "Bowling Alley",
            linetype = "Competition"
        ) +
        
        # moving legend to right, and increasing spacing to prevent overlapping
        theme(
            legend.position = "right",
            legend.direction = "vertical",
            legend.key.spacing.y = unit(0.35, "cm")
        )
}
