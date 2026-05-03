#' This script contains general functions for the mdbl_scoreboard_app

#' Function: integer_breaks
#' ---
#' Function to generate integer breaks for ggplot
#' ---
#' Inputs:
#'     (limits): axis limit values
#' --
#' Output:
#'     integer vector of breaks
integer_breaks <- function(limits) {
    
    limits %>%
        
        # use the "pretty function to get a good set of numbers
        pretty(n = 5) %>%
        
        # filter out non-integer values
        .[. == as.integer(.)] %>%
        
        # ensuring output is integer vector
        as.integer()
}


# duplicating all data and changing ID
# this allows for calculating leaguewide stats in the same pipe
#' Function: duplicate_df_for_leaguewide
#' ---
#' Function to duplicate a dataframe, but change all the identifying data to
#' correspond to a unique bowler, representing the leaguewide group
#' ---
#' Inputs:
#'     (df): dataframe to duplicate
#' --
#' Output:
#'     `df` duplicated, with identifying data converted to represent leaguewide group
duplicate_df_for_leaguewide <- function(df) {
    
    bind_rows(
        df,
        df %>%
            
            # updating identifying cols
            mutate(
                lane_num = NA_integer_,
                bowler_id = "__leaguewide__",
                bowler = "Leaguewide"
            )
    ) 
}


#' Function: calculating_page_length
#' ---
#' Function to calculate the page length (number of rows) for DT::datatables
#' ---
#' Inputs:
#'     (comps): competitions included in data
#' --
#' Output:
#'     Number of rows needed in DT::datatable
calculating_page_length <- function(comps) {
    
    scores_per_frame_df %>%
        filter(comp %in% comps) %>%
        distinct(bowler_id) %>%
        nrow() %>%
        
        # to include leaguewide row
        `+`(1)
}


#' Function: adding_bowler_names
#' ---
#' Adding bowler names from `bowler_id`, and ensuring `bowler_id == "__leaguewide_"`
#' is dealt with appropriately
#' ---
#' Inputs:
#'     (df): dataframe to add bowler names
#' --
#' Output:
#'     `df` with bowler names added
adding_bowler_names <- function(df) {
    
    df %>%
        
        # bringing in bowler names
        left_join(
            bowlers_df %>%
                select(bowler_id, bowler),
            by = join_by(bowler_id)
        ) %>%
            
            # adding "Leaguewide" bowler name to appropriate row
            mutate(bowler = if_else(bowler_id == "__leaguewide__", "Leaguewide", bowler)) %>%
            
            # cleaning data
            relocate(bowler, .before = everything()) %>%
            arrange(bowler_id == "__leaguewide__", bowler) %>%
            select(-bowler_id) 
}


common_renames <- function(df) {
    
    df %>%
        rename(any_of(common_renames_map))
}


#' #' Function: add_mdbl_logo
#' #' ---
#' #' Adds projectB's logo to a corner of a plot
#' #' Note: call this function at the end of the creation of a plot
#' #' ---
#' #' Inputs:
#' #'     (ggplot_plt): `ggplot2` plot to dd logo on
#' #'     (corner): Corner of graph to put logo, either "topright", "topleft", "bottomleft",
#' #'               or "bottomright"
#' #'     (scale): From `cowplot::draw_image()`: Scales the image relative to the rectangle
#' #'              defined by x, y, width, height. A setting of scale = 1 indicates
#' #'              no scaling
#' #' --
#' #' Output:
#' #'     `ggplot_plt` with logo in corner
#' add_mdbl_logo <- function(ggplot_plt, corner = "bottomright", scale = 0.05)  {
#'     stopifnot(
#'         length(corner) == 1,
#'         corner %in% c("topleft", "topright", "bottomleft", "bottomright")
#'     )
#'     logo <- image_read("www/mdbl_logo.png")
#'     
#'     y_val <- if_else(str_detect(corner, "left"),   0.01, 0.99)
#'     vjust_val <- if_else(str_detect(corner, "left"),  0.01, 0.99)
#'     halign_val <- if_else(str_detect(corner, "left"), 0.01, 0.99)
#'     valign_val <- if_else(str_detect(corner, "bottom"), 0.01, 0.99)
#'     
#'     ggdraw() +
#'         draw_plot(ggplot_plt) +
#'         draw_image(logo, x = 1, y = y_val, hjust = 1,
#'                    vjust = vjust_val, halign = halign_val, valign = valign_val,
#'                    scale = scale)
#' }


#' Function: factor_bowler_id_by_bowler
#' ---
#' Factorizes `bowler_id` col based on alphabetical order of `bowler` col
#' Ensures faceted plots on bowler are in alphabetical order
#' ---
#' Inputs:
#'     (df): dataframe with both `bowler_id` and `bowler` cols
#' --
#' Output:
#'     df with `bowler_id` col factorized
factor_bowler_id_by_bowler <- function(df) {
    df %>%
        mutate(
            bowler_id =
                factor(
                    bowler_id,
                    levels = bowlers_df %>%
                        arrange(bowler) %>%
                        .$bowler_id,
                    ordered = TRUE
                )
        )
}


#' Function: order_and_clean_stat_names
#' ---
#' Orders `stat_name` col based on pre-set order and clean them
#' ---
#' Inputs:
#'     (df): dataframe ordered by `stat_name` col, then cleaned
#' --
#' Output:
#'     df ordered by `stat_name`
order_and_clean_stat_names <- function(df) {
    df %>%
        mutate(
            stat_name =
                factor(
                    stat_name,
                    levels =
                        c(
                            # Appearances
                            "num_sessions", "num_games",
                            
                            # Score Summary
                            "total_score", "avg_score", "min_score", "max_score",
                            
                            # Closed Frames
                            "strikes", "strike_rate", "spares", "spare_rate",
                            
                            # Non-Split vs Split Spares
                            "nonsplit_spares", "nonsplit_spare_rate",
                            "split_spares", "split_spare_rate",
                            
                            # Single- vs Multi-Pin Spares
                            "single_pin_spares", "single_pin_spare_rate",
                            "multi_pin_spares", "multi_pin_spare_rate",
                            
                            # 1st Throw
                            "avg_first_throw", "first_throw_gutter_rate"
                        ),
                    ordered = TRUE
                )
        ) %>%
        arrange(stat_name) %>%
        mutate(stat_name = as.character(stat_name)) %>%
        left_join(
            common_renames_map %>%
                enframe(name = "clean_stat_name", value = "stat_name"),
            by = join_by(stat_name)
        ) %>%
        relocate(clean_stat_name, .before = everything()) %>%
        select(-stat_name)
}


#' Function: generate_comps_subtitle
#' ---
#' Generates subtitles of plots which are simply the competitions listed out
#' If 1 comp: "comp1",
#' If 2 comps: "comp1 and comp2"
#' If 3+ comps: "comp1, comp2, and comp3"
#' ---
#' Inputs:
#'     (comps): competitions included in data
#' --
#' Output:
#'     `comps` concatenated, properly separated by commas and "and"
generate_comps_subtitle <- function(comps) {
    str_flatten_comma(comps, last = if_else(length(comps) == 2, " and ", ", and "))
}


# ' Function: info_popups
#' ---
#' Implementations of the pop-up windows that appear when the info icon in a 
#' title of a box is selected
#' ---
#' Inputs:
#'     (input): The shiny server() input object
#'     (output): The shiny server() output object
#'     (session): The shiny server() session object
#' --
#' Output:
#'     The needed observeEvent() calls that will be triggered when an info icon
#'     is clicked
info_popups <- function(input, output, session) {
    
    # League Standings Info Popup
    observeEvent(input$league_standings_popup, {
        shinyalert(
            '<h2>League Standings Explainer</h2>',
            
            str_c(
                '<p style="text-align: left;">Exact formula for league standings are still subject to change.
                Currently, 4 components are involved in calculating league standings:</p><br>
                
                <ol type "1">
                <li style="text-align: left;">
                <strong>Total Score</strong><br>
                The top 5 bowlers by total score will receive in descending order:
                15, 10, 6, 3, and 1 point.
                </li>
                <br>
                <li style="text-align: left;">
                <strong>Average Score</strong><br>
                The top 5 bowlers by average score who bowl in at least half of the
                games in the regular season (denoted by whether the cell in the
                `# Games` column is highlighted green) will receive in descending order:
                15, 10, 6, 3, and 1 point. The current cutoff is',
                avg_score_games_qualify_cutoff, 'games.
                </li>
                <br>
                <li style="text-align: left;">
                <strong># Session Wins</strong><br>
                Each session, across all lanes, the bowler who scores the most points
                across all the games will receive an additional 3 points.
                </li>
                <br>
                <li style="text-align: left;">
                <strong># Games Wins</strong><br>
                Each game, across all lanes, the bowler who scores the most points
                will receive an additional 1 point.
                </li>
                </ol>
                
                <br><br>
                <strong><u>Playoffs</u></strong>
                <br><br>
                
                <p style="text-align: left;">The regular season will consist of
                12 sessions. The top 10 bowlers will qualify for the playoffs.
                The playoffs will take two weeks to complete.</p></br>
                
                <ol type "1">
                <li style="text-align: left;">
                <strong>Week 1: Semifinals</strong><br>
                In the semifinals, the 1st, 4th, 5th, 8th, and 10th seeds will bowl
                on one lane, and the 2nd, 3rd, 6th, 7th, and 9th seeds will bowl
                on a second lane (everyone else is obviously welcome to come and
                bowl as well). The top 2 bowlers by total pins on both lanes and
                the best 3rd-place bowler in the two lanes will qualify for the
                finals. Each bowler will bowl 3 games.
                </li>
                <br>
                <li style="text-align: left;">
                <strong>Week 2: Finals</strong><br>
                In the finals, the 5 bowlers who qualifed will bowl on one lane
                (again, everyone else is obviously welcome to come and bowl as
                well). The top bowler over the 3 games will be declared MDBL
                Champion. 2nd- and 3rd-place will also receive awards.
                </li>
                </ol>',
                sep = " "
            ),
            
            html = TRUE,
            type = "info",
            confirmButtonCol = "#00274C"
        )
    })
    
    # League Standings Info Popup
    observeEvent(input$league_stats_popup, {
        shinyalert(
            "<h2>Glossary</h2>",
            
            '<ul>
            <li style="text-align: left;">
            <strong>Strike %</strong><br>
            % of strike opportunities converted.
            </li>
            <br>
            <li style="text-align: left;">
            <strong>Spare %</strong><br>
            % of spare opportunities converted.
            </li>
            <br>
            <li style="text-align: left;">
            <strong>Non-Split Spare %</strong><br>
            % of spare opportunities converted when not facing a split.
            </li>
            <br>
            <li style="text-align: left;">
            <strong>Split Spare %</strong><br>
            % of spare opportunities converted when facing a split.
            </li>
            <br>
            <li style="text-align: left;">
            <strong>Single-Pin Spare %</strong><br>
            % of spare opportunities converted when facing just a single pin.
            </li>
            <br>
            <li style="text-align: left;">
            <strong>Multi-Pin Spare %</strong><br>
            % of spare opportunities converted when facing multiple pins.
            </li>
            <br>
            <li style="text-align: left;">
            <strong>Avg First Throw</strong><br>
            Average number of pins knocked down on first throw of a frame.
            </li>
            <br>
            <li style="text-align: left;">
            <strong>First Throw Gutter %</strong><br>
            % of first throws in a frame that don\'t knock down a single pin.
            </li>
            </ul>
            <br>
            <p style="text-align: left;">Note: stats from the 10<sup>th</sup>
            frame are accounted for appropriately and included.</p>',
            
            html = TRUE,
            type = "info",
            confirmButtonCol = "#00274C"
        )
    })
    
    # Selected Bowler's Info Popup
    observeEvent(input$selected_bowler_popup, {
        shinyalert(
            '<h2>Note on "Qualified" Ranks</h2>',
            
            '<p style="text-align: left;">"Qualified" ranks refers to stats subsetted
            the set of bowlers who have bowled in at least half of the games in
            the user-selected competitons in the "Competitions to Include"
            dropbox at the top of this sheet. There are separate ranks so that
            bowlers who have note bowled many games do not distort the metrics.</p><br>
            
            <p style="text-align: left;">Inspiration for qualified ranks comes
            from <a href ="https://www.mlb.com/glossary/standard-stats/rate-stats-qualifiers" target="_blank">MLB</a>,
            where a batter/pitcher must surpass a specific threshold of PA/IP to
            qualify for end-of-season awards, like the batting champion (highest
            batting average) or ERA champion (lowest ERA).</p><br>
            
            <p style="text-align: left;">Note that this only applies to stats which
            ends with "%", which denote rate stats. For all other stats,
            <code>Leaguewide Rank == Qualified Leaguewide Rank</code></p><br>',
            
            html = TRUE,
            type = "info",
            confirmButtonCol = "#00274C"
        )
    })
}



