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
        
        # use the 'pretty' function to get a good set of numbers
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
        rename(
            any_of(
                c(
                    "Bowler" = "bowler",
                    "# Sessions" = "num_sessions",
                    "# Games" = "num_games",
                    "Total Score" = "total_score",
                    "Avg Score" = "avg_score",
                    "Min Score" = "min_score",
                    "Max Score" = "max_score",
                    "# Session Wins" = "num_session_wins",
                    "# Game Wins" = "num_game_wins",
                    "Points" = "points",
                    "Strike %" = "strike_rate",
                    "Spare %" = "spare_rate",
                    "Non-Split Spare %" = "nonsplit_spare_rate",
                    "Split Spare %" = "split_spare_rate",
                    "Single-Pin Spare %" = "single_pin_spare_rate",
                    "Multi-Pin Spare %" = "multi_pin_spare_rate",
                    "Avg First Throw" = "avg_first_throw",
                    "First Throw Gutter %" = "first_throw_gutter_rate"
                )
            )
        )
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


#' Function: require_comps
#' ---
#' Controls flow, halts execution if no comps are selected
#' ---
#' Inputs:
#'     (comps): competitions included in data
require_comps <- function(comps) {
    req(length(comps) > 0)
    invisible(NULL)
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
            
            '<p style="text-align: left;">Exact formula for league standings are still subject to change.
            Currently, 4 components are involved in calculating league standings:</p><br>
            
            <ol type "1">
            <li style="text-align: left;">
            <strong>Total Score</strong><br>
            The top 5 players by total score will receive in descending order:
            15, 10, 6, 3, and 1 point.
            </li>
            <br>
            <li style="text-align: left;">
            <strong>Average Score</strong><br>
            The top 5 players by average score who bowl in at least half of the
            games in the regular season will receive in descending order:
            15, 10, 6, 3, and 1 point.
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
            
            <p style="text-align: left;">The regular season will consist of 12 sessions
            (tbd). The 10 bowlers (extreme tbd) will qualify for the playoffs
            (format also tbd)</p>',
            
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
}
    
    # observeEvent(input$discount_rate_header, {
    #     shinyalert("Discount Rate",
    #                "<p>The rate to discount a dollar in any future year when scaling it
    #            back to the previous year to adjust for the time value of money.</p>",
    #                html = TRUE, type = "info", confirmButtonCol = "#c2d3e1")
    # })
    # 
    # observeEvent(input$salary_growth_rate_header, {
    #     shinyalert("Salary Growth Rate",
    #                "<p>The assumed annual rate of growth of total MLB salary.</p>",
    #                html = TRUE, type = "info", confirmButtonCol = "#c2d3e1")
    # })
    # 
    # observeEvent(input$adverse_selection_header, {
    #     shinyalert("Adverse Selection",
    #                "<p>For additional context behind this table, see the \"Adverse Selection
    #            and Model Consistency\" document in the data room.</p>",
    #                html = TRUE, type = "info", confirmButtonCol = "#c2d3e1")
#     })
# }
