#' Mike Dale Bowling Lead Scoreboard App

source("imports_options_constants.R")
source("mdbl_scoreboard_functions.R")
source("download_mdbl_scoreboard_gsheets.R")
source("global_data_structrues.R")
source("league_overview_tab.R")
source("bowler_profile_tabs.R")

ui <-
    dashboardPage(
        dashboardHeader(
            title = "MDBL Scoreboard"
        ),
        dashboardSidebar(
            
            # The photo is 4161 x 2970 px, and the division formats the photo nicely in the sidebar
            img(
                src = "mdbl_logo.png",
                height = 2970 / 18.0913,
                width = 4161 / 18.0913,
                style = "background-color: #FBE6A3;"
            ),
            
            sidebarMenu(
                id = "tabs",
                menuItem(
                    text = "League Overview",
                    tabName = "league_overview_tab",
                    icon = icon("people-group")
                ),
                menuItem(
                    # HTML spacing is to align tab names
                    text = HTML("&ensp;&ensp;&thinsp;Bowler Profiles"),
                    tabName = "bowler_profiles_tab",
                    icon = icon("person")
                )
            )
        ),
        dashboardBody(
            
            # useShinyalert(),
            
            tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
            
            tabItems(
                league_overview_tab_ui, # league_overview_tab.R
                bowler_profiles_tab_ui
            )
        )
    )

server <- function(input, output, session) {
    
    # Handling of the info buttons for the inputs
    info_popups(input, output, session)
    
    #### League Overview tab begin #############################################

    # Reactive variable to control workflow of league_overview_tab "League Stats" box
    league_stats_input <-
        reactiveValues(
            comps = vector("character", 0L)
        )
    
    # Updating league_stats_input after league_stats_input_button is selected
    observeEvent(input$league_stats_input_button, {
        league_stats_input$comps <- input$league_stats_comps_input
    })
    
    # Outputting league standings as a DT::datatable object
    output$league_standings_table <- renderDT({
        league_standings_table_fn()
    })
    
    # Outputting UI wrapper for "League Stats" box content
    output$league_stats_ui <- renderUI({
        league_stats_ui_fn(league_stats_input)
    })
    
    # Outputting league summary stats as a DT::datatable object
    output$league_stats_summary_table <- renderDT({
        league_stats_summary_table_fn(league_stats_input)
    })
    
    # Outputting league avg game as a DT::datatable object
    output$league_stats_avg_game_table <- renderDT({
        league_stats_avg_game_table_fn(league_stats_input)
    })
    
    # Outputting avg score per frame as a DT::datatable object
    output$league_stats_avg_frames_table <- renderDT({
        league_stats_avg_frames_table_fn(league_stats_input)
    })
    
    # Plotting leaguewide league scores
    output$league_stats_leaguewide_scores_plt <- renderPlot({
        generate_league_scores_plots(league_stats_input, per_bowler = FALSE)
    })
    
    # Plotting per bowler league scores
    output$league_stats_per_bowler_scores_plt <- renderPlot({
        generate_league_scores_plots(league_stats_input, per_bowler = TRUE)
    })
    
    #### League Overview tab end ###############################################
    
    
    #### Bowler Profiles tab begin #############################################
    
    # Reactive variable to control workflow of bowler_profiles tab
    bowler_profiles_input <-
        reactiveValues(
            bowler_id = vector("character", 0L),
            comps = vector("character", 0L)
        )
    
    # Updating bowler_profiles_input after bowler_profiles_input_button is selected
    observeEvent(input$bowler_profiles_input_button, {
        bowler_profiles_input$bowler_id <- input$bowler_profiles_bowler_id_input
        bowler_profiles_input$comps <- input$bowler_profiles_comps_input
    })
    
    # Reactive variable to control workflow of bowler_profiles_stats_per_game plot
    bowler_profiles_stats_per_game_input <-
        reactiveValues(
            stats = vector("character", 0L)
        )
    
    # Updating bowler_profiles_input after bowler_profiles_input_button is selected
    observeEvent(input$bowler_profiles_stats_per_game_input_button, {
        bowler_profiles_stats_per_game_input$stats <- input$bowler_profiles_stats_per_game_input
    })
    
    # Refreshing bowler_profiles_input after a new bowler/comp is selected
    observeEvent(input$bowler_profiles_input_button, {
        bowler_profiles_stats_per_game_input$stats <- vector("character", 0L)
    })
    
    # Outputting UI wrapper for selected bowler's box content
    output$bowler_profiles_output_ui <- renderUI({
        bowler_profiles_output_ui_fn(bowler_profiles_input)
    })
    
    # Outputting selected bowler's stats as a DT::datatable object
    output$bowler_profiles_summary_table <- renderDT({
        bowler_profiles_summary_table_fn(bowler_profiles_input)
    })
    
    # Outputting selected bowler's avg game as a DT::datatable object
    output$bowler_profiles_avg_game_table <- renderDT({
        bowler_profiles_avg_game_table_fn(bowler_profiles_input)
    })
    
    # Outputting selected bowler's scoresheets as a DT::datatable object
    output$bowler_profile_scoresheets_table <- renderDT({
        bowler_profile_scoresheets_table_fn(bowler_profiles_input)
    })
    
    # Removing for now because I don't think these graphs look good or are meaningful
    # # Plotting stats distribution for selected bowler
    # output$bowler_profile_stats_distribution_plt <- renderPlot({
    #     bowler_profile_stats_distribution_plt_fn(bowler_profiles_input)
    # }) 
    
    # Outputting ui for plot of selected stats for selected bowler to control flow
    # Also setting height of plot
    output$bowler_profile_stats_per_game_plt_wrapper <- renderUI({
        bowler_profile_stats_per_game_plt_wrapper_fn(bowler_profiles_stats_per_game_input)
    }) 
    
    # Plotting selected stats for selected bowler
    output$bowler_profile_stats_per_game_plt <- renderPlot({
        bowler_profile_stats_per_game_plt_fn(bowler_profiles_input, bowler_profiles_stats_per_game_input)
    }, res = 72) 
    
    #### Bowler Profiles tab end ###############################################
}

shinyApp(ui, server)