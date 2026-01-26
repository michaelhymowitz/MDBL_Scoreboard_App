#' Mike Dale Bowling Lead Scoreboard App


source("imports_options_constants.R")
source("mdbl_scoreboard_functions.R")
source("download_mdbl_scoreboard_gsheets.R")
source("global_data_structrues.R")
source("league_overview_tab.R")

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
            
            # TODO: align icons
            sidebarMenu(
                id = "tabs",
                menuItem(
                    text = "League Overview",
                    tabName = "league_overview_tab",
                    icon = icon("people-group")
                )
            )
        ),
        dashboardBody(
            
            # useShinyalert(),
            
            tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
            
            tabItems(
                league_overview_tab_ui # league_overview_tab.R
            )
        )
    )

server <- function(input, output, session) {
    
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
    
    # Handling of the info buttons for the inputs
    info_popups(input, output, session)

    
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
    
    # Plotting leaguewide league scores
    output$league_stats_leaguewide_scores_plt <- renderPlot({
        generate_league_scores_plots(league_stats_input, per_bowler = FALSE)
    })
    
    # Plotting per bowler league scores
    output$league_stats_per_bowler_scores_plt <- renderPlot({
        generate_league_scores_plots(league_stats_input, per_bowler = TRUE)
    })
    
    #### League Overview tab end ###############################################
}

shinyApp(ui, server)