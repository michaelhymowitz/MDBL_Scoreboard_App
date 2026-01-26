#' This script contains the packages, options(), and constants needed for mdbl_scorekeeper_app

library(shiny)
library(shinydashboard)
library(tidyverse)
library(googledrive)
library(googlesheets4)
library(readxl)
library(DT) # Package for dataframe visualizations
library(shinyWidgets)
library(ggthemes) # theme_fivethirtyeight()
library(cowplot) # combine plots and add logo
# library(shinycustomloader) # Shiny loaders
# library(pkgcond) # suppress_messages()
library(sysfonts)
library(showtext)
library(shinyalert) # Allows for the pop-up boxes when a box's info icon is pressed

# Adding Roboto font to shinyapps.io, so that it can be used in plots
plot_font_name <- "Roboto"
dir.create('~/.fonts')
file.copy(str_c("www/", plot_font_name, "-Regular.ttf"), "~/.fonts")
file.copy(str_c("www/", plot_font_name, "-Bold.ttf"), "~/.fonts")
file.copy(str_c("www/", plot_font_name, "-Italic.ttf"), "~/.fonts")
file.copy(str_c("www/", plot_font_name, "-BoldItalic.ttf"), "~/.fonts")
system('fc-cache -f ~/.fonts')

# Uncomment when running locally
# font_add(
#     family = plot_font_name,
#     regular = str_c("www/", plot_font_name, "-Regular.ttf"),
#     bold = str_c("www/", plot_font_name, "-Bold.ttf"),
#     italic = str_c("www/", plot_font_name, "-Italic.ttf"),
#     bolditalic = str_c("www/", plot_font_name, "-BoldItalic.ttf"),
# )
# showtext_auto()


 # https://debruine.github.io/shinyintro/data.html
# Getting access to my (Michael Hymowitz's) google drive, so that the
# scorekeeper data can be downloaded in
drive_auth(cache = ".secrets", email = "michael.hymowitz@gmail.com")
gs4_auth(cache = ".secrets", email = "michael.hymowitz@gmail.com")

# Link to "Mike Dale Bowling League Scoreboard" google sheets file
MDBL_SCOREBOARD_GSHEETS_LINK <- "https://docs.google.com/spreadsheets/d/1Yvsf1b1Z9yM1LpQ1pIAxdFiThs_vXni8Cm8__TLnfxo/edit?usp=sharing"

# mdbl_scoreboard_raw_excel_filename <- "www/mdbl_scoreboard_raw_excel_2026_01_25_13_14_02_5803.xlsx"

# Fractional cutoff of # games to qualify for "Average Score" standings points
AVG_SCORE_FRAC_QUALIFY_CUTOFF <- 0.5

# Format for plots including dates
DATE_PLOT_FORMAT <- "%m/%d/%y"

# # Plotting theme
theme_fivethirtyeight_mod <- function() {

    theme_fivethirtyeight() +
        theme(
            panel.background = element_rect(fill = "transparent"),
            plot.title = element_text(size = 42, hjust = 0.5, margin = margin(t = 15, b = 15, unit = "pt")),
            plot.subtitle = element_text(size = 28, hjust = 0.5, margin = margin(b = 15, unit = "pt")),
            axis.title.x = element_text(size = 24, margin = margin(b = 5, unit = "pt")),
            axis.title.y = element_text(size = 24, margin = margin(l = 5, unit = "pt")),
            axis.text.x = element_text(size = 18, margin = margin(t = 10, b = 15, unit = "pt")),
            axis.text.y = element_text(size = 18, margin = margin(r = 10, l = 15, unit = "pt")),
            # axis.line.x = element_line(colour = "black", linewidth = 0.5, linetype = "solid"),
            # axis.line.y = element_line(colour = "black", linewidth = 0.5, linetype = "solid"),
            strip.text = element_text(size = 18),
            text = element_text(size = 24, family = plot_font_name)
        )
}
theme_set(theme_fivethirtyeight_mod())
