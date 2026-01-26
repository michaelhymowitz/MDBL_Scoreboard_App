#' This script reads in the scoreboard data from the "Mike Dale Bowling League Scoreboard"
#' google sheets file

# Filename for downloaded data
mdbl_scoreboard_raw_excel_filename <-
    str_c(
        "mdbl_scoreboard_raw_excel_",
        now() %>%
            as.character() %>%
            str_replace_all(" |:|-|\\.", "_"),
        ".xlsx"
    )

# Downloaded google sheet of data
drive_download(
    MDBL_SCOREBOARD_GSHEETS_LINK,
    path = mdbl_scoreboard_raw_excel_filename,
    overwrite = TRUE
)

