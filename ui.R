# ---------------------------------------------------------
# This is the ui file.
# Use it to call elements created in your server file into the app, and define where they are placed.
# Also use this file to define inputs.
#
# Every UI file should contain:
# - A title for the app
# - A call to a CSS file to define the styling
# - An accessibility statement
# - Contact information
#
# Other elements like charts, navigation bars etc. are completely up to you to decide what goes in.
# However, every element should meet accessibility requirements and user needs.
#
# This file uses a slider input, but other inputs are available like date selections, multiple choice dropdowns etc.
# Use the shiny cheatsheet to explore more options: https://shiny.rstudio.com/images/shiny-cheatsheet.pdf
#
# Likewise, this template uses the navbar layout.
# We have used this as it meets accessibility requirements, but you are free to use another layout if it does too.
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# ---------------------------------------------------------

#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# The documentation for this GOVUK components can be found at:
#
#    https://github.com/moj-analytical-services/shinyGovstyle
#


#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# The documentation for this GOVUK components can be found at:
#
#    https://github.com/moj-analytical-services/shinyGovstyle



ui <- function(input, output, session) {
  fluidPage(
    # use_tota11y(),
    title = tags$head(
      tags$link(
        rel = "shortcut icon",
        href = "dfefavicon.png"
      ),
      tags$title(site_title)
    ),
    tags$html(lang = "en"),
    shinyjs::useShinyjs(),
    dfeshiny::custom_disconnect_message(
      dashboard_title = site_title,
      links = site_primary,
      publication_name = ees_pub_name,
      publication_link = ees_publication,
    ),
    tags$head(includeHTML(("google-analytics.html"))),
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "dfe_shiny_gov_style.css"
      )
    ),
    # meta description for search engines
    meta() %>%
      meta_general(
        application_name = "Key stage 4 Transition Matrices",
        description = "Key stage 4 Transition Matrices",
        robots = "index,follow",
        generator = "R-Shiny",
        subject = "Key stage 4",
        rating = "General",
        referrer = "no-referrer"
      ),
    dfe_cookies_script(),
    cookies_banner_ui(name = site_title),
    shinyGovstyle::header(
      main_text = "",
      main_link = "https://www.gov.uk/government/organisations/department-for-education",
      secondary_text = "Key stage 4 Transition Matrices",
      logo = "images/DfE_logo_landscape.png",
      logo_width = 150,
      logo_height = 32
    ),
    shinyGovstyle::banner(
      "beta banner",
      "beta",
      "This Dashboard is in beta phase and we are still reviewing performance and reliability."
    ),
    shiny::navlistPanel(
      "",
      id = "navlistPanel",
      widths = c(2, 8),
      well = FALSE,
      homepage_panel(),
      dashboard_panel(),
      tabPanel(
        value = "support_panel",
        "Support and feedback",
        gov_main_layout(
          gov_row(
            column(
              12,
              dfeshiny::support_panel(
                team_email = "attainment.statistics@education.gov.uk",
                repo_name = "https://github.com/dfe-analytical-services/Transition-matrices-dashboard",
                publication_name = ees_pub_name,
                publication_slug = "key-stage-4-performance-revised"
              )
            )
          )
        )
      ),
      a11y_panel()
    ),
    footer(full = TRUE)
  )
}
