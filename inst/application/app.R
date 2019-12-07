library(shiny)
library(dplyr)
library(rdfanalysis)
library(stargazer)

load("shiny.Rda")
data <- ests
mods <<- NULL

if (!is.null(design)) source_design(design)

est_models <- function(d, choice_df, start_input) {
    mods <- vector("list", nrow(choice_df))
    for (i in 1:nrow(choice_df)) {
        for (step in d) {
            vars <- unlist(get(step)()$choice_type)[names(unlist(get(step)()$choice_type)) %in% "name"]
            params <- choice_df[i, vars]
            if (match(c(step), d) == 1)
                input <- do.call(get(step), list(start_input, params))
            else input <- do.call(get(step), list(input, params))
        }
        mods[[i]] <- input$model
    }
    mods
}

ui <- fluidPage(
    titlePanel(title),
    hr(),
    p(HTML(abstract)),
    hr(),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            lapply(attr(data, "choices"), function(i)
                selectInput(
                    names(data)[i],
                    sprintf("Choose %s", names(data)[i]),
                    unique(data[, i]),
                    selected = {
                        if(! names(data)[i] %in% names(default_choices))
                            unique(data[,i])
                        else default_choices[[names(data)[i]]]
                    },
                    multiple = TRUE
                )
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tags$style(HTML("#regression table{margin: auto;}")),
            uiOutput("ui_display")
        )
    ),
    hr(),
    fluidRow(
        column(
            12, align="center",
            HTML(
                "Created with <a href=https://shiny.rstudio.com>R/shiny</a> by the",
                "<a href=https://joachim-gassen.github.io/rdfanalysis/index.html>",
                "rdfanalysis package</a>.<br>",
                "Joachim Gassen,",
                "<a href=https://www.wiwi.hu-berlin.de/de/professuren/bwl/rwuwp/staff/gassen>",
                "Humboldt-Universität zu Berlin</a>",
                "and <a href=https://www.accounting-for-transparency.de>",
                "TRR 266 'Accounting for Transparency'</a>, 2019."
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    mods <- reactiveVal(NULL)

    plot_df <- reactive({
        filter_choice <- function(data, choice, choices) {
            data[data[, choice] %in% choices, ]
        }
        plot_df <- data
        for (i in attr(plot_df, "choices")) {
            if (nrow(plot_df) > 0) {
                plot_df <- filter_choice(plot_df, names(data)[i],
                                         input[[names(data)[i]]])
            }
        }
        if (nrow(plot_df) <= regression_cutoff && nrow(plot_df) > 0 &&
            !is.null(design)) {
            mods(est_models(design, plot_df, start_input))
        } else mods(NULL)
        plot_df
    })

    output$ui_display <- renderUI({
        if(nrow(plot_df()) > 0) {
            if(!is.null(mods())) htmlOutput("regression")
            else plotOutput("spec_curve")
        } else p("Please select at least one choice")
    })

    output$regression <- renderPrint({
        if (!is.null(mods())) {
            tab <- capture.output(stargazer(mods(), type = "html"))
            row_intro <- '<tr><td style="text-align:left">'
            row_outro <- '</td></tr>'
            cell_break <- '</td><td>'
            start_tab <- tab[1:(length(tab) - 2)]
            for (choice in attr(data, "choices")) {
                name_choice <- names(data)[choice]
                choices_made <- plot_df()[, name_choice]
                new_line <- paste0(
                    row_intro,
                    name_choice,
                    cell_break,
                    paste0(choices_made, collapse = cell_break),
                    row_outro
                )
                start_tab <- c(start_tab, new_line)
            }
            tab <- c(start_tab, tab[(length(tab) - 1):length(tab)])
            htmltools::HTML(tab)
        }
    })

    output$spec_curve <- renderPlot({
        if(nrow(plot_df()) > 0) {
            do.call(
                plot_rdf_spec_curve,
                c(list(plot_df()), unlist(spec_curve_parms),
                  pt_size = min(max(50/nrow(plot_df()), 0.1), 3))
            )
        }
    })
}

# Run the application
shinyApp(ui = ui, server = server)
