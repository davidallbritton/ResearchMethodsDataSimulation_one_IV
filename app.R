#
# Research Methods Data Simulation
#
# A teaching app: students choose parameters and generate fake data for
# class projects. This version generates data for a scatterplot/correlation.
#
# Framing: students set the SLOPE and the amount of NOISE. The correlation
# is a *result* they observe, not something they dial in directly.
#

library(shiny)

# Label for the row-number column, on screen and in the downloaded CSV.
# "Case" is the more general term (works for rats, schools, trials);
# "Participant" is what APA style expects for human research.
ID_LABEL <- "Participant"

ui <- fluidPage(

    withMathJax(),

    # Tighten the vertical rhythm of the parameter inputs, and set off the
    # population-parameter box from the sampling controls below it.
    tags$head(tags$style(HTML("
        .param-box { border: 1px solid #b8c4d0; border-radius: 6px;
                     background: #f4f7fa; padding: 10px 12px 4px 12px; }
        .param-box .form-group { margin-bottom: 6px; }
        .param-box label { margin-bottom: 1px; font-weight: normal; }
        .param-box .box-title { font-weight: bold; display: block;
                                margin-bottom: 2px; }
        .param-box .var-head { font-weight: bold; display: block;
                               margin: 8px 0 2px 0; }
        .param-box .box-note { color: #5a6570; font-size: 90%;
                               margin-bottom: 8px; }
        .param-box .help-block { margin: 8px 0 4px 0; }
        .sample-controls { margin-top: 12px; }
        .sample-controls .form-group { margin-bottom: 0; }

        /* Tables keep their natural width; the plot yields space instead. */
        .panel-row { display: flex; flex-wrap: wrap; gap: 20px;
                     align-items: flex-start; }
        .stats-col { flex: 0 0 auto; }
        .plot-col  { flex: 1 1 340px; min-width: 300px; }
        /* No row of the stats table may ever wrap. */
        .stats-col table td, .stats-col table th { white-space: nowrap; }
        .stats-col h4 { margin-top: 14px; }
        /* Sample Data heading and its small download button on one line. */
        .data-head { display: flex; align-items: baseline; gap: 10px; }
        .data-head h4 { margin-bottom: 6px; }
    "))),

    titlePanel("Simulate Data: Scatterplot / Correlation"),

    sidebarLayout(

        sidebarPanel(
            width = 4,

            div(
                class = "param-box",

                span(class = "box-title", "Population parameters"),
                div(class = "box-note",
                    "The true values in the whole population. Your sample is
                     drawn from a population that looks like this."),

                span(class = "var-head", "Predictor / IV (X)"),
                numericInput("mean_x", "Mean of X:", value = 0),
                numericInput("sd_x",   "SD of X:",   value = 1, min = 0),

                span(class = "var-head", "Outcome / DV (Y)"),
                numericInput("mean_y", "Mean of Y:", value = 0),
                numericInput("slope",  "Slope: how much Y rises per 1-unit rise in X:",
                             value = 0.5, step = 0.1),
                numericInput("sd_e",   "Noise: SD of the random error added to Y:",
                             value = 1, min = 0, step = 0.1),

                uiOutput("r_preview")
            ),

            # Sample size and the button sit outside the box: how many cases
            # you draw is not a property of the population.
            div(
                class = "sample-controls",
                fluidRow(
                    column(6, numericInput("n", "Number of cases (N):",
                                           value = 30, min = 3, step = 1)),
                    column(6, div(style = "margin-top: 25px;",
                                  actionButton("generate", "Generate Data",
                                               class = "btn-primary",
                                               width = "100%")))
                )
            ),

            tags$hr(),
            tags$strong("The model"),
            uiOutput("sd_note"),
            uiOutput("equations"),

            tags$hr(),
            tags$strong("R code"),
            verbatimTextOutput("code")
        ),

        mainPanel(
            width = 8,
            # Flexbox instead of a fixed 4/8 grid: the tables take exactly the
            # width their rows need, and the scatterplot absorbs whatever is
            # left over. Narrow the window and the plot gives up width first.
            div(
                class = "panel-row",
                div(
                    class = "stats-col",
                    tags$h4("Descriptive Statistics"),
                    tableOutput("sample_stats"),
                    div(
                        class = "data-head",
                        tags$h4("Sample Data"),
                        downloadButton("download_csv", "CSV",
                                       class = "btn-xs")
                    ),
                    div(
                        style = "max-height: 420px; overflow-y: auto;",
                        tableOutput("data_table")
                    )
                ),
                div(
                    class = "plot-col",
                    tags$h4("Scatterplot"),
                    plotOutput("scatter", height = "500px")
                )
            )
        )
    )
)

server <- function(input, output, session) {

    # One number format everywhere on the page -- 2 decimals -- so the same
    # quantity looks the same in the equations, the R code, the summary
    # table, and the plot title.
    fmt <- function(x) sprintf("%.2f", x)

    # ...except in the R code block, which must reproduce what the app did.
    # as.character() gives the shortest exact-enough representation, so a
    # typed 1.005 stays 1.005 rather than rounding to 1.00.
    fmt_code <- function(x) as.character(x)

    # Everything implied by a set of parameter values.
    derive <- function(mean_x, sd_x, mean_y, slope, sd_e) {
        sd_x <- abs(sd_x); sd_e <- abs(sd_e)
        sd_y <- sqrt(slope^2 * sd_x^2 + sd_e^2)
        list(
            mean_x = mean_x, sd_x = sd_x, mean_y = mean_y,
            slope = slope, sd_e = sd_e, sd_y = sd_y,
            # intercept that puts the line through (mean_x, mean_y)
            b0 = mean_y - slope * mean_x,
            r  = if (sd_y > 0) slope * sd_x / sd_y else 0
        )
    }

    # Live, un-gated view of the inputs, for the "r so far" preview.
    live <- reactive({
        req(input$sd_x, input$slope, input$sd_e)
        derive(input$mean_x, input$sd_x, input$mean_y, input$slope, input$sd_e)
    })

    # Snapshot of the inputs, taken only when "Generate Data" is clicked.
    params <- eventReactive(input$generate, {
        p <- derive(input$mean_x, input$sd_x, input$mean_y, input$slope, input$sd_e)
        p$n <- max(3, round(input$n))
        p
    }, ignoreNULL = FALSE)

    sim_data <- reactive({
        p <- params()
        X <- rnorm(p$n, mean = p$mean_x, sd = p$sd_x)
        Y <- p$b0 + p$slope * X + rnorm(p$n, mean = 0, sd = p$sd_e)
        data.frame(X = round(X, 2), Y = round(Y, 2))
    })

    # Compact readout inside the parameter box: keeps the Generate button
    # near the top of the panel. The fuller explanation lives below it.
    output$r_preview <- renderUI({
        p <- live()
        helpText(HTML(sprintf(
            "These settings give: total SD of Y = <b>%s</b>,
             correlation &rho; = <b>%s</b>",
            fmt(p$sd_y), fmt(p$r)
        )))
    })

    output$sd_note <- renderUI({
        p <- live()
        helpText(HTML(sprintf(
            "Y ends up with a <b>total</b> SD of <b>%s</b> &mdash; bigger than the
             noise you typed (%s), because Y varies for <i>two</i> reasons: people
             differ on X (which moves them along the line), and each person also
             has their own random error.
             More noise &rarr; weaker r. Steeper slope &rarr; stronger r.",
            fmt(p$sd_y), fmt(p$sd_e)
        )))
    })

    output$equations <- renderUI({
        p <- params()
        withMathJax(
            helpText("Each X score is the mean of X plus random error:"),
            helpText(sprintf(
                "$$X_i = \\bar{X} + e_i = %s + e_i, \\quad e_i \\sim N(0, %s)$$",
                fmt(p$mean_x), fmt(p$sd_x)
            )),
            helpText("Each Y score starts at the mean of Y, is adjusted up or down
                      by how far that person's X is from average, then gets its own
                      random error:"),
            helpText(sprintf(
                "$$Y_i = \\bar{Y} + b(X_i - \\bar{X}) + e_i
                       = %s + %s(X_i - %s) + e_i$$",
                fmt(p$mean_y), fmt(p$slope), fmt(p$mean_x)
            )),
            helpText(sprintf("$$e_i \\sim N(0, %s)$$", fmt(p$sd_e))),
            helpText("Multiplying out gives the usual regression equation:"),
            helpText(sprintf(
                "$$\\hat{Y}_i = b_0 + b_1 X_i = %s + %s X_i$$",
                fmt(p$b0), fmt(p$slope)
            ))
        )
    })

    output$code <- renderText({
        p <- params()
        paste0(
            "n <- ", p$n, "\n",
            "X <- rnorm(n, mean = ", fmt_code(p$mean_x), ", sd = ",
                       fmt_code(p$sd_x), ")\n",
            "\n",
            "# start at the mean of Y, adjust for X, add noise\n",
            "Y <- ", fmt_code(p$mean_y), " + ", fmt_code(p$slope),
            " * (X - ", fmt_code(p$mean_x), ")",
            " + rnorm(n, mean = 0, sd = ", fmt_code(p$sd_e), ")\n",
            "\n",
            "plot(X, Y)\n",
            "abline(lm(Y ~ X))\n",
            "cor(X, Y)"
        )
    })

    # The data as shown on screen and as downloaded: numbered rows.
    labelled_data <- reactive({
        d <- sim_data()
        out <- cbind(seq_len(nrow(d)), d)
        names(out)[1] <- ID_LABEL
        out
    })

    output$data_table <- renderTable({
        labelled_data()
    }, digits = 2, striped = TRUE)

    output$download_csv <- downloadHandler(
        filename = function() {
            paste0("simulated_data_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
        },
        content = function(file) {
            write.csv(labelled_data(), file, row.names = FALSE)
        }
    )

    output$sample_stats <- renderTable({
        d <- sim_data(); p <- params()
        fit <- lm(Y ~ X, data = d)

        # Greek letter for the population parameter, Roman for the sample
        # statistic, each shown next to its own number.
        greek <- c("μ", "σ", "μ", "σ", "σ", "β", "ρ")
        roman <- c("M", "s", "M", "s", "s", "b", "r")
        pop   <- c(p$mean_x, p$sd_x, p$mean_y, p$sd_y, p$sd_e, p$slope, p$r)
        samp  <- c(mean(d$X), sd(d$X), mean(d$Y), sd(d$Y),
                   summary(fit)$sigma, coef(fit)[2], cor(d$X, d$Y))

        data.frame(
            " "          = c("Mean of X", "SD of X", "Mean of Y",
                             "SD of Y (total spread)",
                             "SD of the error (noise)",
                             "Slope", "Correlation"),
            "Population" = paste(greek, "=", fmt(pop)),
            "This sample" = paste(roman, "=", fmt(samp)),
            check.names = FALSE
        )
    }, striped = TRUE, colnames = TRUE, rownames = FALSE, align = "lrr")

    # Axis limits from the MODEL, not the sample, so the plot frame and the
    # true line stay put when students regenerate with the same parameters.
    #
    # X ~ N(mean_x, sd_x) and, marginally, Y ~ N(mean_y, sd_y), so each axis
    # is mean +/- k SDs. Choose k so that every point lands inside the frame
    # about 90% of the time. A point has 2 coordinates, so that is 2n normal
    # deviates: per coordinate, two-tailed tail probability = 1 - .9^(1/(2n)).
    # Using the marginal SD of Y also keeps the true line in frame, since
    # |slope| * k * sd_x <= k * sd_y always.
    axis_limits <- reactive({
        p <- params()
        k <- max(3, qnorm(1 - (1 - 0.9^(1 / (2 * p$n))) / 2))
        pad <- function(center, sd) {
            half <- if (sd > 0) k * sd else max(1, abs(center) * 0.1)
            c(center - half, center + half)
        }
        list(x = pad(p$mean_x, p$sd_x),
             y = pad(p$mean_y, p$sd_y))
    })

    output$scatter <- renderPlot({
        d <- sim_data(); p <- params(); lim <- axis_limits()
        plot(d$X, d$Y,
             xlab = "X", ylab = "Y",
             xlim = lim$x, ylim = lim$y,
             pch = 19, col = "steelblue",
             main = paste("Sample r =", fmt(cor(d$X, d$Y))))
        # the line the data actually came from
        abline(a = p$b0, b = p$slope, col = "grey40", lwd = 2, lty = 2)
        # the line estimated from this particular sample
        fit <- lm(Y ~ X, data = d)
        abline(fit, col = "firebrick", lwd = 2)
        legend("topleft", bty = "n",
               legend = c("Population line (the true model)",
                          "Sample regression line"),
               col = c("grey40", "firebrick"), lwd = 2, lty = c(2, 1))
    })
}

shinyApp(ui = ui, server = server)
