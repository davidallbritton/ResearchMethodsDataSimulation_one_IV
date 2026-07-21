#
# Research Methods Data Simulation -- t-test version
#
# Parallel to the correlation app: students set POPULATION parameters for two
# groups, draw a sample, and see the effect (Cohen's d) as a *result*. The
# IV is now a grouping variable; the DV is a continuous score.
#
# This is a standalone sketch so it can be compared with the correlation page
# before deciding how to combine them.
#

library(shiny)

# Label for the row-number column, on screen and in the downloaded CSV.
ID_LABEL <- "Participant"

# Group names (also the levels of the IV).
G1 <- "Group 1"
G2 <- "Group 2"

ui <- fluidPage(

    withMathJax(),

    # Same styling as the correlation page.
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
        .panel-row { display: flex; flex-wrap: wrap; gap: 20px;
                     align-items: flex-start; }
        .stats-col { flex: 0 0 auto; }
        .plot-col  { flex: 1 1 340px; min-width: 300px; }
        .stats-col table td, .stats-col table th { white-space: nowrap; }
        .stats-col h4 { margin-top: 14px; }
        .data-head { display: flex; align-items: baseline; gap: 10px; }
        .data-head h4 { margin-bottom: 6px; }
    "))),

    titlePanel("Simulate Data: Two-Group t-test"),

    sidebarLayout(

        sidebarPanel(
            width = 4,

            div(
                class = "param-box",

                span(class = "box-title", "Population parameters"),
                div(class = "box-note",
                    "The true values in each group in the whole population. Your
                     sample is drawn from populations that look like this."),

                span(class = "var-head", paste0(G1, "  (IV level 1)")),
                numericInput("mean1", "Mean of Group 1:", value = 100),
                numericInput("sd1",   "SD of Group 1:",   value = 15, min = 0),

                span(class = "var-head", paste0(G2, "  (IV level 2)")),
                numericInput("mean2", "Mean of Group 2:", value = 110),
                numericInput("sd2",   "SD of Group 2:",   value = 15, min = 0),

                uiOutput("d_preview")
            ),

            # Sample size per group and the button sit outside the box.
            div(
                class = "sample-controls",
                fluidRow(
                    column(6, numericInput("n", "Cases per group (n):",
                                           value = 30, min = 2, step = 1)),
                    column(6, div(style = "margin-top: 25px;",
                                  actionButton("generate", "Generate Data",
                                               class = "btn-primary",
                                               width = "100%")))
                )
            ),

            tags$hr(),
            tags$strong("The model"),
            uiOutput("equations"),

            tags$hr(),
            tags$strong("R code"),
            verbatimTextOutput("code")
        ),

        mainPanel(
            width = 8,
            div(
                class = "panel-row",
                div(
                    class = "stats-col",
                    tags$h4("Descriptive Statistics"),
                    tableOutput("sample_stats"),
                    div(
                        class = "data-head",
                        tags$h4("Sample Data"),
                        downloadButton("download_csv", "CSV", class = "btn-xs")
                    ),
                    div(
                        style = "max-height: 420px; overflow-y: auto;",
                        tableOutput("data_table")
                    )
                ),
                div(
                    class = "plot-col",
                    tags$h4("Group Comparison"),
                    plotOutput("dotplot", height = "500px")
                )
            )
        )
    )
)

server <- function(input, output, session) {

    fmt <- function(x) sprintf("%.2f", x)
    fmt_code <- function(x) as.character(x)

    # Everything implied by a set of parameter values.
    derive <- function(mean1, sd1, mean2, sd2) {
        sd1 <- abs(sd1); sd2 <- abs(sd2)
        sd_pooled <- sqrt((sd1^2 + sd2^2) / 2)   # equal-n pooled SD
        list(
            mean1 = mean1, sd1 = sd1, mean2 = mean2, sd2 = sd2,
            diff = mean2 - mean1, sd_pooled = sd_pooled,
            d = if (sd_pooled > 0) (mean2 - mean1) / sd_pooled else 0
        )
    }

    live <- reactive({
        req(input$sd1, input$sd2)
        derive(input$mean1, input$sd1, input$mean2, input$sd2)
    })

    params <- eventReactive(input$generate, {
        p <- derive(input$mean1, input$sd1, input$mean2, input$sd2)
        p$n <- max(2, round(input$n))
        p
    }, ignoreNULL = FALSE)

    sim_data <- reactive({
        p <- params()
        y1 <- rnorm(p$n, mean = p$mean1, sd = p$sd1)
        y2 <- rnorm(p$n, mean = p$mean2, sd = p$sd2)
        data.frame(
            Group = factor(rep(c(G1, G2), each = p$n), levels = c(G1, G2)),
            Score = round(c(y1, y2), 2)
        )
    })

    output$d_preview <- renderUI({
        p <- live()
        helpText(HTML(sprintf(
            "These settings give: mean difference = <b>%s</b>,
             effect size Cohen's <i>d</i> = <b>%s</b>",
            fmt(p$diff), fmt(p$d)
        )))
    })

    output$equations <- renderUI({
        p <- params()
        withMathJax(
            helpText("Each score is its group's mean plus random error:"),
            helpText(sprintf(
                "$$Y_{i,1} = \\mu_1 + e_i = %s + e_i, \\quad e_i \\sim N(0, %s)$$",
                fmt(p$mean1), fmt(p$sd1)
            )),
            helpText(sprintf(
                "$$Y_{i,2} = \\mu_2 + e_i = %s + e_i, \\quad e_i \\sim N(0, %s)$$",
                fmt(p$mean2), fmt(p$sd2)
            )),
            helpText("The effect size is the mean difference in SD units:"),
            helpText(sprintf(
                "$$d = \\frac{\\mu_2 - \\mu_1}{s_{pooled}}
                     = \\frac{%s - %s}{%s} = %s$$",
                fmt(p$mean2), fmt(p$mean1), fmt(p$sd_pooled), fmt(p$d)
            ))
        )
    })

    output$code <- renderText({
        p <- params()
        paste0(
            "n <- ", p$n, "\n",
            "group1 <- rnorm(n, mean = ", fmt_code(p$mean1),
                ", sd = ", fmt_code(p$sd1), ")\n",
            "group2 <- rnorm(n, mean = ", fmt_code(p$mean2),
                ", sd = ", fmt_code(p$sd2), ")\n",
            "\n",
            "t.test(group1, group2)\n",
            "boxplot(group1, group2)"
        )
    })

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
            paste0("simulated_ttest_data_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
        },
        content = function(file) {
            write.csv(labelled_data(), file, row.names = FALSE)
        }
    )

    output$sample_stats <- renderTable({
        d <- sim_data(); p <- params()
        s1 <- d$Score[d$Group == G1]; s2 <- d$Score[d$Group == G2]
        tt <- t.test(s2, s1)
        samp_sd_pooled <- sqrt((var(s1) + var(s2)) / 2)
        samp_d <- (mean(s2) - mean(s1)) / samp_sd_pooled

        greek <- c("μ₁", "σ₁", "μ₂", "σ₂", "μ₂−μ₁", "δ")
        roman <- c("M₁", "s₁", "M₂", "s₂", "M₂−M₁", "d")
        pop   <- c(p$mean1, p$sd1, p$mean2, p$sd2, p$diff, p$d)
        samp  <- c(mean(s1), sd(s1), mean(s2), sd(s2),
                   mean(s2) - mean(s1), samp_d)

        data.frame(
            " "           = c("Mean of Group 1", "SD of Group 1",
                              "Mean of Group 2", "SD of Group 2",
                              "Mean difference", "Cohen's d"),
            "Population"  = paste(greek, "=", fmt(pop)),
            "This sample" = paste(roman, "=", fmt(samp)),
            check.names = FALSE
        )
    }, striped = TRUE, colnames = TRUE, rownames = FALSE, align = "lrr")

    # Y-axis range from the MODEL, so the frame and the population-mean lines
    # stay put when students regenerate with the same parameters.
    y_limits <- reactive({
        p <- params()
        k <- max(3, qnorm(1 - (1 - 0.9^(1 / (2 * p$n))) / 2))
        lo <- min(p$mean1 - k * p$sd1, p$mean2 - k * p$sd2)
        hi <- max(p$mean1 + k * p$sd1, p$mean2 + k * p$sd2)
        if (lo == hi) c(lo - 1, hi + 1) else c(lo, hi)
    })

    output$dotplot <- renderPlot({
        d <- sim_data(); p <- params(); ylim <- y_limits()
        s1 <- d$Score[d$Group == G1]; s2 <- d$Score[d$Group == G2]
        samp_sd_pooled <- sqrt((var(s1) + var(s2)) / 2)
        samp_d <- (mean(s2) - mean(s1)) / samp_sd_pooled

        xpos <- c(1, 2)
        plot(NA, xlim = c(0.5, 2.5), ylim = ylim,
             xaxt = "n", xlab = "", ylab = "Score (DV)",
             main = paste("Sample d =", fmt(samp_d)))
        axis(1, at = xpos, labels = c(G1, G2))

        # jittered raw scores
        set.seed(NULL)
        jit1 <- xpos[1] + runif(length(s1), -0.12, 0.12)
        jit2 <- xpos[2] + runif(length(s2), -0.12, 0.12)
        points(jit1, s1, pch = 19, col = "steelblue")
        points(jit2, s2, pch = 19, col = "steelblue")

        seg <- 0.28
        # population means: grey dashed
        segments(xpos - seg, c(p$mean1, p$mean2),
                 xpos + seg, c(p$mean1, p$mean2),
                 col = "grey40", lwd = 2, lty = 2)
        # sample means: red solid
        segments(xpos - seg, c(mean(s1), mean(s2)),
                 xpos + seg, c(mean(s1), mean(s2)),
                 col = "firebrick", lwd = 2)

        legend("topleft", bty = "n",
               legend = c("Population means (the true model)",
                          "Sample means"),
               col = c("grey40", "firebrick"), lwd = 2, lty = c(2, 1))
    })
}

shinyApp(ui = ui, server = server)
