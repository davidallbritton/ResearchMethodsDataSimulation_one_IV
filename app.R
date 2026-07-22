#
# Research Methods Data Simulation
#
# A teaching app: students choose population parameters and generate fake data
# for class projects, then download it as CSV to analyze in their stats
# software. Two generators, each on its own page:
#
#   * Scatterplot / correlation  (correlation module)
#   * Independent-samples t-test (ttest module)
#
# A landing "Instructions" page explains both and links to each. Navigation
# uses a HIDDEN tabsetPanel (no visible tab bar, to save screen space); the
# buttons/links call updateTabsetPanel() to switch pages. Each generator is a
# Shiny module, so their (identical) input/output IDs never collide.
#

library(shiny)

# ---- Shared constants -------------------------------------------------------

# Label for the row-number column, on screen and in the downloaded CSV.
ID_LABEL <- "Participant"

# Group names for the t-test (also the levels of its IV).
G1 <- "Group 1"
G2 <- "Group 2"

# Condition names for the paired t-test (the two measurements per participant).
C1 <- "Condition 1"
C2 <- "Condition 2"

# ---- Shared formatting helpers ----------------------------------------------

# One display format everywhere on screen: 2 decimals.
fmt <- function(x) sprintf("%.2f", x)

# ...except in the R code blocks, which must reproduce what the app did.
# as.character() keeps a typed 1.005 as 1.005 rather than rounding to 1.00.
fmt_code <- function(x) as.character(x)

# APA p-value: no leading zero, and "< .001" for very small values.
fmt_p <- function(p) {
    if (p < .001) "< .001" else sub("0\\.", ".", sprintf("= %.3f", p))
}

# APA correlation: 2 decimals, no leading zero, sign preserved (e.g. -.45).
fmt_r <- function(x) sub("(-?)0\\.", "\\1.", sprintf("%.2f", x))

# ---- Shared styling ---------------------------------------------------------

app_css <- HTML("
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
    /* Results readout below a plot (used by both generators). */
    .result-box { border: 1px solid #d4d4d4; border-radius: 6px;
                  background: #fafafa; padding: 8px 12px; margin-top: 10px;
                  max-width: 460px; }
    .result-box .result-title { font-weight: bold; }
    .result-box .apa { font-size: 108%; }
    .result-box .decision { color: #5a6570; }
    /* The primary box is the one students should model their write-up on;
       make it stand out from the supplementary boxes below it. */
    .result-box.primary { border: 2px solid #2c5f8a; background: #eef4fa; }
    .result-box.primary .result-title { color: #204c6e; }
    .result-flag { display: inline-block; font-size: 78%; font-weight: bold;
                   text-transform: uppercase; letter-spacing: .04em;
                   color: #fff; background: #2c5f8a; border-radius: 3px;
                   padding: 1px 7px; margin-bottom: 6px; }
    /* Navigation and instructions. */
    .nav-back { margin: 8px 0 0 2px; font-size: 95%; }
    .instructions-wrap { max-width: 860px; }
    .gen-card { border: 1px solid #b8c4d0; border-radius: 6px;
                background: #f4f7fa; padding: 14px 18px; margin-bottom: 16px; }
    .gen-card h3 { margin-top: 4px; }
")

# =============================================================================
#  Correlation module -- scatterplot / correlation data
# =============================================================================

corrUI <- function(id) {
    ns <- NS(id)
    tagList(
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
                    numericInput(ns("mean_x"), "Mean of X:", value = 0),
                    numericInput(ns("sd_x"),   "SD of X:",   value = 1, min = 0),

                    span(class = "var-head", "Outcome / DV (Y)"),
                    numericInput(ns("mean_y"), "Mean of Y:", value = 0),
                    numericInput(ns("slope"),
                                 "Slope: how much Y rises per 1-unit rise in X:",
                                 value = 0.5, step = 0.1),
                    numericInput(ns("sd_e"),
                                 "Noise: SD of the random error added to Y:",
                                 value = 1, min = 0, step = 0.1),

                    uiOutput(ns("r_preview"))
                ),

                # Sample size and the button sit outside the box: how many cases
                # you draw is not a property of the population.
                div(
                    class = "sample-controls",
                    fluidRow(
                        column(6, numericInput(ns("n"), "Number of cases (N):",
                                               value = 30, min = 3, step = 1)),
                        column(6, div(style = "margin-top: 25px;",
                                      actionButton(ns("generate"), "Generate Data",
                                                   class = "btn-primary",
                                                   width = "100%")))
                    )
                ),

                tags$hr(),
                tags$strong("The model"),
                uiOutput(ns("sd_note")),
                uiOutput(ns("equations")),

                tags$hr(),
                tags$strong("R code"),
                verbatimTextOutput(ns("code"))
            ),

            mainPanel(
                width = 8,
                div(
                    class = "panel-row",
                    div(
                        class = "stats-col",
                        tags$h4("Descriptive Statistics"),
                        tableOutput(ns("sample_stats")),
                        div(
                            class = "data-head",
                            tags$h4("Sample Data"),
                            downloadButton(ns("download_csv"), "CSV",
                                           class = "btn-xs")
                        ),
                        div(
                            style = "max-height: 420px; overflow-y: auto;",
                            tableOutput(ns("data_table"))
                        )
                    ),
                    div(
                        class = "plot-col",
                        tags$h4("Scatterplot"),
                        plotOutput(ns("scatter"), height = "500px"),
                        uiOutput(ns("cor_result")),
                        uiOutput(ns("reg_result"))
                    )
                )
            )
        )
    )
}

corrServer <- function(id) {
    moduleServer(id, function(input, output, session) {

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
            p <- derive(input$mean_x, input$sd_x, input$mean_y,
                        input$slope, input$sd_e)
            p$n <- max(3, round(input$n))
            p
        }, ignoreNULL = FALSE)

        sim_data <- reactive({
            p <- params()
            X <- rnorm(p$n, mean = p$mean_x, sd = p$sd_x)
            Y <- p$b0 + p$slope * X + rnorm(p$n, mean = 0, sd = p$sd_e)
            data.frame(X = round(X, 2), Y = round(Y, 2))
        })

        # Compact readout inside the parameter box; fuller note lives below.
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
                "Y ends up with a <b>total</b> SD of <b>%s</b> &mdash; bigger than
                 the noise you typed (%s), because Y varies for <i>two</i> reasons:
                 people differ on X (which moves them along the line), and each
                 person also has their own random error.
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
                helpText("Each Y score starts at the mean of Y, is adjusted up or
                          down by how far that person's X is from average, then
                          gets its own random error:"),
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
                " "           = c("Mean of X", "SD of X", "Mean of Y",
                                  "SD of Y (total spread)",
                                  "SD of the error (noise)",
                                  "Slope", "Correlation"),
                "Population"  = paste(greek, "=", fmt(pop)),
                "This sample" = paste(roman, "=", fmt(samp)),
                check.names = FALSE
            )
        }, striped = TRUE, colnames = TRUE, rownames = FALSE, align = "lrr")

        # Axis limits from the MODEL, not the sample, so the plot frame and the
        # true line stay put when students regenerate with the same parameters.
        # Choose k so that every point lands inside the frame about 90% of the
        # time (a point is 2 coordinates, hence 2n normal deviates). Using the
        # marginal SD of Y also keeps the true line in frame.
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

        output$cor_result <- renderUI({
            d <- sim_data()
            ct <- cor.test(d$X, d$Y)          # Pearson; df = N - 2
            sig <- ct$p.value < .05

            # Fisher-z CI needs N > 3; guard the small-N case.
            ci <- if (length(ct$conf.int) == 2)
                sprintf("95%% CI [%s, %s]",
                        fmt_r(ct$conf.int[1]), fmt_r(ct$conf.int[2]))
            else NULL

            div(class = "result-box primary",
                div(class = "result-flag", "Model write-up"),
                div(class = "result-title", "Pearson correlation"),
                div(class = "apa", HTML(sprintf(
                    "<i>r</i>(%d) = %s, <i>p</i> %s",
                    round(ct$parameter), fmt_r(ct$estimate), fmt_p(ct$p.value)
                ))),
                if (!is.null(ci)) div(HTML(ci)),
                div(class = "decision", sprintf(
                    "The correlation is %sstatistically significant at α = .05.",
                    if (sig) "" else "not "
                ))
            )
        })

        output$reg_result <- renderUI({
            d <- sim_data()
            fit <- lm(Y ~ X, data = d)
            sm  <- summary(fit)
            b0  <- coef(fit)[1]; b1 <- coef(fit)[2]
            se_b1 <- sm$coefficients[2, 2]
            t_b1  <- sm$coefficients[2, 3]
            p_b1  <- sm$coefficients[2, 4]
            r2    <- sm$r.squared
            Fs    <- sm$fstatistic            # value, numdf, dendf
            Fp    <- pf(Fs[1], Fs[2], Fs[3], lower.tail = FALSE)

            # Prediction equation, with the slope's sign read aloud.
            eq <- sprintf("&#374; = %s %s %sX",
                          fmt(b0), if (b1 < 0) "&minus;" else "+", fmt(abs(b1)))

            div(class = "result-box",
                div(class = "result-title", "Linear regression"),
                div(class = "apa", HTML(eq)),
                div(HTML(sprintf(
                    "<i>R</i>&sup2; = %s, <i>F</i>(%d, %d) = %s, <i>p</i> %s",
                    fmt_r(r2), round(Fs[2]), round(Fs[3]), fmt(Fs[1]), fmt_p(Fp)
                ))),
                div(HTML(sprintf(
                    "Slope: <i>b</i> = %s, <i>SE</i> = %s, <i>t</i>(%d) = %s, <i>p</i> %s",
                    fmt(b1), fmt(se_b1), round(sm$df[2]), fmt(t_b1), fmt_p(p_b1)
                )))
            )
        })
    })
}

# =============================================================================
#  t-test module -- independent-samples t-test data
# =============================================================================

ttestUI <- function(id) {
    ns <- NS(id)
    tagList(
        titlePanel("Simulate Data: Independent-samples t-test"),
        sidebarLayout(

            sidebarPanel(
                width = 4,

                div(
                    class = "param-box",

                    span(class = "box-title", "Population parameters"),
                    div(class = "box-note",
                        "The true values in each group in the whole population.
                         Your sample is drawn from populations that look like
                         this."),

                    span(class = "var-head", paste0(G1, "  (IV level 1)")),
                    numericInput(ns("mean1"), "Mean of Group 1:", value = 100),
                    numericInput(ns("sd1"),   "SD of Group 1:",   value = 15,
                                 min = 0),

                    span(class = "var-head", paste0(G2, "  (IV level 2)")),
                    numericInput(ns("mean2"), "Mean of Group 2:", value = 110),
                    numericInput(ns("sd2"),   "SD of Group 2:",   value = 15,
                                 min = 0),

                    uiOutput(ns("d_preview"))
                ),

                # Sample size per group and the button sit outside the box.
                div(
                    class = "sample-controls",
                    fluidRow(
                        column(6, numericInput(ns("n"), "Cases per group (n):",
                                               value = 30, min = 2, step = 1)),
                        column(6, div(style = "margin-top: 25px;",
                                      actionButton(ns("generate"), "Generate Data",
                                                   class = "btn-primary",
                                                   width = "100%")))
                    )
                ),

                tags$hr(),
                tags$strong("The model"),
                uiOutput(ns("equations")),

                tags$hr(),
                tags$strong("R code"),
                verbatimTextOutput(ns("code"))
            ),

            mainPanel(
                width = 8,
                div(
                    class = "panel-row",
                    div(
                        class = "stats-col",
                        tags$h4("Descriptive Statistics"),
                        tableOutput(ns("sample_stats")),
                        div(
                            class = "data-head",
                            tags$h4("Sample Data"),
                            downloadButton(ns("download_csv"), "CSV",
                                           class = "btn-xs")
                        ),
                        div(
                            style = "max-height: 420px; overflow-y: auto;",
                            tableOutput(ns("data_table"))
                        )
                    ),
                    div(
                        class = "plot-col",
                        tags$h4("Independent Groups Comparison"),
                        plotOutput(ns("dotplot"), height = "500px"),
                        uiOutput(ns("ttest_result")),
                        uiOutput(ns("anova_result")),
                        uiOutput(ns("dummy_result"))
                    )
                )
            )
        )
    )
}

ttestServer <- function(id) {
    moduleServer(id, function(input, output, session) {

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
                "# var.equal = TRUE gives Student's t (R defaults to Welch)\n",
                "t.test(group2, group1, var.equal = TRUE)\n",
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
                paste0("simulated_ttest_data_",
                       format(Sys.Date(), "%Y-%m-%d"), ".csv")
            },
            content = function(file) {
                write.csv(labelled_data(), file, row.names = FALSE)
            }
        )

        output$sample_stats <- renderTable({
            d <- sim_data(); p <- params()
            s1 <- d$Score[d$Group == G1]; s2 <- d$Score[d$Group == G2]
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

        output$ttest_result <- renderUI({
            d <- sim_data()
            s1 <- d$Score[d$Group == G1]; s2 <- d$Score[d$Group == G2]
            # Student's t (pooled variance), to match the pooled-SD Cohen's d.
            tt <- t.test(s2, s1, var.equal = TRUE)
            samp_d <- (mean(s2) - mean(s1)) / sqrt((var(s1) + var(s2)) / 2)
            sig <- tt$p.value < .05

            div(class = "result-box primary",
                div(class = "result-flag", "Model write-up"),
                div(class = "result-title", "Independent-samples t-test"),
                div(class = "apa", HTML(sprintf(
                    "<i>t</i>(%d) = %s, <i>p</i> %s, <i>d</i> = %s",
                    round(tt$parameter), fmt(tt$statistic), fmt_p(tt$p.value),
                    fmt(samp_d)
                ))),
                div(HTML(sprintf(
                    "Mean difference = %s, 95%% CI [%s, %s]",
                    fmt(mean(s2) - mean(s1)),
                    fmt(tt$conf.int[1]), fmt(tt$conf.int[2])
                ))),
                div(class = "decision", sprintf(
                    "The difference is %sstatistically significant at α = .05.",
                    if (sig) "" else "not "
                ))
            )
        })

        # Just for fun: the same comparison as a one-way ANOVA. With two groups
        # F = t^2 and the p-value is identical to the t-test above.
        output$anova_result <- renderUI({
            d <- sim_data()
            s  <- summary(aov(Score ~ Group, data = d))[[1]]
            Fv <- s[["F value"]][1]; Fp <- s[["Pr(>F)"]][1]
            ss <- s[["Sum Sq"]]; eta2 <- ss[1] / sum(ss)

            div(class = "result-box",
                div(class = "result-title", "One-way ANOVA"),
                div(class = "apa", HTML(sprintf(
                    "<i>F</i>(%d, %d) = %s, <i>p</i> %s",
                    round(s[["Df"]][1]), round(s[["Df"]][2]),
                    fmt(Fv), fmt_p(Fp)
                ))),
                div(HTML(sprintf("&eta;&sup2; = %s", fmt_r(eta2)))),
                div(class = "decision",
                    "Same p as the t-test — with two groups, F = t².")
            )
        })

        # And again as a correlation: code the groups 0/1 and correlate with the
        # score. This point-biserial r has the same p; the t-test is a
        # correlation with a two-value predictor.
        output$dummy_result <- renderUI({
            d <- sim_data()
            x  <- as.integer(d$Group) - 1L      # Group 1 = 0, Group 2 = 1
            ct <- cor.test(x, d$Score)

            div(class = "result-box",
                div(class = "result-title",
                    "Point-biserial correlation (groups coded 0/1)"),
                div(class = "apa", HTML(sprintf(
                    "<i>r</i>(%d) = %s, <i>p</i> %s",
                    round(ct$parameter), fmt_r(ct$estimate), fmt_p(ct$p.value)
                ))),
                div(class = "decision",
                    "Same p once more — and r² equals the ANOVA's η².")
            )
        })

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
    })
}

# =============================================================================
#  Paired-samples (dependent) t-test module
#
#  Like the independent module, but each participant is measured twice and the
#  two measurements are CORRELATED. That correlation is a third population
#  parameter: the stronger it is, the smaller the SD of the difference scores
#  (sd_D = sqrt(s1^2 + s2^2 - 2*rho*s1*s2)), and the more powerful the test.
# =============================================================================

pairedUI <- function(id) {
    ns <- NS(id)
    tagList(
        titlePanel("Simulate Data: Paired-samples t-test"),
        sidebarLayout(

            sidebarPanel(
                width = 4,

                div(
                    class = "param-box",

                    span(class = "box-title", "Population parameters"),
                    div(class = "box-note",
                        "Each participant is measured twice. Set the true mean and
                         SD of each measurement, plus how strongly the two
                         measurements are correlated."),

                    span(class = "var-head", paste0(C1, "  (measurement 1)")),
                    numericInput(ns("mean_c1"), "Mean of Condition 1:", value = 100),
                    numericInput(ns("sd_c1"),   "SD of Condition 1:",   value = 15,
                                 min = 0),

                    span(class = "var-head", paste0(C2, "  (measurement 2)")),
                    numericInput(ns("mean_c2"), "Mean of Condition 2:", value = 110),
                    numericInput(ns("sd_c2"),   "SD of Condition 2:",   value = 15,
                                 min = 0),

                    numericInput(ns("rho"),
                                 "Correlation between the two measurements (r):",
                                 value = 0.5, min = -1, max = 1, step = 0.05),

                    uiOutput(ns("dz_preview"))
                ),

                # Sample size and the button sit outside the box.
                div(
                    class = "sample-controls",
                    fluidRow(
                        column(6, numericInput(ns("n"),
                                               "Number of participants (n):",
                                               value = 30, min = 2, step = 1)),
                        column(6, div(style = "margin-top: 25px;",
                                      actionButton(ns("generate"), "Generate Data",
                                                   class = "btn-primary",
                                                   width = "100%")))
                    )
                ),

                tags$hr(),
                tags$strong("The model"),
                uiOutput(ns("equations")),

                tags$hr(),
                tags$strong("R code"),
                verbatimTextOutput(ns("code"))
            ),

            mainPanel(
                width = 8,
                div(
                    class = "panel-row",
                    div(
                        class = "stats-col",
                        tags$h4("Descriptive Statistics"),
                        tableOutput(ns("sample_stats")),
                        div(
                            class = "data-head",
                            tags$h4("Sample Data"),
                            downloadButton(ns("download_csv"), "CSV",
                                           class = "btn-xs")
                        ),
                        div(
                            style = "max-height: 420px; overflow-y: auto;",
                            tableOutput(ns("data_table"))
                        )
                    ),
                    div(
                        class = "plot-col",
                        tags$h4("Paired Scores Comparison"),
                        plotOutput(ns("pairplot"), height = "500px"),
                        uiOutput(ns("ttest_result")),
                        uiOutput(ns("anova_result")),
                        uiOutput(ns("cor_result"))
                    )
                )
            )
        )
    )
}

pairedServer <- function(id) {
    moduleServer(id, function(input, output, session) {

        # Everything implied by a set of parameter values.
        derive <- function(mean_c1, sd_c1, mean_c2, sd_c2, rho) {
            sd_c1 <- abs(sd_c1); sd_c2 <- abs(sd_c2)
            rho   <- max(-0.99, min(0.99, rho))   # keep sd_D > 0 and generation valid
            sd_D  <- sqrt(sd_c1^2 + sd_c2^2 - 2 * rho * sd_c1 * sd_c2)
            list(
                mean_c1 = mean_c1, sd_c1 = sd_c1, mean_c2 = mean_c2, sd_c2 = sd_c2,
                rho = rho, sd_D = sd_D, diff = mean_c2 - mean_c1,
                dz = if (sd_D > 0) (mean_c2 - mean_c1) / sd_D else 0
            )
        }

        live <- reactive({
            req(input$sd_c1, input$sd_c2, input$rho)
            derive(input$mean_c1, input$sd_c1, input$mean_c2, input$sd_c2, input$rho)
        })

        params <- eventReactive(input$generate, {
            p <- derive(input$mean_c1, input$sd_c1, input$mean_c2, input$sd_c2,
                        input$rho)
            p$n <- max(2, round(input$n))
            p
        }, ignoreNULL = FALSE)

        # Two correlated measurements per participant (same construction as the
        # correlation module: measurement 2 is measurement 1's z-score, blended
        # with fresh noise in proportion rho).
        sim_data <- reactive({
            p <- params()
            z  <- rnorm(p$n)
            s1 <- p$mean_c1 + p$sd_c1 * z
            s2 <- p$mean_c2 + p$sd_c2 * (p$rho * z + sqrt(1 - p$rho^2) * rnorm(p$n))
            data.frame(Score1 = round(s1, 2), Score2 = round(s2, 2))
        })

        output$dz_preview <- renderUI({
            p <- live()
            helpText(HTML(sprintf(
                "These settings give: SD of the differences = <b>%s</b>,
                 Cohen's <i>d<sub>z</sub></i> = <b>%s</b>.<br/>A higher correlation
                 shrinks the difference SD and strengthens the effect.",
                fmt(p$sd_D), fmt(p$dz)
            )))
        })

        output$equations <- renderUI({
            p <- params()
            withMathJax(
                helpText("Each participant is measured twice; the two scores are
                          correlated:"),
                helpText(sprintf(
                    "$$\\text{Score}_{i,1} = \\mu_1 + e_{i,1}
                       \\quad (\\mu_1 = %s,\\ \\sigma_1 = %s)$$",
                    fmt(p$mean_c1), fmt(p$sd_c1)
                )),
                helpText(sprintf(
                    "$$\\text{Score}_{i,2} = \\mu_2 + e_{i,2}
                       \\quad (\\mu_2 = %s,\\ \\sigma_2 = %s),
                       \\quad \\text{cor} = %s$$",
                    fmt(p$mean_c2), fmt(p$sd_c2), fmt(p$rho)
                )),
                helpText("The test uses each participant's difference score,
                          \\(D_i = \\text{Score}_{i,2} - \\text{Score}_{i,1}\\):"),
                helpText(sprintf(
                    "$$d_z = \\frac{\\mu_2 - \\mu_1}{\\sigma_D}, \\quad
                       \\sigma_D = \\sqrt{\\sigma_1^2 + \\sigma_2^2
                                          - 2\\rho\\sigma_1\\sigma_2} = %s$$",
                    fmt(p$sd_D)
                ))
            )
        })

        output$code <- renderText({
            p <- params()
            paste0(
                "n <- ", p$n, "\n",
                "# two correlated measurements per participant (r = ",
                    fmt_code(p$rho), ")\n",
                "z <- rnorm(n)\n",
                "Score1 <- ", fmt_code(p$mean_c1), " + ", fmt_code(p$sd_c1),
                    " * z\n",
                "Score2 <- ", fmt_code(p$mean_c2), " + ", fmt_code(p$sd_c2),
                    " * (", fmt_code(p$rho), " * z + sqrt(1 - ", fmt_code(p$rho),
                    "^2) * rnorm(n))\n",
                "\n",
                "t.test(Score2, Score1, paired = TRUE)\n",
                "cor(Score1, Score2)"
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
                paste0("simulated_paired_data_",
                       format(Sys.Date(), "%Y-%m-%d"), ".csv")
            },
            content = function(file) {
                write.csv(labelled_data(), file, row.names = FALSE)
            }
        )

        output$sample_stats <- renderTable({
            d <- sim_data(); p <- params()
            s1 <- d$Score1; s2 <- d$Score2; D <- s2 - s1
            samp_dz <- if (sd(D) > 0) mean(D) / sd(D) else 0

            greek <- c("μ₁", "σ₁", "μ₂", "σ₂", "ρ", "μ₂−μ₁", "δ")
            roman <- c("M₁", "s₁", "M₂", "s₂", "r", "M₂−M₁", "d")
            pop   <- c(p$mean_c1, p$sd_c1, p$mean_c2, p$sd_c2, p$rho, p$diff, p$dz)
            samp  <- c(mean(s1), sd(s1), mean(s2), sd(s2),
                       cor(s1, s2), mean(D), samp_dz)

            data.frame(
                " "           = c("Mean of Condition 1", "SD of Condition 1",
                                  "Mean of Condition 2", "SD of Condition 2",
                                  "Correlation of C1 & C2",
                                  "Mean difference", "Cohen's d (dz)"),
                "Population"  = paste(greek, "=", fmt(pop)),
                "This sample" = paste(roman, "=", fmt(samp)),
                check.names = FALSE
            )
        }, striped = TRUE, colnames = TRUE, rownames = FALSE, align = "lrr")

        output$ttest_result <- renderUI({
            d <- sim_data()
            s1 <- d$Score1; s2 <- d$Score2; D <- s2 - s1
            tt <- t.test(s2, s1, paired = TRUE)
            samp_dz <- if (sd(D) > 0) mean(D) / sd(D) else 0
            sig <- tt$p.value < .05

            div(class = "result-box primary",
                div(class = "result-flag", "Model write-up"),
                div(class = "result-title", "Paired-samples t-test"),
                div(class = "apa", HTML(sprintf(
                    "<i>t</i>(%d) = %s, <i>p</i> %s, <i>d<sub>z</sub></i> = %s",
                    round(tt$parameter), fmt(tt$statistic), fmt_p(tt$p.value),
                    fmt(samp_dz)
                ))),
                div(HTML(sprintf(
                    "Mean difference = %s, 95%% CI [%s, %s]",
                    fmt(mean(D)), fmt(tt$conf.int[1]), fmt(tt$conf.int[2])
                ))),
                div(class = "decision", sprintf(
                    "The difference is %sstatistically significant at α = .05.",
                    if (sig) "" else "not "
                ))
            )
        })

        # Just for fun: the same comparison as a repeated-measures ANOVA. With
        # two conditions F = t^2 and the p-value is identical to the paired
        # t-test above.
        output$anova_result <- renderUI({
            d <- sim_data()
            s1 <- d$Score1; s2 <- d$Score2
            tt <- t.test(s2, s1, paired = TRUE)
            Fv <- tt$statistic^2
            df2 <- round(tt$parameter)               # n - 1
            eta2 <- Fv / (Fv + df2)                  # partial eta^2

            div(class = "result-box",
                div(class = "result-title", "Repeated-measures ANOVA"),
                div(class = "apa", HTML(sprintf(
                    "<i>F</i>(1, %d) = %s, <i>p</i> %s",
                    df2, fmt(Fv), fmt_p(tt$p.value)
                ))),
                div(HTML(sprintf("partial &eta;&sup2; = %s", fmt_r(eta2)))),
                div(class = "decision",
                    "Same p as the paired t-test — with two conditions, F = t².")
            )
        })

        # The score1-score2 correlation: the paired-specific relationship. The
        # stronger it is, the smaller the SD of the differences and the more
        # powerful the test.
        output$cor_result <- renderUI({
            d <- sim_data()
            ct <- cor.test(d$Score1, d$Score2)

            ci <- if (length(ct$conf.int) == 2)
                sprintf("95%% CI [%s, %s]",
                        fmt_r(ct$conf.int[1]), fmt_r(ct$conf.int[2]))
            else NULL

            div(class = "result-box",
                div(class = "result-title",
                    "Correlation between the two measurements"),
                div(class = "apa", HTML(sprintf(
                    "<i>r</i>(%d) = %s, <i>p</i> %s",
                    round(ct$parameter), fmt_r(ct$estimate), fmt_p(ct$p.value)
                ))),
                if (!is.null(ci)) div(HTML(ci)),
                div(class = "decision",
                    "Higher r → smaller SD of the differences → a more powerful
                     paired test.")
            )
        })

        # Y-axis range from the MODEL, so the frame and the population-mean lines
        # stay put when students regenerate with the same parameters.
        y_limits <- reactive({
            p <- params()
            k <- max(3, qnorm(1 - (1 - 0.9^(1 / (2 * p$n))) / 2))
            lo <- min(p$mean_c1 - k * p$sd_c1, p$mean_c2 - k * p$sd_c2)
            hi <- max(p$mean_c1 + k * p$sd_c1, p$mean_c2 + k * p$sd_c2)
            if (lo == hi) c(lo - 1, hi + 1) else c(lo, hi)
        })

        output$pairplot <- renderPlot({
            d <- sim_data(); p <- params(); ylim <- y_limits()
            s1 <- d$Score1; s2 <- d$Score2; D <- s2 - s1
            samp_dz <- if (sd(D) > 0) mean(D) / sd(D) else 0

            xpos <- c(1, 2)
            plot(NA, xlim = c(0.5, 2.5), ylim = ylim,
                 xaxt = "n", xlab = "", ylab = "Score (DV)",
                 main = paste("Sample d_z =", fmt(samp_dz)))
            axis(1, at = xpos, labels = c(C1, C2))

            # faint line linking each participant's two scores (the pairing)
            segments(xpos[1], s1, xpos[2], s2,
                     col = adjustcolor("grey30", alpha.f = 0.35), lwd = 1)
            points(rep(xpos[1], length(s1)), s1, pch = 19, col = "steelblue")
            points(rep(xpos[2], length(s2)), s2, pch = 19, col = "steelblue")

            seg <- 0.28
            # population means: grey dashed
            segments(xpos - seg, c(p$mean_c1, p$mean_c2),
                     xpos + seg, c(p$mean_c1, p$mean_c2),
                     col = "grey40", lwd = 2, lty = 2)
            # sample means: red solid
            segments(xpos - seg, c(mean(s1), mean(s2)),
                     xpos + seg, c(mean(s1), mean(s2)),
                     col = "firebrick", lwd = 2)

            legend("topleft", bty = "n",
                   legend = c("Each participant (paired scores)",
                              "Population means (the true model)",
                              "Sample means"),
                   col = c(adjustcolor("grey30", alpha.f = 0.5),
                           "grey40", "firebrick"),
                   lwd = c(1, 2, 2), lty = c(1, 2, 1))
        })
    })
}

# =============================================================================
#  Instructions (landing) page
# =============================================================================

instructionsUI <- function() {
    div(
        class = "instructions-wrap",
        titlePanel("Research Methods Data Simulator"),

        p("Generate realistic fake data for your class projects. Pick the",
          strong("population parameters"), "for the situation you want to
          study, draw a sample, and download it as a CSV file to analyze in
          your statistics software. Choose the kind of data you need:"),

        div(
            class = "gen-card",
            h3("Scatterplot / Correlation"),
            p("For studying the relationship between two continuous variables:
               a predictor (IV) and an outcome (DV). You set the population
               means and SDs, how strongly the outcome depends on the predictor
               (the slope), and how much random noise to add. The app draws a
               sample, plots it with the true and sample regression lines, and
               reports the correlation."),
            actionButton("to_corr", "Generate correlation data →",
                         class = "btn-primary btn-lg")
        ),

        div(
            class = "gen-card",
            h3("Independent-samples t-test"),
            p("For comparing the means of two independent groups on a continuous
               outcome (DV). You set each group's population mean and SD. The app
               draws a sample from each group, shows the group comparison, and
               reports the t-test result and Cohen's d."),
            actionButton("to_ttest", "Generate t-test data →",
                         class = "btn-primary btn-lg")
        ),

        div(
            class = "gen-card",
            h3("Paired-samples t-test"),
            p("For comparing two measurements taken on the ", em("same"),
              " participants (e.g. before vs. after) on a continuous outcome
               (DV). You set each measurement's population mean and SD, plus how
               strongly the two measurements are correlated. The app draws a
               sample, shows each participant's paired scores, and reports the
               paired t-test and Cohen's d."),
            actionButton("to_paired", "Generate paired t-test data →",
                         class = "btn-primary btn-lg")
        ),

        h4("How to use any generator"),
        tags$ol(
            tags$li("Type the population parameters on the left."),
            tags$li("Set how many cases to draw, then click ",
                    strong("Generate Data"), "."),
            tags$li("Review the descriptive statistics and plot on the right."),
            tags$li("Click the ", strong("CSV"), " button above the data table
                     to download your dataset."),
            tags$li("Use the ", strong("← Instructions"), " link to come back
                     to this page.")
        )
    )
}

# =============================================================================
#  Main app: hidden tabset ties the three pages together
# =============================================================================

ui <- fluidPage(
    withMathJax(),
    tags$head(tags$style(app_css)),

    # type = "hidden" => no tab bar is drawn; we switch pages with the buttons
    # and links below via updateTabsetPanel().
    tabsetPanel(
        id = "nav", type = "hidden",

        tabPanelBody("instructions", instructionsUI()),

        tabPanelBody(
            "corr",
            div(class = "nav-back",
                actionLink("home_from_corr", "← Instructions")),
            corrUI("corr")
        ),

        tabPanelBody(
            "ttest",
            div(class = "nav-back",
                actionLink("home_from_ttest", "← Instructions")),
            ttestUI("ttest")
        ),

        tabPanelBody(
            "paired",
            div(class = "nav-back",
                actionLink("home_from_paired", "← Instructions")),
            pairedUI("paired")
        )
    )
)

server <- function(input, output, session) {

    # Page navigation.
    observeEvent(input$to_corr,         updateTabsetPanel(session, "nav", "corr"))
    observeEvent(input$to_ttest,        updateTabsetPanel(session, "nav", "ttest"))
    observeEvent(input$to_paired,       updateTabsetPanel(session, "nav", "paired"))
    observeEvent(input$home_from_corr,  updateTabsetPanel(session, "nav", "instructions"))
    observeEvent(input$home_from_ttest, updateTabsetPanel(session, "nav", "instructions"))
    observeEvent(input$home_from_paired,updateTabsetPanel(session, "nav", "instructions"))

    # Generators.
    corrServer("corr")
    ttestServer("ttest")
    pairedServer("paired")
}

shinyApp(ui = ui, server = server)
