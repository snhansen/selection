library(shiny)
library(shinythemes)
library(ggplot2)
library(ggnewscale)
library(shinyFeedback)
options(scipen = 999)

server <- function(input, output) {
  plot_colors <- c("orange", "black", "red")
  max_obs <- 100000
  
  # Initializing some reactive vectors.
  betas <- reactiveValues(vals = NULL)
  selection <- reactiveValues(vals = NULL)
  coefs <- reactiveValues(vals = NULL,
                          lower = NULL,
                          upper = NULL,
                          se = NULL)
  sel_coefs <- reactiveValues(vals = NULL,
                              lower = NULL,
                              upper = NULL,
                              se = NULL)
  
  # Creating a reactive that indicates whether inputs are valid.
  valid <- reactive({
    input$n >= 2 & (input$n %% 1) == 0 & input$n <= max_obs & input$sd_res >= 0
  })
  
  # Throw warnings if inputs aren't valid. 
  observe({
    feedbackWarning("sd_res", input$sd_res < 0, "Negative sd not allowed.")
    
    # For the number of observations there are two cases that we wish to
    # treat separately. This is done with the following trick.
    hideFeedback("n")
    case1 <- input$n > max_obs
    case2 <- (input$n < 2) | ((input$n %% 1) != 0)
    feedback_text <- ""
    if (case1) {
      feedback_text <- paste0("Too many observations (maximum allowed is ", max_obs, ").")
    }
    else if (case2) {
      feedback_text <- paste0("Number of observations needs to be an integer >= 2.")
    }
    feedbackWarning("n", case1 | case2, feedback_text)
  })
 
  # Simulate data upon pressing the generate button if inputs are valid. 
  dat <- eventReactive(input$generate, {
    if (valid()) {
      x <- runif(input$n, 0, 10)
      y <- input$beta0 + input$beta1*x + rnorm(input$n, 0, input$sd_res)
      data.frame(x,y)
    }
    else {
      NULL
    }
  })
  
  # If data has succesfully been generated, then we store 
  # the true parameters and fit a linear regression model.
  observeEvent(input$generate, {
    betas$vals <- c(input$beta0, input$beta1)
    if (!is.null(dat())) {
      model1 <- lm(y ~ x, dat())
      coefs$vals <- model1$coef
      coefs$lower <- confint(model1)[,1]
      coefs$upper <- confint(model1)[,2]
      coefs$se <- summary(model1)$coef[,2]
    }
    else {
      coefs$vals <- NULL
      coefs$lower <- NULL
      coefs$upper <- NULL
      coefs$se <- NULL
    }
  })
  
  # If data has succesfully been generated, then a vector representing
  # the selection is created.
  selection <- reactive({
    if(!is.null(dat())) {
      (dat()$y>=input$yrange[1] & dat()$y<=input$yrange[2] & dat()$x>=input$xrange[1] & dat()$x<=input$xrange[2])
    }
    else {
      NULL
    }
  })
  
  # The corresponding selected data is created but only if there is at
  # least two observations (otherwise a linear regression makes no sense).
  sel_dat <- reactive({
    if (!is.null(dat()) & sum(selection()) >= 2) {
      subset(dat(), selection())
    } 
    else {
      NULL
    }
  })
  
  # If selected data is available, we fit a linear regression to those.
  observe({
    if (!is.null(sel_dat())) {
      model2 <- lm(y~x, sel_dat())
      sel_coefs$vals <- model2$coef
      sel_coefs$lower <- confint(model2)[,1]
      sel_coefs$upper <- confint(model2)[,2]
      sel_coefs$se <- summary(model2)$coef[,2]  
    }
    else {
      sel_coefs$vals <- NULL
      sel_coefs$lower <- NULL
      sel_coefs$upper <- NULL
      sel_coefs$se <- NULL
    }
  })
  
  # Make various plots depending on the user-specified preferences.
  make_plot <- function(dat, sel_dat, betas, coefs, sel_coefs, points, selection, ptrans) {
    
    # Return NULL if no valid data is available.
    if (is.null(dat)) {
      return(NULL)
    }
    
    # If user has specified selection but no valid selection data is
    # available, then we simply turn off selection.
    if (selection & is.null(sel_dat)) {
      return(make_plot(dat = dat,
                       sel_dat = NULL,
                       betas = betas,
                       coefs = coefs,
                       sel_coefs = NULL,
                       points = points,
                       selection = FALSE,
                       ptrans = ptrans))
    }
    
    # This is the base plot.
    base_plot <- ggplot() +
      theme_minimal() +
      labs(x = "", y = "") +
      geom_abline(aes(color = "True line",
                      linetype = "True line",
                      slope = betas$vals[2],
                      intercept = betas$vals[1]),
                  size = 1) +
      geom_abline(aes(color = "Linear fit (all)",
                      linetype = "Linear fit (all)",
                      slope = coefs$vals[2],
                      intercept = coefs$vals[1]),
                  size=1) +
      scale_color_manual(name = "",
                         values = c("True line" = "orange",
                                    "Linear fit (all)" = "black")) +
      scale_linetype_manual(name = "",
                            values = c("True line" = 1,
                                       "Linear fit (all)" = 2)) +
      geom_point(data = dat,
                 aes(x = x, y = y),
                 alpha = 0) +
      theme(legend.position = "bottom",
            text = element_text(size = 20))
    
    # If points and selection is off, return base plot.
    if (!points & !selection) {
      res_plot <- base_plot
    }
    
    # If selection is on we add a line based on the selected data.
    # We add the points to graph but make them invisible in order for
    # the axes to be identical when toggling points on and off.
    else if (!points & selection) {
      res_plot <- base_plot + 
        geom_abline(aes(color = "Linear fit (selected)",
                        linetype = "Linear fit (selected)",
                        slope = sel_coefs$vals[2],
                        intercept = sel_coefs$vals[1]),
                    size=1) +
        scale_color_manual(name = "",
                           values = c("True line" = "orange",
                                      "Linear fit (all)" = "black",
                                      "Linear fit (selected)" = "red")) +
        scale_linetype_manual(name = "",
                              values = c("True line" = 1,
                                         "Linear fit (all)" = 2,
                                         "Linear fit (selected)" = 2)) +
        geom_point(data = dat(),
                   aes(x = x, y = y),
                   alpha=0)
    }
    
    # If points are on, we plot those.
    else if (points & !selection) {
      res_plot <- base_plot + 
        new_scale_color() +
        geom_point(data = dat,
                   aes(x = x,
                       y = y,
                       shape = "Observations",
                       size = "Observations",
                       color = "Observations"),
                   alpha = input$transparency) +
        scale_size_manual(name = "",
                          values = c("Observations" = 1.5)) +
        scale_shape_manual(name = "",
                           values = c("Observations" = 16)) +
        scale_color_manual(name = "",
                           values = c("Observations" = "black")) +
        guides(size = guide_legend(order = 1),
               color = guide_legend(order = 1),
               shape = guide_legend(order = 1))
    }
    
    # If selection and points are on, we show the points
    # from both datasets.
    else if (points & selection) {
      res_plot <- base_plot + 
        geom_abline(aes(color = "Linear fit (selected)",
                        linetype = "Linear fit (selected)",
                        slope = sel_coefs$vals[2],
                        intercept = sel_coefs$vals[1]),
                    size=1) +
        scale_color_manual(name = "",
                           values = c("True line" = "orange",
                                      "Linear fit (all)" = "black",
                                      "Linear fit (selected)" = "red")) +
        scale_linetype_manual(name = "",
                              values = c("True line" = 1,
                                         "Linear fit (all)" = 2,
                                         "Linear fit (selected)" = 2)) +
        new_scale_color() +
        geom_point(data = dat,
                   aes(x = x,
                       y = y,
                       shape = "Observations",
                       size = "Observations",
                       color = "Observations"),
                   alpha = input$transparency) +
        geom_point(data = sel_dat,
                   aes(x = x,
                       y = y,
                       shape = "Observations (selected)",
                       size = "Observations (selected)",
                       color = "Observations (selected)"),
                   alpha = input$transparency) +
        scale_size_manual(name = "",
                          values = c("Observations" = 1.5,
                                     "Observations (selected)" = 3)) +
        scale_shape_manual(name = "",
                           values = c("Observations" = 16,
                                      "Observations (selected)" = 21)) +
        scale_color_manual(name = "",
                           values = c("Observations" = "black",
                                      "Observations (selected)" = "red")) +
        guides(size = guide_legend(order = 1),
               color = guide_legend(order = 1),
               shape = guide_legend(order = 1))
    }
    return(res_plot)
  }
  
  # A reactive plot is made.
  plot <- reactive({
    make_plot(dat = dat(),
              sel_dat = sel_dat(),
              betas = betas,
              coefs = coefs,
              sel_coefs = sel_coefs,
              points = (input$toggle_points == "Show"),
              selection = (input$toggle_selection == "On"),
              ptrans = input$transparency)
  })
  
  
  # We update the slider range as this range depends on the variation 
  # in the simulated data.
  observe({
    if (!is.null(dat())) {
      y_upper <- ceiling(max(dat()$y))
      y_lower <- floor(min(dat()$y))
      x_upper <- ceiling(max(dat()$x))
      x_lower <- floor(min(dat()$x))
    }
    else {
      y_upper <- 10
      y_lower <- -10
      x_upper <- 10
      x_lower <- -10
    }
    updateSliderInput(
      inputId = "yrange",
      min = y_lower,
      max = y_upper,
      value = c(y_lower, y_upper)
    )
    updateSliderInput(
      inputId = "xrange",
      min = x_lower,
      max = x_upper,
      value = c(x_lower, x_upper)
    )
  })
  
  # Output the plot.
  output$main_plot <- renderPlot({plot()})
  
  # Output estimates based on the original data.
  output$estimates_all <- renderTable({
    if (!is.null(dat())) {
      res <- cbind(round(coefs$vals,2), round(coefs$se,2), round(coefs$lower,2), round(coefs$upper,2))
      res <- cbind(c("Intercept", "Slope"), res)
      colnames(res) <- c(" ", "Estimate", "SE", "Lower", "Upper")
      res
    }
    else {
      NULL
    }
  })
  
  # Output estimates based on the selected data.
  output$estimates_sel <- renderTable({
    if (!is.null(sel_dat()) & (input$toggle_selection == "On")) {
      res <- cbind(round(sel_coefs$vals,2), round(sel_coefs$se,2), round(sel_coefs$lower,2), round(sel_coefs$upper,2))
      res <- cbind(c("Intercept", "Slope"), res)
      colnames(res) <- c(" ", "Estimate", "SE", "Lower", "Upper")
      res
    }
    else {
      NULL
    }
  })
}