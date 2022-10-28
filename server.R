library(shiny)
library(shinythemes)
library(ggplot2)
library(ggnewscale)
library(shinyFeedback)
options(scipen=999)

server <- function(input, output) {
  plot_colors <<- c("orange", "black", "red")
  max_obs <<- 100000
  betas <- reactiveValues(vals=NULL)
  data_generated <- reactiveVal(FALSE)
  selected <- reactiveValues(vals=NULL)
  coefs <- reactiveValues(vals=NULL, lower=NULL, upper=NULL, se=NULL)
  sel_coefs <- reactiveValues(vals=NULL, lower=NULL, upper=NULL, se=NULL)
  
  valid_obs <- reactive({
    input$n <= max_obs
  })
  
  valid_sdres <- reactive({
    input$sd_res >= 0
  })
  
  observe({
    shinyFeedback::feedbackWarning("n", !valid_obs(), paste0("Number of observations set to ", max_obs))
    shinyFeedback::feedbackWarning("sd_res", !valid_sdres(), "Negative sd not allowed (set to 0).")
  })
  
  dat <- eventReactive(input$generate, {
    n <- min(round(input$n), max_obs)
    x <- runif(n, 0, 10)
    y <- input$beta0 + input$beta1*x + rnorm(n, 0, max(input$sd_res,0))
    data.frame(x,y)
  })
  
  observeEvent(input$generate, {
    data_generated(TRUE)
    model1 <- lm(y~x, dat())
    coefs$vals <- model1$coef
    coefs$lower <- confint(model1)[,1]
    coefs$upper <- confint(model1)[,2]
    coefs$se <- summary(model1)$coef[,2]
    betas$vals <- c(input$beta0, input$beta1)
  })
  
  observe({
    selected$vals <- (dat()$y>=input$yrange[1] & dat()$y<=input$yrange[2] & dat()$x>=input$xrange[1] & dat()$x<=input$xrange[2])
  })
  
  sel_dat <- reactive({
    if (data_generated()) {
      subset(dat(), y>=input$yrange[1] & y<=input$yrange[2] & x>=input$xrange[1] & x<=input$xrange[2])
    } else {
      data.frame()
    }
  })
  
  observe({
    if (data_generated()) {
      if (sum(selected$vals) >= 2) {
        model2 <- lm(y~x, sel_dat())
        sel_coefs$vals <- model2$coef
        sel_coefs$lower <- confint(model2)[,1]
        sel_coefs$upper <- confint(model2)[,2]
        sel_coefs$se <- summary(model2)$coef[,2]  
      }
    }
  })
  
  plot <- reactive({
    if (data_generated()) {
      if (input$toggle_points) {
        if (input$toggle_selection) {
          if (sum(selected$vals)>=2) {
            ggplot() + theme_minimal() + labs(x="", y="") + 
              geom_abline(aes(color="True line", linetype="True line", slope=betas$vals[2], intercept=betas$vals[1]), size=1) + 
              geom_abline(aes(color="Linear fit (all)", linetype="Linear fit (all)", slope=coefs$vals[2], intercept=coefs$vals[1]), size=1) +
              geom_abline(aes(color="Linear fit (selected)", linetype="Linear fit (selected)", slope=sel_coefs$vals[2], intercept=sel_coefs$vals[1]), size=1) +
              scale_color_manual(name="", values=c("True line"="orange", "Linear fit (all)"="black", "Linear fit (selected)"="red")) + 
              scale_linetype_manual(name="", values=c("True line"=1, "Linear fit (all)"=2, "Linear fit (selected)"=2)) +
              new_scale_color() +
              geom_point(data=dat(), aes(x=x, y=y, shape="Observations", size="Observations", color="Observations"), alpha=input$transparency) + 
              geom_point(data=sel_dat(), aes(x=x, y=y, shape="Observations (selected)", size="Observations (selected)", color="Observations (selected)"), alpha=input$transparency) +
              scale_size_manual(name="", values=c("Observations"=1.5, "Observations (selected)"=3)) +
              scale_shape_manual(name="", values=c("Observations"=16, "Observations (selected)"=21)) +
              scale_color_manual(name="", values=c("Observations"="black", "Observations (selected)"="red")) +
              theme(legend.position="bottom", text=element_text(size=20)) + 
              guides(size=guide_legend(order=1), color=guide_legend(order=1), shape=guide_legend(order=1))
          }
          else {
            ggplot() + theme_minimal() + labs(x="", y="") + 
              geom_abline(aes(color="True line", linetype="True line", slope=betas$vals[2], intercept=betas$vals[1]), size=1) + 
              geom_abline(aes(color="Linear fit (all)", linetype="Linear fit (all)", slope=coefs$vals[2], intercept=coefs$vals[1]), size=1) +
              scale_color_manual(name="", values=c("True line"="orange", "Linear fit (all)"="black", "Linear fit (selected)"="red")) + 
              scale_linetype_manual(name="", values=c("True line"=1, "Linear fit (all)"=2, "Linear fit (selected)"=2)) +
              new_scale_color() +
              geom_point(data=dat(), aes(x=x, y=y, shape="Observations", size="Observations", color="Observations"), alpha=input$transparency) + 
              scale_size_manual(name="", values=c("Observations"=1.5, "Observations (selected)"=3)) +
              scale_shape_manual(name="", values=c("Observations"=16, "Observations (selected)"=21)) +
              scale_color_manual(name="", values=c("Observations"="black", "Observations (selected)"="red")) +
              theme(legend.position="bottom", text=element_text(size=20)) + 
              guides(size=guide_legend(order=1), color=guide_legend(order=1), shape=guide_legend(order=1))
          }
          
          
        }
        else { 
          ggplot() + theme_minimal() + labs(x="", y="") + 
            geom_abline(aes(color="True line", linetype="True line", slope=betas$vals[2], intercept=betas$vals[1]), size=1) + 
            geom_abline(aes(color="Linear fit (all)", linetype="Linear fit (all)", slope=coefs$vals[2], intercept=coefs$vals[1]), size=1) +
            scale_color_manual(name="", values=c("True line"="orange", "Linear fit (all)"="black")) + 
            scale_linetype_manual(name="", values=c("True line"=1, "Linear fit (all)"=2)) + 
            new_scale_color() + 
            geom_point(data=dat(), aes(x=x, y=y, shape="Observations", size="Observations", color="Observations"), alpha=input$transparency) + 
            scale_size_manual(name="", values=c("Observations"=1.5)) +
            scale_shape_manual(name="", values=c("Observations"=16)) +
            scale_color_manual(name="", values=c("Observations"="black")) +
            theme(legend.position="bottom", text=element_text(size=20)) + 
            guides(size=guide_legend(order=1), color=guide_legend(order=1), shape=guide_legend(order=1))
        }
      }
      
      else {
        if (input$toggle_selection) {
          if (sum(selected$vals)>=2) {
            ggplot() + theme_minimal() + labs(x="", y="") + 
              geom_abline(aes(color="True line", linetype="True line", slope=betas$vals[2], intercept=betas$vals[1]), size=1) + 
              geom_abline(aes(color="Linear fit (all)", linetype="Linear fit (all)", slope=coefs$vals[2], intercept=coefs$vals[1]), size=1) +
              geom_abline(aes(color="Linear fit (selected)", linetype="Linear fit (selected)", slope=sel_coefs$vals[2], intercept=sel_coefs$vals[1]), size=1) +
              scale_color_manual(name="", values=c("True line"="orange", "Linear fit (all)"="black", "Linear fit (selected)"="red")) + 
              scale_linetype_manual(name="", values=c("True line"=1, "Linear fit (all)"=2, "Linear fit (selected)"=2)) +
              geom_point(data=dat(), aes(x=x, y=y), alpha=0) +
              theme(legend.position="bottom", text=element_text(size=20))
          }
          else {
            ggplot() + theme_minimal() + labs(x="", y="") + 
              geom_abline(aes(color="True line", linetype="True line", slope=betas$vals[2], intercept=betas$vals[1]), size=1) + 
              geom_abline(aes(color="Linear fit (all)", linetype="Linear fit (all)", slope=coefs$vals[2], intercept=coefs$vals[1]), size=1) +
              scale_color_manual(name="", values=c("True line"="orange", "Linear fit (all)"="black")) + 
              scale_linetype_manual(name="", values=c("True line"=1, "Linear fit (all)"=2)) + 
              geom_point(data=dat(), aes(x=x, y=y), alpha=0) +
              theme(legend.position="bottom", text=element_text(size=20))
          }
        }
        else {
          ggplot() + theme_minimal() + labs(x="", y="") + 
            geom_abline(aes(color="True line", linetype="True line", slope=betas$vals[2], intercept=betas$vals[1]), size=1) + 
            geom_abline(aes(color="Linear fit (all)", linetype="Linear fit (all)", slope=coefs$vals[2], intercept=coefs$vals[1]), size=1) +
            scale_color_manual(name="", values=c("True line"="orange", "Linear fit (all)"="black")) + 
            scale_linetype_manual(name="", values=c("True line"=1, "Linear fit (all)"=2)) + 
            geom_point(data=dat(), aes(x=x, y=y), alpha=0) +
            theme(legend.position="bottom", text=element_text(size=20))
        }
      }
    } else {
      ggplot()
    }
  })
  
  
  observe({
    round.choose <- function(x, roundTo, dir=1) {
      if(dir == 1) {  ##ROUND UP
        x + (roundTo - x %% roundTo)
      } else {
        if(dir == 0) {  ##ROUND DOWN
          x - (x %% roundTo)
        }
      }
    }
    y_upper <- round.choose(max(dat()$y), 1, 1)
    y_lower <- round.choose(min(dat()$y), 1, 0)
    x_upper <- round.choose(max(dat()$x), 1, 1)
    x_lower <- round.choose(min(dat()$x), 1, 0)
    updateSliderInput(inputId="yrange", min=y_lower, max=y_upper, value=c(y_lower, y_upper))
    updateSliderInput(inputId="xrange", min=x_lower, max=x_upper, value=c(x_lower, x_upper))
  })
  
  output$scatterPlot <- renderPlot({plot()})
  
  output$estimates_all <- renderTable({
    if (data_generated()) {
      res <- cbind(round(coefs$vals,2), round(coefs$se,2), round(coefs$lower,2), round(coefs$upper,2))
      res <- cbind(c("Intercept", "Slope"), res)
      colnames(res) <- c(" ", "Estimate", "SE", "Lower", "Upper")
      res
    }
    else {
      NULL
    }
  })
  
  output$estimates_sel <- renderTable({
    if (data_generated())
      if (input$toggle_selection) {
        if (sum(selected$vals) >= 2) {
          res <- cbind(round(sel_coefs$vals,2), round(sel_coefs$se,2), round(sel_coefs$lower,2), round(sel_coefs$upper,2))
          res <- cbind(c("Intercept", "Slope"), res)
          colnames(res) <- c(" ", "Estimate", "SE", "Lower", "Upper")
          res
        }
      }
    else {
      NULL
    }
  })
}