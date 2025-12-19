library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(bslib)
library(grid)

path <- "Medical_Device_Manufacturing_Dataset.csv"

# SPC constants for subgroup size n (2–10)
K <- list(
  A2 = c(`2`=1.880, `3`=1.023, `4`=0.729, `5`=0.577, `6`=0.483, `7`=0.419, `8`=0.373, `9`=0.337, `10`=0.308),
  D3 = c(`2`=0.000, `3`=0.000, `4`=0.000, `5`=0.000, `6`=0.000, `7`=0.076, `8`=0.136, `9`=0.184, `10`=0.223),
  D4 = c(`2`=3.267, `3`=2.574, `4`=2.282, `5`=2.114, `6`=2.004, `7`=1.924, `8`=1.864, `9`=1.816, `10`=1.777),
  A3 = c(`2`=2.659, `3`=1.954, `4`=1.628, `5`=1.427, `6`=1.287, `7`=1.182, `8`=1.099, `9`=1.032, `10`=0.975),
  B3 = c(`2`=0.000, `3`=0.000, `4`=0.000, `5`=0.000, `6`=0.030, `7`=0.118, `8`=0.185, `9`=0.239, `10`=0.284),
  B4 = c(`2`=3.267, `3`=2.568, `4`=2.266, `5`=2.089, `6`=1.970, `7`=1.882, `8`=1.815, `9`=1.761, `10`=1.716)
)

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("SPC Charts (Control = Black Dotted, Spec = Red Dotted)"),
  sidebarLayout(
    sidebarPanel(
      tags$b("Dataset:"),
      tags$code(path),
      hr(),
      selectInput("chart", "Chart type",
                  choices = c("p", "np", "c", "u", "Xbar-R", "Xbar-S", "I-MR"),
                  selected = "p"),
      uiOutput("selectors"),
      hr(),
      numericInput("lsl", "LSL (red dotted)", value = NA_real_, step = 0.01),
      numericInput("usl", "USL (red dotted)", value = NA_real_, step = 0.01),
      tags$small("Control limits are always black dotted. Spec limits are always red dotted.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Chart", plotOutput("spc", height = "650px")),
        tabPanel("Messages", verbatimTextOutput("msg")),
        tabPanel("Preview", tableOutput("preview"))
      )
    )
  )
)

server <- function(input, output, session) {
  
 
  df <- reactive({
    validate(need(file.exists(path),
                  paste("CSV not found at:", path)))
    d <- readr::read_csv(path, show_col_types = FALSE)
    
       if ("Timestamp" %in% names(d)) {     # Parsing Timestamp
      ts <- d$Timestamp
      d$Timestamp <- as.POSIXct(ts, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
      bad <- is.na(d$Timestamp)
      if (any(bad)) {
        d$Timestamp[bad] <- as.POSIXct(ts[bad], format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
      }
    }
    d
  })
  
  output$preview <- renderTable({
    head(df(), 10)
  })
  
   output$msg <- renderText({
    d <- df()
    msgs <- c()
    
    if (!"Timestamp" %in% names(d)) {
      msgs <- c(msgs, "No 'Timestamp' column found. You can still use row order as time.")
    } else if (any(is.na(d$Timestamp))) {
      msgs <- c(msgs, "Some timestamps could not be parsed (NA). Row order will be used as time.")
    } else {
      msgs <- c(msgs, "'Timestamp' parsed correctly.")
    }
    
    if ("Record ID" %in% names(d)) {
      if (any(is.na(d[["Record ID"]]))) msgs <- c(msgs, "Record ID: missing values found.")
      if (any(duplicated(d[["Record ID"]]))) msgs <- c(msgs, "Record ID: duplicates found (should be unique).")
    }
    
    if (length(msgs) == 0) "No major issues found." else paste(msgs, collapse = "\n")
  })
  
 
  output$selectors <- renderUI({
    d <- df()
    cols <- names(d)
    num_cols <- cols[sapply(d, function(x) is.numeric(x) || is.integer(x))]
    
    tagList(
      selectInput("time_col", "Time / order column", cols,
                  selected = if ("Timestamp" %in% cols) "Timestamp" else cols[1]),
      
     
      conditionalPanel(
        condition = "input.chart == 'p' || input.chart == 'np' || input.chart == 'c' || input.chart == 'u'",
        selectInput("batch_col", "Batch / group column", cols,
                    selected = if ("Batch ID" %in% cols) "Batch ID" else cols[1]),
        conditionalPanel(
          condition = "input.chart == 'p' || input.chart == 'np'",
          selectInput("n_col", "n inspected (numeric)", num_cols,
                      selected = if ("n Inspected" %in% num_cols) "n Inspected" else num_cols[1]),
          selectInput("np_col", "defectives (np) (numeric)", num_cols,
                      selected = if ("Defectives (np)" %in% num_cols) "Defectives (np)" else num_cols[1])
        ),
        conditionalPanel(
          condition = "input.chart == 'c' || input.chart == 'u'",
          selectInput("c_col", "defects (c) (numeric)", num_cols,
                      selected = if ("Defects (c)" %in% num_cols) "Defects (c)" else num_cols[1])
        ),
        conditionalPanel(
          condition = "input.chart == 'u'",
          selectInput("opp_col", "opportunities (numeric)", num_cols,
                      selected = if ("Opportunities" %in% num_cols) "Opportunities" else num_cols[1])
        )
      ),
      
    
      conditionalPanel(
        condition = "input.chart == 'Xbar-R' || input.chart == 'Xbar-S'",
        selectInput("subgroup_col", "Subgroup column", cols,
                    selected = if ("Subgroup ID" %in% cols) "Subgroup ID" else cols[1]),
        selectInput("x_col", "Measurement (numeric)", num_cols,
                    selected = if ("RTD Temperature (C)" %in% num_cols) "RTD Temperature (C)" else num_cols[1])
      ),
      
      conditionalPanel(
        condition = "input.chart == 'I-MR'",
        selectInput("i_col", "Measurement (numeric)", num_cols,
                    selected = if ("RTD Temperature (C)" %in% num_cols) "RTD Temperature (C)" else num_cols[1])
      )
    )
  })
  

  output$spc <- renderPlot({
    d <- df()
    
    validate(need(input$time_col %in% names(d), "Pick a valid time/order column."))
    tvec <- d[[input$time_col]]
    
    # Use timestamp if good; else row index
    use_time <- inherits(tvec, "POSIXct") && !all(is.na(tvec))
    d$time_x <- if (use_time) tvec else seq_len(nrow(d))
    
    usl <- input$usl
    lsl <- input$lsl
    
  
    if (input$chart %in% c("p", "np", "c", "u")) {
      validate(need(input$batch_col %in% names(d), "Pick a valid batch/group column."))
      
      
      g <- d %>%     # Summarise by batch
        group_by(.data[[input$batch_col]]) %>%
        summarise(
          x = min(time_x, na.rm = TRUE),
          n  = if (!is.null(input$n_col))  sum(as.numeric(.data[[input$n_col]]),  na.rm = TRUE) else NA_real_,
          np = if (!is.null(input$np_col)) sum(as.numeric(.data[[input$np_col]]), na.rm = TRUE) else NA_real_,
          c  = if (!is.null(input$c_col))  sum(as.numeric(.data[[input$c_col]]),  na.rm = TRUE) else NA_real_,
          opp= if (!is.null(input$opp_col))sum(as.numeric(.data[[input$opp_col]]),na.rm = TRUE) else NA_real_,
          .groups = "drop"
        ) %>%
        arrange(x)
      
      if (input$chart == "p") {
        validate(need(all(is.finite(g$n)) && sum(g$n, na.rm = TRUE) > 0,
                      "n inspected must be numeric and > 0."))
        validate(need(all(is.finite(g$np)), "defectives (np) must be numeric."))
        
        g <- g %>% mutate(y = np / n)
        pbar <- sum(g$np, na.rm = TRUE) / sum(g$n, na.rm = TRUE)
        ucl <- pmin(1, pbar + 3 * sqrt(pbar * (1 - pbar) / g$n))
        lcl <- pmax(0, pbar - 3 * sqrt(pbar * (1 - pbar) / g$n))
        
        p <- ggplot(g, aes(x, y)) +
          geom_line() +
          geom_point(shape = 16, size = 1.4) +
          geom_hline(yintercept = pbar, linewidth = 0.9) +
          geom_line(aes(y = ucl), linetype = "dotted", color = "black", linewidth = 0.8) +
          geom_line(aes(y = lcl), linetype = "dotted", color = "black", linewidth = 0.8) +
          geom_hline(yintercept = usl, linetype = "dotted", color = "red", linewidth = 0.8, na.rm = TRUE) +
          geom_hline(yintercept = lsl, linetype = "dotted", color = "red", linewidth = 0.8, na.rm = TRUE) +
          labs(title = "p Chart", x = if (use_time) "Time" else "Order", y = "Fraction defective (p)") +
          theme_minimal()
        print(p)
        return(invisible())
        
      } else if (input$chart == "np") {
        validate(need(all(is.finite(g$n)) && mean(g$n, na.rm = TRUE) > 0,
                      "n inspected must be numeric and > 0."))
        validate(need(all(is.finite(g$np)), "defectives (np) must be numeric."))
        
        y <- g$np
        pbar <- sum(g$np, na.rm = TRUE) / sum(g$n, na.rm = TRUE)
        nbar <- mean(g$n, na.rm = TRUE)
        center <- nbar * pbar
        sigma <- sqrt(nbar * pbar * (1 - pbar))
        ucl <- center + 3 * sigma
        lcl <- pmax(0, center - 3 * sigma)
        
        p <- ggplot(g, aes(x, y)) +
          geom_line() +
          geom_point(shape = 16, size = 1.4) +
          geom_hline(yintercept = center, linewidth = 0.9) +
          geom_hline(yintercept = ucl, linetype = "dotted", color = "black", linewidth = 0.8) +
          geom_hline(yintercept = lcl, linetype = "dotted", color = "black", linewidth = 0.8) +
          geom_hline(yintercept = usl, linetype = "dotted", color = "red", linewidth = 0.8, na.rm = TRUE) +
          geom_hline(yintercept = lsl, linetype = "dotted", color = "red", linewidth = 0.8, na.rm = TRUE) +
          labs(title = "np Chart", x = if (use_time) "Time" else "Order", y = "Number defective (np)") +
          theme_minimal()
        print(p)
        return(invisible())
        
      } else if (input$chart == "c") {
        validate(need(all(is.finite(g$c)), "defects (c) must be numeric."))
        
        y <- g$c
        center <- mean(y, na.rm = TRUE)
        ucl <- center + 3 * sqrt(center)
        lcl <- pmax(0, center - 3 * sqrt(center))
        
        p <- ggplot(g, aes(x, y)) +
          geom_line() +
          geom_point(shape = 16, size = 1.4) +
          geom_hline(yintercept = center, linewidth = 0.9) +
          geom_hline(yintercept = ucl, linetype = "dotted", color = "black", linewidth = 0.8) +
          geom_hline(yintercept = lcl, linetype = "dotted", color = "black", linewidth = 0.8) +
          geom_hline(yintercept = usl, linetype = "dotted", color = "red", linewidth = 0.8, na.rm = TRUE) +
          geom_hline(yintercept = lsl, linetype = "dotted", color = "red", linewidth = 0.8, na.rm = TRUE) +
          labs(title = "c Chart", x = if (use_time) "Time" else "Order", y = "Defects (c)") +
          theme_minimal()
        print(p)
        return(invisible())
        
      } else {  # u chart
        validate(need(all(is.finite(g$c)), "defects (c) must be numeric."))
        validate(need(all(is.finite(g$opp)) && sum(g$opp, na.rm = TRUE) > 0,
                      "opportunities must be numeric and > 0."))
        
        y <- g$c / g$opp
        ubar <- sum(g$c, na.rm = TRUE) / sum(g$opp, na.rm = TRUE)
        ucl <- ubar + 3 * sqrt(ubar / g$opp)
        lcl <- pmax(0, ubar - 3 * sqrt(ubar / g$opp))
        
        p <- ggplot(g, aes(x, y)) +
          geom_line() +
          geom_point(shape = 16, size = 1.4) +
          geom_hline(yintercept = ubar, linewidth = 0.9) +
          geom_line(aes(y = ucl), linetype = "dotted", color = "black", linewidth = 0.8) +
          geom_line(aes(y = lcl), linetype = "dotted", color = "black", linewidth = 0.8) +
          geom_hline(yintercept = usl, linetype = "dotted", color = "red", linewidth = 0.8, na.rm = TRUE) +
          geom_hline(yintercept = lsl, linetype = "dotted", color = "red", linewidth = 0.8, na.rm = TRUE) +
          labs(title = "u Chart", x = if (use_time) "Time" else "Order", y = "Defects per opportunity (u)") +
          theme_minimal()
        print(p)
        return(invisible())
      }
    }
    
   
    if (input$chart %in% c("Xbar-R", "Xbar-S")) {
      validate(need(input$subgroup_col %in% names(d), "Pick a valid subgroup column."))
      validate(need(input$x_col %in% names(d), "Pick a valid numeric measurement column."))  # Xbar-R / Xbar-S
      
      g <- d %>%
        group_by(.data[[input$subgroup_col]]) %>%
        summarise(
          x = min(time_x, na.rm = TRUE),
          n = sum(!is.na(.data[[input$x_col]])),
          xbar = mean(.data[[input$x_col]], na.rm = TRUE),
          r = max(.data[[input$x_col]], na.rm = TRUE) - min(.data[[input$x_col]], na.rm = TRUE),
          s = sd(.data[[input$x_col]], na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(x)
      
      validate(need(nrow(g) > 5, "Not enough subgroups to chart."))
      
      nsub <- as.character(round(median(g$n, na.rm = TRUE)))
      if (!(nsub %in% names(K$A2))) nsub <- "5"
      
      xbarbar <- mean(g$xbar, na.rm = TRUE)
      
      if (input$chart == "Xbar-R") {
        Rbar <- mean(g$r, na.rm = TRUE)
        x_ucl <- xbarbar + K$A2[[nsub]] * Rbar
        x_lcl <- xbarbar - K$A2[[nsub]] * Rbar
        r_ucl <- K$D4[[nsub]] * Rbar
        r_lcl <- K$D3[[nsub]] * Rbar
        
        p1 <- ggplot(g, aes(x, xbar)) +
          geom_line() +
          geom_point(shape = 16, size = 1.4) +
          geom_hline(yintercept = xbarbar, linewidth = 0.9) +
          geom_hline(yintercept = x_ucl, linetype = "dotted", color = "black", linewidth = 0.8) +
          geom_hline(yintercept = x_lcl, linetype = "dotted", color = "black", linewidth = 0.8) +
          geom_hline(yintercept = usl, linetype = "dotted", color = "red", linewidth = 0.8, na.rm = TRUE) +
          geom_hline(yintercept = lsl, linetype = "dotted", color = "red", linewidth = 0.8, na.rm = TRUE) +
          labs(title = paste0("X̄ Chart — ", input$x_col),
               x = if (use_time) "Time" else "Order", y = "X̄") +
          theme_minimal()
        
        p2 <- ggplot(g, aes(x, r)) +
          geom_line() +
          geom_point(shape = 16, size = 1.2) +
          geom_hline(yintercept = Rbar, linewidth = 0.9) +
          geom_hline(yintercept = r_ucl, linetype = "dotted", color = "black", linewidth = 0.8) +
          geom_hline(yintercept = r_lcl, linetype = "dotted", color = "black", linewidth = 0.8) +
          labs(title = "R Chart", x = if (use_time) "Time" else "Order", y = "R") +
          theme_minimal()
        
      } else {  # Xbar-S
        Sbar <- mean(g$s, na.rm = TRUE)
        x_ucl <- xbarbar + K$A3[[nsub]] * Sbar
        x_lcl <- xbarbar - K$A3[[nsub]] * Sbar
        s_ucl <- K$B4[[nsub]] * Sbar
        s_lcl <- K$B3[[nsub]] * Sbar
        
        p1 <- ggplot(g, aes(x, xbar)) +
          geom_line() +
          geom_point(shape = 16, size = 1.4) +
          geom_hline(yintercept = xbarbar, linewidth = 0.9) +
          geom_hline(yintercept = x_ucl, linetype = "dotted", color = "black", linewidth = 0.8) +
          geom_hline(yintercept = x_lcl, linetype = "dotted", color = "black", linewidth = 0.8) +
          geom_hline(yintercept = usl, linetype = "dotted", color = "red", linewidth = 0.8, na.rm = TRUE) +
          geom_hline(yintercept = lsl, linetype = "dotted", color = "red", linewidth = 0.8, na.rm = TRUE) +
          labs(title = paste0("X̄ Chart — ", input$x_col),
               x = if (use_time) "Time" else "Order", y = "X̄") +
          theme_minimal()
        
        p2 <- ggplot(g, aes(x, s)) +
          geom_line() +
          geom_point(shape = 16, size = 1.2) +
          geom_hline(yintercept = Sbar, linewidth = 0.9) +
          geom_hline(yintercept = s_ucl, linetype = "dotted", color = "black", linewidth = 0.8) +
          geom_hline(yintercept = s_lcl, linetype = "dotted", color = "black", linewidth = 0.8) +
          labs(title = "S Chart", x = if (use_time) "Time" else "Order", y = "S") +
          theme_minimal()
      }
      
      grid.newpage()
      lay <- grid.layout(2, 1, heights = unit(c(2, 1.3), "null"))
      pushViewport(viewport(layout = lay))
      print(p1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
      print(p2, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
      popViewport()
      return(invisible())
    }
    

    if (input$chart == "I-MR") {
      validate(need(input$i_col %in% names(d), "Pick a valid numeric measurement column."))  # I-MR
      
      xdat <- d %>%
        transmute(x = time_x, y = as.numeric(.data[[input$i_col]])) %>%
        arrange(x) %>%
        filter(is.finite(y))
      
      validate(need(nrow(xdat) > 10, "Not enough points to chart I-MR."))
      
      mr <- abs(xdat$y - dplyr::lag(xdat$y))
      mrbar <- mean(mr, na.rm = TRUE)
      sigma <- mrbar / 1.128
      center <- mean(xdat$y, na.rm = TRUE)
      ucl <- center + 3 * sigma
      lcl <- center - 3 * sigma
      
      p1 <- ggplot(xdat, aes(x, y)) +
        geom_line() +
        geom_point(shape = 16, size = 1.0) +
        geom_hline(yintercept = center, linewidth = 0.9) +
        geom_hline(yintercept = ucl, linetype = "dotted", color = "black", linewidth = 0.8) +
        geom_hline(yintercept = lcl, linetype = "dotted", color = "black", linewidth = 0.8) +
        geom_hline(yintercept = usl, linetype = "dotted", color = "red", linewidth = 0.8, na.rm = TRUE) +
        geom_hline(yintercept = lsl, linetype = "dotted", color = "red", linewidth = 0.8, na.rm = TRUE) +
        labs(title = paste0("I Chart — ", input$i_col),
             x = if (use_time) "Time" else "Order", y = "Value") +
        theme_minimal()
      
      mrd <- tibble::tibble(x = xdat$x, mr = mr)
      p2 <- ggplot(mrd, aes(x, mr)) +
        geom_line() +
        geom_point(shape = 16, size = 1.0) +
        geom_hline(yintercept = mrbar, linewidth = 0.9) +
        geom_hline(yintercept = 3.267 * mrbar, linetype = "dotted", color = "black", linewidth = 0.8) +
        geom_hline(yintercept = 0, linetype = "dotted", color = "black", linewidth = 0.8) +
        labs(title = "MR Chart", x = if (use_time) "Time" else "Order", y = "MR") +
        theme_minimal()
      
      grid.newpage()
      lay <- grid.layout(2, 1, heights = unit(c(2, 1.3), "null"))
      pushViewport(viewport(layout = lay))
      print(p1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
      print(p2, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
      popViewport()
      return(invisible())
    }
  })
}

shinyApp(ui, server)
