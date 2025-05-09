# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(here)
library(rsconnect)

data <- read.csv("epi_jul_mar_25.csv")

# Convert PERIOD to Date format
data$PERIOD <- as.Date(paste0("01-", data$PERIOD), format="%d-%b-%y")

# Define UI for the application
ui <- fluidPage(
  titlePanel("Mukono District EPI Dashboard by Facility and Subcounty"),
  
  # Custom CSS for the coverage boxes
  tags$style(HTML("
    .coverage-box {
      background-color: #d9edf7;  /* Light blue background */
      padding: 10px;
      border-radius: 5px;
      margin-bottom: 10px;
      text-align: center;
      font-weight: bold;
      color: #31708f;  /* Darker blue text */
    }
  ")),
  
  # Subcounty and Facility Selectors at the top
  fluidRow(
    column(6, selectInput("subcounty", "Select Subcounty:", 
                          choices = unique(data$SUBCOUNTY))),
    column(6, selectInput("facility", "Select Health Facility:", 
                          choices = NULL))
  ),
  
  # Graphs Layout with coverage boxes above each plot
  fluidRow(
    column(6, div(textOutput("polio_bcg_coverage"), class = "coverage-box"), plotOutput("polio_bcg_plot")),
    column(6, div(textOutput("vita_coverage"), class = "coverage-box"), plotOutput("vita_plot"))
  ),
  fluidRow(
    column(6, div(textOutput("dew_coverage"), class = "coverage-box"), plotOutput("dew_plot")),
    column(6, div(textOutput("penta_coverage"), class = "coverage-box"), plotOutput("penta_plot"))
  ),
  fluidRow(
    column(6, div(textOutput("rota_coverage"), class = "coverage-box"), plotOutput("rota_plot")),
    column(6, div(textOutput("pcv_coverage"), class = "coverage-box"), plotOutput("pcv_plot"))
  ),
  fluidRow(
    column(6, div(textOutput("mr_coverage"), class = "coverage-box"), plotOutput("mr_plot")),
    column(6, div(textOutput("hpv_coverage"), class = "coverage-box"), plotOutput("hpv_plot"))
  ),
  fluidRow(
    column(6, div(textOutput("penta_dropout"), class = "coverage-box"), plotOutput("dpt_dropout_plot")),
    column(6, div(textOutput("MR1_dropout"), class = "coverage-box"), plotOutput("penta_mr_dropout_plot"))
  ),
  
  # New RED Categorization Table
  fluidRow(
    column(12, h3("RED Categorization for Child Immunization"), tableOutput("red_table"))
  ),
  fluidRow(
    column(12, h3("Summary Table: Number Vaccinated and Coverage by Antigen"), tableOutput("antigen_table"))
  )
)


# Define server logic 
server <- function(input, output, session) {
  
  # Update health facility choices based on selected subcounty
  observe({
    facilities <- unique(data$HEALTH.FACILITY[data$SUBCOUNTY == input$subcounty])
    updateSelectInput(session, "facility", choices = facilities)
  })
  
  # Filter the data based on the selected subcounty and facility
  filtered_data <- reactive({
    data %>%
      filter(SUBCOUNTY == input$subcounty & HEALTH.FACILITY == input$facility)
  })
  
  # Define the x-axis breaks
  x_breaks <- seq(from = as.Date("2024-07-01"), to = as.Date("2025-03-01"), by = "month")
  x_labels <- format(x_breaks, "%b-%y")
  
  # Calculate and display coverage for Polio0 and BCG
  output$polio_bcg_coverage <- renderText({
    df <- filtered_data()
    CumPolio0 <- sum(df$Polio0)
    CumBCG <- sum(df$BCG)
    CumHepB0 <- sum(df$HepB0)
    CumBCG_POLIO_TARGET <- sum(df$BCG_POLIO_TARGET)
    polio_coverage <- round((CumPolio0 / CumBCG_POLIO_TARGET) * 100, 1)
    bcg_coverage <- round((CumBCG / CumBCG_POLIO_TARGET) * 100, 1)
    HepB0_coverage <- round((CumHepB0 / CumBCG_POLIO_TARGET)* 100, 1)
    paste("Polio0 coverage:", polio_coverage, "% | BCG coverage:", bcg_coverage, "% | HepB0 coverage:", HepB0_coverage, "%")
  })
  
  # Calculate and display coverage for VitA1 and VitA2
  output$vita_coverage <- renderText({
    df <- filtered_data()
    CumVitA1 <- sum(df$VitA1)
    CumVitA2 <- sum(df$VitA2)
    CumVITA_TARGET <- sum(df$VITA_TARGET)
    vitA1_coverage <- round((CumVitA1 / CumVITA_TARGET) * 100, 1)
    vitA2_coverage <- round((CumVitA2 / CumVITA_TARGET) * 100, 1)
    paste("VitA1 coverage:", vitA1_coverage, "% | VitA2 coverage:", vitA2_coverage, "%")
  })
  
  # Calculate and display coverage for DEW1 and DEW2
  output$dew_coverage <- renderText({
    df <- filtered_data()
    CumDEW1 <- sum(df$DEW1)
    CumDEW2 <- sum(df$DEW2)
    CumDEW_TARGET <- sum(df$DEW_TARGET)
    dew1_coverage <- round((CumDEW1 / CumDEW_TARGET) * 100, 1)
    dew2_coverage <- round((CumDEW2 / CumDEW_TARGET) * 100, 1)
    paste("DEW1 coverage:", dew1_coverage, "% | DEW2 coverage:", dew2_coverage, "%")
  })
  
  # Calculate and display coverage for Penta1, Penta2, and Penta3
  output$penta_coverage <- renderText({
    df <- filtered_data()
    CumPenta1 <- sum(df$PENTA1)
    CumPenta2 <- sum(df$PENTA2)
    CumPenta3 <- sum(df$PENTA3)
    CumEPI_TARGET <- sum(df$EPI_TARGET)
    penta1_coverage <- round((CumPenta1 / CumEPI_TARGET) * 100, 1)
    penta2_coverage <- round((CumPenta2 / CumEPI_TARGET) * 100, 1)
    penta3_coverage <- round((CumPenta3 / CumEPI_TARGET) * 100, 1)
    paste("Penta1 coverage:", penta1_coverage, "% | Penta2 coverage:", penta2_coverage, "% | Penta3 coverage:", penta3_coverage, "%")
  })
  
  # Calculate and display coverage for Rota1 and Rota2
  output$rota_coverage <- renderText({
    df <- filtered_data()
    CumRota1 <- sum(df$Rota1)
    CumRota2 <- sum(df$Rota2)
    CumRota3 <- sum(df$Rota3)
    CumEPI_TARGET <- sum(df$EPI_TARGET)
    rota1_coverage <- round((CumRota1 / CumEPI_TARGET) * 100, 1)
    rota2_coverage <- round((CumRota2 / CumEPI_TARGET) * 100, 1)
    rota3_coverage <- round((CumRota3 / CumEPI_TARGET) * 100, 1)
    paste("Rota1 coverage:", rota1_coverage, "% | Rota2 coverage:", rota2_coverage, "% | Rota3 coverage:", rota3_coverage, "%")
  })
  
  # Calculate and display coverage for PCV1, PCV2, and PCV3
  output$pcv_coverage <- renderText({
    df <- filtered_data()
    CumPCV1 <- sum(df$PCV1)
    CumPCV2 <- sum(df$PCV2)
    CumPCV3 <- sum(df$PCV3)
    CumEPI_TARGET <- sum(df$EPI_TARGET)
    pcv1_coverage <- round((CumPCV1 / CumEPI_TARGET) * 100, 1)
    pcv2_coverage <- round((CumPCV2 / CumEPI_TARGET) * 100, 1)
    pcv3_coverage <- round((CumPCV3 / CumEPI_TARGET) * 100, 1)
    paste("PCV1 coverage:", pcv1_coverage, "% | PCV2 coverage:", pcv2_coverage, "% | PCV3 coverage:", pcv3_coverage, "%")
  })
  
  # Calculate and display coverage for MR1, MR2 and Fully Immunized
  output$mr_coverage <- renderText({
    df <- filtered_data()
    CumMR1 <- sum(df$MR1)
    CumMR2 <- sum(df$MR2)
    CumYF <- sum(df$YF)
    Cumfully_imm <- sum(df$Fully_imm)
    CumEPI_TARGET <- sum(df$EPI_TARGET)
    mr1_coverage <- round((CumMR1 / CumEPI_TARGET) * 100, 1)
    mr2_coverage <- round((CumMR2 / CumEPI_TARGET) * 100, 1)
    YF_coverage <- round((CumYF / CumEPI_TARGET) * 100, 1)
    fully_imm_coverage <- round((Cumfully_imm / CumEPI_TARGET) * 100, 1)
    paste("MR1 coverage:", mr1_coverage, "% | YF coverage:", YF_coverage, "% | MR2 coverage:", mr2_coverage, "% | Fully coverage:", fully_imm_coverage, "%")
  })
  
  # Calculate and display coverage for HPV1 and HPV2
  output$hpv_coverage <- renderText({
    df <- filtered_data()
    Cumhpv1 <- sum(df$HPV1.Dose.1)
    Cumhpv2 <- sum(df$HPV2.Dose.2)
    CumHPV_TARGET <- sum(df$HPV_TARGET)
    hpv1_coverage <- round((Cumhpv1 / CumHPV_TARGET) * 100, 1)
    hpv2_coverage <- round((Cumhpv2 / CumHPV_TARGET) * 100, 1)
    paste("HPV1 coverage:", hpv1_coverage, "% | HPV2 coverage:", hpv2_coverage, "%")
  })
  
  # Calculate and display dropout for Penta1 to Penta 3
  output$penta_dropout <- renderText({
    df <- filtered_data()
    CumPenta1 <- sum(df$PENTA1)
    CumPenta3 <- sum(df$PENTA3)
    penta1_3_dropout <- round(((CumPenta1 - CumPenta3) / CumPenta1) * 100, 1)
    paste("Penta1-Penta3 drop:", penta1_3_dropout, "%")
  })
  
  # Calculate and display dropout for Penta1 to MR1
  output$MR1_dropout <- renderText({
    df <- filtered_data()
    CumPenta1 <- sum(df$PENTA1)
    CumMR1 <- sum(df$MR1)
    penta1_MR1_dropout <- round(((CumPenta1 - CumMR1) / CumPenta1) * 100, 1)
    paste("Penta1-MR1 drop:", penta1_MR1_dropout, "%")
  })
  
  # Calculate and display RED Categorization Table
  output$red_table <- renderTable({
    df <- filtered_data()
    CumEPI_TARGET <- sum(df$EPI_TARGET)
    CumPenta1 <- sum(df$PENTA1)
    CumPenta3 <- sum(df$PENTA3)
    CumMR1 <- sum(df$MR1)
    
    unimmunized_zero_dose <- CumEPI_TARGET - CumPenta1
    unimmunized_penta3 <- CumEPI_TARGET - CumPenta3
    unimmunized_mr1 <- CumEPI_TARGET - CumMR1
    penta_dropout <- round(((CumPenta1 - CumPenta3) / CumPenta1) * 100, 1)
    mr1_dropout <- round(((CumPenta1 - CumMR1) / CumPenta1) * 100, 1)
    
    access <- ifelse((CumPenta1 / CumEPI_TARGET) * 100 > 90, "Good", "Poor")
    utilization <- ifelse(penta_dropout >= 0 & penta_dropout <= 10, "Good", "Poor")
    
    categorization <- ifelse(access == "Good" & utilization == "Good", "Cat1",
                             ifelse(access == "Good" & utilization == "Poor", "Cat2",
                                    ifelse(access == "Poor" & utilization == "Good", "Cat3", "Cat4")))
    
    red_table <- data.frame(
      Unimmunized_Zero_Dose = as.integer(unimmunized_zero_dose),
      Unimmunized_Penta3 = as.integer(unimmunized_penta3),
      Unimmunized_MR1 = as.integer(unimmunized_mr1),
      Penta_Dropout = paste0(as.integer(penta_dropout), "%"),
      MR1_Dropout = paste0(as.integer(mr1_dropout), "%"),
      Access = access,
      Utilization = utilization,
      Categorization = categorization
    )
    
    red_table
  }, striped = TRUE, bordered = TRUE, align = 'c')
  
  # Cumulative line graph for Polio0 and BCG
  output$polio_bcg_plot <- renderPlot({
    df <- filtered_data()
    df <- df %>%
      mutate(CumPolio0 = cumsum(Polio0),
             CumBCG = cumsum(BCG),
             CumHepB0 = cumsum(HepB0),
             CumBCG_POLIO_TARGET = cumsum(BCG_POLIO_TARGET))
    
    ggplot(df, aes(x = PERIOD)) +
      geom_line(aes(y = CumPolio0, color = "Polio0")) +
      geom_line(aes(y = CumBCG, color = "BCG")) +
      geom_line(aes(y = CumHepB0, color = "HepB0"))+
      geom_line(aes(y = CumBCG_POLIO_TARGET, color = "Target"), linetype="dashed") +
      scale_x_date(breaks = x_breaks, labels = x_labels) +
      labs(y = "Cumulative Count", x = "Period", title = "Cumulative Polio0 and BCG") +
      scale_color_manual(values = c("Polio0" = "blue", "HepB0" = "black","BCG" = "green", "Target" = "red")) +
      theme_minimal()
  })
  
  # Cumulative line graph for VitA1 and VitA2
  output$vita_plot <- renderPlot({
    df <- filtered_data()
    df <- df %>%
      mutate(CumVitA1 = cumsum(VitA1),
             CumVitA2 = cumsum(VitA2),
             CumVITA_TARGET = cumsum(VITA_TARGET))
    
    ggplot(df, aes(x = PERIOD)) +
      geom_line(aes(y = CumVitA1, color = "VitA1")) +
      geom_line(aes(y = CumVitA2, color = "VitA2")) +
      geom_line(aes(y = CumVITA_TARGET, color = "Target"), linetype="dashed") +
      scale_x_date(breaks = x_breaks, labels = x_labels) +
      labs(y = "Cumulative Count", x = "Period", title = "Cumulative VitA1 and VitA2") +
      scale_color_manual(values = c("VitA1" = "blue", "VitA2" = "green", "Target" = "red")) +
      theme_minimal()
  })
  
  # Cumulative line graph for DEW1 and DEW2
  output$dew_plot <- renderPlot({
    df <- filtered_data()
    df <- df %>%
      mutate(CumDEW1 = cumsum(DEW1),
             CumDEW2 = cumsum(DEW2),
             CumDEW_TARGET = cumsum(DEW_TARGET))
    
    ggplot(df, aes(x = PERIOD)) +
      geom_line(aes(y = CumDEW1, color = "DEW1")) +
      geom_line(aes(y = CumDEW2, color = "DEW2")) +
      geom_line(aes(y = CumDEW_TARGET, color = "Target"), linetype="dashed") +
      scale_x_date(breaks = x_breaks, labels = x_labels) +
      labs(y = "Cumulative Count", x = "Period", title = "Cumulative DEW1 and DEW2") +
      scale_color_manual(values = c("DEW1" = "blue", "DEW2" = "green", "Target" = "red")) +
      theme_minimal()
  })
  
  # Cumulative line graph for Penta1, Penta2, and Penta3
  output$penta_plot <- renderPlot({
    df <- filtered_data()
    df <- df %>%
      mutate(CumPenta1 = cumsum(PENTA1),
             CumPenta2 = cumsum(PENTA2),
             CumPenta3 = cumsum(PENTA3),
             CumEPI_TARGET = cumsum(EPI_TARGET))
    
    ggplot(df, aes(x = PERIOD)) +
      geom_line(aes(y = CumPenta1, color = "Penta1")) +
      geom_line(aes(y = CumPenta2, color = "Penta2")) +
      geom_line(aes(y = CumPenta3, color = "Penta3")) +
      geom_line(aes(y = CumEPI_TARGET, color = "Target"), linetype="dashed") +
      scale_x_date(breaks = x_breaks, labels = x_labels) +
      labs(y = "Cumulative Count", x = "Period", title = "Cumulative Penta1, Penta2, and Penta3") +
      scale_color_manual(values = c("Penta1" = "blue", "Penta2" = "green", "Penta3" = "orange", "Target" = "red")) +
      theme_minimal()
  })
  
  # Cumulative line graph for Rota1 and Rota2
  output$rota_plot <- renderPlot({
    df <- filtered_data()
    df <- df %>%
      mutate(CumRota1 = cumsum(Rota1),
             CumRota2 = cumsum(Rota2),
             CumRota3 = cumsum(Rota3),
             CumEPI_TARGET = cumsum(EPI_TARGET))
    
    ggplot(df, aes(x = PERIOD)) +
      geom_line(aes(y = CumRota1, color = "Rota1")) +
      geom_line(aes(y = CumRota2, color = "Rota2")) +
      geom_line(aes(y = CumRota3, color = "Rota3")) +
      geom_line(aes(y = CumEPI_TARGET, color = "Target"), linetype="dashed") +
      scale_x_date(breaks = x_breaks, labels = x_labels) +
      labs(y = "Cumulative Count", x = "Period", title = "Cumulative Rota1 and Rota2") +
      scale_color_manual(values = c("Rota1" = "blue", "Rota2" = "pink", "Rota3" = "black","Target" = "red")) +
      theme_minimal()
  })
  
  # Cumulative line graph for PCV1, PCV2, and PCV3
  output$pcv_plot <- renderPlot({
    df <- filtered_data()
    df <- df %>%
      mutate(CumPCV1 = cumsum(PCV1),
             CumPCV2 = cumsum(PCV2),
             CumPCV3 = cumsum(PCV3),
             CumEPI_TARGET = cumsum(EPI_TARGET))
    
    ggplot(df, aes(x = PERIOD)) +
      geom_line(aes(y = CumPCV1, color = "PCV1")) +
      geom_line(aes(y = CumPCV2, color = "PCV2")) +
      geom_line(aes(y = CumPCV3, color = "PCV3")) +
      geom_line(aes(y = CumEPI_TARGET, color = "Target"), linetype="dashed") +
      scale_x_date(breaks = x_breaks, labels = x_labels) +
      labs(y = "Cumulative Count", x = "Period", title = "Cumulative PCV1, PCV2, and PCV3") +
      scale_color_manual(values = c("PCV1" = "blue", "PCV2" = "pink", "PCV3" = "orange", "Target" = "red")) +
      theme_minimal()
  })
  
  # Cumulative line graph for MR1, MR2 and Fully immunized
  output$mr_plot <- renderPlot({
    df <- filtered_data()
    df <- df %>%
      mutate(CumMR1 = cumsum(MR1),
             CumMR2 = cumsum(MR2),
             CumYF = cumsum(YF),
             cumfully_imm = cumsum(Fully_imm),
             CumEPI_TARGET = cumsum(EPI_TARGET))
    
    ggplot(df, aes(x = PERIOD)) +
      geom_line(aes(y = CumMR1, color = "MR1")) +
      geom_line(aes(y = CumYF, color = "YF")) +
      geom_line(aes(y = CumMR2, color = "MR2")) +
      geom_line(aes(y = cumfully_imm, color = "Fully_imm")) +
      geom_line(aes(y = CumEPI_TARGET, color = "Target"), linetype="dashed") +
      scale_x_date(breaks = x_breaks, labels = x_labels) +
      labs(y = "Cumulative Count", x = "Period", title = "Cumulative MR1 and MR2") +
      scale_color_manual(values = c("MR1" = "green", "YF" = "black", "MR2" = "blue", "Fully_imm" = "pink", "Target" = "red")) +
      theme_minimal()
  })
  
  # Cumulative line graph for HPV1 and HPV2
  output$hpv_plot <- renderPlot({
    df <- filtered_data()
    df <- df %>%
      mutate(Cumhpv1 = cumsum(HPV1.Dose.1),
             Cumhpv2 = cumsum(HPV2.Dose.2),
             CumHPV_TARGET = cumsum(HPV_TARGET))
    
    ggplot(df, aes(x = PERIOD)) +
      geom_line(aes(y = Cumhpv1, color = "hpv1")) +
      geom_line(aes(y = Cumhpv2, color = "hpv2")) +
      geom_line(aes(y = CumHPV_TARGET, color = "Target"), linetype="dashed") +
      scale_x_date(breaks = x_breaks, labels = x_labels) +
      labs(y = "Cumulative Count", x = "Period", title = "Cumulative HPV1 and HPV2") +
      scale_color_manual(values = c("hpv1" = "blue", "hpv2" = "pink", "Target" = "red")) +
      theme_minimal()
  })
  # DPT1-DPT3 Dropout Rate and Target Histogram
  output$dpt_dropout_plot <- renderPlot({
    df <- filtered_data()
    df <- df %>%
      mutate(CumPenta1 = cumsum(PENTA1),
             CumPenta3 = cumsum(PENTA3),
             DropoutRate = ((CumPenta1 - CumPenta3) / CumPenta1) * 100)
    
    ggplot(df, aes(x = PERIOD)) +
      geom_line(aes(y = DropoutRate, color = "Dropout Rate")) +
      geom_bar(stat = "identity", aes(y = 10), fill = "red", alpha = 0.3, width = 30) +
      scale_x_date(breaks = x_breaks, labels = x_labels) +
      labs(y = "Dropout Rate (%)", x = "Period", title = "DPT1-DPT3 Dropout Rate") +
      scale_color_manual(values = c("Dropout Rate" = "blue")) +
      theme_minimal()
  })
  
  # PENTA1-MR1 Dropout Rate and Target Histogram
  output$penta_mr_dropout_plot <- renderPlot({
    df <- filtered_data()
    df <- df %>%
      mutate(CumPenta1 = cumsum(PENTA1),
             CumMR1 = cumsum(MR1),
             DropoutRate = ((CumPenta1 - CumMR1) / CumPenta1) * 100)
    
    ggplot(df, aes(x = PERIOD)) +
      geom_line(aes(y = DropoutRate, color = "Dropout Rate")) +
      geom_bar(stat = "identity", aes(y = 10), fill = "red", alpha = 0.3, width = 30) +
      scale_x_date(breaks = x_breaks, labels = x_labels) +
      labs(y = "Dropout Rate (%)", x = "Period", title = "PENTA1-MR1 Dropout Rate") +
      scale_color_manual(values = c("Dropout Rate" = "blue")) +
      theme_minimal()
  })
  output$antigen_table <- renderTable({
    df <- filtered_data()
    epi_target <- sum(df$EPI_TARGET, na.rm = TRUE)
    vita_target <- sum(df$VITA_TARGET, na.rm = TRUE)
    dew_target <- sum(df$DEW_TARGET, na.rm = TRUE)
    bcg_polio_target <- sum(df$BCG_POLIO_TARGET, na.rm = TRUE)
    hpv_target <- sum(df$HPV_TARGET, na.rm = TRUE)
    
    antigen_summary <- data.frame(
      Antigen = c("BCG", "Polio0", "HepB0", 
                  "Penta1", "Penta2", "Penta3", 
                  "Rota1", "Rota2", "Rota3",
                  "PCV1", "PCV2", "PCV3",
                  "MR1", "YF", "Fully Immunized", 
                  "MR2", "HPV1", "HPV2",
                  "Penta1-Penta3 Dropout", "Penta1-MR1 Dropout",
                  "VitA1", "VitA2", "Dew1", "Dew2"),
      Number_Vaccinated = c(
        sum(df$BCG, na.rm = TRUE),
        sum(df$Polio0, na.rm = TRUE),
        sum(df$HepB0, na.rm = TRUE),
        sum(df$PENTA1, na.rm = TRUE),
        sum(df$PENTA2, na.rm = TRUE),
        sum(df$PENTA3, na.rm = TRUE),
        sum(df$Rota1, na.rm = TRUE),
        sum(df$Rota2, na.rm = TRUE),
        sum(df$Rota3, na.rm = TRUE),
        sum(df$PCV1, na.rm = TRUE),
        sum(df$PCV2, na.rm = TRUE),
        sum(df$PCV3, na.rm = TRUE),
        sum(df$MR1, na.rm = TRUE),
        sum(df$YF, na.rm = TRUE),
        sum(df$Fully_imm, na.rm = TRUE),
        sum(df$MR2, na.rm = TRUE),
        sum(df$HPV1.Dose.1, na.rm = TRUE),
        sum(df$HPV2.Dose.2, na.rm = TRUE),
        NA, NA,  # Dropouts calculated below
        sum(df$VitA1, na.rm = TRUE),
        sum(df$VitA2, na.rm = TRUE),
        sum(df$DEW1, na.rm = TRUE),
        sum(df$DEW2, na.rm = TRUE)
      ),
      Coverage_Percent = c(
        round(sum(df$BCG, na.rm = TRUE) / bcg_polio_target * 100, 1),
        round(sum(df$Polio0, na.rm = TRUE) / bcg_polio_target * 100, 1),
        round(sum(df$HepB0, na.rm = TRUE) / bcg_polio_target * 100, 1),
        round(sum(df$PENTA1, na.rm = TRUE) / epi_target * 100, 1),
        round(sum(df$PENTA2, na.rm = TRUE) / epi_target * 100, 1),
        round(sum(df$PENTA3, na.rm = TRUE) / epi_target * 100, 1),
        round(sum(df$Rota1, na.rm = TRUE) / epi_target * 100, 1),
        round(sum(df$Rota2, na.rm = TRUE) / epi_target * 100, 1),
        round(sum(df$Rota3, na.rm = TRUE) / epi_target * 100, 1),
        round(sum(df$PCV1, na.rm = TRUE) / epi_target * 100, 1),
        round(sum(df$PCV2, na.rm = TRUE) / epi_target * 100, 1),
        round(sum(df$PCV3, na.rm = TRUE) / epi_target * 100, 1),
        round(sum(df$MR1, na.rm = TRUE) / epi_target * 100, 1),
        round(sum(df$YF, na.rm = TRUE) / epi_target * 100, 1),
        round(sum(df$Fully_imm, na.rm = TRUE) / epi_target * 100, 1),
        round(sum(df$MR2, na.rm = TRUE) / epi_target * 100, 1),
        round(sum(df$HPV1.Dose.1, na.rm = TRUE) / hpv_target * 100, 1),
        round(sum(df$HPV2.Dose.2, na.rm = TRUE) / hpv_target * 100, 1),
        round((sum(df$PENTA1, na.rm = TRUE) - sum(df$PENTA3, na.rm = TRUE)) / sum(df$PENTA1, na.rm = TRUE) * 100, 1),
        round((sum(df$PENTA1, na.rm = TRUE) - sum(df$MR1, na.rm = TRUE)) / sum(df$PENTA1, na.rm = TRUE) * 100, 1),
        round(sum(df$VitA1, na.rm = TRUE) / vita_target * 100, 1),
        round(sum(df$VitA2, na.rm = TRUE) / vita_target * 100, 1),
        round(sum(df$DEW1, na.rm = TRUE) / dew_target * 100, 1),
        round(sum(df$DEW2, na.rm = TRUE) / dew_target * 100, 1)
      )
    )
    
    antigen_summary
  }, striped = TRUE, bordered = TRUE, align = 'c')
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)

