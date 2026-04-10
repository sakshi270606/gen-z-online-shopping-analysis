# Libraries
library(shiny)
library(shinydashboard)
library(readr)
library(plotly)
library(ggplot2)
library(dplyr)
library(stringr)
library(scales)

# Load data
setwd("G:/genz online shopping survey project")
customers_data <- read_csv(
  "Gen-Z Online Shopping Survey.csv",
  show_col_types = FALSE,
  na = c("", "NA", "NULL")
)

# Rename columns by position so special characters dont cause errors
colnames(customers_data) <- c(
  "Timestamp", "Username", "Age", "Gender", "City", "Platform",
  "Premium_Sub", "Orders_range", "Spending", "Avg_Bill", "Category",
  "Category_Spend_Month", "Last_Month_Spend", "Discount",
  "Shop_Frequency", "Repeat_Purchase", "Buy_Again",
  "Discount_Importance", "Review_Influence", "Social_Media",
  "Research_Time", "Purchase_Decision", "Switch_Reason", "Time_Saving"
)

# Map order ranges to numeric midpoints since the column has text ranges
orders_map <- c(
  "01-May"       = 3,
  "06-Oct"       = 8,
  "Nov-15"       = 13,
  "16 - 20"      = 18,
  "More than 20" = 25
)

# Map yearly spending ranges to numeric midpoints
spending_map <- c(
  "Below 5,000"      = 2500,
  "5,000 \u2013 10,000"  = 7500,
  "10,000 \u2013 20,000" = 15000,
  "20,000 \u2013 40,000" = 30000
)

# Map average bill ranges to numeric midpoints
bill_map <- c(
  "Below 500"          = 250,
  "500 \u2013 1,000"   = 750,
  "1,000 \u2013 2,500" = 1750,
  "2,500 \u2013 5,000" = 3750,
  "Above 5,000"        = 6000
)

# Map discount text values to numeric percentages
discount_map <- c(
  "No discount" = 0,
  "10%"         = 10,
  "20%"         = 20,
  "30% or more" = 30
)

# Clean and create all derived columns
customers_data <- customers_data %>%
  mutate(
    
    # Fix gender column to consistent values
    Gender = case_when(
      str_detect(tolower(Gender), "female") ~ "Female",
      str_detect(tolower(Gender), "male")   ~ "Male",
      TRUE ~ "Other"
    ),
    
    # Convert all text range columns to numbers using maps
    Orders_num   = orders_map[Orders_range],
    Spending_num = spending_map[Spending],
    Bill_num     = bill_map[Avg_Bill],
    Discount_num = discount_map[Discount],
    
    # Fill any NAs with median so charts dont break
    Orders_num   = ifelse(is.na(Orders_num),   median(orders_map,   na.rm = TRUE), Orders_num),
    Spending_num = ifelse(is.na(Spending_num), median(spending_map, na.rm = TRUE), Spending_num),
    Bill_num     = ifelse(is.na(Bill_num),     median(bill_map,     na.rm = TRUE), Bill_num),
    Discount_num = ifelse(is.na(Discount_num), 0, Discount_num),
    
    # Sales amount is bill multiplied by number of orders
    Sales_Amount = Bill_num * Orders_num,
    
    # Group purchase decision into Habit Better Discounts or Other
    Decision_Group = case_when(
      Purchase_Decision == "Habit"            ~ "Habit",
      Purchase_Decision == "Better Discounts" ~ "Better Discounts",
      TRUE                                    ~ "Other"
    ),
    
    # Segment customers by yearly spending into Low Medium High
    Spending_Segment = case_when(
      Spending_num < 7500  ~ "Low (Below 5K)",
      Spending_num < 20000 ~ "Medium (5K-20K)",
      TRUE                 ~ "High (Above 20K)"
    ),
    
    # Convert research time text to numeric for t test
    Research_Time_num = as.numeric(factor(Research_Time))
    
  ) %>%
  filter(!is.na(Sales_Amount))

# Poisson distribution setup using average orders as lambda
lambda     <- mean(customers_data$Orders_num, na.rm = TRUE)
poisson_df <- data.frame(x = 0:max(customers_data$Orders_num, na.rm = TRUE))
poisson_df$prob <- dpois(poisson_df$x, lambda)

# Theme color used across all charts
DARK_RED <- "#7B1C1C"

# Custom CSS for sidebar and box header styling
custom_css <- "
  .skin-blue .main-header .logo                { background-color: #7B1C1C; }
  .skin-blue .main-header .navbar              { background-color: #7B1C1C; }
  .skin-blue .main-sidebar                     { background-color: #1a1a2e; }
  .skin-blue .sidebar-menu > li.active > a,
  .skin-blue .sidebar-menu > li > a:hover      { background-color: #7B1C1C; border-left: 3px solid #fff; }
  .skin-blue .sidebar-menu > li > a            { color: #ccc; }
  .skin-blue .sidebar-menu > li.active > a     { color: #fff; }
  .box-header { background-color: #7B1C1C !important; color: #fff !important; border-radius: 4px 4px 0 0; }
  .box-title  { color: #fff !important; font-weight: bold; }
  body, .content-wrapper                       { background-color: #f4f4f4; }
  .section-title { text-align: center; font-size: 22px; font-weight: bold; color: #7B1C1C; margin: 15px 0 20px 0; }
"

# UI starts here
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = tags$span(icon("chart-line"), " GEN-Z ANALYTICS")),
  
  dashboardSidebar(
    tags$head(tags$style(HTML(custom_css))),
    sidebarMenu(
      menuItem("Dashboard Home",      tabName = "home",         icon = icon("home")),
      menuItem("Consumer Retention",  tabName = "retention",    icon = icon("star")),
      menuItem("Sales Analysis",      tabName = "sales",        icon = icon("line-chart")),
      menuItem("Consumer Behavior",   tabName = "behavior",     icon = icon("users")),
      menuItem("Purchase Influence",  tabName = "influence",    icon = icon("bullhorn")),
      menuItem("Market Segmentation", tabName = "segmentation", icon = icon("layer-group")),
      menuItem("Buying Decision",     tabName = "buying",       icon = icon("check-circle")),
      menuItem("Summary Analysis",    tabName = "summary",      icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # Dashboard home tab with value boxes and overview charts
      tabItem(tabName = "home",
              fluidRow(
                valueBoxOutput("vb_avg_bill",   width = 3),
                valueBoxOutput("vb_avg_spend",  width = 3),
                valueBoxOutput("vb_total_cust", width = 3),
                valueBoxOutput("vb_age_range",  width = 3)
              ),
              div(class = "section-title", "Gen-Z Shopping Analytics Overview"),
              fluidRow(
                box(title = "Gender Distribution",   width = 6, status = "danger", solidHeader = TRUE, plotOutput("gender_pie",      height = 300)),
                box(title = "Spending Distribution", width = 6, status = "danger", solidHeader = TRUE, plotOutput("spending_bar",    height = 300))
              ),
              fluidRow(
                box(title = "Product Categories", width = 6, status = "danger", solidHeader = TRUE, plotOutput("category_plot",   height = 300)),
                box(title = "Order Frequency",    width = 6, status = "danger", solidHeader = TRUE, plotOutput("order_freq_plot", height = 300))
              )
      ),
      
      # Consumer retention tab with bernoulli poisson and retention probability
      tabItem(tabName = "retention",
              fluidRow(
                box(title = "Bernoulli Distribution: Purchase Decision",
                    width = 12, status = "danger", solidHeader = TRUE,
                    plotOutput("bernoulli_plot", height = 300))
              ),
              fluidRow(
                box(title = "Customer Retention Probability",    width = 6, status = "danger", solidHeader = TRUE, plotlyOutput("retention_prob_plot", height = 300)),
                box(title = "Poisson Distribution of Purchases", width = 6, status = "danger", solidHeader = TRUE, plotOutput("poisson_plot",          height = 300))
              )
      ),
      
      # Sales analysis tab with scatter plot
      tabItem(tabName = "sales",
              fluidRow(
                box(title = "Sales Amount vs. Average Bill Per Order",
                    width = 12, status = "danger", solidHeader = TRUE,
                    plotOutput("scatter_sales", height = 450))
              )
      ),
      
      # Consumer behavior tab with summary stats and histogram
      tabItem(tabName = "behavior",
              fluidRow(
                box(title = "Summary Statistics: Average Bill per Order",
                    width = 12, status = "danger", solidHeader = TRUE,
                    verbatimTextOutput("summary_stats"))
              ),
              fluidRow(
                box(title = "Distribution of Average Bill per Order",
                    width = 12, status = "danger", solidHeader = TRUE,
                    plotOutput("avg_purchase_hist", height = 350))
              )
      ),
      
      # Purchase influence tab with chi square discount and anova
      tabItem(tabName = "influence",
              fluidRow(
                box(title = "Chi-Square Test: Social Media vs Purchase Decision",
                    width = 12, status = "danger", solidHeader = TRUE,
                    verbatimTextOutput("chi_square_result"))
              ),
              fluidRow(
                box(title = "Discount vs Sales Amount",                width = 6, status = "danger", solidHeader = TRUE, plotOutput("discount_scatter", height = 300)),
                box(title = "Sales Amount by Spending Groups (ANOVA)", width = 6, status = "danger", solidHeader = TRUE, plotOutput("anova_boxplot",    height = 300))
              )
      ),
      
      # Market segmentation tab with table and bar chart
      tabItem(tabName = "segmentation",
              fluidRow(
                box(title = "Customer Spending Segmentation",
                    width = 12, status = "danger", solidHeader = TRUE,
                    tableOutput("segmentation_table"))
              ),
              fluidRow(
                box(title = "Segmentation Distribution",
                    width = 12, status = "danger", solidHeader = TRUE,
                    plotOutput("segmentation_bar", height = 350))
              )
      ),
      
      # Buying decision tab with decision groups and t test
      tabItem(tabName = "buying",
              fluidRow(
                box(title = "Decision Group Distribution",     width = 6, status = "danger", solidHeader = TRUE, plotOutput("decision_bar",     height = 300)),
                box(title = "Research Time vs Decision Group", width = 6, status = "danger", solidHeader = TRUE, plotOutput("research_boxplot", height = 300))
              ),
              fluidRow(
                box(title = "T-Test Result",
                    width = 12, status = "danger", solidHeader = TRUE,
                    verbatimTextOutput("ttest_result"))
              )
      ),
      
      # Summary tab with full data overview
      tabItem(tabName = "summary",
              fluidRow(
                box(title = "Full Data Summary",
                    width = 12, status = "danger", solidHeader = TRUE,
                    verbatimTextOutput("full_summary"))
              )
      )
      
    )
  )
)

# Server logic starts here
server <- function(input, output, session) {
  
  # Value boxes at the top of dashboard home
  output$vb_avg_bill <- renderValueBox({
    valueBox(paste0("\u20B9", round(mean(customers_data$Bill_num, na.rm = TRUE), 0)),
             "Avg Bill Per Order", icon = icon("shopping-cart"), color = "purple")
  })
  
  output$vb_avg_spend <- renderValueBox({
    valueBox(paste0("\u20B9", round(mean(customers_data$Spending_num, na.rm = TRUE), 0)),
             "Avg Yearly Spend", icon = icon("rupee-sign"), color = "green")
  })
  
  output$vb_total_cust <- renderValueBox({
    valueBox(nrow(customers_data), "Total Customers", icon = icon("users"), color = "blue")
  })
  
  output$vb_age_range <- renderValueBox({
    valueBox(
      paste0(min(customers_data$Age, na.rm = TRUE), " - ", max(customers_data$Age, na.rm = TRUE)),
      "Age Range", icon = icon("id-card"), color = "orange"
    )
  })
  
  # Gender pie chart showing male vs female split
  output$gender_pie <- renderPlot({
    df <- customers_data %>%
      count(Gender) %>%
      mutate(label = paste0(substr(Gender, 1, 1), "\n", n))
    
    ggplot(df, aes(x = "", y = n, fill = Gender)) +
      geom_col(width = 1, color = "white") +
      coord_polar("y", start = 0) +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 5, fontface = "bold") +
      scale_fill_manual(values = c("Female" = "#F4978E", "Male" = "#72CBBF", "Other" = "#B8B8B8")) +
      theme_void()
  })
  
  # Spending bar chart showing how much customers spend yearly
  output$spending_bar <- renderPlot({
    df <- customers_data %>%
      count(Spending) %>%
      mutate(
        pct      = round(n / sum(n) * 100, 1),
        Spending = factor(Spending, levels = c(
          "Below 5,000", "5,000 \u2013 10,000",
          "10,000 \u2013 20,000", "20,000 \u2013 40,000"
        ))
      )
    
    ggplot(df, aes(x = Spending, y = n)) +
      geom_bar(stat = "identity", fill = DARK_RED) +
      geom_text(aes(label = paste0(pct, "%")), vjust = -0.4, size = 3.5) +
      labs(title = "Yearly Online Shopping Expenditure",
           x = "Spending Range (\u20B9)", y = "Number of Customers") +
      theme_minimal() +
      theme(
        plot.title  = element_text(hjust = 0.5, face = "bold", size = 12),
        axis.text.x = element_text(angle = 15, hjust = 1)
      )
  })
  
  # Product category bar chart showing most shopped categories
  output$category_plot <- renderPlot({
    df <- customers_data %>% count(Category, sort = TRUE)
    
    ggplot(df, aes(x = reorder(Category, n), y = n)) +
      geom_bar(stat = "identity", fill = DARK_RED) +
      coord_flip() +
      labs(x = "Category", y = "Count") +
      theme_minimal()
  })
  
  # Order frequency bar chart showing how often customers order per year
  output$order_freq_plot <- renderPlot({
    df <- customers_data %>%
      count(Orders_range) %>%
      mutate(Orders_range = factor(Orders_range,
                                   levels = c("01-May", "06-Oct", "Nov-15", "16 - 20", "More than 20")))
    
    ggplot(df, aes(x = Orders_range, y = n)) +
      geom_bar(stat = "identity", fill = "#4E9AF1") +
      labs(x = "Number of Orders per Year", y = "Count") +
      theme_minimal()
  })
  
  # Bernoulli chart showing repurchase vs no repurchase using Buy Again column
  output$bernoulli_plot <- renderPlot({
    df_bern <- data.frame(
      Outcome = factor(
        c("Failure (0)\nDid Not Repurchase", "Success (1)\nRepurchased"),
        levels = c("Failure (0)\nDid Not Repurchase", "Success (1)\nRepurchased")
      ),
      Count = c(
        sum(customers_data$Buy_Again != "Yes", na.rm = TRUE),
        sum(customers_data$Buy_Again == "Yes", na.rm = TRUE)
      )
    )
    
    ggplot(df_bern, aes(x = Outcome, y = Count, fill = Outcome)) +
      geom_bar(stat = "identity", width = 0.35) +
      scale_fill_manual(values = c(
        "Failure (0)\nDid Not Repurchase" = "#F4978E",
        "Success (1)\nRepurchased"        = "#90EE90"
      )) +
      labs(x = "", y = "Number of Customers") +
      theme_minimal() +
      theme(legend.position = "none", axis.text.x = element_text(size = 12))
  })
  
  # Customer retention probability using binomial distribution
  output$retention_prob_plot <- renderPlotly({
    n         <- nrow(customers_data)
    p_success <- mean(customers_data$Buy_Again == "Yes", na.rm = TRUE)
    probs     <- dbinom(0:(n - 1), size = n - 1, prob = p_success)
    df        <- data.frame(Index = 0:(n - 1), Probability = probs)
    
    plot_ly(df, x = ~Index, y = ~Probability, type = "scatter", mode = "lines+markers",
            line   = list(color = DARK_RED),
            marker = list(color = DARK_RED)) %>%
      layout(
        xaxis = list(title = "Customer Index"),
        yaxis = list(title = "Probability"),
        plot_bgcolor  = "white",
        paper_bgcolor = "white"
      )
  })
  
  # Poisson distribution of purchase frequency
  output$poisson_plot <- renderPlot({
    ggplot(poisson_df, aes(x = x, y = prob)) +
      geom_line(color  = DARK_RED, size = 1) +
      geom_point(color = DARK_RED, size = 2) +
      labs(x = "Number of Events (x)", y = "Poisson Prob") +
      theme_minimal()
  })
  
  # Scatter plot of sales amount vs average bill with regression line
  output$scatter_sales <- renderPlot({
    ggplot(customers_data, aes(x = Bill_num, y = Sales_Amount)) +
      geom_point(color = "#4E9AF1", size = 3) +
      geom_smooth(method = "lm", se = FALSE, color = DARK_RED, size = 1.2) +
      scale_x_continuous(labels = comma) +
      scale_y_continuous(labels = comma) +
      labs(x = "Avg Bill Per Order (\u20B9)", y = "Sales Amount (\u20B9)") +
      theme_minimal() +
      theme(axis.title = element_text(size = 13))
  })
  
  # Summary statistics mean median and mode for average bill
  output$summary_stats <- renderPrint({
    x        <- customers_data$Bill_num
    mode_val <- as.numeric(names(sort(table(x), decreasing = TRUE)[1]))
    
    cat("--- Central Tendency Measures ---\n")
    cat(sprintf("Mean (Average):   \u20B9%.2f\n", mean(x,   na.rm = TRUE)))
    cat(sprintf("Median (Middle):  \u20B9%s\n",   median(x, na.rm = TRUE)))
    cat(sprintf("Mode (Frequent):  \u20B9%s\n",   mode_val))
  })
  
  # Histogram with actual bill range labels on x axis so its easy to read
  output$avg_purchase_hist <- renderPlot({
    
    bill_labels <- c(
      "250"  = "Below 500",
      "750"  = "500 - 1,000",
      "1750" = "1,000 - 2,500",
      "3750" = "2,500 - 5,000",
      "6000" = "Above 5,000"
    )
    
    df <- customers_data %>%
      mutate(Bill_label = bill_labels[as.character(Bill_num)]) %>%
      count(Bill_num, Bill_label) %>%
      arrange(Bill_num)
    
    ggplot(df, aes(x = factor(Bill_label, levels = df$Bill_label), y = n)) +
      geom_bar(stat = "identity", fill = "#F4978E", color = "white", width = 0.7) +
      geom_text(aes(label = n), vjust = -0.4, size = 4, fontface = "bold") +
      labs(x = "Average Bill Per Order (\u20B9)", y = "Frequency") +
      theme_minimal() +
      theme(
        axis.text.x        = element_text(size = 10),
        axis.title         = element_text(size = 12),
        panel.grid.major.x = element_blank()
      )
  })
  
  # Chi square test to check if social media affects purchase decision
  output$chi_square_result <- renderPrint({
    tbl    <- table(customers_data$Social_Media, customers_data$Purchase_Decision)
    result <- chisq.test(tbl)
    interp <- ifelse(result$p.value < 0.05,
                     "Significant Relationship (Reject H0)",
                     "No Significant Relationship (Fail to Reject H0)")
    
    cat(sprintf("Chi-square Statistic: %.3f\n", result$statistic))
    cat(sprintf("P-value: %.4f\n",              result$p.value))
    cat(sprintf("Interpretation: %s\n",         interp))
  })
  
  # Discount scatter with line starting from origin and y axis from 0
  output$discount_scatter <- renderPlot({
    ggplot(customers_data, aes(x = Discount_num, y = Sales_Amount)) +
      geom_point(color = "#4E9AF1", size = 3, alpha = 0.7) +
      geom_smooth(
        method    = "lm",
        se        = FALSE,
        color     = "red",
        size      = 1,
        fullrange = TRUE
      ) +
      scale_x_continuous(
        breaks = c(0, 10, 20, 30),
        labels = c("0%", "10%", "20%", "30%+"),
        limits = c(0, 32),
        expand = expansion(mult = c(0, 0.02))
      ) +
      scale_y_continuous(
        labels = comma,
        limits = c(0, NA),
        expand = expansion(mult = c(0, 0.05))
      ) +
      coord_cartesian(xlim = c(0, 32), ylim = c(0, NA)) +
      labs(x = "Discount (%)", y = "Sales Amount (\u20B9)") +
      theme_minimal() +
      theme(
        axis.title       = element_text(size = 12),
        axis.text        = element_text(size = 10),
        panel.grid.minor = element_blank()
      )
  })
  
  # ANOVA boxplot with neat outlier styling and y axis from 0
  output$anova_boxplot <- renderPlot({
    df <- customers_data %>%
      mutate(Seg = factor(Spending_Segment,
                          levels = c("Low (Below 5K)", "Medium (5K-20K)", "High (Above 20K)")))
    
    ggplot(df, aes(x = Seg, y = Sales_Amount, fill = Seg)) +
      geom_boxplot(
        width         = 0.45,
        outlier.shape = 21,
        outlier.fill  = "white",
        outlier.color = "gray50",
        outlier.size  = 2,
        linewidth     = 0.6
      ) +
      scale_fill_manual(values = c(
        "Low (Below 5K)"   = "#ADD8E6",
        "Medium (5K-20K)"  = "#90EE90",
        "High (Above 20K)" = "#FFB6C1"
      )) +
      scale_y_continuous(
        labels = comma,
        limits = c(0, NA),
        expand = expansion(mult = c(0, 0.08))
      ) +
      scale_x_discrete(labels = c(
        "Low (Below 5K)"   = "Low\n(Below \u20B95K)",
        "Medium (5K-20K)"  = "Medium\n(\u20B95K-20K)",
        "High (Above 20K)" = "High\n(Above \u20B920K)"
      )) +
      labs(x = "Spending Groups", y = "Sales Amount (\u20B9)") +
      theme_minimal() +
      theme(
        legend.position  = "none",
        axis.text.x      = element_text(size = 11, lineheight = 1.3),
        axis.title       = element_text(size = 12),
        panel.grid.minor = element_blank()
      )
  })
  
  # Segmentation table with mean sd and cv for each spending group
  output$segmentation_table <- renderTable({
    customers_data %>%
      mutate(Segment = case_when(
        Spending_num < 7500  ~ "Low Spenders (Below 5K)",
        Spending_num < 20000 ~ "Medium Spenders (5K-20K)",
        TRUE                 ~ "High Spenders (Above 20K)"
      )) %>%
      group_by(Segment) %>%
      summarise(
        Count         = n(),
        Mean_Spending = round(mean(Spending_num, na.rm = TRUE), 2),
        SD            = round(sd(Spending_num,   na.rm = TRUE), 2),
        CV            = round(sd(Spending_num, na.rm = TRUE) / mean(Spending_num, na.rm = TRUE) * 100, 2),
        .groups = "drop"
      )
  }, striped = FALSE, bordered = TRUE, hover = TRUE)
  
  # Segmentation bar chart showing count of customers in each spending group
  output$segmentation_bar <- renderPlot({
    df <- customers_data %>%
      mutate(Segment = factor(case_when(
        Spending_num < 7500  ~ "Low (Below 5K)",
        Spending_num < 20000 ~ "Medium (5K-20K)",
        TRUE                 ~ "High (Above 20K)"
      ), levels = c("Low (Below 5K)", "Medium (5K-20K)", "High (Above 20K)"))) %>%
      count(Segment)
    
    ggplot(df, aes(x = Segment, y = n, fill = Segment)) +
      geom_bar(stat = "identity", width = 0.5) +
      scale_fill_manual(values = c(
        "Low (Below 5K)"   = "#F4978E",
        "Medium (5K-20K)"  = "#90EE90",
        "High (Above 20K)" = "#ADD8E6"
      )) +
      labs(title = "Customer Segmentation by Spending",
           x = "Segment", y = "Number of Customers") +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.title      = element_text(hjust = 0.5, face = "bold")
      )
  })
  
  # Bar chart showing distribution across decision groups
  output$decision_bar <- renderPlot({
    df <- customers_data %>%
      count(Decision_Group) %>%
      mutate(Decision_Group = factor(Decision_Group,
                                     levels = c("Habit", "Better Discounts", "Other")))
    
    ggplot(df, aes(x = Decision_Group, y = n, fill = Decision_Group)) +
      geom_bar(stat = "identity", width = 0.4) +
      scale_fill_manual(values = c(
        "Habit"            = "#90EE90",
        "Better Discounts" = "#ADD8E6",
        "Other"            = "#F4978E"
      )) +
      labs(x = "", y = "Count") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Research boxplot with proper y axis labels showing actual meaning of each level
  output$research_boxplot <- renderPlot({
    
    df <- customers_data %>%
      mutate(Decision_Label = case_when(
        Decision_Group == "Habit"            ~ "Habit",
        Decision_Group == "Better Discounts" ~ "Better Discounts",
        TRUE                                 ~ "Other\n(Wide Range / Easy Return)"
      ),
      Decision_Label = factor(Decision_Label,
                              levels = c("Habit", "Better Discounts", "Other\n(Wide Range / Easy Return)"))
      )
    
    research_levels <- levels(factor(customers_data$Research_Time))
    
    ggplot(df, aes(x = Decision_Label, y = Research_Time_num)) +
      geom_boxplot(
        fill          = "#f9f9f9",
        color         = DARK_RED,
        outlier.shape = 16,
        outlier.color = "gray60",
        outlier.size  = 2,
        width         = 0.4,
        linewidth     = 0.7
      ) +
      scale_y_continuous(
        breaks = seq_along(research_levels),
        labels = research_levels,
        limits = c(0.5, length(research_levels) + 0.5)
      ) +
      labs(
        x = "Decision Group",
        y = "What Matters More While Shopping"
      ) +
      theme_minimal() +
      theme(
        axis.text.x      = element_text(size = 10, lineheight = 1.3, color = "gray20"),
        axis.text.y      = element_text(size = 10, color = "gray20"),
        axis.title       = element_text(size = 11, face = "bold"),
        panel.grid.minor = element_blank()
      )
  })
  
  # T test comparing research time between habit and better discounts groups
  output$ttest_result <- renderPrint({
    g1 <- customers_data %>% filter(Decision_Group == "Habit")            %>% pull(Research_Time_num)
    g2 <- customers_data %>% filter(Decision_Group == "Better Discounts") %>% pull(Research_Time_num)
    
    if (length(g1) >= 2 && length(g2) >= 2) {
      res <- t.test(g1, g2, var.equal = FALSE)
      cat(sprintf("t-value: %.2f\n", res$statistic))
      cat(sprintf("P-value: %.4f\n", res$p.value))
    } else {
      g_other <- customers_data %>% filter(Decision_Group == "Other") %>% pull(Research_Time_num)
      if (length(g1) >= 2 && length(g_other) >= 2) {
        res <- t.test(g1, g_other, var.equal = FALSE)
        cat(sprintf("t-value: %.2f\n", res$statistic))
        cat(sprintf("P-value: %.4f\n", res$p.value))
      } else {
        cat("Insufficient data for T-Test\n")
      }
    }
  })
  
  # Full summary of all key numeric and categorical columns
  output$full_summary <- renderPrint({
    summary(customers_data %>% select(Age, Gender, Orders_num, Spending_num,
                                      Bill_num, Sales_Amount, Decision_Group))
  })
  
}

# Run the app
shinyApp(ui, server)
