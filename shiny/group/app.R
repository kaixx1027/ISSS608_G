pacman::p_load(readr, readxl, sf, tmap, tidyverse, shinythemes, plotly, ggstatsplot, 
               shiny, shinydashboard, shinyWidgets, shinycssloaders, shinyjs)

#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————Data
Rental_data <- read_csv("data/ResidentialRental_Final.csv")
clustertootliplabel <- read_csv("data/clusterlabels.csv")

visualdata <- read_csv("data/ResidentialRental_Final2.csv")
visualdata %>%
  select(Planning_Region, Property_Type, No_of_Bedroom, everything()) %>%
  arrange(No_of_Bedroom)
MLRdata <- read_excel("data/MLR.xlsx")

Rental_sf <- st_as_sf(Rental_data, coords = c("longitude", "latitude"), crs = 4326) 
Rental_sf_svy21 <- st_transform(Rental_sf, crs = 3414)

colorset <-  c("#E9B9AA", "#7892B5", "#D98481", "#8CB9C0", "#EDCA7F", "#91B5A9", "#726286")

custom_css <- "  
.skin-blue .main-header .navbar {  
  background-color: #8B3A3A;  
}  
  
.skin-blue .main-header .logo {  
  background-color: #8B3A3A;  
}  
  
.content-wrapper,  
.right-side {  
  background-color: white;
}

.box .box-header {  
  background-color: #ffffff;
  color: #8B3A3A; 
  border-bottom: none;
}
" 

#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————model
#———————————————————————————————————————————————————————————————————————————————————————————————————————————prediction
MonthlyRentModel = lm(formula = `Monthly Rent ($)` ~ Lease_Commencement_Date + distance_to_school + 
                        distance_to_mrt + FloorAreaSQFT_Avg + Property_TypeDetachedHouse + 
                        Property_TypeExecutiveCondominium + Property_TypeNonlandedProperties + 
                        Property_TypeSemiDetachedHouse + BEDOK + SERANGOON + BUKITTIMAH + 
                        NOVENA + GEYLANG + DOWNTOWNCORE + BUKITMERAH + TANGLIN + 
                        KALLANG + RIVERVALLEY + QUEENSTOWN + PASIRRIS + MARINEPARADE + 
                        NEWTON + HOUGANG + CLEMENTI + TOAPAYOH + TAMPINES + BUKITBATOK + 
                        JURONGWEST + ROCHOR + SINGAPORERIVER + BISHAN + SENGKANG + 
                        YISHUN + BUKITPANJANG + ANGMOKIO + PUNGGOL + JURONGEAST + 
                        WOODLANDS + SOUTHERNISLANDS + ORCHARD + CHOACHUKANG + MUSEUM + 
                        OUTRAM + SEMBAWANG + CHANGI + MANDAI + CENTRALWATERCATCHMENT + 
                        MARINASOUTH, data = MLRdata)

#———————————————————————————————————————————————————————————————————————————————————————————————————————————cluster
df_clustering <- Rental_data %>%
  select(-Project_Name, -Street_Name, -Postal_District, -Monthly_Rent_PSM, -Floor_Area_SQM_Avg, -Lease_Commencement_Date, 
         -nearest_mrt, -nearest_school, -latitude, -longitude) %>%
  mutate(Monthly_Rent_SGD = cut(Monthly_Rent_SGD,
                                breaks = c(0,2000,3000,4000,5000,Inf),
                                labels = c("0-2k", "2-3k", "3-4k", "4-5k", "5k+"))) %>%
  mutate(Monthly_Rent_PSF = cut(Monthly_Rent_PSF,
                                breaks = c(0,3,4,Inf),
                                labels = c("0-3psf", "3-4psf", "4psf+"))) %>%
  mutate(Floor_Area_SQFT_Avg = cut(Floor_Area_SQFT_Avg,
                                   breaks = c(0,600,1000,1400,Inf),
                                   labels = c("0-600sqft", "600-1000sqft", "1000-1400sqft", "1400sqft+"))) %>%
  mutate(distance_to_mrt = cut(distance_to_mrt,
                               breaks = c(0,0.3,0.6,0.9,Inf),
                               labels = c("0-0.3km", "0.3-0.6km", "0.6-0.9km", "0.9km+"))) %>%
  mutate(distance_to_school = cut(distance_to_school,
                                  breaks = c(0,0.3,0.5,0.7,Inf),
                                  labels = c("0-0.3km", "0.3-0.5km", "0.5-0.7km", "0.7km+"))) %>%
  mutate_all(as.factor) %>%
  drop_na()

set.seed(1234)
clust_f <- as.formula(cbind(Year, Planning_Region, Property_Type, Monthly_Rent_SGD, Monthly_Rent_PSF, Floor_Area_SQFT_Avg, distance_to_mrt, 
                            distance_to_school) ~ 1)

entropy <- function(p) {
  sum(-p*log(p))
}

#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————UI
ui <- dashboardPage(  
  header = dashboardHeader(title = "Singapore Housing Rental Prediction"),  
  sidebar = dashboardSidebar(  
    sidebarMenu(  
      id = "tabs",  
      menuItem("Map", tabName = "map", icon = icon("map")),  
      menuItem("Descriptive Analysis", tabName = "da", icon = icon("chart-bar"),  
               menuSubItem("Exploratory Data Analysis", tabName = "eda"),  
               menuSubItem("Confirmatory Data Analysis", tabName = "cda")
      ),  
      menuItem("Clustering Analysis", tabName = "clustering", icon = icon("align-justify")),
      menuItem("Prediction", tabName = "prediction", icon = icon("home"),  
               menuSubItem("Monthly Rent Prediction (Tenant)", tabName = "tenant"),  
               menuSubItem("Monthly Rent Prediction (Landlord)", tabName = "landlord")
      )
    )  
  ),  
  body = dashboardBody(  
    tags$head(  
      tags$style(HTML(custom_css)),
      tags$style(".fa-calculator {font-size:70%}"),
    ),  
    tabItems(  
      #———————————————————————————————————————————————————————————————————————————————————————————————————————————dashboard
      tabItem(tabName = "map",
              fluidRow(
                tabBox(
                  title = h3("Map"),
                  width = 12,
                  height = "65vh",
                  
                  tabPanel(
                    title = tags$p("Seperation", style = "font-weight: bold;"),
                    fluidRow(
                      div(style = "text-align:center;",  
                          radioButtons('mapby',  
                                       label = tags$strong("Show Map By:"),  
                                       choices = c('Planning_Region', 'Property_Type', 'Monthly_Rent_SGD',
                                                   'Monthly_Rent_PSF', 'Floor_Area_SQM_Avg', 'Floor_Area_SQFT_Avg'),  
                                       inline = TRUE)  
                      )
                    ),
                    fluidRow(
                      tmapOutput("mapby",
                                 width = "100%",
                                 height = "50vh")
                    )
                  ),
                  tabPanel(
                    title = tags$p("Overall", style = "font-weight: bold;"),
                    h5("Click the circle to view more information."),
                    tmapOutput("map",
                               width = "100%",
                               height = "50vh")
                  ),
                )
              )
      ), 

      #———————————————————————————————————————————————————————————————————————————————————————————————————————————eda
      tabItem(tabName = "eda",  
              fluidRow(
                div(style = "padding = 0em; margin-left: 0em; margin-right: -0.5em;",
                    tabBox(
                      title = h3("Exploratory Data Analysis"),
                      width = 16,
                      height = "65vh",
                      
                      #—————————————————————————————————————————————————————————scatterplot
                      tabPanel(  
                        title = tags$p("Correlation Analysis", style = "font-weight: bold;"),  
                        fluidRow(  
                          column(width = 12,  
                                 align = "center",  
                                 selectInput("x_var", "Select x variable:",  
                                             choices = c("Floor_Area_SQM_Avg", "Monthly_Rent_PSF", "distance_to_mrt", "distance_to_school"),  
                                             selected = "Floor_Area_SQM_Avg")  
                          )  
                        ),  
                        fluidRow(  
                          column(width = 12,  
                                 align = "center",  
                                 selectInput("facet_var", "Select facet_wrap variable:",  
                                             choices = c("Year", "Planning_Region", "Property_Type"),  
                                             selected = "Property_Type")  
                          )  
                        ),  
                        fluidRow(  
                          column(width = 12,  
                                 plotOutput("scatterPlot", 
                                            width = "100%", 
                                            height = "55vh")  
                          )  
                        )  
                      ),
                      
                      #—————————————————————————————————————————————————————————barchart
                      tabPanel(title = tags$p("Categorical Variables Analysis", style = "font-weight: bold;"),  
                               fluidRow(  
                                 column(width = 12, 
                                        div(style = "text-align:center;",  
                                            radioButtons('xcol1',  
                                                         label = tags$strong("Analyse Rents By:"),  
                                                         choices = c('Property Type' = 'Property_Type',  
                                                                     'Planning Region' = 'Planning_Region'),  
                                                         inline = TRUE)  
                                        )  
                                 )  
                               ),  
                               fluidRow(  
                                 column(width = 12,  
                                        plotOutput('barchart', 
                                                   width = '100%', 
                                                   height = '55vh') 
                                 )  
                               )  
                      )
                    )
                )
              )
      ),
      
      #———————————————————————————————————————————————————————————————————————————————————————————————————————————cda
      tabItem(tabName = "cda",  
              fluidRow(
                div(style = "padding = 0em; margin-left: 0em; margin-right: -0.5em;",
                    tabBox(
                      width = 16,
                      height = "65vh",
                      fluidRow(
                        column(width = 12,
                               align = "center",
                               selectInput("xvar", "Select x variable:",
                                           choices = c("Planning_Region", "Property_Type"),
                                           selected = "Property_Type")
                        )
                      ),
                      fluidRow(
                        column(width = 12,
                               align = "center",
                               selectInput("type", "Select plot type:",
                                           choices = c('box', 'violin'),
                                           selected = "violin")
                        )
                      ),
                      
                      fluidRow(
                        column(width = 12,
                               plotlyOutput("cda_plot",
                                            width = "100%",
                                            height = "55vh")
                        )
                      )
                    )
                )
              )
      ),
      
      #———————————————————————————————————————————————————————————————————————————————————————————————————————————clustering
      tabItem(tabName = "clustering",
               fluidRow(
                 #——————————————————————————————————————————————————————————————————panel
                 column(width = 2,
                        sliderInput(inputId = "num_clusters",
                                    label = "Number of Clusters:",
                                    min = 2,
                                    max = 8,
                                    value = c(2)),
                        sliderInput(inputId = "num_reps",
                                    label = "Number of Repetitions:",
                                    min = 1,
                                    max = 6,
                                    value = c(1)),
                        actionButton(inputId = "run_cluster",
                                     label = "Run Cluster")
                 ),
                 
                 #——————————————————————————————————————————————————————————————————content
                 column(width = 10,
                        fluidRow(
                          div(style = "padding = 0em; margin-left: -2em; margin-right: -0.5em;",
                              tabBox(
                                title = h3("Clustering Analysis"),
                                width = 12,
                                height = "65vh",
                                
                                tabPanel(
                                  title = tags$p("Cluster Proportion", style = "font-weight: bold;"),
                                  plotlyOutput("clust_bar",
                                               width = "100%",
                                               height = "50vh")
                                ),
                                
                                tabPanel(
                                  title = tags$p("Cluster Characteristics", style = "font-weight: bold;"),
                                  plotlyOutput("clust_plot",
                                               width = "100%",
                                               height = "50vh")
                                ),
                                
                                tabPanel(
                                  title = tags$p("Cluster Characteristic - Ind.", style = "font-weight: bold;"),
                                  fluidRow(
                                    column(width = 4),
                                    column(width = 4,
                                           align = "center",
                                           selectizeInput(inputId = "clust_var",
                                                          width = "100%",
                                                          label = "Select variable:",
                                                          choices = list("Year", "Planning_Region", "Property_Type", "Monthly_Rent_SGD", "Monthly_Rent_PSF", 
                                                                         "Floor_Area_SQFT_Avg", "distance_to_mrt", "distance_to_school"),
                                                          selected = "Planning_Region")),
                                    column(width = 4,
                                           align = "left",
                                           div(style = "margin-left: -1em; margin-top: 1.75em;",
                                               actionButton(inputId = "clust_var_action_", 
                                                            label = "Plot Graph"))
                                    )
                                  ),
                                  fluidRow(
                                    column(width = 12,
                                           align = "center",
                                           plotlyOutput("clust_ind_plot",
                                                        width = "90%",
                                                        height = "45vh"))
                                  )
                                ),
                                
                                tabPanel(
                                  title = tags$p("Insights", style = "font-weight: bold;"),
                                  div(style = "padding = 0em; margin-top: 0em",
                                      htmlOutput("clust_insight_text",
                                                 width = "100%",
                                                 height = "50vh"))
                                )
                              )
                          )
                        ),
                        
                        fluidRow(
                          div(style = "padding = 0em; margin-top: 0em; margin-left: -2em;",
                              valueBoxOutput("show_aic", width = 3)
                          ),
                          div(style = "padding = 0em; margin-top: 0em; margin-left: -2em;",
                              valueBoxOutput("show_bic", width = 3)
                          ),
                          div(style = "padding = 0em; margin-top: 0em; margin-left: -2em;",
                              valueBoxOutput("show_gsq", width = 3)
                          ),
                          div(style = "padding = 0em; margin-top: 0em; margin-left: -2em;",
                              valueBoxOutput("show_entropy", width = 3)
                          )
                        )
                 )
               )
      ), 
      
      #———————————————————————————————————————————————————————————————————————————————————————————————————————————prediction
      #——————————————————————————————————————————————————————————————————tenant
      tabItem(tabName = "tenant", 
               fluidRow(
                 column(width = 6,
                        dateInput(
                          inputId= "Lease_Commencement_Date",
                          label= "Lease Commencement Date",
                          value = NULL,
                          min = "2023-01-01",
                          max = "2034-12-01",
                          format = "yyyy-mm-dd",
                          startview = "month",
                          weekstart = 0,
                          language = "en",
                          width = NULL,
                          autoclose = TRUE,
                          datesdisabled = NULL,
                          daysofweekdisabled = NULL),
                 sliderInput(inputId = "distance_to_school",
                             label = "Distance to School",
                             min = min(MLRdata$distance_to_school),
                             max = max(MLRdata$distance_to_school),
                             value = mean(MLRdata$distance_to_school)),
                 sliderInput(inputId = "distance_to_mrt",
                             label = "Distance to mrt",
                             min = min(MLRdata$distance_to_mrt),
                             max = max(MLRdata$distance_to_mrt),
                             value = mean(MLRdata$distance_to_mrt)),
                 sliderInput(inputId = "FloorAreaSQFT_Avg",
                             label = "Floor Area (SQFT)",
                             min = min(MLRdata$FloorAreaSQFT_Avg),
                             max = max(MLRdata$FloorAreaSQFT_Avg),
                             value = mean(MLRdata$FloorAreaSQFT_Avg)),
                 selectInput(inputId = "Property_Type",
                             label = "Property Type",
                             choices = unique(MLRdata$Property_Type),
                             selected = "Property_Type"),
                 selectInput(inputId = "PLN_AREA_N",
                             label = "Area",
                             choices = unique(MLRdata$PLN_AREA_N),
                             selected = "PLN_AREA_N")
                 ),
                 column(width = 6,
                        mainPanel(
                          h3("Predicted Monthly Rent:"),
                          verbatimTextOutput("prediction"),
                          h3("Prediction Interval:"),
                          verbatimTextOutput("interval"),
                          dataTableOutput(outputId = "datatable")
                        )
                 )
               )
      ),
      
      #——————————————————————————————————————————————————————————————————landlord
      tabItem(tabName = "landlord", 
               fluidRow(
                 column(width = 6,
                        selectInput(inputId = "Project_Name",
                                    label = "Project Name",
                                    choices = unique(MLRdata$Project_Name),
                                    selected = "Project_Name"),
                        dateInput(
                          inputId= "Lease_Commencement_Date2",
                          label= "Lease Commencement Date",
                          value = NULL,
                          min = "2023-01-01",
                          max = "2034-12-01",
                          format = "yyyy-mm-dd",
                          startview = "month",
                          weekstart = 0,
                          language = "en",
                          width = NULL,
                          autoclose = TRUE,
                          datesdisabled = NULL,
                          daysofweekdisabled = NULL),
                        sliderInput(inputId = "FloorAreaSQFT_Avg2",
                                    label = "Floor Area (SQFT)",
                                    min = min(MLRdata$FloorAreaSQFT_Avg),
                                    max = max(MLRdata$FloorAreaSQFT_Avg),
                                    value = mean(MLRdata$FloorAreaSQFT_Avg))
                 ),
                 column(width = 6,
                        mainPanel(
                          h3("Predicted Monthly Rent:"),
                          verbatimTextOutput("predictiona")
                        )
                 )
               ),
               fluidRow(
                 dataTableOutput(outputId = "datatablea")
               )
      )
    )  
  )  
)  

#——————————————————————————————————————————————————————————————————————server

server <- function(input, output) {
  #———————————————————————————————————————————————————————————————————————————————————————————————————————————map
  output$map <- renderTmap({
    tmap_mode("view")  
    tm_shape(Rental_sf_svy21)+
      tm_bubbles(col = "lightblue",
                 size = 0.8,
                 border.col = "black",
                 border.lwd = 1)
  })
  
  output$mapby <- renderTmap({
    tmap_mode("view")  
    tm_shape(Rental_sf_svy21)+
      tm_bubbles(col = input$mapby,
                 size = 1,
                 border.col = "black",
                 border.lwd = 1)
  })
  
  #———————————————————————————————————————————————————————————————————————————————————————————————————————————eda
  output$scatterPlot <- renderPlot({  
    x_var <- input$x_var  
    facet_var <- input$facet_var  
    
    scatterp <- ggscatterstats(data = Rental_data,  
                               x = !!sym(x_var), y = "Monthly_Rent_SGD",  
                               type = "nonparametric") +  
      facet_wrap(vars(!!sym(facet_var))) +  
      labs(x = x_var, y = "Monthly_Rent_SGD") +  
      theme_minimal()  
    
    return(scatterp)  
  }) 
  
  output$barchart <- renderPlot({
    analysis <- visualdata %>%
      group_by(.dots = input$xcol1) %>%
      summarise(basket_value = mean(`Monthly_Rent_SGD`, na.rm = T))
    
    p <- ggplot(analysis, aes_string(y = 'basket_value', x = input$xcol1)) +
      geom_bar(aes_string(fill = input$xcol1), stat = 'identity') +
      scale_fill_manual(values = colorset) +
      labs(title = 'Average Rental Price', subtitle = paste('by', input$xcol1), 
           x = input$xcol1, y = 'Rental Price ($)',
           fill = input$xcol1)
    return(p)
  })
  
  #———————————————————————————————————————————————————————————————————————————————————————————————————————————cda
  output$cda_plot <- renderPlotly({  
    plot_ly(data = Rental_data,  
            x = ~get(input$xvar),  
            y = ~Monthly_Rent_SGD,  
            type = input$type,
            box = list(visible = TRUE),  
            points = "none",  
            color = ~get(input$xvar)) %>%   
      layout(xaxis = list(title = input$xvar),  
             yaxis = list(title = "Monthly Rent SGD"))  
  }) 
  

  #———————————————————————————————————————————————————————————————————————————————————————————————————————————cluster
  clust_LCA_model <- eventReactive(
    input$run_cluster, {
      poLCA(clust_f, df_clustering, nclass=input$num_clusters, nrep=input$num_reps, maxiter=5000)
    })
  
  clustering_results <- eventReactive(input$run_cluster, {
    LCA_model <- poLCA(clust_f, df_clustering, 
                       nclass = input$num_clusters, 
                       nrep = input$num_reps, 
                       maxiter = 5000)
    
    df_clustering$class <- LCA_model$predclass
    plot_year <- df_clustering %>%
      group_by(Year, class) %>%
      summarise(counts = n()) %>%
      ungroup()
    
    cluster_summary <- df_clustering %>%
      group_by(class) %>%
      summarise(total = n()) %>%
      mutate(percentage = round(total / sum(total) * 100, 2))
    
    list(cluster_summary = cluster_summary, plot_year = plot_year)
  })
  
  output$clust_bar <- renderPlotly({
    cluster_summary <- clustering_results()$cluster_summary
    p <- ggplot(cluster_summary, aes(x = factor(class), y = percentage, fill = factor(class))) +
      geom_col() +
      scale_fill_manual(values = colorset) +
      geom_text(aes(label = paste0(percentage, "%")), vjust = -0.5, size = 3) +
      xlab("Cluster") +
      ylab("Percentage of Total Data") +
      ggtitle("Cluster Distribution") +
      theme_minimal()
    ggplotly(p)
  })
  
  show_aicreactive <- eventReactive(
    input$run_cluster, {
      clust_LCA_model()$aic
    })
  
  show_bicreactive <- eventReactive(
    input$run_cluster, {
      clust_LCA_model()$bic
    })
  
  show_gsqreactive <- eventReactive(
    input$run_cluster, {
      clust_LCA_model()$Gsq
    })
  
  error_prior <- eventReactive(
    input$run_cluster, {
      entropy(clust_LCA_model()$P)
    })
  
  error_post <- eventReactive(
    input$run_cluster, {
      mean(apply(clust_LCA_model()$posterior, c(1,2), entropy), na.rm=T)
    })
  
  
  show_entropyreactive <- eventReactive(
    input$run_cluster, {
      (error_prior() - error_post()) / error_prior()
    })
  
  clust_display_aic = function(){
    output$show_aic = renderValueBox(
      valueBox(
        value = tags$p(scales::comma(round(show_aicreactive(),2)), style = "font-size: 50%;"),
        subtitle = tags$p("AIC", style = "font-size: 90%;"),
        icon = icon("calculator"),
        color = "olive"
      )
    )
  }

  observeEvent(input$run_cluster, clust_display_aic())
  
  clust_display_bic = function(){
    output$show_bic = renderValueBox(
      valueBox(
        value = tags$p(scales::comma(round(show_bicreactive(),2)), style = "font-size: 50%;"), 
        subtitle = tags$p("BIC", style = "font-size: 90%;"), 
        icon = icon("calculator"),
        color = "olive"
      )
    )
  }
  
  observeEvent(input$run_cluster, clust_display_bic())
  
  clust_display_gsq = function(){
    output$show_gsq = renderValueBox(
      valueBox(
        value = tags$p(scales::comma(round(show_gsqreactive(),2)), style = "font-size: 50%;"), 
        subtitle = tags$p("Likelihood Ratio", style = "font-size: 90%;"), 
        icon = icon("calculator"),
        color = "purple"
      )
    )
  }
  
  observeEvent(input$run_cluster, clust_display_gsq())
  
  clust_display_entropy = function(){
    output$show_entropy = renderValueBox(
      valueBox(
        value = tags$p(round(show_entropyreactive(), 3), style = "font-size: 50%;"),
        subtitle = tags$p("Entropy", style = "font-size: 90%;"),
        icon = icon("calculator"),
        color = "purple"
      )
    )
  }
  
  observeEvent(input$run_cluster, clust_display_entropy())
  
  df_clustering_prob <- eventReactive(
    input$run_cluster, {
      reshape2::melt(clust_LCA_model()$probs, level = 2) %>%
        rename(Class = Var1,
               Factor_level = Var2,
               Prop = value,
               Category = L2) %>%
        mutate(Prop = round(Prop*100,2))
    })
  
  df_clustering_final <- eventReactive(
    input$run_cluster, {
      left_join(df_clustering_prob(), clustertootliplabel,
                by = c("Factor_level" = "Factor_level",
                       "Category" = "Category"))
    })
  
  clust_plot_reactive <- eventReactive(
    input$run_cluster, {
      ggplot(df_clustering_final(),
             aes(x = Class, y = Prop, group = desc(Factor_level))) + 
        geom_bar(stat = "identity", position = "stack", 
                 aes(fill = Factor_level,
                     text = paste0("Prop: ", Prop,"%","\n",
                                   "Category: ", Level))) + 
        facet_wrap(~ Category) + 
        coord_flip() +
        labs(fill = "Factor Level") +
        scale_y_continuous("Proportion", expand = c(0, 0)) + 
        theme_minimal(base_size = 10) +
        scale_fill_manual(values = colorset)
    })
  
  output$clust_plot <- renderPlotly({
    ggplotly(clust_plot_reactive(), tooltip = c("text", "x")) %>%
      layout(legend = list(orientation = 'h',
                           xanchor = "center",
                           x = 0.5,
                           yanchor = "top",
                           y = 1.15)) 
  })
  
  df_clustering_mod <- eventReactive(
    input$run_cluster, {
      df_clustering %>%
        mutate(class = clust_LCA_model()$predclass) %>%
        mutate(class = as.factor(class)) 
    })
  
  clust_grouped_table <- eventReactive(
    input$clust_var_action_, {
      df_clustering_mod() %>%
        group_by(!!sym(input$clust_var), class) %>% ##input x to be charted
        summarise(counts = n()) %>%
        ungroup()
    })
  
  clust_class_table <- eventReactive(
    input$clust_var_action_, {
      df_clustering_mod() %>%
        group_by(class) %>%
        summarise(sum_count = n()) %>%
        ungroup()
    })
  
  clust_plot_table <- eventReactive(
    input$clust_var_action_, {
      left_join(clust_grouped_table(), clust_class_table()) %>%
        mutate(perc = round(counts*100/sum_count, 1)) %>%
        rename(cluster = class)
    })
  
  clust_ind_plot_reactive <- eventReactive(
    input$clust_var_action_, {
      ggplot(clust_plot_table(), aes(x = cluster, y = counts, group = desc(!!sym(input$clust_var)))) + 
        geom_bar(
          aes(fill = !!sym(input$clust_var), 
              text = paste0("prop: ",perc,"%")),
          color = "black", 
          stat = "identity", 
          position = "fill") +
        coord_flip() +
        labs(x = "Cluster", y = "Proportion", fill = NULL) +
        scale_y_continuous(labels = scales::percent) +
        theme_minimal(base_size = 11) +
        scale_fill_manual(values = colorset)
    })
  
  output$clust_ind_plot <- renderPlotly({
    ggplotly(clust_ind_plot_reactive(), tooltip = c("text", "x", "fill")) %>%
      layout(legend = list(orientation = 'h',
                           xanchor = "center",
                           x = 0.5,
                           yanchor = "top",
                           y = 1.15))
  })
  
  df_clustering_prop <- eventReactive(
    input$run_cluster, {
      df_clustering_mod() %>%
        group_by(class) %>%
        summarise(counts = n()) %>%
        mutate(class_pct = round(counts/ sum(counts), 2)) %>%
        ungroup()
    })
  
  clust_text = function(){
    output$clust_insight_text = renderText({
      HTML(paste0(
        "<span style='font-size: 18px;'>Based on our cluster analysis, here are some interesting insights:",
        "<br><br>1. When the number of categories exceeds 5, the effect of the number of repeats is more pronounced. ",
        "This shows that as the number of categories increases, <b>multiple repetitions</b> can better find the optimal clustering solution.",
        "<br><br>2. The trend of AIC and likelihood ratio is similar to that of <b>BIC</b>, but the trend of <b>entropy</b> is not the same,",
        "because the lowest <b>BIC</b> model does not always give the best <b>entropy</b>.",
        "This shows that different evaluation indicators need to be considered comprehensively when selecting the best solution.",
        "<br><br>3. When determining the best solution, it may be valuable to review the number of members in each category;",
        "It is recommended to set the sample size of the smallest category to <b>at least 5%</b> of the total sample size.",
        "This ensures that there is sufficient sample support for each category, thus improving the <b>stability and reliability</b> of the model.</span>"
      ))
    })
  }
  
  observeEvent(input$run_cluster, clust_text())
  
  #———————————————————————————————————————————————————————————————————————————————————————————————————————————Predict Tenant
  create_newdata <- function() {
    numdate = (as.numeric(as.POSIXct(input$Lease_Commencement_Date, format="%Y-%m-%d")))/86400 + 25570
    data.frame(`Lease_Commencement_Date` = numdate,
               `distance_to_school` = input$distance_to_school,
               `distance_to_mrt` = input$distance_to_mrt,
               `FloorAreaSQFT_Avg` = input$FloorAreaSQFT_Avg,
               Property_TypeDetachedHouse = ifelse(input$Property_Type == "Detached House", 1, 0),
               Property_TypeExecutiveCondominium = ifelse(input$Property_Type == "Executive Condominium", 1, 0),
               Property_TypeNonlandedProperties = ifelse(input$Property_Type == "Non-landed Properties", 1, 0),
               Property_TypeSemiDetachedHouse = ifelse(input$Property_Type == "Semi-Detached House", 1, 0),
               Property_TypeSemiTerraceHouse = ifelse(input$Property_Type == "Terrace House", 1, 0),
               GEYLANG = ifelse(input$PLN_AREA_N == "GEYLANG", 1, 0),
               BUKITTIMAH = ifelse(input$PLN_AREA_N == "BUKIT TIMAH", 1, 0),
               NOVENA = ifelse(input$PLN_AREA_N == "NOVENA", 1, 0),
               TANGLIN = ifelse(input$PLN_AREA_N == "TANGLIN", 1, 0),
               MARINEPARADE = ifelse(input$PLN_AREA_N == "MARINE PARADE", 1, 0),
               NEWTON = ifelse(input$PLN_AREA_N == "NEWTON", 1, 0),
               TOAPAYOH = ifelse(input$PLN_AREA_N == "TOA PAYOH", 1, 0),
               BISHAN = ifelse(input$PLN_AREA_N == "BISHAN", 1, 0),
               ROCHOR = ifelse(input$PLN_AREA_N == "ROCHOR", 1, 0),
               RIVERVALLEY = ifelse(input$PLN_AREA_N == "RIVER VALLEY", 1, 0),
               HOUGANG = ifelse(input$PLN_AREA_N == "HOUGANG", 1, 0),
               QUEENSTOWN = ifelse(input$PLN_AREA_N == "QUEENSTOWN", 1, 0),
               ORCHARD = ifelse(input$PLN_AREA_N == "ORCHARD", 1, 0),
               BEDOK = ifelse(input$PLN_AREA_N == "BEDOK", 1, 0),
               KALLANG = ifelse(input$PLN_AREA_N == "KALLANG", 1, 0),
               DOWNTOWNCORE = ifelse(input$PLN_AREA_N == "DOWNTOWN CORE", 1, 0),
               SINGAPORERIVER = ifelse(input$PLN_AREA_N == "SINGAPORE RIVER", 1, 0),
               PUNGGOL = ifelse(input$PLN_AREA_N == "PUNGGOL", 1, 0),
               ANGMOKIO = ifelse(input$PLN_AREA_N == "ANG MO KIO", 1, 0),
               BUKITMERAH = ifelse(input$PLN_AREA_N == "BUKIT MERAH", 1, 0),
               OUTRAM = ifelse(input$PLN_AREA_N == "OUTRAM", 1, 0),
               SERANGOON = ifelse(input$PLN_AREA_N == "SERANGOON", 1, 0),
               BUKITPANJANG = ifelse(input$PLN_AREA_N == "BUKIT PANJANG", 1, 0),
               TAMPINES = ifelse(input$PLN_AREA_N == "TAMPINES", 1, 0),
               PASIRRIS = ifelse(input$PLN_AREA_N == "PASIR RIS", 1, 0),
               WOODLANDS = ifelse(input$PLN_AREA_N == "WOODLANDS", 1, 0),
               CHANGI = ifelse(input$PLN_AREA_N == "CHANGI", 1, 0),
               CLEMENTI = ifelse(input$PLN_AREA_N == "CLEMENTI", 1, 0),
               SUNGEIKADUT = ifelse(input$PLN_AREA_N == "SUNGEI KADUT", 1, 0),
               BUKITBATOK = ifelse(input$PLN_AREA_N == "BUKIT BATOK", 1, 0),
               SEMBAWANG = ifelse(input$PLN_AREA_N == "SEMBAWANG", 1, 0),
               SOUTHERNISLANDS = ifelse(input$PLN_AREA_N == "SOUTHERN ISLANDS", 1, 0),
               JURONGWEST = ifelse(input$PLN_AREA_N == "JURONG WEST", 1, 0),
               CHOACHUKANG = ifelse(input$PLN_AREA_N == "CHOA CHU KANG", 1, 0),
               SENGKANG = ifelse(input$PLN_AREA_N == "SENGKANG", 1, 0),
               YISHUN = ifelse(input$PLN_AREA_N == "YISHUN", 1, 0),
               JURONGEAST = ifelse(input$PLN_AREA_N == "JURONG EAST", 1, 0),
               MANDAI = ifelse(input$PLN_AREA_N == "MANDAI", 1, 0),
               CENTRALWATERCATCHMENT = ifelse(input$PLN_AREA_N == "CENTRAL WATER CATCHMENT", 1, 0),
               MUSEUM = ifelse(input$PLN_AREA_N == "MUSEUM", 1, 0),
               WESTERNWATERCATCHMENT = ifelse(input$PLN_AREA_N == "WESTERN WATER CATCHMENT", 1, 0),
               SELETAR = ifelse(input$PLN_AREA_N == "SELETAR", 1, 0),
               MARINASOUTH = ifelse(input$PLN_AREA_N == "MARINA SOUTH", 1, 0)
    )
  }
  output$prediction <- renderText({
    newdata <- create_newdata()
    prediction <- predict(MonthlyRentModel, newdata)
    round(prediction, digits = 2)
  })
  output$interval <- renderText({
    newdata <- create_newdata()
    interval <- predict(MonthlyRentModel, newdata, interval = "predict", level = 0.7)
    paste(round(interval[2], digits = 2), " - ", round(interval[3], digits = 2), sep="")
  })
  
  #———————————————————————————————————————————————————————————————————————————————————————————————————————————Predict Landlord
  output$predictiona <- renderText({
    row = match(input$Project_Name, MLRdata$Project_Name)
    numdate2 = (as.numeric(as.POSIXct(input$Lease_Commencement_Date2, format="%Y-%m-%d")))/86400 + 25570
    newdataa = data.frame(`Lease_Commencement_Date` = numdate2,
                          `FloorAreaSQFT_Avg` = input$FloorAreaSQFT_Avg2,
                          distance_to_school=MLRdata$distance_to_school[row],
                          distance_to_mrt=MLRdata$distance_to_mrt[row],
                          `Property_TypeDetachedHouse`=MLRdata$Property_TypeDetachedHouse[row],
                          `Property_TypeExecutiveCondominium`=MLRdata$Property_TypeExecutiveCondominium[row],
                          `Property_TypeNonlandedProperties`=MLRdata$Property_TypeNonlandedProperties[row],
                          `Property_TypeSemiDetachedHouse`=MLRdata$Property_TypeSemiDetachedHouse[row],
                          `BEDOK`=MLRdata$BEDOK[row], `SERANGOON`=MLRdata$SERANGOON[row],
                          `BUKITTIMAH`=MLRdata$BUKITTIMAH[row], `NOVENA`=MLRdata$NOVENA[row],
                          `GEYLANG`=MLRdata$GEYLANG[row], `DOWNTOWNCORE`=MLRdata$DOWNTOWNCORE[row],
                          `BUKITMERAH`=MLRdata$BUKITMERAH[row], `TANGLIN`=MLRdata$TANGLIN[row],
                          `KALLANG`=MLRdata$KALLANG[row], `RIVERVALLEY`=MLRdata$RIVERVALLEY[row],
                          `QUEENSTOWN`=MLRdata$QUEENSTOWN[row],`PASIRRIS`=MLRdata$PASIRRIS[row],
                          `MARINEPARADE`=MLRdata$MARINEPARADE[row], `NEWTON` =MLRdata$NEWTON[row],
                          `HOUGANG`=MLRdata$HOUGANG[row], `CLEMENTI`=MLRdata$CLEMENTI[row],
                          `TOAPAYOH` =MLRdata$TOAPAYOH[row],`TAMPINES`=MLRdata$TAMPINES[row],
                          `BUKITBATOK`=MLRdata$BUKITBATOK[row],`JURONGWEST`=MLRdata$JURONGWEST[row],
                          `ROCHOR`=MLRdata$ROCHOR[row],`SINGAPORERIVER`=MLRdata$SINGAPORERIVER[row],
                          `BISHAN`=MLRdata$BISHAN[row],`SENGKANG` =MLRdata$SENGKANG[row],
                          `YISHUN`=MLRdata$YISHUN[row],`BUKITPANJANG`=MLRdata$BUKITPANJANG[row],
                          `ANGMOKIO`=MLRdata$ANGMOKIO[row],`PUNGGOL`=MLRdata$PUNGGOL[row], 
                          `JURONGEAST`=MLRdata$JURONGEAST[row], `WOODLANDS`=MLRdata$WOODLANDS[row],
                          `SOUTHERNISLANDS`=MLRdata$SOUTHERNISLANDS[row],`ORCHARD`=MLRdata$ORCHARD[row],
                          `CHOACHUKANG`=MLRdata$CHOACHUKANG[row], `MUSEUM`=MLRdata$MUSEUM[row],
                          `OUTRAM`=MLRdata$OUTRAM[row], `SEMBAWANG`=MLRdata$SEMBAWANG[row],
                          `CHANGI`=MLRdata$CHANGI[row], `MANDAI`=MLRdata$MANDAI[row],
                          `CENTRALWATERCATCHMENT`=MLRdata$CENTRALWATERCATCHMENT[row],
                          `MARINASOUTH`=MLRdata$MARINASOUTH[row]
    )
    predictiona <- predict(MonthlyRentModel, newdataa)
    round(predictiona, digits = 2)
  })
  output$datatablea <- renderDataTable({
    dataa <- visualdata[visualdata$Project_Name == input$Project_Name, ]
    dataa %>%
      filter(Project_Name==input$Project_Name) %>%
      select(Project_Name, Street_Name, Monthly_Rent_SGD, Monthly_Rent_PSF, Floor_Area_SQFT_Avg)
  })
}

shinyApp(ui, server)
