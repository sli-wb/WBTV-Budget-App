dashboardPage(
  dashboardHeader(
    title = tags$span(tags$img(src='img/Warner_Bros_Television_2019.png', height="90%"), "TV Show Production"),
    titleWidth = 300
  ),
  dashboardSidebar(
    sidebarMenu(id = "tab",
                menuItem("Cost Summary", tabName = "cost-summary"),
                menuItem("View Budget", tabName = "view-budget"),
                menuItem("Budget Comparison", tabName = "budget-comparison"),
                menuItem("Cost Prediction", tabName = "cost-prediction")
    ),
    conditionalPanel(
      condition = "input.tab == 'cost-summary'",
      #& (input.cost_summary_tabset1 == 'Season Cost' | input.cost_summary_tabset1 == 'Account Cost')",
      selectInput("tvshow", "TV show:", choices =c(unique(showproject[order(TITLE)]$TITLE))),

      uiOutput("season")

    ),
    conditionalPanel(
      condition = "input.tab== 'view-budget'",
      selectInput("tvshow_view", "TV show:", choices =c(unique(showproject[order(TITLE)]$TITLE))),
      uiOutput("season_view"),
      uiOutput("type_view"),
      uiOutput("project_view"),
      uiOutput("budgetnum_view"),
      uiOutput("scenariotitle_view"),
      actionButton("view","View Budget")
      
    ),
    conditionalPanel(
      condition = "input.tab == 'cost-summary' & input.cost_summary_tabset1 == 'Season Cost'",
      radioButtons("order1", "Order By:",
                   c("Episode Number","Above The Line",
                     "Other Costs",
                     "Other Costs 2",
                     "Post Production",
                     "Production")),
      
      uiOutput('episode'),
      
      uiOutput('role'),
      
      uiOutput('group')
    ),
    conditionalPanel(
      condition = "input.tab == 'cost-summary' & input.cost_summary_tabset1 == 'Account Cost'",
      
      uiOutput("role3"),
      
      uiOutput("group3"),
      
      uiOutput("item3")
    ),
    conditionalPanel(
      condition = "input.tab == 'cost-prediction'",
      uiOutput('tv_pred'),
      uiOutput('season_pred'),
      uiOutput('episode_pred'),
      actionButton("forecast", "Forecast")
    )#,
    # conditionalPanel(
    #   condition = "input.tab == 'budget-planning'",
    #   textInput('episode_plan',"Number of Episodes"),
    #   uiOutput('')
    # )
  ),
  dashboardBody(
    #use_waiter(),
    #waiter_show_on_load(spin_fading_circles()),
    #waiter_hide_on_render("budgettotalPlot"),
    tabItems(
      tabItem(
        tabName = "cost-summary",
        fluidRow(
          tabBox(
            id = "cost_summary_tabset1",
            width = 12,
            tabPanel(
              "Season Cost",
              withSpinner(plotOutput("budgettotalPlot"),type = 5, color = "#062ecf"),
              DT::dataTableOutput("summaryTable")
            ),
            tabPanel(
              "Account Cost",
              plotOutput("itemcostPlot"),
              DT::dataTableOutput("itemcostTable")
            )
          )
        )
      ),
      tabItem(
        tabName = "view-budget",
        fluidRow(
          tabBox(
            id = "dataset",
            width = 12,
            tabPanel("Budget",
                     h3(textOutput("info_view")),
                     downloadButton("downloadData", "Download"),
                     withSpinner(DT::dataTableOutput("budget_view"),type = 5, color = "#0dc5c1")
            )
          )
        )
      ),
      tabItem(
        tabName = "budget-comparison",
        fluidRow(
          tabBox(
            id = "dataset",
            width = 12,
            tabPanel("Two Budgets",
                     fluidRow(
                       tabBox(
                         id = "two_budgets_tabset1",
                         width = 12,
                         tabPanel(
                           "Budgets",
                           column(4,
                                  selectInput("tvshow1", "TV show 1:", choices =c(unique(showproject[order(TITLE)]$TITLE))),
                                  uiOutput("season1"),
                                  uiOutput("type1"),
                                  uiOutput("project1"),
                                  uiOutput("budgetnum1"),
                                  uiOutput("scenariotitle1")
                                  
                           ),
                           column(4,
                                  selectInput("tvshow2", "TV show 2:", choices =c(unique(showproject[order(TITLE)]$TITLE))),
                                  uiOutput("season2"),
                                  uiOutput("type2"),
                                  uiOutput("project2"),
                                  uiOutput("budgetnum2"),
                                  uiOutput('scenariotitle2'),
                                  actionButton("compare", "Compare")
                           )
                           #column(4,
                           #      actionButton('insertBtn', 'Insert'),
                           #     actionButton('removeBtn', 'Remove'),
                           #    tags$div(id = 'placeholder')),
                         ),
                         tabPanel(
                           "Summary",
                           column(4,
                           #selectInput("rolename2", "Cost Role:", choices = c("ALL", "Above The Line", "Other Costs","Other Costs 2", "Post Prodcution", "Production")),
                           uiOutput("role2"),
                           uiOutput('group2')),
                           column(8,plotOutput("budgetcomparisonPlot")),
                           DT::dataTableOutput("budgetcomparisonTable")
                         )
                       )
                     )
            ),
            tabPanel("Three Budgets",
                     fluidRow(
                       tabBox(
                         id = "three_budgets_tabset1",
                         width = 12,
                         tabPanel(
                           "Budgets",
                           column(4,
                                  selectInput("tvshow11", "TV show 1:", choices =c(unique(showproject[order(TITLE)]$TITLE))),
                                  uiOutput("season11"),
                                  uiOutput("type11"),
                                  uiOutput("project11"),
                                  uiOutput("budgetnum11"),
                                  uiOutput("scenariotitle11")
                           ),
                           column(4,
                                  selectInput("tvshow22", "TV show 2:", choices =c(unique(showproject[order(TITLE)]$TITLE))),
                                  uiOutput("season22"),
                                  uiOutput("type22"),
                                  uiOutput("project22"),
                                  uiOutput("budgetnum22"),
                                  uiOutput('scenariotitle22')
                           ),
                           column(4,
                                  selectInput("tvshow33", "TV show 3:", choices =c(unique(showproject[order(TITLE)]$TITLE))),
                                  uiOutput("season33"),
                                  uiOutput("type33"),
                                  uiOutput("project33"),
                                  uiOutput("budgetnum33"),
                                  uiOutput('scenariotitle33'),
                                  actionButton("compare_3", "Compare")
                           )
                         ),
                         tabPanel(
                           "Summary",
                           column(4,
                             selectInput("rolename33", "Cost Role:", choices = c("ALL", "Above The Line", "Other Costs","Other Costs 2", "Post Prodcution", "Production")),
                             uiOutput('group33')
                           ),
                           column(8,
                                  plotOutput("budgetcomparisonPlot_3")),
                                  DT::dataTableOutput("budgetcomparisonTable_3")
                           )
                       )
                     )
            )
          )
        )
      ),
      tabItem(
        tabName = "cost-prediction",
        fluidRow(
          tabBox(
            id = "cost_prediction_tabset1",
            width = 12,
            tabPanel(
              "Prediction",
              h4(strong("Comp Movies"),align="center"),
              DT::dataTableOutput('row_modif'),
              fluidRow(
              column(2,uiOutput('comp1')),
              column(2,uiOutput('comp2')),
              column(2,uiOutput('comp3')),
              column(2,uiOutput('comp4')),
              column(2,uiOutput('comp5'))
              ),
              hr(),
              plotOutput("plot_pred"),
              plotOutput("plot_pred_2"),
              DT::dataTableOutput("table_pred"),
              DT::dataTableOutput("test")
            )
          )
        )
      )#,
      # tabItem(
      #   tabName = "budget-planning",
      #   fluidRow(
      #     tabBox(
      #       id = "budget-planning-tabset`1",
      #       width = 12,
      #         rHandsontableOutput("budget_plan")
      #     )
      #   )
      # )
    )
  )
)
