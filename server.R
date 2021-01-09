function(input, output, session){
  val<-reactiveValues()
  val$plot<-1
  val$itemplot<-1
  val$plot_comp<-1
  val$plot_comp33<-1
  val$BudgetDetail_rbind<-data.table()
  val$BudgetDetail_rbind3<-data.table()
  val$BudgetDetail_view_scenario<-data.table()
  val$currentep<-0
  val$finalep<-0
  val$showseasonid<-0
  val$compmovies<-data.table(Comp1=character(),Comp2=character(),Comp3=character(),Comp4=character(),Comp5=character())
  val$knn_ids<-data.table(Comp1=numeric(),Comp2=numeric(),Comp3=numeric(),Comp4=numeric(),Comp5=numeric())
  val$Meta_Season<-data.table()
  val$Pred_Actual<-NULL
###
#  load all tv shows and seasons  
###  
  withProgress(message="Calculating",value=0,{
    incProgress(0.5,"Querying Data")
    coasummary<-dbGetQuery(jdbcConnection, "Select * From cidw_ezbudget_publish.ezbudget_views.v_rpt_COASummary")
    showproject<-dbGetQuery(jdbcConnection,"Select Title, SeasonDescription, EpisodeNumber, ProjectId, ProjectTitle,ShowSeasonId
                                            From
                                            cidw_ezbudget_publish.ezbudget_views.v_TVShowProjectTree
                                            Where IsFakeGL = 0
                                            ")
    showbudget<-dbGetQuery(jdbcConnection,"SELECT MAX(Episodes) As MaxEpisode, Title, SeasonDescription
                                    FROM cidw_ezbudget_publish.ezbudget_views.v_TVShowBudgetTree
                                    Group By Title, SeasonDescription")
    RunningTimeLookUp <-dbGetQuery(jdbcConnection, "SELECT DomainId, ItemDescription as RunningTime FROM cidw_ezbudget_publish.ezbudget_views.v_DomainLookup Where Domain='RunningTime' ")
    MediumLookUp <- dbGetQuery(jdbcConnection, "SELECT DomainId, ItemDescription as Medium FROM cidw_ezbudget_publish.ezbudget_views.v_DomainLookup Where Domain='Medium' ")
    LocationLookUp <- dbGetQuery(jdbcConnection, "SELECT DomainId, ItemDescription as Location FROM cidw_ezbudget_publish.ezbudget_views.v_DomainLookup Where Domain='Location' ")
    GenreLookUp<-dbGetQuery(jdbcConnection, "SELECT Title, b.ItemDescription as Genre
                                From cidw_ezbudget_publish.ezbudget_views.v_TVShow a
                                Left Join
                                cidw_ezbudget_publish.ezbudget_views.v_DomainLookup b
                                On a.GenreDomainId=b.DomainID" )
    # NetworkLookUp<-dbGetQuery(jdbcConnection,"SELECT a.ShowSeasonId, b.ItemDescription as Network
    #                               From (Select distinct ShowTitle, SeasonDomainId, ShowSeasonId, NetworkDomainId From cidw_ezbudget_publish.ezbudget_views.v_ShowSeasonProject) a
    #                               Left Join
    #                               cidw_ezbudget_publish.ezbudget_views.v_DomainLookup b
    #                               On a.NetworkDomainId =b.DomainID
    #                               Where b.DomainID != 67")
    NetworkLookUp_1<-dbGetQuery(jdbcConnection,"SELECT DomainId, ItemDescription as Network
                                  From cidw_ezbudget_publish.ezbudget_views.v_DomainLookup")
    NetworkLookUp<-merge(NetworkLookUp_1,v_showseasonproject,by.x="DOMAINID",by.y="NetworkDomainId")
    Budget <- dbGetQuery(jdbcConnection,"Select e.*, f.ItemDescription BudgetType
                                        From
                                        (Select c.*, d.ItemDescription BudgetLevel
                                              From
                                              (Select a.BudgetId, a.BudgetNumber, a.BudgetTypeDomainId, a.LevelDomainId, a.ProjectId, a.scenario, a.CreatedDate,CAST(a.StartDate AS date) As StartDate, CAST(a.FinishDate AS date) As FinishDate, a.BudgetTotal, a.Revision, b.SeasonDescription,b.SeasonDomainId, b.Title, b.EpisodeNumber, b.ProjectTitle
                                                    From cidw_ezbudget_publish.ezbudget_views.v_Budget a
                                                    Left Join cidw_ezbudget_publish.ezbudget_views.v_TVShowProjectTree b
                                                    On a.ProjectId=b.ProjectId) c
                                              Left Join cidw_ezbudget_publish.ezbudget_views.v_DomainLookup d
                                              On c.LevelDomainId = d.DomainID) e
                                        Left Join cidw_ezbudget_publish.ezbudget_views.v_DomainLookup f
                                        On e.BudgetTypeDomainId = f.DomainID
                                        ")
    Budget_FinalRevision <- dbGetQuery(jdbcConnection, "Select e.*, f.ItemDescription BudgetType
                                                    From
                                                    (Select c.*, d.ItemDescription BudgetLevel
                                                          From
                                                          (Select a.Footage, a.LocationDomainId, a.RunningTimeDomainId, MediumDomainId, a.Cameras,a.BudgetNumber, a.BudgetId, a.LevelDomainId,a.BudgetTypeDomainId,a.Scenario, a.ProjectId,a.CreatedDate, a.BudgetTotal,a.Revision,b.SeasonDescription,b.SeasonDomainId,b.ShowSeasonId,b.Title,b.EpisodeNumber
                                                                From
                                                                (Select aa.*
                                                                          From cidw_ezbudget_publish.ezbudget_views.v_Budget aa
										                                                      INNER JOIN
										                                                      (Select ProjectId, LevelDomainId, max(Revision) Revision
										                                                            From
										                                                            cidw_ezbudget_publish.ezbudget_views.v_Budget
										                                                            Group By ProjectId, LevelDomainId) bb
										                                                      On aa.ProjectId = bb.ProjectId And aa.Revision = bb.Revision And aa.LevelDomainId=bb.LevelDomainId) a
                                                                Left Join
                                                                cidw_ezbudget_publish.ezbudget_views.v_TVShowProjectTree b
                                                                On a.ProjectId=b.ProjectId) c
                                                          Left Join cidw_ezbudget_publish.ezbudget_views.v_DomainLookup d
                                                          On c.LevelDomainId = d.DomainID) e
                                                    Left Join cidw_ezbudget_publish.ezbudget_views.v_DomainLookup f
                                                    On e.BudgetTypeDomainId = f.DomainID
                                                    --Where LevelDomainId!=207
                                                    ")
    Budget_FinalRevision_Archive <- dbGetQuery(jdbcConnection, "Select e.*, f.ItemDescription BudgetType
                                                    From
                                                    (Select c.*, d.ItemDescription BudgetLevel
                                                          From
                                                          (Select a.Footage, a.LocationDomainId, a.RunningTimeDomainId, MediumDomainId, a.Cameras,a.BudgetNumber, a.BudgetId, a.LevelDomainId,a.BudgetTypeDomainId,a.Scenario, a.ProjectId,a.CreatedDate, a.BudgetTotal,a.Revision,b.SeasonDescription,b.SeasonDomainId,b.ShowSeasonId,b.Title,b.EpisodeNumber
                                                                From
                                                                (Select aa.*
                                                                          From cidw_ezbudget_publish.ezbudget_views.v_Budget_Archive aa
										                                                      INNER JOIN
										                                                      (Select ProjectId, LevelDomainId, max(Revision) Revision
										                                                            From
										                                                            cidw_ezbudget_publish.ezbudget_views.v_Budget_Archive
										                                                            Group By ProjectId, LevelDomainId) bb
										                                                      On aa.ProjectId = bb.ProjectId And aa.Revision = bb.Revision And aa.LevelDomainId=bb.LevelDomainId) a
                                                                Left Join
                                                                cidw_ezbudget_publish.ezbudget_views.v_TVShowProjectTree b
                                                                On a.ProjectId=b.ProjectId) c
                                                          Left Join cidw_ezbudget_publish.ezbudget_views.v_DomainLookup d
                                                          On c.LevelDomainId = d.DomainID) e
                                                    Left Join cidw_ezbudget_publish.ezbudget_views.v_DomainLookup f
                                                    On e.BudgetTypeDomainId = f.DomainID
                                                    --Where LevelDomainId!=207
                                                    ")
    setDT(coasummary)
    setDT(showbudget)
    setDT(showproject)
    setDT(Budget)
    setDT(Budget_FinalRevision_Archive)
    setDT(GenreLookUp)
    setDT(NetworkLookUp)
    setDT(Budget_FinalRevision)
    setDT(MediumLookUp)
    setDT(RunningTimeLookUp)
    setDT(LocationLookUp)
    incProgress(0.7,"Transforming Data")
  })
  
###
#  cost summary tab--season cost subtab
###  
  output$season <- renderUI({
    req(input$tvshow)
    selectInput("seasonnumber", "Season Number:", choices =c("ALL",unique(showproject[TITLE==input$tvshow][EPISODENUMBER!=0][order(SEASONDESCRIPTION)]$SEASONDESCRIPTION)))
  })
  
  output$episode <- renderUI({
    req(input$seasonnumber)
    selectInput("episodenumber", "Episode Number:", choices = c("ALL", unique(showproject[TITLE==input$tvshow][SEASONDESCRIPTION==input$seasonnumber][EPISODENUMBER!=0][order(EPISODENUMBER)]$EPISODENUMBER)))
  })
  
  output$role <- renderUI({
    selectInput("rolename", "Cost Role:", choices = c("ALL",unique(coasummary$SUMMARYNAME)))
  })
  
  output$group <- renderUI({
    req(input$rolename)
    selectInput("groupname", "Cost Group", choices = c("ALL", unique(coasummary[SUMMARYNAME==input$rolename]$GROUPDESCRIPTION)))
  })
  
  output$role3 <- renderUI({
    selectInput("rolename3", "Cost Role:", choices=c("ALL","Above The Line", "Other Costs","Other Costs 2", "Post Prodcution", "Production"))
  })
  
  output$group3 <- renderUI({
    req(input$rolename3)
    selectInput("groupname3", "Cost Group:", choices=c("ALL", unique(coasummary[SUMMARYNAME==input$rolename3]$GROUPDESCRIPTION)))
  })
  
  output$item3 <- renderUI({
    req(input$groupname3)
    selectInput("itemname3", "Cost Account:", choices = c("ALL",unique(coasummary[SUMMARYNAME==input$rolename3][GROUPDESCRIPTION==input$groupname3]$DESCRIPTION)))
  })

#get summary data based on cost role and cost group selections    
  getSummary<- reactive({
    if(is.null(input$seasonnumber) & is.null(input$episodenumber)){
      #show plot
      val$plot<-1
    }
    else if(!is.null(input$seasonnumber) & is.null(input$episodenumber)){
      #season plot
      val$plot<-2
    }
    else if(!is.null(input$seasonnumber) & !is.null(input$episodenumber)){
      if(input$seasonnumber=="ALL"&input$episodenumber=="ALL"){
        val$plot<-1
      }
      else if(input$seasonnumber!="ALL"&input$episodenumber=="ALL"){
        val$plot<-2
      }
      else if(input$seasonnumber!="ALL"&input$episodenumber!="ALL"){
        if(is.null(input$groupname)){ val$plot <- 3}
        else{
          if(input$groupname == 'ALL'){
            #cost group plot
            val$plot <- 3
          }
          else{
            #cost account plot
            val$plot<-4
            }
        }
      }
    }
    if(val$plot!=1){
      title<-unique(showproject[TITLE==input$tvshow]$TITLE)
      season<-unique(showproject[TITLE==input$tvshow][SEASONDESCRIPTION==input$seasonnumber]$SEASONDESCRIPTION)
      BudgetDetail<-getBudgetDetail(title,season)
    }
  
    if(val$plot==1){
      validate(
        need(input$tvshow , 'Please choose a tv show')
      )
      Budget_show <- Budget_FinalRevision[TITLE == input$tvshow][BUDGETTYPE != 'Series'][BUDGETLEVEL=='Estimated']#[,.(ProjectAvg = mean(BudgetTotal), BudgetType = unique(BudgetType),SeasonDescription = unique(SeasonDescription)), by=ProjectId]
      Budget_show <- Budget_show[,.(Total = sum(BUDGETTOTAL)),by=.(SEASONDESCRIPTION,BUDGETTYPE)]
      Budget_show[order(-SEASONDESCRIPTION,BUDGETTYPE)]
    }
    else if(val$plot==2){
      validate(
        need(input$seasonnumber , 'Please choose a season'),
        need(input$seasonnumber!="ALL", 'Please choose a season')
      )
      BudgetDetail[,.(AMOUNT=sum(AMOUNT)), by=.(EPISODENUMBER,ROLENAME)][order(EPISODENUMBER,ROLENAME)]
    }
    else if(val$plot==3){
      validate(
        need(input$rolename, 'Please choose a Cost Role you want to analyze.'),
        need(input$rolename !="ALL", 'Please choose a Cost Role you want to analyze.')
      )
      Budget_episode <- BudgetDetail[EPISODENUMBER==input$episodenumber][ROLENAME==input$rolename]
      Budget_episode[,.(AMOUNT=sum(AMOUNT)), by=.(GROUPNAME)][order(-GROUPNAME)]
    }
    else if(val$plot==4){
      Budget_episode <- BudgetDetail[EPISODENUMBER==input$episodenumber][ROLENAME==input$rolename][GROUPNAME==input$groupname]
      Budget_episode[,.(AMOUNT=sum(AMOUNT)), by=.(ACCOUNTNAME)][order(-ACCOUNTNAME)]
    }
  })

#render summary table
  output$summaryTable<-renderDT(DT::datatable(getSummary(),
                                              rownames = FALSE,
                                              filter = list(position = 'top', clear = FALSE),
                                              extensions = 'Buttons',
                                              options = list(
                                                  lengthMenu = list(c(10, 50, 100, -1), c('10', '50', '100', 'All')),
                                                  dom = 'litpB',
                                                  buttons = c('copy', 'csv'))))
#order season plot by radio button selection  
  observeEvent(input$order1,{
    output$budgettotalPlot <- renderPlot({
      if(val$plot!=1){
        title<-unique(showproject[TITLE==input$tvshow]$TITLE)
        season<-unique(showproject[TITLE==input$tvshow][SEASONDESCRIPTION==input$seasonnumber]$SEASONDESCRIPTION)
        BudgetDetail <- getBudgetDetail(title,season)
      }

      if(val$plot==1){
        Budget_show <- Budget_FinalRevision[TITLE == input$tvshow][BUDGETTYPE != 'Series'][BUDGETLEVEL=='Estimated']#[,.(ProjectAvg = mean(BUDGETTOTAL), BUDGETTYPE = unique(BUDGETTYPE),SeasonDescription = unique(SeasonDescription)), by=ProjectId]
        Budget_show <- Budget_show[,.(Total = sum(BUDGETTOTAL)/1000000.0),by=.(SEASONDESCRIPTION,BUDGETTYPE)]
        ggplot(data=Budget_show, aes(x=SEASONDESCRIPTION, y=Total, fill=BUDGETTYPE)) +
          geom_bar(stat="identity") +xlab("Season") +ylab("Cost ($ Million)") +
          scale_fill_brewer(palette="Dark2") + coord_flip()
      }
      else if(val$plot==2){
        validate(
          need(input$seasonnumber, 'Please choose a season'),
          need(input$seasonnumber!="ALL", 'Please choose a season')
        )
        dt <- BudgetDetail[,.(AMOUNT=sum(AMOUNT)/1000000.0), by=.(EPISODENUMBER, ROLENAME)]
        dt[order(EPISODENUMBER)]
        if(input$order1!='Episode Number'){
          dt<-dt[,rank:=rep(dt[ROLENAME==input$order1][,rank:=frank(AMOUNT)]$rank,each=5)]
        }
        else{
          dt[,rank:=dt$EPISODENUMBER]
        }
        ggplot(data=dt,aes(x=reorder(EPISODENUMBER,-rank), y=AMOUNT, fill=ROLENAME))+
          geom_bar(stat="identity") +xlab("Episode") + ylab("Cost ($ Million)")+
          scale_fill_brewer(palette="Dark2") + coord_flip()
      }
      else if(val$plot==3){
        validate(
          need(input$rolename, 'Please choose a cost Role you want to analyze.'),
          need(input$rolename !="ALL", 'Please choose a cost Role you want to analyze.')
        )
        Budget_episode <- BudgetDetail[EPISODENUMBER==input$episodenumber][ROLENAME==input$rolename]
        ggplot(data=Budget_episode[,.(AMOUNT=sum(AMOUNT)/1000000.0), by=.(GROUPNAME)], aes(x=GROUPNAME, y=AMOUNT,fill=GROUPNAME))+
          geom_bar(stat="identity") +xlab("Cost Group") + ylab("Cost ($ Million)")+
          scale_fill_viridis(discrete = TRUE) + coord_flip() + guides(fill=FALSE) #scale_fill_brewer(palette="Paired")
      }
      else if(val$plot==4){
        Budget_episode <- BudgetDetail[EPISODENUMBER==input$episodenumber][ROLENAME==input$rolename][GROUPNAME==input$groupname]
        ggplot(data=Budget_episode[,.(AMOUNT=sum(AMOUNT)/1000000.0), by=.(ACCOUNTNAME)], aes(x=ACCOUNTNAME, y=AMOUNT,fill=ACCOUNTNAME))+
          geom_bar(stat="identity") +xlab("Cost Account") + ylab("Cost ($ Million)")+
          scale_fill_viridis(discrete = TRUE) + coord_flip() + guides(fill=FALSE)
      }
    })
  })

###
#  cost summary tab--account cost subtab
### 
  
#get account cost data based on cost role and cost group selection    
  getItemcost<-reactive({
    validate(
      need(input$tvshow , 'Please choose a show'),
      need(input$seasonnumber , 'Please choose a season'),
      need(input$seasonnumber!="ALL", 'Please choose a season'),
      need(input$rolename3,'Please choose a cost Role'),
      need(input$rolename3!="ALL", 'Please choose a cost Role')
    )
    title<-unique(showproject[TITLE==input$tvshow]$TITLE)
    season<-unique(showproject[TITLE==input$tvshow][SEASONDESCRIPTION==input$seasonnumber]$SEASONDESCRIPTION)
    BudgetDetail <- getBudgetDetail(title,season)
    if(is.null(input$groupname3) & is.null(input$itemname3)){
      #cost role plot
      val$itemplot<-1
    }
    else if(!is.null(input$groupname3) & is.null(input$itemname3)){
      #cost group plot
      val$itemplot<-2
    }
    else if(!is.null(input$groupname3) & !is.null(input$itemname3)){
      if(input$groupname3=="ALL"&input$itemname3=="ALL"){
        #cost role plot
        val$itemplot<-1
      }
      else if(input$groupname3!="ALL"&input$itemname3=="ALL"){
        #cost group plot
        val$itemplot<-2
      }
      else if(input$groupname3!="ALL"&input$itemname3!="ALL"){
        #cost account plot
        val$itemplot <- 3
      }
    }
    
    if(val$itemplot==1){
      BudgetDetail[ROLENAME==input$rolename3][,.(AMOUNT=sum(AMOUNT)), by=.(EPISODENUMBER,ROLENAME)][order(EPISODENUMBER,ROLENAME)]
    }
    else if(val$itemplot==2){
      BudgetDetail[GROUPNAME==input$groupname3][,.(AMOUNT=sum(AMOUNT)), by=.(EPISODENUMBER,GROUPNAME)][order(EPISODENUMBER,GROUPNAME)]
    }
    else if(val$itemplot==3){
      BudgetDetail[ACCOUNTNAME==input$itemname3][,.(AMOUNT=sum(AMOUNT)), by=.(EPISODENUMBER,ACCOUNTNAME)][order(EPISODENUMBER,ACCOUNTNAME)]
    }
  })
  
#render account cost table  
  output$itemcostTable <- renderDT(DT::datatable(getItemcost(),
                                               rownames = FALSE,
                                               filter = list(position = 'top', clear = FALSE),
                                               extensions = 'Buttons',
                                               options = list(
                                                 lengthMenu = list(c(10, 50, 100, -1), c('10', '50', '100', 'All')),
                                                 dom = 'litpB',
                                                 buttons = c('copy', 'csv'))))
  #render account cost plot  
  output$itemcostPlot <- renderPlot({
    validate(
      need(input$tvshow, 'Please choose a show'),
      need(input$seasonnumber , 'Please choose a season'),
      need(input$seasonnumber!="ALL", 'Please choose a season'),
      need(input$rolename3,'Please choose a cost Role'),
      need(input$rolename3!="ALL", 'Please choose a cost Role')
    )
    title<-unique(showproject[TITLE==input$tvshow]$TITLE)
    season<-unique(showproject[TITLE==input$tvshow][SEASONDESCRIPTION==input$seasonnumber]$SEASONDESCRIPTION)
    BudgetDetail <- getBudgetDetail(title,season)
    if(val$itemplot==1){
      dt<-BudgetDetail[ROLENAME==input$rolename3][,.(AMOUNT=sum(AMOUNT)), by=.(EPISODENUMBER,ROLENAME)][order(EPISODENUMBER,ROLENAME)]
      ggplot(data=dt, aes(x=EPISODENUMBER, y=AMOUNT))+
        geom_line(color="#00AFBB")
    }
    else if(val$itemplot==2){
      dt<-BudgetDetail[GROUPNAME==input$groupname3][,.(AMOUNT=sum(AMOUNT)), by=.(EPISODENUMBER,GROUPNAME)][order(EPISODENUMBER,GROUPNAME)]
      ggplot(data=dt, aes(x=EPISODENUMBER, y=AMOUNT))+
        geom_line(color="#00AFBB")
    }
    else if(val$itemplot==3){
      dt<-BudgetDetail[ACCOUNTNAME==input$itemname3][,.(AMOUNT=sum(AMOUNT)), by=.(EPISODENUMBER,ACCOUNTNAME)][order(EPISODENUMBER,ACCOUNTNAME)]
      ggplot(data=dt, aes(x=EPISODENUMBER, y=AMOUNT))+
        geom_line(color="#00AFBB")
    }
  })
  
###
#  view budget tab
###  
  output$season_view <- renderUI({
    req(input$tvshow_view)
    selectInput("seasonnumber_view", "Season Number:", choices =c(unique(showproject[TITLE==input$tvshow_view][EPISODENUMBER!=0][order(SEASONDESCRIPTION)]$SEASONDESCRIPTION)))
    #print(input$tvshow_view)
  })
  
  output$type_view <- renderUI({
    req(input$seasonnumber_view)
    selectInput("budgettype_view", "Budget Type:", choices = c(unique(Budget[TITLE==input$tvshow_view][SEASONDESCRIPTION==input$seasonnumber_view]$BUDGETTYPE)))
  })
  
  output$project_view <- renderUI({
    req(input$budgettype_view)
    selectInput("projecttitle_view", "Project Title:", choices = c(unique(Budget[TITLE==input$tvshow_view][SEASONDESCRIPTION==input$seasonnumber_view][BUDGETTYPE==input$budgettype_view]$PROJECTTITLE)))
  })
  
  output$episode_view <- renderUI({
    req(input$seasonnumber_view)
    selectInput("episodenumber_view", "Episode Number:", choices = c(unique(Budget[TITLE==input$tvshow_view][SEASONDESCRIPTION==input$seasonnumber_view][BUDGETTYPE==input$budgettype_view][PROJECTTITLE==input$projecttitle_view][order(EPISODENUMBER)]$EPISODENUMBER)))
  })
  
  output$budgetnum_view <- renderUI({
    req(input$projecttitle_view)
    selectInput("budgetnumber_view", "Budget Number:", choices = c(unique(Budget[TITLE==input$tvshow_view][SEASONDESCRIPTION==input$seasonnumber_view][BUDGETTYPE==input$budgettype_view][PROJECTTITLE==input$projecttitle_view][order(BUDGETNUMBER)]$BUDGETNUMBER)))
  })
  
  output$scenariotitle_view <- renderUI({
    req(input$budgetnumber_view)
    selectInput("scenario_view", "Scenario:", choices = c(unique(Budget[TITLE==input$tvshow_view][SEASONDESCRIPTION==input$seasonnumber_view][BUDGETTYPE==input$budgettype_view][BUDGETNUMBER==input$budgetnumber_view]$SCENARIO)))
  })
  
  output$info_view<-renderText(input$budgetnumber_view)
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$budgetnumber_view, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(val$BudgetDetail_view_scenario, file, row.names = FALSE)
    }
  )

#get budget detail data based on view budget selection  
  getView<-eventReactive(input$view,{
    req(input$tvshow_view)
    req(input$seasonnumber_view)
    req(input$projecttitle_view)
    req(input$budgetnumber_view)
    req(input$scenario_view)
    BudgetDetail_view <- getBudgetDetail_number(input$budgetnumber_view)
    if(!is.null(input$scenario_view) & input$scenario_view!='NA'){
      val$BudgetDetail_view_scenario <- BudgetDetail_view[SCENARIO==input$scenario_view,.(AMOUNT,ITEM,ACCOUNTCODE,ACCOUNTNAME,GROUPNAME,ROLENAME)]
    }
    else{
      val$BudgetDetail_view_scenario <- BudgetDetail_view[is.na(SCENARIO),.(AMOUNT,ITEM,ACCOUNTCODE,ACCOUNTNAME,GROUPNAME,ROLENAME)]
    }
    val$BudgetDetail_view_scenario[,ACCOUNTCODE:=as.integer(ACCOUNTCODE)]
    val$BudgetDetail_view_scenario
  })
  
#render view budget table  
  output$budget_view<-renderDT(DT::datatable(getView(),
                                             rownames = FALSE,
                                             filter = list(position = 'top', clear = FALSE),
                                             extensions = 'Buttons',
                                             options = list(
                                               lengthMenu = list(c(10, 50, 100, -1), c('10', '50', '100', 'All')),
                                               dom = 'litpB',
                                               buttons = c('copy', 'csv'))))
###
#  budget comparison tab--two budget comparison subtab
###  
  output$season1 <- renderUI({
    req(input$tvshow1)
    selectInput("seasonnumber1", "Season Number 1:", choices =c(unique(showproject[TITLE==input$tvshow1][EPISODENUMBER!=0][order(SEASONDESCRIPTION)]$SEASONDESCRIPTION)))
  })
  
  output$type1 <- renderUI({
    req(input$seasonnumber1)
    selectInput("budgettype1", "Budget Type 1:", choices = c(unique(Budget[TITLE==input$tvshow1][SEASONDESCRIPTION==input$seasonnumber1]$BUDGETTYPE)))
  })
  
  output$project1 <- renderUI({
    req(input$budgettype1)
    selectInput("projecttitle1", "Project Title 1", choices = c(unique(Budget[TITLE==input$tvshow1][SEASONDESCRIPTION==input$seasonnumber1][BUDGETTYPE==input$budgettype1]$PROJECTTITLE)))
  })
  
  output$episode1 <- renderUI({
    req(input$seasonnumber1)
    selectInput("episodenumber1", "Episode Number 1:", choices = c(unique(Budget[TITLE==input$tvshow1][SEASONDESCRIPTION==input$seasonnumber1][BUDGETTYPE==input$budgettype1][PROJECTTITLE==input$projecttitle1][order(EPISODENUMBER)]$EPISODENUMBER)))
  })
  
  output$budgetnum1 <- renderUI({
    req(input$projecttitle1)
    selectInput("budgetnumber1", "Budget Number 1:", choices = c(unique(Budget[TITLE==input$tvshow1][SEASONDESCRIPTION==input$seasonnumber1][BUDGETTYPE==input$budgettype1][PROJECTTITLE==input$projecttitle1][order(BUDGETNUMBER)]$BUDGETNUMBER)))
  })
  
  output$scenariotitle1 <- renderUI({
    req(input$budgetnumber1)
    selectInput("scenario1", "Scenario 1:", choices = c(unique(Budget[TITLE==input$tvshow1][SEASONDESCRIPTION==input$seasonnumber1][BUDGETTYPE==input$budgettype1][BUDGETNUMBER==input$budgetnumber1]$SCENARIO)))
  })
  
  output$season2 <- renderUI({
    req(input$tvshow2)
    selectInput("seasonnumber2", "Season Number 2:", choices = c(unique(showproject[TITLE==input$tvshow2][EPISODENUMBER!=0][order(SEASONDESCRIPTION)]$SEASONDESCRIPTION)))
  })
  
  output$type2 <- renderUI({
    req(input$seasonnumber2)
    selectInput("budgettype2", "Budget Type 2:", choices = c(unique(Budget[TITLE==input$tvshow2][SEASONDESCRIPTION==input$seasonnumber2]$BUDGETTYPE)))
  })
  
  output$project2 <- renderUI({
    req(input$budgettype2)
    selectInput("projecttitle2", "Project Title 2", choices = c(unique(Budget[TITLE==input$tvshow2][SEASONDESCRIPTION==input$seasonnumber2][BUDGETTYPE==input$budgettype2]$PROJECTTITLE)))
  })
  
  output$episode2 <- renderUI({
    req(input$seasonnumber2)
    selectInput("episodenumber2", "Episode Number 2:", choices = c(unique(Budget[TITLE==input$tvshow2][SEASONDESCRIPTION==input$seasonnumber2][BUDGETTYPE==input$budgettype2][PROJECTTITLE==input$projecttitle2][order(EPISODENUMBER)]$EPISODENUMBER)))
  })
  
  output$budgetnum2 <- renderUI({
    req(input$projecttitle2)
    selectInput("budgetnumber2", "Budget Number 2:",
                choices =  c(unique(Budget[TITLE==input$tvshow2][SEASONDESCRIPTION==input$seasonnumber2][BUDGETTYPE==input$budgettype2][PROJECTTITLE==input$projecttitle2][order(BUDGETNUMBER)]$BUDGETNUMBER)))
  })
  
  output$scenariotitle2 <- renderUI({
    req(input$budgetnumber2)
    selectInput("scenario2", "Scenario 2:", choices = c(unique(Budget[TITLE==input$tvshow2][SEASONDESCRIPTION==input$seasonnumber2][BUDGETTYPE==input$budgettype2][BUDGETNUMBER==input$budgetnumber2]$SCENARIO)))
  })
  
  output$role2 <- renderUI({
    selectInput("rolename2", "Cost Role:", choices = c("ALL",unique(val$BudgetDetail_rbind$ROLENAME)))
  })
  
  output$group2 <- renderUI({
    req(input$rolename2)
    if(input$rolename2=="ALL"){    
      selectInput("groupname2", "Cost Group:", choices = c("ALL"))
    }
    else{
      selectInput("groupname2", "Cost Group:", choices = c("ALL",unique(val$BudgetDetail_rbind[ROLENAME==input$rolename2]$GROUPNAME)))
    }
  })

#get two budget detail data     
  observeEvent(input$compare,{
    req(input$tvshow1)
    req(input$tvshow2)
    req(input$seasonnumber1)
    req(input$seasonnumber2)
    req(input$projecttitle1)
    req(input$projecttitle2)
    req(input$budgetnumber1)
    req(input$budgetnumber2)
    req(input$scenario1)
    req(input$scenario2)
    validate(
      need(input$budgetnumber1!=input$budgetnumber2 | input$scenario1 != input$scenario2, 'You need to select DIFFERENT budgets')
    )
    BudgetDetail1 <- getBudgetDetail_number(input$budgetnumber1)[, SCENARIO := as.character(SCENARIO)] 
    BudgetDetail2 <- getBudgetDetail_number(input$budgetnumber2)[, SCENARIO := as.character(SCENARIO)]
    
    if(!is.null(input$scenario1) & input$scenario1!='NA'){
      BudgetDetail1_scenario <- BudgetDetail1[SCENARIO==input$scenario1]
    }
    else{
      BudgetDetail1_scenario <- BudgetDetail1[is.na(SCENARIO)]
    }
    if(!is.null(input$scenario2) & input$scenario2!='NA'){
      BudgetDetail2_scenario <- BudgetDetail2[SCENARIO==input$scenario2]
    }
    else{
      BudgetDetail2_scenario <- BudgetDetail2[is.na(SCENARIO)]
    }
     BudgetDetail1_unite <- unite(BudgetDetail1_scenario, BudgetNumber_scenario, c(BUDGETNUMBER, SCENARIO), remove=FALSE)
     BudgetDetail2_unite <- unite(BudgetDetail2_scenario, BudgetNumber_scenario, c(BUDGETNUMBER, SCENARIO), remove=FALSE)
     val$BudgetDetail_rbind <- rbind(BudgetDetail1_unite, BudgetDetail2_unite, fill = TRUE)
     updateTabsetPanel(session, "two_budgets_tabset1", selected = "Summary")
  })

#plot     
  observeEvent(c(input$rolename2, input$groupname2),{
    if(is.null(input$rolename2)){
      val$plot_comp<-1
    }
    else if(input$rolename2=="ALL"){
      val$plot_comp<-1
    }
    else{
      if(is.null(input$groupname2)){
        val$plot_comp<-2
      }
      else if(input$groupname2=="ALL"){
        val$plot_comp<-2
      }
      else{
        val$plot_comp<-3
      }
   }
  })

#render budget comparison plot    
  output$budgetcomparisonPlot<-renderPlot({
      if(val$plot_comp==1){
        BudgetDetail_sum <- val$BudgetDetail_rbind[,.(AMOUNT=sum(AMOUNT)), by =.(BudgetNumber_scenario,ROLENAME)]#[order(-ROLENAME)]
        diff_df = BudgetDetail_sum %>%
          group_by(ROLENAME) %>%
          spread(BudgetNumber_scenario, AMOUNT)
        diff_df$diff = diff_df[[2]] - diff_df[[3]]
        diff_df$max_y = apply(rbind(diff_df[[2]], diff_df[[3]]),2,max)
        diff_df$sim_higher = diff_df[[2]] > diff_df[[3]]
        
        ggplot(BudgetDetail_sum, aes(x=factor(ROLENAME), AMOUNT)) +
          geom_bar(aes(y = max_y), data = diff_df, stat = "identity", fill = "grey80", width = 0.4) +
          geom_bar(aes(fill = BudgetNumber_scenario), position = "dodge", stat="identity", width = 0.5) +
          geom_text(aes(label = round(diff, 4), y = max_y), vjust=0.7, data = diff_df %>% filter(sim_higher),
                    hjust = 1, colour = scales::muted("red")) +
          geom_text(aes(label = round(diff, 4), y = max_y), vjust=0.7, data = diff_df %>% filter(!sim_higher),
                    hjust = 1, colour = scales::muted("red")) +
          coord_flip()
      }
      else if(val$plot_comp==2){
          BudgetDetail_sum <- val$BudgetDetail_rbind[ROLENAME==input$rolename2][,.(AMOUNT=sum(AMOUNT)), by=.(BudgetNumber_scenario,GROUPNAME)][order(-GROUPNAME)]
          diff_df = BudgetDetail_sum %>%
            group_by(GROUPNAME) %>%
            spread(BudgetNumber_scenario, AMOUNT)
          diff_df$diff = diff_df[[2]] - diff_df[[3]]
          diff_df$max_y = apply(rbind(diff_df[[2]], diff_df[[3]]),2,max)
          diff_df$sim_higher = diff_df[[2]] > diff_df[[3]]
          
          ggplot(BudgetDetail_sum, aes(GROUPNAME, AMOUNT)) +
            geom_bar(aes(y = max_y), data = diff_df, stat = "identity", fill = "grey80", width = 0.4) +
            geom_bar(aes(fill = BudgetNumber_scenario), position = "dodge", stat="identity", width= 0.5) +
            geom_text(aes(label = round(diff, 4), y = max_y), vjust=0.7, data = diff_df %>% filter(sim_higher),
                      hjust = 1, colour = scales::muted("red")) +
            geom_text(aes(label = round(diff, 4), y = max_y), vjust=0.7, data = diff_df %>% filter(!sim_higher),
                      hjust = 1, colour = scales::muted("red")) +
            coord_flip()
      }
      else{
          BudgetDetail_sum <- val$BudgetDetail_rbind[GROUPNAME==input$groupname2][,.(AMOUNT=sum(AMOUNT)), by=.(BudgetNumber_scenario,ACCOUNTNAME)][order(-ACCOUNTNAME)]
          diff_df = BudgetDetail_sum %>%
            group_by(ACCOUNTNAME) %>%
            spread(BudgetNumber_scenario, AMOUNT)
          diff_df$diff = diff_df[[2]] - diff_df[[3]]
          diff_df$max_y = apply(rbind(diff_df[[2]], diff_df[[3]]),2,max)
          diff_df$sim_higher = diff_df[[2]] > diff_df[[3]]
          
          ggplot(BudgetDetail_sum, aes(ACCOUNTNAME, AMOUNT)) +
            geom_bar(aes(y = max_y), data = diff_df, stat = "identity", fill = "grey80", width = 0.4) +
            geom_bar(aes(fill = BudgetNumber_scenario), position = "dodge", stat="identity", width= 0.5) +
            geom_text(aes(label = round(diff, 4), y = max_y), vjust=0.7, data = diff_df %>% filter(sim_higher),
                      hjust = 1, colour = scales::muted("red")) +
            geom_text(aes(label = round(diff, 4), y = max_y), vjust=0.7, data = diff_df %>% filter(!sim_higher),
                      hjust = 1, colour = scales::muted("red")) +
            coord_flip()
      }
    })
  
#render budget comparison table
  output$budgetcomparisonTable<-renderDataTable({
    BudgetDetail_sum <- val$BudgetDetail_rbind[,.(AMOUNT=sum(AMOUNT)), by =.(BudgetNumber_scenario,ROLENAME)]
    BudgetDetail_reshape <- spread(BudgetDetail_sum,BudgetNumber_scenario,AMOUNT)
    diff<-data.table(Variance=c())
    diff$Variance<-BudgetDetail_reshape[,3]-BudgetDetail_reshape[,2]
    BudgetDetail_reshape<-cbind(BudgetDetail_reshape,diff)
    dat<-BudgetDetail_reshape[order(-ROLENAME)]
  
    subdat_list<-list()
    for(rolename in unique(dat$ROLENAME)){
      BudgetDetail_sum_group <- val$BudgetDetail_rbind[ROLENAME==rolename][,.(AMOUNT=sum(AMOUNT)), by =.(BudgetNumber_scenario,GROUPNAME)]
      BudgetDetail_reshape_group <- spread(BudgetDetail_sum_group, BudgetNumber_scenario,AMOUNT)
      diff<-data.table(Variance=c())
      diff$Variance<-BudgetDetail_reshape_group[,3]-BudgetDetail_reshape_group[,2]
      BudgetDetail_group<-cbind(BudgetDetail_reshape_group,diff)[order(-GROUPNAME)]
      subsubdat_list<-list()
      for(groupname in unique(BudgetDetail_group$GROUPNAME)){
        BudgetDetail_sum_account <- val$BudgetDetail_rbind[GROUPNAME==groupname][,.(AMOUNT=sum(AMOUNT)), by =.(BudgetNumber_scenario,ACCOUNTNAME)]
        BudgetDetail_reshape_account <- spread(BudgetDetail_sum_account, BudgetNumber_scenario, AMOUNT)
        diff<-data.table(Variance=c())
        diff$Variance<-BudgetDetail_reshape_account[,3]-BudgetDetail_reshape_account[,2]
        BudgetDetail_account<-cbind(BudgetDetail_reshape_account,diff)[order(-ACCOUNTNAME)]
        subsubdat_list<-list.append(subsubdat_list,BudgetDetail_account)
      }
      subsubdats<-lapply(subsubdat_list,purrr::transpose)
      subdat_i<-cbind(" " = "&oplus;", BudgetDetail_group, "_details" = I(subsubdats))
      subdat_list<-list.append(subdat_list,subdat_i)
    }
  subdats <- lapply(subdat_list, purrr::transpose)
  Dat <- cbind(" " = "&oplus;", dat, "_details" = I(subdats))
  
#enable expanding and collapsing budget comparison datatable 
  callback = JS(
    "table.column(1).nodes().to$().css({cursor: 'pointer'});",
    "",
    "// make the table header of the nested table",
    "var format = function(d, childId){",
    "  if(d != null){",
    "    var html = ", 
    "      '<table class=\"display compact hover\" id=\"' + childId + '\"><thead><tr>';",
    "    for (var key in d[d.length-1][0]) {",
    "      html += '<th>' + key + '</th>';",
    "    }",
    "    html += '</tr></thead></table>'",
    "    return html;",
    "  } else {",
    "    return '';",
    "  }",
    "};",
    "",
    "// row callback to style the rows of the child tables",
    "var rowCallback = function(row, dat, displayNum, index){",
    "  if($(row).hasClass('odd')){",
    "    $(row).css('background-color', 'papayawhip');",
    "    $(row).hover(function(){",
    "      $(this).css('background-color', '#E6FF99');",
    "    }, function() {",
    "      $(this).css('background-color', 'papayawhip');",
    "    });",
    "  } else {",
    "    $(row).css('background-color', 'lemonchiffon');",
    "    $(row).hover(function(){",
    "      $(this).css('background-color', '#DDFF75');",
    "    }, function() {",
    "      $(this).css('background-color', 'lemonchiffon');",
    "    });",
    "  }",
    "};",
    "",
    "// header callback to style the header of the child tables",
    "var headerCallback = function(thead, data, start, end, display){",
    "  $('th', thead).css({",
    "    'border-top': '3px solid indigo',", 
    "    'color': 'indigo',",
    "    'background-color': '#fadadd'",
    "  });",
    "};",
    "",
    "// make the datatable",
    "var format_datatable = function(d, childId){",
    "  var dataset = [];",
    "  var n = d.length - 1;",
    "  for(var i = 0; i < d[n].length; i++){",
    "    var datarow = $.map(d[n][i], function (value, index) {",
    "      return [value];",
    "    });",
    "    dataset.push(datarow);",
    "  }",
    "  var id = 'table#' + childId;",
    "  if (Object.keys(d[n][0]).indexOf('_details') === -1) {",
    "    var subtable = $(id).DataTable({",
    "                 'data': dataset,",
    "                 'autoWidth': true,",
    "                 'deferRender': true,",
    "                 'info': false,",
    "                 'lengthChange': false,",
    "                 'ordering': d[n].length > 1,",
    "                 'order': [],",
    "                 'paging': false,",
    "                 'scrollX': false,",
    "                 'scrollY': false,",
    "                 'searching': false,",
    "                 'sortClasses': false,",
    "                 'rowCallback': rowCallback,",
    "                 'headerCallback': headerCallback,",
    "                 'columnDefs': [{targets: '_all', className: 'dt-center'}]",
    "               });",
    "  } else {",
    "    var subtable = $(id).DataTable({",
    "            'data': dataset,",
    "            'autoWidth': true,",
    "            'deferRender': true,",
    "            'info': false,",
    "            'lengthChange': false,",
    "            'ordering': d[n].length > 1,",
    "            'order': [],",
    "            'paging': false,",
    "            'scrollX': false,",
    "            'scrollY': false,",
    "            'searching': false,",
    "            'sortClasses': false,",
    "            'rowCallback': rowCallback,",
    "            'headerCallback': headerCallback,",
    "            'columnDefs': [", 
    "              {targets: -1, visible: false},", 
    "              {targets: 0, orderable: false, className: 'details-control'},", 
    "              {targets: '_all', className: 'dt-center'}",
    "             ]",
    "          }).column(0).nodes().to$().css({cursor: 'pointer'});",
    "  }",
    "};",
    "",
    "// display the child table on click",
    "table.on('click', 'td.details-control', function(){",
    "  var tbl = $(this).closest('table'),",
    "      tblId = tbl.attr('id'),",
    "      td = $(this),",
    "      row = $(tbl).DataTable().row(td.closest('tr')),",
    "      rowIdx = row.index();",
    "  if(row.child.isShown()){",
    "    row.child.hide();",
    "    td.html('&oplus;');",
    "  } else {",
    "    var childId = tblId + '-child-' + rowIdx;",
    "    row.child(format(row.data(), childId)).show();",
    "    td.html('&CircleMinus;');",
    "    format_datatable(row.data(), childId);",
    "  }",
    "});")
  
  datatable(Dat, callback = callback, escape = -2, selection="none",
            options = list(
              columnDefs = list(
                list(visible = FALSE, targets = ncol(Dat)),
                list(orderable = FALSE, className = 'details-control', targets = 1),
                list(className = "dt-center", targets = "_all")
              )
            ))
  })
  
###
#  budget comparison tab--three budget comparison subtab
### 
  output$season11 <- renderUI({
    req(input$tvshow11)
    selectInput("seasonnumber11", "Season Number 1:", choices =c(unique(showproject[TITLE==input$tvshow11][EPISODENUMBER!=0][order(SEASONDESCRIPTION)]$SEASONDESCRIPTION)))
  })
  
  output$type11 <- renderUI({
    req(input$seasonnumber11)
    selectInput("budgettype11", "Budget Type 1:", choices = c(unique(Budget[TITLE==input$tvshow11][SEASONDESCRIPTION==input$seasonnumber11]$BUDGETTYPE)))
  })
  
  output$project11 <- renderUI({
    req(input$budgettype11)
    selectInput("projecttitle11", "Project Title 1", choices = c(unique(Budget[TITLE==input$tvshow11][SEASONDESCRIPTION==input$seasonnumber11][BUDGETTYPE==input$budgettype11]$PROJECTTITLE)))
  })
  
  output$episode11 <- renderUI({
    req(input$seasonnumber11)
    selectInput("episodenumber11", "Episode Number 1:", choices = c(unique(Budget[TITLE==input$tvshow11][SEASONDESCRIPTION==input$seasonnumber11][BUDGETTYPE==input$budgettype11][PROJECTTITLE==input$projecttitle11][order(EPISODENUMBER)]$EPISODENUMBER)))
  })
  
  output$budgetnum11 <- renderUI({
    req(input$projecttitle11)
    selectInput("budgetnumber11", "Budget Number 1:", choices = c(unique(Budget[TITLE==input$tvshow11][SEASONDESCRIPTION==input$seasonnumber11][BUDGETTYPE==input$budgettype11][PROJECTTITLE==input$projecttitle11][order(BUDGETNUMBER)]$BUDGETNUMBER)))
  })
  
  output$scenariotitle11 <- renderUI({
    req(input$budgetnumber11)
    selectInput("scenario11", "Scenario 1:", choices = c(unique(Budget[TITLE==input$tvshow11][SEASONDESCRIPTION==input$seasonnumber11][BUDGETTYPE==input$budgettype11][BUDGETNUMBER==input$budgetnumber11]$SCENARIO)))
  })
  
  output$season22 <- renderUI({
    req(input$tvshow22)
    selectInput("seasonnumber22", "Season Number 2:", choices = c(unique(showproject[TITLE==input$tvshow22][EPISODENUMBER!=0][order(SEASONDESCRIPTION)]$SEASONDESCRIPTION)))
  })
  
  output$type22 <- renderUI({
    req(input$seasonnumber22)
    selectInput("budgettype22", "Budget Type 2:", choices = c(unique(Budget[TITLE==input$tvshow22][SEASONDESCRIPTION==input$seasonnumber22]$BUDGETTYPE)))
  })
  
  output$project22 <- renderUI({
    req(input$budgettype22)
    selectInput("projecttitle22", "Project Title 2", choices = c(unique(Budget[TITLE==input$tvshow22][SEASONDESCRIPTION==input$seasonnumber22][BUDGETTYPE==input$budgettype22]$PROJECTTITLE)))
  })
  
  output$episode22 <- renderUI({
    req(input$seasonnumber22)
    selectInput("episodenumber22", "Episode Number 2:", choices = c(unique(Budget[TITLE==input$tvshow22][SEASONDESCRIPTION==input$seasonnumber22][BUDGETTYPE==input$budgettype22][PROJECTTITLE==input$projecttitle22][order(EPISODENUMBER)]$EPISODENUMBER)))
  })
  
  output$budgetnum22 <- renderUI({
    req(input$projecttitle22)
    selectInput("budgetnumber22", "Budget Number 2:",
                choices =  c(unique(Budget[TITLE==input$tvshow22][SEASONDESCRIPTION==input$seasonnumber22][BUDGETTYPE==input$budgettype22][PROJECTTITLE==input$projecttitle22][order(BUDGETNUMBER)]$BUDGETNUMBER)))
  })
  
  output$scenariotitle22 <- renderUI({
    req(input$budgetnumber22)
    selectInput("scenario22", "Scenario 2:", choices = c(unique(Budget[TITLE==input$tvshow22][SEASONDESCRIPTION==input$seasonnumber22][BUDGETTYPE==input$budgettype22][BUDGETNUMBER==input$budgetnumber22]$SCENARIO)))
  })
  
  output$season33 <- renderUI({
    req(input$tvshow33)
    selectInput("seasonnumber33", "Season Number 3:", choices = c(unique(showproject[TITLE==input$tvshow33][EPISODENUMBER!=0][order(SEASONDESCRIPTION)]$SEASONDESCRIPTION)))
  })
  
  output$type33 <- renderUI({
    req(input$seasonnumber33)
    selectInput("budgettype33", "Budget Type 3:", choices = c(unique(Budget[TITLE==input$tvshow33][SEASONDESCRIPTION==input$seasonnumber33]$BUDGETTYPE)))
  })
  
  output$project33 <- renderUI({
    req(input$budgettype33)
    selectInput("projecttitle33", "Project Title 3", choices = c(unique(Budget[TITLE==input$tvshow33][SEASONDESCRIPTION==input$seasonnumber33][BUDGETTYPE==input$budgettype33]$PROJECTTITLE)))
  })
  
  output$episode33 <- renderUI({
    req(input$seasonnumber33)
    selectInput("episodenumber33", "Episode Number 3:", choices = c(unique(Budget[TITLE==input$tvshow33][SEASONDESCRIPTION==input$seasonnumber33][BUDGETTYPE==input$budgettype33][PROJECTTITLE==input$projecttitle33][order(EPISODENUMBER)]$EPISODENUMBER)))
  })
  
  output$budgetnum33 <- renderUI({
    req(input$projecttitle33)
    selectInput("budgetnumber33", "Budget Number 3:",
                choices =  c(unique(Budget[TITLE==input$tvshow33][SEASONDESCRIPTION==input$seasonnumber33][BUDGETTYPE==input$budgettype33][PROJECTTITLE==input$projecttitle33][order(BUDGETNUMBER)]$BUDGETNUMBER)))
  })
  
  output$scenariotitle33 <- renderUI({
    req(input$budgetnumber33)
    selectInput("scenario33", "Scenario 3:", choices = c(unique(Budget[TITLE==input$tvshow33][SEASONDESCRIPTION==input$seasonnumber33][BUDGETTYPE==input$budgettype33][BUDGETNUMBER==input$budgetnumber33]$SCENARIO)))
  })
  
  output$role33 <- renderUI({
    selectInput("rolename33", "Cost Role:", choices = c("ALL", "Above The Line", "Other Costs","Other Costs 2", "Post Prodcution", "Production"))
  })
  
  output$group33 <- renderUI({
    req(input$rolename33)
    if(input$rolename33=="ALL"){    
      selectInput("groupname33", "Cost Group:", choices = c("ALL"))
    }
    else{
      selectInput("groupname33", "Cost Group:", choices = c("ALL",unique(val$BudgetDetail_rbind3[ROLENAME==input$rolename33]$GROUPNAME)))
    }  
  })  

#render different plot based on cost group and role selection    
  observeEvent(c(input$rolename33, input$groupname33),{
    if(is.null(input$rolename33)){
      #cost role plot
      val$plot_comp33<-1
    }
    else if(input$rolename33=="ALL"){
      val$plot_comp33<-1
    }
    else{
      if(is.null(input$groupname33)){
        #cost group plot
        val$plot_comp33<-2
      }
      else if(input$groupname33=="ALL"){
        val$plot_comp33<-2
      }
      else{
        #cost account plot
        val$plot_comp33<-3
      }
    }
  })

#get 3 budget data    
  observeEvent(input$compare_3,{
    req(input$tvshow11)
    req(input$tvshow22)
    req(input$seasonnumber11)
    req(input$seasonnumber22)
    req(input$projecttitle11)
    req(input$projecttitle22)
    req(input$budgetnumber11)
    req(input$budgetnumber22)
    req(input$scenario11)
    req(input$scenario33)
    req(input$tvshow33)
    req(input$seasonnumber33)
    req(input$projecttitle33)
    req(input$budgetnumber33)
    req(input$scenario33)
    validate(
      need(input$budgetnumber11!=input$budgetnumber22 | input$scenario11 != input$scenario22, 'You need to select DIFFERENT budgets'),
      need(input$budgetnumber11!=input$budgetnumber33 | input$scenario11 != input$scenario33, 'You need to select DIFFERENT budgets'),
      need(input$budgetnumber33!=input$budgetnumber22 | input$scenario33 != input$scenario22, 'You need to select DIFFERENT budgets')
    )
    BudgetDetail11 <- getBudgetDetail_number(input$budgetnumber11)[, SCENARIO := as.character(SCENARIO)]
    BudgetDetail22 <- getBudgetDetail_number(input$budgetnumber22)[, SCENARIO := as.character(SCENARIO)]
    BudgetDetail33 <- getBudgetDetail_number(input$budgetnumber33)[, SCENARIO := as.character(SCENARIO)]
    
    if(!is.null(input$scenario11) & input$scenario11!='NA'){
      BudgetDetail11_scenario <- BudgetDetail11[SCENARIO==input$scenario11]
    }
    else{
      BudgetDetail11_scenario <- BudgetDetail11[is.na(SCENARIO)]
    }

    if(!is.null(input$scenario22) & input$scenario22!='NA'){
      BudgetDetail22_scenario <- BudgetDetail22[SCENARIO==input$scenario22]
    }
    else{
      BudgetDetail22_scenario <- BudgetDetail22[is.na(SCENARIO)]
    }
    
    if(!is.null(input$scenario33) & input$scenario33!='NA'){
      BudgetDetail33_scenario <- BudgetDetail33[SCENARIO==input$scenario33]
    }
    else{
      BudgetDetail33_scenario <- BudgetDetail33[is.na(SCENARIO)]
    }
    BudgetDetail11_unite <- unite(BudgetDetail11_scenario, BudgetNumber_scenario, c(BUDGETNUMBER, SCENARIO), remove=FALSE)
    BudgetDetail22_unite <- unite(BudgetDetail22_scenario, BudgetNumber_scenario, c(BUDGETNUMBER, SCENARIO), remove=FALSE)
    BudgetDetail33_unite <- unite(BudgetDetail33_scenario, BudgetNumber_scenario, c(BUDGETNUMBER, SCENARIO), remove=FALSE)
    val$BudgetDetail_rbind3<-rbind(BudgetDetail11_unite, BudgetDetail22_unite, BudgetDetail33_unite, fill = TRUE)
    
    updateTabsetPanel(session, "three_budgets_tabset1", selected = "Summary")
  })

#render 3 budget comparison plot  
  output$budgetcomparisonPlot_3<-renderPlot({
    if(val$plot_comp33==1){
      BudgetDetail_sum <- val$BudgetDetail_rbind3[,.(AMOUNT=sum(AMOUNT)), by =.(BudgetNumber_scenario,ROLENAME)]
      ggplot(data=BudgetDetail_sum, aes(x=ROLENAME, y=AMOUNT,fill=BudgetNumber_scenario)) +
        geom_bar(stat="identity", position = "dodge",width = 0.5) +
        coord_flip()
    }
    else if(val$plot_comp33==2){
        BudgetDetail_sum <- val$BudgetDetail_rbind3[ROLENAME==input$rolename33][,.(AMOUNT=sum(AMOUNT)), by=.(BudgetNumber_scenario,GROUPNAME)][order(-GROUPNAME)]
        ggplot(BudgetDetail_sum, aes(x=GROUPNAME, y=AMOUNT,fill=BudgetNumber_scenario)) +
          geom_bar(stat="identity", position = "dodge", width= 0.5) +
          coord_flip()
    }
    else{
        BudgetDetail_sum <- val$BudgetDetail_rbind3[GROUPNAME==input$groupname33][,.(AMOUNT=sum(AMOUNT)), by=.(BudgetNumber_scenario,ACCOUNTNAME)][order(-ACCOUNTNAME)]
        ggplot(BudgetDetail_sum, aes(x=ACCOUNTNAME, y=AMOUNT,fill=BudgetNumber_scenario)) +
          geom_bar(stat="identity", position = "dodge", width= 0.5) +
          coord_flip()
    }
  })

#render 3 budget comparison table 
  output$budgetcomparisonTable_3<-renderDataTable({
    BudgetDetail_sum <- val$BudgetDetail_rbind3[,.(AMOUNT=sum(AMOUNT)), by =.(BudgetNumber_scenario,ROLENAME)]#[order(-ROLENAME)]
    BudgetDetail_reshape <- spread(BudgetDetail_sum,BudgetNumber_scenario,AMOUNT)
    diff<-data.table(Variance1=c(),Variance2=c())
    diff$Variance1<-BudgetDetail_reshape[,3]-BudgetDetail_reshape[,2]
    diff$Variance2<-BudgetDetail_reshape[,4]-BudgetDetail_reshape[,2]
    dat<-cbind(BudgetDetail_reshape[,1:3],diff[,1],BudgetDetail_reshape[,4],diff[,2])[order(-ROLENAME)]
    
    subdat_list<-list()
    for(rolename in unique(dat$ROLENAME)){
      BudgetDetail_sum_group <- val$BudgetDetail_rbind3[ROLENAME==rolename][,.(AMOUNT=sum(AMOUNT)), by =.(BudgetNumber_scenario,GROUPNAME)]#[order(-ROLENAME)]
      BudgetDetail_reshape_group <- spread(BudgetDetail_sum_group, BudgetNumber_scenario,AMOUNT)
      diff<-data.table(Variance1=c(),Variance2=c())
      diff$Variance1<-BudgetDetail_reshape_group[,3]-BudgetDetail_reshape_group[,2]
      diff$Variance2<-BudgetDetail_reshape_group[,4]-BudgetDetail_reshape_group[,2]
      BudgetDetail_group<-cbind(BudgetDetail_reshape_group[,1:3],diff[,1],BudgetDetail_reshape_group[,4],diff[,2])[order(-GROUPNAME)]
      subsubdat_list<-list()
      for(groupname in unique(BudgetDetail_group$GROUPNAME)){
        BudgetDetail_sum_account <- val$BudgetDetail_rbind3[GROUPNAME==groupname][,.(AMOUNT=sum(AMOUNT)), by =.(BudgetNumber_scenario,ACCOUNTNAME)]#[order(-ROLENAME)]
        BudgetDetail_reshape_account <- spread(BudgetDetail_sum_account, BudgetNumber_scenario, AMOUNT)
        diff<-data.table(Variance1=c(),Variance2=c())
        diff$Variance1<-BudgetDetail_reshape_account[,3]-BudgetDetail_reshape_account[,2]
        diff$Variance2<-BudgetDetail_reshape_account[,4]-BudgetDetail_reshape_account[,2]
        BudgetDetail_account<-cbind(BudgetDetail_reshape_account[,1:3],diff[,1],BudgetDetail_reshape_account[,4],diff[,2])[order(-ACCOUNTNAME)]
        subsubdat_list<-list.append(subsubdat_list,BudgetDetail_account)
      }
      subsubdats<-lapply(subsubdat_list,purrr::transpose)
      subdat_i<-cbind(" " = "&oplus;", BudgetDetail_group, "_details" = I(subsubdats))
      subdat_list<-list.append(subdat_list,subdat_i)
    }
    # merge the row details
    subdats <- lapply(subdat_list, purrr::transpose)
    # dataframe for the datatable
    Dat <- cbind(" " = "&oplus;", dat, "_details" = I(subdats))
    # the callback to enable expanding and collapsing
    callback = JS(
      "table.column(1).nodes().to$().css({cursor: 'pointer'});",
      "",
      "// make the table header of the nested table",
      "var format = function(d, childId){",
      "  if(d != null){",
      "    var html = ", 
      "      '<table class=\"display compact hover\" id=\"' + childId + '\"><thead><tr>';",
      "    for (var key in d[d.length-1][0]) {",
      "      html += '<th>' + key + '</th>';",
      "    }",
      "    html += '</tr></thead></table>'",
      "    return html;",
      "  } else {",
      "    return '';",
      "  }",
      "};",
      "",
      "// row callback to style the rows of the child tables",
      "var rowCallback = function(row, dat, displayNum, index){",
      "  if($(row).hasClass('odd')){",
      "    $(row).css('background-color', 'papayawhip');",
      "    $(row).hover(function(){",
      "      $(this).css('background-color', '#E6FF99');",
      "    }, function() {",
      "      $(this).css('background-color', 'papayawhip');",
      "    });",
      "  } else {",
      "    $(row).css('background-color', 'lemonchiffon');",
      "    $(row).hover(function(){",
      "      $(this).css('background-color', '#DDFF75');",
      "    }, function() {",
      "      $(this).css('background-color', 'lemonchiffon');",
      "    });",
      "  }",
      "};",
      "",
      "// header callback to style the header of the child tables",
      "var headerCallback = function(thead, data, start, end, display){",
      "  $('th', thead).css({",
      "    'border-top': '3px solid indigo',", 
      "    'color': 'indigo',",
      "    'background-color': '#fadadd'",
      "  });",
      "};",
      "",
      "// make the datatable",
      "var format_datatable = function(d, childId){",
      "  var dataset = [];",
      "  var n = d.length - 1;",
      "  for(var i = 0; i < d[n].length; i++){",
      "    var datarow = $.map(d[n][i], function (value, index) {",
      "      return [value];",
      "    });",
      "    dataset.push(datarow);",
      "  }",
      "  var id = 'table#' + childId;",
      "  if (Object.keys(d[n][0]).indexOf('_details') === -1) {",
      "    var subtable = $(id).DataTable({",
      "                 'data': dataset,",
      "                 'autoWidth': true,",
      "                 'deferRender': true,",
      "                 'info': false,",
      "                 'lengthChange': false,",
      "                 'ordering': d[n].length > 1,",
      "                 'order': [],",
      "                 'paging': false,",
      "                 'scrollX': false,",
      "                 'scrollY': false,",
      "                 'searching': false,",
      "                 'sortClasses': false,",
      "                 'rowCallback': rowCallback,",
      "                 'headerCallback': headerCallback,",
      "                 'columnDefs': [{targets: '_all', className: 'dt-center'}]",
      "               });",
      "  } else {",
      "    var subtable = $(id).DataTable({",
      "            'data': dataset,",
      "            'autoWidth': true,",
      "            'deferRender': true,",
      "            'info': false,",
      "            'lengthChange': false,",
      "            'ordering': d[n].length > 1,",
      "            'order': [],",
      "            'paging': false,",
      "            'scrollX': false,",
      "            'scrollY': false,",
      "            'searching': false,",
      "            'sortClasses': false,",
      "            'rowCallback': rowCallback,",
      "            'headerCallback': headerCallback,",
      "            'columnDefs': [", 
      "              {targets: -1, visible: false},", 
      "              {targets: 0, orderable: false, className: 'details-control'},", 
      "              {targets: '_all', className: 'dt-center'}",
      "             ]",
      "          }).column(0).nodes().to$().css({cursor: 'pointer'});",
      "  }",
      "};",
      "",
      "// display the child table on click",
      "table.on('click', 'td.details-control', function(){",
      "  var tbl = $(this).closest('table'),",
      "      tblId = tbl.attr('id'),",
      "      td = $(this),",
      "      row = $(tbl).DataTable().row(td.closest('tr')),",
      "      rowIdx = row.index();",
      "  if(row.child.isShown()){",
      "    row.child.hide();",
      "    td.html('&oplus;');",
      "  } else {",
      "    var childId = tblId + '-child-' + rowIdx;",
      "    row.child(format(row.data(), childId)).show();",
      "    td.html('&CircleMinus;');",
      "    format_datatable(row.data(), childId);",
      "  }",
      "});")
    
    # datatable
    datatable(Dat, callback = callback, escape = -2, selection="none",
              options = list(
                columnDefs = list(
                  list(visible = FALSE, targets = ncol(Dat)),
                  list(orderable = FALSE, className = 'details-control', targets = 1),
                  list(className = "dt-center", targets = "_all")
                )))
  })

###
#  cost prediction tab  
###
  output$tv_pred<-renderUI({
    selectInput("tvshow_pred", "TV Show:", choices = unique(Budget_FinalRevision[LEVELDOMAINID==209][EPISODENUMBER==1][order(TITLE)]$TITLE))
  })
  
  output$season_pred <- renderUI({
    req(input$tvshow_pred)
    selectInput("seasonnumber_pred", "Season:", choices =c("ALL",unique(showproject[TITLE==input$tvshow_pred][EPISODENUMBER!=0]$SEASONDESCRIPTION)))
  })
  
  output$episode_pred <-renderUI({
    req(input$seasonnumber_pred)
    selectInput("episodenumber_pred", "Episode:", choices=c("ALL",unique(showproject[TITLE==input$tvshow_pred][EPISODENUMBER!=0][SEASONDESCRIPTION==input$seasonnumber_pred]$EPISODENUMBER)))
  })
  
  output$date_pred <- renderUI({
    req(input$seasonnumber_pred)
    date_start <- min(Budget[TITLE==input$tvshow_pred][LEVELDOMAINID==209][SEASONDESCRIPTION==input$seasonnumber_pred]$FinishDate, na.rm = TRUE)
    date_end <- max(Budget[TITLE==input$tvshow_pred][LEVELDOMAINID==209][SEASONDESCRIPTION==input$seasonnumber_pred]$FinishDate,na.rm = TRUE)
    dateInput('date',
              label = paste('Pick a date for forecasting'),
              value = median(Budget[TITLE==input$tvshow_pred][LEVELDOMAINID==209][SEASONDESCRIPTION==input$seasonnumber_pred]$FinishDate,na.rm = TRUE),
              min = date_start,
              max = date_end,
              format = "yyyy-mm-dd")
  })
  
  observeEvent(input$forecast,{
    validate(
      need(input$tvshow_pred,"Please choose a show"),
      need(input$seasonnumber_pred, 'Please choose a season'),
      need(input$seasonnumber_pred!="ALL", 'Please choose a season'),
      need(input$episodenumber_pred, 'Please choose a episode'),
      need(input$episodenumber_pred!="ALL", 'Please choose a episode')
    )
    #Data Processing
    Meta_Season_RunningTime<-merge(Budget_FinalRevision[LEVELDOMAINID==208][BUDGETTYPE=="Series"], RunningTimeLookUp, by.x = "RUNNINGTIMEDOMAINID", by.y = "DOMAINID", all.x=TRUE)
    Meta_Season_Medium<-merge(Meta_Season_RunningTime, MediumLookUp, by.x="MEDIUMDOMAINID", by.y="DOMAINID", all.x=TRUE)
    Meta_Season_Location<-merge(Meta_Season_Medium, LocationLookUp, by.x="LOCATIONDOMAINID", by.y="DOMAINID", all.x=TRUE)
    Meta_Season_Genre<-merge(Meta_Season_Location, GenreLookUp, by.x="TITLE", by.y="TITLE", all.x=TRUE)
    Meta_Season_Network<-merge(Meta_Season_Genre, NetworkLookUp, by.x="SHOWSEASONID", by.y="ShowSeasonId", all.x=TRUE)
    
    EFC_FinalRevision<-Budget_FinalRevision[LEVELDOMAINID==209][BUDGETTYPE=="Episodic"]
    showmax<-EFC_FinalRevision[, .(MaxEpisode=max(EPISODENUMBER)), by=.(TITLE, SEASONDESCRIPTION)]
    
    Meta_Season<-merge(Meta_Season_Network,showmax,by=c("TITLE","SEASONDESCRIPTION"))
    #merge different film mediums into film    
    Meta_Season[,Medium2:=MEDIUM]
    Meta_Season[MEDIUM=='16mm or 35mm', Medium2:='Film']
    Meta_Season[MEDIUM=='HD 24P Video & 35mm Film',Medium2:='Film']
    Meta_Season[MEDIUM=='3 Perf', Medium2:='Film']
    Meta_Season[MEDIUM=='Film 35mm', Medium2:='Film']
    Meta_Season[MEDIUM=='16mm Film', Medium2:='Film']
    Meta_Season[MEDIUM=='35mm Film - 3 Perf', Medium2:='Film']
    #merge different digital mediums into digital    
    Meta_Season[is.na(MEDIUM),Medium2:='Digital']
    Meta_Season[MEDIUM=='24p Digital Video', Medium2:='Digital']
    Meta_Season[MEDIUM=='24P and 35mm', Medium2:='Digital']
    Meta_Season[MEDIUM=='HD 24P Digital Video', Medium2:='Digital']
    
    Meta_Season[, MediumDigital:=0]
    Meta_Season[Medium2=='Digital', MediumDigital:=1]
    Meta_Season[,MediumFilm:=0]
    Meta_Season[Medium2=='Film', MediumFilm:=1]
    Meta_Season[,MediumAlexa:=0]
    Meta_Season[Medium2=='Alexa', MediumAlexa:=1]
    Meta_Season[, MediumRed:=0]
    Meta_Season[Medium2=='Red', MediumRed:=1]
    
    Meta_Season[,RunningTime2:=0]
    Meta_Season[RUNNINGTIME=='1 Hour', RunningTime2:=60]
    Meta_Season[RUNNINGTIME=='1/2 Hour', RunningTime2:=30]
    Meta_Season[RUNNINGTIME=='10 Minutes', RunningTime2:=120]
    Meta_Season[RUNNINGTIME=='1 1/2 Hour', RunningTime2:=90]
    Meta_Season[,Location2:=LOCATION]
    Meta_Season[LOCATION=='Ranch', Location2:='LA ON LOT']
    Meta_Season[LOCATION=='WBSF', Location2:='LA ON LOT']
    Meta_Season[LOCATION=='LA OFF LOT', Location2:='Los Angeles, CA']
    Meta_Season[LOCATION=='Boston, MA', Location2:='New York, NY']
    Meta_Season[LOCATION=='Australia', Location2:='OVersea']
    Meta_Season[LOCATION=='London, England', Location2:='OVersea']
    Meta_Season[LOCATION=='Prague, Czech Republic', Location2:='OVersea']
    
    Meta_Season[,Location3:=mean(BUDGETTOTAL),Location2]
    Meta_Season[,Year:=as.numeric(substring(SEASONDESCRIPTION,1,4))]
    Meta_Season[,Genre2:=mean(BUDGETTOTAL),GENRE]
    Meta_Season[,Network2:=mean(BUDGETTOTAL),NETWORK]

    val$currentep<-as.numeric(input$episodenumber_pred)
    val$finalep<-Meta_Season[TITLE==input$tvshow_pred][SEASONDESCRIPTION==input$seasonnumber_pred]$MaxEpisode
    #KNN
    val$seasonshowid<- Meta_Season[TITLE==input$tvshow_pred][SEASONDESCRIPTION==input$seasonnumber_pred]$SHOWSEASONID
    BudgetEpisode<-spread(Budget_FinalRevision[LEVELDOMAINID==209][EPISODENUMBER!=0][EPISODENUMBER<=val$currentep][PROJECTID!=12382][,.(SHOWSEASONID,BUDGETTOTAL,EPISODENUMBER)],EPISODENUMBER,BUDGETTOTAL)
    val$Meta_Season<-merge(BudgetEpisode,Meta_Season,by.x="SHOWSEASONID",by.y = "SHOWSEASONID")
    Meta_model<- cbind(val$Meta_Season[MaxEpisode>=val$finalep][,.(SHOWSEASONID,MaxEpisode,CAMERAS,RunningTime2,MediumDigital,MediumAlexa,MediumRed,MediumFilm,Year,Location3,Genre2,Network2,BUDGETTOTAL)]
                  ,val$Meta_Season[MaxEpisode>=val$finalep][,2:(val$currentep+1)])
    targetid<-Meta_model[SHOWSEASONID==val$seasonshowid, which=TRUE]
    #calculate distance matrix
    dis<-as.matrix(daisy(Meta_model[,2:(13+val$currentep)],metric = 'gower'))
    #top 5 comps
    val$knn_ids<-Meta_model$SHOWSEASONID[order(dis[targetid,])[2:6]]
    
    compmovies<-val$Meta_Season[SHOWSEASONID%in%val$knn_ids][,.(TitleSeason=paste0(TITLE, '<br>',SEASONDESCRIPTION))]$TitleSeason
    val$compmovies<-data.table(Comp1=compmovies[1],Comp2=compmovies[2],Comp3=compmovies[3],Comp4=compmovies[4],Comp5=compmovies[5])
    CostPred<-Budget_FinalRevision[LEVELDOMAINID==209][EPISODENUMBER!=0][EPISODENUMBER<=val$finalep][SHOWSEASONID%in%val$knn_ids][,.(Cost=mean(BUDGETTOTAL)),by=EPISODENUMBER]
    CostActual<-Budget_FinalRevision[SHOWSEASONID==val$seasonshowid][LEVELDOMAINID==209][EPISODENUMBER!=0]
    BudgetSeason<-data.table(Budget = rep(val$Meta_Season[SHOWSEASONID==val$seasonshowid]$BUDGETTOTAL,val$finalep))
    val$Pred_Actual<-rbind(data.table(Amount=CostPred$Cost,Episode=CostPred$EPISODENUMBER,DataName='Comp Forecast'),
                        data.table(Amount=CostActual$BUDGETTOTAL, Episode=CostActual$EPISODENUMBER,DataName='EFC'),
                        data.table(Amount=BudgetSeason$Budget,Episode=1:val$finalep,DataName='Season Cost'))
    updateSelectInput(session,'compname1', choices = distinct_comp_names)
    updateSelectInput(session,'compname2', choices = distinct_comp_names)
    updateSelectInput(session,'compname3',choices = distinct_comp_names)
    updateSelectInput(session,'compname4',choices = distinct_comp_names)
    updateSelectInput(session,'compname5',choices = distinct_comp_names)
  })
#render prediction plot by episode  
  output$plot_pred<-renderPlot({
    validate(
      need(input$tvshow_pred,"Please choose a show"),
      need(input$seasonnumber_pred, 'Please choose a season'),
      need(input$seasonnumber_pred!="ALL", 'Please choose a season'),
      need(input$episodenumber_pred, 'Please choose a episode'),
      need(input$episodenumber_pred!="ALL", 'Please choose a episode'),
      need(input$forecast, " ")
    )
    ggplot()+
      geom_line(data=val$Pred_Actual, aes(x=Episode,y=Amount,color=DataName))+
      geom_vline(xintercept=val$currentep)+
      scale_y_continuous(labels = dollar)+
      scale_x_continuous(breaks = seq(2,val$finalep,2), minor_breaks = 1:val$finalep)+
      theme(legend.title = element_blank())
  })
  
#render prediction cumsum plot   
  output$plot_pred_2<-renderPlot({
    validate(
      need(input$tvshow_pred,"Please choose a show"),
      need(input$seasonnumber_pred, 'Please choose a season'),
      need(input$seasonnumber_pred!="ALL", 'Please choose a season'),
      need(input$episodenumber_pred, 'Please choose a episode'),
      need(input$episodenumber_pred!="ALL", 'Please choose a episode'),
      need(input$forecast," ")
    )
    val$Pred_Actual[order(DataName,Episode),cumAmount:=cumsum(Amount),DataName]
    ggplot()+
      geom_line(data=val$Pred_Actual, aes(x=Episode,y=cumAmount,color=DataName))+
      geom_vline(xintercept=val$currentep)+
      scale_y_continuous(labels = dollar)+
      scale_x_continuous(breaks = seq(2,val$finalep,2), minor_breaks = 1:val$finalep)+
      theme(legend.title = element_blank())
  })

#render prediction table by episode    
  output$table_pred<-renderDataTable({
    validate(
      need(input$tvshow_pred,"Please choose a show"),
      need(input$seasonnumber_pred, 'Please choose a season'),
      need(input$seasonnumber_pred!="ALL", 'Please choose a season'),
      need(input$episodenumber_pred, 'Please choose a episode'),
      need(input$episodenumber_pred!="ALL", 'Please choose a episode'),
      need(input$forecast," ")
    )
    spread(val$Pred_Actual[,.(Episode,DataName,Amount)], DataName, Amount)
  },rownames=FALSE)
  
  distinct_comp_names<-c("No Change",as.character(Budget_FinalRevision[LEVELDOMAINID==208][BUDGETTYPE=="Series"][,.(TitleSeason=paste(TITLE,SEASONDESCRIPTION))][order(TitleSeason)]$TitleSeason))
  
  output$row_modif<-renderDataTable({
    DT<-val$compmovies
    cols_display_names<-c("Comp1","Comp2","Comp3","Comp4","Comp5")
    setnames(DT,cols_display_names)
    DT
  },options=list(dom='t'), selection="none",rownames= FALSE, escape=FALSE
  )
  
  output$comp1<-renderUI(
    selectInput('compname1','New Comp1:', choices = distinct_comp_names)
  )
  output$comp2<-renderUI(
    selectInput('compname2','New Comp2:', choices = distinct_comp_names)
  )
  output$comp3<-renderUI(
    selectInput('compname3','New Comp3:',choices = distinct_comp_names)
  )
  output$comp4<-renderUI(
    selectInput('compname4','New Comp4:',choices = distinct_comp_names)
  )
  output$comp5<-renderUI(
    selectInput('compname5','New Comp5:',choices = distinct_comp_names)
  )
  
  observeEvent({input$compname1
                input$compname2
                input$compname3
                input$compname4
                input$compname5},
    {validate(
      need(input$tvshow_pred,"Please choose a show"),
      need(input$seasonnumber_pred, 'Please choose a season'),
      need(input$seasonnumber_pred!="ALL", 'Please choose a season'),
      need(input$episodenumber_pred, 'Please choose a episode'),
      need(input$episodenumber_pred!="ALL", 'Please choose a episode')
    )
    showseasonLookup<-val$Meta_Season[,.(SHOWSEASONID, TitleSeason=paste(TITLE,SEASONDESCRIPTION))]

    if(input$compname1!='No Change'){
      val$compmovies[['Comp1']]<-input$compname1
      val$knn_ids[1]<-unique(showseasonLookup[TitleSeason==input$compname1]$SHOWSEASONID)
    }

    if(input$compname2!='No Change'){
      val$compmovies[["Comp2"]]<-input$compname2
      val$knn_ids[[2]]<-unique(showseasonLookup[TitleSeason==input$compname2]$SHOWSEASONID)
    }

    if(input$compname3!='No Change'){
      val$compmovies[["Comp3"]]<-input$compname3
      val$knn_ids[[3]]<-unique(showseasonLookup[TitleSeason==input$compname3]$SHOWSEASONID)
    }

    if(input$compname4!='No Change'){
      val$compmovies[["Comp4"]]<-input$compname4
      val$knn_ids[[4]]<-unique(showseasonLookup[TitleSeason==input$compname4]$SHOWSEASONID)
    }

    if(input$compname5!='No Change'){
      val$compmovies[["Comp5"]]<-input$compname5
      val$knn_ids[[5]]<-unique(showseasonLookup[TitleSeason==input$compname5]$SHOWSEASONID)
    }
    CostPred<-Budget_FinalRevision[LEVELDOMAINID==209][EPISODENUMBER!=0][EPISODENUMBER<=val$finalep][SHOWSEASONID%in%val$knn_ids][,.(Cost=mean(BUDGETTOTAL)),by=EPISODENUMBER]
    CostActual<-Budget_FinalRevision[SHOWSEASONID==val$seasonshowid][LEVELDOMAINID==209][EPISODENUMBER!=0]
    BudgetSeason<-data.table(Budget = rep(val$Meta_Season[SHOWSEASONID==val$seasonshowid]$BUDGETTOTAL,val$finalep))
    val$Pred_Actual<-rbind(data.table(Amount=CostPred$Cost,Episode=CostPred$EPISODENUMBER,DataName='Comp Forecast'),
                        data.table(Amount=CostActual$BUDGETTOTAL, Episode=CostActual$EPISODENUMBER,DataName='EFC'),
                        data.table(Amount=BudgetSeason$Budget,Episode=1:val$finalep,DataName='Season Cost'))
  })
  
  output$season_plan <- renderUI({
    req(input$tvshow)
    selectInput("seasonnumber", "Season Number:", choices =c("ALL",unique(showproject[TITLE==input$tvshow][EPISODENUMBER!=0]$SEASONDESCRIPTION)))
  })
  
  output$episode_plan <- renderUI({
    req(input$seasonnumber)
    selectInput("episodenumber", "Episode Number:", choices = c("ALL", unique(showproject[TITLE==input$tvshow][SEASONDESCRIPTION==input$seasonnumber][EPISODENUMBER!=0]$EPISODENUMBER)))
  })
  
  output$budget_plan <- renderRHandsontable({
    rhandsontable(BudgetDetail1)
  })
}
