library(shinydashboard)
library(shinyjs)
library(datasets)
library(ggplot2)
library(data.table)
library(DT)
library(viridis)
library(tidyr)
library(tidyverse)
library(grid)
library(reshape2)
library(cluster)
library(scales)
library(plotly)
library(rlist)
library(waiter)
library(rhandsontable)
library(odbc)
library(DBI)
library(shinycssloaders)

v_showseasonproject<-fread("v_showseasonproject.csv")

### BEGIN Uncomment to run locally on RStudio Server
load("~/.smithee.RData")
jdbcConnection <- pool::dbPool(drv = odbc::odbc(),
                               dsn = "snowflake",
                               uid = "SVC_DS_EZBUDGET",
                               pwd = easy_service)
rm("easy_service")
### END Uncomment to run locally on RStudio Server

### BEGIN Uncomment to publish app to RStudio Connect
# jdbcConnection <- pool::dbPool(drv = odbc::odbc(),
#                                dsn = "snowflake",
#                                uid = Sys.getenv("easy_service_uid"),
#                                pwd = Sys.getenv("easy_service_pwd"))
### END Uncomment to publish app to RStudio Connect

showproject<-dbGetQuery(jdbcConnection,"Select * From cidw_ezbudget_publish.ezbudget_views.v_TVShowProjectTree Where IsFakeGL = 0",na.strings = "NULL")
setDT(showproject)

getBudgetDetail<-function(title,season){
  BudgetDetail <- dbGetQuery(jdbcConnection, paste0("
                               Select f.*,g.EpisodeNumber
                                      From
                                      (Select e.*, ee.SummaryName RoleName
                                             From
                                             (Select c.*,d.Description as GroupName
                                                     From
                                                     (Select a.BudgetId, a.Amount, a.Description as Item, a.AccountId, b.COAGroupId, b.Description as AccountName, b.AccountCode
                                                             From cidw_ezbudget_publish.ezbudget_views.v_BudgetDetail a
                                                             Left Join
                                                             cidw_ezbudget_publish.ezbudget_views.v_Account b
                                                             On a.AccountId = b.AccountId) c
                                                      Left Join cidw_ezbudget_publish.ezbudget_views.v_COAGroup d
                                                      On c.COAGroupId = d.COAGroupId) e
                                              Left Join
                                              cidw_ezbudget_publish.ezbudget_views.v_rpt_COASummary ee
                                              On e.AccountId = ee.AccountId) f
                                      Left Join
                                      (Select a.BudgetId, a.LevelDomainId,a.ProjectId,a.CreatedDate, a.BudgetTotal,a.Revision,b.SeasonDescription,b.Title,b.EpisodeNumber
                                              From
                                              (Select aa.*
                                                      From cidw_ezbudget_publish.ezbudget_views.v_Budget aa
										                                  INNER JOIN
										                                  (Select ProjectId, LevelDomainId, max(Revision) Revision
										                                          From cidw_ezbudget_publish.ezbudget_views.v_Budget
										                                          Group By ProjectId, LevelDomainId) bb
										                                  On aa.ProjectId = bb.ProjectId And aa.Revision = bb.Revision And aa.LevelDomainId=bb.LevelDomainId) a
                                              Left Join cidw_ezbudget_publish.ezbudget_views.v_TVShowProjectTree b
                                              On a.ProjectId=b.ProjectId) g
                                      On f.BudgetId = g.BudgetId
                                      Where g.LevelDomainId = 209
                                      And g.EpisodeNumber != 0
                                      And g.Title like '",  title, "'",
                                                    "And g.seasonDescription like '", season, "'"
  ))
  setDT(BudgetDetail)
  return(BudgetDetail)
}

getBudgetDetail_number<-function(budgetnumber){
  BudgetDetail <- dbGetQuery(jdbcConnection, paste0("
  
                                 Select f.*,g.EpisodeNumber, g.scenario, g.BudgetNumber
                                        From
                                        (Select e.*, ee.SummaryName RoleName
                                               From
                                               (Select c.*,d.Description as GroupName
                                                       From
                                                       (Select a.BudgetId, a.Amount, a.Description as Item, a.AccountId, b.COAGroupId, b.Description as AccountName, b.AccountCode
                                                               From cidw_ezbudget_publish.ezbudget_views.v_BudgetDetail a
                                                               Left Join
                                                               cidw_ezbudget_publish.ezbudget_views.v_Account b
                                                               On a.AccountId = b.AccountId) c
                                                        Left Join cidw_ezbudget_publish.ezbudget_views.v_COAGroup d
                                                        On c.COAGroupId = d.COAGroupId) e
                                                Left Join
                                                cidw_ezbudget_publish.ezbudget_views.v_rpt_COASummary ee
                                                On e.AccountId = ee.AccountId) f
                                        Left Join
                                        (Select a.BudgetId, a.BudgetNumber, a.scenario, a.LevelDomainId,a.ProjectId,a.CreatedDate, a.BudgetTotal,a.Revision,b.SeasonDescription,b.Title,b.EpisodeNumber
                                                From
                                                cidw_ezbudget_publish.ezbudget_views.v_Budget a
                                                Left Join cidw_ezbudget_publish.ezbudget_views.v_TVShowProjectTree b
                                                On a.ProjectId=b.ProjectId) g
                                        On f.BudgetId = g.BudgetId
                                        Where g.BudgetNumber = '", budgetnumber,"'"
                                                     
  ))
  setDT(BudgetDetail)
  return(BudgetDetail)
}

onStop(function() {
  cat("Doing application cleanup\n")
  pool::poolClose(jdbcConnection)
})