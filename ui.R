library(shiny)
library(DT)
library(shinyalert)
library(shinyjs)
library(shinymanager)
library(plotly)


## invactivity script is to timeout the login page after 2 mins of inactivity so you dont waste resorces
inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"



shinyUI(
  ## secure login 
  secure_app(
    head_auth = tags$script(inactivity),
    
    #### app ui
    fluidPage(
      shinyjs::useShinyjs(),
      titlePanel("资金台台账核算系统"),
      HTML("<hr>"),
      tabsetPanel(id='maintabset', type='tabs',
                  
                  ## Tab1: 台账上传
                  tabPanel('台账上传', 
                           sidebarPanel(width=3,
                                        wellPanel(
                                          fileInput("ledgerFile", "Choose CSV File",
                                                    multiple = FALSE,
                                                    accept = c("text/csv",
                                                               "text/comma-separated-values,text/plain",
                                                               ".csv")),
                                          actionButton("ledgerUpload", "上传", icon("upload"), 
                                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                        )
                           ),
                           mainPanel(width=9,
                                     tableOutput("ledgerTable")
                           )
                  ),
                  
                  ## Tab2: 佣金计算
                  tabPanel('佣金计算', wellPanel(
                    
                    fluidRow(
                      column(3, offset=3, align="right",
                             actionButton("compute", "开始计算", icon("play-circle"), 
                                          style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                      ),
                      column(3,
                             shinyjs::hidden(
                               downloadButton("computeDownload", "下载详细结果", icon("file-download"), 
                                              style="color: #fff; background-color: #b2626e; border-color: #a4515d")
                             ))  
                    ),
                    HTML("<hr>"),
                    fluidRow(
                      column(12, align="center", 
                             wellPanel(
                               div(tableOutput("compute_summary"), style = "font-size: 120%; width: 100%"),
                               helpText("Note: Download detailed results to check any missing commission!")
                             )
                      )
                    ),
                    fluidRow(
                      column(12, align="center",
                             wellPanel(
                               div(tableOutput("viewByBroker"), style = "font-size: 120%; width: 100%")
                             )
                      )
                    ),
                    fluidRow(
                      column(12, 
                             wellPanel(
                               div(tableOutput('viewMiscalculated'), style = 'overflow-x: scroll')
                             )
                      )
                    )
                  )),
                  
                  ## Tab3: 综合分析
                  tabPanel('综合分析', wellPanel(
                    fluidRow(
                      column(3, dateRangeInput("analysisDateRange","Select Date Range:")),
                      column(2, 
                             wellPanel(actionButton("analysis", "开始分析", icon("play-circle"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4", width='100%'))
                      ),
                      column(2, selectInput("analysisProduct","Select Products:", choices=c("Repo"="repo","Shibor"="depo","Buyout"="buyout"), selected=c("repo","depo","buyout"), multiple=TRUE)),
                      column(3, uiOutput("analysisCP_ui")),
                      column(2, selectInput("analysisMeasure","Select Measure:", choices=c("Commission","Volume","Transactions")))
                    ),
                    # fluidRow(
                    #   column(12, tableOutput("analysisTable"))
                    # ),
                    fluidRow(
                      column(12, 
                             wellPanel(
                               plotlyOutput("analysisPlot", height = "480px")
                             ))
                    )
                  )),
                  
                  ## Tab4: 费率表
                  tabPanel('费率表',
                           useShinyalert(),
                           mainPanel(
                             width = 10,
                             wellPanel(
                               helpText("Note: Remember to save any updates!"),
                               div(DTOutput("feeTable"), style = "font-size: 90%; width: 100%")
                             )
                           ),
                           sidebarPanel(
                             width = 2,
                             wellPanel(
                               actionButton("feeTableSave", "保存", icon("save"), 
                                            style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                             ),
                             wellPanel(
                               actionButton(inputId = "feeTableAdd", label = "增添", icon("plus-square"),
                                            style="color: #fff; background-color: #50c878; border-color: #228b22"),
                               actionButton(inputId = "feeTableDelete", label = "删除", icon("minus-square"),
                                            style="color: #fff; background-color: #ff4040; border-color: #e32636")
                             ),
                             wellPanel(
                               downloadButton("feeTableDownload", "保存并导出CSV表格", icon("file-download"),
                                              style="color: #fff; background-color: #b2626e; border-color: #a4515d")
                             )
                           )
                           
                  ),                
                  
                  ## Tab5: Help
                  tabPanel('Help', wellPanel(
                    fluidRow(column(width = 6,
                                    em("For any support or maintenance, please reach out to "),
                                    a("Vimo",  href="mailto:haowu@tpsitico.com.cn?subject=App Support Request"),
                                    "."))
                  ))
      )
    )        
    
    
  )
)