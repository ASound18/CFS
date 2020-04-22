library(shiny)
library(DT)
library(shinyalert)
library(readr)
library(shinyjs)
library(shinymanager)
library(data.table)
library(plotly)

## host on local ip
options(shiny.host = "192.168.1.6")
options(shiny.port = 80)


shinyServer(function(input, output, session) {
  
  ## secure login
  credentials <- data.frame(
    user = c("Vimo","Admin"),
    password = c("Vimo", "Admin"),
    stringsAsFactors = FALSE
  )
  result_auth <- secure_server(check_credentials = check_credentials(credentials))
  # output$res_auth <- renderPrint({
  #   reactiveValuesToList(result_auth)
  # })
  
  ## handle Chinese characters
  Sys.setlocale(category = "LC_ALL", locale = "chs")
  
  ## reactive values
  values <- reactiveValues()

  
  ## ~~~~ Tab1: 台账上传 ~~~~
  output$ledgerTable <- renderTable({
    req(input$ledgerFile)
    tryCatch(
      {
        df <- read_csv(input$ledgerFile$datapath, 
                       col_types = "cccccddcccc", 
                       col_names = c("Borrower","Borrower Trader","Lender","Lender Trader","Term","Volume","Rate","Depo.Repo","Bid.Broker","Ask.Broker","Net"),
                       locale = readr::locale(encoding = "GB2312")) #GB2312
        ## remove first row if it is header
        if (df$Borrower[1]=="Borrower") {
          df = df[-1,]
        }
        ## handle spaces
        df$Borrower <- unlist(lapply(df$Borrower,function(x) gsub('\\s+', '',x)))
        df$Lender <- unlist(lapply(df$Lender,function(x) gsub('\\s+', '',x)))
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    values$ledger_df <- df
    return(df)
  })
  
  ## upload ledger
  ## file check and date select
  observeEvent(input$ledgerUpload,{
    validate(need(values$ledger_df,"未检测到文件"))
    showModal(
      if(nrow(values$ledger_df) >= 1 ){
        modalDialog(
          title = "准备上传",
          dateInput("ledgerDate","请选择数据日期"),
          footer = tagList(
            actionButton("ledgerUploadOK", "上传")
          ), easyClose = TRUE)
      }
    )
  })
  ## save ledger file when upload
  observeEvent(input$ledgerUploadOK,{
    values$ledger_df$AS_OF_DATE = input$ledgerDate
    write.csv(values$ledger_df, paste0('LedgerFileArchive/LedgerTable ', input$ledgerDate, '.csv'), quote = FALSE, row.names = FALSE, fileEncoding = "GB2312")
    shinyalert(title = "上传成功!", type = "success")
    removeModal()
  })
  
  
  ## ~~~~ Tab2: 佣金计算 ~~~~
  computeCommission <- function(item, fee) {
    borrower = toupper(item["Borrower"])
    borrower_trader = item["Borrower Trader"]
    lender = toupper(item["Lender"])
    lender_trader = item["Lender Trader"]
    term = item["Term"]
    volume = as.numeric(item["Volume"])
    depoRepo = tolower(item["Depo.Repo"])
    bidBroker = item["Bid.Broker"]
    askBroker = item["Ask.Broker"]
    net = tolower(item["Net"])
    asOfDate = as.Date(item["AS_OF_DATE"])
    fee_borrower = fee[fee$CounterpartyName==borrower,]
    fee_lender = fee[fee$CounterpartyName==lender,]
    ## handle counterparty with multiple traders
    if (nrow(fee_borrower)>1) {
      fee_borrower = fee_borrower[sapply(fee_borrower$Trader, function(x) grepl(x, borrower_trader, fixed=TRUE)),]
    }
    if (nrow(fee_lender)>1) {
      fee_lender = fee_lender[sapply(fee_lender$Trader, function(x) grepl(x, lender_trader, fixed=TRUE)),]
    }  
    ## handle missing CP (this should not happen since CP is forced to exist)
    if (nrow(fee_borrower)==0 | nrow(fee_lender)==0) {
      ans = list(
        "borrower_commission" = NA,
        "lender_commission" = NA,
        "bidBroker_commission" = NA,
        "askBroker_commission" = NA,
        "bidBroker" = NA,
        "askBroker" = NA,
        "depoRepo" = NA
      )
      return(ans)
    }
    ## convert depoRepo
    if (is.na(depoRepo)) {
      depoRepo = "repo"
    } else if (depoRepo=="拆借") {
      depoRepo = "depo"
    } else if (depoRepo=="买断") {
      depoRepo = "buyout"
    }
    depoRepo = tolower(depoRepo) ## convert to lower case
    ## convert term
    if (is.na(as.numeric(term))){
      term = as.numeric(substr(term, 1, nchar(term)-1)) * 30 ## convert months to days
    } else {
      term = as.numeric(term)
    }
    if (term==1 & weekdays(asOfDate)=="星期五"){
      term = 3
    }
    ## determine rate by type
    if (depoRepo=="buyout") {
      borrower_feeRate = fee_borrower$Buyout
      lender_feeRate = fee_lender$Buyout
    } else if (depoRepo=="depo") {
      borrower_feeRate = fee_borrower$B_Depo
      lender_feeRate = fee_lender$L_Depo
    } else if (depoRepo=="repo") {
      ## determine repo rate
      if (as.numeric(term)>=1 & as.numeric(term) <=7) {
        borrower_feeRate = fee_borrower$B_1D_7D
        lender_feeRate = fee_lender$L_1D_7D
      } else if (as.numeric(term)<=30) {
        borrower_feeRate = fee_borrower$B_8D_1M
        lender_feeRate = fee_lender$L_8D_1M
      } else {
        borrower_feeRate = fee_borrower$B_Over_1M
        lender_feeRate = fee_lender$L_Over_1M
      }
    }
    ## overwrite rate by net
    if (!is.na(net)) {
      borrower_feeRate = 0
      lender_feeRate = 0
    }
    ## compute borrower,lender commision
    if (!is.na(as.numeric(borrower_feeRate))) { ## borrower
      borrower_commission = volume * as.numeric(borrower_feeRate)
    } else {
      if (grepl("d", borrower_feeRate, fixed=TRUE)){
        borrower_feeRate = as.numeric(substr(borrower_feeRate, 1, nchar(borrower_feeRate)-2))
        borrower_commission = volume * borrower_feeRate * term
      } else if (grepl("bp", borrower_feeRate, fixed=TRUE)) {
        borrower_feeRate = as.numeric(substr(borrower_feeRate, 1, nchar(borrower_feeRate)-2))
        borrower_commission = volume*1e8 * borrower_feeRate/10000 * term/360
      } else {
        borrower_commission= NA
      }
    }
    if (!is.na(as.numeric(lender_feeRate))) { ## lender
      lender_commission = volume * as.numeric(lender_feeRate)
    } else {
      if (grepl("d", lender_feeRate, fixed=TRUE)){
        lender_feeRate = as.numeric(substr(lender_feeRate, 1, nchar(lender_feeRate)-2))
        lender_commission = volume * lender_feeRate * term
      } else if (grepl("bp", lender_feeRate, fixed=TRUE)) {
        lender_feeRate = as.numeric(substr(lender_feeRate, 1, nchar(lender_feeRate)-2))
        lender_commission = volume*1e8 * lender_feeRate/10000 * term/360
      } else {
        lender_commission = NA
      }
    }
    ## determine ask.broker
    if (is.na(bidBroker)) {
      bidBroker = fee_borrower$Broker
    }
    if (is.na(askBroker)) {
      askBroker = fee_lender$Broker
    }
    ## determine broker commission
    if (bidBroker=="CC") {
      bidBroker_commission = sum(borrower_commission,lender_commission, na.rm=T) * 0.01
      askBroker_commission = sum(borrower_commission,lender_commission, na.rm=T) * 0.99
    } else if (askBroker=="CC") {
      bidBroker_commission = sum(borrower_commission,lender_commission, na.rm=T) * 0.99
      askBroker_commission = sum(borrower_commission,lender_commission, na.rm=T) * 0.01
    } else {
      bidBroker_commission = sum(borrower_commission,lender_commission, na.rm=T) * 0.5
      askBroker_commission = sum(borrower_commission,lender_commission, na.rm=T) * 0.5
    }
    ## output
    ans = list(
      "borrower_commission" = borrower_commission,
      "lender_commission" = lender_commission,
      "bidBroker_commission" = bidBroker_commission,
      "askBroker_commission" = askBroker_commission,
      "bidBroker" = bidBroker,
      "askBroker" = askBroker,
      "depoRepo" = depoRepo
    )
    return(ans)
  }
  
  observeEvent(input$compute,{
    validate(need(values$ledger_df,"未检测到文件"))
    req(input$ledgerUploadOK)
    
    ledger_df = values$ledger_df
    fee_df = values$fee_df
    ledger_df$Borrower = toupper(ledger_df$Borrower)
    ledger_df$Lender = toupper(ledger_df$Lender)
    fee_df$CounterpartyName = toupper(fee_df$CounterpartyName)
    ledger_borrower_missing = unique(ledger_df$Borrower[!ledger_df$Borrower %in% fee_df$CounterpartyName])
    ledger_lender_missing = unique(ledger_df$Lender[!ledger_df$Lender %in% fee_df$CounterpartyName])
    total_missing = unique(c(ledger_borrower_missing, ledger_lender_missing))
    showModal(
      if(length(total_missing) == 0 ){
        ## call function
        ans_computeCommission = apply(ledger_df, 1, computeCommission, fee = fee_df)
        ledger_df$borrower_commission = unlist(lapply(ans_computeCommission, function(x) x$borrower_commission))
        ledger_df$lender_commission = unlist(lapply(ans_computeCommission, function(x) x$lender_commission))
        ledger_df$bidBroker_commission = unlist(lapply(ans_computeCommission, function(x) x$bidBroker_commission))
        ledger_df$askBroker_commission = unlist(lapply(ans_computeCommission, function(x) x$askBroker_commission))
        ledger_df$bidBroker = unlist(lapply(ans_computeCommission, function(x) x$bidBroker))
        ledger_df$askBroker = unlist(lapply(ans_computeCommission, function(x) x$askBroker))
        ledger_df$depoRepo = unlist(lapply(ans_computeCommission, function(x) x$depoRepo))
        ## rounding numbers
        ledger_df$borrower_commission=round(ledger_df$borrower_commission,4)
        ledger_df$lender_commission=round(ledger_df$lender_commission,4)
        ledger_df$bidBroker_commission=round(ledger_df$bidBroker_commission,4)
        ledger_df$askBroker_commission=round(ledger_df$askBroker_commission,4)
        ## save to reactiveValues
        values$ledger_df_commission <- ledger_df
        modalDialog(
          title = "准备就绪",
          footer = tagList(
            actionButton("commissionComputeOK", "开始计算")
          ), easyClose = TRUE)
      } else {
        modalDialog(
          title = "Error",
          paste0("以下",as.character(length(total_missing)),"家机构无法在费率表中找到！" ),
          HTML("<hr>"),
          paste(total_missing, collapse = ", "),
          easyClose = TRUE
        )
      }
    )
  })
  
  observeEvent(input$commissionComputeOK,{
    removeModal()
    ledger_df_commission = values$ledger_df_commission
    ledger_df_commission_repo = ledger_df_commission[ledger_df_commission$depoRepo=="repo",]
    ledger_df_commission_depo = ledger_df_commission[ledger_df_commission$depoRepo=="depo",]
    ledger_df_commission_buyout = ledger_df_commission[ledger_df_commission$depoRepo=="buyout",]
    ## compute summary table
    output$compute_summary <- renderTable({
      data.frame(
        "Type" = c("Repo","Shibor","Buyout","Total"),
        "Commission" = c(sum(ledger_df_commission_repo$borrower_commission, na.rm=T) + sum(ledger_df_commission$lender_commission, na.rm=T),
                         sum(ledger_df_commission_depo$borrower_commission, na.rm=T) + sum(ledger_df_commission_depo$lender_commission, na.rm=T),
                         sum(ledger_df_commission_buyout$borrower_commission, na.rm=T) + sum(ledger_df_commission_buyout$lender_commission, na.rm=T),
                         sum(ledger_df_commission$borrower_commission, na.rm=T) + sum(ledger_df_commission$lender_commission, na.rm=T)
                         ),
        "Volume" = c(sum(ledger_df_commission_repo$Volume, na.rm=T),
                     sum(ledger_df_commission_depo$Volume, na.rm=T),
                     sum(ledger_df_commission_buyout$Volume, na.rm=T),
                     sum(ledger_df_commission$Volume, na.rm=T)
                     ),
        "Transactions" = c(nrow(ledger_df_commission_repo),
                           nrow(ledger_df_commission_depo),
                           nrow(ledger_df_commission_buyout),
                           nrow(ledger_df_commission)
                           )
      )
    })
    ## show download button on condition
    shinyjs::show("computeDownload")
    ## view by Broker table
    output$viewByBroker <- renderTable({
      ledger_df_commission_bidBroker = ledger_df_commission[,c("bidBroker","bidBroker_commission","Volume")]
      colnames(ledger_df_commission_bidBroker) = c("Broker","Commission","Volume")
      ledger_df_commission_askBroker = ledger_df_commission[,c("askBroker","askBroker_commission","Volume")]
      colnames(ledger_df_commission_askBroker) = c("Broker","Commission","Volume")
      ledger_df_commission_broker = rbind(ledger_df_commission_bidBroker, ledger_df_commission_askBroker)
      ledger_df_commission_broker$Transactions = 1
      res = aggregate(. ~ Broker, ledger_df_commission_broker, sum)
      res$Transactions = as.integer(res$Transactions)
      res[order(res$Commission, decreasing = TRUE),] 
    })
    
    ## potential miscalculated transactions
    output$viewMiscalculated <- renderTable({
      broker_list = values$fee_df$Broker
      ledger_df_miscalculated = ledger_df_commission[
        is.na(ledger_df_commission$borrower_commission) | 
        is.na(ledger_df_commission$borrower_commission) |
        !ledger_df_commission$bidBroker %in% broker_list | 
        !ledger_df_commission$askBroker %in% broker_list,]
      validate(need(nrow(ledger_df_miscalculated)>0, "No potentially miscalculated items."))
      ledger_df_miscalculated
    }, caption="Potentially Miscalculated Items:",
       caption.placement = getOption("xtable.caption.placement", "top"),
       caption.width = getOption("xtable.caption.width", NULL))
  })
  
  
  ## download
  output$computeDownload <- downloadHandler(
    filename = function() {
      paste('Ledger Table Results -', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(values$ledger_df_commission, con, row.names = FALSE, fileEncoding = "GB2312", quote=F)
    }
  )
  
  ## ~~~~ Tab3: 综合分析 ~~~~
  observeEvent(input$analysis, {
    start_date = input$analysisDateRange[1]
    end_date = input$analysisDateRange[2]
    ledger_filenames = list.files("LedgerFileArchive")
    ledger_filenames_selected = ledger_filenames[(ledger_filenames>=paste0("LedgerTable ",start_date,".csv"))&(ledger_filenames<=paste0("LedgerTable ",end_date,".csv"))] 
    
    validate(need(length(ledger_filenames_selected)>0, "No archived ledger files within range."))
    ledger_df = do.call(rbind, lapply(ledger_filenames_selected, function(x) read_csv(paste0("LedgerFileArchive/",x), locale = readr::locale(encoding = "GB2312"), col_types = "cccccddccccD")))

    ## calculate fee
    withProgress(message = 'Generating data', value = 0.6, {
      ans_computeCommission = apply(ledger_df, 1, computeCommission, fee = values$fee_df)
      ledger_df$borrower_commission = unlist(lapply(ans_computeCommission, function(x) x$borrower_commission))
      ledger_df$lender_commission = unlist(lapply(ans_computeCommission, function(x) x$lender_commission))
      ledger_df$bidBroker_commission = unlist(lapply(ans_computeCommission, function(x) x$bidBroker_commission))
      ledger_df$askBroker_commission = unlist(lapply(ans_computeCommission, function(x) x$askBroker_commission))
      ledger_df$bidBroker = unlist(lapply(ans_computeCommission, function(x) x$bidBroker))
      ledger_df$askBroker = unlist(lapply(ans_computeCommission, function(x) x$askBroker))
      ledger_df$depoRepo = unlist(lapply(ans_computeCommission, function(x) x$depoRepo))
      ## rounding numbers
      ledger_df$borrower_commission=round(ledger_df$borrower_commission,4)
      ledger_df$lender_commission=round(ledger_df$lender_commission,4)
      ledger_df$bidBroker_commission=round(ledger_df$bidBroker_commission,4)
      ledger_df$askBroker_commission=round(ledger_df$askBroker_commission,4)      
    })
    ## save to reactiveValues
    values$ledger_df_analysis <- ledger_df

  })
  
  ## ui of counterparty selectInput
  output$analysisCP_ui <- renderUI({
    req(values$fee_df)
    counterparty_names = c("All",sort(unique(values$fee_df$CounterpartyName)))
    selectInput("analysisCP", "Select Counterparties:", choices=counterparty_names, selected="All", multiple=TRUE)
  })
  
  ## analysis plot
  output$analysisPlot <- renderPlotly({
    req(values$ledger_df_analysis)
    df_plot = values$ledger_df_analysis
    df_plot$total_commission = df_plot$borrower_commission + df_plot$lender_commission
    ## filter by selected product
    df_plot = df_plot[df_plot$depoRepo %in% input$analysisProduct,]
    ## filter by selected counterparty
    if (!"All" %in% input$analysisCP) {
      df_plot = df_plot[(toupper(df_plot$Borrower) %in% input$analysisCP) | (toupper(df_plot$Lender) %in% input$analysisCP),]
    }
    
    validate(need(nrow(df_plot)>0, "No data to analysis."))
    ## aggregate by selected measure
    if (input$analysisMeasure == "Commission") {
      df_plot_agg = aggregate(total_commission ~ AS_OF_DATE, df_plot, sum)
      colnames(df_plot_agg)[2] <- "measure"
    } else if (input$analysisMeasure == "Volume") {
      df_plot_agg = aggregate(Volume ~ AS_OF_DATE, df_plot, sum)
      colnames(df_plot_agg)[2] <- "measure"
    } else if (input$analysisMeasure == "Transactions") {
      df_plot_agg = aggregate(Volume ~ AS_OF_DATE, df_plot, length)
      colnames(df_plot_agg)[2] <- "measure"
    }
    
    ## plotly
    plt <- plot_ly(
      data = df_plot_agg,
      x = ~AS_OF_DATE,
      y = ~measure,
      type = 'scatter',
      mode = 'lines+markers'
    )%>% layout(title = paste0("Trend of Total ",input$analysisMeasure),
                yaxis = list(title = input$analysisMeasure),
                xaxis = list(title = "Date"))
    
  })
  
  
  
  
  
    
  ## ~~~~ Tab4: 费率表 ~~~~
  values$fee_df <- read_csv("FeeTable.csv",col_types = cols(), locale = readr::locale(encoding = "GB2312"))
  
  output$feeTable <- renderDT({
    datatable(values$fee_df, editable = "row", extensions = "Buttons", rownames = T,
              options = list(
                dom = "Bfrtip",
                paging = FALSE,
                autoWidth = TRUE,
                fixedColumns = TRUE,
                searching = TRUE,
                scrollX = T,
                buttons = list()
              ))
  })

  proxyFeeTable = dataTableProxy('feeTable')
  observeEvent(input$feeTable_cell_edit, {
    info = input$feeTable_cell_edit
    str(info)  # check what info looks like (a data frame of 3 columns)
    values$fee_df <<- editData(values$fee_df, info)
    replaceData(proxyFeeTable, values$fee_df, resetPaging = FALSE)  # important    
  })
  
  ## save
  observeEvent(input$feeTableSave,{
    ## uppercase counterparty name
    values$fee_df$CounterpartyName <- toupper(values$fee_df$CounterpartyName)
    ## remove spaces in counterparty name
    values$fee_df$CounterpartyName <- unlist(lapply(values$fee_df$CounterpartyName,function(x) gsub('\\s+', '',x)))
    ## remove duplicated rows by counterparty name and trader name (keep last)
    values$fee_df <- unique(values$fee_df, by=c("CounterpartyName","Trader"), fromLast=TRUE)
    ## save to csv
    write.csv(values$fee_df,"FeeTable.csv", row.names = FALSE, fileEncoding = "GB2312", quote=F)
    write.csv(values$fee_df,paste0("FeeTableArchive/FeeTable ",Sys.Date(),".csv"), row.names = FALSE, fileEncoding = "GB2312", quote=F) ## also save to archive
    shinyalert(title = "Saved!", type = "success")
  })
  
  ## download
  output$feeTableDownload <- downloadHandler(
    filename = function() {
      paste('FeeTable ', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      ## uppercase counterparty name
      values$fee_df$CounterpartyName <- toupper(values$fee_df$CounterpartyName)
      ## remove spaces in counterparty name
      values$fee_df$CounterpartyName <- unlist(lapply(values$fee_df$CounterpartyName,function(x) gsub('\\s+', '',x)))
      ## remove duplicated rows by counterparty name and trader name (keep last)
      values$fee_df <- unique(values$fee_df, by=c("CounterpartyName","Trader"), fromLast=TRUE)
      ## save to csv and download
      write.csv(values$fee_df,"FeeTable.csv", row.names = FALSE, fileEncoding = "GB2312", quote=F)  ## also save in download
      write.csv(values$fee_df, con, row.names = FALSE, fileEncoding = "GB2312", quote=F)
    }
  )
  
  ## delete row(s)
  ## this is warning messge for deleting
  observeEvent(input$feeTableDelete,{
    showModal(
      if(length(input$feeTable_rows_selected) >= 1 ){
        modalDialog(
          title = "Warning",
          paste("Are you sure to delete",length(input$feeTable_rows_selected),"row(s)?" ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("feeTableDeleteOK", "Yes")
          ), easyClose = TRUE)
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select row(s) that you want to delete!" ),easyClose = TRUE
        )
      }
    )
  })
  ## delete selected rows when OK
  observeEvent(input$feeTableDeleteOK, {
    values$fee_df = values$fee_df[-input$feeTable_rows_selected,]
    removeModal()
  })

  ## add a row
  ## pop up modal first
  observeEvent(input$feeTableAdd, {
    showModal(modalDialog(
      title = "新增信息",
      textInput("feeTableAdd_CounterpartyName", "CounterpartyName:"),
      textInput("feeTableAdd_trader", "Trader:"),
      textInput("feeTableAdd_B_1D_7D", "B_1D_7D:"),
      textInput("feeTableAdd_B_8D_1M", "B_8D_1M:"),
      textInput("feeTableAdd_B_Over_1M", "B_Over_1M:"),
      textInput("feeTableAdd_L_1D_7D", "L_1D_7D:"),
      textInput("feeTableAdd_L_8D_1M", "L_8D_1M:"),
      textInput("feeTableAdd_L_Over_1M", "L_Over_1M:"),
      textInput("feeTableAdd_B_Depo", "B_Depo:"),
      textInput("feeTableAdd_L_Depo", "L_Depo:"),
      textInput("feeTableAdd_Buyout", "Buyout:"),
      textInput("feeTableAdd_Broker", "Broker:"),
      actionButton("feeTableAddOK", "Add item"),
      easyClose = TRUE, footer = NULL
    ))
  })
  ## add new row when ok
  observeEvent(input$feeTableAddOK, {
    new_row = data.frame(
      CounterpartyName = as.character(input$feeTableAdd_CounterpartyName),
      Trader = as.character(input$feeTableAdd_trader),
      B_1D_7D = as.character(input$feeTableAdd_B_1D_7D),
      B_8D_1M = as.character(input$feeTableAdd_B_8D_1M),
      B_Over_1M = as.character(input$feeTableAdd_B_Over_1M),
      L_1D_7D = as.character(input$feeTableAdd_L_1D_7D),
      L_8D_1M = as.character(input$feeTableAdd_L_8D_1M),
      L_Over_1M = as.character(input$feeTableAdd_L_Over_1M),
      B_Depo = as.character(input$feeTableAdd_B_Depo),
      L_Depo = as.character(input$feeTableAdd_L_Depo),
      Buyout = as.character(input$feeTableAdd_Buyout),
      Broker = as.character(input$feeTableAdd_Broker)
    )
    values$fee_df <- rbind(values$fee_df, new_row)
    removeModal()
  })
  
  
  
      
})