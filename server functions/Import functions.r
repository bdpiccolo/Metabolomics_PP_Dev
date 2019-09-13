
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Sample Meta Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#			

	output$metaIMPORT_RENDER <- renderUI({ 
		metaFILEtype <- input$metaFILEtype
		if(metaFILEtype == "csv") {
			fileInput(inputId='metaINPUT', label=HTML("<p class=\"tsplot_input\"><strong>Meta File Upload:</strong></p>"),
				accept=c('.csv')
			)
		} else {
			if(metaFILEtype == "txt") {
			fileInput(inputId='metaINPUT', label=HTML("<p class=\"tsplot_input\"><strong>Meta File Upload:</strong></p>"),
				accept=c('.txt')
			)
			} else {
				fileInput(inputId='metaINPUT', label=HTML("<p class=\"tsplot_input\"><strong>Meta File Upload:</strong></p>"),
					accept=c('.xlsx', '.xls')
				)
			}
		}
	})

	output$metaEXCELsheet_RENDER <- renderUI({ 
		if (is.null(input$metaINPUT)) {
			return(NULL)
		} else {
			metaFILEtype <- input$metaFILEtype	
			if(metaFILEtype %in% c("xls", "xlsx")) {
				Sheetnames <- excel_sheets(path = input$metaINPUT$datapath)
				list(
					conditionalPanel(
						condition = "input.metaFILEtype == 'xls' || input.metaFILEtype == 'xlsx'",
						pickerInput(inputId="metaSHEETSELECT", label = HTML("<p class=\"tsplot_input\"><strong>Select Excel Sheet Positive:</strong></>"), 
							choices = Sheetnames, selected = Sheetnames[1]
						)
					)					
				)
			} else {
				NULL
			}
		}
	})
		
	metadat <- reactive({
		if (is.null(input$metaINPUT)) {
			return(NULL)
		} else {
			metaFILEtype <- input$metaFILEtype
			metaINPUT <- input$metaINPUT
			if(metaFILEtype == "csv") {
				CSVimport(Input=metaINPUT)
			} else {
				if(metaFILEtype == "txt") {
					TXTimport(Input = metaINPUT)	
				} else {
					SHEET <- input$metaSHEETSELECT	
					EXCELimport(Input = metaINPUT, Sheet=SHEET)
				}
			}
			
		}
	})	 
				
	# output$importTEXT <- renderPrint({
		# metadat()
	# })
	
	output$importselections_RENDER <- renderUI({ 
		req(metadat())
		metadat <- metadat()	
		list(
			div(class="key_select",
				pickerInput(inputId="labelSELECT", label = HTML("<p class=\"tsplot_input\"><strong>Select Label ID:</strong></>"), 
					choices = colnames(metadat), selected = colnames(metadat)[1], multiple = FALSE
				),
				
				pickerInput(inputId="metaSELECT", label = HTML("<p class=\"tsplot_input\"><strong>Select MetaData:</strong></>"), 
					choices = colnames(metadat), selected = colnames(metadat)[2:ncol(metadat)], multiple = TRUE
				),
				actionBttn(inputId = "importMETA", label = "Finalize Sample Meta Selections", style = "unite", color = "default")
			)
		)
	})
	
	output$metadatDT <- DT::renderDataTable({
		if(input$importMETA){
			isolate({
				metadat <- metaDATA()
				metadat <- data.frame(V1 = 1, metadat)
				nrowmetadata <- nrow(metadat)
				if(nrowmetadata > 5) { 
					if(nrowmetadata > 10) { 
						rowopt <- c(5, 10, nrowmetadata)
						plength <- 10
					} else {
						rowopt <- c(5, nrowmetadata)
						plength <- nrowmetadata
					}
				} else {
						rowopt <- nrowmetadata
						plength <- nrowmetadata
				}
						
				datatable(metadat, extensions='Buttons', rownames = FALSE,
					options = list(
						columnDefs = list(list(visible=FALSE, targets=c(0)),
										  list(className = 'dt-center', targets = c(0, c(1:(ncol(metadat)-1))))
							),
						dom = 'lf<"floatright"B>rtip',
						buttons = c('excel', 'pdf', 'csv'
						),
						pageLength = plength,
						lengthMenu = rowopt,
							# searching = TRUE,
						scrollX = TRUE,
						initComplete = JS(
							"function(settings, json) {",
							"$(this.api().table().header()).css({'background-color': '#28a745', 
								'color': '#fff', 'font-size': '25px', 'border-radius': '20px'});",
							"$('body').css({'font-family': 'Arsenal', 'font-size': '25px'});",
							"$('table').css({'text-align': 'center'});",
							"}"
						)
					)
				) %>% 
				  formatStyle(0, backgroundColor = styleInterval(0, c('#fff', '#fff')),target = 'row') %>%
				  formatStyle(0, fontSize = styleInterval(0, c('25px', '25px')),target = 'row') %>%
				  formatStyle(0, textAlign = styleInterval(0, c('center', 'center')),target = 'row') %>%
				  formatStyle(0, color = styleInterval(0, c('#28a745', '#28a745')),target = 'row') 
			})
		} else {
			NULL
		}
	})		

	label_meta_dup_GO <- reactive({
		if(input$importMETA){
			isolate({
				labelSELECT <- input$labelSELECT
				metaSELECT <- input$metaSELECT
				if(setequal(labelSELECT %in% metaSELECT,FALSE)){
					"No Match"
				} else {
					NULL
				}
			})
		} else {
			NULL
		}		
	})
	
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Metabolite Meta Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#			

	output$metaMETABIMPORT_RENDER <- renderUI({ 
		metaMETABFILEtype <- input$metaMETABFILEtype
		metaMETABFILEmode <- input$metaMETABFILEmode
		if(metaMETABFILEmode == FALSE) {
			if(metaMETABFILEtype == "csv") {
				fileInput(inputId='metaMETABINPUT', label=HTML("<p class=\"tsplot_input\"><strong>Metabolite Metadata File Upload:</strong></p>"),
					accept=c('.csv')
				)
			} else {
				if(metaMETABFILEtype == "txt") {
					fileInput(inputId='metaMETABINPUT', label=HTML("<p class=\"tsplot_input\"><strong>Metabolite Metadata File Upload:</strong></p>"),
						accept=c('.txt')
					)
				} else {
					fileInput(inputId='metaMETABINPUT', label=HTML("<p class=\"tsplot_input\"><strong>Metabolite Metadata File Upload:</strong></p>"),
						accept=c('.xls', '.xlsx')
					)
				}
			}
		} else {
			NULL
		}
	})

	output$metaMETABIMPORTPOS_RENDER <- renderUI({ 
		metaMETABFILEtype <- input$metaMETABFILEtype
		metaMETABFILEmode <- input$metaMETABFILEmode
		if(metaMETABFILEmode == FALSE) {
			NULL
		} else {
			if(metaMETABFILEtype == "csv") {
				fileInput(inputId='metaMETABINPUT_POS', label=HTML("<p class=\"tsplot_input\"><strong>Positive Metabolite Metadata File Upload:</strong></p>"),
					accept=c('.csv')
				)
			} else {
				if(metaMETABFILEtype == "txt") {
					fileInput(inputId='metaMETABINPUT_POS', label=HTML("<p class=\"tsplot_input\"><strong>Positive Metabolite Metadata File Upload:</strong></p>"),
						accept=c('.txt')
					)
				} else {
					fileInput(inputId='metaMETABINPUT_POS', label=HTML("<p class=\"tsplot_input\"><strong>Positive Metabolite Metadata File Upload:</strong></p>"),
						accept=c('.xls', '.xlsx')
					)
				}
			}
		}
	})

	output$metaMETABIMPORTNEG_RENDER <- renderUI({ 
		metaMETABFILEtype <- input$metaMETABFILEtype
		metaMETABFILEmode <- input$metaMETABFILEmode
		if(metaMETABFILEmode == FALSE) {
			NULL
		} else {
			if(metaMETABFILEtype == "csv") {
				fileInput(inputId='metaMETABINPUT_NEG', label=HTML("<p class=\"tsplot_input\"><strong>Negative Metabolite Metadata File Upload:</strong></p>"),
					accept=c('.csv')
				)
			} else {
				if(metaMETABFILEtype == "txt") {
					fileInput(inputId='metaMETABINPUT_NEG', label=HTML("<p class=\"tsplot_input\"><strong>Negative Metabolite Metadata File Upload:</strong></p>"),
						accept=c('.txt')
					)
				} else {
					fileInput(inputId='metaMETABINPUT_NEG', label=HTML("<p class=\"tsplot_input\"><strong>Negative Metabolite Metadata File Upload:</strong></p>"),
						accept=c('.xls', '.xlsx')
					)
				}
			}
		}
	})

	output$metaMETABEXCELsheet_RENDER <- renderUI({ 
		if (is.null(input$metaMETABINPUT)) {
			return(NULL)
		} else {
			metaMETABFILEtype <- input$metaMETABFILEtype	
			metaMETABFILEmode <- input$metaMETABFILEmode
			if(metaMETABFILEtype %in% c("xls", "xlsx")) {
				if(metaMETABFILEmode == FALSE) {
					Sheetnames <- excel_sheets(path = input$metaMETABINPUT$datapath)
					list(
						conditionalPanel(
							condition = "input.metaMETABFILEtype == 'xls' || input.metaMETABFILEtype == 'xlsx'",
							pickerInput(inputId="metaMETABSHEETSELECT", label = HTML("<p class=\"tsplot_input\"><strong>Select Excel Sheet Positive:</strong></>"), 
								choices = Sheetnames, selected = Sheetnames[1]
							)
						)					
					)
				} else {		
					NULL
				
				}
			} else {
				NULL
			}
		}
	})
		
	output$metaMETABEXCELsheetPOS_RENDER <- renderUI({ 
		if (is.null(input$metaMETABINPUT_POS)) {
			return(NULL)
		} else {
			metaMETABFILEtype <- input$metaMETABFILEtype	
			metaMETABFILEmode <- input$metaMETABFILEmode
			if(metaMETABFILEtype %in% c("xls", "xlsx")) {
				if(metaMETABFILEmode == FALSE) {
					NULL
				} else {		
					POSSheetnames <- excel_sheets(path = input$metaMETABINPUT_POS$datapath)
					list(
						conditionalPanel(
							condition = "input.metaMETABFILEtype == 'xls' || input.metaMETABFILEtype == 'xlsx'",
							pickerInput(inputId="metaMETABPOSSHEETSELECT", label = HTML("<p class=\"tsplot_input\"><strong>Select Excel Sheet Positive:</strong></>"), 
								choices = POSSheetnames, selected = POSSheetnames[1]
							)
						)					
					)
				
				}
			} else {
				NULL
			}
		}
	})
		
	output$metaMETABEXCELsheetNEG_RENDER <- renderUI({ 
		if (is.null(input$metaMETABINPUT_NEG)) {
			return(NULL)
		} else {
			metaMETABFILEtype <- input$metaMETABFILEtype	
			metaMETABFILEmode <- input$metaMETABFILEmode
			if(metaMETABFILEtype %in% c("xls", "xlsx")) {
				if(metaMETABFILEmode == FALSE) {
					NULL
				} else {		
					NEGSheetnames <- excel_sheets(path = input$metaMETABINPUT_NEG$datapath)
					list(
						conditionalPanel(
							condition = "input.metaMETABFILEtype == 'xls' || input.metaMETABFILEtype == 'xlsx'",
							pickerInput(inputId="metaMETABNEGSHEETSELECT", label = HTML("<p class=\"tsplot_input\"><strong>Select Excel Sheet Negative:</strong></>"), 
								choices = NEGSheetnames, selected = NEGSheetnames[1]
							)
						)					
					)
				
				}
			} else {
				NULL
			}
		}
	})
	
	metaMETABimportdat <- reactive({
		metaMETABFILEmode <- input$metaMETABFILEmode
		if(metaMETABFILEmode == FALSE) {
			if(is.null(input$metaMETABINPUT)) {
				return(NULL)
			} else {
				metaMETABINPUT <- input$metaMETABINPUT
				metaMETABFILEtype <- input$metaMETABFILEtype	
				if(metaMETABFILEtype == "csv") {					
						CSVimport(Input=metaMETABINPUT)
				} else {
					if(metaMETABFILEtype == "txt") {
						TXTimport(Input = metaMETABINPUT)				
					} else {
						SHEET <- input$metaMETABSHEETSELECT						
						EXCELimport(Input = metaMETABINPUT, Sheet=SHEET)
						
					}
				}
				
			}
		} else {
			NULL
		}
	})	 

	metaMETABimportPOSdat <- reactive({
		metaMETABFILEmode <- input$metaMETABFILEmode
		if(metaMETABFILEmode == FALSE) {
			NULL
		} else {
			if(is.null(input$metaMETABINPUT_POS)) {
				NULL
			} else {
				metaMETABINPUT_POS <- input$metaMETABINPUT_POS
				metaMETABFILEtype <- input$metaMETABFILEtype	
				if(metaMETABFILEtype == "csv") {					
						CSVimport(Input=metaMETABINPUT_POS)
				} else {
					if(metaMETABFILEtype == "txt") {
						TXTimport(FILEmode = metaMETABINPUT_POS)				
					} else {
						SHEET <- input$metaMETABPOSSHEETSELECT						
						EXCELimport(Input = metaMETABINPUT_POS, Sheet=SHEET)
						
					}
				}
				
			}
		}
	})	 

	metaMETABimportNEGdat <- reactive({
		metaMETABFILEmode <- input$metaMETABFILEmode
		if(metaMETABFILEmode == FALSE) {
			NULL
		} else {
			if(is.null(input$metaMETABINPUT_NEG)) {
				NULL
			} else {
				metaMETABINPUT_NEG <- input$metaMETABINPUT_NEG
				metaMETABFILEtype <- input$metaMETABFILEtype	
				if(metaMETABFILEtype == "csv") {					
						CSVimport(Input=metaMETABINPUT_NEG)
				} else {
					if(metaMETABFILEtype == "txt") {
						TXTimport(Input = metaMETABINPUT_NEG)				
					} else {
						SHEET <- input$metaMETABNEGSHEETSELECT						
						EXCELimport(Input = metaMETABINPUT_NEG, Sheet=SHEET)
						
					}
				}
				
			}
		}
	})	 

	output$metaMETABselections_RENDER <- renderUI({ 
		metaMETABFILEmode <- input$metaMETABFILEmode
		if(metaMETABFILEmode == FALSE) {
			req(metaMETABimportdat())
			metaMETABDAT <- metaMETABimportdat()
			metaMETABcolnames <- colnames(metaMETABDAT)
			list(
				div(class="key_select",
					pickerInput(inputId="metaMETABlabelSELECT", label = HTML("<p class=\"tsplot_input\"><strong>Select Metabolite Meta Label ID:</strong></>"), 
						choices = metaMETABcolnames, selected = metaMETABcolnames[1], multiple = FALSE
					),
					
					pickerInput(inputId="metaMETABSELECT", label = HTML("<p class=\"tsplot_input\"><strong>Select Metabolite MetaData:</strong></>"), 
						choices = metaMETABcolnames, selected = metaMETABcolnames[2:length(metaMETABcolnames)], multiple = TRUE
					)
				)
			)
		} else {
			NULL
		}
	})
	  
	output$metaMETABselectionsPOS_RENDER <- renderUI({ 
		metaMETABFILEmode <- input$metaMETABFILEmode
		if(metaMETABFILEmode == FALSE) {
			NULL
		} else {
			req(metaMETABimportPOSdat())
			metaMETABDAT <- metaMETABimportPOSdat()
			metaMETABcolnames <- colnames(metaMETABDAT)
			list(
				div(class="key_select",
					pickerInput(inputId="metaMETABPOSlabelSELECT", label = HTML("<p class=\"tsplot_input\"><strong>Select Positive Metabolite Meta Label ID:</strong></>"), 
						choices = metaMETABcolnames, selected = metaMETABcolnames[1], multiple = FALSE
					),
					
					pickerInput(inputId="metaMETABPOSSELECT", label = HTML("<p class=\"tsplot_input\"><strong>Select Positive Metabolite MetaData:</strong></>"), 
						choices = metaMETABcolnames, selected = metaMETABcolnames[2:length(metaMETABcolnames)], multiple = TRUE
					)
				)
			)
		}
	})
	  
	output$metaMETABselectionsNEG_RENDER <- renderUI({ 
		metaMETABFILEmode <- input$metaMETABFILEmode
		if(metaMETABFILEmode == FALSE) {
			NULL
		} else {
			req(metaMETABimportNEGdat())
			metaMETABDAT <- metaMETABimportNEGdat()
			metaMETABcolnames <- colnames(metaMETABDAT)
			list(
				div(class="key_select",
					pickerInput(inputId="metaMETABNEGlabelSELECT", label = HTML("<p class=\"tsplot_input\"><strong>Select Negative Metabolite Meta Label ID:</strong></>"), 
						choices = metaMETABcolnames, selected = metaMETABcolnames[1], multiple = FALSE
					),
					
					pickerInput(inputId="metaMETABNEGSELECT", label = HTML("<p class=\"tsplot_input\"><strong>Select Negative Metabolite MetaData:</strong></>"), 
						choices = metaMETABcolnames, selected = metaMETABcolnames[2:length(metaMETABcolnames)], multiple = TRUE
					)
				)
			)
		}
	})

	output$metaMETABfinalize_RENDER <- renderUI({ 
		metaMETABFILEmode <- input$metaMETABFILEmode
		if(metaMETABFILEmode == FALSE) {
			req(metaMETABimportdat()) 
			actionBttn(inputId = "import_metaMETAB", label = "Finalize Metabolite Meta Selections", style = "unite", color = "default")
		} else {
			req(setequal(c(is.null(metaMETABimportPOSdat()), is.null(metaMETABimportNEGdat())) == FALSE, TRUE)) 
			actionBttn(inputId = "import_metaMETAB", label = "Finalize Metabolite Meta Selections", style = "unite", color = "default")
		}
	})

	output$metaMETABDT <- DT::renderDataTable({
		if(input$import_metaMETAB){
			isolate({
				metaMETABDAT <- metaMETABimportdat()
				metaMETABSELECT <- input$metaMETABSELECT
				metaMETABlabelSELECT <- input$metaMETABlabelSELECT
				metaMETABDATfilter <- metaMETABDAT[,colnames(metaMETABDAT) %in% c(metaMETABlabelSELECT, metaMETABSELECT)]
				metaMETABDAT_fcolnames <- colnames(metaMETABDATfilter)
				metaMETABDAT <- data.frame(V1 = 1, metaMETABDATfilter)
				colnames(metaMETABDAT) <- c("V1", metaMETABDAT_fcolnames)
				nrowmetaMETABDAT <- nrow(metaMETABDAT)
				if(nrowmetaMETABDAT > 5) { 
					if(nrowmetaMETABDAT > 10) { 
						rowopt <- c(5, 10, nrowmetaMETABDAT)
						plength <- 10
					} else {
						rowopt <- c(5, nrowmetaMETABDAT)
						plength <- nrowmetaMETABDAT
					}
				} else {
						rowopt <- nrowmetaMETABDAT
						plength <- nrowmetaMETABDAT
				}
						
				datatable(metaMETABDAT, extensions='Buttons', rownames = FALSE,
					options = list(
						columnDefs = list(list(visible=FALSE, targets=c(0)),
										  list(className = 'dt-center', targets = c(0, c(1:(ncol(metaMETABDAT)-1))))
							),
						dom = 'lf<"floatright"B>rtip',
						buttons = c('excel', 'pdf', 'csv'
						),
						pageLength = plength,
						lengthMenu = rowopt,
							# searching = TRUE,
						scrollX = TRUE,
						initComplete = JS(
							"function(settings, json) {",
							"$(this.api().table().header()).css({'background-color': '#007bff', 
								'color': '#fff', 'font-size': '25px', 'border-radius': '20px'});",
							"$('body').css({'font-family': 'Arsenal', 'font-size': '25px'});",
							"$('table').css({'text-align': 'center'});",
							"}"
						)
					)
				) %>% 
				  formatStyle(0, backgroundColor = styleInterval(0, c('#fff', '#fff')),target = 'row') %>%
				  formatStyle(0, fontSize = styleInterval(0, c('25px', '25px')),target = 'row') %>%
				  formatStyle(0, textAlign = styleInterval(0, c('center', 'center')),target = 'row') %>%
				  formatStyle(0, color = styleInterval(0, c('#007bff', '#007bff')),target = 'row')

			})
		} else {
			NULL
		}
	})		

	output$metaMETABPOSDT <- DT::renderDataTable({
		if(input$import_metaMETAB){
			isolate({
				metaMETABDAT <- metaMETABimportPOSdat()
				metaMETABPOSSELECT <- input$metaMETABPOSSELECT
				metaMETABPOSlabelSELECT <- input$metaMETABPOSlabelSELECT
				metaMETABDATfilter <- metaMETABDAT[,colnames(metaMETABDAT) %in% c(metaMETABPOSlabelSELECT, metaMETABPOSSELECT)]
				metaMETABDAT_fcolnames <- colnames(metaMETABDATfilter)
				metaMETABDAT <- data.frame(V1 = 1, metaMETABDATfilter)
				colnames(metaMETABDAT) <- c("V1", metaMETABDAT_fcolnames)
				nrowmetaMETABDAT <- nrow(metaMETABDAT)
				if(nrowmetaMETABDAT > 5) { 
					if(nrowmetaMETABDAT > 10) { 
						rowopt <- c(5, 10, nrowmetaMETABDAT)
						plength <- 10
					} else {
						rowopt <- c(5, nrowmetaMETABDAT)
						plength <- nrowmetaMETABDAT
					}
				} else {
						rowopt <- nrowmetaMETABDAT
						plength <- nrowmetaMETABDAT
				}
						
				datatable(metaMETABDAT, extensions='Buttons', rownames = FALSE,
					options = list(
						columnDefs = list(list(visible=FALSE, targets=c(0)),
										  list(className = 'dt-center', targets = c(0, c(1:(ncol(metaMETABDAT)-1))))
							),
						dom = 'lf<"floatright"B>rtip',
						buttons = c('excel', 'pdf', 'csv'
						),
						pageLength = plength,
						lengthMenu = rowopt,
							# searching = TRUE,
						scrollX = TRUE,
						initComplete = JS(
							"function(settings, json) {",
							"$(this.api().table().header()).css({'background-color': '#007bff', 
								'color': '#fff', 'font-size': '25px', 'border-radius': '20px'});",
							"$('body').css({'font-family': 'Arsenal', 'font-size': '25px'});",
							"$('table').css({'text-align': 'center'});",
							"}"
						)
					)
				) %>% 
				  formatStyle(0, backgroundColor = styleInterval(0, c('#fff', '#fff')),target = 'row') %>%
				  formatStyle(0, fontSize = styleInterval(0, c('25px', '25px')),target = 'row') %>%
				  formatStyle(0, textAlign = styleInterval(0, c('center', 'center')),target = 'row') %>%
				  formatStyle(0, color = styleInterval(0, c('#007bff', '#007bff')),target = 'row')
			
			})
		} else {
			NULL
		}
	})		

	output$metaMETABNEGDT <- DT::renderDataTable({
		if(input$import_metaMETAB){
			isolate({
				metaMETABDAT <- metaMETABimportNEGdat()
				metaMETABNEGSELECT <- input$metaMETABNEGSELECT
				metaMETABNEGlabelSELECT <- input$metaMETABNEGlabelSELECT
				metaMETABDATfilter <- metaMETABDAT[,colnames(metaMETABDAT) %in% c(metaMETABNEGlabelSELECT, metaMETABNEGSELECT)]
				metaMETABDAT_fcolnames <- colnames(metaMETABDATfilter)
				metaMETABDAT <- data.frame(V1 = 1, metaMETABDATfilter)
				colnames(metaMETABDAT) <- c("V1", metaMETABDAT_fcolnames)
				nrowmetaMETABDAT <- nrow(metaMETABDAT)
				if(nrowmetaMETABDAT > 5) { 
					if(nrowmetaMETABDAT > 10) { 
						rowopt <- c(5, 10, nrowmetaMETABDAT)
						plength <- 10
					} else {
						rowopt <- c(5, nrowmetaMETABDAT)
						plength <- nrowmetaMETABDAT
					}
				} else {
						rowopt <- nrowmetaMETABDAT
						plength <- nrowmetaMETABDAT
				}
						
				datatable(metaMETABDAT, extensions='Buttons', rownames = FALSE,
					options = list(
						columnDefs = list(list(visible=FALSE, targets=c(0)),
										  list(className = 'dt-center', targets = c(0, c(1:(ncol(metaMETABDAT)-1))))
							),
						dom = 'lf<"floatright"B>rtip',
						buttons = c('excel', 'pdf', 'csv'
						),
						pageLength = plength,
						lengthMenu = rowopt,
							# searching = TRUE,
						scrollX = TRUE,
						initComplete = JS(
							"function(settings, json) {",
							"$(this.api().table().header()).css({'background-color': '#007bff', 
								'color': '#fff', 'font-size': '25px', 'border-radius': '20px'});",
							"$('body').css({'font-family': 'Arsenal', 'font-size': '25px'});",
							"$('table').css({'text-align': 'center'});",
							"}"
						)
					)
				) %>% 
				  formatStyle(0, backgroundColor = styleInterval(0, c('#fff', '#fff')),target = 'row') %>%
				  formatStyle(0, fontSize = styleInterval(0, c('25px', '25px')),target = 'row') %>%
				  formatStyle(0, textAlign = styleInterval(0, c('center', 'center')),target = 'row') %>%
				  formatStyle(0, color = styleInterval(0, c('#007bff', '#007bff')),target = 'row')
				
			})
		} else {
			NULL
		}
	})		

	label_metaMETAB_dup_GO <- reactive({
		if(input$import_metaMETAB){
			isolate({	
				metaMETABFILEmode <- input$metaMETABFILEmode
				if(metaMETABFILEmode == FALSE){
					metaMETABlabelSELECT <- input$metaMETABlabelSELECT
					metaMETABSELECT <- input$metaMETABSELECT
					if(setequal(metaMETABlabelSELECT %in% metaMETABSELECT,FALSE)){
						"No Match"
					} else {
						NULL
					}
				} else {
					NULL
				}
			})
		} else {
			NULL
		}		
	})
			
	label_metaMETAB_POS_dup_GO <- reactive({
		if(input$import_metaMETAB){
			isolate({	
				metaMETABFILEmode <- input$metaMETABFILEmode
				if(metaMETABFILEmode == FALSE){
					NULL
				} else {
					metaMETABPOSlabelSELECT <- input$metaMETABPOSlabelSELECT
					metaMETABPOSSELECT <- input$metaMETABPOSSELECT
					if(setequal(metaMETABPOSlabelSELECT %in% metaMETABPOSSELECT,FALSE)){
						"No Match"
					} else {
						NULL
					}
				
				
				}
			})
		} else {
			NULL
		}		
	})
			
	label_metaMETAB_NEG_dup_GO <- reactive({
		if(input$import_metaMETAB){
			isolate({	
				metaMETABFILEmode <- input$metaMETABFILEmode
				if(metaMETABFILEmode == FALSE){
					NULL
				} else {
					metaMETABNEGlabelSELECT <- input$metaMETABNEGlabelSELECT
					metaMETABNEGSELECT <- input$metaMETABNEGSELECT
					if(setequal(metaMETABNEGlabelSELECT %in% metaMETABNEGSELECT,FALSE)){
						"No Match"
					} else {
						NULL
					}
				
				
				}
			})
		} else {
			NULL
		}		
	})
	
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Metabolomics Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
	output$mppIMPORT_RENDER <- renderUI({ 
		mppFILEtype <- input$mppFILEtype
		mppFILEmode <- input$mppFILEmode
		if(mppFILEmode == FALSE) {
			if(mppFILEtype == "csv") {
				fileInput(inputId='mppINPUT', label=HTML("<p class=\"tsplot_input\"><strong>Metabolite Metadata File Upload:</strong></p>"),
					accept=c('.csv')
				)
			} else {
				if(mppFILEtype == "txt") {
					fileInput(inputId='mppINPUT', label=HTML("<p class=\"tsplot_input\"><strong>Metabolite Metadata File Upload:</strong></p>"),
						accept=c('.txt')
					)
				} else {
					fileInput(inputId='mppINPUT', label=HTML("<p class=\"tsplot_input\"><strong>Metabolite Metadata File Upload:</strong></p>"),
						accept=c('.xls', '.xlsx')
					)
				}
			}
		} else {
			NULL
		}
	})

	output$mppIMPORTPOS_RENDER <- renderUI({ 
		mppFILEtype <- input$mppFILEtype
		mppFILEmode <- input$mppFILEmode
		if(mppFILEmode == FALSE) {
			NULL
		} else {
			if(mppFILEtype == "csv") {
				fileInput(inputId='mppINPUT_POS', label=HTML("<p class=\"tsplot_input\"><strong>Positive Metabolite Metadata File Upload:</strong></p>"),
					accept=c('.csv')
				)
			} else {
				if(mppFILEtype == "txt") {
					fileInput(inputId='mppINPUT_POS', label=HTML("<p class=\"tsplot_input\"><strong>Positive Metabolite Metadata File Upload:</strong></p>"),
						accept=c('.txt')
					)
				} else {
					fileInput(inputId='mppINPUT_POS', label=HTML("<p class=\"tsplot_input\"><strong>Positive Metabolite Metadata File Upload:</strong></p>"),
						accept=c('.xls', '.xlsx')
					)
				}
			}
		}
	})

	output$mppIMPORTNEG_RENDER <- renderUI({ 
		mppFILEtype <- input$mppFILEtype
		mppFILEmode <- input$mppFILEmode
		if(mppFILEmode == FALSE) {
			NULL
		} else {
			if(mppFILEtype == "csv") {
				fileInput(inputId='mppINPUT_NEG', label=HTML("<p class=\"tsplot_input\"><strong>Negative Metabolite Metadata File Upload:</strong></p>"),
					accept=c('.csv')
				)
			} else {
				if(mppFILEtype == "txt") {
					fileInput(inputId='mppINPUT_NEG', label=HTML("<p class=\"tsplot_input\"><strong>Negative Metabolite Metadata File Upload:</strong></p>"),
						accept=c('.txt')
					)
				} else {
					fileInput(inputId='mppINPUT_NEG', label=HTML("<p class=\"tsplot_input\"><strong>Negative Metabolite Metadata File Upload:</strong></p>"),
						accept=c('.xls', '.xlsx')
					)
				}
			}
		}
	})

	output$mppEXCELsheet_RENDER <- renderUI({ 
		if (is.null(input$mppINPUT)) {
			return(NULL)
		} else {
			mppFILEtype <- input$mppFILEtype	
			mppFILEmode <- input$mppFILEmode
			if(mppFILEtype %in% c("xls", "xlsx")) {
				if(mppFILEmode == FALSE) {
					Sheetnames <- excel_sheets(path = input$mppINPUT$datapath)
					list(
						conditionalPanel(
							condition = "input.mppFILEtype == 'xls' || input.mppFILEtype == 'xlsx'",
							pickerInput(inputId="mppSHEETSELECT", label = HTML("<p class=\"tsplot_input\"><strong>Select Excel Sheet Positive:</strong></>"), 
								choices = Sheetnames, selected = Sheetnames[1]
							)
						)					
					)
				} else {		
					NULL
				
				}
			} else {
				NULL
			}
		}
	})
		
	output$mppEXCELsheetPOS_RENDER <- renderUI({ 
		if (is.null(input$mppINPUT_POS)) {
			return(NULL)
		} else {
			mppFILEtype <- input$mppFILEtype	
			mppFILEmode <- input$mppFILEmode
			if(mppFILEtype %in% c("xls", "xlsx")) {
				if(mppFILEmode == FALSE) {
					NULL
				} else {		
					POSSheetnames <- excel_sheets(path = input$mppINPUT_POS$datapath)
					list(
						conditionalPanel(
							condition = "input.mppFILEtype == 'xls' || input.mppFILEtype == 'xlsx'",
							pickerInput(inputId="mppPOSSHEETSELECT", label = HTML("<p class=\"tsplot_input\"><strong>Select Excel Sheet Positive:</strong></>"), 
								choices = POSSheetnames, selected = POSSheetnames[1]
							)
						)					
					)
				
				}
			} else {
				NULL
			}
		}
	})
		
	output$mppEXCELsheetNEG_RENDER <- renderUI({ 
		if (is.null(input$mppINPUT_NEG)) {
			return(NULL)
		} else {
			mppFILEtype <- input$mppFILEtype	
			mppFILEmode <- input$mppFILEmode
			if(mppFILEtype %in% c("xls", "xlsx")) {
				if(mppFILEmode == FALSE) {
					NULL
				} else {		
					NEGSheetnames <- excel_sheets(path = input$mppINPUT_NEG$datapath)
					list(
						conditionalPanel(
							condition = "input.mppFILEtype == 'xls' || input.mppFILEtype == 'xlsx'",
							pickerInput(inputId="mppNEGSHEETSELECT", label = HTML("<p class=\"tsplot_input\"><strong>Select Excel Sheet Negative:</strong></>"), 
								choices = NEGSheetnames, selected = NEGSheetnames[1]
							)
						)					
					)
				
				}
			} else {
				NULL
			}
		}
	})
	
	mppimportdat <- reactive({
		mppFILEmode <- input$mppFILEmode
		if(mppFILEmode == FALSE) {
			if(is.null(input$mppINPUT)) {
				return(NULL)
			} else {
				mppINPUT <- input$mppINPUT
				mppFILEtype <- input$mppFILEtype	
				if(mppFILEtype == "csv") {					
						CSVimport(Input=mppINPUT)
				} else {
					if(mppFILEtype == "txt") {
						TXTimport(Input = mppINPUT)				
					} else {
						SHEET <- input$mppSHEETSELECT						
						EXCELimport(Input = mppINPUT, Sheet=SHEET)
						
					}
				}
				
			}
		} else {
			NULL
		}
	})	 

	mppimportPOSdat <- reactive({
		mppFILEmode <- input$mppFILEmode
		if(mppFILEmode == FALSE) {
			NULL
		} else {
			if(is.null(input$mppINPUT_POS)) {
				NULL
			} else {
				mppINPUT_POS <- input$mppINPUT_POS
				mppFILEtype <- input$mppFILEtype	
				if(mppFILEtype == "csv") {					
						CSVimport(Input=mppINPUT_POS)
				} else {
					if(mppFILEtype == "txt") {
						TXTimport(FILEmode = mppINPUT_POS)				
					} else {
						SHEET <- input$mppPOSSHEETSELECT						
						EXCELimport(Input = mppINPUT_POS, Sheet=SHEET)
						
					}
				}
				
			}
		}
	})	 

	mppimportNEGdat <- reactive({
		mppFILEmode <- input$mppFILEmode
		if(mppFILEmode == FALSE) {
			NULL
		} else {
			if(is.null(input$mppINPUT_NEG)) {
				NULL
			} else {
				mppINPUT_NEG <- input$mppINPUT_NEG
				mppFILEtype <- input$mppFILEtype	
				if(mppFILEtype == "csv") {					
						CSVimport(Input=mppINPUT_NEG)
				} else {
					if(mppFILEtype == "txt") {
						TXTimport(Input = mppINPUT_NEG)				
					} else {
						SHEET <- input$mppNEGSHEETSELECT						
						EXCELimport(Input = mppINPUT_NEG, Sheet=SHEET)
						
					}
				}
				
			}
		}
	})	 

	output$mppselections_RENDER <- renderUI({ 
		mppFILEmode <- input$mppFILEmode
		if(mppFILEmode == FALSE) {
			req(mppimportdat())
			mppDAT <- mppimportdat()
			mppcolnames <- colnames(mppDAT)
			list(
				div(class="key_select",
					pickerInput(inputId="mpplabelSELECT", label = HTML("<p class=\"tsplot_input\"><strong>Select Metabolite Meta Label ID:</strong></>"), 
						choices = mppcolnames, selected = mppcolnames[1], multiple = FALSE
					),

					pickerInput(inputId="mppSELECT", label = HTML("<p class=\"tsplot_input\"><strong>Deselect Variables/Metabolites:</strong></>"), 
						choices = c("NONE", mppcolnames), selected = "NONE", multiple = TRUE							
					# pickerInput(inputId="mppSELECT", label = HTML("<p class=\"tsplot_input\"><strong>Select Metabolite MetaData:</strong></>"), 
						# choices = mppcolnames, selected = mppcolnames[2:length(mppcolnames)], multiple = TRUE
					)
				)
			)
		} else {
			NULL
		}
	})

	output$mppselectionsPOS_RENDER <- renderUI({ 
		mppFILEmode <- input$mppFILEmode
		if(mppFILEmode == FALSE) {
			NULL
		} else {
			req(mppimportPOSdat())
			mppDAT_POS <- mppimportPOSdat()
			mppPOScolnames <- colnames(mppDAT_POS)
			list(
				div(class="key_select",
					pickerInput(inputId="mppPOSlabelSELECT", label = HTML("<p class=\"tsplot_input\"><strong>Select Positive Metabolite Meta Label ID:</strong></>"), 
						choices = mppPOScolnames, selected = mppPOScolnames[1], multiple = FALSE
					),
						pickerInput(inputId="mppPOSSELECT", label = HTML("<p class=\"tsplot_input\"><strong>Deselect Positive Variables/Metabolites:</strong></>"), 
						choices = c("NONE", mppPOScolnames), selected = "NONE", multiple = TRUE							
					# pickerInput(inputId="mppPOSSELECT", label = HTML("<p class=\"tsplot_input\"><strong>Select Positive Metabolite MetaData:</strong></>"), 
						# choices = colnames(mppDAT_POS), selected = colnames(mppDAT_POS)[2:ncol(mppDAT_POS)], multiple = TRUE
					)
				)
			)
		}
	})
	  
	output$mppselectionsNEG_RENDER <- renderUI({ 
		mppFILEmode <- input$mppFILEmode
		if(mppFILEmode == FALSE) {
			NULL
		} else {
			req(mppimportNEGdat())
			mppDAT_NEG <- mppimportNEGdat()
			mppNEGcolnames <- colnames(mppDAT_NEG)
			list(
				div(class="key_select",
					pickerInput(inputId="mppNEGlabelSELECT", label = HTML("<p class=\"tsplot_input\"><strong>Select Negative Metabolite Meta Label ID:</strong></>"), 
						choices = mppNEGcolnames, selected = mppNEGcolnames[1], multiple = FALSE
					),
						pickerInput(inputId="mppNEGSELECT", label = HTML("<p class=\"tsplot_input\"><strong>Deselect Negative Variables/Metabolites:</strong></>"), 
						choices = c("NONE", mppNEGcolnames), selected = "NONE", multiple = TRUE					
					# pickerInput(inputId="mppNEGSELECT", label = HTML("<p class=\"tsplot_input\"><strong>Select Negative Metabolite MetaData:</strong></>"), 
						# choices = colnames(mppDAT_NEG), selected = colnames(mppDAT_NEG)[2:ncol(mppDAT_NEG)], multiple = TRUE
					)
				)
			)
		}
	})
	 
	output$mppfinalize_RENDER <- renderUI({ 
		mppFILEmode <- input$mppFILEmode
		if(mppFILEmode == FALSE) {
			req(mppimportdat()) 
			actionBttn(inputId = "import_mpp", label = "Finalize Metabolite Meta Selections", style = "unite", color = "default")
		} else {
			req(setequal(c(is.null(mppimportPOSdat()), is.null(mppimportNEGdat())) == FALSE, TRUE)) 
			actionBttn(inputId = "import_mpp", label = "Finalize Metabolite Meta Selections", style = "unite", color = "default")
		}
	})

	output$mppDT <- DT::renderDataTable({
		if(input$import_mpp){
			isolate({
				mppDAT <- mppimportdat()
				mppSELECT <- input$mppSELECT
				mpplabelSELECT <- input$mpplabelSELECT
				mppSELECTnoID <- mppSELECT[!(mppSELECT %in% mpplabelSELECT)]
				mppDATfilter <- mppDAT[,!(colnames(mppDAT) %in% mppSELECTnoID)]
				mppDAT_fcolnames <- colnames(mppDATfilter)
				mppDAT <- data.frame(V1 = 1, mppDATfilter)
				colnames(mppDAT) <- c("V1", mppDAT_fcolnames)
				nrowmppDAT <- nrow(mppDAT)
				if(nrowmppDAT > 5) { 
					if(nrowmppDAT > 10) { 
						rowopt <- c(5, 10, nrowmppDAT)
						plength <- 10
					} else {
						rowopt <- c(5, nrowmppDAT)
						plength <- nrowmppDAT
					}
				} else {
						rowopt <- nrowmppDAT
						plength <- nrowmppDAT
				}
						
				datatable(mppDAT, extensions='Buttons', rownames = FALSE,
					options = list(
						columnDefs = list(list(visible=FALSE, targets=c(0)),
										  list(className = 'dt-center', targets = c(0, c(1:(ncol(mppDAT)-1))))
							),
						dom = 'lf<"floatright"B>rtip',
						buttons = c('excel', 'pdf', 'csv'
						),
						pageLength = plength,
						lengthMenu = rowopt,
							# searching = TRUE,
						scrollX = TRUE,
						initComplete = JS(
							"function(settings, json) {",
							"$(this.api().table().header()).css({'background-color': '#808080', 
								'color': '#fff', 'font-size': '25px', 'border-radius': '20px'});",
							"$('body').css({'font-family': 'Arsenal', 'font-size': '25px'});",
							"$('table').css({'text-align': 'center'});",
							"}"
						)
					)
				) %>% 
				  formatStyle(0, backgroundColor = styleInterval(0, c('#fff', '#fff')),target = 'row') %>%
				  formatStyle(0, fontSize = styleInterval(0, c('25px', '25px')),target = 'row') %>%
				  formatStyle(0, textAlign = styleInterval(0, c('center', 'center')),target = 'row') %>%
				  formatStyle(0, color = styleInterval(0, c('#808080', '#808080')),target = 'row')

			})
		} else {
			NULL
		}
	})		

	output$mppPOSDT <- DT::renderDataTable({
		if(input$import_mpp){
			isolate({
				mppDAT <- mppimportPOSdat()
				mppPOSSELECT <- input$mppPOSSELECT
				mppPOSlabelSELECT <- input$mppPOSlabelSELECT
				mppPOSSELECTnoID <- mppPOSSELECT[!(mppPOSSELECT %in% mppPOSlabelSELECT)]
				mppDATfilter <- mppDAT[,!(colnames(mppDAT) %in% mppPOSSELECTnoID)]
				mppDAT_fcolnames <- colnames(mppDATfilter)
				mppDAT <- data.frame(V1 = 1, mppDATfilter)
				colnames(mppDAT) <- c("V1", mppDAT_fcolnames)
				nrowmppDAT <- nrow(mppDAT)
				if(nrowmppDAT > 5) { 
					if(nrowmppDAT > 10) { 
						rowopt <- c(5, 10, nrowmppDAT)
						plength <- 10
					} else {
						rowopt <- c(5, nrowmppDAT)
						plength <- nrowmppDAT
					}
				} else {
						rowopt <- nrowmppDAT
						plength <- nrowmppDAT
				}
						
				datatable(mppDAT, extensions='Buttons', rownames = FALSE,
					options = list(
						columnDefs = list(list(visible=FALSE, targets=c(0)),
										  list(className = 'dt-center', targets = c(0, c(1:(ncol(mppDAT)-1))))
							),
						dom = 'lf<"floatright"B>rtip',
						buttons = c('excel', 'pdf', 'csv'
						),
						pageLength = plength,
						lengthMenu = rowopt,
							# searching = TRUE,
						scrollX = TRUE,
						initComplete = JS(
							"function(settings, json) {",
							"$(this.api().table().header()).css({'background-color': '#808080', 
								'color': '#fff', 'font-size': '25px', 'border-radius': '20px'});",
							"$('body').css({'font-family': 'Arsenal', 'font-size': '25px'});",
							"$('table').css({'text-align': 'center'});",
							"}"
						)
					)
				) %>% 
				  formatStyle(0, backgroundColor = styleInterval(0, c('#fff', '#fff')),target = 'row') %>%
				  formatStyle(0, fontSize = styleInterval(0, c('25px', '25px')),target = 'row') %>%
				  formatStyle(0, textAlign = styleInterval(0, c('center', 'center')),target = 'row') %>%
				  formatStyle(0, color = styleInterval(0, c('#808080', '#808080')),target = 'row')
			
			})
		} else {
			NULL
		}
	})		

	output$mppNEGDT <- DT::renderDataTable({
		if(input$import_mpp){
			isolate({
				mppDAT <- mppimportNEGdat()
				mppNEGSELECT <- input$mppNEGSELECT
				mppNEGlabelSELECT <- input$mppNEGlabelSELECT
				mppNEGSELECTnoID <- mppNEGSELECT[!(mppNEGSELECT %in% mppNEGlabelSELECT)]
				mppDATfilter <- mppDAT[,!(colnames(mppDAT) %in% mppNEGSELECTnoID)]
				mppDAT_fcolnames <- colnames(mppDATfilter)
				mppDAT <- data.frame(V1 = 1, mppDATfilter)
				colnames(mppDAT) <- c("V1", mppDAT_fcolnames)
				nrowmppDAT <- nrow(mppDAT)
				if(nrowmppDAT > 5) { 
					if(nrowmppDAT > 10) { 
						rowopt <- c(5, 10, nrowmppDAT)
						plength <- 10
					} else {
						rowopt <- c(5, nrowmppDAT)
						plength <- nrowmppDAT
					}
				} else {
						rowopt <- nrowmppDAT
						plength <- nrowmppDAT
				}
						
				datatable(mppDAT, extensions='Buttons', rownames = FALSE,
					options = list(
						columnDefs = list(list(visible=FALSE, targets=c(0)),
										  list(className = 'dt-center', targets = c(0, c(1:(ncol(mppDAT)-1))))
							),
						dom = 'lf<"floatright"B>rtip',
						buttons = c('excel', 'pdf', 'csv'
						),
						pageLength = plength,
						lengthMenu = rowopt,
							# searching = TRUE,
						scrollX = TRUE,
						initComplete = JS(
							"function(settings, json) {",
							"$(this.api().table().header()).css({'background-color': '#808080', 
								'color': '#fff', 'font-size': '25px', 'border-radius': '20px'});",
							"$('body').css({'font-family': 'Arsenal', 'font-size': '25px'});",
							"$('table').css({'text-align': 'center'});",
							"}"
						)
					)
				) %>% 
				  formatStyle(0, backgroundColor = styleInterval(0, c('#fff', '#fff')),target = 'row') %>%
				  formatStyle(0, fontSize = styleInterval(0, c('25px', '25px')),target = 'row') %>%
				  formatStyle(0, textAlign = styleInterval(0, c('center', 'center')),target = 'row') %>%
				  formatStyle(0, color = styleInterval(0, c('#808080', '#808080')),target = 'row')
				
			})
		} else {
			NULL
		}
	})		

	label_mpp_dup_GO <- reactive({
		if(input$import_mpp){
			isolate({	
				mppFILEmode <- input$mppFILEmode
				if(mppFILEmode == FALSE){
					mpplabelSELECT <- input$mpplabelSELECT
					mppSELECT <- input$mppSELECT
					if(setequal(mpplabelSELECT %in% mppSELECT,FALSE)){
						"No Match"
					} else {
						NULL
					}
				} else {
					NULL
				}
			})
		} else {
			NULL
		}		
	})
			
	label_mpp_POS_dup_GO <- reactive({
		if(input$import_mpp){
			isolate({	
				mppFILEmode <- input$mppFILEmode
				if(mppFILEmode == FALSE){
					NULL
				} else {
					mppPOSlabelSELECT <- input$mppPOSlabelSELECT
					mppPOSSELECT <- input$mppPOSSELECT
					if(setequal(mppPOSlabelSELECT %in% mppPOSSELECT,FALSE)){
						"No Match"
					} else {
						NULL
					}
				
				
				}
			})
		} else {
			NULL
		}		
	})
			
	label_mpp_NEG_dup_GO <- reactive({
		if(input$import_mpp){
			isolate({	
				mppFILEmode <- input$mppFILEmode
				if(mppFILEmode == FALSE){
					NULL
				} else {
					mppNEGlabelSELECT <- input$mppNEGlabelSELECT
					mppNEGSELECT <- input$mppNEGSELECT
					if(setequal(mppNEGlabelSELECT %in% mppNEGSELECT,FALSE)){
						"No Match"
					} else {
						NULL
					}
				
				
				}
			})
		} else {
			NULL
		}		
	})

	# output$wronglabelWARNING <- renderUI({ 	
		# req(pd_label_STOP())
		# pd_label_STOP <- pd_label_STOP()			
		# list(
			# br(),
			# br(),
			# HTML("
				 # <p class=\"import_warning\">A \"_#\" was not detected at the end of column labels in at least 1 of the uploaded files. This suggests that at least 1 file is either not a Promethion file or the column labels are not formatted correctly.  Please upload correct configuration and proceed.</p>
				 # "
			# )
		# )
	# })		

	# output$difflengthWARNING <- renderUI({ 	
		# req(meta_pd_length_STOP())
		# meta_pd_length_STOP <- meta_pd_length_STOP()			
		# list(
			# br(),
			# br(),
			# HTML("
				 # <p class=\"import_warning\">There is a difference between the number of cages in the Promethion file(s) compared to the number of cages provided in the Metadata.  Please upload correct configuration and proceed.</p>
				# "
			# ),
			# HTML(meta_pd_length_STOP)
		# )
	# })		

	output$mppdat_RENDER <- renderUI({ 	
		if(input$import_mpp){
			isolate({
				mppFILEmode <- input$mppFILEmode
				if(mppFILEmode == FALSE){
					# req(meta_pd_length_GO())
					# req(pd_label_GO())
					list(
						HTML("
							 <p class=\"import_input\">Metabolomics Data</p>
							 "
						),
						DT::dataTableOutput("mppDT")		
					)	
				} else {
				
					list(
						HTML("
							 <p class=\"import_input\">Positive Metabolomics Data</p>
							 "
						),
						DT::dataTableOutput("mppPOSDT"),
							br(),
							br(),
						HTML("
							 <p class=\"import_input\">Negative Metabolomics Data</p>
							 "
						),
						DT::dataTableOutput("mppNEGDT")		
					)	
				}
				
			})
		} else {
			NULL
		}
	})		

	output$metadat_RENDER <- renderUI({ 	
		if(input$importMETA){
			isolate({
				if(is.null(label_meta_dup_GO())){
					list(
						br(),
						br(),
						br(),
						br(),
						HTML("
							 <p class=\"import_warning\">There is at least 1 variable that is selected as both an Experiment and a Covariate. Please correct and press the \"Finalize Meta Selections\" button to proceed.</p>
							 "
						)
					)
				} else {
					list(
						HTML("
							 <p class=\"import_input\">Sample Meta Data</p>
							 "
						),
						DT::dataTableOutput("metadatDT")				
					)
				}
			})
		} else {
			NULL
		}		
	})	

	output$metaMETAB_RENDER <- renderUI({ 	
		if(input$import_metaMETAB){
			isolate({
				metaMETABFILEmode <- input$metaMETABFILEmode
				if(metaMETABFILEmode == FALSE){
					if(is.null(label_metaMETAB_dup_GO())){
						list(
							br(),
							br(),
							br(),
							br(),
							HTML("
								 <p class=\"import_warning\">There is at least 1 variable that is selected as both an Experiment and a Covariate. Please correct and press the \"Finalize Meta Selections\" button to proceed.</p>
								 "
							)
						)
					} else {
						list(
							HTML("
								 <p class=\"import_input\">Metabolite Meta Data</p>
								 "
							),
							DT::dataTableOutput("metaMETABDT")				
						)
					}
				} else {
					if(is.null(label_metaMETAB_POS_dup_GO()) | is.null(label_metaMETAB_NEG_dup_GO())){
						list(
							br(),
							br(),
							br(),
							br(),
							HTML("
								 <p class=\"import_warning\">There is at least 1 variable that is selected as both an Experiment and a Covariate. Please correct and press the \"Finalize Meta Selections\" button to proceed.</p>
								 "
							)
						)
					} else {
						list(
							HTML("
								 <p class=\"import_input\">Metabolite Meta Positive Data</p>
								 "
							),
							DT::dataTableOutput("metaMETABPOSDT"),	
							br(),
							br(),		
							HTML("
								 <p class=\"import_input\">Metabolite Meta Positive Data</p>
								 "
							),
							DT::dataTableOutput("metaMETABNEGDT")				
						)
					}
				
				}
			})
		} else {
			NULL
		}		
	})	

	# exper_mass_dup_STOP <- reactive({
		# if(input$importMETA){
			# isolate({
				# experSELECT <- input$experSELECT
				# metaSELECT <- input$metaSELECT
				# if(setequal(experSELECT %in% metaSELECT,FALSE)){
					# NULL
				# } else {
					# "No Match"
				# }
			# })
		# } else {
			# NULL
		# }		
	# })
	
	# label_cage_dup_STOP <- reactive({
		# if(input$importMETA){
			# isolate({
				# experSELECT <- input$experSELECT
				# metaSELECT <- input$metaSELECT
				# if(setequal(experSELECT %in% metaSELECT,FALSE)){
					# NULL
				# } else {
					# "No Match"
				# }
			# })
		# } else {
			# NULL
		# }		
	# })
	
	
	metaDATA <- reactive({
		req(label_meta_dup_GO())
				metaIMPORT <- metadat()		
				experSELECT <- input$experSELECT
				if(length(experSELECT) > 1){
					metaIMPORT[, colnames(metaIMPORT) %in% experSELECT] <- data.frame(lapply(metaIMPORT[, colnames(metaIMPORT) %in% experSELECT], factor))
				} else {
					metaIMPORT[, colnames(metaIMPORT) %in% experSELECT] <- factor(metaIMPORT[, colnames(metaIMPORT) %in% experSELECT])
				}
				metaIMPORT
			
	})
	
	# pd_label_STOP <- reactive({
		# if(input$importDATA){
			# isolate({
				# pd_dat <- pd_dat()
				# metaDATA <- metaDATA()
				

				# if(NA %in% DATcages) {
					# "labels are incorrect"
				# } else {
					# NULL
				# }
			# })
		# } else {
			# NULL
		# }			
	# })	 
	
	
    # observeEvent(input$importDATA, {
      # withCallingHandlers({
		# The 'html' (NOT HTML) function changes the text or HTML inside an element
        # shinyjs::html("Datauploadtext", "")
        # pd_melt()
      # },
        # message = function(m) {
          # shinyjs::html(id = "Datauploadtext", add = TRUE, 
			# html = HTML(paste0("<p class=\"dataupmessaging\">",m$message, "</h3>"))
		  # )
      # })
    # })
	