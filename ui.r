
library(DT)
library(shinyjs)
library(V8) 
library(markdown) 
library(shinyWidgets)
library(DT)



jscode <- "
shinyjs.disableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.bind('click.tab', function(e) {
    e.preventDefault();
    return false;
  });
  tab.addClass('disabled');
}

shinyjs.enableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.unbind('click.tab');
  tab.removeClass('disabled');
}
"

shinyUI(
	tagList(       
		tags$head(
			tags$link(
				rel="stylesheet", type="text/css", href="custom.css"
			),
			tags$style(
				HTML(' #sidebar {
					background-color: #E8E8E8;
					}

					'
				), ## Hides warnings on web browser
				type="text/css",
					".shiny-output-error { visibility: hidden; }",
					".shiny-output-error:before { visibility: hidden; }"
				
			)
		), 
		shinyjs::useShinyjs(),
		shinyjs::extendShinyjs(text = jscode),
		# shinythemes::themeSelector(),
		navbarPage("mppR", id="navbar_title",
			theme = shinythemes::shinytheme("spacelab"),  # <--- Specify theme here
			tabPanel("Import", 
				fluidPage(
					column(12,
						sidebarPanel(id="sidebar", width=2,
							HTML(
								 "
								  <div class=\"METAimport_input\">Input Sample Metadata File
									<span class=\"helptip1\">
									  <p>First column defaults as sample IDs, but this can be changed after uploading. Include any metadata for QC samples also.</p>
									</span>
								  </div>
								 "
							),
							
							awesomeRadio(inputId = "metaFILEtype", label = HTML("<p class=\"tsplot_input\"><strong>Type of file:</strong></p>"	), 
								choices = list("CSV" = "csv", "TXT" = "txt","Excel" = "xls"), selected = "csv"
							),
							uiOutput("metaIMPORT_RENDER"),
							uiOutput("metaEXCELsheet_RENDER"),
							uiOutput("importselections_RENDER")
						),
						mainPanel(
							verbatimTextOutput("importTEXT"),
							uiOutput("metadat_RENDER")
						)
					)
				),
				fluidPage(
					column(12,
						sidebarPanel(id="sidebar", width=2,
							HTML(
								 "
								  <div class=\"METAimport_input\">Input Metabolite Metadata File
									<span class=\"helptip1\">
									  <p>First column defaults as Metabolite IDs, but this can be changed after uploading. Include any metadata for Internal Standards also.</p>
									</span>
								  </div>
								 "
							),
							awesomeRadio(inputId = "metaMETABFILEtype", label = HTML("<p class=\"tsplot_input\"><strong>Type of file:</strong></p>"	), 
								choices = list("CSV" = "csv", "TXT" = "txt","Excel" = "xls"), selected = "csv"
							),
							HTML("<p class=\"tsplot_input\">Upload for POS and NEG modes:</p>"),
							switchInput("metaMETABFILEmode", label = "Press",labelWidth = "80px", value=FALSE),	
							uiOutput("metaMETABIMPORT_RENDER"),
							uiOutput("metaMETABEXCELsheet_RENDER"),
							uiOutput("metaMETABselections_RENDER"),
							uiOutput("metaMETABIMPORTPOS_RENDER"),
							uiOutput("metaMETABEXCELsheetPOS_RENDER"),
							uiOutput("metaMETABselectionsPOS_RENDER"),
							uiOutput("metaMETABIMPORTNEG_RENDER"),
							uiOutput("metaMETABEXCELsheetNEG_RENDER"),
							uiOutput("metaMETABselectionsNEG_RENDER"),
							uiOutput("metaMETABfinalize_RENDER")
						),
						mainPanel(
							uiOutput("metaMETAB_RENDER")
						)
					)
				),
				fluidPage(
					column(12,
						sidebarPanel(id="sidebar", width=2,
							HTML(
								 "
								  <div class=\"mppimport_input\">Input Metabolomics Data File
									<span class=\"helptip2\">
									  <p>First column defaults as sample IDs, but this can be changed after uploading. Include all data, including QCs and internal standards.  Dimensions should match Sample and Metabolite lengths.</p>
									</span>
								  </div>
								 "
							),
							awesomeRadio(inputId = "mppFILEtype", label = HTML("<p class=\"tsplot_input\"><strong>Type of file:</strong></p>"	), 
								choices = list("CSV" = "csv", "TXT" = "txt","Excel" = "xls"), selected = "csv"
							),
							HTML("<p class=\"tsplot_input\">Upload for POS and NEG modes:</p>"),
							switchInput("mppFILEmode", label = "Press",labelWidth = "80px", value=FALSE),	
							uiOutput("mppIMPORT_RENDER"),
							uiOutput("mppEXCELsheet_RENDER"),
							uiOutput("mppselections_RENDER"),
							uiOutput("mppIMPORTPOS_RENDER"),
							uiOutput("mppEXCELsheetPOS_RENDER"),
							uiOutput("mppselectionsPOS_RENDER"),
							uiOutput("mppIMPORTNEG_RENDER"),
							uiOutput("mppEXCELsheetNEG_RENDER"),
							uiOutput("mppselectionsNEG_RENDER"),
							uiOutput("mppfinalize_RENDER"),
							textOutput("Datauploadtext")	
						),
						mainPanel(
							br(),
							uiOutput("wronglabelWARNING"),
							uiOutput("difflengthWARNING"),
							uiOutput("mppdat_RENDER")
						)
					)
				)
			
			)
		)
	)
)	
				