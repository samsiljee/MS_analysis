library(shiny)
library(DT)

function(input, output, session) {
    
    # Constant variables
    TMT_vector <- c(
        "126",
        paste0(rep(127:134, each = 2), c("N", "C")),
        "135N"
    )
    TMT_Plexes <- list(
        TMTPro_18 = TMT_vector,
        TMTPro_16 = TMT_vector[1:16],
        TMT_11 = TMT_vector[1:11],
        TMT_10 = c(TMT_vector[1:9], "131"),
        TMT_6 = as.character(126:131),
        TMT_2 = c("126", "127")
    )
    
    # Reactive variables
    TMT <- reactive({
        TMT_Plexes[[input$wizardPlexSelected]]
    })
        
    mixtures <- reactive({
        1:input$mixtures
    })
    
    first_two_columns <- reactive({
        expand.grid("Channel" = TMT(), "Mixture" = mixtures())
    })
    
    conditions <- reactiveVal({
        NULL
    })
    
    # Update reactive val to be the length of first_two_columns
    observe({
        conditions({
            rep(NA, nrow(first_two_columns()))
        })
    })
    
    output$table <- DT::renderDataTable({
        datatable(
            data.frame(first_two_columns(), Condition = conditions()),
            options = list(
                dom = "t",
                paging = FALSE,
                ordering = FALSE
            ),
            rownames = FALSE,
            editable = TRUE,
            class = "cell-border stripe"
        )
    })
    
    channels_selected_rows <- reactiveVal(NULL)
    observe({
        channels_selected_rows(input$table_rows_selected)
    })
    
    observeEvent(input$addCondition, {
        if (!is.null(channels_selected_rows())) {
            current_wizard_conditions <- conditions()
            current_wizard_conditions[channels_selected_rows()] <- input$wizardCondition
            conditions(current_wizard_conditions)
        } else {
            showNotification(
                "Please select one or more rows first",
                type = "error",
                duration = NULL,
                closeButton = TRUE
            )
        }
    })
}
