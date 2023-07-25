# Comparison

# Reactive UI
output$select_numerator <- renderUI({
    selectInput("numerator", "Numerator/s",
                choices = conditions(),
                multiple = TRUE)
})

output$select_denominator <- renderUI({
    selectInput("denominator", "Denominator/s",
                choices = setdiff(conditions(), input$numerator),
                multiple = TRUE)
})

# Reactive variables
conditions <- reactive({
    levels(
        switch(input$quant_method,
               LFQ = {
                   MSstats_processed()$ProteinLevelData$GROUP
               },
               TMT = {
                   MSstats_processed()$ProteinLevelData$Condition
               })
    )
})

# Define comparison_matrix as a reactiveValues object
c_vals <- reactiveValues(matrix = NULL, comparison_names = character())

# Initialising the comparison matrix on uploading annotations file
observeEvent(input$go_process, {
    add_comparison()
})

# Define function to add a row
add_comparison <- function() {
    
    # Read the values used to create the row
    row <- ifelse(
        conditions() %in% input$numerator,
        1,
        ifelse(
            conditions() %in% input$denominator,
            -1,
            0))
    
    # Update the row names
    c_vals$comparison_names <- if (is.null(c_vals$matrix)) {
        "First_name_to_be_removed"
    } else {
        c(c_vals$comparison_names, input$comparison_name)
    }
    
    # Update the matrix
    c_vals$matrix <- if (is.null(c_vals$matrix)) {
        c_vals$matrix <- matrix(NA,  nrow = 1, ncol = length(row))
    } else {
        c_vals$matrix <- rbind(c_vals$matrix, matrix(row, nrow = 1, ncol = length(row)))
    }
    
    # Update the column names
    colnames(c_vals$matrix) <- conditions()
    
    #Update the row names
    rownames(c_vals$matrix) <- c_vals$comparison_names
    
    return(c_vals$matrix)
}

# Update the comparison matrix when add_comparison is clicked
comparison_matrix_updated <- eventReactive(input$add_comparison, {
    add_comparison()
})

# Run the comparison function
MSstats_test <- eventReactive(input$go_compare, {
    # Path for log files
    compare_log_file_path <- file.path("logs", "compare_log.txt")
    
    switch(input$quant_method,
           LFQ = {
               groupComparison(
                   contrast.matrix = if (input$contrast_method == "pairwise") {
                       "pairwise"
                   } else {
                       comparison_matrix_updated()[-1, , drop = FALSE]
                   },
                   data = MSstats_processed(),
                   save_fitted_models = input$save_fitted_models,
                   log_base = 2,
                   use_log_file = TRUE,
                   log_file_path = compare_log_file_path)},
           
           TMT = {
               groupComparisonTMT(
                   contrast.matrix = if (input$contrast_method == "pairwise") {
                       "pairwise"
                   } else {
                       comparison_matrix_updated()[-1, , drop = FALSE]
                   },
                   data = MSstats_processed(),
                   moderated = input$moderated,
                   adj.method = input$adj.method,
                   remove_norm_channel = input$remove_norm_channel,
                   remove_empty_channel = TRUE,
                   save_fitted_models = input$save_fitted_models,
                   use_log_file = TRUE,
                   log_file_path = compare_log_file_path)})
})

# Results of comparison, and adding up/downregulation
MSstats_comparison_results <- reactive({
    MSstats_test()$ComparisonResult %>%
        mutate(Dif = ifelse(
            MSstats_test()$ComparisonResult$log2FC > input$FC_threshold & MSstats_test()$ComparisonResult$adj.pvalue < input$pvalue_threshold,
            "Upregulated",
            ifelse(
                MSstats_test()$ComparisonResult$log2FC < -input$FC_threshold & MSstats_test()$ComparisonResult$adj.pvalue < input$pvalue_threshold,
                "Downregulated",
                "Not significant")))
})

MSstats_results <- reactive({
    if(input$filter_results & input$quant_method == "LFQ") {
        MSstats_comparison_results()[-which(MSstats_comparison_results()$log2FC == Inf | MSstats_comparison_results()$log2FC == -Inf),]
    } else {
        MSstats_comparison_results()
    }
})

# Output
output$comparison_matrix_tab <- renderTable(comparison_matrix_updated()[-1, , drop = FALSE], rownames = TRUE)

output$results_tab <- renderDataTable({
    switch(input$results_tab_view,
           ComparisonResult = MSstats_results(),
           ModelQC = MSstats_test()$ModelQC)
})

output$outliers <- renderText(paste("There are",
                                    length(base::which(MSstats_comparison_results()$log2FC == Inf | MSstats_comparison_results()$log2FC == -Inf)),
                                    "results with infinite fold-change.",
                                    sep = " "))