# UI page


# Server side code for comparison



#Turning the comparison_matrix dataframe into a contrast_matrix named matrix
contrast_matrix <- reactive({
  # Extract names
  row_names <- as.vector(comparison_matrix[,1])
  col_names <- colnames(comparison_matrix[,-1])
  
  # Extract numeric values and convert to matrix
  comparison_matrix_numeric <- as.matrix(sapply(comparison_matrix[-1], as.numeric))
  
  # Set names
  rownames(comparison_matrix_numeric) <- row_names
  colnames(comparison_matrix_numeric) <- col_names
  
  return(comparison_matrix_numeric)
})


# chatGPT code
comparison_matrix <- NULL
makeReactiveBinding("comparison_matrix")

observeEvent(input$annotations, {
  
  # Generate empty matrix
  comparison_matrix <- reactiveValues(
    matrix = matrix(nrow = 0, ncol = num_conditions()),
    num_rows = 0)
  
  # Add row to matrix
  observeEvent(input$add_comparison, {
    row <- c(ifelse(
      conditions() %in% input$numerator,
      1,
      ifelse(
        conditions() %in% input$denominator,
        -1,
        0)))
    comparison_matrix$num_rows <- comparison_matrix$num_rows + 1
    comparison_matrix$matrix <- rbind(comparison_matrix$matrix, row)
  })
  
  # Set row and column names
  output$comparison_matrix <- renderPrint({
    colnames(comparison_matrix$matrix) <- conditions()
    rownames(comparison_matrix$matrix) <- c(input$comparison_name, rep("", nrow(comparison_matrix$matrix) - 1))
    print(comparison_matrix$matrix)
  })
  
})