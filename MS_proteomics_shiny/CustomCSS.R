# Script for custom CSS code to modify various elements of the page
# Sam Siljee
# 10th June 2024

# Make the modal pop-up larger
Custom_CSS <- tags$style(
  HTML("
    /* Larger modal pop-up */
    .modal-content {
      width: 180% !important;  /* Adjust the width */
      margin-left: -5%;  /* Adjust the negative margin */
      max-width: 120%;  /* Set a maximum width to prevent excessive width */
    }

    /* CSS for help icon */
    .help-icon {
      margin-left: 5px;
      position: relative;
      top: -2px;
      font-size: 14px;  /* Adjust the size as needed */
    }

    /* CSS for checkbox help container */
    .checkbox-help-container {
      display: inline-flex;
      align-items: center;
    }
  ")
)
