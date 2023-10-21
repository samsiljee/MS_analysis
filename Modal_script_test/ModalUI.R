modalUI <- function() {
    fluidPage(
        checkboxInput("test", "Test"),
        textOutput("testing_text")
    )
}
