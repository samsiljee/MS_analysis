# Input
# Reactive UI
# Quant methods for Instructions page, disable TMT for DIA-NN or Spectronaut input
output$quant_method_input <- renderUI({
  choices <- if (input$platform == "DIANN" | input$platform == "SN") {
    c("LFQ/DIA" = "LFQ")
  } else {
    c("LFQ/DIA" = "LFQ", "TMT" = "TMT")
  }

  selectInput("quant_method",
    "Quantitation method",
    choices = choices,
    multiple = FALSE
  )
})

# Customise placeholders and button names for specific input
output$psm_input <- renderUI({
  fileInput("PSMs",
    switch(input$platform,
      PD = {
        "PSMs file"
      },
      MQ = {
        "MQ evidence file"
      },
      DIANN = {
        "Report file"
      },
      SN = {
        "Report file"
      }
    ),
    buttonLabel = "Browse",
    placeholder = switch(input$platform,
      PD = {
        "Upload PSMs.txt"
      },
      MQ = {
        "Upload evidence.txt"
      },
      DIANN = {
        "Upload report.tsv"
      },
      SN = {
        "Upload \"check what to put here!\""
      }
    )
  )
})

# Wizard button
output$wizard_launch <- renderUI(
  if (is.null(annot_col())) {
    actionButton("launch_wizard", "Launch annotation wizard")
  }
)

# read in annotations
annot_col <- reactive({
  if ( # Only create annotations if there is data to work with, either uploaded or from wizard
    !is.null(input$annotations) |
      (!is.null(input$channel_annotations) & !is.null(input$run_annotations)) |
      !is.null(wizard_data()) |
      !is.null(TMT_wizard_runs_data())
  ) {
    # Input the annotations file
    df <- switch(input$quant_method,
      LFQ = { # input if LFQ selected
        df <- if (!is.null(wizard_data())) { # Load data from wizard if applicable
          df <- wizard_data()
          df
        } else { # Import and clean uploaded data
          df <- vroom(input$annotations$datapath) %>%
            clean_names(case = "upper_camel")

          # Change back "BioReplicate" column if required
          colnames(df)[grep("BioReplicate", colnames(df), ignore.case = TRUE)] <- "BioReplicate"
          df
        }

        # Create column for PCA plot and heatmap
        df$PcaRef <- str_trim(as.character(df$Run))
        df$PcaRef <- gsub(".", "", df$PcaRef, fixed = TRUE)
        df$PcaRef <- gsub(" ", "", df$PcaRef, fixed = TRUE)

        df
      },
      TMT = { # load annotations for TMT methods
        # Channel annotations
        channel_df <- if (!is.null(TMT_wizard_channels_data())) {
          df <- TMT_wizard_channels_data()
          df
        } else {
          df <- vroom(input$channel_annotations$datapath)
          df
        }

        # Run annotations
        run_df <- if (!is.null(TMT_wizard_runs_data())) {
          df <- TMT_wizard_runs_data()
          df
        } else {
          df <- vroom(input$run_annotations$datapath)
          df
        }

        # Combine and clean names
        df <- clean_names(full_join(run_df, channel_df), case = "upper_camel")

        # Change back "BioReplicate" and "TechRepMixture" column if required
        colnames(df)[grep("BioReplicate", colnames(df), ignore.case = TRUE)] <- "BioReplicate"
        colnames(df)[grep("TechRepMixture", colnames(df), ignore.case = TRUE)] <- "TechRepMixture"

        df$PcaRef <- str_trim(as.character(df$Run))
        df$PcaRef <- gsub(".", "", df$PcaRef, fixed = TRUE)
        df$PcaRef <- gsub(" ", "", df$PcaRef, fixed = TRUE)
        df$PcaRef <- paste(df$PcaRef, df$Channel, sep = "_")

        df
      }
    )

    # Add "Experiment' column for PCA and heatmap labeling
    df <- df %>%
      group_by(Condition) %>%
      mutate(Experiment = paste0(Condition, "_", row_number())) %>%
      ungroup()

    # Change condition to factor for correct labelling in heatmap and PCA
    df$Condition <- as.factor(df$Condition)

    df
  } else {
    NULL # Return NULL if there is no input to prevent errors showing
  }
})

# Read in raw PSM data
raw <- reactive({
  if (!is.null(input$PSMs)) {
    switch(input$platform,
      PD = {
        df <- vroom(input$PSMs$datapath)
        df <- clean_names(df, case = "upper_camel")
        # rename columns as required by `MSstats
        df <- switch(input$quant_method,
          LFQ = {
            mutate(df,
              ProteinGroupAccessions = MasterProteinAccessions,
              PrecursorArea = PrecursorAbundance,
              Run = SpectrumFile
            )
          },
          TMT = {
            mutate(df,
              ProteinGroupAccessions = MasterProteinAccessions,
              Run = SpectrumFile
            )
          }
        )
        df
      },
      MQ = {
        df <- vroom(input$PSMs$datapath)
        if (input$keep_contaminants) {
          df$`Potential contaminant` <- NA
        }
        df
      },
      DIANN = {
        df <- vroom(input$PSMs$datapath)
        df$File.Name <- str_replace(df$File.Name, ".*\\\\", "")
        df$File.Name <- str_replace(df$File.Name, ".raw.mzml$", "")
        df
      },
      SN = {
        df <- vroom(input$PSMs$datapath)
        df
      }
    )
  } else {
    data.frame() # Return a blank data.frame if there is no input to prevent errors showing
  }
})

# Read in proteinGroups for MQ experiments
protein_groups <- reactive({
  if (!is.null(input$proteinGroups)) {
    vroom(input$proteinGroups$datapath)
  } else {
    data.frame()
  }
})

# Generate output
output$annotation_tab <- renderDataTable(annot_col())

output$PSMs_tab <- renderDataTable(raw())

output$proteinGroups_tab <- renderDataTable(protein_groups())
