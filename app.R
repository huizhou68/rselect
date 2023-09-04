#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

SplitAlgorithm <- function(total, groupnum) {
  subtotal <- total-groupnum
  if (subtotal < groupnum) {
    shortitems <- groupnum-subtotal
    return(c(rep(1, subtotal), rep(0, shortitems))+1)
  } else if (subtotal == groupnum) {
    return(rep(subtotal/groupnum, groupnum)+1)
  } else if (subtotal > groupnum & (subtotal/groupnum-floor(subtotal/groupnum))<=0.5) {
    groupsize1 <- rep(floor(subtotal/groupnum), groupnum-1)
    groupsize2 <- subtotal - sum(groupsize1)
    groupsize <- c(groupsize1, groupsize2)
    return(groupsize+1)
  } else if (subtotal > groupnum & (subtotal/groupnum-floor(subtotal/groupnum))>0.5) {
    
    groupsize1 <- rep(round(subtotal/groupnum), floor((subtotal-1)/round(subtotal/groupnum)))
    groupsize2 <- subtotal - sum(groupsize1)
    groupsize3 <- groupnum - length(groupsize1) - length(groupsize2)
    groupsize <- c(groupsize1, groupsize2, rep(0, groupsize3))
    return(groupsize+1)
  }
}

# for (i in 100:120) {
#   cat(i, "items in total", "\n")
#   for (j in seq(i)) {
#     print(SplitAlgorithm(i, j))
#     cat("for groups=", j, "the sum is", sum(SplitAlgorithm(i, j)), "\n")
#   }
# }


SelectIndex <- function(names, selected) {
  index <- c()
  i <- 1
  while (i <= length(names)) {
    if (names[i] %in% selected) {
      index <- c(index, i)
    } 
    i <- i+1
  }
  if (is.null(index)) {
    index <- NA
  }
  return(index)
}


SplitVector <- function(names, groups) {
  groupsize <- SplitAlgorithm(total=length(names), groupnum=groups)
  result <- c()
  for (i in groupsize) {
    selected <- sample(names, i, replace=F)
    result <- c(result, "Below is a group", selected)
    selectindex <- SelectIndex(names, selected)
    names <- names[-selectindex]
  }
  return(result)
}


SplitDataframe <- function(dataframe, groups) {
  dataframe$Index <- seq(nrow(dataframe))
  index <- seq(nrow(dataframe))
  groupsize <- SplitAlgorithm(total=nrow(dataframe), groupnum=groups)
  result <- tibble()
  j <- 1
  for (i in groupsize) {
    if (length(index) > 1) {
      selected <- sample(index, i, replace=F)
      # cat("These are the selected:", selected, "; They belong to group", j, "\n")
      selectindex <- SelectIndex(index, selected)
      # cat("These are their index:", selectindex, "\n")
      index <- index[-selectindex]
      # cat("This is the remainder:", index, "\n")
      result <- result %>% bind_rows(tibble(Index=selected, Group=j))
      j <- j+1
    } else {
      selected <- index
      # cat("The last item is selected at index:", selected, "; It belongs to group", j, "\n")
      result <- result %>% bind_rows(tibble(Index=selected, Group=j))
    }
  }
  dataframe <- dataframe %>% inner_join(result, by="Index") %>%
    arrange(Group) %>%
    relocate(Group) %>% 
    select(-Index)
  return(dataframe)
}


ThrowError <- function() {
  ErrorMessage <- "Your selection of the number of groups or individuals has exceeded the data limit!"
  return(tibble(`Error Message`=ErrorMessage)) 
}




# -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
# Define UI 
# -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
ui <- fluidPage(
  titlePanel("A Tool for Randomly Selecting Groups and Individuals"),
  h3("Upload-based Selection"),
  sidebarLayout(
    sidebarPanel(
      fileInput("uploadRoster", "Please upload a roster file (in .csv format)", accept = c(".csv")),
      selectInput("group1", "The number of groups to be selected:", choices = 1:100),
      selectInput("individual1", "The number of individuals to be selected:", choices = 1:100),
      downloadButton("downloadGroupUpload", "Download Groups"),
      downloadButton("downloadIndividualUpload", "Download Individuals")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Upload-based Group Selections", dataTableOutput("GroupsUpload")),
        tabPanel("Upload-based Individual Selections", dataTableOutput("IndividualsUpload"))
      )
    )
  ),
  
  
  h3("Input-based Selection"),
  sidebarLayout(
    sidebarPanel(
      textAreaInput("name", "Please provide names with a comma separating two adjacent persons (e.g., Steve Jobs, Bill Gates, Jeff Bezos)"),
      selectInput("group2", "The number of groups to be selected:", choices = 1:100),
      selectInput("individual2", "The number of individuals to be selected:", choices = 1:100),
      downloadButton("downloadGroupInput", "Download Groups"),
      downloadButton("downloadIndividualInput", "Download Individuals"),
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Input-based Group Selections", dataTableOutput("GroupsInput")),
        tabPanel("Input-based Individual Selections", dataTableOutput("IndividualInput"))
      )
    )
  ),  
)



# -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
# Define server
# -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
server <- function(input, output, session) {
  
  data <- reactive({
    
    req(input$uploadRoster)
    
    ext <- tools::file_ext(input$uploadRoster$name)
    switch(ext,
           csv = vroom::vroom(input$uploadRoster$datapath, delim = ","),
           tsv = vroom::vroom(input$uploadRoster$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv file")
    )
    
  })
  
  
  data1 <- reactive({
    if (as.numeric(input$group1) <= nrow(data())) {
      SplitDataframe(dataframe=data(), groups=as.numeric(input$group1))
    } else {
      ThrowError()
    }
    
  })
  
  output$GroupsUpload <- renderDataTable({
    data1()
  })
  
  output$downloadGroupUpload <- downloadHandler(
    filename = function() {
      "Upload-based Selection of Groups.csv"
    },
    content = function(file) {
      write_csv(data1(), file)
    }
  )
  
  
  data2 <- reactive({
    if (as.numeric(input$individual1) <= nrow(data())) {
      data()[sample(nrow(data()), input$individual1), ]
    } else {
      ThrowError()
    }
    
  })
  
  output$IndividualsUpload <- renderDataTable({
    data2()
  })
  
  output$downloadIndividualUpload <- downloadHandler(
    filename = function() {
      "Upload-based Selection of Individuals.csv"
    },
    content = function(file) {
      write_csv(data2(), file)
    }
  )
  
  
  data3 <- reactive({
    if (as.numeric(input$group2) <= length(str_split(str_replace_all(input$name, ", *", ","), ",")[[1]])) {
      namevector <- SplitVector(names=str_split(str_replace_all(input$name, ", *", ","), ",")[[1]], groups=as.numeric(input$group2))
      myresult <- tibble(Members=namevector)
      myresult$info <- ifelse(myresult$Members=="Below is a group", T, F)
      myresult$Group <- cumsum(myresult$info)
      myresult <- myresult[myresult$info==FALSE, ]
      myresult$info <- NULL
      myresult <- myresult %>% relocate(Group)
      myresult
    } else {
      ThrowError()
    }
    
  })
  
  output$GroupsInput <- renderDataTable({
    data3()
  })
  
  output$downloadGroupInput <- downloadHandler(
    filename = function() {
      "Input-based Selection of Groups.csv"
    },
    content = function(file) {
      write_csv(data3(), file)
    }
  )
  
  
  data4 <- reactive({
    if (input$individual2 <= length(str_split_1(str_replace_all(input$name, ", *", ","), ","))) {
      namevector <- str_split_1(str_replace_all(input$name, ", *", ","), ",")
      result <- tibble(Selected=sample(namevector, input$individual2))
      result
    } else {
      ThrowError()
    }
  })
  
  output$IndividualInput <- renderDataTable({
    data4()
  })
  
  output$downloadIndividualInput <- downloadHandler(
    filename = function() {
      "Input-based Selection of Individuals.csv"
    },
    content = function(file) {
      write_csv(data4(), file)
    }
  )
  
  
}



# -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
# Run the application 
# -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
shinyApp(ui=ui, server=server)
