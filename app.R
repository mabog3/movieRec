#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#library(shiny)
library(irlba)
library(lsa)
library(rsconnect)

print("libs")
load('data/reconstr.RData')

load('data/cols.RData')
print("loaded")
# 
k <- 10
VV <- irlba(reconstr$fit, nu = k, nv = k)$v

T <- t(VV)
Sim <- cosine(T)#the specified similarity measure is the Cosine similarity measure
diag(Sim) <- 0 #remove same i = j, which are automatically 1

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
  titlePanel("Movie Recommendation"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      selectizeInput("var",
                     inputId = "movieSelect",
                     label = "Choose a movie you enjoyed",
                     choices = NULL),
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of movies:",
                  min = 1,
                  max = 50,
                  value = 5)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      p("Method: using the ratings data from the MovieLens dataset (where columns are movies, rows are reviewers, and entries are ratings), the program first imputes missing rating values via repeated rank-10 approximation to minimize squared loss between the estimate matrix and nonempty entries of the original matrix. Then, the right singular vectors of the SVD of the resultant approximation represent movies, and the 'similarity' of any two movies with respect to user preferences is taken to be the cosine similarity of their right singular vectors."),
      
      h3("You're likely to also enjoy: "),
      # Output: Table ----
      tableOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  updateSelectizeInput(session, 'movieSelect', choices = cols, server = TRUE)
  
  output$distPlot <- renderTable({
    print("started plot")
    col <- match(input$movieSelect, cols)
    toySims <- Sim[, col]
    numMovies <- input$bins
    digits = 4
    print("digits = 3")
    lst <- sort(toySims, index.return=TRUE, decreasing=TRUE)
    movs <- cols[lst$ix][1:numMovies]
    rats <- lst$x[1:numMovies]

    df <- data.frame(movs, rats)
    print("df")
    colnames(df) <- c('Movies', 'Similarity')
    
    df
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

#CODE FOR PERFORMING SVD APPROXIMATION: 
# initialize <- function(mat){
#   # get column means ignoring NAs
#   # ave.rat <- colMeans(mat,na.rm = TRUE)
#   # fill NAs by average movie rating
#   for(j in 1:ncol(mat)){
#     mat[is.na(mat[,j]),j] <- 0 # ave.rat[j]
#   }
#   return(mat)
# }
# maxim <- function(mat,k){
#   # temp <- svd(mat)
#   temp<- irlba(mat, nv = k)
#   return(list(U = temp$u[,1:k],
#               D = temp$d[1:k],
#               V = temp$v[,1:k],
#               mat.hat = temp$u[ ,1:k] %*% diag(temp$d[1:k]) %*% t(temp$v[,1:k])))
# }
# 
# recommender <- function(mat, num_steps, k){
#   # initialize loss function tracking
#   loss <- rep(NA,num_steps)
#   # run EM algorithm and save loss
#   ind.known <- !is.na(mat)
#   mat2 <- initialize(mat)
#   for (j in 1:num_steps){
#     mat2 <- maxim(mat2, k)$mat.hat
#     loss[j] <- sum((mat2[ind.known] - mat[ind.known])^2)
#     mat2[ind.known] <- mat[ind.known]
#     print(j)
#   }
#   return(list(loss= loss, fit = mat2))
# }
# 
# k <- 10
# reconstr <- recommender(ratings,num_steps = 200, k = k)