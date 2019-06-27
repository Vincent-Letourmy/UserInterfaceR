

library(shiny)
library(DT)
library(rhandsontable)

dt_output = function(title, id) {
  fluidRow(column(
    12, h1(paste0('Table ', sub('.*?([0-9]+)$', '\\1', id), ': ', title)),
    hr(), DTOutput(id)
  ))
}
render_dt = function(data, editable = 'cell', server = TRUE, ...) {
  renderDT(data, selection = 'none', server = server, editable = editable, ...)
}

shinyApp(
  ui = fluidPage(
    title = 'Double-click to edit table cells',
    dt_output('yo','costsTab')
  ),
  
  server = function(input, output, session) {
    d1 = iris
    
    output$costsTab <- render_dt(
      resultats <<- data.frame(Prediction=c("No","Yes","No","Yes"),
                               Reality=c("No","No","Yes","Yes"),
                               cost=c(0,5,10,0))
      , list(target = 'row', disable = list(columns = c(1, 2)))
    )
    
    # edit rows but disable columns 1 and 2
    observeEvent(input$changeCost, {
      resultats <<- editData(resultats, input$changeCost, 'costsTab')
    })
    
    
    
    
    output$x1 = render_dt(
      d1, list(target = 'row', disable = list(columns = c(2, 4, 5)))
      )
    
    # edit rows but disable columns 2, 4, 5
    observeEvent(input$x1_cell_edit, {
      d1 <<- editData(d1, input$x1_cell_edit, 'x1')
    })
    
  }
)


