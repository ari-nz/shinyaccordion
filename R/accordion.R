
#' @export
accordion<- function(inputId,
                     ...,
                     selected = NULL,
                     direction = c('vertical', 'horizontal'),
                     width = NULL,
                     styleOptions = accordionOptions()
){

  direction <- match.arg(direction)

  boxes = list(...)

  boxes = markSelected(boxes, selected)

  boxes = lapply(boxes, function(x){x[[1]]$attribs$name = inputId; x})
  boxes
  css_file = system.file("css/custom-accordion.css", package="shinyaccordion")
  if (file.exists(css_file)) {
    custom_css = paste(readLines(css_file), collapse = " ")
    custom_css = generateCSS(custom_css, options = styleOptions, direction, nboxes = length(boxes), inputId = inputId)
  }





  accordion = htmltools::tags$div(id = inputId,
                                  class = paste("accordion", inputId),
                                  style = if (!is.null(width))  paste0("width: ", validateCssUnit(width), ";"),
                                  boxes)


  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(
        shiny::tags$link(rel="stylesheet", href="css/base-accordion.css")
      )
    ),
    shiny::tags$head(
      shiny::tags$style(
        shiny::HTML(
          custom_css
        )
      )
    ),
    accordion


  )
}


#' @export
accordionBox<- function(title, ..., value = title){

  htmltools::tagList(
    tags$input(type = 'radio', name = 'accordionBox', class="accordion-select"),
    tags$div(class="accordion-title", tags$span(title), `data-value` = title),
    tags$div(class="accordion-content", tags$span(...))

  )
}


#' @export
accordionDefaultTheme<-function(title_background    = "#ffffff"
                                , title_hover         = "#fdfdfd"
                                , title_text_color    = "#7f8787"
                                , title_border_color  = "#eaeaea"
                                , content_background  = "#f7f7f7"
                                , content_text_color  = "#7f8787"
                                , border_color        = "#dedede",
                                ...){
  c(as.list(environment()), list(...))
}




#' @export
accordionOptions<-function(  title_height        = "65px"
                             , title_width         = "65px"
                             , title_font_size     = "15px"
                             , content_padding     = "5px"
                             , content_height      = "280px"
                             , border_radius       = "8px"
                             , border_size         = "1px"
                             , theme = accordionDefaultTheme()
                             , ...
){
  options = c(as.list(environment()), list(...))
  options
}




markSelected<-function(boxes, selected, foundSelected = FALSE){

  markBoxAsSelected <- function(bx){
    bx[[1]] = htmltools::tagAppendAttributes(bx[[1]], `checked`= 'checked')
    bx
  }


  lapply(boxes, function(bx){
    if (foundSelected || is.character(bx)) {

    } else {

      if (is.null(selected)) {

        foundSelected <<- TRUE
        div <- markBoxAsSelected(bx)

      } else {

        tabValue <- bx[[2]]$attribs$`data-value`
        if (identical(selected, tabValue)) {
          foundSelected <<- TRUE
          bx <- markBoxAsSelected(bx)
        }

      }
    }
    return(bx)




  })


}

generateCSS<-function(template, options, direction, nboxes, inputId){
  opts = options



# Make class label --------------------------------------------------------
  inputId = paste0(".", inputId)

# Positioning -------------------------------------------------------------
  title_height         = opts$title_height
  title_width          = opts$title_width
  title_font_size      = opts$title_font_size
  content_padding      = opts$content_padding
  content_height       = opts$content_height
  border_radius        = opts$border_radius
  border_size          = opts$border_size
# Themes ------------------------------------------------------------------
  title_background     = opts$theme$title_background
  title_hover          = opts$theme$title_hover
  title_text_color     = opts$theme$title_text_color
  content_background   = opts$theme$content_background
  content_text_color   = opts$theme$content_text_color
  border_color         = opts$theme$border_color


  strip_num <- function(x){
    as.numeric(gsub("\\D", "", x))
  }


  if(direction == 'horizontal'){

    flex_direction = 'row'
    accordion_height = paste0(strip_num(content_height)+2, "px")



    accordion_title_width = title_width
    accordion_title_height = '100%'

    title_padding_bottom = paste0(ceiling(strip_num(title_width)/2), "px")
    title_padding_top    = paste0(ceiling(strip_num(title_width)/2), "px")
    title_padding_left   = "0px"
    title_padding_right  = "0px"
    title_rotate = '-90'
    title_ms_rotation = '3'

    title_border_color_right   = 'transparent'
    title_border_color_bottom  = opts$theme$title_border_color


    a_content_adjust = nboxes * strip_num(title_width)

    a_content_height        = content_height
    a_content_margin_bottom = paste0("-", content_height)
    a_content_margin_right  = glue::glue("calc(-1 * calc(100% - {a_content_adjust}px))")
    a_content_width         = glue::glue("calc(100% - {a_content_adjust}px)")



  } else if (direction == 'vertical'){

    flex_direction = 'column'
    accordion_height = "auto"

    accordion_title_width = '100%'
    accordion_title_height = title_height

    title_padding_bottom = "0px"
    title_padding_top    = "0px"
    title_padding_left   = paste0(ceiling(strip_num(title_height)/2), "px")
    title_padding_right  = paste0(ceiling(strip_num(title_height)/2), "px")
    title_rotate = '0'
    title_ms_rotation = '0'

    title_border_color_right   = opts$theme$title_border_color
    title_border_color_bottom  = 'transparent'

    a_content_height        = content_height
    a_content_margin_bottom = paste0("-", content_height)
    a_content_margin_right  = "0"
    a_content_width         = "100%"

  } else {
    stop('accordion needs a direction!')
  }









  glue::glue(template, .open = "{{", .close = "}}")
}









