

library(shiny)
library(tidyr)
library(leaflet)
library(dplyr)
library(shinydashboard)
library(shinyWidgets)
library(reactable)
library(tableHTML)
library(geosphere)
library(reactlog)
library(shinycssloaders)
library(googlesheets4)
library(shinyalert)
library(shinyjs)
library(crosstalk)

gs4_auth(path = "tiffinary_credentials.json")
for (i in list.files("src") )
{ if(grepl("*.R",i)>0){
  source(paste0("src/",i))
  print(paste0("src/",i))}}

tiffin_list = readRDS("data/tiffin_list.rds")
postal_codes = readRDS("data/postal_codes.rds")

default_markers = tiffin_list %>%distinct(Areas.Delivered,.keep_all=TRUE)%>%select(Lat,Long)%>%distinct()

ui <- fluidPage( tags$style('.container-fluid {
                             background-color: #ffffff;
                              overflow-x: hidden; 
                           }'),
  useShinyjs(),
  #tags$script(type="text/javascript",'window.$crisp=[];window.CRISP_WEBSITE_ID="f4d7d86d-41c0-4229-8217-a3da95d782f9";(function(){d=document;s=d.createElement("script");s.src="https://client.crisp.chat/l.js";s.async=1;d.getElementsByTagName("head")[0].appendChild(s);})();'),
  
tags$head( 
  tags$div(class="container-head", tags$img(class="background", src = "https://tiffinary.com/wp-content/uploads/2021/04/wooden-background-laptop-scaled.jpg", width="100%",height = "100%"),
           LOGO, 
           tags$ul(class = "menu-list",
                         tags$li(style="padding-left:1vw",tags$a(href="#mymap","Find Tiffins",align="center")),
                         tags$li(style="padding-left:1vw",tags$a(href="#hiw","How it works?",align="center")),
                         tags$li(style="padding-left:1vw",tags$a(href="https://tiffinary.com/cart/","Cart",align="center"))),
           tags$div(class="tiffin-img", TIFFIN_IMG,TEXT1,TEXT2),
                          align="center"),
  
  tags$script(
    'function myFunction() {
                            var x = document.getElementById("myDIV");
                            if (x.style.display === "none") {
                            x.style.display = "block";
                            x.style.opacity = "1";
                            } else {
                            x.style.display = "none";
                            }
                            }'),
  tags$div(class="mobile", tags$img(class="backgroundmob", src = "https://tiffinary.com/wp-content/uploads/2021/04/wooden-cover-mob.png"),
           LOGO_MOB, tags$div(class = "menu", onclick="myFunction()",tags$img(src="https://tiffinary.com/wp-content/uploads/2021/04/Logo.png",width ="100%",height="100%"),
                                 tags$div(class="menu-content",id="myDIV",
                                          tags$a(href="#mymap","Find Tiffins",align="left"),
                                          tags$a(href="#hiw","How it works?",align="left"),
                                          tags$a(href="https://tiffinary.com/cart/","Cart",align="left")
                                          ,style="display: none"))
           , tags$div(class = "tiffin-img-mob",tiffin_img_mob,mobTEXT1,mobTEXT2
           ),
           align="center"),
  HEADER_TAGS
),
br(),
br(),
br(),

fluidRow(tags$div(class = "asd" ,
         h2("How it works?",style = "padding-top:15px"),
         align = "center",id="hiw",

style ="background-color: #fff3f1",

tags$div(style ="background-color: #fff3f1",
                    tags$div(tags$img(src = "https://tiffinary.com/wp-content/uploads/2021/05/Step1.png"), br(),
                            tags$p("Find and Compare Tiffin Services using our advance filters"),br(),br(),align = "center",class= "column-mob"),
                   tags$div(tags$img(src = "https://tiffinary.com/wp-content/uploads/2021/04/Step2.png"), 
                            tags$p("Contact the Vendor to customize orders or order on our website"),br(),br(),align = "center",class= "column-mob"),
                   tags$div(tags$img(src = "https://tiffinary.com/wp-content/uploads/2021/04/Step3.png"), 
                            tags$p("Enjoy fresh meals delivered right to your doorstep"),br(),br(),align = "center",class= "column-mob")),

fluidRow(
                    tags$div(style ="background-color: #fff3f1",
                             tags$div(tags$img(src = "https://tiffinary.com/wp-content/uploads/2021/05/Step1.png"),align = "center",class= "column-main"), 
                             tags$div(tags$img(src = "https://tiffinary.com/wp-content/uploads/2021/04/Step2.png"),align = "center",class= "column-main"),
                             tags$div(tags$img(src = "https://tiffinary.com/wp-content/uploads/2021/04/Step3.png"),align = "center",class= "column-main"))),

                   tags$div(tags$div(tags$p("Find and Compare Tiffin Services using our advance filters"),align = "center",class= "column-main"),
                            tags$div(tags$p("Contact the Vendor to customize orders or order through our website"),align = "center",class= "column-main"),
                            tags$div(tags$p("Enjoy fresh & healthy meals delivered right to your doorstep"),align = "center",class= "column-main"))
                   )),
         
          
br(),
br(),
br(),
fluidRow(tags$div(class = "asd" ,h2("Find & Compare Tiffin Services "),align = "center")),
 br(),
fluidRow(box(width="100%", leafletOutput("mymap", height =  "200", width = "100%"), tags$div(id = "main", style = "width: 75%"))),
br(),
fluidRow(  column(12, style = "padding-top: 25px; padding-bottom: 25px; background-color: #fff3f1;",box(width = 1),box(width =3,align = "center",searchInput("search_pin", label = "Enter your Postal Code", width = "80%" ,value = "",btnSearch = icon("search"),btnReset = icon("remove"),resetValue = "")), 
              box(width =3, align = "center", checkboxGroupButtons(
                inputId = "checkGroup",
                label = "Food Specifications",
                choiceValues = c(1,2),
                choiceNames = c("Veg","Non-Veg"),
                status = "default",
                checkIcon = list(
                  yes = icon("ok", 
                             lib = "glyphicon"),
                  no = icon("remove",
                            lib = "glyphicon"))
              )),
          

          box(width =4,align = "center",prettyRadioButtons(
            inputId = "radio",
            label = "Delivery/Pickup", 
            inline = TRUE,
            icon = icon("check"), 
            bigger = TRUE,
            status = "info",
            animation = "jelly",
            choiceNames = c("Delivery", "Pickup"),
            choiceValues = c(1,2)
          ),
                                
                       box(width =12,id = "myBox", sliderInput(
             "slider", ticks = FALSE,
            label = "Distance for pickup:", 
            min = 0, max = 50,
            value = 25)
          ))
          )),
br(),
reactableOutput(outputId = "tiffins", width = "100%")%>% withSpinner(color="#0dc5c1"),
fluidRow(tags$div(align="center",tags$img(src = "https://tiffinary.com/wp-content/uploads/2021/04/move.png",width = "40vw", height="18vh"),tags$h5("Scrolll"))),
  br(),
  br(),
  br(),

fluidRow( tags$div(tags$div(class = "asd" ,h2("Why go for tiffin service?"), align = "center",
                   box(width = 4,  
                       tags$img(src = "https://tiffinary.com/wp-content/uploads/2021/02/who-we-are-2.png"), tags$div(style = "font-size: 17px;", tags$p("Yes we did the math ! You save more with tiffins ! "))), 
                   box(width = 4, 
                       tags$img(src = "https://tiffinary.com/wp-content/uploads/2021/02/who-we-are-1.png"), tags$div(style = "font-size: 17px;",tags$p("Tiffin food is healthy, delicious and reminds of home !"))), 
                   box(width = 4, 
                       tags$img(src = "https://tiffinary.com/wp-content/uploads/2021/02/who-we-are-3.png"), tags$div(style = "font-size: 17px;",tags$p("Save time by getting it delivered at your doorstep !"))) 
)),


fluidRow(br(),tags$div(class = "asd" , align = "center",  #h2("Need Help finding a tiffin service?"),
         tags$div(tags$p(br(),br(),"Tiffinary makes the search for tiffin services a cake walk, but we know you deserve more. So reach out to us and we will personally help you find the best one."),
                  style = "font-size: 17px; margin: 20px; color: #e20387"),
         tags$div(tags$p("+1 (833) 409-0570"),style = "font-size: 20px; "))),style="background-color: #fff3f1;"),
tags$head(tags$style(HTML('

.asd>h2:after{
border-bottom: 2px solid black;
content: "";
width: 6%;
align: center;
display: block;
margin-top: 20px;
margin-bottom: 20px;
}

'))),
br(),
br(),
br(),



fluidRow(FOOTER_TAG),
STYLE_FOOTER,
STYLE
#htmltools::includeHTML("src/whatsapp.html")


)

server <- function(input, output, session) {
  
  
    points <- eventReactive(input$search_pin,{
      tiffin_list %>% filter(Postal.Code == pc_clean(input$search_pin))%>% select(Lat,Long)%>%distinct() })
  

  pickup_data =  reactive({ 
  
  if(input$search_pin=="") {
    return(
      tiffin_list%>%filter(Veg_NonVeg%in%searchvegnonveg(input$checkGroup))%>%
             mutate(Postal.Code=toupper(Postal.Code))%>%  arrange(Location)%>%distinct(Tiffin.Service,.keep_all=TRUE)%>% filter(!is.na(Location))%>%
             arrange(Location)%>%distinct(Tiffin.Service,.keep_all=TRUE)%>%
             select(Image.Files, Tiffin.Service, Location, All_Areas_Delivered, Slug, Daily, Weekly, Monthly, Duration, Delivery.Timing, Veg_NonVeg, Type.of.Cuisine,dist,TiffLat,TiffLong)%>%
             mutate(Slug= paste0("https://tiffinary.com/product/", Slug,"/")) %>% mutate(dist= as.character("Postal Code not provided"))
           )
  } else {
    
    input_Lat = postal_codes%>%filter(Postal.Code==pc_clean(input$search_pin))%>%pull(Lat)
    input_Long = postal_codes%>%filter(Postal.Code==pc_clean(input$search_pin))%>%pull(Long)
    if(length(input_Lat)==0){input_Lat=0}
    if(length(input_Long)==0){input_Long=0}
    p1 = tiffin_list%>%filter(!is.na(Location))%>%distinct(Tiffin.Service,.keep_all=TRUE)%>%arrange(Tiffin.Service)%>%select(TiffLong,TiffLat)
    p2 = c(input_Long,input_Lat)
    only_tiffinswith_location =cbind((tiffin_list%>%filter(!is.na(Location))%>%distinct(Tiffin.Service,.keep_all=TRUE)%>%arrange(Tiffin.Service)%>%select(Tiffin.Service)),
                                     as.data.frame(distVincentyEllipsoid(p1,p2)/1000 ))%>%rename(dist=2)
 
    return(tiffin_list%>%filter(Veg_NonVeg%in%searchvegnonveg(input$checkGroup))%>%
             mutate(Postal.Code=toupper(Postal.Code))%>%  arrange(Location)%>%distinct(Tiffin.Service,.keep_all=TRUE)%>% filter(!is.na(Location))%>%
             select(-dist)%>% left_join(only_tiffinswith_location, by="Tiffin.Service")%>%filter(as.numeric(dist)<=input$slider)%>%
             select(Image.Files, Tiffin.Service, Location, All_Areas_Delivered, Slug, Daily, Weekly, Monthly, Duration, Delivery.Timing, Veg_NonVeg, Type.of.Cuisine,dist,TiffLat,TiffLong)%>%
             arrange(dist)%>%mutate(Slug= paste0("https://tiffinary.com/product/", Slug,"/"))%>%mutate(dist = paste0(as.integer(dist)," kms")))
  }
  })
  

delivery_data = reactive({
    if(input$search_pin=="") {
      tiffin_list%>%filter(Veg_NonVeg%in%searchvegnonveg(input$checkGroup))%>%filter(Pickup.Delivery%in%pickup_delivery(input$radio))%>%
        mutate(Postal.Code=toupper(Postal.Code))%>% mutate(check = ifelse(is.na(Radius_kms)| dist<=Radius_kms, 1,0))%>%filter(check == 1)%>%
        select(Image.Files, Tiffin.Service, Location, All_Areas_Delivered, Slug, Daily, Weekly, Monthly, Duration, Delivery.Timing, Veg_NonVeg, Type.of.Cuisine,dist,TiffLat,TiffLong)%>%
        arrange(Location)%>%distinct(Tiffin.Service,.keep_all=TRUE)%>% 
        mutate(Slug= paste0("https://tiffinary.com/product/", Slug,"/"))
    } else {
     tiffin_list%>%filter(Postal.Code == pc_clean(input$search_pin))%>%filter(Veg_NonVeg%in%searchvegnonveg(1))%>%
        mutate(Postal.Code=toupper(Postal.Code))%>% mutate(check = ifelse(is.na(Radius_kms)| dist<=Radius_kms, 1,0))%>%filter(check == 1)%>%
        select(Image.Files, Tiffin.Service, Location, All_Areas_Delivered, Slug, Daily, Weekly, Monthly, Duration, Delivery.Timing, Veg_NonVeg, Type.of.Cuisine,dist,TiffLat,TiffLong)%>%
        arrange(Location)%>%distinct(Tiffin.Service,.keep_all=TRUE)%>% 
        mutate(Slug= paste0("https://tiffinary.com/product/", Slug,"/"))
      } 
    })

pickup_no_pin_data = reactive({
  tiffin_list%>%filter(Veg_NonVeg%in%searchvegnonveg(input$checkGroup))%>%filter(Pickup.Delivery%in%pickup_delivery(input$radio))%>%
                   mutate(Postal.Code=toupper(Postal.Code))%>% mutate(check = ifelse(is.na(Radius_kms)| dist<=Radius_kms, 1,0))%>%filter(check == 1)%>%
                   arrange(Location)%>%distinct(Tiffin.Service,.keep_all=TRUE)                                                                                
})

all_delivery_data = tiffin_list %>%distinct(Areas.Delivered,.keep_all=TRUE)%>%select(Lat,Long)%>%distinct()
observe({
  if(input$radio == 1){
    output$mymap <- renderLeaflet({
      leaflet() %>%addTiles()%>% 
        addMarkers(lng = points()$Long, lat= points()$Lat ) %>% 
        addMarkers( lng = all_delivery_data$Long, lat = all_delivery_data$Lat, icon=LogoIcon ) })
  } else {
    if(input$search_pin==""){
      output$mymap <- renderLeaflet({
        leaflet()%>%addTiles()%>% setView(lat = 43.651070, lng = -79.347015, zoom = 7)%>%
          addMarkers(lng = points()$Long, lat= points()$Lat ) %>% 
          addMarkers( lng = pickup_no_pin_data()$TiffLong, lat = pickup_no_pin_data()$TiffLat, icon=LogoIcon )
        })
    } else {
      output$mymap <- renderLeaflet({
        leaflet() %>%addTiles()%>% 
          addMarkers(lng = points()$Long, lat= points()$Lat ) %>% 
          addMarkers( lng = pickup_data()$TiffLong, lat = pickup_data()$TiffLat, icon=LogoIcon ) }) 
    }  
  } 
})


  
observeEvent(input$radio, {
  
  if(input$radio == 1){
    shinyjs::hide(id = "myBox")
    output$tiffins = renderReactable({
      Delivery_Table(delivery_data())
    })
  }else{
    shinyjs::show(id = "myBox")
    output$tiffins = renderReactable({
      Pickup_Table(pickup_data())
    })
    }
}) 


  
  observe({
    # Take a dependency on input$goButton
    if (input$goButton == 0){
      return(NULL)}else{
        updateTextInput(session ,"text","Enter Email", NA)
        
        # Use isolate() to avoid dependency on input$goButton
        isolate({
          EmailMe(as.data.frame(data.frame(Email = input$text)))
          showModal(modalDialog( 
            title = "Thank you!",
            "Welcome to Team Tiffinary!",
            easyClose = TRUE, fade=TRUE
          ))
        })
      }
  })
}

shinyApp(ui, server)
