

searchvegnonveg = function(x)
{
  if(length(x)==0 | length(x)>1){return(c("V.png","VNV.png","NV.png"))}else{
  if(x==1){return(c("V.png","VNV.png"))} 
  if(x==2){return(c("NV.png","VNV.png"))} }
  # if(length(x)==0){return(c("V.png","VNV.png","NV.png"))}else{
  #   if(length(x)>1){return("VNV.png")}else{
  #     if(x==1){return(c("V.png")} else{
  #       return("NV.png")}}}
}


#clean pin code
pc_clean = function(pc){
  pc=toupper(trimws(gsub(" ","",pc)))
final_pc = paste0(substr(pc,1,3)," " , substr(pc,nchar(pc)-2, nchar(pc)))
return(final_pc)
}

#icons

LogoIcon <- icons(
  iconUrl = "www/Logo.png",
  iconWidth = 15, iconHeight = 15,
  shadowUrl = "www/Logo.png",
  shadowWidth = 15, shadowHeight = 15
)

#Email 

EmailMe = function(info){
  mail_list = read_sheet("https://docs.google.com/spreadsheets/d/1Tgy2l4nbZi5pWqjDmUvyYDCqYWvT8PuFok3U_CktFFY/edit?usp=sharing",sheet = "Email List")
  colnames(info) = colnames(mail_list)
  mail_list = as.data.frame(rbind(mail_list,info))
  sheet_write(data = mail_list,ss = "https://docs.google.com/spreadsheets/d/1Tgy2l4nbZi5pWqjDmUvyYDCqYWvT8PuFok3U_CktFFY/edit?usp=sharing",sheet = "Email List")
}

pickup_delivery = function(x){
  if(x==1){return(c("D","PD"))}  else{return(c("P","PD"))}
}

sticky_style <- list(position = "sticky", left = 0, background = "#fff", zIndex = 1,
                     borderRight = "1px solid #eee")

#Reactable

Delivery_Table = function(final_data){
  slug=final_data%>%select(Slug)
  big_table=final_data%>%select(Image.Files,Tiffin.Service,Duration,Delivery.Timing,Type.of.Cuisine,Slug,All_Areas_Delivered)
  
  reactable(big_table,searchable = TRUE, theme = reactableTheme(searchInputStyle = list(width = "100%")), defaultSorted = "Tiffin.Service" ,defaultPageSize = 5, highlight = TRUE, 
                 bordered = TRUE, class = "my-tbl", rowClass = "my-row",columns = list(
    
  Image.Files = colDef(align = "center",  name = "", width = 70, style = sticky_style,
                       headerStyle = sticky_style,
                       cell = function(value) {
                         img_src <- knitr::image_uri(sprintf("www/%s", value))
                         image <- img(src = img_src, height = "50px")
                         tagList(
                           div(style = list( width = "50px"), image)
                         )}
                       ),
  Type.of.Cuisine = colDef(name = "Type of Cuisine", align = "center", width=150),
  Duration = colDef(align = "center",width = 150),
  Delivery.Timing = colDef(name = "Delivery Timings", align = "center",width = 150),
  All_Areas_Delivered = colDef(name = "Delivery Areas", align = "center",width = 500),
  Tiffin.Service = colDef(align = "center", width = 150, style = sticky_style,
                          headerStyle = sticky_style,
                          header = HTML("<div style='color:#e20387;font-size:15px'>Tiffin <span style='color:#ffbf32';>Services</span></div>"),
                          #name = "Tiffin Services", 
                          html = TRUE, cell = function(value,index) {
                            sprintf('<a href="%s">%s</a>', slug$Slug[index], value)}),
  Slug = colDef(
    name = "",
    sortable = FALSE, align = "center", width = 200, html = TRUE, 
    cell = function(value) { tags$a(class = "det_btn",href = value, "Menu & Contact") 
      }#tags$form(tags$input( type = "button",onclick=paste0("window.location.href='",value,"'"),value ="Click")) }
  )),
  details = function(index) {
    small_table <- filter(final_data, Tiffin.Service == big_table$Tiffin.Service[index]) %>% select(-Tiffin.Service)%>%select(Daily, Weekly,Monthly, Veg_NonVeg)
    tbl <- reactable(small_table, bordered =FALSE,outlined = TRUE, highlight = TRUE, fullWidth = FALSE
                    ,columns = list(
                     Veg_NonVeg = colDef(align = "center",  name = "",
                                         cell = function(value) {
                                           img_src <- knitr::image_uri(sprintf("www/%s", value))
                                           image <- img(src = img_src, height = "35px")
                                           tagList(
                                             div(style = list( width = "35px"), image)
                                           )}),
                     Daily = colDef(format = colFormat(currency = "CAD",separators = TRUE, digits = 0)),
                     Weekly = colDef(format = colFormat(currency = "CAD",separators = TRUE, digits = 0)),
                     Monthly = colDef(format = colFormat(currency = "CAD",separators = TRUE, digits = 0)))
                     )
    htmltools::div(style = list(margin = "12px 45px"), tbl)

  },

  onClick = "expand",
  rowStyle = list(cursor = "pointer")
  )
}


# Pickup Table

Pickup_Table = function(final_data){
  slug=final_data%>%select(Slug)
  big_table=final_data%>%select(Image.Files,Tiffin.Service,Duration,Delivery.Timing,Type.of.Cuisine,Slug,Location,dist)%>%filter(!is.na(Location))%>%mutate(Location = pc_clean(Location))
  
  reactable(big_table,searchable = TRUE, theme = reactableTheme(searchInputStyle = list(width = "100%", align="left")), defaultPageSize = 5, highlight = TRUE, 
            bordered = TRUE, class = "my-tbl", rowClass = "my-row",columns = list(
              
              Image.Files = colDef(align = "center",  name = "", width = 70, style = sticky_style,
                                   headerStyle = sticky_style,
                                   cell = function(value) {
                                     img_src <- knitr::image_uri(sprintf("www/%s", value))
                                     image <- img(src = img_src, height = "50px")
                                     tagList(
                                       div(style = list( width = "50px"), image)
                                     )}
              ),
              Type.of.Cuisine = colDef(name = "Type of Cuisine", align = "center", width=150),
              Duration = colDef(align = "center",width = 150),
              Delivery.Timing = colDef(name = "Pickup Timings", align = "center",width = 150),
              Location = colDef(name = "Pickup Location", align = "center",width = 200),
              Tiffin.Service = colDef(align = "center", width = 150, style = sticky_style,
                                      headerStyle = sticky_style,
                                      header = HTML("<div style='color:#e20387;font-size:15px'>Tiffin <span style='color:#ffbf32';>Services</span></div>"),
                                      #name = "Tiffin Services", 
                                      html = TRUE, cell = function(value,index) {
                                        sprintf('<a href="%s">%s</a>', slug$Slug[index], value)}),
              dist = colDef(name = "Pickup Distance",width = 200),
              Slug = colDef(
                name = "",
                sortable = FALSE, align = "center", width = 200, html = TRUE, 
                cell = function(value) { tags$a(class = "det_btn",href = value, "Menu & Contact") 
                }#tags$form(tags$input( type = "button",onclick=paste0("window.location.href='",value,"'"),value ="Click")) }
              )),
            
            details = function(index) {
              small_table <- filter(final_data, Tiffin.Service == big_table$Tiffin.Service[index]) %>% select(-Tiffin.Service)%>%select(Daily, Weekly,Monthly, Veg_NonVeg)
              tbl <- reactable(small_table, bordered =FALSE,outlined = TRUE, highlight = TRUE, fullWidth = FALSE
                               ,columns = list(
                                 Veg_NonVeg = colDef(align = "center",  name = "",
                                                     cell = function(value) {
                                                       img_src <- knitr::image_uri(sprintf("www/%s", value))
                                                       image <- img(src = img_src, height = "35px")
                                                       tagList(
                                                         div(style = list( width = "35px"), image)
                                                       )}),
                                 Daily = colDef(format = colFormat(currency = "CAD",separators = TRUE, digits = 0)),
                                 Weekly = colDef(format = colFormat(currency = "CAD",separators = TRUE, digits = 0)),
                                 Monthly = colDef(format = colFormat(currency = "CAD",separators = TRUE, digits = 0)))
              )
              htmltools::div(style = list(margin = "12px 45px"), tbl)

            },
            
            onClick = "expand"
            
            #rowStyle = list(cursor = "pointer")
  )
}
