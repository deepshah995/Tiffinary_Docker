FOOTER_TAG = (
  tags$body(
    tags$div(class="container-footer"),
    tags$footer(
      tags$section(class="ft-main",
        tags$div(class="ft-main-item",
          tags$h2(class="ft-title","Resources"),
          tags$ul(
            tags$li(tags$a(href="#","Search Tiffins")),
            tags$li(tags$a(href="#","About Us")),
            tags$li(tags$a(href="#","Join as Vendor"))
          )
        )
        ,
        tags$div( class="ft-main-item",
          tags$h2( class="ft-title","Contact"),
          tags$ul(class="ft-social-list",
                  tags$li(tags$a(href="https://facebook.com/tiffinary",tags$img(class = "icons",src="https://tiffinary.com/wp-content/uploads/2021/04/facebook.png"))),
                  tags$li(tags$a(href="https://instagram.com/tiffinary",tags$img(class = "icons",src="https://tiffinary.com/wp-content/uploads/2021/04/instagram.png")))
                  
          )
        ),
        tags$div( class="ft-main-item",
          tags$h2(class="ft-title","Stay Updated"),
          #tags$p("Subscribe to our newsletter to get our latest news."),
          tags$div(textInput(inputId="text", "Subscribe to latest deals and offers!!",placeholder = "Enter email address"),
          actionButton("goButton",label = "Subscribe"))
        )
        ),

      tags$section(tags$div(align = "center",
                            
    tags$h5("Tiffinary is a platform to help you get in touch with the best tiffin services in town so you dont miss the 'Ghar Ka Khaana'")))
      ,
      #<!-- Footer social -->
      tags$section(class="ft-social",
        tags$ul(class="ft-social-list",
          tags$li(tags$img(class = "icons",src="https://tiffinary.com/wp-content/uploads/2021/04/paypal.png")),
          tags$li(tags$img(class = "icons",src="https://tiffinary.com/wp-content/uploads/2021/04/visa.png")),
          tags$li(tags$img(class = "icons",src="https://tiffinary.com/wp-content/uploads/2021/04/americanexpress.png")),
          tags$li(tags$img(class = "icons",src="https://tiffinary.com/wp-content/uploads/2021/04/mastercard.png")),
          tags$li(tags$img(class = "icons",src="https://tiffinary.com/wp-content/uploads/2021/04/maestro.png"))
        )
      ),
      
#<!-- Footer legal -->
tags$section( class="ft-legal",
  tags$ul(class="ft-legal-list",
          tags$li(tags$img(class = "icons1",src="https://tiffinary.com/wp-content/uploads/2021/03/logo-08.png")),
    tags$li(tags$a(href="","Copyright © 2021 Tiffinary"))
    )
  )
 )
))

STYLE_FOOTER = tags$head(tags$style(HTML('  *{
    box-sizing: border-box;
                                         font-family: ’Lato’, sans-serif;
                                         margin: 0; 
                                         padding: 0;
                                         }
                                         h1, h2, h3, h4, h5, h6 {
                                         color: #4b505e;
                                         }
                                         .icons1 {
                                         
                                         width: 120px
                                         }
                                         .icons {
                                         max-width: 60%;
                                         width: 60px
                                         }
                                         ul {
                                         list-style: none;
                                         padding-left: 0;
                                         }
                                         footer {
                                         background-color: #ffe5b4;
                                         color: #bbb;
                                         line-height: 1.5;
                                         }
                                         footer a {
                                         text-decoration: none;
                                         color: #4b505e;
                                         }
                                         a:hover {
                                         text-decoration: underline;
                                         
                                         }
                                         .ft-title {
                                         color: #e20387;
                                         font-family: ’Merriweather’, serif;
                                         font-size: 2rem;
                                         padding-bottom: 0.625rem;
                                         }
                                         
                                         
                                         .ft-main {
                                         padding: 1.25rem 1.875rem;
                                         display: flex;
                                         flex-wrap: wrap;
                                         }
                                         .ft-main-item {
                                         padding: 1.25rem;
                                         min-width: 12.5rem /*200px*/;
                                         color: #4b505e;
                                         }
                                        
                                         @media only screen and (min-width: 77.5rem /*1240px*/ ) {
                                         .ft-main {
                                         justify-content: space-evenly;
                                         }
                                         }
                                         form {
                                         display: flex;
                                         flex-wrap: wrap;
                                         }
                                         input[type="email"] {
                                         border: 0;
                                         padding: 0.625rem;
                                         margin-top: 0.3125rem;
                                         }
                                         input[type="submit"] {
                                         background-color: #00d188;
                                         color: #fff;
                                         cursor: pointer;
                                         border: 0;
                                         padding: 0.625rem 0.9375rem;
                                         margin-top: 0.3125rem;
                                         }
                                         
                                         .ft-social-list {
                                         display: flex;
                                         justify-content: center;
                                         border-top: 1px #777 solid; 
                                         padding-top: 1rem;
                                         }
                                         .ft-social-list li {
                                         margin: 0.5rem;
                                         font-size: 1rem;
                                         }
                                         .ft-legal {
                                         padding: 0.9375rem 1.875rem;
                                         background-color:#ffbf32;
                                         }
                                         .ft-legal-list {
                                         width: 100%;
                                         display: flex;
                                         flex-wrap: wrap;
                                         }
                                         .ft-legal-list li {
                                         margin: 0.125rem 0.625rem;
                                         white-space: nowrap;
                                         font-size: 1.5rem;
                                         }
                                         /* one before the last child */
                                         .ft-legal-list li:nth-last-child(2) {
                                         flex: 1;       /* same as flex-grow: 1; */
                                         }
                                         /* Smartphones (portrait and landscape) ----------- */
                                         @media only screen
                                         and (min-device-width : 320px) 
                                         and (max-device-width : 480px) {
                                         .ft-title {
                                         color: #e20387;
                                         font-family: ’Merriweather’, serif;
                                         font-size: 2rem;
                                         padding-bottom: 0.625rem;
                                         }
                                         }
                                         ')))
