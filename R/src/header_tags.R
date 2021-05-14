
TIFFIN_IMG = tags$img( src = "https://tiffinary.com/wp-content/uploads/2021/04/Tiffin-wo-bg-e1618081599966.png")        
TEXT1 = tags$div(class="text1", tags$img(src = "https://tiffinary.com/wp-content/uploads/2021/04/TiffinText1.png"))
TEXT2 = tags$div(class="text2", tags$img(src = "https://tiffinary.com/wp-content/uploads/2021/04/TiffinText2.png"))
mobTEXT1 = tags$div(class="mobtext1", tags$img(src = "https://tiffinary.com/wp-content/uploads/2021/04/TiffinText1.png"))
mobTEXT2 = tags$div(class="mobtext2", tags$img(src = "https://tiffinary.com/wp-content/uploads/2021/04/TiffinText2.png"))
tiffin_img_mob = TIFFIN_IMG

LOGO = tags$div(class="logo",tags$img(src = "https://tiffinary.com/wp-content/uploads/2021/02/logo-for-website-19.png"))
LOGO_MOB = tags$div(class="logo-mob",tags$img(src = "https://tiffinary.com/wp-content/uploads/2021/02/logo-for-website-19.png"))

DROPDOWNMENU = tags$div(class = "shinymenu",dropdown( tags$h3("List of Input"),
  "Link1","Link2","Link3",
  style = "unite", icon = icon("gear"),
  status = "danger", width = "300px",
  animate = animateOptions(
    enter = animations$fading_entrances$fadeInLeftBig,
    exit = animations$fading_exits$fadeOutRightBig
  )
))

HEADER_TAGS = tags$head(tags$style(HTML(' 

   .container-head {
      position: relative;
      margin:0;
      padding:0;
      }
      
     .tiffin-img {
      position: absolute;
      margin-left: 0px; 
      width: 50vw;
      display: flex;
      top: 12%;
      left: 10px;
     }

.shinymenu {
display: block;
position: absolute;
  top: -13%;
  right: -92%;
       }
                                        
      /* size of second image */
    .tiffin-img>img {
      position: relative;
      width: 50vw;
      height: 50%;
      margin-left: 0px; 
      }
  
       .logo {
      position: absolute;
      display: flex;
      top: 2%;
      left: 3px;
      
      }
    /* size of logo */
      .logo>img {
      position: relative;
      width: 32vw;
      height: 9%;
      top: 2%;
      left: 10%;
      }
  
      /* size of text1 */
    .text1>img {
      position: relative;
      width: 30vw;
      height: 20%;
      top: 60%;
      left: 40%;
    }

    .text2 {
    position:absolute;
    top: 35%;
    right: -65%; 
    }
  
      /* size of text2 */
      .text2>img {
      position: relative;
     width: 25vw;
      height: 100%;
      
      }
  
      .background {
      width: 100%;
      height: 100vh;
margin: 0;
padding: 0; 
      }
  
     .menu-list {
display: inline;
position: absolute;
top: 2%;
right: 3vw;
     }
.menu-list li{display: inline;}
  .menu-list a {
border-radius: 5px;
  color: white;
  padding: 1.5vh 3vw;
  text-decoration: none;
  display: inline;
  background: #e20387;
font-size: 1.6rem;
  }
  .menu-list a:hover{background: #ffffff; color: #e20387;}
      .mobile  {display: none;}
      
      @media only screen and (max-width: 768px) { 

       .container-head { display: none; }
       
       .mobile { 
      display: block;
      position: relative;
      display: flex;
       } 

 .mobile .backgroundmob {
      width: 100%;
      height: 90vh;
      }

.tiffin-img-mob {
      position: absolute;
      width: 50vw;
      display: flex;
      top: 40%;
      left: 6%;
      }
  
      /* size of second image */
    .tiffin-img-mob>img {
      position: relative;
      margin-left: auto;
      margin-right: auto;
      width: 80vw;
      height: 80%; 
      }


 /* size of text1 */
    .mobtext1>img {
    position: absolute;
      width: 45vw;
      height: 20%;
      top: -30%;
      left: 65%;
    }

  
      /* size of text2 */
      .mobtext2>img {
    position: absolute;
      width: 40vw;
      height: 16%;
      top: -55%;
      left: 32%;
      }   

.logo-mob {
      position: absolute;
      display: flex;
      top: 4%;
      left: 3px;
      
      }
      /* size of logo */
  .logo-mob>img {
      position: relative;
      width: 32vw;
      height: 9%;
      top: 4%;
      left: 10%;
      }

.menu{

}

.menu {
position: absolute;
width: 10vw;
height: 20vh
display: flex;
top: 2%;
left: 84%;
background-color: Transparent;
padding: 0px;
margin: 0px;
transition: 0.3s;
border: none;

-webkit-transition-duration: 0.4s; 
transition-duration: 0.4s;
cursor: pointer;
}

.menu:after {
content: "";
background: #ffbf32;

border-radius: 100%;
display: block;
position: absolute;
padding-top: 120%;
padding-left: 100%;
margin-left: 0%!important;
margin-top: -120%;
opacity: 0;
transition: all 0.8s
}

.menu:active:after {
padding: 0;
margin: 0;
opacity: 1;
transition: 0s
}

.menu:hover {opacity: 1}

.menu-content {
      display: none;
position: absolute;
right: 0;
background-color: #ffffff;
min-width: 110px;
box-shadow: 0px 8px 16px 0px rgba(0,0,0,0.2);
z-index: 1;
}

.menu-content a {
color: white;
padding: 1vh 2vw;
text-decoration: none;
display: block;
background: #e20387;
}
.menu-content a:hover {background-color: #f1f1f1; color: #e20387;}

')))


