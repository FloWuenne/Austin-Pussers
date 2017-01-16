
source("../uifunctions.R")
initialize('ath',TRUE)


shinyUI(bootstrapPage(
  head(),
  navigation(),
  titlePanel("Contact"),
  beginPage(),
  beginPanel('1/3'),
  
  endPanel(),
  beginPanel('2/3'),
  HTML(	"This page is created and operated by: <br>
        <a href='http://www.cbs.dtu.dk/staff/show-staff.php?id=1202'>Lasse Folkersen</a>.
        <br><br>If you encounter problems with it, feel free to write an email."),
  

  
  endPanel(),
  
  
  endPage(),
  footer()
  
  )	
)





