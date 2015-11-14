#Purely envelope and data creation function.

#This is a single function to create the data.frame for the envelopes.
envelopes <- function(search_term){
  #Base functions necessary
  
  #.simpleCap capitalizes the first letter of a word and nothing else.
  .simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
  }
  
  #Create a state dateset for easy envelope conversion.#
  #Get list of states for conversion
  data(state)
  statedata <- data.frame(abb = state.abb, name=state.name)
  #Include Dc
  dcdata <- data.frame(abb='Washington, D.C.', name= 'Washington DC')
  statedata <- rbind(statedata,dcdata)
  
  
  #Get list of messages from Gmail
  mymsg <- messages(search_term)
  #Get specific ids of messages
  msglist <- id(mymsg)
  
  
  #Create template for envelope document
  doc = docx(title='test',
             template='data/a2.docx')

  #Create a blank data.frame outside the loop
  #This data.frame will be filled with customer information.
  final <- c()
  final <- data.frame(final)
  
  
  for(i in 1:length(msglist)){
    
    #Make mail data readable in R
    msg1 <- message(msglist[i])
    data <- gmailr:::base64url_decode_to_char(msg1$payload$body)
    data <- as.character(data)
    data <- readLines(textConnection(data))
    
    #Check length of data for non-orders.
    #If a non-order, skip that e-mail.
    if(length(data)==1){
      i <- i+1
    }else{
      
      #This next section will extract data from the e-mails.
      #A basic grep() algorithm is used to find specific lines.
      
      
      
      #Tag Style
      
      style <- grep('Holder1_lblTagStyle',data)
      style <- substr(data[style],
                      str_locate_all(pattern='TagStyle">',data[style])[[1]][,2]+1,
                      str_locate_all(pattern='</span',data[style])[[1]][,1]-1)
      
      #Tag Quantity
      
      qty <- grep('QTY:',data)
      qty <- substr(data[qty],
                    str_locate_all(pattern='<b>QTY:</b>',data[qty])[[1]][,2]+2,
                    nchar(data[qty]))
      
      
      #Pet Name
      petname <- grep('PET NAME', data)
      petname <- substr(data[petname],
                        str_locate_all(pattern='<tr><td>PET NAME:</td><td>',data[petname])[[1]][,2]+1,
                        str_locate_all(pattern='</td></tr><tr><td>',data[petname])[[1]][,1]-1)
      
      #Microchip Number
      
      mchip_pattern <- 'ID #|MICROCHIP #'
      microchip <- grep(mchip_pattern, data)
      microchip <- substr(data[microchip],
                          str_locate_all(pattern=' #:</td><td>',data[microchip])[[1]][,2]+1,
                          str_locate_all(pattern='</td></tr><tr><td>PH',data[microchip])[[1]][,1]-1)
      
      #Tag Phone Number
      phone <- grep('PHONE:', data)
      phone <- substr(data[phone],
                      str_locate_all(pattern='PHONE:</td><td>',data[phone])[[1]][,2]+1,
                      str_locate_all(pattern='</td></tr></table></span>',data[phone])[[1]][,1]-1)
      
      #customer Name
      customerinfo <- grep('Customer:', data)
      customername <- grep('OwnerName',data)
      customername <- substr(data[customername],
                             str_locate_all(pattern='Holder1_lblOwnerName">',data[customername])[[1]][,2]+1,
                             str_locate_all(pattern='</span><br/>',data[customername])[[1]][,1]-1)
      
      #Customer Address
      customeraddress <- grep('ContentPlaceHolder1_lblAddress',data)
      numlinesaddress <- nrow(str_locate_all(pattern='<br/>', data[customeraddress])[[1]])
      
      #If from Canada, fix the grep() function.
      #The extra line with Canada throws off the general US algorithm.
      if(grepl('Canada',data[customeraddress])==TRUE){
        numlinesaddress <- numlinesaddress-1
      }
      
      #Customer Street
      #Account for if there are or are not two lines in the address
      if(numlinesaddress==2){
        customerstreet1 <- substr(data[customeraddress],
                                  str_locate_all(pattern='"ContentPlaceHolder1_lblAddress">',data[customeraddress])[[1]][,2]+1,
                                  str_locate_all(pattern='<br/>',data[customeraddress])[[1]][1,1]-1)
        customerstreet2 <- ''
      }  else{
        customerstreet1 <- substr(data[customeraddress],
                                  str_locate_all(pattern='"ContentPlaceHolder1_lblAddress">',data[customeraddress])[[1]][,2]+1,
                                  str_locate_all(pattern='<br/>',data[customeraddress])[[1]][1,1]-1)
        customerstreet2 <- substr(data[customeraddress],
                                  str_locate_all(pattern='<br/>',data[customeraddress])[[1]][1,2]+1,
                                  str_locate_all(pattern='<br/>',data[customeraddress])[[1]][2.1]-1)
      }
      
      #Customer city
      #Account for if there are or are not two lines in the address
      
      if(numlinesaddress==2){
        customercity <- substr(data[customeraddress],
                               str_locate_all(pattern='<br/>',data[customeraddress])[[1]][1,2]+1,
                               str_locate_all(pattern=',&nbsp;',data[customeraddress])[[1]][,1]-1)
      }  else if(numlinesaddress!=2){
        customercity <- substr(data[customeraddress],
                               str_locate_all(pattern='<br/>',data[customeraddress])[[1]][2,2]+1,
                               str_locate_all(pattern=',&nbsp;',data[customeraddress])[[1]][,1]-1)
      }
      
      #Customer State
      
      customerstate <- substr(data[customeraddress],
                              str_locate_all(pattern=',&nbsp;',data[customeraddress])[[1]][,2]+1,
                              str_locate_all(pattern='&nbsp;&nbsp;',data[customeraddress])[[1]][,1]-1)
      #If a Canadian customer, do not attempt to abbreviate the state.
      if(grepl('Canada',data[customeraddress])==FALSE){
      customerstate <- statedata[statedata[,2]==customerstate,'abb']
      }
  
      #Customer Zip
      #Special considerations taken for Canada
      
      if(grepl('Canada',data[customeraddress])==TRUE){
        customerzip <- substr(data[customeraddress],
                              str_locate_all(pattern='&nbsp;&nbsp;',data[customeraddress])[[1]][,2]+1,
                              str_locate_all(pattern='<br/>',data[customeraddress])[[1]][,1][2]-1)
      }else{
        customerzip <- substr(data[customeraddress],
                              str_locate_all(pattern='&nbsp;&nbsp;',data[customeraddress])[[1]][,2]+1,
                              str_locate_all(pattern='</span><br/>',data[customeraddress])[[1]][,1]-1)
      }
      
      #Customer Phone Number
      
      customerphone <- grep('"ContentPlaceHolder1_lblPhone">',data)
      customerphone <- substr(data[customerphone],
                              str_locate_all(pattern='"ContentPlaceHolder1_lblPhone">',data[customerphone])[[1]][,2]+1,
                              str_locate_all(pattern='</span><br/>',data[customerphone])[[1]][,1]-1)
      
      #Order Date
      
      order_date <- grep('id="ContentPlaceHolder1_lblDate">',data)
      order_date <- substr(data[order_date],
                           str_locate_all(pattern='id="ContentPlaceHolder1_lblDate">',data[order_date])[[1]][,2]+1,
                           str_locate_all(pattern='</span></b><br/>',data[order_date])[[1]][,1]-1)
      
      
      #Create a data.frame with this information.
      
      customername <- tolower(customername)
      customername <- .simpleCap(customername)
      customerstreet1 <- tolower(customerstreet1)
      customerstreet1 <- .simpleCap(customerstreet1)
      customerstreet2 <- .simpleCap(customerstreet2)
      customercity <- tolower(customercity)
      customercity <- .simpleCap(customercity)
      petname <- .simpleCap(petname)
      style <- .simpleCap(style)
      
      df <- data.frame(custname=customername,
                       custstreet1 = customerstreet1,
                       custstreet2= customerstreet2,
                       custcity = customercity,
                       custstate = customerstate,
                       custzip = customerzip,
                       custphone=customerphone,
                       microchip=microchip,
                       tagphone=phone,
                       petname=petname,style=style,qty=qty,
                       order_date=order_date)
      
      #Merge this data.frame with the previous ones in the search_term
      final <- rbind(final,df)
      
     #Create the pet's first name and human's last name for envelope
      lastname <- tail(strsplit(customername,split=' ')[[1]],1)
      petlastname <- tail(strsplit(petname,split=' ')[[1]],1)
      if(petlastname==lastname){
        petfirstname <- head(strsplit(petname,split=' ')[[1]],sapply(strsplit(petname, "\\s+"), length)-1)
        for(i in 1:sapply(strsplit(petname, "\\s+"), length)-2){
          petname <- paste(petfirstname[i],petfirstname[i+1],sep=' ')
        }
        mailname <- paste(petname,lastname,sep=' ')
      }  else{
        mailname <- paste(petname, lastname, sep= ' ')
      }
      mailname <- tolower(mailname)
      mailname <- .simpleCap(mailname)
      
      ##Add address to new envelope
      
      pot1 = mailname
      if(numlinesaddress==2){
        pot2 = df$custstreet1
        pot3 = paste(df$custcity, df$custstate, sep=', ')
        pot3 = paste(pot3, df$custzip, sep= ' ')
        
        pot1 <- as.character(pot1)
        pot2 <- as.character(pot2)
        pot4 <- as.character(ifelse(grepl('Canada',data[customeraddress])==TRUE,'Canada',''))
        
        mypars = set_of_paragraphs(pot1,pot2,pot3,pot4)
        
      } else if(numlinesaddress!=2){
        pot2 = df$custstreet1
        pot3 = df$custstreet2
        pot4 = paste(df$custcity, df$custstate, sep=', ')
        pot4 = paste(pot4, df$custzip, sep= ' ')
        
        pot1 <- as.character(pot1)
        pot2 <- as.character(pot2)
        pot3 <- as.character(pot3)
        pot5 <- as.character(ifelse(grepl('Canada',data[customeraddress])==TRUE,'Canada',''))
        
        mypars = set_of_paragraphs(pot1,pot2,pot3,pot4,pot5)
        
      }
      
      
      doc = addParagraph(doc, mypars,
                         par.properties=parProperties(text.align='center'))
      
      doc = addPageBreak(doc)
      
    }
  }
  #Output summary Statistics
  list(data.frame(table(final$style)),doc)
}

