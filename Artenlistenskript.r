
# setwd("./Judith/")
# kopf <- read.table("kopf.md", sep="[")
# data <- read.csv("daten.csv")
# artenliste(data = read.csv("Untitled 1.csv"), kopf = "kopf.md")


artenliste <- function(daten, kopf="kopf.md", titel=format(Sys.time(), "%b %Y"), fuss="dfhdfahadh", fontsize="\\large", table.length.adjust=0, wald=FALSE, waldextrablatt=T, output="Artenliste.md"){
  require(openxlsx)
	if(file.exists(kopf)){  # lade kopf  # This is the stuff that's written between the title and the species list
		head <- as.character(read.table(kopf, sep="[")[,1])
	} else {
		warning("ACHTUNG: Der Kopf konnte nicht gefunden werden! Stimmt der Dateiname?")
		head <- ""
	}

  
  # Daten lesen:
  data <- 0
  format <- strsplit(daten, split = "\\.")[[1]][2]
  if(format == "csv"){
    data <- read.csv(daten)    # .csv file einlesen
    if(ncol(data) ==1) data <- read.csv2(daten)   # falls nur eine Spalte erkannt wurde mit csv 2 probieren
  }
  if(format == "xlsx") data <- read.xlsx(daten, sheet= 1)    # .csv file einlesen
  if(!is.data.frame(data)) stop("Keine Daten gefunden. Das script sollte mit .csv oder .xlsx funktionieren.", call. = F)
	
  x <- character()
	for (i in 1:ncol(data)){ #loop for each column (=each Plot) of the data

		size <- fontsize      # this is the latex command for the font size (eg. \\tiny, \\huge)
		header <- "\\begin{tabularx}{\\textwidth}{|l|X|l|X|} \\hline"    # this is the header for the table for the species list
		if(wald) header <- c("\\begin{tabularx}{\\textwidth}{|l|X|X|X|X|} \\hline",
		                     "\\textbf{Arten} \\phantom{ChrysospleniumChrysospleniumChrysosplen} & \\textbf{K} & \\textbf{S}\\tiny{<5m} & \\textbf{B1}\\tiny{<10} & \\textbf{B2}\\tiny{>10m} \\\\\\hline") # this is the wald header
		end <- "\\end{tabularx}"  # this ends the table
		arten <- as.character(data[,i])   # this is the list of speceis
		arten <- arten[which(arten!="")]   #cleanup
		arten <- unique(arten)  #cleanup
    arten <- gsub("_"," ",arten)    # remove underlines
    arten <- sort(arten)          # sort


		cutoff <- 42-length(head) + table.length.adjust   # this is how many rows the list has on the 1st page. It gets shorter when the head is longer
		cutoff2 <- 42 + table.length.adjust               # this is the number of rows on the subsequent pages.


	## Generate the title, extra code is so that can also be displayed in the footer including page number Plotname and date
	totpage <- if(length(arten)<(2*cutoff)) {1} else { ceiling((length(arten)-(2*cutoff))/(2*cutoff2))+1 }

	if(wald) { # in forest we have to adjust the cutoff
	  cutoff  <- cutoff -1
	  cutoff2 <- cutoff2-1
          totpage <- if(length(arten)<(cutoff)) {1} else { ceiling((length(arten)-(cutoff))/(cutoff2))+1 }
	}

  if(fuss=="dfhdfahadh") {fuss <- titel} else {paste(names(data)[i], fuss)}



	## Generate the tables
	if(wald){

	  arten <- paste(arten, "& \\phantom{--} & \\phantom{--} & \\phantom{--}  & \\\\\\hline")

	  if(length(arten) < cutoff) arten <- c(arten, rep("\\phantom{--} & \\phantom{--} & \\phantom{--} & \\phantom{--}  & \\\\\\hline", cutoff-length(arten) ))  # FALLS Artenliste kÃ¼rzer als erster Cuttoff, ergÃ¤nzen mit leeren zeilen
	  table <- c(header, arten[1:cutoff], end)  # Create first table

	  if(length(arten)> cutoff ){ # Create more tables when first page is full

	    arten <- arten[(cutoff+1):length(arten)] # Artenliste kÃ¼rzen
	    while(length(arten) > cutoff2){ # When Cutoff2 is not enough ad table page
	      table <- c(table, "\\newpage", header, arten[1:cutoff2], end)
	      arten <- arten[(cutoff2+1):length(arten)]
	    }

	    if(length(arten) < cutoff2) arten <- c(arten, rep("\\phantom{--} & \\phantom{--} & \\phantom{--} & \\phantom{--}  & \\\\\\hline", cutoff2-length(arten) ))  # FALLS Artenliste kÃ¼rzer als erster Cuttoff, ergÃ¤nzen mit leeren zeilen
	    table <- c(table, "\\newpage", header, arten, end)
	  }

	  if( waldextrablatt &
	    table[length(table)-8] != "\\phantom{--} & \\phantom{--} & \\phantom{--} & \\phantom{--}  & \\\\\\hline"
	  ) {
	    table <- c(table, "\\newpage", header, rep("\\phantom{--} & \\phantom{--} & \\phantom{--} & \\phantom{--}  & \\\\\\hline", cutoff2), end)
	    totpage <- totpage+1
	    }
	}

	title <- paste("\\centering \\section*{", names(data)[i]," ", titel, "}","\\markboth{/", totpage, " | ", names(data)[i]," ",fuss, "}{", names(data)[i]," ",fuss,"}", sep="")
	##

		if(length(arten)<cutoff & !wald){   # how to generate the list when less species than the short cutoff
			arten <- c(arten, rep("", (cutoff-length(arten) )))
			arten <- paste( "\\phantom{--} &", arten, "& \\phantom{--}  & \\\\\\hline")
			table <- c(header,arten,end,"")  # table consists of header, list and end
		}

		if(length(arten)>cutoff & length(arten)<(cutoff*2)  & !wald){ #wenn die Arten die zeilenanzahl von cutoff Ã¼berschreiten-> 2 zeilen machen
			a1 <- arten[1:cutoff]
			a2 <- arten[(cutoff+1):length(arten)]
			if(length(a1) !=length(a2) ) a2 <- c(a2, rep("", (length(a1)-length(a2) )))
			arten <- paste( "\\phantom{--} &", a1, "& \\phantom{--} &", a2, "\\\\\\hline")
			table <- c(header,arten,end,"")
			}

		if(length(arten)>(cutoff*2)  & !wald){   #wenn die Arten die zeilenanzahl von erster seite Ã¼berschreiten-> mehrere Tabellen machen
			table <- character()
			art <- paste( "\\phantom{--} &", arten[1:cutoff], "& \\phantom{--} &", arten[(cutoff+1):(cutoff*2)], "\\\\\\hline")  # first fill the table on page 1 with cutoff NÂ°1
			table <-c(table, header,art,end,"")

			for(j in 1:ceiling((length(arten)-(2*cutoff))/(cutoff2*2)) ){
				if(j!=max(ceiling((length(arten)-(2*cutoff))/(cutoff2*2)))){  # when so many species that the 2nd can be filled. Fill the table
					arten2 <- arten[(((cutoff*2)+1)+ (j-1)*cutoff2*2 ) :length(arten)]
					art <- paste( "\\phantom{--} &", arten2[1:cutoff2], "& \\phantom{--} &", arten2[(cutoff2+1):(cutoff2*2)], "\\\\\\hline")
					table <- c(table, header,art,end,"")
				}else{  #else
					arten2 <- arten[(((cutoff*2)+1)+ (j-1)*cutoff2*2 ) :length(arten)] #the rest of the species
					if(length(arten2)<cutoff2){  #analogous to the first table but with different cutoff
						arten2 <- c(arten2, rep("", (cutoff2-length(arten2) )))
						arten2 <- paste( "\\phantom{--} &", arten2, "& \\phantom{--} & \\\\\\hline")
						table <- c(table,header,arten2,end,"")
					} else {
						a1 <- arten2[1:cutoff2]
						a2 <- arten2[(cutoff2+1):length(arten2)]
						if(length(a1) !=length(a2) ) a2 <- c(a2, rep("", (length(a1)-length(a2) )))
						arten2 <- paste( "\\phantom{--} &", a1, "& \\phantom{--} &", a2, "\\\\\\hline")
						table <- c(table,header,arten2,end,"")
					}
				}
			}
		}
	##
	x = c(x, c("\\setcounter{page}{1}", title, "", head, "", size, table, "\\newpage"))
		# write(x = c("\\setcounter{page}{1}", title, "", head, "", size, table, "\\newpage"), file = paste("./md/",names(data)[i], ".md", sep=""), ncolumns = 1)  # write the md file

	}
  # Correct the Umlauts
#  x <- gsub("?", '"a', x)
#  x <- gsub("?", '"o', x)
#  x <- gsub("?", '"u', x)
#  x <- gsub("?", '"A', x)
#  x <- gsub("?", '"O', x)
#  x <- gsub("?", '"U', x)
#  x <- gsub("?", '"ss', x)

  write(x = x, file = output, ncolumns = 1)
}


# create eingabeformular
eingabeformular <- function(daten, explo, kopf, wald=F, filename = "eingabeformular.xlsx"){
  library(openxlsx)
  
  
  # Daten lesen:
  d <- 0
  format <- strsplit(daten, split = "\\.")[[1]][2]
  if(format == "csv"){
    d <- read.csv(daten)    # .csv file einlesen
    if(ncol(d) ==1) data <- read.csv2(daten)   # falls nur eine Spalte erkannt wurde mit csv 2 probieren
  }
  if(format == "xlsx") d <- read.xlsx(daten, sheet= 1)    # .csv file einlesen
  if(!is.data.frame(d)) stop("Keine Daten gefunden. Das script sollte mit .csv oder .xlsx funktionieren.", call. = F)
  
  if(!missing(explo)) d <- d[, which(substr(names(d), 1,1)==explo)]

  l <- character()
  for( i in 1:ncol(d)){
    l <- c(l,
      paste("Plot_", names(d)[i], sep = ""),
      if(wald) "Layer",
      if(!missing(kopf)) kopf,
      sort(unique(d[d[,i]!="",i])),
      rep("", 5)
    )
  }
  
  plots <- do.call(rbind, strsplit(l[substr(l, 1,5)=="Plot_"], split = "Plot_"))[,2]
  I <- 1
  i <- 1
  lpl <- character()
  for(i in 1:length(plots)){
    lpl[I] <- plots[i]
    I <- I+1
    while( substr(l[I], 1,5) != "Plot_" ){
      lpl[I] <- plots[i]
      I <- I+1
      if(I > length(l)) break
    }
    
  }
  if(wald){
    l <- data.frame(lpl, l, k=character(length(l)), s=character(length(l)), b1=character(length(l)), b2=character(length(l)))
    l$s[which(l$l=="Layer")] <- "S"  
    l$k[which(l$l=="Layer")] <- "K"  
    l$b1[which(l$l=="Layer")] <- "B1"  
    l$b2[which(l$l=="Layer")] <- "B2"  
  } else {
    l <- data.frame(lpl, l)
  }
  names(l) <- NULL
  headStyle <- createStyle(fontColour =  "#a3e8ff", bgFill = "#3a2d0d", textDecoration="bold")
  wb <- createWorkbook()
  addWorksheet(wb, "Sheet 1")
  writeData(wb, sheet = 1, x = l, colNames = F)
  formating <- which(substr(l[,2], 1,4)=="Plot")
  for(i in 1:length(formating)) addStyle(wb, sheet = 1, cols=1:7, rows=formating[i], style = headStyle)
  setColWidths(wb, sheet = 1, cols = 1:ncol(l), widths =  c("auto", "auto", if(ncol(l)>2) rep(5, ncol(l)-2)) )
  
  if(file.exists(filename)) {
    cat("Achtung!\n")
    if(readline(paste("Die Datei", filename, "ist bereits vorhanden! Überschreiben? (j/n)"))=="j"){
      saveWorkbook(wb, filename, overwrite  = TRUE)
    } else {
      stop("Abbruch", call. = F)
    }
  } else {
      saveWorkbook(wb, filename, overwrite  = TRUE)
    }
}

# create.eingabeformular(daten.csv = "Species_2017-2020_for_Artenbogen.csv", kopf = "Deckungsgrad", wald = T)



## read formular and create big table
eingabeformular2tabelle <- function( inputfilename.xlsx = "eingabeformular.xlsx", kopf, outputfilename.xslx, fuzzy=T ){

  require(openxlsx)
  
  d <- read.xlsx( inputfilename.xlsx , colNames = F)
  d <- d[,2:ncol(d)]
  names(d)[1] <- "V1"
  # find where are the plotnames
  pn <- which(substr(d[,1], 1, 5) == "Plot_"  )
  pn <- c(pn, nrow(d))
  
  l <- list()
  for(i in 1:(length(pn)-1)) l[[i]] <- d[(pn[i]+1):(pn[i+1]-1),]
  names(l) <- d[pn[1:(length(pn)-1)],1]
  
  if(ncol(d)>2){
    ll <- list()
    I <- 1
    for(i in 1:length(l)){
      for(j in 2:ncol(d)){
        ll[[I]] <- data.frame(V1= l[[i]][,1], V2= l[[i]][,j])
        names(ll)[I] <- paste( names(l)[i], ll[[I]][1,2], sep= "_")
        ll[[I]] <- ll[[I]][-1,]
        # ll[[I]][,2] <- as.numeric(as.character(ll[[I]][,2]))
        I <- I+1
      }
    }
    l <- ll
  }
  if(!missing(kopf) & is.numeric(kopf)) kopf <- l[[1]][kopf,1]
  if(!missing(kopf)) if(!(identical(kopf %in% d[,1] , rep(TRUE, length(kopf))))) cat("Angegebene Kopfdaten nicht in Datei! \n")
  
  # Warn if there are non numeric characters in data
  ch <- as.character(do.call(rbind, l)[,2])
  if(length(grep("\\.\\.", ch))>0) stop("Non numeric element in data: .. (two points)", call. = F)
  ch <- unique(unlist(strsplit(ch, split = "")))
  ch <- ch[!(ch %in% c(NA , ".", " ", "  ", "   ", "    ", "      ", "       ", "         ", "         "))]
  if(FALSE %in% (ch %in% as.character(0:9))) stop(paste("Non numeric element in data: ", paste(ch[!(ch %in% as.character(0:9))], collapse = ", "), " \n", sep = ""), call. = F)
  
  mergefunc <- function(lis, kopf, fuzzy){
    
    x <- lis[[1]]
    if(missing(kopf)) x <- x[!is.na(x[,2]),] else x <- x[ x$V1 %in% kopf | !is.na(x[,2]),]
    names(x)[2] <- names(lis)[1]
    alarm <- nrow(x)!=length(unique(x[,1]))
    if(alarm) mess <- (paste("Error in ", names(x)[2], ", duplicted Species name: ", x[duplicated(x[,1]),1]  , "! \n", sep = ""))
    for(i in 2:length(lis)){
      if(alarm) stop(mess, call. = F)
      y <- lis[[i]]
      if(missing(kopf)) y <- y[!is.na(y[,2]),] else y <- y[ y$V1 %in% kopf | !is.na(y[,2]),]
      names(y)[2] <- names(lis)[i]
      alarm <- nrow(y)!=length(unique(y[,1]))
      if(alarm) mess <- (paste("Error in ", names(y)[2], ", duplicted Species name: ", y[duplicated(y[,1]),1]  , "! \n", sep = ""))
      if(alarm) {stop(mess, call. = F )}
      x <- merge(x,y, by="V1", all = TRUE, sort = F)
    }
    # order
    if(!missing(kopf)) x <- x[c(1:length(kopf), order(x$V1[(length(kopf)+1):nrow(x)])+length(kopf) )   ,] else x <- x[order(x$V1),]
    
    # use agrep fuccy matching to find typos in the species names
    if(!missing(kopf) & fuzzy){
      candis <- character()
      for(i in (length(kopf)+1):nrow(x)){
        if(!(x[i,1] %in% candis)){
          xx <- agrep(pattern = x[i,1], x[-i,1],  value = T)
          if(length(xx)>0) candis <- c(candis, x[i,1], xx, "/")
        }
      }
      if(length(candis)>0) {
        mess <- paste("Warning: There might be a Typo in these names:", paste( 
          # ((c(x[,1],"/")[candis])#, incomparables = "/", fromLast = F)
          candis, collapse = ", "), " \n")
        # mess <- gsub(" /, /,", "", mess)
        warning(mess, call. = F)
        # cat( mess )
      }
    }
    return(x)
    
  }
    
  D <- mergefunc(lis = l, kopf = kopf, fuzzy=fuzzy)
  if(missing(outputfilename.xslx)) return(D) else write.xlsx(D, file = outputfilename.xslx)
}


