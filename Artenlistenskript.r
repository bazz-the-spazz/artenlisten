
# setwd("./Judith/")
# kopf <- read.table("kopf.md", sep="[")
# data <- read.csv("daten.csv")
# artenliste(data = read.csv("Untitled 1.csv"), kopf = "kopf.md")


artenliste <- function(daten, kopf="kopf.md", titel=format(Sys.time(), "%b %Y"), fuss="dfhdfahadh", fontsize="\\large", table.length.adjust=0, wald=FALSE, waldextrablatt=T, output="Artenliste.md", extrazeile, find.in.head, replace.in.head){
  require(openxlsx)
	if(file.exists(kopf)){  # lade kopf  # This is the stuff that's written between the title and the species list
		head <- head.back <-  as.character(read.table(kopf, sep="[")[,1])
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

  # EXtrazeile: insert a extrainformation between header and table. Either one thing everywhere or one for each Plot.
  extra <- FALSE
  if(!missing(extrazeile)) {
    table.length.adjust <- table.length.adjust -1
    extra <- TRUE
    if(length(extrazeile)!=1 & length(extrazeile)!=ncol(data)){
      warning(paste("Achtung: Nicht genügend Extrazeilen für jeden Plot (", length(extrazeile), " statt ", ncol(data),"). Das erste Element wird wiederholt.", sep = ""))
      extrazeile <- rep(extrazeile[1], ncol(data))
    }
  }

  # Find.in.head & replace.in.head:
  ## Find a string of characters in the head and replace it with another
  replace <- FALSE
  if(!missing(find.in.head)){
  	if(missing(replace.in.head)) {warning("Argument replace.in.head is missing"); break }
  	if(length(find.in.head)>1 ) if(is.list(replace.in.head)==FALSE) {warning(paste("Argument replace.in.head is not a list of length:", length(find.in.head))); break }
  	replace <- TRUE
		if(is.list(replace.in.head)==FALSE)	replace.in.head <- list(replace.in.head) # make list if it isn't already
  	}


  x <- character()
  ## Loop
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

	  if(length(arten) < cutoff) arten <- c(arten, rep("\\phantom{--} & \\phantom{--} & \\phantom{--} & \\phantom{--}  & \\\\\\hline", cutoff-length(arten) ))  # FALLS Artenliste kürzer als erster Cuttoff, ergänzen mit leeren zeilen
	  table <- c(header, arten[1:cutoff], end)  # Create first table

	  if(length(arten)> cutoff ){ # Create more tables when first page is full

	    arten <- arten[(cutoff+1):length(arten)] # Artenliste kürzen
	    while(length(arten) > cutoff2){ # When Cutoff2 is not enough ad table page
	      table <- c(table, "\\newpage", header, arten[1:cutoff2], end)
	      arten <- arten[(cutoff2+1):length(arten)]
	    }

	    if(length(arten) < cutoff2) arten <- c(arten, rep("\\phantom{--} & \\phantom{--} & \\phantom{--} & \\phantom{--}  & \\\\\\hline", cutoff2-length(arten) ))  # FALLS Artenliste kürzer als erster Cuttoff, ergänzen mit leeren zeilen
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

		if(length(arten)<=cutoff & !wald){   # how to generate the list when less or equal species than the short cutoff
			arten <- c(arten, rep("", (cutoff-length(arten) )))
			arten <- paste( "\\phantom{--} &", arten, "& \\phantom{--}  & \\\\\\hline")
			table <- c(header,arten,end,"")  # table consists of header, list and end
		}

		if(length(arten)>cutoff & length(arten)<=(cutoff*2)  & !wald){ #wenn die Arten die zeilenanzahl von cutoff überschreiten-> 2 zeilen machen
			a1 <- arten[1:cutoff]
			a2 <- arten[(cutoff+1):length(arten)]
			if(length(a1) !=length(a2) ) a2 <- c(a2, rep("", (length(a1)-length(a2) )))
			arten <- paste( "\\phantom{--} &", a1, "& \\phantom{--} &", a2, "\\\\\\hline")
			table <- c(header,arten,end,"")
			}

		if(length(arten)>(cutoff*2)  & !wald){   #wenn die Arten die zeilenanzahl von erster seite überschreiten-> mehrere Tabellen machen
			table <- character()
			art <- paste( "\\phantom{--} &", arten[1:cutoff], "& \\phantom{--} &", arten[(cutoff+1):(cutoff*2)], "\\\\\\hline")  # first fill the table on page 1 with cutoff N°1
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

	# find and replace in head
	if(replace){
	head <- head.back
		for(j in 1:length(find.in.head)){ # loop for each term to be replaced
			f <- find.in.head[j]
			r <- replace.in.head[[j]]
	    if(length(r)!=1 & length(r)!=ncol(data)){
  	    warning(paste("Achtung: Nicht genügend Replacements für jeden Plot (", length(r), " statt ", ncol(data),"). Das erste Element wird wiederholt.", sep = ""))
  	    r <- rep(r[1], ncol(data))
	    }
			# cat(r[i])
			head <- gsub(f, r[i], head)
		}
	}


	if(extra){ #insert extrabit
	     x = c(x, c("\\setcounter{page}{1}", title, "", head, extrazeile[i], "" ,size, table, "\\newpage", ""))
	} else {
	  x = c(x, c("\\setcounter{page}{1}", title, "", head, "", size, table, "\\newpage", ""))
	  rm(table)
	}
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
eingabeformular <- function(daten, explo, kopf, wald=F, filename = "eingabeformular.xlsx", dummy=FALSE, overwrite=FALSE){
  library(openxlsx)


  # Daten lesen:
  d <- 0
  format <- strsplit(daten, split = "\\.")[[1]][length(strsplit(daten, split = "\\.")[[1]])]
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
  # fill with dummy numbers
  if(dummy & !wald) {
    l[ !(l[,2] %in% paste("Plot_", plots, sep="")) &  !(l[,2] %in% kopf ) & l[,2]!="",3] <- as.character(.5)
    l[ (l[,2] %in% kopf ),3] <- "random word"
    l[l[,3]=="0.5" & !is.na(l[,3]),3] <- as.character(round(runif(length(l[l[,3]=="0.5" & !is.na(l[,3]),3])), 3))
  }
  if(dummy & wald) {
    for( i in 3:6){
      l[ !(l[,2] %in% paste("Plot_", plots, sep="")) & l[,2]!= "Layer" &  !(l[,2] %in% kopf ) & l[,2]!="",i] <- as.character(.5)
      l[ (l[,2] %in% kopf ),i] <- "random word"
      l[l[,i]=="0.5" & !is.na(l[,i]),i] <- as.character(round(runif(nrow(l[l[,i]=="0.5" & !is.na(l[,i]),i])), 3))
    }
  }


  headStyle <- createStyle(fontColour =  "#a3e8ff", bgFill = "#3a2d0d", textDecoration="bold")
  wb <- createWorkbook()
  addWorksheet(wb, "Sheet 1")
  writeData(wb, sheet = 1, x = l, colNames = F)
  formating <- which(substr(l[,2], 1,4)=="Plot")
  for(i in 1:length(formating)) addStyle(wb, sheet = 1, cols=1:7, rows=formating[i], style = headStyle)
  setColWidths(wb, sheet = 1, cols = 1:ncol(l), widths =  c("auto", "auto", if(ncol(l)>2) rep(5, ncol(l)-2)) )

  if(file.exists(filename)) {
    if(overwrite==TRUE) {saveWorkbook(wb, filename, overwrite  = TRUE)}
    if(overwrite==FALSE) {
      cat("Achtung!\n")
      if(readline(paste("Die Datei", filename, "ist bereits vorhanden! Uberschreiben? (j/n)"))=="j" | overwrite==TRUE){
        saveWorkbook(wb, filename, overwrite  = TRUE)
      } else {
        stop("Abbruch", call. = F)
      }
      }
  } else {
      saveWorkbook(wb, filename, overwrite  = TRUE)
    }
}

# create.eingabeformular(daten.csv = "Species_2017-2020_for_Artenbogen.csv", kopf = "Deckungsgrad", wald = T)
#
#
#




## read formular and create a big, unified table
eingabeformular2tabelle <- function( inputfilename.xlsx, input.data, kopf, outputfilename.xslx, fuzzy=T, write.fuzzy.mistakes= FALSE, wald=FALSE){

	if(!missing(inputfilename.xlsx)){
	  require(openxlsx)
	  d <- read.xlsx( inputfilename.xlsx[1] , colNames = F, sheet = 1, skipEmptyRows = F, rows = NULL)
	  # add dummy line
	  d <- rbind(d,d[nrow(d),])
	  d[nrow(d),2] <- "z"
	  d[nrow(d),3:ncol(d)] <- NA

	  if(length(inputfilename.xlsx)>1) {
	    for(i in 2:length(inputfilename.xlsx)){
	      dd <- read.xlsx( inputfilename.xlsx[i] , colNames = F, sheet = 1, skipEmptyRows = F, rows = NULL)
	      if(ncol(d) != ncol(dd)) stop("Empty data files have not the same number of columns. Did you mix Forest plots with Grassland or Spring flowers?", call. = F)

	      # add dummy line
	      dd <- rbind(dd,dd[nrow(dd),])
	      dd[nrow(dd),2] <- "z"
	      dd[nrow(dd),3:ncol(dd)] <- NA

	      d <- rbind(d,dd)
	    }
	  }
	}

  if(!missing(input.data)){
  	colnames21strow <- function(x, dummyline=T){
  		xx <- xxx <- x[1,]
  		xx[1,] <- t(names(x))
  		xxx[1,] <- t(rep(NA, ncol(x)))
  		if(dummyline) x <- rbind(xx,x, xxx) else x <- rbind(xx,x)
  		names(x) <- paste("V", 1:ncol(x), sep = "")
  		return(x)
  	}

  	if(is.null(dim(input.data))) {
  		for(i in 1:length(input.data)) input.data[[i]] <- colnames21strow(input.data[[i]])
  		d <- do.call(rbind, input.data)
  	} else {
  		d <- input.data
  	}
  }


  if(ncol(d)==2) stop("Empty data file: Abort!", call. = F)
  head(d)
  d <- d[,2:ncol(d)]
  names(d)[1] <- "V1"

    # find where are the plotnames
  pn <- which(substr(d[,1], 1, 5) == "Plot_"  )
  pn <- c(pn, nrow(d))


  # create a list, each containing one plot and level (when forest)
  l <- list()
  for(i in 1:(length(pn)-1)) l[[i]] <- d[(pn[i]+1):(pn[i+1]-1),]
  names(l) <- d[pn[1:(length(pn)-1)],1]

  if(ncol(d)>2){
    if(wald==FALSE) warning("More than one column of data in datafile. Is it forest data?", call. = F)
    ll <- list()
    I <- 1
    for(i in 1:length(l)){
      for(j in 2:ncol(d)){
        ll[[I]] <- data.frame(V1= l[[i]][,1], V2= l[[i]][,j])
        names(ll)[I] <- paste( names(l)[i], ll[[I]][1,2], sep= "_")
        ll[[I]] <- ll[[I]][-1,]
        I <- I+1
      }
    }
    l <- ll
  }



  # Sort out kopf!
  if(missing(kopf)){ kopf <- character()} else {
    if( !is.numeric(kopf)) if(!(identical(kopf %in% d[,1] , rep(TRUE, length(kopf))))){
      cat(paste("Angegebene Kopfdaten nicht in Datei: ", kopf[!(kopf %in% d[,1])] , ". \n", sep=""))
    }
    if(is.numeric(kopf)) kopf <- l[[1]][kopf,1]


    # sort data so that kopf is first
    for(i in 1:length(l)){
      if(length(which( !(kopf %in% l[[i]][,1])))>0) {
        xx <- data.frame(V1= kopf[which( !(kopf %in% l[[i]][,1]))], "")
        names(xx) <- names(l[[i]])
        l[[i]] <- rbind(l[[i]][l[[i]][,1] %in% kopf,], xx, l[[i]][ !(l[[i]][,1] %in% kopf) ,])
      } else {
        l[[i]] <- rbind( l[[i]][l[[i]][,1] %in% kopf,], l[[i]][ !(l[[i]][,1] %in% kopf) ,])
      }
    }
  }



  # Warn if there are non numeric characters in data
  # ch <- as.character(do.call(rbind, l)[,2])

  ch <- character()
  for(i in 1:length(l)){
    ch  <- c(ch, l[[i]][(length(kopf)+1):nrow(l[[i]]) ,2])
    l[[i]][(length(kopf)+1):nrow(l[[i]]) ,2] <- gsub(" ", "", l[[i]][(length(kopf)+1):nrow(l[[i]]) ,2]) # get rid of space (" ") in numeric data
    l[[i]] <- l[[i]][ !(l[[i]][,1] %in% c("", " ", "   ", "    ", "     ")),]  # get rid of empty lines
  }

  if(length(grep("\\.\\.", ch))>0) stop("Non numeric element in data: .. (two points)", call. = F)
  ch <- unique(unlist(strsplit(ch[!is.na(ch)], split = "")))
  ch <- ch[!(ch %in% c(NA , "."))]
  if(FALSE %in% (ch %in% as.character(0:9))) stop(paste("Non numeric element in data: ", paste(ch[!(ch %in% as.character(0:9))], collapse = ", "), " \n", sep = ""), call. = F)

  # ch <- list()
  # for(i in 1:length(l))  {  ch[[i]]  <- l[[i]][(length(kopf)+1):nrow(l[[i]]) ,]
  # ch[[i]]$plot <- names(l)[i]
  # }
  # ch <- do.call(rbind, ch)
  #
  # if(length(grep("\\.\\.", ch$V2))>0) stop(paste("Non numeric element in data: .. (two points) in plot:", ch$plot[grep("\\.\\.", ch$V2)]), call. = F)
  #
  # ch$nonnum <- gsub("\\d","",ch$V2)
  # x <- ch[!is.na(ch$nonnum) & !(ch$nonnum %in% c(".", "")),]
  # x

  # Warn if there is somewhere a Cover estimation without a species
  x <- do.call(rbind,l)
  x <- rownames(x)[which(is.na(x$V1) & !is.na(x$V2))]
  if(length(x)>0) stop(paste("Species name missing in Plot: ", paste(x, collapse = ", "), " \n", sep = ""), call. = F)


  # merge the list of plots
  mergefunc <- function(lis, kopf, fuzzy){

    x <- lis[[1]]
    x <- x[ x$V1 %in% kopf | !is.na(x[,2]),]
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
    rownames(x) <- x$V1
    x <- x[c(kopf, rownames(x[(length(kopf)+1):nrow(x),])[order(rownames(x[(length(kopf)+1):nrow(x),]))]),]
    rownames(x) <- NULL

    # use agrep fuzzy matching to find typos in the species names
    if(fuzzy){
      candis <- character()
      for(i in (length(kopf)+1):nrow(x)){
        if(!(x[i,1] %in% candis)){
          xx <- agrep(pattern = x[i,1], x[-i,1],  value = T)
          if(length(xx)>0) candis <- c(candis, x[i,1], xx, "\n")
        }
      }
      if(length(candis)>0) {
        mess <- paste("Warning: There might be a Typo in these names:", paste(
          # ((c(x[,1],"/")[candis])#, incomparables = "/", fromLast = F)
          candis, collapse = ", "), " \n")
        # mess <- gsub(" /, /,", "", mess)
        warning(mess, call. = F)
        if(write.fuzzy.mistakes) write(mess, "mistakes.txt")
        # cat( mess )
      }
    }

    # x <- x[, !(names(x) %in% "z")]

    return(x)

  }

  D <- mergefunc(lis = l, kopf = kopf, fuzzy=fuzzy)

  # Transpose data
  rownames(D) <- gsub(" ", "_", D[,1])
  names(D) <- gsub("Plot_", "", names(D))
  D[,1] <-NULL
  D <- as.data.frame(t(D))
  for( i in (length(kopf)+1):ncol(D)){
    D[, i] <- as.numeric(as.character(D[,i]))
  }
  D$Plotcode <- rownames(D)
  D <- D[,c("Plotcode", names(D)[-which(names(D) %in% c("Plotcode", "z"))])]
  rownames(D) <- NULL

  if(missing(outputfilename.xslx)) return(D) else write.xlsx(D, file = outputfilename.xslx)
}

# D <- eingabeformular2tabelle(inputfilename.xlsx = c("Eingabeformular_Grünland_HF_Alb.xlsx", "Eingabeformular_Grünland_HF_Hai.xlsx", "Eingabeformular_Grünland_HF_Sch.xlsx"), kopf = 1:14, fuzzy = T)


# Functions to correct species when using eingabeformular2tabelle
## Merge duplicated species in plots
corrections.merge.duplicates <- function(data, plot, species, method="higher"){
	x <- data[data[,1]==plot & data[,2]==species & !is.na(data[,2]) ,] # get the offending species from the plot
	rn <- row.names(x) # get the rownames
	for(i in 3:ncol(data)){ if(!is.na(max(as.numeric(x[,i])))){ # in the forest there are 4 columns with data
		if(method=="higher") data[rn[1],i] <- as.character(max(as.numeric(x[,i]), na.rm = T))  #sum up the two entries and write it down for the first entry
		if(method=="sum") data[rn[1],i] <- as.character(sum(as.numeric(x[,i]), na.rm = T))  #sum up the two entries and write it down for the first entry
		if(method=="mean") data[rn[1],i] <- as.character(mean(as.numeric(x[,i]), na.rm = T))  # alternativels take the mean
	}}
	data <- data[!(rownames(data) %in% rn[-1]),]  # remove all but the first entry from the data
	return(data)
}



## Change species names
corrections.change.name <- function(from, to, data, grep=F){
	count <- 0
	replaced <- character()
	if(is.list(data)){
		for(i in 1:length(data)){
			if(grep){
				x <- data[[i]][,2]
				replaced <- unique(c(replaced, x[grep(from, x)]))
				x[grep(from, x)] <- to
				x -> data[[i]][,2]
			} else {
				count <- count + length(which( data[[i]][,2]==from))
				data[[i]][,2] <- gsub(pattern = from, replacement = to, data[[i]][,2])
			}
		}
	} else {
		if(grep){
			replaced <-	unique(data[grep(from, data[,2]),2])
			unique(data[grep(from, data[,2]),2]) <- to
		} else {
			count <- length(which(data[,2]==from))
			data[,2] <- gsub(pattern = from, replacement = to, data[,2])
		}
	}
	if(grep) cat(paste(replaced[replaced!=to], "was replaced with", to, "\n")) else cat(paste(from, "was found", count, "times.\n"))
	return(data)
}






# Function to append new date from the new year to the e ntire data from the previous years

merge.old.new <- function(old, new, first.species.old){

	if(!is.numeric(first.species.old)) first.species.old <- which(names(old)==first.species.old)
  # is head complete
  stop<- FALSE
  if(length(unique(names(old)[1:(first.species.old-1)] %in% names(new))) != 1) {
    warning(paste("Missing variable in new data frame:",
                  names(old)[
                    c((!names(old) %in% names(new))[1:(first.species.old-1)]
                      , rep(FALSE, ncol(old)-first.species.old+1))
                  ]
    ), call. = F )
    stop <- TRUE
  }

  if(stop==FALSE){
    # add.missing.cols
    add.missing.cols <- function(from, to) {
      ta <- names(from)[! names(from) %in% names(to) ]
      to[,(ncol(to)+1):(ncol(to)+length(ta))] <- NA

      names(to)[(ncol(to)+1-length(ta)):ncol(to)] <- ta
      return(to)
    }

    old <- add.missing.cols(new, old)
    new <- add.missing.cols(old, new)

    # order
    old <- old[, c(names(old)[1:(first.species.old-1)], sort(names(old)[first.species.old:ncol(old)]) )]
    new <- new[, names(old)]

    # rbind
    x <- rbind(old, new)

    return(x)
  }
}


# # test
# o <- n <- as.data.frame(matrix(rnorm(25,25,1), ncol = 5))
# names(o)[4:5] <- 1:2
# names(n)[4:5] <- 3:4
# # names(n)[1] <- "t"
# merge.old.new(old = o, new = n, first.species.old = 3)


# Function to append new date from spring and summer (Forest)
merge.früh.spät <- function(früh, spät, erste = 10, woody){
	# Merge notes and date in spring
	früh$Bemerkungen <- ifelse(is.na(früh$Bemerkungen), paste("Date Frühblüher:", früh$Datum), paste(früh$Bemerkungen,"Date:", früh$Datum))

	früh <- früh[rowSums(früh[,erste:ncol(früh)], na.rm = T)>0,] # get rid of empty rows in Frühling
	rownames(spät) <- spät$Plotcode
	rownames(früh) <- paste(früh$Plotcode, "_K", sep = "" )

	for(i in rownames(früh)){
		spät[i,"Bemerkungen" ] <- ifelse(is.na(spät[i,"Bemerkungen"]), paste("Spring:", früh[i,"Bemerkungen"]), paste(spät[i,"Bemerkungen"],"|Spring:", früh[i,"Bemerkungen"]))
		spät[i,"BearbeiterIn" ] <- ifelse(is.na(spät[i,"BearbeiterIn"]), paste("Spring:", früh[i,"BearbeiterIn"]), paste(spät[i,"BearbeiterIn"],";Spring:", früh[i,"BearbeiterIn"]))
		for(j in names(früh)[erste:ncol(früh)]){
			if(!is.na(früh[i,j])){
				if(is.null(spät[i,j])){
					spät$X <- 0
					names(spät)[ncol(spät)] <- j
				}
				spät[i,j] <- ifelse(spät[i,j]>früh[i,j], spät[i,j], früh[i,j])
			}
		}
	}

	#Create Layer Infos and prune Plotcode
	nc <- max(nchar(spät$Plotcode))
	spät$Layer <- gsub("K", "H" , substr(spät$Plotcode,nc-1,nc ))
	spät$Plotcode <- substr(spät$Plotcode,1,nc-3 )

	# Put the woody species to the Strauch
	if(!missing(woody)){
		for(i in woody[which(woody %in% names(spät))]){
			x <- which(spät[,i]>0 & spät$Layer=="H")
			for(j in x){
				S <- spät[spät$Plotcode==spät$Plotcode[j] & spät$Layer=="S", i]
				H <- spät[spät$Plotcode==spät$Plotcode[j] & spät$Layer=="H", i]
				spät[spät$Plotcode==spät$Plotcode[j] & spät$Layer=="S", i] <- max(c(S,H), na.rm = T)
				spät[spät$Plotcode==spät$Plotcode[j] & spät$Layer=="H", i] <- 0
			}
		}
	}
	return(spät)
}


# Function to sum two or more species to a new column.
aggregate.species <- function(data, from, to, order.from.which.column, method="sum"){
  # check if nothing essential is missing
  if(missing(data) | missing(from) | missing(to)) {warning("something essential is missing", call. = F) ;break}
  # Kick out those that are not in the data set
  from <- from[which(from %in% names(data))]
  # if only one from (and to not in data: rename column, else start the process
  if(length(from) == 1 & !(to %in% names(data))){
    names(data)[which(names(data)==from)] <- to
  } else {
  	from <- unique(c(from,to))
    # Create column
    data[,ncol(data)+1] <- NA
    # aggregate species
    if(method=="sum") data[,ncol(data)] <- rowSums(data[,from], na.rm = TRUE)
    if(method=="higher") data[,ncol(data)] <- apply(data[,from], MARGIN = 1, FUN = function(x){x[is.na(x)] <- 0; max(x ,na.rm = TRUE)})
    # delete all redundant species
    data[,from] <- NULL
    # rename new column
    names(data)[ncol(data)] <- to
    # # Zero become NA
    # data[data[,ncol(data)]==0, ncol(data)] <- NA
  }
  # order columns alphabethically

  if(!missing(order.from.which.column)){
  	if(is.character(order.from.which.column)) order.from.which.column <- which(names(data)==order.from.which.column)
    if(order.from.which.column==1) data <- data[, sort(names(data))] else {
      data <- data[, c( names(data)[1:(order.from.which.column-1)], sort(names(data)[order.from.which.column:ncol(data)]))]
    }
  }
  return(data)
}



# function to turn 0 into NA or vice-versa
zeros2na <- function(data, umbekehrt=TRUE, first.species){ # Function to replace all 0 to NA

  if(missing(first.species)) first.species <- 1 else if(!is.numeric(first.species)) first.species <- which(names(data)==first.species)


  if(umbekehrt){
    for(i in first.species:ncol(data)){
      if(is.numeric(data[,i]) | (length(unique(data[,i])) == 1 & is.na(unique(data[,i])[1]) )){
        data[is.na(data[,i]),i] <- 0
      }
    }
  } else {
    for(i in first.species:ncol(data)){
      if(is.numeric(data[,i])){
        data[data[,i]==0 & !is.na(data[,i]),i] <- NA
      }
    }
  }
  return(data)
}



# wurde der plot erhoben? Function to convert plots without any record (in a year) to NA
plot.surveyed <- function(data, first.species, index){
  if(missing(index)) py <- paste(data$Useful_EPPlotID, data$Year) else py <- index

  if(missing(first.species)) warning("First species missing") else if(!is.numeric(first.species)) first.species <- which(names(data)==first.species)

  for( i in unique(py)){
    if( sum(data[py==i, first.species:ncol(data)], na.rm = T)==0){
      data[py==i, first.species:ncol(data)] <- NA
    }
  }
  return(data)
}


# COMPARO
# how was data in a dataframe changed. The comparo function helps tracking changes that were made to data! Just add in two dataframes and define an unique identifier (column) and let it run!!!
comparo <- function(d1, d2, ID, stop.after=50, subset, output="console"){

	if((ID %in% names(d1) & ID %in% names(d2))==FALSE){ # The ID column has to be in both datasets
		cat(paste(ID, "was not found in both datasets"))
		} else{

		if(!missing(subset)){ # you can specify a subset of columns which to compare. This can be names or numbers
			if(is.numeric(subset)) subset <- names(d1)[subset] # if numbers are chosen, translate to names
			subset <- unique(c(ID, subset))
			d1 <- d1[,subset]
			d2 <- d2[,subset]
		}

		if(identical(names(d1), names(d2))==FALSE){ # are the columns identical?
			if(length(which(!(names(d1) %in% names(d2))))>0){
				cat(paste("These columns are in d1 but missing in d2:", names(d1)[which(!(names(d1) %in% names(d2)))], "\n", sep="\n"))
			}
			if(length(which(!(names(d2) %in% names(d1))))>0){
				cat(paste("These columns are in d2 but missing in d1:", names(d2)[which(!(names(d2) %in% names(d1)))], "\n", sep="\n"))
			}
		} else {
			if(length(unique(d1[,ID]))!=nrow(d1) | length(unique(d2[,ID]))!=nrow(d2)) { #Is the ID unique? Sometimes this might be a problem with years. You can paste plot and year to get a unique ID
				cat("\nID is not unique. ")
			} else {
				if(identical(sort(d1[,ID]), sort(d2[,ID]))==FALSE) { # Are all IDs in both dataset. Order doesnt matter
					if(length(which(!(d1[,ID] %in% d2[,ID])))>0){
						cat(paste("These IDs are in d1 but missing in d2", d1[which(!(d1[,ID] %in% d2[,ID])), ID], "\n", sep="\n"))
					}
					if(length(which(!(d2[,ID] %in% d1[,ID])))>0){
						cat(paste("These IDs are in d2 but missing in d1", d2[!(d2[,ID] %in% d1[,ID]), ID], "\n", sep="\n"))
					}
				} else { # good. Here we can start the loop that compares the datasets

					I <- 1 # Index for the stop and to show the number of the difference
					if(output!="console"){
						R <- data.frame(ID=NA, col=NA, d1=NA, d2=NA)[0,]
						stop.after <- prod(dim(d1))
					}

					for(j in names(d1)){
						for(i in d1[,ID]){

							if(is.na(d1[d1[,ID]==i,j])) d1[d1[,ID]==i,j] <- "NA" # NA's are translated into "NA" to avoid problems
							if(is.na(d2[d2[,ID]==i,j])) d2[d2[,ID]==i,j] <- "NA"
							if(d1[d1[,ID]==i,j]!=d2[d2[,ID]==i,j]){ # Check if two values are identical
								if(output=="console"){
									cat(paste("\n\n", I, ". Difference detected in '", j, "' of '", i, "':\nd1:  ", d1[d1[,ID]==i,j], "\nd2:  ", d2[d2[,ID]==i,j], sep = "" ))
									} else {
									R[I, "ID"] <- i
									R$col[I] <- j
									R$d1[I] <- d1[d1[,ID]==i,j]
									R$d2[I] <- d2[d2[,ID]==i,j]
								}

								I <- I+1
								if(I==stop.after+1) break
							}
							if(I==stop.after+1) break
						}
						if(I==stop.after+1) break
					}
					if(I==stop.after+1) cat(paste("\n\nComparing was stopped after ", stop.after, ". There might be more differences", sep = "")) # tell the user that there might be more differences
				}
			}
		}
	}
	if(output!="console") return(R)
}

#
#
# d1 <- read.xlsx("Rohdaten/REXLUX/RP_UP_2023 Header_vorlage.xlsx")
# d2 <- read.xlsx("Results/REXLUX/RP_UP_2023 Header.xlsx")
# ID <- "useful.PlotID"
#
# comparo(d1,d2, ID, stop.after = 50, subset = c(1:9,11,12))  #comparo with numerical subset
# comparo(d1,d2, ID, stop.after = 50, subset = names(d1)[c(1:9,11,12)]) # comparo with character subset
# comparo(d1[1:4,],d2[1:4,], ID, stop.after = 11) # comparo with early stopping
# comparo(d1[sample(1:nrow(d1)),],d2, ID, stop.after = 50, subset = c(1:9,11,12))  #comparo with numerical subset and reordered data






