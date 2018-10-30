
# setwd("./Judith/")
# kopf <- read.table("kopf.md", sep="[")
# data <- read.csv("daten.csv")
# artenliste(data = read.csv("Untitled 1.csv"), kopf = "kopf.md")


artenliste <- function(daten, kopf="kopf.md", titel=format(Sys.time(), "%b %Y"), fuss="dfhdfahadh", fontsize="\\large", table.length.adjust=0){
	if(file.exists(kopf)){  # lade kopf  # This is the stuff that's written between the title and the species list
		head <- as.character(read.table(kopf, sep="[")[,1])
	} else {
		warning("ACHTUNG: Der Kopf konnte nicht gefunden werden! Stimmt der Dateiname?")
		head <- ""
	}

	data <- read.csv(daten)    # .csv file einlesen
	if(ncol(data) ==1) data <- read.csv2(daten)   # falls nur eine Spalte erkannt wurde mit csv 2 probieren



	x <- character()
	for (i in 1:ncol(data)){ #loop for each column (=each Plot) of the data

		size <- fontsize      # this is the latex command for the font size (eg. \\tiny, \\huge)
		header <- "\\begin{tabularx}{\\textwidth}{|l|X|l|X|} \\hline"    # this is the header for the table for the species list
		end <- "\\end{tabularx}"  # this ends the table
		arten <- as.character(data[,i])   # this is the list of speceis
		arten <- arten[which(arten!="")]   #cleanup
		arten <- gsub("_"," ",arten)    # remove underlines
		arten <- sort(arten)          # sort


		cutoff <- 42-length(head) + table.length.adjust   # this is how many rows the list has on the 1st page. It gets shorter when the head is longer
		cutoff2 <- 42 + table.length.adjust               # this is the number of rows on the subsequent pages.


		## Generate the title, extra code is so that can also be displayed in the footer including page number Plotname and date
		totpage <- if(length(arten)<(2*cutoff)) {1} else { ceiling((length(arten)-(2*cutoff))/(2*cutoff2))+1 }

		if(fuss=="dfhdfahadh") {fuss <- titel} else {paste(names(data)[i], fuss)}

		title <- paste("\\centering \\section*{", names(data)[i]," ", titel, "}","\\markboth{/", totpage, " | ", names(data)[i]," ",fuss, "}{", names(data)[i]," ",fuss,"}", sep="")
		##

		## Generate the tables
		if(length(arten)<cutoff){   # how to generate the list when less species than the short cutoff
			arten <- c(arten, rep("", (cutoff-length(arten) )))
			arten <- paste( "\\phantom{--} &", arten, "& \\phantom{--}  & \\\\\\hline")
			table <- c(header,arten,end,"")  # table consists of header, list and end
		}

		if(length(arten)>cutoff & length(arten)<(cutoff*2)){ #wenn die Arten die zeilenanzahl von cutoff ÃŒberschreiten-> 2 zeilen machen
			a1 <- arten[1:cutoff]
			a2 <- arten[(cutoff+1):length(arten)]
			if(length(a1) !=length(a2) ) a2 <- c(a2, rep("", (length(a1)-length(a2) )))
			arten <- paste( "\\phantom{--} &", a1, "& \\phantom{--} &", a2, "\\\\\\hline")
			table <- c(header,arten,end,"")
		}

		if(length(arten)>(cutoff*2)){   #wenn die Arten die zeilenanzahl von erster seite ÃŒberschreiten-> mehrere Tabellen machen
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
	x <- gsub("ä", '"a', x)
	x <- gsub("ö", '"o', x)
	x <- gsub("ü", '"u', x)
	x <- gsub("Ä", '"A', x)
	x <- gsub("Ö", '"O', x)
	x <- gsub("Ü", '"U', x)
	x <- gsub("ß", '"s', x)

	write(x = x, file = "Artenliste.md", ncolumns = 1)
}
