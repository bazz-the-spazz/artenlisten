# Anleitung um Artenliste zu generieren

## Benötigt wird

- Programme
  - R (https://cran.r-project.org/bin/windows/base/)
  - pandoc (https://github.com/jgm/pandoc/releases/)
  - miktex (https://miktex.org/download)

- Daten:
  - Liste der Arten in Spalten. Die Kopfzeile ist dabei die Namen der Plots. Am besten in `csv` oder anderem R-lesbaren Format.
  - Optional: Textdatei für den Kopf (Inhalt zwischen Titel und Liste). Sollte das Zeichen `]` nicht enthalten. 
- Skripte
  - das `Artenlistenskript.r` (enthält das R skript)
  - die Datei `kopf.md` (enthält die Tabelle nach dem Titel)
  - die Datei `preamble.txt` enthält die Formatierung für das PDF

## Vorgehen

-  In R (Rgui oder Rstudio usw.)
  - Das Working directory zum Ordner mit dem Skript wechseln
    - In RGui unter Datei>Verzeichnis wechseln
	- In Rstudio unter Session>Set Working Directory>Choose Directory
  - Rskript einlesen (copy/paste oder Befehl `source("Artenlistenskript.r")`)
  - nun steht die Funktion 'artenliste(daten)' zur Verfügung. 
      - mit dem Argument 'daten=' gibt man an wie die Datentabelle heisst. Beispiel: 'artenliste("test_daten.csv")
	  
	  - Optional stehen noch weitere Argumente zur Verfügung:
	    - 'titel=': Was soll nebst dem Plotnamen noch im Titel stehen. Standardmässig besteht die Titelzeile aus dem Plotname und dem aktuellen Monat und Jahr. 
		- 'kopf=': Was ist der Name der Datei die den Kopf enthätl. Standart ist "kopf.md".
		- 'fuss=': Was soll nebst dem Plotnamen noch in der Fusszeile stehen. Standart ist der Titel.
		- weiter Argumente sind unter 'Probleme' beschrieben.
        -  Beispiel: `artenliste(daten="test_daten.csv", kopf="kopf.md", titel="Vegetationsaufnahme SADE Grünland Alb (2018)", fuss="SADE 2018")`
   - im Ordner sollten sich nun für eine Datei names `Artenliste.md` befinden.
   
-  In pandoc (Hilfe: http://pandoc.org/getting-started.html)
  - Öffne Programm 'Windows PowerShell( x64)'
  - wechsle zum Ordner mit den Skripts: cd 'N:\Documents\Artenlistenskript von Baschi\' 
  - Den Pfad zum Ordner findet man auch wenn man im Explorer im Ordner rechte Maustaste>Properties unter Location nachschaut.
  - folgenden Befehl eingeben: `pandoc -o Artenlisten.pdf  --template=preamble.txt ./Artenliste.md`
  - `Artenlisten.pdf` erscheint

## Probleme

- Sind die Artnamen zu lang,  können die Tabellen zu lang werden. Das Seitengefüge wird auseinander gerissen. Einzelne hie und da mag es verkraften.
  - Lösungen: 
    - Artnamen abkürzen.
    - Schriftgrösse ändern (in R `fontsize="\\normalsize"` oder  `fontsize="\\small"`(standard ist large) )
    - Tabellen kürzen (in R `table.length.adjust= -3` )

