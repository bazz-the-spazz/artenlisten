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

-  In R 
  - Das Working directory zum Ordner mit dem Skript wechseln
  - Rskript einlesen (copy/paste oder `source(“Artenlistenskript.r”)`)
  - folgenden Befehl eingeben: `artenliste(data=read.csv("daten.csv"), kopf="kopf.md")`
    - Standardmässig besteht die Titelzeile aus dem Plotname und dem aktuellen Monat und Jahr. Dies kann aber mit  `titel="Vegetationsaufnahme SADE Grünland Alb (2018)"` geändert werden.
    - Die Fusszeile beinhaltet neben Seitenzahl die Titelzeile. Dies kann aber mit  `fuss="SADE 2018"` geändert werden.
    -  Beispiel: `artenliste(data=read.csv("daten.csv"), kopf="kopf.md", titel="Vegetationsaufnahme SADE Grünland Alb (2018)", fuss="SADE 2018")`
  - im Ordner `md` sollten sich nun für jeden Plot eine Datei befinden (z.B `AEG1.md`)
-  In pandoc (Hilfe: http://pandoc.org/getting-started.html)
  - Windows Powershell öffnen und zum Ordner mit den Skripten wechseln
  - folgenden Befehl eingeben: `pandoc -o Artenlisten.pdf  --template=preamble.txt ./md/*.md`
  - `Artenlisten.pdf` erscheint

## Probleme

- Sind die Artnamen zu lang,  können die Tabellen zu lang werden. Das Seitengefüge wird auseinander gerissen. Einzelne hie und da mag es verkraften.
  - Lösungen: 
    - Artnamen abkürzen.
    - Schriftgrösse ändern (in R `fontsize="\\normalsize"` oder  `fontsize="\\small"`(standard ist large) )
    - Tabellen kürzen (in R `table.length.adjust= -3` )

