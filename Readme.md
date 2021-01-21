# Scripte zur Verwaltung von Artenlisten bei Vegetationsaufnahmen 

## Anleitung zur Herstellung von Artenlisten

Mit diesem Script erstellt man leicht Aufnahmebögen (als PDF) für Vegetationsaufnahmen. 

### Benötigt wird

- Programme
  - R (https://cran.r-project.org/bin/windows/base/) mit (`openxlsx`-Paket)
  - pandoc (https://github.com/jgm/pandoc/releases/)
  - miktex (https://miktex.org/download)

- Daten:
  - Tabelle der Arten pro Plot in Spalten. Die erste Zeile bezeichnet die Namen der Plots. Im `csv`- oder `xlsx`Format gespeichert.
  - Optional: Textdatei für den Kopf (Inhalt zwischen Titel und Artentabelle). Sollte das Zeichen `]` nicht enthalten. 
- Skripte
  - das `Artenlistenskript.r` (enthält das R skript für alle Funktionen)
  - die Datei `kopf.md` (enthält die Tabelle nach dem Titel, Format ist LaTeX)
  - die Datei `preamble.txt` enthält die Formatierung für das PDF

### Vorgehen

1.  In **R** (Rgui oder Rstudio usw.)

  - Das Working directory zum Ordner mit dem Skript wechseln
    - In RGui unter Datei>Verzeichnis wechseln
    - In Rstudio unter Session>Set Working Directory>Choose Directory

2.  Rskript einlesen (copy/paste oder Befehl `source("Artenlistenskript.r")`)

  - nun steht die Funktion `artenliste()` zur Verfügung. 

    - mit dem Argument `daten=` gibt man an wie die Datentabelle heisst. Beispiel: `artenliste( daten = "daten.csv")`

  - Optional stehen noch weitere Argumente zur Verfügung:

    - `titel=`: Was soll nebst dem Plotnamen noch im Titel stehen. Standardmässig besteht die Titelzeile aus dem Plotname und dem aktuellen Monat und Jahr!
    - `kopf=`: Was ist der Name der Datei die den Kopf enthält. Standard ist "kopf.md".
    - `fuss=`: Was soll nebst dem Plotnamen noch in der Fusszeile stehen. Standard ist der Titel.
    - ***Neu für 2021***`wald=`: Handelt es sich um Waldplots? und `waldextrablatt=`: soll ein leeres Blatt angefügt werden wenn nicht mehr viel Platz (~8 Zeilen) übrig sind.

    **Beispiel:** `artenliste(daten = "daten.csv", kopf = "kopf.md", titel = "Vegetationsaufnahme SADE Grünland Alb (2018)", fuss="SADE 2018", wald=FALSE)`

3.  Nach dem Ausführen des Skripts sollte sich im Ordner nun für eine Datei namens `Artenliste.md` befinden.

4.  In **pandoc** (Hilfe: http://pandoc.org/getting-started.html)
     - Öffne Programm `Windows PowerShell( x64)`
     - wechsle zum Ordner mit den Skripts: `cd 'N:\Documents\Artenlistenskript\'` 
       - Den Pfad zum Ordner findet man auch wenn man im Explorer im Ordner rechte Maustaste>Properties unter Location nachschaut.
    - folgenden *Befehl* eingeben: `pandoc -o Artenlisten.pdf  --template=preamble.txt ./Artenliste.md`
    - `Artenlisten.pdf` erscheint. Bravo!

### Probleme

- Sind die Artnamen zu lang, können die Tabellen zu lang werden. Das Seitengefüge wird auseinander gerissen. Einzelne hie und da mag es verkraften.
  - Lösungen: 
    - Artnamen abkürzen.
    - Schriftgrösse ändern (in R `fontsize="\\normalsize"` oder  `fontsize="\\small"`(standard ist large) )
    - Tabellen kürzen (in R `table.length.adjust= -3` )

## Eintippen und zusammenfügen der Vegetationsdaten (***Neu 2021***)

Um die Ausgefüllten Aufnahmebögen leichter einzutippen kann mit der Funktion  `eingabeformular()` eine praktische Excel-Datei generiert werden. Diese Datei kann dann mit der Funktion `eingabeformular2tabelle()` zu einer Gesamttabelle zusammengefügt werden.

### Benötigt wird

- R (https://cran.r-project.org/bin/windows/base/) mit (`openxlsx`-Paket)
- Tabelle der Arten pro Plot in Spalten. (Dieselbe die auch oben verwendet wurde)
- das `Artenlistenskript.r` (enthält die Funktionen)

### Vorgehen

1. Rskript einlesen (copy/paste oder Befehl `source("Artenlistenskript.r")`)

2. Nun kann mit der Funktion `eingabeformular()`  die Eingabeformular erstellt werden.

   - mit dem Argument `daten=` gibt man an wie die Datentabelle heisst. Beispiel: `eingabeformular( daten = "daten.csv")`

3.  Optional stehen noch weitere Argumente zur Verfügung:

    - `kopf=`: Welche Kopfdaten müssen zusätzlich zu den Arten eingetippt werden. Beispiel: `kopf = c("Bemerkungen", "Datum", "Totholz", "Moos", "Gestein") `
    - `explo=`: Nur ein Subset der Plots, beginnend mit einem bestimmten Buchstaben, soll verwendet werden. Beispiel: `explo="A"`
    - `wald=`: Handelt es sich um Waldplots? Es werden Spalten für die 4 Ebenen (K, S, B1, B2) generiert.
    - `filename=`: Als was soll die Datei gespeichert werden?

    **Beispiel:** `eingabeformular(daten = "Frühblüherliste_GAP.csv", explo = "H", kopf = c("Bemerkungen", "Datum", "Totholz", "Moos", "Gestein") , wald = F, filename = "Eingabeformular_Frühblüher_GAP_Hai.xlsx" )`

4.  Daten eintippen!!!

5.  Mit der Funktion `eingabeformular2tabelle()` kann das Eingabeformular zu einer Gesamttabelle zusammengefügt werden:

    - mit dem Argument `inputfilename.xlsx =` gibt man den Namen des Eingabeformulars an.

6.  Optional

    - `kopf=` benennt oder zählt die Kopfdaten.
    - `outputfilename.xslx=` gibt den Namen der geschriebenen Datei an. Wenn nicht spezifiziert wird die Tabelle direkt in R gelesen.
    - `fuzzy=` schaltet die Tippfehlersuche ein oder aus.

    **Beispiel:** `eingabeformular2tabelle(inputfilename.xlsx = "Eingabeformular_Frühblüher_HF_Hai.xlsx", kopf = 1:5, fuzzy= TRUE)`

7.  Fertig.

























