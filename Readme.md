# Scripte zur Verwaltung von Artenlisten für Vegetationsaufnahmen 

![](https://imgs.xkcd.com/comics/code_lifespan.png)

## 1. Herstellung von Aufnahmebögen

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
    - `wald=`: Handelt es sich um Waldplots? und `waldextrablatt=`: soll ein leeres Blatt angefügt werden wenn nicht mehr viel Platz (~8 Zeilen) übrig sind.
    - `extrazeile=`: Eine zusätzliche Information kann zwischen Kopf und Tabelle eingefügt werden.
    - `find.in.head=` & `replace.in.head=`: Eine Textzeile im Titel kann angepasst werden

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

## 2. Eintippen und zusammenfügen der Vegetationsdaten

Um die Ausgefüllten Aufnahmebögen leichter einzutippen kann mit der Funktion  `eingabeformular()` eine praktische Excel-Datei generiert werden. Diese Datei kann dann mit der Funktion `eingabeformular2tabelle()` zu einer Gesamttabelle zusammengefügt werden.

### Benötigt wird

- R (https://cran.r-project.org/bin/windows/base/) mit (`openxlsx`-Paket; installieren mit `install.packages("openxlsx")`)
- Tabelle der Arten pro Plot in Spalten. (Dieselbe die auch oben verwendet wurde)
- das `Artenlistenskript.r` (enthält die Funktionen)

### Vorgehen

#### Daten digitalisieren

1. Rskript einlesen (copy/paste oder Befehl `source("Artenlistenskript.r")`)

2. Nun kann mit der Funktion `eingabeformular()`  das Eingabeformular erstellt werden.

   - mit dem Argument `daten=` gibt man an wie die Datentabelle heisst. Beispiel: `eingabeformular( daten = "daten.csv")`

3.  Optional stehen noch weitere Argumente zur Verfügung:

    - `kopf=`: Welche Kopfdaten müssen zusätzlich zu den Arten eingetippt werden. Beispiel: `kopf = c("Datum", "Moosschicht", "Gestein", "Aufnahmefläche")`
    - `explo=`: Nur ein Subset der Plots, beginnend mit einem bestimmten Buchstaben, soll verwendet werden. Beispiel: `explo="A"`
    - `wald=`: Handelt es sich um Waldplots? Es werden Spalten für die 4 Ebenen (K, S, B1, B2) generiert.
    - `filename=`: Als was soll die Datei gespeichert werden?

    **Beispiel:** `eingabeformular(daten = "daten.csv", explo = "A", kopf = c("Vegetations-Höhe1", "Vegetations-Höhe2", "Vegetations-Höhe3", "Vegetations-Höhe4", "Vegetations-Höhe1", "Vegetations-Höhe2", "Vegetations-Höhe3", "Vegetations-Höhe4", "Vegetations-Höhe_Durchschnitt", "S (verholzt) >0-5m", "Streu", "BearbeiterIn", "Krautschicht", "Totholz", "Datum", "Moosschicht", "Gestein", "Aufnahmefläche", "Flechtenschicht", "Offene Erde") , wald = F, filename = "Eingabeformular_Alb.xlsx" )`

4. Daten in die generierte Excelliste eintippen!!!

   

#### Daten zusammenfügen



1. Rskript einlesen (copy/paste oder Befehl `source("Artenlistenskript.r")`)

2. Mit der Funktion `eingabeformular2tabelle()` kann das Eingabeformular zu einer Gesamttabelle zusammengefügt werden:

   - mit dem Argument `inputfilename.xlsx =` gibt man den Namen des Eingabeformulars an. Es können auch mit `c(1.xlsx,2.xlsx,3.xlsx)` mehrere Formulare zusammen gelesen werden (wenn sie die selben Dimensionen haben)!

3. Optional

   - `kopf=` benennt oder zählt die Kopfdaten.
   - `outputfilename.xslx=` gibt den Namen der geschriebenen Datei an. Wenn nicht spezifiziert wird die Tabelle direkt in R gelesen.
   - `fuzzy=` schaltet die Tippfehlersuche ein oder aus.
   - `write.fuzzy.mistakes=` sollen die Fehler in seperater txt-Datei gespeichert werden?
   - `wald=` optional, sind layers zu erwarten?

   **Beispiel:** `eingabeformular2tabelle(inputfilename.xlsx = "Eingabeformular_Alb.xlsx", kopf = 1:5, fuzzy= TRUE)`

4. Fertig.

5. **Neu:2023**: Zwei Funktionen helfen bei der Fehlerbehebung: `corrections.change.name()` und `corrections.merge.duplicates()`

6. **Neu:2023**: Funktion zum Zusammenfügen der Frühblüher- und SommerWaldaufnahmen: `merge.früh.spät()`


## 3. Zusammenführen mit früheren Aufnahmen:

Die neue Tabelle kann mit den bisherigen Aufnahmen zusammengefügt werden. 

### Benötigt wird

- Tabelle der bisherigen Aufnahmen (je eine Zeile für jeden Plot, Spalten für Metadaten und Arten (erst Meta, dann Arten!))
- das `Artenlistenskript.r` (enthält die Funktionen)

### Vorgehen

#### Zusammenfügen

1. Rskript einlesen (copy/paste oder Befehl `source("Artenlistenskript.r")`)
2. Mit der Funktion `merge.old.new())` werden die alten und neuen Daten zusammengefügt.
   - Die Daten müssen schon als data.frame in R geladen sein.
   - Argumente:
     - `old=`: Name des Dataframe der vorderen Jahre
     - `new=`: Name des Dataframe des neuen Jahres
     - `first.species.old=`: Nummer der Spalte, die die erste Art enthält (nach den Metadaten)

#### Arten zusammenlegen

Es kann vorkommen, dass eine Art unter mehreren Namen aufgenommen wurde mit der Funktion `aggregate.species`können diese zusammengelegt werden:

- Argumente:
  - `data=`: Name des Dataframe
  - `from=`: Namen der Arten die zusammengelegt werden sollen. z.B.  c("Trifolium_campestre", "Trifolium_dubium")
  - `to=` : Name der neuen Art: E.g. "Trifolium campestre/dubium"
  - `method=`: wie sollen die Arten kombiniert werden
    - `"sum"`: die Werte werden addiert
    - `"higher"`: der höchste Wert wird gebraucht
  - `order.from.which.column`: Nummer der Spalte von der aus Alphabethisch sortiert werden soll. Normalerweise von der ersten Art.

#### NA oder 0

Mit der funktion `zero2na()` werden Nullen zu NA (oder umgekehrt) geändert.

- Argumente:
  - `data=`: Name des Dataframe
  - `first.species`: Nummer der Spalte mit der ersten Art
  - `umbekehrt=`: 
    - Wenn FALSE: Nullen werden zu NA
    - Wenn TRUE: NAs werden zu Nullen

#### Wurde der Plot besucht?

Es kann sein, dass in einem bestimmten Jahr ein Plot gar nicht besucht wurde und keine Daten aufgenommen sind, der Plot dennoch in den Daten geführt wird. Dafür sollten alle Arten auf NA (nicht auf 0) gesetzt werden. Mit der Funktion `plot.surveyed()` wird erst geschaut, ob es in einem Plot aufnahmen gibt. Falls solche fehlen, werden alle Arten auf NA gesetzt.

- Argumente:
  - `data=`: Name des Dataframe
  - `first.species`: Nummer der Spalte mit der ersten Art
  - `index=`: Ein eindeutiger Index für alle Plot X Jahr Kombinationen. Standard ist `index <- paste(data$Useful_EPPlotID, data$Year)`



