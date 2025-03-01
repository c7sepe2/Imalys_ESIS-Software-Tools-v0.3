unit format;

{ FORMAT sammelt Definitionen (Datentypen) und Konstanten für alle Module
  TOOLS enthält elementare IO- und Konvertierungs-Routinen }

{ Nomenklaturen:
  a* = array_Name
  b* = boolean_Name
  c* = Konstante, alle Formate
  d* = Datei_Name
  e* = Eingabe, alle Formate
  f* = float_Name = Fließkomma-Zahlen
  g* = Set(menGe)_Name
  h* = Handle_Name
  i* = integer_Name = ganze Zahlen
  j* = Zwischenlager für Variable
  k* = Kombination (IO-Hilfsvariable)
  l* = Lokal = in Routine definierte Routine
  m* = Menü_Name
  n* = Array|Record-Dimension (integer)
  o* = Objekt_Name (nicht visuell)
  p* = Zeiger_Name, auch Präfix
  q* = Debugger-Hilfsvariable
  r* = Record_Name
  s* = String_Name
  t* = Typ_Name
  u* = Unit_Name,
  v* = Datenmodul_Name (unit mit Objekten)
  w* = Fenster_Name (unit mit Fenstern
  x* = Matrix_Name = mehrdimensionales Array
  y* = Funktion, Procedur als Variable
  z* = Zeiger auf Dimension

  cc*, rr*, ss* .. Verdopplung wenn in Klasse definiert }

{ FORMELN:
  - Varianz = (∑x²-(∑x)²/n)/(n-1)
  - Kovarianz = (∑xy - ∑x∑y/n)/(n-1)
  - Regression = Kovarianz / Varianz
  - Regression = (∑xy-∑x∑y/n) / (∑y²-(∑y)²/n) zur Schätzung von y aus x
  - Korrelation = (∑xy-∑x∑y/n) / sqrt((∑x²-(∑x)²/n)*(∑y²-(∑y)²/n))
  - X²-Test = ∑((n-e)²/e) mit n:Besetztzahl, e:Erwartung
  - StandardNormalVerteilung N(x)=1/sqrt(2Pi)*exp(-x²/2) Mittelwert=0, Abweichung=1
  - NormalVerteilung N(x)=1/(sqrt(2Pi)*s)*exp(-(x-u)²/2s²) u=Mittel, s=Abweichung
  - Rangkorrelation = 1-6∑(x-y)²/(n/(n²-1) (Spearmann) x,y=Rang n=Vergleiche
  - ODER (C-D)/sqrt(C+D+X)/sqrt(C+D+Y) C,D=Ränge, X,Y=Kopplung }

{ BEFEHLE:
  - OsCommand('sh','hostname') gibt den Host (Rechner) zurück
  - OsCommand('gvfs-trash',PATHNAME) verschiebt PATHNAME in den trash
  - OsCommand('sh','rm '+DIR+'temp*') entfernt Dateien die mit "temp" beginnen aus DIR
  - InputBox(Caption,'Prompt',Default) übernimmt Eingaben }

{ BIT-FORMAT:
  (·) Tabellen bestehen aus drei Abschnitten:
  (1) Zahl: Offset für Metadaten-Block: eine int64-Zahl
  (2) Datenblöcke: Länge potentiell variabel
      Datenblöcke bestehen grundsätzlich aus 4-Byte-Zahlen. Anzahl und Länge
      sind nicht begrenzt. Jeder Datenblock kann eine andere Länge haben.
  (3) Metadaten: Array mit Offsets für alle Datenblöcke incl. Metadaten-Block.
      Das Array ist int64 formatiert }

{ ZONEN-INDEX:
   - "index.hdr": Bild mit allen Zonen-IDs als integer, Null für NoData
   - "index.shp": Grenzen zwischen allen Zonen
   - "index.bit": Attribute (Mittelwerte) für alle Zonen als Scalar
   - "topology.bit": Zonen-Verknüpfung (Integers)
      - Dimension(Dim): Index für "Neighbor" und "Perimeter". Der Eintrag ist
        der Beginn (ID) des Datenblocks der indizierten Zone. Die Zone Null
        steht für Nodata. Letzter Eintrag ist der Übertrag zur Zone N+1
      - Neighbor(Nbr): IDs aller Nachbar-Zonen als Array incl. der eigenen ID
      - Perimeter(Prm): Anzahl Pixel mit gemeinsamer Grenze (Kontakte) zur
        Nachbar-Zone. Pixel innerhalb der Zone sind genauso indiziert. }

{ PROJ4-FORMAT:
  '+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs' }

{ HOME:
   - Imalys-Arbeitsverzeichnis
   - Bilddaten: 32 Bit Float, beliebig viele Kanäle, ENVI-Header
   - Klassen: 8 Bit Integer, Palette im Header
   - Tabellen: BIT-Format für Zonen-Attribute, CSV für Statistik }

{ KANAL-NAMEN:
   - Ort (geografisch, Projekt ..)
   - Qualität (bands, thermal, NirV, ..)
   - Spezifikation (Regression, Median, ..)
   - Sensor (S2, L8, ..), Datum [YYYYMMDD]
   - getrennt durch Underscore }

{ AKTUELLE VERÄNDERUNGEN:
   → Zonen:
      - völlig neu gestalteter Prozess
      - innerhalb einer Zone ist die Varianz minimal
      - Kontrast an Grenzen nur indirekt bewertet
      - Suche nach lokalen Minimum zwischen zwei Zonen
      - eindeutige Paare werden vereinigt
      - Prozess iteriert
      - zu Beginn Pixel=Zonen
      - Abbruch wenn mittlere Zonen-Größe Schwelle erreicht
      - sehr kleine Zonen (wenige Pixel) können unterdrückt werden
      - "bonds=accurate" übernimmt Klassen-Grenzen als Zonen
   → Zonen als Kernel:
      - Pixel innerhalb einer kleinen Fläche vergleichen
      - üblich innerhalb eines quadratischen Fensters
      - in Imalys innerhalb einer Zone
      - quadratische Fenster wirken als Weichzeichner
      - Zonen berücksichtigen natürliche Grenzen
      - Kleine Strukturen werden erfasst
      - alle Kanäle getrennt berechnet
      - am Ende Hauptkomponente
   → Diversität erweitert
      - a-Diversität innerhalb abgegrenzter Objekten
      - ß-Diversität zwischen abgegrenzten Objekten
      - spektrale und morphologische Diversität
   → Datum im Kanal-Namen
      - Trends und Perioden benötigen das Datum der Aufnahme
      - Imalys schreibt Datum an das Ende der Kanal-Namen
      - Werden Bilder gestapelt, übernimmt "Compile" die Datums-Angaben in die
        Metadaten.
      - WICHTIG wenn externe Formate (TIFF) verwendet wird
   → Flächen:
      - Flächen Attribut in [ha]
      - Proportion: Verhältnis eigene Fläche / Mittelwert aller Nachbarn
   → Compile.Frame:
      - "frame" unter "compile" setzt alle Bildteile auf NoData die außerhalb
        eines übergebenen Polygons liegen
   → Compile.Projection:
      - schreibt die Projektion aus einem Vorbild in ein beliebiges Bild
   → Reduce.Brightness:
      - bestimmt NUR die erste Hauptkomponente als "Helligkeit".
      - Brightness ist durch eigenen Befehl von "Principal" getrennt
   → Mapping.Equalize:
      - gleiche Wertebereiche für alle Attribute ← nur Zonen
   → Mapping.FabricMap:
      - zwei Optionen: Klassen oder Merkmale räumlich kombinieren
      - Klassen: alle unmittelbaren Nachbar-Zonen
      - Merkmale: lokale Dichte aller Merkmale als zusätzliches Kriterium
   → Compile.Difference:
      - benötigt genau zwei Bildnamen
   → Compile.Retain:
      - entscheidet ob bei der Reduktion von Bilder-Stapeln ob die Kanäle
        zusammengefasst und das Datum der Bilder bewahrt wird oder umgekehrt
      - in beiden Fällen wird ein multispektrales Bild zurückgegeben
   → Reduce.Bestof:
      - bei zwei Bildern nur dann Mittelwert wählen wenn beide >= 99% Abdeckung,
        sonst Bild mit größerer Abdeckung
   → Parse.Home:
      - Protokolle durch gemeinsame Angabe "Datum/Uhrzeit" verknüpft
   → Parse.Replace:
      - Variable können als arrays (kommagetrennt) eingegeben werden
      - jede Spalte wiederholt die Prozesskette
      - mehr als eine Variable kann wechseln
   → Tools.ErrorOut:
      - typisierte Fehlerabfrage
      - Abbruch wahlweise für einzelne Prozesse oder Befehle
   → xImalys:
      - neuer Start-Befehl zusammen mit "Parse.Replace"
   → xImalys -c:
      - Arbeitsverzeichnis leeren
      - wenn Teilergebnisse gesammelt werden sollen
   → Build.xFeatures:
      - verwendet Varianz, nicht Distanz der Zellen-Merkmale
      - alle Kontakte (auch innere) verwendet
   → Parse.NoQuality
      - schreibt eine Vorgabe für Kanäle ohne Qualitätsangabe
   → Archive.Import(Landsat|RapidEye|Aster)
      - importiert mit fest eingestellten Parametern
      - Archive.ImportDefault für Handbetrieb
      - Parameter "sensor=.." ergänzt
   → Drain.xBasins
      - syntheischer Abfluss aus Höhendaten
      - Micro-Catchments = Ebenen + lokale Minima als Basis
      - Abfluss als Vektor-Schema
      - Einzugsgebiete von jedem beliebigen Punkt
      - Stofftransport-Modul
   → Thema.Reduce.Execute:
      - neue Fuktionen:
      - Maximum: höchster Wert aus einer Zeitreihe für alle Pixel
      - Minumum: niedrigster ...
      - Range: maximale Distanz zwischen Maximum und Minimum
      - alle: Max/Min für alle Kanäle getrennt gebildet ← Brightness für einen Kanal
   → Raster.Calculate:
      - Kanäle arithmetisch kombinieren
      - auch Konstante möglich
      - beliebig lange Prozessketten
      - keine Klammern
   → Reduce.Vegettionsindex:
      - Kanal-Auswahl vereinfacht: "B3:B4" für Red=3 und Nir=4
   → Parse.xLoop:
      - Parameter können als Listen mit Kommas übergeben werden. Die Prozess-
        Kette wird für jeden Parameter wiederholt. Wenn sich mehr als ein
        Parameter ändert, müssen die Listen zusammenpassen.
   → Fehlerbehandlung:
      - drei Fehler-Objekte für Abbruch Programms, Kette, Befehl
   → Parse.Catalog:
      - Imalys erfasst für Landsat-Archive die vier Ecken der Bilddaten und
        speichert sie in einer Archiv-Liste zur Auswahl passender Szenen. Die
        Archiv-Liste ist als WKT Datei formatiert und kann im GIS angezeigt
        werden.
   → Parse.Compile:
      - intern drei verschiedene Proceduren "compile", "mosaic" und "stack"
      - kann Bilder in zahlreichen Formaten lesen
      - wenn Projektion und Pixelgröße angegeben sind, transformiert "Compile"
        alle Bilder entsprechend den Angaben.
      - Wenn die Bilder unerschiedliche Bereiche des übergebenen ROI abdecken,
        kombiniert "Compile" die Bilder entsprechend ihrer Lage und beschneidet
        das Ergebnis auf den ROI.
      - "Compile" überschreibt Teilbilder mit gleichem Aufnahmedatum zu einem
        Bild. Für Bilder mit unterschiedlichem Datum bildet "Compile" neue
        Ebenen.
      - alle Prozesse ignorieren NoData Pixel.
   → Parse.Mosaic:
      - Wird von "Compile" mit dem Attribut "flat" aufgerufen. "Mosaic"
        speichert alle Teilbilder in einer Ebene und überschreibt Bildbereiche
        die sich überlappen. Dabei ifgnoriert "Mosaic" NoData Bereiche.
   → Process-Chain:
      - ALLE Imalys Befehle akzeptieren NUR Daten aus dem Arbeits-Verzeichnis
        und speichern ihre Ergebnisse im Arbeits-Verzeichnis. "Compile" holt
        beliebige Bilddaten in das Arbeits-Verzeichnis, "Export" schreibt sie
        an einen anderen Ort.
      - Compile harmonisiert die Bilder so dass Imalys-Befehle nur noch wenige
        Prüfungen durchführen müssen. Innerhalb des Arbets-Verzeichnis sind die
        Daten frei kombinierbar.
   → Gdal.Warp:
      - Name der Ziel-Datei kann direkt gewählt werden
      - Pixel werden auf ein ganzzahliges Vielfaches der Pixelgröße verschoeben
   → Gdal.Import:
      - der Name der Ziel-Datei kann direkt gewählt werden
   → Cover.ClipToShape:
      - Ein Bild kann auch nachträglich auf die Form einer Maske reduziert
        werden. Maskierte Bereiche werden auf NoData gesetzt
   → Parse.Import:
      - "Import" kann Bildfehler aus einer Maske als NoData auf das Ergebnis
        übertragen. "Import" trägt den Anteil klarar Pixel in den IDL-Header
        ein.
      - "Import" benötigt die erweiterte Archiv-Liste mit Polygonen.
   → Parse.Flatten:
      - "Reduce" kann einfache arithmetische Ausdrücke oder Konstante auf alle
        Bildpixel anwenden. Die Formel wird als Text übergeben.
      - "<" und ">" erzeugen Masken [0,1].
      - "Maximum" und "Minimum" bestimmen den größten oder kleinsten Wert einer
        Auswahl von Kanälen.
      - "Range" bestimmt den Wertebereich (Maximum-Minimum) in einem beliebigen
        Blder-Stapel
   → Parse.RunOff:
      - das hydrologische Abfluss-Modell aus Höhendaten wurde reaktiviert.
   → Flatten.Quality:
      - Bild mit der Anzahl klarer Bildebenen pro Pixel
   → Parse.Compile.Flat:
      - Große Mosaike 10 Mio Pixel aus optimierten Teilen zusammensetzen.
        Überlappende Teile werden überschrieben
   → Parse.Compile:
      - Erweiterung für Import von nur einem Bild ohne Veränderungen
   → Table.Focus:
      - Bildmerkmale auf ein regelmäßiges Gitter abbilden
      - NoData Pixel werden ignoriert
      - Merkmale werden gemittelt
      - Min/Max/Var vorgesehen
   → Table.ImageStats:
      - Minimum, Maximum, 1%-Percentile, Mittelwert, Median und Histogramm aus
        Bilddaten
   → Model.RolfFocus:
      - klassifiziert Spektralkombinationen entsprechend der Distanz im
        Merkmalsraum. Die Anzahl der Klassen ist wählbar. Die Klassen werden
        mit Stichproben laufend optimiert. Der Algorithmus ist schnell! }

{ PROJEKTE:
  → nur Landsat-Archive können automatisch importiert werden
  → Stichproben für Klassifikation über zahlreiche Bilder sammeln
  → Indikator "Farbig ←→ Farblos" aus Varianz der Kanäle
  → stabile Perioden in Zeitreihe suchen ← "Parse.Breaks"
  → Konverter: Protokoll → Command-Chain
  → Polygone glätten
     - Pixel-Grenzen erfassen ← immer genau zwei Zonen (incl. Null)
     - Indices der Pixel-Ecken als Koordinaten verwenden
     - jede Zonen-Kombination erfassen und sortieren
     - binäre Suche nach Ketten ← mehr als eine möglich
     - Linien mit echten Koordinaten bilden
     - Glätten mit Diffusion ← Knoten bleiben fixiert }

{ STATISTIK:
   → ca. 15.000 Zeilen
   → ca. 350 Routinen }

{$mode objfpc}{$H+}

interface

uses
  Classes, Math, Process, SysUtils, StrUtils;

const
  //Commands = Prozess-Namen
  cfAcy = 'accuracy'; //Klassen-Kontroll-Bild
  cfAlp = 'alpha'; //Bild mit Alpha-Kanal am Ende
  cfAtr = 'index.bit'; //Zellindex-Attribute
  cfBit = '.bit'; //Binary Table Extension
  //cfBnd = 'boundaries'; //Grenzen zwischen Zonen
  cfBrt = 'brightness'; //erste Hauptkomponente als gemeinsame Dichte
  cfBst = 'bestof'; //Median – Mean – Defined
  cfCbn = 'combination.tab'; //Häufigkeit Cluster in Referenzen ← ALS LINK SPEICHERN
  cfClc = 'calculate'; //Ergebnis einer Kanal-Arithmetik
  cfCmd = 'commands.log'; //aktuelle Befehlskette
  cfCpl = 'compile'; //Ergebnis Bild-Kombination
  cfCtm = 'catchments'; //Einzugsgebiete
  cfCtx = 'context.bit'; //erweiterte Attribut-Tabelle
  cfDdr = 'dendrites'; //dendritische Zellform
  cfDst = 'distance'; //kleinste Distanz zu Maske
  cfDvn = 'deviation'; //Gauß'sche Abweichung
  cfDvs = 'diversity'; //Rao's Q-Index für Zellen
  cfErr = 'error.log'; //Fehler + Warnungen
  cfEtp = 'entropy'; //Rao's Q-Index für Pixel-Klassen
  cfEql = 'equal.bit'; //Attribute mit normalisierten Werten
  cfExt = '.aux.xml'; //Extension für qGis Erweiterungen
  //cfFbr = 'fabric'; //Muster-Klassen
  cfFcs = 'focus.csv'; //Vektoren nach Bearbeitung
  cfGml = '.gml'; //Geographic Markup Language
  cfHdr = '.hdr'; //Raw-Binary mit ENVI-Header
  cfHry = 'history'; //Zeitreihe aus Hauptkomponenten
  cfHse = 'hillshade'; //Beleuchtung
  cfHsv = 'HSV'; //HSV Bildcodierung für RGB
  cfIdm = 'inverse'; //Inverse Difference Moment
  cfIdx = 'index'; //Imalys Zellindex
  cfImp = 'import'; //GDAL Import im ENVI-Format
  cfItf = 'interflow'; //Durchlauf, Austausch eines Merkmals
  cfKey = 'keys.bit'; //Modell aus Keys (Verknüpfungen)
  cfLai = 'leafarea'; //Leaf Area Index Näherung
  cfLmt = 'limits'; //Schwellwert-Ergebnis als Klassen
  //cfLnk = 'links.bit'; //Verknüpfung der Mikro-Catchments
  cfLow = 'lowpass'; //Kontrast-Reduktion (Emulation)
  cfLpc = 'laplace'; //Laplace-Kernel
  cfMap = 'mapping'; //Klassen-Layer aus Modell
  cfMax = 'maximum'; //größter Wert in einem Stapel
  cfMdl = 'model.bit'; //Feature-Modell
  cfMdn = 'median'; //für Rangfolge-Mittelwert
  cfMea = 'mean'; //Mittelwert
  cfMic = 'micro'; //Catchments aus lokalen Minima
  cfMin = 'minimum'; //kleinster Wert in einem Stapel
  cfMrg = 'merge'; //multispektrale Kacheln verknüpfen
  cfMsk = 'mask'; //Maske = binär ja/nein
  cfNrm = 'normal'; //normalisierte Textur
  cfNiv = 'NIRV'; //Near Infrared Vegetation Index
  cfNvi = 'NDVI'; //normalisierter Vegetationsindex
  cfOut = 'output.log'; //Imalys Meldungen
  cfOvl = 'overlay'; //überlagerung von Kanälen oder Bildern
  cfPca = 'principal'; //Hauptkomponenten-Transformation
  cfPrp = 'proportion'; //Flächen-Verhältnis Zelle zu Nachbarn
  cfQuy = 'quality'; //Stack-Quality = Anzahl gestörte Layer
  cfRds = 987654321; //Random Seed initialisierung
  cfRfz = 'reference'; //Raster-Bild mit Referenz-Werten (thematisch)
  cfRgs = 'regression'; //Regression über Kanäle
  cfRlt = 'relation'; //Zell-Umfang durch Kontakte
  cfRnf = 'runoff'; //Abfluss als Tabelle
  cfRng = 'range'; //Wertebereich
  cfRog = 'roughness'; //spektrale Rauhigkeit
  cfRst = 'raster'; //Endprodukt des Bildimports
  cfSlc = 'selection.txt'; //Namen ausgewählter Bilder
  cfSmp = 'samples'; //Stichproben, Varianten mit gleichen Merkmalen
  cfSpc = 'specificity.tab'; //tab-getrennte Tabelle
  cfStk = 'stack'; //Zwischenergebnis bei Layer-Manipulation
  cfSze = 'cellsize'; //Fläche der Zellen in Pixeln
  cfTab = 'table.csv'; //Tablle im Textformat (tab-getrennt)
  cfThm = 'thema'; //überwachte Klassifikation
  cfTpl = 'topology.bit'; //Zell-Topologie
  cfTxr = 'texture'; //Bild nach Textur-Filterung
  cfVal = 'values'; //Bild aus Zell-Attributen
  cfVct = 'vector.csv'; //Vektor-Import als CSV
  cfVrc = 'variance'; //Bild mit Gauß-Abweichung einzelner Pixel
  cfWrp = 'warp'; //Ergebnis einer Pixel-Transformation
  cfWgt = 'weight'; //globale Summe <> Hauptkomponente
  //cfZne = 'zones': //für zonale Atribute

  cfByt:array[0..15] of byte = (0, 1,2,4,4,8, 8,0,0,0,0, 0,2,4,8,8); //Byte pro ENVI-Typ

  eeExc:string = '/home/c7sepe2/Pascal/Modules/x_Imalys';
  eeGdl:string = '/usr/bin/'; //gdal-Befehle
  eeHme:string = '/home/c7sepe2/.imalys/'; //Arbeitsverzeichnis Vorgabe
{ TODO: "eeExc" und "eeHme" müssen vom Anwender abhängig werden }
  eeLog:string = '/home/c7sepe2/.imalys/'; //Ergebnisse + Protokolle ← Vorgabe MUSS geändert werden
  eeNow:string = ''; //Datum:Uhrzeit der Prozessierung

  ccAls = '$'+DirectorySeparator; //Alias für Arbeitsverzeichnis
  ccPrt = #10'___________________________________________________________'#10;

  ccPrj = 'GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,'+
    '298.257223563,AUTHORITY["EPSG","7030"]],AUTHORITY["EPSG","6326"]],'+
    'PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",'+
    '0.0174532925199433,AUTHORITY["EPSG","9122"]],AXIS["Latitude",NORTH],'+
    'AXIS["Longitude",EAST],AUTHORITY["EPSG","4326"]]'; //EPSG=4326

type
   tLoopError = class(Exception); //Programm abbrechen
   tChainError = class(Exception); //Prozesskette abbrechen
   tStepError = class(Exception); //Teilschritt (Schleife) abbrechen

  //Arrays
  tnByt = array of byte; //unspezifisches Array
  tn2Byt = array of array of byte; //unspezifisches Tabellenformat
  tn3Byt = array of array of array of byte; //unspezifisches Bildformat
  tnCrd = array of cardinal; //LongWord-Array (cardinal)
  tnDbl = array of double; //double precision Array
  tn2Dbl = array of array of double; //double precision Matrix
  tnInt = array of integer; //Standard-Array
  tn2Int = array of array of integer; //Standard-Zellindex
  tn3Int = array of array of array of integer; //Integer-Bild
  tnLrg = array of int64; //64-Bit Integer-Array
  tnPnt = array of tPoint; //Punkte-Liste
  tnPtr = array of pointer; //Zeiger-Liste
  tnSgl = array of single; //einfaches Array
  tn2Sgl = array of array of single; //Standard-Bildkanal (2D-Single)
  tn3Sgl = array of array of array of single; //Standard-Bildformat (Single-Qube)
  tnWrd = array of word; //Word-Array
  tn2Wrd = array of array of word; //Word-Matrix
  tnStr = array of string; //simple String-Liste
  tn2Str = array of array of string; //String-Tabelle

  tqInt = array[byte] of integer;
  tqSgl = array[byte] of single;

  //Records
  trBox = record //Rechteck in Pixel-Koordinaten, Ursprung links oben
    Lft:integer;
    Top:integer;
    Rgt:integer;
    Btm:integer;
  end;

  trCvr = record //Raster: Größe, Abdeckung und Ursprung
    Crs:string; //Koordinatensystem z.B. "WGS 84 / UTM zone 32N"
    Pro:string; //PROJ.4-Code
    Epg:integer; //EPSG-Code
    Wdt:integer; //Bildbreite in Pixeln
    Hgt:integer; //Bildhöhe in Pixeln
    Stk:integer; //Kanäle im Bild
    Lft:double; //Koordinaten der Bildecken im aktuellen CRS ..
    Top:double; //.. Bezeichner beziehen sich auf Ursprung links oben
    Rgt:double;
    Btm:double;
    Pix:double; //Pixelgröße
  end; //vgl: trFrm
{ ==> _crCvr aktualisieren }

  trEtp = record //Zonen: Homogenität + Verknüpfung (entropie)
    Min:single; //Minimum Varianz
    Sze:integer; //Größe in Pixeln
    Lnk:integer; //Verknüpfze Fläche
    aBnd:tnSgl; //Summen + Quadrate
  end;
  tpEtp = ^trEtp; //Zeiger auf Record
  tapEtp = array of tpEtp; //Zeiger-Liste

  trFrm = record //Vector: Abdeckung und Größe
    Crs:string; //Koordinatensystem z.B. "WGS 84 / UTM zone 32N"
    Epg:integer; //EPSG-Code
    Lft:double; //Koordinaten der Bounding Box im aktuellen CRS
    Top:double;
    Rgt:double;
    Btm:double;
  end; //vgl: trCvr
  tarFrm = array of trFrm;

  trGeo = record
    Lat:double; //geogr. Breite
    Lon:double; //geogr. Länge
    Epg:integer; //EPSG-Nr
  end;
  tarGeo = array of trGeo; //Linien, Polygone

  trRgb = record
    Red:single; //Farb-Dichten [0..1]
    Grn:single;
    Blu:single;
  end;

  trHdr = record //ENVI-kompatible Header-Information
    Cnt:integer; //höchste Klassen-ID oder Anzahl Zellen
    Fmt:integer; //ENVI-Datenformat
    Lat,Lon:double; //Koordinaten links oben: Y=Breite, X=Länge
    Pix:double; //Pixelgröße in Meter
    Prd:integer; //Kanäle pro Bild im Stack
    Scn,Lin:integer; //Spalten, Zeilen auf Pixelebene
    Stk:integer; //Kanäle in Datei
    Cys:string; //"coordinate system string" aus gdal-Bibliothek
    Fld:string; //Feldnamen für Attribute (kommagetrennt)
    Map:string; //" map info" aus ENVI-Header
    Pif:string; //"projection info" aus ENVI-Header
    Pal:tnCrd; //Palette für alle Klassen
    aBnd:string; //Kanal- oder Klassen-Namen, getrennt durch Zeilenwechsel!
  end;
//==> HEADER.COPY ANPASSEN!

  trLnk = record
    Hig:integer; //Index Quelle
    Low:integer; //Index Senke
    Imp:integer; //Aktivität
  end;
  tprLnk = ^trLnk; //Zeiger
  traLnk = array of trLnk;

  trRfz = record //Klassen nach Häufigkeit sortieren
    Cnt:integer; //Anzahl Treffer
    Map:integer; //Klassen-ID
  end;
  tprRfz = ^trRfz; //für Listen

  trSpc = record //Verteilung von Clustern auf Referenzen
    Map:integer; //Cluster/Klassen-ID
    Rfz:integer; //Referenz-ID
    Val:single; //Wert(Feature) oder Anteil(Fläche)
  end;
  tprSpc = ^trSpc; //für Listen

  trTms = record //Bilder nach Datum (im Dateinamen) sortieren
    Nme:string; //vollständiger Dateiname
    Dat:integer; //Datum [YYYYMMDD]
  end;
  tprTms = ^trTms;

  tTreeRes = function(const rSrc:TSearchRec; sDir:string):string;

  tTools = class(tObject)
    prSys: TProcess;
    function ArcTanQ(fHrz,fVrt:double):double;
    function BandMove(fxSrc:tn2Sgl):tn2Sgl;
    function BitExtract(iPst:integer; sNme:string):tnSgl;
    function BitFormat(var hBit:integer; sNme:string):tnLrg;
    procedure BitInsert(faVal:tnSgl; iPst:integer; sNme:string);
    function BitRead(sNme:string):tn2Sgl;
    function BitSize(sNme:string):integer;
    procedure BitWrite(fxVal:tn2Sgl; sNme:string);
    function CheckOpen(sNme:string; iMds:integer):integer;
    function _CheckTarget(sDir:string):string;
    function CommaToLine(sDlm:string):string;
    procedure ConsoleOut(sHnt:string);
    procedure _CopyDir_(sSrc,sDst:string);
    function CopyEnvi(sOld,sNew:string):string;
    procedure _CopyFile_(sDat,sCpy:string);
    procedure CopyFile(sDat,sCpy:string);
    function CopyGrid(ixTmp:tn2Int):tn2Int;
    function CopyHeader(var rHdr:trHdr):trHdr;
    procedure CopyIndex(sTrg:string); //Quell- und Ziel-Datei
    procedure CopyShape(sSrc,sTrg:string);
    procedure CsvDelete(sNme:string);
    procedure DoubleToText(fxVal:tn2Dbl; sNme:string);
    procedure EnviDelete(sNme:string);
    procedure EnviRename(sOld,sNew:string);
    procedure EnviTrash(sNme:string);
    function ErrorLog(sPrc:string):boolean;
    procedure ErrorOut(iLvl:integer; sErr:string);
    function FileFilter(sMsk:string):TStringList;
    function _FileTree(sDir:string; yRes:TTreeRes):TStringList;
    function _FirstBand(fxImg:tn3Sgl):tn2Sgl;
    function GetBool(sPrm:string):boolean;
    function GetChar(sPrm:string):char;
    function _GetError_:string;
    function GetFloat(sPrm:string):single;
    function GetInt(sPrm:string):integer;
    function GetOutput(oProcess:tProcess):string;
    procedure HintOut(bTms:boolean; sHnt:string);
    function _HsvToRgb(fHue,fSat,fVal:single):trRgb;
    function InitByte(iCnt:integer):tnByt;
    function Init2Byte(iRow,iCol:integer):tn2Byt;
    function Init3Byte(iBnd,iRow,iCol:integer):tn3Byt;
    function InitCardinal(iSze:integer):tnCrd;
    function InitDouble(iSze:integer):tnDbl;
    function InitIndex(iSze:integer):tnInt;
    function InitInteger(iSze:integer; iDfl:dWord):tnInt;
    function Init2Integer(iRow,iCol:integer; iVal:dWord):tn2Int;
    function InitLarge(iSze:integer):tnLrg;
    function InitSingle(iSze:integer; iDfl:dWord):tnSgl;
    function Init2Double(iRow,iCol:integer):tn2Dbl;
    function Init2Single(iRow,iCol:integer; iDfl:dWord):tn2Sgl;
    function Init3Single(iStk,iRow,iCol:integer; iVal:dWord):tn3Sgl;
    function InitWord(iSze:integer):tnWrd;
    function Init2Word(iRow,iCol:integer):tn2Wrd;
    function LineRead(sNme:string):string;
    function LoadClean(sTxt:string):tStringList;
    function MaskExtend(sMsk:string):string;
    function MaxThema(ixBnd:tn2Byt):byte;
    function MinBand(fxBnd:tn2Sgl):single;
    function _MoveThema(ixThm:tn2Byt):tn2Byt;
    function NewFile(iCnt,iVal:dWord; sNme:string):integer;
    function OsCommand(sCmd,sPrm:string):string;
    procedure OsExecute(sExc:string; SlPrm:TStringList);
    function _ReadList_(sLst:string):tnStr;
    function ShortName(iLft,iMid,iRgt:integer; sArc:string):string;
    function SetDirectory(sDir:string):string;
    procedure ShapeDelete(sNme:string);
    procedure ShapeRename(sSrc,sTrg:string);
    procedure TextAppend(sNme,sTxt:string);
    procedure TextOut(sNme,sTxt:string);
    function TextRead(sNme:string):string;
    function _eTextRead(sNme:string):string;
    function ValueList(sLst:string):tnSgl;
  end;

const
  crBox: trBox = (Lft:MaxInt; Top:0-MaxInt; Rgt:0-MaxInt; Btm:MaxInt);
  crCvr: trCvr = (Crs:''; Pro:''; Epg:0; Wdt:0; Hgt:0; Stk:1; Lft:MaxInt;
         Top:0-MaxInt; Rgt:0-MaxInt; Btm:MaxInt; Pix:0); //Koordinaten für min/max-Prozesse
  crEtp: trEtp = (Min:0; Sze:0; Lnk:0; aBnd:nil);
  crFrm: trFrm = (Crs:''; Epg:0; Lft:MaxInt; Top:0-MaxInt; Rgt:0-MaxInt;
         Btm:MaxInt); //unmöglich, für min/max-Prozesse
  crGeo: trGeo = (Lat:0; Lon:0; Epg:0);
  crMax: trFrm = (Crs:''; Epg:0; Lft:0-MaxInt; Top:MaxInt; Rgt:MaxInt;
         Btm:0-MaxInt); //maximaler Rahmen
  crHdr: trHdr = (Cnt:0; Fmt:0; Lat:1; Lon:1; Pix:1; Prd:0; Scn:0; Lin:0;
         Stk:0; Cys:''; Fld:''; Map:''; Pif:''; Pal:nil; aBnd:'');
  //crGry: trRgb = (Red:0.5; Grn:0.5; Blu:0.5);
  crRgb: trRgb = (Red:0.0; Grn:0.0; Blu:0.0);

var
  Tools: tTools;
  LoopError: tLoopError; //Programm abbrechen
  ChainError: tChainError; //Prozesskette abbrechen
  StepError: tStepError; //Teilschritt abbrechen

implementation

{ tRL verteilt Worte in einer kommagetrennten Liste auf ein String-Array }

function tTools._ReadList_(sLst:string):tnStr; //Eingabe(Komma-getrent): String-Array
var
  I:integer;
begin
  SetLength(Result,WordCount(sLst,[','])); //Dimension aus Kommas
  for I:=0 to high(Result) do
    Result[I]:=trim(ExtractWord(succ(I),sLst,[','])); //Worte auf Array verteilen
end;

{ rEL liest die Error-Pipe von "prSys" und speichert sie als "imalys-error" im
  Imalys-Verzeichnis des Anwenders. rEL muss unmittelbar nach "prSys.Execute"
  aufgerufen werden. }
{ ==> WarningLog }

function tTools._GetError_:string;
var
  iSze:integer; //Zeichen im Error-Stream
begin
  iSze:=prSys.StdErr.NumBytesAvailable; //Anzahl Zeichen im Error-Stream
  if iSze>0 then
  begin
    SetLength(Result,iSze); //Platz schaffen
    prSys.StdErr.Read(Result[1],iSze); //Stream als String
  end
  else Result:='';
end;

procedure tTools._CopyFile_(sDat,sCpy:string);
{ tTFC kopiert eine Datei ohne Shell mit einem neuen Namen. tTFC erzeugt bei
  Bedarf ein neues Verzeichnis und überprüft die Schreibrechte.
  ==> BILDDATEN ÜBER 4GB MÜSSEN MIT IMAGE.BAND_COPY_L KOPIERT WERDEN }
const
  cCrt = 'fTFC: Unable to create file: ';
var
  iWrt: int64=0; //Bytes geschrieben
  mDat,mCpy: TFileStream; {Handles zum Kopieren}
begin
  try
    DeleteFile(sCpy); {Sicherheit}
    mDat:=TFileStream.Create(sDat,fmOpenRead);
    mCpy:=TFileStream.Create(sCpy,fmCreate);
    iWrt:=mCpy.CopyFrom(mDat,mDat.Size);
    if iWrt<mDat.Size then Tools.ErrorOut(2,cCrt+sCpy);
  finally
    FreeAndNil(mDat);
    FreeAndNil(mCpy);
  end; {of try ..}
end;

procedure tTools._CopyDir_(sSrc,sDst:string); //Source, Destiny
{ tFC kapselt den OS-Befehl "cp". tFC kopiert genau eine Datei ohne weitere
  Parameter. Die Dateinamen dürfen keine Platzhalter verwenden. }
const
  cSrc = 'tCD: Directory not found: ';
begin
  if not DirectoryExists(sSrc) then Tools.ErrorOut(2,cSrc+sSrc);
  if sSrc[length(sSrc)]=DirectorySeparator then delete(sSrc,length(sSrc),1);
  if sDst[length(sDst)]=DirectorySeparator then delete(sDst,length(sDst),1);
  prSys.Options:=[poWaitOnExit,poUsePipes]; //modal ausführen

  {prSys.Executable:='mkdir';
  prSys.Parameters.Clear; //Sicherheit
  prSys.Parameters.Add(sDst);
  prSys.Execute; //Verzeichnis anlegen}

  prSys.Executable:='cp';
  prSys.Parameters.Clear; //Sicherheit
  prSys.Parameters.Add(sSrc+'/*');
  prSys.Parameters.Add(sDst);
  prSys.Execute; //Inhalte kopieren
end;

function tTools.CheckOpen(
  sNme: string; //Dateiname
  iMds: integer): //Zugriffsmodus
  integer; //Datei-Handle ODER (-1) für Fehler
{ CO gibt ein Handle auf die Datei "sNme" zurück, wenn die Datei existiert und
  für den Zugriffs-Modus "iMds" freigegeben ist. Andernfalls gibt CO (-1) zurück
  und erzeugt eine Fehlermeldung. }
const
  cOpn = 'fTCO: Unable to open file or folder: ';
begin
  Result:=-1; //ungültig
  Result:=FileOpen(sNme,iMds); //Datei öffnen
  if Result<1 then
    Tools.ErrorOut(2,cOpn+sNme);
end;

{ BF öffnet oder erzeugt eine Bit-Tabelle und übergibt eine Liste mit Offsets
  für alle Datenblöcke und ein gültiges File-Handle. Jeder Datenblock hat genau
  einen Eintrag in der Liste. Die Datenblöcke können unterschiedlich lang sein.
  Die Liste steht hinter den Datenblöcken. Das letzte Eintrag in der Liste
  zeigt auf das Offset der Liste am Ende der Datei. Die Länge der Liste codiert
  die Anzahl der Daten-Blöcke. }

function tTools.BitFormat(
  var hBit: integer; //File-Handle
  sNme: string): //Dateiname
  tnLrg; //Cumulierte Einsprung-Adressen
const
  cBit = 'fTBF: Unable to open or create binary table: ';
  cSze = 'fTBF: Binary table corruped: ';
var
  iOfs: int64; //Offset Metadaten
  iSze: int64; //Dateigröße in Byte
begin
  Result:=nil; //Vorgabe = leer (neue Datei)

  sNme:=ChangeFileExt(sNme,cfBit); //Sicherheit
  if FileExists(sNme)
    then hBit:=CheckOpen(sNme,fmOpenReadWrite) //Tabelle öffnen
    else hBit:=NewFile(0,0,sNme); //neue Tabelle
  if hBit<0 then Tools.ErrorOut(2,cBit+sNme);

  iSze:=FileSeek(hBit,0,fsFromEnd); //Dateigröße
  if iSze>0 then //Daten vorhanden?
  begin
    FileSeek(hBit,0,fsFromBeginning); //von Vorne
    FileRead(hBit,iOfs,SizeOf(int64)); //Offset Metadaten
    if iOfs>=iSze then Tools.ErrorOut(2,cSze+sNme);
    SetLength(Result,(iSze-iOfs) div SizeOf(int64)); //ausreichend Platz
    FileSeek(hBit,iOfs,fsFromBeginning); //Beginn Metadaten
    FileRead(hBit,Result[0],iSze-iOfs); //vollständig lesen
  end;
end;

{ tTIL erzeigt ein Large-Array und füllt es mit Nullen}

function tTools.InitLarge(iSze:integer):tnLrg;
begin
  SetLength(Result,iSze);
  FillDWord(Result[0],iSze*2,0);
end;

{ BR liest Tabelle "sNme" im BIT-Format und übergibt sie als heterogenes Array.
  BR liest den Offset der Offset-Tabelle "iOfs" und bestimmt die Dateigröße
  "iSze". Die Differenz ergibt die Länge der Offset-Tabelle = Anzahl der Daten-
  Blöcke. BR liest die Offset-Tabelle, alloziert passenden Speicher in "Result"
  und liest die Datenblöcke. }

function tTools.BitRead(sNme:string):tn2Sgl;
var
  hBit: integer=-1; //File-Handle
  iaOfs: tnLrg=nil; //Offset-Liste
  iOfs: int64; //Offset für Offset-Liste
  iSze: int64; //Dateigröße
  I: integer;
begin
  Result:=nil;
  try
    hBit:=CheckOpen(ChangeFileExt(sNme,cfBit),fmOpenRead); //Tabelle öffnen
    FileRead(hBit,iOfs,SizeOf(int64)); //Offset der Offset-Liste
    iSze:=FileSeek(hBit,0,fsFromEnd); //Dateigröße
    iaOfs:=InitLarge((iSze-iOfs) div SizeOf(int64));

    SetLength(Result,high(iaOfs),0); //Anker für Datenblöcke
    FillDWord(Result[0],length(Result),0);
    FileSeek(hBit,iOfs,fsFromBeginning); //Beginn Liste
    FileRead(hBit,iaOfs[0],iSze-iOfs); //Einträge
    FileSeek(hBit,iaOfs[0],fsFromBeginning); //Beginn Datenblöcke
    for I:=0 to pred(high(iaOfs)) do
    begin
      SetLength(Result[I],(iaOfs[succ(I)]-iaOfs[I]) div SizeOf(single));
      FileRead(hBit,Result[I,0],iaOfs[succ(I)]-iaOfs[I]);
    end;
  finally
    if hBit>=0 then FileClose(hBit)
  end;
end;

{ BW speichert die Tabelle "fxVal" als BIT-Tabelle "sNme". Jeder Datenblock in
  "ixVal" darf eine andere Dimension haben. BW erzeugt zunächst eine Liste mit
  Offsets "iaOfs" für alle Datenblöcke in "fxVal". Der letzte Eintrag ist das
  Offset für die Liste selbst. BW speichert dieses Offset am Beginn der Datei,
  dann alle Datenblöcke und zuletzt die Offset-Liste. }

procedure tTools.BitWrite(
  fxVal: tn2Sgl; //Tabelle
  sNme: string); //Name der BIT-Tabelle
const
  cVal = 'fTBW: no table given!';
var
  hBit: integer=-1; //File-Handle
  iaOfs: tnLrg=nil; //Offset-Liste
  iOfs: int64; //Offset für Offset-Liste
  I: integer;
begin
  if fxVal=nil then Tools.ErrorOut(2,cVal);
  try
    SetLength(iaOfs,succ(length(fxVal)));
    iOfs:=SizeOf(int64); //Offset erster Datenblock
    for I:=0 to high(fxVal) do
    begin
      iaOfs[I]:=iOfs; //Offsets eintragen
      iOfs+=length(fxVal[I])*SizeOf(Single); //Offsets cumulieren
    end;
    iaOfs[high(iaOfs)]:=iOfs; //letzer Offset für Liste
    hBit:=NewFile(0,0,ChangeFileExt(sNme,cfBit)); //neue Tabelle
    FileWrite(hBit,iaOfs[high(iaOfs)],SizeOf(int64)); //Einsprung-Adresse Metadaten
    for I:=0 to high(fxVal) do
      FileWrite(hBit,fxVal[I,0],length(fxVal[I])*SizeOf(single));
    FileWrite(hBit,iaOfs[0],length(iaOfs)*SizeOf(int64));
  finally
    if hBit>=0 then FileClose(hBit)
  end;
end;

{ BE liest den Datenblock "iPst" aus der Bit-Datei "sNme" und gibt ihn als
  Float-Array zurück. "iPst" muss gültig sein. }

function tTools.BitExtract(
  iPst: integer; //Datenblock-Index (ab Null)
  sNme: string): //Name der BIT-Tabelle
  tnSgl; //Datenblock
const
  cFex = 'tBE: Database not found: ';
  cPst = 'tBE: Data block index (';
  cHnt = ') not defined for ';
var
  hBit: integer=-1; //File-Handle
  iaOfs: tnLrg=nil; //Datenblock-Offsets
begin
  sNme:=ChangeFileExt(sNme,cfBit);
  if not FileExists(sNme) then Tools.ErrorOut(2,cFex+sNme);
  SetLength(Result,0); //Vorgabe = nil
  try
    iaOfs:=BitFormat(hBit,sNme); //File-Handle & Datenblock-Offsets
    if (iPst<0) or (iPst>=high(iaOfs)) then
      Tools.ErrorOut(2,cPst+IntToStr(iPst)+cHnt+sNme);
    FileSeek(hBit,iaOfs[iPst],fsFromBeginning); //Beginn Datenblock
    SetLength(Result,(iaOfs[succ(iPst)]-iaOfs[iPst]) div 4); //Kapazität
    FileRead(hBit,Result[0],iaOfs[succ(iPst)]-iaOfs[iPst]); //Block lesen
  finally
    if hBit>=0 then FileClose(hBit)
  end;
end;

function tTools.GetFloat(sPrm:string):single;
const
  cFlt='Numeric (float) expression required: ';
begin
  if not TryStrToFloat(sPrm,Result) then
    Tools.ErrorOut(2,cFlt+sPrm);
end;

function tTools.GetInt(sPrm:string):integer;
const
  cInt='Integer expression required: ';
begin
  if not TryStrToInt(sPrm,Result)
    then Tools.ErrorOut(2,cInt+sPrm);
end;

function tTools.GetBool(sPrm:string):boolean;
const
  cBoo = 'Boolean expression required! Instead given: ';
begin
  if not TryStrToBool(sPrm,Result) then
    Tools.ErrorOut(2,cBoo+sPrm);
end;

function tTools.GetChar(sPrm:string):char;
begin
  Result:=sPrm[1]; //ertser Buchstabe
end;

procedure tTools.EnviRename(sOld,sNew:string);
const
  cFex = 'tER: Image not found: ';
  cNew = 'tER: System operation failed: rename to ';
begin
  if sNew=sOld then exit; //keine Aufgabe
  if not FileExists(sOld) then Tools.ErrorOut(2,cFex+sOld);
  if FileExists(sNew) then EnviDelete(sNew); //Vorgänger löschen wenn vorhanden
  RenameFile(ChangeFileExt(sOld,''),ChangeFileExt(sNew,'')); //Datei umbenennen
  RenameFile(ChangeFileExt(sOld,cfHdr),ChangeFileExt(sNew,cfHdr)); //Header umbenennen
  RenameFile(ChangeFileExt(sOld,cfExt),ChangeFileExt(sNew,cfExt)); //Erweiterung umbenennen
  if not FileExists(sNew) then Tools.ErrorOut(2,cNew+sNew);
end;

procedure tTools.EnviDelete(sNme:string);
begin
  if sNme='' then exit;
  DeleteFile(ChangeFileExt(sNme,''));
  DeleteFile(ChangeFileExt(sNme,cfHdr));
  DeleteFile(ChangeFileExt(sNme,cfExt));
end;

{ tFF übergibt eine Liste mit allen Dateien die zur Maske "sMsk" passen. ENVI-
  Dateien ohne Extension MÜSSEN über die Extension ".hdr" gesucht werden. tFF
  sucht NICHT rekursiv.
  ==> Verzeichnisse sollten mit Attributen ohne $40 ausgeschlossen werden.
  ==> DIE REIHENFOLGE IST NICHT SORTIERT }

function tTools.FileFilter(
  sMsk: string): //Maske für Pfad- und Dateiname mit Platzhaltern
  TStringList; //Bildnamen mit Verzeichnis ODER nil
const
  cAtr = 0; //Datei-Attribut, 0=normale Dateien
  cLst = 'tFF: No match found for ';
  cOpn = 'tFF: Unable to open file or folder: ';
var
  rDir: TSearchRec; //Datei-Attribute
begin
  Result:=nil;
  if not DirectoryExists(ExtractFilePath(sMsk)) then
    Tools.ErrorOut(2,cOpn+sMsk);
  try
    Result:=TStringList.Create;
    if FindFirst(sMsk,cAtr,rDir)<>0 then exit; //Parameter übergeben + suchen
    repeat
      Result.Add(ExtractFilePath(sMsk)+rDir.Name) //vollständiger Name
    until FindNext(rDir)<>0;
  finally
    FindClose(rDir);
  end; //try ..
end;

{ tTEC kopiert ENVI Bilddaten mit einem neuen Namen }

function tTools.CopyEnvi(sOld,sNew:string):string;
begin
  Result:=sNew; //Vorgabe
  if sOld=sNew then exit;
  CopyFile(ChangeFileExt(sOld,''),ChangeFileExt(sNew,'')); //Datei
  CopyFile(ChangeFileExt(sOld,cfHdr),ChangeFileExt(sNew,cfHdr)); //Header
  //FileCopy(ChangeFileExt(sOld,cfExt),ChangeFileExt(sNew,cfExt)); //Erweiterung
end;

procedure tTools.ConsoleOut(sHnt:string);
begin
  writeln('Imalys [',TimeToStr(Time),'] '+sHnt);
end;

function tTools.Init2Integer(iRow,iCol:integer; iVal:dWord):tn2Int; //neue Dimensionen
{ tTII erzeugt ein Integer-Array mit zwei Dimensionen und füllt es mit Null. }
const
  cPos = 'tII: Provided dimension must be positive!';
var
  I: integer;
begin
  if (iRow<1) or (iCol<1) then Tools.ErrorOut(2,cPos);
  SetLength(Result,iRow,iCol);
  for I:=0 to pred(iRow) do
    FillDWord(Result[I,0],iCol,iVal);
end;

function tTools.InitInteger(iSze:integer; iDfl:dWord):tnInt;
{ tTII erzeugt ein Integer-Array und füllt es mit Nullen. }
begin
  SetLength(Result,iSze); //neue Dimension
  FillDWord(Result[0],iSze,iDfl) //leeren
end;

function tTools.InitCardinal(iSze:integer):tnCrd;
{ tTIC erzeugt ein LongWord Array und füllt es mit Nullen. }
begin
  SetLength(Result,iSze); //neue Dimension
  FillDWord(Result[0],iSze,0) //leeren
end;

{ tTIS erzeugt ein Single-Array und füllt es mit "iDfl". Single-Werte müssen
  als "dWord" deklariert sein. }

function tTools.InitSingle(iSze:integer; iDfl:dWord):tnSgl; //neue Dimension, Vorgabe
begin
  SetLength(Result,iSze); //neue Dimension
  FillDWord(Result[0],iSze,iDfl) //Vorgabe
end;

function tTools.Init2Byte(iRow,iCol:integer):tn2Byt;
{ tTIB erzeugt ein Integer-Array mit zwei Dimensionen und füllt es mit Null. }
var
  I: integer;
begin
  SetLength(Result,iRow,iCol);
  for I:=0 to pred(iRow) do
    FillByte(Result[I,0],iCol,0);
end;

function tTools.Init3Single(iStk,iRow,iCol:integer; iVal:dWord):tn3Sgl;
{ tTIS erzeugt ein Single-Array mit drei Dimensionen und füllt es mit Null. }
var
  I,K: integer;
begin
  SetLength(Result,iStk,iRow,iCol);
    for I:=0 to pred(iStk) do
      for K:=0 to pred(iRow) do
        FillDWord(Result[I,K,0],iCol,iVal);
end;

{ tTIS erzeugt ein Single-Array mit zwei Dimensionen und füllt es mit dem Wert
  "iDft. iDft kann jeder 32-Bit Ausdruck sein. }

function tTools.Init2Single(
  iRow,iCol: integer; //Bildgröße
  iDfl: dWord): //Vorgabe-Wert (32 Bit)
  tn2Sgl; //Matrix mit Vorgabe
var
  I: integer;
begin
  SetLength(Result,iRow,iCol);
  for I:=0 to pred(iRow) do
    FillDWord(Result[I,0],iCol,iDfl);
end;

{ tTBM kopiert einen Bildkanal unverändert }

function tTools.BandMove(fxSrc:tn2Sgl):tn2Sgl; //Vorbild, Ergebnis
var
  iSze: integer=0;
  Y: integer;
begin
  SetLength(Result,length(fxSrc),length(fxSrc[0]));
  iSze:=length(Result[0])*SizeOf(single);
  for Y:=0 to high(Result) do
    move(fxSrc[Y,0],Result[Y,0],iSze);
end;

function tTools._CheckTarget(sDir:string):string;
const
  cTrg = 'tTCT: Target directory not found: ';
begin
  Result:=trim(sDir); //Leerzeichen entfernen
  if length(Result)<1 then
    Result:=eeHme //Imalys-Verzeichnis
  else if Result[length(Result)]<>'/' then
    Result:=Result+'/';
  if not DirectoryExists(Result) then Tools.ErrorOut(2,cTrg+Result);
end;

{ tTCI kopiert eine Matrix mit 32-Bit Einträgen (Integer, dWord, Single, …)
  Durch den move-Befehl werden alle Werte unverändert übernommen. }

function tTools.CopyGrid(ixTmp:tn2Int):tn2Int; //Vorlage: Ergebnis
var
  iSze: integer; //Länge einer Zeile in Byte
  Y: integer;
begin
  SetLength(Result,length(ixTmp),length(ixTmp[0]));
  iSze:=length(ixTmp[0])*SizeOf(integer);
  for Y:=0 to high(Result) do
    move(ixTmp[Y,0],Result[Y,0],iSze)
end;

{ tTII erzeugt ein Integer-Array und initialisiert es mit dem eigenen Index }

function tTools.InitIndex(iSze:integer):tnInt;
var
  I:integer;
begin
  SetLength(Result,iSze); //Dimension
  for I:=0 to pred(iSze) do
    Result[I]:=I; //initialisieren
end;

{ tHO schreibt einen Zeitstempel + den Hinweis "sHnt" nach stdOut und in die
  Protokoll-Datei "cfOut"}

procedure tTools.HintOut(
  bTms:boolean; //Zeitstempel schreiben
  sHnt:string); //Meldung, Status
begin
  if bTms then sHnt:='imalys ['+TimeToStr(Time)+'] '+sHnt; //Zeitpunkt ergänzen
  writeln(#13+sHnt); //Meldung auf Konsole
  TextAppend(eeLog+cfOut,sHnt+#10); //Meldung in Textdatei
end;

procedure tTools.ShapeDelete(sNme:string);
{ tTSD löscht alle Dateien für ein ESRI Shape mit Projektion }
begin
  DeleteFile(ChangeFileExt(sNme,'.shp'));
  DeleteFile(ChangeFileExt(sNme,'.shx'));
  DeleteFile(ChangeFileExt(sNme,'.dbf'));
  DeleteFile(ChangeFileExt(sNme,'.prj'));
end;

function tTools._FirstBand(fxImg:tn3Sgl):tn2Sgl;
var
  iSze:integer; //Byte pro Zeile
  Y:integer;
begin
  SetLength(Result,length(fxImg[0]),length(fxImg[0,0]));
  iSze:=length(fxImg[0,0])*SizeOf(single);
  for Y:=0 to high(fxImg[0]) do
    move(fxImg[0,Y,0],Result[Y,0],iSze);
end;

function tTools.Init2Word(iRow,iCol:integer):tn2Wrd;
{ tTIB erzeugt ein Integer-Array mit zwei Dimensionen und füllt es mit Null. }
var
  I: integer;
begin
  SetLength(Result,iRow,iCol);
  for I:=0 to pred(iRow) do
    FillWord(Result[I,0],iCol,0);
end;

function tTools.CopyHeader(var rHdr:trHdr):trHdr;
begin
  Result:=rHdr;
  Result.Pal:=copy(rHdr.Pal);
  Result.aBnd:=copy(rHdr.aBnd,1,length(rHdr.aBnd))
end;

{ gTO schreibt den Text "sTxt" unverändert in die Datei "sNme" }

procedure tTools.TextOut(
  sNme: string; //Dateiname
  sTxt: string); //Inhalt
const
  cNme = 'Impossible to create file ';
var
  dTxt: TextFile; //Initialisierung
begin
  try
    AssignFile(dTxt,sNme);
    {$i-} Rewrite(dTxt); {$i+}
    if IOResult<>0 then Tools.ErrorOut(2,cNme+sNme);
    write(dTxt,sTxt); //bin-Verzeichnis der gdal-Bibliothek
  finally
    Flush(dTxt);
    CloseFile(dTxt);
  end; //of try ..
end;

{ gTO schreibt den Text "sTxt" unverändert in die Datei "sNme" }

procedure tTools.TextAppend(
  sNme: string; //Dateiname
  sTxt: string); //Inhalt als String mit Zeilentrennern
const
  cNme = 'Impossible to open file ';
var
  dTxt: TextFile; //Initialisierung
begin
  try
    AssignFile(dTxt,sNme);
    if FileExists(sNme)
      then {$i-} Append(dTxt) {$i+}
      else {$i-} Rewrite(dTxt); {$i+}
    if IOResult<>0 then Tools.ErrorOut(2,cNme+sNme);
    write(dTxt,sTxt);
  finally
    Flush(dTxt);
    CloseFile(dTxt);
  end; //of try ..
end;

{ tTR gibt den Inhalt der Datei "sNme" zurück. Der Zeilentrenner "sDlm" ist
  wählbar. Mit "sDlm=''" kann tTR Zeilentrenner löschen. }

function tTools.TextRead(
  sNme:string): //Name des Vorbilds
  string; //Text als String
const
  cNme = 'Impossible to open file ';
var
  dTxt: TextFile; //Initialisierung
  sTmp: string='';
begin
  Result:='';
  try
    AssignFile(dTxt,sNme);
    {$i-} Reset(dTxt); {$i+}
    if IOResult<>0 then Tools.ErrorOut(2,cNme+sNme);
    read(dTxt,Result);
  finally
    CloseFile(dTxt);
  end; //of try ..
end;

function tTools.LineRead(
  sNme:string): //Dateiname
  string; //Text/Zeile
{ tTF liest die erste Zeile aus einer Textdatei }
const
  cNme = 'Impossible to open file: ';
var
  dTxt:TextFile; //Initialisierung
begin
  Result:='';
  try
    AssignFile(dTxt,sNme);
    {$i-} Reset(dTxt); {$i+}
    if IOResult<>0 then Tools.ErrorOut(2,cNme+sNme);
    readln(dTxt,Result) //nur erste Zeile
  finally
    CloseFile(dTxt);
  end; //of try ..
end;

{ tFC kapselt den OS-Befehl "cp". tFC kopiert genau eine Datei ohne weitere
  Parameter. Die Dateinamen dürfen keine Platzhalter verwenden. }

procedure tTools.CopyFile(sDat,sCpy:string);
begin
  prSys.Options:=[poWaitOnExit,poUsePipes]; //modal ausführen
  prSys.Executable:='cp';
  prSys.Parameters.Clear; //Sicherheit
  prSys.Parameters.Add(sDat);
  prSys.Parameters.Add(sCpy);
  prSys.Execute;
end;

{ tOC führt einen Befehl aus und gibt das Ergebnis als String zurück. Dabei
  akzeptiert tOC nur einen Parameter. tOC wurde für einfache Shell-Befehle
  implementiert. }
{ Steht in "sCmd" der Befehl "sh" (LINUX) oder "cmd" (Windows) interpretiert
  tOC "sPrm" als Befehlszeile für die Shell und führt den Befehl in einer Shell
  aus. Steht in "sCmd" ein anderer Befehl, kann er mit einem Parameter in einer
  Zeile aufgerufen werden. tOC setzt Optionen, die den Befehl modal machen und
  alle Meldungen (auch Fehler) in das Funktions-Ergebnis umleiten. }

function tTools.OsCommand(
  sCmd: string; //Befehl
  sPrm: string): //ein Parameter, keine Schalter
  string; //Rückgabe-Wert und Fehlermeldungen
begin
  Result:='';
  prSys.Options:=[poWaitOnExit,poUsePipes];
  prSys.Executable:=sCmd;
  prSys.Parameters.Clear;
  if sCmd='cmd' then prSys.Parameters.Add('/c'); //Befehl aus Parameter übernehmen WINDOWS
  if sCmd='sh' then prSys.Parameters.Add('-c'); //Befehl aus Parameter übernehmen DEBIAN
  prSys.Parameters.Add(sPrm);
  prSys.Execute;
  Result:=trim(GetOutput(prSys)); //Output des Befehls
end;

{ tOE ruft den Befehl "sExc" mit den Parametern in "SlPrm" modal auf. Schalter
  und Werte müssen in getrennten Zeilen übergeben werden. "slPrm" darf keine
  Variable enthalten, die von der Shell interpretiert werden müssten. Output
  und Fehler sind als "prSys.Output" und "prSys.StdErr" zugänglich. "OsReturn"
  kann sie in Dateien umleiten. }

procedure tTools.OsExecute(
  sExc:string; //ausführbares Programm
  SlPrm:TStringList); //Befehls-Parameter
begin
  prSys.Options:=[poWaitOnExit,poUsePipes]; //modal ausführen
  prSys.Executable:=sExc;
  prSys.Parameters.Clear; //Sicherheit
  prSys.Parameters.Assign(SlPrm);
  prSys.Execute;
  //Pipes lokal + spezifisch abfragen! ← keine weiteren Befehle
end;

{ tBI schreibt den Datenblock "faVal" an die Position "iPst" in einer BIT-
  Tabelle. "iPst<0" erzeugt eine neue Tabelle. Wenn "faVal" einen bestehenden
  Datenblock ersetzen soll, muss "faVal" genauso groß sein wie der Vorgänger.
  Wenn "iPst" größer ist als die bestehende Anzahl der Datenblöcke, schreibt
  tBI "faVal" an das Ende der Datei. }

procedure tTools.BitInsert(
  faVal: tnSgl; //Werte-Tabelle
  iPst: integer; //Daten-Block-Index (ab Null)
  sNme: string); //Tabellen-Name (Pfad)
const
  cFit = 'fTBW: Field exchange requires identical array size: ';
  cPst = 'fTBI: Data field position must be positive or zero!';
  cVal = 'fTBI: no data field given given to extend table!';
var
  hBit: integer; //File-Handle Tabelle
  iaOfs: tnLrg=nil; //Einsprung-Adressen
begin
  if faVal=nil then Tools.ErrorOut(2,cVal);
  if iPst<0 then
  begin
    DeleteFile(sNme); //Datei ersetzen
    iPst:=0; //erster Datensatz
  end;
  try
    iaOfs:=BitFormat(hBit,ChangeFileExt(sNme,cfBit)); //File-Handle + Einsprung-Adressen
    if length(iaOfs)>0 then
    begin //vorhandene Tabelle verändern|erweitern
      iPst:=min(iPst,high(iaOfs)); //fortlaufend zählen
      if iPst<high(iaOfs) then //nur wenn Block ersetzt wird
        if length(faVal)*SizeOf(single)<>iaOfs[succ(iPst)]-iaOfs[iPst] then
          Tools.ErrorOut(2,cFit+sNme);
      if iPst=high(iaOfs) then
      begin //Tabelle erweitern
        SetLength(iaOfs,succ(length(iaOfs))); //neues Feld
        iaOfs[succ(iPst)]:=iaOfs[iPst]+length(faVal)*SizeOf(single); //neues Offset
      end;
    end
    else
    begin//neue Tabelle erzeugen
      SetLength(iaOfs,2);
      iaOfs[0]:=SizeOf(int64);
      iaOfs[1]:=iaOfs[0]+length(faVal)*SizeOf(single);
      iPst:=0; //erster Block
    end;
    FileSeek(hBit,0,fsFromBeginning); //Dateianfang
    FileWrite(hBit,iaOfs[high(iaOfs)],SizeOf(int64)); //Offset Metadaten
    FileSeek(hBit,iaOfs[iPst],fsFromBeginning); //Offset Block
    FileWrite(hBit,faVal[0],length(faVal)*SizeOf(single)); //Zahlen-Block
    FileSeek(hBit,iaOfs[high(iaOfs)],fsFromBeginning); //Offset Metadaten
    FileWrite(hBit,iaOfs[0],length(iaOfs)*SizeOf(int64)); //Metadaten
  finally
    if hBit>=0 then FileClose(hBit)
  end;
end;

function tTools.SetDirectory(sDir:string):string;
begin
  if sDir[length(sDir)]<>DirectorySeparator
    then Result:=sDir+DirectorySeparator
    else Result:=sDir;
end;

procedure tTools.CsvDelete(sNme:string);
{ tCD löscht alle Dateien einer CSV Geometrie }
begin
  DeleteFile(ChangeFileExt(sNme,'.csv'));
  DeleteFile(ChangeFileExt(sNme,'.csvt'));
end;

{ tSR verändert den Namen einer Shape-Datei. "sSrc" und "sTrg" müssen passen }

procedure tTools.ShapeRename(sSrc,sTrg:string);
begin
  sTrg:=ExtractFilePath(sSrc)+ExtractFileName(sTrg); //gleiches Verzeichnis
  RenameFile(ChangeFileExt(sSrc,'.shp'),ChangeFileExt(sTrg,'.shp'));
  RenameFile(ChangeFileExt(sSrc,'.shx'),ChangeFileExt(sTrg,'.shx'));
  RenameFile(ChangeFileExt(sSrc,'.dbf'),ChangeFileExt(sTrg,'.dbf'));
  RenameFile(ChangeFileExt(sSrc,'.prj'),ChangeFileExt(sTrg,'.prj'));
end;

{ tSC kopiert alle Teile einer ESRI Shape-Datei }

procedure tTools.CopyShape(sSrc,sTrg:string); //Quell- und Ziel-Datei
begin
  CreateDir(ExtractFileDir(sTrg)); //Sicherheit
  CopyFile(ChangeFileExt(sSrc,'.shp'),ChangeFileExt(sTrg,'.shp'));
  CopyFile(ChangeFileExt(sSrc,'.shx'),ChangeFileExt(sTrg,'.shx'));
  CopyFile(ChangeFileExt(sSrc,'.dbf'),ChangeFileExt(sTrg,'.dbf'));
  CopyFile(ChangeFileExt(sSrc,'.prj'),ChangeFileExt(sTrg,'.prj'));
end;

{ BS gibt die Anzahl der Datenblöcke in der BIT-Datei "sNme" zurück }

function tTools.BitSize(sNme:string):integer; //Name: Anzahl Felder
var
  hBit: integer=-1; //File-Handle
  iaOfs: tnLrg=nil; //Datenblock-Offsets
begin
  Result:=0;
  try
    iaOfs:=BitFormat(hBit,sNme); //File-Handle & Datenblock-Offsets
    Result:=high(iaOfs)
  finally
    if hBit>=0 then FileClose(hBit)
  end;
end;

function tTools.InitByte(iCnt:integer):tnByt;
{ tIB erzeugt ein Integer-Array mit einer Dimensione und füllt es mit Null. }
begin
  SetLength(Result,iCnt);
  FillByte(Result[0],iCnt,0);
end;

function tTools.MaxThema(ixBnd:tn2Byt):byte;
{ tMT gibt die höchste Klassen-ID im Kanal "ixBnd" zurück }
const
  cNil = 'fMT: Image data not defined!';
var
  X,Y: integer;
begin
  if ixBnd=nil then Tools.ErrorOut(2,cNil);
  Result:=0; //keine Klasse
  for Y:=0 to high(ixBnd) do //alle Pixel und Kanäle
    for X:=0 to high(ixBnd[0])do
      Result:=max(ixBnd[Y,X],Result)
end;

function tTools.MinBand(fxBnd:tn2Sgl):single;
{ tMB gibt den kleinsten Wert definierter Wert im Kanal "fxBnd" zurück }
const
  cNil = 'tMB: Image data not defined!';
var
  X,Y: integer;
begin
  if fxBnd=nil then Tools.ErrorOut(2,cNil);
  Result:=MaxSingle; //Vorgabe
  for Y:=0 to high(fxBnd) do //alle Pixel und Kanäle
    for X:=0 to high(fxBnd[0])do
      if not isNan(fxBnd[Y,X]) then
        Result:=min(Result,fxBnd[Y,X]);
end;

function tTools.ArcTanQ(fHrz,fVrt:double):double;
begin
  if fVrt>0 then Result:=arctan(fHrz/fVrt) //Quadrant 1+2
  else if fVrt<0 then
  begin
    if fHrz>0
      then Result:=Pi-arctan(fHrz/-fVrt) //Quadrant 3
      else Result:=arctan(fHrz/fVrt)-Pi //Quadrant 4
  end
  else if fVrt=0 then
    if fHrz>0 then Result:=Pi/2 //zwischen 2+4
    else if fHrz<0 then Result:=-Pi/2 //zwischen 1+3
    else Result:=0;
end;

function tTools.Init3Byte(iBnd,iRow,iCol:integer):tn3Byt;
{ tTIB erzeugt ein Integer-Array mit drei Dimensionen und füllt es mit Null. }
var
  B,I: integer;
begin
  SetLength(Result,iBnd,iRow,iCol);
  for B:=0 to pred(iBnd) do
    for I:=0 to pred(iRow) do
      FillByte(Result[B,I,0],iCol,0);
end;

function TTools.NewFile(
  iCnt:dWord; //Pixel in neuer Datei
  iVal:dWord; //Vorgabe-Wert
  sNme:string): //Name der neuen Datei
  integer; //File-Handle ODER (-1) für Fehler
{ tNF erzeugt eine neue Datei mit dem Namen "sNme" und gibt das Handle zurück.
  Mit "iCnt>0" erzeugt tNF eine Datei mit "iCnt*SizeOf(dWord)" Byte und füllt
  sie mit "iVal". }
const
  cCrt = 'fTNF: Unable to create file: ';
  cBlk = $10000;
var
  iaVal:tnCrd=nil; //dWord-Array
  iBlk:integer; //Pixel-Block
begin
  Result:=-1; //Vorgabe = Fehler
  DeleteFile(sNme); //Sicherheit
  Result:=FileCreate(sNme); //neue Datei
  if Result<0 then
    Tools.ErrorOut(2,cCrt+sNme);

  if iCnt>0 then SetLength(iaVal,cBlk);
  while iCnt>0 do
  begin
    iBlk:=min(iCnt,cBlk);
    FillDWord(iaVal[0],iBlk,iVal);
    FileWrite(Result,iaVal[0],iBlk*SizeOf(dWord));
    iCnt:=iCnt-iBlk;
  end;
end;

{ tCL ersetzt Kommas durch Zeilenwechsel. Leerzeichen werden gelöscht }

function tTools.CommaToLine(sDlm:string):string;
var
  I:integer;
begin
  Result:=DelSpace(sDlm); //alle Leerzeichen entfernen
  for I:=1 to length(Result) do
    if Result[I]=',' then Result[I]:=#10;
end;

procedure tTools.EnviTrash(sNme:string);
{ tET verschiebt eine ENVI-Bild mit qGis-Zusätzen in den Trash }
begin
  OsCommand('gvfs-trash',ChangeFileExt(sNme,''));
  OsCommand('gvfs-trash',ChangeFileExt(sNme,cfHdr));
  OsCommand('gvfs-trash',ChangeFileExt(sNme,cfExt));
end;

function tTools.InitWord(iSze:integer):tnWrd;
{ tTIB erzeugt ein Word-Array und füllt es mit Null. }
begin
  SetLength(Result,iSze);
  FillWord(Result[0],iSze,0);
end;

function tTools.MaskExtend(sMsk:string):string;
var
  sNme:string;
begin
  sNme:=ChangeFileExt(ExtractFileName(sMsk),''); //Name ohne Extension
  if length(sNme)>0 then
    Result:=ExtractFilePath(sMsk)+'*'+sNme+'*'+ExtractFileExt(sMsk) //Name doppelt erweitern
  else
    Result:=ExtractFilePath(sMsk)+'*'+ExtractFileExt(sMsk); //nur Extension verwenden
end;

{ tEL schreibt die aktuelle System-Fehlermldung in die Datei "error.log". tEL
  liest die Fehlermeldung aus der Standard-Error-Pipe. Die Meldung kann mit
  "sPrc" erweitert werden (Prozess, ..).
  ==> tEL erzeugt keinen neuen Fehler! das Ergebnis muss überwacht werden! }

function tTools.ErrorLog(
  sPrc:string): //Nachricht von Imalys (optional)
  boolean; //Nachricht gefunden
var
  iSze:integer; //Zeichen im Error-Stream
  sErr:string='';
begin
  iSze:=prSys.StdErr.NumBytesAvailable; //Anzahl Zeichen im Error-Stream
  Result:=iSze>0;
  if iSze>0 then
  begin
    SetLength(sErr,iSze); //Platz schaffen
    prSys.StdErr.Read(sErr[1],iSze); //Stream als String
    TextAppend(eeLog+cfErr,sPrc+#32+sErr); //in Log-Datei speichern
  end;
end;

{ tGP übernimmt den Output-Pipe vom Prozess "oProcess" und gibt ihn als String
  zurück. Die Pipe wird dabei geleert. }

function tTools.GetOutput(oProcess:tProcess):string;
var
  iSze:integer; //Zeichen im Error-Stream
begin
  Result:=''; //Vorgabe
  if oProcess.Output<>nil then
  begin
    iSze:=oProcess.Output.NumBytesAvailable; //Anzahl Zeichen im Error-Stream
    if iSze>0 then
    begin
      SetLength(Result,iSze); //Platz schaffen
      oProcess.Output.Read(Result[1],iSze); //Stream als String
    end
  end
end;

{ tEO schreibt eine Imalys-Fehlermeldungen "sErr" auf die Console und an das
  Ende der Output-Datei. Danach löst tEO einen Fehler aus. Dabei beendet
  "Level=1" das Programm vollständig (Parse.xLoop), "Level=2" beendet die
  aktuelle Prozess-Kette (Parse.xChain). Alle anderen Werte erzeugen einen
  lokalen Fehler.
  ==> ErrorLog schreibt Fehler-Meldungen externer Programme in eine eigene
      Log-Datei. }

procedure tTools.ErrorOut(
  iLvl:integer; // 1=Programm beenden, 2=Prozess-Kette beenden
  sErr:string); //Nachricht
begin
  if iLvl>0 then
  begin
    writeln('ImalysError '+sErr); //Fehler auf Konsole schreiben
    TextAppend(eeLog+cfOut,'ImalysError '+sErr+#10); //Fehler als Text
  end;
  case iLvl of
    0:; //nur Nachricht ausgeben
    1:raise tLoopError.Create(sErr); //Anwendung anhalten
    2:raise tChainError.Create(sErr); //Prozess anhalten
    3:raise tStepError.Create(sErr); //Schritt anhalten
    else raise Exception.Create(sErr); //allgemein
  end;
end;

{ tLC liest die Datei "sPrc" als Text, entfernt leere Zeilen und Kommentare und
  gibt das Ergebnis als String-Liste zurück. }

function tTools.LoadClean(sTxt:string):tStringList;
const
  cDef = 'tLC: File not found: ';
  cLin = 'tLC: Empty file found: ';
var
  I:integer;
  qR:string;
  qT:integer;
begin
  Result:=nil;
  if FileExists(sTxt) then
  begin
    Result:=tStringList.Create;
    Result.LoadFromFile(sTxt);
    if Result.Count<1 then Tools.ErrorOut(2,cLin+sTxt);
    for I:=pred(Result.Count) downto 0 do
    begin
      qR:=Result[I];
      qT:=pos('#',Result[I]);
      if trim(Result[I])='' then Result.Delete(I) else
      if pos('#',Result[I])>0 then
      begin
        Result[I]:=copy(Result[I],1,pred(pos('#',Result[I])));
        if trim(Result[I])='' then Result.Delete(I)
      end;
    end;
  end
  else Tools.ErrorOut(2,cDef+sTxt);
end;

{ tFT durchsucht den Verzeichnisbaum ab "sDir" mit allen Verzweigungen. Dazu
  verwendet tFT zwei Schleifen. In der inneren Schleife registriert FT in
  "rDat" alle Dateien im aktuellen Verzeichnis und übergibt sie an "yRes". Wenn
  FT ein Verzeichnis findet, erweitert FT die Suchliste "SlDir". FT ignoriert
  "." und ".." Dateien. In der äußeren Schleife ruft jeder Eintrag in "SlDir"
  eine innere Schleife auf. }

function tTools._FileTree(
  sDir: string; //Quell-Verzeichnis (Wurzel)
  yRes: TTreeRes): //Datei-Verarbeitung
  TStringList; //Nachrichten von "yRes"
var
  iIdx: integer=0; //aktuelle Zeile aus "Result"
  rDat: TSearchRec; //Datei-Attribute
  SlDir: TStringList=nil; //Verzeichnisse ab Wurzel
  sTmp: string; //Zwischenlager
begin
  if not DirectoryExists(sDir) then exit;
  try
    SlDir:=TStringList.Create; //Verzeichnisse inclusive Wurzel
    if sDir[length(sDir)]<>DirectorySeparator then sDir+=DirectorySeparator;
    SlDir.Add(sDir); //Wurzel-Verzeichnis im Ziel
    Result:=TStringList.Create; //für Nachrichten
    repeat
      sDir:=SlDir[iIdx]; //aktuelles Verzeichnis
      if FindFirst(sDir+'*',$FF,rDat)=0 then //AnyFile erfasst nicht alles
      repeat
        if (rDat.Name='.') or (rDat.Name='..') then continue; //keine Verwaltung
        sTmp:=trim(yRes(rDat,sDir)); //variables Ergebnis
        if length(sTmp)>0 then
          Result.Add(sTmp); //gültige Antwort
        if ((rDat.Attr and $10)=$10) //Verzeichnis
        and ((rDat.Attr and $40)=0) then //kein Link
          SlDir.Add(sDir+rDat.Name+DirectorySeparator); //Verzeichnis registrieren
      until FindNext(rDat)<>0;
      FindClose(rDat);
      inc(iIdx);
    until iIdx>=SlDir.Count;
  finally
    SlDir.Free;
  end; //finally ..
end;

{ tMT kopiert einen thematischen Layer und gibt ihn als Matrix zurück }

function tTools._MoveThema(ixThm:tn2Byt):tn2Byt;
var
  Y:integer;
begin
  SetLength(Result,length(ixThm),length(ixThm[0]));
  for Y:=0 to high(ixThm) do
    move(ixThm[Y,0],Result[Y,0],length(ixThm[0]));
end;

{ tTIL erzeigt ein Large-Array und füllt es mit Nullen}

function tTools.InitDouble(iSze:integer):tnDbl;
begin
  SetLength(Result,iSze);
  FillDWord(Result[0],iSze*2,0);
end;

{ tTT speichert eine Float-Tabelle als Tab-gerennten Text. Die Zahl der Stellen
  nach dem Komma ist wählbar }

procedure tTools.DoubleToText(
  fxVal:tn2Dbl; //Tabelle als Float
  sNme:string); //Dateiname
const
  cNme = 'Impossible to create file ';
var
  dTxt:TextFile; //Initialisierung
  sLin:string=''; //aktuelle Zeile
  C,R:integer;
begin
  try
    AssignFile(dTxt,sNme);
    {$i-} Rewrite(dTxt); {$i+}
    if IOResult<>0 then Tools.ErrorOut(2,cNme+sNme);

    sLin:='0';
    for C:=1 to high(fxVal[0]) do
      sLin+=#9+IntToStr(C);
    writeln(dTxt,sLin);

    for R:=1 to high(fxVal) do
    begin
      sLin:=IntToStr(R);
      for C:=1 to high(fxVal[R]) do
        sLin+=#9+FloatToStrF(fxVal[R,C],ffFixed,7,3);
      writeln(dTxt,sLin);
    end;
  finally
    Flush(dTxt);
    CloseFile(dTxt);
  end; //of try ..
end;

{ tID erzeigt ein zweidimensionales Large-Array und füllt es mit Nullen}

function tTools.Init2Double(iRow,iCol:integer):tn2Dbl;
var
  I:integer;
begin
  SetLength(Result,iRow,iCol);
  for I:=0 to pred(iRow) do
    FillDWord(Result[I,0],iRow*2,0);
end;

{ tRL verteilt Zahlen in einer kommagetrennten Liste auf ein Float-Array }

function tTools.ValueList(sLst:string):tnSgl; //Eingabe(Komma-getrent): Single-Array
var
  I:integer;
begin
  Result:=Tools.InitSingle(WordCount(sLst,[',']),dWord(NaN)); //Dimension aus Kommas
  for I:=0 to high(Result) do
    TryStrToFloat(ExtractWord(succ(I),sLst,[',']),Result[I]);
end;

{ tSN kürzt den Archiv-Namen von Bilddaten auf ein Krzel aus Sensor, Kachel und
  Datum. Die Angaben im Archiv-Namen als Worte vorkommen und durch Undersores
  getrennt sein. Der Ort im Archiv-Name wird durch die Eingabe "iLft,iMid,iRgt"
  übergeben }

function tTools.ShortName(iLft,iMid,iRgt:integer; sArc:string):string;
begin
  sArc:=ChangeFileExt(ExtractFileName(sArc),''); //nur Dateiname
  Result:=eeHme+
    ExtractWord(iLft,sArc,['_'])+'_'+
    ExtractWord(iMid,sArc,['_'])+'_'+
    ExtractWord(iRgt,sArc,['_']);
end;

{ tTR gibt den Inhalt der Datei "sNme" zurück. Der Zeilentrenner "sDlm" ist
  wählbar. Mit "sDlm=''" kann tTR Zeilentrenner löschen. }

function tTools._eTextRead(
  sNme:string): //Name des Vorbilds
  string; //Text als String
const
  cNme = 'Impossible to open file ';
var
  dTxt: TextFile; //Initialisierung
  sTmp: string='';
begin
  Result:='';
  try
    AssignFile(dTxt,sNme);
    {$i-} Reset(dTxt); {$i+}
    if IOResult<>0 then Tools.ErrorOut(2,cNme+sNme);
    repeat
      readln(dTxt,sTmp);
      Result+=sTmp //kontinuierlich
    until eof(dTxt);
  finally
    CloseFile(dTxt);
  end; //of try ..
end;

{ tHR transformiert eine HSV-Farbe nach RGB. HSV muss in Grad [0..360] für Hue
  und in Anteilen [0..1] für Saturation und Value angegeben werden. Der RGB-
  Wert ist als Anteil der Farbdichten [0..1] formatiert. }

function tTools._HsvToRgb(fHue,fSat,fVal:single):trRgb;
const
  cErr = 'tHR: Undefined input for ';
begin
  if (fHue<0) or (fHue>360) then Tools.ErrorOut(3,cErr+'Hue [0..360]');
  if (fSat<0) or (fSat>1.0) then Tools.ErrorOut(3,cErr+'Saturation [0..1]');
  if (fVal<0) or (fVal>1.0) then Tools.ErrorOut(3,cErr+'Value [0..1]');

  Result:=crRgb;

  with Result do
    if fHue<60 then begin Red:=1; Grn:=fHue/60 end else
    if fHue<120 then begin Red:=(120-fHue)/60; Result.Grn:=1 end else
    if fHue<180 then begin Grn:=1; Blu:=1-(180-fHue)/60 end else
    if fHue<240 then begin Grn:=(240-fHue)/60; Result.Blu:=1 end else
    if fHue<300 then begin Blu:=1; Red:=1-(300-fHue)/60 end else
    if fHue<=360 then begin Blu:=(360-fHue)/60; Result.Red:=1 end;

  Result.Red:=(1-fSat+Result.Red*fSat)*fVal;
  Result.Grn:=(1-fSat+Result.Grn*fSat)*fVal;
  Result.Blu:=(1-fSat+Result.Blu*fSat)*fVal;
end;

{ tSC kopiert den Zonen-Index als Raster-Bild mit Attributen }

procedure tTools.CopyIndex(sTrg:string); //Quell- und Ziel-Datei
begin
  CreateDir(ExtractFileDir(sTrg)); //Sicherheit
  CopyFile(ChangeFileExt(eeHme+cfIdx,''),ChangeFileExt(sTrg,''));
  CopyFile(ChangeFileExt(eeHme+cfIdx,'.hdr'),ChangeFileExt(sTrg,'.hdr'));
  CopyFile(ChangeFileExt(eeHme+cfIdx,'.bit'),ChangeFileExt(sTrg,'.bit'));
  CopyFile(eeHme+cfTpl,ChangeFileExt(sTrg,'')+cfTpl);
end;

initialization

  Tools:=tTools.Create;
  Tools.prSys:=tProcess.Create(nil);

finalization

  Tools.prSys.Free;
  Tools.Free;

end.

//==============================================================================

{ tTIL erzeigt ein Large-Array und füllt es mit Nullen}

function tTools._InitPointer(iSze:integer):tnPtr;
begin
  SetLength(Result,iSze);
  FillDWord(Result[0],iSze*2,0); //entspricht NIL ?!
end;

{ tLP gibt den Abschnitt "iPrt" aus "sImg" zurück. Die Abscnitte sind durch "_"
  getrennt. "sImg" wird als Dateiname interpretiert. }

function tTools._LinePart_(
  iPrt:integer; //laufende Nummer des Abschnitts
  sImg:string):string; //Dateiname mit Abschnitten, durch "_" getrennt
begin
  Result:=ChangeFileExt(ExtractFileName(sImg),'');
  Result:=ExtractWord(iPrt,Result,['_']);
end;

