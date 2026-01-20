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

  cc*, rr*, ss* .. Verdopplung wenn global definiert
  cf*, cm* .. wenn für unit definiert }

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
   - Sensor (S2, L8, ..),
   - Ort (geografisch, Projekt ..)
   - Qualität (bands, thermal, NirV, ..)
   - Spezifikation (Regression, Median, ..)
   - Datum [YYYYMMDD] immer am Ende
   - getrennt durch Underscore }

{ VERÄNDERUNGEN:
   250511: PARSE.Import: Auswahl passender Archve von der Extraktion entkoppelt
           ← Archive können nach belieben manuell und aus dem Katalog gewählt
           werden → Bounding-Box aus Raster-Eckpunkten → Eckpunkte aus "gdal-
           info" → Neu: Cover.Intersect → Bounding-Box nach Transformation der
           Exkpunkte bestimmen
   250522: COVER.RasterFrame: Überlappung nahe Null zwishen ROI und Bildkachel
           kann zu Nulldivision führen → Minimale Überlappung erzwingen
   250603: PARSE.Catalog: auch aus Sammlung von extrahierten Bilddaten, sonst
           wie Archive. RasterCover kann wahlweise geographiche oder
           projizierte Koordinaten übergeben
   250620  PARSE.Compile: vereinigt Bilder mit gleichem Datum NUR wenn "merge"
           auf "true" gesetzt ist ← innere Logit führt sonst zu Widersprüchen!
   250626  PARSE.Import: Faktor und Offset können als CSV für alle Kanäle
           getrennt übergeben werden.
   250628  PARSE.Compile: automatisch vergebene Kanal-Namen können durch
           Eingabe im CSV-Format ersetzt werden
   250904  MODEL.Legacy verknüpft jede Zone mit genau einer anderen. Die
           Verknüpfungen verwenden wahlweise den lokal größten oder kleinsten
           Werteines frei wählbaren Attributs. Legacy kann aus einem Höhen-
           Modell den natürlichen Wasser-Abfluss bestimmen oder Zonen mit
           gemeinsamen Merkmalen zusammenfassen, wenn die Merkmale stark
           streuen
   250906  BUILD.EqualFeatures und FILTER.EqualImages normalisieren Attribute
           und Bilddaten auf den Wertebereich [0..1]. Die Grenzwerte werden in
           beiden Fällen durch Percentile bestimmt.
   250918  COMPARE.Distribution erzeugt Tabellen mit der Häufigkeit aller
           Zonen-Attribute in verscheidenen Klassen. Die Klassen werden von
           einer Vektor-Referenz mit passendem Attribut übernommen. Die
           Ergebnisse sind Tab-getrennte Text-Tabellen mit Schwellen und Median
           für alle Attribut-Klassen-Kombinationen
   250929  REDUCE.xIntensity verknüpft Minimum, Maximum und Range in einem
           Drei-Layer-Bild
   251118  PARSE.Import ersetzt. Auswahl geeigneter Kacheln über Kachel-ID für
           alle Sensoren (Archive.xSelectLandsat, Archive.xSelectSentinel),
           Zwischenlager für die extrahierten Layer ist das rbeisverzeichnis.
           "Archive.xQualityMask" wird zum eigenständigen Programm, akzeptierte
           Kanäle werden transformiert, geschnitten und kalibriert aber nicht
           projiziert (Archive.xBandsImport, Filter.xBandsCalibrate)
   251128  PARSE.Compile ersetzt: Bilder mit beliebigem Format werden im ENVI-
           Format gespeichert. Dabei werden sie transformiert, beschnitten,
           projiziert und die Kanäle gefiltert. Alle Ergebnisse bekommen exakt
           denselben Ausschnitt. Kanäle mit gleichem Datum (Flugpfad) werden
           vereinigt und alle Bilder gestackt. Kanal-Namen können überschrieben
           werden. Pixel außerhalb des Frames können auf NoData gesetzt werden.
           Die NoData-Defiition kann erweitert werden.
   251222  ZONES.Elevation erzeugt Micro-Catchments aus einem Höhenmodell und
           speichert sie als Zonen.
   251222  MAPPING.Runoff überarbeitet: Außer dem Höhenmodell und dem Selektor
           für Abfluss am Bildrand [ja|nein] sind keine Eingaben notwendig. Der
           Prozess setzt DrainPoints am Rand der Micro-Catchments, verknüpft
           sie zu immer größeren Einzugsgebieten, bestimmt den Abfluss als
           entwässerte Fläche und zeichnet ihn als Vaktor-Linie.
   260112  MODEL.xZonesMap und MODEL.xFabricMap angepasst. xZonesMap speichert
           ein isoliertes Klassen-Attribut als "mapindex.bit" im Arbeits-
           Speicher, xFabricMap übernimmt es und erzeugt damit seine Attribut-
           Tabelle "context.bit" für Kontakte zwischen Klassen. Die Zonen-
           Attribute werden nicht verändert }

{ STATISTIK:
   → ca. 15.000 Zeilen
   → ca. 350 Routinen }

{$mode objfpc}{$H+}

interface

uses
  Classes, Math, Process, SysUtils, StrUtils;

const
  //Commands = Prozess-Namen
  cfAcy = 'accuracy.tab'; //Klassen-Kontroll-Bild
  cfAlp = 'alpha'; //Bild mit Alpha-Kanal am Ende
  cfAtr = 'index.bit'; //Zellindex-Attribute
  cfBit = '.bit'; //Binary Table Extension
  //cfBnd = 'boundaries'; //Grenzen zwischen Zonen
  cfBrt = 'brightness'; //erste Hauptkomponente als gemeinsame Dichte
  cfBst = 'bestof'; //Median – Mean – Defined
  cfClc = 'calculate'; //Ergebnis einer Kanal-Arithmetik
  cfCmd = 'commands.log'; //aktuelle Befehlskette
  cfCpl = 'compile'; //Ergebnis Bild-Kombination
  cfCre = 'compare.tab'; //Häufigkeit Cluster in Referenzen ← ALS LINK SPEICHERN
  cfCst = 'cluster'; //Clusterung zum Vergleich
  cfCtm = 'catchments'; //Einzugsgebiete
  cfCtx = 'context.bit'; //Attribut-Tabelle für Kontakte
  cfDdr = 'dendrites'; //dendritische Zellform
  cfDrn = 'drainlines'; //Abfluss als Vektor-Graphik
  cfDst = 'distance'; //kleinste Distanz zu Maske
  //cfDvn = 'deviation'; //Gauß'sche Abweichung
  cfDvs = 'diversity'; //Rao's Q-Index für Zellen
  cfErr = 'error.log'; //Fehler + Warnungen
  cfEtp = 'entropy'; //Rao's Q-Index für Pixel-Klassen
  cfEql = 'equal.bit'; //Attribute mit normalisierten Werten
  cfExt = '.aux.xml'; //Extension für qGis Erweiterungen
  cfEvi = 'EVI'; //erweiterter Vegetationsindex
  cfFbr = 'fabric'; //Muster-Klassen
  cfFcs = 'focus.csv'; //Vektoren nach Bearbeitung
  cfFrq = 'frequency.bit'; //Attribute aus Klassen-Häufigkeit
  cfGml = '.gml'; //Geographic Markup Language
  cfHdr = '.hdr'; //Raw-Binary mit ENVI-Header
  cfHry = 'history'; //Zeitreihe aus Hauptkomponenten
  cfHse = 'hillshade'; //Beleuchtung
  cfHsv = 'HSV'; //HSV Bildcodierung für RGB
  cfIdm = 'inverse'; //Inverse Difference Moment
  cfIdx = 'index'; //Zonen-Index (Raster)
  cfImp = 'import'; //GDAL Import im ENVI-Format
  cfItf = 'interflow'; //Durchlauf, Austausch eines Merkmals
  cfKey = 'keys.bit'; //Modell aus Keys (Verknüpfungen)
  cfLai = 'LAI'; //Leaf Area Index Näherung
  cfLcy = 'legacy.bit'; //Attribit für regionale Dichte
  cfLmt = 'limits'; //Schwellwert-Ergebnis als Klassen
  //cfLnk = 'links.bit'; //Verknüpfung der Mikro-Catchments
  cfLow = 'lowpass'; //Kontrast-Reduktion (Emulation)
  cfLpc = 'laplace'; //Laplace-Kernel
  cfLui = 'intensity'; //Landuse-Intensity-Proxy
  cfMap = 'mapping'; //Klassen-Layer aus Modell
  cfMax = 'maximum'; //größter Wert in einem Stapel
  cfMdl = 'model.bit'; //Feature-Modell
  cfMdn = 'median'; //für Rangfolge-Mittelwert
  cfMea = 'mean'; //Mittelwert
  cfMic = 'micro'; //Catchments aus lokalen Minima
  cfMin = 'minimum'; //kleinster Wert in einem Stapel
  cfMix = 'mapindex.bit'; //Klassen-ID der Zonen als Zwischenlager
  cfMrg = 'merge'; //multispektrale Kacheln verknüpfen
  cfMsk = 'mask'; //Maske = binär oder fortlaufende Klassen-ID
  cfNbu = 'NDBI'; //normalized Build-Up Index
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
  cfRnf = 'runoff'; //Abfluss als Zonen-Attribute
  //cfRog = 'roughness'; //spektrale Rauhigkeit
  cfRst = 'raster'; //Endprodukt des Bildimports
  cfSlc = 'selection.txt'; //Namen ausgewählter Bilder
  cfSmp = 'samples'; //Stichproben, Varianten mit gleichen Merkmalen
  cfSpc = 'specificity.tab'; //tab-getrennte Tabelle
  cfStk = 'stack'; //Zwischenergebnis bei Layer-Manipulation
  cfSze = 'cellsize'; //Fläche der Zellen in Pixeln
  cfTab = 'table.csv'; //Tablle im Textformat (tab-getrennt)
  cfThm = 'thema'; //Klassen-Bild aus Referenzen
  cfTpl = 'topology.bit'; //Zell-Topologie
  cfTrf = 'transform'; //Zwischenprodukt
  cfTxr = 'texture'; //Bild nach Textur-Filterung
  cfVal = 'values'; //Bild aus Zell-Attributen
  cfVct = 'vector.csv'; //Vektor-Import als CSV
  cfVrc = 'variance'; //Bild mit Gauß-Abweichung einzelner Pixel
  cfWrp = 'warp'; //Ergebnis einer Pixel-Transformation
  cfWgt = 'weight'; //globale Summe <> Hauptkomponente
  cfZne = 'zones'; //Polygone mit Attributen

  cfByt:array[0..15] of byte = (0, 1,2,4,4,8, 8,0,0,0,0, 0,2,4,8,8); //Byte pro ENVI-Typ

  eeGdl:string = '/usr/bin/'; //gdal-Befehle
  eeHme:string = 'dummy';
  eeLog:string = 'dummy';
  eeNow:string = ''; //Datum:Uhrzeit der Prozessierung

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
  tpCvr = ^trCvr; //typisierter Zeiger
{ ==> _crCvr aktualisieren }

  trEtp = record //Zonen: Homogenität + Verknüpfung (entropie)
    Min:single; //Minimum Varianz
    Sze:integer; //Größe in Pixeln
    Lnk:integer; //Verknüpfte Fläche
    aBnd:tnSgl; //Summen + Quadrate für alle Kanäle
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
  tpFrm = ^trFrm; //typisierter Zeiger

  trGeo = record
    Lat:double; //geogr. Breite
    Lon:double; //geogr. Länge
    Epg:integer; //EPSG-Nr
  end;
  tarGeo = array of trGeo; //Linien, Polygone

  trHdr = record //ENVI-kompatible Header-Information
    Cnt:integer; //höchste Klassen-ID oder Anzahl Zonen (ohne Null)
    Fmt:integer; //ENVI-Datenformat
    Lat,Lon:double; //Koordinaten links oben: Y=Breite, X=Länge
    Nod:double; //NoData-Value
    Pix:double; //Pixelgröße in Meter
    Prd:integer; //Kanäle pro Bild im Stack
    Scn,Lin:integer; //Spalten, Zeilen auf Pixelebene
    Stk:integer; //Kanäle in Datei
    Cys:string; //"coordinate system string" aus gdal-Bibliothek
    Fld:string; //Klassen-Namen ODER Feldnamen für Attribute (kommagetrennt)
    Map:string; //" map info" aus ENVI-Header
    Pif:string; //"projection info" aus ENVI-Header
    Pal:tnCrd; //Palette für alle Klassen
    aBnd:string; //Kanal-Namen, getrennt durch Zeilenwechsel!
    aCvr:string; //Abdeckung mit klaren Pixeln, Bilder getrennt
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

  trRgb = record
    Red:single; //Farb-Dichten [0..1]
    Grn:single;
    Blu:single;
  end;

  trRoi = record
    Frm:trFrm; //Auswahl-Rahmen
    Tle:string; //Kachel-ID
  end;

  trSlc = record //Abschnitte aus Dateinamen lesen
    Ofs:integer;
    Sze:integer;
  end;

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
    function _CheckTarget_(sDir:string):string;
    function CommaToLines(sDlm:string):string;
    procedure CommaToTab(var sDlm:string);
    procedure _CopyDir_(sSrc,sDst:string);
    procedure _CopyFile_(sDat,sCpy:string);
    procedure CopyFile(sDat,sCpy:string);
    function CopyGrid(ixTmp:tn2Int):tn2Int;
    function CopyIndex(sSrc,sTrg:string):boolean;
    function CopyShape(sSrc,sTrg:string):boolean;
    procedure _CsvDelete_(sNme:string);
    function CsvToSingle(fDfl:single; iSze:integer; sLst:string):tnSgl;
    function _DirectoryFilter_(sMsk:string):TStringList;
    function EnviCopy(sOld,sNew:string):string;
    procedure EnviDelete(sNme:string);
    procedure EnviRename(sOld,sNew:string);
    procedure _EnviTrash_(sNme:string);
    function ErrorLog(sPrc:string):boolean;
    procedure ErrorOut(iLvl:integer; sErr:string);
    function FileFilter(sMsk:string):TStringList;
    function _FileTree_(sDir:string; yRes:TTreeRes):TStringList;
    function _GetError_:string;
    function GetOutput(oProcess:tProcess):string;
    procedure HintOut(bTms:boolean; sHnt:string);
    function _HsvToRgb_(fHue,fSat,fVal:single):trRgb;
    function _ImportList(sDir:string; slBnd:tStringList):string;
    function InitByte(iCnt:integer):tnByt;
    function Init2Byte(iRow,iCol:integer):tn2Byt;
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
    function _LastName(sNme:string):string;
    function LineRead(sNme:string):string;
    function LinesToCommas(sDlm:string):string;
    function LoadClean(sTxt:string):tStringList;
    function MinBand(fxBnd:tn2Sgl):single;
    function NewFile(iCnt,iVal:dWord; sNme:string):integer;
    function OsCommand(sCmd,sPrm:string):string;
    procedure OsExecute(sExc:string; SlPrm:TStringList);
    function SetDirectory(sDir:string):string;
    procedure ShapeDelete(sNme:string);
    procedure _ShapeRename_(sSrc,sTrg:string);
    procedure SingleToText(fxVal:tn2Sgl; sCol,sRow,sTrg:string);
    procedure TextAppend(sNme,sTxt:string);
    procedure TextOut(sNme,sTxt:string);
    function _TextRead_(sNme:string):string;
  end;

const
  crBox: trBox = (Lft:MaxInt; Top:0-MaxInt; Rgt:0-MaxInt; Btm:MaxInt); //Vorgabe für min/max-Prozesse
  crCvr: trCvr = (Crs:''; Pro:''; Epg:0; Wdt:0; Hgt:0; Stk:1; Lft:MaxInt;
         Top:0-MaxInt; Rgt:0-MaxInt; Btm:MaxInt; Pix:0); //Vorgabe für min/max-Prozesse
  crEtp: trEtp = (Min:0; Sze:0; Lnk:0; aBnd:nil);
  crFrm: trFrm = (Crs:''; Epg:0; Lft:MaxInt; Top:0-MaxInt; Rgt:0-MaxInt;
         Btm:MaxInt); //unmöglich, Vorgabe für min/max-Prozesse
  crGeo: trGeo = (Lat:0; Lon:0; Epg:0);
  crHdr: trHdr = (Cnt:0; Fmt:0; Lat:1; Lon:1; Nod:NaN; Pix:1; Prd:0; Scn:0;
         Lin:0; Stk:0; Cys:''; Fld:''; Map:''; Pif:''; Pal:nil; aBnd:'';
         aCvr:'');
  //crGry: trRgb = (Red:0.5; Grn:0.5; Blu:0.5);
  crRgb: trRgb = (Red:0.0; Grn:0.0; Blu:0.0);

var
  Tools: tTools;
  LoopError: tLoopError; //Programm abbrechen
  ChainError: tChainError; //Prozesskette abbrechen
  StepError: tStepError; //Teilschritt abbrechen

implementation

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

{ tNF erzeugt eine neue Datei mit dem Namen "sNme" und gibt das Handle zurück.
  Mit "iCnt>0" erzeugt tNF eine Datei mit "iCnt*SizeOf(dWord)" Byte und füllt
  sie mit "iVal". }

function TTools.NewFile(
  iCnt:dWord; //Pixel in neuer Datei
  iVal:dWord; //Vorgabe-Wert
  sNme:string): //Name der neuen Datei
  integer; //File-Handle ODER (-1) für Fehler
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

{ tTEC kopiert ENVI Bilddaten mit einem neuen Namen }

function tTools.EnviCopy(sOld,sNew:string):string;
begin
  Result:=sNew; //Vorgabe
  if sOld=sNew then exit;
  CopyFile(ChangeFileExt(sOld,''),ChangeFileExt(sNew,'')); //Datei
  CopyFile(ChangeFileExt(sOld,cfHdr),ChangeFileExt(sNew,cfHdr)); //Header
  //FileCopy(ChangeFileExt(sOld,cfExt),ChangeFileExt(sNew,cfExt)); //Erweiterung
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

function tTools._CheckTarget_(sDir:string):string;
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
  Durch den move-Befehl werden Inhalte, nicht Adressen kopiert. }

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

{ tTIB erzeugt ein Integer-Array mit zwei Dimensionen und füllt es mit Null. }

function tTools.Init2Word(iRow,iCol:integer):tn2Wrd;
var
  I: integer;
begin
  SetLength(Result,iRow,iCol);
  for I:=0 to pred(iRow) do
    FillWord(Result[I,0],iCol,0);
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
    write(dTxt,sTxt); //als Datei speichern
  finally
    Flush(dTxt);
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
  //prSys.Options:=[poUsePipes];
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
  cVal = 'fTBI: no data field given to extend table!';
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
          Tools.ErrorOut(3,cFit+sNme);
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

{ tSD ergänzt den Directory Separator wenn nötig }

function tTools.SetDirectory(sDir:string):string;
begin
  if sDir[length(sDir)]<>DirectorySeparator
    then Result:=sDir+DirectorySeparator
    else Result:=sDir;
end;

{ tCD löscht alle Dateien einer CSV Geometrie }

procedure tTools._CsvDelete_(sNme:string);
begin
  DeleteFile(ChangeFileExt(sNme,'.csv'));
  DeleteFile(ChangeFileExt(sNme,'.csvt'));
  DeleteFile(ChangeFileExt(sNme,'.prj'));
end;

{ tSR verändert den Namen einer Shape-Datei. "sSrc" und "sTrg" müssen passen }

procedure tTools._ShapeRename_(sSrc,sTrg:string);
begin
  sTrg:=ExtractFilePath(sSrc)+ExtractFileName(sTrg); //gleiches Verzeichnis
  RenameFile(ChangeFileExt(sSrc,'.shp'),ChangeFileExt(sTrg,'.shp'));
  RenameFile(ChangeFileExt(sSrc,'.shx'),ChangeFileExt(sTrg,'.shx'));
  RenameFile(ChangeFileExt(sSrc,'.dbf'),ChangeFileExt(sTrg,'.dbf'));
  RenameFile(ChangeFileExt(sSrc,'.prj'),ChangeFileExt(sTrg,'.prj'));
end;

{ tSC kopiert alle Teile einer ESRI Shape-Datei }

function tTools.CopyShape(sSrc,sTrg:string):boolean; //Quell- und Ziel-Datei
begin
  Result:=False; //Vorgabe = Error
  CreateDir(ExtractFileDir(sTrg)); //Sicherheit
  CopyFile(ChangeFileExt(sSrc,'.shp'),ChangeFileExt(sTrg,'.shp'));
  CopyFile(ChangeFileExt(sSrc,'.shx'),ChangeFileExt(sTrg,'.shx'));
  CopyFile(ChangeFileExt(sSrc,'.dbf'),ChangeFileExt(sTrg,'.dbf'));
  CopyFile(ChangeFileExt(sSrc,'.prj'),ChangeFileExt(sTrg,'.prj'));
  Result:=FileExists(ChangeFileExt(sTrg,'.shp'));
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

{ tCL ersetzt Kommas durch Zeilenwechsel. Leerzeichen werden gelöscht }

function tTools.CommaToLines(sDlm:string):string;
var
  I:integer;
begin
  Result:=DelSpace(sDlm); //alle Leerzeichen entfernen
  for I:=1 to length(Result) do
    if Result[I]=',' then Result[I]:=#10;
  if Result[length(Result)]=#10 then
    delete(Result,length(Result),1);
end;

procedure tTools._EnviTrash_(sNme:string);
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
  aktuelle Prozess-Kette (Parse.xChain), "Level=3" beendet den aktuellen Befehl
  (Prozess) und "Level=0" schreibt nur die Nachricht. Alle anderen "Levels"
  lösen einen Standard-fehler aus.
  ==> ErrorLog schreibt Fehler-Meldungen externer Programme in eine eigene
      Log-Datei. }

procedure tTools.ErrorOut(
  iLvl:integer; // 1=Programm beenden, 2=Prozess-Kette beenden
  sErr:string); //Nachricht
begin
  writeln('ImalysError '+sErr); //Fehler auf Konsole schreiben
  TextAppend(eeLog+cfOut,'ImalysError '+sErr+#10); //Fehler als Text
  case iLvl of
    0:; //nur Nachricht ausgeben
    1:raise tLoopError.Create(sErr); //Anwendung anhalten
    2:raise tChainError.Create(sErr); //Prozess anhalten
    3:raise tStepError.Create(sErr); //Schritt anhalten
    else raise Exception.Create(sErr); //allgemein
  end;
end;

{ tFT durchsucht den Verzeichnisbaum ab "sDir" mit allen Verzweigungen. Dazu
  verwendet tFT zwei Schleifen. In der inneren Schleife registriert FT in
  "rDat" alle Dateien im aktuellen Verzeichnis und übergibt sie an "yRes". Wenn
  FT ein Verzeichnis findet, erweitert FT die Suchliste "SlDir". FT ignoriert
  "." und ".." Dateien. In der äußeren Schleife ruft jeder Eintrag in "SlDir"
  eine innere Schleife auf. }

function tTools._FileTree_(
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

{ tTIL erzeigt ein Large-Array und füllt es mit Nullen}

function tTools.InitDouble(iSze:integer):tnDbl;
begin
  SetLength(Result,iSze);
  FillDWord(Result[0],iSze*2,0);
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

{ tTR gibt den Inhalt der Datei "sNme" zurück. Der Zeilentrenner "sDlm" ist
  wählbar. Mit "sDlm=''" kann tTR Zeilentrenner löschen. }

function tTools._TextRead_(
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

function tTools._HsvToRgb_(fHue,fSat,fVal:single):trRgb;
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

{ tLC liest die Datei "sPrc" als Text, entfernt leere Zeilen und Kommentare und
  gibt das Ergebnis als String-Liste zurück. }

function tTools.LoadClean(sTxt:string):tStringList;
const
  cDef = 'tLC: File not found: ';
  cLin = 'tLC: Empty file found: ';
var
  iTld:integer; //Position der Tilede in einem String
  sIam:string=''; //aktuelles Home-Verzeichnis
  I:integer;
begin
  Result:=nil;
  sIam:='/home/'+OsCommand('whoami','');
  if FileExists(sTxt) then
  begin
    Result:=tStringList.Create;
    Result.LoadFromFile(sTxt);
    if Result.Count<1 then Tools.ErrorOut(2,cLin+sTxt);
    for I:=pred(Result.Count) downto 0 do
    begin
      Result[I]:=trim(Result[I]); //Sicherheit
      if pos('#',Result[I])>0 then //Kommentar
        Result[I]:=copy(Result[I],1,pred(pos('#',Result[I]))); //Kommentar löschen
      if trim(Result[I])='' then //Leerzeile
      begin //Zeile löschen
        Result.Delete(I);
        continue
      end;
      iTld:=pos('~',Result[I]); //Kürzel für Home-Verzeichnis
      if (iTld>0) and (pos('=',Result[I])<iTld) then //Tilde in Parameter-Zeile
        Result[I]:=copy(Result[I],1,pred(iTld))+sIam+ //home-Verzeichnis einsetzen
          copy(Result[I],succ(iTld),$FFF);
          //'/home/'+OsCommand('whoami','')+copy(Result[I],succ(iTld),$FFF);
    end
  end
  else Tools.ErrorOut(2,cDef+sTxt);
end;

{ tCL ersetzt Kommas durch Zeilenwechsel. tCL entfernt alle Leerzeichen }

function tTools.LinesToCommas(sDlm:string):string;
var
  I:integer;
begin
  Result:=DelSpace(sDlm); //alle Leerzeichen entfernen
  for I:=1 to length(Result) do
    if Result[I]=#10 then Result[I]:=',';
  if Result[length(Result)]=',' then
    delete(Result,length(Result),1)
end;

function tTools.CsvToSingle(
  fDfl:single; //Vorgabe für Fehler
  iSze:integer; //Array-Dimension
  sLst:string): //CSV String
  tnSgl; //Float-Array
const
  cCsv= 'pSA: Cannot convert to float: ';
var
  fRes:single; //als Zahl
  iCnt:integer=0; //Anzahl Werte im CSV-String
  iPst:integer; //Position im CSV-String
  I:integer;
begin
  Result:=Tools.InitSingle(iSze,dWord(fDfl)); //Vorgabe
  iCnt:=WordCount(sLst,[',']); //Anzahl übergebene Werte
  for I:=0 to pred(iSze) do
  begin
    if I<iCnt
      then iPst:=succ(I) //aktuellen Wert verwenden
      else iPst:=1; //ersten Wert verwenden
    if TryStrToFloat(ExtractWord(iPst,sLst,[',']),fRes) then
      Result[I]:=fRes
    else Tools.ErrorOut(2,cCsv+sLst);
  end;
end;

{ tCL ersetzt Kommas durch Zeilenwechsel. Leerzeichen werden gelöscht }

procedure tTools.CommaToTab(var sDlm:string);
var
  I:integer;
begin
  for I:=1 to length(sDlm) do
    if sDlm[I]=',' then sDlm[I]:=#9;
end;

{ tST speichert eine Float-Tabelle als Tab-gerennten Text. Mit "sCol" und
  "sRow" können beliebige Texte als Spalten- und Zeilenbeschriftung übergeben
  werden. tST ersetzt leere Eingaben durch eine fortlaufende Nummerierung. Die
  Eingaben müssen im CSV-Format übergeben werden. }

procedure tTools.SingleToText(
  fxVal:tn2Sgl; //Tabelle als Float
  sCol:string; //Spalten-Beschriftung, CSV
  sRow:string; //Zeilen-Beschriftung, CSV
  sTrg:string); //Dateiname
const
  cNme = 'Impossible to create file ';
var
  dTxt:TextFile; //Initialisierung
  sLin:string=''; //aktuelle Zeile
  C,R:integer;
begin
  try
    AssignFile(dTxt,sTrg);
    {$i-} Rewrite(dTxt); {$i+}
    if IOResult<>0 then Tools.ErrorOut(2,cNme+sTrg);

    if sCol='' then
      for C:=0 to high(fxVal) do
        sCol+=#9+IntToStr(succ(C)) //Spalten zählen
      else CommaToTab(sCol);
    writeln(dTxt,sCol); //Kopfzeile

    if sRow='' then
      for R:=0 to high(fxVal[0]) do
        sRow+=#9+IntToStr(succ(R)) //Zeilentitel CSV
      else CommaToTab(sRow);

    for R:=0 to high(fxVal[0]) do
    begin
      sLin:=ExtractWord(succ(R),sRow,[#9]);
      for C:=0 to high(fxVal) do
        //sLin+=#9+FloatToStrF(fxVal[C,R],ffFixed,7,6);
        sLin+=#9+FloatToStr(fxVal[C,R]);
      writeln(dTxt,sLin);
    end;
  finally
    Flush(dTxt);
    CloseFile(dTxt);
  end; //of try ..
end;

{ tSC kopiert den Zonen-Index, die Zonen-Attribute und die Zonen-Topologie.
  Quelle und Ziel ist immer ein Verzeichnis, die Namen bleiben unverändert }

function tTools.CopyIndex(sSrc,sTrg:string):boolean; //Quell- und Ziel-Verzeichnis
begin
  Result:=False; //Vorgabe = fehler
  sSrc:=SetDirectory(ChangeFileExt(sSrc,'')); //mit Seperator
  sTrg:=SetDirectory(ChangeFileExt(sTrg,'')); //mit Seperator
  CreateDir(sTrg); //Sicherheit
  CopyFile(sSrc+cfIdx,sTrg+cfIdx); //Zonen-Index
  CopyFile(sSrc+cfIdx+cfHdr,sTrg+cfIdx+cfHdr);
  CopyFile(sSrc+cfAtr,sTrg+cfAtr); //Atribute
  CopyFile(sSrc+cfTpl,sTrg+cfTpl); //Topologie
  Result:=FileExists(sTrg+cfIdx);
  HintOut(true,'CopyIndex: '+ExtractFileName(sTrg))
end;

{ tDF übergibt eine Liste mit allen Vrzeichnissen die zur Maske "sMsk" passen.
  tDF sucht NICHT rekursiv.
  ==> DIE REIHENFOLGE IST NICHT SORTIERT }

function tTools._DirectoryFilter_(
  sMsk:string): //Maske für Namen mit Platzhaltern
  TStringList; //Bildnamen mit Verzeichnis ODER nil
const
  cOpn = 'tDF: Unable to open file or folder: ';
var
  rDir:TSearchRec; //Datei-Attribute
begin
  Result:=nil;
  if not DirectoryExists(ExtractFilePath(sMsk)) then
    Tools.ErrorOut(2,cOpn+sMsk);
  try
    Result:=TStringList.Create;
    if FindFirst(sMsk,faDirectory,rDir)<>0 then exit; //Parameter übergeben + suchen
    repeat
      Result.Add(ExtractFilePath(sMsk)+rDir.Name+DirectorySeparator) //vollständiger Name
    until FindNext(rDir)<>0;
  finally
    FindClose(rDir);
  end; //try ..
end;

// Liste in neues Verzeichnis kopieren

function tTools._ImportList(
  sDir:string; //Quell-Verzeichnis
  slBnd:tStringList):
  string; //Ziel-Verzeichnis in ".imalys"
var
  sExt:string; //Extension
  B:integer;
begin
  Result:=eeHme+Tools._LastName(sDir)+DirectorySeparator; //neues Verzeichnis ..
  CreateDir(Result); //..erzeugen
  sExt:=ExtractFileExt(slBnd[0]);
  for B:=0 to pred(slBnd.Count) do
    CopyFile(slBnd[B],Result+Tools._LastName(slBnd[B])+sExt);
end;

{ aLN extrahiert den letzten Namen aus einem Pfad. Der Name kann eine Datei
  oder ein Verzeichnis sein }

function tTools._LastName(sNme:string):string;
const
  sDs = DirectorySeparator;
var
  iP:integer;
begin
  iP:=rPos('.',sNme); if iP>0 then delete(sNme,iP,$F);
  iP:=length(sNme); if sNme[iP]=sDs then delete(sNme,iP,1);
  iP:=rPos(sDs,sNme); if iP>0 then delete(sNme,1,iP);
  Result:=sNme;
end;

initialization

  Tools:=tTools.Create;
  Tools.prSys:=tProcess.Create(nil);

finalization

  Tools.prSys.Free;
  Tools.Free;

end.

//==============================================================================

{ tTR verändert den Namen einer Klassifikation als Bild und als Zonen-Attribut }

procedure tTools._ThemaProtect_();
const
  cFex = 'tTR: Image not found: ';
  cNew = 'tTR: System operation failed: rename to ';
begin
  if not FileExists(eeHme+cfMap) then Tools.ErrorOut(2,cFex+eeHme+cfMap);

  DeleteFile(eeHme+cfCst); //Vorgänger löschen wenn vorhanden
  DeleteFile(eeHme+cfCst+cfHdr);
  DeleteFile(eeHme+cfCst+cfBit);
  DeleteFile(eeHme+c_fTmp);

  RenameFile(eeHme+cfMap,eeHme+cfCst); //Bild umbenennen
  RenameFile(eeHme+cfMap+cfHdr,eeHme+cfCst+cfHdr); //Header umbenennen
  RenameFile(eeHme+cfMap+cfBit,eeHme+cfCst+cfBit); //Attribut umbenennen
  RenameFile(eeHme+cfMdl,eeHme+c_fTmp); //Modell umbenennen

  if not FileExists(eeHme+cfCst) then Tools.ErrorOut(2,cNew+eeHme+cfCst);
end;

