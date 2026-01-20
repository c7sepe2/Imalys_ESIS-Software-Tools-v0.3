unit custom;

{ CUSTOM interpretiert Imalys-Befehle und vergibt Aufgaben. Jeder Befehl ist
  mit einer Routine verknüpft. Die Routinen treffen logische Entscheidungen und
  rufen weitere Routinen auf. }

{ HOME:
  Imalys benötigt ein Arbeitsverzeichnis. Mit "home" kann jedes Verzeichnis mit
  Schreibrechten gewählt werden. "/home/USER/.imalys" übernimmt Anpassungen und
  aktuelle Befehle. }

{ IMPORT:
  Imalys extrahiert, formatiert, beschneidet und projiziert mit "archives" und
  "region" Bilddaten aus externen Quellen. "region" sammelt die Zwischen-
  Ergebnisse und speichert Bilder als "raster" und Polygone als "vector.csv" im
  Arbeitsverzeinis. Alle weiteren Prozesse übernehmen "raster" und vector.csv"
  als Eingangsdaten. }

{ PROZESSE:
  Imalys liest und schreibt Bilddaten im ENVI-Format und Vektor-Daten im WKT-
  Format. "reduce", "kernel", "index", "features", "mapping" und "compare"
  bearbeiten Bild- und Vektor-Daten. Sie speichern ihr Ergebnis mit dem Namen
  der aktiven Prozesse im Arbeitsspeicher. Bestehende Ergebnisse werden ohne
  Warnung überschrieben! }

{ EXPORT:
  Imalys exportiert Ergebnisse in verschiedene Raster- und Vektor-Formate.
  "target" sammelt die Ergebnisse im Arbeits-Verzeichnis, transformiert und
  vergibt neue Namen. }

{$mode objfpc}{$H+}

interface

uses
  Classes, Math, SysUtils, StrUtils, format;

type
  tParse = class(tObject)
    private
      function Bonds(sVal:string):integer;
      function CheckExtern(slImg:tStringList):boolean;
      function CheckInput(slImg:tStringList):boolean;
      function _Compare(iLin:integer; slPrc:tStringList):integer;
      function Compile(iLin:integer; slPrc:tStringList):integer;
      procedure _ExportMapping(sTrg:string);
      procedure ExportZones(sSrc,sTrg:string);
      function Features(iLin:integer; slPrc:tStringList):integer;
      function Flatten(iLin:integer; slPrc:tStringList):integer;
      function Focus(iLin:integer; slPrc:tStringList):integer;
      function GetParam(sLin:string; var sKey,sVal:string):boolean;
      function Home(iLin:integer; slPrc:tStringList):integer;
      function Import(iLin:integer; slPrc:tStringList):integer;
      procedure _ImportMapping(sSrc:string);
      function Kernel(iLin:integer; slPrc:tStringList):integer;
      function Mapping(iLin:integer; slPrc:tStringList):integer;
      function Protect(iLin:integer; slPrc:tStringList):integer;
      function Replace(iLin:integer; slPrc:tStringList; saVar:tnStr):integer;
      function Retain(sVal:string):integer;
      function Search(sFlt:string):tStringList;
      function _SensorType(sMsk:string):integer;
      function SplitCommands(slCmd:tStringList):tStringList;
      function Variables(var iMax:integer; slPrc:tStringList):tn2Str;
      function wDir(sNme:string):string;
      function Zones(iLin:integer; slPrc:tStringList):integer;
    public
      procedure xChain(slPrc:tStringList; saVar:tnStr);
      procedure xLoop(sPrc:string); //Parameter, Prozesskette
  end;

var
  Parse: tParse;

implementation

uses
  index, mutual, raster, thema, vector;

function ImgDat(p1,p2:Pointer):integer;
begin
  if tprTms(p1)^.Dat<tprTms(p2)^.Dat then Result:=-1 else //nach vorne
  if tprTms(p1)^.Dat>tprTms(p2)^.Dat then Result:=1 else Result:=0;
end;

{ cGP extrahiert Schlüssel und Wert aus einer Parameter-Zeile. cGP orientiert
  sich dabei nur am "=" Zeichen. }

function tParse.GetParam(
  sLin:string; //aktuelle Zeile aus dem Script
  var sKey:string; //Schlüssel: vor dem "=" Zeichen
  var sVal:string): //Wert: nach dem "=" Zeichen
  boolean; //formal gültige Parameter-Zeile
var
  iPst:integer;
begin
  iPst:=pos('=',sLin); //Trenner
  Result:=iPst>0;
  if not Result then exit;
  sKey:=trim(copy(sLin,1,pred(iPst)));
  sVal:=trim(copy(sLin,succ(iPst),$FF));
end;

{ pWk ergänzt das Home-Verzeichnis für Dateinamen ohne Verzeichnis }

function tParse.wDir(sNme:string):string;
begin
  if length(sNme)>0 then
    if ExtractFileDir(sNme)=''
      then Result:=eeHme+sNme
      else Result:=sNme;
end;

{ pSC trennt die Prozesse aus "slCmd" in Prozesse zur Zonen-Geometrie für
  "Build.xFeatures" und in Kernel-Prozesse für einzelne Zonen mit
  "Build.xZonesKernel" }

function tParse.SplitCommands(slCmd:tStringList):tStringList;
var
  I:integer;
begin
  Result:=tStringList.Create;
  for I:=pred(slCmd.Count) downto 0 do
    if (slCmd[I]=cfEtp)
    or (slCmd[I]=cfNrm)
    or (slCmd[I]=cfTxr) then
    begin
      Result.Insert(0,slCmd[I]);
      slCmd.Delete(I)
    end;
end;

{ pBs transformiert die Bezeichner "low"|"medium"|"high" in die Zahlen 1|2|3 }

function tParse.Bonds(sVal:string):integer;
const
  cBnd = 'pBs: Undefined Input to select [zones | bonds]';
begin
  Result:=1; //Vorgabe
  if sVal='' then Result:=$FF else //Höhenmodell
  if sVal='accurate' then Result:=-1 else //Klassifikation
  if sVal='low' then Result:=0 else //keine Beschränkung
  if sVal='medium' then Result:=1 else //geringe Beschränkung
  if sVal='high' then Result:=2 else //starke Beschränkung
    Tools.ErrorOut(3,cBnd);
end;

{ pKn berechnet Kernel- und DTM-Transformationen und speichert das Ergebnis
  unter dem Prozess-Namen. Der Prozess ist durch die Bezeichner in "execute"
  eindeutig bestimmt. Mit "radius">1 glättet pKn das Ergebnis in "R-1" Stufen
  mit einem Gauß-LowPass. pKn verwendet für DTM-Prozesse die GDAL-Bibliothek.
  Sie wird separat aufgerufen. }

function tParse.Kernel(iLin:integer; slPrc:tStringList):integer;
const
  cKey = 'pKl: Keyword not defined: ';
var
  iRds:integer=3; //Vorgabe Kernel-Radius
  sImg:string=''; //Dateiname Raster-Import
  sTrg:string=''; //Ergebnis-Name ODER leer für Prozess-Name
  sKey,sVal:string; //linke, rechte Hälfte der Parameter-Zeile
  slExc:tStringList=nil; //ausführbare Befehle
  I:integer;
begin
  sImg:=eeHme+cfRst; //Vorgabe Vorbild
  try
    slExc:=tStringList.Create;
    for I:=succ(iLin) to pred(slPrc.Count) do
    begin
      Result:=I;
      if not GetParam(slPrc[I],sKey,sVal) then break; //Parameter, Wert
      if sKey='execute' then slExc.Add(sVal) else
      if sKey='radius' then iRds:=StrToInt(sVal) else
      if sKey='select' then sImg:=ChangeFileExt(wDir(sVal),'') else
      if sKey='target' then sTrg:=ChangeFileExt(wDir(sVal),'') else
        Tools.ErrorOut(2,cKey+sKey);
    end;

    for I:=0 to pred(slExc.Count) do
      if slExc[I]=cfHse
        then Filter.Hillshade(sImg) //GDAL-Transformation
        else Filter.xKernel(iRds,slExc[I],sImg,sTrg); //Kernel Transformationen
  finally
    slExc.Free;
  end;
end;

{ pSh übergibt alle Bilder im Arbeitsverzeichnis, die zur Maske "sFlt" passen.
  pSh sucht NICHT rekursiv. ENVI Bilddaten müssen mit der Extension ".hdr"
  gesucht werden, andernfalls werden sie doppelt erfasst. }

function tParse.Search(sFlt:string):tStringList;
var
  I:integer;
begin
  Result:=Tools.FileFilter(sFlt); //Liste aus Maske
  for I:=pred(Result.Count) downto 0 do
    if ExtractFileExt(Result[I])='.hdr' then
      Result[I]:=ChangeFileExt(Result[I],''); //Bild statt Header
  Tools.HintOut(true,'Search: '+IntToStr(Result.Count)+' files');
end;

{ pZs erzeugt aus dem mit "select" übergebenen Bild neue Zonen und visualisiert
  sie als ESRI-Shape ohne Attribute. pZs erzeugt die thematische Datei "index"
  mit den Zonen als Klassen, die Grenzen der Zonen "index.shp" und die Zonen-
  Verknüpfungs-Tabelle "topology.bit". Zonen-Attribute (index.bit) werden mit
  dem Befehl "Features" erzeugt. }

function tParse.Zones(iLin:integer; slPrc:tStringList):integer;
const
  cCmd = 'Key misspelled or not appropriate for "zones": ';
  cGrw = 'Key misspelled or not appropriate for "zones": ';
var
  bDem:boolean=False; //Micro-Catchments aus Höhenmodell erzeugen
  iGrw:integer=1; //Typ Zonen-Wachstum (Vorgabe = medium)
  iMin:integer=0; //kleine Zonen nachträglich löschen
  iSze:integer=50; //Pixel pro Zone (Mittelwert)
  sKey,sVal:string; //linke, rechte Hälfte der Parameter-Zeile
  sImg:string=''; //Vorbild ODER externer Index
  I:integer;
begin
  for I:=succ(iLin) to pred(slPrc.Count) do
  begin
    Result:=I; //aktuelle Zeile
    if not GetParam(slPrc[I],sKey,sVal) then break; //Parameter, Wert
    if sKey='bonds' then iGrw:=Bonds(sVal) else
    if sKey='elevation' then bDem:=sVal='true' else
    if sKey='select' then sImg:=wDir(sVal) else
    if sKey='sieve' then iMin:=StrToInt(sVal) else
    if sKey='size' then iSze:=StrToInt(sVal) else
      Tools.ErrorOut(2,cCmd+sKey) //nicht definierte Eingabe
  end;

  if bDem then Union.xDemZones(sImg) else //Micro-Catchments (für Hydrologischen Abfluss)
  if iGrw=-1 then Union.xMapZones(sImg) else //Vektor-Layer aus Klassen
  if (iGrw>=0) and (Image.AlphaMask(sImg)>0) then //NoData-Maske auf alle Kanäle ausdehnen
    Union.xImgZones(iGrw,iSze,sImg); //Zonen aus Bilddaten
  if iMin>0 then Limits.xSieveZones(iMin); //kleine Zonen löschen
end;

{ pRn überetzt "date" in (-1), "bands" in (+1) und alles Andere in Null }

function tParse.Retain(sVal:string):integer;
begin
  if sVal='date' then Result:=-1 else
  if sVal='bands' then Result:=1 else Result:=0;
end;

{ pFn übernimmt mit "select" ein Bild und reduziert seine Kanäle entsprechend
  dem "execute" Befehl. Das Ergebnis hat den Namen des Befehls. Der Name kann
  mit "target" geändert werden. pFn akzeptiert mehr als einen "execute" Befehl
  für das gleiche Vorbild. Mit "quality" erzeugt pFn ein Bild mit der Anzahl
  der klaren Pixel im Stack. }
{ Die Hauptkomponenten-Transformation "principal" benötigt einen eigenen
  Prozess, der "count" Ergebnis-Kanäle zurückgibt. Mit "count=1" hat das
  Ergebnis genau einen Kanal. }
{ Für "difference" dürfen nur zwei Kanäle oder zwei Bilder übergeben werden.
  In allen anderen Fällen entscheidet "retain", ob pFn Kanäle mit gleichem
  Datum zusammenfasst oder Kanäle mit gleicher Wellenlänge. Für "retain=time"
  muss das Datum der Aufnahme am Ende des Dateinamens stehen oder "import" und
  alle anderen Prozesse müssen im ENVI/IDL Format gerechnet worden sein. }
{ "bands" und "formula" füllen dieselbe Variable. Die verschiedenen Bezeichner
  sollen die Bedienung erleichtern. "bands" ist nur mit ":" gültig, "formula"
  ignoriert ":"}

{ HINT: "reduce" kann zur Kalibrierung verwendet werden wenn die Rohdaten nicht
        in einem Archiv stehen }

function tParse.Flatten(iLin:integer; slPrc:tStringList):integer;
const
  cErr = 'pFl: Reduce command terminated';
  cKey = 'pFl: Undefined parameter under "reduce": ';
var
  iCnt:integer=0; //Kanäle/Hauptkomponenten
  iRtn:integer=0; //auf einen Kanal reduzieren [-1..+1]
  slCmd:tStringList=nil; //Prozesse für gleiche Bildquelle
  sArt:string=''; //Kanäle für NDVI aus Landsat-5-9 oder Sentinel-2 (optisch)
  sImg:string=''; //Bildquelle
  sTrg:string=''; //gewählter Ergebnis-Name
  sKey,sVal:string; //linke, rechte Hälfte der Parameter-Zeile
  I:integer;
begin
  try
    try
      slCmd:=tStringList.Create;
      for I:=succ(iLin) to pred(slPrc.Count) do
      begin
        Result:=I; //aktuelle Zeile
        if not GetParam(slPrc[I],sKey,sVal) then break; //Parameter, Wert
        if sKey='bands' then sArt:=sVal else //Kanal-IDs GLEICHE VARIABLE WIE "FORMULA"
        if sKey='count' then iCnt:=StrToInt(sVal) else //Anzahl Hauptkomponenten
        if sKey='execute' then slCmd.Add(sVal) else //Prozess-Namen (Liste)
        if sKey='formula' then sArt:=sVal else //Arithmetik GLEICHE VARIABLE WIE "BANDS"
        if sKey='retain' then iRtn:=Retain(sVal) else //Zeitpunkt oder Spektrum oder Null
        if sKey='select' then sImg:=wDir(sVal) else //Bildquelle
        if sKey='target' then sTrg:=wDir(sVal) else //neuer Name
          Tools.ErrorOut(2,cKey+sKey);
      end;

      for I:=0 to pred(slCmd.Count) do //alle Befehle
      begin
        if slCmd[I]=cfPca then //alle Hauptkomponenten
        begin
          Image.AlphaMask(sImg); //gleicher Definitionsbereich für alle Kanäle
          Separate.xPrincipal(iCnt,sImg); //rotieren
          Image.HSV(eeHme+cfPca); //in HSV-Farben
        end
        else if slCmd[I]=cfLui then Reduce.xComposit(sImg,sTrg) //AOI-Komposit
        else if slCmd[I]=cfQuy then Reduce.xQualityImage(sImg,sTrg) //Quality-Image für Eingangs-Stack
        else if iRtn<0 then Reduce.xHistory(sArt,slCmd[I],sImg,sTrg) //Zeitreihe erzeugen
        { ToDo: "xHistory" erwartet nach der Zeit sortierte Bilder.
          Nur "Compile.xMergeBands sortiert die Bilder }
        else if iRtn>0 then Reduce.xSplice(sArt,slCmd[I],sImg,sTrg) //Spektralbild erzeugen
        else Reduce.xReduce(sArt,slCmd[I],sImg,sTrg); //auf einen Kanal reduzieren ODER maskieren
      end;
    finally
      slCmd.Free;
    end;
  except
    on tStepError do Tools.ErrorOut(0,cErr);
  end;
end;

{ pFs überträgt Werte aus Bilddaten auf Vektor-Punkte oder ein Pixelraster und
  bestimmt statistische Kennwerte aus Bilddaten. pFs gibt die Ergebnisse einer
  Rasterung als Bild und statistische Werte als Text zurück. }
{ Für "raster" müssen alle Bilddaten mit derselben Projektion übergeben werden,
  Vektor-Punkte "points" transfrmiert pFs automatisch. }

function tParse.Focus(iLin:integer; slPrc:tStringList):integer;
const
  cKey = 'pFs: Undefined parameter under "focus": ';
  cSmp = 100000; //Stichproben
var
  bCrl:boolean=False; //Layer korrelieren
  bSts:boolean=false; //Kanal-Statistik
  sFtr:string=''; //Liste für Input-Attribute (Bilder), mit Kommas getrennt
  sGrd:string=''; //Gitter-Koordinaten UND Schalter
  sImg:string=''; //Vorbild
  sPnt:string=''; //Geometrie für Attribut-Auswahl, Punkte
  sKey,sVal:string; //linke, rechte Hälfte der Parameter-Zeile
  I:integer;
begin
  for I:=succ(iLin) to pred(slPrc.Count) do
  begin
    Result:=I; //aktuelle Zeile
    if not GetParam(slPrc[I],sKey,sVal) then break; //Parameter, Wert
    if sKey='correlate' then bCrl:=sVal='true' else //Liste (Komma) mit Attributen
    if sKey='fields' then sFtr:=sVal else //Liste (Komma) mit Attributen
    if sKey='raster' then sGrd:=wDir(sVal) else //Gitter-Dateiname
    if sKey='points' then sPnt:=wDir(sVal) else //Punkt-Geometrie für Attribute
    if sKey='select' then sImg:=wDir(sVal) else //Vorbild mit Werten
    if sKey='stats' then bSts:=sVal='true' else //Statistik berechnen
       Tools.ErrorOut(2,cKey+sKey); //nicht definierte Eingabe
  end;

  if length(sPnt)>0 then Points.xPointAttrib(0,sFtr,sPnt) else //Bildwerte auf Punkte übertragen
  if length(sGrd)>0 then Points.xGridAttrib(sGrd,sImg,'') else //Bildwerte auf Gitter projizieren
  if bSts then Table.xImageStats(cSmp,sImg) else //statistische Kennwerte
  if bCrl then Table.xLayerCorrelate(sImg); //Korrelation Layer 1 mit allen anderen
end;

{ pHe erzeugt oder verknüpft das Arbeitsverzeichnis und richtet die Protokolle
  ein. Mit "clear=true" löscht pHe das Arbeitsverzeichnis vollständig. Für die
  Protokolle sollten mit "log=Verzeichnis" ein geeigneter Ort gewählt werden. }

function tParse.Home(iLin:integer; slPrc:tStringList):integer;
const
  cDir = 'pHe: Cannot create directory: ';
  cHme = 'pHe: Imalys needs a working directory "user-home/.imalys" !';
var
  bClr:boolean=False; //Home-Verzeichnis leeren
  sKey,sVal:string; //linke, rechte Hälfte der Parameter-Zeile
  I:integer;
begin
  for I:=succ(iLin) to pred(slPrc.Count) do
  begin
    Result:=I; //aktuelle Zeile
    if not GetParam(slPrc[I],sKey,sVal) then break; //Parameter, Wert
    if sKey='directory' then
      eeHme:=Tools.SetDirectory(sVal) else //Arbeits-Verzeichnis
    if sKey='clear' then bClr:=sVal='true' else
    if sKey='log' then
      eeLog:=Tools.SetDirectory(sVal) else //Protokoll-Verzeichnis
      Tools.ErrorOut(2,cHme+sVal);
  end;

  if not DirectoryExists(eeHme) then CreateDir(eeHme); //Arbeitsverzeichnis
  if bClr then Tools.OsCommand('sh','rm -R '+eeHme+'*'); //Verzeichnis leeren
  slPrc.SaveToFile(eeHme+'commands'); //NUR KONTROLLE

  if not DirectoryExists(eeLog) then //Verzeichnis für Protokolle
    if not CreateDir(eeLog) then raise Exception.Create(cDir+eeLog);
  eeNow:='Process: '+FormatDateTime('[YYYY-MM-DD] [tt]',Now)+#10; //Datum + Uhrzeit als ID
  Tools.TextAppend(eeLog+cfOut,ccPrt+eeNow+#10); //Trenn-Linie für Prozess-Output
  Tools.TextAppend(eeLog+cfCmd,ccPrt+eeNow+#10+slPrc.Text); //aktuelle Befehle ergänzen
  Tools.TextAppend(eeLog+cfErr,ccPrt+eeNow+#10); //neues Kapitel
end;

{ pFs ersetzt oder erweitert die Zonen-Attribut-Tabelle "Index.bit". Die
  Attributekönnen mit "append" spezifisch erweitert werden. Ohne "append werden
  sie neu berechnet. }
{ Für Bilddaten übernimmt pFs den Mittelwert aller Pixel in einer Zone. Dabei
  können zwei verschiedene Quellen angegeben werden: "select" und "include".
  Die Kernel-Befehle ("normal", "entropy" unter "execute") wirken nur auf die
  Kanäle unter "select". Mit "include" können weitere Bilddaten für zusätzliche
  Attribute übernommen werden. pFs kann grundsäzlich alle geometrisch passenden
  Bilddaten mit den vorhandenen Zonen kombinieren. "diffusion" implementiert
  einen Werteausgleich für alle Attribute. Mit "values" erzeugt pFs ein
  Kontroll-Bildfür alle Attribute. }

function tParse.Features(iLin:integer; slPrc:tStringList):integer;
const
  cCmd = 'pFs: Key misspelled or not appropriate for "attribute": ';
  cFit = 'pFs: Image size must fit zones geometry!';
var
  bApd:boolean=False; //Attribute nicht erweitern sondern neu rechnen
  bVal:boolean=False; //Bild aus Attribut-Tabelle
  iGen:integer=0; //Generationen für Attribut-Ausgleich
  sIcl:string=''; //Bild-Stapel mit zusätzlichen Attributen
  sImg:string=''; //Bilddaten für Attribute + SCHALTER
  slCmd:tStringList=nil; //Befehle für Zonen-Attribute
  slKrn:tStringList=nil; //Befehle für Zonen-Kernel
  sKey,sVal:string; //linke, rechte Hälfte der Parameter-Zeile
  I:integer;
begin
  try
    slCmd:=tStringList.Create;
    for I:=succ(iLin) to pred(slPrc.Count) do
    begin
      Result:=I; //aktuelle Zeile
      if not GetParam(slPrc[I],sKey,sVal) then break; //Parameter, Wert
      if sKey='append' then bApd:=sVal='true' else //Attribute an bestehende Liste anhängen
      if sKey='diffusion' then iGen:=StrToInt(sVal) else //Attribute "weich" rechnen
      if sKey='execute' then slCmd.Add(sVal) else //Attribute aus Zonen-Geometrie
      if sKey='include' then sIcl:=wDir(sVal) else //Bilddaten auf Zonen abbilden
      if sKey='select' then sImg:=wDir(sVal) else //Bilddaten (Stack) für Attribute
      if sKey='values' then bVal:=sVal='true' else //Attribute-Bild erzeugen
        Tools.ErrorOut(2,cCmd+sKey) //nicht definierte Eingabe
    end;

    if bApd=False then DeleteFile(eeHme+cfAtr); //bestehende Attribute löschen
    slKrn:=SplitCommands(slCmd); //Kernel-Befehle getrennt verarbeiten!
    if length(sImg)>0 then Build.xImageFeatures(sImg); //Attribute aus allen Kanälen (Mittelwerte)
    if slKrn.Count>0 then Build.xKernelFeatures(slKrn,sImg); //Attrubute aus Zonen-Pixeln
    if length(sIcl)>0 then Build.xImageFeatures(sIcl); //Attribute (Mittelwerte) aus weiteren Bilddaten
    if slCmd.Count>0 then Build.xShapeFeatures(sImg,slCmd); //Attribute aus Zonen-Geometrie
    if iGen>0 then Build.xDiffusion(iGen,eeHme+cfAtr); //Attribut lokal mitteln
    if bVal then Build.AttributeImage(nil); //Raster-Bild aus Attributen
  finally
    slCmd.Free;
    slKrn.Free;
  end;
end;

{ pRp ersetzt im Hook Variable im Format "$Ziffer" durch den Text nach dem "="
  Zeichen. pRp ersetzt jedes Vorkommen im übergebenen Text. pRp ignoriert
  Leerzeichen, Tabs und dergl. nach einem Gleichheits-Zeichen.
  VARIABLE DÜRFEN NUR AUS ZWEI BUCHSTANEN (DOLLAR-ZEICHEN + ZIFFER) BESTEHEN }

function tParse.Replace(iLin:integer; slPrc:tStringList; saVar:tnStr):integer;
var
  iItm:integer; //ID der Variable
  iPst:integer; //Position "$" in slPrc-Zeile
  P:integer;
begin
  for P:=succ(iLin) to pred(slPrc.Count) do
    if trim(slPrc[P])='home' then
    begin //"home" Zeile zurückgeben
      Result:=P;
      break
    end;

  for P:=succ(Result) to pred(slPrc.Count) do //alle Zeilen unter "home"
    while pos('$',slPrc[P])>0 do //Variablen-Indikator
    begin
      iPst:=pos('$',slPrc[P]); //Position Indikator
      if TryStrToInt(slPrc[P][succ(iPst)],iItm) then
        slPrc[P]:=copy(slPrc[P],1,pred(iPst))+ //Abschnitt vor Indikator
        saVar[iItm]+copy(slPrc[P],iPst+2,$FFF); //Variable + Abschnitt nach Indikator
    end;
end;

{ pCI prüft, ob mindestens eine gültige Datei in "slImg" übergeben wurde }

function tParse.CheckInput(slImg:tStringList):boolean;
var
  I:integer;
  qS:string;
begin
  for I:=pred(slImg.Count) downto 0 do
  begin
    qS:=slImg[I];
    if not FileExists(slImg[I]) then
      slImg.Delete(I);
  end;
  Result:=slImg.Count>0
end;

{ pCF prüft ob "slImg" nicht ENVI-formatierte Bilder enthält }

function tParse.CheckExtern(slImg:tStringList):boolean;
var
  I:integer;
begin
  Result:=False; //Vorgabe = ENVI-Format
  for I:=0 to pred(slImg.Count) do
    if ExtractFileExt(slImg[I])<>'' then
    begin
      Result:=True;
      break;
    end;
end;

{ pCp kombiniert allen gewählten Bilder zu einem Stack. Dabei können Kanäle,
  CRS, Pixelgröße, der NoData-Wert und der Ausschnitt gewählt werden. Leere
  Bereiche werden auf NaN gesetzt }
{ pCp konvertiert alle gewählten Bilder in das ENVI-Format und speichert sie im
  Arbeitsverzeichnis. Dabei übernimmt pCp einzelne Kanäle, passt CRS und die
  Pixelgröße an und selektiert einzelne Kanäle. pCp projiziert alle Bilder in
  den gewählten Ausschnitt. Ist kein Ausschnitt abgegeben, verwendet pCp die
  Abdeckung des ersten Bilds in der Liste. NACH PCP SIND ALLE BILDER DECKUNGS-
  GLEICH. pCp vereinigt Bilder mit identischem Datum = gleicher Flugpfad zu
  einem Bild und stapelt alle anderen chronologisch }
{ Mit "names" können alle Kanal-Namen nachträglich neu gesetzt werden. Mit
  "clip" setzt pCp alle Pixel außerhalb des "frame" auf NaN und mit "nodata"
  kann ein gewählter Wert auf NaN gesetzt werden }

function tParse.Compile(iLin:integer; slPrc:tStringList):integer;
const
  cCmd = 'pCp: Key misspelled or not appropriate for "compile": ';
  cErr = 'pCp: Compile process terminated';
  cImg = 'pCp: Empty result after image name selection!';
var
  bClp:boolean=False; //Pixel außerhalb des Rahmens auf NoData setzen
  bMrg:boolean=False; //Kanäle mit gleichem datum vereinigen
  fNod:single=NaN; //NoData-Value in Bilddaten
  iEpg:integer=0; //EPSG-Nummer des Koordinatensystema
  iPix:integer=0; //Pixelgröße in Metern
  sArt:string=''; //Kanäle extrahieren von .. bis als BX:BY.
  sFrm:string=''; //Dateiname geometrie mit Bounding-Box = ROI
  sNme:string=''; //Kanal-Namen als CSV Liste
  sTrg:string=''; //Ergebnis-Vorgabe
  slImg:tStringList=nil; //Kanal-Namen
  sKey,sVal:string; //linke, rechte Hälfte der Parameter-Zeile
  I:integer;
begin
  try
    try
      slImg:=tStringList.Create; //ausgewählte Bilder als Liste
      sTrg:=eeHme+'compile'; //Vorgabe
      for I:=succ(iLin) to pred(slPrc.Count) do
      begin
        Result:=I; //aktuelle Zeile
        if not GetParam(slPrc[I],sKey,sVal) then break; //Parameter, Wert
        if sKey='bands' then sArt:=sVal else //Kanal-Nummern "von..bis"
        if sKey='clip' then bClp:=sVal='true' else //Pixeel am Rand auf NoData setzen
        if sKey='crsystem' then iEpg:=StrToInt(sVal) else //Ziel-Projektion
        if sKey='frame' then sFrm:=wDir(sVal) else //Rahmen für Beschnitt und Auswahl
        if sKey='merge' then bMrg:=sVal='true' else //Kanäle mit gleichem Datum überschreiben
        if sKey='names' then sNme:=sVal else //Kanalnamen [CSV] übernehmen
        if sKey='nodata' then fNod:=StrToFloat(sVal) else //NoData im Vorbild
        if sKey='pixel' then iPix:=StrToInt(sVal) else //Pixelgröße [m]
        if sKey='search' then slImg.AddStrings(Search(wDir(sVal))) else //Dateinamen-Filter
        if sKey='select' then slImg.Add(wDir(sVal)) else //Dateinamen explizit übernehmen
        if sKey='target' then sTrg:=wDir(sVal) else //neuer Name
          Tools.ErrorOut(2,cCmd+sKey) //nicht definierte Eingabe
      end;

      //slImg.Count=0?
      if CheckInput(slImg)=False then Tools.ErrorOut(3,cImg); //Eingabe nicht definiert?
      if sTrg='' then sTrg:=eeHme+'compile'; // Vorgabe für Ergebnis verwenden
      if (iEpg>0) or (iPix>0) or (sFrm<>'') or CheckExtern(slImg) then //Transformationen, falsches Format
      begin
        Archive.xImageImport(iEpg,iPix,sArt,sFrm,slImg); //ENVI-Format, projizieren, Rahmen, Kanäle als "c_Name.."
        if bMrg
          then Archive.xMergeBands(sTrg,slImg) //Kanäle mit gleichem Datum vereinigen, Stack
          else Image.xStackImages(sArt,sTrg,slImg); //Bilder als Stack
      end
      else Image.xStackImages(sArt,sTrg,slImg); //Bilder als Stack
      if length(sNme)>0 then Header.BandNames(sNme,sTrg); //Kanalnamen überschreiben
      if bClp then Cover.xClipToShape(sFrm,sTrg); //auf Frame zuschneiden
      if not isNan(fNod) then Filter.xSetNodata(fNod,sTrg); //NoData anpassen
    finally
      slImg.Free;
    end;
  except
    on tStepError do Tools.ErrorOut(0,cErr);
  end;
end;

//Konstante für Sensor-Typ [Landsat, Sentinel, Verzeichnis, allgemein]

function tParse._SensorType(sMsk:string):integer; //Suchmaske: [0..3]
begin
  if ((LeftStr(ExtractFileName(sMsk),2)='LC') //Landsat 8,9
  or (LeftStr(ExtractFileName(sMsk),2)='LT')) and //Landsat 4,5
     (ExtractFileExt(sMsk)='.tar') then Result:=1 //Landsat-Archiv
  else if (LeftStr(ExtractFileName(sMsk),2)='S2') and
    (ExtractFileExt(sMsk)='.zip') then Result:=2 //Sentinel-Archiv
  else Result:=3; //Verzeichnis mit Bilddaten
end;

{ pIM importiert eine Imalys Klassen-Definition zusammen mit den Bilddaten }

procedure tParse._ImportMapping(sSrc:string);
begin
  Gdal.Translate(0,1,1,crFrm,sSrc,eeHme+cfMap); //als ENVI-Datei, ungültiger Frame wird ignoriert
  Tools.CopyFile(ChangeFileExt(sSrc,cfBit),eeHme+cfMap+cfBit)
end;

{ pIp importiert Kanäle aus komprimierten Archiven oder Sammlungen mit Rohdaten
  der Provider und gibt sie als Multi-Kanal-Bilder im ENVI-Format zurück.
  Kacheln, Zeitraum, Kanäle, Ausschnitt, Kalibrierung und NoData können gewählt
  werden. Bei Landsat können mit dem QA-Kanal Bildstörungen maskiert werden }
{ pIp selektiert Archive oder Verzeichnisse nach der Kachel-ID (tiles) und dem
  Zeitraum (period), extrahiert bei Bedarf die gewählten Kanäle (bands),
  beschneidet sie auf den Ausschnitt in "frame", stapelt die gewählten Kanäle
  einzelner Bilder als "Sensor_Kachel_Datum" im Arbeitsverzeichnis }
{ Die Kalchel-IDs können als CSV-Liste übergeben werden. Mit "quality" (nur
  Landsat) extrahiert pIp zunächst den QA-Kanal und übernimmt nur Ausschnitte,
  die mindestens 50% verwendbare Pixel enthalten. pIp kalibriert die Bilder mit
  "offset" und "factor". Beide können als CSV-Liste übergeben werden, wenn die
  Kanäle unterschiedliche Parameter benötigen. Mit "nodata" kann ein gewählter
  Wert in den Bilddaten (z.B. Null) auf NaN gesetzt werden. Die Ergebnisse sind
  im CRS des Providers gespeichert }
{ ==> pIp übernimmt die Projektion der Vorbilder unverändert! }

function tParse.Import(iLin:integer; slPrc:tStringList):integer;
const
  cCmd = 'pIt: Key misspelled or not appropriate for "import": ';
  cErr = 'pIp: Import command terminated';
  cImp = 'pIt: "archive" or ';
  cSns:integer=0; //Sensor-Typ
var
  bQlt:boolean=False; //Landsat-QA-Layer verwenden
  fFit:single=1.0; //Qualität akzeptieren
  fNod:single=0.0; //NoData-Wert in Bilddaten
  sBnd:string=''; //Kanal-Namen als CSV.
  sFct:string=''; //Faktoren für Bild-Kalibrierung (CSV)
  sFrm:string=''; //Dateiname geometrie mit Bounding-Box = ROI
  sMsk:string=''; //Suchmaske für Archiv-Namen (.tar) oder Verzeichnisse
  sOfs:string=''; //Offset(s) für Bild-Kalibrierung (CSV)
  sPrd:string=''; //Zeitperiode als [YYYYMMDD-YYYYMMDD]
  sRes:string=''; //STD-Name im Arbeitsverzeichnis
  sTls:string=''; //Kachel-IDs als CSV.
  slBnd:tStringList=nil; //ausgeählte Kanäle
  slArc:tStringList=nil; //Verzeichnisse mit Original-Bilddaten
  sKey,sVal:string; //linke, rechte Hälfte der Parameter-Zeile
  I:integer;
begin
  try
    try
      for I:=succ(iLin) to pred(slPrc.Count) do
      begin
        Result:=I; //aktuelle Zeile
        if not GetParam(slPrc[I],sKey,sVal) then break; //Parameter, Wert
        if sKey='bands' then sBnd:=sVal else //Kanal-Namen als CSV.
        if sKey='factor' then sFct:=sVal else //Faktor(en) für Kanal-Kalibrierung
        if sKey='frame' then sFrm:=wDir(sVal) else //Rahmen für Beschnitt und Auswahl
        if sKey='nodata' then fNod:=StrToFloat(sVal) else //NoData Wert in Bilddaten
        if sKey='offset' then sOfs:=sVal else //Offset(s) für Kalibrierung
        if sKey='period' then sPrd:=sVal else //Zeitperiode
        if sKey='quality' then bQlt:=sVal='true' else //Quality-Layer einbinden
        if sKey='search' then sMsk:=sVal else //Suchmaske für Archiv-Namen
        if sKey='tiles' then sTls:=sVal else //Kachel-IDs als CSV.
          Tools.ErrorOut(2,cCmd+sKey) //nicht definierte Eingabe
      end;

      if DirectoryExists(sMsk) then Tools.CopyIndex(sMsk,eeHme) else //Zonen-Definition zurückspeichern
      if FileExists(ChangeFileExt(sMsk,cfBit)) then _ImportMapping(sMsk) else //Klassen-Defintion + Bild
      begin
        cSns:=_SensorType(sMsk); //Sensor-ID
{ TODO: "search" mit einem, volständig bezeichnetem Archiv testen }
        if cSns=1
          then slArc:=Archive.xSelectLandsat(bQlt,sMsk,sBnd,sPrd,sTls) //gewählte Archive, Kanäle extrahieren
          else slArc:=Archive.xSelectSentinel(sMsk,sBnd,sPrd,sTls); //Verzeichnisse mit Rohdaten, nur gewählte Kanäle
        for I:=0 to pred(slArc.Count) do
        begin //Bilder aus gewählten Kanälen
          if bQlt and (cSns=1) then
            fFit:=Archive.xQualityMask(sFrm,slArc[I]); //QA-Maske extrahieren und bewerten NUR LANDSAT
          if fFit>0.5 then //nur ausreichende Qualität
          begin
            sRes:=Archive.xBandsImport(bQlt,cSns,sFrm,slArc[I]); //ENVI-Format, beschneiden, maskieren
            Filter.xBandsCalibrate(fNod,sFct,sOfs,sRes); //kalibrieren, NoData
          end;
        end
      end;
    finally
      slBnd.Free;
      slArc.Free;
    end;
  except
    on tStepError do Tools.ErrorOut(0,cErr);
  end;
end;

{ pEZ exportiert Zonen als Shapes mit Attributen. Dazu bildet pET eine CSV-
  Datei mit der Geometrie des Kontroll-Shapes "index.shp" im WKT-Format,
  ergänzt alle Attribute aus der Tabelle "index.bit" und speichert Geometrie
  und Attribute im Shape-Format. Ist nur das Kontroll-Shape (ohne Attribute)
  vorhanden, kopiert pTg es unverändert in das Ziel-Verzeichnis. }

procedure tParse.ExportZones(sSrc,sTrg:string);
const
  cExp = 'pEZ: Zone shape export not successful: ';
var
  iEpg:integer=0; //EPSG-Code
begin
  if FileExists(eeHme+cfAtr) then //Attribute existieren
  begin
    iEpg:=Cover.CrsInfo(eeHme+cfIdx); //EPSG-Code
    Gdal.ImportVect(iEpg,eeHme+cfIdx+'.shp'); //CSV-Geometrie aus Kontroll-Shape
    Points.xPolyAttrib; //Attribute aus Index.bit in CSV-Geometrie eintragen
    Gdal.ExportShape(iEpg,0,eeHme+cfFcs,sTrg); //als Shape exportieren
  end
  else Tools.CopyShape(eeHme+cfIdx,sTrg); //unverändert kopieren
  if not FileExists(sTrg) then Tools.ErrorOut(3,cExp+sTrg);
end;

{ pEM exportiert die aktuelle Klassifikation sowie die Klassen-Definition }

procedure tParse._ExportMapping(sTrg:string);
begin
  Gdal.ExportTo(1,2,eeHme+cfMap,sTrg); //als Byte mit Palette exportieren
  Tools.CopyFile(eeHme+cfMap+cfBit,ChangeFileExt(sTrg,cfBit)) //unter neuem Namen speichern
end;

{ pPt exportiert Bild- und Vektor-Daten aus dem Arbeitsverzeichnis an einen
  gewählten Ort. Bei Bilddaten steuert Extension das Datenformat im Ziel. pPt
  exportiert Bilder ohne Extension im ENVI-Format }
{ Zonen, die Zonen-Definition und Tabellen verwenden eigene Export-Routinen }

function tParse.Protect(iLin:integer; slPrc:tStringList):integer;
const
  cCpy = 'pTg: Image export not successful: ';
  cKey = 'pTg: Keyword not defined: ';
  cPrc = 'pTg: Parameter combination not defined';
  cSrc = 'pTg: Source image not found: ';
var
  sKey,sVal:string; //linke, rechte Hälfte der Parameter-Zeile
  sSrc:string=''; //Quelle
  sTrg:string=''; //Target-Name
  I:integer;
begin
  for I:=succ(iLin) to pred(slPrc.Count) do
  begin
    Result:=I; //aktuelle Zeile
    if not GetParam(slPrc[I],sKey,sVal) then break; //Parameter, Wert
    if sKey='select' then sSrc:=wDir(sVal) else
    if sKey='target' then sTrg:=wDir(sVal) else
    Tools.ErrorOut(2,cKey+sKey); //unzulässiger Parameter
  end;

  if (length(sTrg)<1) or (length(sSrc)<1) then exit; //kein Befehl
  if DirectoryExists(ExtractFilePath(sTrg))=False then
    CreateDir(ExtractFileDir(sTrg)); //Sicherheit

  if sSrc=eeHme+cfIdx then Tools.CopyIndex(eeHme,sTrg) else //Zonen-Definition
  if sSrc=eeHme+cfZne then ExportZones(sSrc,sTrg) else //Zonen-Polygon
  if sSrc=eeHme+cfMap then _ExportMapping(sTrg) else //Klassifikation
  if sSrc=eeHme+cfTab then Tools.CopyFile(sSrc,sTrg) else //Tabelle
  if ExtractFileExt(sTrg)<>'' then Gdal.ExportTo(0,1,sSrc,sTrg) else //anderes Format
  if ExtractFileExt(sTrg)='' then Tools.EnviCopy(sSrc,sTrg) //ENVI-Kopie
  else Tools.ErrorOut(3,cPrc);
{ todo [Parse.Protect]: Export-Funktionen für Abfluss und Legacy fehlen }
end;

{ pMg clustert Bilddaten oder Zonen. Mit "select=index" clustert pMg die
  Attribut-Tabelle der Zonen, in allen anderen Fällen die angegeben Bilddaten.
  Die Anzahl der gewünschten Cluster wird mit "classes" übergeben, die Zahl der
  Stichproben für die Klassifikation mit "samples". Mit "equalize" erzeugt pMg
  temporär auf 0..1 normalisierte Bilddaten bzw. Attribute. pMg normalisiert
  mit dem 1%-99% Percentil. Mit "fabric" klassifiziert pMg Zonen-Attribute
  weiter zu Objekten. Der Wert von "fabric" entscheidet über die räumliche
  Integration der Objekte. "fabric" ist nur zusammen mit "select=index"
  wirksam. }

function tParse.Mapping(iLin:integer; slPrc:tStringList):integer;
const
  cKey = 'pMg: Keyword not defined: ';
var
  bFbr:boolean=False; //Objekte erzeugen
  bRls:boolean=False; //Abfluss am Bildrand frei geben
  bVal:boolean=False; //Klassen in Bildfarben
  fEql:single=0; //Werte normalisieren
  iGen:integer=1; //Generationen bei Fabric-Klassen
  iMap:integer=0; //Vorgabe Anzahl Ergebnis-Klassen
  iSmp:integer=30000; //Vorgabe Stichproben für Klassifikation
  sAtr:string=''; //Zonen-Attribute
  sElv:string=''; //Höhenmodell, Vorbild für Zonen + Schalter
  sImg:string=''; //Vorbild
  sKey,sVal:string; //linke, rechte Hälfte der Parameter-Zeile
  I:integer;
begin
  for I:=succ(iLin) to pred(slPrc.Count) do
  begin
    Result:=I; //aktuelle Zeile
    if not GetParam(slPrc[I],sKey,sVal) then break; //Parameter, Wert
    if sKey='classes' then iMap:=StrToInt(sVal) else //Anzahl Cluster
    if sKey='equalize' then fEql:=StrToFloat(sVal) else //Werte normalisieren
    if sKey='fabric' then bFbr:=sVal='true' else //Fabric-Klassen
    if sKey='reach' then iGen:=StrToInt(sVal) else //Generationen bei Fabric-Klassen
    if sKey='release' then bRls:=sVal='true' else //Abfluss am Bildrand ermöglichen
    if sKey='runoff' then sElv:=wDir(sVal) else //Höhenmodell, Vorbild für Zonen
    if sKey='samples' then iSmp:=StrToInt(sVal) else //Anzahl Stichproben
    if sKey='select' then sImg:=wDir(sVal) else //Vorbild
    if sKey='values' then bVal:=sVal='true' else //Attribute als Bild speichern
      Tools.ErrorOut(2,cKey+sKey) //nicht definierte Eingabe
  end;

  if sImg=eeHme+cfIdx then
  begin //Zonen klassifizieren
    if fEql>0
      then sAtr:=Build.xEqualFeatures(fEql) //Werte mit Percentilen normalisieren
      else sAtr:=eeHme+cfAtr; //Standard-Attribute
    if bFbr then Model.xFabricMap(iMap,iGen,iSmp) else //Kontext klassifizieren
    if sElv<>'' then Model.xRunOff(bRls,sElv) else //Synthetischer Abfluss
    if iMap>0 then Model.xZonesMap(iMap,iSmp,sAtr,eeHme+cfMap); //Klassen-Attribut + Bild
  end
  else
  begin //Pixel klassifizieren
    if fEql>0 then sImg:=Filter.xEqualImages(fEql,iSmp,sImg); //Bildwerte normalisieren
    Model.xPixelMap(iMap,iSmp,sImg); //Klassen-Bild
  end;
  if bVal then Model.xClassValues(5,4,3); //RGB-Palette aus Klassen-Definition
end;

{ pCh interpretiert die Prozess-Kette "sPrc". Die Kette besteht aus Befehlen
  und Parametern. Jeder Befehl oder Parameter steht in einer eigenen Zeile.
  Parameter haben die Form [Schlüssel = Wert]. Das "=" Zeichen macht eine Zeile
  zur Parameter-Zeile. Jeder Befehl ist mit einer Routine verknüpft, die seine
  Parameter liest und die passenden Funktionen aufruft. }
{ pCh liest die Prozess-Kette bis zu einem Befehl. Die entsprechende Procedur
  liest die folgenden Parameter bis zum nächsten Befehl, führt den Befehl aus
  und gibt den Index der zuletzt gelesenen Zeile zurück. }
{ Nur "Import" kann Archive extrahieren und dabei Ausschnitt und Kanäle
  wählen. "Compile" kann beliebige Bildformate lesen und Bildteile zu einem
  zusammenhängenden Bild vereinigen. Nur "Target" kann Bilder und Vektoren
  exportieren. Alle anderen Prozesse bearbeiten Daten im Arbeitsverzeichnis. }
{ Bilder im Arbeitsverzeichnis sind als Raw Binary mit Header im IDL-Format
  gespeichert, Vektordaten im WKT-Format als als CSV-Dateien und Tabellen für
  schnellen Zugriff im generischen BIT-Format. }
{ ToDo: "ChainError" beendet das ganze Programm. Eigene Abfrage für
  "X"-Routinen? }

procedure tParse.xChain(slPrc:tStringList; saVar:tnStr);
const
  cCmd = 'pCn: Unknown command: ';
  cSkp = 'pCn: PROCESS CHAIN TERMINATED!';
var
  iSkp:integer=0; //Zeilen überspringen
  sCmd:string=''; //aktuelle Zeile
  I:integer;
begin
  //xLoop überprüft Header und "replace"
  write(ccPrt+#10); //Trenner
  try
    for I:=1 to pred(slPrc.Count) do
    begin
      if I<iSkp then continue; //Zeilen sind gelesen
      if pos('=',slPrc[I])>0 then continue; //Parameter-Zeile ignorieren
      sCmd:=trim(slPrc[I]);
      if sCmd='' then continue; //leere Zeile
      if sCmd='compare' then iSkp:=_Compare(I,slPrc) else //Referenz vergleichen
      if sCmd='compile' then iSkp:=Compile(I,slPrc) else //Stack aus Bilddaten, Formate
      if sCmd='export' then iSkp:=Protect(I,slPrc) else //Export
      if sCmd='focus' then iSkp:=Focus(I,slPrc) else //Statistik und Punkte
      if sCmd='features' then iSkp:=Features(I,slPrc) else //Zell-Attribute ergänzen
      if sCmd='home' then iSkp:=Home(I,slPrc) else //Arbeits-Verzeichnis
      if sCmd='import' then iSkp:=Import(I,slPrc) else //Extraktion aus Archiven
      if sCmd='kernel' then iSkp:=Kernel(I,slPrc) else //Kernel-Prozesse
      if sCmd='mapping' then iSkp:=Mapping(I,slPrc) else //Clusterer
      if sCmd='reduce' then iSkp:=Flatten(I,slPrc) else //Kanäle reduzieren + Indices
      if sCmd='replace' then iSkp:=Replace(I,slPrc,saVar) else //Variable im Hook ersetzen
      if sCmd='zones' then iSkp:=Zones(I,slPrc) else //Clusterer
        Tools.ErrorOut(2,cCmd+sCmd);
    end;
    Tools.HintOut(true,'DONE');
  except
    on tChainError do Tools.ErrorOut(0,cSkp); //Nachricht, weiter mit nächster Prozesskette
    on tStepError do Tools.ErrorOut(0,cSkp);
{ ToDo: Fehler nach Möglichkeit auf Befehls-Ebene abfangen }
  end;
end;

{ pVs überträgt Variable in der Prozesskette in eine String-Matrix. }
{ Variable werden unter dem Befehl "replace" als "Name = Wert" definiert. pVs
  erlaubt 10 verschiedene Namen: "$0" bis "$9". Jede definierte Eingabe kann
  als Wert übergeben werden. Wird der Wert als kommagetrennte Liste in
  geschweiften Klammern angegeben "{Value-1, Value-2, ...}", wiederholt "xLoop"
  die Prozesskette für jeden Eintrag in der Liste. Wird mehr als eine Variable
  als Liste übergeben, müssen alle Listen gleich lang sein. "xLoop" wechselt
  alle Variablen gemeinsam. Variablen mit einem Wert werden nicht verändert und
  können nach Belieben mit Listen gemischt werden. }

function tParse.Variables(
  var iMax:integer; //höchste Anzahl Spalten = Iterationen
  slPrc:tStringList):
  tn2Str; //Variablen als Matrix
var
  iCnt:integer=0; //Anzahl Variable in Liste
  iHig:integer=0; //ID der "home" Zeile
  iItm:integer=0; //Variablen-ID
  iLow:integer=0; //ID der "replace" Zeile
  sLin:string=''; //aktuelle Variable (Liste)
  C,R:integer;
const
  cFmt = 'pVs: Variable must be defined as "$(one figure)=value": ';
  cHme = 'pVs: The process chain needs a "home" section!';
  cVar = 'pVs: Variable not defined: ';
begin
  Result:=nil; iMax:=1; //Vorgaben
  for R:=1 to pred(slPrc.Count) do
  begin //Variablen-Abschnitt eingrenzen
    if trim(slPrc[R])='replace' then iLow:=R;
    if trim(slPrc[R])='home' then iHig:=R;
    if iHig>0 then break; //"home"-Abschnitt erreicht
  end;
  if iHig=0 then Tools.ErrorOut(2,cHme); //kein "home"
  if iLow=0 then exit; //keine Variable

  SetLength(Result,10,1); //Vorgabe = eine Variable pro ID [0..9]
  for R:=succ(iLow) to pred(iHig) do //Variablen-Block
  begin //Variable in String-Matrix eintragen
    if TryStrToInt(slPrc[R][succ(pos('$',slPrc[R]))],iItm)=False then
      Tools.ErrorOut(2,cFmt+slPrc[R]);
    sLin:=DelSpace(copy(slPrc[R],succ(pos('=',slPrc[R])),$FFF)); //alles ab "=" Zeichen
    if sLin='' then Tools.ErrorOut(2,cVar+IntToStr(iItm));
    if (sLin[1]='{') and (sLin[length(sLin)]='}') then
    begin
      sLin:=copy(sLin,2,length(sLin)-2); //Klammern entfernen
      iCnt:=WordCount(sLin,[',']); //Anzahl Komma-getrennte Ausdrücke
      SetLength(Result[iItm],iCnt); //Platz für Variable
      for C:=0 to pred(iCnt) do
        Result[iItm,C]:=ExtractWord(succ(C),sLin,[',']);
      iMax:=max(iCnt,iMax); //größte Anzahl Variable
    end
    else Result[iItm,0]:=sLin; //eine Variable, auch CSV-Ausdruck
  end;
end;

{ pLp startet die Prozesskette und wiederholt sie, wenn Variable als komma-
  getrennte Liste eingetragen sind (vgl. "Variables"). }
{ pLp prüft die Formatierung der Prozesskette, speichert eine bereinige Kopie,
  ersetzt Variable durch den angegebenen Wert und führt die Prozesskette mit
  "xChain" aus. Zu Beginn bestimmt pLp die Grenzen des "replace" Abschnitts und
  bildet eine Matrix "sxVar" mit allen übergebenen Variablen. Wenn "sxVar" mehr
  als eine Spalte hat, wiederholt pLp die Prozesskette "xChain" für jede Spalte
  in "sxVar". Dabei kombiniert pLp Listen und einzelne Variable in "saTmp" }

procedure tParse.xLoop(sPrc:string); //Parameter, Prozesskette
const
  cFmt = 'pLp: Second line must contain a command. Found: ';
  cIdf = 'pLp: Process chain must start with the key "IMALYS"';
  cSkp = 'pLp: APPLICATION TERMINATED!';
var
  iMax:integer=0; //längste Variablen-Kette
  slPrc:tStringList=nil; //Prozesskette
  slTmp:tStringList=nil; //Prozesskette-Kopie
  saTmp:tnStr=nil; //Liste der aktuellen Variablen
  sxVar:tn2Str=nil; //Variable als Tabelle
  M,V:integer;
begin
  try
    //funktionieren die Variablen?
    eeHme:='/home/'+Tools.OsCommand('whoami','')+'/.imalys/'; //Vorgabe mit Benutzer
    eeLog:=eeHme; //Vorgabe mit Benutzer
    try
      slPrc:=Tools.LoadClean(sPrc); //Prozesskette ohne Kommentare und Leerzeilen
      slTmp:=tStringList.Create; //Kopie für Iterationen
      if pos('IMALYS',slPrc[0])<1 then Tools.ErrorOut(3,cIdf); //Identifier
      if pos('=',slPrc[1])>0 then Tools.ErrorOut(3,cFmt+slPrc[1]); //Parameter ohne Befehl
      sxVar:=Variables(iMax,slPrc); //Variable als Matrix, Anzahl Iterationen in iMax
      SetLength(saTmp,length(sxVar)); //Variable für aktuelle Iteration
      if sxVar<>nil then
        for M:=0 to pred(iMax) do //Iterationen
        begin //Variable für aktuelle Iteration
          for V:=0 to high(sxVar) do //alle Variable
            if length(sxVar[V])>1
              then saTmp[V]:=sxVar[V,M]
              else saTmp[V]:=sxVar[V,0];
          slTmp.Assign(slPrc); //identische Kopie
          xChain(slTmp,saTmp); //Prozess-Kette mit aktuellen Variablen
        end
      else xChain(slPrc,nil); //keine Variable
    finally
      slPrc.Free;
      slTmp.Free;
    end;
  except
    on tLoopError do Tools.ErrorOut(0,cSkp); //Nachricht: Programm Ende
    on tChainError do Tools.ErrorOut(0,cSkp);
    on tStepError do Tools.ErrorOut(0,cSkp);
  end;
end;

{ pCp vergleicht eine Clusterung (mapping) mit einer Referenz auf Pixelebene. }
{ Die Referenzen müssen mit der Clusterung deckungsgleich sein. Wird eine
  Vektor-Datei übergeben, transformiert pCp sie in eine passende Raster-Datei.
  Das Ergebnis sind Tabellen im Text-Format und ein Klassen-Bild mit IDs aus
  der Referenz. }
// Vergleich von Zonen-Atributen mit Klassen ergänzt

function tParse._Compare(iLin:integer; slPrc:tStringList):integer;
const
  cKey = 'pCe: Keyword not defined: ';
  cRfz = 'pCe: Reference format not defined: ';
var
  iSmp:integer=0; //Anzahl Stichproben bei Vergleichen
  sFld:string=''; //Feldname mit Klassen-Namen aus Vektor-Referenz
  sRfz:string=''; //Referenz-Klassifikation zum Vergleich mit "cfMap"
  sKey,sVal:string; //linke, rechte Hälfte der Parameter-Zeile
  I:integer;
begin
  for I:=succ(iLin) to pred(slPrc.Count) do
  begin
    Result:=I;
    if not GetParam(slPrc[I],sKey,sVal) then break; //Parameter, Wert
    if sKey='reference' then sRfz:=wDir(sVal); //Klassen zum Vergleich mit "sRfz"
    if sKey='fieldname' then sFld:=sVal else //Feldname in Referenz (nur vektor)
    if sKey='samples' then iSmp:=StrToInt(sVal) else //Stichproben aus Referenz/Daten
    Tools.ErrorOut(2,cKey+sKey); //unzulässiger Parameter
  end;

  if length(sFld)>0 then
  begin
    Rank.xVectorReference(sFld,sRfz); //Referenz als Raster-Datei
    Build.AttributeImage(nil); //Attribute als Raster-Datei
  end;
  if iSmp>0
    then Rank.xDistribution(iSmp) //Feature-Verteilung auf Referenz-Klassen
    else Rank._xCorrelation(); //Korrelation zwischen Klassen und Referenz
end;

end.

{==============================================================================}

