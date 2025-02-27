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
      function _Breaks(iLin:integer; slPrc:tStringList):integer;
      function Catalog(iLin:integer; slPrc:tStringList):integer;
      function Compare(iLin:integer; slPrc:tStringList):integer;
      function Compile(iLin:integer; slPrc:tStringList):integer;
      function DataType(sKey:string):integer;
      function Features(iLin:integer; slPrc:tStringList):integer;
      function Flatten(iLin:integer; slPrc:tStringList):integer;
      function Focus(iLin:integer; slPrc:tStringList):integer;
      function GetParam(sLin:string; var sKey,sVal:string):boolean;
      function Home(iLin:integer; slPrc:tStringList):integer;
      function Import(iLin:integer; slPrc:tStringList):integer;
      function Kernel(iLin:integer; slPrc:tStringList):integer;
      function Mapping(iLin:integer; slPrc:tStringList):integer;
      procedure MaskPeriod(slImg:tStringList; sPrd:string);
      procedure NoQuality(slArc:tStringList);
      function Protect(iLin:integer; slPrc:tStringList):integer;
      function Replace(iLin:integer; slPrc:tStringList):integer;
      function Retain(sVal:string):integer;
      function RunOff(iLin:integer; slPrc:tStringList):integer;
      function Search(sFlt:string):tStringList;
      function SplitCommands(slCmd:tStringList):tStringList;
      function wDir(sNme:string):string;
      function Zones(iLin:integer; slPrc:tStringList):integer;
    public
      procedure xChain(slPrc:tStringList);
      procedure xLoop(sClr,sPrc:string);
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
  if ExtractFileDir(sNme)=''
    then Result:=eeHme+sNme
    else Result:=sNme;
end;

{ pCp vergleicht eine Clusterung (mapping) mit einer Referenz auf Pixelebene. }
{ Die Referenzen müssen mit der Clusterung deckungsgleich sein. Wird eine
  Vektor-Datei übergeben, transformiert pCp sie in eine passende Raster-Datei.
  Das Ergebnis sind Tabellen im Text-Format und ein Klassen-Bild mit IDs aus
  der Referenz. }

function tParse.Compare(iLin:integer; slPrc:tStringList):integer;
const
  cKey = 'pPt: Keyword not defined: ';
var
  bAcy:boolean=False; //Accuracy-Kontrolle (Bild + Tabellen)
  bAsg:boolean=False; //aktuelle Clusterung referenzieren
  bRst:boolean=False; //Referenz als Raster exportieren
  iEpg:integer=0; //EPSG-Code
  sFld:string=''; //Feldname mit Klassen-Namen aus Vektor-Referenz
  sImg:string=''; //Vorbild
  sRfz:string=''; //Referenz (Vektor oder Raster)
  sKey,sVal:string; //linke, rechte Hälfte der Parameter-Zeile
  I:integer;
begin
  for I:=succ(iLin) to pred(slPrc.Count) do
  begin
    Result:=I;
    if not GetParam(slPrc[I],sKey,sVal) then break; //Parameter, Wert
    if sKey='reference' then sRfz:=sVal else //Referenz (raster oder vektor)
    if sKey='fieldname' then sFld:=sVal else //Feld in Referenz (nur vektor)
    if sKey='raster' then bRst:=sVal='true' else //Raster-Referenz exportieren
    if sKey='assign' then bAsg:=sVal='true' else //Klassen aus Referenz übernehmen
    if sKey='control' then bAcy:=sVal='true' else //Zusätzliche Tabellen
    if sKey='select' then sImg:=sVal else //Vorbld
    Tools.ErrorOut(2,cKey+sKey); //unzulässiger Parameter
  end;

  iEpg:=Cover.CrsInfo(eeHme+cfMap); //EPSG-Code
  if ExtractFileExt(sRfz)<>'' then //keine ENVI-Datei
  begin
    Rank.FieldToMap(iEpg,sFld,eeHme+cfMap,sRfz); //Referenz im Raster-Format
    if bRst then Tools.CopyEnvi(eeHme+cfRfz,ChangeFileExt(sRfz,'_raster')); // Raster-Version exportieren
  end
  else Tools.CopyEnvi(sRfz,eeHme+cfRfz); //Raster-Referenz importieren
  if bAsg //Klassen/Cluster vergleichen
    then Rank.xThemaFit(bAcy,eeHme+cfMap,eeHme+cfRfz)
    else Rank.xScalarFit(bAcy,sImg,eeHme+cfRfz); //Scalare vergleichen
end;

{ pTg exportiert Bild- und Vektor-Daten aus dem Arbeitsverzeichnis an einen
  gewählten Ort. Die Extension steuert das Datenformat im Ziel. }
{ pTg exportiert Zonen als Shapes mit Attributen. Dazu bildet pTg eine CSV-
  Datei mit der Geometrie des Kontroll-Shapes "index.shp" im WKT-Format,
  ergänzt alle Attribute aus der Tabelle "index.bit" und speichert Geometrie
  und Attribute im Shape-Format. Ist nur das Kontroll-Shape (ohne Attribute)
  vorhanden, kopiert pTg es unverändert in das Ziel-Verzeichnis. }
{ pTg exportiert eine Klassifikation zusammen mit der Klassen-Definition. Das
  Bild ist immer im Byte-Format, die Definition im internen BIT-Format. }
{ pTg exportiert Bilddaten in das angegebene Verzeichnis. pTg exportiert
  Bilder ohne Extension im ENVI-Format. In diesem fall bleiben die erweiterten
  Header-Informationen erhalten. }

function tParse.Protect(iLin:integer; slPrc:tStringList):integer;
const
  cBnd = 0; //alle Kanäle exportieren
  cFmt = 1; //unverändert exportieren
  cCpy = 'pTg: Image export not successful: ';
  cKey = 'pTg: Keyword not defined: ';
  cPrc = 'pTg: Parameter combination not defined';
  cSrc = 'pTg: Source image not found: ';
var
  iEpg:integer=0; //EPSG-Code
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
  if not FileExists(sSrc) then Tools.ErrorOut(2,cSrc+sSrc);
  if length(sTrg)<1 then exit; //kein Befehl
  if DirectoryExists(ExtractFileDir(sTrg))=False then
    CreateDir(ExtractFileDir(sTrg)); //Sicherheit

  if ExtractFileName(sSrc)=cfIdx then //Zonen als Polygone
  begin
    if FileExists(eeHme+cfAtr) then //Attribute existieren
    begin
      iEpg:=Cover.CrsInfo(sSrc); //EPSG-Code
      Gdal.ImportVect(iEpg,eeHme+cfIdx+'.shp'); //CSV-Geometrie aus Kontroll-Shape
      Points.xPolyAttrib; //Attribute aus Index.bit in CSV-Geometrie eintragen
      Gdal.ExportShape(iEpg,0,eeHme+cfFcs,sTrg); //als Shape exportieren
    end
    else
    begin
      Tools.CopyShape(sSrc,sTrg); //unverändert kopieren
      Tools.HintOut(true,'Tools.Export: '+ExtractFileName(sTrg))
    end;
{   TODO: Zellindex, Attribute und Topologie in separates Verzeichnis kopieren.
          Das Verzeichnis kann dann für eine Klassifikation verwendet werden. }
  end
  else if ExtractFileName(sSrc)=cfMap then //Klassen: Bild + Definition
  begin
    Gdal.ExportTo(1,2,sSrc,sTrg) //ein Kanal, Byte
  end
  else if ExtractFileName(sSrc)=cfTab then //Tabelle im Text-Format
    Tools.CopyFile(eeHme+cfTab,sTrg) //kopieren
  else if ExtractFileExt(sTrg)<>'' then //neues Bildformat, anderes Verzeichnis
  begin
    Gdal.ExportTo(cBnd,cFmt,sSrc,sTrg); //Bild im gewählten Format
    if not FileExists(sTrg) then Tools.ErrorOut(2,cCpy+sTrg); //Kontrolle
  end
  else if ExtractFileExt(sTrg)='' then //ENVI-Format, externes Verzeichnis
  begin
    Tools.CopyEnvi(sSrc,sTrg); //im ENVI-Format kopieren
    Tools.HintOut(true,'Tools.Export: '+ExtractFileName(sTrg))
  end
  else Tools.ErrorOut(2,cPrc);
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
    if (slCmd[I]=cfEtp) or (slCmd[I]=cfNrm) then
    begin
      Result.Add(slCmd[I]);
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
    Tools.ErrorOut(2,cBnd);
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

{ pHe erzeugt oder verknüpft das Arbeitsverzeichnis und richtet die Protokolle
  ein. Mit "clear=true" oder "eeClr=True" löscht pHe das Arbeitsverzeichnis
  vollständig. Für die Protokolle sollten mit "log=Verzeichnis" ein geeigneter
  Ort gewählt werden. }

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
    if sKey='directory' then eeHme:=sVal  else //Arbeits-Verzeichnis
    if sKey='clear' then bClr:=sVal='true' else
    if sKey='log' then eeLog:=sVal else //Protokoll-Verzeichnis
      Tools.ErrorOut(2,cHme+sVal);
  end;

  if not DirectoryExists(eeHme) then CreateDir(eeHme); //Arbeitsverzeichnis
  eeHme:=Tools.SetDirectory(eeHme); //Delimiter ergänzen
  if bClr then Tools.OsCommand('sh','rm -R '+eeHme+'*'); //Verzeichnis leeren
  slPrc.SaveToFile(eeHme+'commands'); //NUR KONTROLLE

  if not DirectoryExists(eeLog) then //Verzeichnis für Protokolle
    if not CreateDir(eeLog) then raise Exception.Create(cDir+eeLog);
  eeLog:=Tools.SetDirectory(eeLog); //Delimiter ergänzen
  eeNow:='Process: '+FormatDateTime('[YYYY-MM-DD] [tt]',Now)+#10; //Datum + Uhrzeit als ID
  Tools.TextAppend(eeLog+cfOut,ccPrt+eeNow+#10); //Trenn-Linie für Prozess-Output
  Tools.TextAppend(eeLog+cfCmd,ccPrt+eeNow+#10+slPrc.Text); //aktuelle Befehle ergänzen
  Tools.TextAppend(eeLog+cfErr,ccPrt+eeNow+#10); //neues Kapitel
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
  Tools.HintOut(true,'Parse.Search: '+IntToStr(Result.Count)+' files');
end;

{ pRp ersetzt im Hook Variable im Format "$Ziffer" durch den Text nach dem "="
  Zeichen. pRp ersetzt jedes Vorkommen im übergebenen Text. pRp ignoriert
  Leerzeichen, Tabs und dergl. nach dem Gleichheits-Zeichen.
  VARIABLE DÜRFEN NUR AUS ZWEI BUCHSTANEN (DOLLAR-ZEICHEN + ZIFFER) BESTEHEN }

function tParse.Replace(iLin:integer; slPrc:tStringList):integer;
const
  cKey = 'pRe: Alias definition must be formatted as "$Figure = Value"';
  cVar = 'pRe: Variable not defined: $';
var
  iPst:integer; //Position "$" in slPrc-Zeile
  iRow:integer; //Zeile in "slVar" ab Null
  iVid:integer; //Variable-ID aus slPrc-Zeile
  slVar:tStringList=nil; //Variable als Liste
  sKey,sVal:string; //linke, rechte Hälfte der Parameter-Zeile
  C,I:integer;
begin
  try
    slVar:=tStringList.Create;
    for I:=succ(iLin) to pred(slPrc.Count) do
    begin
      Result:=I; //aktuelle Zeile
      if not GetParam(slPrc[I],sKey,sVal) then break; //Parameter, Wert
      if sKey[1]<>'$' then Tools.ErrorOut(2,cKey+sKey); //nicht definierte Eingabe
      iRow:=StrToInt(sKey[2]); //laufende Nummer = Zeile in "slVar"
      while slVar.Count<=iRow do
        slVar.Add(#32); //"leere" Zeile
      slVar[iRow]:=sVal; //eigegebener Wert
    end;

    for C:=I to pred(slPrc.Count) do //nicht Definitionen
    begin
      iPst:=pos('$',slPrc[C]); //Variable suchen
      while iPst>0 do
      begin
        iVid:=StrToInt(slPrc[C][succ(iPst)]); //Variablen-ID als Zahl
        if iVid>=slVar.Count then Tools.ErrorOut(2,cVar+IntToStr(iVid)); //nicht definierte Eingabe
        slPrc[C]:=copy(slPrc[C],1,pred(iPst))+slVar[iVid]+
          copy(slPrc[C],iPst+2,$FF); //Variable einsetzen
        iPst:=pos('$',slPrc[C]); //nächste Variable
      end;
    end;
  finally
    slVar.Free;
  end;
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
    // if sKey='apply' then sCpy:=wDir(sVal) else
    if sKey='select' then sImg:=wDir(sVal) else
    if sKey='sieve' then iMin:=StrToInt(sVal) else
    if sKey='size' then iSze:=StrToInt(sVal) else
      Tools.ErrorOut(2,cCmd+sKey) //nicht definierte Eingabe
  end;

  // if sCpy<>'' then _ImportZones(sIdx) else
  if iGrw>=0 then
  begin
    Image.AlphaMask(sImg); //NoData-Maske auf alle Kanäle ausdehnen
    Union.xZones(iGrw,iSze,sImg); //Zonen erzeugen
  end
  else if iGrw=-1 then Union.xBorders(sImg) //Klassen abgrenzen
  else Tools.ErrorOut(2,cGrw); //nicht definierte Eingabe
  if iMin>0 then Limits.xSieveZones(iMin); //kleine Zonen löschen
end;

{ pSn löscht alle Dateinamen aus der Liste "slImg" die außerhalb des Bereichs
  "sPrd" liegen. pSn unterstellt, dass der Dateiname mit dem Datum endet und
  das Datum als YYYYMMDD codiert ist. "sPrd" muss aus zwei solchen Blöcken mit
  dem ersten und dem letzten zulässigen Datum bestehen. }

procedure tParse.MaskPeriod(
  slImg:tStringList; //Bildnamen
  sPrd:string); //Zeitperiode [YYYYMMDD-YYYYMMDD]
const
  cDat = 'pMP: No date found at: ';
  cPrd = 'pMP: Selection period must be passed as "YYYYMMDD - YYYYMMDD"';
var
  iDat:integer; //Datum im Dateinamen
  iHig,iLow:integer; //Zeitperiode
  I:integer;
begin
  if (TryStrToInt(LeftStr(sPrd,8),iLow)=False) //erstes Datum
  or (TryStrToInt(RightStr(sPrd,8),iHig)=False) then //letztes Datum
    Tools.ErrorOut(2,cPrd); //falsche Eingabe

  for I:=pred(slImg.Count) downto 0 do
  begin
    if TryStrToInt(RightStr(ChangeFileExt(slImg[I],''),8),iDat)=False then //Datum im Bild
      Tools.ErrorOut(2,cDat+slImg[I]); //falsche Eingabe
    if (iDat<iLow) or (iDat>iHig) then
      slImg.Delete(I); //Bild aus Liste löschen
  end;
  Tools.HintOut(true,'Parse.Period: '+IntToStr(slImg.Count)+' images');
end;

{ pFs ersetzt die Zonen-Attribut-Tabelle "Index.bit". Mit "select" übernimmt
  pFs alle Kanäle aus dem Bild "sImg" als Attribute. "sImg" muss nicht das Bild
  sein, mit dem die Zonen gebildet wurden, es muss lediglich dieselbe Länge und
  Breite haben. Mit "execute" erzeugt pFs Attribute aus der Geometrie der Zonen
  und aus den Pixeln einzelner Zonen. Dazu trennt pFs die Befehle in die Listen
  "slCmd" und "slKrn". "diffusion" implementiert einen Werteausgleich für
  Geometrie-Attribute. Mit "values" erzeugt pFs ein Attribut-Kontroll-Bild. }

function tParse.Features(iLin:integer; slPrc:tStringList):integer;
const
  cCmd = 'pFs: Key misspelled or not appropriate for "attribute": ';
  cFit = 'pFs: Image size must fit zones geometry!';
var
  bApd:boolean=False; //Attribute nicht erweitern sondern neu rechnen
  bVal:boolean=False; //Bild aus Attribut-Tabelle
  iGen:integer=0; //Generationen für Attribut-Ausgleich
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
      if sKey='select' then sImg:=wDir(sVal) else //Bilddaten (Stack) für Attribute
      if sKey='values' then bVal:=sVal='true' else //Attribute-Bild erzeugen
        Tools.ErrorOut(2,cCmd+sKey) //nicht definierte Eingabe
    end;

    if bApd=False then DeleteFile(eeHme+cfAtr); //bestehende Attribute löschen
    slKrn:=SplitCommands(slCmd); //Kernel-Befehle getrennt verarbeiten!
    if length(sImg)>0 then //Attribute aus Bilddaten
    begin
      if not Build.SizeFit(eeHme+cfIdx,sImg) then Tools.ErrorOut(2,cCmd+sKey); //nicht definierte Eingabe
      Image.AlphaMask(sImg); //NoData-Maske auf alle Kanäle ausdehnen
      Build.xAttributes(sImg); //Attribute aus allen Bilddaten (Mittelwerte)
      if slKrn.Count>0 then Build.xKernels(slKrn,sImg); //Attrubute aus Zonen-Kerneln
    end;
    if slCmd.Count>0 then Build.xFeatures(sImg,slCmd); //Attribute aus Zonen-Geometrie
    if iGen>0 then Build.xDiffusion(iGen,eeHme+cfAtr); //Attribut lokal mitteln
{   ToDo: [RECENT] "Build._xDiffusion" testen }
    if bVal then Build.xZoneValues(eeHme+cfAtr); //Raster-Bild aus Attributen
  finally
    slCmd.Free;
    slKrn.Free;
  end;
end;

{ pMp erzeugt ein Klassen-Modell und clustert damit Bilddaten. Mit "pixel" als
  Modell clustert pMp den Import auf Pixelbasis, mit "zonal" Zonen-Attribute
  und mit "fabric" Zonen-Kontakte. Für "region" und "fabric" muss ein Zonen-
  Index erzeugt oder importiert werden. Der "entropy" Prozess benötigt eine
  Pixel-Klassifikation. }

function tParse.Mapping(iLin:integer; slPrc:tStringList):integer;
const
  cKey = 'pMg: Keyword not defined: ';
var
  bVal:boolean=False; //Klassen in Bildfarben
  fEql:single=0; //Werte normalisieren
  iGen:integer=0; //Diffusion in Stufen NUR "xFabricMap"
  iItm:integer=30; //Vorgabe Anzahl Ergebnis-Klassen
  iSmp:integer=30000; //Vorgabe Stichproben für Klassifikation
  sAtr:string=''; //Zonen-Attribute
  sImg:string=''; //Vorbild
  sKey,sVal:string; //linke, rechte Hälfte der Parameter-Zeile
  I:integer;
begin
  for I:=succ(iLin) to pred(slPrc.Count) do
  begin
    Result:=I; //aktuelle Zeile
    if not GetParam(slPrc[I],sKey,sVal) then break; //Parameter, Wert
    if sKey='classes' then iItm:=StrToInt(sVal) else //Anzahl Cluster
    if sKey='fabric' then iGen:=StrToInt(sVal) else //Klassen-Muster
    if sKey='equalize' then fEql:=StrToFloat(sVal) else //Werte normalisieren
    if sKey='samples' then iSmp:=StrToInt(sVal) else //Anzahl Stichproben
    if sKey='select' then sImg:=wDir(sVal) else //Vorbild
    if sKey='values' then bVal:=sVal='true' else
      Tools.ErrorOut(2,cKey+sKey) //nicht definierte Eingabe
  end;

{ TODO: Bilder werden mit Percentilen normalisiert, Attribute mit der Standard-
        Abweichung. Percentile für Attribute anbieten! }

  if sImg=eeHme+cfIdx then //Zonen klassifizieren
  begin
    if fEql>0
      then sAtr:=Build.xEqualFeatures(fEql)  //Attribute normalisieren
      else sAtr:=eeHme+cfAtr;
    Model.xZonesMap(iItm,iSmp,sAtr); //Klassen-Attribut + Bild
    if iGen>0 then
      Model._xFabricMap(iGen,iSmp) //Kontext-Attribute klassifizieren
  end
  else
  begin
    if fEql>0 then sImg:=Filter.Normalize(iSmp,sImg); //Bildwerte normalisieren
    Model.xPixelMap(iItm,iSmp,sImg); //Klassen-Bild
  end;

  if bVal then Model.ClassValues(5,4,3); //RGB-Palette aus Klassen-Definition
end;

{ pDT gibt "true" zurück, wenn als Key "true" übergeben wird. In allen anderen
  Fällen ist pDT "false". }

function tParse.DataType(sKey:string):integer;
begin
  Result:=byte(sKey='true');
end;

{ pRn überetzt "time" in (-1), "bands" in (+1) und alles Andere in Null }

function tParse.Retain(sVal:string):integer;
begin
  if sVal='time' then Result:=-1 else
  if sVal='bands' then Result:=1 else Result:=0;
end;

{ pNQ schreibt für alle Kanäle den Quality-Faktor 99.9% . Damit werden alle
  Kanäle übernommen. }

procedure tParse.NoQuality(slArc:tStringList);
var
  I:integer;
begin
  for I:=0 to pred(slArc.Count) do
    slArc.Objects[I]:=tObject(pointer(999)) //Qualität als Zeiger
end;

{ pRO bestimmt den Abfluss aus einem Höhenmodell und gibt das Ergebnis als
  Zonen, Abfluss, Einzugsgebiete und Vektor-Schema zurück. pRO speichert Ebenen
  und Einzugsgebiete lokaler Minima als Zonen ("micro"), die Zonen-Attribute
  "runoff.bit" sind die Verknüpfung der Zonen und die Koordinaten der Abfluss-
  Punkte am Rand der Zonen. Zusäzlich speichert pRO ein Bild der globalen
  Eiznugsgebiete "catchment", wie es sich aus dem aktuellen Ausschnitt ergibt.}

function tParse.RunOff(iLin:integer; slPrc:tStringList):integer;
const
  cKey = 'Key misspelled or not appropriate for "runoff": ';
var
  fNod:single=1-MaxInt; //Wert für NoData
  iLmt:integer=100; //Minimum Einzugsgebiete
  iPln:integer; //Anzahl Ebenen
  sDem:string=''; //Name Höhenmodell
  sTrg:string=''; //Name Ergebnis (kann fehlen)
  sKey,sVal:string; //linke, rechte Hälfte der Parameter-Zeile
  I:integer;
begin
  for I:=succ(iLin) to pred(slPrc.Count) do
  begin
    Result:=I; //aktuelle Zeile
    if not GetParam(slPrc[I],sKey,sVal) then break; //Parameter, Wert
    if sKey='nodata' then fNod:=StrToFloat(sVal) else //nicht definierter Wert
    if sKey='control' then iLmt:=StrToInt(sVal) else //Minimum Catchments
    if sKey='select' then sDem:=wDir(sVal) else //Bildquelle
    if sKey='target' then sTrg:=wDir(sVal) else //neuer Name NICHT AKTIVIERT
      Tools.ErrorOut(2,cKey);
  end;

  iPln:=Drain.xBasins(fNod,iLmt,sDem); //primäre Zonen, Zonen-Attribute
  Lines.xDrainPoints(); //Abfluss-Punkte am Rand der Zonen
  Lines.xDrainLines(iPln); //Vektor-Darstellung des Abflusses
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

function tParse.Flatten(iLin:integer; slPrc:tStringList):integer;
const
  cKey = 'pFl: Undefined parameter under "reduce": ';
var
  iCnt:integer=0; //Kanäle/Hauptkomponenten
  iRtn:integer=0; //auf einen Kanal reduzieren [-1..+1]
  slCmd:tStringList=nil; //Prozesse für gleiche Bildquelle
  sArt:string='B3:B4'; //Kanäle für NDVI aus Landsat-5-9 oder Sentinel-2 (optisch)
  sImg:string=''; //Bildquelle
  sTrg:string=''; //gewählter Ergebnis-Name
  sKey,sVal:string; //linke, rechte Hälfte der Parameter-Zeile
  I:integer;
begin
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
      else if slCmd[I]=cfQuy then Reduce.QualityImage(sImg,sTrg) //Quality-Image für Eingangs-Stack
      else if iRtn<0 then Reduce.xHistory(sArt,slCmd[I],sImg,sTrg) //Zeitreihe erzeugen
      else if iRtn>0 then Reduce.xSplice(sArt,slCmd[I],sImg,sTrg) //Spektralbild erzeugen
      else Reduce.xReduce(sArt,slCmd[I],sImg,sTrg); //auf einen Kanal reduzieren ODER maskieren
{     TODO: Reduce.Execute sollte NoData aufheben können: "?" steht für NoData,
            der Wert danach für den Ersatz → B1?0 ersetzt alle Nodata-Werte in
            "B1" durch Null, → B1>0?0 setzt alle positiven Werte durch 1 und
            anschließend alle NoData-Werte durch Null }
    end;
  finally
    slCmd.Free;
  end;
end;

{ pCg erzeugt eine Liste aus Polygonen im WKT-Format die die Bildfläche aller
  Landsat-Archive im Verzeichnis "archives" enthält und speichert das Ergebnis
  als "target". pCg verwendet geographische Koordinaten (EPSG = 4326). }

function tParse.Catalog(iLin:integer; slPrc:tStringList):integer;
const
  cFmt = 'WKT,Integer64(10),String(250)';
  cKey = 'pCg: Undefined parameter under "catalog": ';
var
  sMsk:string=''; //Maske Archiv-Namen
  sTrg:string='boundingbox.csv'; //Dateiname für Datenbank
  sKey,sVal:string; //linke, rechte Hälfte der Parameter-Zeile
  I:integer;
begin
  for I:=succ(iLin) to pred(slPrc.Count) do
  begin
    Result:=I; //aktuelle Zeile
    if not GetParam(slPrc[I],sKey,sVal) then break; //Parameter, Wert
    if sKey='archives' then sMsk:=sVal else
    if sKey='target' then sTrg:=ChangeFileExt(sVal,'.csv') else
      Tools.ErrorOut(2,cKey+sKey) //nicht definierte Eingabe
  end;

  Tools.TextOut(sTrg,Archive.xCatalog(sMsk).Text); //Bounding-Box im CSV-Format?
  Tools.TextOut(ChangeFileExt(sTrg,'.csvt'),cFmt); //Spaltendefinition
  Tools.TextOut(ChangeFileExt(sTrg,'.prj'),ccPrj); //EPSG = 4326
end;

{ pIt extrahiert, bescheidet, maskiert und kalibriert Bilddaten aus Archiven
  und speichert die Ergebnisse im Arbeitsverzeichnis als Sensor_Kachel_Datum. }
{ Mit "database", "distance", "period" und "frame" können passende Archive
  gefiltert werden. Die "database" enthält ein Polygon der Bilddaten. pIt
  ignoriert Bilder außerhalb des mit "frame" übergebenen ROI und importiert nur
  Bilddaten innerhalb des "frame". "Period" [YYYYMMDD-YYYYMMDD] reduziert die
  selektierten Bilddaten auf eine beliebige Zeitperiode. Mit "select", "search"
  und "frame" können Archiv-Namen und Ausschnitte auch direkt gewählt werden. }
{ "Quality" [0..1] und "cover" [0..1] reduzieren gewählte oder gefilterte
  Auswahl auf Bilder, die mehr als "quality" klare Pixel enthalten und die vom
  gewählten "frame" mindestens den Anteil "cover" abdecken. }

function tParse.Import(iLin:integer; slPrc:tStringList):integer;
const
  cKey = 'pIp: Undefined parameter under "import": ';
  cTyp = 'pIp: Sensor-type not detected!';
var
  bMsk:boolean=True; //gestörte Pixel maskieren CONSTANT
  fFct:single=1.0; //Faktor für Kalibrierung CONSTANT
  fCpx:single=0.0; //gemessener Anteil klare Pixel
  fLmt:single=0.0; //Minimum fehlerfreie Pixel
  fOfs:single=0.0; //Offset für Kalibrierung CONSTANT
  rImg:trFrm; //Bounding Box der Bildkachel
  rRoi:trFrm; //Bounding Box des ROI
  rSct:trFrm; //Rahmen-Bild-verschnitt
  sArc:string=''; //Archiv Dateiname (ohne Koordinaten)
  sBnd:string=''; //Filter für Kanal-Namen, kommagetrennt
  sDat:string=''; //Datum (isoliert)
  sFrm:string=''; //Rahmen für Kachel-Ausschnitt
  sGrv:string=''; //Archiv-Zentren-Liste
  sImg:string=''; //Dateiname im Arbeitsverzeichnis
  sPrd:string=''; //Zeitperiodde für Auswahl
  sTyp:string=''; //Sensor für Import/Kalibrierung CONSTANT
  sWkt:string=''; //Polygon im WKT-Format
  slArc:tStringList=nil; //Archiv-Namen
  sKey,sVal:string; //linke, rechte Hälfte der Parameter-Zeile
  I:integer;
begin
  try
    slArc:=tStringList.Create;
    for I:=succ(iLin) to pred(slPrc.Count) do
    begin
      Result:=I; //gültige Zeile
      if not GetParam(slPrc[I],sKey,sVal) then break; //neuer Befehl
      if sKey='bands' then sBnd:=sVal else //Kanal-Namen-Masken, kommagetrennt
      if sKey='database' then sGrv:=sVal else //Position und Namen im WKT-Format
      if sKey='frame' then sFrm:=wDir(sVal) else //Rahmen für Beschnitt
      if sKey='factor' then fFct:=StrToFloat(sVal) else //Faktoren für Kanal-Kalibrierung
      if sKey='offset' then fOfs:=StrToFloat(sVal) else //Offset für Kalibrierung
      if sKey='period' then sPrd:=sVal else //Zeitperiode
      if sKey='quality' then fLmt:=StrToFloat(sVal) else //Maximum Fehler
      if sKey='select' then slArc.Add(sVal) else //direkt gewähltes Archiv
      if sKey='sensor' then sTyp:=LowerCase(sVal) else //Sensor-Typ
        Tools.ErrorOut(2,cKey+sKey);
    end;

    if length(sFrm)>0
      then rRoi:=Cover.VectorCrsFrame(4326,sFrm) //ROI-Box, geographisch
      else rRoi:=crFrm; //Vorgabe = endlos groß
    rSct:=crFrm; //Vorgabe = alles

{ TODO: [URGENT] archive auch manuell auswählen → Archive.ImportLandsat direkt aufrufen }

    if length(sGrv)>0 then //Archiv-Datenbank (nur Landsat)
    begin
      slArc.LoadFromFile(sGrv);
      //slArc.Count<1?
      for I:=1 to pred(slArc.Count) do //Archiv-Namen ohne Kopf-Zeile
      begin
        sArc:=copy(slArc[I],succ(rPos(',',slArc[I])),$FFF); //Dateiname am Ende
        sDat:=copy(ExtractFileName(sArc),18,8); //Datum im Dateinamen
        sWkt:=ExtractWord(1,slArc[I],['"']); //WKT-Polygon aus Archiv-Liste
        rImg:=Lines.LandsatFrame(sWkt); //Bounding-Box der Bildkachel, geographisch
        if Archive.QueryDate(sDat,sPrd) and //übergebenen Zeitraum filtern
           Archive.QueryPosition(rRoi,rImg) //nutzbare Bildfläche
        then fCpx:=Archive.QueryQuality(rImg,rRoi,rSct,sArc) //Anteil klare Pixel, rotierte Bounding-Box
        else fCpx:=0;
        if fCpx>=fLmt then
        begin
          sImg:=Archive.ImportLandsat(rSct,sArc,sBnd); //Bild importieren, neuer Name
          {Archive._ImportAster_(sArc:string):string;}
          {Archive._ImportRapidEye_(sArc:string):string;}
          Header.AddLine('quality',FloatToStr(fCpx),sImg); //Anteil klare Pixel im Header
        end;
      end;
    end;
  finally
    slArc.Free;
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

  if length(sPnt)>0 then Points.xPointAttrib(0,sFtr,sPnt) else
  if length(sGrd)>0 then Points.xGridAttrib(sGrd,sImg,'') else //Bildwerte auf Raster projizieren
  if bSts then Table._xImageStats(cSmp,sImg) else //statistische Kennwerte
  if bCrl then Table.xLayerCorrelate(sImg); //Korrelation Layer 1 mit allen anderen
  //if bCrl then Table._xTimeCorrelation_(sImg:string):tn2Sgl; IN ARBEIT
end;

{ pCe speichert alle übergebenen Bilder als Stack "compile" im Arbeits-
  Verzeichnis. Das Format ist immer 32Bit float. Die Dateinamen können direkt
  gewählt (select) oder mit Wildchars selektiert werden (search). Fehlt der
  Pfadname, sucht pCe im Arbeitsverzeichnis. Die Auswahl kann durch eine Zeit-
  Periode "period" weiter eingeschränkt werden. Dazu muss ein Datum [YYYYMMDD]
  am Ende des Dateinamens stehen. }
{ Wird pCe mit "pixel" oder "crsystem" aufgerufen, projiziert pCe alle Bilder
  beim Import in das neue Raster. Mit "bands" können einzelne Kanäle gewählt
  werden. Mit "nodata" kann ein eigener Wert für NoData gewählt werden. Der
  Vorgabe-Name im Arbeitsverzeichnis "compile" kann mit "target" geändert
  werden. }
{ Sind die übergebenen Bilder Teile eines größeren Bilds, sollte ein Rahmen
  "frame" für das Ergebnis angegeben werden. pCe erzeugt dann ein zusammen-
  hängendes Bild innerhalb des Rahmens. Dabei überschreibt pCe Bildern mit
  gleichem Datum (= gleicher Flugpfad) und erzeugt neue Ebenen für alle
  Anderen. Das Datum [YYYYMMDD] oder das Jahr [YYYY] muss dazu am Ende des
  Dateinamens stehen. pCe füllt leere Bereiche mit NoData. }
{ Sind die übergebenen Bilder deckungsgleich, stapelt pCe alle Bilder in der
  übergebenen Reihenfolge. Die Zahl der Kanäle pro Bild ist dabei beliebig. Mit
  "clip=true" füllt pCe alle Pixel außerhalb des Rahmens mit NoData. Damit
  können Formen wie Landesgrenzen aus den Bildern ausgeschnitten werden. }

function tParse.Compile(iLin:integer; slPrc:tStringList):integer;
const
  cDat = 'pCe: Date [YYYYMMDD] or year [YYYY] must be passed at the end of '+
    'each input filename!';
  cImg = 'pCe: Image names not defined or empty image list under "compile"';
  cKey = 'pCe: Undefined parameter under "compile": ';
var
  bClp:boolean=False; //Pixel außerhalb des Rahmens auf NoData setzen
  bFit:boolean=False; //Bilder justieren als Vorgabe
  bNme:boolean=False; //Dateinamen in Kanalnamen eintragen
  fNan:single=NaN; //NoData-Value
  iDat:integer=0; //Buchstaben für Dtum|Jahr am Ende des Dateinamens
  iEpg:integer=0; //EPSG-Code für Umprojektion
  iPix:integer=0; //Pixelgröße in Metern
  rFrm:trFrm; //Bounding Box des ROI
  slImg:tStringList=nil; //ausgewählte Bilder
  sFrm:string=''; //Ergebnis-Name optional
  sPrd:string=''; //Zeitperiode
  sRdz:string=''; //Kanal-Intervall auswählen
  sTrg:string=''; //Ergebnis-Name (Vorgabe)
  sKey,sVal:string; //linke, rechte Hälfte der Parameter-Zeile
  I:integer;
begin
  sTrg:=eeHme+cfCpl; //Vorgabe-Name
  try
    slImg:=tStringList.Create;
    for I:=succ(iLin) to pred(slPrc.Count) do
    begin
      Result:=I; //gültige Zeile
      if not GetParam(slPrc[I],sKey,sVal) then break; //neuer Befehl
      if sKey='bands' then sRdz:=sVal else //Kanal-Intervall wie "B1:B4"
      if sKey='clip' then bClp:=sVal='true' else //Pixeel am Rand auf NoData setzen
      if sKey='crsystem' then iEpg:=StrToInt(sVal) else //Ziel-Projektion
      if sKey='frame' then sFrm:=sVal else //Maske aus Polygon
      if sKey='names' then bNme:=sVal='true' else //Dateinamen in Kanalnamen eintragen
      if sKey='nodata' then fNan:=StrToFloat(sVal) else //NoData im Vorbild
      if sKey='pixel' then iPix:=StrToInt(sVal) else //Pixelgröße [m]
      if sKey='period' then sPrd:=sVal else //Zeitperiode selektieren
      if sKey='search' then slImg.AddStrings(Search(wDir(sVal))) else //Dateinamen-Filter
      if sKey='select' then slImg.Add(wDir(sVal)) else //einzelner Name, "eeHme" ist Vorgabe
      if sKey='target' then sTrg:=wDir(sVal) else //neuer Name AUF KONSTANTE GESETZT
        Tools.ErrorOut(2,cKey+sKey);
    end;

    if length(sPrd)>0 then MaskPeriod(slImg,sPrd); //Periode filtern
    for I:=pred(slImg.Count) downto 0 do
      if not FileExists(slImg[I]) then slImg.Delete(I); //nür gültige Namen
    if slImg.Count<1 then Tools.ErrorOut(3,cImg); //keine Aufgabe
    Archive.xTransform(iPix,iEpg,sRdz,slImg); //Projektion, Pixel, Kanäle, Arbeitsverzeichnis
    if bNme then Archive.BandNames(slImg); //Dateinamen als Kanalnamen
    if length(sFrm)>0 //Rahmen übergeben
      then rFrm:=Cover.VectorCrsFrame(iEpg,sFrm) //Rahmen aus Eingabe; bFit=False aus Vorgabe!
      else rFrm:=Cover.xCover(bFit,slImg); //Rahmen aus Überlagerung der Bilder, bFit = alle Rahmen gleich
    if (bFit=False) or (length(sFrm)>0) then //Bilder justieren
    begin
      iDat:=Reduce.SortDate(slImg); //sortieren nach Jahr oder Datum
      Archive.xCompile(iDat,rFrm,sTrg,slImg) //überlagern, stapeln, beschneiden
    end
    else Image.StackImages(slImg,sTrg); //Bilder stapeln
    if bClp then Cover.ClipToShape(sFrm,sTrg); //auf Frame zuschneiden
    if not isNan(fNan) then Filter.SetNodata(fNan,sTrg); //NoData anpassen
  finally
    slImg.Free;
  end;
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

procedure tParse.xChain(slPrc:tStringList);
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
      if sCmd='breaks' then iSkp:=_Breaks(I,slPrc) else //Brüche in Zeitreihen
      if sCmd='catalog' then iSkp:=Catalog(I,slPrc) else //Kachel-Mitten-Katalog
      if sCmd='compare' then iSkp:=Compare(I,slPrc) else //Referenz vergleichen
      if sCmd='compile' then iSkp:=Compile(I,slPrc) else //Stack aus Zwischenprodukten
      if sCmd='export' then iSkp:=Protect(I,slPrc) else //Export
      if sCmd='focus' then iSkp:=Focus(I,slPrc) else //focale Attribute NEU!
      if sCmd='features' then iSkp:=Features(I,slPrc) else //Zell-Attribute ergänzen
      if sCmd='home' then iSkp:=Home(I,slPrc) else //Arbeits-Verzeichnis
{     TODO: Das Arbeitsverzeichnis ".imalys" wird nicht immer erzeugt}
      if sCmd='import' then iSkp:=Import(I,slPrc) else //Extraktion aus Archiven
      if sCmd='kernel' then iSkp:=Kernel(I,slPrc) else //Kernel-Prozesse
      if sCmd='mapping' then iSkp:=Mapping(I,slPrc) else //Clusterer
      if sCmd='reduce' then iSkp:=Flatten(I,slPrc) else //Kanäle reduzieren + Indices
      if sCmd='replace' then iSkp:=Replace(I,slPrc) else //Variable im Hook ersetzen
      if sCmd='runoff' then iSkp:=Runoff(I,slPrc) else //synhetischer Abfluss
      if sCmd='zones' then iSkp:=Zones(I,slPrc) else //Clusterer
        Tools.ErrorOut(2,cCmd+sCmd);
    end;
    Tools.HintOut(true,'DONE');
  except
    on tChainError do Tools.ErrorOut(0,cSkp); //Nachricht, weiter mit nächster Prozesskette
    on tStepError do Tools.ErrorOut(0,cSkp);
  end;
end;

{ xLp startet und wiederholt die Prozesskette wenn Variable als kommagetrennte
  Liste eingetragen sind. }
{ Variable ($1..$9) können mit einem Wert verknüft sein oder mit einer Liste
  verschiedener Werte. Die Werte müssen durch Kommas getrennt werden. Ist
  mindestens eine Liste angegeben, wiederholt Imalys die Prozesskette für jeden
  Wert in der Liste. Ist mehr als eine Variable als Liste angegeben, werden
  alle Listen gleich behandelt. Vorgabe ist die erste Variable in der Liste. }
{ pLp prüft die Formatierung des Hooks, bestimmt die Zeilen mit Variablen-
  Definitionen, erzeugt eine Variablen Tabelle und ersetzt die Variablen unter
  "replace" wenn die Tabelle mehr als eine Spalte hat. "xChain" ersetzt mit
  "replace" die Variablen in allen anderen Befehlen. }
{ Wenn Werte aus verschiedenen Prozessen im Arbeitsverzeichnis gesammelt werden
  sollen, kann das Arbeitsverzeichnis mit dem Schalter "-c" vor der ersten
  Prozesskette geleert werden. }

procedure tParse.xLoop(sClr,sPrc:string); //Parameter, Prozesskette
const
  cFmt = 'pLp: Second line must contain a command. Found: ';
  cHme = 'pLp: The process chain needs a "home" section!';
  cIdf = 'pLp: Process chain must start with the key "IMALYS"';
  cSkp = 'pLp: APPLICATION TERMINATED!';
  cVar = 'pLp: Multiple variable lists with different word counts';
var
  bTbl:boolean=False; //Variablen-Tabelle gefunden
  iCol:integer; //aktuelle Spalte in Tabelle "sxVar"
  iHig:integer=1; //letzte Zeile mit Variablen
  iLow:integer=1; //erste Zeile mit Variablen
  iMax:integer=1; //Spalten in Variablen-Tabelle
  sLin:string; //aktuelle Zeile
  slPrc:tStringList=nil; //Prozesskette
  sxVar:tn2Str=nil; //Variable als Tabelle
  I,K:integer;
  qP:string;
begin
  try
    slPrc:=Tools.LoadClean(sPrc); //Prozesskette ohne Kommentare und Leerzeilen
    if pos('IMALYS',slPrc[0])<1 then Tools.ErrorOut(2,cIdf); //Identifier
    if pos('=',slPrc[1])>0 then Tools.ErrorOut(2,cFmt+slPrc[1]); //Parameter ohne Befehl
    for I:=1 to pred(slPrc.Count) do
    begin
      qP:=slPrc[I];
      if iHig>1 then break;
      if (iLow>1) and (pos(',',slPrc[I])>0) then
        bTbl:=True; //Variablen-Tabelle!
      if trim(slPrc[I])='home' then
        iHig:=pred(I) else //letzte Zeile Variable
      if trim(slPrc[I])='replace' then
        iLow:=succ(I); //erste Zeile Variable
    end;
    if iHig=1 then Tools.ErrorOut(1,cHme); //Programm beenden

    if bTbl then
    begin
      SetLength(sxVar,succ(iHig-iLow),1); //Vorgabe = ein Parameter
      for I:=iLow to iHig do
      begin
        sLin:=copy(slPrc[I],succ(pos('=',slPrc[I])),$FFF); //alles ab "=" Zeichen
        iCol:=WordCount(sLin,[',']); //Komma-getrennte Ausdrücke
        if iCol>1 then //neue Dimension
          if iMax=1 then
          begin
            SetLength(sxVar,succ(iHig-iLow),iCol);
            iMax:=iCol;
          end
          else if iCol<>iMax then Tools.ErrorOut(2,cVar);
        for K:=1 to iCol do
          sxVar[I-iLow,pred(K)]:=ExtractWord(K,sLin,[',']);
      end;

      if trim(sClr)='-c' then Tools.OsCommand('sh','rm -R '+eeHme+'*'); //Verzeichnis leeren
      for K:=0 to high(sxVar[0]) do //Spalten = Wiederholungen
      begin
        if K>0 then slPrc:=Tools.LoadClean(sPrc); //Prozesskette ohne Kommentare und Leerzeilen
        for I:=0 to high(sxVar) do //Zeilen = Variable
          if sxVar[I,K]<>''
            then slPrc[iLow+I]:=#9+'$'+IntToStr(succ(I))+'='+sxVar[I,K]
            else slPrc[iLow+I]:=#9+'$'+IntToStr(succ(I))+'='+sxVar[I,0];
        xChain(slPrc); //Prozess-Kette mit aktuellen Variablen
      end
    end
    else xChain(slPrc); //keine Tabelle
  except
    on tLoopError do Tools.ErrorOut(0,cSkp); //Nachricht: Programm Ende
    on tChainError do Tools.ErrorOut(0,cSkp);
    on tStepError do Tools.ErrorOut(0,cSkp);
  end;
end;

{ stabile Perioden in Zeitreihe suchen }

function tParse._Breaks(iLin:integer; slPrc:tStringList):integer;
const
  cKey = 'pBk: Undefined parameter under "break": ';
var
  fMax:single=0.5; //maximale Varianz
  sImg:string=''; //Vorbild
  sCmd:string=''; //Befehl
  sKey,sVal:string; //linke, rechte Hälfte der Parameter-Zeile
  I:integer;
begin
  for I:=succ(iLin) to pred(slPrc.Count) do
  begin
    Result:=I; //aktuelle Zeile
    if not GetParam(slPrc[I],sKey,sVal) then break; //Parameter, Wert
    if sKey='execute' then sCmd:=sVal else //Befehl
    if sKey='maximum' then fMax:=StrToFloat(sVal) else //maximale Varianz
    if sKey='select' then sImg:=wDir(sVal) else //Bilddaten (Zeitverlauf) für Statistik
      Tools.ErrorOut(2,cKey+sKey); //nicht definierte Eingabe
  end;

  if sCmd='nochange' then Model._xNoChange(fMax,sImg);
end;

end.

{==============================================================================}

// für tStringList.CustomSort, Datum "YYYYMMDD" am Ende des Namens

function EndDate(sl:tStringList; i1,i2:integer):integer;
begin
  if RightStr(ChangeFileExt(sl[i1],''),8) <
     RightStr(ChangeFileExt(sl[i2],''),8) then
     Result:=-1 else
  if RightStr(ChangeFileExt(sl[i1],''),8) >
     RightStr(ChangeFileExt(sl[i2],''),8) then
     Result:=1 else
  Result:=0
end;

{ pEF prüft, ob der Rahmen "rFrm" zum Bild "sImg" passt. pEF toleriert dabei
  Abweichungen bis zu 0.4% die durch Rundungsfehler entstehen können }

function tParse._EqualFrame_(
  const rFrm:trFrm; //Rahmen für alle Importe
  sImg:string): //erstes Bild in der Liste
  boolean; //Bildausschnitt = Rahmen
var
  rHdr:trHdr;
begin
  Header.Read(rHdr,sImg); //erster Header
  //Result:=(rHdr.Lon=rFrm.Lft) and (rHdr.Lon+rHdr.Scn*rHdr.Pix=rFrm.Rgt) and (rHdr.Lat=rFrm.Top) and (rHdr.Lat-rHdr.Lin*rHdr.Pix=rFrm.Btm);

  Result:=(round(rHdr.Lon*$FF)=round(rFrm.Lft*$FF))
    and (round((rHdr.Lon+rHdr.Scn*rHdr.Pix)*$FF)=round(rFrm.Rgt*$FF))
    and (round(rHdr.Lat*$FF)=round(rFrm.Top*$FF))
    and (round((rHdr.Lat-rHdr.Lin*rHdr.Pix)*$FF)=round(rFrm.Btm*$FF));
end;

function tParse._MergeValue(sKey:string):integer;
begin
  if sKey='date' then Result:=8 else
  if sKey='year' then Result:=4 else
  if sKey='flat' then Result:=1 else
    Result:=0;
end;

