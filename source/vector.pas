unit vector; //+miscellanous

{ VECTOR sammelt Routinen um Vektoren zu transformieren und zu attributieren.
  Polygone werden als "vector.csv" importiert, zeilenweise bearbeitet und die
  Ergebnis-Zeilen als "focus.csv" und "focus.csvt" gespeichert. Das WKT-Format
  kann anschließend in einem wählbaren Format exportiert werden. In- und Export
  erledigt "gdal.ogr2ogr".
  ==> https://gdal.org/drivers/vector/csv.html beschreibt das CSV-Frmat
  ==> https://gdal.org/ dokumentiert

  COVER:  transformiert und vereinigt Koordinaten und Rahmen
  POINTS: erzeugt, transformiert und attributiert Vektor-Punkte
  LINES:  erzeugt Abfluss Diagramme als Linien-Vektoren
  TABLE:  liest und schreibt Werte in einer CSV-Tabelle

  BEGRIFFE:}

{$mode objfpc}{$H+}

interface

uses
  Classes, Math, StrUtils, SysUtils, format;

type
  tCover = class(tObject) //checked 251116
    private
      function EnviCover(sImg:string):trCvr;
      function _EnviFrame_(sImg:string):trFrm;
      function EqualTileDat(sHig,sLow:string):boolean;
      function _FrameHeader_(rFrm:trFrm; sImg:string):trHdr;
      procedure GeoBonds(slInf:tStringList; iOfs:integer; var rRes:trFrm);
      procedure ImgBonds(slInf:tStringList; iOfs:integer; var rRes:trFrm);
      function _LandsatFrame_(sArc:string):tarGeo;
      function _PointInside_(rPnt:trGeo; rPly:tarGeo):boolean;
      function _RasterCover_(sImg:string):trCvr;
      function WordDelimiter(sNme:string):tnInt;
    public
      function CrsInfo(sImg:string):integer;
      function _RasterFrame(sImg:string):trFrm;
      function VectorFrame(sFrm:string):trFrm;
      function FrameWarp(sPly,sImg:string):trFrm;
      procedure xClipToShape(sFrm,sImg:string);
  end;

  tLines = class(tObject) //checked 251223
    private
      procedure _LocalMinima_(fxElv:tn2Sgl; ixIdx:tn2Int; const rHdr:trHdr);
    public
      function DrainWeight(iaLnk:tnInt; var rHdr:trHdr):tnSgl;
      procedure PointChain(sGrv:string);
      procedure RunoffPoints(faWgt:tnSgl; iaDrn,iaLnk:tnInt; var rHdr:trHdr);
  end;

  tPoints = class(tObject) //checked 250819
    private
      function AttribValues(sFtr:string):tn2Sgl;
      procedure DefaultFormat(iFtr:integer);
      procedure FieldTypes(sVct:string);
      procedure FormatAppend(iFtr:integer);
      function GetIndex(var rHdr:trHdr):tnInt;
      function PointAttributes(iaPix:tnInt; var rHdr:trHdr; sBnd:string):tnSgl;
      procedure _RandomPoints_(iCnt:integer; sImg:string);
      procedure PointAppend(fxVal:tn2Sgl; sFtr:string);
      procedure ValueAppend(fxVal:tn2Sgl; sFtr:string);
    public
      procedure xGridAttrib(sGrd,sImg,sTrg:string);
      procedure xPointAttrib(iCrs:integer; sFtr,sPnt:string);
      procedure xPolyAttrib;
  end;

  tTable = class(tObject)
    private
      procedure AddFormat(sFmt:string);
      procedure AddInteger(iaVal:tnInt; sFtr:string);
      function BandHist(var fSum:double; var iHit:integer; iBnd,iSmp:integer; sImg:string):tnSgl;
      function _FeatureHist_(var fSum:double; var iHit:integer; faFtr:tnSgl):tnSgl;
      function FieldValues(sFld:string):tStringList;
    public
      function AddIndex(sFld:string):tStringList;
      procedure QuickSort(faDev:tnSgl; iDim:integer);
      procedure VectorMask(sFld,sVct:string);
      function xImageStats(iSmp:integer; sImg:string):tStringList;
      function xLayerCorrelate(sImg:string):tStringList;
  end;

var
  Cover:tCover;
  Lines:tLines;
  Points:tPoints;
  Table:tTable;

implementation

uses
  index, mutual, raster, thema;

const
  FormatStrings : Array[1..1] of string = ('###.#####');

var
  ruFrq:trSlc=(Ofs:0; Sze:0);
  ruDat:trSlc=(Ofs:0; Sze:0);
  ruTle:trSlc=(Ofs:0; Sze:0);

{ ED sortiert eine String-Liste nach dem 8-stelligen Datum am Ende der Zeilen
  und der Kanal-ID. Sensor, Kachel- und Kanal-ID müssen durch Underscore
  getrennt im Kanal-Namen stehen }

function DateBand(sl:tStringList; i1,i2:integer):integer;
begin
  with ruFrq do
    if RightStr(sl[i1],8) < RightStr(sl[i2],8) then Result:=-1 else
    if RightStr(sl[i1],8) > RightStr(sl[i2],8) then Result:=1 else
      if copy(sl[i1],Ofs,Sze) < copy(sl[i2],Ofs,Sze) then Result:=-1 else
      if copy(sl[i1],Ofs,Sze) > copy(sl[i2],Ofs,Sze) then Result:=1 else
        Result:=0
end;

function TileDate(sl:tStringList; i1,i2:integer):integer;
begin
  with ruTle do
    if copy(sl[i1],Ofs,Sze) < copy(sl[i2],Ofs,Sze) then Result:=-1 else
    if copy(sl[i1],Ofs,Sze) > copy(sl[i2],Ofs,Sze) then Result:=1 else
      if RightStr(sl[i1],8) < RightStr(sl[i2],8) then Result:=-1 else
      if RightStr(sl[i1],8) > RightStr(sl[i2],8) then Result:=1 else
        Result:=0
end;

// Raster-Bild Statistik
// Min/Max für Extremwerte
// Hig/Low für 99%/1% Percentil
// Mid/Mea für mean/median

function tTable._FeatureHist_(
 var fSum:double; //Summe aller Stichproben
 var iHit:integer; //Anzahl Stichpropen → gültige Treffer
 faFtr:tnSgl): //Rohdaten (Attribut)
 tnSgl; //sortierte Stichproben
var
 fPrt:single; //Stichproben-Distanz
 iIdx:integer; //Zonen-ID
 Z:integer;
begin
 //iHit>0!
 Result:=Tools.InitSingle(min(iHit,high(faFtr)),0); //Stichproben als Liste
 fPrt:=length(faFtr)/length(Result); //Stichproben-Distanz
 fSum:=0; //Vorgabe
 iHit:=0; //Vorgabe
 for Z:=1 to high(Result) do //ohne Zone Null
 begin
   iIdx:=trunc(fPrt*Z); //regelmäßige Abstände
   if isNan(faFtr[iIdx]) then continue; //nur definierte Punkte
   Result[iHit]:=faFtr[iIdx]; //Wert am Stichpunkt
   fSum+=faFtr[iIdx]; //Summe aller Werte
   inc(iHit) //gültige Stichproben
 end;
 SetLength(Result,iHit); //NoData entfernen
 //Reduce.QuickSort_(Result,iHit);
 QuickSort(Result,iHit);
end;

{ cPI gibt True zurück, wenn der Punkt "pPnt" im Polygon "pPly" liegt. Dazu
  bestimmt cPI die Position der Schnittpunkte zwischen der horizontalen und
  vertikalen Koordinate des Fixpunkts mit allen Kanten eines Polygons. cPI
  zählt das Vorzeichen von horizontalen und vertikalen Differenzen. Ist das
  Ergebnis ungerade, liegt der Punkt außerhalb des Polygons. }

function tCover._PointInside_(
  rPnt:trGeo; //Punkt, unabhängig von "rPly"
  rPly:tarGeo): //geschlossenes Polygon
  boolean;
var
  fHrz,fVrt:double; //Schnittpunkt Lat/Lon
  fLft,fTop,fRgt,fBtm:double;
  iHrz:integer=0; //pos/neg Distanzen zu Schnittpunkt
  iVrt:integer=0; //pos/neg Distanzen zu Schnittpunkt
  p:integer;
begin
  //rPly[0].Lat=rPly[high(rPly)].Lat? //Polygon geschlossen?
  //rPly[0].Lon=rPly[high(rPly)].Lon?
  Result:=false;
  for P:=1 to high(rPly) do //alle Linien zwischen zwei Polygon-Punkten
  begin
    fBtm:=rPly[pred(P)].Lat; //Zeiger für Übersicht
    fTop:=rPly[P].Lat;
    fLft:=rPly[pred(P)].Lon;
    fRgt:=rPly[P].Lon;

    if (fTop<rPnt.Lat) and (fBtm>rPnt.Lat) //horizontal suchen
    or (fTop>rPnt.Lat) and (fBtm<rPnt.Lat) then
    begin
      fHrz:=fLft+(rPnt.Lat-fBtm)/(fTop-fBtm)*(fRgt-fLft);
      if fHrz>=rPnt.Lon
        then inc(iHrz)
        else dec(iHrz);
    end;

    if (fLft<rPnt.Lon) and (fRgt>rPnt.Lon) //vertikal suchen
    or (fLft>rPnt.Lon) and (fRgt<rPnt.Lon) then
    begin
      fVrt:=fBtm+(rPnt.Lon-fLft)/(fRgt-fLft)*(fTop-fBtm);
      if fVrt>=rPnt.Lat
        then inc(iVrt)
        else dec(iVrt);
    end;
  end;

  Result:=(odd(iHrz)=False) and (odd(iVrt)=False)
end;

{ cLF übernimmt die vier Bildecken aus den Landsat Metadaten und gibt sie als
  geschlossenes Polygon in UTM, WGS84 (geographisch) zurück }

function tCover._LandsatFrame_(sArc:string):tarGeo; //Archiv-Name: Eckpunkte ODER leer
const
  cArc = 'cLF: Archives must formatted as level 2 tier 1!';
  cGeo = 'cLF: Coordinates not found in "MTL" file!';
var
  bHit:boolean=False; //Koordinaten gefunden
  slInf:tStringList=nil;
  I:integer;
begin
  if RightStr(sArc,14)<>'_02_T1_MTL.txt' then Tools.ErrorOut(3,cArc);
  SetLength(Result,5); //geschlossenes Polygon
  try
    slInf:=tStringList.Create;
    slInf:=Archive.ExtractFilter(sArc,'_MTL'); //aus Archiv extrahieren
    slInf.LoadFromFile(slInf[0]); //MTL vollständig lesen
    for I:=0 to pred(slInf.Count) do
      if slInf[I]='  GROUP = PROJECTION_ATTRIBUTES' then
      begin
        if not (slInf[I+12]='    CORNER_UL_LAT_PRODUCT') then
          Tools.ErrorOut(3,cGeo);
        Result[0].Lat:=StrToFloat(copy(slInf[I+12],29,8));
        Result[0].Lon:=StrToFloat(copy(slInf[I+13],29,8));
        Result[1].Lat:=StrToFloat(copy(slInf[I+14],29,8));
        Result[1].Lon:=StrToFloat(copy(slInf[I+15],29,8));
        Result[3].Lat:=StrToFloat(copy(slInf[I+16],29,8));
        Result[3].Lon:=StrToFloat(copy(slInf[I+17],29,8));
        Result[2].Lat:=StrToFloat(copy(slInf[I+18],29,8));
        Result[2].Lon:=StrToFloat(copy(slInf[I+19],29,8));
        Result[4].Lat:=Result[0].Lat; //Polygon schließen
        Result[4].Lon:=Result[0].Lon;
        bHit:=True;
        break; //Aufgabe abgeschlossen
      end;
  finally
    slInf.Free;
  end;
  if not bHit then SetLength(Result,0); //nil zurückgeben
end;

{ cFH überträgt die Koordinaten aus einem Frame in einen ENVI-Header.
  ==> CFH ÜBERPRÜFT NICHT DAS CRS }

function tCover._FrameHeader_(
  rFrm:trFrm; //Rahmen (aus Polygon)
  sImg:string): //Vorbild (Teil)
  trHdr; //Metadaten innerhalb des Frames
var
  sMap:string; //ENVI Map-Info
  I:integer;
begin
  Header.Read(Result,sImg);
  with Result do
  begin
    Lat:=trunc(rFrm.Top/Pix)*Pix; //nach links runden
    Lon:=trunc(rFrm.Lft/Pix)*Pix+Pix; //nach oben runden
    sMap:=ExtractWord(1,Map,[',']);
    for I:=2 to 3 do
      sMap+=', '+ExtractWord(I,Map,[',']);
    sMap+=', '+FloatToStr(Lon);
    sMap+=', '+FloatToStr(Lat);
    for I:=6 to 10 do
      sMap+=', '+ExtractWord(I,Map,[',']);
    Map:=sMap;
    {UTM, 1, 1, 594810, 5903730, 30, 30, 32, North,WGS-84}
    Lin:=round((rFrm.Top-rFrm.Btm)/Pix);
    Scn:=round((rFrm.Rgt-rFrm.Lft)/Pix);
    Prd:=Stk;
    Stk:=0; //neue Datei erzeugen
  end;
  //Header.Write(Result,'compare',eeHme+'temp.hdr'); KONTROLLE
end;

{ fPA gibt die Werte einzelner Pixel aus dem Kanal "sBnd" zurück. Die Position
  der Pixel muss als Pixel-Index in "iaPix" übergeben werden. }

function tPoints.PointAttributes(
  iaPix:tnInt; //Indices ausgewählter Pixel
  var rHdr:trHdr; //Metadaten
  sBnd:string): //Bild-Name
  tnSgl; //Werte der ausgewählten Pixel
const
  cPix = 'fPA: List of pixel indices must be provided!';
var
  fxBnd:tn2Sgl=nil; //aktueller Kanal
  iCnt:integer; //Anzahl Pixel
  I:integer;
begin
  if iaPix=nil then Tools.ErrorOut(3,cPix);
  Result:=Tools.InitSingle(length(iaPix),dWord(Nan)); //Vorgabe
  iCnt:=rHdr.Scn*rHdr.Lin; //Anzahl Pixel
  fxBnd:=Image.ReadBand(0,rHdr,sBnd); //Bildkanal
  for I:=0 to high(iaPix) do
    if (iaPix[I]>=0) and (iaPix[I]<iCnt) then
      with rHdr do
        Result[I]:=fxBnd[iaPix[I] div Scn,iaPix[I] mod Scn];
end;

function _PointAttributes(
  iaPix:tnInt; //Indices ausgewählter Pixel
  var rHdr:trHdr; //Metadaten
  sBnd:string): //Bild-Name
  tnSgl; //Werte der ausgewählten Pixel
{ fPA gibt die Werte einzelner Pixel aus dem Kanal "sBnd" zurück. Die Position
  der Pixel wird als Pixel-Indices in "iaPix" übergeben. }
var
  fxBnd:tn2Sgl=nil; //Kanal
  I:integer;
begin
  fxBnd:=Image.ReadBand(0,rHdr,sBnd); //Bildkanal
  Result:=Tools.InitSingle(length(iaPix),dWord(Nan)); //Vorgabe
  for I:=0 to high(iaPix) do
    with rHdr do
      Result[I]:=fxBnd[iaPix[I] div Scn,iaPix[I] mod Scn];
end;

{ fGI liest projizierte Koordinaten von Vektor-Punkten im WKT-Format aus einer
  CSV Datei und transformiert sie in Pixel-Indices. Für Punkte außerhalb der
  Bildfläche gibt fGI (-1) zurück. Der Pixel-Ursprung ist links oben. }

function tPoints.GetIndex(
  var rHdr:trHdr): //Metadaten Vorbild
  tnInt; //Pixel-Indices[Vektor-Punkt]
const
  cCsv = 'Impossible to read file: ';
  cPnt = 'Not a WKT point format: ';
  cWkt = 'Geometry must be WKT formatted: ';
var
  dCsv:TextFile; //Datei
  fLat,fLon:single; //aktuelle Koordinaten (Ursprung links unten)
  iHrz,iVrt:integer; //Pixel-Koordinaten (Ursprung links oben)
  nRes:integer=0; //Dimension Pixelindices
  sLin:string; //aktuelle Zeile
begin
  SetLength(Result,$FF); //nicht definiert
  try
    AssignFile(dCsv,eeHme+cfVct);
    {$i-} Reset(dCsv); {$i+}
    if IOResult<>0 then Tools.ErrorOut(3,cCsv+eeHme+cfVct);
    readln(dCsv,sLin); //Zeile mit Feldnamen
    if ExtractDelimited(1,sLin,[','])<>'WKT' then
      Tools.ErrorOut(3,cCsv+eeHme+cWkt);
    repeat
      readln(dCsv,sLin); //Zeile mit Koordinaten (und Attributen)
      sLin:=ExtractDelimited(1,sLin,[',']); //WKT-Geometrie
      if (pos('POINT Z',sLin)>0) or (pos('POINT',sLin)>0) then //nur Punkte
      begin
        sLin:=ExtractDelimited(2,sLin,['(',')']); //Substring in Klammern
        fLon:=StrToFloat(ExtractDelimited(1,sLin,[#32])); //1. Substring in Blankes
        fLat:=StrToFloat(ExtractDelimited(2,sLin,[#32])); //2. Substring in Blankes
        iHrz:=trunc((fLon-rHdr.Lon)/rHdr.Pix); //Pixel-Koordinaten
        iVrt:=trunc((rHdr.Lat-fLat)/rHdr.Pix);
        if (iHrz<0) or (iHrz>pred(rHdr.Scn))
        or (iVrt<0) or (iVrt>pred(rHdr.Lin))
          then Result[nRes]:=-1 //Pixel außerhhalb der Bildfläche
          else Result[nRes]:=iVrt*rHdr.Scn+iHrz; //Pixel-Index
        inc(nRes); //fortlaufend zählen
        if nRes>=length(Result) then
          SetLength(Result,nRes*2);
      end
      else Tools.ErrorOut(3,cPnt+eeHme+cfVct);
    until eof(dCsv);
    SetLength(Result,nRes);
  finally
    CloseFile(dCsv);
  end; //of try ..
end;

procedure tPoints.FieldTypes(sVct:string); //Geometrie im CSV-Format
{ pFT erzeugt eine CSVT-Datei mit Formatangaben für die Attribute der Vektor-
  Datei "sVct". pFT übernimmt aus der CSV-Datei die Feldnamen (erste Zeile) des
  Originals und verknüpft sie mit den Format-Angaben aus dem "ogrinfo"-Prozess
  des Originals. }
const
  cWkt = 'CSV vector geometry must use WKT format! ';
var
  iBrk:integer; //Grenze Feldname-Dimension
  iFld:integer; //Anzahl felder ohne "WKT"
  slIfo:tStringList=nil; //Container für OgrInfo
  sFld:string; //Feldname mit Dimension
  sTyp:string=''; //Format-String
  I:integer;
begin
  sFld:=Tools.LineRead(eeHme+cfVct); //erste Zeile der CSV-Version
  if LeftStr(sFld,3)<>'WKT' then Tools.ErrorOut(3,cWkt+eeHme+cfVct);
  iFld:=pred(WordCount(sFld,[','])); //Anzahl Felder ohne "WKT"
  try
    slIfo:=tStringList.Create;
    slIfo.AddText(Gdal.OgrInfo(sVct)); //Info als Stream
    for I:=pred(slIfo.Count) downto slIfo.Count-iFld do //nur Feldnamen
    begin
      iBrk:=pos(': ',slIfo[I])+2; //Grenze Name: Format
      if iBrk>2
        then sFld:=copy(slIfo[I],iBrk,$FF) //nur Format
        else sFld:=''; //Vorgabe = String
      sTyp:=','+sFld+sTyp;
    end;
  finally
    slIfo.Free;
  end;
end;

procedure tPoints._RandomPoints_(
  iCnt:integer; //Anzahl zufälliger Punkte
  sImg:string); //Bilddaten für Bounding Box
{ pRP erzeugt "iCnt" Vektor-Punkte im CSV-Format, die zufällig über die
  Bounding-Box des Bilds "sImg" verteilt sind. Die Koordinaten werden im WKT-
  Format gespeichert, einziges Attribut ist eine fortlaufende ID. }
const
  cCrt = 'pRP: Error while creating: ';
  cFmt = 'WKT,Integer(12)';
var
  dVct:TextFile; //Vektor-Punkte im CSV-Format
  fLat,fLon:double; //zufällige Koordinaten
  iHrz,iVrt:integer; //Bildgröße in Metern
  rHdr:trHdr; //Metadaten
  I:integer;
begin
  Header.Read(rHdr,sImg);
  iHrz:=trunc(rHdr.Scn*rHdr.Pix); //Bildbreite in Metern
  iVrt:=trunc(rHdr.Lin*rHdr.Pix); //Bildhöhe in Metern
  try
    AssignFile(dVct,eeHme+cfVct); //Punkte mit ergänzten Attributen
    {$i-} Rewrite(dVct); {$i+}
    if IOResult<>0 then Tools.ErrorOut(3,cCrt+eeHme+cfVct);
    writeln(dVct,'WKT,ID'); //Feldnamen
    Randomize;
    for I:=1 to iCnt do
    begin
      fLat:=rHdr.Lat-random(iVrt);
      fLon:=rHdr.Lon+random(iHrz);
      writeln(dVct,'"POINT ('+FloatToStr(fLon)+#32+FloatToStr(fLat)+')",'+
        IntToStr(I));
    end;
  finally
    Flush(dVct);
    CloseFile(dVct);
  end; //of try ..
  Tools.TextOut(eeHme+ChangeFileExt(cfVct,'.csvt'),cFmt) //Format der Felder
end;

function tPoints.AttribValues(sFtr:string):tn2Sgl; {lokale Bilder: Attribut-Tabelle}
{ pAV erzeugt eine Tabelle mit dem Wert der Bildpixel am Ort der Punkt-Vektoren
  "vector.csv". Die Bilddaten aus der Liste "sFtr" müssen im Imalys-Verzeichnis
  stehen. Die Punkt-Vektoren müssen im WKT-Format in "vector.csv" stehen. Bild-
  und Vektordaten müssen dieselbe Projektion haben. Die Geometrie der Bilddaten
  muss identisch sein. }
{ pAV konvertiert die Koordinaten aus "vector.csv" in einen Pixelindex. Der
  Index basiert auf der Bounding-Box des ersten Bilds in "sFtr". pAV ignoriert
  Vektor-Punkte außerhalb der Bounding-Box. }
var
  iaPix:tnInt=nil; //Pixelindices
  rHdr:trHdr; //Metadaten
  slFtr:tStringList=nil; //Dateinamen-Liste
  I:integer;
begin
// gleiches CRS bei allen Bildern und cfVct? ← EPSG überprüfen
  try
    slFtr:=tStringList.Create;
    slFtr.AddText(Tools.CommaToLines(sFtr)); //ausgewählte lokale Bilder
    Header.Read(rHdr,eeHme+slFtr[0]); //Metadaten aus erstem Bild
    iaPix:=GetIndex(rHdr); //Pixelindices aus Vektor-Punkten
    //PixMask(iaPix,slFtr[0]); //NUR KONTROLLE DER LAGE
    SetLength(Result,slFtr.Count,1); //leere Attribut-Liste
    for I:=0 to pred(slFtr.Count) do //alle gewählten Prozesse
      Result[I]:=PointAttributes(iaPix,rHdr,eeHme+slFtr[I]); //Werte am Ort der Punkte
  finally
    slFtr.Free;
  end;
end;

{ fVA liest die Vorlage "vector.csv", ergänzt Attribute und speichert das
  Ergebnis als "focus.csv". fVA liest und schreibt einzelne Zeilen im Text-
  Format. fVA übernimmt die Feldnamen aus "sFtr" und die Werte der Tabelle aus
  "fxVal". Feldnamen und Werte müssen korresponieren. fVA schreibt alle Werte
  im Float-Format und markiert nicht definierte Werte als "NA". }

procedure tPoints.PointAppend(
  fxVal:tn2Sgl; //Werte[Kanal,Punkt] (aus Vorbild)
  sFtr:string); //Feldnamen = Prozess-Namen als CSV
const
  cCsv = 'fVA: File not available: ';
  cFcs = 'fVA: File creation failed: ';
  cFld = 'fVA: Number of field names differ from field values!';
  cRcd = 'fVA: Number of vector records differ from extraced values!';
var
  dCsv:TextFile; //Vektor-Import im CSV-Format
  dFcs:TextFile; //ergänzte Attribute im CSV-Format
  iRcd:integer=0; //Anzahl Punkte=Zeilen
  sLin:string; //Zeilen-Puffer
  I:integer;
begin
  if WordCount(sFtr,[','])<>length(fxVal) then
    Tools.ErrorOut(3,cFld);
  //length(slPrc)=length(fxVal)?
  //Anzahl Zeilen <> Länge Attribut-Arrays
  try
    AssignFile(dCsv,eeHme+cfVct); //Test-Punkte als CSV
    {$i-} Reset(dCsv); {$i+}
    if IOResult<>0 then Tools.ErrorOut(3,cCsv+eeHme+cfVct);

    AssignFile(dFcs,eeHme+cfFcs); //Punkte mit ergänzten Attributen
    {$i-} Rewrite(dFcs); {$i+}
    if IOResult<>0 then Tools.ErrorOut(3,cFcs+eeHme+cfFcs);

    readln(dCsv,sLin); //bestehene Feldnamen
    writeln(dFcs,sLin+','+DelSpace(sFtr)); //neuer Header
    repeat
      readln(dCsv,sLin); //bestehene Werte
      for I:=0 to high(fxVal) do //alle neuen Felder
        if not IsNan(fxVal[I,iRcd])
          then sLin+=','+FloatToStr(fxVal[I,iRcd])
          else sLin+=',NA';
      inc(iRcd); //neue Zeile
      writeln(dFcs,sLin); //Zeile speichern
    until eof(dCsv);
    if length(fxVal[0])<>iRcd then Tools.ErrorOut(3,cRcd);
  finally
    CloseFile(dCsv);
    Flush(dFcs);
    CloseFile(dFcs);
  end; //of try ..
end;

procedure tPoints.FormatAppend(iFtr:integer);
{ pFA erweitert die CSVT-Datei "vector.csvt" um "iFtr" neue Felder im Float-
  Format und speichert das Ergebnis als "focus.csvt". }
const
  cFlt = ',Real(24.15)';
var
  sFmt:string;
  I:integer;
begin
  sFmt:=Tools.LineRead(eeHme+ChangeFileExt(cfVct,'.csvt'));
  for I:=0 to pred(iFtr) do
    sFmt+=cFlt; //Single-Format-Code anhängen
  Tools.TextOut(eeHme+ChangeFileExt(cfFcs,'.csvt'),sFmt);
end;

{ pAt überträgt Werte von Bildpixeln in die Attribut-Tabelle von Punkt-
  Vektoren. Dazu importiert pAt die Punkte als "vector.csv", transformiert sie
  in die Projektion "iCrs", ergänzt die Werte der Attribute für alle Records,
  speichert das Ergebnis als "focus.csv" und transformiert die CSV-Datei in das
  Shape-Formet. "sFtr" muss gültige Dateinamen aus dem Imalys-Verzeichnis
  enthalten. "Raster- und CSV-Vektoren müssen dieselbe Projektion haben. }

procedure tPoints.xPointAttrib(
  iCrs:integer; //Projektion der Bilddaten
  sFtr:string; //Merkmale = Dateinamen in kommagetrennter Liste
  sPnt:string); //Vorlage Punkt-Vektoren
const
  cGeo = 'fE: Observation points file (vector) not available! ';
var
  fxVal:tn2Sgl=nil; //Attribute[Kanal][Punkt]
begin
  //alle feature-Dateien gleich groß?
  Gdal.ImportVect(iCrs,sPnt); //Punkt-Vektoren als "vector.csv"
  FieldTypes(sPnt); //CSVT-Datei mit Feldtypen aus gdal-Info
  if not FileExists(eeHme+cfVct) then
    Tools.ErrorOut(3,cGeo+eeHme+cfVct);
  fxVal:=AttribValues(sFtr); //Attribute an gewählten Punkten
  PointAppend(fxVal,sFtr); //Feldnamen + Attribute im CSV erweitern
  FormatAppend(high(fxVal)); //Formatangaben als CSVT erweitern
  //Gdal.ExportShape(False,iCrs,eeHme+cfFcs+'.csv',sTrg); //als ESRI-Shape speichern
  Gdal.ExportShape(iCrs,0,eeHme+cfFcs,eeHme+ChangeFileExt(cfFcs,'.shp')); //als ESRI-Shape speichern
  Tools.HintOut(true,'PointAttrib: '+cfFcs)
end;

{ pVA liest die Vorlage "vector.csv", ergänzt numerische Attribute und
  speichert das Ergebnis als "focus.csv". pVA liest und schreibt einzelne
  Zeilen im Textformat. pVA übernimmt die Feldnamen aus "sFtr" und die Werte
  der Tabelle aus "fxVal". Feldnamen und Werte müssen korresponieren. pVA
  schreibt alle Werte im Float-Format und markiert nicht definierte Werte als
  "NA". }

procedure tPoints.ValueAppend(
  fxVal:tn2Sgl; //Werte[Kanal,Punkt] (aus Vorbild)
  sFtr:string); //Feldnamen = Prozess-Namen als CSV
const
  cAtr = 'pVA: Geometry import must contain only a WKT and a DN field!';
  cCsv = 'pVA: File not available: ';
  cFcs = 'pVA: File creation failed: ';
  cRcd = 'pVA: Record-ID not provided by index attributes: ';
var
  dCsv:TextFile; //Vektor-Import im CSV-Format
  dFcs:TextFile; //ergänzte Attribute im CSV-Format
  iRcd:integer; //Anzahl Punkte=Zeilen
  sLin:string; //Zeilen-Puffer
  I:integer;
begin
  try
    AssignFile(dCsv,eeHme+cfVct); //bestehende Geometrie als CSV
    {$i-} Reset(dCsv); {$i+}
    if IOResult<>0 then Tools.ErrorOut(3,cCsv+eeHme+cfVct);

    AssignFile(dFcs,eeHme+cfFcs); //Geometrie mit Attributen
    {$i-} Rewrite(dFcs); {$i+}
    if IOResult<>0 then Tools.ErrorOut(3,cFcs+eeHme+cfFcs);

    readln(dCsv,sLin); //bestehene Feldnamen
    if sLin<>'WKT,DN' then Tools.ErrorOut(3,cAtr);
    writeln(dFcs,'WKT,DN,'+DelSpace(sFtr)); //erweiterte Feldnamen
    repeat
      readln(dCsv,sLin); //bestehene Werte
      iRcd:=rPos(',',sLin); //letztes Komma
      iRcd:=StrToInt(copy(sLin,iRcd+2,length(sLin)-iRcd-2)); //Record-ID
      if iRcd>high(fxVal[0]) then Tools.ErrorOut(3,cRcd+IntToStr(iRcd));
      for I:=0 to high(fxVal) do //alle Attribute
        if not IsNan(fxVal[I,iRcd])
          then sLin+=','+FloatToStr(fxVal[I,iRcd])
          else sLin+=',NA';
      writeln(dFcs,sLin); //Zeile speichern
    until eof(dCsv);
  finally
    CloseFile(dCsv);
    Flush(dFcs);
    CloseFile(dFcs);
  end; //of try ..
end;

{ pDF erzeugt eine CSVT-Datei für eine WKT-Geometrie mit Attributen. Die "DN"
  für die Datensätze ist "integer", alle Attribute sind "real". pDF speichert
  das Ergebnis als "focus.csvt". }

procedure tPoints.DefaultFormat(iFtr:integer);
const
  cFlt = ',Real(24.15)';
  cInt = ',Integer(9.0)';
var
  sFmt:string;
  I:integer;
begin
  sFmt:='WKT'+cInt; //Geometrie und "DN"
  for I:=0 to pred(iFtr) do
    sFmt+=cFlt; //Single-Format für alle Attribute
  Tools.TextOut(eeHme+ChangeFileExt(cfFcs,'.csvt'),sFmt);
end;

{ pPA überträgt die Attribut-Tabelle "index.bit" auf die Geometrie "vector.csv"
  und speichert das Ergebnis als "focus.csv". "vector.csv" muss existieren und
  darf keine Attribute enthalten. pPn läd die Polygone einzeln als Textzeile,
  ergänzt alle Werte aus der Attribut-Tabelle und schreibt die erweiterten
  Zeilen nach "focus.csv". Zum Schluss erzeugt pPn eine angepasste "focus.csvt"
  Datei. qGis kann "focus.csv" direkt lesen. }

procedure tPoints.xPolyAttrib;
const
  cVal = 'pPA: Number of fields at index table differs from field names!';
  cVct = 'pPA: Geometry file not found: ';
var
  fxVal:tn2Sgl=nil; //Zell-Attribute als Maxtrix
  sFtr:string=''; //Liste mit Feldnamen aus Index-Header
begin
  if not FileExists(eeHme+cfVct) then Tools.ErrorOut(3,cVct+eeHme+cfVct);
  sFtr:=Header.ReadLine(true,'field names =',eeHme+cfIdx); //Liste mit Feldnamen (CSV)
  fxVal:=Tools.BitRead(eeHme+cfIdx); //aktuelle Zellindex-Attribute als Matrix
  if WordCount(sFtr,[','])<>length(fxVal) then Tools.ErrorOut(3,cVal);
  ValueAppend(fxVal,sFtr); //"vector.csv" mit Attributen als "focus.csv" speichern
  DefaultFormat(length(fxVal)); //CSVT-Datei für Attribute
  Tools.HintOut(true,'PolyAttrib: '+cfFcs);
end;

{ tFV gibt alle Werte aus dem Feld "sFld" in der Tabelle von "vector.csv" als
  String-Liste zurück. tFV unterstellt, dass "vector.csv" Polygone im WKT-
  Format enthält. }

function tTable.FieldValues(sFld:string):tStringList;
const
  cCsv = 'Impossible to read file: ';
  cFld = 'Field name not provided: ';
  cWkt = 'Geometry must be WKT formatted: ';
var
  dCsv:TextFile; //Datei
  iCol:integer=0; //Spalte für gesuchtes Feld
  iWkt:integer; //Ende Polygon-Teil
  sLin:string; //aktuelle Zeile
  I:integer;
begin
  Result:=tStringList.Create;
  try
    AssignFile(dCsv,eeHme+cfVct);
    {$i-} Reset(dCsv); {$i+}
    if IOResult<>0 then Tools.ErrorOut(3,cCsv+eeHme+cfVct);
    readln(dCsv,sLin); //Zeile mit Feldnamen
    if ExtractDelimited(1,sLin,[','])<>'WKT' then
      Tools.ErrorOut(3,cCsv+eeHme+cWkt);
    for I:=2 to WordCount(sLin,[',']) do
      if ExtractDelimited(I,sLin,[','])=sFld then iCol:=pred(I); //Spalte mit Feldnamen ohne "WKT"
    if iCol<1 then Tools.ErrorOut(3,cFld+sFld);
    repeat
      readln(dCsv,sLin); //ab zweite Zeile = Inhalte
      iWkt:=PosEx('"',sLin,2); //Position zweites Doppelhochkomma
      Delete(sLin,1,succ(iWkt)); //Polygon-Teil + Komma entfernen
      sLin:=trim(ExtractDelimited(iCol,sLin,[','])); //Wort im Abschnitt "iCol"
      for I:=length(sLin) downto 1 do
        if sLin[I]='"' then delete(sLin,I,1); //Doppelhochkommata entfernen
      Result.Add(sLin);
    until eof(dCsv);
  finally
    CloseFile(dCsv);
  end;
end;

{ tAI erweitert die Tabelle aus "vector.csv" um das Integer-Feld "sFtr"-ID.
  Dazu muss die Tabelle als "focus.csv" neu geschrieben werden. tAI unterstellt
  dass "vector.csv" Polygone im WKT-Format enthält. }

procedure tTable.AddInteger(
  iaVal:tnInt; //Werte (Liste) Index wie Vektoren
  sFtr:string); //neue Feldnamen, kommagetrennt
const
  cAtr = 'pVA: Geometry import must contain a WKT field!';
  cCsv = 'pVA: File not available: ';
  cFcs = 'pVA: File creation failed: ';
var
  dCsv:TextFile; //Vektor-Import im CSV-Format
  dFcs:TextFile; //ergänzte Attribute im CSV-Format
  iCnt:integer=0; //Zeilen-ID (ab Null)
  sLin:string; //Zeilen-Puffer
begin
  try
    AssignFile(dCsv,eeHme+cfVct); //bestehende Geometrie als CSV
    {$i-} Reset(dCsv); {$i+}
    if IOResult<>0 then Tools.ErrorOut(3,cCsv+eeHme+cfVct);

    AssignFile(dFcs,eeHme+cfFcs); //Geometrie mit Attributen
    {$i-} Rewrite(dFcs); {$i+}
    if IOResult<>0 then Tools.ErrorOut(3,cFcs+eeHme+cfFcs);

    readln(dCsv,sLin); //bestehene Feldnamen
    if LeftStr(sLin,3)<>'WKT' then Tools.ErrorOut(3,cAtr);
    if sFtr[1]<>',' then sFtr:=','+sFtr; //führendes Komma
    writeln(dFcs,sLin+DelSpace(sFtr)); //erweiterte Feldnamen
    repeat
      readln(dCsv,sLin); //bestehene Werte
      //if not IsNan(fxVal[I,iRcd]) .. sLin+='NA'
      sLin+=','+FloatToStr(iaVal[iCnt]); //Wert ergänzen
      writeln(dFcs,sLin); //Zeile speichern
      inc(iCnt) //Zeilen-Index
    until eof(dCsv);
  finally
    CloseFile(dCsv);
    Flush(dFcs);
    CloseFile(dFcs);
  end; //of try ..
  Tools.HintOut(true,'AddInteger: '+cfFcs);
end;

procedure tTable.AddFormat(sFmt:string);
{ tAF erweitert die CSVT-Tabelle um den Eintrag "sFmt". "vector.csvt" muss
  existieren. tAF schreibt nach "focus.csvt". }
const
  cVct = 'Vector format definition vector.CSVT needed!';
begin
  if not FileExists(ChangeFileExt(eeHme+cfVct,'.csvt')) then
    Tools.ErrorOut(3,cVct+ChangeFileExt(eeHme+cfVct,'.csvt'));
  if sFmt[1]<>',' then sFmt:=','+sFmt;
  sFmt:=Tools.LineRead(ChangeFileExt(eeHme+cfVct,'.csvt'))+sFmt;
  Tools.TextOut(eeHme+ChangeFileExt(cfFcs,'.csvt'),sFmt);
end;

{ tAI erzeugt aus dem Feld "sFld" in "vector.csv" ein Array mit Klassen-IDs für
  die Inhalte von "sFld", ergänzt damit die Tabelle und speichert das Ergebnis
  als "focus.csv". }
{ tAI betrachtet die Inhalte von "sFld" als Strings und vergibt für jedes
  Muster eine Klassen-ID. Dazu kopiert tAI die ursprüngliche Liste, sortiert
  sie und reduziert gleiche Einträge bis von jedem Muster nur noch ein Beispiel
  übrig bleibt. tAI verwendet den Index der reduzierten Liste als Klassen-ID,
  trägt die IDs in ein neues Integer-Array ein und ergänzt das Array als neues
  Feld für "focus.csv". tAI erweitert auch die CSVT-Datei und übernimmt die
  PRJ-Datei aus "vector.prj" }

function tTable.AddIndex(sFld:string):tStringList;
var
  iaMap:tnInt=nil; //Klassen-IDs
  slFld:tStringList=nil; //Klassen-Bezeichner, alle Polygone
  I:integer;
  qS:string;
begin
  Result:=tStringList.Create; //klassifizierte Bezeichner
  try
    slFld:=FieldValues(sFld); //Klassen-Bezeichner, alle Polygone
    qS:=slFld.CommaText; //CONTROL
    Result.AddStrings(slFld); //Liste kopieren
    Result.Sort; //alphabetisch
    for I:=pred(Result.Count) downto 1 do
      if Result[I]=Result[pred(I)] then Result.Delete(I); //nur verschiedene Bezeichner
    Result.Insert(0,'–'); //leere Klasse für Rückweisung, ID=0
    iaMap:=Tools.InitInteger(slFld.Count,0); //Klassen-IDs als Array
    for I:=0 to pred(slFld.Count) do
      iaMap[I]:=Result.IndexOf(slFld[I]); //Nummer des Bezeichners = Klassen-ID
    Table.AddInteger(iaMap,cfMsk); //Klassen-IDs als "mask" an Tabelle anhängen
    Table.AddFormat('Integer(10)'); //Feld-Format an CSVT anhängen
    Tools.CopyFile(ChangeFileExt(eeHme+cfVct,'.prj'),
      eeHme+ChangeFileExt(cfFcs,'.prj')); //Projektion als WKS
    //Result.Count darf $FF nicht überschreiten
  finally
    slFld.Free;
  end;
end;

{ cCI übernimmt den EPSG-Code als Integer aus dem Text von "gdalsrsinfo" }

function tCover.CrsInfo(
  sImg:string): //Bidname
  integer; //EPSG-Code ODER Null
const
  cCrs = 'cCI: No CRS code found in: ';
var
  slInf:tStringList=nil;
  I:integer;
begin
  Result:=0; //Vorgabe = undefiniert
  try
    slInf:=TStringList.Create;
    slInf.AddText(Gdal.SrsInfo(sImg)); //GDAL-Info übernehmen
    for I:=0 to pred(slInf.Count) do
      if LeftStr(slInf[I],4)='EPSG' then
      begin
        Result:=StrToInt(copy(slInf[I],6,length(slInf[I])-5));
        break
      end;
  finally
    slInf.Free;
  end;
end;

{ cVF gibt das Auswahl-Rechnteck und die Projektion einer Vektor-Datei zurück.
  cVF ruft dazu ogrinfo auf, sucht die passenden Stichworte und konvertiert den
  Inhalt. Die Koordinaten beziehen sich auf das CRS der Vektoren. }

function tCover.VectorFrame(sFrm:string):trFrm; //Dateiname: Auswahlrahmen
const
  cFrm = 'cVF: Unable to open bounding geometry: ';
var
  sLin:string; //Zwischenlager
  slInf:tStringList=nil; //Vektor-Info
  I:integer;
begin
  Result:=crFrm; //Vorgabe = unmöglich
  //if length(sFrm)<1 then exit; //kein Aufruf
  if FileExists(sFrm)=False then Tools.ErrorOut(3,cFrm+sFrm);
  try
    slInf:=tStringList.Create;
    slInf.AddText(Gdal.OgrInfo(sFrm)); //Info-Text
    for I:=0 to pred(slInf.Count) do
    begin
      if LeftStr(slInf[I],13)='    ID["EPSG"' then
        Result.Epg:=StrToInt(copy(slInf[I],15,length(slInf[I])-16)) else
      if LeftStr(slInf[I],4)='    ' then continue; //nur linksbündige Einträge

      if LeftStr(slInf[I],7)='Extent:' then
      begin
        sLin:=ExtractDelimited(2,SlInf[I],['(',')']);
        Result.Lft:=StrToFloat(ExtractDelimited(1,sLin,[',']));
        Result.Btm:=StrToFloat(ExtractDelimited(2,sLin,[',']));
        sLin:=ExtractDelimited(4,SlInf[I],['(',')']);
        Result.Rgt:=StrToFloat(ExtractDelimited(1,sLin,[',']));
        Result.Top:=StrToFloat(ExtractDelimited(2,sLin,[',']));
      end else
      if (LeftStr(slInf[I],7)='PROJCRS')
      or (LeftStr(slInf[I],7)='GEOGCRS') then
        Result.Crs:=ExtractWord(2,slInf[I],['"']) else
    end;
  finally
    slInf.Free;
  end;
end;

{ pGA bildet Mittelwerte aus den Bilddaten "sImg" für ein regelmäßiges Gitter
  "sGrd". Das Gitter wird als Bild (Pixelraster) übergeben. Die Projektion MUSS
  in beiden Fällen identisch sein. }
{ pGA bestimmt für jede Gitter-Zelle den Mittelwert aller Bildpixel die zu mehr
  als der Hälfte im Gitter liegen. Dabei ignoriert pGA NoData Pixel. Intern
  verwendet pGA die Pixel-Koordinaten des Vorbilds "sImg" }

procedure tPoints.xGridAttrib(
  sGrd:string; //Raster als Bild
  sImg:string; //Vorbild
  sTrg:string); //Ziel oder leer DERZEIT NUR RASTER
const
  cPrj = 'pGA: Projection seems to differ: ';
var
  fHrz,fVrt:double; //horizontaler|vertikaler Versatz bezogen auf Vorbild
  fxBnd:tn2Sgl=nil; //Bild-Kanal
  fxRes:tn2Sgl=nil; //(summierte) Werte pro Gitterzelle
  iCol,iRow:integer; //Spalte, Zeile im Gitter
  ixCnt:tn2Wrd=nil; //gültige Pixel pro Gitterzelle
  rGrd,rHdr:trHdr; //Metdaten Gitter, Vorbild
  B,X,Y:integer;
begin
  if sTrg='' then sTrg:=ChangeFileExt(eeHme+cfFcs,''); //Vorgabe
  Header.Read(rGrd,sGrd); //Raster
  Header.Read(rHdr,sImg); //Vorbild
  if ExtractWord(2,rHdr.Cys,['"'])<>ExtractWord(2,rGrd.Cys,['"']) then
    Tools.ErrorOut(3,cPrj+ExtractFileName(sGrd)+' – '+ExtractFileName(sImg));
  fHrz:=rGrd.Lon-rHdr.Lon; //Versatz in CRS, bezogen auf Bild [links-oben]
  fVrt:=rHdr.Lat-rGrd.Lat;
  for B:=0 to pred(rHdr.Stk) do
  begin
    ixCnt:=Tools.Init2Word(rGrd.Lin,rGrd.Scn); //neu zählen für jeden Kanal
    fxRes:=Tools.Init2Single(rGrd.Lin,rGrd.Scn,0); //Vorgabe = Null für Summen
    fxBnd:=Image.ReadBand(B,rHdr,sImg);
    for Y:=0 to pred(rHdr.Lin) do
    begin
      iRow:=trunc((rHdr.Pix*(0.5+Y)-fVrt)/rGrd.Pix); //Gitter-Zeile mit 1/2 Pixel Versatz
      if (iRow<0) or (iRow>=rGrd.Lin) then continue;
      for X:=0 to pred(rHdr.Scn) do
      begin
        iCol:=trunc((rHdr.Pix*(0.5+X)-fHrz)/rGrd.Pix); //Gitter-Spalte mit 1/2 Pixel Versatz
        if (iCol<0) or (iCol>=rGrd.Scn) then continue;
        if isNan(fxBnd[Y,X]) then continue;
        fxRes[iRow,iCol]+=fxBnd[Y,X]; //Werte summieren
        inc(ixCnt[iRow,iCol]) //Treffer zählen
      end;
    end;
    for Y:=0 to pred(rGrd.Lin) do
      for X:=0 to pred(rGrd.Scn) do
        if ixCnt[Y,X]>0 then //Daten registriert
          fxRes[Y,X]/=ixCnt[Y,X] //Mittelwert
        else fxRes[Y,X]:=NaN; //keine Daten
    Image.WriteBand(fxRes,B,sTrg);
    write(#13,rHdr.Stk-B,#32);
  end;
  if rHdr.Stk>1
    then with rHdr do Header.WriteMulti(Prd,Stk,rGrd,aBnd,sTrg) //sollte so verwendbar sein
    else Header.WriteScalar(rGrd,sTrg);
  Tools.HintOut(true,'GridAttrib: '+ExtractFileName(sTrg));
end;

{ tLC korreliert den ersten Kanal aus "sImg" mit allen anderen und gibt das
  Ergebnis als Text zurück. tLC ignoriert NoData Pixel }
{ tLC bestimmt den Korrelations-Koeffitienden nach Gauß für alle definieren
  Pixel im Bild. }

function tTable.xLayerCorrelate(sImg:string):tStringList;
var
  fPrd:double=0; //Summe der Podukte (∑x*y)
  fSum,fMus:double; //Summe der Werte ∑x, ∑y
  fSqr,fRqs:double; //Summe der Quadrate ∑x², ∑y²
  fRes:double=0; //Ergebnis = Koeffitiend
  fxBnd:tn2Sgl=nil; //Bilddaten, erster Kanal
  fxCmp:tn2Sgl=nil; //Bilddaten, zweiter Kanal "compare"
  iCnt:dWord=0; //gültige Pixel
  rHdr:trHdr; //Metadaten
  B,X,Y:integer;
begin
  Result:=tStringList.Create;
  Header.Read(rHdr,sImg); //Stack
  Result.Text:=rHdr.aBnd; //bestehende Kanal-Namen
  fxBnd:=Image.ReadBand(0,rHdr,sImg); //erster Kanal = Indikator
  for B:=1 to pred(rHdr.Stk) do
  begin
    fxCmp:=Image.ReadBand(B,rHdr,sImg); //Kanäle 2..N = Vergleiche
    fPrd:=0; fSum:=0; fMus:=0; fSqr:=0; fRqs:=0; iCnt:=0; //Vorgabe
    for Y:=0 to pred(rHdr.Lin) do
      for X:=0 to pred(rHdr.Scn) do
      begin
        if isNan(fxBnd[Y,X]) or isNan(fxCmp[Y,X]) then continue;
        fSum+=fxBnd[Y,X]; //Werte (Summe)
        fMus+=fxCmp[Y,X];
        fPrd+=fxBnd[Y,X]*fxCmp[Y,X]; //Produkt (Summe)
        fSqr+=sqr(fxBnd[Y,X]); //Quadrate (Summe)
        fRqs+=sqr(fxCmp[Y,X]);
        inc(iCnt); //gültige Pixel
      end;
    if iCnt>0 then
    begin
      fRes:=(fSqr-sqr(fSum)/iCnt)*(fRqs-sqr(fMus)/iCnt);
      if fRes>0 then fRes:=(fPrd-fSum*fMus/iCnt)/sqrt(fRes);
      Result[B]:=#9+Result[B]+#9+FloatToStrF(fRes,ffFixed,7,4);
    end;
  end;
  //Result.Insert(1,'Correlates:');
  //Result.Insert(0,'Source:');
  Result[0]:=Result[0]+' correlates with:';
  Result.SaveToFile(eeHme+cfTab);
  Tools.HintOut(true,'LayerCorrelate: '+eeHme+cfTab)
end;

{ cRF extrahiert die Ursprung, Projektion, Bounding-Box, Pixelgröße und Kanäle
  der Bilddaten "sImg" aus dem "gdalinfo"-Text und gibt sie als "trCvr" zurück.
  Der Ursprung bezeichnet die linke obere Ecke des Bildes. Der Rahmen kann
  leere Bildflächen enthalten. }

function tCover._RasterCover_(sImg:string):trCvr; //Bildname: Abdeckung
const
  cCrs = 'cRF: Insufficient coordinate system information: ';
  cSqr = 'Reprojection necessary to get square pixels: ';
var
  fXip:single; //Pixel height
  sLin:string; //Zeilen-Puffer
  slInf:tStringList=nil; //GDAL ImageInfo
  I:integer;
begin
  Result:=crCvr; //Vorgabe = unmöglich
  try
    slInf:=TStringList.Create;
    slInf.AddText(Gdal.ImageInfo(sImg)); //GDAL-Info übernehmen
    for I:=0 to pred(slInf.Count) do
    begin
      if LeftStr(slInf[I],13)='    ID["EPSG"' then
        Result.Epg:=StrToInt(copy(slInf[I],15,length(slInf[I])-16))
      else if slInf[I][1]=#32 then continue; //nur linksbündige Einträge

      if LeftStr(slInf[I],7)='Size is' then
      begin
        sLin:=copy(slInf[I],9,$FF); //nur zahlen
        Result.Wdt:=StrToInt(ExtractDelimited(1,sLin,[',']));
        Result.Hgt:=StrToInt(ExtractDelimited(2,sLin,[',']));
      end
      else if LeftStr(slInf[I],21)='Coordinate System is:' then
        Result.Crs:=ExtractWord(2,slInf[succ(I)],['"'])
      else if LeftStr(slInf[I],6)='Origin' then
      begin
        sLin:=ExtractDelimited(2,slInf[I],['(',')']); //Ausdruck in Klammern
        Result.Lft:=StrToFloat(ExtractDelimited(1,sLin,[',']));
        Result.Top:=StrToFloat(ExtractDelimited(2,sLin,[',']));
      end
      else if LeftStr(slInf[I],10)='Pixel Size' then
      begin
        Result.Pix:=abs(StrToFloat(ExtractDelimited(2,slInf[I],['(',','])));
        fXip:=abs(StrToFloat(ExtractDelimited(2,slInf[I],[',',')'])));
        if (fXip-Result.Pix)/(fXip+Result.Pix)>0.0001 then
          Tools.ErrorOut(3,cSqr+sImg); //nur quadratische Pixel!
      end
      else if LeftStr(slInf[I],5)='Band ' then //"Band_" codiert Kanal-Namen
        Result.Stk:=max(StrToInt(ExtractDelimited(2,slInf[I],[#32])),Result.Stk);
    end;
  finally
    slInf.Free;
  end;
  Result.Rgt:=Result.Lft+Result.Wdt*Result.Pix; //Bounding Box
  Result.Btm:=Result.Top-Result.Hgt*Result.Pix;
  if Result.Epg=0 then Result.Epg:=CrsInfo(sImg); //EPSG-Code
  if (Result.Pix=0) or (Result.Hgt=0) or (Result.Wdt=0) then
    Tools.ErrorOut(3,cCrs+sImg);
end;

{ cGB gibt die Bounding-Box von Bilddaten als geographische Koordinaten zurück.
  cGB übernimmt dazu den Text das gdalinfo Befehls als "slInf" und die Zeilen-
  ID der Überschrift "Corner Coordinates" als "iOfs". "gdalinfo" übergibt alle
  Koordinaten auch geographisch.
  ==> DIE KOORDINATEN SIND IMMER GEOGRAPHISCH, UNABHÄNGIF VON RRES.ETP }

procedure tCover.GeoBonds(slInf:tStringList; iOfs:integer; var rRes:trFrm);

function lGetLon(sPnt:string):double;
begin
  Result:=
    StrToFloat(copy(sPnt,1,3))+
    StrToFloat(copy(sPnt,5,2))/60+
    StrToFloat(copy(sPnt,8,5))/3600
end;

function lGetLat(sPnt:string):double;
begin
  Result:=
    StrToFloat(copy(sPnt,16,3))+
    StrToFloat(copy(sPnt,20,2))/60+
    StrToFloat(copy(sPnt,23,5))/3600
end;

var
  sLin:string; //Koordinaten
  K:integer;
begin
  for K:=1 to 4 do
  begin
    sLin:=ExtractWord(4,slInf[iOfs+K],['(',')']); //Lon + Lat geographisch
    case K of
      1:begin
          rRes.Lft:=min(lGetLon(sLin),rRes.Lft);
          rRes.Top:=max(lGetLat(sLin),rRes.Top);
        end;
      2:begin
          rRes.Lft:=min(lGetLon(sLin),rRes.Lft);
          rRes.Btm:=min(lGetLat(sLin),rRes.Btm);
        end;
      3:begin
          rRes.Rgt:=max(lGetLon(sLin),rRes.Rgt);
          rRes.Top:=max(lGetLat(sLin),rRes.Top);
        end;
      4:begin
          rRes.Rgt:=max(lGetLon(sLin),rRes.Rgt);
          rRes.Btm:=min(lGetLat(sLin),rRes.Btm);
        end;
    end;
  end;
end;

{ cIB gibt in "rRes" die Bounding-Box von Bilddaten zurück. Dazu muss der
  "gdalinfo" Text in "slInf" und die Zeilen-ID der Überschrift "Corner
  Coordinates:" in "iOfs" übergeben werden. }

procedure tCover.ImgBonds(slInf:tStringList; iOfs:integer; var rRes:trFrm);
var
  fRes:double; //Koordinate
  sLin:string; //Koordinaten-Klammer
  K:integer;
begin
  for K:=1 to 4 do
  begin
    sLin:=ExtractWord(2,slInf[iOfs+K],['(',')']); //Lon + Lat projiziert

    if TryStrToFloat(ExtractWord(1,sLin,[',']),fRes) then
    begin
      rRes.Lft:=min(fRes,rRes.Lft);
      rRes.Rgt:=max(fRes,rRes.Rgt);
    end;

    if TryStrToFloat(ExtractWord(2,sLin,[',']),fRes) then
    begin
      rRes.Top:=max(fRes,rRes.Top);
      rRes.Btm:=min(fRes,rRes.Btm);
    end;
  end;
end;

{ lLM bestimmt den niedrigsten Pixel in jeder Zone und trägt seine Koordinaten
  in die Attribut-Tabelle ein. }
{ lLM sucht im Rasterbild (Höhemodell "fxElv", Zonen-Index "ixIdx") in jeder
  Zone nach dem niedrigsten Pixel und registriert seine Pixel-Koordinaten. Dann
  transformiert lLM die Pixel-Koordinaten in das CRS der Index-Datei und trägt
  sie als Lat/Lon in die Attribut-Tabelle ein. }

procedure tLines._LocalMinima_(
  fxElv:tn2Sgl; //Vorbild Rasterdaten (Höhe)
  ixIdx:tn2Int; //Zonen-Index aus "fxElv"
  const rHdr:trHdr); //Zonen-Metadaten
const
  cMax:single=MaxInt; //sehr hoch"
var
  faLat:tnSgl=nil; //Minimum Höhe: Latitute
  faLon:tnSgl=nil; //Minimum Höhe: Longitude
  faMin:tnSgl=nil; //Höhe niedrigster Pixel pro Zone
  iaHrz:tnInt=nil; //Pixel-Index "Longitude"
  iaVrt:tnInt=nil; //Pixel-Index "Latitude"
  X,Y,Z:integer;
begin
  //passt die Anzahl der Zonen? fxElv <=> iCnt
  faMin:=Tools.InitSingle(succ(rHdr.Cnt),dWord(cMax)); //Minimum Höhe pro Zone
  faLat:=Tools.InitSingle(succ(rHdr.Cnt),0); //Pixel-Index "Latitude"
  faLon:=Tools.InitSingle(succ(rHdr.Cnt),0); //Pixel-Index "Longitude"
  iaHrz:=Tools.InitInteger(succ(rHdr.Cnt),0); //Pixel-Index "horizontal"
  iaVrt:=Tools.InitInteger(succ(rHdr.Cnt),0); //Pixel-Index "vertikal"
  for Y:=0 to high(ixIdx) do
    for X:=0 to high(ixIdx[0]) do
      if fxElv[Y,X]<faMin[ixIdx[Y,X]] then
      begin
        iaVrt[ixIdx[Y,X]]:=Y; //Pixel-Indices
        iaHrz[ixIdx[Y,X]]:=X;
        faMin[ixIdx[Y,X]]:=fxElv[Y,X]; //neues Minimum
      end;
  for Z:=1 to high(faMin) do
  begin
    faLat[Z]:=rHdr.Lat-iaVrt[Z]*rHdr.Pix-rHdr.Pix/2;
    faLon[Z]:=rHdr.Lon+iaHrz[Z]*rHdr.Pix+rHdr.Pix/2;
  end;
  Tools.BitInsert(faLat,$FFF,eeHme+cfAtr); //Latitude ergänzen
  Tools.BitInsert(faLon,$FFF,eeHme+cfAtr); //Longitude ergänzen
end;

{ tVM erzeugt einen Raster-Layer mit Klassen aus dem Attribut "sFld" im Vektor-
  Layer "sVct". "sFld" muss natürliche Zahlen zwischen 1 und 250 entalten.
  Table.AddThema kann beliebige Bezeichner in fortlaufende IDs aus natürlichen
  Zahlen verwandeln. }

procedure tTable.VectorMask(
  sFld:string; //Name des Klassen-Attributs in der Vector-Datei
  sVct:string); //Name der Vektor-Datei
var
  ixRfz:tn2Byt=nil; //Klassen-Maske
  rHdr:trHdr; //gemeinsame Metadaten
begin
  Header.Read(rHdr,eeHme+cfIdx); //Vorbild für richtige Größe
  ixRfz:=Tools.Init2Byte(rHdr.Lin,rHdr.Scn); //leere Maske
  Image.WriteThema(ixRfz,eeHme+cfMap); //leeres Pixel-Bild als cfMap gespeichert
  Header.WriteThema(1,rHdr,'zero,one',eeHme+cfMap); //als Klassen-Metadaten speichern
  Gdal.Rasterize(0,sFld,eeHme+cfMap,eeHme+cfFcs); //Klassen-IDs einbrennen
end;

// Raster-Bild Statistik
// Min/Max für Extremwerte
// Hig/Low für 99%/1% Percentil
// Mid/Mea für mean/median

function tTable.BandHist(
  var fSum:double; //Summe aller Werte
  var iHit:integer; //gültige Treffer
  iBnd:integer; //Kanal in "sImg" (Eingabe)
  iSmp:integer; //Stichproben (Eingabe)
  sImg:string): //Vorbild (Eingabe)
  tnSgl; //sortierte Stichproben der Werte
var
  fPrt:single=0; //Stichproben-Distanz in Pixeln
  fxBnd:tn2Sgl=nil; //Kanal aus "sImg"
  iCol,iRow:integer; //Pixel-Koordinaten der Stichprobe
  rHdr:trHdr; //Metadaten
  I:integer;
begin
  //iSmp>0!
  Result:=nil; //Vorgabe
  Header.Read(rHdr,sImg); //Metadaten
  fxBnd:=Image.ReadBand(iBnd,rHdr,sImg);
  SetLength(Result,min(iSmp,rHdr.Lin*rHdr.Scn)); //Stichpoben
  fPrt:=rHdr.Lin*rHdr.Scn/length(Result); //Stichproben-Distanz
  fSum:=0; //Vorgabe
  iHit:=0; //Vorgabe
  for I:=0 to high(Result) do
  begin
    iCol:=trunc(fPrt*I) mod rHdr.Scn; //regelmäßige Abstände
    iRow:=trunc(fPrt*I) div rHdr.Scn;
    if isNan(fxBnd[iRow,iCol]) then continue; //nur definierte Punkte
    Result[iHit]:=fxBnd[iRow,iCol]; //Wert am Stichpunkt
    fSum+=Result[iHit]; //Summe aller Werte
    inc(iHit) //gültige Stichproben
  end;
  QuickSort(Result,iHit);
end;

{ lIS bestimmt Minimum und Maximum, 1%-Percentile, Mittelwert und Median sowie
  ein cumuliertes Histogramm aus den Bilddaten von "sImg" und gibt das Ergebnis
  als Text zurück. lIS ignoriert Nodata Pixel. }
{ lIS nimmt "iSmp" regelmäßig vereilte Stichproben aus dem Bild. lIS zählt die
  die gültigen Proben in "iHit" und summiert ihre Werte in "fSum". Für die
  Statistik sortiert lIS die Liste und bildet die Kenwerte }

function tTable.xImageStats(
  iSmp:integer; //Stichproben
  sImg:string): //Vorbild
  tStringList; //Ergebnis als Text
const
  cHst=64; //Stützpunkte im Histogramm
var
  faVal:tnSgl=nil; //Stichproben
  fDff:single=0; //Maximale Werte-Differenz
  fSum:double=0; //Summe aller Werte
  iaHst:tnInt=nil; //Anzahl Treffer
  iHit:integer=0; //gültige Pixel
  iLow,iPrd:integer; //Stichproben-Intervall für Histogramm
  {
  fPrt:single=1; //Stichproben-Distenz
  fxBnd:tn2Sgl=nil; //Kanal aud "sImg"
  iCol,iRow:integer; //Pixel-Position
  }
  rHdr:trHdr; //Metadaten
  B,I:integer;
begin
  Result:=tStringList.Create;
  Header.Read(rHdr,sImg); //Metadaten Zellindex
  for B:=0 to pred(rHdr.Stk) do //Kanäle einzeln
  begin
    faVal:=BandHist(fSum,iHit,B,iSmp,sImg); //iHit wird gesetzt

    Result.Add('Band: '+ExtractWord(succ(B),rHdr.aBnd,[#10]));
    Result.Add(#9'Min  '#9+FloatToStrF(faVal[0],ffFixed,7,4));
    Result.Add(#9'Low  '#9+FloatToStrF(faVal[round(iHit*0.01)],ffFixed,7,4));
    Result.Add(#9'Mean '#9+FloatToStrF(fSum/iHit,ffFixed,7,4));
    Result.Add(#9'Median'#9+FloatToStrF(faVal[round(iHit*0.50)],ffFixed,7,4));
    Result.Add(#9'High '#9+FloatToStrF(faVal[round(iHit*0.99)],ffFixed,7,4));
    Result.Add(#9'Max  '#9+FloatToStrF(faVal[pred(iHit)],ffFixed,7,4));

    iLow:=round(iHit*0.01); //eingeschränktes Intervall
    iPrd:=round(iHit*0.99)-iLow; //Intervall-Länge
    iaHst:=Tools.InitInteger(succ(cHst),0); //Anzahl Treffer
    fDff:=faVal[iLow+iPrd]-faVal[iLow]; //Werte-Differenz
    for I:=iLow to pred(iLow+iPrd) do
      inc(iaHst[trunc((faVal[I]-faVal[iLow])/fDff*cHst)]);
    Result.Add('Histogramm:');
    for I:=0 to cHst do
      //Result.Add(#9+IntToStr(I)+#9+IntToStr(iaHst[I]));
      Result.Add(#9+FloatToStrF(faVal[iLow+trunc(iPrd/cHst*I)],ffFixed,7,4)+
        #9+IntToStr(iaHst[I]));
    Tools.HintOut(true,'ImageStats: '+IntToStr(succ(B))); //Fortschritt
  end;
  Result.SaveToFile(eeHme+cfTab)
end;

{ rQS sortiert das Array "faDev" aufsteigend. Dazu vertauscht rQS Werte-Paare
  bis alle Vergliche passen. rQS verwendet zu Beginn große Abstände zwischen
  den Positionen im Array und reduziert die Distanz schrittweise. }

procedure tTable.QuickSort(
  faDev:tnSgl; //unsortiertes Array
  iDim:integer); //gültige Stellen im Array
var
  fTmp:single; //Zwischenlager
  iChg:integer; //Tausch notwendig
  iStp:integer; //Distanz zwischen Positionen
  B:integer;
begin
  if iDim<2 then exit; //nichts zu sortieren
  iStp:=iDim; //Vorgabe
  repeat
    if iStp>1 then iStp:=iStp div 2;
    iChg:=0; //Vorgabe
    for B:=iStp to pred(iDim) do
      if faDev[B-iStp]>faDev[B] then //große Werte nach hinten
      begin
        fTmp:=faDev[B-iStp];
        faDev[B-iStp]:=faDev[B];
        faDev[B]:=fTmp;
        inc(iChg)
      end;
    {for B:=pred(iDim) downto iStp do
      if faDev[B-iStp]>faDev[B] then //große Werte nach hinten
      begin
        fTmp:=faDev[B-iStp];
        faDev[B-iStp]:=faDev[B];
        faDev[B]:=fTmp;
        inc(iChg)
      end;}
  until (iStp=1) and (iChg=0); //alle Vergleiche richtig
end;

{ cEP prüft ob die Dateinamen "sHig" und "sLow" dieselben Werte für Kachel-ID
  und Datum haben. "ruTle" und "ruDat" müssen aktuell sein. Die Abschnitte sind
  getrennt. cEP muss deshalb zweimal prüfen und zählt die passenden Buchstaben }

function tCover.EqualTileDat(sHig,sLow:string):boolean;
var
  iSeq:integer=0; //Anzahl gleiche Buchstaben
  I:integer;
begin
  if length(sHig)=length(sLow) then
  begin
    for I:=ruTle.Ofs to pred(ruTle.Ofs+ruTle.Sze) do //Kachel-ID
      if sLow[I]=sHig[I] then inc(iSeq);
    for I:=ruDat.Ofs to pred(ruDat.Ofs+ruDat.Sze) do //Datum
      if sLow[I]=sHig[I] then inc(iSeq);
  end;
  Result:=iSeq=ruTle.Sze+ruDat.Sze
end;

{ cEF überträgt Informationen aus dem ENVI-Haeder in einen neuen Cover-Record }

function tCover.EnviCover(
  sImg:string): //Dateiname
  trCvr; //Rahmen
var
  rHdr:trHdr; //Metadaten
begin
  Result:=crCvr; //Vorgabe = unmöglich
  Header.Read(rHdr,sImg);
  Result.Crs:=ExtractWord(1,rHdr.Cys,['"']); //Projekton als Text
  //Result.Pro:=
  Result.Epg:=CrsInfo(sImg); //EPSG-Code
  Result.Wdt:=rHdr.Scn;
  Result.Hgt:=rHdr.Lin;
  Result.Stk:=rHdr.Stk;
  Result.Lft:=rHdr.Lon; //Koordinaten ..
  Result.Top:=rHdr.Lat;
  Result.Rgt:=rHdr.Lon+rHdr.Scn*rHdr.Pix;
  Result.Btm:=rHdr.Lat-rHdr.Lin*rHdr.Pix;
  Result.Pix:=rHdr.Pix;
end;

{ cEF überträgt Projektion und Koordinaten aus einem ENVI-Header in einen
  "trFrm" Record. }

function tCover._EnviFrame_(
  sImg:string): //Dateiname
  trFrm; //Rahmen
var
  rHdr:trHdr; //Metadaten
begin
  Result:=crFrm; //Vorgabe = unmöglich
  Header.Read(rHdr,sImg);
  Result.Crs:=ExtractWord(1,rHdr.Cys,['"']); //Projekton als Text
  Result.Epg:=CrsInfo(sImg); //EPSG-Code
  Result.Lft:=rHdr.Lon; //Koordinaten ..
  Result.Top:=rHdr.Lat;
  Result.Rgt:=rHdr.Lon+rHdr.Scn*rHdr.Pix;
  Result.Btm:=rHdr.Lat-rHdr.Lin*rHdr.Pix;
end;

{ cWD bestimmt Position aller Underscores "_" im übergebenen Dateinamen als
  Array. Result[0] gibt die Position des letzten Zeichen des Pfadnamens zurück,
  Result[iDim] die Position NACH dem Ende des Namens als Übertrag. }

function tCover.WordDelimiter(sNme:string):tnInt;
var
  iDim:integer=0; //Anzahl gefundene Delimiter
  iOfs:integer=0; //Länge Pfadname incl. Seperator
  I:integer;
begin
  iOfs:=length(ExtractFilePath(sNme)); //Länge Pfadname incl. Seperator
  SetLength(Result,length(sNme)-iOfs); //Vorgabe = alle Buchstaben
  Result[0]:=iOfs; //letztes Zeichen im Pfadnamen
  for I:=succ(iOfs) to length(sNme) do
    if sNme[I]='_' then
    begin
      inc(iDim);
      Result[iDim]:=I;
    end;
  inc(iDim);
  Result[iDim]:=succ(length(sNme)); //Übertrag für Ende des Namens
  SetLength(Result,succ(iDim)) //Array abschießen
end;

{ cRF gibt Bounding-Box und CRS der Bilddaten "sImg" zurück. Dazu ruft cRF
  "gdalinfo" auf und transformiert die Eck-Koordinaten des Bilds in eine
  Bounding-Box. cRF übernimmt EPSG-Code und CRS-Name direkt aus dem Text.
  Mit "bGeo=True" übergibt cRF IN JEDEM FALL geographische Koordinaten, auch
  wenn der EPSG-Code projiziert ist. }

function tCover._RasterFrame(sImg:string):trFrm; //Bilddaten => Rahmen
const
  cImg = 'cRF: File not available: ';
var
  slInf:tStringList=nil; //GDAL ImageInfo
  I:integer;
begin
  Result:=crFrm; //Vorgabe = unmöglich
  if not FileExists(sImg) then Tools.ErrorOut(3,cImg+sImg);

  try
    slInf:=TStringList.Create;
    slInf.AddText(Gdal.ImageInfo(sImg)); //GDAL-Info übernehmen
    for I:=0 to pred(slInf.Count) do
    begin
      if LeftStr(slInf[I],13)='    ID["EPSG"' then
        Result.Epg:=StrToInt(copy(slInf[I],15,length(slInf[I])-16))
      else if slInf[I][1]=#32 then continue; //nur linksbündige Einträge

      if LeftStr(slInf[I],21)='Coordinate System is:' then
        Result.Crs:=ExtractWord(2,slInf[succ(I)],['"'])
      else if LeftStr(slInf[I],19)='Corner Coordinates:' then
        ImgBonds(slInf,I,Result); //Bounding-Box wie EPSG
    end;
    if Result.Epg=0 then Result.Epg:=CrsInfo(sImg); //EPSG-Code
  finally
    slInf.Free;
  end;
end;

// Frame an Projektion der Bilddaten anpassen.
// bestehende Einstellungen möglichst lange nutzen

function tCover.FrameWarp(sPly,sImg:string):trFrm;
var
  iEpg:integer; //Projektion als EPSG
begin
  iEpg:=Cover.CrsInfo(sImg); //Projektion der Bilddaten
  Gdal.ImportVect(iEpg,sPly); //Vektor umprojizieren und als "vector.csv" speichern
  Result:=VectorFrame(eeHme+cfVct) //Rahmen aus umprojizierten Vektoren
end;

{ cCS setzt in "sImg" alle Bildpixel außerhalb des Polygons "sFrm" auf NoData.
  cCS projiziert den Rahmen "sFrm" auf das CRS von "sImg", erzeugt eine Pixel-
  Maske [0,1] mit dem Rahmen von "sFrm" und setzt alle Pixel aßerhalb des
  Rahmens auf NoData. }

procedure tCover.xClipToShape(
  sFrm:string; //Geometrie (ROI)
  sImg:string); //Vorbild WIRD VERÄNDERT!
var
  fxBnd:tn2Sgl=nil; //Vorbild (multiband)
  fxMsk:Tn2Sgl=nil; //Maske für ROI
  iEpg:integer; //EPSG-Code Vorbild
  rHdr:trHdr; //Metadaten
  B,X,Y:integer;
begin
  iEpg:=Cover.CrsInfo(sImg); //Projektion der Bilddaten
  Gdal.ImportVect(iEpg,sFrm); //Rahmen projizieren + als CSV speichern
  Header.Read(rHdr,sImg); //für Bounding Box des Vorbilds
  fxMsk:=Tools.Init2Single(rHdr.Lin,rHdr.Scn,0); //Maske mit Vorgabe = Null
  Image.WriteBand(fxMsk,0,eeHme+cfMsk); //als "mask" für GDAL speichern
  Header.WriteScalar(rHdr,eeHme+cfMsk);
  Gdal.Rasterize(1,'',eeHme+cfMsk,eeHme+cfVct); //ROI als (1) einbrennen
  fxMsk:=Image.ReadBand(0,rHdr,eeHme+cfMsk); //Raster-Maske [0,1] lesen

  Tools.EnviRename(sImg,eeHme+'temp'); //Zwischenlager
  Header.Read(rHdr,eeHme+'temp'); //Metadaten für Bild
  for B:=0 to pred(rHdr.Stk) do
  begin
    fxBnd:=Image.ReadBand(B,rHdr,eeHme+'temp');
    for Y:=0 to pred(rHdr.Lin) do
      for X:=0 to pred(rHdr.Scn) do
        if fxMsk[Y,X]<1 then //maskierte Pixel ..
          fxBnd[Y,X]:=NaN; //.. auf NaN setzen
    Image.WriteBand(fxBnd,B,sImg); //Vorbild ersetzen
  end;
  with rHdr do Header.WriteMulti(Prd,Stk,rHdr,aBnd,sImg);
  Tools.HintOut(true,'ClipToShape: '+ExtractFileName(sImg));
end;

{ lDW bestimmt den cumulierten Abfluss über alle in "iaLnk" verknüpften Zonen
  und gibt ihn als Zonen-Attribut zurück }

function tLines.DrainWeight(
  iaLnk:tnInt; //Zonen-Verknüpfungen (Bezug "ixIdx")
  var rHdr:trHdr): //Metadaten der Zonen
  tnSgl; //Cumulierter Abfluss in Hektar
var
  faSze:tnSgl=nil; //Fläche der Zonen in [ha]
  pSze:^single; //Fläche der aktuellen Zone
  iIdx:integer; //Zonen-ID
  Z:integer;
begin
  Result:=Tools.InitSingle(length(iaLnk),0);
  faSze:=Build.ZonesSize(rHdr);
  //length(iaLnk)=length(faSze)?
  for Z:=1 to high(iaLnk) do
  begin
    iIdx:=Z;
    pSze:=@faSze[Z];
    repeat
      Result[iIdx]+=pSze^;
      iIdx:=iaLnk[iIdx];
    until iIdx=iaLnk[iIdx];
  end;
end;

{ lDP transformiert die Pixel-Indices der Zonen-Abfluss-Punkte in "iaDrn" in
  Welt-Koordinaten und speichert sie zusammen mit dem Abfluss und den
  Verknüpfungen der Zonen als Attribut-Tabelle. lDL verwendet die Projektion
  von "rHdr". lDP vergibt an Zonen ohne Abfluss Koordinaten knapp außerhalb der
  Bounding Box nahe der NW-Ecke (Ursprung) }

procedure tLines.RunoffPoints(
  faWgt:tnSgl; //Cumulierter Abfluss
  iaDrn:tnInt; //Pixelindex der Abfluss-Punkte
  iaLnk:tnInt; //Verknüpfungen als Zonen-Indices
  var rHdr:trHdr); //Metadaten Zonen-Index
var
  fxDrn:tn2Sgl=nil; //Abfluss-Punkte als Welt-Koordinaten
  iScn:integer; //Bildbreite
  pLat,pLon:^tnSgl; //simple Notation
  Z:integer;
begin
  fxDrn:=Tools.Init2Single(2,succ(rHdr.Cnt),0); //Lat, Lon
  pLat:=@fxDrn[0];
  pLon:=@fxDrn[1];
  iScn:=rHdr.Scn;
  for Z:=1 to rHdr.Cnt do
    with rHdr do
      if iaDrn[Z]<0 then //kein Abfluss definiert
      begin //Punkt außerhalb der Bounding-Box
        pLat^[Z]:=Lat+Pix/2;
        pLon^[Z]:=Lon-Pix/2;
      end
      else
      begin //Welt-Koordinaten aus Pixel-Koordinaten
        pLat^[Z]:=Lat-(iaDrn[Z] div iScn)*Pix-Pix/2;
        pLon^[Z]:=Lon+(iaDrn[Z] mod iScn)*Pix+Pix/2;
      end;

  Tools.BitWrite(fxDrn,eeHme+cfLcy); //Koordinaten speichern + Liniendicke
  Tools.BitInsert(faWgt,2,eeHme+cfLcy); //Fläche ergänzen
  Tools.BitInsert(tnSgl(iaLnk),3,eeHme+cfLcy); //Verknüpfungen ergänzen
end;

{ lPC übersetzt die Abfluss-Punkte aus "legacy.bit" in Linien-Vektoren zwischen
  je zwei Punkten uns speichert das Ergebnis als focus.csv (WKT-Format) }
{ lPC interpretiert die Felder [0] und [1] als Koordinaten [Lat,Lon], das Feld
  [2] als Abfluss und das Feld [3] als ID der verknüpften Zone. lPC zeichnet
  von jedem Punkt "fxPnt[Z]" eine Linie zum Punkt "fxPnt[laLnk[Z]]" und ergänzt
  das Feld[2] (Abfluss) als Linien-Attribut. Dabei ignoriert lPC Punkte, die in
  "iaLnk" mit sich selbst verknüpft sind. lPC übernimmt die Projektion aus den
  Zonen-Medataten "index.hdr" }

procedure tLines.PointChain(sGrv:string);
const
  cCsv = 'lDL: Unable to read / write file: ';
  cFmt = 'WKT,Integer(9.0),Integer(9.0),Real(18.1)'; //Real(24.15)
var
  dCsv:TextFile; //Datei
  fxPnt:tn2Sgl=nil; //Linien-Attribute
  iaLnk:tnInt=nil; //Verknüpfungen (integer)
  pLat,pLon,pWgt:^tnSgl; //Zeiger auf Single-Arrays
  sLnk:string; //Zeile in WKT-Datei
  rHdr:trHdr; //Zonen-Metadaten
  Z:integer;
begin
  //if not FileExists(eeHme+cfIdx) then
  Header.Read(rHdr,eeHme+cfIdx); //Zonen Metadaten
  fxPnt:=Tools.BitRead(sGrv); //Wert, Points und Attribute
  //if length(fxPnt)<4 then SetLength ...
  pLat:=@fxPnt[0]; //Label-Point Latitude
  pLon:=@fxPnt[1]; //Label-Point Longitude
  pWgt:=@fxPnt[2]; //Linien-Attribut
  iaLnk:=tnInt(fxPnt[3]); //verknüpfte Zone (ID)
  try
    AssignFile(dCsv,eeHme+cfFcs);
    {$i-} Rewrite(dCsv); {$i+}
    if IOResult<>0 then Tools.ErrorOut(3,cCsv+cfFcs);
    writeln(dCsv,'WKT,source,drain,weight');
    for Z:=1 to high(fxPnt[0]) do //alle Überlauf-Punkte
      if (iaLnk[iaLnk[Z]]<>iaLnk[Z]) //kein Selbstbezug im Ziel
      and (iaLnk[Z]<>Z) then //kein Selbstbezug der Quelle
      begin
        sLnk:='"MULTILINESTRING (('+ //Kopf
          FloatToStr(pLon^[Z])+#32+ //von..
          FloatToStr(pLat^[Z])+','+
          FloatToStr(pLon^[iaLnk[Z]])+#32+ //nach..
          FloatToStr(pLat^[iaLnk[Z]])+'))",'+
          IntToStr(Z)+','+IntToStr(iaLnk[Z])+','+ //Zonen-IDs
          FloatToStr(pWgt^[Z]); //cumulierte Fläche
        writeln(dCsv,sLnk);
      end
  finally
    Flush(dCsv);
    CloseFile(dCsv);
  end;
  Tools.TextOut(ChangeFileExt(eeHme+cfFcs,'.csvt'),cFmt);
  Tools.TextOut(ChangeFileExt(eeHme+cfFcs,'.prj'),rHdr.Cys);
  Tools.HintOut(true,'PointChain: '+eeHme+cfFcs);
end;

end.

{==============================================================================}

{ lDL transformiert die Zonen-Abfluss-Punkte in Pixel-Koordinaten "iaDrn" in
  Welt-Koordinaten in der Projektion von "rHdr" und speichert das Ergebnis als
  Zonen-Attribute. Die Koordinaten bezeichnen die Pixel-Mitte }

procedure tLines.DrainLines_(
  faWgt:tnSgl; //Cumulierter Abfluss
  iaDrn:tnInt; //Pixelindex der Abfluss-Punkte
  iaLnk:tnInt; //Verknüpfungen als Zonen-Indices
  var rHdr:trHdr); //Metadaten Zonen-Index
var
  fxDrn:tn2Sgl=nil; //Abfluss-Punkte als Welt-Koordinaten
  iScn:integer; //Bildbreite
  pLat,pLon:^tnSgl; //simple Notation
  Z:integer;
begin
  fxDrn:=Tools.Init2Single(2,succ(rHdr.Cnt),0); //Lat, Lon, Wgt
  pLat:=@fxDrn[0];
  pLon:=@fxDrn[1];
  iScn:=rHdr.Scn;
  for Z:=1 to rHdr.Cnt do
    with rHdr do
    begin //Welt-Koordinaten aus Pixel-Koordinaten
      pLat^[Z]:=Lat-(iaDrn[Z] div iScn)*Pix-Pix/2;
      pLon^[Z]:=Lon+(iaDrn[Z] mod iScn)*Pix+Pix/2;
    end;
  Tools.BitWrite(fxDrn,eeHme+cfLcy); //Koordinaten speichern + Liniendicke
  Tools.BitInsert(faWgt,2,eeHme+cfLcy); //Fläche ergänzen
  Tools.BitInsert(tnSgl(iaLnk),3,eeHme+cfLcy); //Verknüpfungen ergänzen
end;

{ lPC übersetzt die "gravity.bit" Attribute in Linien-Vektoren im WKT-Format }
{ lPC interpretiert das Feld[0] als Höhe (Attribut) der Zonen, die Felder[1,2]
  als Koordinaten der Label-Punkte, das Feld[3] als ID der verknüpften Zone und
  das Feld[4] als cumulierten Abfluss. lPC übernimmt das Koordinatensystem aus
  den aktuellen Zonen "index.hdr" }

procedure tLines.PointChain_(sGrv:string);
const
  cCsv = 'lDL: Unable to read / write file: ';
  cFmt = 'WKT,Integer(9.0),Integer(9.0),Real(18.1)'; //Real(24.15)
var
  dCsv:TextFile; //Datei
  fxPnt:tn2Sgl=nil; //Linien-Attribute
  iaLnk:tnInt=nil; //Verknüpfungen (integer)
  pLat,pLon,pWgt:^tnSgl; //Zeiger auf Single-Arrays
  sLnk:string; //Zeile in WKT-Datei
  rHdr:trHdr; //Zonen-Metadaten
  Z:integer;
begin
  //if not FileExists(eeHme+cfIdx) then
  Header.Read(rHdr,eeHme+cfIdx); //Zonen Metadaten
  fxPnt:=Tools.BitRead(sGrv); //Wert, Points und Attribute
  //if length(fxPnt)<4 then SetLength ...
  pLat:=@fxPnt[0]; //Label-Point Latitude
  pLon:=@fxPnt[1]; //Label-Point Longitude
  pWgt:=@fxPnt[2]; //Linien-Attribut
  iaLnk:=tnInt(fxPnt[3]); //verknüpfte Zone (ID)
  try
    AssignFile(dCsv,eeHme+cfFcs);
    {$i-} Rewrite(dCsv); {$i+}
    if IOResult<>0 then Tools.ErrorOut(3,cCsv+cfFcs);
    writeln(dCsv,'WKT,source,drain,weight');
    for Z:=1 to high(fxPnt[0]) do //alle Überlauf-Punkte
      if iaLnk[Z]>0 then
      begin
        sLnk:='"MULTILINESTRING (('+ //Kopf
          FloatToStr(pLon^[Z])+#32+ //von..
          FloatToStr(pLat^[Z])+','+
          FloatToStr(pLon^[iaLnk[Z]])+#32+ //nach..
          FloatToStr(pLat^[iaLnk[Z]])+'))",'+
          IntToStr(Z)+','+IntToStr(iaLnk[Z])+','+ //Zonen-IDs
          FloatToStr(pWgt^[Z]); //cumulierte Fläche
        writeln(dCsv,sLnk);
      end
  finally
    Flush(dCsv);
    CloseFile(dCsv);
  end; //of try ..
  Tools.TextOut(ChangeFileExt(eeHme+cfFcs,'.csvt'),cFmt);
  Tools.TextOut(ChangeFileExt(eeHme+cfFcs,'.prj'),rHdr.Cys);
  Tools.HintOut(true,'PointChain: '+eeHme+cfFcs);
end;

{ lDW bestimmt den cumulierten Abfluss über alle in "iaLnk" verknüpften Zonen
  und gibt ihn als Zonen-Attribut zurück }

function tLines._LinkWeight(
  iaLnk:tnInt; //Zonen-Verknüpfungen (Bezug "ixIdx")
  var rHdr:trHdr): //Metadaten der Zonen
  tnSgl; //Cumulierter Abfluss in Hektar
var
  faSze:tnSgl=nil; //Fläche der Zonen in [ha]
  fSze:single; //abnehmender Abfluss
  iIdx:integer; //Zonen-ID
  Z:integer;
begin
  Result:=Tools.InitSingle(length(iaLnk),0);
  faSze:=Build.ZonesSize(rHdr);
  //length(iaLnk)=length(faSze)?
  for Z:=1 to high(iaLnk) do
  begin
    iIdx:=Z;
    fSze:=faSze[Z];
    repeat
      Result[iIdx]+=fSze;
      fSze:=fSze*0.9;
      iIdx:=iaLnk[iIdx];
    until iIdx=iaLnk[iIdx];
  end;
end;

{ lPC übersetzt die "gravity.bit" Attribute in Linien-Vektoren im WKT-Format }
{ lPC interpretiert das Feld[0] als Höhe (Attribut) der Zonen, die Felder[1,2]
  als Koordinaten der Label-Punkte, das Feld[3] als ID der verknüpften Zone und
  das Feld[4] als cumulierten Abfluss. lPC zeichnet für jede Zone eine Linie
  von ihrem Abfluss-Punkt zum Abfluss-Punkt der in "iaLnk" verknüpften Zone und
  ergänzt "fxPnt[4]" als Attribut für die Linienbreite. lPC ignoriert Punkte
  mit den Koordinaten [-1,-1]. lPC übernimmt das Koordinatensystem aus den
  Zonen-Medataten "index.hdr" }

procedure tLines.PointChain_(sGrv:string);
const
  cCsv = 'lDL: Unable to read / write file: ';
  cFmt = 'WKT,Integer(9.0),Integer(9.0),Real(18.1)'; //Real(24.15)
var
  dCsv:TextFile; //Datei
  fxPnt:tn2Sgl=nil; //Linien-Attribute
  iaLnk:tnInt=nil; //Verknüpfungen (integer)
  pLat,pLon,pWgt:^tnSgl; //Zeiger auf Single-Arrays
  sLnk:string; //Zeile in WKT-Datei
  rHdr:trHdr; //Zonen-Metadaten
  Z:integer;
begin
  //if not FileExists(eeHme+cfIdx) then
  Header.Read(rHdr,eeHme+cfIdx); //Zonen Metadaten
  fxPnt:=Tools.BitRead(sGrv); //Wert, Points und Attribute
  //if length(fxPnt)<4 then SetLength ...
  pLat:=@fxPnt[0]; //Label-Point Latitude
  pLon:=@fxPnt[1]; //Label-Point Longitude
  pWgt:=@fxPnt[2]; //Linien-Attribut
  iaLnk:=tnInt(fxPnt[3]); //verknüpfte Zone (ID)
  try
    AssignFile(dCsv,eeHme+cfFcs);
    {$i-} Rewrite(dCsv); {$i+}
    if IOResult<>0 then Tools.ErrorOut(3,cCsv+cfFcs);
    writeln(dCsv,'WKT,source,drain,weight');
    for Z:=1 to high(fxPnt[0]) do //alle Überlauf-Punkte
      if (pLon^[Z]<>-1) and (pLat^[Z]<>-1) then
        if iaLnk[Z]>0 then
        begin
          sLnk:='"MULTILINESTRING (('+ //Kopf
            FloatToStr(pLon^[Z])+#32+ //von..
            FloatToStr(pLat^[Z])+','+
            FloatToStr(pLon^[iaLnk[Z]])+#32+ //nach..
            FloatToStr(pLat^[iaLnk[Z]])+'))",'+
            IntToStr(Z)+','+IntToStr(iaLnk[Z])+','+ //Zonen-IDs
            FloatToStr(pWgt^[Z]); //cumulierte Fläche
          writeln(dCsv,sLnk);
        end
  finally
    Flush(dCsv);
    CloseFile(dCsv);
  end;
  Tools.TextOut(ChangeFileExt(eeHme+cfFcs,'.csvt'),cFmt);
  Tools.TextOut(ChangeFileExt(eeHme+cfFcs,'.prj'),rHdr.Cys);
  Tools.HintOut(true,'PointChain: '+eeHme+cfFcs);
end;

