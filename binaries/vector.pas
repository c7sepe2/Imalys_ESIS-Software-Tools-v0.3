unit vector; //+miscellanous

{ VECTOR sammelt Routinen um Vektoren zu transformieren und zu attributieren.
  Dazu werden alle Vektor-Dateien als "vector.csv" importiert, zeilenweise
  bearbeitet, die ergebnis-Zeilen als "focus.csv" und "focus.csvt" gespeichert
  und zum Schluss in einem wählbaren Vektor-Format exportiert. In- und Export
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
  tCover = class(tObject) //checked 241116
    private
      function _BandCount_(sImg:string):integer;
      function _FrameHeader_(rFrm:trFrm; sImg:string):trHdr;
      function Intersect(rImg,rRoi:trFrm):trFrm;
      function _LandsatFrame_(sArc:string):tarGeo;
      function _PixelSize_(sImg:string):single;
      function _PointInside_(rPnt:trGeo; rPly:tarGeo):boolean;
      function VectorFrame(sFrm:string):trFrm;
      procedure WriteFrame(rFrm:trFrm; sPrj:string);
  public
      procedure ClipToShape(sFrm,sImg:string);
      function CrsInfo(sImg:string):integer;
      function MergeFrames(slImg:tStringList):trFrm;
      function RasterFrame(sImg:string):trCvr;
      function ReadFrame(sVct:string):trFrm;
      function Rotate(iEpg:integer; rImg,rRoi:trFrm):trFrm;
      function VectorCrsFrame(iEpg:integer; sPly:string):trFrm;
      function xCover(var bFit:boolean; slImg:tStringList):trFrm;
  end;

  tLines = class(tObject) //Abfluss als Linien-Vektoren
    private
      function _BoundingBoxGML_(raLnk:tarFrm):trFrm;
      function _CoordinatesGML_(iaNxt,iaPix:tnInt; var rHdr:trHdr):tarFrm;
      procedure _RunOffGML_(iCrs:integer; sGml:string);
    public
      function LandsatFrame(sWkt:string):trFrm;
      procedure xDrainLines(iPln:integer);
      procedure xDrainPoints();
  end;

  tPoints = class(tObject) //Punkt-Vektor Anwendungen <== tTable?
    private
      function AttribValues(sFtr:string):tn2Sgl;
      procedure DefaultFormat(iFtr:integer);
      procedure FieldTypes(sVct:string);
      procedure FormatAppend(iFtr:integer);
      function GetIndex(var rHdr:trHdr):tnInt;
      procedure PixMask(iaPix:tnInt; sTmp:string);
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
      function FieldValues(sFld:string):tStringList;
    public
      function AddThema(sFld:string):tStringList;
      function _BandHist(var fSum:double; var iHit:integer; iBnd,iSmp:integer;
               sImg:string):tnSgl;
      function xLayerCorrelate(sImg:string):tStringList;
      function _xImageStats(iSmp:integer; sImg:string):tStringList;
      function _xStackCorrelation_(sImg:string):tn2Sgl; //
  end;

var
  Cover:tCover;
  Lines:tLines;
  Points:tPoints;
  Table:tTable;

implementation

uses
  mutual, raster, thema;

{ tLC korreliert den ersten Kanal aus "sImg" mit allen anderen und gibt das
  Ergebnis als Text zurück. tLC ignoriert NoData Pixel }
{ tLC bestimmt den Korrelations-Koeffitienden nach Gauß für alle definieren
  Pixel im Bild. }

// Input=Vorbild-Timeline + Referenz-TimeLine

function tTable._xStackCorrelation_(sImg:string):tn2Sgl; //
const
  cStk = 'tTC: Equal number of value and reference bands required! ';
var
  fPrd:double=0; //Summe der Podukte (∑x*y)
  fRes:single=0; //Ergebnis-Zwischenlager
  fSum,fMus:double; //Summe der Werte ∑x, ∑y
  fSqr,fRqs:double; //Summe der Quadrate ∑x², ∑y²
  fxImg:tn3Sgl=nil; //Zeitreihe Bilddaten + Zeitreihe Referenzen
  iCnt:integer=0; //gültige Pixel
  iStk:integer=0; //Kanäle pro Zeitreihe
  rHdr:trHdr; //Metadaten
  B,X,Y:integer;
  //(∑xy-∑x∑y/n) / sqrt((∑x²-(∑x)²/n)*(∑y²-(∑y)²/n))
begin
  Header.Read(rHdr,sImg); //Metadaten
  if rHdr.Stk mod 2 > 0 then Tools.ErrorOut(3,cStk+sImg); //rHdr.Stk muss durch 2 teilbar sein
  fxImg:=Image.Read(rHdr,sImg); //Messung + Referenz
  Result:=Tools.Init2Single(rHdr.Lin,rHdr.Scn,dWord(NaN));

  iStk:=rHdr.Stk div 2; //Kanäle pro Zeireihe
  for Y:=0 to pred(rHdr.Lin) do
    for X:=0 to pred(rHdr.Scn) do
    begin
      fPrd:=0; fSum:=0; fMus:=0; fSqr:=0; fRqs:=0; iCnt:=0; //Vorgabe
      for B:=0 to pred(iStk) do
      begin
        if isNan(fxImg[B,Y,X]) or isNan(fxImg[B+iStk,Y,X]) then continue;
        fSum+=fxImg[B,Y,X]; //Werte (Summe)
        fMus+=fxImg[B+iStk,Y,X];
        fPrd+=fxImg[B,Y,X]*fxImg[B+iStk,Y,X]; //Produkt (Summe)
        fSqr+=sqr(fxImg[B,Y,X]); //Quadrate (Summe)
        fRqs+=sqr(fxImg[B+iStk,Y,X]);
        inc(iCnt); //gültige Pixel
      end;
      fRes:=(fSqr-sqr(fSum)/iCnt)*(fRqs-sqr(fMus)/iCnt);
      if fRes>0 then Result[Y,X]:=(fPrd-fSum*fMus/iCnt)/sqrt(fRes);
    end;
  Image.WriteBand(Result,0,eeHme+'time-corl');
  Header.WriteScalar(rHdr,eeHme+'time-corl');
  Tools.HintOut(true,'table.TimeCorrelation: '+'time-corl')
end;

{ cPS zählt die Kanäle in "sImg"}

function tCover._BandCount_(sImg:string):integer;
var
  bCnt:boolean=False;
  slInf:tStringList=nil;
  I:integer;
begin
  Result:=0;
  try
    slInf:=TStringList.Create;
    slInf.AddText(Gdal.ImageInfo(sImg)); //GDAL-Info übernehmen
    for I:=0 to pred(slInf.Count) do
    begin
      if bCnt then
        if LeftStr(slInf[I],7)='  Band_'
          then inc(Result)
          else break;
      if LeftStr(slInf[I],9)='Metadata:' then
        bCnt:=True;
    end;
  finally
    slInf.Free
  end;
end;

{ cPS übernimmt die Pixelgröße aus dem GDAL ImageInfo Text }

function tCover._PixelSize_(sImg:string):single;
var
  slInf:tStringList=nil;
  I:integer;
  qS:string;
begin
  try
    slInf:=TStringList.Create;
    slInf.AddText(Gdal.ImageInfo(sImg)); //GDAL-Info übernehmen
    for I:=pred(slInf.Count) downto 0 do
      if LeftStr(slInf[I],14)='Pixel Size = (' then
      begin
        qS:=ExtractWord(2,slInf[I],['(',',']);
        if not TryStrToFloat(ExtractWord(2,slInf[I],['(',',']),Result) then
          Result:=0;
        break; //fertig
      end;
  finally
    slInf.Free
  end;
end;

{ cFH überträgt die Koordinaten aus einem Frame in einen IDL-Header.
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
  if iaPix=nil then Tools.ErrorOut(2,cPix);
  Result:=Tools.InitSingle(length(iaPix),dWord(Nan)); //Vorgabe
  iCnt:=rHdr.Scn*rHdr.Lin; //Anzahl Pixel
  fxBnd:=Image.ReadBand(0,rHdr,sBnd); //Bildkanal
  for I:=0 to high(iaPix) do
    if (iaPix[I]>=0) and (iaPix[I]<iCnt) then
      with rHdr do
        Result[I]:=fxBnd[iaPix[I] div Scn,iaPix[I] mod Scn];
  Tools.HintOut(true,'Points.Attributes: menory');
end;

function tLines._CoordinatesGML_(
  iaNxt:tnInt; //Zellindex der verknüpften Zelle
  iaPix:tnInt; //Pixelindex des Minimums jeder Zelle
  var rHdr:trHdr): //Metadaten
  tarFrm; //Koordinaten für zwei Punkte = [Lft,Top] – [Rgt,Btm]
{ lC konvertiert Pixel-Koordinaten in geographische Koordinaten und gibt sie
  als "tBox" zurück. lC übernimmt die Projektion aus dem ENVI-Header in "rHdr".
  Die Koordinaten im ENVI-Header beziehen sich auf die linke untere Ecke des
  Bilds. lC korrigiert auf Pixelmitte. lC ist auf Linien aus zwei Punkten
  spezialisiert. }
var
  fLat,fLon:single; //Kordinaten-ursprung für Pixelmitte
  Z:integer;
begin
  SetLength(Result,length(iaPix));
  fLon:=rHdr.Lon+0.5*rHdr.Pix; //Ursprung auf Pixelmitte
  fLat:=rHdr.Lat-0.5*rHdr.Pix;
  for Z:=1 to high(iaPix) do
    if iaNxt[Z]>0 then
    begin
      Result[Z].Lft:=fLon+(iaPix[Z] mod rHdr.Scn)*rHdr.Pix; //Geo-Koordinaten aus Pixelkoordinaten
      Result[Z].Top:=fLat-(iaPix[Z] div rHdr.Scn)*rHdr.Pix;
      Result[Z].Rgt:=fLon+(iaPix[iaNxt[Z]] mod rHdr.Scn)*rHdr.Pix;
      Result[Z].Btm:=fLat-(iaPix[iaNxt[Z]] div rHdr.Scn)*rHdr.Pix;
    end
    else Result[Z]:=crFrm; //Vorgabe
end;

function tLines._BoundingBoxGML_(raLnk:tarFrm):trFrm;
{ lBB bestimmt ein einschließendes Rechteck aus allen Koordinaten in "raLnk" }
const
  cMax:single=MaxInt;
var
  Z:integer;
begin
  Result:=crFrm; //Vorgabe, nicht definiert
  for Z:=1 to high(raLnk) do
    if raLnk[Z].Lft<cMax then
    begin
      Result.Lft:=min(raLnk[Z].Lft,Result.Lft);
      Result.Top:=max(raLnk[Z].Top,Result.Top);
      Result.Rgt:=max(raLnk[Z].Rgt,Result.Rgt);
      Result.Btm:=min(raLnk[Z].Btm,Result.Btm);
    end;
end;

procedure tLines._RunOffGML_( //==> mit "resistance" = RST verknüpft
  iCrs:integer; //EPSG-Code
  sGml:string); //Dateiname
{ lRO überträgt Abfluss-Attribute aus "index.bit" in Linien-Polygone aus
  jeweils zwei Punkten. Attribute sind eine fortlaufende ID und der Abfluss.
  lRO bietet keine Format-Altenativen. }
const
  cGml = 'Impossible to create file ';
var
  dGml: TextFile; //Initialisierung
  faWgt:tnSgl=nil; //scalares Attribut
  iaNxt:tnInt=nil; //Pixelindex der verknüpften Zelle
  iaPix:tnInt=nil; //Pixelindex der Zelle
  raLnk:tarFrm=nil; //Punkt-Verknüpfungen
  iFtr:integer=0; //fortlaufende Nummer
  rFrm:trFrm; //Bounding-Box
  rHdr:trHdr; //Metadaten Zellindex
  sCrs:string=''; //EPSG-Code ausgeschrieben
  Z:integer;
begin
  iaPix:=tnInt(Tools.BitExtract(0,eeHme+cfIdx)); //Quelle-Pixelindex
  iaNxt:=tnInt(Tools.BitExtract(1,eeHme+cfIdx)); //Senke-Zellindex
  faWgt:=Tools.BitExtract(4,eeHme+cfIdx); //Attribut
  Header.Read(rHdr,eeHme+cfIdx); //Pixelgröße, Koordinaten
  sCrs:='"EPSG:'+IntToStr(iCrs)+'"'; //ausgeschrieben
  sGml:=ExtractFileName(ChangeFileExt(sGml,'')); //ohne Pfad, ohne Extension
  raLnk:=_CoordinatesGML_(iaNxt,iaPix,rHdr); //Geo-Koordinaten aus Pixelindices
  rFrm:=_BoundingBoxGML_(raLnk); //Bounding-Box
  try
    AssignFile(dGml,eeHme+sGml+cfGml);
    {$i-} Rewrite(dGml); {$i+}
    if IOResult<>0 then Tools.ErrorOut(2,cGml+sGml);
    writeln(dGml,'<?xml version="1.0" encoding="utf-8" ?>');
    writeln(dGml,'<ogr:FeatureCollection');
    writeln(dGml,'     xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"');
    writeln(dGml,'     xsi:schemaLocation="http://ogr.maptools.org/ gml_test.xsd"');
    writeln(dGml,'     xmlns:ogr="http://ogr.maptools.org/"');
    writeln(dGml,'     xmlns:gml="http://www.opengis.net/gml">');
    writeln(dGml,'  <gml:boundedBy>');
    writeln(dGml,'    <gml:Box>');
    writeln(dGml,'      <gml:coord><gml:X>'+FloatToStr(rFrm.Lft)+'</gml:X>'+
      '<gml:Y>'+FloatToStr(rFrm.Top)+'</gml:Y></gml:coord>');
    writeln(dGml,'      <gml:coord><gml:X>'+FloatToStr(rFrm.Rgt)+'</gml:X>'+
      '<gml:Y>'+FloatToStr(rFrm.Btm)+'</gml:Y></gml:coord>');
    writeln(dGml,'    </gml:Box>');
    writeln(dGml,'  </gml:boundedBy>');
    for Z:=1 to high(iaPix) do
      if iaNxt[Z]>0 then
      begin
        writeln(dGml,'  <gml:featureMember>');
        writeln(dGml,'    <ogr:'+sGml+' fid="'+sGml+'.'+IntToStr(iFtr)+'">');
        writeln(dGml,'      <ogr:geometryProperty>'+'<gml:MultiLineString '+
          'srsName='+sCrs+'><gml:lineStringMember><gml:LineString>'+
          '<gml:coordinates>'+
          FloatToStr(raLnk[Z].Lft)+','+FloatToStr(raLnk[Z].Top)+#32+
          FloatToStr(raLnk[Z].Rgt)+','+FloatToStr(raLnk[Z].Btm)+
          '</gml:coordinates></gml:LineString></gml:lineStringMember>',
          '</gml:MultiLineString></ogr:geometryProperty>');
        writeln(dGml,'      <ogr:id>'+IntToStr(succ(iFtr))+'</ogr:id>');
        writeln(dGml,'      <ogr:flow>'+FloatToStr(faWgt[Z])+'</ogr:flow>');
        writeln(dGml,'    </ogr:'+sGml+'>');
        writeln(dGml,'  </gml:featureMember>');
        inc(iFtr); //fortlaufend zählen
      end;
    writeln(dGml,'</ogr:FeatureCollection>');
  finally
    Flush(dGml);
    CloseFile(dGml);
  end; //of try ..
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
    if IOResult<>0 then Tools.ErrorOut(2,cCsv+eeHme+cfVct);
    readln(dCsv,sLin); //Zeile mit Feldnamen
    if ExtractDelimited(1,sLin,[','])<>'WKT' then
      Tools.ErrorOut(2,cCsv+eeHme+cWkt);
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
      else Tools.ErrorOut(2,cPnt+eeHme+cfVct);
    until eof(dCsv);
    SetLength(Result,nRes);
  finally
    CloseFile(dCsv);
  end; //of try ..
  Tools.HintOut(true,'Points.GetIndex: memory');
end;

procedure tPoints.PixMask(
  iaPix:tnInt; //Pixelindices als Array
  sTmp:string); //Vorbild für Geometrie
{ fPM erzeugt eine Maske aus Pixelindices und gibt sie als Klassen-Bild zurück }
var
  iCnt:integer; //Anzahl Pixel
  ixMsk:tn2Byt=nil; //Raster-Maske Testpunkte
  rHdr:trHdr;
  I:integer;
begin
  Header.Read(rHdr,sTmp);
  ixMsk:=Tools.Init2Byte(rHdr.Lin,rHdr.Scn);
  iCnt:=rHdr.Lin*rHdr.Scn; //Anzahl Pixel
  for I:=0 to high(iaPix) do
    if (iaPix[I]>=0) and (iaPix[I]<iCnt) then //nur innerhalb der Bildfläche
      ixMsk[iaPix[I] div rHdr.Scn,iaPix[I] mod rHdr.Scn]:=1;
  Header.WriteThema(1,rHdr,'',eeHme+cfMsk);
  Image.WriteThema(ixMsk,eeHme+cfMsk);
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
  if LeftStr(sFld,3)<>'WKT' then Tools.ErrorOut(2,cWkt+eeHme+cfVct);
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
    if IOResult<>0 then Tools.ErrorOut(2,cCrt+eeHme+cfVct);
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
    slFtr.AddText(Tools.CommaToLine(sFtr)); //ausgewählte lokale Bilder
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
    Tools.ErrorOut(2,cFld);
  //length(slPrc)=length(fxVal)?
  //Anzahl Zeilen <> Länge Attribut-Arrays
  try
    AssignFile(dCsv,eeHme+cfVct); //Test-Punkte als CSV
    {$i-} Reset(dCsv); {$i+}
    if IOResult<>0 then Tools.ErrorOut(2,cCsv+eeHme+cfVct);

    AssignFile(dFcs,eeHme+cfFcs); //Punkte mit ergänzten Attributen
    {$i-} Rewrite(dFcs); {$i+}
    if IOResult<>0 then Tools.ErrorOut(2,cFcs+eeHme+cfFcs);

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
    if length(fxVal[0])<>iRcd then Tools.ErrorOut(2,cRcd);
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
    Tools.ErrorOut(2,cGeo+eeHme+cfVct);
  fxVal:=AttribValues(sFtr); //Attribute an gewählten Punkten
  PointAppend(fxVal,sFtr); //Feldnamen + Attribute im CSV erweitern
  FormatAppend(high(fxVal)); //Formatangaben als CSVT erweitern
  //Gdal.ExportShape(False,iCrs,eeHme+cfFcs+'.csv',sTrg); //als ESRI-Shape speichern
  Gdal.ExportShape(iCrs,0,eeHme+cfFcs,eeHme+ChangeFileExt(cfFcs,'.shp')); //als ESRI-Shape speichern
  Tools.HintOut(true,'Points.Attributes: '+cfFcs)
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
    if IOResult<>0 then Tools.ErrorOut(2,cCsv+eeHme+cfVct);

    AssignFile(dFcs,eeHme+cfFcs); //Geometrie mit Attributen
    {$i-} Rewrite(dFcs); {$i+}
    if IOResult<>0 then Tools.ErrorOut(2,cFcs+eeHme+cfFcs);

    readln(dCsv,sLin); //bestehene Feldnamen
    if sLin<>'WKT,DN' then Tools.ErrorOut(2,cAtr);
    writeln(dFcs,'WKT,DN,'+DelSpace(sFtr)); //erweiterte Feldnamen
    repeat
      readln(dCsv,sLin); //bestehene Werte
      iRcd:=rPos(',',sLin); //letztes Komma
      iRcd:=StrToInt(copy(sLin,iRcd+2,length(sLin)-iRcd-2)); //Record-ID
      if iRcd>high(fxVal[0]) then Tools.ErrorOut(2,cRcd+IntToStr(iRcd));
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

procedure tPoints.DefaultFormat(iFtr:integer);
{ pDF erzeugt eine CSVT-Datei für eine WKT-Geometrie mit Attributen. Die "DN"
  für die Datensätze ist "integer", alle Attribute sind "real". pDF speichert
  das Ergebnis als "focus.csvt". }
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
  Tools.TextOut(eeHme+ChangeFileExt(cfFcs,'csvt'),sFmt);
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
  if not FileExists(eeHme+cfVct) then Tools.ErrorOut(2,cVct+eeHme+cfVct);
  sFtr:=Header.ReadLine('field names =',eeHme+cfIdx); //Liste mit Feldnamen (CSV)
  fxVal:=Tools.BitRead(eeHme+cfIdx); //aktuelle Zellindex-Attribute als Matrix
  if WordCount(sFtr,[','])<>length(fxVal) then Tools.ErrorOut(2,cVal);
  ValueAppend(fxVal,sFtr); //"vector.csv" mit Attributen als "focus.csv" speichern
  DefaultFormat(length(fxVal)); //CSVT-Datei für Attribute
  Tools.HintOut(true,'Points.Attributes: '+cfFcs);
end;

function tTable.FieldValues(sFld:string):tStringList;
{ tFV gibt alle Werte aus dem Feld "sFld" in der Tabelle von "vector.csv" als
  String-Liste zurück. tFV unterstellt, dass "vector.csv" Polygone im WKT-
  Format enthält. }
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
    if IOResult<>0 then Tools.ErrorOut(2,cCsv+eeHme+cfVct);
    readln(dCsv,sLin); //Zeile mit Feldnamen
    if ExtractDelimited(1,sLin,[','])<>'WKT' then
      Tools.ErrorOut(2,cCsv+eeHme+cWkt);
    for I:=2 to WordCount(sLin,[',']) do
      if ExtractDelimited(I,sLin,[','])=sFld then iCol:=pred(I); //Spalte mit Feldnamen ohne "WKT"
    if iCol<1 then Tools.ErrorOut(2,cFld+sFld);
    repeat
      readln(dCsv,sLin); //ab zweite Zeile = Inhalte
      iWkt:=PosEx('"',sLin,2); //Position zweites Doppelhochkomma
      Delete(sLin,1,succ(iWkt)); //Polygon-Teil + Komma entfernen
      Result.Add(trim(ExtractDelimited(iCol,sLin,[',']))); //Bezeichner ohne Leerzeichen
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
    if IOResult<>0 then Tools.ErrorOut(2,cCsv+eeHme+cfVct);

    AssignFile(dFcs,eeHme+cfFcs); //Geometrie mit Attributen
    {$i-} Rewrite(dFcs); {$i+}
    if IOResult<>0 then Tools.ErrorOut(2,cFcs+eeHme+cfFcs);

    readln(dCsv,sLin); //bestehene Feldnamen
    if LeftStr(sLin,3)<>'WKT' then Tools.ErrorOut(2,cAtr);
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
  Tools.HintOut(true,'Table.AddInteger: '+cfFcs);
end;

procedure tTable.AddFormat(sFmt:string);
{ tAF erweitert die CSVT-Tabelle um den Eintrag "sFmt". "vector.csvt" muss
  existieren. tAF schreibt nach "focus.csvt". }
const
  cVct = 'Vector format definition vector.CSVT needed!';
begin
  if not FileExists(ChangeFileExt(eeHme+cfVct,'.csvt')) then
    Tools.ErrorOut(2,cVct+ChangeFileExt(eeHme+cfVct,'.csvt'));
  if sFmt[1]<>',' then sFmt:=','+sFmt;
  sFmt:=Tools.LineRead(ChangeFileExt(eeHme+cfVct,'.csvt'))+sFmt;
  Tools.TextOut(eeHme+ChangeFileExt(cfFcs,'.csvt'),sFmt);
end;

function tTable.AddThema(sFld:string):tStringList;
{ tAT erzeugt aus dem Feld "sFld" in "vector.csv" ein Array mit Klassen-IDs für
  die Inhalte von "sFld", ergänzt damit die Tabelle und speichert das Ergebnis
  als "focus.csv". }
{ tAT betrachtet die Inhalte von "sFld" als Strings und vergibt für jedes
  Muster eine Klassen-ID. Dazu kopiert tAT die ursprüngliche Liste, sortiert
  sie und reduziert gleiche Einträge bis von jedem Muster nur noch ein Beispiel
  übrig bleibt. tAT verwendet den Index der reduzierten Liste als Klassen-ID,
  trägt die IDs in ein neues Integer-Array ein und ergänzt das Array als neues
  Feld für "focus.csv". tAT erweitert auch die CSVT-Datei und übernimmt die
  PRJ-Datei aus "vector.prj" }
var
  iaMap:tnInt=nil; //Klassen-IDs
  slFld:tStringList=nil; //Klassen-Bezeichner, alle Polygone
  I:integer;
begin
  Result:=tStringList.Create; //klassifizierte Bezeichner
  try
    slFld:=FieldValues(sFld); //Klassen-Bezeichner, alle Polygone
    Result.AddStrings(slFld); //Liste kopieren
    Result.Add(#32); //leere Klasse für Rückweisung, ID=0
    Result.Sort; //alphabetisch
    for I:=pred(Result.Count) downto 1 do
      if Result[I]=Result[pred(I)] then Result.Delete(I); //nur verschiedene Bezeichner
    iaMap:=Tools.InitInteger(slFld.Count,0); //Klassen-IDs als Array
    for I:=0 to pred(slFld.Count) do
      iaMap[I]:=Result.IndexOf(slFld[I]); //Index des Bezeichners = Klassen-ID
    Table.AddInteger(iaMap,sFld+'-ID'); //Werte an Tabelle anhängen
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
  if FileExists(sFrm)=False then Tools.ErrorOut(2,cFrm+sFrm);
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

function tCover.MergeFrames(
  slImg:tStringList): //Bildnamen
  trFrm; //Rechteck + Projektion
{ cMF gibt einen Rahmen zurück, der alle Bilder in "slImg" aufnimmt. cMF
  verwendet dazu die Header-Information der einzelnen Kanäle.
  ==> Das Koordinatensystem aller Bilder muss gleich sein. }
const
  cCrs = 'cMF: Inport images must share coordinate system!';
  cPix = 'cMF: Image merge needs identical pixel size!';
var
  I:integer;
  rHdr:trHdr; //Metadaten Bilder
  sCrs:string=''; //Bezeichner des Koordinatensystems aus Header
begin
  Result:=crFrm; //Vorgabe
  for I:=0 to pred(slImg.Count) do
  begin
    Header.Read(rHdr,slImg[I]);
    if I=0 then
      sCrs:=ExtractWord(2,rHdr.Cys,['"']) //CRS-Zusammenfassung
    else if ExtractWord(2,rHdr.Cys,['"'])<>sCrs then
      Tools.ErrorOut(2,cCrs);
    with Result do
    begin
      Lft:=min(rHdr.Lon,Lft);
      Top:=max(rHdr.Lat,Top);
      Rgt:=max(rHdr.Lon+rHdr.Scn*rHdr.Pix,Rgt);
      Btm:=min(rHdr.Lat-rHdr.Lin*rHdr.Pix,Btm);
    end;
  end;
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
  if RightStr(sArc,14)<>'_02_T1_MTL.txt' then Tools.ErrorOut(2,cArc);
  SetLength(Result,5); //geschlossenes Polygon
  try
    slInf:=tStringList.Create;
    slInf:=Archive.ExtractFilter(sArc,'_MTL'); //aus Archiv extrahieren
    slInf.LoadFromFile(slInf[0]); //MTL vollständig lesen
    for I:=0 to pred(slInf.Count) do
      if slInf[I]='  GROUP = PROJECTION_ATTRIBUTES' then
      begin
        if not (slInf[I+12]='    CORNER_UL_LAT_PRODUCT') then
          Tools.ErrorOut(2,cGeo);
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

{ cCF setzt in "sImg" alle Bildpixel außerhalb des Polygons "sFrm" auf NoData.
  cCF projiziert den Rahmen "sFrm" auf das CRS von "sImg", erzeugt eine Pixel-
  Maske [0,1] mit dem Rahmen und setzt alle Pixel in allen Layern auf NoData
  die außerhalb des Rahmens liegen. }

procedure tCover.ClipToShape(
  sFrm:string; //Geometrie (ROI)
  sImg:string); //Vorbild WIRD VERÄNDERT!
var
  fxImg:tn3Sgl=nil; //Vorbild (multiband)
  fxMsk:Tn2Sgl=nil; //Maske für ROI
  iEpg:integer; //EPSG-Code Vorbild
  rHdr:trHdr; //Metadaten
  B,X,Y:integer;
begin
  iEpg:=Cover.CrsInfo(sImg); //Projektion der Bilddaten
  Gdal.ImportVect(iEpg,sFrm); //Rahmen projizieren + als CSV speichern
  Header.Read(rHdr,sImg); //Bounding Box des Vorbilds
  fxMsk:=Tools.Init2Single(rHdr.Lin,rHdr.Scn,0); //Maske mit Vorgabe = Null
  Image.WriteBand(fxMsk,0,eeHme+cfMsk); //als "mask" für GDAL speichern
  Header.WriteScalar(rHdr,eeHme+cfMsk);
  Gdal.Rasterize(1,'',eeHme+cfMsk,eeHme+cfVct); //ROI als (1) einbrennen

  fxMsk:=Image.ReadBand(0,rHdr,eeHme+cfMsk); //Maske [0,1]
  Header.Read(rHdr,sImg); //Maske hat Header verändert
  fxImg:=Image.Read(rHdr,sImg); //Maske [0,1]
  for Y:=0 to pred(rHdr.Lin) do
    for X:=0 to pred(rHdr.Scn) do
      if fxMsk[Y,X]<1 then //Pixel innerhalb des ROI
        for B:=0 to pred(rHdr.Stk) do
          fxImg[B,Y,X]:=NaN;
  Image.WriteMulti(fxImg,sImg); //Header bleibt gleich
end;

// Ablauf-Punkte zwischen primären Hydrologie-Zonen

{ lDP bestimmt zu jeder primären Zone aus "micro" den Abflusspunkt und schreibt
  ihn nach "runoff.bit" }
{ lDP unterstellt, dass jede Zone in genau eine andere Zone entwässert. Die
  Verknüpfung (Abfluss) der Zonen übernimmt lDP aus dem ersten Feld in
  "runoff.bit". lDP sucht im Höhenmodell "compile" nach dem niedrigsten Punkt
  zwischen je zwei verknüpften Zonen und übernimmt die Koordinaten als "iaLat"
  und "iaLon". lDP konvertiert die erfassten Pixel-Koordinaten in das CRS der
  Zonen aus "micro" und speichert sie als zweites und drittes Feld in
  "runoff.bit". }
{ ==> LDP GREIFT AUSSCHLIEẞLICH AUF DATEIEN IM ARBEITSVERZEICHNIS ZU. }

procedure tLines.xDrainPoints();
var
  faLat:tnSgl=nil; //Latitude Überlauf-Punkt
  faLon:tnSgl=nil; //Longitude Überlauf-Punkt
  faMax:tnSgl=nil; //aktuell niedrigster Überlauf
  fxElv:tn2Sgl=nil; //Höhendaten
  iaLnk:tnInt=nil; //Verknüpfung der primären Zonen
  ixIdx:tn2Int=nil; //primäre Zonen

procedure lLink(
  const iIdx:integer;
  const iLat,iLon:integer;
  const iVrt,iHrz:integer;
  const fLat,fLon:single);
begin
  if (iIdx>0) and (iaLnk[iIdx]=ixIdx[iVrt,iHrz]) then
    if max(fxElv[iLat,iLon],fxElv[iVrt,iHrz])<faMax[iIdx] then
    begin
      faLat[iIdx]:=fLat; //Mitte
      faLon[iIdx]:=fLon; //rechts
      faMax[iIdx]:=max(fxElv[iLat,iLon],fxElv[iVrt,iHrz]);
    end;
end;

const
  fMax:single=MaxInt;
var
  iLin,iScn:integer; //höchste Zeilen/Spalten-ID
  rHdr:trHdr; //Metadaten
  X,Y,Z:integer;
begin
  iaLnk:=tnInt(Tools.BitExtract(0,eeHme+cfRnf)); //Überlauf der elementaren Zonen als IDs
  faLat:=Tools.InitSingle(length(iaLnk),0);
  faLon:=Tools.InitSingle(length(iaLnk),0);
  Header.Read(rHdr,eeHme+cfMic); //Metadaten aus Zonen
  //rHdr.Cnt=high(iaLnk)?
  ixIdx:=tn2Int(Image.ReadBand(0,rHdr,eeHme+cfMic)); //elementare Zonen
  fxElv:=Image.ReadBand(0,rHdr,eeHme+cfCpl); //Höhendaten lesen
  faMax:=Tools.InitSingle(length(iaLnk),dWord(fMax));

  iLin:=high(ixIdx);
  iScn:=high(ixIdx[0]);
  for Y:=0 to high(ixIdx) do
    for X:=0 to high(ixIdx[0]) do
    begin
      if X>0 then lLink(ixIdx[Y,X],Y,X,Y,pred(X),    Y+0.5,X); //nach links
      if Y>0 then lLink(ixIdx[Y,X],Y,X,pred(Y),X,    Y,X+0.5); //oben
      if X<iScn then lLink(ixIdx[Y,X],Y,X,Y,succ(X), Y+0.5,succ(X)); //rechts
      if Y<iLin then lLink(ixIdx[Y,X],Y,X,succ(Y),X, succ(Y),X+0.5); //unten
    end;

  for Z:=1 to high(iaLnk) do
  begin
    faLat[Z]:=rHdr.Lat-faLat[Z]*rHdr.Pix;
    faLon[Z]:=rHdr.Lon+faLon[Z]*rHdr.Pix;
  end;

  Tools.BitInsert(faLat,1,eeHme+cfRnf);
  Tools.BitInsert(faLon,2,eeHme+cfRnf);
end;

{ lLM verbindet primäre Zonen aus der Liste "runoff.bit" durch Linien und
  speichert das Ergebis als "vector.csv". Quelle und Ziel stehen als Zonen-IDs
  im ersten Feld von "runoff.bit", die Koordinaten des Abfluss-Punkts [Lon,Lat]
  im zweiten und dritten Feld. }
{ lLM zeichnet keine Linien innerhalb von Ebenen (Zonen bis zum Index "iPln").
  lLM übernimmt die Beschribung des CRS für die ".PRJ" Datei aus den Metadaten
  von "micro". }

procedure tLines.xDrainLines(iPln:integer); //Anzahl Ebenen
const
  cCsv = 'Impossible to read file: ';
var
  dCsv:TextFile; //Datei
  faLat:tnSgl=nil; //Latitude Überlauf-Punkt
  faLon:tnSgl=nil; //Longitude Überlauf-Punkt
  iaLnk:tnInt=nil; //Verknüpfung der primären Zonen
  iLnk:integer; //verknüpfte Zone
  rHdr:trHdr; //Metadaten
  sLnk:string; //Zwischenlager
  Z:integer;
begin
  //iPln>=0?
  iaLnk:=tnInt(Tools.BitExtract(0,eeHme+cfRnf)); //Überlauf der elementaren Zonen als IDs
  faLat:=Tools.BitExtract(1,eeHme+cfRnf); //Überlauf der elementaren Zonen als IDs
  faLon:=Tools.BitExtract(2,eeHme+cfRnf); //Überlauf der elementaren Zonen als IDs
  Header.Read(rHdr,eeHme+cfMic); //Metadaten aus Zonen
{ TODO: "DrainLines" muss eine PRJ Datei erzeugen, dazu braucht es einen CRS-
        String. Reicht "Cover.CrsInfo"?}
  try
    AssignFile(dCsv,eeHme+cfVct);
    {$i-} Rewrite(dCsv); {$i+}
    if IOResult<>0 then Tools.ErrorOut(2,cCsv+cfVct);
    writeln(dCsv,'WKT,id');
    for Z:=1 to high(iaLnk) do //alle Überlauf-Punkte
      if iaLnk[Z]>iPln then //nur zu definierten Zonen ohne Ebenen
      begin
        iLnk:=iaLnk[Z];
        sLnk:='"MULTILINESTRING (('; //Kopf
        sLnk+=FloatToStr(faLon[Z])+#32; //von..
        sLnk+=FloatToStr(faLat[Z])+',';
        sLnk+=FloatToStr(faLon[iLnk])+#32; //nach..
        sLnk+=FloatToStr(faLat[iLnk])+'))","';
        sLnk+=IntToStr(Z)+'"';
        writeln(dCsv,sLnk);
      end;
  finally
    Flush(dCsv);
    CloseFile(dCsv);
  end; //of try ..
  Tools.TextOut(ChangeFileExt(eeHme+cfVct,'.prj'),rHdr.Cys);
end;

{ cIt gibt die Schnittmenge aus zwei Rahmen zurück }

function tCover.Intersect(
  rImg:trFrm; //Auswahl-Rahmen
  rRoi:trFrm): //Vorbild-Rahmen
  trFrm; //gemeinsamer Rahmen
const
  cEpg = 'cCF: Coordinate systems must be equal!';
  cFrm = 'cCF: No overlap between selected frame an image!';
begin
  if rImg.Epg<>rRoi.Epg then Tools.ErrorOut(2,cEpg);

  Result.Crs:=rRoi.Crs;
  Result.Epg:=rRoi.Epg;
  Result.Lft:=max(rImg.Lft,rRoi.Lft); //gemeinsame Abdeckung
  Result.Top:=min(rImg.Top,rRoi.Top);
  Result.Rgt:=min(rImg.Rgt,rRoi.Rgt);
  Result.Btm:=max(rImg.Btm,rRoi.Btm);
  with Result do
  begin
    if Lft>Rgt then Rgt:=Lft;
    if Top<Btm then Btm:=Top;
  end;
end;

function tCover.ReadFrame(sVct:string):trFrm;
var
  fLat,fLon:double; //Koordinaten als Zahl
  iHig,iLow:integer; //Klammern für Koordinaten
  iPnt:integer=0; //Anzahl Punkte
  sCsv:string=''; //CSV-Inhalt
  sPly:string=''; //nur Koordinaten
  sPnt:string=''; //Punkt-Koordinaten als Text
  P:integer;
begin
  Result:=crFrm; //Vorgane = unmöglich
  Result.Epg:=Cover.CrsInfo(sVct);
  //Result.Crs:= CRS-Info erweitern
  sCsv:=Tools._eTextRead(sVct); //gesamten Text
  sPly:=ExtractWord(2,sCsv,['"']); //erstes Polygon
  iHig:=pos(')',sPly); //erste Klamer ")"
  iLow:=rpos('(',sPly); //letzte Klammer "("
  sPly:=copy(sPly,succ(iLow),pred(iHig-iLow)); //nur Polygon-Inhalt}
  iPnt:=WordCount(sPly,[',']); //Punkte mit je zwei Koordinaten
  for P:=1 to iPnt do
  begin
    sPnt:=ExtractWord(P,sPly,[',']); //zwei Koordinaten als Text
    fLat:=StrToFloat(ExtractWord(2,sPnt,[#32]));
    fLon:=StrToFloat(ExtractWord(1,sPnt,[#32]));
    Result.Lft:=min(fLon,Result.Lft); //äußere Grenze
    Result.Top:=max(fLat,Result.Top);
    Result.Rgt:=max(fLon,Result.Rgt);
    Result.Btm:=min(fLat,Result.Btm);
  end;
end;

// Liste aus Koordinaten als Polygon.csv speihern

procedure tCover.WriteFrame(
  rFrm:trFrm; //Frame
  sPrj:string); //Name passende PRJ-Datei
const
  cFld = 'WKT,id'; //Feldnamen
  cFmt = 'WKT,Integer64(10)'; //Format
var
  sRes:string='';
begin
  sRes:=cFld+#10; //Kopfzeile
  sRes+='"MULTIPOLYGON ((('+
    FloatToStr(rFrm.Lft)+#32+FloatToStr(rFrm.Top)+','+
    FloatToStr(rFrm.Rgt)+#32+FloatToStr(rFrm.Top)+','+
    FloatToStr(rFrm.Rgt)+#32+FloatToStr(rFrm.Btm)+','+
    FloatToStr(rFrm.Lft)+#32+FloatToStr(rFrm.Btm)+','+
    FloatToStr(rFrm.Lft)+#32+FloatToStr(rFrm.Top)+')))",1';
  Tools.TextOut(eeHme+cfFcs,sRes); //als Datei "focus.csv"
  Tools.TextOut(eeHme+ChangeFileExt(cfFcs,'.csvt'),cFmt); //Formate
  if ExtractFileExt(sPrj)='.prj'
    then Tools.CopyFile(sPrj,eeHme+ChangeFileExt(cfFcs,'.prj')) //Vorbild=Datei
    else Tools.TextOut(eeHme+ChangeFileExt(cfFcs,'.prj'),ccPrj); //Geographisch
end;

{ lLF übergibt die WKS-Koordinaten aus der Archiv-Liste als Bounding-Box }

function tLines.LandsatFrame(
  sWkt:string): //Polygon im WKT-Format (aus Archiv-Liste)
  trFrm; //Bounding-Box in Projektion aus "iEpg"
const
  cFld = 'WKT,id'+#10; //Feldnamen
  cFmt = 'WKT,Integer64(10)'; //Format
var
  fLat,fLon:double; //Punkt-Koordinaten als Zahl
  iHig,iLow:integer; //Position innerste Klammern
  iPnt:integer; //Anzahl Punkte im Polygon
  sPly:string; //erstes Polygon im CSV-Text
  sPnt:string; //Punkt-Koordinaten als Text
  P:integer;
begin
  Result:=crFrm; //Vorgabe=unmöglich
  Result.Crs:='WGS 84'; //geographisch
  Result.Epg:=4326; //geographisch
  sPly:=ExtractWord(1,sWkt,['"']); //erstes Polygon
  iHig:=pos(')',sPly); //erste Klamer ")"
  iLow:=rpos('(',sPly); //letzte Klammer "("
  sPly:=copy(sPly,succ(iLow),pred(iHig-iLow)); //nur Koordinaten
  iPnt:=WordCount(sPly,[',']); //Punkte mit je zwei Koordinaten
  for P:=1 to iPnt do
  begin
    sPnt:=ExtractWord(P,sPly,[',']); //zwei Koordinaten als Text
    fLat:=StrToFloat(ExtractWord(2,sPnt,[#32]));
    fLon:=StrToFloat(ExtractWord(1,sPnt,[#32]));
    Result.Lft:=min(fLon,Result.Lft); //äußere Grenze
    Result.Top:=max(fLat,Result.Top);
    Result.Rgt:=max(fLon,Result.Rgt);
    Result.Btm:=min(fLat,Result.Btm);
  end;
end;

{ cVC gibt Projektion und Bounding-Box der Geometrie "sPly" zurück. Mit "iEpg"
  <> Null transformiert cVC die Koordinaten nach "iEpg". Dazu erzeugt cVC eine
  CSV-Datei im neuen Koordinatensystem und übernimmt ihre Werte. }

function tCover.VectorCrsFrame(
  iEpg:integer; //Ziel-Projekton als EPSG-Code, Null für unverändert
  sPly:string): //Geometrie-Quelle
  trFrm; //Bounding-Box + CRS
begin
  if iEpg<>0 then
  begin
    Gdal.ImportVect(iEpg,sPly); //Vektor umprojizieren und als "vector.csv" speichern
    Result:=VectorFrame(eeHme+cfVct) //Rahmen aus umprojizierten Vektoren
  end
  else Result:=VectorFrame(sPly) //Geometrie unverändert übernehmen
end;

{ cRt transformiert die Bounding-Boxen "rImg" und "rRoi" in das System "iEpg",
  bildet neue Bounding-Boxen in diesem System und gibt den Verschnitt der
  transformierten Boxen zurück. Diese Reihenfolge ist notwendig um verschieden
  projizierte Ausschntt verlustlos zu kombinieren
  ==> CRT UNTERSTELLT, DASS "RIMG" UND "RROI" GEOGRAPHISCH SIND }

function tCover.Rotate(
  iEpg:integer; //Projektion im Ziel
  rImg,rRoi:trFrm): //Rahmen von Bild und Ziel, geographisch
  trFrm; //gemeinsamer Rahmen in neuer Projektion
begin
  Result:=crFrm; //Vorgabe = unmöglich

  Cover.WriteFrame(rImg,ccPrj); //Bildkachel als CVS-Datei
  Gdal.ImportVect(iEpg,eeHme+cfFcs); //Umprojizieren
  rImg:=Cover.ReadFrame(eeHme+cfVct); //transformierte Bounding-Box
{ TODO: Cover.Rotate verwendet Vektor-Transformation im CSV-Format um Punkte
        neu zu projizieren. Punkte können auch direkt umprojiziert werden. }
  Cover.WriteFrame(rRoi,ccPrj); //ROI als CVS-Datei
  Gdal.ImportVect(iEpg,eeHme+cfFcs); //Umprojizieren
  rRoi:=Cover.ReadFrame(eeHme+cfVct); //transformierte Bounding-Box

  Result:=Cover.Intersect(rImg,rRoi); //Rahmen verschneiden, projiziert wie Archiv
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
    write(#13,rHdr.Stk-B);
  end;
  if rHdr.Stk>1
    then Header.WriteMulti(rGrd,rHdr.aBnd,sTrg) //sollte so verwendbar sein
    else Header.WriteScalar(rGrd,sTrg);
  Tools.HintOut(true,'Points.GridAttrib: '+ExtractFileName(sTrg));
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
  Tools.HintOut(true,'table.LayerCorrelate: '+eeHme+cfTab)
end;

{ cRF extrahiert die Ursprung, Projektion, Bounding-Box, Pixelgröße und Kanäle
  der Bilddaten "sImg" aus dem "gdalinfo"-Text und gibt sie als "trCvr" zurück.
  Der Ursprung bezeichnet die linke obere Ecke des Bildes. Der Rahmen kann
  leere Bildflächen enthalten. }

function tCover.RasterFrame(sImg:string):trCvr; //Bildname: Abdeckung
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
          Tools.ErrorOut(2,cSqr+sImg); //nur quadratische Pixel!
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
    Tools.ErrorOut(2,cCrs+sImg);
end;

{ cCr erzeugt aus den Envi-Headern aller Bilder in "slImg" einen gemeinsamen
  Rahmen UND gibt "bFit=true" zurück, wenn alle Rahmen dieselbe Größe haben. }

function tCover.xCover(
  var bFit:boolean; //alle Bilder gleiche Größe
  slImg:tStringList): //Namen aller Bilder
  trFrm; //Rahmen für alle Bilder
const
  cImg = 'cCt: Either a polygon or a list of images must be submitted!';
var
  rHdr:trHdr; //Metadaten
  I:integer;
begin
  bFit:=True; //Vorgabe
  if slImg.Count<1 then Tools.ErrorOut(3,cImg);

  Result:=crFrm; //Vorgabe
  for I:=0 to pred(slImg.Count) do //Rahmen aus allen Bildern
  begin
    Header.Read(rHdr,slImg[I]);
    if I>0 then
      if (rHdr.Lon<>Result.Lft) or (rHdr.Lon+rHdr.Scn*rHdr.Pix<>Result.Rgt)
      or (rHdr.Lat<>Result.Top) or (rHdr.Lat-rHdr.Lin*rHdr.Pix<>Result.Btm)
      then bFit:=False; //Rahmen differieren
    Result.Lft:=min(rHdr.Lon,Result.Lft);
    Result.Top:=max(rHdr.Lat,Result.Top);
    Result.Rgt:=max(rHdr.Lon+rHdr.Scn*rHdr.Pix,Result.Rgt);
    Result.Btm:=min(rHdr.Lat-rHdr.Lin*rHdr.Pix,Result.Btm);
  end;
end;

// Raster-Bild Statistik
// Min/Max für Extremwerte
// Hig/Low für 99%/1% Percentil
// Mid/Mea für mean/median

function tTable._BandHist(
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
  Reduce.QuickSort(Result,iHit);
end;

{ lIS bestimmt Minimum und Maximum, 1%-Percentile, Mittelwert und Median sowie
  ein cumuliertes Histogramm aus den Bilddaten von "sImg" und gibt das Ergebnis
  als Text zurück. lIS ignoriert Nodata Pixel. }
{ lIS nimmt "iSmp" regelmäßig vereilte Stichproben aus dem Bild. lIS zählt die
  die gültigen Proben in "iHit" und summiert ihre Werte in "fSum". Für die
  Statistik sortiert lIS die Liste und bildet die Kenwerte }

function tTable._xImageStats(
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
    faVal:=_BandHist(fSum,iHit,B,iSmp,sImg); //iHit wird gesetzt

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
    Tools.HintOut(true,'table.ImageStats '+IntToStr(succ(B))); //Fortschritt
  end;
  Result.SaveToFile(eeHme+cfTab)
end;

end.

{==============================================================================}

{ lIS bestimmt Minimum und Maximum, 1%-Percentile, Mittelwert und Median sowie
  ein cumuliertes Histogramm aus den Bilddaten von "sImg" und gibt das Ergebnis
  als Text zurück. lIS ignoriert Nodata Pixel. }
{ lIS nimmt "iSmp" regelmäßig vereilte Stichproben aus dem Bild. lIS zählt die
  die gültigen Proben in "iHit" und summiert ihre Werte in "fSum". Für die
  Statistik sortiert lIS die Liste und bildet die Kenwerte }

function tTable._xImageStats(
  iSmp:integer; //Stichproben
  sImg:string): //Vorbild
  tStringList; //Ergebnis als Text
const
  cHst=64; //Stützpunkte im Histogramm
var
  faVal:tnSgl=nil; //Stichproben
  fDff:single; //Maximale Werte-Differenz
  fPrt:single=1; //Stichproben-Distenz
  fSum:double=0; //Summe aller Werte
  fxBnd:tn2Sgl=nil; //Kanal aud "sImg"
  iaHst:tnInt=nil; //Anzahl Treffer
  iCol,iRow:integer; //Pixel-Position
  iHit:integer; //gültige Pixel
  iLow,iPrd:integer; //Stichproben-Intervall für Histogramm
  rHdr:trHdr; //Metadaten
  B,I:integer;
begin
  Result:=tStringList.Create;
  Header.Read(rHdr,sImg); //Metadaten Zellindex
  SetLength(faVal,min(iSmp,rHdr.Lin*rHdr.Scn));
  fPrt:=rHdr.Lin*rHdr.Scn/iSmp; //Stichproben-Distanz in Pixeln
  for B:=0 to pred(rHdr.Stk) do //Kanäle einzeln
  begin
    fxBnd:=Image.ReadBand(B,rHdr,sImg);
    fSum:=0; //Vorgabe
    iHit:=0; //Vorgabe
    for I:=0 to high(faVal) do
    begin
      iCol:=trunc(fPrt*I) mod rHdr.Scn;
      iRow:=trunc(fPrt*I) div rHdr.Scn;
      if isNan(fxBnd[iRow,iCol]) then continue;
      faVal[iHit]:=fxBnd[iRow,iCol];
      fSum+=faVal[iHit];
      inc(iHit)
    end;
    Reduce.QuickSort(faVal,iHit);
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
    Tools.HintOut(true,'table.ImageStats '+IntToStr(succ(B))); //Fortschritt
  end;
  Result.SaveToFile(eeHme+cfTab)
end;

{ cCr transformiert das Polygon aus "sFrm" in einen Rahmen mit der Projektion
  "iEpg". Wenn kein Rahmen angegeben wurde, bildet cCr einen Rahmen aus allen
  Bildern in "slImg" }

function tCover.xCover_(
  iEpg:integer; //Koordinatensystem als EPSG-Code ODER leer für unverändert
  sFrm:string; //Rahmen für alle Ausschnitte ODER leer für "alles verwenden"
  slImg:tStringList): //Namen aller Bilder
  trFrm; //Rahmen für alle Bilder
const
  cImg = 'cCt: Either a polygon or a list of images must be submitted!';
var
  rHdr:trHdr; //Metadaten
  I:integer;
begin
  if length(sFrm)>0 then
    Result:=Cover.VectorCrsFrame(iEpg,sFrm) //Rahmen aus Eingabe
  else if slImg.Count>0 then
  begin
    Result:=crFrm; //Vorgabe
    for I:=0 to pred(slImg.Count) do //Rahmen aus allen Bildern
      begin
        Header.Read(rHdr,slImg[I]);
        Result.Lft:=min(rHdr.Lon,Result.Lft);
        Result.Top:=max(rHdr.Lat,Result.Top);
        Result.Rgt:=max(rHdr.Lon+rHdr.Scn*rHdr.Pix,Result.Rgt);
        Result.Btm:=min(rHdr.Lat-rHdr.Lin*rHdr.Pix,Result.Btm);
      end;
  end
  else Tools.ErrorOut(3,cImg);
end;

