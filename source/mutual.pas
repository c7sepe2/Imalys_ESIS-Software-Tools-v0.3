unit mutual;

{ MUTUAL sammelt Wrapper, die von allen Routinen verwendet werden können und
  Routinen die Kanäle, Referenzen, Attribute und Flächen vereinigen und
  vergleichen.

  ARCHIVE:  listet, extrahiert und kombiniert Dateien aus Archiven
  GDAL:     sammelt und kapselt Aufrufe für GDAL Routinen aller Art
  RANK:     korreliert Verteilungen und verknüpft Referenzen mit Clustern
  SEPARATE: bestimmt Hauptkomponenten und segmentiert den Merkmalsraum

  BEGRIFFE MIT SPEZIFISCHER ANWENDUNG:
  Gravity: Schwerpunkt einer Kachel oder Fläche }

{$mode objfpc}{$H+}

interface

uses
  Classes, Math, StrUtils, SysUtils, format;

type
  taIdf = array[0..3] of integer; //Position der Identifier in Dateinamen

  trPrd = record //Outlier in Zeitreihen
    Mea:double; //Mittelwert
    Vrz:double; //quadrierte Abweichung vom Mittelwert
    Low,Hig:integer; //Zeit-Intervall + Trenner als Indices [0..N]
  end;

  tr_Prd = record //Outlier in Zeitreihen
    Low,Hig:word; //Zeit-Intervall + Trenner als Indices [0..N]
    Prv:double; //Varianz/Mittelwert zum Vorgänger (previous)
    Nxt:double; //Varianz/Mittelwert zum Nachfolger (subsequent)
  end;
  tra_Prd = array of tr_Prd;
  tpr_Prd = ^tr_Prd;

  tArchive = class(tObject) //checked 251121
    private
      function CheckItem(sNme:string; iPst:integer; slItm:tStringList):boolean;
      function CheckPeriod(sNme:string; iPst,iLow,iHig:integer):boolean;
      function EqualStack(iStk:integer; slImg:tStringList):boolean;
      function _GetNadir_(sXml:string):single;
      procedure GetPeriod(var iHig,iLow:integer; sPrd:string);
      function ShortName(aIdf:taIdf; sNme:string):string;
      function TarContent(sArc:string; slMsk:tStringList):tStringList;
      function TarExtract(sArc:string; slNme:tStringList):string;
      function ValidDate(slImg:tStringList):boolean;
    public
      function ExtractFilter(sArc,sBnd:string):tStringList;
      function xBandsImport(bQlt:boolean; iSns:integer; sFrm,sDir:string):string;
      function xSelectLandsat(bQlt:boolean; sArc,sFrq,sPrd,sTls:string):tStringList;
      function xSelectSentinel(sArc,sFrq,sPrd,sTls:string):tStringList;
      procedure xMergeBands(sTrg:string; slImg:tStringList);
      procedure xImageImport(iEpg,iPix:integer; sArt,sFrm:string; slImg:tStringList);
      function xQualityMask(sFrm,sDir:string):single;
    end;

  tGdal = class(tObject) //checked 251121
    private
      procedure Warp(iCrs,iPix:integer; rFrm:trFrm; sImg,sTrg:string);
    public
      procedure ExportShape(iPrj,iWrp:integer; sSrc,sTrg:string);
      procedure ExportTo(iBnd,iFmt:integer; sNme,sRes:string);
      procedure Hillshade(sDem:string);
      function ImageInfo(sImg:string):string;
      procedure ImportVect(iPrj:integer; sGeo:string);
      function OgrInfo(sVct:string):string;
      procedure Rasterize(iVal:integer; sAtr,sBnd,sVct:string);
      function SrsInfo(sImg:string):string;
      procedure Translate(iSgl,iHig,iLow:integer; rFrm:trFrm; sImg,sTrg:string);
      procedure ZonalBorders(sIdx:string);
  end;

  tRank = class(tObject) //checked 250913
    private
      procedure _AccuracyMask_(sRfz,sThm:string);
      function _Correlation(iMap,iRfz:integer; ixMap,ixRfz:tn2Byt):tn2Sgl;
      function Distribution(iAtr,iMap,iSmp:integer; ixRfz:tn2Byt):tn2Sgl;
      procedure _SortByte_(iMap:integer; ixMap:tn2Byt);
      function _Spearmann_(fxVal:tn2Sgl):tn2Sgl;
    public
      function Percentil(faVal:tnSgl; fLmt:single; iHig:integer):single;
      procedure _xCorrelation();
      procedure xDistribution(iSmp:integer);
      procedure xVectorReference(sFld,sVct:string);
  end;

  tSeparate = class(tObject)
    private
      fcPrd:double; //fxBnd*fxCmp product (∑xy)
      fcHrz:double; //fxBnd sum (∑x)
      fcVrt:double; //fxCnt sum (∑y)
      fcSqr:double; //fxCnt square (∑y²)
      icCnt:int64; //pixel > fNod
      rcHdr:trHdr; //Metadaten
      procedure _Equalize_(fFct:single; fxBnd:tn2Sgl);
      procedure Regression(fxBnd,fxCmp:Tn2Sgl);
      procedure Rotation(fxBnd,fxCmp:Tn2Sgl);
    public
      procedure xPrincipal(iDim:integer; sImg:string);
  end;

const
  //Zuordnung: Sensor - Kachel - Frequenz - Datum
  cmImd:taIdf = (1,2,3,4); //Imalys extrahierte Kanäle
  cmLst:taIdf = (1,3,9,4); //Landsat Archive & Kanal-Namen
  cmS2a:taIdf = (1,6,0,3); //Sentinel-2 Archive
  cmS2b:taIdf = (0,1,3,2); //Sentinel-2 Kanal-Namen

var
  Archive:tArchive;
  Gdal:tGdal;
  Rank:tRank;
  Separate:tSeparate;

implementation

uses
  index, raster, thema, vector;

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

function DateBand(sl:tStringList; i1,i2:integer):integer;
var
  iHig,iLow:integer;
begin
  iLow:=integer(pointer(sl.Objects[i1]));
  iHig:=integer(pointer(sl.Objects[i2]));
  if RightStr(sl[i1],8) < RightStr(sl[i2],8) then Result:=-1 else
  if RightStr(sl[i1],8) > RightStr(sl[i2],8) then Result:=1 else
    if iLow < iHig then Result:=-1 else
    if iLow > iHig then Result:=1 else
      Result:=0
end;

function Right10(sl:tStringList; i1,i2:integer):integer;
begin
  if RightStr(sl[i1],10) < RightStr(sl[i2],10) then Result:=-1 else
  if RightStr(sl[i1],10) > RightStr(sl[i2],10) then Result:=1 else Result:=0
end;

function SpcCount(p1,p2:Pointer):integer;
begin
  if tprRfz(p1)^.Cnt>tprRfz(p2)^.Cnt then Result:=-1 else //nach vorne
  if tprRfz(p1)^.Cnt<tprRfz(p2)^.Cnt then Result:=1 else Result:=0;
end;

function SpcValues(p1,p2:Pointer):integer;
begin
  if tprSpc(p1)^.Val>tprSpc(p2)^.Val then Result:=-1 else //nach vorne
  if tprSpc(p1)^.Val<tprSpc(p2)^.Val then Result:=1 else Result:=0;
end;

function SpcRfzMap(p1,p2:Pointer):integer;
begin
  if tprSpc(p1)^.Rfz<tprSpc(p2)^.Rfz then Result:=-1 else //nach vorne
  if tprSpc(p1)^.Rfz>tprSpc(p2)^.Rfz then Result:=1 else
    if tprSpc(p1)^.Map<tprSpc(p2)^.Map then Result:=-1 else //nach vorne
    if tprSpc(p1)^.Map>tprSpc(p2)^.Map then Result:=1 else Result:=0;
end;

{ sFloat transformiert einen String in das Single-Format und setzt bei Fehlern
  die Variable "bErr". "bErr" kann cumulativ verwendet werden }

function sFloat(var bErr:boolean; sLin:string):single;
begin
  if TryStrToFloat(copy(sLin,succ(rPos('=',sLin)),$FF),Result)=false then
    bErr:=True; //Flag setzen
end;

function _eFloat(var bErr:boolean; sLin:string):single;
begin
  if TryStrToFloat(copy(sLin,succ(rPos('=',sLin)),$FF),Result) then
    bErr:=False; //Fehler-Flag setzen
end;

{ rAM löscht aus dem Klassen-Layer "sThm" alle Pixel, die nicht mit der
  Referenz "sRfz" identisch sind. }

procedure tRank._AccuracyMask_(sRfz,sThm:string);
var
  ixThm:tn2Byt=nil; //Klassen-Bild
  ixRfz:tn2Byt=nil; //Referenzen-Bild
  rHdr:trHdr; //Metadaten
  Y,X:integer;
begin
  Header.Read(rHdr,sThm); //recodierte Cluster
  ixThm:=Image.ReadThema(rHdr,sThm);
  Header.Read(rHdr,sRfz); //Referenz als Raster
  ixRfz:=Image.ReadThema(rHdr,sRfz);
  for Y:=0 to high(ixThm) do
    for X:=0 to high(ixThm[0]) do
      if ixThm[Y,X]<>ixRfz[Y,X] then
        ixThm[Y,X]:=0;
  Image.WriteThema(ixThm,eeHme+cfMap);
  Header.WriteThema(rHdr.Cnt,rHdr,rHdr.Fld,eeHme+cfMap);
end;

{ rCn bestimmt die Rang-Korrelation nach Spearmann für alle Kombinationen aus
  zwei Zeilen in "fxVal" und gibt sie als Tabelle zurück.
    rCn erzeugt eine Rang-Matrix "ixRnk" für alle Zeilen in "fxVal". "ixRnk"
  hat dieselben Zeilen wie "fxVal". Die erste Spalte von "fxVal" ist
  unterdrückt. Als Vorgabe nummeriert rCn jede "ixRnk"-Zeile fortlaufend. rVL
  kopiert einzelne "fxVal"-Zeilen ohne die führende Null nach "faTmp" und
  sortiert sie
  zusammen mit derselben Zeile aus "ixRnk". rVL bestimmt für alle Kombinationen
  aus zwei Zeilen die Korrelation nach Spaermann und gibt sie als Tabelle
  [Referenz][Referenz] zurück. Die erste Zeile und die erste Spalte (Index=0)
  sind nicht definiert }
// Rangkorrelation = 1-6∑(x-y)²/n/(n²-1) (Spearmann) x,y=Rang n=Vergleiche

function tRank._Spearmann_(
  fxVal:tn2Sgl): //Mittelwerte[Referenz][Value]
  tn2Sgl; //Rang-Korrelation, alle Referenz-Kombinationen
var
  faTmp:tnSgl=nil; //Zwischenlager
  fFct:single; //Spearmann-Faktor
  fRes:single; //Zwischenergebnis
  iDim:integer; //Anzahl Kanäle
  iSze:integer; //Byte pro Spektralkombination
  ixRnk:tn2Int=nil; //Werte-Rangfolge, alle Referenzen & Kanäle
  R,S,V:integer;
begin
  Result:=nil;
  SetLength(ixRnk,length(fxVal),high(fxVal[0]));
  for R:=1 to high(ixRnk) do //erste Zeile NICHT definiert!
    for S:=0 to high(ixRnk[0]) do //alle Spalten verwendet!
      ixRnk[R,S]:=succ(S); //fortlaufende Nummer (ab Eins)
  iSze:=high(fxVal[0])*SizeOf(single); //Byte pro "fxVal[?]
  iDim:=high(fxVal[0]); //Anzahl Kanäle
  faTmp:=Tools.InitSingle(iDim,0); //Zwischenlager
  for R:=1 to high(fxVal) do
  begin
    move(fxVal[R,1],faTmp[0],iSze); //Werte ohne erste Spalte
    Reduce.IndexSort(ixRnk[R],faTmp); //"iaRnk" und "faTmp" verändert
  end;
  Result:=Tools.Init2Single(length(fxVal),length(fxVal),0); //alle Referenz-Kombinationen
  fFct:=6/iDim/(sqr(iDim)-1); //Spearmann-Faktor
  for R:=2 to high(fxVal) do //alle Referenz-Referenz-Kombinationen
    for S:=1 to pred(R) do
    begin
      fRes:=0;
      for V:=0 to pred(high(fxVal[0])) do
        fRes+=sqr(ixRnk[R,V]-ixRnk[S,V]); //Summe quadrierte Rang-Differenzen
      Result[S,R]:=1-fFct*fRes; //Rang-Korrelation
    end;
end;

{ sNz normalisiert alle Werte im Kanal "fxBnd" auf den Bereich 0.5±S*fFct. "S"
  ist die Standardabweichung. Varianz = (∑x²-(∑x)²/n)/(n-1) }
{ "fFct" MUSS POSITIV SEIN }

procedure tSeparate._Equalize_(
  fFct:single; //Abweichnung scalieren
  fxBnd:tn2Sgl); //Datenblock (Bild, Array)
var
  fDev:double; //Abweichung (Standard)
  fOfs:double; //Offset für Ergebnis
  fScl:double; //Scalierung für Ergebnis
  fVal:double; //Mittelwert
  X,Y: integer; //Pixel
begin
  fcPrd:=0; fcSqr:=0; fcHrz:=0; fcVrt:=0; icCnt:=0; //Vorgabe
  for Y:=0 to high(fxBnd) do
    for X:=0 to high(fxBnd[0]) do
    begin
      if isNan(fxBnd[Y,X]) then continue;
      fcHrz+=fxBnd[Y,X]; //Summe Werte
      fcSqr+=sqr(fxBnd[Y,X]); //Summe Quadrate
      inc(icCnt); //Summe gültige Pixel
    end; //for X ..
  fVal:=fcHrz/icCnt; //Mittelwert
  fDev:=sqrt((fcSqr-sqr(fcHrz)/icCnt)/pred(icCnt));
  fScl:=1/(fDev*2*fFct); //Abweichung nach oben und unten
  fOfs:=fVal-fDev*fFct; //Mittelwert ohne Abweichung
  for Y:=0 to high(fxBnd) do
    for X:=0 to high(fxBnd[0]) do
      if not isNan(fxBnd[Y,X]) then
        fxBnd[Y,X]:=(fxBnd[Y,X]-fOfs)*fScl;
end;

{ rSB sortiert Klassen-IDs nach ihrer Häufigkeit im Bild. Häufige Klassen
  erhalten kleine IDs. rSB erzeugt eine sortierbare Liste "lsRfz" mit ID und
  Fläche der Klassen, sortiert die Liste und konvertiert die Klassen-IDs. Die
  Liste "iaRfz" macht die Transformation einfacher. }

procedure tRank._SortByte_(
  iMap:integer; //Anzahl Klassen
  ixMap:tn2Byt); //Klassen-Bild
var
  iaRfz:tnByt=nil;
  lsRfz:TFPList=nil; //Zeiger-Liste
  pRfz:tprRfz=nil; //Zeiger auf Klassen-Referenz
  I,X,Y:integer;
begin
  try
    lsRfz:=TFPList.Create;
    lsRfz.capacity:=iMap; //Platz für alle Klassen
    for I:=0 to iMap do
    begin
      new(pRfz);
      pRfz^.Cnt:=0; //Vorgabe
      pRfz^.Map:=I; //fortlaufend
      lsRfz.Add(pRfz);
    end;
    for Y:=0 to high(ixMap) do
      for X:=0 to high(ixMap[0]) do
        inc(tprRfz(lsRfz.Items[ixMap[Y,X]])^.Cnt); //Pixel pro Klasse
    lsRfz.Delete(0); //Rückweisung + leere Flächen löschen
    lsRfz.Sort(@SpcCount); //nach Fläche sortieren
    iaRfz:=Tools.InitByte(succ(iMap)); //Vorgabe, Null
    for I:=0 to pred(lsRfz.Count) do //alle gültigen Klassen
      iaRfz[tprRfz(lsRfz.Items[I])^.Map]:=succ(I); //neue Klassen-IDs
  finally
    for I:=0 to pred(lsRfz.Count) do
      dispose(TprRfz(lsRfz[I])); //Speicher freigeben
    lsRfz.Free;
  end;
  for Y:=0 to high(ixMap) do
    for X:=0 to high(ixMap[0]) do
      ixMap[Y,X]:=iaRfz[ixMap[Y,X]];
end;

{ pIV konvertiert ein bekanntes Vektor-Format in eine CSV-Datei und speichert
  das Ergebnis als "vector.csv" im WKT-Format. Mit "iPrj>0" projiziert pIV
  dabei nach "iPrj". "iPrj" muss als EPSG-Code übergeben werden. }
{ pIV kapselt (wrapper) den GDAL-Befehl "ogr2ogr" und überprüft den Erfolg.
  pIV leitet GDAL-Fehlermeldungen nach StdIO um. }

procedure tGdal.ImportVect(
  iPrj:integer; //Ziel-Projektion als EPSG, Null für unverändert
  sGeo:string); //Geometrie-Quelle
const
  cGdl = 'iGW: GDAL image warp not successful: ';
  cSrc = 'iGW: Vector source file not found: ';
var
  slCmd:tStringList=nil; //Parameter als Liste
begin
  if not FileExists(sGeo) then Tools.ErrorOut(3,cSrc+sGeo);
  DeleteFile(eeHme+cfVct);
  DeleteFile(eeHme+cfVct+'t');
  DeleteFile(eeHme+ChangeFileExt(cfVct,'.prj'));
  try
    slCmd:=TStringList.Create;
    slCmd.Add('-f'); //Output-File-Format
    slCmd.Add('CSV'); //als Comma Seperated Values
    slCmd.Add('-overwrite'); //neue Datei
    slCmd.Add('-lco'); //Layer Creation Option
    slCmd.Add('GEOMETRY=AS_WKT'); //Geometrie als WKT
    slCmd.Add('-lco');
    slCmd.Add('CREATE_CSVT=YES'); //Format der Attribute
    if iPrj>0 then
    begin
      slCmd.Add('-t_srs'); //projizieren in Format
      slCmd.Add('EPSG:'+IntToStr(iPrj)); //als EPSG
    end;
    slCmd.Add(eeHme+cfVct); //Ziel = ".imalys/vector.csv"
    slCmd.Add(sGeo); //Quelle
    Tools.OsExecute(eeGdl+'ogr2ogr',slCmd);
    Tools.ErrorLog('gIV:'); //Exceptions speichern
  finally
    slCmd.Free;
  end;
  if not FileExists(eeHme+cfVct) then Tools.ErrorOut(3,cGdl+sGeo);
  Tools.HintOut(true,'ImportVect: '+ExtractFileName(sGeo));
end;

{ gRz brennt Vektor-Polygone in das Vorbild "sBnd". Nur der erste Kanal wird
  verändert. Die Polygone müssen nicht dieselbe Projektion besitzen wie das
  Vorbild. }

procedure tGdal.Rasterize(
  iVal:integer; //eingebrannter Wert wenn "sAtr"=''
  sAtr:string; //Vektor-Attribut-Name + Schalter
  sBnd:string; //Vorbild, ein Kanal, wird verändert
  sVct:string); //Polygone
const
  //cGdl = 'tGI: GDAL image inport not successful: ';
  cSrc = 'tGI: Image source file not available: ';
var
  slCmd: tStringList=nil; //Befehls-Parameter für gdal
begin
  if not FileExists(sBnd) then Tools.ErrorOut(3,cSrc+sBnd);
  try
    slCmd:=TStringList.Create;
    if sAtr='' then
    begin
      slCmd.Add('-burn'); //ein Wert für alle Polygone
      slCmd.Add(IntToStr(iVal)); //Wert der Maske
    end
    else
    begin
      slCmd.Add('-a'); //Attribute wiedergeben
      slCmd.Add(sAtr) //Attribut Name
    end;
    {
    slCmd.Add('-of'); //Bildformat
    slCmd.Add('ENVI'); //ENVI
    slCmd.Add('-ot'); //Datenformat
    slCmd.Add('Byte'); //für Klassen
    }
    slCmd.Add(sVct); //Polygone
    slCmd.Add(sBnd); //Vorbild
    //slCmd.SaveToFile(eeHme+'gdal_translate.params'); //KONTROLLE
    Tools.OsExecute(eeGdl+'gdal_rasterize',slCmd);
    Tools.ErrorLog('gRz:'); //Fehler in Konsole umleiten
  finally
    slCmd.Free;
  end;
end;

{ gII extrahiert Metadaten aus dem Bild "sImg" und aktualisiert bei Bedarf die
  Statistik. Zur Statistik gehören Grauwert-Histogramme für alle Kanäle. Das
  Ergebnis ist formatierter Text. }

function tGdal.ImageInfo(sImg:string):string;
const
  cGdl = 'iGI: GDAL image information not successful: ';
  cImg = 'iGI: Image source file not found: ';
var
  slPrm: tStringList=nil; //Parameter-Liste
begin
  Result:='';
  if not FileExists(sImg) then Tools.ErrorOut(3,cImg+sImg);
  try
    slPrm:=TStringList.Create;
    //slPrm.Add('-stats'); //bei Bedarf Statistik erzeugen
    //slPrm.Add('-hist'); //Histogramm-Werte anzeigen
    slPrm.Add('-proj4'); //Projektion als "proj4"-String
    slPrm.Add(sImg); //Quell-Datei
    Tools.OsExecute(eeGdl+'gdalinfo',slPrm); //modal ausführen
    Result:=Tools.GetOutput(Tools.prSys); //GDAL Image Information (Text)
    Tools.ErrorLog('gII'); //Exceptions speichern
    if length(Result)=0 then Tools.ErrorOut(3,cGdl);
  finally
    slPrm.Free;
  end;
end;

procedure tGdal.ZonalBorders(sIdx:string);
const
  cRes = 'gZB: Zonal polygonization not successful: ';
  cShp = '.shp';
var
  slCmd: tStringList=nil;
begin
  Tools.ShapeDelete(sIdx); //"gdal_polygonize.py" überschreibt nicht
  sIdx:=ChangeFileExt(sIdx,''); //ohne Extension
  try
    slCmd:=tStringList.Create;
    slCmd.Add(sIdx); //Bildquelle
    //slCmd.Add('-b');
    //slCmd.Add('1'); //ein Kanal
    //slCmd.Add('-f');
    //slCmd.Add('SHP'); //Vektorformat
    slCmd.Add(sIdx+cShp); //Ziel
    //slCmd.Add('cell'); //Feldname
    //slCmd.Add('DN'); //Feldwert
    Tools.OsExecute(eeGdl+'gdal_polygonize.py',slCmd);
    Tools.ErrorLog('gZB'); //Exceptions speichern
  finally
    slCmd.Free;
  end;
  if FileExists(sIdx+cShp)=False then Tools.ErrorOut(3,cRes+sIdx+cShp);
  Tools.HintOut(true,'ZonalBorders: '+ExtractFileName(sIdx));
end;

{ tGET kapselt den Befehl GDAL_Translate. tGET speichert das Bild "sNme" als
  "sRes", die Extension steuert das Format. Für Exporte im IDL-Format muss der
  Header übergeben werden. "iFmt" steuert Bit pro Pixel. Mit iFmt=1 übernimmt
  tGET das bestehende Pixelformat, mit iFmt>1 exportiert tGET im gewählten
  Format. }

procedure tGdal.ExportTo(
  iBnd: integer; //Kanal im Vorbild ODER Null für alle
  iFmt: integer; //1=unmodified 2=Byte 3=Small 4:Integer 5:Single
  sNme: string; //Vorbild (Zwischenergebnis)
  sRes: string); //neuer Dateiname, Extension steuert Format
const
  caFmt: array[1..5] of string = ('','Byte','Int16','Int32','Float32');
  cFmt = 'rGE runtime error: Image transformation format not defined: ';
  cRes = 'rGE runtime error: Image export not successful: ';
var
  slCmd: TStringList=nil; //Parameter
begin
  if (iFmt<1) or (iFmt>5) then Tools.ErrorOut(3,cFmt+IntToStr(iFmt));
  try
    slCmd:=TStringList.Create;
    if iFmt>1 then slCmd.Add('-ot'); //Pixelformat erwarten
    if iFmt>1 then slCmd.Add(caFmt[iFmt]); //Format-Bezeichner
    if ExtractFileExt(sRes)='' then //ENVI-Format verwenden
    begin
      slCmd.Add('-of');
      slCmd.Add('ENVI');
      sRes:=ChangeFileExt(sRes,'') //Bilddaten ohne Extension!
    end;
    if iBnd>0 then slCmd.Add('-b'); //bestimmten Kanal verwenden
    if iBnd>0 then slCmd.Add(IntToStr(iBnd)); //Kanal-Nummer
    { todo: [Gdal.ExportTo] könnte mit -expand eine Palette ermöglichen. }
    slCmd.Add(sNme); //Vorbild
    slCmd.Add(sRes); //Ergebnis
    Tools.OsExecute(eeGdl+'gdal_translate',slCmd);
    Tools.ErrorLog('gET:'); //bei Exceptions anhalten
  finally
    slCmd.Free;
  end;
  if FileExists(sRes)=False then Tools.ErrorOut(3,cRes+sRes);
  Tools.HintOut(true,'ExportTo: '+ExtractFileName(sRes));
end;

procedure tSeparate.Regression(
  fxBnd: Tn2Sgl; //Basis-Kanal
  fxCmp: Tn2Sgl); //Vergleichs-Kanal
{ sRn bestimmt Zwischenergebnisse für die Regression der Pixel-Dichte zwischen
  den Kanälen "fxBnd" und "fxCmp". sRn ignoriert maskierte NoData-Pixel. }
// Regression = (∑xy-∑x∑y/n) / (∑y²-(∑y)²/n) >> dX/dY
var
  X,Y: integer; //Pixel
begin
  fcPrd:=0; fcSqr:=0; fcHrz:=0; fcVrt:=0; icCnt:=0; //Vorgabe
  for Y:=0 to pred(rcHdr.Lin) do
    for X:=0 to pred(rcHdr.Scn) do
    begin
      if isNan(fxBnd[Y,X])
      or isNan(fxCmp[Y,X]) then continue; //X:
      fcPrd+=fxBnd[Y,X]*fxCmp[Y,X];
      fcHrz+=fxBnd[Y,X];
      fcVrt+=fxCmp[Y,X];
      fcSqr+=sqr(fxCmp[Y,X]);
      inc(icCnt); //Pixel ohne NoData
    end; //for X ..
end;

procedure tSeparate.Rotation(
  fxBnd: Tn2Sgl; //Basis-Kanal
  fxCmp: Tn2Sgl); //Vergleichs-Kanal
{ sRn rotiert die Dichte-Matrix für die Kanäle "fxBnd" und "fxCmp" so, dass die
  maximale Ausdehnung in der X-Achse liegt. sRn bestimmt den Rotations-Winkel
  aus der Regression der Kanäle "fxCmp" und "fxBnd". Als Rotations-Zentrum
  verwendet sRn den Schwerpunkt aller Dichte-Kombinationen. Die X-Achse "fxBnd"
  sammelt die Hauptkomponente, die Y-Achse "fxCmp" die Rest-Werte. }
var
  fDst:double; //Distanz Schwerpunkt-Dichte
  fGam:double; //Winkel nach Rotation
  fOmg:double; //Regression als Winkel (omega)
  fRgs:double; //Regression dX/dY
  X,Y:integer; //Pixel
begin
  if fcSqr=sqr(fcVrt)/icCnt then exit; //Maske ohne Differenzierung
  fRgs:=(fcPrd-fcVrt/icCnt*fcHrz) / (fcSqr-sqr(fcVrt)/icCnt); //Regression dX/dY
  fOmg:=Pi/2 - Tools.ArcTanQ(fRgs,1); //Drehung auf Y=0 aus Regression
  fcHrz/=icCnt; //Schwerpunkt aus Summen
  fcVrt/=icCnt;
  for Y:=0 to pred(rcHdr.Lin) do
    for X:=0 to pred(rcHdr.Scn) do
    begin
      if isNan(fxBnd[Y,X])
      or isNan(fxCmp[Y,X]) then continue; //X:
      fDst:=sqrt(sqr(fxBnd[Y,X]-fcHrz) + sqr(fxCmp[Y,X]-fcVrt));  //Distanz Schwerpunkt-Dichte
      fGam:=Tools.ArcTanQ(fxBnd[Y,X]-fcHrz,fxCmp[Y,X]-fcVrt) + fOmg; //Winkel nach Rotation
      fxBnd[Y,X]:=sin(fGam)*fDst + fcHrz; //neue Koordinaten
      fxCmp[Y,X]:=cos(fGam)*fDst + fcVrt;
    end; //for X ..
end;

{ sPl gibt eine vollständige Hauptkomponenten-Transformation zurück. Die Zahl
  der Komponenten kann mit "iDim" reduziert werden. "Reduce.Principal" gibt mit
  einem schnelleren Algorithmus die erste Hauptkomponente alleine zurück. }
{ sPl verwendet zwei geschachtelte Schleifen. In der Inneren bestimmt
  "Regression" die mittlere Richtung aller Dichte-Vektoren für die ersten zwei
  Kanäle und "Rotation" rotiert die Vektoren so, dass die mittlere (gemeinsame)
  Dichte genau in die X-Richtung zeigt. sPl speichert die Dichte in X-Richtung
  im ersten und die Dichte in Y-Richtung im zweiten Ergebnis-Kanal. }
{ Diesen Prozess wiederholt sPl mit allen übrigen Kanälen als zweiten Kanal bis
  im ersten Kanal eine gemeinsame Hauptrichtung (Helligkeit) und in allen
  anderen Kanälen die verschiedenen Abweichungen übrig sind. In der äußeren
  Schleife verwendet sPl die Abweichungen vom letzten Schritt als neues Bild
  und wiederholt den Prozess ohne das bisherige Ergebnis zu erändern. Da jedes
  neue Bild einen Kanal weniger hat als der Vorgänger hat das ergebnis dieselbe
  Anzahl an Kanälen wie das Vorbild. }
{ Durch die Rotation können negative Werte entstehen. sPl transformiert alle
  Werte linear zu positiven Werten. }

procedure tSeparate.xPrincipal(
  iDim:integer; //Maximum Dimensionen
  sImg:string); //Pfadname Vorbild
const
  //cDim = '[Maximum Dimension] input must be larger than 0!';
  cFex = 'sPl: Image not found: ';
  cStk = 'Given image needs at least two image bands';
var
  fxBnd:Tn2Sgl=nil; //Basis-Kanal für erste Hauptkomponente
  fxCmp:Tn2Sgl=nil; //Vergleichs-Kanal
  B,C:integer;
begin
  if not FileExists(sImg) then Tools.ErrorOut(3,cFex+sImg);
  //if iDim<1 then Tools.ErrorOut(3,cDim);
  iDim:=max(iDim,1); //mindestens erste Hauptkomponente
  Header.Read(rcHdr,sImg); //Header wird verändert!
  if rcHdr.Stk<2 then Tools.ErrorOut(3,cStk);
  iDim:=min(iDim,rcHdr.Stk); //maximum Komponenten
  Image.WriteBand(Tools.Init2Single(rcHdr.Lin,rcHdr.Scn,0),0,eeHme+cfPca); //leere Kachel für 1. Kanal

  for C:=0 to pred(iDim) do
  begin
    fxBnd:=Image.ReadBand(C,rcHdr,sImg); //Basis-Kanal
    for B:=succ(C) to pred(rcHdr.Stk) do //alle anderen Kanäle
    begin
      fxCmp:=Image.ReadBand(B,rcHdr,sImg); //Vergleichs-Kanal
      Regression(fxBnd,fxCmp); //Regression für Kanäle C/B
      Rotation(fxBnd,fxCmp); //maximale Ausdehnung für "fxBnd"
      Filter.MoveValue(-Tools.MinBand(fxBnd),fxCmp); //alle Werte positiv machen
      Image.WriteBand(fxCmp,B,eeHme+cfPca); //Ergebnis als Zwischenlager für "Reste"
      write('.') //Fortschritt
    end;
    write(#13);
    Filter.MoveValue(-Tools.MinBand(fxBnd),fxBnd); //alle Werte positiv machen
    Image.WriteBand(fxBnd,C,eeHme+cfPca); //Ergebnis "C" dauerhaft speichern
    sImg:=eeHme+cfPca; //ab jetzt "Reste"-Kanäle als Vorbild
    Tools.HintOut(true,'Principal: '+IntToStr(succ(C))+'/'+
      IntToStr(iDim)+': '+cfPca);
  end;
  Header.WriteMulti(iDim,iDim,rcHdr,'',sImg);
end;

{ fOI extrahiert die Metadaten aus "sVct" und gibt sie als Text zurück. fOI
  leitet Fehlermeldungen von "ogrinfo" in den "exception"-Prozess um. }

function tGdal.OgrInfo(sVct:string):string; //Quelle: Ergebnis
const
  cGdl = 'iGW: GDAL geometry info not successful: ';
  cSrc = 'iGW: Vector source file not found: ';
var
  slCmd:tStringList=nil; //Parameter als Liste
begin
  if not FileExists(sVct) then Tools.ErrorOut(3,cSrc+sVct);
  try
    slCmd:=TStringList.Create;
    slCmd.Add('-al'); //alle Layer anzeigen ← für Vergleich mit CSV-Version
    slCmd.Add('-so'); //nur Zusammenfassung
    slCmd.Add(sVct); //Quelle
    Tools.OsExecute(eeGdl+'ogrinfo',slCmd);
    Result:=Tools.GetOutput(Tools.prSys); //GDAL Image Information (Text)
    Tools.ErrorLog('gOI:'); //Exception speichern
    if length(Result)=0 then Tools.ErrorOut(3,cGdl);
  finally
    slCmd.Free;
  end;
end;

{ gSI ruft "gdalsrsinfo" mit dem Parameter "-e" für den EPSG-Code auf und gibt
  das Ergebnis als String zurück. Der String enthält Zeilenende-Marken. gSI
  ==> DAS ERGEBNIS KANN EIN LEERER STRING SEIN }

function tGdal.SrsInfo(sImg:string):string;
const
  cGdl = 'gSI: GDAL image information not successful: ';
  cImg = 'gSI: Image source file not found: ';
var
  slPrm: tStringList=nil; //Parameter-Liste
begin
  Result:='';
  if not FileExists(sImg) then Tools.ErrorOut(3,cImg+sImg);
  try
    slPrm:=TStringList.Create;
    slPrm.Add('-e'); //EPSG-Code zurückgeben
    slPrm.Add(sImg); //Quell-Datei
    Tools.OsExecute(eeGdl+'gdalsrsinfo',slPrm); //modal ausführen
    Result:=Tools.GetOutput(Tools.prSys); //GDAL Image Information (Text)
    if Result='' then Tools.ErrorOut(3,cGdl+sImg); //Command-Fehler
  finally
    slPrm.Free;
  end;
end;

procedure tGdal.Hillshade(sDem:string); //Vorbild-Dateiname
{ gIt konvertiert das Bild "sImg" in das ENVI-Format und speichert das Ergebnis
  als "import". Mit "iSgl=0" übernimmt gIt das Format des Originals, in allen
  anderen Fällen speichert gIt das Bilder als 32-Bit Float. gIt aktualisiert
  die Bildstatistik. Mit "iHig >= iLow > Null" werden nur die Kanäle zwischen
  "iLow" und "iHig" übernommen. Mit "rFrm.Lft < rFrm.Rgt" beschneidet gIt das
  Bild auf den übergebenen Rahmen. Die Koordinaten müssen zm Bild-CRS passen.
  Der Rahmen darf größer sein als das Bild. }
// gdaldem input output options
const
  cHme = 'tHs: Imalys needs a working directory. Try to run initialization!';
  cRes = 'tHs: GDAL hillshade not successful: ';
  cSrc = 'tHs: Image source file not available: ';
var
  slCmd: tStringList=nil; //Befehls-Parameter für gdal
begin
  if not FileExists(eeHme) then Tools.ErrorOut(3,cHme);
  if not FileExists(sDem) then Tools.ErrorOut(3,cSrc+sDem);
  try
    slCmd:=TStringList.Create;
    slCmd.Add('hillshade'); //Modus
    slCmd.Add(sDem); //Vorbild
    slCmd.Add(eeHme+cfHse); //Ergebnis
    slCmd.Add('-of'); //Bildformat
    slCmd.Add('ENVI'); //ENVI
    Tools.OsExecute(eeGdl+'gdaldem',slCmd);
    Tools.ErrorLog('gHs:'); //Fehler in Konsole umleiten
  finally
    slCmd.Free;
  end;
  if not FileExists(eeHme+cfHse) then Tools.ErrorOut(3,cRes+eeHme+cfHse);
end;

{ aTC gibt eine Liste aller Dateinamen zurück, die im Archiv "sArc" gespeichert
  sind. Mit "slMsk<>nil" reduziert aTC die Liste auf Namen, die mindestens
  einen der Filter-Strings aus "slMsk" enthalten.
==> DAS ERGEBNIS KANN EINE LEERE LISTE SEIN }

function tArchive.TarContent(
  sArc:string; //Archiv-Name
  slMsk:tStringList): //Filter für benötigte Namen
  tStringList; //gefilterte Namen im Archiv
const
  cArc = 'Archive not available: ';
  cCmd = 'tar';
var
  bHit:boolean; //Suchmaske gefunden
  slPrm:tStringList=nil; //Parameter für Tar-Befehl
  sRes:string=''; //tar-Output
  B,R:integer;
begin
  Result:=nil;
  if not FileExists(sArc) then Tools.ErrorOut(3,cArc+sArc);
  try
    slPrm:=tStringList.Create;
    slPrm.Add('-t'); //liste erzeugen
    slPrm.Add('-f'); //Archiv-Name
    slPrm.Add(sArc); //Archiv-Name
    Tools.OsExecute(cCmd,slPrm); //Liste in StdIO
    sRes:=Tools.GetOutput(Tools.prSys); //Inhaltsverzeichnis im StdOut
    Tools.ErrorLog('aTC:'); //Error-Log ergänzen
  finally
    slPrm.Free;
  end;

  if length(sRes)>0 then
  begin
    Result:=tStringList.Create;
    Result.Text:=sRes; //Output als Liste
    if slMsk<>nil then //Suchmaske übergeben
      for R:=pred(Result.Count) downto 0 do
      begin
        bHit:=False; //Vorgabe
        for B:=0 to pred(slMsk.Count) do
          bHit:=bHit or (pos(slMsk[B],Result[R])>0); //Ausdruck kommt vor
        if not bHit then Result.Delete(R); //nicht benötigte Namen löschen
      end
  end
end;

{ aTE extrahiert die Dateien "slNme" aus dem Archiv "sArc" mit dem OS-Befehl
  "tar". Die extrahierten Dateien werden von "tar" im aktuellen Arbeits-
  Verzeichnis gespeichert. }
{ ES GIBT "CPIO" FÜR ARCHIVIERTE DATEIEN }

function tArchive.TarExtract(
  sArc:string; //Archiv-Name
  slNme:tStringList): //gewählte Kanäle im Archiv
  string; //Verzeichnis mit extrahierten Kanälen
const
  cArc='aTE: Archive name does not fit Landsat convention! ';
  cDir='aTE: Unable to create import directory: ';
  cCmd = 'tar';
var
  iSze:integer=0; //ausgewählte Buchstaben am Anfang des Dateinamens
  slPrm:tStringList=nil; //Parameter für "tar"-Befehl
begin
  iSze:=pred(nPos('_',ExtractFileName(sArc),4)); //NUR LANDSAT!
  if iSze<0 then Tools.ErrorOut(3,cArc+sArc);
  Result:=eeHme+copy(ExtractFileName(sArc),1,iSze); //neues Verzeichnis
  CreateDir(Result); //Ziel-Verzeichnis
  if not DirectoryExists(Result) then Tools.ErrorOut(2,cDir+Result);
  SetCurrentDir(Result); //aktuell machen
  if (slNme<>nil) and (slNme.Count>0) then
  try
    slPrm:=tStringList.Create;
    slPrm.Add('-x'); //liste erzeugen
    slPrm.Add('-f'); //Archiv-Name
    slPrm.Add(sArc); //Archiv-Name
    slPrm.AddStrings(slNme); //gefilterte Namen
    Tools.OsExecute(cCmd,slPrm); //gefilterte Namen extrahieren
    Tools.ErrorLog('aTE:'); //tar-Fehlermeldungen
  finally
    slPrm.Free;
  end;
  Tools.HintOut(true,'TarExtract: '+ExtractFileName(sArc));
end;

{ pEF liest Kanäle aus dem Artćhiv "sArc", die zu den Strings in "sBnd" passen.
  pEF liest das Inhaltsverzeichnis des Archivs "sArc", reduziert es auf Namen
  die in "sBnd" vorkommen und extrahiert die gefilterten Namen. Der Befehl
  "tar" extrahiert in das System-Arbeitsverzeichnis (pwd). "sBnd" kann eine
  kommagetrennte Liste sein. Jeder Eintrag wird getrennt gefiltert. }

function tArchive.ExtractFilter(
  sArc:string; //Archiv-Name (TAR)
  sBnd:string): //Filter für Kanal-Namen als CSV
  tStringList; //extrahierte Kanäle ODER nil
const
  cArc = 'aEF: Archive not defined or content missing: ';
var
  sDir:string; //Verzeichnis der ausführbaren Datei = Ziel der Extraktion
  slMsk:tStringList=nil; //Kanal-Namen-Suchmasken als Liste
  I:integer;
begin
  Result:=nil;
  try
    slMsk:=tStringList.Create;
    slMsk.Text:=Tools.CommaToLines(sBnd); //Liste aus CSV
    Result:=TarContent(sArc,slMsk); //Dateinamen im Archiv lesen und filtern
    if (Result<>nil) and (Result.Count>0) then
    begin
      TarExtract(sArc,Result); //Namen in "Result" extrahieren
      sDir:=Tools.OsCommand('pwd','')+DirectorySeparator;
      for I:=pred(Result.Count) downto 0 do
      begin
        if FileExists(sDir+Result[I]) then //Extraktion erfolgreich
          Result[I]:=sDir+Result[I] //vollständiger Pfadname
        else Result.Delete(I); //kein Ergebnis
      end;
    end
    else Tools.ErrorOut(0,cArc+sArc) //Nachricht, keine Unerbrechung
  finally
    slMsk.Free;
  end;
  if (Result<>nil) and (Result.Count=0) then
    FreeAndNil(Result); //nil zurückgeben
end;

{ aGE liest den Nadir-Sonnenwinkel aus den RapidEye Metadaten }

function tArchive._GetNadir_(sXml:string):single; //RapidEye-Metadaten
const
  cIea = '<opt:illuminationElevationAngle'; //Kennung
  cPos = 'aGE: Illumination angle not found in ';
var
  fVal:single; //Winkel als Zahl > Divisor
  iHig,iLow:integer; //Ende, Anfang der Ziffer
  sRem:string; //XML-Text
begin
  Result:=1; //Vorgabe
  sRem:=Tools.LineRead(sXml); //Metadaten als String
  iLow:=pos(cIea,sRem); //Position Kennung ODER "1"

  if iLow>0 then //Kennung gefunden
  begin
    iLow:=succ(pos('>',sRem,iLow)); //Ende Kennung
    iHig:=pos('</',sRem,iLow); //Ende Ziffer
    fVal:=StrToFloat(copy(sRem,iLow,iHig-iLow)); //Zahl
    Result:=cos((90-fVal)/180*Pi); //Nadir-Winkel
  end
  else Tools.ErrorOut(3,cPos+sXml);
end;

{ gIt konvertiert das Bild "sImg" in das IDL-Format und speichert das Ergebnis
  als "import". Mit "sTrg" kann ein anderer Name gewählt werden. MiEnviRenamt "iSgl=0"
  übernimmt gIt das Format des Originals, in allen anderen Fällen speichert gIt
  das Bilder als 32-Bit Float. gIt aktualisiert die Bildstatistik. Mit "iHig >=
  iLow > Null" werden nur die Kanäle zwischen "iLow" und "iHig" übernommen. Mit
  "rFrm.Lft < rFrm.Rgt" beschneidet gIt das Bild auf den übergebenen Rahmen.
  Das CRS des Rahmens kann vom Bild abweichen. Der Rahmen darf größer sein als
  das Bild. }

procedure tGdal.Translate(
  iSgl:integer; //Ergebnis im Single-Format [0,1]
  iHig,iLow:integer; //letzter, erster Kanal beginnend mit Eins; iHig<iLow für alle
  rFrm:trFrm; //Auswahl-Rahmen, Koordinaten-System muss eingetragen sein
  sImg:string; //Vorbild-Dateiname
  sTrg:string); //Ergebnis-Name
const
  cGdl = 'gIt: GDAL image inport not successful: ';
  cHme = 'gIt: Imalys needs a working directory. Try to run initialization!';
  cSrc = 'gIt: Image source file not available: ';
var
  slCmd: tStringList=nil; //Befehls-Parameter für gdal
  B: integer;
  qS:string;
begin
  //if not FileExists(eeHme) then Tools.ErrorOut(3,cHme);
  if not DirectoryExists(eeHme) then Tools.ErrorOut(3,cHme);
  if not FileExists(sImg) then Tools.ErrorOut(3,cSrc+sImg);

  if sTrg='' then sTrg:=eeHme+cfImp; //Vorgabe
  try
    slCmd:=TStringList.Create;
    slCmd.Add('-of'); //Bildformat
    slCmd.Add('ENVI'); //ENVI
    if iSgl>0 then
    begin
      slCmd.Add('-ot'); //Datenformat
      slCmd.Add('Float32')
    end;
    slCmd.Add('-stats'); //Statistik neu rechnen
    if iHig>=iLow then //Kanäle selektiert (ab Eins)!
      for B:=iLow to iHig do
      begin
        slCmd.Add('-b');
        slCmd.Add(IntToStr(B))
      end;
    if rFrm.Lft<rFrm.Rgt then //nicht Vorgabe "crFrm"
    begin
      slCmd.Add('-projwin');
      slCmd.Add(FloatToStr(rFrm.Lft));
      slCmd.Add(FloatToStr(rFrm.Top));
      slCmd.Add(FloatToStr(rFrm.Rgt));
      slCmd.Add(FloatToStr(rFrm.Btm));
    end;
    if rFrm.Epg>0 then
    begin
      slCmd.Add('-projwin_srs');
      //slCmd.Add('EPSG:4326'); //CRS = geographisch
      slCmd.Add('EPSG:'+IntToStr(rFrm.Epg)); //CRS übergeben
    end;
    if ExtractFileExt(sImg)=cfHdr
      then slCmd.Add(ChangeFileExt(sImg,'')) //Vorbild im IDL-Format
      else slCmd.Add(sImg); //Vorbild in anderem Format
    slCmd.Add(sTrg); //Ergebnis Dateiname
    //slCmd.SaveToFile(eeHme+'gdal_translate.params'); //KONTROLLE
    Tools.OsExecute(eeGdl+'gdal_translate',slCmd);
    qS:=slCmd.Text;
    Tools.ErrorLog('gIt:'); //Fehler speichern
  finally
    slCmd.Free;
  end;
  if not FileExists(sTrg) then Tools.ErrorOut(3,cGdl+sImg);
  Tools.HintOut(true,'Translate: '+ExtractFileName(sImg));
end;

{ gES transformiert eine Vektor-Datei und ihre Attribute in das Shape-Format.
  Ist die Vorlage eine CSV-Datei, wird die übergebene Projektion auf In- und
  Output angewendet. Sind beide Dateien projiziert, nimmt fES mit "sPrj" eine
  Umprojektion vor. fEG kapselt (wrapper) die GDAL-Funktion "ogr2ogr" }

procedure tGdal.ExportShape(
  iPrj:integer; //EPSG-Code der Vorlage, nötig bei csv-Dateien
  iWrp:integer; //EPSG-Code des Targets, Null wie Vorlage
  sSrc:string; //Vorbild Geometrie (CSV)
  sTrg:string); //Ergebnis Geometrie mit Attributen
const
  cGdl = 'gES: GDAL vector transformation not successful: ';
  cSrc = 'gES: CSV file not found: ';
var
  slCmd:tStringList=nil; //Parameter als Liste
begin
  if not FileExists(sSrc) then Tools.ErrorOut(3,cSrc+sSrc);
  try
    slCmd:=TStringList.Create;
    slCmd.Add('-f'); //Output-File-Format
    slCmd.Add('ESRI Shapefile'); //Format
    if iPrj>0 then
    begin
      slCmd.Add('-a_srs'); //Projektion der Vorlage ← nur für csv-Dateien
      slCmd.Add('EPSG:'+IntToStr(iPrj)); //EPSG-Code der Vorlage überschreiben
    end;
    if iWrp>0 then
    begin
      slCmd.Add('-t_srs'); //Projektion ändern
      slCmd.Add('EPSG:'+IntToStr(iWrp)); //EPSG-Code im Ziel
    end;
    slCmd.Add('-nlt'); //Geometrie-Typ
    slCmd.Add('MULTIPOLYGON');
    { todo [Gdal.ExportShape] slCmd.Add('-preserve_fid') bewahrt Feldnamen }
    slCmd.Add(sTrg); //Ziel: formatierte Geometrie
    slCmd.Add(sSrc); //Vorlage: erweiterte CSV-Datei
    Tools.OsExecute(eeGdl+'ogr2ogr',slCmd);
    Tools.ErrorLog('gES:'); //Exceptions speichern
  finally
    slCmd.Free;
    if not FileExists(sTrg) then Tools.ErrorOut(3,cGdl+sTrg);
  end;
  Tools.HintOut(true,'ExportShape: '+ExtractFileName(sTrg));
end;

//Korrelations-Tabelle aus Klassen-Layern

function tRank._Correlation(
  iMap,iRfz:integer; //Anzahl Klassen/Referenzen (ohne Rückweisung)
  ixMap,ixRfz:tn2Byt): //Klassen/Referenz-Layer
  tn2Sgl; //Anteile der Klassen in den Referenzen
var
  M,R,X,Y:integer;
begin
  Result:=Tools.Init2Single(succ(iMap),succ(iRfz),0);
  for Y:=0 to high(ixMap) do
    for X:=0 to high(ixMap[0]) do
      Result[ixMap[Y,X],ixRfz[Y,X]]+=1.0;
  for M:=1 to high(Result) do
    for R:=1 to high(Result[0]) do
      Result[M,0]+=Result[M,R]; //Summe für alle Klassen
  for M:=1 to high(Result) do
    for R:=1 to high(Result[0]) do
      Result[M,R]/=Result[M,0]; //Anzeilepro Referenz
end;

// Korrelation einer Klassifikation mit einer Referenz

procedure tRank._xCorrelation();
var
  fxCrl:tn2Sgl=nil; //Verteilung Klassen : Referenz
  ixMap:tn2Byt=nil; //Klassifikation
  ixRfz:tn2Byt=nil; //Klassen-Referenz
  rMap,rRfz:trHdr; //Metadaten Klassifikation, Referenz
begin
  Header.Read(rRfz,eeHme+cfRfz); //Metadaten Referenz
  ixRfz:=Image.ReadThema(rRfz,eeHme+cfRfz); //Referenzen als Raster-Maske
  Header.Read(rMap,eeHme+cfMap); //Metadaten Klassifikation
// Bilder gleich groß?
  ixMap:=Image.ReadThema(rMap,eeHme+cfMap); //Klassen als Raster-Maske
  fxCrl:=_Correlation(rMap.Cnt,rRfz.Cnt,ixMap,ixRfz);
  Tools.SingleToText(fxCrl,rMap.Fld,rRfz.Fld,eeHme+cfAcy);
end;

{ rDn bestimmt Median, 5% und 95% Percentil der Werte im Bild "values" und gibt
  das Ergebnis als Tabelle zurück. Die Tabelle enthält in der Zeile Null das
  Ergebnis für das ganze Bild, in den Zeilen darunter die Ergebnisse für
  einzelne Klassen aus "ixRfz". rDn ignoriert NoData und die Klasse Null }

function tRank.Distribution(
  iAtr:integer; //Attribut-ID (ab Null)
  iMap:integer; //Anzahl Klassen in Referenz
  iSmp:integer; //Anzahl Stichproben
  ixRfz:tn2Byt): //Klassen-Maske = Referenz
  tn2Sgl; //Mittelwert, Varianz der Bilddaten in Referenzen
var
  faSmp:tnSgl=nil; //Stichproben-Liste
  faTmp:tnSgl=nil; //Stichproben mit gleicher Klasse
  fStp:double=0; //Schrittweite für Stichproben
  fSum:double=0; //cumulierte Schritte
  fxVal:tn2Sgl=nil; //Kanal mit Attribut-Werten als Bild
  iaThm:tnByt=nil; //Klasse pro Stichprobe
  iCnt:integer=0; //Anzahl gültige Stichproben
  iDim:integer=0; //Anzahl Stichproben mit aktueller Klasse
  iHrz,iVrt:integer; //Pixel-Koordinaten
  iScn:integer=0; //Bildbreite in Pixeln
  rHdr:trHdr; //Metadaten Attribute als Bild "values"
  I,R:integer;
begin
  //0<=iThm<rHdr.Cnt?
  //eeHme+cfVal muss existieren

  Result:=Tools.Init2Single(3,succ(iMap),0); //Ergebnis
  faSmp:=Tools.InitSingle(iSmp,0); //Stichproben-Liste
  fStp                             :=length(ixRfz)*length(ixRfz[0])/succ(iSmp); //Schrittweite in Pixeln
  iaThm:=Tools.InitByte(iSmp); //Klassen-ID der Stichproben
  iScn:=length(ixRfz[0]); //Bildbreite in Pixeln
  Header.Read(rHdr,eeHme+cfVal); //Metadaten Attribut-Bilder
  fxVal:=Image.ReadBand(iAtr,rHdr,eeHme+cfVal); //aktuelles Attribut als Bild

  for I:=0 to pred(iSmp) do
  begin //Statistik für alle Werte ← nur sinnvoll nach Normalisierung → Trennen ?
    fSum+=fStp;
    iVrt:=trunc(fSum) div iScn; //Y-Koordinate
    iHrz:=trunc(fSum) mod iScn; //X-Koordinate
    if not isNan(fxVal[iVrt,iHrz]) then
    begin
      faSmp[I]:=fxVal[iVrt,iHrz]; //Stichprobe Nr."I"
      iaThm[I]:=ixRfz[iVrt,iHrz]; //Klassen-ID dazu
      inc(iCnt)
    end;
  end;
  Result[0,0]:=Percentil(faSmp,0.05,pred(iCnt)); //5% Percentil = Minimum
  Result[1,0]:=Percentil(faSmp,0.50,pred(iCnt)); //50% Percentil = Median
  Result[2,0]:=Percentil(faSmp,0.95,pred(iCnt)); //95% Percentil = Maximum

  for R:=1 to iMap do
  begin //Statistik für einzelne Klassen
    SetLength(faTmp,iSmp); //maximale Größe
    iDim:=0;
    for I:=0 to pred(iCnt) do
      if iaThm[I]=R then
      begin //Pixel pro Klasse sammeln
        faTmp[iDim]:=faSmp[I];
        inc(iDim)
      end;
    if iDim>0 then
    begin
      SetLength(faTmp,iDim); //maximale Größe
      Result[0,R]:=Percentil(faTmp,0.05,pred(iDim)); //5% Percentil = Minimum
      Result[1,R]:=Percentil(faTmp,0.50,pred(iDim)); //50% Percentil = Median
      Result[2,R]:=Percentil(faTmp,0.95,pred(iDim)); //95% Percentil = Maximum
    end
    else
    begin
      Result[0,R]:=0;
      Result[1,R]:=0;
      Result[2,R]:=0;
    end;
  end;
end;

{ rVR speichert eine Geometrie "sVct" mit einem passenden Attribut "sFld" als
  Klassen-Layer "mapping" im Rasterformat. rVR interpretiert die Einträge im
  Attribut "sFld" als Text. Bis zu 250 verschiedene Einträge sind zulässig. }
{ rVR importiert die Geometrie als als "vector.csv", ergänzt eine fortlaufende
  Klassen-ID in "focus.csv" und erzeugt daraus einen Klassen-Layer "mapping"
  mit dem Klassen-Index als Wert. rVR überträgt die Attribute als Klassen-Namen
  in "mapping" }

procedure tRank.xVectorReference(
  sFld:string; //Feldname für Klassen in Geometrie
  sVct:string); //Dateiname Klassen-Geometrie
var
  rHdr:trHdr; //Metadaten Raster-Klassen
  slThm:tStringList=nil;
begin
  Gdal.ImportVect(0,sVct); //Polygone als "vector.csv" importieren
  try
    slThm:=Table.AddIndex(sFld); //Fortlaufende Klassen-ID in "vector.csv" eintragen
    slThm[0]:='all'; //Klasse Null als ID für ganzes Bild verwenden
    Table.VectorMask(cfMsk,eeHme+cfFcs); //Raster-Maske aus Klassen-IDs
    Header.Read(rHdr,eeHme+cfMap); //Metadaten Klassen-Maske ← ohne Klassen-Namen
    Header.WriteThema(pred(slThm.Count),rHdr,slThm.CommaText,eeHme+cfMap); //Klassen-Namen ergänzen
  finally
    slThm.Free
  end;
end;

{ rDn bestimmt 5%-50%-95% Percentile für alle Zonen-Attribute in "sBnd" und
  alle Klassen "fxMap" und speichert das Ergebnis getrennt nach Attributen als
  Text-Tabellen (Tab-getrennt). Klassen und Zonen-Attribute müssen als Raster-
  Layer (values, mapping) verfügbar sein um beliebige Geometrieen zu
  verschneiden. Die Klassen müssen fortlaufend nummeriert sein }
{ rDn vergleicht sukzessive alle in sBnd gewählten Attribute mit den Klassen
  aus "mapping". Die Tabellen enthalten getrennte Spalten für 5%, 50% und 95%
  Percentile und zu Beginn eine Zeile für alle Klassen. Die Tabellen sind nach
  den Attributen benannt }

procedure tRank.xDistribution( // <=> xCorrelation
  iSmp:integer); //Anzahl Stichproben
const
  cSmp = 'rDn: Sample input must be a positive integer: ';
  cTop = ' ,low (5%),median (50%),high (95%)';
var
  fxRes:tn2Sgl=nil;
  ixRfz:tn2Byt=nil; //Klassen-Layer aus Referenzen
  rMap:trHdr; //Metadaten der Referenz (Klassen)
  slAtr:tStringList=nil; //Attribut-Namen = Feld-Namen im Index
  F:integer;
begin
  if iSmp<1 then Tools.ErrorOut(3,cSmp+IntToStr(iSmp));
  Header.Read(rMap,eeHme+cfMap); //Referenzen als Raster-Maske
  ixRfz:=Image.ReadThema(rMap,eeHme+cfMap); //Klassen-Maske, fortlaufende Klassen-ID
  iSmp:=min(pred(rMap.Lin*rMap.Scn),iSmp); //höchstens jeder Pixel
  slAtr:=tStringList.Create;
  try
    slAtr.CommaText:=Header.ReadLine(False,'field names',eeHme+cfIdx); //Attribut-Namen
    for F:=0 to pred(slAtr.Count) do //Ausgewählte (?) Attribute
    begin
      fxRes:=Distribution(F,rMap.Cnt,iSmp,ixRfz);
      Tools.SingleToText(fxRes,cTop,rMap.Fld,eeHme+IntToStr(succ(F))+'_'+slAtr[F]);
      write(#13+IntToStr(succ(F))+#32); //Ablauf
    end;
  finally
    slAtr.Free;
  end;
end;

{ rPl bestimmt den Percentil "fLmt" für das Array "faVal" }
{ rPl bestimmt zunächst den Wertebereich [fMin,fMax] der gesamten Liste und
  daraus Summe aller Daten und das vorläufige Ergebnis (Mittelwert). Danach
  summiert rPl alle Werte bis zur aktuellen Ergebnis "result" und prüft, ob das
  Ziel der Suche "fTrg" erreicht wird. Je nach Ergebnis übernimmt rPl das
  aktuelle Ergebnis als obere oder untere Schwelle und wiederholt den Prozess
  bis er stabil ist }
{ ==> DIE LISTE DARF KEIN NODADA ENTHALTEN }

function tRank.Percentil(
  faVal:tnSgl; //Werte-Liste
  fLmt:single; //Schwelle [0..1]
  iHig:integer): //Indices bis faVal[iHig] verwenden
  single; //Schwelle im Attribut
var
  fMax:single=1-MaxSingle; //obere Schwelle
  fMin:single=MaxSingle; //untere Schwelle
  iCnt:integer=0; //aktuelle Anzahl Elemente
  iTmp:integer=0; //Zwischenlager
  iTrg:integer=0; //Soll Anzahl sortierte Elemente
  I:integer;
begin
  Result:=0; //Vorgabe
  for I:=0 to iHig do
  begin
    fMax:=max(faVal[I],fMax);
    fMin:=min(faVal[I],fMin);
  end;
  Result:=(fMax+fMin)/2; //Vorgabe = Mittelwert
  iTrg:=round(succ(iHig)*fLmt); //Soll-Anzahl

  repeat
    iTmp:=iCnt;
    iCnt:=0; //neu beginnen
    for I:=0 to iHig do
      if faVal[I]<=Result then //Werte bis zur Schwelle ..
        inc(iCnt); //.. zählen
    if iCnt>=iTrg
      then fMax:=Result //oben einschränken
      else fMin:=Result; //unten einschränken
    Result:=(fMin+fMax)/2; //weiter in der Mitte
  until (iCnt=iTrg) or (iCnt=iTmp)
end;

{ aGP prüft ob der String "sPrd" als Periode aus zwei Datumsangaben im Format
  YYYYMMDD interpretiert werden kann und gibt beide Angaben in "iLow/iHig" als
  Zahlen zurück. Ist "sPrd" leer, passt "iLow/iHig" zu jedem Datum }

procedure tArchive.GetPeriod(var iHig,iLow:integer; sPrd:string);
const
  cPrd='aGP: Date input must be formatted as [YYYYMMDD-YYYYMMDD]: ';
  cVal='aGP: Earlier date must be passed on the left side: ';
begin
  if length(sPrd)>0 then
  begin
    if (TryStrToInt(LeftStr(sPrd,8),iLow)=False)
    or (TryStrToInt(RightStr(sPrd,8),iHig)=False) then
      Tools.ErrorOut(3,cPrd+sPrd); //Format-Fehler
    if iLow>iHig then Tools.ErrorOut(3,cVal+sPrd) //Format-Fehler
  end
  else
  begin //Vorgabe = alles
    iLow:=0;
    iHig:=99999999;
  end
end;

{ gWp transformiert das Bild "sImg" in die Projektion "iCrs" und die Pixelgröße
  "iPix" und speichert das Ergebnis im IDL-Format als "warp". Mit "target" kann
  ein anderer Name gewählt werden. Die Pixel sind quadratisch. Ihre Position
  folgt den Ziel-Koordinaten (TAP). Die Pixel-Werte sind bicubisch interpoliert.
  Leere Bereiche sind auf NoData gesetzt. }

// Ausschnitt aktiviert

procedure tGdal.Warp(
  iCrs:integer; //Ziel-Koordinatensystem als EPSG-Code ODER Null für unverändert
  iPix:integer; //Pixelgröße in Metern
  rFrm:trFrm; //Bounding-Box für Ausschnitt, CRS beliebig wenn angegeben
  sImg:string; //Vorbild
  sTrg:string); //Ergebnis-Name ODER leer
const
  cGdl = 'gWp: GDAL image warp not successful: ';
  cSrc = 'gWp: Image source file not found: ';
var
  slCmd: tStringList=nil; //Parameter-Liste für "prSys"
begin
  if sTrg='' then sTrg:=eeHme+cfWrp;
  if not FileExists(sImg) then Tools.ErrorOut(3,cSrc+sImg);
  try
    slCmd:=TStringList.Create;
    if iCrs>0 then
    begin
      slCmd.Add('-t_srs'); //target-CRS
      slCmd.Add('EPSG:'+IntToStr(iCrs)); //gewähltes CRS
    end;
    if iPix>0 then
    begin
      slCmd.Add('-tr'); //target-Pixelgröße
      slCmd.Add(IntToStr(iPix)); //gewählte Pixelgröße horizontal
      slCmd.Add(IntToStr(iPix)); //gewählte Pixelgröße vertikal
      slCmd.Add('-tap'); //Target Alligned Pixels
    end;
    if rFrm.Lft<rFrm.Rgt then //nicht "crMax"
    begin //Bild beschneiden, Koordinaten im Target-CRS
      slCmd.Add('-te'); //target beschneiden
      slCmd.Add(FloatToStr(rFrm.Lft));
      slCmd.Add(FloatToStr(rFrm.Btm));
      slCmd.Add(FloatToStr(rFrm.Rgt));
      slCmd.Add(FloatToStr(rFrm.Top));
      slCmd.Add('-te_srs'); //abweichendes CRS des ROI
      slCmd.Add('EPSG:'+IntToStr(rFrm.Epg)); //CRS des ROI
    end;
    slCmd.Add('-ot'); //Datenformat
    slCmd.Add('Float32'); //Single = Standard
    slCmd.Add('-r'); //Resampling
    slCmd.Add('cubic'); //Quadratisch = Standard
    slCmd.Add('-dstnodata');
    slCmd.Add(FloatToStr(NaN));
    slCmd.Add('-of'); //Bildformat
    slCmd.Add('ENVI'); //ENVI
    slCmd.Add('-overwrite'); //Ergebnis ersetzen
    slCmd.Add(sImg); //Vorbild
    slCmd.Add(sTrg); //Ergebnis
    //slCmd.SaveToFile(eeHme+'gdalwarp.params'); //KONTROLLE
    Tools.OsExecute(eeGdl+'gdalwarp',slCmd);
    Tools.ErrorLog('gWp:'); //Protokoll
  finally
    slCmd.Free;
    if not FileExists(sTrg) then Tools.ErrorOut(3,cGdl+sTrg);
  end;
  Tools.HintOut(true,'GdalWarp: '+ExtractFileName(sImg));
end;

{ aSN gibt einen Standard-Kurz-Namen aus Kachel, Kanal und Datum für das
  Arbeitsverzeichnis zurück. Dazu muss "sNme" der Konvention der Provider
  entsprechen }

function tArchive.ShortName(
  aIdf:taIdf; //Position der Abschnitte im Provider-Namen
  sNme:string): //Provider-Name
  string; //Imalys-Kurz-Name
begin
  case aIdf[1] of
    1: Result:='S2';
    3: Result:='LS';
    6: Result:='S2';
    else Result:='Im'; //Imalys
  end;
  sNme:=ChangeFileExt(ExtractFileName(sNme),''); //Nur Name ohne Extension
  Result+='_'+ExtractWord(aIdf[1],sNme,['_'])+ //Kachel
    '_'+LeftStr(ExtractWord(aIdf[3],sNme,['_']),8); //Datum (ohne Zeit)
end;

{ aII importiert alle Bilder in "slImg" in das Arbeitsverzeichnis, projiziert
  sie auf "iEpg", beschneidet sie auf "rFrm", wählt die Kanäle aus "sArt" und
  speichert das Ergebnis im Arbeitsverzeichnis mit einem "c_" vor dem alten
  Dateinamen. Alle Eingaben bis auf "slImg" sind optional }
{ Mit "iEpg">0 transformiert aII die Bilder in das neue CRS, mit "iPix">0
  stellt aII die übergebene Pixelgröße ein und mit "sFrm"<>'' projiziert aII
  alle übergebenen Bilder in den angegebenen Rahmen. Ohne diese Angaben
  verwendet aII das CRS, die Pixelgröße und den Rahmen des ersten Bilds als
  Vorgabe. Mit "sArt"<>'' übernimmt aII nur die angegebenen Kanäle. "sTrg"<>''
  wählt den Ergebnis-Namen, die Vorgabe ist "compile". Die Kanal-Namen werden
  fortlaufend nummeriert }
{ Durch den "warp" Prozess zwingt aIC alle Bilder in den vorgegebenen Rahmen,
  Überstände werden beschnitten, leere Bereiche auf NoData gesetzt. Der Stack
  entspricht aufeinander liegenden multispektralen Bildern }

procedure tArchive.xImageImport(
  iEpg:integer; //Koordinatensystem als EPSG-Code
  iPix:integer; //Pixel-Größe Ergebnis, Null für keine Veränderung
  sArt:string; //Formel für Kanal-Auswahl
  sFrm:string; //Vektor-Auswahl-Rahmen ODER leer für erstes Bild
  slImg:tStringList); //selektierte Kanäle
const
  cEpg = 'aII: Coordinate system unknown. Please add projektion!';
  cStk = 'aII: No bands found at position: ';
var
  fxBnd:tn2Sgl=nil; //Bildkanal
  iHig,iLow:integer; //letzter, erster Kanal (ab Eins!)
  iStk:integer=0; //Kanäle im Ergebnis
  rFrm:trFrm; //Auswahl-Rahmen
  rHdr:trHdr; //aktuelle Metadaten
  sCmp:string=''; //Name nach gdal_warp
  sFrq:string=''; //Namen der einzelnen Kanäle
  B,I:integer;
begin
  if iEpg=0 then
  begin
    iEpg:=Cover.CrsInfo(slImg[0]); //Projektion im ersten Bild
    if iEpg<0 then Tools.ErrorOut(3,cEpg+slImg[0]);
  end;
  if length(sFrm)>0
    then rFrm:=Cover.VectorFrame(sFrm) //ROI aus Polygon
    else rFrm:=Cover._RasterFrame(slImg[0]); //ROI aus erstem Bild

  for I:=0 to pred(slImg.Count) do
  begin
    sCmp:=eeHme+'c_'+ChangeFileExt(ExtractFileName(slImg[I]),''); //zwischenlager
    Gdal.Warp(iEpg,iPix,rFrm,slImg[I],sCmp); //Transformieren, an Raster ausrichten
    slImg[I]:=sCmp; //neuer Name
  end;

  Reduce.GetBands(iHig,iLow,MaxInt,sArt); //Kanal-Nummern aus Eingabe
  if iHig<MaxInt then
  begin
    iStk:=succ(iHig-iLow); //neue Anzahl Kanäle
    Header.Read(rHdr,slImg[0]); //einmal sollte genügen
    for I:=0 to pred(slImg.Count) do
    begin
      sFrq:=''; //neu beginnen
      Tools.EnviCopy(slImg[I],eeHme+'temp'); //Zwischenlager
      for B:=pred(iLow) to pred(iHig) do //gewählte Kanäle
      begin
        fxBnd:=Image.ReadBand(B,rHdr,eeHme+'temp'); //aus Kopie laden
        Image.WriteBand(fxBnd,B-pred(iLow),slImg[I]); //Original überschreiben
        sFrq+='B'+IntToStr(succ(B))+#10; //Kanal-Nummer + Zeilentrenner
      end;
      Delete(sFrq,length(sFrq),1); //Letzte Zeilentrenner löschen
      Header.WriteMulti(iStk,iStk,rHdr,sFrq,slImg[I]);
      Tools.HintOut(true,'ImageImport: '+ExtractFileName(slImg[I]));
    end;
  end;
end;

{ aCI extrahiert der String Nr. "iPst" aus "sNme" und vergleicht ihn mit den
  Vorgaben in "slItm". Die Worte müssen durch Underscores "_" getrennt sein.
  Die Vorgabe muss zumindest Teil des extrahierten Worts sein. aCI prüft dabei
  nur den Dateinamen, nicht den Pfad }

function tArchive.CheckItem(
  sNme:string; //Datename
  iPst:integer; //Position des gesuchten Worts (ab Eins)
  slItm:tStringList): //zulässige Ergebnisse als Liste
  boolean; //Treffer!
var
  sPrt:string=''; //mit "iPst" gesuchtes Wort aus "sNme"
  I:integer;
begin
  if (iPst>0) and (slItm.Count>0) then
  begin
    Result:=False; //Vorgabe = Übereinstimmung suchen
    sNme:=Tools._LastName(sNme);
    sPrt:=ExtractWord(iPst,sNme,['_']); //gesuchtes Wort
    for I:=0 to pred(slItm.Count) do
      if slItm[I]=sPrt then
      begin
        Result:=True;
        break
      end;
  end
  else Result:=True; //alles akzeptieren
end;

{ aCP prüft ob das Wort Nr. "iPst" im String "sNme" als Datum interpretiert
  werden kann und in den Zeitraum von "iLow" bis "iHig" fällt. Die Worte in
  "sNme" müssen duch Underscores "_" getrennt sein. aCP prüft nur den Datei-
  Namen ohne den Pfad. }

function tArchive.CheckPeriod(
  sNme:string; //Datename OHNE Pfad!
  iPst:integer; //Position des gesuchten Worts (ab Eins)
  iLow:integer; //erstes Datum als Zahl (YYYYMMDD)
  iHig:integer): //letztes Datum als Zahl (YYYYMMDD)
  boolean; //Treffer!
var
  iDat:integer=0; //Abschnitt als Zahl
begin
  if iPst>0 then
  begin
    Result:=False;
    sNme:=Tools._LastName(sNme);
    if TryStrToInt(LeftStr(ExtractWord(iPst,sNme,['_']),8),iDat) then
      if (iDat>=iLow) and (iDat<=iHig) then
        Result:=True;
  end
  else Result:=True; //alles akzeptieren
end;

{ aBE extrahiert Kanäle aus Archiven und gibt die Namen der Verzeichnisse für
  die ausgewählten Bilder zurück. Kachel, Datum und Kanäle sind wählbar. Mit
  "bQlt" wird der landsat-QA-Kanal ergänzt }
{ aBE übernimmt in "sArc" einen Suchstring für alle Archive und reduziert diese
  Liste mit "sFrq", sPrd" und "sTls" für Kanal-ID, Zeitperiode und Kachel-ID.
  Kachel- und Kanal-ID können als CSV-Liste übergeben werden, das Datum muss
  als Periode im Format YYYYMMDD-YYYYMMDD übergeben werden. }
{ aBE erzeugt zunächst eine Liste aller Archive die zur Eingabe-Maske "sArc"
  passen und reduziert sie mit "sPrd" und "sTls". Im zweiten Schritt erzeugt
  aBE in "slBnd" ein Inhaltsverzeichnis der Archivs, wählt daraus alle Kanäle,
  die zu "sTls" passen und extrahiert die gewählten Kanäle in Verzeichnisse mit
  einem verkürzten Namen des Archivs }

// wahlweise Archive extrahieren ODER Kanäle kopieren, beide selektiv

function tArchive.xSelectLandsat(
  bQlt:boolean; //QA-Layer integrieren
  sArc:string; //Filter für Archiv-Namen
  sFrq:string; //zulässige Kanal-Namen als CSV.
  sPrd:string; //Zeitperiode als "YYYYMMDD-YYYYMMDD"
  sTls:string): //zulässige Kachel-IDs als CSV.
  tStringList; //Namen der gewählten Archive
const
  cBnd='aBE: Band names not found: ';
  cDat='aBE: Image Archives not found at: ';
  cPrd='aBE: Date input [YYYYMMDD-YYYYMMDD] not defined: ';
  cPst='aBE: Sensor type not detected!';
var
  iHig,iLow:integer; //Datum bis|von als YYYYMMDD
  slBnd:tStringList=nil; //Liste mit passenden Kanal-Namen
  slFrq:tStringList=nil; //zulässige Kanal-Namen (IDs des Providers)
  slTle:tStringList=nil; //zulässige Kachel-Namen (IDs des Providers)
  A,B:integer;
  qA:string;
begin
  Result:=Tools.FileFilter(sArc); //Archiv-Namen Vorauswahl
  if Result.Count<1 then Tools.ErrorOut(3,cDat+sArc); //leere Liste
  GetPeriod(iHig,iLow,sPrd); //Zeitperiode [iLow..iHig] ODER Alles
  try
    slFrq:=tStringList.Create;
    if bQlt and (sFrq<>'') then sFrq+=', PIXEL'; //QA-Maske ergänzen
    slFrq.CommaText:=sFrq; //gewählte Kanal-Namen als Liste
    slTle:=tStringList.Create;
    slTle.CommaText:=sTls; //gewählte Kachel-IDs als Liste

//- sensorunabhängig -----------------------------------------------------------
    for A:=pred(Result.Count) downto 0 do
    begin
      qA:=Result[A];
      if (CheckItem(Result[A],cmLst[1],slTle)=False) //passende Kachel?
      or (CheckPeriod(Result[A],cmLst[3],iLow,iHig)=False) then //passendes Datum?
        Result.Delete(A); //Verzeichnis aus Liste entfernen
    end;
    if Result.Count=0 then Tools.ErrorOut(3,cDat+sArc); //leere Liste
//------------------------------------------------------------------------------

    for A:=0 to pred(Result.Count) do
    begin
      qA:=Result[A];
      slBnd:=TarContent(Result[A],nil); //Kanäle im Archiv ohne Pfadnamen

//- sensorunabhängig -----------------------------------------------------------
      for B:=pred(slBnd.Count) downto 0 do
        if CheckItem(slBnd[B],cmLst[2],slFrq)=False then
          slBnd.Delete(B); //Kanal aus Liste löschen
      if slBnd.Count=0 then Tools.ErrorOut(3,cBnd+slFrq.CommaText); //leere Liste
//------------------------------------------------------------------------------

      Result[A]:=TarExtract(Result[A],slBnd); //slBnd nach Result[A] extrahieren
    end;
  finally
    slBnd.Free;
    slFrq.Free;
    slTle.Free;
  end;
end;

{ aBE extrahiert Kanäle aus Archiven und gibt die Namen der Verzeichnisse für
  die ausgewählten Bilder zurück. Kachel, Datum und Kanäle sind wählbar. Mit
  "bQlt" wird der landsat-QA-Kanal ergänzt }
{ aBE übernimmt in "sArc" einen Suchstring für alle Archive und reduziert diese
  Liste mit "sFrq", sPrd" und "sTls" für Kanal-ID, Zeitperiode und Kachel-ID.
  Kachel- und Kanal-ID können als CSV-Liste übergeben werden, das Datum muss
  als Periode im Format YYYYMMDD-YYYYMMDD übergeben werden. }
{ aBE erzeugt zunächst eine Liste aller Archive die zur Eingabe-Maske "sArc"
  passen und reduziert sie mit "sPrd" und "sTls". Im zweiten Schritt erzeugt
  aBE in "slBnd" ein Inhaltsverzeichnis der Archivs, wählt daraus alle Kanäle,
  die zu "sTls" passen und extrahiert die gewählten Kanäle in Verzeichnisse mit
  einem verkürzten Namen des Archivs }
{ ToDo: Sentinel-Kanäle auch aus Archiv extrahieren }

function tArchive.xSelectSentinel(
  sArc:string; //Filter für Archiv-Namen
  sFrq:string; //zulässige Kanal-Namen als CSV.
  sPrd:string; //Zeitperiode als "YYYYMMDD-YYYYMMDD"
  sTls:string): //zulässige Kachel-IDs als CSV.
  tStringList; //Namen der gewählten Archive
const
  cBnd='aBE: Band names not found: ';
  cDat='aBE: Image Archives not found at: ';
  cPrd='aBE: Date input [YYYYMMDD-YYYYMMDD] not defined: ';
  cPst='aBE: Sensor type not detected!';
var
  iHig,iLow:integer; //Datum bis|von als YYYYMMDD
  slBnd:tStringList=nil; //Liste mit passenden Kanal-Namen
  slFrq:tStringList=nil; //zulässige Kanal-Namen (IDs des Providers)
  slTle:tStringList=nil; //zulässige Kachel-Namen (IDs des Providers)
  A,B:integer;
  qA:string;
begin
  Result:=Tools._DirectoryFilter_(sArc); //Verzeichnisse Vorauswahl
  if Result.Count<1 then Tools.ErrorOut(3,cDat+sArc); //leere Liste
  GetPeriod(iHig,iLow,sPrd); //Zeitperiode ODER Alles
  try
    slFrq:=tStringList.Create;
    slFrq.CommaText:=sFrq; //gewählte Kanal-Namen als Liste
    slTle:=tStringList.Create;
    slTle.CommaText:=sTls; //gewählte Kachel-IDs als Liste

    //siehe Landsat
    for A:=pred(Result.Count) downto 0 do
      if (CheckItem(Result[A],cmS2b[1],slTle)=False) //passende Kachel?
      or (CheckPeriod(Result[A],cmS2b[3],iLow,iHig)=False) then //passendes Datum?
        Result.Delete(A); //Verzeichnis aus Liste entfernen
    if Result.Count=0 then Tools.ErrorOut(3,cDat+sArc); //leere Liste

    //siehe Landsat
    for A:=0 to pred(Result.Count) do
    begin
      qA:=Result[A];
      slBnd:=Tools.FileFilter(Result[A]+'*'); //Kanäle im Verzeichnis ohne Pfadnamen
      for B:=pred(slBnd.Count) downto 0 do
        if CheckItem(slBnd[B],cmS2b[2],slFrq)=False then
          slBnd.Delete(B); //Kanal aus Liste löschen
      if slBnd.Count=0 then Tools.ErrorOut(3,cBnd+slFrq.CommaText); //leere Liste
      Result[A]:=Tools._ImportList(Result[A],slBnd); //Kanäle im Arbeitsverzeichnis
    end;
  finally
    slBnd.Free;
    slFrq.Free;
    slTle.Free;
  end;
end;

// gültiges Datum am Ende von allen Dateinamen in slImg?

function tArchive.ValidDate(slImg:tStringList):boolean;
var
  iFit:integer=0; //Anzahl gültige Angaben
  iDat:integer=0; //Nimmt Datum auf
  I:integer;
begin
  Result:=False;
  for I:=0 to pred(slImg.Count) do
    if TryStrToInt(RightStr(slImg[I],8),iDat) then inc(iFit);
  Result:=iFit=slImg.Count;
end;

// Anzahl Kanäle für alle Bilder gleich?

function tArchive.EqualStack(
  iStk:integer; //Anzahl Kanäle "soll"
  slImg:tStringList):boolean;
var
  iFit:integer=0; //nzahl Kanäle wie slImg[0]
  I:integer;
begin
  Result:=False;
  for I:=0 to pred(slImg.Count) do
    if StrToInt(Header.ReadLine(False,'bands',slImg[I]))=iStk then inc(iFit); //passt!
  Result:=iFit=slImg.Count;
end;

{ aMB verknüpft alle Bilder in "slImg" zu einem Stack und vereinigt dabei
  Kanäle aus dem gleichen Flugpfad. Dazu müssen CRS, Bildgröße und Pixel gleich
  sein. Nach "ImageImport" ist das der Fall. Mit "sTrg" kann ein Name gewählt
  werden. }
{ aMB überträgt nacheinander gleiche Kanäle aus den verschiedenen Bildern in
  den Stack. Dabei überschreibt aIC Kanäle mit gleichem Datum, alle anderen
  werden in getrennte Layer kopiert. Gleiches Datum bedeutet gleicher Flugpfad.
  aIC sortiert die Bilder zu Beginn der Procedur. aIC zählt die resultierenden
  Kanäle und beschriftet sie nach Bild und Kanal-ID }
{ ==> GDAL.WARP ZUSAMMEN MIT EINEM FRAME GARANTIERT, DASS ALLE BILDAUSSCHNITTE
      GLEICH GROSS SIND }

procedure tArchive.xMergeBands(
  sTrg:string; //Ergebnis-Name ODER '' für "compile"
  slImg:tStringList); //selektierte Bilder
const
  cImg = 'aMB: Selected image names must end with a date [YYYYMMDD]!';
  cStk = 'aMB: Selected imageges must share the same number of bands!';
var
  fxRes:tn2Sgl=nil; //Bildkanal Ergebnis
  fxTmp:tn2Sgl=nil; //Bildkanal Zwischenlager
  iHig:integer=0; //Ende der Periode mit gleichem Datum
  iLow:integer=0; //Beginn der Periode mit gleichem Datum
  iStk:integer=0; //Kanäle im Ergebnis
  rHdr:trHdr; //aktuelle Metadaten
  sFrq:string=''; //Namen der einzelnen Kanäle
  B,I,X,Y:integer;
begin
  //slImg=nil? slImg.Count=0?
  Header.Read(rHdr,slImg[0]); //Vorbild
  if not ValidDate(slImg) then Tools.ErrorOut(3,cImg);
  if not EqualStack(rHdr.Stk,slImg) then Tools.ErrorOut(3,cStk);
  slImg.CustomSort(@EndDate); //Nach Datum [YYYYMMDD] am Ende sortieren
  if sTrg='' then sTrg:=eeHme+cfCpl; //Vorgabe-Name
  repeat
    iHig:=iLow;
    while (iHig<pred(slImg.Count)) //Nachfolger existiert
    and (RightStr(slImg[succ(iHig)],8)=RightStr(slImg[iLow],8)) do //gleiches Datum
      inc(iHig);
    for B:=0 to pred(rHdr.Stk) do
    begin
      fxRes:=Image.ReadBand(B,rHdr,slImg[iLow]); //Ergebnis-Vorlage
      for I:=succ(iLow) to iHig do
      begin
        fxTmp:=Image.ReadBand(B,rHdr,slImg[I]);
        for Y:=0 to pred(rHdr.Lin) do
          for X:=0 to pred(rHdr.Scn) do
            if not isNan(fxTmp[Y,X]) then
              fxRes[Y,X]:=fxTmp[Y,X];
      end;
      Image.WriteBand(fxRes,iStk,sTrg); //(vereinigtes) Ergebnis
      sFrq+='B'+IntToStr(succ(iStk mod rHdr.Stk))+'-'+IntToStr(succ(B))+#10;
      inc(iStk);
    end;
    iLow:=succ(iHig);
  until iLow>=slImg.Count;
  Delete(sFrq,length(sFrq),1); //Letztes Zeichen (#10) löschen
  Header.WriteMulti(rHdr.Stk,iStk,rHdr,sFrq,sTrg);
  Tools.HintOut(true,'MergeBands: '+ExtractFileName(sTrg));
end;

{ aQI bestimmt den Anteil klarer Pixel im QA-Layer aus dem Verzeichnis "sDir",
  speichert den Layer als [0,1]-Maske "mask" und gibt die Namen aller anderen
  Layer im Verzeichnis "sDir" zurück. Dabei verwendet aQI nur den Ausschnitt
  "sFrm". Wenn der QA-Layer weniger als 50% klare Pixel enthält, ist die Namen-
  Liste leer }
{ aQI erzeugt mit "sFrm" einen Ausschnitt aus dem QA-Layer und transformiert
  den Binär-Code für Wolken, Schatten und Cirren in eine [0,1]-Maske. aQI zählt
  die klaren Pixel. Die Maske muss mindestens 1000 Pixel und 50% Abdeckung für
  echte Pixel haben. aQI speichert akzeptierte Masken als "mask" und gibt die
  Namen der übrigen Layer aus dem Verzeichnis "sDir" nur dann zurück, wenn die
  Maske akzeptiert ist }
{ todo: Eigene Q-Maske: Ausreißer aus kurzen Zeitreihen }

function tArchive.xQualityMask(
  sFrm:string; //Rahmen mit Kachel-ID
  sDir:string): //Verzeichnisse mit extrahierten Bildern
  single; //Qualität ablehnen|akzeptieren = [0,1]
const
  cCld = $300; //2⁸+2⁹ = Clouds (768)
  cSdw = $C00; //2¹⁰+2¹¹ = Shadow (3072)
  cIce = $3000; //2¹²+2¹³ = Ice, Snow (12288)
  cCir = $C000; //2¹⁴+2¹⁵ = Cirrus (49152)
var
  iClr:integer=0; //Anzahl klare Pixel in der Maske
  iCnt:integer=0; //Anzahl definierte Pixel in der Maske
  ixBin:tn2Wrd=nil; //QA-Kanal, Ausschnitt "rFrm"
  rFrm:trFrm; //Auswahl-Rahmen aus Eingabe
  rHdr:trHdr; //Metadaten Vorbild
  slBnd:tStringList=nil;
  R,X,Y:integer;
begin
  Result:=0; //Vorgabe
  //slDir.Count>in Parse.Import
  try
    slBnd:=Tools.FileFilter(Tools.SetDirectory(sDir)+'*'); //alle Dateien im Verzeichnis
    slBnd.Sort; //Kanäle ordnen
    for R:=0 to pred(slBnd.Count) do
      if RightStr(slBnd[R],13)='_QA_PIXEL.TIF' then //nur Landsat QA-Kanäle
      begin
        rFrm:=Cover.FrameWarp(sFrm,slBnd[R]); //Rahmen in Projektion der Bilddaten
        Gdal.Translate(0,0,1,rFrm,slBnd[R],eeHme+cfMsk); //Ausschnitt "rFrm" im ENVI-Format
        Header.Read(rHdr,eeHme+cfMsk); //Metadaten
        ixBin:=Image.ReadWord(rHdr,eeHme+cfMsk); //QA-Layer als 16-Bit-Integer Ausschnitt "rFrm"

        iClr:=0; iCnt:=0; //neu zählen
        for Y:=0 to high(ixBin) do
          for X:=0 to high(ixBin[0]) do
            if ixBin[Y,X]>1 then //definierter Bildbereich
            begin
              inc(iCnt); //Summe definierte Pixel
              if (ixBin[Y,X] and cCld=cCld) //binäre Marker für Wolken und Schatten
              or (ixBin[Y,X] and cSdw=cSdw)
              or (ixBin[Y,X] and cCir=cCir) //Bildstörung
                then ixBin[Y,X]:=0 //Pixel maskieren
                else ixBin[Y,X]:=1; //Pixel übernehmen
              if ixBin[Y,X]>0 then inc(iClr) //klare Pixel zählen
            end
            else ixBin[Y,X]:=0; //Pixel maskieren

        if iClr>1000 //mindestens 1000 Pixel
          //iQlt:=iClr*1000 div iCnt; //rundet ab
          then Result:=iClr/iCnt //Anteil klare Pixel als Promille
          else Result:=0; //Qualität ablehnen
        //Result.Objects[R]:=tObject(pointer(iQlt)); //QA-Layer & Quality-ID listen
        if Result>0.5 then //mindestens 50% Abdeckung
          Image.WriteWord(ixBin,eeHme+cfMsk); //Daten überschreiben, Header bleibt gültig
        Tools.HintOut(true,'QualityMask: '+FloatToStrF(Result,ffFixed,7,3));
      break; //nur ein Kanal
    end;
  finally
    slBnd.Free;
  end;
end;

{ aBI kombiniert alle Kanäle aus "slBnd" zu einem Multi-Kanal-Bild in den
  Grenzen von "sFrm", speichert das Ergenis mit STD-Namen und gibt den Namen
  zurück. Mit "bQlt" maskiert aBI alle Pixel, die in der Maske den Wert Null
  haben }
{ aBI transformiert das CRS von "sFrm" in das CRS der Bilddaten, beschneidet
  damit das Original und bildet einen Stack aus allen Kanälen in "slBnd". aBI
  speichert das Ergebnis als "Sensor_Kachel_Kanal_Datum" }

function tArchive.xBandsImport(
  bQlt:boolean; //QA-Maske verwenden
  iSns:integer; //Sensor-Typ
  sFrm:string; //Auswahl-Rahmen
  sDir:string): //Verzeichnis mit Bilddaten
  string; //Name für Multikanal-Bild
const
  cHdr = 'aBI: Quality mask and image bands do not match: ';
var
  aIdf:taIdf; //Position der Marker im Original
  fxBnd:tn2Sgl=nil; //ausgeschnittener Kanal
  ixMsk:tn2Wrd=nil; //Maske
  rFrm:trFrm; //Rahmen und Kachel-ID
  rHdr:trHdr; //Metadaten
  sFrq:string=''; //Kanal-Namen, getrennt durch Zeilenwechsel
  slBnd:tStringList=nil; //Kanal-Namen
  B,X,Y:integer;
begin
  case iSns of
    1:aIdf:=cmLst;
    3:aIdf:=cms2b;
    else aIdf:=cmImd;
  end;
  if iSns<>1 then bQlt:=False; //QA-Layer NUR für Landsat
{ TODO: Die Position der Identifier im Dateinamen je nach Sensor muss ein Array
  werden und der Sensor-Typ ist der Index. Anpassen!}

  try
    slBnd:=Tools.FileFilter(Tools.SetDirectory(sDir)+'*'); //alle Kanal-Namen im Verzeichnis
    for B:=pred(slBnd.Count) downto 0 do
      if RightStr(slBnd[B],13)='_QA_PIXEL.TIF' then //Landsat QA-Maske
        slBnd.Delete(B); //QA-Maske ignorieren
    slBnd.Sort; //Kanal-Reihenfolge

    Result:=eeHme+ShortName(aIdf,slBnd[0]); //Name im Arbeitsverzeichnis
{ TODO: ShortName liefert nur Sensor }
    rFrm:=Cover.FrameWarp(sFrm,slBnd[0]); //Rahmen in Projektion der Bilddaten
    if bQlt then
    begin //Maske mit QA-Information lesen
      Header.Read(rHdr,eeHme+cfMsk);
      ixMsk:=Image.ReadWord(rHdr,eeHme+cfMsk)
    end;

    for B:=0 to pred(slBnd.Count) do //alle Kanäle (ohne QA-Kanal)
    begin //Bild-Kanäle lesen, maskieren und stapeln
      Gdal.Translate(1,0,1,rFrm,slBnd[B],eeHme+cfImp); //Ausschnitt "rFrm" als "import" speichern
      if B=0 then Header.Read(rHdr,eeHme+cfImp); //einmal müsste genügen
      fxBnd:=Image.ReadBand(0,rHdr,eeHme+cfImp);
      if bQlt then
      begin
        for Y:=0 to pred(rHdr.Lin) do
          for X:=0 to pred(rHdr.Scn) do
            if ixMsk[Y,X]=0 then fxBnd[Y,X]:=NaN;
      end;
      Image.WriteBand(fxBnd,B,Result);
      sFrq+='B'+IntToStr(succ(B))+#10;
    end;
    Delete(sFrq,length(sFrq),1); //Letztes "#10" löschen
    Header.WriteMulti(slBnd.Count,slBnd.Count,rHdr,sFrq,Result);
  finally
    slBnd.Free
  end;
  Tools.HintOut(true,'BandsImport: '+ExtractFileName(Result));
end;

initialization

  Separate:=tSeparate.Create;
  Separate.fcPrd:=0;
  Separate.fcHrz:=0;
  Separate.fcVrt:=0;
  Separate.fcSqr:=0;
  Separate.icCnt:=0;

finalization

  Separate.Free;

end.

{==============================================================================}

{ aIC verknüpft alle Bilder in "slImg" zu einem Stack und vereinigt dabei
  Kanäle aus dm gleichen Flugpfad (Datum). Dazu müssen CRS, Bildgröße und Pixel
  gleich sein. Nach "ImageImport" ist das der Fall. Die Dateinamen müssen mit
  Reduce.SortDate sortiert sein }
{ aIC erkennt gleiche Flugpfade am gleichen Datum im Dateinamen. Das Datum muss
  YYYYMMDD formatiert sein und am Ende des Namens stehen. aIC übernimmt gezielt
  einzelne Kanäle aus den Vorbildern, überschreibt alle Werte<>NaN wenn sie
  dasselbe Datum haben speichert alle anderen Kanäle analog zu aufeinander
  liegenden multispektralen Bildern. Mit "sTrg" kann ein Name gewählt werden,
  Vorgabe ist "compile" }

procedure tArchive.x_ImageCompile_(
  sTrg:string; //Ergebnis-Name ODER '' für "compile"
  slImg:tStringList); //selektierte Kanäle
var
  bGet,bSet:boolean; //Kanal laden, speichern
  fxRes:tn2Sgl=nil; //Bildkanal Ergebnis
  fxTmp:tn2Sgl=nil; //Bildkanal Zwischenlager
  iStk:integer=0; //Kanäle im Ergebnis
  rHdr:trHdr; //aktuelle Metadaten
  sFrq:string=''; //Namen der einzelnen Kanäle
  B,I,X,Y:integer;
begin
  if sTrg='' then sTrg:=eeHme+cfCpl; //Vorgae-Name
  Header.Read(rHdr,slImg[0]); //einmal sollte genügen
  for I:=0 to pred(slImg.Count) do
  begin
    bGet:=(I=0) or (RightStr(slImg[pred(I)],8)<>RightStr(slImg[I],8)); //Datum <> Vorgänger
    bSet:=(I=pred(slImg.Count)) or (RightStr(slImg[succ(I)],8)<>RightStr(slImg[I],8)); //Datum <> Vorgänger
    for B:=0 to pred(rHdr.Stk) do
    begin
      if bGet=False then
      begin
        fxTmp:=Image.ReadBand(B,rHdr,slImg[I]); //Basis kombinieren
        for Y:=0 to pred(rHdr.Lin) do
          for X:=0 to pred(rHdr.Scn) do
            if not isNan(fxTmp[Y,X]) then
              fxRes[Y,X]:=fxTmp[Y,X];
      end
      else fxRes:=Image.ReadBand(B,rHdr,slImg[I]); //neue Basis
      if bSet then
      begin
        Image.WriteBand(fxRes,iStk,sTrg); //vereinigtes Ergebnis
        sFrq+='B'+IntToStr(succ(I))+'_'+IntToStr(succ(B))+#10;
        inc(iStk)
      end;
    end;
  end;
  Delete(sFrq,length(sFrq),1); //Letztes Zeichen (#10) löschen
  Header.WriteMulti(rHdr.Stk,iStk,rHdr,sFrq,sTrg);
  Tools.HintOut(true,'Compile.Stack: '+ExtractFileName(sTrg));
end;

{ aCI extrahiert der String Nr. "iPst" aus "sNme" und vergleicht ihn mit den
  Vorgaben in "slItm". Die Worte müssen durch Underscores "_" getrennt sein.
  Die Vorgabe muss zumindest Teil des extrahierten Worts sein. aCI prüft dabei
  nur den Dateinamen, nicht den Pfad }

function tArchive.C_heckItem_(
  sNme:string; //Datename
  iPst:integer; //Position des gesuchten Worts (ab Eins)
  slItm:tStringList): //zulässige Ergebnisse als Liste
  boolean; //Treffer!
var
  sPrt:string=''; //mit "iPst" gesuchtes Wort aus "sNme"
  I:integer;
begin
  if (iPst>0) and (slItm.Count>0) then
  begin
    Result:=False; //Vorgabe = Übereinstimmung suchen
    sNme:=ChangeFileExt(ExtractFileName(sNme),''); //Name ohne Extension
    sPrt:=ExtractWord(iPst,sNme,['_']); //gesuchtes Wort
    for I:=0 to pred(slItm.Count) do
      //if pos(sPrt,slItm[I])>0 then //Ausdruck im Wort vorhanden
      if slItm[I]=sPrt then
      begin
        Result:=True;
        break
      end;
  end
  else Result:=True; //alles akzeptieren
end;

{ aCP prüft ob das Wort Nr. "iPst" im String "sNme" als Datum interpretiert
  werden kann und in den Zeitraum von "iLow" bis "iHig" fällt. Die Worte in
  "sNme" müssen duch Underscores "_" getrennt sein. aCP prüft nur den Datei-
  Namen ohne den Pfad. }

function tArchive.C_heckPeriod_(
  sNme:string; //Datename OHNE Pfad!
  iPst:integer; //Position des gesuchten Worts (ab Eins)
  iLow:integer; //erstes Datum als Zahl (YYYYMMDD)
  iHig:integer): //letztes Datum als Zahl (YYYYMMDD)
  boolean; //Treffer!
var
  iDat:integer=0; //Abschnitt als Zahl
begin
  if iPst>0 then
  begin
    Result:=False;
    sNme:=ChangeFileExt(ExtractFileName(sNme),''); //Name ohne Extension
    if TryStrToInt(LeftStr(ExtractWord(iPst,sNme,['_']),8),iDat) then
      if (iDat>=iLow) and (iDat<=iHig) then
        Result:=True;
  end
  else Result:=True; //alles akzeptieren
end;

{ aBI kombiniert alle Kanäle aus "slBnd" zu einem Multi-Kanal-Bild in den
  Grenzen von "sFrm", speichert das Ergenis mit STD-Namen und gibt den Namen
  zurück. Mit "bQlt" maskiert aBI alle Pixel, die in der Maske den Wert Null
  haben }
{ aBI transformiert das CRS von "sFrm" in das CRS der Bilddaten, beschneidet
  damit das Original und bildet einen Stack aus allen Kanälen in "slBnd". aBI
  speichert das Ergebnis als "Sensor_Kachel_Kanal_Datum" }

function tArchive.x_BandsImport_(
  bQlt:boolean; //QA-Maske verwenden
  sFrm:string; //Auswahl-Rahmen
  slBnd:tStringList): //selektierte Kanäle
  string; //Name für Multikanal-Bild
const
  cHdr = 'aBI: Quality mask and image bands do not match: ';
var
  aIdf:taIdf; //Position der Marker im Original
  fxBnd:tn2Sgl=nil; //ausgeschnittener Kanal
  ixMsk:tn2Wrd=nil; //Maske
  rFrm:trFrm; //Rahmen und Kachel-ID
  rHdr:trHdr; //Metadaten
  sFrq:string=''; //Kanal-Namen, getrennt durch Zeilenwechsel
  B,X,Y:integer;
begin
  if bQlt
    then aIdf:=cmLst
    else aIdf:=cmS2b;

  Result:=eeHme+ShortName(aIdf,slBnd[0]); //Name im Arbeitsverzeichnis
  rFrm:=Cover.FrameWarp(sFrm,slBnd[0]); //Rahmen in Projektion der Bilddaten
  if bQlt then
  begin //Maske mit QA-Information lesen
    Header.Read(rHdr,eeHme+cfMsk);
    ixMsk:=Image.ReadWord(rHdr,eeHme+cfMsk)
  end;

  for B:=0 to pred(slBnd.Count) do //alle Kanäle (ohne QA-Kanal)
  begin //Bild-Kanäle lesen, maskieren und stapeln
    Gdal.Translate(1,0,1,rFrm,slBnd[B],eeHme+cfImp); //Ausschnitt "rFrm" als "import" speichern
    if B=0 then Header.Read(rHdr,eeHme+cfImp); //einmal müsste genügen
    fxBnd:=Image.ReadBand(0,rHdr,eeHme+cfImp);
    if bQlt then
    begin
      for Y:=0 to pred(rHdr.Lin) do
        for X:=0 to pred(rHdr.Scn) do
          if ixMsk[Y,X]=0 then fxBnd[Y,X]:=NaN;
    end;
    Image.WriteBand(fxBnd,B,Result);
    sFrq+='B'+IntToStr(succ(B))+#10;
  end;
  Delete(sFrq,length(sFrq),1); //Letztes "#10" löschen
  Header.WriteMulti(slBnd.Count,slBnd.Count,rHdr,sFrq,Result);
  Tools.HintOut(true,'Compile.Import: '+ExtractFileName(Result));
end;

{ aBE extrahiert Kanäle aus Archiven und gibt die Namen der Verzeichnisse für
  die ausgewählten Bilder zurück. Kachel, Datum und Kanäle sind wählbar. Mit
  "bQlt" wird der landsat-QA-Kanal ergänzt }
{ aBE übernimmt in "sArc" einen Suchstring für alle Archive und reduziert diese
  Liste mit "sFrq", sPrd" und "sTls" für Kanal-ID, Zeitperiode und Kachel-ID.
  Kachel- und Kanal-ID können als CSV-Liste übergeben werden, das Datum muss
  als Periode im Format YYYYMMDD-YYYYMMDD übergeben werden. }
{ aBE erzeugt zunächst eine Liste aller Archive die zur Eingabe-Maske "sArc"
  passen und reduziert sie mit "sPrd" und "sTls". Im zweiten Schritt erzeugt
  aBE in "slBnd" ein Inhaltsverzeichnis der Archivs, wählt daraus alle Kanäle,
  die zu "sTls" passen und extrahiert die gewählten Kanäle in Verzeichnisse mit
  einem verkürzten Namen des Archivs }

// wahlweise Archive extrahieren ODER Kanäle kopieren, beide selektiv

function tArchive.x_Select_(
  bQlt:boolean; //QA-Layer integrieren
  sArc:string; //Filter für Archiv-Namen
  sFrq:string; //zulässige Kanal-Namen als CSV.
  sPrd:string; //Zeitperiode als "YYYYMMDD-YYYYMMDD"
  sTls:string): //zulässige Kachel-IDs als CSV.
  tStringList; //Namen der gewählten Archive
const
  cBnd='aBE: Band names not found: ';
  cDat='aBE: Image Archives not found at: ';
  cPrd='aBE: Date input [YYYYMMDD-YYYYMMDD] not defined: ';
  cPst='aBE: Sensor type not detected!';
var
  bArc:boolean; //Archiv extrahieren <> Kanäle kopieren
  aIdf:taIdf; //Positionen der Identifier (Sensor - Kachel - Frequenz - Datum)
  iHig,iLow:integer; //Datum bis|von als YYYYMMDD
  slBnd:tStringList=nil; //Liste mit passenden Kanal-Namen
  slFrq:tStringList=nil; //zulässige Kanal-Namen (IDs des Providers)
  slTle:tStringList=nil; //zulässige Kachel-Namen (IDs des Providers)
  A,B:integer;
  qA:string;
begin
  bArc:=ExtractFileExt(sArc)='.tar';
  if bArc
    then Result:=Tools.FileFilter(sArc) //Archiv-Namen Vorauswahl
    else Result:=Tools._DirectoryFilter_(sArc); //Verzeichnisse Vorauswahl
  if Result.Count<1 then Tools.ErrorOut(3,cDat+sArc); //leere Liste

  if (TryStrToInt(LeftStr(sPrd,8),iLow)=False) //erstes Datum
  or (TryStrToInt(RightStr(sPrd,8),iHig)=False) then //letztes Datum
    Tools.ErrorOut(3,cPrd+sPrd);

  try
    slFrq:=tStringList.Create;
    if bQlt and (sFrq<>'') then sFrq+=', PIXEL'; //QA-Maske ergänzen
    slFrq.CommaText:=sFrq; //gewählte Kanal-Namen als Liste
    slTle:=tStringList.Create;
    slTle.CommaText:=sTls; //gewählte Kachel-IDs als Liste

    if bArc
      then aIdf:=cmLst //Landsat
      else aIdf:=cmS2b; //Sentinel-2

    for A:=pred(Result.Count) downto 0 do
      if (CheckItem(Result[A],aIdf[1],slTle)=False) //passende Kachel?
      or (CheckPeriod(Result[A],aIdf[3],iLow,iHig)=False) then //passendes Datum?
        Result.Delete(A); //Verzeichnis aus Liste entfernen
    if Result.Count=0 then Tools.ErrorOut(3,cDat+sArc); //leere Liste

    for A:=0 to pred(Result.Count) do
    begin
      qA:=Result[A];
      if bArc
        then slBnd:=TarContent(Result[A],nil) //Kanäle im Archiv ohne Pfadnamen
        else slBnd:=Tools.FileFilter(Result[A]+'*'); //Kanäle im Verzeichnis ohne Pfadnamen
      for B:=pred(slBnd.Count) downto 0 do
        if CheckItem(slBnd[B],aIdf[2],slFrq)=False then
          slBnd.Delete(B); //Kanal aus Liste löschen
      if slBnd.Count=0 then Tools.ErrorOut(3,cBnd+slFrq.CommaText); //leere Liste
      if bArc
        then Result[A]:=_TarExtract(Result[A],slBnd) //Namen in "slBnd" extrahieren
        else Result[A]:=Tools._ImportList(Result[A],slBnd); //Verzeichnis im Home
    end;
  finally
    slBnd.Free;
    slFrq.Free;
    slTle.Free;
  end;
end;

{ aQI bestimmt den Anteil klarer Pixel im QA-Layer aus dem Verzeichnis "sDir",
  speichert den Layer als [0,1]-Maske "mask" und gibt die Namen aller anderen
  Layer im Verzeichnis "sDir" zurück. Dabei verwendet aQI nur den Ausschnitt
  "sFrm". Wenn der QA-Layer weniger als 50% klare Pixel enthält, ist die Namen-
  Liste leer }
{ aQI erzeugt mit "sFrm" einen Ausschnitt aus dem QA-Layer und transformiert
  den Binär-Code für Wolken, Schatten und Cirren in eine [0,1]-Maske. aQI zählt
  die klaren Pixel. Die Maske muss mindestens 1000 Pixel und 50% Abdeckung für
  echte Pixel haben. aQI speichert akzeptierte Masken als "mask" und gibt die
  Namen der übrigen Layer aus dem Verzeichnis "sDir" nur dann zurück, wenn die
  Maske akzeptiert ist }

function tArchive.x_QualityMask_(
  sFrm:string; //Rahmen mit Kachel-ID
  sDir:string): //Verzeichnisse mit extrahierten Bildern
  tStringList; //Masken-Layer gebildet
const
  cCld = $300; //2⁸+2⁹ = Clouds (768)
  cSdw = $C00; //2¹⁰+2¹¹ = Shadow (3072)
  cIce = $3000; //2¹²+2¹³ = Ice, Snow (12288)
  cCir = $C000; //2¹⁴+2¹⁵ = Cirrus (49152)
var
  iClr:integer=0; //Anzahl klare Pixel in der Maske
  iCnt:integer=0; //Anzahl definierte Pixel in der Maske
  iQlt:integer=0; //Anteil klarer Pixel als promille
  ixBin:tn2Wrd=nil; //QA-Kanal, Ausschnitt "rFrm"
  rFrm:trFrm; //Auswahl-Rahmen aus Eingabe
  rHdr:trHdr; //Metadaten Vorbild
  R,X,Y:integer;
begin
  //slDir.Count>in Parse.Import
  Result:=Tools.FileFilter(Tools.SetDirectory(sDir)+'*'); //alle Dateien im Verzeichnis
  Result.Sort; //Kanäle ordnen
  for R:=0 to pred(Result.Count) do
    if RightStr(Result[R],13)='_QA_PIXEL.TIF' then //nur Landsat QA-Kanäle
    begin
      rFrm:=Cover.FrameWarp(sFrm,Result[R]); //Rahmen in Projektion der Bilddaten
      Gdal.Translate(0,0,1,rFrm,Result[R],eeHme+cfMsk); //Ausschnitt "rFrm" im ENVI-Format
      Header.Read(rHdr,eeHme+cfMsk); //Metadaten
      ixBin:=Image.ReadWord(rHdr,eeHme+cfMsk); //QA-Layer als 16-Bit-Integer Ausschnitt "rFrm"
      Result.Delete(R); //Maske nicht übergeben

      iClr:=0; iCnt:=0; //neu zählen
      for Y:=0 to high(ixBin) do
        for X:=0 to high(ixBin[0]) do
          if ixBin[Y,X]>1 then //definierter Bildbereich
          begin
            inc(iCnt); //Summe definierte Pixel
            if (ixBin[Y,X] and cCld=cCld) //binäre Marker für Wolken und Schatten
            or (ixBin[Y,X] and cSdw=cSdw)
            or (ixBin[Y,X] and cCir=cCir) then //Bildstörung
              ixBin[Y,X]:=0 //Pixel maskieren
            else ixBin[Y,X]:=1; //Pixel übernehmen
          end
          else ixBin[Y,X]:=0;

      if iClr>1000 //mindestens 1000 Pixel
        //iQlt:=iClr*1000 div iCnt; //rundet ab
        then iQlt:=round(iClr/iCnt*1000) //Anteil klare Pixel als Promille
        else iQlt:=0; //keine güligen Pixel
      if iQlt>500 then //mindestens 50% Abdeckung
        Image.WriteWord(ixBin,eeHme+cfMsk) //"Gdal.Import" überschreiben, Header bleibt gültig
      else Result.Clear; //leere Liste übergeben
      //Result.Objects[R]:=tObject(pointer(iQlt)); //QA-Layer & Quality-ID listen
      Tools.HintOut(true,'Compile.Quality: '+ExtractFileName(sDir));
    break; //nur ein Kanal
  end;
end;

{ aTE extrahiert die Dateien "slNme" aus dem Archiv "sArc" mit dem OS-Befehl
  "tar". Die extrahierten Dateien werden von "tar" im aktuellen Arbeits-
  Verzeichnis gespeichert. }
{ ES GIBT "CPIO" FÜR ARCHIVIERTE DATEIEN }

procedure tArchive.T_arExtract_(
  sArc:string; //Archiv-Name
  slNme:tStringList); //gewählte Namen im Archiv
const
  cCmd = 'tar';
var
  slPrm:tStringList=nil; //Parameter für "tar"-Befehl
begin
  SetCurrentDir(eeHme); //Arbeitsverzeichnis als Ziel
  if (slNme<>nil) and (slNme.Count>0) then
  try
    slPrm:=tStringList.Create;
    slPrm.Add('-x'); //liste erzeugen
    slPrm.Add('-f'); //Archiv-Name
    slPrm.Add(sArc); //Archiv-Name
    slPrm.AddStrings(slNme); //gefilterte Namen
    Tools.OsExecute(cCmd,slPrm); //gefilterte Namen extrahieren
    Tools.ErrorLog('aTE:'); //tar-Fehlermeldungen
  finally
    slPrm.Free;
  end;
end;

{ aGP übergibt die Position der Abschnitte für Sensor, Kachel, Frequenz und
  Datum im Dateinamen. Die Abschnitte müssen durch Underscores "_" getrennt
  sein. Andere Abschnitte können dazwischen liegen. }

function tArchive.G_etWordCount(sNme:string):tpIdf;
begin
  Result:=@cmImd; //Vorgabe = Abschnitte Imalys-Intern
  if ExtractFileExt(sNme)='.jp2' then Result:=@cmS2b; //Sentinel-Kanäle (kein anderer Indikator)
  sNme:=ChangeFileExt(ExtractFileName(sNme),''); //Name ohne Extension
  if copy(sNme,6,4)='L2SP' then Result:=@cmLst else //Landsat-Archiv
  if copy(sNme,5,6)='MSIL2A' then Result:=@cmS2a; //Sentinel-2-Archiv
end;

