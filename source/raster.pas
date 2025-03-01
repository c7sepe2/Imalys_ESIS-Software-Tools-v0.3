unit raster;

{ RASTER sammelt Routinen zur Konversion von Bilddaten in ein für Imalys
  passendes Format, zum Filtern von Kanälen und zum Export. Imalys Prozesse
  lesen und schreiben Bilder im ENVI-Format. Die Projektion ist UTM, die Pixel
  sind quadratisch. Imalys erwartet thematische Daten als Paletten-Bitmaps
  (byte), den Zonen-Index als 32 Bit Integer und scalare Daten als 32 Bit Float
  mit kalibrierten Reflektanzen. Prozesse mit führendem "x" (xProcess) werden
  von "custom" aufgerufen.

  FILTER: sammelt Routinen die Werte aus einzelnen Layern ableiten <=> Reduce
  HEADER: liest und schreibt den ENVI-Header
  IMAGE:  liest, schreibt und maskiert Bilddaten und Kanäle }

{$mode objfpc}{$H+}

interface

uses
  Classes, DateUtils, Math, StrUtils, SysUtils, format;

type
  tAlphaMask = function(var rHdr:trHdr; sNme:string):tn2Sgl; //für Header

  tFilter = class(tObject) //Aufruf geprüft 22-11-17
    private
      function Circle(iRds:integer):tnPnt;
      function Combination(fxBnd:tn2Sgl; iBtm,iRgt,iRds:integer):single;
      function Deviation(fxBnd:tn2Sgl; iBtm,iRgt,iRds:integer):single;
      function _Distance_(fLmt:single; fxBnd:tn2Sgl):tn2Sgl;
      procedure _DistanceOut_(fLmt:single; sImg:string);
      function Laplace(fxBnd:tn2Sgl; iRds:integer):tn2Sgl;
      function LowPass(fxBnd:tn2Sgl; iRds:integer):tn2Sgl;
      function _ParseBand_(sLin:string):integer;
      function _ParseValue_(sLin:string):single;
      function _ParseMLT_(sNme:string):tn2Sgl;
      function _RaosDiv_(iCnt,iRds:integer; ixThm:tn2Byt):tn2Sgl;
      function Roughness(fxBnd:tn2Sgl; iRds,iTyp:integer):tn2Sgl;
      function Texture(fxBnd:tn2Sgl; sExc:string):tn2Sgl;
    public
      procedure Calibrate(faFct:tnSgl; fOfs,fNod:single; sImg:string);
      procedure Hillshade(sDem:string);
      function Normalize(iSmp:integer; sImg:string):string;
      procedure ReplaceNan(fNan:single; fxVal:tn2Sgl);
      procedure SetNoData(fNan:single; sImg:string);
      procedure ValueMove(fMve:single; fxImg:tn2Sgl);
      procedure xKernel(iRds:integer; sExc,sImg,sTrg:string);
  end;

  tHeader = class(tObject) //Aufruf geprüft 22-11-17
    private
      procedure BandNamesAdd(var rHdr:trHdr; slHdr:tStringList);
      function ClassNames(var rHdr:trHdr):string;
      function FileType(var rHdr:trHdr):string;
      function Intersect(var iHrz,iVrt:integer; const rHdr,rNew:trHdr; sImg:string):trBox;
      function LayerName(sImg:string):string;
      function _MapInfo_(fLat,fLon,fPix:double; sMif:string):string;
      function _MeanQuality_(sXpq:string):single;
      function PalCode(sCsv:string):tnCrd;
      function RandomPal(iCnt:integer):tnCrd;
      function ThemaPalette(var rHdr:trHdr):string;
      procedure WriteCover(rFrm:trFrm; var rHdr:trHdr; sImg:string);
    public
      procedure AddLine(sCde,sNew,sImg:string);
      function BandCompare(var rHdr:trHdr; sImg:string):boolean;
      procedure CopyRecord(const rHdr:trHdr; var rRdh:trHdr);
      procedure Origin(fLat,fLon:double; var rHdr:trHdr);
      procedure Read(var rHdr:trHdr; sImg:string);
      function ReadLine(sCde,sImg:string):string;
      procedure Write(var rHdr:trHdr; sHnt,sNme:string);
      procedure WriteIndex(iCnt:integer; var rHdr:trHdr; sImg:string);
      procedure WriteMulti(var rHdr:trHdr; sBnd,sImg:string);
      procedure WriteScalar(var rHdr:trHdr; sImg:string);
      procedure WriteThema(iCnt:integer; var rHdr:trHdr; sFld,sRes:string);
      //checked 241116
  end;

  tImage = class(tObject) //Aufruf geprüft 22-11-17
    private
    public
      procedure AlphaMask(sImg:string);
      function HSV(sImg:string):tn3Sgl;
      function Read(const rHdr:trHdr; sNme:string):Tn3Sgl;
      function ReadBand(iBnd:integer; var rHdr:trHdr; sNme:string):Tn2Sgl;
      function ReadThema(const rHdr:trHdr; sNme:string):Tn2Byt;
      function ReadWord(const rHdr:trHdr; sNme:string):Tn2Wrd;
      function SkipMask(sImg:string):Tn2Byt;
      function StackBands(slBnd:tStringList; sMsk:string):string;
      procedure StackImages(slImg:tStringList; sTrg:string);
      procedure WriteBand(fxImg:tn2Sgl; iBnd:integer; sImg:string);
      procedure WriteMulti(fxImg:tn3Sgl; sImg:string);
      procedure WriteThema(ixImg:tn2Byt; sRes:string);
      procedure WriteWord(ixMsk:tn2Wrd; sRes:string);
      procedure WriteZero(iCol,iRow:integer; sRes:string);
      //checked 241116
  end;

var
  Filter: tFilter;
  Header: tHeader;
  Image: tImage;

implementation

uses
  mutual, thema, vector;

function tHeader._MapInfo_(
  fLat,fLon:double; //Koordinaten für linke obere Ecke
  fPix:double; //Pixelgröße
  sMif:string): //bestehender MapInfo-String
  string; //angepasster MapInfo-String
{ hMI passt den MapInfo-String an eine neue linke obere Ecke an. Alle anderen
  Einträge werden unverändert übernommen. }
// map info = {UTM, 1, 1, 357330, 5452670, 10, 10, 33, North,WGS-84}
begin
  Result:=ExtractWord(1,sMif,[',']); //Projektion
  Result:=Result+','+ExtractWord(2,sMif,[',']); //Ursprung Pixel
  Result:=Result+','+ExtractWord(3,sMif,[',']);
  Result:=Result+','+FloatToStr(fLon); //Ursprung Geo
  Result:=Result+','+FloatToStr(fLat);
  Result:=Result+','+FloatToStr(fPix); //Pixelgröße
  Result:=Result+','+FloatToStr(fPix);
  Result:=Result+','+ExtractWord(8,sMif,[',']); //Zone
  Result:=Result+','+ExtractWord(9,sMif,[',']); //Hemisphäre
  Result:=Result+','+ExtractWord(10,sMif,[',']); //Datum
end;

{ iRB liest den Kanal "iBnd" im Bild "rImg" und gibt ihn als Array[Lat,Long]
  zurück. iRB akzeptiert nur Single-Werte im ENVI-Format. }

function tImage.ReadBand(
  iBnd: integer; //Kanal-ID ab Null
  var rHdr: trHdr; //passender Header
  sNme: string): //Bildname
  Tn2Sgl; //Pixelraster
const
  cRst = 'rIFR: 4-Bit-Values (integer, single, longword) required: ';
var
  hImg: integer=-1; //Filehandle "Bild"
  iSze: integer; //Byte pro Bildzeile
  Y: integer; //Bildzeilen-ID
begin
  Result:=nil;
  if not rHdr.Fmt in [3,4,13] then Tools.ErrorOut(2,cRst+sNme);
  Result:=Tools.Init2Single(rHdr.Lin,rHdr.Scn,0); //Pixelraster Ergebnis
  try
    hImg:=Tools.CheckOpen(ChangeFileExt(sNme,''),fmOpenRead); //Bilddaten
    iSze:=rHdr.Scn*SizeOf(single); //Byte pro Bildzeile
    FileSeek(hImg,iBnd*rHdr.Lin*rHdr.Scn*SizeOf(single),0); //Offset
    for Y:=0 to pred(rHdr.Lin) do
    begin
      FileRead(hImg,Result[Y,0],iSze); //Zeile lesen
      if Y and $FFF=$FFF then write(#13+IntToStr((rHdr.Lin-Y) div $FFF))
    end;
  finally
    if hImg>=0 then FileClose(hImg);
  end; //try ..
end;

function tHeader.ThemaPalette(var rHdr:trHdr):string;
{ TP erzeugt für "rHdr" eine Palette mit "rHdr.Cnt" zufälligen Farben. Die
  Farben enthalten mindestens zwei Komponenten und sind nicht zu dunkel. Imalys
  verwaltet die Palette als Integer-Array. Im Header ist sie als RGB-Dichte
  ausgeschrieben. TB setzt das Array direkt und gibt die Schrift-Form zurück. }
const
  cCnt = 'rHTP: At least one class is recommended!';
  cFmt = 'rHTP: Thematic files must be byte formattet!';
  cPal = 'rHTP: Class colors not initialized!';
var
  I: integer;
begin
  Result:='';
  if rHdr.Fmt<>1 then Tools.ErrorOut(2,cFmt);
  if rHdr.Cnt<1 then Tools.ErrorOut(2,cCnt);
  if length(rHdr.Pal)<1 then Tools.ErrorOut(2,cPal);
  Result:='0, 0, 0';
  for I:=1 to rHdr.Cnt do
    Result:=Result+', '+
      IntToStr(rHdr.Pal[I] and $FF)+', '+
      IntToStr((rHdr.Pal[I] and $FF00) shr 8)+', '+
      IntToStr((rHdr.Pal[I] and $FF0000) shr 16);
end;

procedure tHeader.BandNamesAdd(
  var rHdr: trHdr; //Header als Record
  slHdr: tStringList); //Header als Text
{ tHBN schreibt alle Kanal-Namen als Liste aus einzelnen Zeilen. Strings können
  häufig nur $FFF Zeichen aufnehmen! }
const
  cSpc = #32#32; //Einrückung
var
  I: integer;
begin
  if rHdr.aBnd='' then exit;
  slHdr.Add('band names = {'); //Kopfzeile
  for I:=1 to pred(rHdr.Stk) do //Einträge
    slHdr.Add(cSpc+ExtractWord(I,rHdr.aBnd,[#10])+',');
  slHdr.Add(cSpc+ExtractWord(rHdr.Stk,rHdr.aBnd,[#10])+'}'); //letzter Eintrag
end;

function tHeader.FileType(var rHdr:trHdr):string;
{ tHFT schreibt die Typ-Zeile im Envi-Header }
begin
  if (rHdr.Cnt>0) and (rHdr.Fmt=1)
    then Result:='ENVI Classification'
    else Result:='ENVI Standard';
end;

{ tHW schreibt die Werte von "rHdr" als ENVI-Header in die Datei "sNme+cxHdr".
  tHW gibt Kanal-Namen in getrennten Zeilen zurück, um die $FFF-Schwelle für
  Strings nicht zu überschreiten. tHW ergänzt einen Header mit zufälligen
  Farben, wenn Klassen definiert sind (rHdr.Cnt>0) und die Bilddaten Byte-
  formatiert sind (rHdr.Fmt=1).
  ==> FÜR KLASSEN-BILDER: rHdr.Cnt und rHdr.Fmt müssen gesetzt sein }

procedure tHeader.Write(
  var rHdr: trHdr; //Vorbild
  sHnt: string; //Prozess-Stichwort
  sNme: string); //Dateiname
var
  slHdr: tStringList=nil; //nimmt Header auf
begin
  try
    slHdr:=tStringList.Create;
    slHdr.Add('ENVI');
    slHdr.Add('description = {'+sHnt+'}');
    slHdr.Add('samples = '+IntToStr(rHdr.Scn));
    slHdr.Add('lines = '+IntToStr(rHdr.Lin));
    slHdr.Add('bands = '+IntToStr(rHdr.Stk));
    slHdr.Add('period = '+IntToStr(rHdr.Prd));
    slHdr.Add('header offset = 0');
    slHdr.Add('file type = '+FileType(rHdr));
    slHdr.Add('data type = '+IntToStr(rHdr.Fmt));
    slHdr.Add('interleave = bsq');
    if (rHdr.Cnt>0) and (rHdr.Fmt=1) then
    begin
      slHdr.Add('classes = '+IntToStr(succ(rHdr.Cnt)));
      slHdr.Add('class lookup = {'+ThemaPalette(rHdr)+'}');
      slHdr.Add('class names = {'+ClassNames(rHdr)+'}'); //als Klassen-Namen
    end
    else slHdr.Add('field names = {'+rHdr.Fld+'}'); //als Feldnamen
    slHdr.Add('byte order = 0');
    slHdr.Add('map info = {'+rHdr.Map+'}');
    slHdr.Add('coordinate system string = {'+rHdr.Cys+'}');
    BandNamesAdd(rHdr,slHdr);
    slHdr.Add('cell count = '+IntToStr(rHdr.Cnt));
    slHdr.SaveToFile(ChangeFileExt(sNme,cfHdr));
  finally
    slHdr.Free;
  end;
end;

{ IW schreibt as Bild "fxImg" als Raw-Binary im Single-Format in die Datei
  "sImg". ENVI-Bilddaten brauchen einen passenden Header! }

procedure tImage.WriteMulti(
  fxImg: tn3Sgl; //Bilddaten
  sImg: string); //Ergebnis-Name
var
  hImg: integer=-1; //Filehandle "Bild"
  iSze: integer; //Byte pro Bildzeile
  B,Y: integer;
begin
  try
    hImg:=Tools.NewFile(0,0,sImg); //Bilddaten
    iSze:=length(fxImg[0,0])*SizeOf(single); //Byte pro Bildzeile
    for B:=0 to high(fxImg) do
      for Y:=0 to high(fxImg[0]) do
        FileWrite(hImg,fxImg[B,Y,0],iSze); //Zeile schreiben
  finally
    if hImg>=0 then FileClose(hImg);
  end;
end;

{ FR liest die Bilddaten "sImg" im Format "rHdr" und gibt sie als
  Array[Band,Lat,Long] zurück. FR akzeptiert nur Single-Werte im ENVI-Format. }

function tImage.Read(
  const rHdr: trHdr; //Metadaten
  sNme: string): //Dateiname
  Tn3Sgl; //Pixelraster nach Filterung
const
  cRst = 'rIFR: Four byte image format required (single, integer, longword): ';
var
  hImg: integer=-1; //Filehandle "Bild"
  iSze: integer; //Byte pro Bildzeile
  B,Y: integer; //Pixelindex
begin
  Result:=nil;
  if cfByt[rHdr.Fmt]<>4 then Tools.ErrorOut(2,cRst+sNme);
  Result:=Tools.Init3Single(rHdr.Stk,rHdr.Lin,rHdr.Scn,0); //Pixelraster Ergebnis
  try
    hImg:=Tools.CheckOpen(ChangeFileExt(sNme,''),fmOpenRead); //Bilddaten
    iSze:=rHdr.Scn*SizeOf(single); //Byte pro Bildzeile
    for B:=0 to pred(rHdr.Stk) do
      for Y:=0 to pred(rHdr.Lin) do
      begin
        FileRead(hImg,Result[B,Y,0],iSze); //Zeile lesen
        if Y and $FFF=$FFF then write(#13+IntToStr((rHdr.Lin-Y) div $FFF))
      end;
  finally
    if hImg>=0 then FileClose(hImg);
  end; //try ..
end;

{ hRL sucht im Header von "sImg" nach dem Codewort "sCde" und gibt den
  entsprechenden Eintrag zurück }

function tHeader.ReadLine(
  sCde: string; //Bezeichner
  sImg: string): //Bildname
  string; //Werte ohne Code, evt CSL-String
const
  cCde = 'hRL: Item not found: ';
  cImg = 'hRL: File not found: ';
var
  slHdr: tStringList=nil; //ENVI-Header Zeilen
  I: integer;
begin
  Result:='';
  if not FileExists(sImg) then Tools.ErrorOut(2,cImg+sImg);
  try
    slHdr:=tStringList.Create;
    slHdr.LoadFromFile(ChangeFileExt(sImg,cfHdr)); //Header-Vorbild
    for I:=1 to pred(slHdr.Count) do
      if LeftStr(slHdr[I],length(sCde))=sCde then
      begin
        Result:=trim(copy(slHdr[I],succ(pos('=',slHdr[I])),$FFF));
        if Result[1]='{' then delete(Result,1,1); //Klammer entfernen
        if Result[length(Result)]='}' then delete(Result,length(Result),1); //Klammer entfernen
        break; //nur erstes Ergebnis
      end;
  finally
    slHdr.Free;
  end;
  if Result='' then Tools.ErrorOut(2,cCde+sCde);
end;

{ tWM erzeugt Metadaten für alle Kanäle in "sBnd" und speichert sie als
  "sImg.hdr". }

procedure tHeader.WriteMulti(
  var rHdr:trHdr; //Metadaten Vorbild
  sBnd:string; //Kanal-Namen, String mit Zeilentrennern
  sImg:string); //Bild-Name
begin
  rHdr.Cnt:=0; //Scalar
  rHdr.Fmt:=4; //Single-Format
  rHdr.aBnd:=copy(sBnd,1,length(sBnd)); //echte Kopie
  rHdr.Stk:=WordCount(sBnd,[#10]);
  rHdr.Prd:=min(rHdr.Prd,rHdr.Stk); //anpassen
  rHdr.Fld:=''; //keine Attribute
  SetLength(rHdr.Pal,0); //keine Palette
  Write(rHdr,ExtractFileName(sImg),sImg); //Header schreiben
end;

{ tHR liest den ENVI-Header und gibt ihn als "trHdr" Variable zurück. tHR gibt
  Kanal-Namen in einzelnen Zeilen als String-Liste zurück. tHR akzeptiert nur
  quadratische Pixel. }

procedure tHeader.Read(
  var rHdr:trHdr; //Header-Record
  sImg:string); //Bildname
const
  cEnv = 'File is not an ENVI header: ';
  cHdr = 'Image not available or not ENVI formatted: ';
  cPix = 'Imalys algorithms require square pixels!';
var
  fPix: double; //Pixelgröße Lat
  sCde,sVal: string; //Codewort, Werte-Wort
  slHdr: tStringList=nil; //nimmt Header auf
  I: integer;
begin
  if not FileExists(ChangeFileExt(sImg,cfHdr)) then Tools.ErrorOut(2,cHdr+sImg);
  SetLength(rHdr.Pal,0); //Palette entfernen
  rHdr:=crHdr; //Vorgabe
  try
    slHdr:=tStringList.Create;
    slHdr.LoadFromFile(ChangeFileExt(sImg,cfHdr)); //Header-Vorbild
    if slHdr[0]<>'ENVI' then
      Tools.ErrorOut(2,cEnv+ChangeFileExt(sImg,cfHdr));
    for I:=1 to pred(slHdr.Count) do //Alles außer Überschrift
    begin
      if pos('=',slHdr[I])>0 then
      begin
        sCde:=trim(copy(slHdr[I],1,pred(pos('=',slHdr[I])))); //Feldname
        sVal:=trim(copy(slHdr[I],succ(pos('=',slHdr[I])),$FFF)); //Inhalt
        if sVal[length(sVal)]='}' then delete(sVal,length(sVal),1); //hintere Klammer entfernen
        if sVal[1]='{' then delete(sVal,1,1); //vordere Klammer entfernen
      end
      else
      begin
        sVal:=trim(slHdr[I]); //nur Inhalt
        delete(sVal,length(sVal),1); //Komma oder Klammer entfernen
      end;
      if length(sVal)<1 then continue;
      if sCde='samples' then rHdr.Scn:=StrToInt(sVal) else
      if sCde='lines' then rHdr.Lin:=StrToInt(sVal) else
      if sCde='bands' then rHdr.Stk:=StrToInt(sVal) else
      if sCde='period' then rHdr.Prd:=StrToInt(sVal) else
      if sCde='data type' then rHdr.Fmt:=StrToInt(sVal) else
      if sCde='classes' then rHdr.Cnt:=StrToInt(sVal);
      if sCde='class lookup' then rHdr.Pal:=PalCode(sVal);
      if sCde='class names' then rHdr.Fld:=sVal else
      if sCde='map info' then rHdr.Map:=sVal else
      if sCde='projection info' then rHdr.Pif:=sVal else
      if sCde='coordinate system string' then rHdr.Cys:=sVal else
      if sCde='band names' then rHdr.aBnd+=sVal+#10 else
      if sCde='field names' then rHdr.Fld:=sVal else
      if sCde='cell count' then rHdr.Cnt:=StrToInt(sVal) else
         continue;
    end;
    if length(rHdr.Map)>0 then
    begin
      fPix:=StrToFloat(ExtractDelimited(7,rHdr.Map,[',']));
      rHdr.Pix:=StrToFloat(ExtractDelimited(6,rHdr.Map,[',']));
      if (rHdr.Pix-fPix)/(rHdr.Pix+fPix)>1/10000 then
        Tools.ErrorOut(2,cPix);
      rHdr.Lat:=StrToFloat(ExtractDelimited(5,rHdr.Map,[',']));
      rHdr.Lon:=StrToFloat(ExtractDelimited(4,rHdr.Map,[',']));
    end;
    if length(rHdr.aBnd)<1 then
    begin
      rHdr.aBnd:='band_1';
      for I:=2 to rHdr.Stk do
        rHdr.aBnd+=#10+'band_'+IntToStr(I); //neutrale Kanal-Namen
    end;
    if rHdr.Prd<1 then rHdr.Prd:=rHdr.Stk;
  finally
    slHdr.Free;
  end;
end;

{ hRP erzeugt eine Palette mit Zufalls-Farben. Die Farben haben eine mittlere
  Helligkeit und sind eine Mischung aus ein oder zwei Grundfarben. }

function tHeader.RandomPal(
  iCnt: integer): //Anzahl Klassen (ohne Rückweisung)
  tnCrd; //Paletten-Farben als Array
var
  I,R,G,B: integer;
begin
  Result:=nil;
  Result:=Tools.InitCardinal(succ(iCnt)); //Klassen + NoData
  if iCnt>1 then
  begin
    RandSeed:=cfRds; //Reihe zurückstellen
    for I:=1 to iCnt do
    begin
      repeat
        R:=random($100);
        G:=random($100);
        B:=random($100);
      until (byte(R>$7F) + byte(G>$7F) + byte(B>$7F)) in [1,2];
      Result[I]:=R+(G shl 8)+(B shl 16);
    end;
  end
  else Result[1]:=$FFFFFF; //weiß
end;

function tHeader.PalCode(sCsv:string):tnCrd;
var
  I: integer;
begin
  Result:=Tools.InitCardinal(WordCount(sCsv,[',']) div 3);
  for I:=0 to high(Result) do
    Result[I]:=
      StrToInt(ExtractDelimited(I*3+1,sCsv,[','])) or
      StrToInt(ExtractDelimited(I*3+2,sCsv,[','])) shl 8 or
      StrToInt(ExtractDelimited(I*3+3,sCsv,[','])) shl 16;
end;

{ tFT gibt die Textur des übergebenen Kanals zurück. tFT bestimmt die Textur
  mit einem kreuzförmigen Kernel. Größere Gauß-Kernel können durch Iteration
  emuliert werden. Der Textur-Typ ist wählbar. "cfTxr" bestimmt die mittlere
  Distanz aller Werte als Hauptkomponente der fünf Pixel, "cfIdm" bestimmt
  analog das Inverse Difference Moment und "cfNrm" das Verhältnis von Differenz
  und Summe aller Werte (Modulation). }

function tFilter.Texture(
  fxBnd: tn2Sgl; //Vorbild, ein Kanal
  sExc: string): //Modus: cfTxr, cfIdm, cfNrm
  tn2Sgl; //Textur
var
  iExc: integer=0; //Run-Modus

{ lSD gibt die quadrierte Differenz zwischen "fNbr" und "fVal" zurück. Mit
  "iExc=2" ist die Differenz normalisiert. lSD gibt für Kontakte zu NoData
  Null zurück. }

function _lSqrDiff(const fNbr,fVal:single):single; //quadrierte Differenz
begin
  if isNan(fNbr) then
    Result:=0 //Kontakt zu NoData verändert Summe nicht
  else if (iExc=2) //"normalisiert"
  and ((fVal>0) or (fNbr>0))
    then Result:=sqr((fVal-fNbr)/(abs(fVal)+abs(fNbr))) //normalisierte Differenz
    else Result:=sqr(fVal-fNbr);
end;

var
  fBnd: single=0; //Wert aktueller Pixel
  fRes: single=0; //Wert aktuelle Textur
  X,Y: integer;
begin
  if sExc=cfIdm then iExc:=1 //inverse difference moment
  else if sExc=cfNrm then iExc:=2 //normalisierte Textur
  else iExc:=0; //Standard
  Result:=Tools.Init2Single(length(fxBnd),length(fxBnd[0]),dWord(NaN)); //Vorgabe = NoData
  for Y:=0 to high(fxBnd) do
    for X:=0 to high(fxBnd[0]) do
    begin
      if isNan(fxBnd[Y,X]) then continue;
      fBnd:=fxBnd[Y,X];
      fRes:=0;
      if X>0 then fRes+=_lSqrDiff(fxBnd[Y,pred(X)],fBnd);
      if Y>0 then fRes+=_lSqrDiff(fxBnd[pred(Y),X],fBnd);
      if X<high(fxBnd[0]) then fRes+=_lSqrDiff(fxBnd[Y,succ(X)],fBnd);
      if Y<high(fxBnd) then fRes+=_lSqrDiff(fxBnd[succ(Y),X],fBnd);
      if iExc<>1 then
        Result[Y,X]:=sqrt(fRes) //geometrisches Mittel
      else if fRes>0 then
        Result[Y,X]:=1/sqrt(fRes); //inverses Mittel
    end;
end;

{ fLP emuliert einen Gauß'schen LowPass. Dazu fragt fLP einen Kreuz-Kernel ab,
  gewichtet die Mitte mit dem Faktor 2 und iteriert den Prozess "iRds-1 mal. }

function tFilter.LowPass(
  fxBnd: tn2Sgl; //Vorbild
  iRds: integer): //Kernel-Radius
  tn2Sgl; //Vorlage: Kopie

function lValue(fVal:single; var iCnt:integer):single; //Pixel-Wert: Rückgabe ohne NoData
{ lPV gibt für NoData-Pixel im Original den Wert Null zurück }
begin
  if not isNan(fVal) then
  begin
    Result:=fVal;
    inc(iCnt)
  end
  else Result:=0;
end; //lPixelValue

var
  fTmp: single; //Summe der Differenzen
  iCnt: integer; //gültige Pixel
  I,X,Y: integer;
begin
  Result:=Tools.BandMove(fxBnd); //Ergebnis = Vorbild
  for I:=2 to iRds do
  begin
    for Y:=0 to high(fxBnd) do
      for X:=0 to high(fxBnd[0]) do
      begin
        if IsNan(fxBnd[Y,X]) then continue;
        fTmp:=fxBnd[Y,X]*2; //Vorgabe = eigene Dichte
        iCnt:=2; //Vorgabe = Zentralpixel
        if X>0 then fTmp+=lValue(fxBnd[Y,pred(X)],iCnt);
        if Y>0 then fTmp+=lValue(fxBnd[pred(Y),X],iCnt);
        if X<high(fxBnd[0]) then fTmp+=lValue(fxBnd[Y,succ(X)],iCnt);
        if Y<high(fxBnd) then fTmp+=lValue(fxBnd[succ(Y),X],iCnt);
        Result[Y,X]:=fTmp/iCnt; //LowPass in Kreuz-Kernel
      end;
    if I<iRds then
      fxBnd:=Tools.BandMove(Result); //Ergebnis kopieren
  end;
end;

{ fE bestimmt Rao's Q-Index (Distanzabhängige Divergenz) auf der Basis der
  Klassifikation "ixThm" und speichert das Ergebnis als "entropy". fE bildet
  eine Matrix mit den spektralen Distanzen aller Klassen-Kombinationen "fxDst",
  zählt die Häufigkeit der Pixel pro Klasse in einem Kernel mit Radius "iRds",
  summiert das Produkt aus spektraler Distanz und Häufigkeit für alle Klassen-
  Kombinationen im Kernel und gibt den Mittelwert zurück. fE wiederholt den
  Prozess für alle Pixel im Bild. }

function tFilter._RaosDiv_(
  iCnt: integer; //Anzahl Klassen / Cluster
  iRds: integer; //Kernel-Radius
  ixThm: tn2Byt): //Vorbild = Klassen oder Clusterung
  tn2Sgl; //Entropie der Klassen / Cluster
var
  fRes: single; //Zwischenergebnis
  fxDst: tn2Sgl=nil; //Distanz-Matrix zwischen Klassen
  iaAbd: tnInt=nil; //Klassen-Häufigkeit
  I,K,V,W,X,Y: integer;
begin
  //iRds>0!
  Result:=Tools.Init2Single(length(ixThm),length(ixThm[0]),0); //leeres Ergebnis
  iaAbd:=Tools.InitInteger(succ(iCnt),0); //Klassen-Häufigkeit im Kernel
  fxDst:=Model.FeatureDist; //Distanzen zwischen Klassen-Merkmalen
  for Y:=iRds to high(ixThm)-iRds do
  begin
    for X:=iRds to high(ixThm[0])-iRds do
    begin
      FillDWord(iaAbd[0],succ(iCnt),0);
      for W:=Y-iRds to Y+iRds do
        for V:=X-iRds to X+iRds do
          inc(iaAbd[ixThm[W,V]]); //Pixel pro Klasse
      iaAbd[0]:=0; //Vorgabe
      for I:=1 to iCnt do
        iaAbd[0]+=iaAbd[I]; //Summe Treffer
      iaAbd[0]:=sqr(iaAbd[0]); //für Produkt der Häufigkeiten

      fRes:=0; //Vorgabe
      if iaAbd[0]>0 then
        for I:=2 to iCnt do
          for K:=1 to pred(I) do //alle Kombinationen
            fRes+=iaAbd[I]*iaAbd[K]/iaAbd[0]*fxDst[I,K];
      Result[Y,X]:=fRes; //alle Kombinationen
    end;
    if Y and $FF=$FF then write('.');
  end;
  write(#13); //carriage return
end;

{ iRB liest den Kanal "iBnd" im Bild "rImg" und gibt ihn als Array[Lat,Long]
  zurück. RB akzeptiert nur Byte-Kacheln im ENVI-Format. }

function tImage.ReadThema(
  const rHdr: trHdr; //passender Header
  sNme: string): //Bildname
  Tn2Byt; //Pixelraster
const
  cRst = 'iRB: Byte formatted (thematic) images required: ';
  cStk = 'iRB: Only one layer defined for thematic images: ';
var
  hImg: integer=-1; //Filehandle "Bild"
  Y: integer; //Bildzeilen-ID
begin
  Result:=nil;
  if rHdr.Fmt<>1 then Tools.ErrorOut(2,cRst+sNme);
  if rHdr.Stk>1 then Tools.ErrorOut(2,cStk+sNme);
  Result:=Tools.Init2Byte(rHdr.Lin,rHdr.Scn); //Pixelraster Ergebnis
  try
    hImg:=Tools.CheckOpen(ChangeFileExt(sNme,''),fmOpenRead); //Bilddaten
    for Y:=0 to pred(rHdr.Lin) do
      FileRead(hImg,Result[Y,0],rHdr.Scn); //Zeile lesen
  finally
    if hImg>=0 then FileClose(hImg);
  end; //try ..
end;

{ hWS erzeugt Metadaten für einen Kanal im Single-Format und speichert ihn als
  "sImg.hdr". hWS schreibt den Kanal-Namen als Prozess+Datum. }

procedure tHeader.WriteScalar(
  var rHdr:trHdr; //Metadaten Vorbild
  sImg:string); //Ergebnis-Name
var
  iDat:integer; //für Test
  sDat:string; //Datum [YYYYMMDD] oder leer
begin
  rHdr.Cnt:=0; //Scalar
  rHdr.Fmt:=4; //Single-Format
  rHdr.Stk:=1; //ein Kanal
  rHdr.Prd:=1; //ein Kanal
  rHdr.Fld:=''; //keine Attribute

  sDat:=RightStr(rHdr.aBnd,8); //Datum für Kanal-Namen
  if TryStrToInt(sDat,iDat)
    then rHdr.aBnd:=ExtractFileName(sImg)+'_'+sDat //Prozess+Datum
    else rHdr.aBnd:=ExtractFileName(sImg); //nur Prozess
  SetLength(rHdr.Pal,0); //keine Palette
  Write(rHdr,rHdr.aBnd,sImg); //Header schreiben
end;

function tFilter.Circle(iRds:integer):tnPnt; //Stützpunkte auf Kreis
{ tFC gibt ein Array mit Punkten zurück, die einen lückenlosen Kreis bilden.
  Der radius ist frei wählbar. Der Kordinaten-Ursprung ist in der Kreis-Mitte.}
var
  fRad: single; //Winkel pro Schritt im Bogenmaß
  iPst: integer=0; //Array-Index
  I: integer;
begin
  Result:=nil;
  fRad:=arctan(0.5/iRds); //Winkel im Bogenmaß
  SetLength(Result,round(2*Pi/fRad));
  for I:=0 to high(Result) do
  begin
    Result[iPst].X:=round(cos(I*fRad)*iRds);
    Result[iPst].Y:=round(sin(I*fRad)*iRds);
    if (iPst=0)
    or (Result[iPst].X<>Result[pred(iPst)].X)
    or (Result[iPst].Y<>Result[pred(iPst)].Y)
    then inc(iPst);
  end;
  SetLength(Result,iPst);
end;

function tFilter._Distance_(
  fLmt: single; //Schwelle für Vorbild
  fxBnd: tn2Sgl): //Vorbild (Scalar)
  tn2Sgl; //Distanz zur nächsten Schwelle
{ tFD bestimmt die räumliche Distanz aller Pixel im Bild zu einer Maske, die
  aus einer Schwelle gebildet wird. Dazu initialisiert tFD das Ergebnis mit der
  Maske (Null) und der maximalen Distanz (MaxInt). "MaxInt" ist gleichzeitig
  ein Marker für Pixel ohne Ergebnis. Um den minimalen Radius zu bestimmen
  prüft tFD jeden Pixel mit zunehmendem Radius bis die Maske erreich wird.
  tFD verwendet ein Array aus kreisförmigen Testpunkten "arPnt" das für jeden
  Radius neu gerechnet wird. Die Geometrie kann einzelne Pixel am Bildrand
  ausschließen. tFD setzt nach einer vollständigen Prüfung alle übrigen Pixel
  auf Null}
const
  cMax: single = MaxInt; //maximale Distanz
var
  arPnt: tnPnt=nil; //Kreislinie
  iHit: integer=0; //Treffer pro Radius
  iRds: integer=0; //Suchradius
  P,X,Y: integer;
begin
  Result:=Tools.Init2Single(length(fxBnd),length(fxBnd[0]),dWord(cMax)); //Vorgabe = nicht bearbeitet
  for Y:=0 to high(fxBnd) do
    for X:=0 to high(fxBnd[0]) do
      if isNan(fxBnd[Y,X])
      or (fxBnd[Y,X]<=fLmt) then
        Result[Y,X]:=0; //Maske und NoData

  repeat
    inc(iRds);
    arPnt:=Circle(iRds); //Stützpunkte auf Kreis
    iHit:=0;
    for Y:=iRds to high(fxBnd)-iRds do
      for X:=iRds to high(fxBnd[0])-iRds do
        if Result[Y,X]=cMax then //Punkt nicht bearbeitet
          for P:=0 to high(arPnt) do
            if Result[Y+arPnt[P].Y,X+arPnt[P].X]=0 then //Maske oder NoData
            begin
              Result[Y,X]:=iRds;
              inc(iHit) //Treffer zählen
            end
  until iHit=0;

  for Y:=0 to high(Result) do
    for X:=0 to high(Result[0]) do
      if isNan(fxBnd[Y,X]) then Result[Y,X]:=NaN else
      if Result[Y,X]=cMax then Result[Y,X]:=0; //Marke löschen
end;

procedure tFilter._DistanceOut_(
  fLmt:single; //Schwelle für Maske in scalarem Bild
  sImg:string); //Bild dazu
{ tFDO erzeugt aus einem vorbild mit einem Kanal einen neuen Kanal mit der
  kürzesten Distanz zwischen jedem Bildpixel und einer beliebig geformten
  Maske. tFDO definiert alle Werte unter "fLmt" im Vorbild als maskiert. }
var
  fxRes: tn2Sgl=nil; //Ergebnis-Kanal
  rHdr: trHdr; //gemeinsame Metadaten
begin
  Header.Read(rHdr,sImg); //Metadaten Clusterung
  fxRes:=_Distance_(fLmt,Image.ReadBand(0,rHdr,sImg));
  Image.WriteBand(fxRes,0,eeHme+cfDst); //Bilddaten
  Header.WriteScalar(rHdr,eeHme+cfDst); //Metadaten
  Tools.HintOut(true,'Filter.Distance: '+cfDst);
end;

procedure tFilter.ValueMove(
  fMve:single; // Summand
  fxImg:tn2Sgl); //Vorbild
{ tFVM verschiebt alle Werte im Bild "fxImg" um den Wert von "fMve" }
const
  cNil = 'fVM: Image data not defined!';
var
  X,Y:integer;
begin
  if fxImg=nil then Tools.ErrorOut(2,cNil);
  for Y:=0 to high(fxImg) do
    for X:=0 to high(fxImg[0]) do
      if not IsNan(fxImg[Y,X]) then
        fxImg[Y,X]+=fMve;
end;

procedure tFilter.ReplaceNan(
  fNan:single; // Wert für NoData
  fxVal:tn2Sgl); //Vorbild
{ tFVM verschiebt alle Werte im Bild "fxImg" um den Wert von "fMve" }
var
  X,Y:integer;
begin
  for Y:=0 to high(fxVal) do
    for X:=0 to high(fxVal[0]) do
      if IsNan(fxVal[Y,X]) then
        fxVal[Y,X]:=fNan;
end;

{ iWT speichert den Kanal "ixImg" als Klassen-Bild (Byte) im IDL-Format. }

procedure tImage.WriteThema(
  ixImg:tn2Byt;
  sRes:string); //Ergebnis
var
  hRes: integer=-1; //Filehandle "Bild"
  iSze: integer; //Byte pro Bildzeile
  Y: integer;
begin
  try
    hRes:=Tools.NewFile(0,0,sRes); //Bilddaten
    iSze:=length(ixImg[0]); //Byte pro Bildzeile
    for Y:=0 to high(ixImg) do
      FileWrite(hRes,ixImg[Y,0],iSze); //Zeile schreiben
  finally
    if hRes>=0 then FileClose(hRes);
  end;
end;

{ iRW liest den Kanal "rHdr.Imp" im Word-Format und gibt ihn als Kanal zurück.
  iRW akzeptiert nur Bilder im 16 Bit Format. }

function tImage.ReadWord(
  const rHdr: trHdr; //passender Header
  sNme: string): //Bildname
  Tn2Wrd; //Pixelraster
const
  cRst = 'rIFR: Word formatted (16 Bit) images required: ';
var
  hImg: integer=-1; //Filehandle "Bild"
  Y: integer; //Bildzeilen-ID
begin
  Result:=nil;
  if not rHdr.Fmt in [2,12] then Tools.ErrorOut(2,cRst+sNme);
  Result:=Tools.Init2Word(rHdr.Lin,rHdr.Scn); //Pixelraster Ergebnis
  try
    hImg:=Tools.CheckOpen(ChangeFileExt(sNme,''),fmOpenRead); //Bilddaten
    for Y:=0 to pred(rHdr.Lin) do
      FileRead(hImg,Result[Y,0],rHdr.Scn*SizeOf(word)); //Zeile lesen
  finally
    if hImg>=0 then FileClose(hImg);
  end; //try ..
end;

{ fL gibt eine Laplace-Transformation als Differenz zwischen zwei Gauß-Kerneln
  zurück. Der größere Kernel hat den dreifachen radius des kleineren. Beide
  Kernel werden durch eine Iteration angenähert. }

function tFilter.Laplace(
  fxBnd:tn2Sgl; //Vorbild, ein Kanal
  iRds:integer): //Radius, äußerer Kernel
  tn2Sgl; //Ergebnis, ein Kanal
const
  cLrg = 3-1; //Faktor für größeren Kernel
var
  fxTmp:tn2Sgl=nil; //Zwischenlager
  X,Y:integer;
begin
  Result:=LowPass(fxBnd,iRds); //kleiner Kernel
  fxTmp:=LowPass(Result,iRds*cLrg); //großer Kernel
  for Y:=0 to high(fxBnd) do
    for X:=0 to high(fxBnd[0]) do
      if not isNan(fxTmp[Y,X]) then
        Result[Y,X]:=Result[Y,X]-fxTmp[Y,X]; //Differenz Klein-Groß
end;

function tHeader.BandCompare(
  var rHdr:trHdr; //Referenz
  sImg:string): //zweiter Header oder Bild
  boolean; //Null-Eins für Fehler-Passend
{ hCp prüft, ob das Bild "sImg" genauso groß ist wie "rHdr". Dazu vergleicht
  hCp Anzahl und Größe der Pixel sowie das Pixel-Format. }
{ hCp IGNORIERT DAS KOORDINATENSYSTEM }
var
  rRdh:trHdr; //zweiter header
begin
  Result:=False; //Vorgabe=Fehler
  Header.Read(rRdh,sImg);
  Result:=(rRdh.Lin=rHdr.Lin)
      and (rRdh.Scn=rHdr.Scn)
      and (rRdh.Pix=rHdr.Pix)
      and (rRdh.Fmt=rHdr.Fmt);
end;

function tImage.SkipMask(sImg:string):Tn2Byt; //Bild:Pixelraster
{ iRM gibt eine Maske von "sImg" zurück. Das Ergebnis ist Eins für alle nicht
  definierten Pixel und Null für alle anderen. }
var
  fxBnd:tn2Sgl=nil; //Kanal aus Vorbild
  rHdr:trHdr; //Metadaten
  X,Y: integer; //Bildzeilen-ID
begin
  Result:=nil;
  Header.Read(rHdr,sImg);
  fxBnd:=ReadBand(0,rHdr,sImg);
  SetLength(Result,rHdr.Lin,rHdr.Scn);
  for Y:=0 to pred(rHdr.Lin) do
    for X:=0 to pred(rHdr.Scn) do
      Result[Y,X]:=byte(isNan(fxBnd[Y,X]) or (fxBnd[Y,X]=0));
end;

{ hWI modifiziert den Header "rHdr" für den Zellindex. }

procedure tHeader.WriteIndex(
  iCnt:integer; //Anzahl Zonen
  var rHdr:trHdr; //Metadaten
  sImg:string); //Name der Bilddaten
begin
  rHdr.Fmt:=3; //Integer
  rHdr.Stk:=1; //ein Kanal
  rHdr.Prd:=1; //Ein Kanal
  rHdr.Cnt:=iCnt; //Zellen
  rHdr.aBnd:='cell_index';
  SetLength(rHdr.Pal,0);
  Write(rHdr,'Imalys cell index',sImg); //Header schreiben
end;

function tFilter._ParseBand_(sLin:string):integer;
// extrahiert Wert und Kanal-ID aus USGS "MLT"-Script
const
  cBnd = 'fPB: Band number not defined: ';
var
  iDlm:integer=0;
begin
  Result:=0; //Vorgabe = ungültig
  iDlm:=pos('=',sLin); //Position Trenner
  if TryStrToInt(copy(sLin,iDlm-2,1),Result)=False
  or (Result<1) or (Result>7) then
    Tools.ErrorOut(2,cBnd+IntToStr(Result));
end;

function tFilter._ParseValue_(sLin:string):single;
// extrahiert Wert und Kanal-ID aus USGS "MLT"-Script
const
  cVal = 'fPV: Calibration value not found or not defined';
var
  iDlm:integer=0;
begin
  iDlm:=pos('=',sLin); //Position Trenner
  if not TryStrToFloat(trim(copy(sLin,succ(iDlm),$FF)),Result) then
    Tools.ErrorOut(2,cVal);
end;

function tFilter._ParseMLT_(sNme:string):tn2Sgl;
// liest kalibrierungs-Faktoren aus dem USGS "MLT"-Script
const
  cNme = 'Impossible to open file ';
var
  bGrp:boolean; //Level-2 Parameter
  dTxt:TextFile; //Initialisierung
  sLin:string; //Text-Zeile
begin
  Result:=Tools.Init2Single(2,7,0); //Faktor, Offset für 7 Kanäle
  try
    AssignFile(dTxt,sNme);
    {$i-} Reset(dTxt); {$i+}
    if IOResult<>0 then Tools.ErrorOut(2,cNme+sNme);
    repeat
      readln(dTxt,sLin); //zeilenweise lesen
      bGrp:=pos('GROUP = LEVEL2_SURFACE_REFLECTANCE_PARAMETERS',sLin)>0;
      if bGrp then
        if pos('REFLECTANCE_MULT_BAND',sLin)>0 then
          Result[0,_ParseBand_(sLin)]:=_ParseValue_(sLin) else
        if pos('REFLECTANCE_ADD_BAND',sLin)>0 then
          Result[1,_ParseBand_(sLin)]:=_ParseValue_(sLin) else
        if pos('END_GROUP = LEVEL2_SURFACE_REFLECTANCE_PARAMETERS',sLin)>0 then
          break; //repeat ..
    until eof(dTxt);
  finally
    CloseFile(dTxt);
  end; //of try ..
end;

{ erste drei Kanäle aus "sImg" nach HSV transformieren
  WERTE MÜSSEN MIT NULL BEGINNEN (SHIFT_VALUE)
  NO_DATA MUSS FÜR ALLE KANÄLE GELTEN }

function tImage.HSV(sImg:string):tn3Sgl;
const
  cFex = 'iHV: Image not found: ';
var
  fBlu,fGrn,fRed:single; //Ergebnis RGB
  fNrm:single; //normalisierter Wert
  fHue:single=0; //Maxima
  fSat:single=0;
  fVal:single=0;
  fzHue:tn2Sgl=nil; //Kanäle Vorbild
  fzSat:tn2Sgl=nil;
  fzVal:tn2Sgl=nil;
  rHdr:trHdr; //Metadaten
  X,Y:integer;
begin
  Result:=nil;
  if not FileExists(sImg) then Tools.ErrorOut(2,cFex+sImg);
  SetLength(Result,3,1,1); //Vorgabe
  Header.Read(rHdr,sImg);
  //rHdr.Stk<3?
  Result[0]:=Image.ReadBand(0,rHdr,sImg); fzVal:=Result[0];
  Result[1]:=Image.ReadBand(1,rHdr,sImg); fzHue:=Result[1];
  Result[2]:=Image.ReadBand(2,rHdr,sImg); fzSat:=Result[2];

  for Y:=0 to pred(rHdr.Lin) do
    for X:=0 to pred(rHdr.Scn) do
    begin
      if isNan(fzVal[Y,X]) then continue;
      fHue:=max(fzHue[Y,X],fHue); //Maxima
      fSat:=max(fzSat[Y,X],fSat);
      fVal:=max(fzVal[Y,X],fVal);
    end;

  for Y:=0 to pred(rHdr.Lin) do
    for X:=0 to pred(rHdr.Scn) do
    begin
      if isNan(fzVal[Y,X]) then continue;
      fRed:=0; fGrn:=0; fBlu:=0; //Vorgabe

      //Hue
      fNrm:=fzHue[Y,X]/fHue*3.0; //normalisiert [0..3]
      if fNrm<1 then
      begin
        fRed:=fRed+1-fNrm;
        fGrn:=fGrn+fNrm
      end
      else if fNrm<2 then
      begin
        fGrn:=fGrn+2-fNrm;
        fBlu:=fBlu-1+fNrm
      end
      else
      begin
        fBlu:=fBlu+3-fNrm;
        fRed:=fRed-2+fNrm
      end;

      //Saturation
      fNrm:=1-fzSat[Y,X]/fSat; //normalisiert
      fRed:=fRed+(1-fRed)*fNrm;
      fGrn:=fGrn+(1-fGrn)*fNrm;
      fBlu:=fBlu+(1-fBlu)*fNrm;

      //Value
      fNrm:=fzVal[Y,X]/fVal; //normalisiert
      fzVal[Y,X]:=fRed*fNrm;
      fZHue[Y,X]:=fGrn*fNrm;
      fzSat[Y,X]:=fBlu*fNrm;
    end;

  Image.WriteMulti(Result,eeHme+cfHsv);
  rHdr.Stk:=3; //RGB
  rHdr.aBnd:='pca-1'+#10+'pca-2'+#10+'pca-3'+#10;
  Header.Write(rHdr,'PCA as HSV',eeHme+cfHsv);
end;

procedure tImage.WriteZero(
  iCol,iRow:integer; //Bildgröße
  sRes:string); //Ergebnis
{ iWZ erzeugt einen leeren Kanal im Byte-Format und speichert ihn als "sRes".
  → iWZ SCHREIBT KEINEN HEADER! }
var
  hRes: integer=-1; //Filehandle "Bild"
  iaLin:tnByt=nil; //leere Zeile
  Y: integer;
begin
  iaLin:=Tools.InitByte(iCol); //leere Zeile
  try
    hRes:=Tools.NewFile(0,0,sRes); //Bilddaten
    for Y:=0 to pred(iRow) do
      FileWrite(hRes,iaLin[0],iCol); //Zeile schreiben
  finally
    if hRes>=0 then FileClose(hRes);
  end;
end;

{ hWT modifiziert den übergebenen Header für ein Klassen-Bild und speichert ihn
  als "sRes". Die Zahl der Klassen wird mit "iCnt" übergeben. Das Ergebnis hat
  immer einen Kanal im Byte-Format und eine Farbpalette. Wenn die Palette nicht
  belegt ist, erzeugt hWT Zufallsfarben. Zwei Klassen sind immer schwarz und
  weiß. }

procedure tHeader.WriteThema(
  iCnt:integer; //Anzahl Klassen ohne Rückweisung
  var rHdr:trHdr; //Vorbild
  sFld:string; //Klassen-Namen, kommagetrennt
  sRes:string); //Dateinamen: Ergebnis
begin
  rHdr.Cnt:=iCnt; //Anzahl Klassen ohne Rückweisung
  rHdr.Fmt:=1; //Byte
  rHdr.Prd:=1; //ein Kanal
  rHdr.Stk:=1; //ein Kanal
  rHdr.Fld:=sFld; //Klassen-Namen
  if rHdr.Cnt>=length(rHdr.Pal) then
    rHdr.Pal:=RandomPal(rHdr.Cnt); //Palette mit Zufalls-Farben
    //rHdr.Pal:=_RandomPal(rHdr.Cnt); //Palette mit Zufalls-Farben
  rHdr.aBnd:='thema';
  Write(rHdr,'thema',ChangeFileExt(sRes,'.hdr')); //Header schreiben
end;

function tHeader.ClassNames(var rHdr:trHdr):string;
{ CN schreibt "1+iCnt" Default-Klassen-Namen in einen String }
var
  iCnt:integer; //Anzahl kommagetrennte Strings
  I: integer;
begin
  Result:=rHdr.Fld; //bestehende Liste
  iCnt:=WordCount(Result,[',']);
  if iCnt<succ(rHdr.Cnt) then
    for I:=succ(iCnt) to rHdr.Cnt do
      Result:=Result+', class_'+IntToStr(I);
end;

procedure tHeader.WriteCover(
  rFrm:trFrm; //Koordinaten
  var rHdr:trHdr; //Metadaten
  sImg:string); //Ergebnis
{ tHWS erzeugt Metadaten mit Koordinaten aus "rFrm" und speichert sie als
  "merge.hdr" }

function lSetMap:string;
var
  sMap:string;
  I:integer;
begin
  sMap:=rHdr.Map; //Vorbild
  Result:=ExtractWord(1,sMap,[',']);
  for I:=2 to 3 do
    Result+=','+ExtractWord(I,sMap,[',']);
  Result+=','+FloatToStr(rFrm.Lft);
  Result+=','+FloatToStr(rFrm.Top);
  for I:=6 to 10 do
    Result+=','+ExtractWord(I,sMap,[',']);
end;

const
  cHnt = 'image combination to common frame';
begin
  rHdr.Cnt:=0; //Scalar
  rHdr.Fmt:=4; //Single-Format
  rHdr.Lat:=rFrm.Top; //Koordinaten
  rHdr.Lon:=rFrm.Lft;
  rHdr.Scn:=round((rFrm.Rgt-rFrm.Lft)/rHdr.Pix); //Spalten in Pixeln
  rHdr.Lin:=round((rFrm.Top-rFrm.Btm)/rHdr.Pix); //Zeilen in Pixeln
  rHdr.Fld:=''; //keine Attribute
  rHdr.Map:=lSetMap; //Map-Info anpassen
  SetLength(rHdr.Pal,0); //keine Palette
  Write(rHdr,cHnt,sImg); //Header schreiben
end;

{ fHs speichert die Beleuchtung einer Topographie als "cfHse". Die Werte
  stammen aus "gdaldem" und sind ursprünglich auf Byte normalisiert. }

procedure tFilter.Hillshade(sDem:string);
const
  cFex = 'fHs: Image not found: ';
var
  fxRes:tn2Sgl=nil; //Ergebnis als float
  ixTmp:tn2Byt=nil; //Hillschade aus "gdaldem"
  rHdr:trHdr; //Metadaten
  X,Y:integer;
begin
  if not FileExists(sDem) then Tools.ErrorOut(2,cFex+sDem);
  Gdal.Hillshade(sDem); //Hillshade als Byte-Bild
  Header.Read(rHdr,eeHme+cfHse);
  ixTmp:=Image.ReadThema(rHdr,eeHme+cfHse);
  fxRes:=Tools.Init2Single(length(ixTmp),length(ixTmp[0]),0);
  for Y:=0 to pred(rHdr.Lin) do
    for X:=0 to pred(rHdr.Scn) do
      if ixTmp[Y,X]>0
        then fxRes[Y,X]:=ixTmp[Y,X]/$FF //auf [0..1] normalisieren
        else fxRes[Y,X]:=NaN; //nicht definiert
  Image.WriteBand(fxRes,0,eeHme+cfHse);
  Header.WriteScalar(rHdr,eeHme+cfHse);
  Tools.HintOut(true,'Filter.HillShade: '+cfHse);
end;

{ fDn bestimmt die Abweichung nach Gauß in einem Kernel mit dem Radius "iRds"
  und gibt das Ergebnis als Float zurück.
  → Varianz = (∑x²-(∑x)²/n)/(n-1) }

function tFilter.Deviation(
  fxBnd:tn2Sgl; //Vorbild
  iBtm,iRgt:integer; //Koordinaten rechte untere Ecke
  iRds:integer): //Kernel-Radius
  single; //Gauß'sche Abweichung
var
  fSqr:single=0;
  fSum:single=0;
  iCnt:integer=0;
  V,W:integer;
begin
  for W:=iBtm-iRds*2 to iBtm do
    for V:=iRgt-iRds*2 to iRgt do
      if not isNan(fxBnd[W,V]) then
      begin
        fSqr+=sqr(fxBnd[W,V]);
        fSum+=fxBnd[W,V];
        inc(iCnt)
      end;
  if iCnt>1 then
    Result:=sqrt((fSqr-sqr(fSum)/iCnt)/pred(iCnt));
end;

{ fCn bestimmt Rao's ß-Diversity mit einem quadratischen Kernel und gibt das
  Ergebnis als Float zurück. Um alle Kombinationen zwischen zwei beliebigen
  Pixeln zu erfassen bildet fCn aus dem ursprünglichen Kernel iterativ kleinere
  bis der Prozess bei einen 2x2-Kernel endet. }

function tFilter.Combination(
  fxBnd:tn2Sgl; //Vorbild
  iBtm,iRgt:integer; //Koordinaten rechte untere Ecke
  iRds:integer): //Kernel-Radius (Size=iRds*2+1)
  single; //Rao's ß-Diversität
var
  iCnt:integer=0; //Anzahl gültige Vergleiche
  iExt:integer=0; //Extension des Sub-Kernels
  iLft,iTop:integer; //variable linke obere Ecke
  V,W:integer;
begin
  Result:=0;
  for iExt:=iRds*2 downto 1 do //Sub-Kernel sukzessive verkleinern
  begin
    iLft:=iRgt-iExt;
    iTop:=iBtm-iExt;
    for W:=iTop to iBtm do //vertikale Kombination
      if not isNan(fxBnd[W,iLft]) then
        for V:=succ(iLft) to iRgt do
        begin
          if isNan(fxBnd[W,V]) then continue;
          Result+=abs(fxBnd[W,iLft]-fxBnd[W,V]); //absolute Differenz
          inc(iCnt)
        end;
    for V:=iLft to iRgt do //horizontale Kombination
      if not isNan(fxBnd[iTop,V]) then
        for W:=succ(iTop) to iBtm do
        begin
          if isNan(fxBnd[W,V]) then continue;
          Result+=abs(fxBnd[iTop,V]-fxBnd[W,V]); //absolute Differenz
          inc(iCnt)
        end;
  end;
  if iCnt>0 then Result/=iCnt;
end;

{ fRs bildet die Abweichung nach Gauß oder die Diversity nach Rao mit einem
  beweglichen Kernel und gibt das Ergebnis als Kanal zurück. Beide Ansätze
  liefern gleiche (?) Ergebnisse, werden aber unterschiedlich gerechnet. }

function tFilter.Roughness(
  fxBnd:tn2Sgl; //Vorbild, ein Kanal
  iRds:integer; //Kernel-Radius
  iTyp:integer): //Kernel-Typ
  tn2Sgl; //Ergebnis, ein Kanal
var
  X,Y:integer;
begin
  Result:=Tools.Init2Single(length(fxBnd),length(fxBnd[0]),dWord(NaN)); //Vorgabe = NoData
  for Y:=iRds*2 to high(fxBnd) do
    for X:=iRds*2 to high(fxBnd[0]) do
    begin
      if isNan(fxBnd[Y-iRds,X-iRds]) then continue;
      case iTyp of
        1: Result[Y-iRds,X-iRds]:=Deviation(fxBnd,Y,X,iRds);
        2: Result[Y-iRds,X-iRds]:=Combination(fxBnd,Y,X,iRds);
      end;
    end;
end;

{ fKl filtert das Bild "sImg" mit dem Prozess "sExc" und speichert das Ergebnis
  unter dem Namen des Filters im Imalys-Verzeichnis. Im Gegensatz zu Hillshade
  verwendet fKl keine Bibliotheken. }
{ fKl reduziert Bilder mit mehr als einem Kanal vor der Filterung auf die erste
  Hauptkomponente. Wird ein Kernel-Radius "iRds>1" angebeben, glättet fKl das
  Ergebnis mit einem LowPass. Beim "Laplace"-Prozess steuert "iRds" beide
  Kernel. Er wird nicht nachträglich geglättet. }

procedure tFilter.xKernel(
  iRds:integer; //Kernel-Radius (Distanz zum Zentralpixel)
  sExc:string;//Filter-Prozess
  sImg:string; //Dateiname Vorbild
  sTrg:string); //Ergebnis-Name ODER leer für Prozess-Name
const
  cCmd = 'fKl: Command not defined: filter.kernel.';
  cFex = 'fKl: Image not found: ';
  cRds = 'fKl: Radius must be positive!';
  //cKrn: array[0..3] of string = (cfLpc,cfTxr,cfIdm,cfNrm);
var
  fxRes:tn2Sgl=nil; //Ergebnis-Kanal
  rHdr:trHdr; //gemeinsame Metadaten
begin
  if not FileExists(sImg) then Tools.ErrorOut(2,cFex+sImg);
  SetLength(fxRes,0);
  if iRds<1 then Tools.ErrorOut(2,cRds);
  Header.Read(rHdr,sImg);
  if rHdr.Stk>1
    then fxRes:=Reduce.Brightness(Image.Read(rHdr,sImg)) //erste Hauptkomponente
    else fxRes:=Image.ReadBand(0,rHdr,sImg); //Kanal lesen

  if sExc=cfDvn then fxRes:=Roughness(fxRes,iRds,1) else //Gauß-Abweichung = Rao
  if sExc=cfRog then fxRes:=Roughness(fxRes,iRds,2) else //Rao's ß-Diversity
  if sExc=cfLow then fxRes:=Lowpass(fxRes,iRds) else //Lowpass-Filter
  if sExc=cfLpc then fxRes:=Laplace(fxRes,iRds) else //Laplace-Filter
  if sExc=cfNrm then fxRes:=Texture(fxRes,sExc) else //normalisierte Textur
  if sExc=cfTxr then fxRes:=Texture(fxRes,sExc) else //Standard Textur
  if sExc=cfIdm then fxRes:=Texture(fxRes,sExc) else //inverse Textur
    Tools.ErrorOut(2,cCmd+sExc);

  if iRds>1 then
    if (sExc=cfLpc) or (sExc=cfTxr) or (sExc=cfIdm) or (sExc=cfNrm)
    then fxRes:=LowPass(fxRes,iRds); //größeren Kernel emulieren

  if sTrg='' then sTrg:=eeHme+sExc; //Standard oder gewählter Name

  Image.WriteBand(fxRes,0,sTrg); //neue Datei aus Ergebnis
  Header.WriteScalar(rHdr,sTrg); //Header für einen Kanal
  Tools.HintOut(true,'Filter.KernelExecute: '+ExtractFileName(sExc));
end;

{ hLN erzeugt einen Kanal-Namen aus dem Dateinamen des Kanals. Der Namen hat
  das Format: SSbBB_YYMMDD mit S=sensor B=Band Y=Year M=Month D=Day. Die Kanäle
  müssen mit "ImportT" erzeugt worden sein. }
{ Beispiel: LT05_L2SP_195024_19890518_20200916_02_T1_SR_B1.TIF }
{ Beispiel: LT05_195024_19890806_B3 }

function tHeader.LayerName(sImg:string):string; //Datei-Name: Kanal-Name
var
  sTmp:string=''; //Zwischenlager
begin
  sImg:=ExtractFileName(sImg);
  if LeftStr(sImg,4)='LC09' then Result:='L9b' else //Landsat-9,
  if LeftStr(sImg,4)='LC08' then Result:='L8b' else //Landsat-8,
  if LeftStr(sImg,4)='LT05' then Result:='L5b' else //Landsat-5,
  if LeftStr(sImg,4)='LT04' then Result:='L4b';     //Landsat-4

  sTmp:=ExtractWord(4,sImg,['_']); //letzter Abschnitt = Kanal-ID
  Result+='0'+sTmp[2]; //Kanal zweistellig
end;

{ iWW speichert den Kanal "ixMsk" als 16-Bit integer im IDL-Format. }

procedure tImage.WriteWord(
  ixMsk:tn2Wrd; //Maske 16 Bit
  sRes:string); //Ergebnis
var
  hRes: integer=-1; //Filehandle "Bild"
  iSze: integer; //Byte pro Bildzeile
  Y: integer;
begin
  try
    hRes:=Tools.NewFile(0,0,sRes); //Bilddaten
    iSze:=length(ixMsk[0])*SizeOf(word); //Byte pro Bildzeile
    for Y:=0 to high(ixMsk) do
      FileWrite(hRes,ixMsk[Y,0],iSze); //Zeile schreiben
  finally
    if hRes>=0 then FileClose(hRes);
  end;
end;

{ hMQ gibt einen Wert für eine Liste aus Quality-Werten im Header zurück. Die
  Quality-Werte können aus Listen bestehen, wenn verschiedene Bilder vereinigt
  wurden. }

function tHeader._MeanQuality_(sXpq:string):single;
var
  fQpx:single=0; //Summe Qualitäts-Indices → Mittelwert
  iCnt:integer=0; //Anzahl Qualitäts-Indices
  I:integer;
begin
  iCnt:=WordCount(sXpq,[',']);
  if iCnt>1 then
  begin
    for I:=1 to iCnt do
      fQpx+=StrToFloat(ExtractWord(I,sXpq,[',']));
    Result:=fQpx/iCnt;
  end
  else if iCnt>0 then
    Result:=StrToFloat(sXpq)
  else Result:=0;
end;

{ hAL ändert im Header "sImg" den Eintrag neben "sCde" zu "sNew". hSL sucht
  nach dem Code, ersetzt die gefundene Zeile und speichert den übrigen Header
  ohne Änderungen. hSL ergänzt eine neue Zeile im Header, wenn der Code nicht
  im Original steht.
  ==> KLAMMERN UND UMBRÜCHE MÜSSEN ALS WERT MIT ÜBERGEBEN WERDEN }

procedure tHeader.AddLine(
  sCde:string; //Bezeichner
  sNew:string; //neuer Wert
  sImg:string); //Bildname
const
  cCde = 'hRL: Item not found: ';
  cImg = 'hRL: File not found: ';
var
  bAdd:boolean=False; //Code gefunden und Wert vertauscht
  slHdr: tStringList=nil; //ENVI-Header Zeilen
  I: integer;
begin
  if not FileExists(sImg) then Tools.ErrorOut(2,cImg+sImg);
  try
    slHdr:=tStringList.Create;
    slHdr.LoadFromFile(ChangeFileExt(sImg,cfHdr)); //Header-Vorbild
    for I:=1 to pred(slHdr.Count) do
      if LeftStr(slHdr[I],length(sCde))=sCde then
      begin
        slHdr[I]:=sCde+' = '+sNew; //Eintrag ändern
        bAdd:=True; //
        break; //Ziel gefunden
      end;
    if not bAdd then
      slHdr.Add(sCde+' = '+sNew); //Zeile ergänzen
    slHdr.SaveToFile(ChangeFileExt(sImg,cfHdr)); //Header-Vorbild
  finally
    slHdr.Free;
  end;
end;

{ iAM überträgt NoData-Pixel aus allen Kanälen im Bils auf alle anderen Kanäle.
  iAM sammelt zunächst alle NoData-Marken in einer Maske und überträgt sie dann
  auf alle Kanäle. Nach iAM muss nur noch der erste Kanal geprüft werden. }

procedure tImage.AlphaMask(sImg:string); //Bilddaten
const
  cFex = 'iAM: Image not found: ';
var
  fxImg:tn3Sgl=nil; //Vorbild, alle Kanäle
  pMsk: ^tn2Sgl; //Zeiger auf erste Kanal
  rHdr:trHdr; //Metadaten
  B,X,Y:integer;
begin
  if not FileExists(sImg) then Tools.ErrorOut(2,cFex+sImg);
  Header.Read(rHdr,sImg);
  fxImg:=Image.Read(rHdr,sImg);
  pMsk:=@fxImg[0]; //zeige auf ersten Kanal

  for B:=1 to pred(rHdr.Stk) do
    for Y:=0 to pred(rHdr.Lin) do
      for X:=0 to pred(rHdr.Scn) do
        if isNan(fxImg[B,Y,X]) then
          pMsk^[Y,X]:=NaN;

  for Y:=0 to pred(rHdr.Lin) do
    for X:=0 to pred(rHdr.Scn) do
      if isNan(pMsk^[Y,X]) then
        for B:=1 to pred(rHdr.Stk) do
          fxImg[B,Y,X]:=NaN;

  WriteMulti(fxImg,sImg); //Vorbild überschreiben
  Tools.HintOut(true,'Image.AlphaMask: '+cfAlp);
end;

{ fCb maskiert und scaliert den Kanal "sBnd". fCb setzt alle Bildpixel mit dem
  Wert von "fNod" auf NoData. fCb scaliert alle gültigen Pixel mit dem Faktor
  "faFct" und anschließend mit dem Offset "fOfs". fCb überschreibt die
  übernommenen Bilddaten. fCb liest und schreibt im IDL-Format.
==> "Calibrate" wurde für Reflektanz/Radiation implemeniert. "Normalize" setzt
    den 1% Percentil auf Null und den 99% Percentil auf Eins }

procedure tFilter.Calibrate(
  faFct:tnSgl; //Faktor für einzelne Kanäle ODER "nil"
  fOfs:single; //Offset für alle Kanäle oder "0"
  fNod:single; //NoData-Äquivalent in den Bilddaten
  sImg:string); //Vorbild im Single-Format
var
  fxBnd:tn2Sgl=nil; //Kanal aus Vorbild
  rHdr:trHdr; //Metadaten
  B,X,Y:integer;
begin
  if (faFct=nil) and (fOfs=0) then exit; //keine Veränderung
  Header.Read(rHdr,sImg);
  //rHdr.Stk=length(faFct)?
  for B:=0 to pred(rHdr.Stk) do
  begin
    fxBnd:=Image.ReadBand(B,rHdr,sImg);
    for Y:=0 to pred(rHdr.Lin) do
      for X:=0 to pred(rHdr.Scn) do
        if not isNan(fxBnd[Y,X]) then //nur definierte Pixel
          if not (fxBnd[Y,X]=fNod) //nich NoData-Äquivalent
            then fxBnd[Y,X]:=fxBnd[Y,X]*faFct[B]+fOfs //Werte scaTools.ErrorOut(2,cFex+slImg[I]); lieren
            else fxBnd[Y,X]:=NaN; //auf NoData setzen
    Image.WriteBand(fxBnd,B,sImg);
  end;
end;

{ iSB stapelt alle Kanäle aus "slBnd", maskiert sie und speichert das Ergebnis
  als "stack". Mit einer gültigen Maske "sMsk" setzt iSB alle Pixel auf NoData
  die in "sMsk" den Wert Null haben. iSB überträgt die Namen aus "slBnd" in den
  ENVI-Header. iSB liest und schreibt 32-Bit float-Werte im IDL-Format. }
{ ==> iSB ERWARTET, DASS ALLE KÄNÄLE ZUM GLEICHEN BILD GEHÖREN }

function tImage.StackBands( // "StackBands" <==> "StackImages"
  slBnd:tStringList; //Dateinamen als Liste
  sMsk:string): //Maske [0..1] für definierte Werte ODER leer
  string; //Name des Stacks
var
  ixBin:tn2Wrd=nil; //Masken-Kanal
  fxBnd:tn2Sgl=nil; //Bildkanal
  rHdr:trHdr; //Metadaten
  sBnd:string=''; //Kanal-Namen als Strings mit Zeilentrennern
  I,X,Y:integer;
begin
  Result:=eeHme+cfStk; //Ergebnis-Name
  if length(sMsk)>0 then
  begin
    Header.Read(rHdr,sMsk); //QA-Maske
    ixBin:=Image.ReadWord(rHdr,sMsk); //QA-Layer
  end;
  Header.Read(rHdr,slBnd[0]); //sollte immer gleich sein
  for I:=0 to pred(slBnd.Count) do
  begin
    fxBnd:=ReadBand(0,rHdr,slBnd[I]); //einen Kanal lesen
    if ixBin<>nil then
      for Y:=0 to pred(rHdr.Lin) do
        for X:=0 to pred(rHdr.Scn) do
          if ixBin[Y,X]=0 then
            fxBnd[Y,X]:=NaN; //ausmaskierte Werte löschen
    WriteBand(fxBnd,I,Result); //Kanäle stapeln
    sBnd+=Header.LayerName(slBnd[I])+#10; //mit Zeilentrenner
  end;
  Header.WriteMulti(rHdr,sBnd,Result);
end;

{ tIWB schreibt den layer "fxImg" als Kanal "iBnd" in das Bild "sImg". Wenn
  "iBnd" auf einen bestehenden Kanal zeigt, wird er überschrieben. "iBnd"=0
  erzeugt ein neuen Bild. }

procedure tImage.WriteBand(
  fxImg:tn2Sgl; //Bilddaten
  iBnd:integer; //Kanal-ID ab Null
  sImg:string); //Dateiname
const
  cNil = 'iWB: Image data not defined!';
  cNme = 'iWB: Image filename missing!';
var
  hImg: integer=-1; //Filehandle "Bild"
  iBlk: int64; //Byte pro Zeile
  Y: integer;
begin
  //Datei muss zu fxImg passen
  if fxImg=nil then Tools.ErrorOut(2,cNil);
  if sImg='' then Tools.ErrorOut(2,cNme);
  try
    if FileExists(sImg)
      then hImg:=Tools.CheckOpen(sImg,fmOpenReadWrite) //öffnen
      else hImg:=Tools.NewFile(0,0,sImg); //neue Datei
    iBlk:=length(fxImg[0])*SizeOf(single); //Byte pro Bildzeile
    FileSeek(hImg,iBlk*length(fxImg)*iBnd,0); //Offset in Byte
    for Y:=0 to high(fxImg) do
      FileWrite(hImg,fxImg[Y,0],iBlk); //Zeile schreiben
  finally
    if hImg>=0 then FileClose(hImg);
  end;
end;

{ hIt bestimmt die Bonding-Box des Bilds "rNew" innerhalb des Frames "rHdr" in
  Pixelkoordinaten sowie den Versatz des Koordinaten-Ursprungs (links oben) in
  Pixel-Koordinaten.
    Für die Position des Bilds "rNew" bestimmt hIt zunächst die gemeinsam
  abgedeckte Fläche (fLft..fBtm) in Welt-Koordinaten und transformiert sie in
  Pixel-Koordinten von "rHdr". Analog gibt hIt mit "iHrz" und "iVrt" den Veratz
  des Koordinaten-Ursprungs (links-oben) von "rNew" zu "rHdr" in Pixel-
  Koordinaten zurück. hIt erzeugt Step-Fehler wenn die Ausschnitte nicht
  überlappen. }

function tHeader.Intersect(
  var iHrz,iVrt:integer; //Versatz von "rNew" zu "rHdr" in Pixel-Koordinaten
  const rHdr,rNew:trHdr; //Header: gemeinsamer Rahmen, aktuellen Bild
  sImg:string): //Name des aktuellen Bilds (nur für Fehlermeldung)
  trBox; //gemeinsamer Ausschnitt innnerhalb "rHdr" in Pixelkordinaten
const
  cOvl = 'iCp: no overlap between frame and image: ';
  cSze = 'hIt: coordinate transformation error: ';
var
  fLft,fTop,fRgt,fBtm:double; //gemeinsame Fläche (Koordinaten)
begin
  fLft:=max(rHdr.Lon,rNew.Lon); //gemeinsame Fläche innerhalb des ROI
  fRgt:=min(rHdr.Lon+rHdr.Scn*rHdr.Pix,rNew.Lon+rNew.Scn*rNew.Pix);
  fTop:=min(rHdr.Lat,rNew.Lat);
  fBtm:=max(rHdr.Lat-rHdr.Lin*rHdr.Pix,rNew.Lat-rNew.Lin*rNew.Pix);
  if (fLft>=fRgt) or (fBtm>=fTop) then
    Tools.ErrorOut(3,cOvl+sImg); //Siherheit

  Result.Lft:=round((fLft-rHdr.Lon)/rHdr.Pix); //erster Pixel links (Rahmen)
  Result.Top:=round((rHdr.Lat-fTop)/rHdr.Pix); //erster Pixel oben (Rahmen)
  Result.Rgt:=Result.Lft+trunc((fRgt-fLft)/rHdr.Pix); //letzer Pixel horizontal
  Result.Btm:=Result.Top+trunc((fTop-fBtm)/rHdr.Pix); //letzer Pixel vertikal

  iHrz:=round((rNew.Lon-rHdr.Lon)/rHdr.Pix);
  iVrt:=round((rHdr.Lat-rNew.Lat)/rHdr.Pix);

  if (Result.Top-iVrt<0) or (Result.Btm-iVrt>rNew.Lin)
  or (Result.Lft-iHrz<0) or (Result.Rgt-iHrz>rNew.Scn) then
    Tools.ErrorOut(3,cSze+sImg); //Siherheit
end;

{ iSN setzt einen definierten Wert (fNan) im Bild "sImg" auf NoData und
  speichert das Ergebnis unter em bestehenden Namen. }

procedure tFilter.SetNoData(
  fNan:single; //NoData Wert im Vorbild
  sImg:string); //Bild-Name
var
  fxImg:tn3Sgl=nil; //Bilddaten
  rHdr:trHdr; //Metadaten
  B,X,Y:integer;
begin
  Header.Read(rHdr,sImg); //Metadaten
  fxImg:=Image.Read(rHdr,sImg); //Bilddaten
  for B:=0 to pred(rHdr.Stk) do
    for Y:=0 to pred(rHdr.Lin) do
      for X:=0 to pred(rHdr.Scn) do
      begin
        if isNan(fxImg[B,Y,X]) then continue;
        if fxImg[B,Y,X]=fNan then
          fxImg[B,Y,X]:=NaN;
      end;
  Image.WriteMulti(fxImg,sImg); //Werte zurückschreiben
  Header.Write(rHdr,'NoData to NaN',sImg); //Metadaten
end;

procedure tHeader.CopyRecord(
  const rHdr:trHdr; //Vorlage
  var rRdh:trHdr); //Ergebnis
begin
  rRdh.Cnt:=rHdr.Cnt;
  rRdh.Fmt:=rHdr.Fmt;
  rRdh.Lat:=rHdr.Lat;
  rRdh.Lon:=rHdr.Lon;
  rRdh.Pix:=rHdr.Pix;
  rRdh.Prd:=rHdr.Prd;
  rRdh.Scn:=rHdr.Scn;
  rRdh.Lin:=rHdr.Lin;
  rRdh.Stk:=rHdr.Stk;
  rRdh.Cys:=rHdr.Cys;
  rRdh.Fld:=rHdr.Fld;
  rRdh.Map:=rHdr.Map;
  rRdh.Pif:=rHdr.Pif;
  rRdh.Pal:=nil;
  rRdh.aBnd:=rHdr.aBnd;
end;

//map info = {UTM, 1, 1, 570015, 5812905, 30, 30, 32, North,WGS-84}

{ hOg ersetzt Ursprung des Koordinatensystems in "map info" string }

procedure tHeader.Origin(
  fLat,fLon:double; //neue Kordinaten für linke obere Ecke
  var rHdr:trHdr); //ENVI Header
var
  sMap:string; //"map info"-Kopie
  I:integer;
begin
  sMap:=copy(rHdr.Map,1,$FFF); //physikalische Kopie
  rHdr.Map:=ExtractWord(1,sMap,[',']);
  for I:=2 to 3 do
    rHdr.Map+=', '+ExtractWord(I,sMap,[',']);
  rHdr.Map+=', '+FloatToStr(fLon)+', '+FloatToStr(fLat);
  for I:=6 to 10 do
    rHdr.Map+=', '+ExtractWord(I,sMap,[',']);
  rHdr.Lat:=fLat;
  rHdr.Lon:=fLon;
end;

{ iSI bildet einen Stapel im ENVI-Format aus Bildern in "slImg". CRS, Kanäle,
  Ausschnitt und Pixel MÜSSEN gleich sein und werden nur rudimentär überprüft.
  iSI kann auch sehr große Kanäle kopieren. }

procedure tImage.StackImages(
  slImg:tStringList; //Bilder-Liste
  sTrg:string); //Ergebnis-Name
const
  cOvl='iSI: Image size not fitting: ';
var
  fxBnd:tn2Sgl=nil; //aktueller Kanal
  iLin,iScn:integer; //Bildgröße, nur Kontrolle
  iStk:integer=0; //Kanal-Zähler
  rHdr:trHdr; //Metadaten
  sBnd:string=''; //Kanal-Namen als Liste
  B,I:integer;
begin
  if slImg.Count>1 then
  begin
    for I:=0 to pred(slImg.Count) do
    begin
      if (I>0) and ((rHdr.Lin<>iLin) or (rHdr.Scn<>iScn)) then
        Tools.ErrorOut(3,cOvl+slImg[I]); //Siherheit
      Header.Read(rHdr,slImg[I]); //Header
      iLin:=rHdr.Lin; iScn:=rHdr.Scn; //für nächses Bild
      for B:=0 to pred(rHdr.Stk) do
      begin
        fxBnd:=ReadBand(B,rHdr,slImg[I]);
        WriteBand(fxBnd,iStk,sTrg); //Kanal ergänzen
        inc(iStk) //Kanäle zählen
      end;
      sBnd+=rHdr.aBnd; //Kanal-Namen,
      if sBnd[length(sBnd)]<>#10 then sBnd+=#10; //Zeilentrenner
    end;
    Header.WriteMulti(rHdr,sBnd,sTrg)
  end
  else Tools.EnviRename(slImg[0],sTrg);
  Tools.HintOut(true,'Image.Stack: '+ExtractFileName(sTrg));
end;

{ fNz normalisiert alle Kanäle aus "sImg" auf den Bereich [0..1] und speichert
  das Ergebnis als "equal". nFz scaliert die Bildwerte am 1% Percentil auf Null
  und die Werte am 99% Percentil auf Eins. fNZ liest und schreibt im IDL-
  Format. }

function tFilter.Normalize(
  iSmp:integer; //Anzahl Stichproben
  sImg:string): //Bild-Name
  string; //neuer Bildname
var
  faVal:tnSgl=nil; //Werte-Stichproben
  fFct:single; //Faktor für Scalierung
  fHig,fLow:single; //Percentil-Werte
  fSum:double; //Summe Stichproben
  fxBnd:tn2Sgl=nil; //Kanal aus "sImg"
  iHit:integer; //gültige Stichproben
  rHdr:trHdr; //Metadaten
  B,X,Y:integer;
begin
  Header.Read(rHdr,sImg); //Metadaten
  Result:=eeHme+ChangeFileExt(cfEql,'');
  for B:=0 to pred(rHdr.Stk) do //Kanäle einzeln
  begin
    faVal:=Table._BandHist(fSum,iHit,B,iSmp,sImg); //"fSum", "iHit" wird gesetzt
    fLow:=faVal[round(iHit*0.01)]; //1% Percebntil
    fHig:=faVal[round(iHit*0.99)]; //99% Percebntil
    fFct:=1.0/(fHig-fLow); //Steigung
    fxBnd:=Image.ReadBand(B,rHdr,sImg); //Kanäle einzeln lesen
    for Y:=0 to pred(rHdr.Lin) do
      for X:=0 to pred(rHdr.Scn) do
        if not IsNan(fxBnd[Y,X]) then //NoData ignorieren
          fxBnd[Y,X]:=(fxBnd[Y,X]-fLow)*fFct; //scalieren
    Image.WriteBand(fxBnd,B,Result); //Bilddaten
  end;
  Header.WriteMulti(rHdr,rHdr.aBnd,Result) //Metadaten
end;

end.

{==============================================================================}

{ tFEO bestimmt Rao's Q-Index (Distanzabhängige Divergenz) auf der Basis vom
  "/.imalys/mapping" und speichert das Ergebnis als "/.imalys/entropy". Das
  Ergebnis ist das Produkt aus Klassen-Häufigkeit im Kernel und den spektralen
  Distanzen der Klassen. }

procedure tFilter.xRaosDiv(
  iRds:integer; //Kernel-Radius
  sImg:string); //Vorbild (Klassifikation)
const
  cFex = 'fEy: Image not found: ';
  cThm = 'fEy: Selected image must be a classification: ';
var
  fxRes: tn2Sgl=nil; //Entropie-Werte
  ixThm: tn2Byt=nil; //Klassein-Layer
  rHdr: trHdr; //gemeinsame Metadaten
begin
  if not FileExists(sImg) then Tools.ErrorOut(2,cFex+sImg);
  //iRds>0!
  Header.Read(rHdr,sImg); //Metadaten Clusterung
  if rHdr.Fmt<>1 then Tools.ErrorOut(2,cThm+sImg);
  ixThm:=Image.ReadThema(rHdr,sImg); //Klassen, Karte
  fxRes:=RaosDiv(rHdr.Cnt,iRds,ixThm); //Entropie aus Clusterung
  Image.WriteBand(fxRes,0,eeHme+cfEtp); //Bilddaten
  Header.WriteScalar(rHdr,eeHme+cfEtp); //Metadaten
  Tools.HintOut(true,'Filter.Entropy: '+cfAlp);
end;

{ tFC gibt die Summe aller Differenzen zwischen den Werten der linken und der
  oberen Kernel-Kante mit allen anderen Pixeln zurück. tFC zählt die Vergleiche
  und gibt ihre Anzahl als "iCnt" zurück. Wird tFC iteriert, wird jede Pixel-
  Kombination im Kernel genau einmal erfasst. Für die Iteration muss die linke
  obere Ecke diagonal nach rechts unten verschoben werden. }

function tFilter.R_aosQ(
  fxBnd: tn2Sgl; //Dichte (Hauptkomponenten)
  var iCnt:integer; //Anzahl Vergleiche
  iLft,iTop,iRgt,iBtm: integer): //Kernel-Grenzen (NICHT ÜBERPRÜFT)
  single; //Summe Distanzen
var
  V,W,X,Y: integer;
begin
  Result:=0;
  //iLft>=0; iTop>=0;
  //iRgt<length(fxImg[0])
  //iBtm<length(fxImg)

  for W:=iTop to iBtm do //linke Vertikale
    if not isNan(fxBnd[W,iLft]) then
      for Y:=iTop to iBtm do
        for X:=succ(iLft) to iRgt do
          if not isNan(fxBnd[Y,X]) then //NaNata addiert nichts
          begin
            Result+=abs(fxBnd[Y,X]-fxBnd[W,iLft]); //mit linker Kante
            inc(iCnt)
          end;

  for V:=iLft to iRgt do //obere Horizontale
    if not isNan(fxBnd[iTop,V]) then
      for Y:=succ(iTop) to iBtm do
        for X:=iLft to iRgt do
          if not isNan(fxBnd[Y,X]) then
          begin
            Result+=abs(fxBnd[Y,X]-fxBnd[iTop,V]); //mit oberer Kante
            inc(iCnt)
          end;
end;

