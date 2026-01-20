unit thema;

{ THEMA sammelt Routinen zur Clusterung und Klassifikation von Bilddaten. Dabei
  clustert "Model" Bildmerkmale anhand einzelner Pixel und "Fabric<" verwendet
  aus der Zellbildung (index) abgeleitete Teilflächen und ihre räumliche
  Verknüpfung. In jedem Fall nimmt "Thema" zuerst Stichproben ("Samples") aus
  den Bilddaten, erzeugt ein "Model" mit verschiedenen Clustern und
  klassifiziert damit das Bild.

  LIMITS: bestimmt Schwellen und Masken(Grenzen) aus Werten im Bild
  MODEL:  clustert multidimensionale Bilddaten
  REDUCE: extrahiert Ergebnisse aus n-dimensionalen Bilddaten <=> Filter

  BEGRIFFE MIT SPEZIFISCHER ANWENDUNG:
  Band:    Bildkanal
  Dict:    Zuweisung von Begriffen oder Werten: Bezeichner = Wert
  Feature: Bild-Merkmal (Dichte) in einem →Modell oder einer →Sample-Liste
  Key:     Zonen-Merkmal = Häufigkeit von Kontakten in einem →Model oder einer
           →Sample-Liste
  Layer:   Bildkanal oder Bild-Merkmal
  Model:   Ergebnis einer Clusterung (Selbstorganisation) mit den typischen
           Merkmalen (Werten) der Klassen als Matrix[Klasse,Merkmal].
           Model[?,0] speichert das Quadrat des Suchradius (für SOM-Neurone),
           alle anderen Werte sind Bildmerkmale, auch NoData.
  Samples: Stichproben mit allen Merkmalen eines Pixels oder einer →Zone als
           Matrix[Probe,Merkmal]. Die erste Stelle (Sample[?,0]) bleibt frei
           für den Suchradius (SOM-Neurone).
  Stack:   Stapel aus Kanälen mit Bilddaten, gleiche Geometrie
  Sync:    Imalys kann Merkmale von mehr als einem Bild gemeinsam bearbeiten.
           Dazu müssen die Bilder einen Teil des Namens gemeinsam haben. Lage
           und Geometrie der Bilder sind frei.
  Wave:    zeitlich konstante Periode in der Veränderung von Werten
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, StrUtils, DateUtils, format;

type
  tFig2Int = function(fVal,fFct:single):string;

  tLimits = class(tObject) //checked 250913
    private
      function _MaskLimit_(fMin:single; fxBnd:tn2Sgl):tn2Byt;
      procedure MergeZones(ixTpl:tn2Int; iaFix:tnInt);
      function RecodeIndex(iaFix:tnInt; ixIdx:tn2Int):integer;
      function SieveZones(iaFix:tnInt; iMin:integer; ixTpl:tn2Int):integer;
    public
      procedure xSieveZones(iMin:integer);
  end;

  tModel = class(tObject) //checked 251223
    private
      function BasinCount(iaLnk:tnInt; var iHig:integer):tnInt;
      function BasinIndex(iaCtm:tnInt; ixIdx:tn2Int):tn2Int;
      procedure BorderDrain(faMin:tnSgl; fxElv:tn2Sgl; ixCtm,ixRes:tn2Int);
      procedure _CheckLoops_(iaLnk:tnInt);
      function ClassifyPixels(fxMdl:tn2Sgl; sImg:string):tn2Byt;
      function ClassifyZones(fxAtr,fxMdl:tn2Sgl):tnInt;
      function DrainLinks(ixDrn,ixIdx:tn2Int):tnInt;
      function DrainPoints(bRls:boolean; fxElv:tn2Sgl; iaLnk:tnInt; iCnt:integer; ixCtm:tn2Int):tn2Int;
      procedure HorzDrain(faMin:tnSgl; fxElv:tn2Sgl; iaLnk:tnInt; ixCtm,ixRes:tn2Int);
      function MergeLinks(iaLnk:tnInt; ixDrn,ixIdx:tn2Int):integer;
      procedure SampleAdd(fxMdl:tn2Sgl; iPst:integer);
      procedure SampleFit(fxMdl:tn2Sgl);
      procedure SampleMerge(fxMdl:tn2Sgl);
      procedure SelectPixel(fxMdl:tn2Sgl; fxImg:tn3Sgl; var rHdr:trHdr);
      procedure SelectZone(fxAtr,fxMdl:tn2Sgl; ixIdx:tn2Int; var rHdr:trHdr);
      procedure VertDrain(faMin:tnSgl; fxElv:tn2Sgl; iaLnk:tnInt; ixCtm,ixRes:tn2Int);
    public
      function _FeatureDist_:tn2Sgl;
      procedure xClassValues(iRed,iGrn,iBlu:integer);
      procedure xFabricMap(iFbr,iGen,iSmp:integer);
      procedure xPixelMap(iMap,iSmp:integer; sImg:string);
      procedure xRunOff(bRls:boolean; sElv:string);
      procedure xZonesMap(iMap,iSmp:integer; sAtr,sRes:string);
  end;

  tReduce = class(tObject) //checked 241116
    private
      function BandCalc(fxImg:tn3Sgl; sPrc:string):tn2Sgl;
      function BestOf(fxImg:tn3Sgl; sCvr:string):tn2Sgl;
      function _CoVariance_(fxImg:tn3Sgl; iaTms:tnInt):tn2Sgl;
      function Execute(fxImg:tn3Sgl; sArt,sCmd:string; var rHdr:trHdr):tn2Sgl;
      function GlobalSum(fxImg:tn3Sgl):tn2Sgl;
      function ImageDate(sImg:string):integer;
      function _LeafArea(fxImg:tn3Sgl; iNir,iRed:integer):tn2Sgl;
      function _LeafAreaIndex_(fxImg:tn3Sgl; iNir,iRed:integer):tn2Sgl;
      function Limits(bMax:boolean; fxStk:tn3Sgl):tn2Sgl;
      function MeanValue(fxImg:tn3Sgl):tn2Sgl;
      function Median(fxImg:tn3Sgl):tn2Sgl;
      function NewBand(fxImg:tn3Sgl; sVal:string):tn2Sgl;
      function Overlay(fxImg:tn3Sgl):tn2Sgl;
      function Regression(fxImg:tn3Sgl):tn2Sgl;
      procedure ScaleValues(fxLft,fxRgt:tn2Sgl; iTyp:integer);
      function Variance(fxImg:tn3Sgl):tn2Sgl;
      function Vegetation(fxImg:tn3Sgl; iNir,iRed,iTyp:integer):tn2Sgl;
    public
      function Brightness(fxImg:tn3Sgl):tn2Sgl;
      procedure GetBands(var iHig,iLow:integer; iStk:integer; sArt:string);
      procedure IndexSort(iaIdx:tnInt; faVal:tnSgl);
      procedure xComposit(sImg,sTrg:string);
      procedure xHistory(sArt,sCmd,sImg,sTrg:string);
      procedure xQualityImage(sImg,sTrg:string);
      procedure xReduce(sArt,sCmd,sImg,sTrg:string);
      procedure xSplice(sArt,sCmd,sImg,sTrg:string);
  end;

var
  Limits: tLimits;
  Model: tModel;
  Reduce: tReduce;

implementation

uses
  index, mutual, raster, vector;

const
  ccOvr = 3; //Oversampling bei Klassifikation
  ccRtn = 30; //Klassen immer um 1/cRtn ändern
  //ccRtn = 10; //Klassen immer um 1/cRtn ändern

  // für tStringList.CustomSort, Datum "YYYYMMDD" am Ende des Namens

{ mCL prüft, ob Bezüge in "iaLnk" eine Schleife bilden. Nur eine direkte
  Rückkoppung ist definiert! }

procedure tModel._CheckLoops_(iaLnk:tnInt);
var
  iaMsk:tnByt=nil; //Maske: 0=leer, 1=benutzt, $FF=neue Spur
  iIdx:integer=0; //aktuelle Zone
  Z:integer;
begin
  iaMsk:=Tools.InitByte(length(iaLnk));
  for Z:=1 to high(iaLnk) do
    if iaLnk[Z]<>Z then //nur verknüpfte Zonen
    begin
      iIdx:=Z;
      repeat
        iaMsk[iIdx]:=$FF;
        iIdx:=iaLnk[iIdx]
      until iaMsk[iIdx]<>0;

      if (iaMsk[iIdx]=$FF)
      and (iaLnk[iIdx]<>iIdx) then
        iaMsk[iIdx]:=$FF; //NUR KONTROLLE

      iIdx:=Z;
      repeat
        iaMsk[iIdx]:=1;
        iIdx:=iaLnk[iIdx]
      until iaMsk[iIdx]=1;
    end;
end;

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

// für tStringList.CustomSort, Datum "YYYY" am Ende des Namens

function EndYear(sl:tStringList; i1,i2:integer):integer;
begin
  if RightStr(ChangeFileExt(sl[i1],''),4) <
     RightStr(ChangeFileExt(sl[i2],''),4) then
     Result:=-1 else
  if RightStr(ChangeFileExt(sl[i1],''),4) >
     RightStr(ChangeFileExt(sl[i2],''),4) then
     Result:=1 else
  Result:=0
end;

function ImgDat(p1,p2:Pointer):integer;
begin
  if tprTms(p1)^.Dat<tprTms(p2)^.Dat then Result:=-1 else //nach vorne
  if tprTms(p1)^.Dat>tprTms(p2)^.Dat then Result:=1 else Result:=0;
end;

function Float2Number(fVal,fFct:single):string;
begin
  Result:=FloatToStrF(fVal*fFct,ffFixed,7,0)
end;

function Int2Number(fVal,fFct:single):string;
begin
  Result:=FloatToStrF(integer(fVal)*fFct,ffFixed,7,0)
end;

function Distance(p1,p2:Pointer):integer;
begin
  if tprSpc(p1)^.Val<tprSpc(p2)^.Val then Result:=-1 else //nach vorne
  if tprSpc(p1)^.Val>tprSpc(p2)^.Val then Result:=1 else Result:=0;
end;

function tReduce._CoVariance_(
  fxImg:tn3Sgl; //Bilddaten-Stack
  iaTms:tnInt): //Zeitstempel pro Bild
  tn2Sgl; //CoVarianz
{ rVc }
{ CoVarianz = (∑xy - ∑x∑y/n)/(n-1) }
var
  fPrd:single=0; //Produkt beider Variablen
  fSum:single=0; //Summe der ersten Variablen
  fMus:single=0; //Summe der zweiten Variablen
  iCnt:integer; //Anzahl gültiger Kanäle (n)
  B,X,Y: integer;
begin
  Result:=Tools.Init2Single(length(fxImg[0]),length(fxImg[0,0]),dWord(NaN)); //Vorgabe = NoData
  for B:=0 to high(fxImg) do //alle Kanäle
    fMus+=iaTms[B]; //Summe aller Zeitstempel
  for Y:=0 to high(fxImg[0]) do
    for X:=0 to high(fxImg[0,0]) do
    begin
      fPrd:=0; fSum:=0; iCnt:=0;
      for B:=0 to high(fxImg) do //alle Kanäle
      begin
        if IsNan(fxImg[B,Y,X]) then continue;
        fPrd:=fxImg[B,Y,X]*iaTms[B];
        fSum+=fxImg[B,Y,X];
        inc(iCnt) //Anzahl gültige Schritte
      end;
      if iCnt>1
        then Result[Y,X]:=(fPrd-fSum*fMus/iCnt)/pred(iCnt)
        else Result[Y,X]:=NaN;
    end;
end;

{ tMFD bestimmt die (spektrale) Distanz zwischen allen Klassen-Kombinationen im
  aktuellen Modell "fxMdl" und gibt das Ergebnis als Matrix zurück. Nicht
  definierte Kombinationen sind auf Null gesetzt. }

function tModel._FeatureDist_:tn2Sgl; //Distanzen-Matrix
var
  fRes: single; //Zwischenergebnis
  fxMdl: tn2Sgl=nil; //aktuelles Modell
  I,N,M: integer;
begin
  Result:=nil;
  fxMdl:=Tools.BitRead(eeHme+cfMdl); //aktuelles Modell lesen
  Result:=Tools.Init2Single(length(fxMdl),length(fxMdl),0);
  for M:=2 to high(fxMdl) do
    for N:=1 to pred(M) do
    begin
      fRes:=0; //Vorgabe
      for I:=1 to high(fxMdl[0]) do //alle Merkmale
        fRes+=sqr(fxMdl[M,I]-fxMdl[N,I]); //Summe Quadrate
      Result[N,M]:=sqrt(fRes); //Hauptkomponente
      Result[M,N]:=Result[N,M]; //symmetrisch füllen
    end;
end;

{ rMV gibt den Mittelwert aller Kanäle aus "fxImg" zurück. rMV überprüft NoData
  in jedem Kanal. }

function tReduce.MeanValue(fxImg:tn3Sgl):tn2Sgl; //Vorbild: Mittelwert
var
  fRes:single=0; //Summe Werte in allen Kanälen
  iCnt:integer; //Anzahl gültiger Kanäle
  B,X,Y: integer;
begin
  Result:=Tools.Init2Single(length(fxImg[0]),length(fxImg[0,0]),dWord(NaN)); //Vorgabe = NoData
  for Y:=0 to high(fxImg[0]) do
    for X:=0 to high(fxImg[0,0]) do
    begin
      fRes:=0; iCnt:=0;
      for B:=0 to high(fxImg) do
      begin
        if IsNan(fxImg[B,Y,X]) then continue;
        fRes+=fxImg[B,Y,X]; //Summe
        inc(iCnt) //Anzahl Summanden
      end;
      if iCnt>0
        then Result[Y,X]:=fRes/iCnt //Mittelwert
        else Result[Y,X]:=NaN;
    end;
end;

{ lML erzeugt mit der Schwelle "fMin" eine Maske ([0,1]-Kanal) aus einem
  scalaren Bild. }

function tLimits._MaskLimit_(
  fMin:single; //Schwelle = kleinster zulässiger Wert
  fxBnd:tn2Sgl): //Vorbild
  tn2Byt; //Maske [0,1]
var
  X,Y:integer;
begin
  Result:=Tools.Init2Byte(length(fxBnd),length(fxBnd[0])); //Maske
  for Y:=0 to high(fxBnd) do
    for X:=0 to high(fxBnd[0]) do
      if not isNan(fxBnd[Y,X]) then
        Result[Y,X]:=byte(fxBnd[Y,X]>=fMin); //Schwelle anwenden
end;

function tReduce._LeafAreaIndex_(
  fxImg:tn3Sgl; //Vorbild
  iNir,iRed:integer): //Parameter
  tn2Sgl; //Vorbild: Vegetationsindex
{ rLA gibt eine Näherung für den Leaf Area Index als Kanal zurück. "iRed" und
  "iNir" müssen die Wellenländen von Rot und nahem Infrarot bezeichnen. Formel
  nach Yao et.al, 2017 }
const
  cDim = 'tFV: Vegetation index calculation needs two bands!';
var
  X,Y: integer;
begin
  if length(fxImg)<2 then Tools.ErrorOut(3,cDim);
  Result:=Tools.Init2Single(length(fxImg[0]),length(fxImg[0,0]),dWord(NaN)); //Vorgabe = NoData
  for Y:=0 to high(fxImg[0]) do
    for X:=0 to high(fxImg[0,0]) do
      if not IsNan(fxImg[0,Y,X]) then
        Result[Y,X]:=exp((fxImg[iNir,Y,X]-fxImg[iRed,Y,X])/(fxImg[iNir,Y,X]+
          fxImg[iRed,Y,X])*0.08); //LAI-Näherung
end;

{ rPc bestimmt die erste Hauptkomponente im Stack "fxImg" und gibt das Ergebnis
  als Kanal zurück. rPc prüft jeden Kanal auf NoData, so dass auch lückige
  Stacks verarbeitet werden können. }

function tReduce.Brightness(fxImg:tn3Sgl):tn2Sgl; //Vorbild: Erste Hauptkomponente
const
  cDim = 'tFP: A principal component needs more than one band';
var
  //fMin:single; //kleinster Wert in allen Kanälen
  fSed:double=0; //aktuelles Ergebnis
  iDef:integer=0; //Anzahl definierte Pixel
  B,X,Y:integer;
begin
  if (length(fxImg)<2) or (length(fxImg[0,0])<1) then
    Tools.ErrorOut(3,cDim);
  Result:=Tools.Init2Single(length(fxImg[0]),length(fxImg[0,0]),dWord(NaN)); //Vorgabe = NoData
  for Y:=0 to high(fxImg[0]) do
    for X:=0 to high(fxImg[0,0]) do
    begin
      fSed:=0; iDef:=0; //Vorgabe
      for B:=0 to high(fxImg) do //alle Kanäle
        if not IsNan(fxImg[B,Y,X]) then
        begin
          fSed+=sqr(fxImg[B,Y,X]);
          inc(iDef)
        end;
      if iDef>0 then Result[Y,X]:=sqrt(fSed) //Länge im n-Raum
    end;
end;

{ rOy überlagert alle definierten Pixel im Vorbild "fxImg" in einem Kanal. }

function tReduce.Overlay(fxImg:tn3Sgl):tn2Sgl; //Vorbilder: Mischung
var
  B,X,Y: integer;
begin
  Result:=Tools.Init2Single(length(fxImg[0]),length(fxImg[0,0]),dWord(NaN)); //Vorgabe = NoData
  for Y:=0 to high(fxImg[0]) do
    for X:=0 to high(fxImg[0,0]) do
      for B:=0 to high(fxImg) do
        if not isNan(fxImg[B,Y,X]) then
          Result[Y,X]:=fxImg[B,Y,X]; //definierte Werte überlagern
end;

{ rMn bildet den Median aus allen übergebenen Kanälen. Dazu kopiert rMn alle
  Werte eines Pixels nach "fxDev", sortiert "faDev" mit "QuickSort" und
  übernimmt den Wert in der Mitte der gültigen Einträge in "fxDev". rMn kopiert
  NoData Werte in den Bilddaten nicht nach "faDev" sondern reduziert mit "iDim"
  die gültigen Stellen in "faDev". }

function tReduce.Median(fxImg:tn3Sgl):tn2Sgl; //Vorbild: Median
const
  cDim = 'rMn: Less than three bands provided for median calculation';
var
  faDev:tnSgl=nil; //ein Pixel aus allen Kanälen
  iDim:integer; //Anzahl Kanäle
  B,X,Y: integer;
begin
  if length(fxImg)<3 then Tools.ErrorOut(3,cDim);
  Result:=Tools.Init2Single(length(fxImg[0]),length(fxImg[0,0]),dWord(NaN)); //Vorgabe = NoData
  SetLength(faDev,length(fxImg)); //alle Kanäle
  for Y:=0 to high(fxImg[0]) do
  begin
    for X:=0 to high(fxImg[0,0]) do
    begin
      iDim:=0;
      for B:=0 to high(faDev) do
        if not isNan(fxImg[B,Y,X]) then
        begin
          faDev[iDim]:=fxImg[B,Y,X]; //Pixel-Stack
          inc(iDim)
        end;
      if iDim>2 then
      begin
        Table.QuickSort(faDev,iDim); //gültige Stellen im Array
        if not odd(iDim) then
          Result[Y,X]:=(faDev[pred(iDim div 2)]+faDev[iDim div 2])/2
        else Result[Y,X]:=faDev[iDim div 2]
      end
      else if iDim=2 then Result[Y,X]:=(faDev[0]+faDev[1])/2
      else if iDim=1 then Result[Y,X]:=faDev[0];
    end;
  end;
end;

procedure tReduce.IndexSort(
  iaIdx:tnInt; //Indices der übergebenen Werte
  faVal:tnSgl); //Werte, unsortiert → sortiert
{ rIS sortiert das Array "faVal" absteigend. Dazu vertauscht rIS Werte und die
  mit dem Wert verknüpften Indices in "iaIdx" bis alle Vergliche passen. rIS
  verwendet zu Beginn große Abstände zwischen den Positionen im Array und
  reduziert sie schrittweise.
  ==> Werte statt Zeiger zu sortieren kann schneller sein, wenn die Werte klein
      und die Vorbereitung der Werte aufwändig ist.
  ==> vgl. Reduce.QuickSort }
var
  fVal:single; //Zwischenlager
  iFix:integer; //Erfolge
  iStp:integer; //Distanz zwischen Positionen
  iIdx:integer; //Zwischenlager
  B:integer;
begin
  //length(iaIdx)=length(faVal)?
  if length(iaIdx)<2 then exit; //nichts zu sortieren
  iStp:=round(length(iaIdx)/2); //erster Vergleich = halbmaximale Distanz
  repeat
    iFix:=0; //Vorgabe
    for B:=iStp to high(iaIdx) do
      if faVal[B]>faVal[B-iStp] then //große Werte nach vorne
      begin
        iIdx:=iaIdx[B-iStp];
        fVal:=faVal[B-iStp];
        iaIdx[B-iStp]:=iaIdx[B];
        faVal[B-iStp]:=faVal[B];
        iaIdx[B]:=iIdx;
        faVal[B]:=fVal;
      end
      else inc(iFix);
      if iStp>1 then iStp:=round(iStp/2) //Distanz halbieren
  until iFix=high(iaIdx); //alle Vergleiche richtig
end;

{ rV gibt einen von drei Vegetation Indices als Kanal zurück. "iRed" und "iNir"
  müssen die Wellenländen von Rot und nahem Infrarot bezeichnen. "iTyp" wählt
  den Index: 0=NIRV, 1=NDVI, 2=EVI }
{ Die Reflektanz kann bei kleinen Werten negativ werden. Die Formeln sind nur
  für positive Werte definiert. rVn setzt deshalb alle negativen Werte auf
  Null. Sind beide Kanäle negativ, setzt rVn das Ergebnis aus Nodata }

function tReduce.Vegetation(
  fxImg:tn3Sgl; //Vorbild
  iNir,iRed:integer; //Kanal-Indices (ab Null)
  iTyp:integer): //Index-ID
  tn2Sgl; //Vorbild: Vegetationsindex
const
  cDim = 'rVn: Vegetation index calculation needs two bands!';
  cTyp = 'rVn: Undefined ID for vegetation index"';
var
  pNir:^tn2Sgl=nil; //NIR-Kanal
  pRed:^tn2Sgl=nil; //Rot-Kanal
  X,Y: integer;
begin
  if length(fxImg)<2 then Tools.ErrorOut(3,cDim);
  if (iTyp<0) or (iTyp>2) then Tools.ErrorOut(3,cTyp);
  Result:=Tools.Init2Single(length(fxImg[0]),length(fxImg[0,0]),dWord(NaN)); //Vorgabe = NoData
  pNir:=@fxImg[iNir]; pRed:=@fxImg[iRed]; //Zeiger auf Rot und Nir-Layer
  for Y:=0 to high(fxImg[0]) do
    for X:=0 to high(fxImg[0,0]) do
    begin
      if IsNan(pRed^[Y,X]) or IsNan(pNir^[Y,X]) then continue; //NoData ignorieren
      if (pNir^[Y,X]<>0) or (pRed^[Y,X]<>0) then
        case iTyp of
          0: Result[Y,X]:=(pNir^[Y,X]-pRed^[Y,X])/
             (abs(pNir^[Y,X])+abs(pRed^[Y,X]))*abs(pNir^[Y,X]); //NIRv Vegetattionsindex
          1: Result[Y,X]:=(pNir^[Y,X]-pRed^[Y,X])/
             (abs(pNir^[Y,X])+abs(pRed^[Y,X])); //NDVI Vegetationsindex
          2: Result[Y,X]:=2.5*(pNir^[Y,X]-pRed^[Y,X])/
             (pNir^[Y,X]+2.4*pRed^[Y,X]+1.0); //EVI Vegetationsindex
        end
      else Result[Y,X]:=NaN; //nicht definiert
    end;
end;

{ rID übersetzt einen Datums-String [YYYYMMDD] am Ende einer Dateinamens in
  Sekunden Systemzeit. Kann das Datum nict interpretiert werden, gibt rDI einen
  negativen Wert zurück. }

function tReduce.ImageDate(sImg:string):integer;
var
  iInt:integer; //für Format-Test
begin
  sImg:=RightStr(ChangeFileExt(sImg,''),8); //nur Datum [YYYYMMDD]
  if TryStrToInt(sImg,iInt) then
    Result:=trunc(EncodeDate(
      StrToInt(copy(sImg,1,4)),
      StrToInt(copy(sImg,5,2)),
      StrToInt(copy(sImg,7,2)))) //Integer(tDateTime)
  else Result:=-1; //Fehler gefunden
end;

{ rVc bestimmt die Varianz aller Layer in "fxImg" für einzelne Pixel und gibt
  sie als Bild zurück. rVc überprüft jeden Eingangs-Kanal auf NoData, so dass
  auch lückige Bilder verarbeitet werden können. "Variance" unter "history"
  bestimmt die "Farbigkeit" der Kanäle = Gegenstück zu "Brightness" }

function tReduce.Variance(fxImg:tn3Sgl):tn2Sgl; //Vorbild: Varianz
var
  fSqr:double; //Summe Werte-Quadrate (x²) ← maximale Genauigkeit
  fSum:double; //Summe Werte (∑x) ← maximale Genauigkeit
  iCnt:integer; //Anzahl gültiger Kanäle (n)
  B,X,Y: integer;
begin
  //mindestens 3 Kanäle?
  Result:=Tools.Init2Single(length(fxImg[0]),length(fxImg[0,0]),dWord(NaN)); //Vorgabe = NoData
  for Y:=0 to high(fxImg[0]) do
    for X:=0 to high(fxImg[0,0]) do
    begin
      fSqr:=0; fSum:=0; iCnt:=0;
      for B:=0 to high(fxImg) do
      begin
        if IsNan(fxImg[B,Y,X]) then continue;
        fSum+=fxImg[B,Y,X];
        fSqr+=sqr(fxImg[B,Y,X]);
        inc(iCnt) //Anzahl gültige Schritte
      end;
      if iCnt>1
        then Result[Y,X]:=(fSqr-sqr(fSum)/iCnt)/pred(iCnt)
        else Result[Y,X]:=NaN;
    end;
end;

{ rNB kopiert den Kanal "sVal" aus "fxImg" wenn "sVal" mit "B" beginnt oder
  füllt einen neuen Kanal mit der Konstante in "sVal". rNB wurde spezielle für
  "BandCalc" implementiert. }

function tReduce.NewBand(
  fxImg:tn3Sgl; //Vorbild
  sVal:string): //Kanal-ID oder Zahl
  tn2Sgl; //Kanal oder Matrix mit Konstanten
const
  cBnd = 'fNB: Band identifier missing or undefined: ';
  cVal = 'fNB: Defined pixel density expected: ';
var
  fVal:single; //Konstante
  iBnd:integer; //aktueller Kanal [0..N]
begin
  Result:=nil; //Vorgabe
  if sVal[1]='\' then sVal[1]:='-'; //Vorzeichen!
  if sVal[1]='B' then //Kanal aus "fxImg" gewählt
  begin
    if (TryStrToInt(copy(sVal,2,$F),iBnd)) and
       (iBnd<=length(fxImg)) and (iBnd>0)
    //then Result:=fxImg[pred(iBnd)] //Kanal kopieren
    then Result:=tn2Sgl(Tools.CopyGrid(tn2Int(fxImg[pred(iBnd)]))) //Inhalt kopieren
    else Tools.ErrorOut(3,cBnd+sVal);
  end
  else if TryStrToFloat(sVal,fVal) then //konstanter Wert ohne "C" am Anfang
    Result:=Tools.Init2Single(length(fxImg[0]),length(fxImg[0,0]),dWord(fVal))
  else Tools.ErrorOut(3,cVal+sVal);
end;

{ fSV kombiniert alle Pixel der Kanäle "fxLft" und "fxRgt" mit dem Operator
  "iTyp" [+,-,*,/]. Der Prozess ignoriert Nodata Pixel und Null-Division. }

procedure tReduce.ScaleValues(
  fxLft,fxRgt:tn2Sgl; //erster, zweiter Kanal
  iTyp:integer); //Kombination (+,-,*,/)
const
  cOpr = 'Figure or filename expected: ';
var
  X,Y:integer;
begin
  //iTyp in [1..4]
  for Y:=0 to high(fxLft) do
  begin
    for X:=0 to high(fxLft[0]) do
      if isNan(fxLft[Y,X]) or isNan(fxRgt[Y,X]) then //nur definierte Werte
        fxLft[Y,X]:=NaN
      else //beide Seiten sind definiert
        case iTyp of
          1:fxLft[Y,X]+=fxRgt[Y,X];
          2:fxLft[Y,X]-=fxRgt[Y,X];
          3:fxLft[Y,X]*=fxRgt[Y,X];
          4:if fxRgt[Y,X]<>0 then fxLft[Y,X]/=fxRgt[Y,X];
          5:fxLft[Y,X]:=byte(fxLft[Y,X]<fxRgt[Y,X]);
          6:fxLft[Y,X]:=byte(fxLft[Y,X]>fxRgt[Y,X]);
        end;
  end;
end;

{ fBC kombiniert gewählte Kanäle aus "fxImg" mit arithmetischen Operatoren. Die
  Zahl der aufeinander folgenden Prozesse ist nicht beschränkt. Statt einem
  Kanal kann auch eine Konstante angegeben werden. }
{ fBC übernimmt ein Bild und eine Prozess-Kette. Die Kette besteht aus Kanal-
  Namen oder Zahlen, die durch Operatoren [+,-,*,/] getrennt sind. Kanal-Namen
  bestehen aus dem Buchstaben "B" und einer Ziffer für die fortlaufende Nummer
  des Kanals (B1, B2, ..). Zahlen ohne "B" werden als Konstante interpretiert.
  fCe erzeugt dafür einen Kanal, der nur den übergebenen Wert enthält. fCe
  führt alle Prozesse nacheinander aus. Die Algorithmen ignorieren NoData und
  Nulldivision. }

function tReduce.BandCalc(
  fxImg:tn3Sgl; //Vorbild
  sPrc:string): //Kombinations-Befehle (B1*2.4-B2+B3 ...), Blank-getrennt
  tn2Sgl; //Ergebnis der cumulierten Operationen
const
  cTyp = 'fCe: Calculation symbol [*,-,*,/,<,>] lacking or misspelled: ';
var
  fxCmb:tn2Sgl=nil; //kombinierter Kanal
  iOfs:integer=0; //Start-Position der Suche
  iPst:integer=0; //Position Verknüfungs-Symbol
  iTyp:integer=0; //Operator-Typ [1..4]
begin
  Result:=nil; //Sicherheit
  sPrc:=DelSpace(sPrc); //Blanks entfernen
  repeat
    iPst:=PosSetEx(['+','-','*','/','<','>'],sPrc,succ(iOfs));
    if (iPst=0) and (iOfs>0) then iPst:=succ(length(sPrc)); //letzter Eintrag (virtuelle Position)
    if iOfs>0 then //nicht erster Eintrag
    begin
      if sPrc[iOfs]='+' then iTyp:=1 else
      if sPrc[iOfs]='-' then iTyp:=2 else
      if sPrc[iOfs]='*' then iTyp:=3 else
      if sPrc[iOfs]='/' then iTyp:=4 else
      if sPrc[iOfs]='<' then iTyp:=5 else
      if sPrc[iOfs]='>' then iTyp:=6 else
        Tools.ErrorOut(3,cTyp+sPrc);
      fxCmb:=NewBand(fxImg,copy(sPrc,succ(iOfs),pred(iPst-iOfs))); //Kanal oder Konstante
      ScaleValues(Result,fxCmb,iTyp);
    end
    else Result:=NewBand(fxImg,LeftStr(sPrc,pred(iPst))); //Kanal oder Konstante
    iOfs:=iPst //aufholen
  until iPst=succ(length(sPrc)); //letzter Block
end;

{ rGS bestimmt die arithmetische Summe aller Kanäle in "fxImg" und gibt das
  Ergebnis als Bild zurück. rGS ignoriert NoData }

function tReduce.GlobalSum(
  fxImg:tn3Sgl): //Vorbild
  tn2Sgl; //Summe (Gewicht)
var
  B,X,Y:integer;
begin
  Result:=Tools.Init2Single(length(fxImg[0]),length(fxImg[0,0]),0); //Vorgabe = Null
  for B:=0 to high(fxImg) do
    for Y:=0 to high(Result) do
      for X:=0 to high(Result[0]) do
        if not isNan(fxImg[B,Y,X]) then
          Result[Y,X]+=fxImg[B,Y,X]; //arithmetische Summe
end;

{ rBO übernimmt den "besten" Pixel aus einem beliebigen Pixel-Stack und gibt
  das Ergebnis als Layer zurück }
{ rBO kopiert für jeden Bild-Pixel alle klaren Pixel in ein Array und übergibt
  es an die "Statistik". Dort sortiert rBO die Werte aufsteigend und übernimmt
  den Pixel in der Mitte als Ergebnis. Ist die Anzahl der Pixel gerade, bildet
  rBO den Mittelwert der beiden Pixel in der Mitte des Arrays. Bei zwei klaren
  Pixeln bildet rBO immer den Mittelwert, ein Pixel wird direkt übernommen. Ist
  kein Layer definiert, gibt rBO "NaN" zurück }

function tReduce.BestOf(
  fxImg:tn3Sgl; //Vorbild
  sCvr:string): //Qualitäts-Marker
  tn2Sgl; //Median
const
  cOne:single=1;
var
  faCvr:tnSgl=nil; //Qualitäts-Marker der verschiedenen Bilder
  faDev:tnSgl=nil; //ein Pixel aus allen Kanälen
  iDim:integer; //Anzahl Kanäle
  B,X,Y: integer;
begin
  Result:=Tools.Init2Single(length(fxImg[0]),length(fxImg[0,0]),dWord(NaN)); //Vorgabe = NoData
  SetLength(faDev,length(fxImg)); //alle Kanäle

  faCvr:=Tools.InitSingle(length(fxImg),dWord(cOne));
  if WordCount(sCvr,[','])=length(fxImg) then
    for B:=0 to high(faCvr) do //Qualitäts-Marker zusammenfassen
      faCvr[B]:=StrToFloat(ExtractWord(succ(B),sCvr,[',']));

  for Y:=0 to high(fxImg[0]) do
    for X:=0 to high(fxImg[0,0]) do
    begin //Array aus klaren Pixeln, Median oder Mittelwert bilden
      iDim:=0;
      for B:=0 to high(faDev) do
        if isNan(fxImg[B,Y,X])=False then //nicht NoData
        begin //Array mit definierten Werten (Layer)
          faDev[iDim]:=fxImg[B,Y,X]; //Pixel-Stack
          inc(iDim) //gültige Stellen
        end;
      if iDim>2 then //mindestens drei gültige Layer
      begin
        Table.QuickSort(faDev,iDim); //aufsteigend sortieren
        if not odd(iDim) then
          Result[Y,X]:=(faDev[pred(iDim div 2)]+faDev[iDim div 2])/2 //zwei Werte in der Mitte
        else Result[Y,X]:=faDev[iDim div 2] //Wert in der Mitte
      end
      else if iDim=2 then Result[Y,X]:=(faDev[0]+faDev[1])/2 //Mittelwert
      else if iDim=1 then Result[Y,X]:=faDev[0]; //verbleibenden Pixel verwenden
    end;
end;

{ rLi bestimmt Maximum oder Minimum für alle Pixel im Stack "fxStk" und gibt
  das Ergebnis als Kanal zurück. Einzelne Pixel dürfen leer (NoData) sein }

function tReduce.Limits(
  bMax:boolean; //ja=Maximum, nein=Minimum
  fxStk:tn3Sgl): //vergleichbare Kanäle
  tn2Sgl; //Ergebnis als Kanal
var
  fRes:single=0; //Ergebnis-Zwischenlager
  B,X,Y:integer;
begin
  Result:=Tools.Init2Single(length(fxStk[0]),length(fxStk[0,0]),dWord(NaN)); //Vorgabe
  for Y:=0 to high(fxStk[0]) do
    for X:=0 to high(fxStk[0,0]) do
    begin
      if bMax
        then fRes:=1-MaxSingle
        else fRes:=MaxSingle;
      for B:=0 to high(fxStk) do
        if not isNan(fxStk[B,Y,X]) then
          if bMax
            then fRes:=max(fxStk[B,Y,X],fRes)
            else fRes:=min(fxStk[B,Y,X],fRes);
      if (fRes<MaxSingle) and (fRes>1-MaxSingle) then
        Result[Y,X]:=fRes;
    end;
end;

{ rRg bestimmt die Regression aller Kanäle aus "fxImg" für einzelne Pixel und
  gibt sie als Bild zurück. rRg unterstellt, dass die Bilder nach dem Datum
  sortiert und die zeitlichen Abstände konstant sind }
{ rRg verwendet die Gauß'sche Regression. rRg prüft jeden Kanal auf NoData, so
  dass auch lückige Bilder verarbeitet werden können. }

function tReduce.Regression(
  fxImg:tn3Sgl): //Vorbild
  tn2Sgl; //Regression
var
  fDvs:double; //Dividend in Regressionsgleichung ← Nulldivision!
  fPrd:double; //Produkt aus Zeit und Wert (∑xy)
  fSqr:double; //Summe Werte-Quadrate (∑y²)
  fSum:double; //Summe Werte (∑y)
  fTms:double; //Summe Zeitachse (∑x)
  fVal:double; //aktueller Wert
  iCnt:integer=0; //Anzahl gültiger Kanäle (n)
  B,X,Y: integer;
begin
  //mindestens 3 Kanäle?
  Result:=Tools.Init2Single(length(fxImg[0]),length(fxImg[0,0]),dWord(NaN)); //Vorgabe = NoData
  for Y:=0 to high(fxImg[0]) do
    for X:=0 to high(fxImg[0,0]) do
    begin
      fPrd:=0; fSqr:=0; fSum:=0; fTms:=0; fVal:=0; iCnt:=0;
      for B:=0 to high(fxImg) do
      begin
        fVal:=fxImg[B,Y,X];
        if IsNan(fVal) then continue;
        fTms+=succ(B); //∑x <== gleiche Abstände unterstellt
        fSum+=fVal; //∑y
        fPrd+=fVal*succ(B); //∑xy
        fSqr+=sqr(fVal); //∑y²
        inc(iCnt) //Anzahl gültige Schritte
      end;
      if iCnt>0
        then fDvs:=fSqr-sqr(fSum)/iCnt
        else fDvs:=0;
      if fDvs>0
        then Result[Y,X]:=(fPrd-fTms*fSum/iCnt)/fDvs
        else Result[Y,X]:=NaN;
    end;
end;

{ rEx reduziert ein Multikanal-Bild zu einem Kanal. Der Prozess wird durch die
  Konstante "sCmd" gewählt. Für Vegetationsindices müssen zusätzlich mit "sPrc"
  die Kanal-IDs für Nir und Rot übergeben werden, für die Kanal-Arithmetik eine
  Formel. "BestOf" benötigt die Anteile klarer Pixel im Bild aus dem Header.
  Für "Regression" müssen die Kanalnamen mit einem Datum enden und die Periode
  (Kanäle pro Bild ) im Header muss definiert sein. }
{ ==> "fxImg" kann weniger Kanäle enthalten als "rHdr.Stk" angibt }

function tReduce.Execute(
  fxImg:tn3Sgl; //Vorbild, >1 Kanal
  sArt:string; //Kanäle, Arithmetik (Formel)
  sCmd:string; //Reduktions-Befehl (Konstante)
  var rHdr:trHdr): //Metadaten
  tn2Sgl; //Ergebnis
const
  cCmd = 'rEc: Reduce command not defined: ';
  cStk = 'rEc: Empty image passed: ';
var
  iLow,iHig:integer;
begin
  if Length(fxImg)<1 then Tools.ErrorOut(3,cStk+sCmd);
  if (length(sArt)>0) and (pos(':',sArt)>1) then
    Reduce.GetBands(iHig,iLow,rHdr.Stk,sArt); //Kanal-Nummern, Zahlen ab Eins
//  if (sCmd=cfLai) or (sCmd=cfNiv) or (sCmd=cfNbu) or (sCmd=cfNvi) or (sCmd=cfEvi) then Reduce.GetBands(iHig,iLow,rHdr.Stk,sArt); //Kanal-Nummern, Zahlen ab Eins
  if sCmd=cfBrt then Result:=Brightness(fxImg) else //Hauptkomponente
  if sCmd=cfBst then Result:=BestOf(fxImg,rHdr.aCvr) else //Median-Mean-Defined
  if sCmd=cfClc then Result:=BandCalc(fxImg,sArt) else //Kanal-Arithmetik
  if sCmd=cfLai then Result:=_LeafArea(fxImg,pred(iHig),pred(iLow)) else //LAI-Näherung
  if sCmd=cfMax then Result:=Limits(true,fxImg) else //Maximum
  if sCmd=cfMdn then Result:=Median(fxImg) else //Median
  if sCmd=cfMea then Result:=MeanValue(fxImg) else //Mittelwert
  if sCmd=cfMin then Result:=Limits(false,fxImg) else //Minimum
  if sCmd=cfOvl then Result:=Overlay(fxImg) else //Überlagerung
  if sCmd=cfRgs then Result:=Regression(fxImg) else //Regression
  if sCmd=cfNbu then Result:=Vegetation(fxImg,pred(iHig),pred(iLow),1) else //NDBI Index NICHT DOKUMENTIERT
  if sCmd=cfNiv then Result:=Vegetation(fxImg,pred(iHig),pred(iLow),0) else //NirV Index
  if sCmd=cfNvi then Result:=Vegetation(fxImg,pred(iHig),pred(iLow),1) else //NDVI Index
  if sCmd=cfEvi then Result:=Vegetation(fxImg,pred(iHig),pred(iLow),2) else //EVI Index
  if sCmd=cfVrc then Result:=Variance(fxImg) else //Varianz
  if sCmd=cfWgt then Result:=GlobalSum(fxImg) else //Summe aller Kanäle
    Tools.ErrorOut(3,cCmd+sCmd);
end;

{ rSc reduziert gestapelte multispektrale Bilder zu einem multispektralen Bild.
  Dabei reduziert rSc gleiche Kanäle aus verschiedenen Bildern mit dem Befehl
  "sCmd" zu jeweils einem Kanal und speichert die neuen Kanäle in der alten
  Reihenfolge unter dem Namen des Befehls. rSc mittelt das Datum in den Kanal-
  Namen. Alle Bilder im Stapel "sImg" müssen dieselben Kanäle haben. rSc liest
  und schreibt im ENVI-Format.
  ==> vgl. "xReduce" auf einen Kanal (Indices) }

procedure tReduce.xSplice(
  sArt:string; //Kanäle, Prozesse
  sCmd:string; //Prozess
  sImg:string; //Vorbild
  sTrg:string); //Ergebnis-Name ODER leer für Prozess-Name
const
  cFex = 'rSe: Image not found: ';
  cPrd = 'rSe: No period given for image splice: ';
var
  fxRes:tn2Sgl=nil; //Ergebnis-Kanal für aktuelle Gruppe
  fxStk:tn3Sgl=nil; //Kanäle aus Import mit gleicher Gruppen-Nr
  rHdr,rRdh:trHdr; //Metadaten, M mit reduzierten Kanälen
  B,I:integer;
begin
  if not FileExists(sImg) then Tools.ErrorOut(3,cFex+sImg);
  Header.Read(rHdr,sImg);
  if rHdr.Prd<1 then Tools.ErrorOut(3,cPrd+sImg);
  if trim(sTrg)='' then sTrg:=eeHme+sCmd; //Vorgabe = Prozess-Name
  if rHdr.Prd<rHdr.Stk then //nur wenn mehr als ein Bild
  begin
    SetLength(fxStk,rHdr.Stk div rHdr.Prd,1,1); //Dummy, Ein Kanal für jedes Bild
    Header.CopyRecord(rHdr,rRdh); //Kopie (ohne Palette)
    rRdh.Stk:=rHdr.Stk div rHdr.Prd; //ein Kanal pro Vorbild
    for B:=0 to pred(rHdr.Prd) do //alle Ergebnis-Kanäle
    begin
      for I:=0 to pred(rHdr.Stk div rHdr.Prd) do //alle Vorbilder
        fxStk[I]:=Image.ReadBand(I*rHdr.Prd+B,rHdr,sImg); //Kanal "B" aus Bild "I" laden
      fxRes:=Execute(fxStk,sArt,sCmd,rRdh); //multiplen Kanal reduzieren
      Image.WriteBand(fxRes,B,sTrg); //Kanal schreiben
      with rHdr do write(#13+IntToStr(Stk-Stk div Prd*B-B)+#32); //Ablauf
    end;
    Header.WriteMulti(rHdr.Prd,rHdr.Prd,rHdr,'',sTrg); //Kanal-Namen
  end
  else Tools.EnviCopy(sImg,sTrg); //unverändert verwenden
  Tools.HintOut(true,'Splice: '+sCmd); //Ablauf ersetzen
end;

{ mIC klassifiziert die spektralen Attribute aller Pixel in "sImg" mit dem
  Modell "fxMdl" und gibt das Ergebnis als Byte-Matrix zurück. mIC
  klassifiziert nach dem Prinzip des "minimum distance" im Merkmalsraum. }

function tModel.ClassifyPixels(
  fxMdl:tn2Sgl; //Klassen-Vorbild
  sImg:string): //Vorbild (ENVI-Format)
  tn2Byt; //Klassifikation als Bild
const
  cMdl = 'tMIC: No density model given!';
var
  fMin:single; //kleinste (quadrierte) Distanz
  fSed:single; //quadrierte Distanz
  fxImg:Tn3Sgl=nil; //Vorbild mit allen Kanälen
  rHdr:trHdr; //Metadaten
  B,M,X,Y:integer;
begin
  Result:=nil;
  if fxMdl=nil then Tools.ErrorOut(3,cMdl);
  Header.Read(rHdr,sImg); //Metadaten
  fxImg:=Image.Read(rHdr,sImg); //Bild mit allen Kanälen
  Result:=Tools.Init2Byte(rHdr.Lin,rHdr.Scn); //Klassen-Ergebnis
  for Y:=0 to pred(rHdr.Lin) do
    for X:=0 to pred(rHdr.Scn) do
    begin
      if IsNan(fxImg[0,Y,X]) then continue; //nur definierte Pixel
      fMin:=MaxSingle; //"unendlich"
      for M:=1 to high(fxMdl) do
      begin
        fSed:=0;
        for B:=0 to high(fxImg) do
          fSed+=sqr(fxImg[B,Y,X]-fxMdl[M,succ(B)]);
        if fSed<fMin then
        begin
          Result[Y,X]:=M;
          fMin:=fSed
        end;
      end;
    end;
  Tools.HintOut(true,'ClassifyPixels: '+IntToStr(length(fxMdl)));
end;

{ lSZ vergibt einen negative ID an alle Zonen, die weniger als "iMin" innere
  Kontakte haben. Dazu verwendet lSZ ausschließlich die Topologie-Tabelle.
  lSZ gibt das Ergebnis in "iaFix" und die Zahl der markierten Zonen als
  Funktionswert zurück. "iaFix" muss mit allen aktuellen IDs incl. Null als
  fortlaufende Reihe initialisiert sein. }

function tLimits.SieveZones(
  iaFix:tnInt; //negatives Vorzeichen für Zonen mit zu wenig inneren Kontakten
  iMin:integer; //Minimum interne Kontakte
  ixTpl:tn2Int): //Zonen-Topologie
  integer; //Anzahl kleine Zonen
var
  iaDim:tnInt=nil; //Index auf "iacNbr, iacPrm"
  iaNbr:tnInt=nil; //Index der Nachbarzelle
  iaPrm:tnInt=nil; //Kontakte zur Nachbarzelle
  iLnk:integer; //innere Kontakte
  N,Z:integer;
begin
  Result:=0;
  iaDim:=ixTpl[0]; //Zeiger auf Startadressen
  iaNbr:=ixTpl[1]; //Zeiger auf IDs der Nachbarzellen
  iaPrm:=ixTpl[2]; //Zeiger auf Kontakte zu Nachbarzellen

  for Z:=1 to high(iaDim) do //alle Zonen
  begin
    iLnk:=0; //Vorgabe = keine inneren Grenzen
    for N:=iaDim[pred(Z)] to pred(iaDim[Z]) do //alle Kontakte (auch innere)
      if iaNbr[N]=Z then
        iLnk:=iaPrm[N];
    if iLnk<iMin then
    begin
      iaFix[Z]:=-iaFix[Z]; //kleine Zone = negativ
      inc(Result) //Veränderungen zählen
    end;
  end;
end;

{ lMZ sucht zu jeder Zone mit negativer ID eine Zone mit positiver ID und der
  höchsten Zahl an Kontakten. Findet lMZ eine solche Zone, übernimmt lMZ die
  ID der Nachbar-Zone auch für die aktuelle. lMZ verwendet dafür ausschließlich
  die Topologie-Tabelle. Die Suche muss nicht in jedem Fall Erfolg haben. Das
  Ergebnis kann negative IDs enthalten! }

procedure tLimits.MergeZones(
  ixTpl:tn2Int; //Zonen-Topologie
  iaFix:tnInt); //Zonen-IDs, negativ für kleine Zonen
var
  iaDim:tnInt=nil; //Index auf "iacNbr, iacPrm"
  iaNbr:tnInt=nil; //Index der Nachbarzelle
  iaPrm:tnInt=nil; //Kontakte zur Nachbarzelle
  iPrm:integer; //Anzahl Kontakte
  N,Z:integer;
begin
  iaDim:=ixTpl[0]; //Zeiger auf Startadressen
  iaNbr:=ixTpl[1]; //Zeiger auf IDs der Nachbarzellen
  iaPrm:=ixTpl[2]; //Zeiger auf Kontakte zu Nachbarzellen
  for Z:=1 to high(iaDim) do //alle Zellen
    if iaFix[Z]<0 then //nur kleine Zonen
    begin
      iPrm:=0;
      for N:=iaDim[pred(Z)] to pred(iaDim[Z]) do //alle Kontakte (auch innere)
        if (iaPrm[N]>iPrm) //längster Kontakt
        and (iaNbr[N]<>Z) //nicht innere Kontakte
        and (iaFix[iaNbr[N]]>0) then //stabile Zone
        begin
          iaFix[Z]:=iaNbr[N]; //neue ID ← nicht mit Array-Index identisch!
          iPrm:=iaPrm[N]; //längster Kontakt
        end;
    end;
end;

{ lRI trägt die Zonen-IDs aus "iaFix" in das Zonen-Bild "ixIdx" ein und gibt
  die neue Anzahl aller Zonen als Funktionswert zurück. "iaFix" kann negative
  IDs enthalten und die IDs können sich wiederholen. lRI zählt in "iaCnt" die
  Anzahl aller IDs, vergibt neue, fortlaufende IDs in "iaFix" und trägt sie in
  das Zonen-Bild "iaIdx" ein. }

function tLimits.RecodeIndex(
  iaFix:tnInt; //Transformations-Liste
  ixIdx:tn2Int): //Zonen-IDs WIRD VERÄNDERT!
  integer; //neue Anzahl Zonen
var
  iaCnt:tnInt=nil; //Pixel pro Zone
  X,Y,Z:integer;
begin
  Result:=0;
  iaCnt:=Tools.InitInteger(length(iaFix),0); //Zähler

  for Y:=0 to high(ixIdx) do
    for X:=0 to high(ixIdx[0]) do
    begin
      ixIdx[Y,X]:=abs(iaFix[ixIdx[Y,X]]); //neue Zonen-ID
      inc(iaCnt[ixIdx[Y,X]]) //Pixel pro Zone
    end;

  FillDWord(iaFix[0],length(iaFix),0); //leeren für neue IDs
  for Z:=1 to high(iaFix) do
    if iaCnt[Z]>0 then
    begin
      inc(Result); //neue fortlaufende Zonen-ID
      iaFix[Z]:=Result//neue ID an alter Position
    end;

  for Y:=0 to high(ixIdx) do
    for X:=0 to high(ixIdx[0]) do
      ixIdx[Y,X]:=iaFix[ixIdx[Y,X]]; //ID im Zonen-Bild ändern
end;

{ lSZ entfernt Zonen mit weniger als "iMin" inneren Kontakten und vereinigt sie
  mir der Zone, mit der sie die meisten Kontakte gemeinsam hat. Der Prozess
  kann eine Iteration erfordern. lSZ markiert entsprechende Zonen mit einer
  negativen ID, überträgt die ID der Nachbar-Zone mit der längsten gemeinsamen
  Grenze, vergibt fortlaufende IDs an alle Zonen und erneuert die Topologie-
  Tabelle. Nach der Transformation ersetzt lSZ den Raster-Index-Datei und
  bildet neue Grenzen. }

procedure tLimits.xSieveZones(iMin:integer); //Minimum innere Kontakte
var
  iaFix:tnInt=nil; //Transformations-Liste für neue Zonen-IDs
  iMdf:integer; //Kontrolle
  ixIdx:tn2Int=nil; //Zonen-IDs
  ixTpl:tn2Int=nil; //Zonen-Topologie
  rHdr:trHdr;
begin
  Header.Read(rHdr,eeHme+cfIdx); //Metadaten Zellindex
  ixIdx:=tn2Int(Image.ReadBand(0,rHdr,eeHme+cfIdx)); //Zellindex-Bild
  repeat
    iaFix:=Tools.InitIndex(succ(rHdr.Cnt)); //fortlaufend
    ixTpl:=tn2Int(Tools.BitRead(eeHme+cfTpl)); //aktuelle Topologie
    iMdf:=SieveZones(iaFix,iMin,ixTpl); //negative Zonen-IDs in "iaFix" + Anzahl markierter Zonen
    if iMdf<1 then break;
    MergeZones(ixTpl,iaFix); //Nachbar-Zone mit häufigsten Kontakten
    rHdr.Cnt:=RecodeIndex(iaFix,ixIdx); //neue IDs eintragen + speichern
    Build.IndexTopology(rHdr.Cnt,ixIdx); //neue Topologie
  until iMdf<1;
  Image.WriteBand(tn2Sgl(ixIdx),0,eeHme+cfIdx); //transformierte Zonen speichern
  Header.WriteIndex(rHdr.Cnt,rHdr,eeHme+cfIdx); //Metadaten speichern
  Gdal.ZonalBorders(eeHme+cfIdx); //neue Zellgrenzen
end;

{ mCV überträgt die Farben aus einer Klassen-Definition auf den Klassen-Layer.
  Die Klassen-Definition und Klassen-Layer müssen korrespondieren. mCV bestimmt
  für alle Kanäle die maximalen Werte aus der Klassen-Definition und scaliert
  damit die Farbdichten in der Palette auf 0..$FF. mCV verändert nur den Header
  des Klassen-Layers. }

procedure tModel.xClassValues(iRed,iGrn,iBlu:integer); //Kanäle [1..N]
const
  cCnt = 'mCV: Misfit between internal class layer and class definition!';
var
  fMax:single=0; //höchster Wert in Klassen-Defiition
  fxMdl:tn2Sgl=nil; //Klassen-Definition
  rHdr:trHdr; //Metadaten
  T:integer;
begin
  fxMdl:=Tools.BitRead(eeHme+cfMdl); //Klassen-Definition
  Header.Read(rHdr,eeHme+cfMap); //Metadaten
  if rHdr.Cnt<>high(fxMdl) then Tools.ErrorOut(3,cCnt+cfMdl);

  for T:=1 to high(fxMdl) do
  begin
    fMax:=max(fxMdl[T,iRed],fMax);
    fMax:=max(fxMdl[T,iGrn],fMax);
    fMax:=max(fxMdl[T,iBlu],fMax);
  end;
  fMax:=$FF/fMax;

  rHdr.Pal:=Tools.InitCardinal(succ(rHdr.Cnt)); //leere Palette
  for T:=1 to high(fxMdl) do
    rHdr.Pal[T]:=
      trunc(fxMdl[T,iRed]*fMax) +
      trunc(fxMdl[T,iGrn]*fMax) shl 8 +
      trunc(fxMdl[T,iBlu]*fMax) shl 16;
  rHdr.Pal[0]:=0;

  Header.WriteThema(rHdr.Cnt,rHdr,rHdr.Fld,eeHme+cfMap);
end;

{ rHy reduziert einen Multi-Image-Stack zu einer Zeitreihe. Im Ergebnis hat
  jeder Kanal ein anderes Datum. Der Vorbild-Stack muss einen erweiterten ENVI-
  Header haben.
    rHy übernimmt aus dem Stapel alle Kanäle mit gleichem Datum und reduziert
  sie entsprechend dem "execute" Befehl. Danach stapelt rHy die Ergebnisse.
  Jedes Ergebnis hat dann ein anderes Aufnahmedatum. Vergleiche: "xSplice"
  reduziert ebenfalls multispektrale Bilder-Stapel, bewahrt die Kanäle und
  reduziert die Zeit. }

procedure tReduce.xHistory(
  sArt:string; //Kanäle, Arithmetik
  sCmd:string; //Reduktions-Befehl
  sImg:string; //Vorbild [ENVI]
  sTrg:string); //Ergebnis-Name, Vorgabe = Prozess-Name
const
  cPrd = 'rCg: Bands per image seem to differ at: ';
var
  fNan:single=NaN; //NoData als Variable
  fxRes:tn2Sgl=nil; //Ergebnis-Kanal = Hauptkomponente aus einem Bild
  fxTmp:tn3Sgl=nil; //quadrierte Differenzen
  iStk:integer=0; //neue Anzahl Kanäle
  rHdr,rRdh:trHdr; //Metadata, M mit reuzierten Kanälen
  B,I:integer;
begin
  Header.Read(rHdr,sImg);
  if rHdr.Stk mod rHdr.Prd>0 then Tools.ErrorOut(3,cPrd+sImg);
  if sTrg='' then sTrg:=eeHme+sCmd; //Prozess-Name
  fxRes:=Tools.Init2Single(rHdr.Lin,rHdr.Scn,dWord(fNan)); //Vorgabe = ungültig
  fxTmp:=Tools.Init3Single(rHdr.Prd,rHdr.Lin,rHdr.Scn,0); //Vorgabe = leer
  Header.CopyRecord(rHdr,rRdh); //Kopie (ohne Palette) für "execute"
  rRdh.Stk:=rHdr.Prd; //einzelnes Bild, alle Kanäle
  for I:=0 to pred(rHdr.Stk div rHdr.Prd) do //alle Bilder, notfalls nur eines
  begin
    for B:=0 to pred(rHdr.Prd) do //Kanäle in einem Bild
      fxTmp[B]:=Image.ReadBand(I*rHdr.Prd+B,rHdr,sImg); //Kanal "B" aus Bild "I" laden
    fxRes:=Execute(fxTmp,sArt,sCmd,rRdh); //Reduktion für ausgewählte Kanäle
    Image.WriteBand(fxRes,iStk,sTrg); //neues Bild für B=0, dann stapeln
    inc(iStk); //neue Kanäle zählen
    write(#13+IntToStr(iStk)+#32);
  end;
  Header.WriteMulti(1,iStk,rHdr,'',sTrg);
  Tools.HintOut(true,'History: '+sCmd);
end;

{ rRe reduziert mit dem Befehl "sCmd" alle Kanäle in "sImg" auf einen Ergebnis-
  Kanal und speichert ihn unter dem Namen des Befehls. rRe aktualisiert den
  Header und mittelt den Qualitäts-Index. }

procedure tReduce.xReduce(
  sArt:string; //Kanäle, Arithmetik, Ergänzung
  sCmd:string; //Befehl
  sImg:string; //Name des Vorbilds
  sTrg:string); //Name Ergebnis ODER leer
const
  cFex = 'rRe: Image not found: ';
var
  fxRes:tn2Sgl=nil; //Ergebnis der Reduktion
  fxStk:tn3Sgl=nil; //Stack zur Reduktion
  rHdr:trHdr; //Metadaten
begin
  if not FileExists(sImg) then Tools.ErrorOut(3,cFex+sImg);
  Header.Read(rHdr,sImg);
  fxStk:=Image.Read(rHdr,sImg); //Import vollständig
  fxRes:=Execute(fxStk,sArt,sCmd,rHdr); //Befehl anwenden
  if trim(sTrg)='' then sTrg:=eeHme+sCmd; //Vorgane = Name des Befehls
  Image.WriteBand(fxRes,0,sTrg); //neue Datei aus Ergebnis
  rHdr.Prd:=1; //nur ein Kanal
  rHdr.aBnd:=sCmd; //Kanal-Name = Prozess
  Header.WriteScalar(rHdr,sTrg); //Header für einen Kanal
  Tools.HintOut(true,'Reduce:'+ExtractFileName(sTrg)); //Status
end;

{ rLA gibt eine Näherung für den Leaf Area Index als Kanal zurück. "iRed" und
  "iNir" müssen die Wellenländen von Rot und nahem Infrarot bezeichnen
  (vgl. Liu_2012 "LAI") }
//fEvi:=2.5 × (NIR − RED) / (1 + NIR + 6 × RED − 7.5 × GRN)

function tReduce._LeafArea(
  fxImg:tn3Sgl; //Vorbild
  iNir,iRed:integer): //Kanal-IDs, kommagetrennt
  tn2Sgl; //Vorbild: Vegetationsindex
const
  cDim = 'tFV: Vegetation index calculation needs two bands!';
var
  fEvi:single;
  X,Y:integer;
begin
  if length(fxImg)<2 then Tools.ErrorOut(3,cDim);
  Result:=Tools.Init2Single(length(fxImg[0]),length(fxImg[0,0]),dWord(NaN)); //Vorgabe = NoData
  for Y:=0 to high(fxImg[0]) do
    for X:=0 to high(fxImg[0,0]) do
      if not IsNan(fxImg[0,Y,X]) then
      begin
        fEvi:=2.5*(fxImg[iNir,Y,X]-fxImg[iRed,Y,X])/
          (fxImg[iNir,Y,X]+2.4*fxImg[iRed,Y,X]+1.0);
        Result[Y,X]:=-(1/0.273)*ln(1.102*(1.0-0.910*fEvi));
      end;
end;

{ rQC zählt die gültigen Kanäle für jeden Pixel in "fxImg" und gibt das
  Ergebnis als Bild zurück.}

procedure tReduce.xQualityImage(
  sImg:string; //Vorbild
  sTrg:string); //Ergebnis-Name ODER leer für Vorgabe
const
  cImg = 'rQI: Image not found: ';
var
  fxBnd:tn2Sgl=nil; //Vorbild-Kanal
  fxRes:tn2Sgl=nil; //Ergebnis
  iCnt:int64=0; //Anzahl Pixel in allen Layern
  iClr:integer=0; //Anzahl klare Pixel
  rHdr:trHdr; //Metadaten
  B,X,Y:integer;
begin
  if not FileExists(sImg) then Tools.ErrorOut(3,cImg+sImg);
  Header.Read(rHdr,sImg);
  fxRes:=Tools.Init2Single(rHdr.Lin,rHdr.Scn,0); //Vorgabe = Null

  for B:=0 to pred(rHdr.Stk div rHdr.Prd) do //alle (multispektralen) Bilder
  begin
    fxBnd:=Image.ReadBand(B*rHdr.Prd,rHdr,sImg); //erster Kanal in Bild "B"
    for Y:=0 to pred(rHdr.Lin) do
      for X:=0 to pred(rHdr.Scn) do
        if not isNan(fxBnd[Y,X]) then
        begin
          fxRes[Y,X]+=1.0;
          inc(iClr)
        end;
  end;

  if trim(sTrg)=''
    then sTrg:=eeHme+cfQuy //Vorgane = Name des Befehls
    else sTrg:=sTrg+'_'+cfQuy; //Erweiterung
  Image.WriteBand(fxRes,0,sTrg); //neue Datei aus Ergebnis
  rHdr.aBnd:=cfQuy; //Kanal-Name = Quality
  iCnt:=rHdr.Lin*rHdr.Scn*(rHdr.Stk div rHdr.Prd); //Pixel in allen Layern
  if iCnt>0 then rHdr.aCvr:=FloatToStrF(iClr/iCnt,ffFixed,7,4); //Anteil klarer Pixel
  Header.WriteScalar(rHdr,sTrg); //Header für einen Kanal
  Tools.HintOut(true,'QualityImage: '+ExtractFileName(sTrg)); //Status
end;

{ mPS wählt einen zufäligen Bildpunkt und gibt seine Spektralkombination in
  "fxMdl[0]" zurück. mPs ignoriert NoData-Pixel. NoData-Pixel müssen in allen
  Kanälen gleich sein. "fxMdl[?,0]" enthält Metadaten, die Merkmale beginnen
  mit "fxMdl[?,1] }

procedure tModel.SelectPixel(
  fxMdl:tn2Sgl; //Klassen-Definition
  fxImg:tn3Sgl; //Vorbild
  var rHdr:trHdr); //Metadaten
var
  B,X,Y:integer;
begin
  repeat
    X:=random(rHdr.Scn); //zufällige Auswahl
    Y:=random(rHdr.Lin);
  until not isNan(fxImg[0,Y,X]); //nur definierte Pixel
  for B:=1 to high(fxMdl[0]) do //alle Kanäle
    fxMdl[0,B]:=fxImg[pred(B),Y,X]; //neue Merkmale in Null eintragen
end;

{ mSF integriert die Stichprobe in "fxMdl[0]" in die am besten passende Klasse
  von "fxMdl[1..N]". Durch die Integration bewegt sich die Klasse um 1/ccRtn
  auf die neue Probe zu }

procedure tModel.SampleFit(fxMdl:tn2Sgl);
var
  fMin:single=MaxSingle; //kleinste (quadrierte) Distanz
  fTmp:single; //Zwischenlager für "fMin"
  iFit:integer=0; //ID der ähnlichsten Klasse
  B,M:integer;
begin
  fMin:=MaxSingle;
  for M:=1 to high(fxMdl) do //ähnlichste Klasse suchen
  begin
    fTmp:=0; //Vorgabe
    for B:=1 to high(fxMdl[0]) do //alle Kanäle/Attribute ohne Null
      fTmp+=sqr(fxMdl[M,B]-fxMdl[0,B]);
    if fTmp<fMin then
    begin
      iFit:=M; //temporär ähnlichste Klasse
      fMin:=fTmp //quadrierte Distanz
    end;
  end;
  for B:=1 to high(fxMdl[0]) do
    fxMdl[iFit,B]:=(fxMdl[iFit,B]*ccRtn + fxMdl[0,B])/succ(ccRtn); //Merkmale vereinigen
end;

{ mSM integriert die Stichprobe "fxMdl[0]" in die bestehenden Klassem ODER
  vereinigt zwei bestehende Klassem und ergänzt "fxMdl[0]" als neue Klasse.
  mSM bestimmt dazu die Distanz aller Klassen-Kombinationen incl. "fxMdl[0]".
  Besteht die kleinste Differenz zwischen zwei bestehenden Klassen, bildet mSM
  den Mittelwert zwischen beiden und übernimmt die Stichprobe als neue Klasse.
  Andernfalls bewegt mSM die am besten passende Klasse um 1/ccRtn auf die
  Stichprobe zu. }

procedure tModel.SampleMerge(fxMdl:tn2Sgl);
var
  fMin:single=MaxSingle; //kleinste (quadrierte) Distanz
  fTmp:single=0; //aktuelle (quadrierte) Distanz
  iHig,iLow:integer; //IDs der ähnlichsten Klassen
  B,M,N:integer;
begin
  for M:=1 to high(fxMdl) do
    for N:=0 to pred(M) do //alle Klassen-Kombinationen incl. Null
    begin
      fTmp:=0;
      for B:=1 to high(fxMdl[0]) do
        fTmp+=sqr(fxMdl[M,B]-fxMdl[N,B]); //quadrierte Distanz
      if fTmp<fMin then
      begin
        iHig:=M;
        iLow:=N; //ähnlichstes Paar
        fMin:=fTmp //kleinste Distanz
      end;
    end;

  if iLow>0 then //wenn ähnlichstes Paar bei alten Klassen
    for B:=1 to high(fxMdl[0]) do
    begin
      fxMdl[iHig,B]:=(fxMdl[iHig,B]+fxMdl[iLow,B])/2; //Klassen 1:1 vereinigen
      fxMdl[iLow,B]:=fxMdl[0,B] //freie Klasse füllen
    end
  else //Stichprobe integrieren
    for B:=1 to high(fxMdl[0]) do
      fxMdl[iHig,B]:=(fxMdl[iHig,B]*ccRtn + fxMdl[0,B])/succ(ccRtn); //Merkmale vereinigen
end;

{ mSA übernimmt die Stichprobe in "fxMdl[0]" als neue Klasse "fxMdl[iPst]" }

procedure tModel.SampleAdd(fxMdl:tn2Sgl; iPst:integer);
var
  B:integer;
begin
  for B:=1 to high(fxMdl[0]) do
    fxMdl[iPst,B]:=fxMdl[0,B]; //neue Klasse kopieren
end;

{ mPM clustert Spektralkombinationen im einem Bild. Die Zahl der Klassen und
  die Dichte der Stichproben ist wählbar. mPM speichert die Klassen-Definition
  als "model.bit" und den Klassen-Layer als "mapping". }
{ mPM nimmt "iSmp" zufällige Stichproben aus dem gesamten Bild und bildet damit
  "iMap" Klassen. Das Training hat drei Phasen. (1) mPM übernimmt die ersten
  "iMap" Proben als Klassen. (2) Ab "iMap" Proben vergleicht mPM alle Klassen-
  Kombinaionen einschließlich der neuen Probe untereinander. Sind sich zwei
  alte Klassen ähnlicher als die Probe mit einer der alten Klassen, vereinigt
  mRF die beiden ähnlichsten Klassen und ergänzt die Probe als neue Klasse.
  Andernfalls vereinigt mPM die neue Probe mit der ähnlichsten Klasse. (3) Ab
  "iMap*cOvr" Proben vereinigt mPM neue Proben mit der ähnlichsten Klasse. }
{ Wenn mPM eine Probe mit einer Klasse vereinigt, bewegt sich die Klassen-
  Definition um "1/cRtn" auf die Probe zu. Der Anteil einer Probe an der Klasse
  sinkt damit im Lauf des Trainings. Mit "cRtn"=30 ist nach ca. 150 Proben ein
  Gleichgewicht zwischen Bestand und Veränderung erreicht, mit "cRtn"=10
  genügen ca. 50 Proben. }

procedure tModel.xPixelMap(
  iMap:integer; //Anzahl Klassen
  iSmp:integer; //Stichproben
  sImg:string); //Vorbild
const
  cMap = 'mRP: Too many classes! Maximal 250 classes are defined';
  cSmp = 'mRP: At least 100 samples per class must be passed!';
var
  fxImg:tn3Sgl=nil; //Vorbild
  fxMdl:tn2Sgl=nil; //Klassen-Definitionen
  ixMap:tn2Byt=nil; //Klassen-Layer
  rHdr:trHdr; //Metadaten
  I:integer;
begin
  if iMap>250 then Tools.ErrorOut(3,cMap);
  if iSmp<iMap*100 then Tools.ErrorOut(3,cSmp);
  Image.AlphaMask(sImg); //NoData-Maske auf alle Kanäle ausdehnen
  Header.Read(rHdr,sImg); //Metadaten Vorbild
  fxImg:=Image.Read(rHdr,sImg); //Vorbild, alle Kanäle
  fxMdl:=Tools.Init2Single(succ(iMap),succ(length(fxImg)),0); //leere Definitionen
  RandSeed:=cfRds; //Zufalls-Generator initialisieren
  for I:=1 to iSmp do
  begin
    SelectPixel(fxMdl,fxImg,rHdr); //zufälligen Bildunkt in fxMdl[0] eintragen
    if I>iMap*ccOvr then SampleFit(fxMdl) //Phase 3: Sample in ähnlichste Klasse integrieren
    else if I>iMap then SampleMerge(fxMdl) //Phase 2: Klassen vereinigen ODER Sample integrieren
    else SampleAdd(fxMdl,I); //Phase 1: Klassen-Array initialisieren
  end;
  Tools.BitWrite(fxMdl,eeHme+cfMdl); //Klassen-Definition als BIT-Tabelle speichern
  Tools.HintOut(true,'Model: '+cfMdl);
  Tools.CopyFile(eeHme+cfMdl,eeHme+ChangeFileExt(cfMdl,'')+'_full'+cfBit);
  fxMdl:=Tools.BitRead(eeHme+cfMdl); //Klassen-Definition
  ixMap:=ClassifyPixels(fxMdl,sImg); //Klassen-IDs als Raster
  Image.WriteThema(ixMap,eeHme+cfMap); //Bild als "mapping" speichern
  Header.WriteThema(iMap,rHdr,'',eeHme+cfMap); //Metadaten speichern
  Tools.HintOut(true,'PixelMap: '+cfMap)
end;

{ mZS wählt zufällige Punkte aus dem Zonen-Bild "ixIdx" und gibt die Attribute
  der getroffenen Zone als "fxMdl[0]" zurück. }

procedure tModel.SelectZone(
  fxAtr:tn2Sgl; //Attribut-Tabelle
  fxMdl:tn2Sgl; //Klassen-Definitionen
  ixIdx:tn2Int; //Zonen-IDs als Bild
  var rHdr:trHdr); //Metadaten
var
  bRun:boolean; //alle Attribute definiert
  B,X,Y:integer;
begin
  repeat
    bRun:=True; //Vorgabe = alles definiert
    repeat
      X:=random(rHdr.Scn); //zufällige Auswahl
      Y:=random(rHdr.Lin);
    until not isNan(ixIdx[Y,X]); //nur definierte Pixel
    for B:=1 to high(fxMdl[0]) do //alle Attribute
    begin
      fxMdl[0,B]:=fxAtr[pred(B),ixIdx[Y,X]]; //neue Merkmale in Null eintragen
      bRun:=isNan(fxMdl[0,B])=False; //nicht definierte Attribute?
      if not bRun then break; //neues Beispiel suchen
    end
  until bRun;
end;

{ mFC klassifiziert die Attribute aller Zonen in "fxAtr" mit dem Modell "fxMdl"
  und gibt das Ergebnis als Klassen-Attribut "mapping.bit" und als Wert der
  Funktion zurück. mFC klassifiziert nach dem Prinzip des "minimum distance" im
  Merkmalsraum. }

function tModel.ClassifyZones(
  fxAtr:tn2Sgl; //Zonen-Attribute
  fxMdl:tn2Sgl): //Klassen-Vorbild
  tnInt; //Klassen-Attribut
const
  cMdl = 'tMIC: No density model given!';
var
  fMin:single; //kleinste (quadrierte) Distanz
  fSed:single; //quadrierte Distanz
  B,M,Z:integer;
begin
  Result:=nil;
  if fxMdl=nil then Tools.ErrorOut(3,cMdl);
  Result:=Tools.InitInteger(length(fxAtr[0]),0); //Klassen-IDs als Attribut

  for Z:=1 to high(fxAtr[0]) do //alle Zonen
  begin
    fMin:=MaxSingle; //"unendlich"
    for M:=1 to high(fxMdl) do
    begin
      fSed:=0;
      for B:=0 to high(fxAtr) do
        fSed+=sqr(fxAtr[B,Z]-fxMdl[M,succ(B)]);
      if fSed<fMin then
      begin
        Result[Z]:=M;
        fMin:=fSed
      end;
    end;
  end;
  Tools.HintOut(true,'ClassifyZones: '+IntToStr(length(fxMdl)));
end;

{ iGB gibt die Ziffern in einer Formel "BX:BY" zurück, wobei X,Y für natürliche
  Zahlen stehen, die durch einen Doppelpunkt getrennt sind. Mit "sArt"='' gibt
  iGB das Intervall 1.."iStk" zurück. "BX" ohne Doppelpunkt selektiert einen
  Kanal }

procedure tReduce.GetBands(
  var iHig,iLow:integer; //Kanal-Nummern als Rückgabe
  iStk:integer; //Anzahl Kanäle als Vorgabe
  sArt:string); //Kanal-IDs (Arithmetik) z.B. B2:B4 für Kanal 2 bis 4
const
  cBnd = 'rNR: Band indicator missing, misspelled or exceed image bands: ';
var
  sHig:string=''; //Nummer letzter Kanal (ab Eins)
  sLow:string=''; //dto
begin
  iLow:=1; //Vorgabe = alle Kanäle
  iHig:=iStk; //dto
  if length(sArt)>0 then //Kanäle gewählt
    if pos(':',sArt)>2 then //Periode angegeben
    begin
      sLow:=ExtractWord(1,sArt,['B',':']);
      sHig:=ExtractWord(2,sArt,['B',':']);
      if (TryStrToInt(sLow,iLow)=False)
      or (TryStrToInt(sHig,iHig)=False)
      or (iHig<iLow) or (iHig<1) or (iHig>iStk) then
        Tools.ErrorOut(3,cBnd+sArt);
    end
    else if TryStrToInt(copy(sArt,2,$F),iLow) then //nur ein Kanal angegeben?
      iHig:=iLow
    else Tools.ErrorOut(3,cBnd+sArt);
end;

{ rCt gibt Maximum, Minimum und Range einzelner Pixel aus "sImg" als 3-Kanal-
  Bild zurück }
{ rCt wurde von "Reduce.Execute" abgespalten um sehr große "sImg" Dateien zu
  prozessieren. rCt alloziert dazu zuerst das Ergebnis und verwendet es als
  Zwischenlager. Dann läd rCt sukzessive alle Kanäle aus "sImg" und bildet die
  Min/Max-Werte sukzessive mit dem Ergebnis als Zwischenlager. Am Ende ergänzt
  rCt den Range-Kanal. rCt prüft alle Pixel in "sImg" auf NoData. }

procedure tReduce.xComposit(
  sImg:string; //Name des Vorbilds
  sTrg:string); //Name Ergebnis ODER leer
const
  cFex = 'rCt: Image not found: ';
  cLui = 'maximum'#10'minimum'#10'range'; //AIR-Kanäle
var
  fxBnd:tn2Sgl=nil; //Kanäle im Vorbild
  fxRes:tn3Sgl=nil; //Ergebnis der Reduktion
  rHdr:trHdr; //Metadaten
  B,X,Y:integer;
begin
  if not FileExists(sImg) then Tools.ErrorOut(3,cFex+sImg);
  Header.Read(rHdr,sImg); //Metadaten Vorbild
  fxRes:=Tools.Init3Single(3,rHdr.Lin,rHdr.Scn,dWord(NaN)); //3-Kanal-Bild, NoData
  if sTrg='' then sTrg:=eeHme+cfLui;

  for B:=0 to pred(rHdr.Stk) do
  begin //Vorbild-Kanäle sukzessive prüfen
    fxBnd:=Image.ReadBand(B,rHdr,sImg); //aktueller Kanal
    for Y:=0 to pred(rHdr.Lin) do
      for X:=0 to pred(rHdr.Scn) do
        if IsNan(fxBnd[Y,X])=False then
          if isNan(fxRes[0,Y,X]) then
          begin //erster Eintrag
            fxRes[0,Y,X]:=fxBnd[Y,X];
            fxRes[1,Y,X]:=fxBnd[Y,X];
          end
          else
          begin //Minimum / Mximum suchen
            fxRes[0,Y,X]:=max(fxBnd[Y,X],fxRes[0,Y,X]);
            fxRes[1,Y,X]:=min(fxBnd[Y,X],fxRes[1,Y,X]);
          end;
  end;

  for Y:=0 to pred(rHdr.Lin) do
    for X:=0 to pred(rHdr.Scn) do
      if not IsNan(fxRes[0,Y,X]) then
        fxRes[2,Y,X]:=fxRes[0,Y,X]-fxRes[1,Y,X]; //absolute Differenz

  Image.WriteMulti(fxRes,sTrg); //drei Kanäle
  Header.WriteMulti(3,3,rHdr,cLui,sTrg); //Metadaten
  Tools.HintOut(true,'Composit:'+ExtractFileName(sTrg)); //Status
end;

{ mBI übersetzt die Zonen-Catchment-Liste "iaMrg" in einen neuen Catchment-
  Index (flächengleich mit "ixIdx") }

function tModel.BasinIndex(
  iaCtm:tnInt; //Catchment-ID für alle Zonen
  ixIdx:tn2Int): //Zonen-Index
  tn2Int; //neuer Index
var
  X,Y:integer;
begin
  Result:=Tools.Init2Integer(length(ixIdx),length(ixIdx[0]),0);
  for Y:=0 to high(ixIdx) do
    for X:=0 to high(ixIdx[0]) do
      Result[Y,X]:=iaCtm[ixIdx[Y,X]];
end;

{ mML überträgt Abfluss-Punkte aus "ixDrn" in die Zonen-Verknüpfung "iaLnk" und
  vereinigt bestehende Verknüpfngs-Bäume zu größeren. }
{ mML verknüpft zunächst die beiden Zonen, die in "ixDrn" durch Pixel-Indices
  markiert sind. Dabei geht eine bestehende Verknüpfung in das Zentrum des
  ursprünglichen Catchments verloren. mMl stellt die Verbindung wieder her,
  indem die ursprünglichen Verknüpfungen vom der der Catchment-Grenze bis zum
  Zentrum des Catchments invertiert werden. Dabei wird auch der Selbstbezug im
  früheren Catchment-Zentrum zu einer gewöhnlichen Verknüpfung. }
{ mML bestimmt zunächst die IDs der Zonen am Kontakt-Punkt der Catchments,
  verfolgt dann die Kette bis zum Selbstbezug und invertiert anschließend
  sukzessive alle Verknüpfungen von außen anch innen. }

function tModel.MergeLinks(
  iaLnk:tnInt; //Verknüpfung der Zonen
  ixDrn:tn2Int; //Catchment-Index analog zu "ixIdx"
  ixIdx:tn2Int): //Zonen-Index
  integer; //Zahl der neuen Brücken
var
  iFcs:integer; //Zonen-ID mit Rückbezug
  iSrc,iDrn:integer; //Zonen-IDs: alte Quelle, neuer Abfluss
  iTmp:integer; //zwischenlager
  iWdt:integer; //Bildbreite
  D:integer;
begin
  Result:=0;
  iWdt:=length(ixIdx[0]);
  for D:=1 to high(ixDrn[0]) do
    if (ixDrn[0,D]>=0) and (ixDrn[1,D]>=0) then //nicht iolierte Zonen
    begin
      iSrc:=ixIdx[ixDrn[0,D] div iWdt,ixDrn[0,D] mod iWdt]; //Abfluss in der Zone
      iDrn:=ixIdx[ixDrn[1,D] div iWdt,ixDrn[1,D] mod iWdt]; //Abfluss neben der Zone

      iFcs:=iSrc;
      repeat
        iFcs:=iaLnk[iFcs]
      until iaLnk[iFcs]=iFcs; //Sammelpunkt

      while iSrc<>iFcs do
      begin
        iTmp:=iaLnk[iSrc]; //alte Quelle
        iaLnk[iSrc]:=iDrn; //neues Ziel
        iDrn:=iSrc; //neues Ziel
        iSrc:=iTmp //neue Quelle
      end;
      iaLnk[iSrc]:=iDrn; //neues Ziel
      inc(Result)
    end
end;

{ mHD sucht systematisch nach Nachbarpixeln, die eine Grenze zwischen Flächen
  in "ixIdx" einschließen und registriert in "ixRes" die niedrigste Grenze für
  jede Fläche. Mit "iaLnk"<>nil berücksichtigt mHD nur Grenzen, für die in
  "iaLnk" ein Abfluss in der gefundenen Richtung eingetragen ist }
{ "faMin" und "ixRes" werden von "HorzDrain" und "VertDrain" gemeinsam
  verändert }

procedure tModel.HorzDrain(
  faMin:tnSgl; //niedrigstes Pixel-Paar am Rand
  fxElv:tn2Sgl; //Höhenmodell
  iaLnk:tnInt; //Zonen-Verknüpfungen
  ixCtm:tn2Int; //Zonen-Index
  ixRes:tn2Int); //Pixel-Indices am Abfluss, beide Catchments
var
  fLmt:single; //Höhe am Abfluss
  iWdt:integer; //Bildbreite
  pDrn:^tnInt; //Pixel-Indices Abfluss
  pSrc:^tnInt;
  X,Y:integer;
begin
  pSrc:=@ixRes[0]; //Pixel-Koordinaten
  pDrn:=@ixRes[1];
  iWdt:=length(ixCtm[0]); //Bildbreite
  for Y:=0 to high(ixCtm) do
    for X:=1 to high(ixCtm[0]) do
    begin //horizontale Grenzen
      if (isNan(fxElv[Y,pred(X)])) or (isNan(fxElv[Y,X])) then continue;
      if ixCtm[Y,pred(X)]<>ixCtm[Y,X] then //Grenze!
      begin
        fLmt:=max(fxElv[Y,pred(X)],fxElv[Y,X]); //Schwelle aus beiden Pixeln

        if ((iaLnk=nil) or (iaLnk[ixCtm[Y,pred(X)]]=ixCtm[Y,X]))
        and (fLmt<faMin[ixCtm[Y,pred(X)]]) then //Zone links
        begin
          pSrc^[ixCtm[Y,pred(X)]]:=Y*iWdt+pred(X); //Pixel-Indices
          pDrn^[ixCtm[Y,pred(X)]]:=Y*iWdt+X;
          faMin[ixCtm[Y,pred(X)]]:=fLmt; //neue Höhe
        end;

        if ((iaLnk=nil) or (iaLnk[ixCtm[Y,X]]=ixCtm[Y,pred(X)]))
        and (fLmt<faMin[ixCtm[Y,X]]) then //Zone rechts
        begin
          pSrc^[ixCtm[Y,X]]:=Y*iWdt+X; //Pixel-Indices
          pDrn^[ixCtm[Y,X]]:=Y*iWdt+pred(X);
          faMin[ixCtm[Y,X]]:=fLmt; //neue Höhe
        end;
      end;
    end;
end;

{ wie "HorzDrain" }

procedure tModel.VertDrain(
  faMin:tnSgl; //niedrigstes Pixel-Paar am Rand
  fxElv:tn2Sgl; //Höhenmodell
  iaLnk:tnInt; //Zonen-Verknüpfungen ODER leer
  ixCtm:tn2Int; //Zonen-Index
  ixRes:tn2Int); //Pixel-Indices am Abfluss, beide Catchments
var
  fLmt:single; //Höhe am Abfluss
  iWdt:integer; //Bildbreite
  pDrn:^tnInt; //Pixel-Koordinaten Abfluss
  pSrc:^tnInt;
  X,Y:integer;
begin
  pSrc:=@ixRes[0]; //Pixel-Koordinaten
  pDrn:=@ixRes[1];
  iWdt:=length(ixCtm[0]); //Bildbreite
  for Y:=1 to high(ixCtm) do
    for X:=0 to high(ixCtm[0]) do
    begin //horizontale Grenzen
      if (isNan(fxElv[pred(Y),X])) or (isNan(fxElv[Y,X])) then continue;
      if ixCtm[pred(Y),X]<>ixCtm[Y,X] then //Grenze!
      begin
        fLmt:=max(fxElv[pred(Y),X],fxElv[Y,X]); //Schwelle aus beiden Pixeln

        if ((iaLnk=nil) or (iaLnk[ixCtm[pred(Y),X]]=ixCtm[Y,X]))
        and (fLmt<faMin[ixCtm[pred(Y),X]]) then //Zone oben
        begin
          pSrc^[ixCtm[pred(Y),X]]:=pred(Y)*iWdt+X; //Pixel-Indices
          pDrn^[ixCtm[pred(Y),X]]:=Y*iWdt+X;
          faMin[ixCtm[pred(Y),X]]:=fLmt; //neue Höhe
        end;

        if ((iaLnk=nil) or (iaLnk[ixCtm[Y,X]]=ixCtm[pred(Y),X]))
        and (fLmt<faMin[ixCtm[Y,X]]) then //Zone unten
        begin
          pSrc^[ixCtm[Y,X]]:=Y*iWdt+X; //Pixel-Koordinaten
          pDrn^[ixCtm[Y,X]]:=pred(Y)*iWdt+X;
          faMin[ixCtm[Y,X]]:=fLmt; //Höhe Abfluss
        end;
      end;
    end;
end;

{ mBD prüft, ob der Abfluss (= niedrigster Punkt) eines Catchments am Bildrand
  liegt. Wenn ja, vergibt mBD für Quelle und Abfluss denselben Pixel. mDB prüft
  ausschließlich Pixel am Bildrand. }

procedure tModel.BorderDrain(
  faMin:tnSgl; //kleinste Höhe am Catchment-Rand
  fxElv:tn2Sgl; //Höhenmodell
  ixCtm:tn2Int; //Catchment-Index (zu Beginn Zonen)
  ixRes:tn2Int); //Pixel am Bildrand

procedure lSetResult(
  iHrz,iVrt:integer; //Pixel-Koordinaten
  iWdt:integer); //Bild-Breite
begin
  if ixCtm[iVrt,iHrz]=0 then exit; //nur definierte Zonen
  if isNan(fxElv[iVrt,iHrz]) then exit; //nur definierte Höhe
  if fxElv[iVrt,iHrz]<faMin[ixCtm[iVrt,iHrz]] then
  begin
    ixRes[0,ixCtm[iVrt,iHrz]]:=iVrt*iWdt+iHrz;
    ixRes[1,ixCtm[iVrt,iHrz]]:=iVrt*iWdt+iHrz; //gleicher Punkt
    faMin[ixCtm[iVrt,iHrz]]:=fxElv[iVrt,iHrz]; //Höhe Abfluss
  end
end;

var
  iWdt:integer; //Bildbreite
  X,Y:integer;
begin
  iWdt:=length(ixCtm[0]);
  for Y:=0 to high(ixCtm) do
  begin
    lSetResult(0,Y,iWdt);
    lSetResult(high(ixCtm),Y,iWdt);
  end;
  for X:=0 to high(ixCtm[0]) do
  begin
    lSetResult(X,0,iWdt);
    lSetResult(X,high(ixCtm[0]),iWdt);
  end;
end;

{ mDP bestimmt den niedrigsten Kontakt-Punkt zu einem Nachbar-Catchment für
  allen Flächen in "ixCtm". mDP gibt das Ergebnis als Pixelindex-Paare zurück.
  Die beiden Pixel markieren die Grenze zwischen den beiden Catchments. Für
  isolierte Catchments gibt mDP zweimal -1 zurück }
{ Mit "iaLnk"<>nil berücksichtigt mDP nur Grenzen, die in "iaLnk" eingetragen
  sind. Da "iaLnk" den Zonen-Index registriert (und nicht größere Catchments),
  MUSS in "ixIdx" der ursprüngliche Zonen-Index übergeben werden. Mit "iaLnk"
  gibt mDP Verknüpfungen einzelner Zonen zurück, sofern sie in "ialnk"
  eingetragen sind. }
{ mDP durchsucht systematisch den gesamten Index "ixIdx" nach Pixel-Paaren, die
  eine Grenze einschließen. Als "Höhe" verwendet mDP den höheren der beiden
  Pixel. mDP durchsucht horizontale und vertikale Grenzen in getrennten
  Routinen. }

function tModel.DrainPoints(
  bRls:boolean; //Abfluss am Bildrand ermöglichen
  fxElv:tn2Sgl; //Höhenmodell
  iaLnk:tnInt; //Zonen-verknüpfung ODER nil
  iCnt:integer; //Anzahl Catchments (Zonen)
  ixCtm:tn2Int): //Catchments, zu Beginn Zonen
  tn2Int; //Pixel-Indices von-nach
const
  cMax:single=MaxInt; //sehr hoch
var
  faMin:tnSgl=nil; //Niedrigser Pixel am Rand der Zone
begin
  Result:=Tools.Init2Integer(2,succ(iCnt),dWord(-1)); //Pixel-Indices am Abfluss
  faMin:=Tools.InitSingle(succ(iCnt),dWord(cMax)); //niedrigste Höhe am Rand der Zonen
  HorzDrain(faMin,fxElv,iaLnk,ixCtm,Result); //Abfluss-Punkte als Pixel-Indices ..
  VertDrain(faMin,fxElv,iaLnk,ixCtm,Result); // .. von-bis
  if bRls then BorderDrain(faMin,fxElv,ixCtm,Result); //Pixel am Bildrand
end;

{ mDL initialisiert die Zonen-Verküpfung "iaLnk". Als Vorgabe verknüpft mDL
  alle Zonen mit sich selbst. Wenn in "ixDrn" gültige Pixel eingetragen sind,
  übernimmt mDL die ID der Nachbar-Zone }

function tModel.DrainLinks(
  ixDrn:tn2Int; //Pixelindices der Abflüsse "von-nach"
  ixIdx:tn2Int): //Zonen-Index
  tnInt; //Verknüpfungen als Zonen-IDs
var
  iScn:integer; //Bildbreite
  D:integer;
begin
  Result:=Tools.InitIndex(length(ixDrn[0])); //Vorgabe = Selbstbezug
  iScn:=length(ixIdx[0]); //Bildbreite
  for D:=1 to high(ixDrn[0]) do //alle Einträge <?> Anzahl Zonen
    if (ixDrn[0,D]>=0) and (ixDrn[1,D]>=0) then //nur verknüpfte Catchments
      Result[ixIdx[ixDrn[0,D] div iScn,ixDrn[0,D] mod iScn]]:=
             ixIdx[ixDrn[1,D] div iScn,ixDrn[1,D] mod iScn]; //Abfluss oder Selbstbezug
end;

{ mBC vergibt eine fortlaufende Catchment-ID an alle Zonen und entfernt
  Schleifen aus "iaLnk" }
{ mBC startet mit einem leeren Ergebnis, beginnt bei einer beliebigen Zone und
  markiert alle gefundenen Zonen mit -1 bis ein bestehendes Catchment (>0) oder
  der eigene Pfad (-1) erreicht ist. Endet die Kette in der eigenen Spur (-1)
  erzeugt mBC ein neues Catchment und ändert den letzten Verweis in einen
  Selbstbezug. "iaLnk" darf keine Schleifen enthalten. Am Ende überträgt mBC
  die neue oder alte Catchment-ID auf die aktuelle Verweis-Kette. mBC vergibt
  an isolierte Zonen eine eigene Catchment-ID }

function tModel.BasinCount(
  iaLnk:tnInt; //Zonen-Verknüpfung (WIRD VERÄNDERT)
  var iHig:integer): //Anzahl Catchments
  tnInt; //Catchment-IDs der Zonen
var
  iCtm:integer=0; //aktuelles Catchment
  iIdx:integer=0; //akuelle Zonen-ID
  iRct:integer=0; //Zone mit Rückbezug
  Z:integer;
begin
  //length(iaLnk)=length(iaCid)?
  Result:=Tools.InitInteger(length(iaLnk),0); //Vorgabe = leer
  iHig:=0; //fortlaufend zählen
  for Z:=1 to high(iaLnk) do
    if Result[Z]=0 then //Catchment nicht eingetragen
      if iaLnk[Z]=Z then //isolierte Zone
      begin
        inc(iHig);
        Result[Z]:=iHig //Catchment aus einer Zone
      end
      else //verknüpfte Zone
      begin
        iIdx:=Z; //aktuelle Zone
        repeat
          Result[iIdx]:=-1; //Marke setzen
          iRct:=iIdx; //Vorläufer
          iIdx:=iaLnk[iIdx] //nächste Zone
        until Result[iIdx]<>0; //bestehendes Catchment oder eigene Spur

        if Result[iIdx]<0 then
        begin //eigene Spur gefunden
          inc(iHig); //neues Catchment
          iCtm:=iHig; //ID verwenden
          iaLnk[iRct]:=iRct //Ringschluss => Selbstbezug
        end
        else iCtm:=Result[iIdx]; //ID bestehendes Catchment

        iIdx:=Z; //von vorne beginnen
        repeat
          Result[iIdx]:=iCtm;
          iIdx:=iaLnk[iIdx]
        until Result[iIdx]=iCtm
      end;
end;

{ mRO bestimmt den Abfluss zwischen Zonen aus einem Höhenmodell und speichert
  ihn als Zonen-Attribute und als Linien-Vektoren }
{ mRO erzeugt einen Verknüpfungs-Baum "iaLnk" der jede Zone mit ihrem Vorfluter
  verknüpft. Dazu sucht mRO in "DrainPoints" für jede Zone den niedrigsten
  Punkt an der Grenze zu einer anderen Zone und registriert die Verknüpfungen
  als Zonen-IDs in "ixLnk" und als Pixel-Indices für beide Zonen in "ixDrn".
  "ixDrn" registriert so den Abfluss-Ort UND beide Zonen-IDs. Das Ergebnis sind
  Inseln aus verknüpften Zonen. Jede Insel endet in einer Zone, nicht abfließen
  kann und in "iaLnk" als Selbstbezug registriert wird. mRO verknüpft mit
  "iaMrg" die Zonen zu Catchments und erzeugt einem Catchment-Index "ixCtm" der
  analog zu "ixIdx" für jede Zonen eine fortlaufende Catchment-ID registriert }
{ mRO verknüpft die erste Gereration der Catchments mit derselben Methode
  sukzessive zu immer größeren Catchments. Dazu sucht mRO mit "DrainPoints"
  Abfluss-Punkte zwischen Catchments statt zwischen Zonen. Die neuen Punkte
  konkurrieren mit bestehenden Verknüpfungen in "iaLnk". "MergeLinks" kann
  deshalb die Richtung bestehender Verknüpfungen in "iaLnk" invertieren, so
  dass keine Verknüpfung verloren geht. Das ist möglich, weil am Ende jeder
  Verknüpfungs-Kette ein Selbstbezug steht, der aufgelöst wird. Hydrologisch
  gesehen wir ein lokales Catchment zu einer Quelle }
{ Am Ende aktualisiert mRO "ixDrn" so, dass jede Zonen-Verknüpfung in "iaLnk"
  mit einen passenden Abfluss-Punkt verknüpft wird, übersetzt die Punkte in
  Welt-Koordinaten und zeichnet Vektor-Linien mit dem kumulierten Abfluss als
  Parameter für alle Verknüpfungen in "iaLnk" }

procedure tModel.xRunOff(
  bRls:boolean; //Abfluss am Bildrand freigeben
  sElv:string); //Höhenmodell als Raster
const
  cFit='mRO: Seleced index and elevation data do not fit!';
var
  fxElv:tn2Sgl=nil; //Höhenmodell als Pixelbild
  faWgt:tnSgl=nil; //cmulierter Abfluss
  fMax:single=0; //cumulierter Abfluss
  iaLnk:tnInt=nil; //Zonen-Verknüpfung (IDs)
  iaMrg:tnInt=nil; //Catchment-IDs für Zonen
  iCtm:integer=0; //Anzahl Catchments
  iLin,iScn:integer; //Kontrolle
  iTmp:integer; //Zwischenlager für Anzahl Catchments
  ixCtm:tn2Int=nil; //Catchment-Index (analog Zonen-Index)
  ixDrn:tn2Int=nil; //Pixel-Indices am Abfluss von-nach
  ixIdx:tn2Int=nil; //Zonen-Index
  rHdr:trHdr; //Metadaten, gemeinsam
begin
  Header.Read(rHdr,sElv); //Metadaten Höhenmodell
  fxElv:=Image.ReadBand(0,rHdr,sElv); //Höhendaten als Raster
  iLin:=rHdr.Lin; iScn:=rHdr.Scn;
  Header.Read(rHdr,eeHme+cfIdx); //Metadaten Zonen
  if (rHdr.Lin<>iLin) or (rHdr.Scn<>iScn) then Tools.ErrorOut(3,cFit);
  ixIdx:=tn2Int(Image.ReadBand(0,rHdr,eeHme+cfIdx)); //Zonen Raster

  iCtm:=rHdr.Cnt; //Vorgabe = Anzahl Zonen
  ixCtm:=Tools.CopyGrid(ixIdx); //Catchment-Index, zu Beinn wie Zonen
  iaLnk:=Tools.InitIndex(succ(rHdr.Cnt)); //Vorgabe = Selbstbezug
  repeat
    iTmp:=iCtm;
    ixDrn:=DrainPoints(bRls,fxElv,nil,iCtm,ixCtm); //Abfluss-Punkte (Pixel-Indices)
    MergeLinks(iaLnk,ixDrn,ixIdx); //lokale Bäume verknüpfen
    iaMrg:=BasinCount(iaLnk,iCtm); //Catchment-IDs für Zonen, keine Schleifen in "iaLnk"
    write(#13+IntToStr(iCtm),#32); //Anzahl Catchments
    ixCtm:=BasinIndex(iaMrg,ixIdx); //Catchment-Index mit IDs aus "iaMrg"
  until iCtm=iTmp;

  ixDrn:=DrainPoints(bRls,fxElv,iaLnk,high(iaLnk),ixIdx); //Abfluss-Punkt für jede Zone
  faWgt:=Lines.DrainWeight(iaLnk,rHdr); //Cumulierter Abfluss als Zonen-Attribut
  fMax:=MaxValue(faWgt); //höchster Abfluss-Wert
  Tools.HintOut(true,'Maximum drained area [ha]: '+IntToStr(round(fMax)));
  Lines.RunoffPoints(faWgt,ixDrn[0],iaLnk,rHdr); //Abfluss-Punkte mit Linien verbinden
  Lines.PointChain(eeHme+cfLcy); //Linien-Graphik als WKT.
  Image.WriteBand(tn2Sgl(ixCtm),0,eeHme+cfCtm); //Catchments als Bild speichern
  Header.WriteIndex(iCtm,rHdr,eeHme+cfCtm); //Index-Header dazu WIRD VERÄNDERT
  Tools.HintOut(true,'Runoff Micro Catchments: '+IntToStr(iCtm));
end;

{ mZM verwendet dieselben Routinen wie "xPixelMap", klassifiziert aber kein
  Bild sondern die Attribut-Tabelle "sAtr". mZM gibt das Ergebnis als Klassen-
  Attribut "mapindex", als Bild "mapping" und als Klassen-Definition
  "mapping.bit" zurück. Die Zonen-Definition "index", "index.bit" und
  "topology.bit" muss im Arbeitsverzeichnis verfügbar sein. }
{ mZM wählt Stichproben aus dem Bild, große Zonen werden dabei absichtlich
  häufiger getroffen als kleine. mZM akzeptiert nur Zonen, in denen alle
  Attribute definiert sind. Für das Klassen-Bild kombiniert mZM das Raster-Bild
  der Zonen-IDs "index" mit dem Klassen-Attribut "iaThm". }

procedure tModel.xZonesMap(
  iMap:integer; //Anzahl Klassen
  iSmp:integer; //Stichproben
  sAtr:string; //Attribut-Tabelle
  sRes:string); //Ergebnis-Name
const
  cAtr = 'mZM: Properties "index.bit" not found in the working directory!';
  cCnt = 'mZM: Number of zones does not fit with zone proerties!';
  cIdx = 'mZM: Image zones "index" not found in he working directory!';
  cMap = 'mZM: Too many classes! Maximal 250 classes are defined';
  cSmp = 'mZM: At least 100 samples per class must be passed!';
var
  fxAtr:tn2Sgl=nil; //Zonen-Attribute
  fxMdl:tn2Sgl=nil; //Klassen-Definitionen
  iaThm:tnInt=nil; //Klassen-Attribut
  ixIdx:tn2Int=nil; //Zonen-IDs als Raster
  ixMap:tn2Byt=nil; //Klassen-IDs als Raster
  rHdr:trHdr; //Metadaten
  I:integer;
begin
  if iMap>250 then Tools.ErrorOut(3,cMap);
  if iSmp<iMap*100 then Tools.ErrorOut(3,cSmp);
  if not FileExists(eeHme+cfIdx) then Tools.ErrorOut(3,cIdx);
  if not FileExists(sAtr) then Tools.ErrorOut(3,cAtr);

  Header.Read(rHdr,eeHme+cfIdx); //Metadaten Zonen-IDs
  ixIdx:=tn2Int(Image.ReadBand(0,rHdr,eeHme+cfIdx)); //Zonen-IDs
  fxAtr:=Tools.BitRead(sAtr); //Attribut-Tabelle lesen
  fxMdl:=Tools.Init2Single(succ(iMap),succ(length(fxAtr)),0); //Klassen-Definitionen, Attribute+1, ID=0
  if rHdr.Cnt<>high(fxAtr[0]) then Tools.ErrorOut(3,cCnt); //Zonen + Attribute passen?
  RandSeed:=cfRds; //Zufalls-Generator initialisieren

  for I:=1 to iSmp do
  begin //ROLF-Neurone siehe "xPixelMap"
    SelectZone(fxAtr,fxMdl,ixIdx,rHdr); //zufällige Zone in fxMdl[0] eintragen
    if I>iMap*ccOvr then SampleFit(fxMdl) //Phase 3: Sample in ähnlichste Klasse integrieren
    else if I>iMap then SampleMerge(fxMdl) //Phase 2: Klassen vereinigen ODER Sample integrieren
    else SampleAdd(fxMdl,I); //Phase 1: Klassen-Array initialisieren
  end;

  Tools.BitWrite(fxMdl,sRes); //Klassen-Definitionen als Bit-Tabelle
  //Tools.HintOut(true,'Model: '+cfMdl);
  iaThm:=ClassifyZones(fxAtr,fxMdl); //Klassen-Attribut
  Tools.BitInsert(tnSgl(iaThm),-1,eeHme+cfMix); //Klassen-Attribut als Bit-Array
  ixMap:=Build.ThemaImage(iaThm); //Klassen-Rasterbild
  Image.WriteThema(ixMap,sRes); //Klassen-Bild speichern
  Header.WriteThema(iMap,rHdr,'',sRes); //Metadaten speichern
  Tools.HintOut(true,'ZonesMap: '+ExtractFileName(sRes))
end;

{ mFM klassifiziert Muster aus klassifizierten Zonen. }
{ mFM erzeugt eine Tabelle "fxDns" mit der Anzahl der Kontake aller Klassen
  untereinander. Innere Kontakte gehören dazu. Mit "iGen>1" diffundieren die
  Werte in "iGen" Zyklen in die Nachbarzonen. Am Ende klassifiziert mFM die
  Tabelle analog zur Klassifikation von Zonen mit der Attribut-Tabelle. }

procedure tModel.xFabricMap(
  iFbr:integer; //Anzahl Fabric-Klassen
  iGen:integer; //Iterationen für Kontext
  iSmp:integer); //Stichproben
const
  cThm = 'mFM: Classification result "mapping.bit" not found!';
var
  fxDns:tn2Sgl=nil; //Attribt "Klassen-Dichte"
  iaThm:tnInt=nil; //Klasen-Attribut
  iCnt:integer=0; //Anzahl Kontakte pro Zone
  iMap:integer; //Anzahl Attribut-Klassen
  ixTpl:tn2Int=nil; //Index-Topologie
  paDim:^tnInt=nil; //Topoligie-Index
  paNbr:^tnInt=nil; //Nachbar-Zonen-ID
  paPrm:^tnInt=nil; //Kontakte zur Nachbar-Zone
  D,N,Z:integer;
begin
  if not FileExists(eeHme+cfMap+cfBit) then Tools.ErrorOut(3,cThm);
  ixTpl:=tn2Int(Tools.BitRead(eeHme+cfTpl)); //Topologie
  paDim:=@ixTpl[0]; //Zeiger auf Startadressen
  paNbr:=@ixTpl[1]; //Zeiger auf Nachbarzonen-IDs
  paPrm:=@ixTpl[2]; //Anzahl der Kontakte
  iaThm:=tnInt(Tools.BitExtract(0,eeHme+cfMix)); //Klassen-Attribut
  iMap:=MaxIntValue(iaThm); //höchste Klassen-ID aus "mapping"
  fxDns:=Tools.Init2Single(succ(iMap),length(iaThm),0); //leere Vorgabe

  for Z:=1 to high(ixTpl[0]) do //alle Zonen
  begin //Kontakte pro Klasse zählen + normalisieren
    iCnt:=0;
    for N:=paDim^[pred(Z)] to pred(paDim^[Z]) do //alle Kontakte, auch innere!
    begin
      fxDns[iaThm[paNbr^[N]],Z]+=paPrm^[N]; //Kontakte zur Klasse der Nachbar-Zone
{ ToDo: Interne Kontakte könnten zu stark dominieren. Wurzel statt Anzahl ?}
      iCnt+=paPrm^[N]; //Anzahl aller Kontakte
    end;
    if iCnt>0 then
      for D:=1 to iMap do //alle Klassen
        fxDns[D,Z]/=iCnt; //Kontakte normalisieren
  end;

  if iGen>1 then //Attribute auf Nachbarn ausweiten
    for D:=1 to iMap do //alle Klassen
      Build.FeatureDrain(fxDns[D],iGen,ixTpl); //Attribut ausgleichen

  Tools.BitWrite(fxDns,eeHme+cfCtx); //Zonen-Attribute "Kontakte"
  xZonesMap(iFbr,iSmp,eeHme+cfCtx,eeHme+cfFbr); //Kontakt-Attribute klassifizieren
end;

end.

{==============================================================================}

{ mFM klassifiziert Muster aus klassifizierten Zonen. }
{ mFM erzeugt eine Tabelle "fxDns" mit der Anzahl der Kontake aller Klassen
  untereinander. Innere Kontakte gehören dazu. Mit "iGen>1" diffundieren die
  Werte in "iGen" Zyklen in die Nachbarzonen. Am Ende klassifiziert mFM die
  Tabelle analog zur Klassifikation von Zonen mit der Attribut-Tabelle. }

procedure tModel.xFabricMap_(
  iFbr:integer; //Anzahl Fabric-Klassen
  iGen:integer; //Iterationen für Kontext
  iSmp:integer); //Stichproben
const
  cThm = 'mFM: Classification result "mapping.bit" not found!';
var
  fxDns:tn2Sgl=nil; //Attribt "Klassen-Dichte"
  iaThm:tnInt=nil; //Klasen-Attribut
  iMap:integer; //Anzahl Attribut-Klassen
  ixTpl:tn2Int=nil; //Index-Topologie
  D,Z:integer;
begin
  if not FileExists(eeHme+cfMap+cfBit) then Tools.ErrorOut(3,cThm);
  iaThm:=tnInt(Tools.BitExtract(0,eeHme+cfMix)); //Klassen-Attribut
  iMap:=MaxIntValue(iaThm); //höchste Klassen-ID aus "mapping"
  fxDns:=Tools.Init2Single(succ(iMap),length(iaThm),0); //leere Vorgabe
//------------------------------------------------------------------------------
  for Z:=1 to high(iaThm) do
    fxDns[iaThm[Z],Z]:=1.0; //Dichte = Eins für richtige Klasse
//------------------------------------------------------------------------------
  if iGen>1 then
  begin
    ixTpl:=tn2Int(Tools.BitRead(eeHme+cfTpl)); //Topologie
    for D:=0 to high(fxDns) do
      Build.FeatureDrain(fxDns[D],iGen,ixTpl); //Attribut ausgleichen
  end;
  Tools.BitWrite(fxDns,eeHme+cfCtx); //Zonen-Attribute "Kontakte"
  _xZonesMap(iFbr,iSmp,eeHme+cfCtx,eeHme+cfFbr); //Attribute klassifizieren
end;

{ mZM verwendet dieselben Routinen wie "xPixelMap", klassifiziert aber kein
  Bild sondern die Attribut-Tabelle "index.bit". mZM gibt das Ergebnis als
  Klassen-Attribut "iaThm" und als Bild "mapping" zurück. }
{ Zonen-IDs als Raster "index", Zonen-Attribute "index.bit" und Zonen-Topologie
  "topology.bit" müssen existieren. mZM wählt Stichproben aus dem Bild, große
  Zonen werden dabei absichtlich häufiger getroffen als kleine. mZM akzeptiert
  nur Zonen, in denen alle Attribute dfiniert sind. Für das Klassen-Bild
  kombiniert mZM das Raster-Bild der Zonen-IDs "index" mit dem Klassen-Attribut
  "iaThm". }

procedure tModel.x_ZonesMap_(
  iMap:integer; //Anzahl Klassen
  iSmp:integer; //Stichproben
  sAtr:string); //Attribut-Tabelle
const
  cAtr = 'mZM: Properties "index.bit" not found in the working directory!';
  cCnt = 'mZM: Number of zones does not fit with zone proerties!';
  cIdx = 'mZM: Image zones "index" not found in he working directory!';
  cMap = 'mZM: Too many classes! Maximal 250 classes are defined';
  cSmp = 'mZM: At least 100 samples per class must be passed!';
var
  fxAtr:tn2Sgl=nil; //Zonen-Attribute
  fxMdl:tn2Sgl=nil; //Klassen-Definitionen
  iaThm:tnInt=nil; //Klassen-Attribut
  ixIdx:tn2Int=nil; //Zonen-IDs als Raster
  ixMap:tn2Byt=nil; //Klassen-IDs als Raster
  rHdr:trHdr; //Metadaten
  I:integer;
begin
  if iMap>250 then Tools.ErrorOut(3,cMap);
  if iSmp<iMap*100 then Tools.ErrorOut(3,cSmp);
  if not FileExists(eeHme+cfIdx) then Tools.ErrorOut(3,cIdx);
  if not FileExists(sAtr) then Tools.ErrorOut(3,cAtr);

  Header.Read(rHdr,eeHme+cfIdx); //Metadaten Zonen-IDs
  ixIdx:=tn2Int(Image.ReadBand(0,rHdr,eeHme+cfIdx)); //Zonen-IDs
  fxAtr:=Tools.BitRead(sAtr); //Attribut-Tabelle lesen
  fxMdl:=Tools.Init2Single(succ(iMap),succ(length(fxAtr)),0); //Klassen-Definitionen, Attribute+1, ID=0
  if rHdr.Cnt<>high(fxAtr[0]) then Tools.ErrorOut(3,cCnt); //Zonen + Attribute passen?
  RandSeed:=cfRds; //Zufalls-Generator initialisieren

  for I:=1 to iSmp do
  begin //ROLF-Neurone
    SelectZone(fxAtr,fxMdl,ixIdx,rHdr); //zufällige Zone in fxMdl[0] eintragen
    if I>iMap*ccOvr then SampleFit(fxMdl) //Phase 3: Sample in ähnlichste Klasse integrieren
    else if I>iMap then SampleMerge(fxMdl) //Phase 2: Klassen vereinigen ODER Sample integrieren
    else SampleAdd(fxMdl,I); //Phase 1: Klassen-Array initialisieren
  end;

  Tools.BitWrite(fxMdl,eeHme+cfMdl); //Klassen-Definitionen als Bit-Tabelle
  Tools.HintOut(true,'Model: '+cfMdl);
  iaThm:=ClassifyZones(fxAtr,fxMdl); //Klassen-Attribut
  ixMap:=Build.ThemaImage(iaThm); //Klassen-Rasterbild
  Image.WriteThema(ixMap,eeHme+cfMap); //Bild als "mapping" speichern
  Header.WriteThema(iMap,rHdr,'',eeHme+cfMap); //Metadaten speichern
  Tools.HintOut(true,'ZonesMap: '+cfMap)
end;

