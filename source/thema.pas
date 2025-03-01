unit thema;

{ THEMA sammelt Routinen zur Clusterung und Klassifikation von Bilddaten. Dabei
  clustert "Model" Bildmerkmale anhand einzelner Pixel und "Fabric" verwendet
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

  tLimits = class(tObject)
    private
      function SieveZones(iaFix:tnInt; iMin:integer; ixTpl:tn2Int):integer;
      function _MaskLimit_(fMin:single; fxBnd:tn2Sgl):tn2Byt;
      procedure MergeZones(ixTpl:tn2Int; iaFix:tnInt);
      function RecodeIndex(iaFix:tnInt; ixIdx:tn2Int):integer;
    public
      procedure xSieveZones(iMin:integer); //Minimum innere Kontakte
  end;

  tModel = class(tObject) //checked 250110
    private
      function ClassifyPixels(fxMdl:tn2Sgl; sImg:string):tn2Byt;
      function ClassifyZones(fxAtr:tn2Sgl; fxMdl:tn2Sgl):tnInt;
      function CountDiff(iaBck,iaThm:tnInt):integer;
      procedure ModelAdjust(fxMdl,fxSmp:tn2Sgl; iaThm:tnInt);
      function ModelInit(fxSmp:tn2Sgl; iExt,iMap:integer):tn2Sgl;
      function _Periods(fMax:single; faVal:tnSgl; iPrd:integer):single;
      procedure SampleAdd(fxMdl:tn2Sgl; iPst:integer);
      procedure SampleFit(fxMdl:tn2Sgl);
      procedure SampleMerge(fxMdl:tn2Sgl);
      procedure SelectPixel(fxMdl:tn2Sgl; fxImg:tn3Sgl; var rHdr:trHdr);
      procedure SelectZone(fxAtr,fxMdl:tn2Sgl; ixIdx:tn2Int; var rHdr:trHdr);
    public
      procedure ClassValues(iRed,iGrn,iBlu:integer);
      function FeatureDist:tn2Sgl;
      procedure _xFabricMap(iGen,iSmp:integer);
      procedure _xNoChange(fMax:single; sStk:string);
      procedure xPixelMap(iMap,iSmp:integer; sImg:string);
      procedure xZonesMap(iMap,iSmp:integer; sAtr:string);
  end;

  tReduce = class(tObject) //checked 241116
    private
      function BandCalc(fxImg:tn3Sgl; sPrc:string):tn2Sgl;
      function BestOf(bSve:boolean; fxImg:tn3Sgl):tn2Sgl;
      function _CoVariance_(fxImg:tn3Sgl; iaTms:tnInt):tn2Sgl;
      function Execute(fxImg:tn3Sgl; sArt,sCmd:string; var rHdr:trHdr):tn2Sgl;
      function GlobalSum(fxImg:tn3Sgl):tn2Sgl;
      function ImageDate(sImg:string):integer;
      function _LeafArea(fxImg:tn3Sgl; iNir,iRed:integer):tn2Sgl;
      function _LeafAreaIndex_(fxImg:tn3Sgl; iNir,iRed:integer):tn2Sgl;
      function Maximum(fxStk:tn3Sgl):tn2Sgl;
      function MeanValue(fxImg:tn3Sgl):tn2Sgl;
      function Median(fxImg:tn3Sgl):tn2Sgl;
      function Minimum(fxStk:tn3Sgl):tn2Sgl;
      function NewBand(fxImg:tn3Sgl; sVal:string):tn2Sgl;
      function Overlay(fxImg:tn3Sgl):tn2Sgl;
      function Range(fxStk:tn3Sgl):tn2Sgl;
      function _Regression(fxImg:tn3Sgl):tn2Sgl;
      procedure ScaleValues(fxLft,fxRgt:tn2Sgl; iTyp:integer);
      function _SentinelQuality_(rFrm:trFrm; sImg,sMsk:string):tn2Byt;
      function Variance(fxImg:tn3Sgl):tn2Sgl; //Vorbild: Varianz
      function Vegetation(fxImg:tn3Sgl; iNir,iRed,iTyp:integer):tn2Sgl;
    public
      function Brightness(fxImg:tn3Sgl):tn2Sgl;
      procedure GetBands(var iHig,iLow:integer; iStk:integer; sArt:string);
      procedure IndexSort(iaIdx:tnInt; faVal:tnSgl);
      procedure QualityImage(sImg,sTrg:string);
      procedure QuickSort(faDev:tnSgl; iDim:integer);
      function SortDate(slImg:tStringList):integer;
      procedure xHistory(sArt,sCmd,sImg,sTrg:string);
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

// für tStringList.CustomSort, Datum "YYYYMMDD" am Ende des Namens

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

function tModel.FeatureDist:tn2Sgl; //Distanzen-Matrix
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

function tLimits._MaskLimit_(
  fMin:single; //Schwelle = kleinster zulässiger Wert
  fxBnd:tn2Sgl): //Vorbild
  tn2Byt; //Maske [0,1]
{ lML erzeugt mit der Schwelle "fMin" eine Maske ([0,1]-Kanal) aus einem
  scalaren Bild. }
var
  X,Y:integer;
begin
  Result:=Tools.Init2Byte(length(fxBnd),length(fxBnd[0])); //Maske
  for Y:=0 to high(fxBnd) do
    for X:=0 to high(fxBnd[0]) do
      if not isNan(fxBnd[Y,X]) then
        Result[Y,X]:=byte(fxBnd[Y,X]>=fMin); //Schwelle anwenden
end;

function tReduce._SentinelQuality_(
  rFrm:trFrm; //Rahmen oder Vorgabe
  sImg:string; //Vorbild
  sMsk:string): //Sentinel-2 Maske im xml-Format
  tn2Byt;
begin
  Result:=nil; //leeren
  Gdal.Import(1,1,1,rFrm,sImg,''); //Import ohne Veränderung, Beschnitt
  Gdal.Rasterize(0,'OPAQUE',sImg,sMsk); //Polygone mit Null einbrennen
  Result:=Image.SkipMask(eeHme+cfMsk); //Maske aus Bild, Null für alle gültigen Werte
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
  if length(fxImg)<2 then Tools.ErrorOut(2,cDim);
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
  fSed: double=0; //aktuelles Ergebnis
  B,X,Y: integer;
begin
  if (length(fxImg)<2) or (length(fxImg[0,0])<1) then
    Tools.ErrorOut(2,cDim);
  Result:=Tools.Init2Single(length(fxImg[0]),length(fxImg[0,0]),dWord(NaN)); //Vorgabe = NoData
  for Y:=0 to high(fxImg[0]) do
    for X:=0 to high(fxImg[0,0]) do
    begin
      fSed:=0; //Vorgabe
      for B:=0 to high(fxImg) do //alle Kanäle
        if not IsNan(fxImg[B,Y,X]) then
          fSed+=sqr(fxImg[B,Y,X]);
      Result[Y,X]:=sqrt(fSed); //Länge im n-Raum
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

{ rQS sortiert das Array "faDev" aufsteigend. Dazu vertauscht rQS Werte, bis
  alle Vergliche passen. rQS verwendet zu Beginn große Abstände zwischen den
  Positionen im Array und reduziert sie schrittweise. }

procedure tReduce.QuickSort(
  faDev:tnSgl; //unsortiertes Array
  iDim:integer); //gültige Stellen im Array
var
  fTmp:single; //Zwischenlager
  iFix:integer; //Erfolge
  iStp:integer; //Distanz zwischen Positionen
  B:integer;
begin
  if iDim<2 then exit; //nichts zu sortieren
  iStp:=round(iDim/2); //erster Vergleich = halbmaximale Distanz
  repeat
    iFix:=0; //Vorgabe
    for B:=iStp to pred(iDim) do
      if faDev[B]<faDev[B-iStp] then //große Werte nach vorne
      begin
        fTmp:=faDev[B-iStp];
        faDev[B-iStp]:=faDev[B];
        faDev[B]:=fTmp;
      end
      else inc(iFix);
      if iStp>1 then iStp:=round(iStp/2) //Distanz halbieren
  until iFix=pred(iDim); //alle Vergleiche richtig
end;

{ rMn bildet den Median aus allen übergebenen Kanälen. Dazu kopiert rMn alle
  Werte eines Pixels nach "fxDev", sortiert "fxDev" mit "QuickSort" und
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
  if length(fxImg)<3 then Tools.ErrorOut(2,cDim);
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
        QuickSort(faDev,iDim); //ordnen
        //Result[Y,X]:=faDev[trunc(iDim/2)] //median
        Result[Y,X]:=faDev[round(iDim/2)] //median
      end;
    end;
    if Y and $FF=0 then write('.');
  end;
  write(#13)
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

{ rV gibt den Near Infrared Vegetation Index als Kanal zurück. "iRed" und
  "iNir" müssen die Wellenländen von Rot und nahem Infrarot bezeichnen. }

function tReduce.Vegetation(
  fxImg:tn3Sgl; //Vorbild
  iNir,iRed:integer; //Kanal-Indices (ab Null)
  iTyp:integer): //Index-ID
  tn2Sgl; //Vorbild: Vegetationsindex
const
  cDim = 'rVn: Vegetation index calculation needs two bands!';
  cTyp = 'rVn: Undefined ID for vegetation index"';
var
  X,Y: integer;
begin
  if length(fxImg)<2 then Tools.ErrorOut(2,cDim);
  if (iTyp<0) or (iTyp>2) then Tools.ErrorOut(2,cTyp);
  Result:=Tools.Init2Single(length(fxImg[0]),length(fxImg[0,0]),dWord(NaN)); //Vorgabe = NoData
  for Y:=0 to high(fxImg[0]) do
    for X:=0 to high(fxImg[0,0]) do
    begin
      if IsNan(fxImg[iNir,Y,X])
      or IsNan(fxImg[iRed,Y,X])
      or (fxImg[iNir,Y,X]+fxImg[iRed,Y,X]=0) then continue;
      case iTyp of
        0: Result[Y,X]:=(fxImg[iNir,Y,X]-fxImg[iRed,Y,X])/(fxImg[iNir,Y,X]+
           fxImg[iRed,Y,X])*fxImg[iNir,Y,X]; //NIRv Vegetattionsindex
        1: Result[Y,X]:=(fxImg[iNir,Y,X]-fxImg[iRed,Y,X])/(fxImg[iNir,Y,X]+
           fxImg[iRed,Y,X]); //NDVI Vegetationsindex
        2: Result[Y,X]:=2.5*(fxImg[iNir,Y,X]-fxImg[iRed,Y,X])/
           (fxImg[iNir,Y,X]+2.4*fxImg[iRed,Y,X]+1.0); //EVI Vegetationsindex
      end;
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
  auch lückige Bilder verarbeitet werden können. }
{ Varianz = (∑x²-(∑x)²/n)/(n-1) }

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

{ rMi bestimmt den kleinsten Wert in einem Stapel. rMx ignoriert NoData }

function tReduce.Maximum(
  fxStk:tn3Sgl): //gleicher Kanal aus allen Bildern
  tn2Sgl; //Maximum
const
  cMin:single=1-MaxSingle; //sehr kleiner definierter Wert
var
  B,X,Y:integer;
begin
  Result:=Tools.Init2Single(length(fxStk[0]),length(fxStk[0,0]),dWord(cMin)); //Vorgabe
  for Y:=0 to high(Result) do
    for X:=0 to high(Result[0]) do
    begin
      for B:=0 to high(fxStk) do
        if not isNan(fxStk[B,Y,X]) then
          Result[Y,X]:=max(fxStk[B,Y,X],Result[Y,X]); //Minimum
      if Result[Y,X]=cMin then
        Result[Y,X]:=NaN;
    end;
end;

{ rMi bestimmt den kleinsten Wert in einem Stapel. rMn ignoriert NoData }

function tReduce.Minimum(
  fxStk:tn3Sgl): //gleicher Kanal aus allen Bildern
  tn2Sgl; //Minimum
const
  cMax:single=MaxSingle; //sehr großer definierter Wert
var
  B,X,Y:integer;
begin
  Result:=Tools.Init2Single(length(fxStk[0]),length(fxStk[0,0]),dWord(cMax)); //Vorgabe
  for Y:=0 to high(Result) do
    for X:=0 to high(Result[0]) do
    begin
      for B:=0 to high(fxStk) do
        if not isNan(fxStk[B,Y,X]) then
          Result[Y,X]:=min(fxStk[B,Y,X],Result[Y,X]); //Minimum
      if Result[Y,X]=cMax then
        Result[Y,X]:=NaN;
    end;
end;

{ rMi bestimmt den Wertebereich in einem Stapel. rMx ignoriert NoData }

function tReduce.Range(
  fxStk:tn3Sgl): //gleicher Kanal aus allen Bildern
  tn2Sgl; //Maximum
const
  cMax:single = MaxSingle;
  cMin:single = 1-MaxSingle;
var
  fMax,fMin:single; //Zwischenlager
  B,X,Y:integer;
begin
  Result:=Tools.Init2Single(length(fxStk[0]),length(fxStk[0,0]),dWord(NaN)); //Vorgabe
  for Y:=0 to high(Result) do
    for X:=0 to high(Result[0]) do
    begin
      fMax:=cMin; fMin:=cMax; //Vorgabe
      for B:=0 to high(fxStk) do
        if not isNan(fxStk[B,Y,X]) then
        begin
          fMax:=max(fxStk[B,Y,X],fMax);
          fMin:=min(fxStk[B,Y,X],fMin);
        end;
      if (fMax>cMin) and (fMin<cMax) then
        Result[Y,X]:=abs(fMax-fMin); //Wertebereich
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
    else Tools.ErrorOut(2,cBnd+sVal);
  end
  else if TryStrToFloat(sVal,fVal) then //konstanter Wert ohne "C" am Anfang
    Result:=Tools.Init2Single(length(fxImg[0]),length(fxImg[0,0]),dWord(fVal))
  else Tools.ErrorOut(2,cVal+sVal);
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
    if Y and $FFF=$FFF then write(#13+intToStr((length(fxLft)-Y) div $1000));
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
        Tools.ErrorOut(2,cTyp+sPrc);
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

{ rBO übernimmt den "besten" Pixel aus einem beliebigen Stack. rBO kopiert alle
  definierten Werte eines Pixels nach "faDev". Sind mehr als 2 Werte definiert,
  sortiert rBO die Werte und übernimmt den Wert in der Mitte (Median). Bei zwei
  Werten bildet rBO den Mittelwert, wenn beide Bilder sehr wenig Fehler haben,
  andernfalls das "bessere" Bild. Ein Kanal wird unverändert übernommen. rBO
  füllt leere Bereiche mit NoData. }

function tReduce.BestOf(
  bSve:boolean; //nur sichere Pixel zurückgeben
  fxImg:tn3Sgl): //Vorbild
  tn2Sgl; //Median
const
  cEql = 0.989; //bis 1% Fehler als "optimal" betrachten
var
  faDev:tnSgl=nil; //ein Pixel aus allen Kanälen
  iDim:integer; //Anzahl Kanäle
  B,X,Y: integer;
begin
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
      if iDim=0 then continue; //leerer Pixel

      if iDim>2 then //mindestens drei gültige Layer
      begin
        QuickSort(faDev,iDim); //ordnen
        Result[Y,X]:=faDev[round(iDim/2)] //median
      end
      else if bSve=False then //alle Pixel verwenden
        if iDim>1 //zwei gültige Layer
          then Result[Y,X]:=(faDev[0]+faDev[1])/2 //Mittelwert
          else Result[Y,X]:=faDev[0] //verbleibenden Pixel verwenden
    end;
  end;
end;

{ iGB gibt die Ziffern in einer Formel "BX:BY" zurück, wobei X,Y für natürliche
  Zahlen stehen, die durch einen Doppelpunkt getrennt sind. Mit "sArt"='' gibt
  iGB das Intervall 1.."iStk" zurück. "BX" selektiert einen Kanak }

procedure tReduce.GetBands(
  var iHig,iLow:integer; //Kanal-Nummern als Rückgabe
  iStk:integer; //Anzahl Kanäle als Vorgabe
  sArt:string); //Kanal-IDs (Arithmetik) z.B. B2:B4 für Kanal 2 bis 4
const
  cBnd = 'rNR: Band indicator missing or not a number: ';
  cPsd = 'rNR: Band indicators not passed or misspelled: ';
var
  iPst:integer; //delimiter position
begin
  iLow:=1; //Vorgabe = alle Kanäle
  iHig:=iStk; //dto
  if length(sArt)<1 then exit;

  iPst:=pos(':',sArt);
  if iPst>2 then //Intervall angegeben
  begin
    if (TryStrToInt(copy(sArt,2,iPst-2),iLow)=False)
    or (TryStrToInt(copy(sArt,iPst+2,$FF),iHig)=False)
    or (iHig<iLow) or (iHig<1) then Tools.ErrorOut(2,cBnd+sArt);
  end
  else if TryStrToInt(copy(sArt,2,$F),iLow) then //nur ein Kanal angegeben?
    iHig:=iLow
  else Tools.ErrorOut(3,cPsd+sArt);
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
  bSve:boolean=false; //alle Pixel für Median verwenden
  iLow,iHig:integer;
begin
  if Length(fxImg)<1 then Tools.ErrorOut(3,cStk+sCmd);
  if (sCmd=cfLai) or (sCmd=cfNiv) or (sCmd=cfNvi) or (sCmd=cfEvi) then
     Reduce.GetBands(iHig,iLow,rHdr.Stk,sArt) //Kanal-Nummern, Zahlen ab Eins
  else if sCmd=cfBst then
    bSve:=sArt='full'; //Median aus mindestens drei Pixeln bilden
  if sCmd=cfBrt then Result:=Brightness(fxImg) else //Hauptkomponente
  if sCmd=cfBst then Result:=BestOf(bSve,fxImg) else //Median-Mean-Defined
  if sCmd=cfClc then Result:=BandCalc(fxImg,sArt) else //Kanal-Arithmetik
  if sCmd=cfLai then Result:=_LeafArea(fxImg,pred(iHig),pred(iLow)) else //LAI-Näherung
  if sCmd=cfMax then Result:=Maximum(fxImg) else //Maximum
  if sCmd=cfMdn then Result:=Median(fxImg) else //Median
  if sCmd=cfMea then Result:=MeanValue(fxImg) else //Mittelwert
  if sCmd=cfMin then Result:=Minimum(fxImg) else //Minimum
  if sCmd=cfOvl then Result:=Overlay(fxImg) else //Überlagerung
  if sCmd=cfRgs then Result:=_Regression(fxImg) else //Regression
  if sCmd=cfRng then Result:=Range(fxImg) else //Wertebereich
  if sCmd=cfNiv then Result:=Vegetation(fxImg,pred(iHig),pred(iLow),0) else //NirV Index
  if sCmd=cfNvi then Result:=Vegetation(fxImg,pred(iHig),pred(iLow),1) else //NDVI Index
  if sCmd=cfEvi then Result:=Vegetation(fxImg,pred(iNir),pred(iRed),2) else //EVI Index
  if sCmd=cfVrc then Result:=Variance(fxImg) else //Varianz
  if sCmd=cfWgt then Result:=GlobalSum(fxImg) else //Summe aller Kanäle
    Tools.ErrorOut(2,cCmd+sCmd);
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
  sBnd:string=''; //Kanal-Namen, durch LF getrennt
  B,I:integer;
begin
  if not FileExists(sImg) then Tools.ErrorOut(2,cFex+sImg);
  Header.Read(rHdr,sImg);
  if rHdr.Prd<1 then Tools.ErrorOut(2,cPrd+sImg);
  if trim(sTrg)='' then sTrg:=eeHme+sCmd; //Vorgabe = Prozess-Name
  if rHdr.Prd<rHdr.Stk then //nur wenn mehr als ein Bild
  begin
    SetLength(fxStk,rHdr.Stk div rHdr.Prd,1,1); //Dummy, Ein Kanal für jedes Bild
    Header.CopyRecord(rHdr,rRdh); //Kopie (ohne Palette)
    rRdh.Stk:=rHdr.Stk div rHdr.Prd; //ein kanal pro Vorbild
    for B:=0 to pred(rHdr.Prd) do //alle Ergebnis-Kanäle
    begin
      for I:=0 to pred(rHdr.Stk div rHdr.Prd) do //alle Vorbilder
        fxStk[I]:=Image.ReadBand(I*rHdr.Prd+B,rHdr,sImg); //Kanal "B" aus Bild "I" laden
      fxRes:=Execute(fxStk,sArt,sCmd,rRdh); //multiplen Kanal reduzieren
      Image.WriteBand(fxRes,B,sTrg); //Kanal schreiben
      sBnd+=ExtractWord(succ(B),rHdr.aBnd,[#10])+#10; //Kanal-Namen aus erstem Bild
      write(#13'Band '+IntToStr(succ(B))+' Image '+IntToStr(succ(I)));
    end;
    Header.WriteMulti(rHdr,sBnd,sTrg); //Kanal-Namen
  end
  else Tools.CopyEnvi(sImg,sTrg); //unverändert verwenden
  write(#13); //Zeilenanfang
  Tools.HintOut(true,'Reduce.Splice: '+sCmd);
end;

{ mCD zählt die Unterschiede zwischen den Arrays "iaBck" und "iaThm" }

function tModel.CountDiff(iaBck,iaThm:tnInt):integer;
var
  I:integer;
begin
  Result:=0;
  for I:=0 to high(iaBck) do
    if iaBck[I]<>iaThm[I] then inc(Result);
end;

{ nMA bestimmt neue Werte für die Klassen-Definitionen in "fxMdl". Dazu muss in
  "iaThm" eine Klassifikation von "fxSmp" übergeben werden. nMA summiert die
  Merkmale aller klassifizierten Proben aus "fxSmp" und gewichtet die Werte mit
  der Fläche der Proben. Die neuen Werte von "fxMdl" sind das gewichtete Mittel
  aller klassifizierten Proben. }

procedure tModel.ModelAdjust(
  fxMdl:tn2Sgl; //Klassen-Modell
  fxSmp:tn2Sgl; //Stichproben
  iaThm:tnInt); //Klassen-Attribut zu "fxSmp"
var
  pM:^tnSgl;
  F,M,S:integer;
begin
  for M:=0 to high(fxMdl) do
    FillDWord(fxMdl[M,0],length(fxMdl[0]),0); //leeren
  for S:=1 to high(fxSmp) do
  begin
    pM:=@fxMdl[iaThm[S]]; //aktuelle Klasse
    for F:=1 to high(fxMdl[0]) do
      pM^[F]+=fxSmp[S,F]*fxSmp[S,0]; //Merkmal*Fläche summieren
    pM^[0]+=fxSmp[S,0]; //Fläche summieren
  end;
  for M:=1 to high(fxMdl) do
    if fxMdl[M,0]>0 then
      for F:=1 to high(fxMdl[0]) do
        fxMdl[M,F]/=fxMdl[M,0]; //durch Fläche teilen
end;

{ mMI wählt "iMap" Proben aus der Stichproben-Liste "fxSmp" und gibt sie als
  Klassen-Vorläufer zurück. Die Klasse "null" ist leer, das erste Merkmal ist
  die Fläche der Probe. Mit "iExt">"iMap" übergibt mMI möglichst verschiedene
  Klassen-Vorläufer. Dazu vereinigt mMI nach den ersten "iMap" Proben jeweils
  die zwei ähnlihsten Proben in der Liste. Proben mit großen Unterschieden
  bleiben übrig.}

function tModel.ModelInit(
  fxSmp:tn2Sgl; //Stichproben-Liste
  iExt:integer; //Erweiterte Auswahl
  iMap:integer): //Anzahl Klassen ohne Rückweisung
  tn2Sgl; //Klassen-Vorläufer
var
  fMin:single; //kleinste Differenz
  fSed:single; //Summe Differenz-Quadrate
  iHig,iLow:integer; //ähnlichste Proben
  iSlc:integer; //zufällige Auswahl
  E,F,K,L:integer;
begin
  Result:=Tools.Init2Single(succ(iMap),length(fxSmp[0]),0); //leer
  for E:=1 to iExt do //erweiterte Anzahl Proben
  begin
    iSlc:=succ(Random(high(fxSmp))); //zufällige Auswahl
    if E>iMap then
    begin
      for F:=1 to high(Result[0]) do
        Result[0,F]:=fxSmp[iSlc,F]; //neues Beispiel
      fMin:=MaxInt;
      for K:=1 to iMap do
        for L:=0 to pred(K) do
        begin
          fSed:=0; //leer
          for F:=1 to high(Result[0]) do
            fSed+=sqr(Result[K,F]-Result[L,F]);
          if fSed<fMin then
          begin
            fMin:=fSed;
            iLow:=L;
            iHig:=K;
          end;
        end;
      for F:=1 to high(Result[0]) do
        Result[iHig,F]:=(Result[iHig,F]+Result[iLow,F])/2; //mitteln
      if iLow>0 then
        for F:=1 to high(Result[0]) do
          Result[iLow,F]:=Result[0,F]; //übernehmen
    end
    else
      for F:=1 to high(Result[0]) do
        Result[E,F]:=fxSmp[iSlc,F]; //übernehmen
  end;
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
  if fxMdl=nil then Tools.ErrorOut(2,cMdl);
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
  Tools.HintOut(true,'Model.ImageClassify: '+IntToStr(length(fxMdl)));
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

procedure tModel.ClassValues(iRed,iGrn,iBlu:integer); //Kanäle [1..N]
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
  if rHdr.Cnt<>high(fxMdl) then Tools.ErrorOut(2,cCnt+cfMdl);

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

{ rHy reduziert unteschiedliche Kanäle in einem multilayer Bild, bewahrt dabei
  aber das Aufnahmedatum. Im Ergebnis hat jeder Kanal ein anderes Datum. Der
  Vorbild-Stack muss einen erweiterten ENVI-Header haben.
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
  rHdr,rRdh:trHdr; //Metadata, M mit reuzierten Kanälen
  sBnd:string='t1'; //erste Zeiperiode
  B,I:integer;
begin
  Header.Read(rHdr,sImg);
  if rHdr.Stk mod rHdr.Prd>0 then Tools.ErrorOut(2,cPrd+sImg);
  if sTrg='' then sTrg:=eeHme+sCmd; //Prozess-Name
  fxRes:=Tools.Init2Single(rHdr.Lin,rHdr.Scn,dWord(fNan)); //Vorgabe = ungültig
  fxTmp:=Tools.Init3Single(rHdr.Prd,rHdr.Lin,rHdr.Scn,0); //Vorgabe = leer
  Header.CopyRecord(rHdr,rRdh); //Kopie (ohne Palette)
  rRdh.Stk:=rHdr.Prd; //ruzierte Kanäle
  for I:=0 to max(pred(rHdr.Stk div rHdr.Prd),0) do //alle Bilder, notfalls nur eines
  begin
    for B:=0 to pred(rHdr.Prd) do //alle Kanäle
      fxTmp[B]:=Image.ReadBand(I*rHdr.Prd+B,rHdr,sImg); //Kanal "B" aus Bild "I" laden
    fxRes:=Execute(fxTmp,sArt,sCmd,rRdh); //Reduktion für ausgewählte Kanäle
    Image.WriteBand(fxRes,B,sTrg); //neues Bild für B=0, dann stapeln
    write(#13'Image '+IntToStr(rHdr.Stk div rHdr.Prd-B));
  end;
  for I:=2 to (rHdr.Stk div rHdr.Prd) do //Datum+Trenner
    sBnd+=#10't'+IntToStr(I); //laufende Nummer mit Zeilenwechsel
  rHdr.Prd:=1; //Zeitreihe!
  Header.WriteMulti(rHdr,sBnd,sTrg);
  Tools.HintOut(true,'Reduce.History: '+sCmd);
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
  if not FileExists(sImg) then Tools.ErrorOut(2,cFex+sImg);
  Header.Read(rHdr,sImg);
  fxStk:=Image.Read(rHdr,sImg); //Import vollständig
  fxRes:=Execute(fxStk,sArt,sCmd,rHdr); //Befehl anwenden
  if trim(sTrg)='' then sTrg:=eeHme+sCmd; //Vorgane = Name des Befehls
  Image.WriteBand(fxRes,0,sTrg); //neue Datei aus Ergebnis
  rHdr.Prd:=1; //nur ein Kanal
  rHdr.aBnd:=sCmd; //Kanal-Name = Prozess
  Header.WriteScalar(rHdr,sTrg); //Header für einen Kanal
  Tools.HintOut(true,'Reduce.Execute :'+ExtractFileName(sTrg)); //Status
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
  if length(fxImg)<2 then Tools.ErrorOut(2,cDim);
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

function tModel._Periods(
  fMax:single; //höchste zulässige Varianz
  faVal:tnSgl; //Bildpunkt-Zeitreihe (alle Kanäle)
  iPrd:integer): //Kanäle pro Bild
  single; //Anzahl Perioden
//Varianz = (∑x²-(∑x)²/n)/(n-1)
const
  cMax:single=MaxSingle;
var
  bSkp:boolean; //Schleife verlassen
  faMin:tnSgl=nil; //kleinste Varianz zum Nachbarn
  faSqr:tnSgl=nil; //quadrierte Werte/Summen
  fSum,fSqr,fVrz:single; //Zwischenlager Summe, Quadrat, Varianz
  iaCnt:tnInt=nil; //Bilder pro Periode
  iCnt:integer; //Zwischanlager Bilder pro Periode
  iHig:integer; //höchster definierter Index
  B,I:integer;
begin
  Result:=0;
  SetLength(faSqr,length(faVal)); //Speicher
  for I:=0 to high(faVal) do
    faSqr[I]:=sqr(faVal[I]); //Quadrate
  iHig:=pred(length(faVal) div iPrd); //höchster definierter Index
  iaCnt:=Tools.InitInteger(succ(iHig),1); //Vorgabe = ein Bild pro Periode

  repeat
    faMin:=Tools.InitSingle(succ(iHig),dWord(cMax)); //Vorgabe = sehr hoch
    for I:=1 to iHig do //Perioden
      if not isNan(faVal[pred(I)*iPrd]) and not isNan(faVal[I*iPrd]) then
      begin
        iCnt:=iaCnt[I]+iaCnt[pred(I)];
        if iCnt>1 then //nur definierte Summen
        begin
          fVrz:=0;
          for B:=0 to pred(iPrd) do
          begin
            fSum:=faVal[I*iPrd+B]+faVal[pred(I)*iPrd+B];
            fSqr:=faSqr[I*iPrd+B]+faVal[pred(I)*iPrd+B];
            fVrz+=max((fSqr-sqr(fSum)/iCnt)/pred(iCnt),0);
          end;
          if fVrz<faMin[pred(I)] then
            faMin[pred(I)]:=fVrz;
          if fVrz<faMin[I] then
            faMin[I]:=fVrz;
        end;
      end;

    bSkp:=True; //Vorgabe = Schleife verlassen
    for I:=iHig downto 1 do //alle Paare
      //if (faMin[pred(I)]=faMin[I]) and (faMin[I]<fMax) then //lokales Minimum, Schwelle
      if faMin[pred(I)]=faMin[I] then //lokales Minimum, Schwelle
      begin
        iaCnt[pred(I)]+=iaCnt[I]; //Treffer summieren
        for B:=0 to pred(iPrd) do
        begin
          faVal[pred(I)*iPrd+B]+=faVal[I*iPrd+B]; //Werte summieren
          faSqr[pred(I)*iPrd+B]+=faSqr[I*iPrd+B];
        end;
        if I<iHig then
        begin
          iaCnt[I]:=0; //KONTROLLE
          move(iaCnt[succ(I)],iaCnt[I],(iHig-I)*SizeOf(integer));
          move(faVal[succ(I)*iPrd],faVal[I*iPrd],(iHig-I)*iPrd*SizeOf(single));
          move(faSqr[succ(I)*iPrd],faSqr[I*iPrd],(iHig-I)*iPrd*SizeOf(single));
        end;
        iaCnt[iHig]:=0; //Kontrolle
        dec(iHig); //Stack kürzen
        faMin[pred(I)]:=cMax; //Verknüpfung ungültig machen
        faMin[I]:=cMax; //KONTROLLE
        bSkp:=False //Schleife wiederholen
      end;
  until bSkp;
  //Result:=succ(iHig) //Anzahl Perioden
  Result:=fVrz //stärkste Veränderung
end;

procedure tModel._xNoChange(fMax:single; sStk:string);
var
  faVal:tnSgl=nil; //Pixel-Werte (Kanäle als Stack)
  fxPrd:tn2Sgl=nil; //Anzahl Perioden
  fxStk:tn3Sgl=nil; //Bild als Kanal-Stapel, Frequenzen + Zeitpunkte
  rHdr:trHdr; //Metadaten
  I,X,Y:integer;
begin
  Header.Read(rHdr,sStk);
{ TODO: bei langen Zeireihen Bilder kacheln}
  fxStk:=Image.Read(rHdr,sStk); //Zeitreihe
  faVal:=Tools.InitSingle(rHdr.Stk,0); //Übergabe
  fxPrd:=Tools.Init2Single(length(fxStk[0]),length(fxStk[0,0]),0);

  for Y:=0 to pred(rHdr.Lin) do
    for X:=0 to pred(rHdr.Scn) do
    begin
      for I:=0 to pred(rHdr.Stk) do
        faVal[I]:=fxStk[I,Y,X]; //Pixel, multispektral, multitemporal
      fxPrd[Y,X]:=_Periods(fMax,faVal,rHdr.Prd);
    end;

  Image.WriteBand(fxPrd,0,eeHme+'periods');
  Header.WriteScalar(rHdr,eeHme+'periods');
end;

{ rQC zählt die gültigen Kanäle für jedem Pixel in "fxImg" und gibt das
  Ergebnis als Bild zurück.}

procedure tReduce.QualityImage(
  sImg:string; //Vorbild
  sTrg:string); //Ergebnis-Name ODER leer für Vorgabe
const
  cImg = 'rQI: Image not found: ';
var
  fxBnd:tn2Sgl=nil; //Vorbild-Kanal
  fxRes:tn2Sgl=nil; //Ergebnis
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
          fxRes[Y,X]+=1.0;
  end;

  if trim(sTrg)=''
    then sTrg:=eeHme+cfQuy //Vorgane = Name des Befehls
    else sTrg:=sTrg+'_'+cfQuy; //Erweiterung
  Image.WriteBand(fxRes,0,sTrg); //neue Datei aus Ergebnis
  rHdr.aBnd:=cfQuy; //Kanal-Name = Quality
  Header.WriteScalar(rHdr,sTrg); //Header für einen Kanal
  Tools.HintOut(true,'Reduce.QualityImage: '+ExtractFileName(sTrg)); //Status
end;

{ rRg bestimmt die Regression aller Kanäle aus "fxImg" für einzelne Pixel und
  gibt sie als Bild zurück. Wenn im Header das Feld "akquisition" definiert
  ist, verwendet rRg reale Zeitperioden, sonst gleiche Zeitabstände zwischen
  allen Bidern.


  unterstellt, dass die Datei-Namen mit einem
  Datum enden. Ist das nicht der Fall, verwendet rRg gleiche Abstände zwischen
  allen Kanälen in der gegebenen Reihenfolge. rRg prüft jeden Kanal auf NoData,
  so dass auch lückige Bilder verarbeitet werden können. }
{ ==> Regression = (∑xy-∑x∑y/n) / (∑y²-(∑y)²/n); x=Zeit, y=Wert }

function tReduce._Regression(
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
{     TODO: Regression: für unterschiedliche Zeitperioden müsste "SortDate"
            das Datum der verschiedenen Bilder als Systemzeit in eine Liste
            schreiben, die "Regression" lesen kann }
      if iCnt>0
        then fDvs:=fSqr-sqr(fSum)/iCnt
        else fDvs:=0;
      if fDvs>0
        then Result[Y,X]:=(fPrd-fTms*fSum/iCnt)/fDvs
        else Result[Y,X]:=NaN;
    end;
end;

{ rSD sortiert Dateinamen nach dem Datum am Ende des Namens. rSD erkennt ob nur
  das Jahr [YYYY] oder Jahr und Datum [YYYYMMDD] angegeben sind und reagiert
  entsprechend. }

function tReduce.SortDate(
  slImg:tStringList): //Liste Bildnamen: WIRD VERÄNDERT
  integer; //Buchstaben für Datum am Ende des Dateinamens
const
  cDat = 'rSD: Date or year must be provided at the end of the filenames';
  cMrg = 'rSD: Undefined parameter for date/year ending';
var
  iInt:integer; //Ganzzahl
begin
  Result:=0;
  if TryStrToInt(RightStr(ChangeFileExt(slImg[0],''),8),iInt) then Result:=8 else
  if TryStrToInt(RightStr(ChangeFileExt(slImg[0],''),4),iInt) then Result:=4 else
    Tools.ErrorOut(3,cDat);
  if Result=8 then slImg.CustomSort(@EndDate) else //Nach Datum [YYYYMMDD] am Ende sortieren
  if Result=4 then slImg.CustomSort(@EndYear) else //Nach Jahr [YYYY] am Ende sortieren
    Tools.ErrorOut(3,cMrg);
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
      iFit:=M; //temporär ähnlichte Klasse
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
  Tools.HintOut(true,'Model.FeatureCombinations: '+IntToStr(high(fxMdl)));
  ixMap:=ClassifyPixels(fxMdl,sImg); //Klassen-IDs als Raster
  Image.WriteThema(ixMap,eeHme+cfMap); //Raster speichern
  Header.WriteThema(iMap,rHdr,'',eeHme+cfMap); //Metadaten speichern
  Tools.HintOut(true,'Model.RolfFeatures: '+cfMap)
end;

{ mZS wählt zufällige Punkte aus dem Zonen-Bild "ixIdx" und gibt die Attribute
  der getroffenen Zone als "fxMdl[0]" zurück. }

procedure tModel.SelectZone(
  fxAtr:tn2Sgl; //Attribut-Tabelle
  fxMdl:tn2Sgl; //Klassen-Definitionen
  ixIdx:tn2Int; //Zonen-IDs als Bild
  var rHdr:trHdr); //Metadaten
var
  B,X,Y:integer;
begin
  repeat
    X:=random(rHdr.Scn); //zufällige Auswahl
    Y:=random(rHdr.Lin);
  until not isNan(ixIdx[Y,X]); //nur definierte Pixel
  for B:=1 to high(fxMdl[0]) do //alle Attribute
    fxMdl[0,B]:=fxAtr[pred(B),ixIdx[Y,X]]; //neue Merkmale in Null eintragen
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
  if fxMdl=nil then Tools.ErrorOut(2,cMdl);
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
  Tools.BitInsert(tnSgl(Result),0,eeHme+cfMap+cfBit); //als BIT-Tabelle speichern
  Tools.HintOut(true,'Model.ZonesClassify: '+IntToStr(length(fxMdl)));
end;

{ mZM verwendet dieselben Routinen wie "xPixelMap", klassifiziert aber kein
  Bild sondern die Attribut-Tabelle "index.bit". mZM gibt das Ergebnis als
  Klassen-Attribut "iaThm" und als Bild "mapping" zurück. }
{ Zonen-IDs als Raster "index", Zonen-Attribute "index.bit" und Zonen-Topologie
  "topology.bit" müssen existieren. mZM wählt Stichproben aus dem Bild, große
  Zonen werden dabei häufiger getroffen als kleine. Für das Klassen-Bild
  kombiniert mZM das Raster-Bild der Zonen-IDs "index" mit dem Klassen-Attribut
  "iaThm". }

procedure tModel.xZonesMap(
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
  qA:integer;
begin
  if iMap>250 then Tools.ErrorOut(3,cMap);
  if iSmp<iMap*100 then Tools.ErrorOut(3,cSmp);
  if not FileExists(eeHme+cfIdx) then Tools.ErrorOut(3,cIdx);
  if not FileExists(sAtr) then Tools.ErrorOut(3,cAtr);

  //Image.AlphaMask(sImg); //NUR BEI PIXELN NÖTIG, ATTRIBUTE SIND DEFINIERT
  Header.Read(rHdr,eeHme+cfIdx); //Metadaten Zonen-IDs
  ixIdx:=tn2Int(Image.ReadBand(0,rHdr,eeHme+cfIdx)); //Zonen-IDs
  fxAtr:=Tools.BitRead(sAtr); //Attribut-Tabelle lesen
  fxMdl:=Tools.Init2Single(succ(iMap),succ(length(fxAtr)),0); //Klassen-Definitionen, Attribute+1, ID=0
  qA:=high(fxAtr[0]);
  if rHdr.Cnt<>high(fxAtr[0]) then Tools.ErrorOut(3,cCnt);
  RandSeed:=cfRds; //Zufalls-Generator initialisieren

  for I:=1 to iSmp do
  begin
    SelectZone(fxAtr,fxMdl,ixIdx,rHdr); //zufällige Zone in fxMdl[0] eintragen
    if I>iMap*ccOvr then SampleFit(fxMdl) //Phase 3: Sample in ähnlichste Klasse integrieren
    else if I>iMap then SampleMerge(fxMdl) //Phase 2: Klassen vereinigen ODER Sample integrieren
    else SampleAdd(fxMdl,I); //Phase 1: Klassen-Array initialisieren
  end;

  Tools.BitWrite(fxMdl,eeHme+cfMdl); //Klassen-Definitionen als Bit-Tabelle
  Tools.HintOut(true,'Class definition: '+cfMdl);
  iaThm:=ClassifyZones(fxAtr,fxMdl); //Klassen-Attribut
  ixMap:=Build.ThemaImage(iaThm); //Klassen-Rasterbild
  Image.WriteThema(ixMap,eeHme+cfMap); //Raster speichern
  Header.WriteThema(iMap,rHdr,'',eeHme+cfMap); //Metadaten speichern
  Tools.HintOut(true,'Class image: '+cfMap)
end;

procedure tModel._xFabricMap(
  iGen:integer; //Iterationen für Kontext
  iSmp:integer); //Stichproben
var
  fxDns:tn2Sgl=nil; //Attribt "Klassen-Dichte"
  iaThm:tnInt=nil; //Klasen-Attribut
  iMap:integer; //Anzahl Attribut-Klassen
  Z:integer;
const
  cThm = 'mFM: Classification result "mapping.bit" not found!';
begin
  if not FileExists(eeHme+cfMap+cfBit) then Tools.ErrorOut(3,cThm);
  iaThm:=tnInt(Tools.BitExtract(0,eeHme+cfMap+cfBit)); //Klassen-Attribut
  iMap:=MaxIntValue(iaThm); //höchste Klassen-ID aus "mapping"
  fxDns:=Tools.Init2Single(succ(iMap),length(iaThm),0); //leere Vorgabe
  for Z:=1 to high(iaThm) do
    fxDns[iaThm[Z],Z]:=1.0; //Dichte = Eins für richtige Klasse
  Tools.BitWrite(fxDns,eeHme+cfCtx); //für "xDiffusion"
  Build.xDiffusion(iGen,eeHme+cfCtx); //Attribut-Tabelle ausgleichen
  xZonesMap(iMap,iSmp,eeHme+cfCtx); //neue Klassen
end;

end.

{==============================================================================}

{ mCD klassifiziert die lokale Klassen-Dichte = Kontakte zwischen Klassen und
  gibt das Ergebnis als Klassen-Layer "density" zurück. mCD benötigt einen
  Zonen-Index, Zonen-Klassen und eine Zonen-Topologie. }
{ mCD liest das Klassen-Attribut, erzeugt daraus eine scalare Attribut-Tabelle
  mit der lokalen Häufigkeit jeder einzelnen Klasse. Zu Beginn gibt es nur die
  Häufigkeit Eins und Null. mCD diffundiert die Klassen-Häufigkeit in "iGen"
  Iterationen. Das Ergebnis ist eine Klassifikation der so erstellten Klassen-
  Dichte. Für eine Iteration ist das Ergebnis identisch mit der Anzahl der
  Kontakte zwischen den Zonen inclusive innerer Kontakte. }

procedure tModel._ClassDensity(
  iDns:integer; //Density-Klassen
  iGen:integer; //Iterationen beim Ausgleich
  iSmp:integer); //Anzahl Stichproben
var
  fxDns:tn2Sgl; //lokale Dichte für alle Klassen
  iaThm:tnInt=nil; //Klassen-Attribut
  iMap:integer; //Anzahl Klassen in "iaThm"
  ixMap:tn2Byt=nil; //Klassen-Layer
  Z:integer;
begin
  iaThm:=tnInt(Tools.BitExtract(0,eeHme+'mapping.bit')); //KLassen-Attribut
  iMap:=MaxIntValue(iaThm); //höchste Klassen-ID aus "mapping"
  fxDns:=Tools.Init2Single(succ(iMap),length(iaThm),0); //Klassen-Dichte pro Zone
  for Z:=1 to high(iaThm) do
    fxDns[iaThm[Z],Z]:=1.0; //Dichte = Eins für eine Klasse
  Tools.BitWrite(fxDns,eeHme+'density.bit'); //Attribut-Tabelle speichern
  Build.xDiffusion(iGen,eeHme+'density.bit'); //Attribut-Tabelle ausgleichen
  fxDns:=Tools.BitRead(eeHme+'density.bit'); //Dichte Attribut lesen
  iaThm:=ThemaIndex(iDns,iSmp,eeHme+'density.bit'); //Klassen-Attribut "Dichte"
  ixMap:=Build.ThemaImage(iaThm); //Klassen-Rasterbild
  ThemaLayer(iDns,ixMap,eeHme+cfIdx,eeHme+'density'); //Klassen-Layer
  Tools.HintOut(true,'Build.ClassDensity: '+IntToStr(iDns))
end;

{==============================================================================}

{ mZM klassifiziert Pixel mit ihren Spektralkombinationen und gibt das Ergebnis
  als Klassen-Layer zurück. }
{ mZM nimmt "iSmp" Proben aus dem übergebeben Bild, analysiert die lokale
  Dichte der Proben im Merkmalsraum und fasst lokale Schwerpunkte zu Klassen
  zusammen. Der Einfluss einzelner Proben auf die Klassen entspricht ihrer
  Häufigkeit. Mit diesen Klassen klassifiziert mZM alle Pixel und speichert das
  Ergebnis als Raster-Layer mit zufälligen Paletten-Farben. Bei Klassen und
  Proben ist das erste Element die Rückweisung, das erste Merkmal die Fläche
  der Zone bzw Klasse. }

procedure tModel.xImageMap(
  iMap:integer; //Anzahl Klassen
  iSmp:integer; //Anzahl Stichproben
  sImg:string); //Vorbild (nur für Geometrie)
const
  cFex = 'mZM: Image not found: ';
  cMap = 'fFc: Number of fabric classes must exeed 2!';
  cSmp = 'fFc: Number of fabric samples must exeed 1000!';
var
  fxMdl:tn2Sgl=nil; //Klassen
  fxSmp:tn2Sgl=nil; //Stichproben → Klassen-Definition
  ixMap:tn2Byt=nil; //Klassen-Layer
begin
  if not FileExists(sImg) then Tools.ErrorOut(2,cFex+sImg);
  if iMap<2 then Tools.ErrorOut(2,cMap);
  if iSmp<1000 then Tools.ErrorOut(2,cSmp);
  Image.AlphaMask(sImg); //NoData-Maske auf alle Kanäle ausdehnen
  fxSmp:=PixelSamples(iSmp,sImg); //Stichproben aus Bilddaten
  fxMdl:=SampleModel(fxSmp,iMap); //Klassen aus Stichproben
  ixMap:=PixelClassify(fxMdl,sImg); //Klassen-Matrix aus Bilddaten
  ThemaLayer(iMap,ixMap,sImg,eeHme+cfMap); //Klassen-Layer
  Tools.HintOut(true,'Model.ZonesMap: '+cfMap)
end;

{ mZM klassifiziert Zonen mit ihren Attributen und gibt das Ergebnis als
  Klassen-Layer zurück. Zonen, Attribute und Topologie müssen im Arbeits-
  Verzeichnis stehen. }
{ mZM nimmt "iSmp" Proben aus den aktuellen Zonen und fasst lokale Schwerpunkte
  im Merkmalsraum zu Klassen zusammen. Große Zonen können mehrmals getroffen
  sein und haben damit mehr Einfluss auf das Ergebnis. mZM klassifiiert alle
  Zonen und speichert das Ergebnis als Raster-Layer mit zufälligen Paletten-
  Farben. Bei Klassen und Proben ist das erste Element (Index=0) die
  Rückweisung, das erste Merkmal die Fläche der Zone bzw Klasse. }

procedure tModel._xZonesMap(
  iMap:integer; //Anzahl Klassen
  iSmp:integer; //Anzahl Stichproben
  sAtr:string); //Attribut-Tabelle (Original/Equalize)
var
  ixMap:tn2Byt=nil; //Klassen-Layer
  iaThm:tnInt=nil; //KLassen-Attribut
begin
  iaThm:=ThemaIndex(iMap,iSmp,sAtr); //Klassen-Attribut
  Tools.BitInsert(tnSgl(iaThm),-1,eeHme+'mapping.bit'); //Klassen-Attribut speichern
  ixMap:=Build.ThemaImage(iaThm); //Klassen-Rasterbild
  ThemaLayer(iMap,ixMap,eeHme+cfIdx,eeHme+cfMap); //Klassen-Layer
  Tools.HintOut(true,'Model.ZonesMap: '+cfMap)
end;

{ mFM clustert räumliche Kombinationen aus Zonen und gibt das Ergebnis als
  Klassen-Layer mit zufälligen Farben zurück. Topologie, Zonen und Attribute
  müssen im Arbeitsverzeichnis stehen. mFM kann Muster aus klassifizierten
  Zonen bilden (iGen=0) oder zusätzlich die lokale Dichte aller Merkmale für
  die Klassifikation verwenden (iGen>0). Im diesem Fall steuert "iGen" wie weit
  der Einfluss einer Zone reichen soll. }
{ Mit "iGen=0" clustert fFM die Attribute wie "xZonesMap", bestimmt für jede
  Zone die Häufigkeit der Klassen-Kontakte und clustert diese Häufigkeiten als
  Muster-Klassen "cfCtx". }
{ Mit "iGen>0" erzeugt mFM eine erweiterte Attribut-Tabelle "cfCtx", die
  zusätzlich die Attribute der Nachbarzonen enthält und clustert beide Tabellen
  zusammen. Für die Erweiterung diffundiert fFM alle Attribute iterativ in die
  jeweiligen Nachbarzonen. Der Austausch ist proportional zur gemeinsamen
  Grenze und zur Zahl der Iterationen "iGen". }
{ In beiden Fällen nimmt mFM zufällige Proben aus der Bildfläche und clustert
  die Proben nach den Distanzen im Merkmalsraum. Alle Zonen beeinflussen das
  Ergebnis entsprechend ihrer Fläche. Bei Proben und Klassen-Definitionen ist
  das erste Element die Rückweisung und das erste Merkmal die Fläche der Zone
  bzw Klasse. }

procedure tModel.xFabricMap(
  iGen:integer; //Anzahl Diffusions-Schritte
  iMap:integer; //Anzahl Klassen
  iSmp:integer; //Anzahl Stichproben
  sAtr:string); //Attrbut-Tabelle (Original/Equalize)
const
  cGen = 'fFM: The "diffusion" parameter must never be negative! ';
var
  ixMap:tn2Byt=nil; //Klassen-Layer
  iaThm:tnInt=nil; //KLassen-Attribut
 begin
  if iGen<0 then Tools.ErrorOut(2,cGen);
  if iGen=0 then
  begin
    iaThm:=ThemaIndex(iMap,iSmp,sAtr); //Klassen-Attribut
    Build.LinkFeatures(iaThm,iMap); //Attribute "cfCtx" aus Nachbar-Klassen-Häufigkeit
  end
  else Build.ExtendFeatures(iGen,sAtr); //Attribute ausgleichen
  iaThm:=ThemaIndex(iMap,iSmp,eeHme+cfCtx); //Klassen-Attribut
  ixMap:=Build.ThemaImage(iaThm); //Klassen-Layer
  ThemaLayer(iMap,ixMap,eeHme+cfIdx,eeHme+cfMap); //Klassen-Layer
  Tools.HintOut(true,'Model.FabricMap: '+cfMap)
end;

// Klassen zusammenfassen, wenn sie häufig nebeneinander vorkommen.

procedure tModel._ClassRegions(sMap:string); //Klassen-Layer
var
  ixMap:tn2Byt=nil; //Klassen-Layer als Bild
  ixRes:tn2Int=nil; //Tabelle für alle Klassen-Kombinationen
  rHdr:trHdr; //Metadaten
  sTab:string=''; //Ergebnis als Text
  R,S,X,Y:integer;
begin
  Header.Read(rHdr,sMap); //Metadaten Vorbild
  ixMap:=Image.ReadThema(rHdr,sMap); //Klassen-Layer
  ixRes:=Tools.Init2Integer(succ(rHdr.Cnt),succ(rHdr.Cnt),0); //Klassen-Kombinationen

  for Y:=1 to high(ixMap) do
    for X:=0 to high(ixMap[0]) do
    begin
      inc(ixRes[ixMap[Y,X],ixMap[pred(Y),X]]);
      inc(ixRes[ixMap[pred(Y),X],ixMap[Y,X]])
    end;
  for Y:=0 to high(ixMap) do
    for X:=1 to high(ixMap[0]) do
    begin
      inc(ixRes[ixMap[Y,X],ixMap[Y,pred(X)]]);
      inc(ixRes[ixMap[Y,pred(X)],ixMap[Y,X]])
    end;

  {for R:=0 to high(ixRes) do
  begin
    ixRes[R,0]:=0; //Verknüpfung zu NoData löschen
    ixRes[0,R]:=0;
    ixRes[R,R]:=0;
  end;

  for R:=1 to high(ixRes) do
    for S:=1 to high(ixRes[0]) do
    begin
      ixRes[0,S]+=ixRes[R,S]; //Summe Kontakte
      ixRes[R,0]+=ixRes[R,S]; //Summe Kontakte
    end;}

  for R:=0 to high(ixRes) do
  begin
    ixRes[0,R]:=R;
    ixRes[R,0]:=R;
    ixRes[R,R]:=0;
  end;

  for R:=0 to high(ixRes) do
  begin
    for S:=0 to high(ixRes[0]) do
      sTab+=IntToStr(ixRes[R,S])+#9;
    sTab[length(sTab)]:=#10; //Zeilenwechsel statt Tab
  end;
  Tools.TextOut(eeHme+cfTab,sTab);
end;

procedure tModel._ClassCombine(iStp:integer; sMap:string); //Klassen-Layer
var
  iaCnt:tnInt=nil; //Klassen-IDs
  iaLnk:tnInt=nil; //Klassen-Verknüpfung
  ixMap:tn2Byt=nil; //Klassen-Layer (Bild)
  ixTab:tn2Int=nil; //Häufigkeit der Klassen-Kontakte
  rHdr:trHdr; //Metadaten
  sRes:string='';
  sTab:string=''; //Ergebnis als Text
  I,R,S,X,Y:integer;
begin
  Header.Read(rHdr,sMap); //Metadaten Vorbild
  ixMap:=Image.ReadThema(rHdr,sMap); //Klassen-Layer

  for I:=1 to iStp do
  begin
    ixTab:=Tools.Init2Integer(succ(rHdr.Cnt),succ(rHdr.Cnt),0); //Klassen-Kombinationen
    for Y:=1 to high(ixMap) do
      for X:=0 to high(ixMap[0]) do
      begin
        inc(ixTab[ixMap[Y,X],ixMap[pred(Y),X]]);
        inc(ixTab[ixMap[pred(Y),X],ixMap[Y,X]])
      end;
    for Y:=0 to high(ixMap) do
      for X:=1 to high(ixMap[0]) do
      begin
        inc(ixTab[ixMap[Y,X],ixMap[Y,pred(X)]]);
        inc(ixTab[ixMap[Y,pred(X)],ixMap[Y,X]])
      end;

    for R:=0 to high(ixTab) do
    begin
      ixTab[0,R]:=R; //Kontakte zu NoData löschen
      ixTab[R,0]:=R;
      ixTab[R,R]:=0; //interne Kontakte löschen
    end;

    {for R:=0 to high(ixTab) do
    begin
      for S:=0 to high(ixTab[0]) do
        sTab+=IntToStr(ixTab[R,S])+#9;
      sTab[length(sTab)]:=#10; //Zeilenwechsel statt Tab
    end;
    Tools.TextOut(eeHme+cfTab,sTab); //Tabelle als Text} //KONTROLLE

    iaLnk:=Tools.InitIndex(length(ixTab));
    for R:=1 to high(ixTab) do
      for S:=1 to high(ixTab) do
        if ixTab[R,S]>ixTab[R,iaLnk[R]] then
          iaLnk[R]:=S; //stärkste Verknüpfung

    iaCnt:=Tools.InitInteger(length(ixTab),0);
    rHdr.Cnt:=0; //Klassen neu zählen
    for R:=1 to high(iaLnk) do
      if (iaCnt[R]=0) //kein Eintrag
      and (iaLnk[iaLnk[R]]=R) then //Rückbezug
      begin
        inc(rHdr.Cnt);
        iaCnt[iaLnk[R]]:=rHdr.Cnt;
        iaCnt[R]:=rHdr.Cnt
      end;

    for R:=1 to high(iaLnk) do
      if iaCnt[R]=0 then
      begin
        inc(rHdr.Cnt);
        iaCnt[R]:=rHdr.Cnt
      end;

    for Y:=0 to high(ixMap) do
      for X:=0 to high(ixMap[0]) do
        ixMap[Y,X]:=iaCnt[ixMap[Y,X]]; //neue Klassen eintragen
  end;

  sRes:=eeHme+'regions';
  Image.WriteThema(ixMap,sRes);
  Header.WriteThema(rHdr.Cnt,rHdr,'',sRes);
end;

{ mMS wählt "iMap"+1 Proben aus der Stichproben-Liste "fxSmp" und gibt sie als
  Klassen-Vorläufer zurück. Die erste Klasse ist leer, das erste Merkmal die
  Fläche der Probe. }

function tModel._M_odelInit_(
  fxSmp:tn2Sgl; //Stichproben-Liste
  iMap:integer): //Anzahl Klassen ohne Rückweisung
  tn2Sgl; //Klassen-Vorläufer
var
  iSlc:integer; //zufällige Auswahl
  F,M:integer;
begin
  Result:=Tools.Init2Single(succ(iMap),length(fxSmp[0]),0); //leer
  for M:=1 to iMap do
  begin
    iSlc:=Random(succ(high(fxSmp))); //zufällige Auswahl
    for F:=0 to high(fxSmp[0]) do
      Result[M,F]:=fxSmp[iSlc,F];
  end;
end;

procedure tModel._P_eriods_(
  fMax:single; //höchste zulässige Varianz
  faVal:tnSgl; //Bildpunkt-Zeitreihe (alle Kanäle)
  iPrd:integer); //Kanäle pro Bild
//Varianz = (∑x²-(∑x)²/n)/(n-1)
var
  bSkp:boolean; //Schleife verlassen
  faMin:tnSgl=nil; //kleinste Varianz zum Nachbarn
  faSqr:tnSgl=nil; //quadrierte Werte/Summen
  fSum,fSqr,fVrz:single; //Zwischenlager Summe, Quadrat, Varianz
  iaCnt:tnInt=nil; //Bilder pro Periode
  iaLnk:tnInt=nil; //beste Verknüpfung der Perioden
  iCnt:integer; //Zwischanlager Bilder pro Periode
  iHig:integer; //höchste definierte Perioden-ID (ab Null)
  B,I:integer;
begin
  SetLength(faSqr,length(faVal)); //Speicher
  for I:=0 to high(faVal) do
    faSqr[I]:=sqr(faVal[I]); //Quadrate
  iHig:=pred(length(faSqr) div iPrd); //höchster definierter Index
  iaCnt:=Tools.InitInteger(succ(iHig),1); //Vorgabe = ein Bild pro Periode

  repeat
    faMin:=Tools.InitSingle(succ(iHig),dWord(fMax)); //Vorgabe = sehr hoch
    iaLnk:=Tools.InitIndex(succ(iHig)); //Vorgabe = Selbstbezug
    for I:=1 to iHig do //Perioden
      if not isNan(faVal[pred(I)*iPrd]) and not isNan(faVal[I*iPrd]) then
      begin
        iCnt:=iaCnt[I]+iaCnt[pred(I)];
        if iCnt>1 then //nur definierte Summen
        begin
          fVrz:=0;
          for B:=0 to pred(iPrd) do
          begin
            fSum:=faVal[I*iPrd+B]+faVal[pred(I)*iPrd+B];
            fSqr:=faSqr[I*iPrd+B]+faVal[pred(I)*iPrd+B];
            fVrz+=max((fSqr-sqr(fSum)/iCnt)/pred(iCnt),0);
          end;
          if fVrz<faMin[pred(I)] then
          begin
            iaLnk[pred(I)]:=I;
            faMin[pred(I)]:=fVrz
          end;
          if fVrz<faMin[I] then
          begin
            iaLnk[I]:=pred(I);
            faMin[I]:=fVrz
          end;
        end;
      end;

    bSkp:=True; //Vorgabe = Schleife verlassen
    for I:=iHig downto 1 do //alle Paare
      if (iaLnk[pred(I)]=I) and (iaLnk[I]=pred(I)) then
      begin
        for B:=0 to pred(iPrd) do
        begin
          faVal[pred(I)*iPrd+B]+=faVal[I*iPrd+B]; //Werte summieren
          faSqr[pred(I)*iPrd+B]+=faSqr[I*iPrd+B];
        end;
        iaCnt[pred(I)]+=iaCnt[I]; //Summe Bilder
        if I<iHig then
        begin
          move(iaCnt[succ(I)],iaCnt[I],(iHig-I)*SizeOf(integer));
          move(faVal[succ(I)*iPrd],faVal[I*iPrd],(iHig-I)*iPrd*SizeOf(single));
          move(faSqr[succ(I)*iPrd],faSqr[I*iPrd],(iHig-I)*iPrd*SizeOf(single));
        end;
        iaLnk[pred(I)]:=-1; //Verknüpfung löschen
        iaLnk[I]:=-1;
        dec(iHig);
        bSkp:=False //Schleife wiederholen
      end;
  until bSkp;
end;

{ mIS gibt "iSmp" Stichproben aus den Bilddaten zurück, die zufällig über die
  Bildfläche verteilt sind. Dazu wählt mIS mit einem Zufalls-Generator einzelne
  Pixel im Bild und gibt die Werte aller Kanäle des Pixels als Array zurück.
  Die Proben sind lineare Arrays. Array[0] nimmt die Fläche auf. }

function tModel.P_ixelSamples(
  iSmp: integer; //Anzahl Stichproben
  sImg: string): //Vorbild (ENVI-Format)
  Tn2Sgl; //Stichproben[Probe][Merkmale]
const
  cSmp = 'tMAS: Amount of samples must be greater than 1!';
var
  fxImg: tn3Sgl=nil; //Bilddaten, alle Kanäle
  rHdr: trHdr; //Metadaten
  B,S,X,Y: integer;
begin
  Result:=nil;
  if iSmp<2 then Tools.ErrorOut(2,cSmp);
  Header.Read(rHdr,sImg); //Metadaten
  fxImg:=Image.Read(rHdr,sImg); //Bild mit allen Kanälen
  Result:=Tools.Init2Single(succ(iSmp),succ(length(fxImg)),0); //Merkmale-Liste
  RandSeed:=cfRds; //Reihe zurückstellen
  for S:=1 to high(Result) do //Proben ohne Rückweisung
  begin
    repeat
      Y:=random(rHdr.Lin);
      X:=random(rHdr.Scn)
    until not isNaN(fxImg[0,Y,X]); //nur definierte Orte
    for B:=0 to high(fxImg) do
      Result[S,succ(B)]:=fxImg[B,Y,X]; //Dichte-Kombination,
    Result[S,0]:=1; //Fläche = 1 Pixel
  end;
  Tools.HintOut(true,'Model.Samples: '+IntToStr(iSmp));
end;

function tModel.T_hemaIndex(
  iMap:integer; //Anzahl Klassen
  iSmp:integer; //Anzahl Stichproben
  sAtr:string): //Name der Attribut-Tabelle
  tnInt; //Klassen-Attribut
const
  cAtr = 'bCZ: Zonal classification needs a zones attribute table ';
  cIdx = 'bCZ: Zonal classification needs a zones definition image "index"';
  cMap = 'bCZ: Number of fabric classes must exeed 2!';
  cSmp = 'bCZ: Number of fabric samples must exeed 1000!';
  cTpl = 'bCZ: Zonal classification needs a zones topology "topology.bit"';
var
  fxMdl:tn2Sgl=nil; //Klassen-Definitionen
  fxSmp:tn2Sgl=nil; //Strichproben aus Attribut-Tabelle
begin
  Result:=nil; //Klassen-Attribut
  if iMap<2 then Tools.ErrorOut(2,cMap);
  if iSmp<1000 then Tools.ErrorOut(2,cSmp);
  if not FileExists(eeHme+cfIdx) then Tools.ErrorOut(2,cIdx);
  if not FileExists(sAtr) then Tools.ErrorOut(2,cAtr+'sAtr');
  if not FileExists(eeHme+cfTpl) then Tools.ErrorOut(2,cTpl);

  fxSmp:=SelectSamples(iSmp,sAtr);
  fxMdl:=SampleModel(fxSmp,iMap); //Klassen-Definitionen aus Proben
  Result:=ClassifySamples(fxMdl,sAtr); //Klassen-Attribut für alle Zonen
end;

{ mTL speichert den Klassen-Layer "ixMap" als RGB-Bild mit Palette }

procedure tModel.T_hemaLayer(
  iMap:integer; //Anzahl Klassen ohne Rückweisung
  ixMap:tn2Byt; //Klassen-Layer
  sImg:string; //Vorbild (nur für Geometrie)
  sRes:string); //Name Klassen-Bild
var
  rHdr:trHdr; //Metadaten
begin
  //Rank.SortByte(iFtr,ixMap); //Klassen-ID nach Fläche
  Image.WriteThema(ixMap,sRes); //
  Header.Read(rHdr,sImg); //Vorbild (nur Geometrie)
  Header.WriteThema(iMap,rHdr,'',sRes); //umwadeln und speichern
end;

{ mRF klassifiziert Spektralkombinationen im einem Bild. Die Zahl der Klassen
  und die Dichte der Stichproben ist wählbar. mRF bestimmt die Definition der
  Klassen, speichert sie als "model.bit" und klassifiziert die Bilddaten als
  "mapping". }
{ mRF nimmt "iSmp" zufällige Stichproben aus dem gesamten Bild und bildet damit
  "iMap" Klassen. Das Training hat drei Phasen. (1) mRF übernimmt die ersten
  "iMap" Proben unverändert. (2) mRF vergleicht bis zu den nächsten "iMap*cOvr"
  Proben alle bestehenden Klassen. Sind sich zwei Klassen ähnlicher als die
  Probe mit jeder bestehenden Klasse, vereinigt mRF die beiden ähnlichsten
  Klassen und ergänzt die Probe als neue Klasse. (3) Ab "iMap*cOvr" Proben
  vereinigt mRF die Probe mit der ähnlichsten Klasse. }
{ mRF vereinigt eine Probe mit einer Klassen so, dass die Probe die Klasse nur
  wenig verändert. Der Faktor "cRtn" bestimmt die Gewichte zwischen den alten
  Werten der Klasse und den Werten der Probe. Der Anteil älterer Proben sinkt
  damit kontinuierlich. Mit "cRtn"=30 sinkt das Gewicht einer Probe nach ca 130
  Proben unter 1% bei "cRtn"=10 nach ca. 45 Proben. }

procedure tModel.x_RolfPixel_(
  iMap:integer; //Anzahl Klassen
  iSmp:integer; //Stichproben
  sImg:string); //Vorbild
const
  cOvr = 3; //Oversampling
  cMap = 'mRP: Too many classes! Maximal 250 classes are defined';
  cSmp = 'mRP: At least 100 samples per class must be passed!';
  cRtn = 30; //Klassen immer um 1/cRtn ändern
  //cRtn = 10; //Klassen immer um 1/cRtn ändern
var
  fMin:single; //kleinste Distanz
  fTmp:single; //Zwischenlager
  fxImg:tn3Sgl=nil; //Vorbild
  fxMdl:tn2Sgl=nil; //Klassen-Definitionen
  iFit:integer; //beste Klasse
  iHig,iLow:integer; //ähnlichste Klassen
  ixMap:tn2Byt=nil; //Klassen-Layer
  rHdr:trHdr; //Metadaten
  B,I,M,N,X,Y:integer;
begin
  if iMap>250 then Tools.ErrorOut(3,cMap);
  if iSmp<iMap*100 then Tools.ErrorOut(3,cSmp);
  Image.AlphaMask(sImg); //NoData-Maske auf alle Kanäle ausdehnen
  Header.Read(rHdr,sImg); //Metadaten Vorbild
  fxImg:=Image.Read(rHdr,sImg); //Vorbild, alle Kanäle
  fxMdl:=Tools.Init2Single(1,succ(length(fxImg)),0); //Rückweisung als Vorgabe
  RandSeed:=cfRds; //Zufalls-Generator initialisieren

  for I:=1 to iSmp do
  begin
    //zufälligen Bildunkt wählen und als fxMdl[0] eintragen
    repeat
      X:=random(rHdr.Scn); //zufällige Auswahl
      Y:=random(rHdr.Lin);
    until not isNan(fxImg[0,Y,X]); //nur definierte Pixel
    for B:=0 to high(fxImg) do //alle Kanäle
      fxMdl[0,succ(B)]:=fxImg[B,Y,X]; //neue Merkmale in Null eintragen

    if I>iMap then //nicht in Phase I
    begin
      fMin:=MaxSingle; iFit:=0; //Vorgaben
      for M:=1 to high(fxMdl) do //ähnlichste Klasse suchen
      begin
        fTmp:=0; //Vorgabe
        for B:=1 to length(fxImg) do //alle Kanäle
          fTmp+=sqr(fxMdl[M,B]-fxMdl[0,B]);
        if fTmp<fMin then
        begin
          iFit:=M; //temporär ähnlichte Klasse
          fMin:=fTmp //quadrierte Distanz
        end;
      end;

      iLow:=0; //Vorgabe
      if I<=iMap*cOvr then //nur in Phase II
      begin
        fMin:=MaxSingle; //Vorgabe
        for M:=1 to high(fxMdl) do
          for N:=0 to pred(M) do //alle Klassen-Kombinationen incl. Null
          begin
            fTmp:=0;
            for B:=1 to length(fxImg) do
              fTmp+=sqr(fxMdl[M,B]-fxMdl[N,B]); //quadrierte Distanz
            if fTmp<fMin then
            begin
              iHig:=M;
              iLow:=N; //ähnlichstes Paar
              fMin:=fTmp //kleinste Distanz
            end;
          end;
      end;

      if iLow>0 then //wenn ähnlichstes Paar bei alten Klassen
        for B:=1 to length(fxImg) do
        begin
          fxMdl[iHig,B]:=(fxMdl[iHig,B]+fxMdl[iLow,B])/2; //Klassen 1:1 vereinigen
          fxMdl[iLow,B]:=fxMdl[0,B] //freie Klasse füllen
        end
      else //Stichprobe integrieren
        for B:=1 to length(fxImg) do
          fxMdl[iFit,B]:=(fxMdl[iFit,B]*cRtn + fxMdl[0,B])/succ(cRtn); //Merkmale vereinigen
    end
    else //Klassen-Array verlängern
    begin
      SetLength(fxMdl,succ(length(fxMdl)),length(fxMdl[0])); //neues Neuron
      for B:=1 to length(fxImg) do
        fxMdl[high(fxMdl),B]:=fxMdl[0,B]; //neue Klasse eintragen
    end;
  end;

  Tools.BitWrite(fxMdl,eeHme+cfMdl); //Klassen-Definition als BIT-Tabelle speichern
  Tools.HintOut(true,'Model.FeatureCombinations: '+IntToStr(high(fxMdl)));

  ixMap:=PixelClassify(fxMdl,sImg); //Klassen-IDs als Raster
  Image.WriteThema(ixMap,eeHme+cfMap); //Raster speichern
  Header.WriteThema(iMap,rHdr,'',eeHme+cfMap); //Metadaten speichern
  Tools.HintOut(true,'Model.RolfFeatures: '+cfMap)
end;

{ mFC klassifiziert die scalaren Attribute aller Zonen mit dem Modell "fxMdl"
  und gibt als Ergebnis ein Zonen-Attribut mit allen Klassen zurück. Das erste
  Attribut "fxMdl[?,0]" kann ein Gewicht oder ein Radius sein. Alle anderen
  sind "normale" Merkmale der Zonen. mFC unterstellt, dass die erste Klasse in
  "fxMdl" eine leere Rückweisung ist. }

function tModel.C_lassifySamples(
  fxMdl:tn2Sgl; //Klassen-Vorbild
  sAtr:string): //Attribut-Tabelle, auch erweitert
  tnInt; //Klassen-Attribut
const
  cMdl = 'tMFC: no feature model given!';
var
  faSmp: TnSgl=nil; //Zellmerkmale als Array
  fxAtr: Tn2Sgl=nil; //Feature-Kombination der Zelle "Z"
  B,Z: integer;
begin
  Result:=nil;
  if fxMdl=nil then Tools.ErrorOut(2,cMdl);
  fxAtr:=Tools.BitRead(sAtr); //Attribut-Tabelle lesen
  Result:=Tools.InitInteger(length(fxAtr[0]),0); //alle Zonen incl. Null
  faSmp:=Tools.InitSingle(length(fxMdl[0]),0); //alle Attribute incl. Gewicht
  for Z:=1 to high(fxAtr[0]) do //alle Zonen
  begin
    for B:=1 to high(faSmp) do
      faSmp[B]:=fxAtr[pred(B),Z]; //Merkmale einer Zone
    Result[Z]:=SampleThema(faSmp,MaxSingle,fxMdl); //Klasse
  end; //for Z ..
  Tools.HintOut(true,'Model.FeatureClassify: '+IntToStr(length(Result)));
end;

{ mSM klassifiziert eine Liste mit Stichproben und gibt das Ergebnis als Matrix
  zurück. Das erste Element der Matrix ist eine leere Rückweisungs-Klasse. Das
  erste Merkmal aller Klassen ist die Fläche der Klasse in Pixeln.
    mSM nimmt "iMap" Proben aus der Liste "fxSmp", klassifiziert mit ihnen die
  Liste und bildet neue Klassen aus den Merkmalen der klassifizierten Proben.
  Die neuen Merkmale entsprechen dem Mittelwert aller Proben und ihrer Fläche.
  mSM wiederholt den Prozess bis die Klassifikation konstant ist. }

function tModel.S_ampleModel(
  fxSmp:tn2Sgl; //Stichproben
  iMap:integer): //Anzahl Klassen ohne Rückweisung
  tn2Sgl; //Klassen-Definition aus Stichproben
var
  iaBck:tnInt=nil; //Klassen-Attribut alte Version
  iaThm:tnInt=nil; //Klassen-Attribut
  iMdf:integer; //Veränderungen im Klassen-Attribut
begin
  //Result:=_ModelInit_(fxSmp,iMap); //Auswahl aus Stichproben
  Result:=ModelInit(fxSmp,iMap*3,iMap); //Auswahl aus Stichproben
  SetLength(iaBck,length(fxSmp)); //Speicher
  SetLength(iaThm,length(fxSmp)); //so
  repeat
    move(iaThm[0],iaBck[0],length(iaThm)*SizeOf(integer)); //alter Stand
    iaThm:=SampleClassify(Result,fxSmp); //Klassen-Attribut
    iMdf:=CountDiff(iaBck,iaThm); //Veränderungen zählen
    ModelAdjust(Result,fxSmp,iaThm); //Klassen neu einstellen
    write(#13+IntToStr(iMdf));
  until iMdf<3;
  Tools.BitWrite(Result,eeHme+cfMdl); //als BIT-Tabelle speichern
  Tools.HintOut(true,'Model.FeatureCombinations: '+IntToStr(iMap));
end;

{ mST klassifiziert die Probe "faSmp" mit dem Modell "fxMdl" und gibt die
  Klassen-ID zurück. mST klassifiziert mit (quadrierten) Distanzen im Merkmals-
  Raum. ST ignoriert
  Klassen aus dem Modell, wenn der Distanz-Radius überschritten wird. ST gibt
  die (quadrierte) Distanz zum Modell in "faSmp[0]" zurück. }

function tModel.S_ampleThema(
  faSmp:TnSgl; //Vorbild (Probe)
  fLmt:single; //Maxmum Distanz (quadriert)
  fxMdl:tn2Sgl): //spektrales Modell
  integer; //Klassen-ID
var
  fSed: single; //aktuelle Distanz (quadriert)
  pMdl: ^TnSgl; //Zeiger auf aktuelle Definition
  B,M: integer;
begin
  Result:=0; //Vorgabe
  for M:=1 to high(fxMdl) do //ohne Rückweisung
  begin
    pMdl:=@fxMdl[M]; //Verweis
    fSed:=0;
    for B:=1 to high(faSmp) do
      fSed+=sqr(pMdl^[B]-faSmp[B]); //Summe quadrierte Distanzen
    if fSed<fLmt then //beste Anpassung
    begin
      Result:=M;
      fLmt:=fSed;
    end;
  end;
end;

{ mSS gibt "iSmp" Stichproben aus der Attribut-Tabelle "sAtr" zurück. Die
  Stichproben sind zufällig über die Bildfläche verteilt. Dazu wählt mSS
  einzelne Pixel im Index und gibt die Attribute und die Fläche der
  entsprechenden Zone zurück. Die Samples sind dabei geographisch gleichmäßig
  verteilt. Einzelne Zonen können mehr als einmal getroffen sein. Das Ergebnis
  enthält im Index Null eine leere Rückweisungsklasse. Die Stichproben
  enthalten im Index 1..N die Attribute der Zonen, im Index Null die Fläche der
  Zone in Pixeln. }

function tModel.S_electSamples(
  iSmp:integer; //Anzahl Stichproben
  sAtr:string): //Attribut-Tabelle, auch erweitert
  tn2Sgl; //Stichproben[Probe][Merkmale]
const
  cSmp = 'tMAS: Amount of samples must be greater than 1!';
var
  fxAtr:Tn2Sgl=nil; //spektrale Attribute
  iaSze:tnInt=nil; //Zonen-Größe in Pixeln
  ixIdx:Tn2Int=nil; //Zellindex (Zeiger auf fxTmp[0])
  rHdr:trHdr; //Metadaten
  B,R,X,Y:integer;
begin
  Result:=nil;
  if iSmp<2 then Tools.ErrorOut(2,cSmp);
  Header.Read(rHdr,eeHme+cfIdx); //Metadaten
  ixIdx:=tn2Int(Image.ReadBand(0,rHdr,eeHme+cfIdx)); //Zonen-IDs
  iaSze:=Tools.InitInteger(succ(rHdr.Cnt),0); //Vorgabe = leer
  for Y:=0 to pred(rHdr.Lin) do
    for X:=0 to pred(rHdr.Scn) do
      inc(iaSze[ixIdx[Y,X]]); //Pixel pro Zone

  fxAtr:=Tools.BitRead(sAtr); //Attribut-Tabelle lesen
  Result:=Tools.Init2Single(succ(iSmp),succ(length(fxAtr)),0); //leere Liste
  RandSeed:=cfRds; //Zufalls-Generator initialisieren
  for R:=1 to iSmp do //alle Beispiele, Null für Rückweisung
  begin
    repeat
      Y:=random(length(ixIdx));
      X:=random(length(ixIdx[0]))
    until (ixIdx[Y,X]>0); //nur definierte Orte
    for B:=1 to length(fxAtr) do
      Result[R,B]:=fxAtr[pred(B),ixIdx[Y,X]]; //Merkmale
    Result[R,0]:=iaSze[ixIdx[Y,X]]; //Zonen-Fläche in Pixeln
    iaSze[ixIdx[Y,X]]:=0; //Fläche nur einmal zählen
  end;
  Tools.HintOut(true,'Model.AttributeSamples: '+IntToStr(iSmp));
end;

{ mSC klassifiziert die Feature-Liste "fxSmp" mit den Klassen "fxMdl" und gibt
  das Ergebnis als Attribut für "fxSmp" zurück. mSC klassifiziert nach den
  kleinsten Distanzen im n-dimensionalen Merkmalsraum. }

function tModel.S_ampleClassify(
  fxMdl:tn2Sgl; //Klassen-Definitionen
  fxSmp:tn2Sgl): //Merkmals-Liste
  tnInt; //Klassen-IDs für "fsSmp"
var
  faSed:tnSgl=nil; //kleinste Distanz im Test
  fSed:single; //aktuelle quadrierte Distanz
  F,M,S:integer;
begin
  //high(fxMdl[0])=high(fxSmp[0])?
  Result:=Tools.InitInteger(length(fxSmp),0); //leer
  faSed:=Tools.InitSingle(length(fxSmp),dWord(single(MaxSingle)));
  for S:=1 to high(fxSmp) do //Rückweisung ignorieren
    for M:=1 to high(fxMdl) do //so
    begin
      fSed:=0;
      for F:=1 to high(fxMdl[0]) do //Fläche ignorieren
        fSed+=sqr(fxMdl[M,F]-fxSmp[S,F]); //quadrierte Distanz
      if fSed<faSed[S] then
      begin
        Result[S]:=M; //aktuelle Klasse
        faSed[S]:=fSed //neue Schwelle
      end;
    end;
end;

