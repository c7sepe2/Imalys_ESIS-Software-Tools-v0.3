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

  tArchive = class(tObject) //checked 241116
    private
      function GetElevation(sXml:string):single;
      function _ImportAster_(sArc:string):string;
      function _ImportRapidEye_(sArc:string):string;
      function TarContent(sArc:string; slMsk:tStringList):tStringList;
      procedure TarExtract(sArc:string; slNme:tStringList);
    public
      procedure BandNames(slImg:tStringList);
      function ExtractFilter(sArc,sBnd:string):tStringList;
      function ImportLandsat(rFrm:trFrm; sArc,sBnd:string):string;
      function QueryDate(sDat,sPrd:string):boolean;
      function QueryPosition(rImg,rRoi:trFrm):boolean;
      function QueryQuality(rImg,rRoi:trFrm; var rSct:trFrm; sArc:string):single;
      // vgl: Reduce._SentinelQuality_
      function xCatalog(sMsk:string):tStringList;
      procedure xCompile(iMrg:integer; rFrm:trFrm; sTrg:string; slImg:tStringList);
      procedure xTransform(fPix:double; iEpg:integer; sArt:string; slImg:tStringList);
  end;

  tGdal = class(tObject) //checked 241116
    private
      procedure Warp(iCrs,iPix:integer; sImg,sTrg:string);
    public
      procedure ExportShape(iPrj,iWrp:integer; sSrc,sTrg:string);
      procedure ExportTo(iBnd,iFmt:integer; sNme,sRes:string);
      procedure Hillshade(sDem:string);
      procedure Import(iSgl,iHig,iLow:integer; rFrm:trFrm; sImg,sTrg:string);
      function ImageInfo(sImg:string):string;
      procedure ImportVect(iPrj:integer; sGeo:string);
      function OgrInfo(sVct:string):string;
      procedure Rasterize(iVal:integer; sAtr,sBnd,sVct:string);
      function SrsInfo(sImg:string):string;
      procedure ZonalBorders(sIdx:string);
  end;

  tRank = class(tObject)
    private
      procedure AccuracyMask(sRfz,sThm:string);
      procedure _ChainLine_(faDns:tnSgl; iRds:integer);
      function Combination(sMap,sRfz:string):tn2Int;
      function Correlation(fxVal:tn2Sgl):tn2Sgl;
      function Distribution(sImg,sRfz:string):tn3Sgl;
      function MaxCover(ixCmb:tn2Int; lsSpc:tFPList):tnInt;
      procedure _MeanLine_(faDns:tnSgl; iRds:integer);
      procedure _Median_(faDns:tnSgl; iRds:integer);
      function _NormDiff_(faDns:tnSgl):single;
      function _Outlier(faDns:tnSgl; fLmt:single):single;
      procedure Remap(iaLnk:tnInt; sMap:string);
      procedure ReportOut(lsSpc:tFPList);
      procedure _SortByte_(iMap:integer; ixMap:tn2Byt);
      procedure TableFormat(iFmt:integer; ixCnt:tn2Int; sRes:string);
    public
      procedure FieldToMap(iCrs:integer; sFld,sMap,sRfz:string);
      procedure _xEqualize(iRds:integer; sImg:string);
      procedure xScalarFit(bAcy:boolean; sImg,sRfz:string);
      procedure xThemaFit(bAcy:boolean; sMap,sRfz:string);
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
  cuPrd: trPrd = (Mea:0; Vrz:0; Low:0; Hig:-1);

var
  Archive:tArchive;
  Gdal:tGdal;
  Rank:tRank;
  Separate:tSeparate;

implementation

uses
  raster, thema, vector;

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

procedure tRank.AccuracyMask(sRfz,sThm:string);
{ rAM löscht aus dem Klassen-Layer "sThm" alle Pixel, die nicht mit der
  Referenz "sRfz" identisch sind. }
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
  Image.WriteThema(ixThm,eeHme+cfAcy);
  Header.WriteThema(rHdr.Cnt,rHdr,rHdr.Fld,eeHme+cfAcy);
end;

function tRank.Combination(
  sMap:string; //Clusterung
  sRfz:string): //Referenz
  tn2Int; //Pixel pro Cluster x Referenz
{ rCn zählt alle Referenz-Cluster-Kombinationen in den thematischen Bildern
  "sRfz" und "sMap" und gibt sie als Tabelle[Mapping][Reference] zurück. Die
  Bilder "sMap" und "sRfz" müssen deckungsgleich sein. rCn belegt die erste
  Spalte der Tabelle mit den Pixeln pro Referenz un die erste Zeile mit den
  Pixeln pro Cluster(Mapping). Result[0,0] gibt die Summe aller erfassten Pixel
  zurück. rCn speichert die Tabelle unverändert als tab-getrennten Text. }
const
  cHdr = 'rCn: Mapping and reference differ in size or format!';
var
  iMap:integer; //höchste Cluster-ID
  ixMap:tn2Byt=nil; //Clusterung als Bild
  ixRfz:tn2Byt=nil; //Referenzen als Bild
  rHdr:trHdr; //Metadaten
  M,R,X,Y:integer;
begin
  Result:=nil;
  Header.Read(rHdr,sMap); //Clusterung
  if not Header.BandCompare(rHdr,sRfz) then Tools.ErrorOut(2,cHdr);
  //Projektion?
  ixMap:=Image.ReadThema(rHdr,sMap);
  iMap:=rHdr.Cnt; //höchste Cluster-ID
  Header.Read(rHdr,sRfz); //Referenz
  ixRfz:=Image.ReadThema(rHdr,sRfz);
  Result:=Tools.Init2Integer(succ(iMap),succ(rHdr.Cnt),0);
  for Y:=0 to pred(rHdr.Lin) do
    for X:=0 to pred(rHdr.Scn) do
      if ixRfz[Y,X]>0 then
        inc(Result[ixMap[Y,X],ixRfz[Y,X]]); //Kombinationen
  for M:=1 to high(Result) do
    for R:=1 to high(Result[0]) do
    begin
      Result[M,0]+=Result[M,R]; //Pixel pro Cluster (Mapping)
      Result[0,R]+=Result[M,R]; //Pixel pro Referenz
      Result[0,0]+=Result[M,R]; //alle Pixel
    end;
end;

{ rDn bestimmt Mittelwert und Varianz aller Kanäle in den Bilddaten "sImg"
  bezogen auf die referenzierten Flächen in "sRfz" und gibt das Ergebnis als
  Tabelle zurück. }
{ rDn bildet Summe und Quadrat-Summe aller Werte in den verschiedenen Kanälen
  innerhalb der verschiedenen Referenzen und berechnet damit Mittelwert und
  Varianz für alle Referenz-Kanal Kombinationen. rDn gibt die Varianz als
  doppelte Abweichung zurück. Die Indices sind natürliche Zahlen ab Eins, die
  erste Zeile und erste Spalte ist nicht definiert! }
// Varianz = (∑x²-(∑x)²/n)/(n-1)

function tRank.Distribution(
  sImg:string; //Werte in scalarem Bild
  sRfz:string): //Klassen-Layer (Clusterung)
  tn3Sgl; //[Mittwelwert|Varianz][Referenzen][Kanäle]
const
  cNeg = 'rDn: Negative results for variance!';
var
  bNgv:boolean=False;
  fVrz:single; //Zwischenergebnis
  fxVal:tn2Sgl=nil; //Kanal aus Scalarem Bild
  fzVrz:tn2Sgl=nil; //Zeiger auf Varianz
  fzMdn:tn2Sgl=nil; //Zeiger auf Mittelwert
  iaCnt:tnInt=nil; //Pixel pro Cluster
  iCnt:integer; //Anzahl Cluster
  ixRfz:tn2Byt=nil; //Clusterung oder Klassen-Layer
  rHdr:trHdr; //Metadaten
  B,R,X,Y:integer;
begin
  //sMap = Klassen?
  //sVal = Scalar?
  //gleiche Größe?
  Header.Read(rHdr,sRfz); //Metadaten Cluster
  ixRfz:=Image.ReadThema(rHdr,sRfz); //Cluster, Klassen
  iaCnt:=Tools.InitInteger(succ(rHdr.Cnt),0); //Pixel pro Cluster
  for Y:=0 to pred(rHdr.Lin) do
    for X:=0 to pred(rHdr.Scn) do
      inc(iaCnt[ixRfz[Y,X]]); //Pixel pro Referenz

  iCnt:=rHdr.Cnt; //höchte Cluster-ID
  Header.Read(rHdr,sImg); //Metadaten Scalare
  Result:=Tools.Init3Single(2,succ(iCnt),succ(rHdr.Stk),0);
  fzMdn:=Result[0]; //Zeiger
  fzVrz:=Result[1];
  for B:=1 to rHdr.Stk do //alle Kanäle
  begin
    fxVal:=Image.ReadBand(pred(B),rHdr,sImg); //scalarer Kanal
    for Y:=0 to pred(rHdr.Lin) do
      for X:=0 to pred(rHdr.Scn) do
      begin
        fzVrz[ixRfz[Y,X],B]+=sqr(fxVal[Y,X]);
        fzMdn[ixRfz[Y,X],B]+=fxVal[Y,X];
      end;
  end;
  for R:=1 to iCnt do //alle Referenzen
    for B:=1 to rHdr.Stk do
    begin
      if iaCnt[R]>1
        then fVrz:=(fzVrz[R,B]-(sqr(fzMdn[R,B])/iaCnt[R]))/(pred(iaCnt[R]))
        else fVrz:=0;
      if fVrz>=0
        then fzVrz[R,B]:=sqrt(fVrz)*2 //Doppelte Abweichung
        else bNgv:=True; //Rundungsfehler!
      if iaCnt[R]>0 then
        fzMdn[R,B]:=fzMdn[R,B]/iaCnt[R];
    end;
  if bNgv then Tools.ErrorOut(2,cNeg+sRfz);
  Tools.HintOut(true,'Rank.Distribution: memory');
end;

function tRank.MaxCover(
  ixCmb:tn2Int; //Cluster-Referenz-Kombinationen [Pixel]
  lsSpc:tFPList): //Kombinationen als Liste
  tnInt; //Klassen für Cluster aus Referenzen
{ rSy gibt ein Array mit den häufigsten Kombination zwischen Zeilen (Clustern)
  und Spalten (Referenzen) in "ixCmb" zurück. rSy erwartet in "ixCmb[0,0]" die
  Anzahl aller referenzierten Pixel. }
{ rSy erzeugt aus der Tabelle "ixCmb" eine Liste mit allen Cluster-Referenz-
  Kombinationen, sortiert sie nach der Fläche der Cluster in einer Referenz
  und sortiert sie nach der gemeinsamen Fläche "pSpc^.Prt". rSy gibt für jeden
  Cluster eine ID zurück, eine Referenz kann durch viele Cluster abgebildet
  werden. }
var
  pSpc:tprSpc=nil; //Zeiger auf Kombination
  I,M,R:integer;
begin
  Result:=Tools.InitInteger(length(ixCmb),0); //beste Referenz pro Cluster
  for M:=1 to high(ixCmb) do
    for R:=1 to high(ixCmb[0]) do
      if ixCmb[M,R]>0 then
      begin
        new(pSpc);
        pSpc^.Rfz:=R;
        pSpc^.Map:=M;
        pSpc^.Val:=ixCmb[M,R]/ixCmb[0,0]; //Anteil Cluster in Referenz
        lsSpc.Add(pSpc)
      end;
  lsSpc.Sort(@SpcValues); //nach Fläche sortieren
  for I:=0 to pred(lsSpc.Count) do
    with tprSpc(lsSpc[I])^ do
      if Result[Map]=0
        then Result[Map]:=Rfz //Cluster mit Referenz verknüpfen
        else Val:=-Val; //Fehler-Markierung
end;

procedure tRank.Remap(
  iaLnk:tnInt; //Klassen-ID aus Referenz
  sMap:string); //Clusterung
{ rRp ersetzt die Klassen in "sMap" mit der Transformation "iaLnk" durch neue
  Werte und speichert das Ergebnis als "thema" im Home-Verzeichnis. rRp liest
  und schreibt Bilder im ENVI-Byte-Format (Klassen). }
var
  ixMap:tn2Byt=nil; //Klassen-Bild
  rHdr:trHdr; //Metadaten
  sFld:string=''; //Feldnamen
  Y,X:integer;
begin
  Header.Read(rHdr,sMap); //Clusterung
  ixMap:=Image.ReadThema(rHdr,sMap);
  for Y:=0 to high(ixMap) do
    for X:=0 to high(ixMap[0]) do
      ixMap[Y,X]:=iaLnk[ixMap[Y,X]];
  Image.WriteThema(ixMap,eeHme+cfThm);
  sFld:=Header.ReadLine('class names',eeHme+cfRfz);
  Header.WriteThema(MaxIntValue(iaLnk),rHdr,sFld,eeHme+cfThm);
end;

procedure tRank.ReportOut(
  lsSpc:tFPList); //Cluster-Referenz-Verteilung
{ rRO überträgt die Cluster-Referenz-Combination "lsSpc" in einen formatierten
  Text. Dazu fassr rRO alle Einträge mir gleicher Referenz-ID zu einer Zeile
  zusammen. Das Ergebnis enthält die Summe der richtig und falsch verknüpften
  Pixel und die IDs der beteiligten Cluster. rRO unterdrückt bei den ID's
  Cluster mit weniger als 0.1% Anteil, die Flächen-Summen sind vollständig. }
var
  fErr:single=0; //Anteil falsch
  fHit:single=0; //Anteil richtig
  sErr:string=''; //Cluster-IDs falsch
  sHit:string=''; //Cluster-IDs richtig

function lLineOut(iRfz:integer):string;
begin
  if length(sErr)>0 then delete(sErr,1,1); //führendes Komma
  if length(sHit)>0 then delete(sHit,1,1);
  Result:=IntToStr(iRfz)+#9+
    FloatToStrF(fHit*100,ffFixed,7,1)+#9+
    FloatToStrF(fErr*100,ffFixed,7,1)+#9+
    FloatToStrF((fHit+fErr)*100,ffFixed,7,1)+#9+
    '('+sHit+') – ('+sErr+')';
  fHit:=0; fErr:=0;
  sHit:=''; sErr:='';
end;

var
  fNgv:single=0; //Summe negative Anteile
  fPst:single=0; //Summe positive Anteile
  slRes:tStringList=nil; //Ergebnis als Text
  I:integer;
begin
  try
    slRes:=tStringList.Create;
    slRes.Add('Refz-ID'#9'Link %'#9'Error %'#9'Bilanz %'#9'Cluster-IDs');
    lsSpc.Sort(@SpcRfzMap); //Nach Referenz und Fläche sortieren
    for I:=0 to pred(lsSpc.Count) do
    with tprSpc(lsSpc[I])^ do
    begin
      if (I>0) and (tprSpc(lsSpc[pred(I)])^.Rfz<Rfz) then
        slRes.Add(lLineOut(tprSpc(lsSpc[pred(I)])^.Rfz));
      if Val>0
        then fHit+=Val
        else fErr+=Val; //Flächen-Anteile richtig / falsch
      if Val>1/1000 then sHit+=','+IntToStr(Map) else
      if Val<-1/1000 then sErr+=','+IntToStr(Map); //Cluster-ID richtig / falsch
      if Val>0
        then fPst+=Val
        else fNgv+=Val;
    end;
    slRes.Add(lLineOut(tprSpc(lsSpc.last)^.Rfz));
    slRes.Add('Sum'+
      #9+FloatToStrF(fPst*100,ffFixed,7,1)+
      #9+FloatToStrF(fNgv*100,ffFixed,7,1)+
      #9+FloatToStrF((fPst+fNgv)*100,ffFixed,7,1));
    slRes.SaveToFile(eeHme+cfSpc);
  finally
    slRes.Free;
  end
end;

procedure tRank.TableFormat(
  iFmt:integer; //0=unverändert, 1=Anteile Bild, 2=Anteile Cluster, negativ=Stellen als Float
  ixCnt:tn2Int; //Integer-Tabelle
  sRes:string); //Dateiname Ergebnis als Text
{ rTF schreibt eine Tabelle als tab-getrennten Text. rTF unterstellt, dass die
  erste Zeile und erste Spalte Hilfswerte enthalten und ersetzt sie durch die
  passenden Indices. }
{ Mit "iFmt<0" interpretiert rTF "ixCnt" als Single-Matrix und schreibt alle
  Werte mit abs(iFmt) Stellen. Mit "iFmt=0" schreibt rTF Integers. Mit "iFmt>0"
  normalisiert rTF die Werte auf die Summe der Zeilen, Spalten oder aller Werte.
  Dazu muss die erste Zeile bzw Spalte die Summen enthalten und die Summe aller
  Werte in "ixCnt[0,0]" stehen. In die Ausgabe ersetzt rTF in jedem Fall die
  erste Zeile und die erste Spalte durch die passenden Indices. }
var
  sLin:string; //aktuelle Textzeile
  slOut:tStringList=nil;
  C,R:integer;
begin
  try
    slOut:=tStringList.Create;
    sLin:='0'; //erste Zelle
    for C:=1 to high(ixCnt[0]) do
      sLin+=#9+IntToStr(C); //erste Zeile = Indices
    slOut.Add(sLin);
    for R:=1 to high(ixCnt) do //Zeilen
    begin
      sLin:=IntToStr(R); //erste Spalte = Index
      for C:=1 to high(ixCnt[0]) do //Werte in Spalten
        case iFmt of
          0:sLin+=#9+IntToStr(ixCnt[R,C]); //unverändert
          1:sLin+=#9+FloatToStrF(ixCnt[R,C]/ixCnt[0,0]*1000,ffFixed,7,0); //Anteil Gesamtfräche
          2:sLin+=#9+FloatToStrF(ixCnt[R,C]/ixCnt[0,C],ffFixed,7,0); //Anteil Spalte
          3:sLin+=#9+FloatToStrF(ixCnt[R,C]/ixCnt[R,0],ffFixed,7,0); //Anteil Zeile
          else sLin+=#9+FloatToStrF(tn2Sgl(ixCnt)[R,C],ffFixed,7,-iFmt); //Werte als Float, "iFmt" Stellen
        end;
      slOut.Add(sLin);
    end;
    slOut.SaveToFile(sRes)
  finally
    slOut.Free;
  end;
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
  if not FileExists(sGeo) then Tools.ErrorOut(2,cSrc+sGeo);
  DeleteFile(eeHme+cfVct);
  DeleteFile(eeHme+cfVct+'t');
  try
    slCmd:=TStringList.Create;
    slCmd.Add('-f'); //Output-File-Format
    slCmd.Add('CSV'); //als Comma Seperated Values
    slCmd.Add('-overwrite'); //neue Datei
    slCmd.Add('-lco'); //Layer Creation Option
    slCmd.Add('GEOMETRY=AS_WKT'); //Geometrie als WKT
    slCmd.Add('-lco');
    slCmd.Add('CREATE_CSVT=YES'); //Format der Attribute
    slCmd.Add('-t_srs'); //projizieren in Format
    slCmd.Add('EPSG:'+IntToStr(iPrj)); //als EPSG
    slCmd.Add(eeHme+cfVct); //Ziel = ".imalys/vector.csv"
    slCmd.Add(sGeo); //Quelle
    Tools.OsExecute(eeGdl+'ogr2ogr',slCmd);
    Tools.ErrorLog('gIV:'); //Exceptions speichern
  finally
    slCmd.Free;
    if not FileExists(eeHme+cfVct) then Tools.ErrorOut(2,cGdl+sGeo);
  end;
  Tools.HintOut(true,'GDAL.Import: '+cfVct);
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
  if not FileExists(sBnd) then Tools.ErrorOut(2,cSrc+sBnd);
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

{ rCe erzeugt ein Klassen-Bild "reference" aus der Vektor-Geometrie "sRfz" und
  dem Vektor-Attribut "sFld". Raster und Abdeckung sind identisch mit "sMap".
  rCe importiert das Vektor-Vorbild als CSV, klassifiziert das Feld "sFld",
  erweitert die Attribute in der CSV-Kopie um eine fortlaufende Klassen-ID und
  verwendet die ID als Wert für das Bild. rCe interpretiert die Werte in "sFld"
  als Strings und übernimmt sie als Klassen-Namen. rCe ignoriert Vektoren
  außerhalb von "sMap". }

procedure tRank.FieldToMap(
  iCrs:integer; //Projektion als EPSG
  sFld:string; //Feldname für Referenz-ID in Vektor-Tabelle
  sMap:string; //Klassifikation (Testobjekt)
  sRfz:string); //Klassen-Referenz (Vektoren)
var
  rHdr:trHdr; //Vorbild (Clusterung)
  slRfz:tStringList=nil; //Klassen-Namen aus Referenz
  sNme:string='NA'; //Klassen-Namen, kommagetrennt
  I:integer;
begin
  Gdal.ImportVect(iCrs,sRfz); //Referenz als "vector.csv speichern, Umprojektion!
  try
    slRfz:=Table.AddThema(sFld); //Namen der Referenz-Klassen, Klassen-IDs in "focus.csv"
    Header.Read(rHdr,sMap); //Dimension Vorbild
    Image.WriteZero(rHdr.Scn,rHdr.Lin,eeHme+cfRfz); //leere Kopie erzeugen
    for I:=1 to pred(slRfz.Count) do
      sNme+=','+slRfz[I];
    Header.WriteThema(pred(slRfz.Count),rHdr,sNme,eeHme+cfRfz); //Kassen-Header dazu
  finally
    slRfz.Free;
  end;
  Gdal.Rasterize(0,sFld+'-ID',eeHme+cfRfz,eeHme+cfFcs); //Klassen-IDs einbrennen
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

function tRank.Correlation(
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

{ rSF bestimmt eine Rang-Korrelation zwischen allen Kanälen im Import und der
  Referenz "sRfz". rSF gibt Mittelwerte, Abweichung (2@) und Korrelation als
  Text-Tabellen zurück. }

procedure tRank.xScalarFit(
  bAcy:boolean; //Mittelwert und Abweichung als Tabelle
  sImg:string; //Vorbild
  sRfz:string); //Referenz als Raster-Bild
var
  fxDst:tn3Sgl=nil; //Mittelwert+Variank für alle Kombinationen
  fxRnk:tn2Sgl=nil; //Rang-Korrelation für alle Kombinationen
begin
  fxDst:=Rank.Distribution(sImg,sRfz); //Mittelwert, Varianz in Referenzen
  if bAcy then TableFormat(-3,tn2Int(fxDst[0]),eeHme+'meanvalues.tab');
  if bAcy then TableFormat(-3,tn2Int(fxDst[1]),eeHme+'deviation.tab');
  fxRnk:=Correlation(fxDst[0]); //Korrelation für alle Referenzen
  TableFormat(-2,tn2Int(fxRnk),eeHme+'correlation.tab');
end;

{ rTF überträgt Klassen-IDs der Referenz "sRfz" auf die Clusterung "sMap" und
  speichert das Ergebnis als "thema". Die Referenzen müssen als Raster-Layer
  verfügbar sein. }
{ rTF zählt die Cluster-Referenz-Kombinationen aller referenzierten Pixel in
  "ixCmb", erzeugt eine Liste "lsSpc" der C/R-Kombinationen, sortiert sie nach
  der Fläche der Cluster in den Referenzen und vergibt für alle Cluster die ID
  der Referenz mit der größten Fläche. Mit "bAcy=true" speichert rTF "ixCmb"
  als "combination", eine Zusammenfassung der Liste "lsSpc" als "specificity"
  und einen Klassen-Layer "accuracy", in dem nur korrekt abgebildete Referenzen
  sichtbar sind. }

procedure tRank.xThemaFit(
  bAcy:boolean; //Accuracy-Kontrollen erzeugen
  sMap,sRfz:string); //Cluster, Referenz als thematisches Bild
var
  iaLnk:tnInt=nil; //referenzierte Klassen-IDs
  ixCmb:tn2Int=nil; //Anzahl Cluster-Referenz-Kombinationen
  lsSpc:tFPList=nil; //Kombinationen nach Häufigkeit
  I:integer;
begin
  //Projektion?
  try
    ixCmb:=Combination(sMap,sRfz); //Cluster-Referenz-Kombinationen + Summen
    lsSpc:=tFPList.Create; //Cluster-Klassen-Kombinationen
    iaLnk:=MaxCover(ixCmb,lsSpc); //Cluster-IDs aus Referenz
    Remap(iaLnk,sMap); //Cluster-IDs durch Referenz ersetzen
    if bAcy then
    begin
      ReportOut(lsSpc); //Kombinationen als Text
      TableFormat(0,ixCmb,eeHme+cfCbn); //als Text-Tabelle
      AccuracyMask(sRfz,eeHme+cfThm) //Filter für Cluster=Referenz
    end;
  finally
    for I:=0 to pred(lsSpc.Count) do
      dispose(tprSpc(lsSpc[I])); //Speicher freigeben
    lsSpc.Free;
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
  if not FileExists(sImg) then Tools.ErrorOut(2,cImg+sImg);
  try
    slPrm:=TStringList.Create;
    //slPrm.Add('-stats'); //bei Bedarf Statistik erzeugen
    //slPrm.Add('-hist'); //Histogramm-Werte anzeigen
    slPrm.Add('-proj4'); //Projektion als "proj4"-String
    slPrm.Add(sImg); //Quell-Datei
    Tools.OsExecute(eeGdl+'gdalinfo',slPrm); //modal ausführen
    Result:=Tools.GetOutput(Tools.prSys); //GDAL Image Information (Text)
    Tools.ErrorLog('gII'); //Exceptions speichern
    if length(Result)=0 then Tools.ErrorOut(2,cGdl);
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
  if FileExists(sIdx+cShp)=False then Tools.ErrorOut(2,cRes+sIdx+cShp);
  Tools.HintOut(true,'gdal.ZoneBorders: '+ExtractFileName(sIdx));
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
  if (iFmt<1) or (iFmt>5) then Tools.ErrorOut(2,cFmt+IntToStr(iFmt));
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
{   TODO: Gdal.ExportTo könnte mit -expand eine Palette ermöglichen. }
    slCmd.Add(sNme); //Vorbild
    slCmd.Add(sRes); //Ergebnis
    Tools.OsExecute(eeGdl+'gdal_translate',slCmd);
    Tools.ErrorLog('gET:'); //bei Exceptions anhalten
  finally
    slCmd.Free;
  end;
  if FileExists(sRes)=False then Tools.ErrorOut(2,cRes+sRes);
  Tools.HintOut(true,'GDAL.Export: '+ExtractFileName(sRes));
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
  iDim: integer; //Maximum Dimensionen
  sImg: string); //Pfadname Vorbild
const
  //cDim = '[Maximum Dimension] input must be larger than 0!';
  cFex = 'sPl: Image not found: ';
  cStk = 'Given image needs at least two image bands';
var
  fxBnd:Tn2Sgl=nil; //Basis-Kanal für erste Hauptkomponente
  fxCmp:Tn2Sgl=nil; //Vergleichs-Kanal
  sBnd:string=''; //Kanal-Namen, kommagetrennt
  B,C:integer;
begin
  if not FileExists(sImg) then Tools.ErrorOut(2,cFex+sImg);
  //if iDim<1 then Tools.ErrorOut(2,cDim);
  iDim:=max(iDim,1); //mindestens erste Hauptkomponente
  Header.Read(rcHdr,sImg); //Header wird verändert!
  if rcHdr.Stk<2 then Tools.ErrorOut(2,cStk);
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
      Filter.ValueMove(-Tools.MinBand(fxBnd),fxCmp); //alle Werte positiv machen
      Image.WriteBand(fxCmp,B,eeHme+cfPca); //Ergebnis als Zwischenlager für "Reste"
      write('.') //Fortschritt
    end;
    write(#13);
    Filter.ValueMove(-Tools.MinBand(fxBnd),fxBnd); //alle Werte positiv machen
    Image.WriteBand(fxBnd,C,eeHme+cfPca); //Ergebnis "C" dauerhaft speichern
    sImg:=eeHme+cfPca; //ab jetzt "Reste"-Kanäle als Vorbild
    Tools.HintOut(true,'Separate.Principal: '+IntToStr(succ(C))+'/'+IntToStr(iDim)+
      ': '+cfPca);
  end;
  for C:=1 to iDim do
    sBnd+='PC-'+IntToStr(C)+#10; //Kanal-Bezeichner
  Header.WriteMulti(rcHdr,sBnd,sImg);
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
  if not FileExists(sVct) then Tools.ErrorOut(2,cSrc+sVct);
  try
    slCmd:=TStringList.Create;
    slCmd.Add('-al'); //alle Layer anzeigen ← für Vergleich mit CSV-Version
    slCmd.Add('-so'); //nur Zusammenfassung
    slCmd.Add(sVct); //Quelle
    Tools.OsExecute(eeGdl+'ogrinfo',slCmd);
    Result:=Tools.GetOutput(Tools.prSys); //GDAL Image Information (Text)
    Tools.ErrorLog('gOI:'); //Exception speichern
    if length(Result)=0 then Tools.ErrorOut(2,cGdl);
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
  if not FileExists(sImg) then Tools.ErrorOut(2,cImg+sImg);
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
  if not FileExists(eeHme) then Tools.ErrorOut(2,cHme);
  if not FileExists(sDem) then Tools.ErrorOut(2,cSrc+sDem);
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
  if not FileExists(eeHme+cfHse) then Tools.ErrorOut(2,cRes+eeHme+cfHse);
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
  if not FileExists(sArc) then Tools.ErrorOut(2,cArc+sArc);
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

procedure tArchive.TarExtract(
  sArc:string; //Archiv-Name
  slNme:tStringList); //gewählte Namen im Archiv
const
  cCmd = 'tar';
var
  slPrm:tStringList=nil; //Parameter für "tar"-Befehl
begin
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

{ pEF liest Kanäle aus dem Artćhiv "sArc", die zu den Strings in "sBnd" passen.
  pEF liest das Inhaltsverzeichnis des Archivs "sArc", reduziert es auf Namen
  die in "sBnd" vorkommen und extrahiert die gefilterten Namen. Der Befehl
  "tar" extrahiert in das Stamm-Verzeichnis wenn keine Pfade gesetzt sind,
  unter gdb in das Verzeichnis des aufrufenden Programms. "sBnd" kann eine
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
    slMsk.Text:=Tools.CommaToLine(sBnd); //Liste aus CSV
    Result:=TarContent(sArc,slMsk); //Dateinamen im Archiv lesen und filtern
    if (Result<>nil) and (Result.Count>0) then
    begin
      TarExtract(sArc,Result); //Namen in "Result" extrahieren
      sDir:=Tools.OsCommand('pwd','')+DirectorySeparator;
      //writeln('pwd = '+sDir);
      {if FileExists(ExtractFilePath(eeExc)+Result[0])=False
        then sDir:=Tools.SetDirectory('/home/'+Tools.OsCommand('whoami','')) //Stammverzeichnis
        else sDir:=ExtractFilePath(eeExc); //Verzeichnis der ausführbaren Datei}
      for I:=pred(Result.Count) downto 0 do
        if FileExists(sDir+Result[I]) //Extraktion erfolgreich
          then Result[I]:=sDir+Result[I] //vollständiger Pfadname
          else Result.Delete(I); //kein Ergebnis
    end
    else Tools.ErrorOut(0,cArc+sArc) //Nachricht, keine Unerbrechung
  finally
    slMsk.Free;
  end;
  if (Result<>nil) and (Result.Count>0)
    then Tools.HintOut(true,'Archive.ExtractF: '+sBnd)
    else FreeAndNil(Result); //nil zurückgeben
end;

{ aQD vergleicht das Datum "sDat" mit der Zeitperiode "sPrd". aQD ist wahr,
  wenn Jahr, Monat und Tag aus "sDat" zu den Grenzen in "sPrd" passen. Alle
  Datumsangaben müssen als YYYYMMDD formatiert sein. }

function tArchive.QueryDate(
  sDat:string; //Datum im Vorbild YYYYMMDD
  sPrd:string): //Periode als YYYYMMDD-YYYYMMDD
  boolean; //Übereinstimmung
const
  cDat = 'aQD: Dates must be passed as "YYYYMMDD": ';
var
  iDat,iHig,iLow:integer; //Datum als Zahl
begin
  Result:=False;
  if (TryStrToInt(sDat,iDat)=False)
  or (TryStrToInt(LeftStr(sPrd,8),iLow)=False)
  or (TryStrToInt(RightStr(sPrd,8),iHig)=False)
  then Tools.ErrorOut(2,cDat+sDat);
  Result:=(iDat>=iLow) and (iDat<=iHig);
end;

{ rOl gibt für jeden Bildpixel in einem Stack (Zeitreihe) den Quotient zwischen
  Abweichung und Mittelwert zurück. Ausreißer und Veränderungen werden mit
  hohen Werten abgebildet. }

function tRank._NormDiff_(
  faDns:tnSgl): //Werte-Reihe
  single; //Abweichung/Helligkeit
// Varianz = (∑x²-(∑x)²/n)/(n-1)
var
  fSum:double=0;
  fSqr:double=0;
  fVrz:double=0;
  I:integer;
begin
  for I:=0 to high(faDns) do
  begin
    fSqr+=sqr(faDns[I]);
    fSum+=faDns[I];
  end;
  fVrz:=(fSqr-sqr(fSum)/length(faDns))/high(faDns); //Varianz ACHTUNG Rundung!
  Result:=sqrt(max(fVrz,0))/fSum*length(faDns); //Relativ zur Helligkeit
end;

{ rCL dämpft die Zeitreihe "faDns" mit einem gewichteten Mittelwert in einem
  beweglichen Fenster. Das Fenster ist "iRds"*2+1 Punkte lang, am Anfang und
  Ende der Zeitreihe kürzer. Der Mittelpunkt im Fenster hat das höchste
  Gewicht, Nachbarn sind mit dem Quadrat der Distanz reduziert. }

procedure tRank._ChainLine_(
  faDns:tnSgl; //Zeitreihe
  iRds:integer); //Fang-Radius
const
  cTms = sqrt(2);
var
  faTmp:tnSgl=nil; //Zwischenlager
  fDst:single; //Distanz Wert und Zeit
  fVal:single; //Summe gewichtete Werte
  fWgt:single; //Summe gewichtete Distanzen
  I,R:integer;
begin
  SetLength(faTmp,length(faDns));
  move(faDns[0],faTmp[0],length(faDns)*SizeOf(single)); //Kopie als Backup
  for I:=0 to high(faDns) do
  begin
    fVal:=faDns[I]; //Vorgabe
    fWgt:=1; //Vorgabe
    for R:=I-iRds to I+iRds do
    begin
      if (R<0) or (R=I) or (R>high(faDns)) then continue;
      fDst:=power(cTms,abs(I-R)); //Distanz-Faktor
      fWgt+=1/fDst; //Summe Gewichte
      fVal+=faTmp[R]/fDst //Summe gewichtete Werte
    end;
    if fWgt>0 then
      faDns[I]:=fVal/fWgt;
  end;
end;

{ rMn bildet den Median aus allen übergebenen Kanälen. Dazu kopiert rMn alle
  Werte eines Pixels nach "fxDev", sortiert "fxDev" mit "QuickSort" und
  übernimmt den Wert in der Mitte der gültigen Einträge in "fxDev". rMn kopiert
  NoData Werte in den Bilddaten nicht nach "faDev" sondern reduziert mit "iDim"
  die gültigen Stellen in "faDev". }

procedure tRank._Median_(
  faDns:tnSgl; //Zeitreihe, wird verändert!
  iRds:integer);
var
  faPrt:tnSgl=nil; //Zeitreihe-Fenster
  faTmp:tnSgl=nil; //Puffer für "faDns"
  iCnt:integer; //gültige Zeitpunkte
  I,R: integer;
begin
  SetLength(faTmp,length(faDns));
  move(faDns[0],faTmp[0],length(faDns)*SizeOf(single)); //Kopie als Backup
  SetLength(faPrt,succ(iRds*2));
  for I:=0 to high(faDns) do
  begin
    iCnt:=0;
    for R:=I-iRds to I+iRds do
    begin
      if (R<0) or (R>high(faDns)) then continue;
      faPrt[iCnt]:=faTmp[R];
      inc(iCnt);
    end;
    Reduce.QuickSort(faPrt,iCnt); //ordnen
    faDns[I]:=faPrt[trunc(iCnt/2)] //Median
  end;
end;

// Zeitverlauf stark dämpfen
// NoData von Kanal 1 muss für alle Kanäle gelten
// Outlier erzeugt Suchbild = Abweichung / Helligkeit
// Median entfernt Ausreißer

procedure tRank._xEqualize(
  iRds:integer; //Fang-Radius
  sImg:string); //Vorbild
const
  //cLmt = 0.3;
  cLmt = 5;
  cStk = 'rSg: The time course must contain at least tree layers: ';
var
  fNan:single=NaN; //NoData
  faDns:tnSgl=nil; //Werte für einen Pixel
  fxOtl:tn2Sgl=nil; //Kontroll-Bild
  fxStk:tn3Sgl=nil; //Stack aus Zeitpunkten
  rHdr:trHdr; //Metadata
  S,X,Y:integer;
begin
  Header.Read(rHdr,sImg);
  if rHdr.Stk<3 then Tools.ErrorOut(2,cStk+sImg);
  faDns:=Tools.InitSingle(rHdr.Stk,0); //ein Pixel, alle Zeitpunkte
  fxStk:=Image.Read(rHdr,sImg); //Stack aus Zeitpunkten
  fxOtl:=Tools.Init2Single(rHdr.Lin,rHdr.Scn,dWord(fNan));

  for Y:=0 to pred(rHdr.Lin) do
    for X:=0 to pred(rHdr.Scn) do
    begin
      if isNan(fxStk[0,Y,X]) then continue;
      for S:=0 to pred(rHdr.Stk) do
        faDns[S]:=fxStk[S,Y,X]; //Zeit-Array aus einem Pixel
      //fxOtl[Y,X]:=NormDiff(faDns); //normalisierte Abweichung
      //Median(faDns,iRds); //outlier entfernen
      //ChainLine(faDns,iRds); //LowPass
      //fxOtl[Y,X]:=_Outlier(faDns,iRds); //"Outlier" UND "Stack" füllen
      //_MeanLine(faDns,iRds); //Mittelwerte
      fxOtl[Y,X]:=_Outlier(faDns,cLmt);
// Schwellen scheinen nie zu funktionieren
      for S:=0 to pred(rHdr.Stk) do
        fxStk[S,Y,X]:=faDns[S]; //Ausgleich als Bild
    end;
  Image.WriteMulti(fxStk,eeHme+'equalize');
  Header.WriteMulti(rHdr,rHdr.aBnd,eeHme+'equalize');
  Image.WriteBand(fxOtl,0,eeHme+'outlier');
  Header.WriteScalar(rHdr,eeHme+'outlier');
end;

// arithmetisch mitteln

procedure tRank._MeanLine_(
  faDns:tnSgl; //Zeitreihe
  iRds:integer); //Fang-Radius
const
  cTms = sqrt(2);
var
  faTmp:tnSgl=nil; //Zwischenlager
  fSum:single; //Summe Werte
  iCnt:integer; //Anzahl Werte
  I,R:integer;
begin
  SetLength(faTmp,length(faDns));
  move(faDns[0],faTmp[0],length(faDns)*SizeOf(single)); //Kopie als Backup
  for I:=0 to high(faDns) do
  begin
    fSum:=0; iCnt:=0; //Vorgabe
    for R:=I-iRds to I+iRds do
    begin
      if (R<0) or (R>high(faDns)) then continue;
      fSum+=faTmp[R]; //Summe gewichtete Werte
      inc(iCnt) //gültige Punkte
    end;
    if iCnt>0 then
      faDns[I]:=fSum/iCnt
    else faDns[I]:=0;
  end;
end;

// isolierte Outlier entfernen

function tRank._Outlier(
  faDns:tnSgl; //Zeitreihe
  fLmt:single): //Schwelle für normaisierte Abweichung
  single; //Abweichung
// Varianz = (∑x²-(∑x)²/n)/(n-1)
var
  faTmp:tnSgl=nil; //Zwischenlager
  fSqr:double=0; //Quadratsumme Werte
  fSum:double=0; //Summe Werte
  I,R:integer;
begin
  SetLength(faTmp,length(faDns));
  move(faDns[0],faTmp[0],length(faDns)*SizeOf(single)); //Kopie als Backup


  for I:=0 to high(faDns) do
  begin
    fSqr+=sqr(faTmp[I]);
    fSum+=faTmp[I];
  end;
  Result:=sqrt(max((fSqr-sqr(fSum)/length(faDns))/high(faDns),0)); //Abweichung(Rundungsfehler!)


  if abs(faTmp[0]-Result)>Result*fLmt then
    faDns[I]:=(faTmp[1]); //ersetzen
  for I:=1 to length(faDns)-2 do
    if abs(faTmp[I]-Result)>Result*fLmt then
      faDns[I]:=(faTmp[pred(I)]+faTmp[succ(I)])/2; //interpolieren
  if abs(faTmp[high(faDns)]-Result)>Result*fLmt then
    faDns[high(faDns)]:=(faTmp[length(faDns)-2]); //ersetzen
end;

{ aGE liest den Nadir-Sonnenwinkel aus den RapidEye Metadaten }

function tArchive.GetElevation(sXml:string):single; //RapidEye-Metadaten
const
  cIea = '<opt:illuminationElevationAngle'; //Kennung
  cPos = 'aGE: Illumination angle not found in ';
var
  fVal:single; //Winkel als Zahl > Divisor
  iHig,iLow:integer; //Ende, Anfang der Ziffer
  sRem:string; //XML-Text
  qS:string;
begin
  Result:=1; //Vorgabe
  sRem:=Tools.LineRead(sXml); //Metadaten als String
  iLow:=pos(cIea,sRem); //Position Kennung ODER "1"

  if iLow>0 then //Kennung gefunden
  begin
    iLow:=succ(pos('>',sRem,iLow)); //Ende Kennung
    iHig:=pos('</',sRem,iLow); //Ende Ziffer
    qS:=copy(sRem,iLow,iHig-iLow); //Ziffer
    fVal:=StrToFloat(copy(sRem,iLow,iHig-iLow)); //Zahl
    Result:=cos((90-fVal)/180*Pi); //Nadir-Winkel
  end
  else Tools.ErrorOut(2,cPos+sXml);
end;

{ aIR übernimmt unkomprimierte RapidEye-Kacheln wie sie geliefert werden und
  speichert sie als kalibrierte Bilder im IDL-Format in das Arbeitsverzeichnis.
  Der Name besteht aus Sensor, Kachel und Datum. In "sArc" der Name aus der
  Lieferung übergeben werden. aIR übernimmt variable Faktoren für die
  Kalibrierung direkt aus den Metadaten und kombiniert sie mit konstanten
  Faktoren. }

function tArchive._ImportRapidEye_(
  sArc:string): //Original-Name (multispektrale Bilddaten)
  string; //Dateiname nach Import
const
  cNod:single=0; //NoData-Äquivalent
  caEai: array[0..4] of single = (1.572526106e-5, 1.685855999e-5,
    2.013325207e-5, 2.252037745e-5,2.794016946e-5); //aus RapidEye-Product-Specifications.pdf
var
  faFct:tnSgl=nil; //Faktoren für Kalibrierung
  fSia:single; //Solar Incidence Angle
  I:integer;
begin
  //sArc=*.zip? *.tar?
  Gdal.Import(byte(true),0,1,crFrm,sArc,''); //als ~/import speichern
  fSia:=GetElevation(ChangeFileExt(sArc,'_metadata.xml')); //Nadir-Sonnen-Winkel ergänzen
  SetLength(faFct,length(caEai)); //Array erzeugen
  if fSia<>0 then
    for I:=0 to high(faFct) do
      faFct[I]:=caEai[I]/fSia; //Winkel
  Filter.Calibrate(faFct,0,cNod,eeHme+cfImp); //Kanäle einzeln kalibrieren
  Result:=Tools.ShortName(3,5,2,sArc); //
  for I:=1 to 2 do
    delete(Result,rPos('-',Result),1); //Bindestriche
  Tools.EnviRename(eeHme+cfImp,Result); //Zwischenlager, getrennte Kanäle
end;

{ aIR übernimmt unkomprimierte RapidEye-Kacheln wie sie geliefert werden und
  speichert sie als kalibrierte Bilder im IDL-Format in das Arbeitsverzeichnis.
  Der Name besteht aus Sensor, Kachel und Datum. In "sArc" der Name aus der
  Lieferung übergeben werden. aIR übernimmt variable Faktoren für die
  Kalibrierung direkt aus den Metadaten und kombiniert sie mit konstanten
  Faktoren. }

function tArchive._ImportAster_(
  sArc:string): //Original-Namen
  string; //Dateiname nach Import
const
  cNod:single=-$FFFF; //NoData-Äquivalent
var
  faFct:tnSgl=nil; //Faktor für Kalibrierung
begin
  //sArc=*.zip? *.tar?
  Gdal.Import(byte(true),0,1,crFrm,sArc,''); //als ~/import speichern + beschneiden
  faFct:=Tools.InitSingle(1,dWord(single(1)));
  Filter.Calibrate(faFct,0,cNod,eeHme+cfImp); //Kanäle einzeln kalibrieren
  Result:=eeHme+'A3_'+ExtractWord(2,sArc,['_'])+'_20000101'; //Sensor_Kachel_Dummy-Datum
  Tools.EnviRename(eeHme+cfImp,Result); //Zwischenlager, getrennte Kanäle
end;

{ aQP prüft, ob die Bounding-Box der Bildkachel in "rImg" vollständig außerhalb
  des Auswahl-Rechecks "rRoi" liegt. }

function tArchive.QueryPosition(
  rImg:trFrm; //nutzbare Fläche
  rRoi:trFrm): //Auswahl-Rechteck
  boolean; //Überlappung existiert
const
  cEpg = 'aQP: Coordinate system mismatch: ';
begin
  if rImg.Epg<>rRoi.Epg then
    Tools.ErrorOut(3,IntToStr(rImg.Epg)+' ‹› '+IntToStr(rRoi.Epg));
  Result:=not(
    (rImg.Rgt<rRoi.Lft) or
    (rImg.Btm>rRoi.Top) or
    (rImg.Lft>rRoi.Rgt) or
    (rImg.Top<rRoi.Btm));
end;

{ gIt konvertiert das Bild "sImg" in das IDL-Format und speichert das Ergebnis
  als "import". Mit "sTrg" kann ein anderer Name gewählt werden. Mit "iSgl=0"
  übernimmt gIt das Format des Originals, in allen anderen Fällen speichert gIt
  das Bilder als 32-Bit Float. gIt aktualisiert die Bildstatistik. Mit "iHig >=
  iLow > Null" werden nur die Kanäle zwischen "iLow" und "iHig" übernommen. Mit
  "rFrm.Lft < rFrm.Rgt" beschneidet gIt das Bild auf den übergebenen Rahmen.
  Das CRS des Rahmens kann vom Bild abweichen. Der Rahmen darf größer sein als
  das Bild. }

procedure tGdal.Import(
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
begin
  //if not FileExists(eeHme) then Tools.ErrorOut(2,cHme);
  if not DirectoryExists(eeHme) then Tools.ErrorOut(2,cHme);
  if not FileExists(sImg) then Tools.ErrorOut(2,cSrc+sImg);

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
    if rFrm.Lft<rFrm.Rgt then //nicht Vorgabe
    begin
      slCmd.Add('-projwin');
      slCmd.Add(FloatToStr(rFrm.Lft));
      slCmd.Add(FloatToStr(rFrm.Top));
      slCmd.Add(FloatToStr(rFrm.Rgt));
      slCmd.Add(FloatToStr(rFrm.Btm));
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
    Tools.ErrorLog('gIt:'); //Fehler speichern
  finally
    slCmd.Free;
  end;
  if not FileExists(sTrg) then Tools.ErrorOut(2,cGdl+sImg);
  Tools.HintOut(true,'GDAL.Import: '+ExtractFileName(sImg));
end;

{ gWp transformiert das Bild "sImg" in die Projektion "iCrs" und die Pixelgröße
  "iPix" und speichert das Ergebnis im IDL-Format als "warp". Mit "target" kann
  ein anderer Name gewählt werden. Die Pixel sind quadratisch. Ihre Position
  folgt den Ziel-Koordinaten (TAP). Die Pixel-Werte sind bicubisch interpoliert.
  Leere Bereiche sind auf NoData gesetzt. }

procedure tGdal.Warp(
  iCrs:integer; //EPSG-Code für Ergebnis
  iPix:integer; //Pixelgröße in Metern
  sImg:string; //Vorbild
  sTrg:string); //Ergebnis-Name ODER leer
const
  cGdl = 'gWp: GDAL image warp not successful: ';
  cSrc = 'gWp: Image source file not found: ';
var
  slCmd: tStringList=nil; //Parameter-Liste für "prSys"
begin
  if sTrg='' then sTrg:=eeHme+cfWrp;
  if not FileExists(sImg) then Tools.ErrorOut(2,cSrc+sImg);
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
    end;
    slCmd.Add('-ot'); //Datenformat
    slCmd.Add('Float32'); //Single = Standard
    slCmd.Add('-r'); //Resampling
    slCmd.Add('cubic'); //Quadratisch = Standard
    slCmd.Add('-dstnodata');
    slCmd.Add(FloatToStr(NaN));
    slCmd.Add('-tap'); //Target Alligned Pixels
    slCmd.Add('-of'); //Bildformat
    slCmd.Add('ENVI'); //ENVI
    slCmd.Add(sImg); //Vorbild
    slCmd.Add('-overwrite'); //Ergebnis ersetzen
    slCmd.Add(sTrg); //Ergebnis
    //slCmd.SaveToFile(eeHme+'gdalwarp.params'); //KONTROLLE
    Tools.OsExecute(eeGdl+'gdalwarp',slCmd);
    Tools.ErrorLog('gWp:'); //Bei Exceptions anhalten
  finally
    slCmd.Free;
    if not FileExists(sTrg) then Tools.ErrorOut(2,cGdl+sTrg);
  end;
  Tools.HintOut(true,'GDAL.Warp: '+ExtractFileName(sImg));
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
  if not FileExists(sSrc) then Tools.ErrorOut(2,cSrc+sSrc);
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
{   TODO: slCmd.Add('-preserve_fid') bewahrt Feldnamen }
    slCmd.Add(sTrg); //Ziel: formatierte Geometrie
    slCmd.Add(sSrc); //Vorlage: erweiterte CSV-Datei
    Tools.OsExecute(eeGdl+'ogr2ogr',slCmd);
    Tools.ErrorLog('gES:'); //Exceptions speichern
  finally
    slCmd.Free;
    if not FileExists(sTrg) then Tools.ErrorOut(2,cGdl+sTrg);
  end;
  Tools.HintOut(true,'GDAL.Export: '+ExtractFileName(sTrg));
end;

{ aCg erzeugt eine Landsat-Archiv-Liste mit einem Polygon für die nutzbare
  Bildfläche und dem Namen des Archivs. Die Polygone sind im WKT-Format
  gespeichert, die Koordinaten sind geographisch. Der Katalog lässt sich im
  GIS visualisieren. }

function tArchive.xCatalog(
  sMsk:string): //Maske für Archiv-Dateinamen
  tStringList; //Bildrahmen
const
  cBox = 'aCg: Image coordinates not found or not defined: ';
var
  bErr:boolean=False; //Fehler bei Zahlenkonvertierung
  slArc:tStringList=nil; //Archiv-Namen nach Filterung
  slBnd:tStringList=nil; //Dateinamen im Archiv
  slMtl:tStringList=nil; //Text in MTL-Datei
  sRes:string; //Zeile im Katalog
  I,K:integer;
begin
  Result:=tStringList.Create;
  try
    slMtl:=tStringList.Create; //Metadaten als text
    slArc:=Tools.FileFilter(ChangeFileExt(sMsk,'.tar')); //Archiv-Namen
    Result.Add('WKT,id,filename');
    for I:=0 to pred(slArc.Count) do
    begin
      slBnd:=ExtractFilter(slArc[I],'_MTL.txt'); //MTL-Text extrahieren
      if FileExists(slBnd[0]) then
      begin
        slMtl.LoadFromFile(slBnd[0]); //MTL-Text lesen
        bErr:=False;
        for K:=0 to pred(slMtl.Count) do
          if slMtl[K]='  GROUP = PROJECTION_ATTRIBUTES' then
          begin
            sRes:='"MULTIPOLYGON ((('+
              RightStr(slMtl[K+13],8)+#32+RightStr(slMtl[K+12],8)+','+
              RightStr(slMtl[K+15],8)+#32+RightStr(slMtl[K+14],8)+','+
              RightStr(slMtl[K+19],8)+#32+RightStr(slMtl[K+18],8)+','+
              RightStr(slMtl[K+17],8)+#32+RightStr(slMtl[K+16],8)+','+
              RightStr(slMtl[K+13],8)+#32+RightStr(slMtl[K+12],8)+')))"';
            break; //fertig
          end;
        DeleteFile(slBnd[0]); //aufräumen
        if bErr or (K=slMtl.Count) then
          Tools.ErrorOut(2,cBox+slArc[I]); //Zahlen nicht gefunden oder nicht lesbar
        Result.Add(sRes+','+IntToStr(succ(I))+','+slArc[I]);
      end;
      FreeAndNil(slBnd); //Speicher freigeben in der Schleife
      write(#13+IntToStr(slArc.Count-I)+#32) //Ablauf
    end;
  finally
    slArc.Free;
    slMtl.Free;
  end;
  Tools.HintOut(true,'Archive.Catalog: '+ChangeFileExt(sMsk,'.tar'));
end;

{ aQQ extrahiert den QA-Kanal aus dem Landsat-Archiv "sArc" innerhalb des ROIs
  "sFrm", bestimmt den Anteil ungestörter Pixel innerhalb der Bilddaten und
  speichert das Ergebnis als binäre Maske "mask", wenn der Anteil klarer Pixel
  größer ist als die Schwelle "fLmt". aQQ filtert den QA-Kanal mit binären
  Filtern für sichere opake Wolken, Wolken-Schatten, Cirren und Bild-Fehler. }

function tArchive.QueryQuality(
  rImg,rRoi:trFrm; //Rahmen-Verschnitt, geographisch
  var rSct:trFrm; //Verschnitt nach Rotation auf Bild-Projektion
  sArc:string): //BildArchiv-Name
  single; //Anteil nutzbare Pixel
const
  cCld = $300; //2⁸+2⁹ = Clouds (768)
  cSdw = $C00; //2¹⁰+2¹¹ = Shadow (3072)
  cIce = $3000; //2¹²+2¹³ = Ice, Snow (12288)
  cCir = $C000; //2¹⁴+2¹⁵ = Cirrus (49152)
var
  iCnt:integer=0; //Pixel innerhalb der Kachel, einschließlich leere Pixel am rand
  iEpg:integer=4326; //Projektion im Archiv (EPSG)
  iPix:integer=0; //Pixel ohne Störung
  ixBin:tn2Wrd=nil; //Landsat-QA-Layer
  rHdr:trHdr; //Metadaten
  slBnd:tStringList=nil; //Dateien im Archiv, ausgewählte Kanäle
  X,Y:integer;
begin
  Result:=0; //Vorgabe
  try
    slBnd:=ExtractFilter(sArc,'_QA_PIXEL'); //QA-Layer extrahieren
    iEpg:=Cover.CrsInfo(slBnd[0]); //EPSG-Code des QA-Layers
    rSct:=Cover.Rotate(iEpg,rImg,rRoi); //auf "iEpg" rotierte Bounding-Boxen

    if (rSct.Lft<rSct.Rgt) and (rSct.Top>rSct.Btm) then //Überlappung existiert
    begin
      Gdal.Import(0,0,1,rSct,slBnd[0],eeHme+cfImp); //Ausschnitt "rFrm, als "import"
      DeleteFile(slBnd[0]); //Extrakt löschen
      DeleteFile(slBnd[0]+cfExt); //Extrakt löschen
      Header.Read(rHdr,eeHme+cfImp);
      ixBin:=Image.ReadWord(rHdr,eeHme+cfImp); //QA-Layer
      for Y:=0 to pred(rHdr.Lin) do
        for X:=0 to pred(rHdr.Scn) do
          if ixBin[Y,X]>1 then //Bildpixel innerhalb der Szene
          begin
            inc(iCnt); //Summe definierte Pixel
            if (ixBin[Y,X] and cCld=cCld) //binäre Marker für Wolken und Schatten
            or (ixBin[Y,X] and cSdw=cSdw)
            or (ixBin[Y,X] and cCir=cCir) then //Bildstörung
              ixBin[Y,X]:=0 //Pixel maskieren
            else
            begin
              ixBin[Y,X]:=1; //Pixel öffnen
              inc(iPix); //Summe klare Pixel
            end;
          end;
      if iCnt>0 then
        Result:=iPix/iCnt; //Schwelle
    end;
    if Result>0 then
    begin
      Image.WriteWord(ixBin,eeHme+cfMsk); //als Maske speichern
      Tools.CopyFile(eeHme+cfImp+cfHdr,eeHme+cfMsk+cfHdr); //Header kopieren
    end;
  finally
    slBnd.Free;
  end;
  Tools.HintOut(true,'Archive.Quality: '+ExtractFileName(sArc));
end;

{ aIL extrahiert die Kanäle "sBnd" aus dem Archiv "sArc", beschneidet sie auf
  den Ausschnitt "rFrm", kalibriert optische Kanäle mit Landsat-Konstanten,
  setzt Pixel auf Nodata die in "sMsk" auf Null gesetzt sind und speichert das
  Ergebnis als Stack im Arbeitsverzeichnis. Der Ergebnis-Name enthält ein
  Sensor-Kürzen, die Kachel-ID und das Datum.
     aIL extrahiert ganze Kanäle in das aktuelle Verzeichnis und speichert das
  Ergebnis im IDL-Format in das Arbeitsverzeichnis. aIL löscht alle Zwischen-
  Produkte. }

function tArchive.ImportLandsat(
  rFrm:trFrm; //Bounding-Box des ROI, geographisch
  sArc:string; //Archiv-Name
  sBnd:string): //Kanal-Namen-Filter, kommagetrennt
  string; //Name multispektrales Bild
const
  cBnd = 'aIL: No results for passed band filter: ';
  cFct:single=2.75e-5; //optische Kanäle
  cNod:single=0.0; //NoData-Äquivalent in Rohdaten
  cOfs:single=-0.2; //optische Kanäle
{ TODO: der Landsat-Thermal-Kanal (B10) braucht eine andere Calibrierung}
var
  faFct:tnSgl=nil; //Kanal-spezifische Faktoren für Kalibrierung
  slBnd:tStringList=nil; //Kanäle im Archiv
  B:integer;
  qS:string;
begin
  Result:=Tools.ShortName(1,3,4,sArc); //Sensor-Kachel-Datum
  try
    slBnd:=ExtractFilter(sArc,sBnd); //ausgewählte Kanäle extrahieren
    if slBnd=nil then Tools.ErrorOut(2,cBnd+sBnd); //kein passender Kanal
    faFct:=Tools.InitSingle(slBnd.Count,dWord(cFct)); //konstanter Faktor für alle Kanäle
    for B:=0 to pred(slBnd.Count) do
    begin
      Gdal.Import(1,0,1,rFrm,slBnd[B],eeHme+cfImp); //Float-Format, manueller Ausschnitt, als "import"
      DeleteFile(slBnd[B]); //Zwischenlager löschen
      DeleteFile(slBnd[B]+cfExt); //Extrakt löschen
      qS:=ExtractWord(9,ChangeFileExt(ExtractFileName(slBnd[B]),''),['_']);
      slBnd[B]:=Result+'_'+ExtractWord(9,ChangeFileExt(ExtractFileName(slBnd[B]),''),['_']);
      Tools.EnviRename(eeHme+cfImp,slBnd[B]); //Zwischenlager, getrennte Kanäle
    end;
    Image.StackBands(slBnd,eeHme+cfMsk); //Kanäle als Stack & maskieren
    Filter.Calibrate(faFct,cOfs,cNod,eeHme+cfStk); //Dichte kalibrieren
    Tools.EnviRename(eeHme+cfStk,Result); //
    for B:=0 to pred(slBnd.Count) do
      Tools.EnviDelete(slBnd[B]); //aufräumen
  finally
    slBnd.Free;
  end;
  Tools.HintOut(true,'Archive.Import: '+ExtractFileName(Result));
end;

{ mCt transformiert beliebige Bilddaten in das ENVI-Format und speichert sie im
  Arbeitsverzeichnis. Das Format ist immer 32-Bit Float (single). Pixelgröße,
  Projektion und Kanäle sind wählbar. "slImg" übergibt die Ergebnis-Namen. }
{ mCt ändert zuerst das Format nach ENVI / Single (gdal.import) und dann
  Projektion und Pixelgröße nach "iEpg" (gdal.warp) / "fPix". Die Trennung ist
  nötig, weil nur der "import"-Befehl einzelne Kanäle selektieren kann. }

procedure tArchive.xTransform(
  fPix:double; //Pixelgröße ODER Null für unverändert
  iEpg:integer; //CRS als EPEG-Nummer ODER Null für unverändert
  sArt:string; //Kanal-Auswahl [BX:BY] mit [X,Y] ab Eins ODER leer für alle Kanäle
  slImg:tStringList); //Namen der Eingangs-Bilder und Namen der Ergebnisse
var
  iHig,iLow:integer; //letzter, erster Kanal
  rCvr:trCvr; //Metadaten für einzelne Bilder
  sRes:string=''; //Dateiname wechselt durch Bearbeitung
  I:integer;
begin
  for I:=0 to pred(slImg.Count) do
  begin
    rCvr:=Cover.RasterFrame(slImg[I]); //Bild-Metadaten
    Reduce.GetBands(iHig,iLow,rCvr.Stk,sArt); //ausgewählte Kanäle
    sRes:=slImg[I]; //für Bearbeitungs-Stufen
    if (ExtractFilePath(slImg[I])<>eeHme) or (sArt<>'') then
    begin
      Gdal.Import(1,iHig,iLow,crFrm,slImg[I],''); //als "import" im Arbeitsverzeichnis
      sRes:=eeHme+cfImp; //Zwischenlager
    end;
    if ((iEpg>0) and (iEpg<>rCvr.Epg))
    or ((fPix>0) and (fPix<>rCvr.Pix)) then //Projektion, Pixelgröße ändern
    begin
      Gdal.Warp(iEpg,round(fPix),sRes,''); //als "warp" im Arbeitsverzeichnis
      sRes:=eeHme+cfWrp //Zwischenlager
    end;
    slImg[I]:=eeHme+ChangeFileExt(ExtractFileName(slImg[I]),''); //ursprünglicher Name
    Tools.EnviRename(sRes,slImg[I]); //Name vergeben
  end;
  Tools.HintOut(true,'Merge.Transform: '+IntToStr(slImg.Count)+' imges');
end;

{ mCp schreibt alle Bilder in "slImg" in den gemeinsamen Rahmen "rFrm". mCP
  trennt Bilder mit verschiedenem Datum und überschreibt Bilder mit gleichem
  Datum. mCp setzt die Bilder an die richtigen Position und löscht alle Bild-
  Teile außerhalb des Rahmens. Pixelgröße, Kanäle und Projektion aller Bilder
  müssen gleich sein. mCp setzt leere Bildteile auf NoData. }
{ Für die Überlagerung müssen die Bilder nach dem datum sortiert sein.  mCp
  erzeugt aus "rFrm" eine leere (NoData) Vorlage und ergänzt sukzessive die
  übergebenen Bilder. mCp bestimmt für jedes Bild den Versatz der Bilddaten
  zum Rahmen (iHrz,iVrt) und die verwendbare Fläche innerhalb des Rahmens
  (iRgt,iTop,iLft,iBtm). mCp überträgt alle Kanäle und ignoriert dabei NoData-
  Pixel im Vorbild. mCp überschreibt die Pixel des Vorgängers, wenn zwei Bilder
  dasselbe Datum haben  (gleicher Pfad). mCp erzeugt für jedes neue Datum ein
  neues leeres Bild und speichert erst, wenn alle Bilder mit gleichem Datum
  erfasst sind. mCp erzeugt und speichert einen Header mit den Koordinaten aus
  dem Rahmen. }

procedure tArchive.xCompile(
  iMrg:integer; //Überschreiben: 8=Datum; 4=Jahr; 0=Alles trennen
  rFrm:trFrm; //Auswahlrahmen
  sTrg:string; //Ergebnis-Name ODER leer für Vorgabe
  slImg:tStringList); //Liste Bildnamen
const
  cFrm = 'mCp: Image frame (ROI) not defined!';
var
  fPix:double=0; //gemeinsame Pixelgröße
  fxImg:tn3Sgl=nil; //Kanal im aktuellen Bild
  fxRes:tn3Sgl=nil; //Ergebnis-Kanal, ganze Fläche
  iLft,iTop,iRgt,iBtm:integer; //Pixel-Koordinaten für nutzbaren Ausschnitt im Rahmen
  iHgt,iWdt:integer; //Bildfläche aus Rahmen ODER alle übergebenen Bilder
  iHrz,iVrt:integer; //Versatz Bild gegen Rahmen in Pixeln (AUCH NEGATIV!)
  iStk:integer=0; //Anzahl Kanäle
  iPrd:integer=0; //Kanäle pro Einzelbild (aus Import)
  rHdr:trHdr; //ENVI-Metadaten
  sBnd:string=''; //Kanal-Namen mit Zeilentrenner
  B,I,X,Y:integer;
begin
  //Bildnamen dürfen keine Extension haben (ENVI-Stil)
  with rFrm do if (Top<Btm) or (Rgt<Lft) then Tools.ErrorOut(3,cFrm);
  if slImg.Count<1 then exit;
  if sTrg='' then sTrg:=eeHme+cfCpl; //Vorgabe verwenden

  Header.Read(rHdr,slImg[0]); //"fPix", "iPrd" werden kontrolliert
  fPix:=rHdr.Pix; //gemeinsame Pixelgröße
  iPrd:=rHdr.Stk; //Anzahl Kanäle aus erstem Bild
  iHgt:=round((rFrm.Top-rFrm.Btm)/fPix);
  iWdt:=round((rFrm.Rgt-rFrm.Lft)/fPix);
  for I:=0 to pred(slImg.Count) do
  begin
    if I>0 then Header.Read(rHdr,slImg[I]);
    if (I>0) and ((rHdr.Pix<>fPix) or (rHdr.Stk<>iPrd)) then
      Tools.ErrorOut(3,slImg[I]); //Kanäle müssen passen
    if (I=0) //erster Kanal
    or ((iMrg>0) //Datum/Jahr verwenden
    and (RightStr(slImg[pred(I)],iMrg)<>RightStr(slImg[I],iMrg))) then
      fxRes:=Tools.Init3Single(rHdr.Stk,iHgt,iWdt,dWord(NaN)); //leere Vorlage

    iHrz:=round((rHdr.Lon-rFrm.Lft)/fPix); //Versatz horizontal in Pixeln
    iVrt:=round((rFrm.Top-rHdr.Lat)/fPix); //Versatz vertikal in Pixeln
    iLft:=max(iHrz,0); //Pixelindx linker Rand
    iTop:=max(iVrt,0); //Pixelindex oberer Rand
    iRgt:=min((rHdr.Scn+iHrz),iWdt); //Pixelindex rechter Rand
    iBtm:=min((rHdr.Lin+iVrt),iHgt); //Pixelindex unterer Rand

    fxImg:=Image.Read(rHdr,slImg[I]);
    for Y:=iTop to pred(iBtm) do //alle gemeinsam abgedeckten Pixel
      for X:=iLft to pred(iRgt) do
        if not isNan(fxImg[0,Y-iVrt,X-iHrz]) then //nur gültige Pixel
          for B:=0 to pred(iPrd) do
            fxRes[B,Y,X]:=fxImg[B,Y-iVrt,X-iHrz]; //Bild in Vorlage einfügen

    if (I=pred(slImg.Count)) //letzter Kanal
    or ((iMrg>0) //Datum/Jahr verwenden
    and (RightStr(slImg[I],iMrg)<>RightStr(slImg[succ(I)],iMrg))) then //aktuellen tag speichern
      for B:=0 to pred(iPrd) do
      begin
        Image.WriteBand(fxRes[B],iStk,sTrg); //aktuellen Kanal speichern
        sBnd+=RightStr(slImg[I],iMrg)+' B'+IntToStr(succ(B))+#10; //Datum + Kanal für Header
        inc(iStk); //gespeicherte Kanäle
      end;
    write(#13,IntToStr(slImg.Count-I))
  end;

  rHdr.Lat:=round(rFrm.Top/fPix)*fPix; //Frame auf Pixel-Raster normieren
  rHdr.Lon:=round(rFrm.Lft/fPix)*fPix;
  Header.Origin(rHdr.Lat,rHdr.Lon,rHdr); //mapinfo-String verändern
  rHdr.Scn:=iWdt; //neue Parameter
  rHdr.Lin:=iHgt;
  rHdr.Stk:=iStk;
  rHdr.Prd:=iPrd;
  delete(sBnd,length(sBnd),1); //letztes Zeichen (#10) löschen
  Header.WriteMulti(rHdr,sBnd,sTrg);
  Tools.HintOut(true,'Merge.Compile('+IntToStr(I)+'): '+ExtractFileName(sTrg));
end;

{ aBN trägt die Dateinamen aus "slImg" als Kanal-Namen in die Envi-Header ein,
  wenn der bishereige Kanal-Name mit "Band" beginnt. GDAL setzt "Band" + Nummer
  wenn keine anderen Kanal-Namen registriert sind. }

procedure tArchive.BandNames(slImg:tStringList);
var
  rHdr:trHdr;
  sBnd:string; //Kanal-Namen mit Zeilentrennern
  B,I:integer;
  qS:string;
begin
  for I:=0 to pred(slImg.Count) do
  begin
    Header.Read(rHdr,slImg[I]);
    qS:=slImg[I];
    sBnd:=''; //neu zusammenstellen
    for B:=1 to WordCount(rHdr.aBnd,[#10]) do
      sBnd:=ExtractWord(B,rHdr.aBnd,[#10])+#32+ //alter Kanal-Name
        ChangeFileExt(ExtractFileName(slImg[I]),'')+#10; //Erweiterung + Zeilentrenner
    Delete(sBnd,length(sBnd),1); //Zeilentrenner am Ende
    rHdr.aBnd:=sBnd;
    Header.Write(rHdr,'bandnames',slImg[I]);
  end;
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

procedure tMerge._xCompile_(
  rFrm:trFrm; //Name Auswahlrahmen
  sTrg:string; //Ergebnis-Name ODER leer für Vorgabe
  slImg:tStringList); //Liste Bildnamen
const
  cFrm = 'mCp: Image frame (ROI) not defined!';
var
  fPix:double=0; //gemeinsame Pixelgröße
  fxImg:tn3Sgl=nil; //Kanal im aktuellen Bild
  fxRes:tn3Sgl=nil; //Ergebnis-Kanal, ganze Fläche
  iLft,iTop,iRgt,iBtm:integer; //Pixel-Koordinaten für nutzbaren Ausschnitt im Rahmen
  iHgt,iWdt:integer; //Bildfläche aus Rahmen ODER alle übergebenen Bilder
  iHrz,iVrt:integer; //Versatz Bild gegen Rahmen in Pixeln (AUCH NEGATIV!)
  iStk:integer=0; //Anzahl Kanäle
  iPrd:integer=0; //Kanäle pro Einzelbild (aus Import)
  rHdr:trHdr; //ENVI-Metadaten
  sBnd:string=''; //Kanal-Namen mit Zeilentrenner
  B,I,X,Y:integer;
begin
  //Bildnamen dürfen keine Extension haben (ENVI-Stil)
  with rFrm do if (Top<Btm) or (Rgt<Lft) then Tools.ErrorOut(3,cFrm);
  if slImg.Count<1 then exit;
  if sTrg='' then sTrg:=eeHme+cfCpl; //Vorgabe verwenden

  Header.Read(rHdr,slImg[0]); //"fPix", "iPrd" werden kontrolliert
  fPix:=rHdr.Pix; //gemeinsame Pixelgröße
  iPrd:=rHdr.Stk; //Anzahl Kanäle aus erstem Bild
  iHgt:=round((rFrm.Top-rFrm.Btm)/fPix);
  iWdt:=round((rFrm.Rgt-rFrm.Lft)/fPix);
  for I:=0 to pred(slImg.Count) do
  begin
    if I>0 then Header.Read(rHdr,slImg[I]);
    if (I>0) and ((rHdr.Pix<>fPix) or (rHdr.Stk<>iPrd)) then
      Tools.ErrorOut(3,slImg[I]); //Kanäle müssen passen
    if (I=0) or (RightStr(slImg[pred(I)],8)<>RightStr(slImg[I],8)) then
      fxRes:=Tools.Init3Single(rHdr.Stk,iHgt,iWdt,dWord(NaN)); //leere Vorlage

    iHrz:=round((rHdr.Lon-rFrm.Lft)/fPix); //Versatz horizontal in Pixeln
    iVrt:=round((rFrm.Top-rHdr.Lat)/fPix); //Versatz vertikal in Pixeln
    iLft:=max(iHrz,0); //Pixelindx linker Rand
    iTop:=max(iVrt,0); //Pixelindex oberer Rand
    iRgt:=min((rHdr.Scn+iHrz),iWdt); //Pixelindex rechter Rand
    iBtm:=min((rHdr.Lin+iVrt),iHgt); //Pixelindex unterer Rand

    fxImg:=Image.Read(rHdr,slImg[I]);
    for Y:=iTop to pred(iBtm) do //alle gemeinsam abgedeckten Pixel
      for X:=iLft to pred(iRgt) do
        if not isNan(fxImg[0,Y-iVrt,X-iHrz]) then //nur gültige Pixel
          for B:=0 to pred(iPrd) do
            fxRes[B,Y,X]:=fxImg[B,Y-iVrt,X-iHrz]; //Bild in Vorlage einfügen

    if (I=pred(slImg.Count))
    or (RightStr(slImg[I],8)<>RightStr(slImg[succ(I)],8)) then //aktuellen tag speichern
      for B:=0 to pred(iPrd) do
      begin
        Image.WriteBand(fxRes[B],iStk,sTrg); //aktuellen Kanal speichern
        sBnd+=RightStr(slImg[I],8)+' B'+IntToStr(succ(B))+#10; //Datum + Kanal für Header
        inc(iStk); //gespeicherte Kanäle
      end;
    write(#13,IntToStr(slImg.Count-I))
  end;

  rHdr.Lat:=round(rFrm.Top/fPix)*fPix; //Frame auf Pixel-Raster normieren
  rHdr.Lon:=round(rFrm.Lft/fPix)*fPix;
  Header.Origin(rHdr.Lat,rHdr.Lon,rHdr); //mapinfo-String verändern
  rHdr.Scn:=iWdt; //neue Parameter
  rHdr.Lin:=iHgt;
  rHdr.Stk:=iStk;
  rHdr.Prd:=iPrd;
  delete(sBnd,length(sBnd),1); //letztes Zeichen (#10) löschen
  Header.WriteMulti(rHdr,sBnd,sTrg);
  Tools.HintOut(true,'Merge.Compile('+IntToStr(I)+'): '+ExtractFileName(sTrg));
end;

{ "natürliche" Klassen finden = lokale Minima der Dichte im Merkmalsraum
  - Merkmalsraum rastern (nVoxel)
    - 1% .. 99% Percentile als Grenzen
    - Bilder evt. normalisieren
  - Merkmalsraum (nVoxel) fortlaufend indizieren [1..N]
  - Bild aus nVoxel-IDs erstellen
  - lokale Maxima suchen
    - zufälligen (nicht belegte) nVoxel wählen
    - Nachbar-nVoxel in allen Dimensionen testen
    - zunehmende Häufigkeit verfolgen }

procedure tSeparate._FeatureGrid(
  faLow:tnSgl; //kleinster Wert (Voxel-Mitte) pro Merkmal (Dimension)
  faHig:tnSgl; //größter Wert (so)
  iPrt:integer; //Schritte pro Dimension (iaPrt für individuelle Schritte?)
  sImg:string); //Vorbild
var
  faStp:tnSgl=nil; //Werte-Bereich pro Dimension
  fxImg:tn3Sgl=nil; //Bilddaten
  iLmt:integer; //höchste ID pro Dimension
  iVox,iTmp:integer; //Voxel-ID, Zwischenlager
  ixIdx:tn2Int=nil; //Voxel-Index
  rHdr:trHdr; //Metadaten
  B,X,Y:integer;
begin
  Header.Read(rHdr,sImg); //Metadaten
  fxImg:=Image.Read(rHdr,sImg); //Vorbild
  ixIdx:=Tools.Init2Integer(rHdr.Lin,rHdr.Scn,0); //Voxel-Index
  faStp:=Tools.InitSingle(pred(rHdr.Stk),0); //Werte-Bereich pro Dimension
  for B:=0 to pred(rHdr.Stk) do
    faStp[B]:=(faHig[B]-faLow[B])/iPrt; //Bereich pro Dimension
  iLmt:=pred(iPrt);

{ TODO: [RECENT] Separate.FeatureGrid prüfen }

  for Y:=0 to pred(rHdr.Lin) do
    for X:=0 to pred(rHdr.Scn) do
    begin
      if isNan(fxImg[0,Y,X]) then continue; //nur definierte Pixel
      iVox:=0;
      for B:=0 to pred(rHdr.Stk) do //Kanäle im Vorbild
      begin
        iTmp:=trunc((fxImg[B,Y,X]-faLow[B])/faStp[B]); //Index in Dimension "B"  KONTROLLE
        iTmp:=min(max(trunc((fxImg[B,Y,X]-faLow[B])/faStp[B]),0),iLmt); //Index in Dimension "B"
        iVox:=iVox*iPrt+iTmp; //neue Position ergänzen
      end;
      ixIdx[Y,X]:=succ(iVox) //Null für NoData, ID immer positiv!
    end;
  Image.WriteBand(tn2Sgl(ixIdx),0,eeHme+cfIdx); //Bilddaten schreiben
  Header.WriteIndex(round(power(iPrt,rHdr.Stk)),rHdr,eeHme+cfIdx);
end;

