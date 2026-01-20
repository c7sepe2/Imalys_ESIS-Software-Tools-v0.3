unit index;

{ INDEX sammelt Routinen zur Abgrenzung und Nutzung von Zonen. "Zonen" sind
  zusammenhängende Teile des Bilds, deren Pixel mehr Merkmale gemeinsam haben
  als Pxel außerhalb der Zone. Zonen sind durch einen Index, Attribute und eine
  Topologie charakterisiert. Det "Index" ist ein Bild mit Zonen-IDs als Wert.
  Attribute sind eine Tabelle mit Scalaren, die den Zonen zugeordnet sind. Die
  "Topologie" ist eine Liste mit der ID aller Nachbarzonen und der Anzahl der
  Pixel zwischen zwei benachbarten Zonen sowie ein Index auf beide Listen.

  BUILD:  sammelt vom Zellindex abhängige Routinen
  UNION:  vereinigt Pixel zu Zonen basierend auf Varianz

            BEGRIFFE
  Feature:  Wert, der einer Zone zugeordnet ist, meistens der Mittelwert aller
            Pixel. Aus Form und Größe der Zonen abgeleitete Werte können ebenso
            Features sein. SYNONYM: Attribut
  Basin:    Gebiet mit einem gemeinsamen Abfluss SYNONYM: Catchment
  Drain:    Abfluss eines Basins am niedrigsten Punkt am Rand des Basins. Auch
            Höhe dieses Punkts.
  Equal:    Wertebereich angepasst auf Mittelwert ± Standardabweichung * Eingabe
  Flow:     Cumulierter Abfluss aus allen verknüpften Zonen
  Link:     Verknüpfung zwischen Zonen. Jede Zone ist mit genau einer anderen
            verknüpft
  Index:    Bereich eines Bildes mit gleicher ID in "index" → Zone
  Kontakt:  Grenze zwischen zwei Pixeln vor allem im Zusammenhang mit Grenzen
            zwischen Zonen
  Zone:     Pixel mit gleichem Wert der Zonen-ID "index". Pixel einer Zone sind
            immer mit mindestens einer Kante verknüpft. Die Zonen-ID ist
            eindeutig. }

{$mode objfpc}{$H+}

interface

uses
  Classes, StrUtils, SysUtils, Math, format;

type
  tBuild = class(tObject) //checked 250819
    private
      function Dendrites:tnSgl;
      function Deviation(fxImg:tn3Sgl; iCnt:integer; ixIdx:tn2Int):tnSgl;
      function Diversity(sImg:string):tnSgl;
      function _EqualDeviation_(fDvt:single):string;
      function _EqualVariance_(fDvt:single):string;
      procedure _IntegerValues_(iaRgn:tnInt);
      function InterFlow:tnSgl;
      function _LowPass_(fxBnd:tn2Sgl; iRds:integer; ixIdx:tn2Int):tn2Sgl;
      function Proportion:tnSgl;
      function Relations:tnSgl;
      function Texture_(fxImg:tn3Sgl; iCnt,iTyp:integer; ixIdx:tn2Int):tnSgl;
      function _Texture(fxImg:tn3Sgl; iCnt,iTyp:integer; ixIdx:tn2Int):tnSgl;
      function ZonePercentil(faAtr,faSze:tnSgl; fLmt:single):single;
    public
      procedure AttributeImage(faVal:tnSgl);
      procedure FeatureDrain(faVal:tnSgl; iGen:integer; ixTpl:tn2Int);
      procedure IndexTopology(iCnt:integer; ixIdx:tn2Int);
      procedure IntegerImage(iaCtm:tnInt);
      function SizeFit_(sIdx,sStk:string):boolean;
      function _SizeFit(rIdx,rStk:trHdr):boolean;
      function ThemaImage(iaThm:tnInt):tn2Byt;
      function ZonesSize(var rHdr:trHdr):tnSgl;
      procedure xDiffusion(iGen:integer; sAtr:string);
      function xEqualFeatures(fPrc:single):string;
      procedure xImageFeatures(sImg:string);
      procedure xKernelFeatures(slCmd:tStringList; sImg:string);
      procedure xShapeFeatures(sImg:string; slCmd:tStringList);
  end;

  tUnion = class(tObject)
    private
      iacPix:tnInt; //Pixel-Indices auf Suchpfad
      ixcIdx:tn2Int; //Zonen-IDs NUR FÜR "xBorders"
      ixcMap:tn2Byt; //Klassen-Layer NUR FÜR "xBorders"
      function Borders(var iRes:integer; ixMap:tn2Byt):tn2Int;
      function DemIndex(fxElv:tn2Sgl):tn2Int;
      procedure ExtendIndex(ixIdx:tn2Int);
      function ExtendZero(ixDrn:tn2Int):integer;
      function NewIndex(fxImg:tn3Sgl; var iCnt:integer):tn2Int;
      function IndexMerge(apEtp:tapEtp; iaLnk:tnInt; ixIdx:tn2Int):tapEtp;
      function LinksMerge(apEtp:tapEtp; ixIdx:tn2Int):tnInt;
      function NewEntropy(fxImg:tn3Sgl; iCnt:integer; ixIdx:tn2Int):tapEtp;
      procedure MinEntropy(apEtp:tapEtp; iGrw:integer; ixIdx:tn2Int);
      procedure PatchGrow(iLat,iLon,iVrt,iHrz:integer);
      procedure ZoneGrow(iLat,iLon,iVrt,iHrz:integer; ixDrn:tn2Int);
    public
      procedure xMapZones(sImg:string);
      procedure xDemZones(sDem:string);
      procedure xImgZones(iGrw,iSze:integer; sImg:string);
  end;

var
  Build: tBuild;
  Union: tUnion;

implementation

uses
  Mutual, Raster, Thema, Vector;

// effektiver Lowpass mit Zonen
// wahlweise innerhalb der Zonen oder ganzes Bild

function tBuild._LowPass_(
  fxBnd:tn2Sgl; //Vorbild-Kanal
  iRds:integer; //Kernel-Radius (Iterationen)
  ixIdx:tn2Int): //Zonen-IDs
  tn2Sgl; //LowPass-Kanal
var
  ixCnt:tn2Int=nil; //Zähler für Werte pro Pixel
  fxTmp:tn2Sgl=nil; //LowPass Iterations-Stufe

procedure lSum(const iHrz,iVrt,iLon,iLat:integer);
begin
  if isNan(fxTmp[iVrt,iHrz]) or isNan(fxTmp[iLat,iLon]) then exit;
  //if ixIdx[pred(iVrt),iHrz]<>ixIdx[iLat,iLon] then exit;
  Result[iVrt,iHrz]+=fxTmp[iLat,iLon];
  Result[iLat,iLon]+=fxTmp[iVrt,iHrz];
  inc(ixCnt[iLat,iLon]);
  inc(ixCnt[iVrt,iHrz]);
end;

var
  R,X,Y:integer;
begin
  Result:=nil;
  for R:=1 to iRds do
  begin
    if R=1
      then fxTmp:=tn2Sgl(Tools.CopyGrid(tn2Int(fxBnd))) //Vorlage sichern
      else fxTmp:=tn2Sgl(Tools.CopyGrid(tn2Int(Result))); //Zwischenergebnis sichern
    Result:=Tools.Init2Single(length(ixIdx),length(ixIdx[0]),0); //neu zählen
    ixCnt:=Tools.Init2Integer(length(ixIdx),length(ixIdx[0]),0); //neu zählen
    for Y:=1 to high(ixIdx) do
      for X:=0 to high(ixIdx[0]) do
        lSum(X,pred(Y),X,Y);
    for Y:=0 to high(ixIdx) do
      for X:=1 to high(ixIdx[0]) do
        lSum(pred(X),Y,X,Y);
    for Y:=0 to high(ixIdx) do
      for X:=0 to high(ixIdx[0]) do
        if ixCnt[Y,X]>1 then
          Result[Y,X]/=ixCnt[Y,X];
  end;
end;

{ bEF normalisiert die Werte aller Attribute in "cfAtr" auf 0.5±Vrz*fFct mit
  Vrz=Gauss'scher Varianz (∑x²-(∑x)²/n)/(n-1) und "fFct" = Faktor aus Eingabe.
  ACHTUNG: "fFct" MUSS POSITIV SEIN }

function tBuild._EqualVariance_(fDvt:single):string; //Soll-Abweichung
var
  fFct:single; //Scalierungs-Faktor
  fSum:single; //Summe Werte
  fOfs:single; //Offset Minimum
  fSqr:single; //Quadratsumme Werte
  iCnt:integer; //Anzahl Zähler
  fxAtr:tn2Sgl=nil; //alle Attribute
  F,Z:integer;
begin
  Result:=eeHme+cfEql;
  fxAtr:=Tools.BitRead(eeHme+cfAtr); //Attribut-Tabelle lesen
  for F:=0 to high(fxAtr) do
  begin
    fSum:=0; fSqr:=0; iCnt:=0;
    for Z:=1 to high(fxAtr[0]) do
    begin
      if isNan(fxAtr[F,Z]) then continue;
      fSum+=fxAtr[F,Z]; //Summe Werte
      fSqr+=sqr(fxAtr[F,Z]); //Summe Quadrate
      inc(iCnt); //Summe gültige Pixel
    end;
    if iCnt<2 then continue;
    fSqr:=sqrt((fSqr-sqr(fSum)/iCnt)/pred(iCnt)); //Abweichung
    fSum:=fSum/iCnt; //Mittelwert
    fFct:=1/(fSqr*2*fDvt); //Abweichung nach oben und unten
    fOfs:=fSum-fSqr*fDvt; //Offset für Null
    for Z:=1 to high(fxAtr[0]) do
      fxAtr[F,Z]:=(fxAtr[F,Z]-fOfs)*fFct;
  end;
  Tools.BitWrite(fxAtr,Result);
end;

{ bEF normalisiert die Werte aller Attribute in "cfAtr" auf Mittelwert und
  fDvt*Abweichung der direkt bestimmten Werte. bEF berücksichtigt dabei die
  Zonengröße so dass sich die Schwellen auf die Zahl der Pixel mit dem
  entsprecheden Attribut beziehen.
  ACHTUNG: "fFct" MUSS POSITIV SEIN }

function tBuild._EqualDeviation_(fDvt:single):string; //Soll-Abweichung
var
  fFct:single; //Scalierungs-Faktor
  fSum:single; //Summe Werte
  fOfs:single; //Offset Minimum
  fSqr:single; //Quadratsumme Werte
  iCnt:integer; //Anzahl Zähler
  fxAtr:tn2Sgl=nil; //alle Attribute
  F,Z:integer;
begin
  Result:=eeHme+cfEql;
  fxAtr:=Tools.BitRead(eeHme+cfAtr); //Attribut-Tabelle lesen
  for F:=0 to high(fxAtr) do
  begin
    fSum:=0; fSqr:=0; iCnt:=0;
    for Z:=1 to high(fxAtr[0]) do
    begin
      if isNan(fxAtr[F,Z]) then continue;
      fSum+=fxAtr[F,Z]; //Summe Werte
      fSqr+=sqr(fxAtr[F,Z]); //Summe Quadrate
      inc(iCnt); //Summe gültige Pixel
    end;
    if iCnt<2 then continue;
    fSqr:=sqrt((fSqr-sqr(fSum)/iCnt)/pred(iCnt)); //Abweichung
    fSum:=fSum/iCnt; //Mittelwert
    fFct:=1/(fSqr*2*fDvt); //Abweichung nach oben und unten
    fOfs:=fSum-fSqr*fDvt; //Offset für Null
    for Z:=1 to high(fxAtr[0]) do
      fxAtr[F,Z]:=(fxAtr[F,Z]-fOfs)*fFct;
  end;
  Tools.BitWrite(fxAtr,Result);
end;

// übersetzt Integer-Zonen-Attribut in Grauwerte

procedure tBuild._IntegerValues_(iaRgn:tnInt); //
var
  fxBnd:tn2Sgl=nil; //Kanal mit Regionen als Werte
  ixIdx:tn2Int=nil; //Zonen als Raster
  rHdr:trHdr; //gemeinsame Metadaten
  X,Y:integer;
begin
  //high(iaRgn) = Tools.CommaToLine.Count?
  Header.Read(rHdr,eeHme+cfIdx); //Zonen Metadaten
  ixIdx:=tn2Int(Image.ReadBand(0,rHdr,eeHme+cfIdx)); //Zonen Raster
  fxBnd:=Tools.Init2Single(length(ixIdx),length(ixIdx[0]),dWord(NaN)); //leerer Kanal
  for Y:=0 to high(ixIdx) do
    for X:=0 to high(ixIdx[0]) do
      if ixIdx[Y,X]>0 then //nur definierte Zonen
        fxBnd[Y,X]:=iaRgn[ixIdx[Y,X]]; //Regionen-ID als Grauwert
  Image.WriteBand(fxBnd,0,eeHme+cfCtm); //Regionen als Bild
  Header.WriteScalar(rHdr,eeHme+cfCtm);
end;

{ tCTI transformiert ein Klassen-Attribut in einen Klassen-Layer. }

function tBuild.ThemaImage(iaThm:tnInt):tn2Byt;
var
  ixIdx: tn2Int=nil; //Zellindex-Bild
  rHdr: trHdr; //Zellindex-Metadaten
  X,Y: integer;
begin
  Result:=nil;
  Header.Read(rHdr,eeHme+cfIdx); //Metadaten Zellindex
  Result:=Tools.Init2Byte(rHdr.Lin,rHdr.Scn); //
  ixIdx:=tn2Int(Image.ReadBand(0,rHdr,eeHme+cfIdx)); //Zellindex-Bild
  for Y:=0 to pred(rHdr.Lin) do
    for X:=0 to pred(rHdr.Scn) do
      Result[Y,X]:=iaThm[ixIdx[Y,X]];
end;

{ bIF gibt die Intensität aller Verknüpfungen aus der Drain-Analyse zurück. }

function tBuild.InterFlow:tnSgl;
var
  faWgt:tnSgl=nil; //Summe der Werte pro Zelle
  iaNxt:tnInt=nil; //Verknüpfung der Zelle
  Z:integer;
begin
  // Drain-Index geladen?
  iaNxt:=tnInt(Tools.BitExtract(1,eeHme+cfAtr)); //Verknüpfung, aus Selbstbezug
  faWgt:=Tools.BitExtract(3,eeHme+cfAtr); //Summe aller Werte der Zelle
  Result:=Tools.InitSingle(length(iaNxt),0);
  for Z:=1 to high(iaNxt) do
    Result[Z]:=min(faWgt[Z],faWgt[iaNxt[Z]]);
end;

{ bDd erzeugt ein Attribut mit der Kompaktheit der Zonen. Dazu bildet bDd das
  normalisierte Verhältnis zwischen äußeren und inneren Kontakten aller Pixel
  einer Zone und gibt es als Array zurück. }

function tBuild.Dendrites:tnSgl; //Attribut "Kompaktheit"
var
  paDim:^tnInt=nil; //Index auf "iacNbr, iacPrm"
  paNbr:^tnInt=nil; //Index der Nachbarzelle
  paPrm:^tnInt=nil; //Kontakte zur Nachbarzelle
  iExt,iInt:integer; //externe, interne Kontakte einer Zelle
  ixTpl:tn2Int=nil; //Zell-Topologie
  N,Z:integer;
begin
  //Topology defined?
  ixTpl:=tn2Int(Tools.BitRead(eeHme+cfTpl));
  paDim:=@ixTpl[0]; //Zeiger auf Startadressen
  paNbr:=@ixTpl[1]; //Zeiger auf IDs der Nachbarzellen
  paPrm:=@ixTpl[2]; //Zeiger auf Kontakte zu Nachbarzellen
  Result:=Tools.InitSingle(length(paDim^),0); //Umfang als Attribut
  for Z:=1 to high(paDim^) do //alle Zellen
  begin
    iExt:=0; iInt:=0; //Vorgabe
    for N:=paDim^[pred(Z)] to pred(paDim^[Z]) do //alle Kontakte (auch innere)
      if paNbr^[N]=Z //innere Kontakte
        then iInt+=paPrm^[N]
        else iExt+=paPrm^[N];
    if iExt>0 then
      Result[Z]:=iExt/(iInt+iExt) //Verhältnis innere/äußere Kontakte
  end;
end;

{ bRt gibt das Verhältnis zwischen Anzahl der Nachbarzonen und dem Umfang der
  zentralen Zone als Attribut zurück. Index und Topologie müssen existieren.
  Das Ergebnis sollte von der Größe der zentralen Zone unabhängig sein. }

function tBuild.Relations:tnSgl;
var
  paDim:^tnInt=nil; //Index auf "iacNbr, iacPrm"
  paNbr:^tnInt=nil; //Index der Nachbarzelle
  paPrm:^tnInt=nil; //Kontakte zur Nachbarzelle
  iPrm,iRes:integer; //Umfang der Zelle, Anzahl Kontakte
  ixTpl:tn2Int=nil; //Zell-Topologie (heterogen)
  N,Z:integer;
begin
  ixTpl:=tn2Int(Tools.BitRead(eeHme+cfTpl));
  paDim:=@ixTpl[0]; //Zeiger auf Startadressen
  paNbr:=@ixTpl[1]; //Zeiger auf IDs der Nachbarzellen
  paPrm:=@ixTpl[2]; //Zeiger auf Kontakte zu Nachbarzellen
  Result:=Tools.InitSingle(length(paDim^),0); //Verknüpfungen als Attribut
  for Z:=1 to high(paDim^) do //alle Zellen
  begin
    iPrm:=0; iRes:=0; //Vorgaben
    for N:=paDim^[pred(Z)] to pred(paDim^[Z]) do //alle Kontakte
      if paNbr^[N]<>Z then //äußere Kontakte
      begin
        iPrm+=paPrm^[N]; //Umfang
        inc(iRes) //Anzahl Kontakte
      end;
    if iPrm>0 then
      Result[Z]:=iRes/iPrm; //Verhältnis innere/äußere Kontakte}
  end;
end;

{ fNI erzeugt einen neuen Zonen-Index. In der Vorgabe bilden alle definierten
  Pixel eine Zone. }

function tUnion.NewIndex(
  fxImg:tn3Sgl; //Vorbild
  var iCnt:integer): //definierte Pixel
  tn2Int; //Zonen-IDs
var
  X,Y:integer;
begin
  Result:=Tools.Init2Integer(length(fxImg[0]),length(fxImg[0,0]),0); //Zonen-IDs
  for Y:=0 to high(fxImg[0]) do
    for X:=0 to high(fxImg[0,0]) do
    begin
      if isNan(fxImg[0,Y,X]) then continue; //Bild nicht definiert
      inc(iCnt); //fortlaufende Zonen-ID
      Result[Y,X]:=iCnt; //Pixel = Zone
    end;
end;

{ fNR erzeugt ein neues "Entropy"-Array mit einem "tapEtp" Record pro Zone. Die
  Records bestehen aus einem konstanten teil für Minimum, Größe und Verknüpfung
  und einem dynamischen Teil für Summe und Quadrat-Summe aller Pixel in allen
  Kanälen. Summen verwenden die niedrigere Hälfte der Indices, Quadrate die
  hohen Indices. Wenn später Flächen vereinigt werden, müssen nur die Record-
  Inhalte addiert werden. }

function tUnion.NewEntropy(
  fxImg:tn3Sgl; //Vorbild
  iCnt:integer; //definierte Pixel
  ixIdx:tn2Int): //Zonen-Index
  tapEtp; //Varianz, Size, Link, Sum, Square
var
  pBnd:^TnSgl=nil; //Zeiger auf Summen + Quadrate
  pEtp:tpEtp; //Zeiger auf Zonen-Merkmale
  iBnd:integer=0; //Anzahl Kanäle
  B,X,Y,Z:integer;
begin
  Result:=nil;
  SetLength(Result,succ(iCnt));
  iBnd:=length(fxImg); //Anzahl Kanäle
  for Z:=1 to iCnt do
  begin
    new(pEtp); //neue Zone
    pEtp^:=crEtp; //Vorgabe
    SetLength(pEtp^.aBnd,iBnd*2);
    FillDWord(pEtp^.aBnd[0],iBnd*2,0);
    Result[Z]:=pEtp;
  end;
  Result[0]:=nil;

  for Y:=0 to high(ixIdx) do
    for X:=0 to high(ixIdx[0]) do
    begin
      if ixIdx[Y,X]=0 then continue; //NoData
      pBnd:=@Result[ixIdx[Y,X]]^.aBnd; //Zeiger
      for B:=0 to high(fxImg) do //alle Kanäle
      begin
        pBnd^[B]+=fxImg[B,Y,X]; //Summe aller Werte pro Kanal
        pBnd^[iBnd+B]+=sqr(fxImg[B,Y,X]); //Quadrate aller Werte pro Kanal
      end;
      inc(Result[ixIdx[Y,X]]^.Sze); //Pixel pro Zone
    end;
end;

{ uME prüft alle Grenzen zwischen zwei Zonen ob ihre gemeinsame Entropie ein
  lokales Minimum ergeben würde und gibt die Verknüpfung mit der kleinsten
  Entropie als tapEtp^.Lnk zurück. uME verwendet als Maß für die "Entropie" die
  Gauß'sche Varianz aller Pixel einer Zone. }
{ lGL bestimmt die Entropie der vereinigten Zonen "iIdx" und "iNxt" und
  vergleicht sie mit bereits getesteten Alternativen. lGL registriert die ID
  der Zone mit der niedrigsten gemeinsamen Entropie. Um jede Kombination nur
  einmal zu berechnen, registriert lGL alle getesteten Kombinationen in
  "ixLnk". "ixLnk[Zone,0]" enthält die Zahl der Vergleiche. }

procedure tUnion.MinEntropy(
  apEtp:tapEtp; //Varianz, Size, Link, Summen, Quadrate
  iGrw:integer; //Zonen Wachstum eingeschränkt
  ixIdx:tn2Int); //Zonen-Index als Bild
const
  cLmt = MaxInt; //"unendliche" Varianz
var
  iBnd:integer; //Anzahl Kanäle
  ixLnk:tn2Int=nil; //getestete Verknüpfungen

procedure lGetLink(const iIdx,iNxt:integer); //Zonen-IDs
//Varianz = (∑x²-(∑x)²/n)/(n-1)
var
  fRes,fSqr,fSum:single; //Zwischenlager für Varianz
  pIdx,pNxt:tpEtp; //Zeiger auf trEtp-Eintrag
  iSze:integer; //gemeinsame Fläche
  B,I:integer;
begin
  if (iNxt=iIdx) //gleiche Zone
  or (iNxt=0) or (iIdx=0) then exit; //keine Definition
  for I:=1 to ixLnk[iIdx,0] do
    if ixLnk[iIdx,I]=iNxt then exit; //Vergleich existiert
  for I:=1 to ixLnk[iNxt,0] do
    if ixLnk[iNxt,I]=iIdx then exit;

  pIdx:=apEtp[iIdx];
  pNxt:=apEtp[iNxt];
  iSze:=pIdx^.Sze+pNxt^.Sze; //gemeinsame Fläche
  fRes:=0;
  for B:=0 to pred(iBnd) do
  begin
    fSum:=pIdx^.aBnd[B]+pNxt^.aBnd[B]; //Summe Dichte
    fSqr:=pIdx^.aBnd[iBnd+B]+pNxt^.aBnd[iBnd+B]; //Summe Dichte-Quadrate
    fRes+=(fSqr-sqr(fSum)/iSze)/pred(iSze); //Varianzen aller Kanäle
  end;
  case iGrw of
    0:; //keine Bindung
    1: fRes*=ln(iSze); //mäßige Anpassung
    2: fRes*=iSze; //starke Anpassung
  end;

  if fRes<pIdx^.Min then //neues Minimum
  begin
    pIdx^.Min:=fRes;
    pIdx^.Lnk:=iNxt;
    inc(ixLnk[iIdx,0]); //neuer Vergleich
    if length(ixLnk[iIdx])<=ixLnk[iIdx,0] then
      SetLength(ixLnk[iIdx],length(ixLnk[iIdx])*2); //Array erweitern
    ixLnk[iIdx,ixLnk[iIdx,0]]:=iNxt; //Verknüpfung
  end;

  if fRes<pNxt^.Min then //neues Minimum
  begin
    pNxt^.Min:=fRes;
    pNxt^.Lnk:=iIdx;
    inc(ixLnk[iNxt,0]); //neuer Vergleich
    if length(ixLnk[iNxt])<=ixLnk[iNxt,0] then
      SetLength(ixLnk[iNxt],length(ixLnk[iNxt])*2); //Array erweitern
    ixLnk[iNxt,ixLnk[iNxt,0]]:=iIdx; //Verknüpfung
  end;
end;

var
  X,Y,Z:integer;
begin
  iBnd:=length(apEtp[1]^.aBnd) div 2; //Anzahl Kanäle
  ixLnk:=Tools.Init2Integer(length(apEtp),4,0); //Verknüpfungen, Dimensuion=Vorgabe
  for Z:=1 to high(apEtp) do
  begin
    apEtp[Z]^.Min:=cLmt; //Vorgabe Varianz
    apEtp[Z]^.Lnk:=0; //Vorgabe = keine Verknüpfung
  end;
  for Y:=0 to high(ixIdx) do
    for X:=1 to high(ixIdx[0]) do
      lGetLink(ixIdx[Y,pred(X)],ixIdx[Y,X]);
  for Y:=1 to high(ixIdx) do
    for X:=0 to high(ixIdx[0]) do
      lGetLink(ixIdx[pred(Y),X],ixIdx[Y,X]);
end;

{ fIM gibt eine Transformations-Liste für neue, vereinigte Zonen-IDs zurück.
  Dazu erzeugt fIM zuerst eine Liste der bestehenden IDs und sucht nach
  rückbezüglichen Verweisen. fIM vergibt für jeden Treffer eine neue ID und
  markiert sie als negative Zahl. Wenn alle rückbezüglichen Verweise erfasst
  sind, vergibt fIM neue, fortlaufende IDs an alle anderen Zonen. fIM übergibt
  die höchste Zonen.ID als Result[0]. }

function tUnion.LinksMerge(
  apEtp:tapEtp; //bestehende Statistik
  ixIdx:tn2Int): //Zonen-Index
  tnInt; //neue Zonen-IDs
var
  iMrg:integer=0; //neue IDs
  Z:integer;
begin
  SetLength(Result,length(apEtp));
  for Z:=1 to high(apEtp) do
    Result[Z]:=apEtp[Z]^.Lnk; //bestehende Verknüpfungen
  Result[0]:=0;

  for Z:=1 to high(apEtp) do
    if (Result[Z]>0) //Verknüpfung ist registriert
    and (Result[Result[Z]]=Z) then //Rückbezug
    begin
      inc(iMrg); //neue Zone
      Result[Result[Z]]:=-iMrg; //markieren
      Result[Z]:=-iMrg
    end;

  for Z:=1 to high(Result) do
    if Result[Z]>=0 then //nicht verknüpfte Zone
    begin
      inc(iMrg); //neu zählen
      Result[Z]:=iMrg
    end
    else Result[Z]:=-Result[Z]; //Vorzeichen löschen

  Result[0]:=iMrg; //höchste ID
end;

{ fIM transformiert den Zonen-Index "iaIdx" und die Zonen-Attribute "apEtp"
  mit der Liste "iaLnk". Der Index erhält einfach andere Werte. fIM vereinigt
  zuerst Entropy-Records die zur gleichen neuen Zone gehören und verschiebt
  anschließend unveränderte Records in die neue Entropy-Liste. Dabei entfernt
  fIM Variable und konstante Komponenten der nicht mehr benötigten Records.
  fIM gibt die verkürzte Entropy-Liste zurück. }

function tUnion.IndexMerge(
  apEtp:tapEtp; //bestehende Statistik
  iaLnk:tnInt; //transformations-Liste
  ixIdx:tn2Int): //Zonen-Index
  tapEtp; //neue Statistik
var
  iBnd:integer; //Anzahl Kanäle
  pEtp,pRes:tpEtp; //Zeiger auf Zonen-Statistik
  B,X,Y,Z:integer;
begin
  SetLength(Result,succ(iaLnk[0]));
  FillDWord(Result[0],length(Result)*2,0);
  iaLnk[0]:=0;

  for Y:=0 to high(ixIdx) do
    for X:=0 to high(ixIdx[0]) do
      ixIdx[Y,X]:=iaLnk[ixIdx[Y,X]];

  iBnd:=length(apEtp[1]^.aBnd) div 2; //Anzahl Kanäle
  for Z:=1 to high(iaLnk) do //alle alten Zonen
    if Result[iaLnk[Z]]<>nil then
    begin
      pRes:=Result[iaLnk[Z]]; //Zeiger
      pEtp:=apEtp[Z]; //verknüpftes Element
      pRes^.Sze+=pEtp^.Sze; //Pixel pro Zone
      for B:=0 to pred(iBnd) do
        pRes^.aBnd[B]+=pEtp^.aBnd[B]; //Summe Dichte
      for B:=iBnd to pred(iBnd*2) do
        pRes^.aBnd[B]+=pEtp^.aBnd[B]; //QuadratSumme Dichte
      SetLength(pEtp^.aBnd,0); //altes Array frei geben
      dispose(pEtp); //alten Record frei geben
    end
    else Result[iaLnk[Z]]:=apEtp[Z]; //Zeiger kopieren
  Result[0]:=nil;
  SetLength(apEtp,0); //alte Liste frei geben
end;

{ fZs erzeugt einen neuen Zonen-Index als Bild und ein ESRI-Shape mit allen
  Grenzen zwischen zwei Zonen. fZs bestimmt die Varianz aller Kombinationen
  zwischen zwei Nachbar-Zonen und vereinigt lokale Minima. Der Prozess iteriert
  bis die mittlere Fläche (Pixel) aller Zonen "iSze" überschreitet. Mit
  "iGrw>1" wird die Varianz großer Zonen vergrößert. }

procedure tUnion.xImgZones(
  iGrw:integer; //Zonen-Wachstum einschränken [0,1,2]
  iSze:integer; //Pixel/Zone
  sImg:string); //Name Bilddaten
var
  apEtp:tapEtp=nil; //Varianzen als Liste

{ lCE löscht den Entropie-Speicher }

procedure lClearEntropy;
var
  I:integer;
begin
  for I:=0 to high(apEtp) do
    if apEtp[I]<>nil then
      SetLength(apEtp[I]^.aBnd,0);
  SetLength(apEtp,0);
end;

var
  fxImg:tn3Sgl=nil; //Vorbild, alle Kanäle
  iCnt:integer=0; //Anzahl Zonen
  iDef:integer=0; //Definierte Pixel im Index
  iaLnk:tnInt=nil; //Verknüpfungs-Liste
  ixIdx:tn2Int=nil; //Zonen-IDs
  rHdr:trHdr; //gemeinsamer Header
begin
  Header.Read(rHdr,sImg); //Metadaten
  fxImg:=Image.Read(rHdr,sImg); //Vorbild mit allen Kanälen
  ixIdx:=NewIndex(fxImg,iDef); //Index einrichten, 0=nicht definiert
  apEtp:=NewEntropy(fxImg,iDef,ixIdx); //Entropie-Record für alle Pixel
  repeat
    iCnt:=length(apEtp);
    MinEntropy(apEtp,iGrw,ixIdx); //Summen, Quadrate, Varianzen
    iaLnk:=LinksMerge(apEtp,ixIdx); //Verknüpfungen
    apEtp:=IndexMerge(apEtp,iaLnk,ixIdx); //neuer Index, Statistik
    write(#13+IntToStr(iCnt),#32);
  until (iDef/length(apEtp)>iSze) //mittlere Flächengröße
     or ((iCnt-length(apEtp))/(iCnt+length(apEtp))<0.0001); //leerlauf
  iCnt:=high(apEtp); //Anzahl definierte Zonen (ohne Null)
  lClearEntropy; //Speicher frei geben
  Image.WriteBand(tn2Sgl(ixIdx),0,eeHme+cfIdx); //Bilddaten schreiben
  Header.WriteIndex(iCnt,rHdr,eeHme+cfIdx); //Index-Header dazu
  Tools.HintOut(true,'ImageZones: '+IntToStr(iCnt));
  Build.IndexTopology(iCnt,ixIdx); //Topologie-Tabelle
  Gdal.ZonalBorders(eeHme+cfIdx); //Zellgrenzen als Shape
end;

{ bDn bestimmt die Standardaweichung zwischen allen Pixeln einer Zone und gibt
  das Ergebnis als Array für alle Zonen zurück. bDn bestimmt zunächst die
  Abweichung in jedem einzelnen Kanal und bildet dann aus den Ergebnissen der
  Kanäle die Hauptkomponente. Auf diese Weise werden auch Farbkontraste bei
  konstanter Helligkeit erfasst.
  → Die Varianz relativ zur Helligkeit ist als Kommentar notiert
  → Varianz = (∑x²-(∑x)²/n)/(n-1) }

function tBuild.Deviation(
  fxImg:tn3Sgl; //Vorbild
  iCnt:integer; //Anzahl Zonen
  ixIdx:tn2Int): //Zonen-IDs
  tnSgl; //Gauß'sche Abweichung pro Zone
var
  //faBrt:tnSgl=nil; //Zwischenlager für Helligkeit
  faSqr:tnSgl=nil; //Zwischenlager für Varianz
  faSum:tnSgl=nil;
  iaSze:tnInt=nil; //definierte Pixel pro Zone
  pBnd:^tn2Sgl=nil;
  B,X,Y,Z:integer;
begin
  Result:=Tools.InitSingle(succ(iCnt),0);
  //faBrt:=Tools.InitSingle(succ(iCnt),0);
  iaSze:=Tools.InitInteger(succ(iCnt),0);
  SetLength(faSqr,succ(iCnt));
  SetLength(faSum,succ(iCnt));

  for B:=0 to high(fxImg) do
  begin
    FillDWord(faSqr[0],succ(iCnt),0);
    FillDWord(faSum[0],succ(iCnt),0);
    pBnd:=@fxImg[B]; //Zeiger
    for Y:=0 to high(ixIdx) do
      for X:=0 to high(ixIdx[0]) do
        if not isNan(pBnd^[Y,X]) then //NoData-Pixel ignorieren
        begin
          faSqr[ixIdx[Y,X]]+=sqr(pBnd^[Y,X]); //für Varianz
          faSum[ixIdx[Y,X]]+=pBnd^[Y,X];
          if B=0 then inc(iaSze[ixIdx[Y,X]]); //definierte Pixel pro Zone
        end;
    for Z:=1 to iCnt do
      if iaSze[Z]>1 then
      begin
        Result[Z]+=(faSqr[Z]-sqr(faSum[Z])/iaSze[Z])/pred(iaSze[Z]);
        //faBrt[Z]+=sqr(faSum[Z]/iaSze[Z]); //für Hauptkomponente Helligkeit
      end;
  end;
  for Z:=1 to iCnt do
    if Result[Z]>0 then
      Result[Z]:=sqrt(Result[Z]); //Abweichung aus Varianz
  Result[0]:=0;
end;

{ bSF prüft ob Zonen-Raster und Vorbilder genau gleich groß sind. }

function tBuild.SizeFit_(sIdx,sStk:string):boolean; //Bildnamen
const
  cSze = 'bSF: Image size and zones size differ! ';
var
  rIdx,rStk:trHdr; //Metadaten
begin
  Header.Read(rStk,sStk); //Metadaten Vorbild
  Header.Read(rIdx,eeHme+cfIdx); //Metadaten Zonen
  Result:=(round(rIdx.Lat/rIdx.Pix)=round(rStk.Lat/rStk.Pix)) //linke obere Ecke
      and (round(rIdx.Lon/rIdx.Pix)=round(rStk.Lon/rStk.Pix))
      and (rIdx.Lin=rStk.Lin) and (rIdx.Scn=rStk.Scn) //Höhe, Breite
      and ((rIdx.Pix-rStk.Pix)/(rIdx.Pix+rStk.Pix)<1e-5); //Pixelgröße
  if not Result then Tools.ErrorOut(3,cSze);
end;

{ bCW bestimmt die Größe der Zonen direkt aus dem Index und gibt sie als
  Attribut zurück. Das Attribut enthält die Fläche in [ha]. }

function tBuild.ZonesSize(var rHdr:trHdr):tnSgl;
var
  fSze:single; //Faktor Pixel → Hektar
  ixIdx:tn2Int=nil; //Zellindex-Bild
  X,Y,Z:integer;
begin
  Result:=Tools.InitSingle(succ(rHdr.Cnt),0); //Vorgabe = leer
  ixIdx:=tn2Int(Image.ReadBand(0,rHdr,eeHme+cfIdx)); //Zellindex-Bild
  for Y:=0 to pred(rHdr.Lin) do
    for X:=0 to pred(rHdr.Scn) do
      Result[ixIdx[Y,X]]+=1; //Pixel pro Zone
  fSze:=sqr(rHdr.Pix)/10000; //Hektar pro Pixel
  for Z:=1 to rHdr.Cnt do
    Result[Z]*=fSze; //Fläche in [ha]
  Result[0]:=0;
end;

{ bDy leitet die spektrale Diversität direkt aus den Zonen-Attributen ab und
  gibt sie als neues Attribut zurück. Dazu benötigt bDy den Index, die Anzahl
  der scalaren Attribute und die Topologie. }
{ bDy bestimmt für jede benachbarte Zone und jeden Kanal die Varianz der
  spektralen Merkmale und gewichtet sie mit der Anzahl der Kontakte inclusive
  innere Kontakte. Für die verschiedenen Kanäle verwendet bDY die erste Haupt-
  Komponente. bDy trägt den Prozess-Namen als Feldname in den Zonen-Header ein }
{ todo: Diversity sollte wie "entropy" die Gauß'sche Diversität zwischen allen
  beteiligten Zonen messen - nicht nur zwischen der zentralen Zone und ihren
  Nachbarn }

function tBuild.Diversity(
  sImg:string): //Dateiname Bilddaten
  tnSgl; //Diversität
const
  cImg = 'bDy: Input image not available: ';
var
  fSum,fSqr:double; //Zwischenlager Summe, Quadrat-Summe
  fVrz:double; //Summe Varianzen der Kanäle
  fxVal:tn2Sgl=nil; //Zell-Attribute
  paDim:^tnInt=nil; //Index auf "iacNbr, iacPrm"
  paNbr:^tnInt=nil; //Index der Nachbarzelle
  paPrm:^tnInt=nil; //Kontakte zur Nachbarzelle
  iCnt:integer; //Summe Kontakte
  iSpc:integer=0; //spektrale Attribute
  ixTpl:tn2Int=nil; //Zell-Topologie
  B,N,Z:integer;
begin
  Result:=nil; //Vorgabe
  if length(sImg)<1 then Tools.ErrorOut(3,cImg+sImg);
  ixTpl:=tn2Int(Tools.BitRead(eeHme+cfTpl));
  paDim:=@ixTpl[0]; //Zeiger auf Startadressen
  paNbr:=@ixTpl[1]; //Zeiger auf IDs der Nachbarzellen
  paPrm:=@ixTpl[2]; //Zeiger auf Kontakte zu Nachbarzellen
  fxVal:=Tools.BitRead(eeHme+cfAtr); //Zell-Attribute
  Result:=Tools.InitSingle(length(paDim^),0); //Entropie-Attribut
  iSpc:=StrToInt(Header.ReadLine(true,'bands',sImg));
  for Z:=1 to high(paDim^) do
  begin
    fVrz:=0;
    for B:=0 to pred(iSpc) do //nur Spektralkanäle
    begin
      fSqr:=0; fSum:=0; iCnt:=0; //Vorgabe
      for N:=paDim^[pred(Z)] to pred(paDim^[Z]) do //alle Kontakte (auch innere)
      begin
        fSqr+=sqr(fxVal[B,paNbr^[N]])*paPrm^[N]; //Summe Quadrate
        fSum+=fxVal[B,paNbr^[N]]*paPrm^[N]; //Summe
        iCnt+=paPrm^[N]; //Anzahl Kontakte
      end;
      if iCnt>1 then
        fVrz+=(fSqr-sqr(fSum)/iCnt)/succ(iCnt); //Varianz im Kanal "B"
    end;
    if fVrz>=0 then
      Result[Z]:=sqrt(fVrz); //Hauptkomponente der Varianzen
  end;
end;

{ bPt bestimmt die "Textur" der Zonengröße als Attribut. Das Ergebnis ist das
  Verhältnis zwischen eigener Fläche und Mittelwert aller Flächen der Nachbar-
  Zonen. bPt verwendet die Summe der inneren Kontakte als Maß für die Fläche. }

function tBuild.Proportion:tnSgl;
var
  faSze:tnSgl=nil; //Zellgröße aus internen Kontakten
  paDim:^tnInt=nil; //Index auf "iacNbr, iacPrm"
  paNbr:^tnInt=nil; //Index der Nachbarzelle
  paPrm:^tnInt=nil; //Kontakte zur Nachbarzelle
  iNbr:integer; //Anzahl Nachbarzellen + Zentrum
  ixTpl:tn2Int=nil; //Zell-Topologie (heterogen)
  N,Z:integer;
begin
  ixTpl:=tn2Int(Tools.BitRead(eeHme+cfTpl));
  paDim:=@ixTpl[0]; //Zeiger auf Startadressen
  paNbr:=@ixTpl[1]; //Zeiger auf IDs der Nachbarzellen
  paPrm:=@ixTpl[2]; //Zeiger auf Kontakte zu Nachbarzellen
  Result:=Tools.InitSingle(length(paDim^),0); //Verknüpfungen als Attribut
  faSze:=Tools.InitSingle(length(paDim^),0); //Zellgröße aus internen Kontakten

  for Z:=1 to high(paDim^) do //alle Zellen
    for N:=paDim^[pred(Z)] to pred(paDim^[Z]) do //alle Kontakte
      if paNbr^[N]=Z then //innere Kontakte
        faSze[Z]:=succ(paPrm^[N]); //Summe innere Kontakte als Zellgröße
{ ToDo "proportion" sollte die Zonene-Größe in ha verwenden! "ZonesSize"
       ist bei ShapeFeatures dabei ← immer berechnen und übergeben }

  for Z:=1 to high(paDim^) do //alle Zellen
  begin
    iNbr:=0; //Vorgabe
    for N:=paDim^[pred(Z)] to pred(paDim^[Z]) do //alle Kontakte
      if paNbr^[N]<>Z then //Nachbar-Zone
      begin
        Result[Z]+=faSze[paNbr^[N]]; //Flächen summieren
        inc(iNbr) //Flächen zählen
      end;
    if Result[Z]>0
      then Result[Z]:=faSze[Z]/Result[Z]*iNbr //Verhältnis zum Mittelwert
      else Result[Z]:=0;
  end;
end;

{ bIT erzeugt eine heterogene Tabelle "topology.bit". Die Tabelle enthält in
  der ersten Spalte den Index "iaDim" mit der Zahl der Einträge pro Zone, in
  der zweiten Spalte "iaNbr" für jede Zelle die IDs aller Nachbarzellen und in
  der dritten Spalte "iaPrm" die Anzahl der Kontakte. IT enthält auch innere
  Kontakte. IT ignoriert NoData-Pixel.
    IT erzeugt znächst ein Zwischenprodukt "ixNbr" und "ixPrm" die für jede
  Zone ein eigenes Array besitzen. Die Anzahl der gültigen Einträge steht in
  "iaNbr[?,0]". IT passt die Länge der Arrays dynamisch an den Bedarf an. IT
  scannt den Zellindex getrennt vollständig horizontal und vertikal. Am Ende
  übersetzt IT die Zwischenprodukte in die Tabelle und speichert sie als BIT-
  Datei. }

procedure tBuild.IndexTopology(
  iCnt:integer; //Anzahl Zonen (höchste ID)
  ixIdx:tn2Int); //Zonen-Raster
var
  ixNbr: Tn2Int=nil; //Nachbar-Zell-IDs
  ixPrm: Tn2Int=nil; //Kontakte zu Nachbar-Zellen

procedure lLink(const iLow,iHig:integer);
var
  N: integer;
begin
  if (iLow<1) or (iHig<1) then exit; //NoData ignorieren

  for N:=1 to ixNbr[iLow,0] do
    if ixNbr[iLow,N]=iHig then
    begin
      inc(ixPrm[iLow,N]);
      exit;
    end; //if iaNbr[iLow,N]=iHig

  inc(ixNbr[iLow,0]);
  if ixNbr[iLow,0]>high(ixNbr[iLow]) then
  begin
    SetLength(ixNbr[iLow],ixNbr[iLow,0]*2);
    SetLength(ixPrm[iLow],ixNbr[iLow,0]*2);
  end; //if ixNbr[iLow,0] ..
  ixNbr[iLow,ixNbr[iLow,0]]:=iHig;
  ixPrm[iLow,ixNbr[iLow,0]]:=1;
end; //lLink.

procedure lLinksIndex(ixIdx:tn2Int);
var
  X,Y,Z: integer;
begin
  for Z:=1 to iCnt do
  begin
    ixNbr[Z]:=Tools.InitInteger(7,0);
    ixPrm[Z]:=Tools.InitInteger(7,0);
  end; //if ixFtr[1,Z]<iMin

  for Y:=1 to high(ixIdx) do
    for X:=0 to high(ixIdx[0]) do
    begin
      lLink(ixIdx[pred(Y),X],ixIdx[Y,X]);
      lLink(ixIdx[Y,X],ixIdx[pred(Y),X]);
    end; //for X ..

  for Y:=0 to high(ixIdx) do
    for X:=1 to high(ixIdx[0]) do
    begin
      lLink(ixIdx[Y,pred(X)],ixIdx[Y,X]);
      lLink(ixIdx[Y,X],ixIdx[Y,pred(X)]);
    end; //for X ..
end; //lLinksIndex.

procedure lInternal;
var
  N,Z: integer;
begin
  for Z:=1 to iCnt do
    for N:=1 to ixNbr[Z,0] do
      if ixNbr[Z,N]=Z then
        ixPrm[Z,N]:=ixPrm[Z,N] div 2;
end; //lInternal.

procedure lIndexSave;
var
  iaDim: TnInt=nil; //Abschnitte in "iaNbr"
  iaNbr: TnInt=nil; //Zell-ID Kontakt
  iaPrm: TnInt=nil; //Anzahl Kontakte
var
  iDim: integer; //Topologie-Dimension bis zur Zelle "Z"
  Z: integer;
begin
  iaDim:=Tools.InitInteger(succ(iCnt),0);
  iaNbr:=Tools.InitInteger(succ(iCnt),0);
  iaPrm:=Tools.InitInteger(succ(iCnt),0);
  for Z:=1 to iCnt do
  begin
    iDim:=iaDim[pred(Z)]+ixNbr[Z,0]; //neue Dimension
    if iDim>high(iaNbr) then
    begin
      SetLength(iaNbr,round(iDim*sqrt(2)));
      SetLength(iaPrm,length(iaNbr));
    end; //if iaDim[pred(Z)] ..
    move(ixNbr[Z,1],iaNbr[iaDim[pred(Z)]],ixNbr[Z,0]*SizeOf(integer));
    move(ixPrm[Z,1],iaPrm[iaDim[pred(Z)]],ixNbr[Z,0]*SizeOf(integer));
    iaDim[Z]:=iDim;
    SetLength(ixNbr[Z],0); //Speicher freigeben
    SetLength(ixPrm[Z],0);
  end; //for Z ..
  SetLength(iaNbr,iaDim[iCnt]); //Speicher reduzieren
  SetLength(iaPrm,iaDim[iCnt]);
  DeleteFile(eeHme+cfTpl);
  Tools.BitInsert(tnSgl(iaDim),0,eeHme+cfTpl);
  Tools.BitInsert(tnSgl(iaNbr),1,eeHme+cfTpl);
  Tools.BitInsert(tnSgl(iaPrm),2,eeHme+cfTpl);
end; //lIndexSave.

const
  cIdx = 'iPIT: Cell index file not provided: ';
begin
  if not FileExists(eeHme+cfIdx) then Tools.ErrorOut(3,cIdx+eeHme+cfIdx);
  ixNbr:=Tools.Init2Integer(succ(iCnt),1,0); //Container
  ixPrm:=Tools.Init2Integer(succ(iCnt),1,0);
  lLinksIndex(ixIdx); //Pixel-Kontakte indizieren (Y*2)
  lInternal; //interne Grenzen einfach zählen
  lIndexSave; //Topologie indizieren und speichern (Z)
  Tools.HintOut(true,'IndexTopology: '+cfTpl)
end;

{ uBs erzeugt Zonen aus einer Maske oder einem Klassen-Layer. uBs bildet alle
  Grenzen (auch zu NoData) als Zonen-Grenzen ab. uBs akzeptiert als Vorbild nur
  einzelne Layer im Byte-Format. uBs interpretiert Null im Vorbild als nicht
  definierten Bereich.
    uBs füllt die klassifizierten Flächen mit einem Flood-Algorithmus. Dazu
  prüft uBs systemetisch die Umgebung von jeden Pixel auf identische Nachbar-
  Pixel und erweitert den Index entsprechend. Zusammenhängende Flächen müssen
  durch mindestens eine Pixel-Kante miteinander verbunden sein. Der Suchpfad
  "iacChn" und die Zeiger "pcIdx" und "pcMap" sind in der Klasse definiert um
  das Interface klein zu halten.
    "BORDERS" IST ALS MODUL FORMATIERT, DAS VON GLOBALEN ROUTINEN AUFGERUFEN
    WERDEN KANN }

function tUnion.Borders(
  var iRes:integer; //Anzahl Zonen
  ixMap:tn2Byt): //Vorbild-Klassen
  tn2Int; //Zonen-IDs
{ iacCnt:tnInt; //Pixel-Indices auf Suchpfad NUR FÜR "xBorders"
  pcIdx:^tn2Int; //Zonen-IDs NUR FÜR "Borders"
  pcMap:^tn2Byt; //Klassen-Layer NUR FÜR "Borders" }
var
  iChn:integer=0; //Position aktueller Pixel in "iaxChn"
  iHrz,iVrt:integer; //Pixel-Position
  iScn:integer; //Bildbreite (lokal)
  X,Y:integer;
begin
  //ixMap gesetzt?
  iRes:=0; //Anzahl Zonen
  Result:=Tools.Init2Integer(length(ixMap),length(ixMap[0]),0); //Zonen-IDs, leer
  iacPix:=Tools.InitInteger($100,0); //Pixelindices geprüfte Pixel, leer
  iScn:=length(ixMap[0]); //Bildbreite für Pixelindex
  for Y:=0 to high(ixMap) do
    for X:=0 to high(ixMap[0]) do
    begin
      if ixMap[Y,X]=0 then continue; //Bild nicht definiert
      if Result[Y,X]>0 then continue; //Zonen-ID vergeben
      inc(iRes); //neue Zone
      iacPix[0]:=1; //ein gültiger Eintrag
      iacPix[1]:=Y*iScn+X; //erster Pixelindex
      iChn:=1; //Position der Prüfung
      Result[Y,X]:=iRes; //Wert vergeben
      repeat
        iVrt:=iacPix[iChn] div iScn;
        iHrz:=iacPix[iChn] mod iScn;
        if iVrt>0 then PatchGrow(pred(iVrt),iHrz,iVrt,iHrz);
        if iHrz>0 then PatchGrow(iVrt,pred(iHrz),iVrt,iHrz);
        if iVrt<high(ixMap) then PatchGrow(succ(iVrt),iHrz,iVrt,iHrz);
        if iHrz<high(ixMap[0]) then PatchGrow(iVrt,succ(iHrz),iVrt,iHrz);
        inc(iChn) //nächster Pixel
      until iChn>iacPix[0];
    end;
end;

{ uBs erzeugt Zonen aus einem Klassen-Layer. uBs bildet alle Klassen-Grenzen
  als Zonen-Grenzen ab. uBs akzeptiert als Vorbild nur einzelne Layer im Byte-
  Format. uBs interpretiert Null im Vorbild als nicht definierte Bereiche.
    uBs füllt die klassifizierten Flächen mit einem Flood-Algorithmus. Dazu
  prüft uBs systemetisch die Umgebung von jeden Pixel auf identische Nachbar-
  Pixel und erweitert den Index entsprechend. Zusammenhängende Flächen müssen
  durch mindestens eine Pixel-Kante miteinander verbunden sein. Das Vorbild
  "ixcMap", der Zonen-Index "ixcIdx" und der Suchpfad "iacPix" sind in der
  Klasse definiert um das Interface klein zu halten. }

procedure tUnion.xMapZones(sImg:string); //Vorbild (Klassen)
{ iacCnt:tnInt; //Pixel-Indices auf Suchpfad NUR FÜR "xBorders"
  ixcIdx:tn2Int; //Zonen-IDs NUR FÜR "xBorders"
  ixcMap:tn2Byt; //Klassen-Layer NUR FÜR "xBorders" }
var
  iRes:integer=0; //Zonen-ID → Anzahl Zonen
  rHdr:trHdr; //gemeinsamer Header
begin
  Header.Read(rHdr,sImg); //Vorbild
  //if rHdr.Fmt<>1 then
  ixcMap:=Image.ReadThema(rHdr,sImg); //Klassen-Layer lesen
  ixcIdx:=Borders(rHdr.Cnt,ixcMap);
  Image.WriteBand(tn2Sgl(ixcIdx),0,eeHme+cfIdx); //Bilddaten schreiben
  Header.WriteIndex(iRes,rHdr,eeHme+cfIdx); //Index-Header dazu
  Tools.HintOut(true,'MapZones: '+IntToStr(iRes));
  Build.IndexTopology(iRes,ixcIdx); //Topologie-Tabelle
  Gdal.ZonalBorders(eeHme+cfIdx); //Zellgrenzen als Shape
  SetLength(ixcIdx,0);
  SetLength(ixcMap,0);
  SetLength(iacPix,0);
end;

{ uPG prüft ob der Pixel [iLat,iLon] zur gleichen Klasse gehört wie [iVrt,iHrz]
  und markiert akzeptierte Pixel mit der Zonen-ID des Vorbilds. uPG trägt neu
  registrierte Pixel in den Suchpfad "iacPix" ein. "xBorders" übergibt diese
  Pixel systematisch an uPG bis alle Pixel geprüft sind. }

procedure tUnion.PatchGrow(
  iLat,iLon:integer; //neuer Pixel
  iVrt,iHrz:integer); //alter Pixel
{ iacCnt:tnInt; //Pixel-Indices auf Suchpfad NUR FÜR "xBorders"
  ixcIdx:tn2Int; //Zonen-IDs NUR FÜR "xBorders"
  ixcMap:tn2Byt; //Klassen-Layer NUR FÜR "xBorders"}
begin
  if (ixcMap[iLat,iLon]=ixcMap[iVrt,iHrz]) and //gleiche Klasse
     (ixcIdx[iLat,iLon]=0) then //Pixel nicht geprüft
  begin
    inc(iacPix[0]); //neuer Test
    if length(iacPix)<=iacPix[0] then
      SetLength(iacPix,length(iacPix)*2); //neuer Speicher
    iacPix[iacPix[0]]:=iLat*length(ixcMap[0])+iLon; //Pixelindex
    ixcIdx[iLat,iLon]:=ixcIdx[iVrt,iHrz] //ID übernehmen
  end
end;

{ bFI überträgt alle Zonen-Attribute auf ein Raster-Bild im Float-Format }
{ bFI konvertiert Zonen-Attribute zu einem Bild mit Zonen-Mittelwerten. Mit
  "faAtr<>nil" verwendet bFI nur das übergebene Attribut, mit "faVal"=nil alle
  Attribute. bFI überträgt die Zone Null als NoData }

procedure tBuild.AttributeImage(faVal:tnSgl);
var
  fxAtr:tn2Sgl=nil; //vollständige Attribut-Tabelle ODER nil
  fxBnd:tn2Sgl=nil; //Kanal mit Attribut-Werten
  iHig:integer=0; //höchste Attribut-ID
  ixIdx:tn2Int=nil; //Zonen als Raster
  pVal:^tnSgl=nil; //Zeiger auf Attribut
  rHdr:trHdr; //gemeinsame Metadaten
  I,X,Y:integer;
begin
  //Tools.BitSize = Tools.CommaToLine.Count?
  if faVal=nil then fxAtr:=Tools.BitRead(eeHme+cfAtr); //alle Zonen Attribute
  Header.Read(rHdr,eeHme+cfIdx); //Zonen Metadaten
  ixIdx:=tn2Int(Image.ReadBand(0,rHdr,eeHme+cfIdx)); //Zonen Raster
  fxBnd:=Tools.Init2Single(length(ixIdx),length(ixIdx[0]),dWord(NaN)); //leerer Kanal
  if faVal=nil then iHig:=high(fxAtr); //alle Attribute
  for I:=0 to iHig do
  begin
    if faVal=nil
      then pVal:=@fxAtr[I]
      else pVal:=@faVal;
    for Y:=0 to high(ixIdx) do
      for X:=0 to high(ixIdx[0]) do
        if ixIdx[Y,X]>0 then
          fxBnd[Y,X]:=pVal^[ixIdx[Y,X]]; //nur definierte Bildpixel
    Image.WriteBand(fxBnd,I,eeHme+cfVal);
  end;
  if faVal=nil
    then Header.WriteMulti(1,length(fxAtr),rHdr,Tools.CommaToLines(rHdr.Fld),eeHme+cfVal)
    else Header.WriteScalar(rHdr,eeHme+cfVal);
  Tools.HintOut(true,'AttributeImage: '+cfVal);
end;

{ dII überträgt Zonen-Attribute im Integer-Format auf ein Raster-Bild }
{ dII wurde zur Visualisierung implementiert. }

procedure tBuild.IntegerImage(iaCtm:tnInt); //Catchment-IDs
var
  iBsn:integer; //Anzahl Catchments
  ixBsn:tn2Int=nil; //Catchments als Bild
  ixIdx:tn2Int=nil; //Zonen als Bild
  rIdx:trHdr; //Metadaten Zonen
  X,Y:integer;
begin
  Header.Read(rIdx,eeHme+cfIdx); //Zonen Metadaten
  ixIdx:=tn2Int(Image.ReadBand(0,rIdx,eeHme+cfIdx)); //Zonen Raster
  ixBsn:=Tools.Init2Integer(rIdx.Lin,rIdx.Scn,0); //Catchment-Bild
  iBsn:=MaxIntValue(iaCtm); //Anzahl Catchments

  for Y:=0 to pred(rIdx.Lin) do
    for X:=0 to pred(rIdx.Scn) do
      ixBsn[Y,X]:=iaCtm[ixIdx[Y,X]];
  Image.WriteBand(tn2Sgl(ixBsn),0,eeHme+cfCtm); //Catchments als Bild
  Header.WriteIndex(iBsn,rIdx,eeHme+cfCtm); //Index-Header dazu
  Tools.HintOut(true,'IntegerImage: '+cfCtm);
end;

{ bFs erweitert die Attribut-Tabelle "index.bit" mit Attributen aus der
  Geometrie und den spektralen Attributen ganzer Zonen. Wenn "index.bit" nicht
  existiert, erzeugt bFs eine neue Tabelle. Zellindex und Topologie müssen
  existieren. bFs trägt die Prozess-Namen aus "slCmd" als Feldnamen in den
  Index-Header ein. "Drainlines" muss nach "runoff" aufgerufen werden. }

procedure tBuild.xShapeFeatures(
  sImg:string; //Name Bilddaten ODER leer
  slCmd:tStringList); //Prozesse + Ergebnis-Namen
const
  cCmd = 'Error(bA): Command not appropriate to run "attributes": ';
var
  faVal:tnSgl=nil; //gewähltes Attribut
  rHdr:trHdr; //gemeinsame Metadaten
  I:integer;
begin
  if slCmd.Count<1 then exit; //kein Aufruf
  Header.Read(rHdr,eeHme+cfIdx); //Zonen
  if FileExists(eeHme+cfAtr) //Attribute vorhanden
    then rHdr.Fld:=rHdr.Fld+','+slCmd.CommaText //Feldnamen aus Bilddaten ergänzen
    else rHdr.Fld:=slCmd.CommaText; //Feldnamen ersetzen
  Header.Write(rHdr,'Shape features',eeHme+cfIdx); //Feldnamen speichern

  for I:=0 to pred(slCmd.Count) do
  begin
    if slCmd[I]=cfDdr then faVal:=Dendrites else
    if slCmd[I]=cfDvs then faVal:=Diversity(sImg) else
    if slCmd[I]=cfItf then faVal:=Interflow else
    if slCmd[I]=cfPrp then faVal:=Proportion else
    if slCmd[I]=cfRlt then faVal:=Relations else
    if slCmd[I]=cfSze then faVal:=ZonesSize(rHdr) else
      Tools.ErrorOut(3,cCmd+slCmd[I]); //nicht definierter Befehl
    Tools.BitInsert(faVal,$FFF,eeHme+cfAtr); //bestehende Attribute erzeugen oder erweitern
  end;
  Tools.HintOut(true,'ShapeFeatures: '+cfAtr);
end;

{ dDI erzeugt einen neuen Zonen-Index-Vorläufer. Er besteht aus Null für lokale
  Minima, [-1..-4] für Abfluss in Richtung [↑,→,↓,←] und -MaxInt für NoData. }
{ Die Strategie ist indirekt. dDI schreibt in jeden Pixel die Zahlen [-1..-4]
  für den steilsten Abfluss. Lokale Minima und NoData bleiben übrig. dDI gibt
  NoData als hohe negative Zahl zurück }

function tUnion.DemIndex(
  fxElv:tn2Sgl): //Höhenmodell als Bild
  tn2Int; //vorläufiger Zonen-Index aus Abfluss-Richtung und Null für lokale Minima
const
  cMin:integer=1-MaxInt;
var
  fxMin:tn2Sgl=nil; //Höhe des niedrigsten Nachbar-Pixels

{ lF trägt die steilste Abfluss-Richtung in einzelne Pixel ein. dDI verwendet
  nur direkte Nachbarpixel. Die Reihenfolge der Zahlen ist rechtsdrehend. }

procedure lFlow(
  iDrc:integer; //Flussrichtung [-1..-4] im Uhrzeigersinn (-1=oben)
  iHrz,iVrt:integer; //Ausgangspunkt [X,Y]
  iLat,iLon:integer); //Nachbar-Punkt [Y,X]
begin
  if fxElv[iLat,iLon]<fxMin[iVrt,iHrz] then
  begin
    Result[iVrt,iHrz]:=iDrc; //Richtung [-1..-4]
    fxMin[iVrt,iHrz]:=fxElv[iLat,iLon];
  end;
end;

var
  iHrz,iVrt:integer; //höchster Pixel-Index vertikal, horizontal
  X,Y:integer;
begin
  Result:=nil; //Sicherheit
  fxMin:=tn2Sgl(Tools.CopyGrid(tn2Int(fxElv))); //fxElv kopieren
  Result:=Tools.Init2Integer(length(fxElv),length(fxElv[0]),0); //Catchments

  for Y:=0 to high(fxElv) do
    for X:=0 to high(fxElv[0]) do
      if isNan(fxElv[Y,X]) then
        Result[Y,X]:=cMin; //NoData auf Ergebnis übertragen

  iVrt:=high(fxElv);
  iHrz:=high(fxElv[0]);
  for Y:=0 to high(fxElv) do
    for X:=0 to high(fxElv[0]) do
      if Result[Y,X]>cMin then
      begin
        if X>0 then lFlow(-4,X,Y,Y,pred(X));
        if Y>0 then lFlow(-1,X,Y,pred(Y),X);
        if X<iHrz then lFlow(-2,X,Y,Y,succ(X));
        if Y<iVrt then lFlow(-3,X,Y,succ(Y),X);
      end;
end;

{ dZG bestimmt den Region-Grow von "ExtendZero" für einzelne Pixel. Dazu trägt
  dZG jeden passenden Nachbar-Pixel in die Kette "iacPix" ein und erweitert bei
  Bedatf die Kette. "iacPix" muss deshalb in der Klasse definiert werden. }

procedure tUnion.ZoneGrow(
  iLat,iLon:integer; //neuer Pixel
  iVrt,iHrz:integer; //alter Pixel
  ixDrn:tn2Int);
begin
  if ixDrn[iLat,iLon]=0 then //Pixel nicht geprüft
  begin
    inc(iacPix[0]); //neuer Test
    if length(iacPix)<=iacPix[0] then
      SetLength(iacPix,length(iacPix)*2); //neuer Speicher
    iacPix[iacPix[0]]:=iLat*length(ixDrn[0])+iLon; //Pixelindex
    ixDrn[iLat,iLon]:=ixDrn[iVrt,iHrz] //ID übernehmen
  end
end;

{ dEZ zählt alle lokalen Minima (Wert=Null) im vorläufigen Zonen-Index "ixDrn".
  dEZ gibt für zusammenhängende Ebenen nur eine Zonen-ID zurück. }
{ Höhenmodelle zeigen für Gewässer und andere Ebenen konstante Werte. uEZ
  versucht deshalb jeden Pixel mit dem Wert Null mit einem Region-Grow Prozess
  auf eine beliebig geformte Fläche auszuweiten. uEZ sucht mit Hilfe der Kette
  "iacPix" systematisch alle möglichen Pixel-Nachbarn ab. }

function tUnion.ExtendZero(ixDrn:tn2Int):integer;
var
  iChn:integer=0; //Position aktueller Pixel in "iaxChn"
  iHrz,iVrt:integer; //Pixel-Position
  iScn:integer; //Bildbreite (lokal)
  X,Y:integer;
begin
  Result:=0; //Anzahl Zonen
  iacPix:=Tools.InitInteger($100,0); //Pixelindices geprüfte Pixel, leer
  iScn:=length(ixDrn[0]); //Bildbreite (lokal)
  for Y:=0 to high(ixDrn) do
    for X:=0 to high(ixDrn[0]) do
    begin
      if ixDrn[Y,X]<>0 then continue; //kein Minimum
      inc(Result); //neue Zone
      iacPix[0]:=1; //ein gültiger Eintrag
      iacPix[1]:=Y*iScn+X; //erster Pixelindex
      iChn:=1; //Position der Prüfung
      ixDrn[Y,X]:=Result; //Wert vergeben
      repeat
        iVrt:=iacPix[iChn] div iScn;
        iHrz:=iacPix[iChn] mod iScn;
        if iVrt>0 then ZoneGrow(pred(iVrt),iHrz,iVrt,iHrz,ixDrn);
        if iHrz>0 then ZoneGrow(iVrt,pred(iHrz),iVrt,iHrz,ixDrn);
        if iVrt<high(ixDrn) then ZoneGrow(succ(iVrt),iHrz,iVrt,iHrz,ixDrn);
        if iHrz<high(ixDrn[0]) then ZoneGrow(iVrt,succ(iHrz),iVrt,iHrz,ixDrn);
        inc(iChn) //nächster Pixel
      until iChn>iacPix[0];
    end;
  SetLength(iacPix,0); //Speicher frei geben
end;

{ dEI erweitert die Nummern der lokalen Minima auf das gesamte Catchment. Nach
  dEI ist der Zonen-Index vollständig }
{ Dazu verfolgt dEI systematisch alle Pixel mit einem Rictungs-Verweis [-1..-4]
  bis zu einem Pixel mit einer (positiven) Zonen-ID. dEI regiestriert die ID
  und wiederholt die Kette der Verweise. Dabei trägt dEI die gefundene Zonen-ID
  ein. }

procedure tUnion.ExtendIndex(ixIdx:tn2Int);
var
  iIdx:integer; //Zonen-ID am Ende der verweis-Kette
  iHrz,iVrt:integer; //Nachbar-Koordinaten
  iLat,iLon:integer; //Ausgans-Koordinaten
  X,Y:integer;
begin
  for Y:=0 to high(ixIdx) do
    for X:=0 to high(ixIdx[0]) do
      if (ixIdx[Y,X]<0) and (ixIdx[Y,X]>-5) then //nur Richtungen
      begin
        iHrz:=X; iVrt:=Y;
        repeat
          case ixIdx[iVrt,iHrz] of
            -4:dec(iHrz); //nächster Pixel
            -3:inc(iVrt);
            -2:inc(iHrz);
            -1:dec(iVrt);
          end;
        until ixIdx[iVrt,iHrz]>0; //Zonen-ID

        iIdx:=ixIdx[iVrt,iHrz]; //Catchment-ID merken
        iHrz:=X; iVrt:=Y; //von vorne beginnen
        repeat
          iLat:=iVrt; iLon:=iHrz; //alte Koordinaten
          case ixIdx[iVrt,iHrz] of
            -4:dec(iHrz); //nächster Pixel
            -3:inc(iVrt);
            -2:inc(iHrz);
            -1:dec(iVrt);
          end;
          ixIdx[iLat,iLon]:=iIdx;
        until ixIdx[iVrt,iHrz]>0;
      end;
end;

{ uEn erzeugt Zonen aus einem Höhenmodell und speichert sie als "index" }
{ (1) uEn lokalisiert alle lokalen Minima im Höhenmodell "sDem" und zählt sie
  fortlaufend (DemIndex). (2) uEn erweitert lokale Ebenen (Stillwasser) zu
  flächigen Minima (ExtendZero). (3) uEn bestimmt die (Micro-) Catchments der
  lokalen Minima und verwendet ihre Grenzen als Zonen (ExtendIndex(). (4) uEn
  speichert den Zonen-Index, die Zonen-Topologie, die Zonen-Polygone und das
  Zonen-Attribut wie bei gewöhnlichen Bilddaten. }

procedure tUnion.xDemZones(sDem:string);
var
  fxElv:tn2Sgl=nil; //Höhenmodell als Bild
  iCnt:integer; //Anzahl Zonen
  ixIdx:tn2Int=nil; //Zonen-Index
  rHdr:trHdr; //Metadaten, WERDEN VERÄNDERT!
begin
  Header.Read(rHdr,sDem); //Vorbild Metadaten
  fxElv:=Image.ReadBand(0,rHdr,sDem); //Vorbild Rasterdaten
  ixIdx:=DemIndex(fxElv); //lokale Minima (null) und Verknüpfungen (negativ)
  iCnt:=ExtendZero(ixIdx); //lokale Minima für Ebenen erweitern, Zonen zählen
  ExtendIndex(ixIdx); //Verweise mit Zonen-ID füllen
{ TODO: Im neuen Index steht "1-MaxInt" für NoData im Höhenmodell. Das wird nie
        aufgelöst!}
  Image.WriteBand(tn2Sgl(ixIdx),0,eeHme+cfIdx); //Index als Bild speichern
  Header.WriteIndex(iCnt,rHdr,eeHme+cfIdx); //Index-Header dazu
  Build.IndexTopology(iCnt,ixIdx); //Topologie-Tabelle erzeugen
  Gdal.ZonalBorders(eeHme+cfIdx); //Zellgrenzen als Shape
  Tools.HintOut(true,'DemZones: '+cfIdx);
end;

{ bFD mischt die Attribute "faVal" von benachbarten Zonen in Analogie zu einer
  Diffusion löslicher Stoffe. bFD wiederholt den Vorgang "iGen" mal. }
{ Die Diffusion ist nur von der Anzahl der Kontakte abhängig. Jede Zone "saugt"
  Merkmale aus allen Nachbarzonen, ergänzt damit die eigenen (Anzahl innere
  Kontakte) und übernimmt den Mittelwert der Mischung. Große Zonen verändern
  sich langsamer als kleine. }

procedure tBuild.FeatureDrain(
  faVal:tnSgl; //Attribute (Vorbild => Ergebnis)
  iGen:integer; //Iterationen
  ixTpl:tn2Int); //Zonen-Topologie
var
  faTmp:tnSgl=nil; //Zwischenlager für Vorgaben
  fVal:single=0; //Werte aus Nachbar-Zonen
  iCnt:integer=0; //Anzahl Kontakte
  paDim:^tnInt=nil; //Topoligie-Index
  paNbr:^tnInt=nil; //Nachbar-Zonen-ID
  paPrm:^tnInt=nil; //Kontakte zur Nachbar-Zone
  G,N,Z:integer;
begin
  faTmp:=Tools.InitSingle(length(faVal),0); //neues Gravity-Attribut
  paDim:=@ixTpl[0]; //Zeiger auf Startadressen
  paNbr:=@ixTpl[1]; //Zeiger auf Nachbarzonen-IDs
  paPrm:=@ixTpl[2]; //Anzahl der Kontakte

  for G:=1 to iGen do
  begin
    move(faVal[0],faTmp[0],length(faVal)*SizeOf(single)); //Backup altes Attribut
    FillDWord(faVal[0],length(faVal),0); //bestehendes Attribut leeren
    for Z:=1 to high(ixTpl[0]) do //alle Zonen
    begin
      fVal:=0; iCnt:=0;
      for N:=paDim^[pred(Z)] to pred(paDim^[Z]) do //alle Kontakte, auch innere!
        if not isNan(faTmp[paNbr^[N]]) then
        begin
          fVal+=faTmp[paNbr^[N]]*paPrm^[N];
          iCnt+=paPrm^[N]
        end;
      if iCnt>0 then faVal[Z]:=fVal/iCnt
    end;
  end;
end;

{ bDn mischt die Attribute jeder Zone und allen direkten Nachbar-Zonen und
  speichert das Ergebnis unter dem ursprünglichen Namen. bDn wiederholt den
  Vorgang "iGen" mal. Die Attribute müssen existieren }

procedure tBuild.xDiffusion(
  iGen:integer; //Nachbar-Generationen
  sAtr:string); //Zonen-Attribute
const
  cGen = 'Error(bDn): Iterations must be positive!';
  cVal = 'Error(bDn): Attribute table not found!';
var
  fxAtr:tn2Sgl=nil; //Attribut-Tabelle
  ixTpl:tn2Int=nil; //Zell-Topologie
  A:integer;
begin
  if not FileExists(sAtr) then Tools.ErrorOut(3,cVal);
  if iGen<1 then Tools.ErrorOut(3,cGen);
  ixTpl:=tn2Int(Tools.BitRead(eeHme+cfTpl)); //Topologie
  fxAtr:=Tools.BitRead(sAtr); //Attribute[band,zone]
  for A:=0 to high(fxAtr) do
    FeatureDrain(fxAtr[A],iGen,ixTpl); //Attribut ausgleichen
  Tools.BitWrite(fxAtr,sAtr); //überschreiben
end;

{ bAs ersetzt die Tabelle "index.bit" durch neue Attribute aus den Bilddaten in
  "sImg". Zonen und Topologie müssen existieren }
{ bAs bildet für alle Zonen den Mittelwert aller Pixel und schreibt das
  Ergebnis nach Kanälen getrennt in die Attribut-Tabelle. bAs überträgt die
  Kanal-Namen aus "sImg" als Feldnamen in den Zonen-Header. }

procedure tBuild.xImageFeatures(sImg:string); //Vorbild für Attribute
const
  cHdr = 'bIF: Selected image and zones differ in size: ';
var
  fxAtr:tn2Sgl=nil; //neue Attribute (Tabelle)
  fxImg:tn3Sgl=nil; //Vorbild, NoData-Pixel müssen zum Index passen!
  iaSze:tnInt=nil; //Pixel pro Zone
  ixIdx:tn2Int=nil; //Zonen-Index
  pBnd:^tn2Sgl=nil; //Zeiger auf aktuellen Kanal
  rHdr,rIdx:trHdr; //Metadaten: Bilder, Zonen
  B,X,Y,Z:integer;
begin
  Header.Read(rHdr,sImg); //Metadaten
  Header.Read(rIdx,eeHme+cfIdx); //Zonen Metadaten
  if not _SizeFit(rIdx,rHdr) then Tools.ErrorOut(3,cHdr+sImg);
  if FileExists(eeHme+cfAtr)
    then rIdx.Fld+=','+Tools.LinesToCommas(rHdr.aBnd) //Attribut-Namen erweitern
    else rIdx.Fld:=Tools.LinesToCommas(rHdr.aBnd); //alte Attribut-Namen ersetzen
  Header.Write(rIdx,'Imalys Cell Index',eeHme+cfIdx); //Header mit Feldnamen
  fxImg:=Image.Read(rHdr,sImg); //Stack für Attribute aus Bilddaten
  ixIdx:=tn2Int(Image.ReadBand(0,rIdx,eeHme+cfIdx)); //Zonen Raster
  fxAtr:=Tools.Init2Single(rHdr.Stk,succ(rIdx.Cnt),0); //leere Tabelle

  for B:=0 to high(fxImg) do
  begin
    iaSze:=Tools.InitInteger(succ(rIdx.Cnt),0);
    pBnd:=@fxImg[B]; //Zeiger auf aktuellen Kanal
    for Y:=0 to high(ixIdx) do
      for X:=0 to high(ixIdx[0]) do
        if not isNan(pBnd^[Y,X]) then
        begin
            fxAtr[B,ixIdx[Y,X]]+=pBnd^[Y,X]; //Merkmale summieren
            inc(iaSze[ixIdx[Y,X]]) //Pixel dazu zählen
          end;
    for Z:=1 to rIdx.Cnt do
      if iaSze[Z]>0 then
        fxAtr[B,Z]/=iaSze[Z];
    fxAtr[B,0]:=0; //Null ist nicht definiert
  end;

  for B:=0 to high(fxAtr) do
    Tools.BitInsert(fxAtr[B],$FFF,eeHme+cfAtr); //Attribute erzeugen oder erweitern
  Tools.HintOut(true,'ImageFeatures: '+cfAtr);
end;

{ bEF normalisiert alle Attribute und speichert das Ergebnis als "equal.bit".
  Die Normalisierung verwendet den "fPrc" und den "1-fPrc" Percentil für die
  neuen Werte Null und Eins. alle anderen Werte werden linear interpoliert.
  bEF toleriert Nodata-Werte }

function tBuild.xEqualFeatures(fPrc:single):string; //Schwellen:Tabellen-Name
const
  cPrc = 'bEF: "percentil" must larger than 0 and smaller than 0.5: ';
var
  faSze:tnSgl=nil; //Größe der Zonen [ha]
  fxAtr:tn2Sgl=nil; //aktuelle Zonen-Attribute
  fFct:single=1; //Steigung für Normalisierung
  fLow,fHig:single; //Percentile
  rIdx:trHdr; //Metadaten Zonen-Index
  F,Z:integer;
begin
  Result:=eeHme+cfEql; //neue Attribut-Tabelle
  if (fPrc<=0) or (fPrc>=0.5) then Tools.ErrorOut(3,cPrc+FloatToStr(fPrc));
  Header.Read(rIdx,eeHme+cfIdx); //Zonen Metadaten
  faSze:=ZonesSize(rIdx); //Größe der Zonen
  fxAtr:=Tools.BitRead(eeHme+cfAtr); //ursprüngliche Attribut-Tabelle
  for F:=0 to high(fxAtr) do
  begin
    fLow:=ZonePercentil(fxAtr[F],faSze,fPrc);
    fHig:=ZonePercentil(fxAtr[F],faSze,1-fPrc);
    if fHig>fLow then
      fFct:=1.0/(fHig-fLow) //Steigung
    else fFct:=0;
    for Z:=1 to high(fxAtr[0]) do
      if not IsNan(fxAtr[F,Z]) then //NoData ignorieren
        fxAtr[F,Z]:=(fxAtr[F,Z]-fLow)*fFct; //scalieren
  end;
  Tools.BitWrite(fxAtr,Result);
end;

{ bSF prüft ob Zonen-Raster und Vorbilder genau gleich groß sind. }

function tBuild._SizeFit(rIdx,rStk:trHdr):boolean; //Metadaten Zonen, Bilder: Passen ja/nein
begin
  Result:=(round(rIdx.Lat/rIdx.Pix)=round(rStk.Lat/rStk.Pix)) //linke obere Ecke
      and (round(rIdx.Lon/rIdx.Pix)=round(rStk.Lon/rStk.Pix)) //rechte untere Ecke
      and (rIdx.Lin=rStk.Lin) and (rIdx.Scn=rStk.Scn) //Höhe, Breite
      and ((rIdx.Pix-rStk.Pix)/(rIdx.Pix+rStk.Pix)<1e-5); //Pixelgröße
end;

{ bPt bestimmt den "fLmt" Percentil für das Attribut "faAtr". bPt bestimmt alle
  Summen als Wert*Fläche der Zonen.}
{ bPt bestimmt zunächst Extremwerte, Mittelwert und Summe aller Attribute. Im
  zweiten Schritt summiert bPt alle Attribute bis zum vorläufigen Percentil,
  korrigiert die Schwelle fMin oder fMax je nach Etrgebnis auf den aktuellen
  Mittelwert und bildet einen neue Mittelwert zwischen den neuen Schwellen.
  bPt iteriert die quasi-binäre Suche bis das Ergebnis stabil ist.

  ACHTUNG: bPt ignoriert "faAtr[0]", Zonen-IDs beginnen bei Eins. }

function tBuild.ZonePercentil(
  faAtr:tnSgl; //aktuelles Attribut
  faSze:tnSgl; //Fläche der Zonen [a]
  fLmt:single): //Schwelle = Percentil
  single; //Schwelle im Attribut
const
  cSkp = 0.0001; //zulässige Abweichung
  cLmt = 'bPt: Percentile input [0<input<1] not defined: ';
var
  fMin:single=MaxSingle; //untere Schwelle
  fMax:single=1-MaxSingle; //obere Schwelle
  fSum:double=0; //Summe aller Werte
  fTrg:double=0; //Zielgröße Summe der Zonen-Fläche
  Z:integer;
begin
  if (fLmt<=0) or (fLmt>=1) then Tools.ErrorOut(3,cLmt+FloatToStr(fLmt));
  //0<=fLmt<=1 !!
  Result:=0;
  for Z:=1 to high(faAtr) do //Zone Null ignorieren
    if not isNan(faAtr[Z]) then
    begin
      if faAtr[Z]<fMin then fMin:=faAtr[Z];
      if faAtr[Z]>fMax then fMax:=faAtr[Z];
      fTrg+=faSze[Z]; //Summe Fläche aller Zonen
    end;
  Result:=(fMin+fMax)/2; //Vorgabe = Mitte
  fTrg:=fTrg*fLmt; //Ziel = Teilsumme aller Flächen

  repeat
    fSum:=0; //neu zählen
    for Z:=1 to high(faAtr) do
      if not isNan(faAtr[Z]) then
        if faAtr[Z]<=Result then //Flächen unter gesuchter Schwelle ..
          fSum+=faSze[Z]; //.. summieren
    if fSum>fTrg
      then fMax:=Result //oben einschränken
      else fMin:=Result; //unten einschränken
    Result:=(fMin+fMax)/2; //weiter in der Mitte
  until (fMax-fMin)/(fMax+fMin)<cSkp
end;

{ bTe bestimmt die normalisierte Textur der Bilddaten "fxImg" mit Zonen als
  Kernel und gibt das Ergebnis als Array (Zonen-Attribut) zurück }
{ bTe setzt alle negativen Werte auf Null, weil die normalisierte Differenz nur
  für positive Werte definiert ist. bTe scannt das gesamte Bild horizontal und
  vertikal, bestimmt die Textur für jedes Pixel-Paar innerhalb derselben Zone
  und zählt die Paare. bTe bestimmt die Textur für jeden Kanal getrennt und
  bildt am Ende die Hauptkomponente aller Kanäle als endgültiges Ergebnis }

function tBuild.Texture_(// vgl. Filter.Texture
  fxImg:tn3Sgl; //Vorbild
  iCnt:integer; //Anzahl Zonen
  iTyp:integer; //0=absolute Differenz, 1=normalisierte Differenz
  ixIdx:tn2Int): //Zonen-IDs
  tnSgl; //normalisierte Textur pro Zone

function lDiff(
  const fHig,fLow:single;
  const iIdx,iNxt:integer;
  var iCmp:integer):single;
// (∑x²-(∑x)²/n)/(n-1)
begin
  Result:=0; //Vorgabe
  if isNan(fHig) or isNan(fLow) then exit; //nur definierte Pixel
  if (iIdx<1) or (iNxt<>iIdx) then exit; //nur definierte Zonen, nur innerhalb der Zone
  if (fHig=0) and (fLow=0) then exit; //keine Nulldivision
  if iTyp>0
    then Result:=abs(fHig-fLow)/(abs(fLow)+abs(fHig)) //normalisierte Differenz
    else Result:=abs(fHig-fLow); //absolute Differenz
  inc(iCmp); //Anzahl Vergleiche
end;

var
  faRes:tnSgl=nil; //Ergebnis für einen Kanal
  iaCmp:tnInt=nil; //Anzahl Vergleiche Nachbarpixel
  pBnd:^tn2Sgl=nil; //Zeiger auf aktuellen Kanal
  B,X,Y,Z:integer;
begin
  Result:=Tools.InitSingle(succ(iCnt),0);
  iaCmp:=Tools.InitInteger(succ(iCnt),0); //Anzahl Vergleiche
  SetLength(faRes,succ(iCnt)); //Zwischenlager
  for B:=0 to high(fxImg) do
  begin
    pBnd:=@fxImg[B]; //Zeiger
    FillDWord(faRes[0],succ(iCnt),0); //für jeden Kanal leeren
    for Y:=0 to high(ixIdx) do
      for X:=1 to high(ixIdx[0]) do
        faRes[ixIdx[Y,X]]+=lDiff(pBnd^[Y,pred(X)],pBnd^[Y,X],
          ixIdx[Y,pred(X)],ixIdx[Y,X],iaCmp[ixIdx[Y,X]]);
    for X:=0 to high(ixIdx[0]) do
      for Y:=1 to high(ixIdx) do
        faRes[ixIdx[Y,X]]+=lDiff(pBnd^[pred(Y),X],pBnd^[Y,X],
          ixIdx[pred(Y),X],ixIdx[Y,X],iaCmp[ixIdx[Y,X]]);
    for Z:=1 to iCnt do
      if iaCmp[Z]>1 then
        Result[Z]+=sqr(faRes[Z]/iaCmp[Z]); //für Hauptkomponente
  end;
  for Z:=1 to iCnt do
    Result[Z]:=sqrt(Result[Z]); //erste Hauptkomponente aller Kanäle
  Result[0]:=0;
end;

{ bKs bestimmt Kernel-Indices mit Zonen als Kernel und speichert die Ergebnisse
  in der Attribut-Tabelle "index.bit". fZK ergänzt die Feldnamen im Zonen-
  Header um die Namen der Prozesse. }
{ Der "runoff"-Prozess muss unmittelbar nach den Höhendaten aufgerufen werden
  und "drainlines" nach "runoff". "Drainlines" erzeugt (derzeit) kein Attribut
  sondern nur eine Vektor-Datei mit den Verknüpfungen der Zonen. }

procedure tBuild.xKernelFeatures(
  slCmd:tStringList;//Prozess-Namen
  sImg:string); //Dateiname Vorbild
const
  cCmd = 'bKF: Command not defined in this context: ';
  cFex = 'bKF: Image not found: ';
  cHdr = 'bKF: Selected image and zones differ in size: ';
  cRds = 1; //Kernel-Radius = Iterationen
var
  faRes:tnSgl=nil; //Ergebnis pro Zone
  fxImg:tn3Sgl=nil; //Vorbild, alle Kanäle
  ixIdx:tn2Int=nil; //Zonen-IDs
  rHdr,rIdx:trHdr; //Metadaten
  C:integer;
begin
  if slCmd=nil then exit; //keine Befehle
  if not FileExists(sImg) then Tools.ErrorOut(3,cFex+sImg);
  Header.Read(rIdx,eeHme+cfIdx); //Metadaten Zonenindex
  Header.Read(rHdr,sImg); //Metadaten Vorbild
  if not _SizeFit(rIdx,rHdr) then Tools.ErrorOut(3,cHdr+sImg);
  if FileExists(eeHme+cfAtr)
    then rIdx.Fld+=','+slCmd.CommaText //Attribut-Namen erweitern
    else rIdx.Fld:=slCmd.CommaText; //Attribut-Namen ersetzen
  Header.Write(rIdx,'Imalys Cell Index',eeHme+cfIdx); //Header mit Feldnamen

  fxImg:=Image.Read(rHdr,sImg); //Stack für Attribute aus Bilddaten
  ixIdx:=tn2Int(Image.ReadBand(0,rIdx,eeHme+cfIdx)); //Zonen-Bild
  for C:=0 to pred(slCmd.Count) do //alle Befehle
  begin
    if slCmd[C]=cfEtp then faRes:=Deviation(fxImg,rIdx.Cnt,ixIdx) else //Abweichung
    if slCmd[C]=cfNrm then faRes:=_Texture(fxImg,rIdx.Cnt,1,ixIdx) else //normalisierte Textur
    if slCmd[C]=cfTxr then faRes:=_Texture(fxImg,rIdx.Cnt,0,ixIdx) else //absolute Textur
      Tools.ErrorOut(3,cCmd+slCmd[C]); //nicht definierter Befehl
    if faRes<>nil then Tools.BitInsert(faRes,$FFF,eeHme+cfAtr); //Attribute erzeugen oder erweitern
  end;
  Tools.HintOut(true,'KernelFeatures: '+slCmd.CommaText);
end;

{ bTe bestimmt die normalisierte Textur der Bilddaten "fxImg" mit Zonen als
  Kernel und gibt das Ergebnis als Array (Zonen-Attribut) zurück }
{ bTe setzt alle negativen Werte auf Null, weil die normalisierte Differenz nur
  für positive Werte definiert ist. bTe scannt das gesamte Bild horizontal und
  vertikal, bestimmt die Textur für jedes Pixel-Paar innerhalb derselben Zone
  und zählt die Paare. bTe bestimmt die Textur für jeden Kanal getrennt und
  bildt am Ende die Hauptkomponente aller Kanäle als endgültiges Ergebnis }

function tBuild._Texture(// vgl. Filter.Texture
  fxImg:tn3Sgl; //Vorbild
  iCnt:integer; //Anzahl Zonen
  iTyp:integer; //0=absolute Differenz, 1=normalisierte Differenz
  ixIdx:tn2Int): //Zonen-IDs
  tnSgl; //normalisierte Textur pro Zone
var
  iaCmp:tnInt=nil; //Vergleiche pro Zone

function lTexture(const iHrz,iVrt,iLon,iLat:integer):single;
var
  B:integer;
begin
  Result:=0;
  if ixIdx[iVrt,iHrz]<>ixIdx[iLat,iLon] then exit;
  if isNan(fxImg[0,iVrt,iHrz]) or isNan(fxImg[0,iLat,iLon]) then exit;
  for B:=0 to high(fxImg) do
    if iTyp>0 then
      Result+=sqr(fxImg[B,iVrt,iHrz]-fxImg[B,iLat,iLon])/
        (abs(fxImg[B,iVrt,iHrz])+abs(fxImg[B,iLat,iLon])) //Textrur / Helligkeit
    else Result+=sqr(fxImg[B,iVrt,iHrz]-fxImg[B,iLat,iLon]); //absolute Differenz
  Result:=sqrt(Result); //Hauptkomponente
  inc(iaCmp[ixIdx[iLat,iLon]]); //Vergleiche pro Zone
end;

var
  X,Y,Z:integer;
begin
  Result:=Tools.InitSingle(succ(iCnt),0);
  iaCmp:=Tools.InitInteger(succ(iCnt),0); //Anzahl Vergleiche
  for Y:=0 to high(ixIdx) do
    for X:=0 to high(ixIdx[0]) do
      if ixIdx[Y,X]>0 then
      begin
        if X>0 then Result[ixIdx[Y,X]]+=lTexture(pred(X),Y,X,Y);
        if Y>0 then Result[ixIdx[Y,X]]+=lTexture(X,pred(Y),X,Y);
      end;
  for Z:=1 to iCnt do
    if iaCmp[Z]>0 then
      Result[Z]/=iaCmp[Z];
  Result[0]:=0;
end;

initialization

  Union:=tUnion.Create;
  Union.ixcIdx:=nil;
  Union.ixcMap:=nil;
  Union.iacPix:=nil;

finalization

  SetLength(Union.ixcIdx,0);
  SetLength(Union.ixcMap,0);
  SetLength(Union.iacPix,0);
  Union.Free;

end.

{==============================================================================}

