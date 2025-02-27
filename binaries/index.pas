unit index;

{ INDEX sammelt Routinen zur Zellbildung. "Zellen" sind zusammenhängende Teile
  des Bilds, deren Pixel mehr Merkmale gemeinsam haben als Pxel außerhalb der
  Zelle. "Zellen" werden durch eine Iteration gebildet. In jedem Schritt werden
  spektral maximal ähnliche Pixel oder Teilflächen zusammenfasst.

  BUILD:  sammelt vom Zellindex abhängige Routinen
  DRAIN:  bestimmt Catchments und verknüpft sie entsprechend der Topographie
  UNION:  vereinigt Pixel zu Zonen basierend auf Varianz

  BEGRIFFE:
  Attribut: Wert, der einer Zelle zugeordnet ist, meistens der Mittelwert aller
            Pixel. Aus Form und Größe der Zonen abgeleitete Werte können ebenso
            Attribute sein.
  Basin:    Gebiet mit einem gemeinsamen Abfluss
  Drain:    Abfluss mit Richtung entlang eines Gradienten, auch übertragen auf
            große, miteinander verknüpfte primäre Catchments
  Equal:    Wertebereich angepasst auf Mittelwert ± Standardabweichung * Eingabe
  Flow:     Abfluss mit Mengenangabe?
  Link:     Verknüpfung primärer Catchmenrs ohne Richtung
  Index:    Bereich eines Bildes mit gleicher ID in "index" → Zelle
  Kontakt:  Grenze zwischen zwei Pixeln vor allem im Zusammenhang mit Grenzen
            zwischen Zellen
  Zone:     Pixel mit gleichem Wert im Zellindex "index". Pixel einer Zone
            sind immer mit mindestens einer Kante verknüpft. Die Zell-ID ist
            eindeutig.
  }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, format;

type
  tBuild = class(tObject) //Aufruf geprüft 22-11-17
    private
      function Attributes(rHdr,rIdx:trHdr; sImg:string):tn2Sgl;
      function BandNames(var rHdr:trHdr):string;
      function CellWeight(var rHdr:trHdr):tnSgl;
      function Dendrites:tnSgl;
      function Deviation(fxImg:tn3Sgl; iCnt:integer; ixIdx:tn2Int):tnSgl;
      function Diversity(sImg:string):tnSgl;
      function InterFlow:tnSgl;
      function NormalZ(fxImg:tn3Sgl; iCnt:integer; ixIdx:tn2Int):tnSgl;
      function Proportion:tnSgl;
      function Relations:tnSgl;
    public
      procedure IndexTopology(iCnt:integer; ixIdx:tn2Int);
      function SizeFit(sIdx,sStk:string):boolean;
      function ThemaImage(iaThm:tnInt):tn2Byt;
      procedure xDiffusion(iGen:integer; sAtr:string);
      function xEqualFeatures(fDvt:single):string;
      procedure xFeatures(sImg:string; slCmd:tStringList);
      procedure xKernels(slCmd:tStringList; sImg:string);
      procedure xAttributes(sImg:string);
      procedure xZoneValues(sFtr:string);
  end;

  tDrain = class(tObject) //Aufruf geprüft 22-11-17
    const
      fcNan: single=0-MaxInt; //Wert für NoData
    private
      function AddLakes(fxDem:tn2Sgl; var iCnt:integer):tn2Int;
      procedure LocalMinima(fxElv:tn2Sgl; var iCnt:integer; ixIdx:tn2Int);
      function LinkZones(fxDem:tn2Sgl; iCnt:integer; ixCtm:Tn2Int):tn2Int;
      procedure LocalBasins(ixIdx:tn2Int);
      function MergeLinks(iaLnk:tnInt):integer;
      function MergeIndex(iaDrn:tnInt; ixCtm:tn2Int):tn2Int;
      procedure RunOff(iaPix,iaNxt,iaLnk:tnInt; ixIdx:tn2Int);
    public
      function InitElevation(fNod:single; var rHdr:trHdr; sDem:string):tn2Sgl;
      function xBasins(fNod:single; iLmt:integer; sDem:string):integer;
  end;

  tUnion = class(tObject)
    private
      iacChn:tnInt; //Pixel-Indices auf Suchpfad NUR FÜR "xBorders"
      ixcIdx:tn2Int; //Zonen-IDs NUR FÜR "xBorders"
      ixcMap:tn2Byt; //Klassen-Layer NUR FÜR "xBorders"
      function Borders(var iRes:integer; ixMap:tn2Byt):tn2Int;
      function NewIndex(fxImg:tn3Sgl; var iCnt:integer):tn2Int;
      function IndexMerge(apEtp:tapEtp; iaLnk:tnInt; ixIdx:tn2Int):tapEtp;
      function LinksMerge(apEtp:tapEtp; ixIdx:tn2Int):tnInt;
      function NewEntropy(fxImg:tn3Sgl; iCnt:integer; ixIdx:tn2Int):tapEtp;
      procedure MinEntropy(apEtp:tapEtp; iGrw:integer; ixIdx:tn2Int);
      procedure PatchGrow(iLat,iLon,iVrt,iHrz:integer);
    public
      procedure xBorders(sImg:string);
      procedure xZones(iGrw,iSze:integer; sImg:string);
  end;

var
  Build: tBuild;
  Drain: tDrain;
  Union: tUnion;

implementation

uses
  Mutual, Raster, Thema, Vector;

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

function tBuild.Dendrites:tnSgl; //Attribut "Kompaktheit"
{ bDd erzeugt ein Attribut mit der Kompaktheit der Zellen. Dazu bildet bDd das
  normalisierte Verhältnis zwischen äußeren und inneren Kontakten aller Pixel
  einer Zelle und gibt es als Array zurück. }
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
    if iExt>0
      then Result[Z]:=iExt/(iInt+iExt) //Verhältnis innere/äußere Kontakte
      else Result[Z]:=0;
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

{ bBN überträgt die Kanal-Namen in getrennten Zeilen aus "rHdr.aBnd" in eine
  kommagetrennte Liste oder erzeugt eine neue }

function tBuild.BandNames(var rHdr:trHdr):string;
var
  iCnt:integer=0; //Anzahl Zeilentrenner
  I:integer;
begin
  Result:=copy(rHdr.aBnd,1,length(rHdr.aBnd)); //Kopie
  for I:=1 to length(Result) do
    if Result[I]=#10 then
    begin
      Result[I]:=',';
      inc(iCnt)
    end;
  if Result[length(Result)]=','
    then delete(Result,length(Result),1)
    else inc(iCnt); //erster Eintrag
  while rHdr.Stk>iCnt do
    Result+=',b'+IntToStr(iCnt); //folgende
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

procedure tUnion.xZones(
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
  write(#13); //gleiche Zeile
  iCnt:=length(apEtp);
  lClearEntropy; //Speicher frei geben
  Tools.HintOut(true,'Force.Zones: '+IntToStr(iCnt));
  Image.WriteBand(tn2Sgl(ixIdx),0,eeHme+cfIdx); //Bilddaten schreiben
  Header.WriteIndex(iCnt,rHdr,eeHme+cfIdx); //Index-Header dazu
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
        if ixIdx[Y,X]>0 then //NoData-Pixel ignorieren
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
      Result[Z]:=sqrt(Result[Z]); //erste Hauptkomponente
  Result[0]:=0;
end;

{ bSF prüft ob Zonen-Raster und Vorbilder genau gleich groß sind. }

function tBuild.SizeFit(sIdx,sStk:string):boolean; //Bildnamen
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
  if not Result then Tools.ErrorOut(2,cSze);
end;

{ uAs erzeugt eine Zonen-Attribut-Tabelle für alle Kanäle von "fxImg" und gibt
  sie als Matrix zurück. Die Attribute sind der Mittelwert aller Pixel der
  einzelnen Zone. }

function tBuild.Attributes(
  rHdr,rIdx:trHdr; //Metadaten: Vorbild, Zonen
  sImg:string): //Name Vorbild
  tn2Sgl; //spektrale Merkmale aller Zonen
var
  fxImg:tn3Sgl=nil; //Vorbild, NoData-Pixel müssen zum Index passen!
  iaSze:tnInt=nil; //Pixel pro Zelle
  iIdx:integer; //aktuelle Zell-ID
  ixIdx:tn2Int; //Zellindex
  pMsk:^tn2Sgl=nil; //Zeiger auf ersten Kanal
  B,X,Y,Z:integer;
begin
  Result:=Tools.Init2Single(rHdr.Stk,succ(rIdx.Cnt),0); //leere Tabelle
  fxImg:=Image.Read(rHdr,sImg); //Stack für Attribute aus Bilddaten
  ixIdx:=tn2Int(Image.ReadBand(0,rIdx,eeHme+cfIdx)); //Zonen Raster
  iaSze:=Tools.InitInteger(succ(rIdx.Cnt),0);
  pMsk:=@fxImg[0]; //Zeiger auf ersten Kanal
  for Y:=0 to high(ixIdx) do
    for X:=0 to high(ixIdx[0]) do
    begin
      if isNan(pMsk^[Y,X]) then continue; //nicht definiert
      iIdx:=ixIdx[Y,X]; //aktuelle Zelle
      for B:=0 to high(fxImg) do
        Result[B,iIdx]+=fxImg[B,Y,X]; //Merkmale summieren
      inc(iaSze[iIdx]) //Pixel dazu zählen
    end;
  for Z:=1 to rIdx.Cnt do
    if iaSze[Z]>0 then
      for B:=0 to high(fxImg) do
        Result[B,Z]/=iaSze[Z];
  for B:=0 to high(fxImg) do
    Result[B,0]:=0; //Null ist nicht definiert
end;

{ bAs ersetzt die Tabelle "index.bit" durch die Bilddaten in "sImg". Zonen und
  Topologie müssen existieren. bAs bestimmt den Mittelwert der Pixel für jede
  Zone und schreibt das Ergebnis nach Kanälen getrennt in die Tabelle. bAs
  überträgt die Kanal-Namen aus "sImg" als Feldnamen in den Zonen-Header. }

procedure tBuild.xAttributes(sImg:string); //Vorbild für Attribute
var
  fxAtr:tn2Sgl=nil; //scalare Attribute
  rStk,rIdx:trHdr; //Vorbild (Layer-Stack)
  sFld:string=''; //Kanal-Namen als kommagetrennte Liste
  I:integer;
begin
  // fileexists(sImg)?
  Header.Read(rStk,sImg); //Metadaten Import
  sFld:=BandNames(rStk); //Kanal-Namen aus Bilddaten als kommagetrennte Liste
  Header.Read(rIdx,eeHme+cfIdx); //Zonen Metadatem
  fxAtr:=Attributes(rStk,rIdx,sImg); //Attribute aus Bilddaten
  if FileExists(eeHme+cfAtr) //Attribute vorhanden (Sicherheit)
    then rIdx.Fld:=rIdx.Fld+','+sFld //Feldnamen aus Bilddaten ergänzen
    else rIdx.Fld:=sFld; //Feldnamen ersetzen
  for I:=0 to high(fxAtr) do
    Tools.BitInsert(fxAtr[I],$FFF,eeHme+cfAtr); //Attribute erzeugen oder erweitern
  Header.Write(rIdx,'Imalys Cell Index',eeHme+cfIdx); //Header mit Feldnamen
  Tools.HintOut(true,'Build.Attributes: '+cfAtr);
end;

{ bNZ bestimmt die normalisierte Textur mit Zonen als Kernel und gibt das
  Ergebnis als Array (Zonen-Attribut) zurück. bNZ scannt das gesamte Bild
  horizontal und vertikal und registriert dabei jedes Pixel-Paar das in der
  gleichen Zone liegt. bNZ bestimmt die mittlere Differenz dieser Paare für
  jeden Kanal getrennt. Das Ergebnis ist dann die Hauptkomponente aller Werte
  der einzelnen Kanäle. (Mit bNrm=true) normalisiert bNZ die Differenz mit der
  Helligkeit beider Pixel. }
{ lD bestimmt die (normalisierte) Differenz zwischen zwei Pixeln. lD prüft ob
  beide Pixel zur gleichen Zelle gehören (iIdx,iNxt), bestimmt das Ergebnis
  und zählt die berechneten Differenzen (iCnt). }

function tBuild.NormalZ(
  fxImg:tn3Sgl; //Vorbild
  iCnt:integer; //Anzahl Zonen
  ixIdx:tn2Int): //Zonen-IDs
  tnSgl; //normalisierte Textur pro Zone

function lDiff(
  const fHig,fLow:single;
  const iIdx,iNxt:integer;
  var iCmp:integer):single;
begin
  Result:=0; //Vorgabe
  if (iIdx<1) or (iNxt<>iIdx) then exit;
  if fHig+fLow=0 then exit;
  Result:=abs(fHig-fLow)/(fHig+fLow); //normalisierte Differenz
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
    Result[Z]:=sqrt(Result[Z]); //erste Hauptkomponente
  Result[0]:=0;
end;

{ bCW bestimmt die Größe der Zonen direkt aus dem Zellindex und gibt sie als
  Attribut zurück. Das Attribut enthält die Fläche in [ha]. }

function tBuild.CellWeight(var rHdr:trHdr):tnSgl;
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
{ TODO: Zonen am Ende der Liste sind definiert aber leer }
  Result[0]:=0;
end;

{ bPt bestimmt die "Textur" der Zonengröße als Attribut. Das Ergebnis ist der
  Mittelwert aller Flächen-Differenzen zu allen Nachbarzellen. Das Ergebnis
  kann negativ sein! bPt verwendet die Summe der inneren Kontakte als Fläche. }

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
        faSze[Z]:=ln(succ(paPrm^[N])); //Logarithmus innere Kontakte als Zellgröße

  for Z:=1 to high(paDim^) do //alle Zellen
  begin
    iNbr:=0; //Vorgabe
    for N:=paDim^[pred(Z)] to pred(paDim^[Z]) do //alle Kontakte, auch innere
      if paNbr^[N]<>Z then
      begin
        Result[Z]+=faSze[paNbr^[N]]; //Flächen (logarithmen) summieren
        inc(iNbr) //zählen
      end;
    if Result[Z]>0
      then Result[Z]:=faSze[Z]/Result[Z]*iNbr //Verhältnis zum Mittelwert
      else Result[Z]:=0;
  end;
end;

{ bDy leitet die spektrale Diversität direkt aus den Zonen-Attributen ab und
  gibt sie als neues Attribut zurück. Dazu benötigt bDy den Index, die Anzahl
  der scalaren Attribute und die Topologie. }
{ bDy bestimmt für jede benachbarte Zone und jeden Kanal die Varianz der
  spektralen Merkmale und gewichtet sie mit der Anzahl der Kontakte inclusive
  innere Kontakte. Für die verschiedenen Kanäle verwendet bDY die erste Haupt-
  Komponente. bDy trägt den Prozess-Namen als Feldname in den Zonen-Header ein }

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
  if length(sImg)<1 then Tools.ErrorOut(2,cImg+sImg);
  ixTpl:=tn2Int(Tools.BitRead(eeHme+cfTpl));
  paDim:=@ixTpl[0]; //Zeiger auf Startadressen
  paNbr:=@ixTpl[1]; //Zeiger auf IDs der Nachbarzellen
  paPrm:=@ixTpl[2]; //Zeiger auf Kontakte zu Nachbarzellen
  fxVal:=Tools.BitRead(eeHme+cfAtr); //Zell-Attribute
  Result:=Tools.InitSingle(length(paDim^),0); //Entropie-Attribut
  iSpc:=StrToInt(Header.ReadLine('bands',sImg));
  for Z:=1 to high(paDim^) do
  begin
    fVrz:=0;
    for B:=0 to pred(iSpc) do //nur Spektralkanäle
    begin
      fSqr:=0; fSum:=0; iCnt:=0; //Vorgabe
      for N:=paDim^[pred(Z)] to pred(paDim^[Z]) do //alle Kontakte (auch innere)
      begin
//------------------------------------------------------------------------------
        //if paNbr^[N]=Z then continue;
//------------------------------------------------------------------------------
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

{ bFs erweitert die Attribut-Tabelle "index.bit" mit Attributen aus der
  Geometrie und den spektralen Attributen ganzer Zonen. Wenn "index.bit" nicht
  existiert, erzeugt bFs eine neue. Zellindex und Topologie müssen existieren.
  Mit "iGen>0" werden die Attribute lokal mit einer Diffusion gemittelt. bFs
  trägt die Prozess-Namen aus "slCmd" als Feldnamen in den Index-Header ein. }

procedure tBuild.xFeatures(
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
  Header.Read(rHdr,eeHme+cfIdx); //Zellindex
  if FileExists(eeHme+cfAtr) //Attribute vorhanden
    then rHdr.Fld:=rHdr.Fld+','+slCmd.CommaText //Feldnamen aus Bilddaten ergänzen
    else rHdr.Fld:=slCmd.CommaText; //Feldnamen ersetzen
  for I:=0 to pred(slCmd.Count) do
  begin
    if slCmd[I]=cfDdr then faVal:=Dendrites else
    if slCmd[I]=cfDvs then faVal:=Diversity(sImg) else
    if slCmd[I]=cfItf then faVal:=Interflow else
    if slCmd[I]=cfPrp then faVal:=Proportion else
    if slCmd[I]=cfRlt then faVal:=Relations else
    if slCmd[I]=cfSze then faVal:=CellWeight(rHdr) else
      Tools.ErrorOut(2,cCmd+slCmd[I]); //nicht definierter Befehl
    Tools.BitInsert(faVal,$FFF,eeHme+cfAtr); //bestehende Attribute erzeugen oder erweitern
  end;
  Header.Write(rHdr,'Imalys zonal index',eeHme+cfIdx); //speichern
  Tools.HintOut(true,'Build.Features: '+cfAtr);
end;

{ IT erzeugt eine heterogene Tabelle "topology.bit". Die Tabelle enthält in
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
  if not FileExists(eeHme+cfIdx) then Tools.ErrorOut(2,cIdx+eeHme+cfIdx);
  ixNbr:=Tools.Init2Integer(succ(iCnt),1,0); //Container
  ixPrm:=Tools.Init2Integer(succ(iCnt),1,0);
  lLinksIndex(ixIdx); //Pixel-Kontakte indizieren (Y*2)
  lInternal; //interne Grenzen einfach zählen
  lIndexSave; //Topologie indizieren und speichern (Z)
  Tools.HintOut(true,'Union.IndexTopology: '+cfTpl)
end;

{ fZK bestimmt Kernel-Attribute mit Zonen als Kernel und speichert das Ergebnis
  in der Attribut-Tabelle "index.bit". fZK ergänzt die Prozess-Namen im Index-
  Header als Feldnamen. }

procedure tBuild.xKernels(
  slCmd:tStringList;//Prozess-Namen
  sImg:string); //Dateiname Vorbild
const
  cCmd = 'bZK: Command not defined in this context: ';
  cFex = 'bZK: Image not found: ';
var
  faDvs:tnSgl=nil; //Werte (Diversity) pro Zone
  fxImg:tn3Sgl=nil; //Vorbild, alle Kanäle
  ixIdx:tn2Int=nil; //Zonen-IDs
  rHdr,rIdx:trHdr; //Metadaten
  C:integer;
begin
  if slCmd=nil then exit; //keine Befehle
  if not FileExists(sImg) then Tools.ErrorOut(2,cFex+sImg);
  // slCmd muss gefiltert sein ← "Parse.KernelCmd"
  // iBnd>0?

  Header.Read(rIdx,eeHme+cfIdx); //Metadaten Zonenindex
  ixIdx:=tn2Int(Image.ReadBand(0,rIdx,eeHme+cfIdx)); //Zonen-Bild
  if FileExists(eeHme+cfAtr) //Attribute vorhanden
    then rIdx.Fld:=rIdx.Fld+','+slCmd.CommaText //Feldnamen aus Bilddaten ergänzen
    else rIdx.Fld:=slCmd.CommaText; //Feldnamen ersetzen
  Header.Write(rIdx,'Imalys Cell Index',eeHme+cfIdx); //Header mit Feldnamen
  Header.Read(rHdr,sImg); //Metadaten Vorbild
  fxImg:=Image.Read(rHdr,sImg); //Stack für Attribute aus Bilddaten
  for C:=0 to pred(slCmd.Count) do //alle Befehle
  begin
    if slCmd[C]=cfEtp then faDvs:=Deviation(fxImg,rIdx.Cnt,ixIdx) else //Abweichung
    if slCmd[C]=cfNrm then faDvs:=NormalZ(fxImg,rIdx.Cnt,ixIdx) else //normalisierte Textur
        Tools.ErrorOut(2,cCmd+slCmd[C]); //nicht definierter Befehl
    Tools.BitInsert(faDvs,$FFF,eeHme+cfAtr); //Attribute erzeugen oder erweitern
  end;
  Tools.HintOut(true,'Build.ZonesKernel: '+slCmd.CommaText);
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
  iacChn:=Tools.InitInteger($100,0); //Pixelindices geprüfte Pixel, leer
  iScn:=length(ixMap[0]); //Bildbreite für Pixelindex
  for Y:=0 to high(ixMap) do
    for X:=0 to high(ixMap[0]) do
    begin
      if ixMap[Y,X]=0 then continue; //Bild nicht definiert
      if Result[Y,X]>0 then continue; //Zonen-ID vergeben
      inc(iRes); //neue Zone
      iacChn[0]:=1; //ein gültiger Eintrag
      iacChn[1]:=Y*iScn+X; //erster Pixelindex
      iChn:=1; //Position der Prüfung
      Result[Y,X]:=iRes; //Wert vergeben
      repeat
        iVrt:=iacChn[iChn] div iScn;
        iHrz:=iacChn[iChn] mod iScn;
        if iVrt>0 then PatchGrow(pred(iVrt),iHrz,iVrt,iHrz);
        if iHrz>0 then PatchGrow(iVrt,pred(iHrz),iVrt,iHrz);
        if iVrt<high(ixMap) then PatchGrow(succ(iVrt),iHrz,iVrt,iHrz);
        if iHrz<high(ixMap[0]) then PatchGrow(iVrt,succ(iHrz),iVrt,iHrz);
        inc(iChn) //nächster Pixel
      until iChn>iacChn[0];
    end;
end;

{ uBs erzeugt Zonen aus einem Klassen-Layer. uBs bildet alle Klassen-Grenzen
  als Zonen-Grenzen ab. uBs akzeptiert als Vorbild nur einzelne Layer im Byte-
  Format. uBs interpretiert Null im Vorbild als nicht definierte Bereiche.
    uBs füllt die klassifizierten Flächen mit einem Flood-Algorithmus. Dazu
  prüft uBs systemetisch die Umgebung von jeden Pixel auf identische Nachbar-
  Pixel und erweitert den Index entsprechend. Zusammenhängende Flächen müssen
  durch mindestens eine Pixel-Kante miteinander verbunden sein. Das Vorbild
  "ixcMap", der Zonen-Index "ixcIdx" und der Suchpfad "iacChn" sind in der
  Klasse definiert um das Interface klein zu halten. }

procedure tUnion.xBorders(sImg:string); //Vorbild (Klassen)
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
  Tools.HintOut(true,'Union.Borders: '+IntToStr(iRes));
  Image.WriteBand(tn2Sgl(ixcIdx),0,eeHme+cfIdx); //Bilddaten schreiben
  Header.WriteIndex(iRes,rHdr,eeHme+cfIdx); //Index-Header dazu
  Build.IndexTopology(iRes,ixcIdx); //Topologie-Tabelle
  Gdal.ZonalBorders(eeHme+cfIdx); //Zellgrenzen als Shape
  SetLength(ixcIdx,0);
  SetLength(ixcMap,0);
  SetLength(iacChn,0);
end;

{ bEF normalisiert alle Werte im Kanal "fxBnd" auf den Bereich 0.5±S*fFct. "S"
  ist die Standardabweichung. Varianz = (∑x²-(∑x)²/n)/(n-1) }
{ "fFct" MUSS POSITIV SEIN }

function tBuild.xEqualFeatures(fDvt:single):string; //Soll-Abweichung
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

{ dIE setzt alle NoData-Werte aus "sDem" auf die Konstante "fcNan". Dasselbe
  gilt für alle Werte bis zur Eingabe "fNod". "DemLinks" und "LinkZones"
  benötigen einen NoData-Wert, der arithmetisch verglichen werden kann. }

function tDrain.InitElevation(
  fNod:single; //Schwelle für NoData (inclusiv)
  var rHdr:trHdr; //gemeinsame Metadaten
  sDem:string): //Dateiname Höhenmodell
  tn2Sgl; //angepasstes Höhenmodell
var
  X,Y:integer;
begin
  Result:=Image.ReadBand(0,rHdr,sDem); //Höhendaten lesen
  for Y:=0 to high(Result) do
    for X:=0 to high(Result[0]) do
      if isNan(Result[Y,X]) or (Result[Y,X]<=fNod) then
        Result[Y,X]:=fcNan; //Wert für Nodata, kann verglichen werden
  Result[0,0]:=fcNan; //NoDataTest
end;

{ dLZ verknüpft Catchments aus "ixCtm" am niedrigsten Punkt der gemeinsamen
  Grenzen und lokalisiert Abflüsse am Bildrand und an der Grenze zu NoData.
  dLZ gibt das Ergebnis in "Result[0]" als Verknüpfung der Catchment-IDs zurück
  und in "Result[1]" und "Result[2]" als Pixelindex der beiden Pixel, die den
  Ort des Überlaufs in beiden Zoen markieren. }
{ Der Überlauf ist immer der niedrigste Punkt am Rand des Catchments. Als
  "Rand" wertet dLZ das Pixel-Paar, das zwei Catchments am Ort des Überlaufs
  verbindet. Am Bildrand und am Rand zu Nodata ist zweimal derselbe Pixel am
  Rand des Catchments markiert. }

function tDrain.LinkZones(
  fxDem:tn2Sgl; //Höhendaten
  iCnt:integer; //Anzahl lokale Minima
  ixCtm:tn2Int): //Catchments = verknüpfte Zonen
  tn2Int; //Verknüpfungen zwischen Catchments
var
  faMin:tnSgl=nil; //niedrigster Punkt an Grenze mit Verknüpfung
  iScn:integer;

{ lD bestimmt die Höhe eines Pixel-Paars [iLat,iLon],[iVrt,iHrz] an der Grenze
  zwischen zwei Catchments oder die Höhe eines Pixels und vergleicht sie mit
  dem bisherigen Minimum "faMin". Bei einem neuen Minimum registriert lD die
  Catchment-ID, die Höhe und die Pixel-Indices der Pixel an der Grenze. }

procedure lDrain(iLat,iLon,iVrt,iHrz:integer);
begin
  if ixCtm[iLat,iLon]>0 then //nur gültige Punkte
    if ixCtm[iVrt,iHrz]>0 then //nur definierte Paare
    begin
      if max(fxDem[iLat,iLon],fxDem[iVrt,iHrz])<faMin[ixCtm[iLat,iLon]] then
      begin
        faMin[ixCtm[iLat,iLon]]:=max(fxDem[iLat,iLon],fxDem[iVrt,iHrz]);
        Result[0,ixCtm[iLat,iLon]]:=ixCtm[iVrt,iHrz]; //neue Verknüpfung
        Result[1,ixCtm[iLat,iLon]]:=iLat*iScn+iLon; //Pixelindex "von"
        Result[2,ixCtm[iLat,iLon]]:=iVrt*iScn+iHrz; //Pixelindex "nach"
      end;
    end
    else //zweiter Pixel nicht dfiniert (NoData)
      if fxDem[iLat,iLon]<faMin[ixCtm[iLat,iLon]] then
        begin
          faMin[ixCtm[iLat,iLon]]:=fxDem[iLat,iLon];
          Result[0,ixCtm[iLat,iLon]]:=ixCtm[iLat,iLon]; //Selbstbezug
          Result[1,ixCtm[iLat,iLon]]:=iLat*iScn+iLon; //Pixelindex "von"
          Result[2,ixCtm[iLat,iLon]]:=iLat*iScn+iLon; //Pixelindex "nach"
        end;
end;

var
  X,Y:integer;
begin
  Result:=Tools.Init2Integer(3,succ(iCnt),dWord(-1)); //Vorgabe = ungültig
  faMin:=Tools.InitSingle(succ(iCnt),dWord(single(MaxInt))); //Vorgabe = sehr groß
  iScn:=length(ixCtm[0]); //Bildbreite

  for Y:=0 to high(ixCtm) do //vertikale Grenzen
  begin
    for X:=1 to high(ixCtm[0]) do //alle Paare innerhalb der Bildfläche
      if ixCtm[Y,pred(X)]<>ixCtm[Y,X] then //Grenze (Zonen, NoData)
      begin
        lDrain(Y,pred(X),Y,X); //beide Richtungen
        lDrain(Y,X,Y,pred(X));
      end;
    lDrain(Y,0,Y,0); //linker Bildrand
    lDrain(Y,high(ixCtm[0]),Y,high(ixCtm[0])); //rechter Bildrand
  end;

  for X:=0 to high(ixCtm[0]) do //horizontale Grenzen
  begin
    for Y:=1 to high(ixCtm) do
      if ixCtm[pred(Y),X]<>ixCtm[Y,X] then
      begin
        lDrain(pred(Y),X,Y,X);
        lDrain(Y,X,pred(Y),X);
      end;
    lDrain(0,X,0,X);
    lDrain(high(ixCtm),X,high(ixCtm),X);
  end;
end;

{ dRO trägt neue Verknüpfungen aus "iaPix,iaNxt" in die Liste "iaLnk" ein.
  "iaLnk" beschreibt den Abfluss zwischen primären Zonen. Die Verknüpfugen
  bilden zunächst Inseln, die Ebenen oder lokalen Minima entsprechen. Diese
  Inseln werden sukzessive verknüpft. }
{ Jede Zone ist mit genau einer anderen verknüpft. Die Verknüpfungen können
  Paare oder Kreise bilden. Eine neue Verknüpfung löscht eine alte. dRO
  verfolgt deshalb die alte Verknüpfung iterativ zurück bis ein Rückbezug
  auftaucht. Jede Insel muss einen Kreis oder Rückbezug enthalten. }

procedure tDrain.RunOff(iaPix,iaNxt,iaLnk:tnInt; ixIdx:tn2Int);
var
  iDrn,iIdx,iSrc:integer; //Ziel, Mitte, Quelle als Zonen-Indices
  iScn:integer; //Bildbreite
  I:integer;
begin
  iScn:=length(ixIdx[0]); //Bildbreite in Pixeln
  for I:=1 to high(iaPix) do
  begin
    iSrc:=iaLnk[ixIdx[iaPix[I] div iScn,iaPix[I] mod iScn]]; //altes Ziel = neue Quelle
    iIdx:=ixIdx[iaPix[I] div iScn,iaPix[I] mod iScn]; //Zone "von"
    iDrn:=ixIdx[iaNxt[I] div iScn,iaNxt[I] mod iScn]; //Zone "nach"
    iaLnk[iIdx]:=iDrn; //neue Verknüpfung
    while iaLnk[iSrc]<>iIdx do
    begin
      iDrn:=iIdx;
      iIdx:=iSrc;
      iSrc:=iaLnk[iIdx];
      iaLnk[iIdx]:=iDrn;
    end;
  end;
end;

{ dML verfolgt gestaffelte Verknüpfungen in "iaLnk" bis zu einem gemeinsamen
  Ziel und gibt für alle Stufen einer Verküpfung dieselbe ID zurück. Die neuen
  IDs sind fortlaufend gezählt. Da Verknüpfungen Kreise oder Rückkopplungen
  bilden, markiert dML zunächst den Weg durch die Verknüpfungen mit (-1) bis
  die Suche auf sich selbst oder auf eine schon gesetzte neue ID stößt und
  vergibt erst dann den endgültigen Wert. }

function tDrain.MergeLinks(iaLnk:tnInt):integer;
var
  iTmp:integer=0; //neue Catchment-ID (Zone)
  iaTmp:tnInt=nil; //übernimmt ursprüngliche Werte
  I,Z:integer;
begin
  Result:=0;
  iaTmp:=copy(iaLnk,0,length(iaLnk)); //Eingabe schützen
  FillDWord(iaLnk[0],length(iaLnk),0); //Vorgabe = NoData
  for I:=1 to high(iaTmp) do
    if iaLnk[I]=0 then //Zone nicht zugeordnet
    begin
      Z:=I;
      while iaLnk[Z]=0 do
      begin
        iaLnk[Z]:=-1; //geprüft-Flag
        Z:=iaTmp[Z] //nächste Zone
      end;

      if iaLnk[Z]<0 then //neue Verknüpfung
      begin
        inc(Result); //neues Catchment
        iTmp:=Result //übernehmen
      end
      else iTmp:=iaLnk[Z]; //bestehendes Catchment

      Z:=I; //von vorne beginnen
      while iaLnk[Z]<0 do //nur markierte Zonen
      begin
        iaLnk[Z]:=iTmp; //Catchment eintragen
        Z:=iaTmp[Z] //nächste Zone
      end;
    end;
end;

{ dMI überschreibt die Catchment-ID in "ixCtm" mit einer neuen ID aus "iaDrn".
  dMI wird verwendet um verknüpfte Catchment-IDs aus "mergeLinks" auf das
  Catchment-Bild "ixCtm" zu übertragen }

function tDrain.MergeIndex(
  iaDrn:tnInt; //Verknüpfungen Catchments
  ixCtm:tn2Int): //Index aus lokalen Minima
  tn2Int; //Catchments
var
  X,Y:integer;
begin
  Result:=Tools.Init2Integer(length(ixCtm),length(ixCtm[0]),0);
  for Y:=0 to high(ixCtm) do
    for X:=0 to high(ixCtm[0]) do
      Result[Y,X]:=iaDrn[ixCtm[Y,X]]; //verknüpfte Zonen
end;

{ uPG prüft ob der Pixel [iLat,iLon] zur gleichen Klasse gehört wie [iVrt,iHrz]
  und markiert akzeptierte Pixel mit der Zonen-ID des Vorbilds. uPG trägt neu
  registrierte Pixel in den Suchpfad "iacChn" ein. "xBorders" übergibt diese
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
    inc(iacChn[0]); //neuer Test
    if length(iacChn)<=iacChn[0] then
      SetLength(iacChn,length(iacChn)*2); //neuer Speicher
    iacChn[iacChn[0]]:=iLat*length(ixcMap[0])+iLon; //Pixelindex
    ixcIdx[iLat,iLon]:=ixcIdx[iVrt,iHrz] //ID übernehmen
  end
end;

{ dAL registriert ebene Flächen in den Höhendaten und gibt sie als negative
  Zonen-IDs zurück. Dazu erzeugt dAL eine Maske "ixMsk" für alle ebenen
  Flächen. dAL füllt die maskierten Flächen mit einer fortlaufenden, negativen
  ID. Das Ergebnis enthält drei mögliche Werte: [0]:NoData; [-1]:keine Ebene;
  [-2..-N]:fortlaufend gezählte Ebenen. }

  // UNION.ADDLAKES MIT ANALOGER FUNKTION ABER ANDEREM NODATA / IMDEX

function tDrain.AddLakes(
  fxDem:tn2Sgl;
  var iCnt:integer):
  tn2Int; //Zonen-Vorläufer: 0=NoData, -1=ungeprüft -2..-N=Zonen aus Ebenen
var
  iHrz,iVrt:integer; //höchster Spalten- und Zeilen-Index
  ixMsk:tn2Byt=nil; //Maske ebene Flächen
  X,Y:integer;
begin
  ixMsk:=Tools.Init2Byte(length(fxDem),length(fxDem[0])); //keine Maske
  iHrz:=high(fxDem[0]);
  iVrt:=high(fxDem);
  for Y:=0 to high(fxDem) do
    for X:=0 to high(fxDem[0]) do
      if fxDem[Y,X]>fcNan then
      begin
        if (X>0) and (fxDem[Y,pred(X)]=fxDem[Y,X]) then ixMsk[Y,X]:=1;
        if (Y>0) and (fxDem[pred(Y),X]=fxDem[Y,X]) then ixMsk[Y,X]:=1;
        if (X<iHrz) and (fxDem[Y,succ(X)]=fxDem[Y,X]) then ixMsk[Y,X]:=1;
        if (Y<iVrt) and (fxDem[succ(Y),X]=fxDem[Y,X]) then ixMsk[Y,X]:=1;
      end;

  Result:=Union.Borders(iCnt,ixMsk); //ebene Flächen indizieren
  for Y:=0 to high(fxDem) do
    for X:=0 to high(fxDem[0]) do
      if fxDem[Y,X]>fcNan then
        if Result[Y,X]>0
          then Result[Y,X]:=-succ(Result[Y,X]) //Zone registriert
          else Result[Y,X]:=-1; //Pixel nicht geprüft
  Result[0,0]:=0; //Pixelindex Null = NoData
end;

{ dLP sucht zu jedem Pixel den niedrigsten Nachbarpixel und trägt das Ergebnis
  als Pixelidex [1..N] ein. dLP ignoriert Pixel mit dem Wert Null (NoData) und
  Pixel mit Werten unter -1 (Ebenen). Der Pixelindex zeigt nie nach NoData. Bei
  der Suche bleiben lokale Minima als [-1] bestehen. Nach dLP hat "ixIdx" vier
  mögliche Werte: [N..1]:Abfluss zum Nachbarpixel; [0]:NoData; [-1]:Fehler;
  [-2..-N]:definierte Zonen }

procedure tDrain.LocalMinima(
  fxElv:tn2Sgl; //Höhendaten
  var iCnt:integer; //Anzahl Zonen (als positive Zahl)
  ixIdx:tn2Int); //Zonen-Vorläufer: Abfluss>0, NoData=0, Zonen<-1

procedure lDrain(
  fMin:single; //aktuelles Minimum
  const iLat,iLon:integer; //Position Referenz
  const iVrt,iHrz:integer); //Position Nachbar
begin
  if fxElv[iVrt,iHrz]>fcNan then //nur gültige Höhe
    if fxElv[iVrt,iHrz]<fMin then //streng nach unten
    begin
      ixIdx[iLat,iLon]:=iVrt*length(fxElv[0])+iHrz; //Pixelindex Abfluss
      fMin:=fxElv[iVrt,iHrz] //neues Minimum
    end;
end;

var
  fMin:single; //aktuelles Minimum Höhe
  X,Y:integer;
begin
  for Y:=0 to high(fxElv) do
    for X:=0 to high(fxElv[0]) do
      if ixIdx[Y,X]=-1 then //nur ungeprüft
      begin
        fMin:=fxElv[Y,X];
        if X>0 then lDrain(fMin,Y,X,Y,pred(X));
        if Y>0 then lDrain(fMin,Y,X,pred(Y),X);
        if X<high(fxElv[0]) then lDrain(fMin,Y,X,Y,succ(X));
        if Y<high(fxElv) then lDrain(fMin,Y,X,succ(Y),X);
      end;

  for Y:=0 to high(ixIdx) do
    for X:=0 to high(ixIdx[0]) do
      if ixIdx[Y,X]=-1 then
      begin
        inc(iCnt); //neue Zone
        ixIdx[Y,X]:=-succ(iCnt) //als negative Zahl <-1 eintragen
      end;
end;

{ dLB vergibt dieselbe ID an alle verknüpften Pixel. dLB verfolgt zunächst die
  Pixelindices [>0] bis zur eingetragenen Zonen-ID [<1], registriert die ID und
  trägt sie auf demselben Weg in alle verknüpften Pixel ein. Nach dLB hat
  "ixIdx" noch zwei Werte: [0]:NoData; [-2..-N]:Zonen (Ebenen und Minima). }

procedure tDrain.LocalBasins(ixIdx:tn2Int); //Zonen-Vorläufer: 0:NoData; <1:Zonen
var
  iPix:integer; //Pixelindex
  iIdx:integer; //aktuelle Zone
  iScn:integer; //Bildbreite in Pixeln
  iTmp:integer; //Zwischenlager
  X,Y:integer;
begin
  iScn:=length(ixIdx[0]); //Bildbreite in Pixeln
  for Y:=0 to high(ixIdx) do
    for X:=0 to high(ixIdx[0]) do
      if ixIdx[Y,X]>0 then //gültiger Abfluss
      begin
        iPix:=Y*iScn+X; //Pixelindex
        while ixIdx[iPix div iScn,iPix mod iScn]>0 do //gültiger Abfluss
          iPix:=ixIdx[iPix div iScn,iPix mod iScn]; //nächster Pixel

        iIdx:=ixIdx[iPix div iScn,iPix mod iScn]; //ID (<-1) übernehmen

        iPix:=Y*iScn+X; //neu beginnen
        while ixIdx[iPix div iScn,iPix mod iScn]>0 do //gültiger Abfluss
        begin
          iTmp:=iPix;
          iPix:=ixIdx[iPix div iScn,iPix mod iScn]; //nächster Pixel
          ixIdx[iTmp div iScn,iTmp mod iScn]:=iIdx //Zonen-ID
        end;
      end;

  for Y:=0 to high(ixIdx) do
    for X:=0 to high(ixIdx[0]) do
      if ixIdx[Y,X]<0 then
        ixIdx[Y,X]:=pred(-ixIdx[Y,X]); //positive Values ab 1
end;

{ dBs bestimmt lokale Minima, Abfluss und Einzugsgebiete aus dem Höhenmodell
  "sDem". dBs speichert die lokalen Minima als Bild "runoff" und den Abfluss
  zwischen den Zonen als "runoff.bit". }
{ (1) dBs bildet zuerst Zonen aus allen ebenen Flächen und nummeriert sie
  fortlaufend. Dann ergänzt dBs die Einzugsgebiete und IDs der lokalen Minima.
  Dazu bestimmt dBs zu jedem Pixel die Position des niedrigsten Nachbar-Pixels.
  Lokale Minima bleiben übrig. Nodata hat die ID Null. Der Zonen-Vorläufer
  "ixIdx" kann folgende Werte haben: <[-1] für Zonen-IDs, [-1] für ungeprüfte
  Pixel, [0] für Nodata, >[0] für Verweie auf den Nachbarpixel als Pixelindex.
  Das Ergebnis ist [0] für Nodata und [N] für Zonen. }
{ (2) dBs verknüpft primäre Zonen zu Cachments und später iterativ Catchments
  zu größeren Catchments bis alle einen Abfluss am Bildrand oder zu Nodata
  besitzen. Der Abfluss ist immer der niedrigste Punkt an dem sich zwei Zonen
  berühren. Abfluss-Ketten und -Ringe sind zulässig. Die Iteration endet, wenn
  die Catchments konstant sind. }
{ (3) dBs regisriert alle Abflüsse in "iaLnk" auf der Ebene der primären Zonen.
  Jede Zone entwässert in genau eine andere. So entstehen Abfluss-Bäume, die am
  Bildrand oder an NoData Flächen enden. Der Abfluss kann mit "Lines.RunOff"
  als Linienvektoren dargestellt werden. }

function tDrain.xBasins(
  fNod:single; //nicht definierte Höhe als definierte Zahl
  iLmt:integer; //Maximum getrennte Gebiete NICHT AKTIV
  sDem:string): //Höhenmodell
  integer; //Anzahl Ebenen (Stillgewässer)
  //sTrg wählen?
var
  fxElv:tn2Sgl=nil; //Höhenmodell als Bild ← faElv (elevation)
  ixDrn:tn2Int=nil; //Catchment-Verknüpfungen
  iaLnk:tnInt=nil; //Zonen-Verknüpfungen (IDs)
  iTmp:integer; //Anzahl Catchments
  ixCtm:tn2Int=nil; //Catchments
  ixIdx:tn2Int=nil; //Zonen-IDs der erweiterten Catchments
  rHdr:trHdr; //gemeinsame Metadaten
begin
  Result:=0;
  Header.Read(rHdr,sDem); //gemeinsame Metadaten, "sDem" existiert
  fxElv:=InitElevation(fNod,rHdr,sDem); //Höhenmodell: NoData = fcNan
  ixIdx:=AddLakes(fxElv,Result); //ebene Flächen erfassen + zählen (rHdr.Cnt)
  rHdr.Cnt:=Result; //Vorgabe = Ebenen
  LocalMinima(fxElv,rHdr.Cnt,ixIdx);  //Lokale Minima finden + zählen
  LocalBasins(ixIdx); //Zonen-ID aus Abfluss
  Image.WriteBand(tn2Sgl(ixIdx),0,eeHme+cfMic); //Zonen speichern
  Header.WriteIndex(rHdr.Cnt,rHdr,eeHme+cfMic);
  ixCtm:=Tools.CopyGrid(ixIdx); //Index auf Pixelbasis schützen
  repeat
    iTmp:=rHdr.Cnt;
    ixDrn:=LinkZones(fxElv,rHdr.Cnt,ixCtm); //neue Verknüpfungen zwischen Catchments
    if iaLnk=nil
      then iaLnk:=copy(ixDrn[0],0,length(ixDrn[0])) //Verknüpfung der Zonen
      else RunOff(ixDrn[1],ixDrn[2],iaLnk,ixIdx); // neue Verknüfungen in "iaLnk" ergänzen
      rHdr.Cnt:=MergeLinks(ixDrn[0]); //gleiche ID für verknüpfte Catchments
    ixCtm:=MergeIndex(ixDrn[0],ixCtm); //verknüpfte Catchments als Bild
  until rHdr.Cnt=iTmp;
  Image.WriteBand(tn2Sgl(ixCtm),0,eeHme+cfCtm); //Catchments als Bild
  Header.WriteIndex(rHdr.Cnt,rHdr,eeHme+cfCtm);
  Tools.BitInsert(tnSgl(iaLnk),-1,eeHme+cfRnf);
  Tools.HintOut(true,'Drain.Basins: '+cfCtm);
end;

{ bDn bildet einen Mittelwert aus den Attributen jeder Zone und allen direkten
  Nachbar-Zonen und speichert das Ergebnis unter dem ursprünglichen Namen. bDn
  wiederholt die Procedur "iGen" mal. Die Attribute müssen existieren }
{ bDn kopiert jedes bestehende Attribut in einen Buffer, berechnet mit ihm die
  neue Attribute und speicher sie in der ursprünglichen Tabelle. bDn gewichtet
  die Attribute aller Zonen mit der Anzahl der Kontakte. Für die zentrale Zone
  sind das die inneren Kontakte. Große Zonen verändern sich dadurch weniger als
  kleine. }

procedure tBuild.xDiffusion(
  iGen:integer; //Nachbar-Generationen
  sAtr:string); //Zonen-Attribute
const
  cGen = 'Error(bDn): Iterations must be positive!';
  cVal = 'Error(bDn): Attribute table not found!';
var
  faTmp:tnSgl=nil; //Backup für altes Attribut
  fRes:single; //Zwischenlager für Mittelwert
  fxAtr:tn2Sgl=nil; //Attribut-Tabelle
  paDim:^tnInt=nil; //Index auf "iacNbr, iacPrm"
  paNbr:^tnInt=nil; //Index der Nachbarzelle
  paPrm:^tnInt=nil; //Kontakte zur Nachbarzelle
  iCnt:integer; //Summe Kontakte
  ixTpl:tn2Int=nil; //Zell-Topologie
  A,G,N,Z:integer;
begin
  if not FileExists(sAtr) then Tools.ErrorOut(2,cVal);
  if iGen<1 then Tools.ErrorOut(2,cGen);
  ixTpl:=tn2Int(Tools.BitRead(eeHme+cfTpl)); //Topologie
  paDim:=@ixTpl[0]; //Zeiger auf Startadressen
  paNbr:=@ixTpl[1]; //Zeiger auf Nachbar-IDs
  paPrm:=@ixTpl[2]; //Länge der Grenzen
  fxAtr:=Tools.BitRead(sAtr); //Attribute[band,zone]
  faTmp:=Tools.InitSingle(length(fxAtr[0]),0); //Zwischenlager
  for G:=1 to iGen do
    for A:=0 to high(fxAtr) do
    begin
      move(fxAtr[A,0],faTmp[0],length(faTmp)*SizeOf(single)); //Backup altes Attribut
      FillDWord(fxAtr[A,0],length(fxAtr[A]),0); //Attribut leeren
      for Z:=1 to high(paDim^) do
      begin
        fRes:=0; iCnt:=0;
        for N:=paDim^[pred(Z)] to pred(paDim^[Z]) do //alle Kontakte (auch innere)
        begin
          fRes+=faTmp[paNbr^[N]]*paPrm^[N]; //gewichteter Mittelwert
          iCnt+=paPrm^[N]; //Kontakte zählen
        end;
        if iCnt>0 then fxAtr[A,Z]:=fRes/iCnt;
      end;
    end;
  Tools.BitWrite(fxAtr,sAtr); //überschreiben
end;

{ iZV erzeugt ein Multikanal-Bild mit den Zonen-Attributen als Farben. iZV
  liest den Zellindex und die Attribte und überträgt die Zonen-Mittelwerte
  sukzessive auf die Kanäle der Bildaten. iZV überträgt Null im Zellindex auf
  NoData im Bild. }

procedure tBuild.xZoneValues(sFtr:string); //Attribute als Bit-Tabelle
var
  fxBnd:tn2Sgl=nil; //Kanal mit Attribut-Werten
  fxVal:tn2Sgl=nil; //Attribut-Tabelle
  ixIdx:tn2Int=nil; //Zonen als Raster
  pVal:^tnSgl=nil; //Zeiger auf Attribut
  rHdr:trHdr; //gemeinsame Metadaten
  I,X,Y:integer;
begin
  //Tools.BitSize = Tools.CommaToLine.Count?
  fxVal:=Tools.BitRead(sFtr); //alle Zonen Attribute
  Header.Read(rHdr,eeHme+cfIdx); //Zonen Metadaten
  ixIdx:=tn2Int(Image.ReadBand(0,rHdr,eeHme+cfIdx)); //Zonen Raster
  fxBnd:=Tools.Init2Single(length(ixIdx),length(ixIdx[0]),dWord(NaN)); //leerer Kanal
  for I:=0 to high(fxVal) do
  begin
    pVal:=@fxVal[I];
    for Y:=0 to high(ixIdx) do
      for X:=0 to high(ixIdx[0]) do
        if ixIdx[Y,X]>0 then
          fxBnd[Y,X]:=pVal^[ixIdx[Y,X]]; //nur definierte Bildpixel
    Image.WriteBand(fxBnd,I,eeHme+cfVal);
  end;
  Header.WriteMulti(rHdr,Tools.CommaToLine(rHdr.Fld),eeHme+cfVal);
  Tools.HintOut(true,'Image.ZoneValues: '+cfVal);
end;

initialization

  Union:=tUnion.Create;
  Union.ixcIdx:=nil;
  Union.ixcMap:=nil;
  Union.iacChn:=nil;

finalization

  SetLength(Union.ixcIdx,0);
  SetLength(Union.ixcMap,0);
  SetLength(Union.iacChn,0);
  Union.Free;

end.

{==============================================================================}

{ bDn erzeugt einen Werte-Ausgleich zwischen Attributen. }
{ bDn bestiimt für jede Zelle den Mittelwert das Attributs "faVal" für die
  zentrale Zelle und alle Nachbarzellen. Die Werte sind mit der Länge der
  gemensamen Grenze gewichtet. bDn iteriert den Vorgang "iGen" mal. }

procedure tBuild.xDiffusion_(iGen:integer); //Nachbar-Generationen
const
  cGen = 'Error(bDn): Iterations must be positive!';
  cVal = 'Error(bDn): Attribute table not found!';
var
  faTmp:tnSgl=nil; //Backup für altes Attribut
  fRes:single; //Zwischenlager für Mittelwert
  fxAtr:tn2Sgl=nil; //Attribut-Tabelle
  paDim:^tnInt=nil; //Index auf "iacNbr, iacPrm"
  paNbr:^tnInt=nil; //Index der Nachbarzelle
  paPrm:^tnInt=nil; //Kontakte zur Nachbarzelle
  iCnt:integer; //Summe Kontakte
  ixTpl:tn2Int=nil; //Zell-Topologie
  A,G,N,Z:integer;
begin
  if not FileExists(eeHme+cfAtr) then Tools.ErrorOut(2,cVal);
  if iGen<1 then Tools.ErrorOut(2,cGen);
  ixTpl:=tn2Int(Tools.BitRead(eeHme+cfTpl)); //Topologie
  paDim:=@ixTpl[0]; //Zeiger auf Startadressen
  paNbr:=@ixTpl[1]; //Zeiger auf Nachbar-IDs
  paPrm:=@ixTpl[2]; //Länge der Grenzen
  fxAtr:=Tools.BitRead(eeHme+cfAtr); //Attribute[band,zone]
  faTmp:=Tools.InitSingle(length(fxAtr[0]),0); //Zwischenlager
  for G:=1 to iGen do
    for A:=0 to high(fxAtr) do
    begin
      move(fxAtr[A,0],faTmp[0],length(faTmp)*SizeOf(single)); //Backup altes Attribut
      FillDWord(fxAtr[A,0],length(fxAtr[A]),0); //Attribut leeren
      for Z:=1 to high(paDim^) do
      begin
        fRes:=0; iCnt:=0;
        for N:=paDim^[pred(Z)] to pred(paDim^[Z]) do //alle Kontakte (auch innere)
        begin
          fRes+=faTmp[paNbr^[N]]*paPrm^[N]; //gewichteter Mittelwert
          iCnt+=paPrm^[N]; //Kontakte zählen
        end;
        if iCnt>0 then fxAtr[A,Z]:=fRes/iCnt;
      end;
    end;
  Tools.BitWrite(fxAtr,eeHme+cfAtr); //überschreiben
end;

function tBuild.C_heckZones(sSrc:string):boolean;
const
  cAtr = 'Zonal classification needs a zonas attribute table "index.bit"';
  cIdx = 'Zonal classification needs a zones definition image "index"';
  cTpl = 'Zonal classification needs a zonas topology "topology.bit"';
begin
  Result:=
    FileExists(eeHme+cfIdx) and
    FileExists(eeHme+cfAtr) and
    FileExists(eeHme+cfTpl);
  if not Result then
  begin
    if not FileExists(eeHme+cfIdx) then Tools.ErrorOut(2,sSrc+cIdx);
    if not FileExists(eeHme+cfAtr) then Tools.ErrorOut(2,sSrc+cAtr);
    if not FileExists(eeHme+cfTpl) then Tools.ErrorOut(2,sSrc+cTpl);
  end;
end;

{ bEF erweitert die Attribut-Tabelle "cfAtr" um die lokale Dichte aller
  Attribute in der Umgebung und speichert das Ergebnis als "cfCtx". }
{ bEF bestimmt die lokale Dichte durch einen imitierten Diffusions-Prozess.
  Jeder Kontakt am Rand der Zone schickt eine Einheit Merkmale an die Nachbar-
  Zone. Aufgenommene Einheiten verdünnen die eigenen. Zu Beginn hat jede Zone
  so viele Einheiten wie Kontakte. Der Algorithmus iteriert, die Zahl der
  Iterationen ist einstellbar. Das Ergebnis konvergiert. }

procedure tBuild.E_xtendFeatures(
  iGen:integer; //Iterationen für Kontext
  sAtr:string); //Attribut-Tabelle
const
  cGen = 'bEF: Feature extension needs a positive input!';
var
  fxAtr:tn2Sgl=nil; //Attribute mit Diffusion
  F:integer;
begin
  if iGen<1 then Tools.ErrorOut(2,cGen);
  fxAtr:=Tools.BitRead(sAtr); //Attribut-Tabelle lesen
  for F:=0 to high(fxAtr) do //alle Merkmale
    fxAtr[F]:=Diffusion_(fxAtr[F],iGen); //Werteausgleich, iteriert
  Tools.CopyFile(sAtr,eeHme+cfCtx); //Originale kopieren           ..
  for F:=0 to high(fxAtr) do //alle Merkmale
    Tools.BitInsert(fxAtr[F],MaxInt,eeHme+cfCtx); //neue Attribute ergänzen
  Tools.HintOut(true,'Build.ExtendFeatures: '+cfCtx+': '+IntToStr(iGen))
end;

{ bDn erzeugt einen Werte-Ausgleich zwischen Attributen. }
{ bDn bestiimt für jede Zelle den Mittelwert das Attributs "faVal" für die
  zentrale Zelle und alle Nachbarzellen. Die Werte sind mit der Länge der
  gemensamen Grenze gewichtet. bDn iteriert den Vorgang "iGen" mal. }

function tBuild.D_iffusion_(
  faVal:tnSgl; //Attribut
  iGen:integer): //Nachbar-Generationen
  tnSgl; //Attribut nach Vereinigung
const
  cGen = 'Error(bLM): Iterations must be positive!';
  cVal = 'Error(bLM): Attribute not defined!';
var
  fRes:single; //Zwischenlager für Mittelwert
  paDim: ^tnInt=nil; //Index auf "iacNbr, iacPrm"
  paNbr: ^tnInt=nil; //Index der Nachbarzelle
  paPrm: ^tnInt=nil; //Kontakte zur Nachbarzelle
  iCnt: integer; //Summe Kontakte
  ixTpl: tn2Int=nil; //Zell-Topologie
  I,N,Z:integer;
begin
  if faVal=nil then Tools.ErrorOut(2,cVal);
  if iGen<1 then Tools.ErrorOut(2,cGen);
  Result:=Tools.InitSingle(length(faVal),0);
  ixTpl:=tn2Int(Tools.BitRead(eeHme+cfTpl));
  paDim:=@ixTpl[0]; //Zeiger auf Startadressen
  paNbr:=@ixTpl[1]; //Zeiger auf Nachbar-IDs
  paPrm:=@ixTpl[2]; //Länge der Grenzen
  for I:=1 to iGen do
  begin
    for Z:=1 to high(paDim^) do
    begin
      fRes:=0; iCnt:=0;
      for N:=paDim^[pred(Z)] to pred(paDim^[Z]) do //alle Kontakte (auch innere)
      begin
        fRes+=faVal[paNbr^[N]]*paPrm^[N]; //gewichteter Mittelwert
        iCnt+=paPrm^[N]; //Kontakte zählen
      end;
      if iCnt>0 then Result[Z]:=fRes/iCnt;
    end;
    faVal:=copy(Result,0,length(faVal)); //identische Kopie
  end;
end;

{ bLF bestimmt die Häufigkeit der Kontakte zwischen jeweils zwei Klassen auf
  Pixelebene und speichert das Ergebnis als "cfCtx". Dazu muss die Zonen-
  Topologie im Arbeitsverzeichnis stehen. bLF zählt alle Kontakte, auch die
  Kontakte innerhalb der Zonen. Große Zonen sind dann weitgehend durch ihre
  interne Klasse bestimmt. }

procedure tBuild.L_inkFeatures(
  iaThm:tnInt; //Klassen-Attribut
  iMap:integer); //Anzahl Klassen
var
  fxCtx:tn2Sgl=nil; //Anteile Nachbar-Klassen
  iSze:integer; //Summe Kontakte
  ixTpl:tn2Int=nil; //Zonen-Topplogie
  paDim:^tnInt=nil; //Topologie Einsprung-Adressen
  paNbr:^tnInt=nil; //Topologie ID Nachbarzonen
  paPrm:^tnInt=nil; //Topologie Anzahl Kontakte
  N,M,Z:integer;
begin
 ixTpl:=tn2Int(Tools.BitRead(eeHme+cfTpl));
 paDim:=@ixTpl[0]; //Zeiger auf Startadressen
 paNbr:=@ixTpl[1]; //Zeiger auf Nachbar-IDs
 paPrm:=@ixTpl[2]; //Länge der Grenzen
 fxCtx:=Tools.Init2Single(succ(iMap),length(ixTpl[0]),0);
 for Z:=1 to high(paDim^) do //alle Zonen
 begin
   iSze:=0;
   for N:=paDim^[pred(Z)] to pred(paDim^[Z]) do //alle Kontakte (auch innere)
   begin
     fxCtx[iaThm[paNbr^[N]],Z]+=paPrm^[N]; //Kontakte pro Klasse
     iSze+=paPrm^[N]; //Summe Kontakte
   end;
   if iSze>0 then
     for M:=1 to iMap do //alle Klassen
       fxCtx[M,Z]/=iSze; //Kontakte normalisieren
 end;
 Tools.BitWrite(fxCtx,eeHme+cfCtx); //"ZonesSample" braucht Datei
 Tools.HintOut(true,'Build.LinkFeatures: '+cfCtx)
end;

