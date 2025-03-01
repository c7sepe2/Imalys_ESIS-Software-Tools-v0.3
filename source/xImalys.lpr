program xImalys;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, raster, format, mutual, thema, index, custom, vector;

const
  cPrm = 'Imalys call: Parameter file must be provided!';
begin
  if ParamCount>1 then
    Parse.xLoop(ParamStr(1),ParamStr(2)) //Schalter + Hook
  else if ParamCount>0 then
    Parse.xLoop('',ParamStr(1)) //nur Hook
  else raise Exception.Create(cPrm);
end.

