      {******************************************************************}
      { SVG common                                                       }
      {                                                                  }
      { home page : http://www.mwcs.de                                   }
      { email     : martin.walter@mwcs.de                                }
      {                                                                  }
      { date      : 05-04-2008                                           }
      {                                                                  }
      { Use of this file is permitted for commercial and non-commercial  }
      { use, as long as the author is credited.                          }
      { This file (c) 2005, 2008 Martin Walter                           }
      {                                                                  }
      { This Software is distributed on an "AS IS" basis, WITHOUT        }
      { WARRANTY OF ANY KIND, either express or implied.                 }
      {                                                                  }
      { *****************************************************************}

unit SVGCommon;

interface

uses
  SVGTypes;

function TryStrToTFloat(const S: AnsiString; out Value: TFloat): Boolean;

function StrToTFloat(const S: AnsiString): TFloat;

implementation

uses
  SysUtils;

function TryStrToTFloat(const S: AnsiString; out Value: TFloat): Boolean;
var
  S1: AnsiString;
begin
  S1 := StringReplace(S, ',', DecimalSeparator, [rfReplaceAll]);
  S1 := StringReplace(S1, '.', DecimalSeparator, [rfReplaceAll]);
  Result := TryStrToFloat(S1, Value);
  if not Result then
    Value := 0;
end;

function StrToTFloat(const S: AnsiString): TFloat;
begin
  TryStrToTFloat(S, Result);
end;

end.
