      {******************************************************************}
      { SVG style class                                                  }
      {                                                                  }
      { home page : http://www.mwcs.de                                   }
      { email     : martin.walter@mwcs.de                                }
      {                                                                  }
      { date      : 26-04-2005                                           }
      {                                                                  }
      { Use of this file is permitted for commercial and non-commercial  }
      { use, as long as the author is credited.                          }
      { This file (c) 2005 Martin Walter                                 }
      {                                                                  }
      { This Software is distributed on an "AS IS" basis, WITHOUT        }
      { WARRANTY OF ANY KIND, either express or implied.                 }
      {                                                                  }
      { *****************************************************************}

unit SVGStyle;

interface

uses
  Classes,
  WideStringList;

type
  TStyle = class(TObject)
  private
    FValues: TWideStringList;
    FName: WideString;

    function GetCount: Integer;
    procedure Put(const Key: WideString; const Value: WideString);
    function Get(const Key: WideString): WideString;

    procedure PutValue(Index: Integer; const Value: WideString);
    function GetValue(Index: Integer): WideString;

    procedure PutKey(Index: Integer; const Key: WideString);
    function GetKey(Index: Integer): WideString;

    function Dequote(const Value: WideString): WideString;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Clone: TStyle;
    procedure SetValues(const Values: WideString);

    function AddStyle(const Key, Value: WideString): Integer;
    function IndexOf(const Key: WideString): Integer;
    procedure Delete(Index: Integer);
    function Remove(const Key: WideString): Integer;

    property Count: Integer read GetCount;
    property Values[const Key: WideString]: WideString read Get write Put; default;
    property ValuesByNum[Index: Integer]: WideString read GetValue write PutValue;
    property Keys[Index: Integer]: WideString read GetKey write PutKey;
  end;

  TStyleList = class(TObject)
  private
    FList: TList;

    function GetCount: Integer;
    function Get(Index: Integer): TStyle;
    procedure Put(Index: Integer; Style: TStyle);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Clone: TStyleList;

    procedure Delete(Index: Integer);
    function Remove(Style: TStyle): Integer;
    function Add(Style: TStyle): Integer; overload;
    function Add(const Name, Values: WideString): Integer; overload;
    function Add(const AStyle: WideString): Integer; overload;

    procedure Insert(Index: Integer; Style: TStyle); overload;
    procedure Insert(Index: Integer; const Name, Values: WideString); overload;
    procedure Exchange(Index1, Index2: Integer);
    procedure Move(CurIndex, NewIndex: Integer);
    function IndexOf(Style: TStyle): Integer;
    function GetStyle(const Name: WideString): TStyle;

    property Style[Index: Integer]: TStyle read Get write Put; default;
    property Count: Integer read GetCount;
  end;

implementation

uses
  SysUtils, StrUtils;

// TStyle

constructor TStyle.Create;
begin
  inherited;
  FValues := TWideStringList.Create;
  FValues.NameValueSeparator := '"';
end;

destructor TStyle.Destroy;
begin
  FreeAndNil(FValues);
  inherited;
end;

procedure TStyle.Clear;
begin
  if FValues <> nil then
    FValues.Clear;
end;

function TStyle.Clone: TStyle;
begin
  Result := TStyle.Create;
  Result.FName := FName;
  Result.FValues.Assign(FValues);
end;

function TStyle.GetCount: Integer;
begin
  Result := FValues.Count;
end;

procedure TStyle.Put(const Key: WideString; const Value: WideString);
var
  Index: Integer;
begin
  Index := IndexOf(Key);
  if Index > 0 then
    PutValue(Index, Value)
  else
    AddStyle(Key, Value);
end;

function TStyle.Get(const Key: WideString): WideString;
begin
  Result := GetValue(IndexOf(Key));
end;

procedure TStyle.PutValue(Index: Integer; const Value: WideString);
begin
  if (Index >= 0) and (Index < FValues.Count) then
    FValues.ValueFromIndex[Index] := DeQuote(Value);
end;

function TStyle.GetValue(Index: Integer): WideString;
begin
  if (Index >= 0) and (Index < FValues.Count) then
    Result := FValues.ValueFromIndex[Index]
  else
    Result := '';
end;

procedure TStyle.PutKey(Index: Integer; const Key: WideString);
begin
  if (Index >= 0) and (Index < FValues.Count) then
    FValues[Index] := Key + FValues.NameValueSeparator + FValues.ValueFromIndex[Index];
end;

function TStyle.GetKey(Index: Integer): WideString;
begin
  if (Index >= 0) and (Index < FValues.Count) then
    Result := FValues.Names[Index]
  else
    Result := '';
end;

function TStyle.Dequote(const Value: WideString): WideString;
begin
  if Value <> '' then
  begin
    if (Value[1] = '''') and (Value[Length(Value)] = '''') then
      Result := Copy(Value, 2, Length(Value) - 2)
    else
      if (Value[1] = '"') and (Value[Length(Value)] = '"') then
        Result := Copy(Value, 2, Length(Value) - 2)
      else
        Result := Value;
  end else
    Result := Value;
end;

procedure TStyle.SetValues(const Values: WideString);
var
  C: Integer;
  Key: WideString;
  Value: WideString;
  Help: WideString;
begin
  Help := Trim(Values);

  while Help <> '' do
  begin
    C := Pos(';', Help);
    if C = 0 then
      C := Length(Help) + 1;
    Key := Copy(Help, 1, C - 1);
    Help := Trim(Copy(Help, C + 1, MaxInt));
    C := Pos(':', Key);
    if C <> 0 then
    begin
      Value := Trim(Copy(Key, C + 1, MaxInt));
      Key := Trim(Copy(Key, 1, C - 1));

      C := IndexOf(Key);
      if C = -1 then
        FValues.Add(Key + FValues.NameValueSeparator + DeQuote(Value))
      else
        PutValue(C, Value);
    end;
  end;
end;

function TStyle.AddStyle(const Key, Value: WideString): Integer;
begin
  Result := IndexOf(Key);
  if Result = -1 then
    Result := FValues.Add(Key + FValues.NameValueSeparator + DeQuote(Value))
  else
    PutValue(Result, Value);
end;

function TStyle.IndexOf(const Key: WideString): Integer;
begin
  for Result := 0 to FValues.Count - 1 do
    if FValues.Names[Result] = Key then
      Exit;
  Result := -1;
end;

procedure TStyle.Delete(Index: Integer);
begin
  if (Index >= 0) and (Index < FValues.Count) then
    FValues.Delete(Index);
end;

function TStyle.Remove(const Key: WideString): Integer;
begin
  Result := IndexOf(Key);
  Delete(Result);
end;

// TStyleList

constructor TStyleList.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TStyleList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

procedure TStyleList.Clear;
begin
  while FList.Count > 0 do
  begin
    TStyle(FList[0]).Free;
    FList.Delete(0);
  end;
end;

function TStyleList.Clone: TStyleList;
var
  C: Integer;
begin
  Result := TStyleList.Create;
  for C := 0 to FList.Count - 1 do
    Result.Add(Get(C).Clone);
end;

function TStyleList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TStyleList.Get(Index: Integer): TStyle;
begin
  if (Index >= 0) and (Index < FList.Count) then
    Result := FList[Index]
  else
    Result := nil;
end;

procedure TStyleList.Put(Index: Integer; Style: TStyle);
begin
  if (Index >= 0) and (Index < FList.Count) then
  begin
    try
      TStyle(FList[Index]).Free;
    except
    end;
    FList[Index] := Style;
  end;
end;

procedure TStyleList.Delete(Index: Integer);
begin
  if (Index >= 0) and (Index < FList.Count) then
  begin
    TStyle(FList[Index]).Free;
    FList.Delete(Index);
  end;
end;

function TStyleList.Remove(Style: TStyle): Integer;
begin
  Result := IndexOf(Style);
  Delete(Result);
end;

function TStyleList.Add(Style: TStyle): Integer;
begin
  Result := FList.Add(Style);
end;

function TStyleList.Add(const Name, Values: WideString): Integer;
var
  S: TStyle;
begin
  S := TStyle.Create;
  S.FName := Name;
  S.SetValues(Values);
  Result := Add(S);
end;

function TStyleList.Add(const AStyle: WideString): Integer;
var
  Name: WideString;
  Style: WideString;
  Values: WideString;
  C, D: Integer;
begin
  Result := -1;
  Style := Trim(AStyle);
  for C := 1 to Length(Style) do
    if Style[C] = '{' then
    begin
      for D := Length(Style) downto C + 1 do
        if Style[D] = '}' then
        begin
          Name := Trim(Copy(Style, 1, C - 1));

          Values := Copy(Style, C + 1, D - C - 1);
          Result := Add(Name, Values);
        end;
    end;
end;

procedure TStyleList.Insert(Index: Integer; Style: TStyle);
begin
  if (Index >= 0) and (Index < FList.Count) then
    FList.Insert(Index, Style);
end;

procedure TStyleList.Insert(Index: Integer; const Name, Values: WideString);
var
  S: TStyle;
begin
  if (Index >= 0) and (Index < FList.Count) then
  begin
    S := TStyle.Create;
    S.FName := Name;
    S.SetValues(Values);
    Insert(Index, S);
  end;
end;

procedure TStyleList.Exchange(Index1, Index2: Integer);
begin
  if (Index1 >= 0) and (Index1 < FList.Count) and
     (Index2 >= 0) and (Index2 < FList.Count) then
    FList.Exchange(Index1, Index2);
end;

procedure TStyleList.Move(CurIndex, NewIndex: Integer);
begin
  if (CurIndex >= 0) and (CurIndex < FList.Count) and
     (NewIndex >= 0) and (NewIndex < FList.Count) then
    FList.Move(CurIndex, NewIndex);
end;

function TStyleList.IndexOf(Style: TStyle): Integer;
begin
  Result := FList.IndexOf(Style);
end;

function TStyleList.GetStyle(const Name: WideString): TStyle;
var
  C: Integer;
begin
  for C := 0 to FList.Count - 1 do
  begin
    Result := TStyle(FList[C]);
    if Result.FName = Name then
      Exit;
  end;

  Result := nil;
end;


end.
