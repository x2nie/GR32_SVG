      {******************************************************************}
      { SVG path classes                                                 }
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

unit SVGPath;

interface

uses
  Windows, Classes,
  //GDIPOBJ,
  GR32,
  SVGTypes, SVG;

type
  TSVGPathElement = class(TSVGObject)
  private
    FStartX: TFloat;
    FStartY: TFloat;
    FStopX: TFloat;
    FStopY: TFloat;
  protected
    procedure Assign(SVG: TSVGObject); override;
    function New(Parent: TSVGObject): TSVGObject; override;
  public
    function GetBounds: TFRect; virtual; abstract;
    procedure AddToPath(Path: TArrayOfArrayOfFloatPoint); virtual; abstract;
    procedure Read(SL: TStrings; var Position: Integer;
      Previous: TSVGPathElement); virtual;

    procedure PaintToGraphics(Graphics: TBitmap32); override;
    procedure PaintToPath(Path: TArrayOfArrayOfFloatPoint); override;

    property StartX: TFloat read FStartX write FStartX;
    property StartY: TFloat read FStartY write FStartY;
    property StopX: TFloat read FStopX write FStopX;
    property StopY: TFloat read FStopY write FStopY;
  end;

  TSVGPathMove = class(TSVGPathElement)
  private
  protected
    function New(Parent: TSVGObject): TSVGObject; override;
  public
    function GetBounds: TFRect; override;
    procedure AddToPath(Path: TArrayOfArrayOfFloatPoint); override;
    procedure Read(SL: TStrings; var Position: Integer;
      Previous: TSVGPathElement); override;
  end;

  TSVGPathLine = class(TSVGPathElement)
  private
  protected
    function New(Parent: TSVGObject): TSVGObject; override;
  public
    function GetBounds: TFRect; override;
    procedure AddToPath(Path: TArrayOfArrayOfFloatPoint); override;
    procedure Read(SL: TStrings; var Position: Integer;
      Previous: TSVGPathElement); override;
  end;

  TSVGPathCurve = class(TSVGPathElement)
  private
    FControl1X: TFloat;
    FControl1Y: TFloat;
    FControl2X: TFloat;
    FControl2Y: TFloat;
  protected
    procedure Assign(SVG: TSVGObject); override;
    function New(Parent: TSVGObject): TSVGObject; override;
  public
    function GetBounds: TFRect; override;
    procedure AddToPath(Path: TArrayOfArrayOfFloatPoint); override;
    procedure Read(SL: TStrings; var Position: Integer;
      Previous: TSVGPathElement); override;

    property Control1X: TFloat read FControl1X write FControl1X;
    property Control1Y: TFloat read FControl1Y write FControl1Y;
    property Control2X: TFloat read FControl2X write FControl2X;
    property Control2Y: TFloat read FControl2Y write FControl2Y;
  end;

  TSVGPathEllipticArc = class(TSVGPathElement)
  private
    FRX: TFloat;
    FRY: TFloat;
    FXRot: TFloat;
    FLarge: Integer;
    FSweep: Integer;
  protected
    procedure Assign(SVG: TSVGObject); override;
    function New(Parent: TSVGObject): TSVGObject; override;
  public
    function GetBounds: TFRect; override;
    procedure AddToPath(Path: TArrayOfArrayOfFloatPoint); override;
    procedure Read(SL: TStrings; var Position: Integer;
      Previous: TSVGPathElement); override;

    property RX: TFloat read FRX write FRX;
    property RY: TFloat read FRY write FRY;
    property XRot: TFloat read FXRot write FXRot;
    property Large: Integer read FLarge write FLarge;
    property Sweep: Integer read FSweep write FSweep;
  end;

  TSVGPathClose = class(TSVGPathElement)
  private
  protected
    function New(Parent: TSVGObject): TSVGObject; override;
  public
    function GetBounds: TFRect; override;
    procedure AddToPath(Path: TArrayOfArrayOfFloatPoint); override;
    procedure Read(SL: TStrings; var Position: Integer;
      Previous: TSVGPathElement); override;
  end;

implementation

uses
  SysUtils, Math,
  //GDIPAPI,
  SVGCommon, SVGParse;

// TSVGPathElement

procedure TSVGPathElement.Assign(SVG: TSVGObject);
begin
  inherited;
  if SVG is TSVGPathElement then
  begin
    FStartX := TSVGPathElement(SVG).FStartX;
    FStartY := TSVGPathElement(SVG).FStartY;
    FStopX :=  TSVGPathElement(SVG).FStopX;
    FStopY :=  TSVGPathElement(SVG).FStopY;
  end;
end;

function TSVGPathElement.New(Parent: TSVGObject): TSVGObject;
begin
  Result := nil;
end;

procedure TSVGPathElement.Read(SL: TStrings; var Position: Integer;
  Previous: TSVGPathElement);
begin
  if Assigned(Previous) then
  begin
    FStartX := Previous.FStopX;
    FStartY := Previous.FStopY;
  end;
end;

procedure TSVGPathElement.PaintToGraphics(Graphics: TBitmap32);
begin
end;

procedure TSVGPathElement.PaintToPath(Path: TArrayOfArrayOfFloatPoint);
begin
end;

// TSVGPathMove

function TSVGPathMove.GetBounds: TFRect;
begin
  Result.Width := 0;
  Result.Height := 0;
end;

function TSVGPathMove.New(Parent: TSVGObject): TSVGObject;
begin
  Result := TSVGPathMove.Create(Parent);
end;

procedure TSVGPathMove.AddToPath(Path: TArrayOfArrayOfFloatPoint);
begin
  //x2nie Path.StartFigure;
end;

procedure TSVGPathMove.Read(SL: TStrings; var Position: Integer;
  Previous: TSVGPathElement);
begin
  inherited;
  if not TryStrToTFloat(SL[Position + 1], FStopX) then
    FStopX := 0;
  if not TryStrToTFloat(SL[Position + 2], FStopY) then
    FStopY := 0;

  if SL[Position] = 'm' then
  begin
    FStopX := FStartX + FStopX;
    FStopY := FStartY + FStopY;
  end;

  Inc(Position, 2);
end;


// TSVGPathLine

function TSVGPathLine.GetBounds: TFRect;
begin
  Result.Left := Min(FStartX, FStopX);
  Result.Top := Min(FStartY, FStopY);
  Result.Width := Abs(FStartX - FStopX);
  Result.Height := Abs(FStartY - FStopY);
end;

function TSVGPathLine.New(Parent: TSVGObject): TSVGObject;
begin
  Result := TSVGPathLine.Create(Parent);
end;

procedure TSVGPathLine.AddToPath(Path: TArrayOfArrayOfFloatPoint);
begin
  //x2niePath.AddLine(FStartX, FStartY, FStopX, FStopY);
end;

procedure TSVGPathLine.Read(SL: TStrings; var Position: Integer;
  Previous: TSVGPathElement);
var
  Command: AnsiString;
begin
  inherited;

  Command := SL[Position];
  if (Command = 'L') or (Command = 'l') then
  begin
    if not TryStrToTFloat(SL[Position + 1], FStopX) then
      FStopX := 0;
    if not TryStrToTFloat(SL[Position + 2], FStopY) then
      FStopY := 0;

    if SL[Position] = 'l' then
    begin
      FStopX := FStartX + FStopX;
      FStopY := FStartY + FStopY;
    end;

    Inc(Position, 2);
  end;

  if (Command = 'H') or (Command = 'h') then
  begin
    if not TryStrToTFloat(SL[Position + 1], FStopX) then
      FStopX := 0;

    if Command = 'h' then
      FStopX := FStartX + FStopX;
    FStopY := FStartY;
    Inc(Position);
  end;


  if (Command = 'V') or (Command = 'v') then
  begin
    if not TryStrToTFloat(SL[Position + 1], FStopY) then
      FStopY := 0;

    if Command = 'v' then
      FStopY := FStartY + FStopY;
    FStopX := FStartX;
    Inc(Position);
  end;
end;


// TSVGPathCurve

procedure TSVGPathCurve.Assign(SVG: TSVGObject);
begin
  inherited;
  if SVG is TSVGPathCurve then
  begin
    FControl1X := TSVGPathCurve(SVG).FControl1X;
    FControl1Y := TSVGPathCurve(SVG).FControl1Y;
    FControl2X := TSVGPathCurve(SVG).FControl2X;
    FControl2Y := TSVGPathCurve(SVG).FControl2Y;
  end;
end;

function TSVGPathCurve.GetBounds: TFRect;
var
  Right, Bottom: TFloat;
begin
  Result.Left := Min(FStartX, Min(FStopX, Min(FControl1X, FControl2X)));
  Result.Top := Min(FStartY, Min(FStopY, Min(FControl1Y, FControl2Y)));

  Right := Max(FStartX, Max(FStopX, Max(FControl1X, FControl2X)));
  Bottom := Max(FStartY, Max(FStopY, Max(FControl1Y, FControl2Y)));
  Result.Width := Right - Result.Left;
  Result.Height := Bottom - Result.Top;
end;

function TSVGPathCurve.New(Parent: TSVGObject): TSVGObject;
begin
  Result := TSVGPathCurve.Create(Parent);
end;

procedure TSVGPathCurve.AddToPath(Path: TArrayOfArrayOfFloatPoint);
begin
  //x2niePath.AddBezier(FStartX, FStartY, FControl1X, FControl1Y,
    //FControl2X, FControl2Y, FStopX, FStopY);
end;

procedure TSVGPathCurve.Read(SL: TStrings; var Position: Integer;
  Previous: TSVGPathElement);
var
  Command: AnsiString;
begin
  inherited;

  Command := SL[Position];
  if (Command = 'C') or (Command = 'c') then
  begin
    TryStrToTFloat(SL[Position + 1], FControl1X);
    TryStrToTFloat(SL[Position + 2], FControl1Y);
    TryStrToTFloat(SL[Position + 3], FControl2X);
    TryStrToTFloat(SL[Position + 4], FControl2Y);
    TryStrToTFloat(SL[Position + 5], FStopX);
    TryStrToTFloat(SL[Position + 6], FStopY);
    Inc(Position, 6);

    if Command = 'c' then
    begin
      FControl1X := FStartX + FControl1X;
      FControl1Y := FStartY + FControl1Y;
      FControl2X := FStartX + FControl2X;
      FControl2Y := FStartY + FControl2Y;
      FStopX := FStartX + FStopX;
      FStopY := FStartY + FStopY;
    end;
  end;

  if (Command = 'S') or (Command = 's') then
  begin
    FControl1X := FStartX;
    FControl1Y := FStartY;
    TryStrToTFloat(SL[Position + 1], FControl2X);
    TryStrToTFloat(SL[Position + 2], FControl2Y);
    TryStrToTFloat(SL[Position + 3], FStopX);
    TryStrToTFloat(SL[Position + 4], FStopY);
    Inc(Position, 4);

    if Previous is TSVGPathCurve then
    begin
      FControl1X := FStartX + (FStartX - TSVGPathCurve(Previous).FControl2X);
      FControl1Y := FStartY + (FStartY - TSVGPathCurve(Previous).FControl2Y);
    end;

    if Command = 's' then
    begin
      FControl2X := FStartX + FControl2X;
      FControl2Y := FStartY + FControl2Y;
      FStopX := FStartX + FStopX;
      FStopY := FStartY + FStopY;
    end;
  end;

  if (Command = 'Q') or (Command = 'q') then
  begin
    TryStrToTFloat(SL[Position + 1], FControl1X);
    TryStrToTFloat(SL[Position + 2], FControl1Y);
    TryStrToTFloat(SL[Position + 3], FStopX);
    TryStrToTFloat(SL[Position + 4], FStopY);
    FControl2X := FControl1X;
    FControl2Y := FControl1Y;
    Inc(Position, 4);

    if Command = 'q' then
    begin
      FControl1X := FStartX + FControl1X;
      FControl1Y := FStartY + FControl1Y;
      FControl2X := FStartX + FControl2X;
      FControl2Y := FStartY + FControl2Y;
      FStopX := FStartX + FStopX;
      FStopY := FStartY + FStopY;
    end;
  end;

  if (Command = 'T') or (Command = 't') then
  begin
    FControl1X := FStartX;
    FControl1Y := FStartY;
    TryStrToTFloat(SL[Position + 1], FStopX);
    TryStrToTFloat(SL[Position + 2], FStopY);
    Inc(Position, 2);

    if Previous is TSVGPathCurve then
    begin
      FControl1X := FStartX + (FStartX - TSVGPathCurve(Previous).FControl2X);
      FControl1Y := FStartY + (FStartY - TSVGPathCurve(Previous).FControl2Y);
    end;

    FControl2X := FControl1X;
    FControl2Y := FControl1Y;

    if Command = 't' then
    begin
      FStopX := FStartX + FStopX;
      FStopY := FStartY + FStopY;
    end;
  end;
end;


// TSVGPathEllipticArc

procedure TSVGPathEllipticArc.Assign(SVG: TSVGObject);
begin
  inherited;
  if SVG is TSVGPathEllipticArc then
  begin
    FRX := TSVGPathEllipticArc(SVG).FRX;
    FRY := TSVGPathEllipticArc(SVG).FRY;
    FXRot := TSVGPathEllipticArc(SVG).FXRot;
    FLarge := TSVGPathEllipticArc(SVG).FLarge;
    FSweep := TSVGPathEllipticArc(SVG).FSweep;
  end;
end;

function TSVGPathEllipticArc.GetBounds: TFRect;
begin
  Result.Left := Min(FStartX, FStopX);
  Result.Top := Min(FStartY, FStopY);
  Result.Width := Abs(FStartX - FStopX);
  Result.Height := Abs(FStartY - FStopY);
end;

function TSVGPathEllipticArc.New(Parent: TSVGObject): TSVGObject;
begin
  Result := TSVGPathEllipticArc.Create(Parent);
end;

procedure TSVGPathEllipticArc.AddToPath(Path: TArrayOfArrayOfFloatPoint);
var
  R: TFloatRect;
  X1, Y1, X2, Y2: TFloat;
  Xl, Yl: TFloat;
  Phi: TFloat;
  Factor: TFloat;
  CXl, CYl: TFloat;
  CenterX, CenterY: TFloat;
  StartAngle, SweepAngle: TFloat;
  A, B, C, D: TFloat;
begin
  if (FStartX = FStopX) and (FStartY = FStopY) then
    Exit;

  if (FRX = 0) or (FRY = 0) then
  begin
    //x2niePath.AddLine(FStartX, FStartY, FStopX, FStopY);
    Exit;
  end;

  X1 := FStartX;
  Y1 := FStartY;
  X2 := FStopX;
  Y2 := FStopY;

  Phi := DegToRad(-FXRot);

  Xl := (X1 - X2) / 2;
  Yl := (Y1 - Y2) / 2;
  
  Xl :=  Cos(Phi) * Xl + Sin(Phi) * Yl;
  Yl := -Sin(Phi) * Xl + Cos(Phi) * Yl;

  A := Sqr(Xl) / Sqr(FRX) + Sqr(Yl) / Sqr(FRY);
  if A > 1 then
  begin
    A := Sqrt(A);
    FRX := A * FRX;
    FRY := A * FRY;
  end;


  A := Sqr(FRX) * Sqr(FRY) - Sqr(FRX) * Sqr(Yl) - Sqr(FRY) * Sqr(Xl);
  B := Sqr(FRX) * Sqr(Yl) + Sqr(FRY) * Sqr(Xl);
  A := A / B;
  if A > 0 then
    Factor := Sqrt(A)
  else
    Factor := 1;

  if FLarge = FSweep then
    Factor := -Factor;

  CXl := Factor * (FRX * Yl) / FRY;
  CYl := Factor * -(FRY * Xl) / FRX;

  CenterX := Cos(Phi) * CXl -Sin(Phi) * CYl + (X1 + X2) / 2;
  CenterY := Sin(Phi) * CXl + Cos(Phi) * CYl + (Y1 + Y2) / 2;

  R.Left := CenterX - FRX;
  R.Top := CenterY - FRY;
  R.Right := FRX * 2;
  R.Bottom := FRY * 2;

  A := 1;
  B := 0;
  C := (Xl - CXl) / FRX;
  D := (Yl - CYl) / FRY;

  StartAngle := (A * C + B * D) / (Sqrt(Sqr(A) + Sqr(B)) * Sqrt(Sqr(C) + Sqr(D)));

  StartAngle := ArcCos(StartAngle);
  StartAngle := RadToDeg(StartAngle);

  A := C;
  B := D;
  C := (-Xl - CXl) / FRX;
  D := (-Yl - CYl) / FRY;
  SweepAngle := (A * C + B * D) / (Sqrt(Sqr(A) + Sqr(B)) * Sqrt(Sqr(C) + Sqr(D)));
  SweepAngle := ArcCos(SweepAngle);
  SweepAngle := RadToDeg(SweepAngle);

  if FSweep = 0 then
    SweepAngle := -SweepAngle;

  if FLarge = 0 then
    StartAngle := -StartAngle;

  if (FLarge = 1) then
  begin
    if Sweep = 0  then
      StartAngle := -StartAngle;
    if SweepAngle < 0 then
      SweepAngle := -360 - SweepAngle
    else
      SweepAngle := 360 - SweepAngle;
  end;

  //x2niePath.AddArc(R, StartAngle, SweepAngle);
end;

procedure TSVGPathEllipticArc.Read(SL: TStrings; var Position: Integer;
  Previous: TSVGPathElement);
var
  Command: AnsiString;
begin
  inherited;

  Command := SL[Position];
  if (Command = 'A') or (Command = 'a') then
  begin
    TryStrToTFloat(SL[Position + 1], FRX);
    TryStrToTFloat(SL[Position + 2], FRY);
    TryStrToTFloat(SL[Position + 3], FXRot);
    TryStrToInt(SL[Position + 4], FLarge);
    TryStrToInt(SL[Position + 5], FSweep);
    TryStrToTFloat(SL[Position + 6], FStopX);
    TryStrToTFloat(SL[Position + 7], FStopY);
    Inc(Position, 7);

    FRX := Abs(FRX);
    FRY := Abs(FRY);

    if FLarge <> 0 then
      FLarge := 1;

    if FSweep <> 0 then
      FSweep := 1;

    if Command = 'a' then
    begin
      FStopX := FStartX + FStopX;
      FStopY := FStartY + FStopY;
    end;
  end;
end;

// TSVGPathClose

function TSVGPathClose.GetBounds: TFRect;
begin
  Result.Width := 0;
  Result.Height := 0;
end;

function TSVGPathClose.New(Parent: TSVGObject): TSVGObject;
begin
  Result := TSVGPathClose.Create(Parent);
end;

procedure TSVGPathClose.Read(SL: TStrings; var Position: Integer;
  Previous: TSVGPathElement);
begin
  FStartX := Previous.FStopX;
  FStartY := Previous.FStopY;
  FStopX := FStartX;
  FStopY := FStartY;
end;

procedure TSVGPathClose.AddToPath(Path: TArrayOfArrayOfFloatPoint);
begin
  //x2niePath.CloseFigure;
end;

end.
