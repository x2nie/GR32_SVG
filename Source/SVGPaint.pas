      {******************************************************************}
      { SVG fill classes                                                 }
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

unit SVGPaint;

interface

uses
  Windows, Classes, Graphics,
  //MSXML2_TLB_Light,
  OEncoding, OWideSupp, OTextReadWrite, OXmlReadWrite, OXmlUtils,
  OXmlCDOM, OXmlPDOM, OXmlSAX, OXmlSeq, OXmlSerialize,

  //GDIPOBJ, GDIPAPI,
  GR32, GR32_Paths, GR32_Transforms, GR32_Polygons,
  SVGTypes, SVG;

type
  TColors = record
    Colors: packed array of Cardinal;// ARGB;
    Positions: packed array of Single;
    Count: Integer;
  end;

  TSVGStop = class(TSVGObject)
  private
    FStop: TFloat;
    FStopColor: TColor;
    FOpacity: TFloat;

  protected
    function New(Parent: TSVGObject): TSVGObject; override;
    procedure Assign(SVG: TSVGObject); override;
  public
    procedure ReadIn(const Node: PXMLNode); override;
    procedure PaintToGraphics(Graphics: TBitmap32); override;
    procedure PaintToPath(Path: TFlattenedPath); override;

    property Stop: TFloat read FStop write FStop;
    property StopColor: TColor read FStopColor write FStopColor;

    property Opacity: TFloat read FOpacity write FOpacity;
  end;

  TSVGFiller = class(TSVGMatrix)
  private
  protected
    function New(Parent: TSVGObject): TSVGObject; override;
  public
    procedure ReadIn(const Node: PXMLNode); override;
    function GetBrush(Alpha: Byte; const DestObject: TSVGBasic): {TGPBrush} TCustomPolygonFiller; virtual; abstract;
    procedure PaintToGraphics(Graphics: TBitmap32); override;
    procedure PaintToPath(Path: TFlattenedPath); override;
  end;

  TSVGGradient = class(TSVGFiller)
  private
    FURI: WideString;
    FGradientUnits: TGradientUnits;
  protected
    function GetColors(Alpha: Byte): TColors; virtual;
  public
    procedure ReadIn(const Node: PXMLNode); override;
  end;

  TSVGLinearGradient = class(TSVGGradient)
  private
    FX1: TFloat;
    FY1: TFloat;
    FX2: TFloat;
    FY2: TFloat;
  protected
    function New(Parent: TSVGObject): TSVGObject; override;
    procedure Assign(SVG: TSVGObject); override;
  public
    procedure ReadIn(const Node: PXMLNode); override;
    function GetBrush(Alpha: Byte; const DestObject: TSVGBasic): TCustomPolygonFiller; override;

    property X1: TFloat read FX1 write FX1;
    property Y1: TFloat read FY1 write FY1;
    property X2: TFloat read FX2 write FX2;
    property Y2: TFloat read FY2 write FY2;
  end;

  TSVGRadialGradient = class(TSVGGradient)
  private
    FCX: TFloat;
    FCY: TFloat;
    FR: TFloat;
    FFX: TFloat;
    FFY: TFloat;
  protected
    function New(Parent: TSVGObject): TSVGObject; override;
    procedure Assign(SVG: TSVGObject); override;
  public
    procedure Clear; override;
    procedure ReadIn(const Node: PXMLNode); override;
    function GetBrush(Alpha: Byte; const DestObject: TSVGBasic): TCustomPolygonFiller; override;

    property CX: TFloat read FCX write FCX;
    property CY: TFloat read FCY write FCY;
    property R: TFloat read FR write FR;
    property FX: TFloat read FFX write FFX;
    property FY: TFloat read FFY write FFY;
  end;


implementation

uses
  SysUtils,
  //Matrix,
  GR32_ColorGradients,
  SVGParse, SVGStyle, SVGProperties, SVGColor;

// TSVGStop

procedure TSVGStop.PaintToPath(Path: TFlattenedPath);
begin
end;

procedure TSVGStop.ReadIn(const Node: PXMLNode);
var
  S: WideString;
begin
  inherited;
  LoadPercent(Node, 'offset', FStop);

  LoadString(Node, 'stop-color', S);
  FStopColor := GetColor(S);

  if FStopColor = INHERIT then
  begin
    S := Style['stop-color'];
    FStopColor := GetColor(S);
  end;

  S := Style['stop-opacity'];
  if (S <> '') then
    FOpacity := ParsePercent(S)
  else
    FOpacity := 1;

  if (FOpacity < 0) then
    FOpacity := 0;

  if (FOpacity > 1) then
    FOpacity := 1;
end;

procedure TSVGStop.Assign(SVG: TSVGObject);
begin
  inherited;
  if SVG is TSVGStop then
  begin
    FStop := TSVGStop(SVG).FStop;
    FStopColor := TSVGStop(SVG).FStopColor;
  end;
end;

function TSVGStop.New(Parent: TSVGObject): TSVGObject;
begin
  Result := TSVGStop.Create(Parent);
end;

procedure TSVGStop.PaintToGraphics(Graphics: TBitmap32);
begin
end;

// TSVGFiller

procedure TSVGFiller.PaintToPath(Path: TFlattenedPath);
begin
end;           

procedure TSVGFiller.ReadIn(const Node: PXMLNode);
begin
  inherited;
  Display := 0;
end;

function TSVGFiller.New(Parent: TSVGObject): TSVGObject;
begin
  Result := nil;
end;

procedure TSVGFiller.PaintToGraphics(Graphics: TBitmap32);
begin
end;

// TSVGGradient

procedure TSVGGradient.ReadIn(const Node: PXMLNode);
var
  C: Integer;
  Stop: TSVGStop;
begin
  inherited;

  LoadGradientUnits(Node, FGradientUnits);

  for C := 0 to Node.childNodes.Count - 1 do
   if Node.childNodes[C].nodeName = 'stop' then
   begin
     Stop := TSVGStop.Create(Self);
     Stop.ReadIn(Node.childNodes[C]);
   end;

  FURI := Style['xlink:href'];
  if FURI <> '' then
  begin
    FURI := Trim(FURI);
    if (FURI <> '') and (FURI[1] = '#') then
      FURI := Copy(FURI, 2, MaxInt);
  end;
end;

// TSVGLinearGradient

function TSVGLinearGradient.New(Parent: TSVGObject): TSVGObject;
begin
  Result := TSVGLinearGradient.Create(Parent);
end;

procedure TSVGLinearGradient.ReadIn(const Node: PXMLNode);
var
  Matrix: TFloatMatrix;
begin
  inherited;
  LoadLength(Node, 'x1', FX1);
  LoadLength(Node, 'y1', FY1);
  LoadLength(Node, 'x2', FX2);
  LoadLength(Node, 'y2', FY2);

  FillChar(Matrix, SizeOf(Matrix), 0);
  LoadTransform(Node, 'gradientTransform', Matrix);
  PureMatrix := Matrix;
end;

procedure TSVGLinearGradient.Assign(SVG: TSVGObject);
begin
  inherited;
  if SVG is TSVGLinearGradient then
  begin
    FX1 := TSVGLinearGradient(SVG).FX1;
    FY1 := TSVGLinearGradient(SVG).FY1;
    FX2 := TSVGLinearGradient(SVG).FX2;
    FY2 := TSVGLinearGradient(SVG).FY2;
  end;
end;


function TSVGLinearGradient.GetBrush(Alpha: Byte; const DestObject: TSVGBasic): TCustomPolygonFiller;
var
  //Sampler: TLinearGradientSampler;

  //TGP: TGPMatrix;
  Colors: TColors;
  Gradient : TColor32Gradient;
  GradientLUT : TColor32LookupTable;
  LinearGradFiller: TCustomLinearGradientPolygonFiller;

  i : Integer;
  R : TFloatRect;
  T : TAffineTransformation;
  M : TFloatMatrix;
begin
  if not Assigned(DestObject) then
    M := IdentityMatrix
  else
    M := DestObject.Matrix;

  if Assigned(DestObject) and (FGradientUnits = guObjectBoundingBox) then
    R := FloatRect(DestObject.X, DestObject.Y, DestObject.X + DestObject.Width, DestObject.Y + DestObject.Height)
  else
    R := FloatRect(FX1, FY1, FX2, FY2);

  if (PureMatrix[2, 2] <> 0 ) {and false} then
  begin
    M := Mult(M,PureMatrix);
  end;

  T := SVGTypes.GetSVGTransformation(M);
  try
    R.TopLeft := T.Transform(R.TopLeft);
    R.BottomRight := T.Transform(R.BottomRight);
  finally
    T.Free;
  end;



  Colors := GetColors(Alpha);

  GradientLUT := TColor32LookupTable.Create;
  try
    Gradient := TColor32Gradient.Create;
    try
      for i := 0 to Colors.Count -1 do
      begin
        Gradient.AddColorStop(Colors.Positions[i], Colors.Colors[i]);
      end;
      Gradient.FillColorLookUpTable(GradientLUT);
    finally
      Gradient.Free;
    end;

    LinearGradFiller := TLinearGradientPolygonFiller.Create(GradientLUT);
    //LinearGradFiller.GradientLUT := GradientLUT;
    LinearGradFiller.StartPoint := R.TopLeft;
    LinearGradFiller.EndPoint := R.BottomRight;
    LinearGradFiller.WrapMode := wmClamp;


    //Brush.SetInterpolationColors(PGPColor(Colors.Colors),
      //PSingle(Colors.Positions), Colors.Count);
  finally
    //GradientLUT.Free;
  end;

  Finalize(Colors);

  {if PureMatrix.Cells[2, 2] = 1 then
  begin
    TGP := GetGPMatrix(PureMatrix);
    Brush.SetTransform(TGP);
    TGP.Free;
  end;

  Result := Brush;
  }
  Result := LinearGradFiller;
end;

// TSVGRadialGradient

procedure TSVGRadialGradient.Assign(SVG: TSVGObject);
begin
  inherited;
  if SVG is TSVGRadialGradient then
  begin
    FCX := TSVGRadialGradient(SVG).FCX;
    FCY := TSVGRadialGradient(SVG).FCY;
    FFX := TSVGRadialGradient(SVG).FFX;
    FFY := TSVGRadialGradient(SVG).FFY;
    FR := TSVGRadialGradient(SVG).FR;
  end;
end;

procedure TSVGRadialGradient.Clear;
begin
  inherited;

  FCX := 0.5;
  FCY := 0.5;
  FR := 0.5;
  FFX := FCX;
  FFY := FCY;
end;

procedure TSVGRadialGradient.ReadIn(const Node: PXMLNode);
begin
  inherited;

  LoadLength(Node, 'cx', FCX);
  LoadLength(Node, 'cy', FCY);
  LoadLength(Node, 'r', FR);
  LoadLength(Node, 'fx', FFX);
  LoadLength(Node, 'fy', FFY);
end;

function TSVGRadialGradient.GetBrush(Alpha: Byte; const DestObject: TSVGBasic): TCustomPolygonFiller;
{var
  Brush: TGPPathGradientBrush;
  Path: TArrayOfArrayOfFloatPoint;
  TGP: TGPMatrix;
  Colors: TColors;
begin
  Path := TArrayOfArrayOfFloatPoint.Create;

  if Assigned(DestObject) and (FGradientUnits = guObjectBoundingBox) then
    Path.AddEllipse(DestObject.X, DestObject.Y, DestObject.Width, DestObject.Height)
  else
    Path.AddEllipse(FCX - FR, FCY - FR, 2 * FR, 2 * FR);

  Brush := TGPPathGradientBrush.Create(Path);
  Path.Free;

  Colors := GetColors(Alpha);
  Brush.SetInterpolationColors(PARGB(Colors.Colors), PSingle(Colors.Positions), Colors.Count);

  Finalize(Colors);

  Brush.SetCenterPoint(MakePoint(FFX, FFY));

  if PureMatrix.Cells[2, 2] = 1 then
  begin
    TGP := GetGPMatrix(PureMatrix);
    Brush.SetTransform(TGP);
    TGP.Free;
  end;
  Result := Brush;}
{$DEFINE SVGGRADIENT}
var
  //Sampler: TLinearGradientSampler;

  //TGP: TGPMatrix;
  Colors: TColors;
  Gradient : TColor32Gradient;
  GradientLUT : TColor32LookupTable;
  {$IFDEF SVGGRADIENT}
  RadialGradFiller : TSVGRadialGradientPolygonFiller;
  {$ELSE}
  RadialGradFiller : TRadialGradientPolygonFiller;
  {$ENDIF}
  i : Integer;
  R : TFloatRect;
  F,C : TFloatPoint;
  T : TAffineTransformation;
  M : TFloatMatrix;
begin
  M := IdentityMatrix;
  if Assigned(DestObject) then
    M := DestObject.Matrix;

  if Assigned(DestObject) and (FGradientUnits = guObjectBoundingBox) then
    R := FloatRect(DestObject.X, DestObject.Y, DestObject.X + DestObject.Width, DestObject.Y + DestObject.Height)
  else
    R := FloatRect(FCX - FR, FCY - FR, FCX + FR, FCY + FR);


  Colors := GetColors(Alpha);

  GradientLUT := TColor32LookupTable.Create;
  try
    Gradient := TColor32Gradient.Create;
    try
      for i := 0 to Colors.Count -1 do
      begin
        Gradient.AddColorStop(Colors.Positions[i], Colors.Colors[i]);
      end;
      Gradient.FillColorLookUpTable(GradientLUT);
    finally
      Gradient.Free;
    end;

    F := FloatPoint(FFX, FFY);
    C := FloatPoint(CX,CY);

    if PureMatrix[2, 2] <> 0 then
    //if PureMatrix <> IdentityMatrix then
    begin
      M := Mult(M,PureMatrix);
    end;
    
    T := SVGTypes.GetSVGTransformation(M);
    try
      //T.SrcRect := FloatRect(-1,-1, 1, 1);
      R.TopLeft := T.Transform(R.TopLeft);
      R.BottomRight := T.Transform(R.BottomRight);
      F := T.Transform(F);
      C := T.Transform(C);
    finally
      T.Free;
    end;

    {$IFDEF SVGGRADIENT}
    RadialGradFiller := TSVGRadialGradientPolygonFiller.Create(GradientLUT);
    RadialGradFiller.WrapMode := wmClamp;
    RadialGradFiller.EllipseBounds := R;
    RadialGradFiller.FocalPoint := F;
    {$ELSE}
    RadialGradFiller := TRadialGradientPolygonFiller.Create(GradientLUT);
    RadialGradFiller.WrapMode := wmClamp;
    RadialGradFiller.EllipseBounds := R;
    RadialGradFiller.Center := C;
    {$ENDIF}

    //Brush.SetInterpolationColors(PGPColor(Colors.Colors),
      //PSingle(Colors.Positions), Colors.Count);
  finally
    //GradientLUT.Free;
  end;
  Finalize(Colors);

  {if PureMatrix.Cells[2, 2] = 1 then
  begin
    TGP := GetGPMatrix(PureMatrix);
    Brush.SetTransform(TGP);
    TGP.Free;
  end;

  Result := Brush;
  }
  Result := RadialGradFiller;
end;

function TSVGRadialGradient.New(Parent: TSVGObject): TSVGObject;
begin
  Result := TSVGRadialGradient.Create(Parent);
end;

function TSVGGradient.GetColors(Alpha: Byte): TColors;
var
  C, Start, ColorCount: Integer;
  Stop: TSVGStop;
  Item: TSVGGradient;
begin
  Result.Count := 0;
  if FURI = '' then
    Item := Self
  else
  begin
    Item := TSVGGradient(GetRoot.FindByID(FURI));
    if not (Item is TSVGGradient) then
      Exit;
  end;

  Start := 0;
  ColorCount := Item.Count;

  if Item.Count = 0 then
    Exit;

  if TSVGStop(Item.Items[ColorCount - 1]).FStop < 1 then
    Inc(ColorCount);

  if TSVGStop(Item.Items[0]).FStop > 0 then
  begin
    Inc(ColorCount);
    Inc(Start);
  end;

  SetLength(Result.Colors, ColorCount);
  SetLength(Result.Positions, ColorCount);

  if Start > 0 then
  begin
    Stop := TSVGStop(Item.Items[0]);
    Result.Colors[0] := ConvertColor(Stop.FStopColor, Round(Alpha * Stop.FOpacity));
    Result.Positions[0] := 0;
  end;

  for C := 0 to Item.Count - 1 do
  begin
    Stop := TSVGStop(Item.Items[C]);
    Result.Colors[C + Start] := ConvertColor(Stop.FStopColor, Round(Alpha * Stop.FOpacity));
    Result.Positions[C + Start] := Stop.FStop;
  end;

  if (ColorCount - Start) > Item.Count then
  begin
    Stop := TSVGStop(Item.Items[Item.Count - 1]);
    Result.Colors[ColorCount - 1] := ConvertColor(Stop.FStopColor, Round(Alpha * Stop.FOpacity));
    Result.Positions[ColorCount - 1] := 1;
  end;

  Result.Count := ColorCount;
end;

end.
