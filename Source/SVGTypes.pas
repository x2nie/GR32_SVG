      {******************************************************************}
      { SVG types                                                        }
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

unit SVGTypes;

interface

uses
  Windows, Math,
  GR32, GR32_Blend, GR32_Transforms, GR32_Polygons;
  //GDIPAPI;

const
  INHERIT = -1;

  FontNormal = 0;
  FontItalic = 1;

  MaxTFloat = MaxSingle;

type
  TFloat = Single;

  {TFPoint = record
    X, Y: TFloat;
  end;}

  TFRect = record
    Left, Top,
    Width, Height: TFloat;
  end;

  {TListOfPoints = array of TFPoint;
  PListOfPoints = ^TListOfPoints;}

  TSingleA = array of Single;
  PSingleA = ^TSingleA;

  TRectarray = packed array of TRect;
  PRectArray = ^TRectArray;

  TTextDecoration = set of (tdInherit, tdUnderLine, tdOverLine, tdStrikeOut);

  TTextPathMethod = (tpmAlign, tpmStretch);

  TTextPathSpacing = (tpsAuto, tpsExact);

  TSVGUnit = (suNone, suPX, suPT, suPC, suMM, suCM, suIN, suEM, suEX, suPercent);

  TGradientUnits = (guObjectBoundingBox, guUserSpaceOnUse);

  TBounds = record
    TopLeft: TFloatPoint;
    TopRight: TFloatPoint;
    BottomLeft: TFloatPoint;
    BottomRight: TFloatPoint;
  end;

  //I don't know how to fill polygon with single color
  TSolidPoligonFiller = class(TCustomPolygonFiller)
  private
    FColor: TColor32;
  protected
    function GetFillLine: TFillLineEvent; override;
    procedure FillLineBlend(Dst: PColor32; DstX, DstY, Length: Integer; AlphaValues: PColor32);
  public
    property Color : TColor32 read FColor write FColor;
  published

  end;


function GetSVGTransformation(M: TFloatMatrix) : TAffineTransformation;

//function ToGPPoint(const Point: TFPoint): TGPPointF;

function Intersect(const Bounds: TBounds; const Rect: TRect): Boolean;


implementation

{function ToGPPoint(const Point: TFPoint): TGPPointF;
begin
  Result := MakePoint(Point.X, Point.Y);
end;}
type
  TAffineTransformationAccess = class(TAffineTransformation);

function GetSVGTransformation(M: TFloatMatrix) : TAffineTransformation;
begin
  Result := TAffineTransformation.Create;
  with TAffineTransformationAccess(Result) do
  begin
    FMatrix := M;
    Changed;
  end;
end;  

function Intersect(const Bounds: TBounds; const Rect: TRect): Boolean;
var
  R1, R2: THandle;
  P: array[0..3] of TPoint;
begin
  P[0].X := Round(Bounds.TopLeft.X);
  P[0].Y := Round(Bounds.TopLeft.Y);

  P[1].X := Round(Bounds.TopRight.X);
  P[1].Y := Round(Bounds.TopRight.Y);

  P[2].X := Round(Bounds.BottomRight.X);
  P[2].Y := Round(Bounds.BottomRight.Y);

  P[3].X := Round(Bounds.BottomLeft.X);
  P[3].Y := Round(Bounds.BottomLeft.Y);

  R1 := CreatePolygonRgn(P, 4, ALTERNATE);
  R2 := CreateRectRgn(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);

  Result := CombineRgn(R1, R1, R2, RGN_AND) <> NULLREGION;

  DeleteObject(R1);
  DeleteObject(R2);
end;

{ TSolidPoligonFiller }

procedure TSolidPoligonFiller.FillLineBlend(Dst: PColor32; DstX, DstY,
  Length: Integer; AlphaValues: PColor32);
var
  X: Integer;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[cmBlend]^;
  for X := DstX to DstX + Length - 1 do
  begin
    BlendMemEx(FColor, Dst^, AlphaValues^);
    Inc(Dst);
    Inc(AlphaValues);
  end;
  EMMS;
end;

function TSolidPoligonFiller.GetFillLine: TFillLineEvent;
begin
  Result := FillLineBlend;
end;

end.
