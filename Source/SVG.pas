      {******************************************************************}
      { SVG Image                                                        }
      {                                                                  }
      { home page : http://www.mwcs.de                                   }
      { email     : martin.walter@mwcs.de                                }
      {                                                                  }
      { date      : 22-09-2008                                           }
      {                                                                  }
      { version   : 0.69b                                                }
      {                                                                  }
      { Use of this file is permitted for commercial and non-commercial  }
      { use, as long as the author is credited.                          }
      { This file (c) 2005, 2008 Martin Walter                           }
      {                                                                  }
      { Thanks to:                                                       }
      { Bart Vandromme (parsing errors)                                  }
      { Chris Ueberall (parsing errors)                                  }
      { Elias Zurschmiede (font error)                                   }
      { Christopher Cerny  (Dash Pattern)                                }
      {                                                                  }
      { This Software is distributed on an "AS IS" basis, WITHOUT        }
      { WARRANTY OF ANY KIND, either express or implied.                 }
      {                                                                  }
      { *****************************************************************}

unit SVG;

{ $DEFINE USE_TEXT} // Define to use "real" text instead of paths

interface

uses
  Windows, Classes,
  Math,
  Graphics, ActiveX,
  //MSXML2_TLB_Light,
  OEncoding, OWideSupp, OTextReadWrite, OXmlReadWrite, OXmlUtils,
  OXmlCDOM, OXmlPDOM, OXmlSAX, OXmlSeq, OXmlSerialize,
  
  WideStringList,
  //GDIPOBJ, GDIPAPI, GDIPOBJ2, GDIPKerning, GDIPPathText,
  GR32, GR32_Paths, GR32_Transforms, GR32_Polygons,
  SVGTypes,
  //Matrix,
  SVGStyle;

type
  TSVG = class;

  TSVGObject = class(TObject)
  private
    FItems: TList;
    FVisible: Integer;
    FDisplay: Integer;
    FBounds: TBounds;
    FParent: TSVGObject;
    FStyle: TStyle;
    FID: WideString;
    FObjectName: WideString;
    FClasses: TWideStringList;

    function GetCount: Integer;
    procedure SetItem(Index: Integer; Item: TSVGObject);
    function GetItem(Index: Integer): TSVGObject;

    function GetDisplay: Integer;
    function GetObjectBounds: TBounds;
    function GetVisible: Integer;
  protected
    procedure Assign(SVG: TSVGObject); reintroduce; virtual;
    function New(Parent: TSVGObject): TSVGObject; virtual; abstract;
    procedure CalcObjectBounds; virtual;

    function GetRoot: TSVG;
    procedure ReadChildren(const Node: PXMLNode); virtual;
  public
    constructor Create; overload; virtual;
    constructor Create(Parent: TSVGObject); overload;
    destructor Destroy; override;
    procedure Clear; virtual;
    function Clone(Parent: TSVGObject): TSVGObject;
    function Add(Item: TSVGObject): Integer;
    procedure Delete(Index: Integer);
    function Remove(Item: TSVGObject): Integer;
    function IndexOf(Item: TSVGObject): Integer;
    function FindByID(const Name: WideString): TSVGObject;
    function FindByType(Typ: TClass; Previous: TSVGObject = nil): TSVGObject;
    procedure CalculateMatrices;

    procedure PaintToGraphics(Graphics: TBitmap32); virtual; abstract;
    procedure PaintToPath(Path: TFlattenedPath); virtual; abstract;
    procedure ReadIn(const Node: PXMLNode); virtual;

    property Items[Index: Integer]: TSVGObject read GetItem write SetItem; default;
    property Count: Integer read GetCount;

    property Display: Integer read GetDisplay write FDisplay;
    property Visible: Integer read GetVisible write FVisible;
    property ObjectBounds: TBounds read GetObjectBounds;
    property Parent: TSVGObject read FParent;
    property Style: TStyle read FStyle;
    property ID: WideString read FID;
    property ObjectName: WideString read FObjectName;
  end;

  TSVGMatrix = class(TSVGObject)
  private
    FMatrix: TFloatMatrix;
    FCompleteCalculatedMatrix: TFloatMatrix;
    FCalculatedMatrix: TFloatMatrix;
    procedure SetMatrix(const Value: TFloatMatrix); virtual;
    procedure CalcMatrix;
//    function TransformComplete(P: TFPoint): TFPoint; overload;
//    function TransformComplete(X, Y: TFloat): TFPoint; overload;

    function Transform(P: TFloatPoint): TFloatPoint; overload;
    function Transform(X, Y: TFloat): TFloatPoint; overload;
  protected
    procedure Assign(SVG: TSVGObject); override;
    //function New(Parent: TSVGObject): TSVGObject; override;
  public
    procedure Clear; override;
    procedure ReadIn(const Node: PXMLNode); override;
    property Matrix: TFloatMatrix read FCompleteCalculatedMatrix;
    property PureMatrix: TFloatMatrix read FMatrix write SetMatrix;
  end;

  TSVGBasic = class(TSVGMatrix)
  private
    FFillColor: Integer;
    FStrokeColor: Integer;
    FFillOpacity: TFloat;
    FStrokeOpacity: TFloat;
    FStrokeWidth: TFloat;
    FStrokeLineJoin: WideString;
    FStrokeLineCap: WideString;
    FStrokeMiterLimit: TFloat;
    FStrokeDashOffset: TFloat;
    FStrokeDashArray: TSingleA;
    FStrokeDashArrayCount: Integer;
    FArrayNone: Boolean;

    FFontName: WideString;
    FFontSize: TFloat;
    FFontWeight: Integer;
    FFontStyle: Integer;
    FTextDecoration: TTextDecoration;

    FPath: TFlattenedPath;
    FClipPath: TFlattenedPath;
    FX: TFloat;
    FY: TFloat;
    FWidth: TFloat;
    FHeight: TFloat;

    function IsFontAvailable: Boolean;
    //procedure ReadChildren(const Node: PXMLNode); virtual;
    procedure SetStrokeDashArray(const S: WideString);
    procedure SetClipURI(const Value: WideString);

    function GetFillColor: Integer;
    function GetStrokeColor: Integer;
    function GetFillOpacity: TFloat;
    function GetStrokeOpacity: TFloat;
    function GetStrokeWidth: TFloat;
    function GetClipURI: WideString;
    function GetStrokeLineCap: TEndStyle;
    function GetStrokeLineJoin: TJoinStyle;
    function GetStrokeMiterLimit: TFloat;
    function GetStrokeDashOffset: TFloat;
    function GetStrokeDashArray(var Count: Integer): PSingle;

    function GetFontName: WideString;
    function GetFontWeight: Integer;
    function GetFontSize: TFloat;
    function GetFontStyle: Integer;
    function GetTextDecoration: TTextDecoration;
    procedure ParseFontWeight(const S: WideString);
  protected
    FRX: TFloat;
    FRY: TFloat;
    FFillURI: WideString;
    FStrokeURI: WideString;
    FClipURI: WideString;
    FLineWidth: TFloat;
    FFillRule: Integer;
    FColorInterpolation: TFloat;
    FColorRendering: TFloat;

    procedure Assign(SVG: TSVGObject); override;
    function New(Parent: TSVGObject): TSVGObject; override;
    procedure ReadStyle(Style: TStyle); virtual;
    procedure ConstructPath; virtual;
    function GetClipPath: TFlattenedPath;
    procedure CalcClipPath;
    function GetBrush(AURI: WideString): TCustomPolygonFiller;
    function GetFillBrush: {TGPBrush} TCustomPolygonFiller;
    function GetStrokeBrush: TCustomPolygonFiller;
    {$IFDEF GPPen}
    function GetStrokePen(const StrokeBrush: TGPBrush): TGPPen;

    procedure BeforePaint(const Graphics: TBitmap32; const Brush: TGPBrush;
      const Pen: TGPPen); virtual;
    procedure AfterPaint(const Graphics: TBitmap32; const Brush: TGPBrush;
      const Pen: TGPPen); virtual;
    {$ENDIF}
  public
    constructor Create; override;
    procedure Clear; override;
    procedure PaintToGraphics(Graphics: TBitmap32); override;

    procedure PaintToPath(Path: TFlattenedPath); override;
    procedure ReadIn(const Node: PXMLNode); override;

    property Root: TSVG read GetRoot;

    property FillColor: Integer read GetFillColor write FFillColor;
    property StrokeColor: Integer read GetStrokeColor write FStrokeColor;
    property FillOpacity: TFloat read GetFillOpacity write FFillOpacity;
    property StrokeOpacity: TFloat read GetStrokeOpacity write FStrokeOpacity;
    property StrokeWidth: TFloat read GetStrokeWidth write FStrokeWidth;
    property ClipURI: WideString read GetClipURI write SetClipURI;
    property FillURI: WideString read FFillURI write FFillURI;
    property StrokeURI: WideSTring read FStrokeURI write FStrokeURI;
    property X: TFloat read FX write FX;
    property Y: TFloat read FY write FY;
    property Width: TFloat read FWidth write FWidth;
    property Height: TFloat read FHeight write FHeight;
    property RX: TFloat read FRX write FRX;
    property RY: TFloat read FRY write FRY;

    property StrokeLineCap: TEndStyle read GetStrokeLineCap;
    property StrokeLineJoin: TJoinStyle read GetStrokeLineJoin;
    property StrokeMiterLimit: TFloat read GetStrokeMiterLimit write FStrokeMiterLimit;
    property StrokeDashOffset: TFloat read GetStrokeDashOffset write FStrokeDashOffset;

    property FontName: WideString read GetFontName write FFontName;
    property FontSize: TFloat read GetFontSize write FFontSize;
    property FontWeight: Integer read GetFontWeight write FFontWeight;
    property FontStyle: Integer read GetFontStyle write FFontStyle;
    property TextDecoration: TTextDecoration read GetTextDecoration write FTextDecoration;
  end;

  TSVG = class(TSVGBasic)
  private
    FDC: HDC;
    FRootBounds: TFloatRect;
    FDX, FDY: TFloat;
    FStyles: TStyleList;
    FInitialMatrix: TFloatMatrix;
    FSource: WideString;
    FAngle: TFloat;
    FAngleMatrix: TFloatMatrix;
    FRootMatrix: TFloatMatrix;
    FViewBox: TFRect;
    FFileName: WideString;
    FSize: TFloatRect;
    
    procedure SetViewBox(const Value: TFRect);

    procedure SetSVGOpacity(Opacity: TFloat);
    procedure SetAngle(Angle: TFloat);
    procedure Paint(const Graphics: TBitmap32; Rects: PRectArray;
      RectCount: Integer);
    procedure CalcRootMatrix;
    procedure CalcCompleteSize;
  protected
    procedure Assign(SVG: TSVGObject); override;
    function New(Parent: TSVGObject): TSVGObject; override;
    procedure ReadStyles(const Node: PXMLNode);
    property RootMatrix: TFloatMatrix read FRootMatrix;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear; override;
    procedure ReadIn(const Node: PXMLNode); override;

    procedure DeReferenceUse;
    function GetStyleValue(const Name, Key: WideString): WideString;

    procedure LoadFromText(const Text: WideString);
    procedure LoadFromFile(const FileName: WideString);
    procedure LoadFromStream(Stream: TStream); overload;
    procedure LoadFromStream(const Stream: IStream); overload;
    procedure SaveToFile(const FileName: WideString);
    procedure SaveToStream(Stream: TStream);

    function SaveToNode(const Parent: PXMLNode;
      Left, Top, Width, Height: TFloat): PXMLNode;

    procedure SetBounds(const Bounds: TFloatRect);
    procedure Scale(DX: TFloat; DY: TFloat = -1);
    procedure PaintTo(DC: HDC; Bounds: TFloatRect;
      Rects: PRectArray; RectCount: Integer); overload;
    {$IFDEF GPMetaFile}
    procedure PaintTo(MetaFile: TGPMetaFile; Bounds: TFloatRect;
      Rects: PRectArray; RectCount: Integer); overload;
    {$ENDIF}
    procedure PaintTo(Graphics: TBitmap32; Bounds: TFloatRect;
      Rects: PRectArray; RectCount: Integer); overload;
    procedure PaintTo(Bitmap: TBitmap; Bounds: TFloatRect;
      Rects: PRectArray; RectCount: Integer); overload;

    //x2niefunction RenderToIcon(Size: Integer): HICON;
    //x2niefunction RenderToBitmap(Width, Height: Integer): HBITMAP;

    property InitialMatrix: TFloatMatrix read FInitialMatrix write FInitialMatrix;
    property SVGOpacity: TFloat write SetSVGOpacity;
    property Source: WideString read FSource;
    property Angle: TFloat read FAngle write SetAngle;
    property ViewBox: TFRect read FViewBox write SetViewBox;
  end;

  TSVGSymbol = class(TSVGBasic)
  private
    FViewBox: TFRect;
    procedure SetViewBox(const Value: TFRect);
  protected
    function New(Parent: TSVGObject): TSVGObject; override;
  public
    procedure ReadIn(const Node: PXMLNode); override;
    property ViewBox: TFRect read FViewBox write SetViewBox;    
  end;


  TSVGContainer = class(TSVGBasic)
  protected
    function New(Parent: TSVGObject): TSVGObject; override;
  public
    procedure ReadIn(const Node: PXMLNode); override;
  end;

  TSVGSwitch = class(TSVGBasic)
  protected
    function New(Parent: TSVGObject): TSVGObject; override;
  public
    procedure ReadIn(const Node: PXMLNode); override;
  end;

  TSVGDefs = class(TSVGBasic)
  protected
    function New(Parent: TSVGObject): TSVGObject; override;
  public
    procedure ReadIn(const Node: PXMLNode); override;
  end;

  TSVGUse = class(TSVGBasic)
  private
    FReference: WideString;
  protected
    procedure Assign(SVG: TSVGObject); override;
    function New(Parent: TSVGObject): TSVGObject; override;
    procedure Construct;
  public
    procedure PaintToPath(Path: TFlattenedPath); override;
    procedure PaintToGraphics(Graphics: TBitmap32); override;
    procedure Clear; override;
    procedure ReadIn(const Node: PXMLNode); override;
  end;

  TSVGRect = class(TSVGBasic)
  protected
    function New(Parent: TSVGObject): TSVGObject; override;
  protected
    procedure ConstructPath; override;
    procedure CalcObjectBounds; override;
  public
    procedure ReadIn(const Node: PXMLNode); override;
  end;

  TSVGLine = class(TSVGBasic)
  private
  protected
    function New(Parent: TSVGObject): TSVGObject; override;
    procedure ConstructPath; override;
    procedure CalcObjectBounds; override;
  public
    procedure ReadIn(const Node: PXMLNode); override;
  end;

  TSVGPolyLine = class(TSVGBasic)
  private
    FPoints: TArrayOfFloatPoint;
    FPointCount: Integer;
    function GetPoints: PArrayOfFloatPoint;
    procedure ConstructPoints(const S: WideString);
  protected
    procedure Assign(SVG: TSVGObject); override;
    function New(Parent: TSVGObject): TSVGObject; override;
    procedure ConstructPath; override;
    procedure CalcObjectBounds; override;
  public
    constructor Create; override;
    procedure Clear; override;
    procedure ReadIn(const Node: PXMLNode); override;

    property Points: PArrayOfFloatPoint read GetPoints;
  end;

  TSVGPolygon = class(TSVGPolyLine)
  private
  protected
    function New(Parent: TSVGObject): TSVGObject; override;
    procedure ConstructPath; override;
  public
  end;

  TSVGEllipse = class(TSVGBasic)
  private
  protected
    function New(Parent: TSVGObject): TSVGObject; override;
    procedure ConstructPath; override;
    procedure CalcObjectBounds; override;
  public
    procedure ReadIn(const Node: PXMLNode); override;
  end;

  TSVGPath = class(TSVGBasic)
  private
    procedure PrepareMoveLineCurveArc(SL: TStringList);
    function SeparateValues(const S: WideString): TStringList;
    function Split(const S: WideString): TStringList;
  protected
    function New(Parent: TSVGObject): TSVGObject; override;
    procedure ConstructPath; override;
    procedure CalcObjectBounds; override;
  public
    procedure ReadIn(const Node: PXMLNode); override;
  end;

  TSVGImage = class(TSVGBasic)
  private
    FFileName: WideString;
    {$IFDEF GPPen}FImage: TGPImage;{$ENDIF}
    FStream: TMemoryStream;
  protected
    procedure Assign(SVG: TSVGObject); override;
    function New(Parent: TSVGObject): TSVGObject; override;
    procedure CalcObjectBounds; override;
  public
    constructor Create; override;
    procedure Clear; override;
    procedure PaintToGraphics(Graphics: TBitmap32); override;
    procedure ReadIn(const Node: PXMLNode); override;
    property Data: TMemoryStream read FStream;
  end;
  {$IFDEF GPTEXT}
  TSVGCustomText = class(TSVGBasic)
  private
    FText: WideString;
    FUnderlinePath: TArrayOfArrayOfFloatPoint;
    FStrikeOutPath: TArrayOfArrayOfFloatPoint;

    FFontHeight: TFloat;
    FDX: TFloat;
    FDY: TFloat;

    FHasX: Boolean;
    FHasY: Boolean;

    function GetCompleteWidth: TFloat;
    procedure SetSize; virtual;
    function GetFont: TFont;
    {$IFDEF GPPen}
    function GetFontFamily(const FontName: WideString): TFontFamily;
    {$ENDIF}

    function IsInTextPath: Boolean;
  protected
    procedure Assign(SVG: TSVGObject); override;
    function New(Parent: TSVGObject): TSVGObject; override;
    procedure ConstructPath; override;
    procedure ParseNode(const Node: PXMLNode); virtual;
    procedure CalcObjectBounds; override;
    {$IFDEF GPPen}
    procedure BeforePaint(const Graphics: TBitmap32; const Brush: TGPBrush;
      const Pen: TGPPen); override;
    procedure AfterPaint(const Graphics: TBitmap32; const Brush: TGPBrush;
      const Pen: TGPPen); override;
    {$ENDIF}

    procedure ReadTextNodes(const Node: PXMLNode); virtual;
  public
    constructor Create; override;
    procedure Clear; override;
    procedure ReadIn(const Node: PXMLNode); override;
    procedure PaintToGraphics(Graphics: TBitmap32); override;

    property DX: TFloat read FDX write FDX;
    property DY: TFloat read FDY write FDY;
    property FontHeight: TFloat read FFontHeight write FFontHeight;
    property Text: WideString read FText write FText;
  end;

  TSVGText = class(TSVGCustomText)
  public
    procedure ReadIn(const Node: PXMLNode); override;
  end;

  TSVGTSpan = class(TSVGText)
  private
  protected
    procedure ReadTextNodes(const Node: PXMLNode); override;
  public
  end;

  TSVGTextPath = class(TSVGCustomText)
  private
    FOffset: TFloat;
    FOffsetIsPercent: Boolean;
    FPathRef: WideString;
    FMethod: TTextPathMethod;
    FSpacing: TTextPathSpacing;
  protected
    procedure ConstructPath; override;
    procedure ReadTextNodes(const Node: PXMLNode); override;
  public
    procedure Clear; override;
    procedure ReadIn(const Node: PXMLNode); override;
    procedure PaintToGraphics(Graphics: TBitmap32); override;
  end;
  {$ENDIF}
  TSVGClipPath = class(TSVGBasic)
  private
    FClipPath: TFlattenedPath;
  protected
    function New(Parent: TSVGObject): TSVGObject; override;
    procedure ConstructClipPath;
  public
    destructor Destroy; override;
    procedure Clear; override;
    procedure PaintToPath(Path: TFlattenedPath); override;
    procedure PaintToGraphics(Graphics: TBitmap32); override;
    procedure ReadIn(const Node: PXMLNode); override;
    function GetClipPath: TFlattenedPath;
  end;

implementation

uses
  SysUtils,
  //XMLHelp,
  //GDIPUtils,
  GR32_Clipper,

  SVGParse, SVGProperties, SVGColor, SVGPaint, SVGPath, SVGCommon;

// TSVGObject

constructor TSVGObject.Create;
begin
  inherited;
  FParent := nil;
  FStyle := TStyle.Create;
  FItems := TList.Create;
  FClasses := TWideStringList.Create;
  FClasses.Delimiter := ' ';
  Clear;
end;

constructor TSVGObject.Create(Parent: TSVGObject);
begin
  Create;
  if Assigned(Parent) then
    Parent.Add(Self);
end;

destructor TSVGObject.Destroy;
begin
  Clear;
  FItems.Free;

  if Assigned(FParent) then
  try
    FParent.Remove(Self);
  except
  end;

  FStyle.Free;
  FClasses.Free;

  inherited;
end;

procedure TSVGObject.CalcObjectBounds;
begin
end;

procedure TSVGObject.CalculateMatrices;
var
  C: Integer;
begin
  if Self is TSVGMatrix then
  begin
    if Self is TSVG then
      TSVG(Self).CalcRootMatrix
    else
      TSVGMatrix(Self).CalcMatrix;

    if Self is TSVGBasic then
      TSVGBasic(Self).CalcClipPath;

    CalcObjectBounds;
  end;

  for C := 0 to FItems.Count - 1 do
    TSVGObject(FItems[C]).CalculateMatrices;
end;

procedure TSVGObject.Clear;
begin
  while Count > 0 do
    Items[0].Free;

  Visible := 1;
  Display := 1;
  FID := '';

  FClasses.Clear;
  FStyle.Clear;
  FObjectName := '';
end;                           

function TSVGObject.Clone(Parent: TSVGObject): TSVGObject;
var
  C: Integer;
begin
  Result := New(Parent);
  Result.Assign(Self);

  for C := 0 to FItems.Count - 1 do
    GetItem(C).Clone(Result);
end;

function TSVGObject.Add(Item: TSVGObject): Integer;
begin
  Result := FItems.Add(Item);
  Item.FParent := Self;
end;

procedure TSVGObject.Delete(Index: Integer);
var
  Item: TSVGBasic;
begin
  if (Index >= 0) and (Index < Count) then
  begin
    Item := FItems[Index];
    FItems.Delete(Index);
    Remove(Item);
  end;
end;

function TSVGObject.Remove(Item: TSVGObject): Integer;
begin
  Result := FItems.Remove(Item);
  if Assigned(Item) then
  try
    if Item.FParent = Self then
      Item.FParent := nil;
  except
  end;
end;

function TSVGObject.IndexOf(Item: TSVGObject): Integer;
begin
  Result := FItems.IndexOf(Item);
end;

function TSVGObject.FindByID(const Name: WideString): TSVGObject;

  procedure Walk(SVG: TSVGObject);
  var
    C: Integer;
  begin
    if (SVG.FID = Name) or ('#' + SVG.FID = Name) then
    begin
      Result := SVG;
      Exit;
    end;

    for C := 0 to SVG.Count - 1  do
    begin
      Walk(SVG[C]);
      if Assigned(Result) then
        Exit;
    end;
  end;

begin
  Result := nil;
  Walk(Self);
end;

function TSVGObject.FindByType(Typ: TClass; Previous: TSVGObject = nil): TSVGObject;
var
  Found: Boolean;

  procedure Walk(SVG: TSVGObject);
  var
    C: Integer;
  begin
    if (SVG.ClassName = Typ.ClassName) and
       (Found) then
    begin
      Result := SVG;
      Exit;
    end;

    if SVG = Previous then
      Found := True;

    for C := 0 to SVG.Count - 1  do
    begin
      Walk(SVG[C]);
      if Assigned(Result) then
        Exit;
    end;
  end;

begin
  Found := (Previous = nil);
  Result := nil;
  Walk(Self);
end;

procedure TSVGObject.Assign(SVG: TSVGObject);
begin
  FVisible := SVG.FVisible;
  FDisplay := SVG.FDisplay;
  FBounds := SVG.GetObjectBounds;
  FID := SVG.FID;
  FObjectName := SVG.FObjectName;

  FreeAndNil(FStyle);
  FStyle := SVG.FStyle.Clone;
  FClasses.Assign(SVG.FClasses);
end;

{function TSVGObject.New(Parent: TSVGObject): TSVGObject;
begin
  Result := nil;
end;}

function TSVGObject.GetCount: Integer;
begin
  Result := FItems.Count;
end;

procedure TSVGObject.SetItem(Index: Integer; Item: TSVGObject);
begin
  if (Index >= 0) and (Index < Count) then
    FItems[Index] := Item;
end;

function TSVGObject.GetItem(Index: Integer): TSVGObject;
begin
  if (Index >= 0) and (Index < Count) then
    Result := FItems[Index]
  else
    Result := nil;
end;

function TSVGObject.GetObjectBounds: TBounds;
begin
  Result := FBounds;
end;

function TSVGObject.GetRoot: TSVG;
var
  Temp: TSVGObject;
begin
  Temp := Self;

  while Assigned(Temp) and (not (Temp is TSVG)) do
    Temp := Temp.FParent;

  Result := TSVG(Temp);
end;

function TSVGObject.GetDisplay: Integer;
var
  SVG: TSVGObject;
begin
  SVG := Self;
  while Assigned(SVG) and (SVG.FDisplay = INHERIT) do
    SVG := SVG.FParent;

  if Assigned(SVG) then
    Result := SVG.FDisplay
  else
    Result := 1;
end;

function TSVGObject.GetVisible: Integer;
var
  SVG: TSVGObject;
begin
  SVG := Self;
  while Assigned(SVG) and (SVG.FVisible = INHERIT) do
    SVG := SVG.FParent;

  if Assigned(SVG) then
    Result := SVG.FVisible
  else
    Result := 1;
end;

procedure TSVGObject.ReadIn(const Node: PXMLNode);
var
  S: WideString;
  C: Integer;
  A: PXMLNode;
begin
  LoadString(Node, 'id', FID);

  LoadDisplay(Node, FDisplay);
  LoadVisible(Node, FVisible);

  LoadString(Node, 'style', S);
  FStyle.SetValues(S);

  A := Node.FirstAttribute;
  while A <> nil do
  begin
    FStyle.AddStyle(A.NodeName, A.nodeValue);
    A := A.NextSibling;
  end;
  {for C := 0 to Node.AttributeCount - 1 do
  begin
    S := Node.attributes[C].nodeName;
    FStyle.AddStyle(S, Node.attributes[C].nodeValue);
  end;}

  S := '';
  LoadString(Node, 'class', S);

  FClasses.DelimitedText := S;
  for C := FClasses.Count - 1 downto 0 do
  begin
    FClasses[C] := Trim(FClasses[C]);
    if FClasses[C] = '' then
      FClasses.Delete(C);
  end;

  FObjectName := Node.nodeName;
end;

// TSVGMatrix

procedure TSVGMatrix.Assign(SVG: TSVGObject);
begin
  inherited;
  if SVG is TSVGMatrix then
    FMatrix := TSVGMatrix(SVG).FMatrix;
end;

{function TSVGMatrix.New(Parent: TSVGObject): TSVGObject;
begin
  Result := nil;
end;}

procedure TSVGMatrix.CalcMatrix;
var
  C: Integer;
  List: TList;
  SVG: TSVGObject;
  CompleteMatrix, Matrix, NewMatrix: TFloatMatrix;
begin
  {FCalculatedMatrix := FMatrix;
  if Parent is TSVGMatrix then
  begin
    if Parent is TSVG then
      Matrix := TSVG(Parent).RootMatrix
    else
      Matrix := TSVGMatrix(Parent).Matrix;

    if Matrix[2, 2] = 1 then
    begin
      if FCalculatedMatrix[2, 2] = 0 then
        FCalculatedMatrix := Matrix
      else
        FCalculatedMatrix := MultiplyMatrices(FCalculatedMatrix, Matrix);
    end;
  end;
  Exit;}

  List := TList.Create;

  SVG := Self;

  while Assigned(SVG) do
  begin
    List.Insert(0, SVG);
    SVG := SVG.FParent;
  end;

  FillChar(CompleteMatrix, SizeOf(CompleteMatrix), 0);
  FillChar(Matrix, SizeOf(Matrix), 0);
  for C := 0 to List.Count - 1 do
  begin
    SVG := TSVGMatrix(List[C]);
    if (SVG is TSVGMatrix) then
    begin
      if SVG is TSVG then
        NewMatrix := TSVG(SVG).RootMatrix
      else
        NewMatrix := TSVGMatrix(SVG).FMatrix;

      if NewMatrix[2, 2] = 1 then
      begin
        if CompleteMatrix[2, 2] = 0 then
          CompleteMatrix := NewMatrix
        else
          CompleteMatrix := Mult(CompleteMatrix, NewMatrix);

        if not (SVG is TSVG) then
        begin
          if Matrix[2, 2] = 0 then
            Matrix := NewMatrix
          else
            Matrix := Mult(Matrix, NewMatrix);
        end;
      end;
    end;
  end;

  List.Free;

  FCompleteCalculatedMatrix := CompleteMatrix;
  FCalculatedMatrix := Matrix;
end;

procedure TSVGMatrix.Clear;
begin
  inherited;
  FillChar(FMatrix, SizeOf(FMatrix), 0);
end;

procedure TSVGMatrix.ReadIn(const Node: PXMLNode);
var
  M: TFloatMatrix;
begin
  inherited;
  M := FMatrix;
  LoadTransform(Node, 'transform', M);
  FMatrix := M;
end;

procedure TSVGMatrix.SetMatrix(const Value: TFloatMatrix);
begin
  FMatrix := Value;

  CalcMatrix;
end;

function TSVGMatrix.Transform(P: TFloatPoint): TFloatPoint;
var
  TGP: TAffineTransformation;
  Point: TFloatPoint;
begin
  if FCalculatedMatrix[2, 2] = 1 then
  begin
    TGP := GetSVGTransformation(FCalculatedMatrix);
    Point.X := P.X;
    Point.Y := P.Y;
    P := TGP.Transform(Point);
    TGP.Free;
  end;
  Result := P;
end;

function TSVGMatrix.Transform(X, Y: TFloat): TFloatPoint;
var
  P: TFloatPoint;
begin
  P.X := X;
  P.Y := Y;
  Result := Transform(P);
end;

{function TSVGMatrix.TransformComplete(X, Y: TFloat): TSPoint;
var
  P: TSPoint;
begin
  P.X := X;
  P.Y := Y;
  Result := TransformComplete(P);
end;

function TSVGMatrix.TransformComplete(P: TSPoint): TSPoint;
var
  TGP: TGPMatrix;
begin
  if Matrix[2, 2] = 1 then
  begin
    TGP := GetGPMatrix(Matrix);
    TGP.TransformPoints(PGPPointF(@P));
    TGP.Free;
  end;
  Result := P;
end;}

// TSVGBasic

constructor TSVGBasic.Create;
begin
  inherited;
  FPath := nil;
  SetLength(FStrokeDashArray, 0);

  FClipPath := nil;
end;

{$IFDEF GPPen}
procedure TSVGBasic.BeforePaint(const Graphics: TBitmap32;
  const Brush: TGPBrush; const Pen: TGPPen);
begin

end;
{$ENDIF}

procedure TSVGBasic.CalcClipPath;
begin
  //FreeAndNil(FClipPath);
  FClipPath := GetClipPath;
end;

procedure TSVGBasic.Clear;
begin
  inherited;

  FX := 0;
  FY := 0;
  FWidth := 0;
  FHeight := 0;
  FRX := INHERIT;
  FRY := INHERIT;
  FFillURI := '';
  FStrokeURI := '';
  FillColor := INHERIT;
  StrokeColor := INHERIT;

  StrokeWidth := INHERIT;

  StrokeOpacity := 1;
  FillOpacity := 1;
  FLineWidth := INHERIT;

  FStrokeLineJoin := '';
  FStrokeLineCap := '';
  FStrokeMiterLimit := INHERIT;
  FStrokeDashOffset := INHERIT;

  SetLength(FStrokeDashArray, 0);
  FStrokeDashArrayCount := 0;
  FArrayNone := False;

  FFontName := '';
  FFontSize := INHERIT;
  FFontWeight := INHERIT;
  FFontStyle := INHERIT;

  FTextDecoration := [tdInherit];

  FreeAndNil(FPath);
  FClipPath := nil;
end;

procedure TSVGBasic.PaintToGraphics(Graphics: TBitmap32);

var
  {Brush, StrokeBrush: TGPBrush;
  Pen: TGPPen;

  TGP: TFloatMatrix;
  }
  TGP : TTransformation;
  Brush, StrokeBrush : TCustomPolygonFiller;
  ClipRoot: TSVGBasic;
  LPath : TArrayOfArrayOfFloatPoint;
begin
  if (FPath = nil) {or (FPath.GetLastStatus <> OK)} then
    Exit;

  LPath := self.FPath.Path;

  if FClipPath = nil then
    CalcClipPath;

  try
    if Assigned(FClipPath) then
    begin
      if ClipURI <> '' then
      begin
        {ClipRoot := TSVGBasic(GetRoot.FindByID(ClipURI));
        if Assigned(ClipRoot) then
        begin
          TGP := GetGPMatrix(ClipRoot.Matrix);
          Graphics.SetTransform(TGP);
          TGP.Free;
        end;}
      end;
      try
        //Graphics.SetClip(FClipPath);
      except
      end;
      //Graphics.ResetTransform;

      with TClipper.Create do
      try
        //add multiple contours of existing polygons as subject polygons ...
        Add(LPath, ptSubject);
        //add the single contour of the new polygon as the clipping polygon ...
        Add(FClipPath.Path, ptClip);
        //do the clipping operation (result => Polys) ...
        Execute(ctIntersection, LPath, pftEvenOdd);
      finally
        free;
      end;
    end;
    
    TGP := GetSVGTransformation({PureMatrix}Matrix);
    {
    Graphics.SetTransform(TGP);
    TGP.Free;
    }
    Brush := GetFillBrush;
    try
      StrokeBrush := GetStrokeBrush;
      {Pen := GetStrokePen(StrokeBrush);
      }
      try
        {
        BeforePaint(Graphics, Brush, Pen);
        }
        if Assigned(Brush) {and (Brush.GetLastStatus = OK)} then
          //Graphics.FillPath(Brush, FPath);
          PolyPolygonFS( Graphics, LPath, Brush, pfAlternate, TGP);


        if Assigned(StrokeBrush) {and (Brush.GetLastStatus = OK)} then
        begin
          PolyPolylineFS( Graphics, LPath, StrokeBrush, Assigned(Brush), GetStrokeWidth(),
          jsMiter,esButt, 4.0, TGP  );
        end;
        //PolyPolylineFS( Graphics, LPath, clTrBlue32, True);
        {if Assigned(Pen) and (Pen.GetLastStatus = OK) then
          Graphics.DrawPath(Pen, FPath);

        AfterPaint(Graphics, Brush, Pen);
        }
      finally
        //Pen.Free;
        StrokeBrush.Free;
      end;
    finally
      Brush.Free;
      TGP.Free;
    end;

  finally
    //Graphics.ResetTransform;
    //Graphics.ResetClip;

  end;
end;
{ $ELSE
var Color : TColor32;
  Opacity : Byte;
begin
  if Assigned(FPath) then
  begin
    if Assigned(FPath.Points) then
      FPath.ClosePath;
    //PolyPolylineFS( Graphics, self.FPath.Path, clGray32, True);
    Opacity := Round(255 * FillOpacity);
    Color := Color32(FillColor);
    PColor32Entry(@Color)^.A := Opacity;

    PolyPolygonFS( Graphics, self.FPath.Path, Color);
  end;
end;
{ $ENDIF}

procedure TSVGBasic.PaintToPath(Path: TFlattenedPath);
var
  P: TArrayOfArrayOfFloatPoint;
  M: TFloatMatrix;
  i : Integer;
begin
  if FPath = nil then
    Exit;
  Path.ClosePath;
  for i := 0 to Length(FPath.Path)-1 do
  begin
    Path.Polygon(FPath.path[i]);
  end;
  Path.ClosePath;

  {$IFDEF gpMatrix}
  P := FPath.Clone;

  if Matrix[2, 2] = 1 then
  begin
    M := GetGPMatrix(Matrix);
    P.Transform(M);
    M.Free;
  end;

  Path.AddPath(P, False);
  P.Free;
  {$ENDIF}
end;

procedure TSVGBasic.ReadIn(const Node: PXMLNode);
var
  Root: TSVG;
  C: Integer;
  Style: TStyle;
begin
  inherited;

  LoadLength(Node, 'x', FX);
  LoadLength(Node, 'y', FY);
  LoadLength(Node, 'width', FWidth);
  LoadLength(Node, 'height', FHeight);
  LoadLength(Node, 'rx', FRX);
  LoadLength(Node, 'ry', FRY);

  if (FRX = INHERIT) and (FRY <> INHERIT) then
    FRX := FRY;

  if (FRY = INHERIT) and (FRX <> INHERIT) then
    FRY := FRX;

  Root := GetRoot;
  for C := -2 to FClasses.Count do
  begin
    case C of
      -2: Style := Root.FStyles.GetStyle(FObjectName);
      -1: Style := Root.FStyles.GetStyle('#' + FID);
      else
        begin
          if C < FClasses.Count then
          begin
            if Assigned(Root) then
            begin
              Style := Root.FStyles.GetStyle('.' + FClasses[C]);
              if Style = nil then
                Style := Root.FStyles.GetStyle(FClasses[C]);
            end else
              Style := nil;
          end else
            Style := FStyle;
          end;
        end;

    if Assigned(Style) then
      ReadStyle(Style);
  end;

  FillColor := GetColor(FFillURI);
  StrokeColor := GetColor(FStrokeURI);
  FFillURI := ParseURI(FFillURI);
  FStrokeURI := ParseURI(FStrokeURI);
  ClipURI := ParseURI(FClipURI);
end;
{$IFDEF gppen}
procedure TSVGBasic.AfterPaint(const Graphics: TBitmap32;
  const Brush: TGPBrush; const Pen: TGPPen);
begin

end;
{$ENDIF}
procedure TSVGBasic.Assign(SVG: TSVGObject);
var
  C: Integer;
begin
  inherited;

  if SVG is TSVGBasic then
  begin
    FFillColor := TSVGBasic(SVG).FFillColor;
    FStrokeColor := TSVGBasic(SVG).FStrokeColor;
    FFillOpacity := TSVGBasic(SVG).FFillOpacity;
    FStrokeOpacity := TSVGBasic(SVG).FStrokeOpacity;
    FStrokeWidth := TSVGBasic(SVG).FStrokeWidth;
    FStrokeLineJoin := FStrokeLineJoin;
    FStrokeLineCap := TSVGBasic(SVG).FStrokeLineCap;
    FStrokeMiterLimit := TSVGBasic(SVG).FStrokeMiterLimit;
    FStrokeDashOffset := TSVGBasic(SVG).FStrokeDashOffset;
    FStrokeDashArrayCount := TSVGBasic(SVG).FStrokeDashArrayCount;

    FFontName := TSVGBasic(SVG).FFontName;
    FFontSize := TSVGBasic(SVG).FFontSize;
    FFontWeight := TSVGBasic(SVG).FFontWeight;
    FFontStyle := TSVGBasic(SVG).FFontStyle;
    FTextDecoration := TSVGBasic(SVG).FTextDecoration;

    if Assigned(TSVGBasic(SVG).FStrokeDashArray) then
    begin
      SetLength(FStrokeDashArray, FStrokeDashArrayCount);
      for C := 0 to FStrokeDashArrayCount - 1 do
        FStrokeDashArray[C] := TSVGBasic(SVG).FStrokeDashArray[C];
    end;

    FArrayNone := TSVGBasic(SVG).FArrayNone;

    if Assigned(TSVGBasic(SVG).FPath) then
      FPath := TSVGBasic(SVG).FPath;

    FRX := TSVGBasic(SVG).FRX;
    FRY := TSVGBasic(SVG).FRY;
    FFillURI := TSVGBasic(SVG).FFillURI;
    FStrokeURI := TSVGBasic(SVG).FStrokeURI;
    ClipURI := TSVGBasic(SVG).FClipURI;
    FLineWidth := TSVGBasic(SVG).FLineWidth;
    FFillRule := TSVGBasic(SVG).FFillRule;
    FColorInterpolation := TSVGBasic(SVG).FColorInterpolation;
    FColorRendering := TSVGBasic(SVG).FColorRendering;

    FX := TSVGBasic(SVG).X;
    FY := TSVGBasic(SVG).Y;
    FWidth := TSVGBasic(SVG).Width;
    FHeight := TSVGBasic(SVG).Height;
  end;
end;

function TSVGBasic.New(Parent: TSVGObject): TSVGObject;
begin
  Result := TSVGBasic.Create(Parent);
end;

procedure TSVGBasic.ReadStyle(Style: TStyle);
var
  Value: WideString;
  SL: TWideStringList;

  procedure ConstructFont;
  var
    Bold, Italic: Integer;
    FN: WideString;
  begin
    Bold := Pos('Bold', FFontName);
    Italic := Pos('Italic', FFontName);

    FN := FFontName;

    // Check for Bold
    if Bold <> 0 then
    begin
      FFontName := Copy(FN, 1, Bold - 1) + Copy(FN, Bold + 4, MaxInt);
      if Copy(FFontName, Length(FFontName), 1) = '-' then
        FFontName := Copy(FFontName, 1, Length(FFontName) - 1);
      if IsFontAvailable then
      begin
        Style['font-weight'] := 'bold';
        Exit;
      end;
      if Copy(FFontName, Length(FFontName) - 1, 2) = 'MT' then
      begin
        FFontName := Copy(FFontName, 1, Length(FFontName) - 2);
        if Copy(FFontName, Length(FFontName), 1) = '-' then
          FFontName := Copy(FFontName, 1, Length(FFontName) - 1);
        if IsFontAvailable then
        begin
          Style['font-weight'] := 'bold';
          Exit;
        end;
      end;
    end;

    // Check for Italic
    if Italic <> 0 then
    begin
      FFontName := Copy(FN, 1, Italic - 1) + Copy(FN, Italic + 6, MaxInt);
      if Copy(FFontName, Length(FFontName), 1) = '-' then
        FFontName := Copy(FFontName, 1, Length(FFontName) - 1);
      if IsFontAvailable then
      begin
        Style['font-style'] := 'italic';
        Exit;
      end;
      if Copy(FFontName, Length(FFontName) - 1, 2) = 'MT' then
      begin
        FFontName := Copy(FFontName, 1, Length(FFontName) - 2);
        if Copy(FFontName, Length(FFontName), 1) = '-' then
          FFontName := Copy(FFontName, 1, Length(FFontName) - 1);
        if IsFontAvailable then
        begin
          Style['font-style'] := 'italic';
          Exit;
        end;
      end;
    end;

    // Check for Bold and Italic
    if (Bold <> 0) and (Italic <> 0) then
    begin
      FFontName := Copy(FN, 1, Bold - 1) + Copy(FN, Bold + 4, MaxInt);
      if Copy(FFontName, Length(FFontName), 1) = '-' then
        FFontName := Copy(FFontName, 1, Length(FFontName) - 1);
      Italic := Pos('Italic', FFontName);

      FFontName := Copy(FFontName, 1, Italic - 1) + Copy(FFontName, Italic + 6, MaxInt);
      if Copy(FFontName, Length(FFontName), 1) = '-' then
        FFontName := Copy(FFontName, 1, Length(FFontName) - 1);

      if IsFontAvailable then
      begin
        Style['font-weight'] := 'bold';
        Style['font-style'] := 'italic';
        Exit;
      end;
      if Copy(FFontName, Length(FFontName) - 1, 2) = 'MT' then
      begin
        FFontName := Copy(FFontName, 1, Length(FFontName) - 2);
        if Copy(FFontName, Length(FFontName), 1) = '-' then
          FFontName := Copy(FFontName, 1, Length(FFontName) - 1);
        if IsFontAvailable then
        begin
          Style['font-weight'] := 'bold';
          Style['font-style'] := 'italic';
          Exit;
        end;
      end;
    end;

    FFontName := FN;
    if Copy(FFontName, Length(FFontName) - 1, 2) = 'MT' then
    begin
      FFontName := Copy(FFontName, 1, Length(FFontName) - 2);
      if Copy(FFontName, Length(FFontName), 1) = '-' then
        FFontName := Copy(FFontName, 1, Length(FFontName) - 1);
      if IsFontAvailable then
        Exit;
    end;

    FFontName := FN;
  end;

begin
  Value := Style.Values['stroke-width'];
  if Value <> '' then
    FStrokeWidth := ParseLength(Value);

  Value := Style.Values['line-width'];
  if Value <> '' then
    FLineWidth := ParseLength(Value);

  Value := Style.Values['opacity'];
  if Value <> '' then
  begin
    FStrokeOpacity := ParsePercent(Value);
    FFillOpacity := FStrokeOpacity;
  end;

  Value := Style.Values['stroke-opacity'];
  if Value <> '' then
    FStrokeOpacity := ParsePercent(Value);

  Value := Style.Values['fill-opacity'];
  if Value <> '' then
    FFillOpacity := ParsePercent(Value);

  Value := Style.Values['color'];
  if Value <> '' then
  begin
    FStrokeURI := Value;
    FFillURI := Value;
  end;

  Value := Style.Values['stroke'];
  if Value <> '' then
    FStrokeURI := Value;

  Value := Style.Values['fill'];
  if Value <> '' then
    FFillURI := Value;

  Value := Style.Values['clip-path'];
  if Value <> '' then
    ClipURI := Value;

  Value := Style.Values['stroke-linejoin'];
  if Value <> '' then
    FStrokeLineJoin := Value;

  Value := Style.Values['stroke-linecap'];
  if Value <> '' then
    FStrokeLineCap := Value;

  Value := Style.Values['stroke-miterlimit'];
  if Value <> '' then
    if not TryStrToTFloat(Value, FStrokeMiterLimit) then
      FStrokeMiterLimit := 0;

  Value := Style.Values['stroke-dashoffset'];
  if Value <> '' then
    if not TryStrToTFloat(Value, FStrokeDashOffset) then
      FStrokeDashOffset := 0;

  Value := Style.Values['stroke-dasharray'];
  if Value <> '' then
    SetStrokeDashArray(Value);

  Value := Style['font-family'];
  if Value <> '' then
  begin
    FFontName := Value;
    if not IsFontAvailable then
      ConstructFont;
  end;

  Value := Style['font-weight'];
  if Value <> '' then
    ParseFontWeight(Value);

  Value := Style['font-size'];
  if Value <> '' then
    FFontSize := ParseLength(Value);

  Value := Style['text-decoration'];
  if Value <> '' then
  begin
    SL := TWideStringList.Create;
    SL.Delimiter := ' ';
    SL.DelimitedText := Value;

    if SL.IndexOf('underline') > -1 then
    begin
      Exclude(FTextDecoration, tdInherit);
      Include(FTextDecoration, tdUnderLine);
    end;

    if SL.IndexOf('overline') > -1 then
    begin
      Exclude(FTextDecoration, tdInherit);
      Include(FTextDecoration, tdOverLine);
    end;

    if SL.IndexOf('line-through') > -1 then
    begin
      Exclude(FTextDecoration, tdInherit);
      Include(FTextDecoration, tdStrikeOut);
    end;

    if SL.IndexOf('none') > -1 then
      FTextDecoration := [];

    SL.Free;
  end;

  Value := Style['font-style'];
  if Value <> '' then
  begin
    if Value = 'normal' then
      FFontStyle := FontNormal;

    if Value = 'italic' then
      FFontStyle := FontItalic;
  end;
end;

//procedure TSVGBasic.ReadChildren(const Node: PXMLNode);
procedure TSVGObject.ReadChildren(const Node: PXMLNode);
var
  C: Integer;
  SVG: TSVGObject;
  Root: TSVG;
  tag : WideString;
begin
  for C := 0 to Node.childNodes.Count - 1 do
  begin
    SVG := nil;

    tag := Node.childNodes[C].nodeName;

    if tag = 'g' then
      SVG := TSVGContainer.Create(Self)
    else

    if tag = 'switch' then
      SVG := TSVGSwitch.Create(Self) 
    else

    if tag = 'defs' then
      SVG := TSVGDefs.Create(Self)
    else

    if tag = 'use' then
      SVG := TSVGUse.Create(Self)
    else

    if tag = 'rect' then
      SVG := TSVGRect.Create(Self)
    else

    if tag = 'line' then
      SVG := TSVGLine.Create(Self)
    else

    if tag = 'polyline' then
      SVG := TSVGPolyLine.Create(Self)
    else

    if tag = 'polygon' then
      SVG := TSVGPolygon.Create(Self)
    else

    if tag = 'circle' then
      SVG := TSVGEllipse.Create(Self)
    else

    if tag = 'ellipse' then
      SVG := TSVGEllipse.Create(Self)
    else

    if tag = 'path' then
      SVG := TSVGPath.Create(Self)
    else

    if tag = 'image' then
      SVG := TSVGImage.Create(Self)
    else
    {$IFDEF GPTEXT}
    if tag = 'text' then
      SVG := TSVGText.Create(Self)
    else

    if tag = 'tspan' then
      SVG := TSVGTSpan.Create(Self)
    else

    if tag = 'textPath' then
      SVG := TSVGTextPath.Create(Self)
    else
    {$ENDIF}
    if tag = 'clipPath' then
      SVG := TSVGClipPath.Create(Self)
    else

    if tag = 'linearGradient' then
      SVG := TSVGLinearGradient.Create(Self)
    else

    if tag = 'radialGradient' then
      SVG := TSVGRadialGradient.Create(Self)
    else

    if tag = 'symbol' then
      SVG := TSVGSymbol.Create(Self)
    else

    if tag = 'pattern' then
      SVG := TSVGPattern.Create(Self)
    else

    if tag = 'style' then
    begin
      Root := GetRoot;
      Root.ReadStyles(Node.childNodes[C]);
    end;

    if Assigned(SVG) then
      SVG.ReadIn(Node.childNodes[C]);
  end;
end;

procedure TSVGBasic.SetClipURI(const Value: WideString);
begin
  FClipURI := Value;

  CalcClipPath;
end;

procedure TSVGBasic.SetStrokeDashArray(const S: WideString);
var
  C, E: Integer;
  SL: TStringList;
  D: TFloat;
begin
  SetLength(FStrokeDashArray, 0);

  FArrayNone := False;
  if Trim(S) = 'none' then
  begin
    FArrayNone := True;
    Exit;
  end;

  SL := TStringList.Create;
  SL.Delimiter := ',';
  SL.DelimitedText := S;

  for C := SL.Count - 1 downto 0 do
  begin
    SL[C] := Trim(SL[C]);
    if SL[C] = '' then
      SL.Delete(C);
  end;

  if SL.Count = 0 then
  begin
    SL.Free;
    Exit;
  end;

  if SL.Count mod 2 = 1 then
  begin
    E := SL.Count;
    for C := 0 to E - 1 do
      SL.Add(SL[C]);
  end;

  SetLength(FStrokeDashArray, SL.Count);
  FStrokeDashArrayCount := SL.Count;

  for C := 0 to SL.Count - 1 do
  begin
    if not TryStrToTFloat(SL[C], D) then
      D := 0;
    FStrokeDashArray[C] := D;
  end;

  SL.Free;
end;

function TSVGBasic.GetBrush(AURI: WideString): TCustomPolygonFiller;
var
  Color: Integer;
  C32 : TColor32;
  Opacity: Integer;
  Filler: TSVGObject;
begin
  Result := nil;
  Opacity := Round(255 * FillOpacity);

  if AURI <> '' then
  begin
    Filler := GetRoot.FindByID(AURI);
    if Assigned(Filler) and (Filler is TSVGFiller) then
      Result := TSVGFiller(Filler).GetBrush(Opacity, Self);
  end
  else
  begin
    Color := FillColor;
    if Color >= 0 then
    begin
      C32 := Color32(Color);
      PColor32Entry(@C32)^.A := Opacity;

      //Result := TGPSolidBrush.Create(ConvertColor(Color, Opacity));
      Result := TSolidPoligonFiller.Create;
      TSolidPoligonFiller(Result).Color := C32;
    end;
  end;
end;

function TSVGBasic.GetFillBrush: {TGPBrush} TCustomPolygonFiller;
begin
  Result := GetBrush(FFillURI);
end;


function TSVGBasic.GetFillColor: Integer;
var
  SVG: TSVGObject;
begin
  SVG := Self;
  while Assigned(SVG) and (TSVGBasic(SVG).FFillColor = INHERIT) do
    SVG := SVG.FParent;

  if Assigned(SVG) then
    Result := TSVGBasic(SVG).FFillColor
  else
    Result := 0;
end;

function TSVGBasic.GetStrokeBrush: TCustomPolygonFiller;
var
  Color: Integer;
  C32 : TColor32;
  Opacity: Integer;
  Filler: TSVGObject;
begin
  Result := nil;


  Opacity := Round(255 * StrokeOpacity);

  if FStrokeURI <> '' then
  begin
    Filler := GetRoot.FindByID(FStrokeURI);
    if Assigned(Filler) and (Filler is TSVGFiller) then
      Result := TSVGFiller(Filler).GetBrush(Opacity, Self);
  end
  else
  begin
    Color := StrokeColor;
    if Color >= 0 then
    begin
      C32 := Color32(Color);
      PColor32Entry(@C32)^.A := Opacity;

      //Result := TGPSolidBrush.Create(ConvertColor(Color, Opacity));
      Result := TSolidPoligonFiller.Create;
      TSolidPoligonFiller(Result).Color := C32;
    end;
  end;
end;
{var
  Color: Integer;
  Opacity: Integer;
  Filler: TSVGObject;
begin
  Result := nil;
  Color := StrokeColor;
  Opacity := Round(255 * StrokeOpacity);

  if FStrokeURI <> '' then
  begin
    Filler := GetRoot.FindByID(FStrokeURI);
    if Assigned(Filler) and (Filler is TSVGFiller) then
      Result := TSVGFiller(Filler).GetBrush(Opacity, Self);
  end else
    if Color >= 0 then
      Result := TGPSolidBrush.Create(ConvertColor(Color, Opacity));
end;
{ $ENDIF}
function TSVGBasic.GetStrokeColor: Integer;
var
  SVG: TSVGObject;
begin
  SVG := Self;
  while Assigned(SVG) and (TSVGBasic(SVG).FStrokeColor = INHERIT) do
    SVG := SVG.FParent;

  if Assigned(SVG) then
    Result := TSVGBasic(SVG).FStrokeColor
  else
    Result := -2;
end;

function TSVGBasic.GetFillOpacity: TFloat;
var
  SVG: TSVGObject;
begin
  SVG := Self;
  while Assigned(SVG) and (TSVGBasic(SVG).FFillOpacity = INHERIT) do
    SVG := SVG.FParent;

  if Assigned(SVG) then
    Result := TSVGBasic(SVG).FFillOpacity
  else
    Result := 1;

  SVG := FParent;
  while Assigned(SVG) do
  begin
    Result := Result * TSVGBasic(SVG).FillOpacity;
    SVG  := SVG.FParent;
  end;
end;

function TSVGBasic.GetStrokeOpacity: TFloat;
var
  SVG: TSVGObject;
begin
  SVG := Self;
  while Assigned(SVG) and (TSVGBasic(SVG).FStrokeOpacity = INHERIT) do
    SVG := SVG.FParent;

  if Assigned(SVG) then
    Result := TSVGBasic(SVG).FStrokeOpacity
  else
    Result := 1;

  SVG := FParent;
  while Assigned(SVG) do
  begin
    Result := Result * TSVGBasic(SVG).StrokeOpacity;
    SVG  := SVG.FParent;
  end;
end;
{$IFDEF GPPEN}
function TSVGBasic.GetStrokePen(const StrokeBrush: TGPBrush): TGPPen;
var
  Pen: TGPPen;
  DashArray: PSingle;
  C: Integer;
begin
  if Assigned(StrokeBrush) and (StrokeBrush.GetLastStatus = OK) then
  begin
    Pen := TGPPen.Create(0, GetStrokeWidth);
    Pen.SetLineJoin(GetStrokeLineJoin);
    Pen.SetMiterLimit(GetStrokeMiterLimit);
    Pen.SetLineCap(GetStrokeLineCap, GetStrokeLineCap, GetStrokeLineCap);

    DashArray := GetStrokeDashArray(C);
    if Assigned(DashArray) then
    begin
      Pen.SetDashPattern(PSingle(DashArray^), C);
      Pen.SetDashStyle(DashStyleCustom);
      Pen.SetDashOffset(GetStrokeDashOffset);
    end;

    Pen.SetBrush(StrokeBrush);
    Result := Pen;
  end else
    Result := nil;
end;
{$ENDIF}
function TSVGBasic.GetStrokeWidth: TFloat;
var
  SVG: TSVGObject;
begin
  SVG := Self;
  while Assigned(SVG) and (TSVGBasic(SVG).FStrokeWidth = INHERIT) do
    SVG := SVG.FParent;

  if Assigned(SVG) and (SVG is TSVGBasic) then
    Result := TSVGBasic(SVG).FStrokeWidth
  else
    Result := -2;
end;

function TSVGBasic.GetTextDecoration: TTextDecoration;
var
  SVG: TSVGObject;
begin
  SVG := Self;
  while Assigned(SVG) and (tdInherit in TSVGBasic(SVG).FTextDecoration) do
    SVG := SVG.FParent;

  if Assigned(SVG) then
    Result := TSVGBasic(SVG).FTextDecoration
  else
    Result := [];
end;

function TSVGBasic.IsFontAvailable: Boolean;
//x2nie var   FF: TGPFontFamily;
begin
  //FF := TGPFontFamily.Create(GetFontName);
  //Result :=  FF.GetLastStatus = OK;
  //FF.Free;
end;

function TSVGBasic.GetClipURI: WideString;
var
  SVG: TSVGObject;
begin
  SVG := Self;
  while Assigned(SVG) and (SVG is TSVGBasic) and  (TSVGBasic(SVG).FClipURI = '') do
    SVG := SVG.FParent;

  if Assigned(SVG) and (SVG is TSVGBasic) then
    Result := TSVGBasic(SVG).FClipURI
  else
    Result := '';
end;

function TSVGBasic.GetStrokeLineCap: TEndStyle;
var
  SVG: TSVGObject;
begin
  Result := esButt;

  SVG := Self;
  while Assigned(SVG) and (TSVGBasic(SVG).FStrokeLineCap = '') do
    SVG := SVG.FParent;

  if Assigned(SVG) then
  begin
    if TSVGBasic(SVG).FStrokeLineCap = 'round' then
      Result := esRound;

    if TSVGBasic(SVG).FStrokeLineCap = 'square' then
      Result := esSquare;
  end;
end;

function TSVGBasic.GetStrokeLineJoin: TJoinStyle;
var
  SVG: TSVGObject;
begin
  Result := jsMiter;

  SVG := Self;
  while Assigned(SVG) and (TSVGBasic(SVG).FStrokeLineJoin = '') do
    SVG := SVG.FParent;

  if Assigned(SVG) then
  begin
    if TSVGBasic(SVG).FStrokeLineJoin = 'round' then
      Result := jsRound;

    if TSVGBasic(SVG).FStrokeLineJoin = 'bevel' then
      Result := jsBevel;
  end;
end;

function TSVGBasic.GetStrokeMiterLimit: TFloat;
var
  SVG: TSVGObject;
begin
  Result := 4;

  SVG := Self;
  while Assigned(SVG) and (TSVGBasic(SVG).FStrokeMiterLimit = INHERIT) do
    SVG := SVG.FParent;

  if Assigned(SVG) and (TSVGBasic(SVG).FStrokeMiterLimit <> INHERIT) then
      Result := TSVGBasic(SVG).FStrokeMiterLimit;
end;

function TSVGBasic.GetStrokeDashOffset: TFloat;
var
  SVG: TSVGObject;
begin
  Result := 0;

  SVG := Self;
  while Assigned(SVG) and (TSVGBasic(SVG).FStrokeDashOffset = INHERIT) do
    SVG := SVG.FParent;

  if Assigned(SVG) and (TSVGBasic(SVG).FStrokeDashOffset <> INHERIT) then
      Result := TSVGBasic(SVG).FStrokeDashOffset;
end;

function TSVGBasic.GetStrokeDashArray(var Count: Integer): PSingle;
var
  SVG: TSVGObject;
begin
  Result := nil;
  Count := 0;

  SVG := Self;
  while Assigned(SVG) and
        (TSVGBasic(SVG).FStrokeDashArrayCount = 0) and
        (not TSVGBasic(SVG).FArrayNone) do
    SVG := SVG.FParent;

  if Assigned(SVG) and Assigned(TSVGBasic(SVG).FStrokeDashArray) and
     (not TSVGBasic(SVG).FArrayNone) then
  begin
    Result := @TSVGBasic(SVG).FStrokeDashArray;
    Count := TSVGBasic(SVG).FStrokeDashArrayCount;
  end;
end;

function TSVGBasic.GetFontName: WideString;
var
  SVG: TSVGObject;
begin
  SVG := Self;
  while Assigned(SVG) and
    ((not (SVG is TSVGBasic)) or (TSVGBasic(SVG).FFontName = '')) do
    SVG := SVG.FParent;

  if Assigned(SVG) and (SVG is TSVGBasic) then
    Result := TSVGBasic(SVG).FFontName
  else
    Result := 'Arial';
end;

function TSVGBasic.GetFontWeight: Integer;
var
  SVG: TSVGObject;
begin
  SVG := Self;
  while Assigned(SVG) and
    ((not (SVG is TSVGBasic)) or (TSVGBasic(SVG).FFontWeight = INHERIT)) do
    SVG := SVG.FParent;

  if Assigned(SVG) and (SVG is TSVGBasic) then
    Result := TSVGBasic(SVG).FFontWeight
  else
    Result := FW_NORMAL;
end;

function TSVGBasic.GetFontSize: TFloat;
var
  SVG: TSVGObject;
begin
  SVG := Self;
  while Assigned(SVG) and
    ((not (SVG is TSVGBasic)) or (TSVGBasic(SVG).FFontSize = INHERIT)) do
    SVG := SVG.FParent;

  if Assigned(SVG) and (SVG is TSVGBasic) then
    Result := TSVGBasic(SVG).FFontSize
  else
    Result := 11;
end;

function TSVGBasic.GetFontStyle: Integer;
var
  SVG: TSVGObject;
begin
  SVG := Self;
  while Assigned(SVG) and
    ((not (SVG is TSVGBasic)) or (TSVGBasic(SVG).FFontStyle = INHERIT)) do
    SVG := SVG.FParent;

  if Assigned(SVG) and (SVG is TSVGBasic) then
    Result := TSVGBasic(SVG).FFontStyle
  else
    Result := 0;
end;

procedure TSVGBasic.ParseFontWeight(const S: WideString);
begin
  TryStrToInt(S, FFontWeight);
  if S = 'normal' then
    FFontWeight := FW_NORMAL;

  if S = 'bold' then
    FFontWeight := FW_BOLD;

  if S = 'bolder' then
    FFontWeight := FW_EXTRABOLD;

  if S = 'lighter' then
    FFontWeight := FW_LIGHT;
end;

procedure TSVGBasic.ConstructPath;
begin
  FreeAndNil(FPath);
end;

function TSVGBasic.GetClipPath: TFlattenedPath;
var
  Path: TSVGObject;
  ClipRoot: TSVGClipPath;
  //R: TDRect;
  //M: TGPMatrix;
  //Root: TSVG;
begin
  Result := nil;

  if ClipURI <> '' then
  begin
    Path := GetRoot.FindByID(ClipURI);
    if Path is TSVGClipPath then
      ClipRoot := TSVGClipPath(Path)
    else
      ClipRoot := nil;
    if Assigned(ClipRoot) then
      Result := ClipRoot.GetClipPath;
    Exit;
  end;

(*  if Result = nil then
  begin
    Root := GetRoot;
    if Root = nil then
      Exit;
    Result := TGPGraphicsPath.Create;
    R := Root.ViewBox;
    if (R.Width = 0) then
      R.Width := Root.Width;
    if (R.Height = 0) then
      R.Height := Root.Height;
    R.Left := 0;
    R.Top := 0;
    R.Width := Root.Width;
    R.Height := Root.Height;
    Result.AddRectangle(MakeRect(R.Left, R.Top, R.Width, R.Height));

    {Root.CalcRootMatrix;
    M := GetGPMatrix(Root.RootMatrix);
    Result.Transform(M);
    M.Free;}
  end;*)
end;

// TSVG

procedure TSVG.LoadFromText(const Text: WideString);
var
  XML: IXMLDocument;
  DocNode: PXMLNode;
begin
  Clear;
  try
    FSource := Text;
    XML := TXMLDocument.Create();
    XML.LoadFromXML(Text);
    if Assigned(XML) then
    begin
      DocNode := XML.documentElement;
      if DocNode.nodeName = 'svg' then
        ReadIn(DocNode)
      else
        FSource := '';
    end else
      FSource := '';
  finally
    XML := nil;
  end;
end;

procedure TSVG.LoadFromFile(const FileName: WideString);
var
  St: TFileStream;
begin
  St := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(St);
    FFileName := FileName;
  finally
    St.Free;
  end;
end;

procedure TSVG.LoadFromStream(const Stream: IStream);
var
 Stat: STATSTG;
 Size: Int64;
 SizeRead: Longint;
 S: TMemoryStream;
begin
  if Stream.Stat(Stat, 1) <> S_OK then
    Exit;

  Size := Stat.cbSize;
  if Size <= 0 then
    Exit;

  S := TMemoryStream.Create;
  try
    S.SetSize(Size);

    if Stream.Read(S.Memory, Size, @SizeRead) = S_OK then
    begin
      S.Position := 0;
      LoadFromStream(S);
    end;
  finally
    S.Free;
  end;
end;

procedure TSVG.LoadFromStream(Stream: TStream);
var
  SL: TWideStringList;
begin
  SL := TWideStringList.Create;
  try
    Stream.Position := 0;
    SL.LoadFromStream(Stream);
    LoadFromText(SL.Text);
  finally
    SL.Free;
  end;
end;

procedure TSVG.SaveToFile(const FileName: WideString);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TSVG.SaveToStream(Stream: TStream);
var
  SL: TWideStringList;
begin
  SL := TWideStringList.Create;
  try
    SL.Text := FSource;
    SL.SaveToStream(Stream);
  finally
    SL.Free;
  end;
end;

function TSVG.SaveToNode(const Parent: PXMLNode; Left, Top, Width, Height: TFloat): PXMLNode;
var
  XML: IXMLDocument;
  Translation: AnsiString;
  Scale: AnsiString;
  C: Integer;
  Container: PXMLNode;
  Attribute: PXMLNode;
  NewNode: PXMLNode;

  function ConvertFloat(D: TFloat): AnsiString;
  begin
    Result := FloatToStr(D);
    Result := StringReplace(Result, ',', '.', []);
  end;

begin
  Result := nil;
  if FSource = '' then
    Exit;
(**x2nie
  try
    XML := CreateFromString(FSource);

    Container := Parent.ownerDocument.createElement('g');
    Parent.appendChild(Container);

    if (Left <> 0) or (Top <> 0) then
      Translation := 'translate(' + ConvertFloat(Left) + ', ' + ConvertFloat(Top) + ')'
    else
      Translation := '';

    if (Width <> FWidth) or (Height <> FHeight) then
      Scale := 'scale(' + ConvertFloat(Width / FWidth) + ', ' +
        ConvertFloat(Width / FWidth) + ')'
    else
       Scale := '';

    if Scale <> '' then
    begin
      if Translation <> '' then
        Translation := Translation + ' ' + Scale
      else
        Translation := Scale;
    end;

    if Translation <> '' then
    begin
      Attribute := Container.ownerDocument.createElement('transform');
      Container.attributes.setNamedItem(Attribute);
    end;

    for C := 0 to XML.documentElement.childNodes.length - 1 do
    begin
      NewNode := XML.documentElement.childNodes[C].cloneNode(True);
      Container.childNodes[C].appendChild(NewNode);
      Result := NewNode;
    end;
  finally
    XML := nil;
  end;
  *)
end;

procedure TSVG.Scale(DX: TFloat; DY: TFloat = -1);
begin
  if DY < 0 then
    DY := DX;

  if SameValue(FDX, DX) and SameValue(FDY, DY) then
    Exit;

  FDX := DX;
  FDY := DY;
end;

procedure TSVG.PaintTo(DC: HDC; Bounds: TFloatRect;
  Rects: PRectArray; RectCount: Integer);
var
  Graphics: TBitmap32;
begin
  {x2nie
  Graphics := TBitmap32.Create(DC);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    PaintTo(Graphics, Bounds, Rects, RectCount);
  finally
    Graphics.Free;
  end;
  }
end;

{$IFDEF GPMetaFile}
procedure TSVG.PaintTo(MetaFile: TGPMetaFile; Bounds: TFloatRect;
  Rects: PRectArray; RectCount: Integer);
var
  Graphics: TBitmap32;
begin
  Graphics := TBitmap32.Create(MetaFile);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    PaintTo(Graphics, Bounds, Rects, RectCount);
  finally
    Graphics.Free;
  end;
end;
{$ENDIF}

procedure TSVG.PaintTo(Graphics: TBitmap32; Bounds: TFloatRect;
  Rects: PRectArray; RectCount: Integer);
var
  M: TFloatMatrix;
  //x2nieMA: TMatrixArray;
begin
  //x2nieM := TFloatMatrix.Create;
  try
    //x2nieGraphics.GetTransform(M);
    try
      {x2nie M.GetElements(MA);

      FInitialMatrix[0, 0] := MA[0];
      FInitialMatrix[0, 1] := MA[1];
      FInitialMatrix[1, 0] := MA[2];
      FInitialMatrix[1, 1] := MA[3];
      FInitialMatrix[2, 0] := MA[4];
      FInitialMatrix[2, 1] := MA[5];
      FInitialMatrix[2, 2] := 1;
      }
      SetBounds(Bounds);

      Paint(Graphics, Rects, RectCount);
    finally
      //x2nieGraphics.SetTransform(M);
    end;
  finally
    //x2nie M.Free;
  end;
end;

procedure TSVG.PaintTo(Bitmap: TBitmap; Bounds: TFloatRect;
  Rects: PRectArray; RectCount: Integer);
var
  Graphics: TBitmap32;
begin
  Graphics := TBitmap32.Create();
  Graphics.Assign(Bitmap);
  {x2nie try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    PaintTo(Graphics, Bounds, Rects, RectCount);
  finally
    Graphics.Free;
  end;}
end;

constructor TSVG.Create;
begin
  inherited;
  FStyles := TStyleList.Create;
  FillChar(FInitialMatrix, SizeOf(FInitialMatrix), 0);
  FDX := 1;
  FDY := 1;
end;

destructor TSVG.Destroy;
begin
  FreeAndNil(FStyles);
  inherited;
end;

procedure TSVG.Clear;
begin
  inherited;

  FSource := '';

  if Assigned(FStyles) then
    FStyles.Clear;

  FillChar(FViewBox, SizeOf(FViewBox), 0);
  FillChar(FInitialMatrix, SizeOf(FInitialMatrix), 0);
  FDC := 0;

  FX := 0;
  FY := 0;
  FWidth := 0;
  FHeight := 0;

  FSize := FloatRect(0.0, 0, 0, 0);

  FRX := 0;
  FRY := 0;

  FillColor := -2;
  FillOpacity := 1;
  StrokeColor := -2;
  StrokeWidth := 1;
  StrokeOpacity := 1;

  FAngle := 0;
  FillChar(FAngleMatrix, SizeOf(TFloatMatrix), 0);

  FLineWidth := 1;

  FFileName := '';
end;

procedure TSVG.SetSVGOpacity(Opacity: TFloat);
begin
  StrokeOpacity := Opacity;
  FillOpacity := Opacity;
end;

procedure TSVG.SetViewBox(const Value: TFRect);
begin
  FViewBox := Value;
end;

procedure TSVG.SetAngle(Angle: TFloat);
begin
  if SameValue(FAngle, Angle) then
    Exit;

  FAngle := Angle;
  {x2nie
  FAngleMatrix := CalcRotationMatrix(Width / 2, Height / 2, FAngle);
  }
end;

procedure TSVG.SetBounds(const Bounds: TFloatRect);
begin
  FRootBounds := Bounds;

  if FWidth = 0 then
    FWidth := FRootBounds.Right;

  if FHeight = 0 then
    FHeight := FRootBounds.Bottom;

  if (FWidth > 0) and (FRootBounds.Right <> -1) then
    FDX := FRootBounds.Right / FWidth;

  if (FHeight > 0) and (FRootBounds.Bottom <> -1) then
    FDY := FRootBounds.Bottom / FHeight;

  CalculateMatrices;
end;

procedure TSVG.Paint(const Graphics: TBitmap32; Rects: PRectArray;
  RectCount: Integer);
  {x2nie
  procedure PaintBounds(const Item: TSVGObject);
  var
    Pen: TGPPen;
  begin
    Graphics.ResetTransform;
    Pen := TGPPen.Create(MakeColor(0, 0, 0), 2);
    Graphics.DrawLine(Pen, Item.ObjectBounds.TopLeft.X, Item.ObjectBounds.TopLeft.Y,
      Item.ObjectBounds.TopRight.X, Item.ObjectBounds.TopRight.Y);

    Graphics.DrawLine(Pen, Item.ObjectBounds.TopRight.X, Item.ObjectBounds.TopRight.Y,
      Item.ObjectBounds.BottomRight.X, Item.ObjectBounds.BottomRight.Y);

    Graphics.DrawLine(Pen, Item.ObjectBounds.BottomRight.X, Item.ObjectBounds.BottomRight.Y,
      Item.ObjectBounds.BottomLeft.X, Item.ObjectBounds.BottomLeft.Y);

    Graphics.DrawLine(Pen, Item.ObjectBounds.BottomLeft.X, Item.ObjectBounds.BottomLeft.Y,
      Item.ObjectBounds.TopLeft.X, Item.ObjectBounds.TopLeft.Y);

    Pen.Free;
  end;
  }
  function InBounds(Item: TSVGObject): Boolean;
  var
    C: Integer;
    Bounds: TBounds;
  begin
    Result := True;
    if RectCount > 0 then
    begin
      for C := 0 to RectCount - 1 do
      begin
        Bounds := Item.ObjectBounds;
        if Intersect(Bounds, Rects^[C]) then
          Exit;
      end;
      Result := False;
    end;
  end;
  
  function NeedsPainting(Item: TSVGObject): Boolean;
  begin
    Result := (Item.Display = 1) and
       (Item.FStyle.Values['display'] <> 'none') and
       (Item.Visible = 1);
  end;
  
  procedure PaintItem(const Item: TSVGObject);
  var
    C: Integer;
  begin
    if NeedsPainting(Item) then
    begin
      if InBounds(Item) then
        Item.PaintToGraphics(Graphics);
      for C := 0 to Item.Count - 1 do
        PaintItem(Item[C]);
    end;
  end;
  
begin
  PaintItem(Self);
end;

procedure TSVG.Assign(SVG: TSVGObject);
begin
  inherited;
  if SVG is TSVG then
  begin
    FDC := TSVG(SVG).FDC;
    FRootBounds := TSVG(SVG).FRootBounds;
    FDX := TSVG(SVG).FDX;
    FDY := TSVG(SVG).FDY;
    FInitialMatrix := TSVG(SVG).FInitialMatrix;
    FViewBox := TSVG(SVG).FViewBox;
    FSource := TSVG(SVG).Source;
    FSize := TSVG(SVG).FSize;

    FreeAndNil(FStyles);
    FStyles := TSVG(SVG).FStyles.Clone;
    FFileName := TSVG(SVG).FFileName;
  end;
end;

function TSVG.New(Parent: TSVGObject): TSVGObject;
begin
  Result := TSVG.Create(Parent);
end;

procedure TSVG.ReadStyles(const Node: PXMLNode);
var
  C: Integer;
  SL: TStringList;
begin
  SL := TStringList.Create;
  for C := 0 to Node.childNodes.Count - 1 do
    if Node.childNodes[C].nodeName = '#cdata-section' then
      SL.Text := Node.childNodes[C].text;

  for C := SL.Count - 1 downto 0 do
  begin
    SL[C] := Trim(SL[C]);
    if SL[C] = '' then
      SL.Delete(C);
  end;

  for C := 0 to SL.Count - 1 do
    FStyles.Add(SL[C]);

  SL.Free;
end;

{function TSVG.RenderToBitmap(Width, Height: Integer): HBITMAP;
var
  Bitmap: TGPBitmap;
  Graphics: TBitmap32;
  R: TFloatRect;
begin
  Result := 0;
  if (Width = 0) or (Height = 0) then
    Exit;

  Bitmap := TGPBitmap.Create(Width, Height);
  try
    Graphics := TBitmap32.Create(Bitmap);
    try
      Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
      R := CalcRect(MakeRect(0.0, 0, Width, Height), FWidth, FHeight, baCenterCenter);
      try
        PaintTo(Graphics, R, nil, 0);
      except
      end;
    finally
      Graphics.Free;
    end;
    Bitmap.GetHBITMAP(MakeColor(255, 255, 255), Result);
  finally
    Bitmap.Free;
  end;
end;

function TSVG.RenderToIcon(Size: Integer): HICON;
var
  Bitmap: TGPBitmap;
  Graphics: TBitmap32;
  R: TFloatRect;
begin
  Result := 0;
  if Size = 0 then
    Exit;

  Bitmap := TGPBitmap.Create(Size, Size);
  try
    Graphics := TBitmap32.Create(Bitmap);
    try
      Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
      R := CalcRect(MakeRect(0.0, 0, Size, Size), Width, Height, baCenterCenter);
      try
        PaintTo(Graphics, R, nil, 0);
      except
      end;
    finally
      Graphics.Free;
    end;
    Bitmap.GetHICON(Result);
  finally
    Bitmap.Free;
  end;
end; }

procedure TSVG.CalcCompleteSize;

  function GetLeft(const Bounds: TBounds): TFloat;
  begin
    Result := Min(Bounds.TopLeft.X,
      Min(Bounds.TopRight.X,
        Min(Bounds.BottomLeft.X, Bounds.BottomRight.X)));
  end;

  function GetTop(const Bounds: TBounds): TFloat;
  begin
    Result := Min(Bounds.TopLeft.Y,
      Min(Bounds.TopRight.Y,
        Min(Bounds.BottomLeft.Y, Bounds.BottomRight.Y)));
  end;

  function GetRight(const Bounds: TBounds): TFloat;
  begin
    Result := Max(Bounds.TopLeft.X,
      Max(Bounds.TopRight.X,
        Max(Bounds.BottomLeft.X, Bounds.BottomRight.X)));
  end;

  function GetBottom(const Bounds: TBounds): TFloat;
  begin
    Result := Max(Bounds.TopLeft.Y,
      Max(Bounds.TopRight.Y,
        Max(Bounds.BottomLeft.Y, Bounds.BottomRight.Y)));
  end;

  procedure Walk(Item: TSVGObject);
  var
    C: Integer;
    Left, Top, Right, Bottom, Width, Height: TFloat;
  begin
    Item.CalcObjectBounds;
    Left := GetLeft(Item.FBounds);
    Top := GetTop(Item.FBounds);
    Right := GetRight(Item.FBounds);
    Bottom := GetBottom(Item.FBounds);

    Width := Right - Left;
    Height := Bottom - Top;

    FSize.Right := Max(Width, FSize.Right);
    FSize.Bottom := Max(Height, FSize.Bottom);

    for C := 0 to Item.Count - 1 do
      Walk(Item[C]);
  end;

begin
  Walk(Self);
end;

procedure TSVG.CalcRootMatrix;
var
  ViewBoxMatrix: TFloatMatrix;
  BoundsMatrix: TFloatMatrix;
  ScaleMatrix: TFloatMatrix;
begin
  FillChar(ViewBoxMatrix, SizeOf(ViewBoxMatrix), 0);
  ViewBoxMatrix[0, 0] := 1;
  ViewBoxMatrix[1, 1] := 1;
  ViewBoxMatrix[2, 0] := -FViewBox.Left;
  ViewBoxMatrix[2, 1] := -FViewBox.Top;
  ViewBoxMatrix[2, 2] := 1;

  FillChar(BoundsMatrix, SizeOf(BoundsMatrix), 0);
  BoundsMatrix[0, 0] := 1;
  BoundsMatrix[1, 1] := 1;
  BoundsMatrix[2, 0] := FRootBounds.Left;
  BoundsMatrix[2, 1] := FRootBounds.Top;
  BoundsMatrix[2, 2] := 1;

  FillChar(ScaleMatrix, SizeOf(ScaleMatrix), 0);
  ScaleMatrix[0, 0] := FDX;
  ScaleMatrix[1, 1] := FDY;
  ScaleMatrix[2, 2] := 1;

  if FInitialMatrix[2, 2] = 1 then
    FRootMatrix := FInitialMatrix else
  begin
    FillChar(FRootMatrix, SizeOf(FRootMatrix), 0);
    FRootMatrix[0, 0] := 1;
    FRootMatrix[1, 1] := 1;
    FRootMatrix[2, 2] := 1;
  end;

  FRootMatrix := Mult(FRootMatrix, BoundsMatrix);
  FRootMatrix := Mult(FRootMatrix, ViewBoxMatrix);
  FRootMatrix := Mult(FRootMatrix, ScaleMatrix);
  if FAngleMatrix[2, 2] = 1 then
    FRootMatrix := Mult(FRootMatrix, FAngleMatrix);

  if FMatrix[2, 2] = 1 then
    FRootMatrix := Mult(FRootMatrix, FMatrix);
end;

procedure TSVG.ReadIn(const Node: PXMLNode);
var
  ViewBox: WideString;
begin
  if Node.nodeName <> 'svg' then
    Exit;

  inherited;

  Display := 1;
  Visible := 1;

  FViewBox.Width := FWidth;
  FViewBox.Height := FHeight;

  ViewBox := Node.GetAttribute('viewBox');
  if ViewBox <> '' then
    FViewBox := ParseDRect(ViewBox);

  FWidth := FViewBox.Width;
  FHeight := FViewBox.Height;

  ReadChildren(Node);

  DeReferenceUse;

  CalcCompleteSize; 

  if ParseUnit(Node.GetAttribute('width')) = suPercent then
    FWidth := FSize.Right * 100 / FWidth;

  if ParseUnit(Node.GetAttribute( 'height')) = suPercent then
    FHeight := FSize.Bottom * 100 / FHeight;
end;

procedure TSVG.DeReferenceUse;
var
  Child: TSVgObject;
begin
  Child := FindByType(TSVGUse);
  while Assigned(Child) do
  begin
    TSVGUse(Child).Construct;
    Child := FindByType(TSVGUse, Child);
  end;
end;

function TSVG.GetStyleValue(const Name, Key: WideString): WideString;
var
  Style: TStyle;
begin
  Result := '';
  Style := FStyles.GetStyle(Name);
  if Assigned(Style) then
    Result := Style[Key];
end;

// TSVGContainer

function TSVGContainer.New(Parent: TSVGObject): TSVGObject;
begin
  Result := TSVGContainer.Create(Parent);
end;

procedure TSVGContainer.ReadIn(const Node: PXMLNode);
begin
  inherited;
  ReadChildren(Node);
end;

// TSVGSwitch

function TSVGSwitch.New(Parent: TSVGObject): TSVGObject;
begin
  Result := TSVGSwitch.Create(Parent);
end;

procedure TSVGSwitch.ReadIn(const Node: PXMLNode);
begin
  inherited;
  ReadChildren(Node);
end;

// TSVGDefs

function TSVGDefs.New(Parent: TSVGObject): TSVGObject;
begin
  Result := TSVGDefs.Create(Parent);
end;

procedure TSVGDefs.ReadIn(const Node: PXMLNode);
begin
  inherited;
  Display := 0;
  ReadChildren(Node);
end;

// TSVGDefs

function TSVGUse.New(Parent: TSVGObject): TSVGObject;
begin
  Result := TSVGUse.Create(Parent);
end;

procedure TSVGUse.PaintToGraphics(Graphics: TBitmap32);
begin
end;

procedure TSVGUse.PaintToPath(Path: TFlattenedPath);
var
  UseObject: TSVGBasic;
begin
  inherited;

  if FReference <> '' then
  begin
    UseObject := TSVGBasic(GetRoot.FindByID(FReference));
    if Assigned(UseObject) then
      UseObject.PaintToPath(Path);
  end;
end;

procedure TSVGUse.Construct;
var
  Container: TSVGContainer;
  SVG, Child: TSVGObject;

  Matrix: TFloatMatrix;
begin
  while Count > 0 do
    GetItem(0).Free;

  SVG := nil;
  if FReference <> '' then
  begin
    if FReference[1] = '#' then
      SVG := GetRoot.FindByID(Copy(FReference, 2, MaxInt));
  end;

  if SVG = nil then
    Exit;

  FillChar(Matrix, SizeOf(Matrix), 0);
  Matrix[0, 0] := 1;
  Matrix[0, 1] := 0;
  Matrix[1, 0] := 0;
  Matrix[1, 1] := 1;
  Matrix[2, 0] := X;
  Matrix[2, 1] := Y;
  Matrix[2, 2] := 1;

  Container := TSVGContainer.Create(Self);
  Container.FObjectName := 'g';
  Container.FMatrix := Matrix;
  SVG := SVG.Clone(Container);

  Child := SVG.FindByType(TSVGUse);
  while Assigned(Child) do
  begin
    TSVGUse(Child).Construct;
    Child := SVG.FindByType(TSVGUse);
  end;
end;

procedure TSVGUse.Assign(SVG: TSVGObject);
begin
  inherited;
  if SVG is TSVGUse then
  begin
    FReference := TSVGUse(SVG).FReference;
  end;
end;

procedure TSVGUse.Clear;
begin
  inherited;
  FReference := '';
end;

procedure TSVGUse.ReadIn(const Node: PXMLNode);
begin
  inherited;
  LoadString(Node, 'xlink:href', FReference);
end;

// TSVGRect

procedure TSVGRect.ReadIn(const Node: PXMLNode);
begin
  inherited;

  if FRX > FWidth / 2 then
    FRX := FWidth / 2;

  if FRY > FHeight / 2 then
    FRY := FHeight / 2;

  ConstructPath;
end;

function TSVGRect.New(Parent: TSVGObject): TSVGObject;
begin
  Result := TSVGRect.Create(Parent);
end;

procedure TSVGRect.CalcObjectBounds;
var
  SW: TFloat;
begin
  SW := Max(0, GetStrokeWidth) / 2;
  FBounds.TopLeft := Transform(FX - SW, FY - SW);
  FBounds.TopRight := Transform(FX + FWidth + SW, FY - SW);
  FBounds.BottomRight := Transform(FX + FWidth + SW, FY + Height + SW);
  FBounds.BottomLeft := Transform(FX - SW, FY + FHeight + SW);
end;

procedure TSVGRect.ConstructPath;
begin
  inherited;
  FPath := TFlattenedPath.Create;

  if (FRX <= 0) and (FRY <= 0) then
    FPath.Rectangle(FloatRect(FX, FY, FX + FWidth, FY + FHeight))
  else
    FPath.RoundRect(FloatRect( FX, FY, FX+FWidth, FY+FHeight), FRX {, FRY});
end;

// TSVGLine

procedure TSVGLine.ReadIn(const Node: PXMLNode);
begin
  inherited;

  LoadLength(Node, 'x1', FX);
  LoadLength(Node, 'y1', FY);
  LoadLength(Node, 'x2', FWidth);
  LoadLength(Node, 'y2', FHeight);

  ConstructPath;
end;

function TSVGLine.New(Parent: TSVGObject): TSVGObject;
begin
  Result := TSVGLine.Create(Parent);
end;

procedure TSVGLine.CalcObjectBounds;
var
  SW: TFloat;
  Left, Top, Right, Bottom: TFloat;
begin
  SW := Max(0, GetStrokeWidth) / 2;
  Left := Min(X, Width) - SW;
  Top := Min(Y, Height) - SW;
  Right := Max(X, Width) + SW;
  Bottom := Max(Y, Height) + SW;
  FBounds.TopLeft := Transform(Left, Top);
  FBounds.TopRight := Transform(Right, Top);
  FBounds.BottomRight := Transform(Right, Bottom);
  FBounds.BottomLeft := Transform(Left, Bottom);
end;

procedure TSVGLine.ConstructPath;
begin
  inherited;
  FPath := TFlattenedPath.Create;
  //FPath.Line(X, Y, Width, Height);
  FPath.MoveTo(X,Y);
  FPath.LineTo(Width, Height);
  FPath.ClosePath;
end;

// TSVGPolyLine

constructor TSVGPolyLine.Create;
begin
  inherited;
  FPointCount := 0;
end;

function TSVGPolyLine.GetPoints: PArrayOfFloatPoint;
begin
  Result := @FPoints;
end;

procedure TSVGPolyLine.CalcObjectBounds;
var
  Left, Top, Right, Bottom: TFloat;
  C: Integer;
  SW: TFloat;
begin
  Left := MaxTFloat;
  Top := MaxTFloat;
  Right := -MaxTFloat;
  Bottom := -MaxTFloat;
  for C := 0 to FPointCount - 1 do
  begin
    if FPoints[C].X < Left then
      Left := FPoints[C].X;

    if FPoints[C].X > Right then
      Right := FPoints[C].X;

    if FPoints[C].Y < Top then
      Top := FPoints[C].Y;

    if FPoints[C].Y > Bottom then
      Bottom := FPoints[C].Y;
  end;

  SW := Max(0, GetStrokeWidth) / 2;
  FBounds.TopLeft := Transform(Left - SW, Top - SW);
  FBounds.TopRight := Transform(Right + SW, Top - SW);
  FBounds.BottomRight := Transform(Right + SW, Bottom + SW);
  FBounds.BottomLeft := Transform(Left - SW, Bottom + SW);
end;

procedure TSVGPolyLine.Clear;
begin
  inherited;

  SetLength(FPoints, 0);
  FPointCount := 0;
end;

procedure TSVGPolyLine.Assign(SVG: TSVGObject);
var
  C: Integer;
begin
  inherited;
  if SVG is TSVGPolyLine then
  begin
    FPointCount := TSVGPolyLine(SVG).FPointCount;

    if Assigned(TSVGPolyLine(SVG).FPoints) then
    begin
      SetLength(FPoints, FPointCount);
      for C := 0 to TSVGPolyLine(SVG).FPointCount - 1 do
      begin
        FPoints[C].X := TSVGPolyLine(SVG).FPoints[C].X;
        FPoints[C].Y := TSVGPolyLine(SVG).FPoints[C].Y;
      end;
    end;
  end;
end;

function TSVGPolyLine.New(Parent: TSVGObject): TSVGObject;
begin
  Result := TSVGPolyLine.Create(Parent);
end;

procedure TSVGPolyLine.ConstructPoints(const S: WideString);
var
  SL: TStringList;
  C: Integer;
begin
  SL := TStringList.Create;
  SL.Delimiter := ' ';
  SL.DelimitedText := S;

  for C := SL.Count - 1 downto 0 do
    if SL[C] = '' then
      SL.Delete(C);

  if SL.Count mod 2 = 1 then
  begin
    SL.Free;
    Exit;
  end;

  SetLength(FPoints, 0);

  FPointCount := SL.Count div 2;
  SetLength(FPoints, FPointCount);

  for C := 0 to FPointCount - 1 do
  begin
    if not TryStrToTFloat(SL[C * 2], FPoints[C].X) then
      FPoints[C].X := 0;
    if not TryStrToTFloat(SL[C * 2 + 1], FPoints[C].Y) then
      FPoints[C].Y := 0;
  end;

  SL.Free;
end;

procedure TSVGPolyLine.ReadIn(const Node: PXMLNode);
var
  S: WideString;
begin
  inherited;

  LoadString(Node, 'points', S);

  S := StringReplace(S, ',', ' ', [rfReplaceAll]);
  S := StringReplace(S, '-', ' -', [rfReplaceAll]);
  S := StringReplace(S, 'e -', 'e-', [rfReplaceAll]);

  ConstructPoints(S);

  ConstructPath;
end;

procedure TSVGPolyLine.ConstructPath;
var
  C: Integer;
begin
  inherited;
  if FPoints = nil then
    Exit;

  FPath := TFlattenedPath.Create;

  //for C := 1 to FPointCount - 1 do
    //FPath.AddLine(FPoints[C - 1].X, FPoints[C - 1].Y, FPoints[C].X, FPoints[C].Y);
  FPath.Polygon(FPoints);
  FPath.ClosePath;  
end;


// TSVGPolygon

function TSVGPolygon.New(Parent: TSVGObject): TSVGObject;
begin
  Result := TSVGPolygon.Create(Parent);
end;

procedure TSVGPolygon.ConstructPath;
begin
  inherited;

  if FPoints = nil then
    Exit;

  FPath.ClosePath;
end;


// TSVGEllipse

procedure TSVGEllipse.ReadIn(const Node: PXMLNode);
var
  rx,ry : TFloat;
begin
  inherited;

  LoadLength(Node, 'cx', FX);
  LoadLength(Node, 'cy', FY);

  if Node.NodeName = 'circle' then
  begin
    LoadLength(Node, 'r', rx);
    ry := rx;
  end else
  begin
    LoadLength(Node, 'rx', rx);
    LoadLength(Node, 'ry', ry);
  end;

  FWidth  := rx * 2;
  FHeight := ry * 2;

  ConstructPath;
end;

function TSVGEllipse.New(Parent: TSVGObject): TSVGObject;
begin
  Result := TSVGEllipse.Create(Parent);
end;

procedure TSVGEllipse.CalcObjectBounds;
var
  SW: TFloat;
begin
  SW := Max(0, GetStrokeWidth) / 2;
  FBounds.TopLeft := Transform(X - Width - SW, Y - Height - SW);
  FBounds.TopRight := Transform(X + Width + SW, Y - Height - SW);
  FBounds.BottomRight := Transform(X + Width + SW, Y + Height + SW);
  FBounds.BottomLeft := Transform(X - Width - SW, Y + Height + SW);
end;

procedure TSVGEllipse.ConstructPath;
begin
  inherited;
  FPath := TFlattenedPath.Create;
  FPath.Ellipse(X,Y, Width/2, Height/2);
end;

// TSVGPath

procedure TSVGPath.CalcObjectBounds;
var
  C: Integer;
  R: TFRect;
  Left, Top, Right, Bottom: TFloat;
  Found: Boolean;
  SW: TFloat;
begin
  Left := MaxTFloat;
  Top := MaxTFloat;
  Right := -MaxTFloat;
  Bottom := -MaxTFloat;
  Found := False;

  for C := 0 to Count - 1 do
  begin
    R := TSVGPathElement(Items[C]).GetBounds;
    if (R.Width <> 0) or (R.Height <> 0) then
    begin
      Found := True;
      Left := Min(Left, R.Left);
      Top := Min(Top, R.Top);
      Right := Max(Right, R.Left + R.Width);
      Bottom := Max(Bottom, R.Top + R.Height);
    end;
  end;

  if not Found then
  begin
    Left := 0;
    Top := 0;
    Right := 0;
    Bottom := 0;
  end;

  SW := Max(0, GetStrokeWidth) / 2;
  FBounds.TopLeft := Transform(Left - SW, Top - SW);
  FBounds.TopRight := Transform(Right + SW, Top - SW);
  FBounds.BottomRight := Transform(Right + SW, Bottom + SW);
  FBounds.BottomLeft := Transform(Left - SW, Bottom + SW);
end;

procedure TSVGPath.ConstructPath;
var
  C: Integer;
  Element: TSVGPathElement;
begin
  inherited;

  FPath := TFlattenedPath.Create;
  for C := 0 to Count - 1 do
  begin
    Element := TSVGPathElement(Items[C]);
    Element.AddToPath(FPath);
  end;
  FPath.ClosePath;
end;

function TSVGPath.New(Parent: TSVGObject): TSVGObject;
begin
  Result := TSVGPath.Create(Parent);
end;

procedure TSVGPath.PrepareMoveLineCurveArc(SL: TStringList);
var
  C, D: Integer;
  S: AnsiString;
begin
  S := SL[0];

  if SL[0] = 'M' then
    S := 'L';
  if SL[0] = 'm' then
    S := 'l';

  D := 0;

  if (S = 'A') or (S = 'a') then
    D := 7;
  if (S = 'C') or (S = 'c') then
    D := 6;
  if (S = 'S') or (S = 's') or
     (S = 'Q') or (S = 'q') then
    D := 4;
  if (S = 'T') or (S = 't') or
     (S = 'M') or (S = 'm') or
     (S = 'L') or (S = 'l') then
    D := 2;
  if (S = 'H') or (S = 'h') or
     (S = 'V') or (S = 'v') then
    D := 1;

  if (D = 0) or (SL.Count = D + 1) or
     ((SL.Count - 1) mod D = 1) then
    Exit;

  for C := SL.Count - D downto (D + 1) do
    if (C - 1) mod D = 0 then
      SL.Insert(C, S);
end;

function TSVGPath.SeparateValues(const S: WideString): TStringList;
var
  C: Integer;
  Help: WideString;
begin
  Help := S;
  Insert(' ', Help, 2);
  Help := StringReplace(Help, '-', ' -', [rfReplaceAll]);
  Help := StringReplace(Help, 'e -', '-', [rfReplaceAll]);

  Result := TStringList.Create;
  Result.Delimiter := ' ';

  Result.DelimitedText := Help;

  for C := Result.Count - 1 downto 0 do
    if Result[C] = '' then
      Result.Delete(C);

  if Result.Count > 0 then
  begin
    if (Result[0] = 'M') or (Result[0] = 'm') or
       (Result[0] = 'L') or (Result[0] = 'l') or
       (Result[0] = 'H') or (Result[0] = 'h') or
       (Result[0] = 'V') or (Result[0] = 'v') or
       (Result[0] = 'C') or (Result[0] = 'c') or
       (Result[0] = 'S') or (Result[0] = 's') or
       (Result[0] = 'Q') or (Result[0] = 'q') or
       (Result[0] = 'T') or (Result[0] = 't') or
       (Result[0] = 'A') or (Result[0] = 'a') then
      PrepareMoveLineCurveArc(Result);

    if (Result[0] = 'Z') or (Result[0] = 'z') then
      while Result.Count > 1 do
        Result.Delete(1);
  end;
end;

function TSVGPath.Split(const S: WideString): TStringList;
var
  C, D: Integer;
  Part: WideString;
  Help: WideString;
  SL: TStringList;
  Found: Integer;

  function IsID(Ch: WideChar): Boolean;
  const
    IDs: array [0..19] of WideChar = ('M', 'm', 'L', 'l', 'H', 'h', 'V', 'v',
      'C', 'c', 'S', 's', 'Q', 'q', 'T', 't', 'A', 'a', 'Z', 'z');
  var
    C: Integer;
  begin
    Result := True;
    for C := 0 to 19 do
     if Ch = IDs[C] then
       Exit;
     Result := False;
  end;

begin
  Result := TStringList.Create;

  Help := S;
  while Help <> '' do
  begin
    Found := Length(Help) + 1;
    for C := 2 to Length(Help) do
      if IsID(Help[C]) then
      begin
        Found := C;
        Break;
      end;

    Part := Trim(Copy(Help, 1, Found - 1));
    SL := SeparateValues(Part);
    for D := 0 to SL.Count - 1 do
      Result.Add(SL[D]);
    SL.Free;
    Help := Trim(Copy(Help, Found, Length(Help)));
  end;
end;

procedure TSVGPath.ReadIn(const Node: PXMLNode);
var
  S: WideString;
  SL: TStringList;
  C: Integer;

  Element, LastElement: TSVGPathElement;
begin
  inherited;

  LoadString(Node, 'd', S);
  S := StringReplace(S, ',', ' ', [rfReplaceAll]);
  SL := Split(S);

  C := 0;
  LastElement := nil;

  if SL.Count > 0 then
    repeat
      case SL[C][1] of
        'M', 'm': Element := TSVGPathMove.Create(Self);

        'L', 'l': Element := TSVGPathLine.Create(Self);

        'H', 'h', 'V', 'v': Element := TSVGPathLine.Create(Self);

        'C', 'c': Element := TSVGPathCurve.Create(Self);

        'S', 's', 'Q', 'q': Element := TSVGPathCurve.Create(Self);

        'T', 't': Element := TSVGPathCurve.Create(Self);

        'A', 'a': Element := TSVGPathEllipticArc.Create(Self);

        'Z', 'z': Element := TSVGPathClose.Create(Self);

        else
          Element := nil;
      end;

      if Assigned(Element) then
      begin
        Element.Read(SL, C, LastElement);
        LastElement := Element;
      end;
      Inc(C);
    until C = SL.Count;
  SL.Free;

  ConstructPath;
end;


// TSVGImage

constructor TSVGImage.Create;
begin
  inherited;
  {$IFDEF GPPen}FImage := nil;{$ENDIF}
  FStream := nil;
end;

procedure TSVGImage.CalcObjectBounds;
var
  SW: TFloat;
begin
  SW := Max(0, GetStrokeWidth) / 2;
  FBounds.TopLeft := Transform(X - SW, Y - SW);
  FBounds.TopRight := Transform(X + Width + SW, Y - SW);
  FBounds.BottomRight := Transform(X + Width + SW, Y + Height + SW);
  FBounds.BottomLeft := Transform(X - SW, Y + Height - SW);
end;

procedure TSVGImage.Clear;
begin
  inherited;
  //x2nieFreeAndNil(FImage);
  FreeAndNil(FStream);
  FFileName := '';
end;

procedure TSVGImage.Assign(SVG: TSVGObject);
var
  SA: TStreamAdapter;
begin
  inherited;
  if SVG is TSVGImage then
  begin
    FFileName := TSVGImage(SVG).FFileName;
    if Assigned(TSVGImage(SVG).FStream) then
    begin
      FStream := TMemoryStream.Create;
      TSVGImage(SVG).FStream.Position := 0;
      FStream.LoadFromStream(TSVGImage(SVG).FStream);
      FStream.Position := 0;
      SA := TStreamAdapter.Create(FStream, soReference);
      //x2nieFImage := TGPImage.Create(SA);
    end else
    begin
      FStream := TMemoryStream.Create;
      FStream.LoadFromFile(FFileName);
      FStream.Position := 0;
      SA := TStreamAdapter.Create(FStream, soReference);
      //x2nieFImage := TGPImage.Create(SA);
    end;
  end;
end;

function TSVGImage.New(Parent: TSVGObject): TSVGObject;
begin
  Result := TSVGImage.Create(Parent);
end;

procedure TSVGImage.PaintToGraphics(Graphics: TBitmap32);
var
  //ClipPath: TGPGraphicsPath;
  TGP: TFloatMatrix;
  //x2nieImAtt: TGPImageAttributes;
  //x2nieColorMatrix: TColorMatrix;

begin
  //x2nieif FImage = nil then
    Exit;

  {ClipPath := GetClipPath;

  if ClipPath <> nil then
    Graphics.SetClip(ClipPath);}

  {x2nie
  TGP := GetGPMatrix(Matrix);
  Graphics.SetTransform(TGP);
  TGP.Free;

  FillChar(ColorMatrix, Sizeof(ColorMatrix), 0);
  ColorMatrix[0, 0] := 1;
  ColorMatrix[1, 1] := 1;
  ColorMatrix[2, 2] := 1;
  ColorMatrix[3, 3] := GetFillOpacity;
  ColorMatrix[4, 4] := 1;

  ImAtt := TGPImageAttributes.Create;
  ImAtt.SetColorMatrix(colorMatrix, ColorMatrixFlagsDefault,
    ColorAdjustTypeDefault);

  Graphics.DrawImage(FImage, MakeRect(X, Y, Width, Height),
    0, 0, FImage.GetWidth, FImage.GetHeight, UnitPixel, ImAtt);

  ImAtt.Free;

  Graphics.ResetTransform;
  Graphics.ResetClip;
  }
  //FreeAndNil(ClipPath);
end;

procedure TSVGImage.ReadIn(const Node: PXMLNode);
var
  S: WideString;
  SA: TStreamAdapter;

  function IsValid(var S: WideString): Boolean;
  var
    Semicolon: Integer;
  begin
    Result := False;
    if Copy(S, 1, 5) <> 'data:' then
      Exit;
    S := Copy(S, 6, MaxInt);
    Semicolon := Pos(';', S);
    if Semicolon = 0 then
      Exit;
    if Copy(S, Semicolon, 8) = ';base64,' then
    begin
      S := Copy(S, Semicolon + 8, MaxInt);
      Result := True;
    end;
  end;

begin
  inherited;

  LoadString(Node, 'xlink:href', S);
  {x2nie
  if IsValid(S) then
  begin
    FStream := TMemoryStream.Create;
    DecodeBase64(S, FStream);
    FStream.Position := 0;
    SA := TStreamAdapter.Create(FStream, soReference);
    FImage := TGPImage.Create(SA);
    FImage.GetLastStatus;
  end
  else
  begin
    FFileName := S;
    FStream := TMemoryStream.Create;
    FStream.LoadFromFile(FFileName);
    FStream.Position := 0;
    SA := TStreamAdapter.Create(FStream, soReference);
    FImage := TGPImage.Create(SA);
    FImage.GetLastStatus;
  end;
  }
end;


// TSVGText
{$IFDEF GPTEXT}
constructor TSVGCustomText.Create;
begin
  inherited;
  FDX := 0;
  FDY := 0;
end;

{$IFDEF gppen}
procedure TSVGCustomText.BeforePaint(const Graphics: TBitmap32;
  const Brush: TGPBrush; const Pen: TGPPen);
begin
  inherited;
  if Assigned(FUnderlinePath) then
  begin
    if Assigned(Brush) and (Brush.GetLastStatus = OK) then
      Graphics.FillPath(Brush, FUnderlinePath);

    if Assigned(Pen) and (Pen.GetLastStatus = OK) then
      Graphics.DrawPath(Pen, FUnderlinePath);
  end;
end;
{$ENDIF}

procedure TSVGCustomText.CalcObjectBounds;
var
  SW: TFloat;
begin
  SW := Max(0, GetStrokeWidth) / 2;
  FBounds.TopLeft := Transform(X - SW, Y - FFontHeight - SW);
  FBounds.TopRight := Transform(X + Width + SW, Y - FFontHeight - SW);
  FBounds.BottomRight := Transform(X + Width + SW, Y - FFontHeight + Height + SW);
  FBounds.BottomLeft := Transform(X - SW, Y - FFontHeight + Height + SW);
end;

procedure TSVGCustomText.Clear;
begin
  inherited;
  FreeAndNil(FUnderlinePath);
  FreeAndNil(FStrikeOutPath);
  FText := '';
  FFontHeight := 0;
  FDX := 0;
  FDY := 0;
end;

function TSVGCustomText.GetCompleteWidth: TFloat;
var
  C: Integer;
begin
  Result := Width;
  for C := 0 to Count - 1 do
    if GetItem(C) is TSVGCustomText then
      Result := Result + TSVGCustomText(GetItem(C)).GetCompleteWidth;
end;

function TSVGCustomText.GetFont: TFont;
var
  //x2nieFF: TGPFontFamily;
  FontStyle: TFontStyle;
  TD: TTextDecoration;
//  Font: HFont;

{  function CreateFont: HFont;
  var
    LogFont: TLogFont;
  begin
    with LogFont do
    begin
      lfHeight := Round(GetFont_Size);
      lfWidth := 0;
      lfEscapement := 0;
      lfOrientation := 0;
      lfWeight := GetFont_Weight;

      lfItalic := GetFont_Style;

      TD := GetText_Decoration;

      if tdUnderLine in TD then
        lfUnderline := 1
      else
        lfUnderline := 0;

      if tdStrikeOut in TD then
        lfStrikeOut := 1
      else
        lfStrikeOut := 0;

      lfCharSet := 1;
      lfOutPrecision := OUT_DEFAULT_PRECIS;
      lfClipPrecision := CLIP_DEFAULT_PRECIS;
      lfQuality := DEFAULT_QUALITY;
      lfPitchAndFamily := DEFAULT_PITCH;
      StrPCopy(lfFaceName, GetFont_Name);
    end;
    Result := CreateFontIndirect(LogFont);
  end;}

begin
  {x2nieFF := GetFontFamily(GetFontName);

  FontStyle := FontStyleRegular;
  if GetFontWeight = FW_BOLD then
    FontStyle := FontStyle or FontStyleBold;

  if GetFontStyle = 1 then
    FontStyle := FontStyle or FontStyleItalic;

  TD := GetTextDecoration;

  if tdUnderLine in TD then
    FontStyle := FontStyle or FontStyleUnderline;

  if tdStrikeOut in TD then
    FontStyle := FontStyle or FontStyleStrikeout;

  FFontHeight := FF.GetCellAscent(FontStyle) / FF.GetEmHeight(FontStyle);
  FFontHeight := FFontHeight * GetFontSize;

  Result := TGPFont.Create(FF, GetFontSize, FontStyle, UnitPixel);
  FF.Free;
  }
end;
{$IFDEF GPPen}
function TSVGCustomText.GetFontFamily(const FontName: WideString): TGPFontFamily;
var
  FF: TGPFontFamily;
  C: Integer;
  FN: WideString;
begin
  FF := TGPFontFamily.Create(FontName);
  if FF.GetLastStatus <> OK then
  begin
    FreeAndNil(FF);

    C := Pos('-', FontName);
    if (C <> 0) then
    begin
      FN := Copy(FontName, 1, C - 1);
      FF := TGPFontFamily.Create(FN);
      if FF.GetLastStatus <> OK then
        FreeAndNil(FF);
    end;
  end;
  if not Assigned(FF) then
    FF := TGPFontFamily.Create('Arial');

  Result := FF;
end;
{$ENDIF}
function TSVGCustomText.IsInTextPath: Boolean;
var
  Item: TSVGObject;
begin
  Result := True;
  Item := Self;
  while Assigned(Item) do
  begin
    if Item is TSVGTextPath then
      Exit;
    Item := Item.Parent;
  end;
  Result := False;
end;

procedure TSVGCustomText.SetSize;
var
  Graphics: TBitmap32;
  SF: TGPStringFormat;
  Font: TGPFont;
  Rect: TFloatRect;
  Index: Integer;
  Previous: TSVGCustomText;
  DC: HDC;
begin
  DC := GetDC(0);
  Graphics := TBitmap32.Create(DC);

  Font := GetFont;

  SF := TGPStringFormat.Create(StringFormatFlagsMeasureTrailingSpaces);

  Graphics.MeasureString(FText, -1, Font, MakePoint(0.0, 0), SF, Rect);

  Rect.Width := KerningText.MeasureText(FText, Font);

  SF.Free;

  Graphics.Free;
  ReleaseDC(0, DC);

  Font.Free;

  FWidth := 0;
  FHeight := 0;

  if Assigned(FParent) and (FParent is TSVGCustomText) then
  begin
    Index := FParent.IndexOf(Self);

    Previous := nil;
    if (Index > 0) and (FParent[Index - 1] is TSVGCustomText) then
      Previous := TSVGCustomText(FParent[Index - 1]);

    if (Index = 0) and (FParent is TSVGCustomText) then
      Previous := TSVGCustomText(FParent);

    if Assigned(Previous) then
    begin
      if not FHasX then
        FX := Previous.X + Previous.GetCompleteWidth;

      if not FHasY then
        FY := Previous.Y;
    end;
  end;

  FX := FX + FDX;
  FY := FY + FDY;

  FWidth := Rect.Width;
  FHeight := Rect.Height;
end;

procedure TSVGCustomText.AfterPaint(const Graphics: TBitmap32;
  const Brush: TGPBrush; const Pen: TGPPen);
begin
  inherited;
  if Assigned(FStrikeOutPath) then
  begin
    if Assigned(Brush) and (Brush.GetLastStatus = OK) then
      Graphics.FillPath(Brush, FStrikeOutPath);

    if Assigned(Pen) and (Pen.GetLastStatus = OK) then
      Graphics.DrawPath(Pen, FStrikeOutPath);
  end;
end;

procedure TSVGCustomText.Assign(SVG: TSVGObject);
begin
  inherited;
  if SVG is TSVGCustomText then
  begin
    FText := TSVGCustomText(SVG).FText;
    FFontHeight := TSVGCustomText(SVG).FFontHeight;
    FDX := TSVGCustomText(SVG).FDX;
    FDY := TSVGCustomText(SVG).FDY;
  end;
end;

function TSVGCustomText.New(Parent: TSVGObject): TSVGObject;
begin
  Result := TSVGCustomText.Create(Parent);
end;

procedure TSVGCustomText.ConstructPath;
var
  FF: TGPFontFamily;
  FontStyle: TFontStyle;
  SF: TGPStringFormat;
  TD: TTextDecoration;
begin
  inherited;
  FreeAndNil(FUnderlinePath);
  FreeAndNil(FStrikeOutPath);

  if IsInTextPath then
    Exit;
  
  if FText = '' then
    Exit;
  FPath := TFlattenedPath.Create;

  FF := GetFontFamily(GetFontName);

  FontStyle := FontStyleRegular;
  if FFontWeight = FW_BOLD then
    FontStyle := FontStyle or FontStyleBold;

  if GetFontStyle = 1 then
    FontStyle := FontStyle or FontStyleItalic;

  TD := GetTextDecoration;

  if tdUnderLine in TD then
  begin
    FontStyle := FontStyle or FontStyleUnderline;
    FUnderlinePath := TArrayOfArrayOfFloatPoint.Create;
  end;

  if tdStrikeOut in TD then
  begin
    FontStyle := FontStyle or FontStyleStrikeout;
    FStrikeOutPath := TArrayOfArrayOfFloatPoint.Create;
  end;

  SF := TGPStringFormat.Create(TGPStringFormat.GenericTypographic);
  SF.SetFormatFlags(StringFormatFlagsMeasureTrailingSpaces);

  KerningText.AddToPath(FPath, FUnderlinePath, FStrikeOutPath,
    FText, FF, FontStyle, GetFontSize,
    MakePoint(X, Y - FFontHeight), SF);

  SF.Free;
  FF.Free;
end;

procedure TSVGCustomText.PaintToGraphics(Graphics: TBitmap32);
{$IFDEF USE_TEXT}
var
  Font: TGPFont;
  SF: TGPStringFormat;
  Brush: TGPBrush;

  TGP: TFloatMatrix;
  ClipRoot: TSVGBasic;
{$ENDIF}
begin
  if FText = '' then
    Exit;

{$IFDEF USE_TEXT}
  if FClipPath = nil then
    CalcClipPath;

  try
    if Assigned(FClipPath) then
    begin
      if ClipURI <> '' then
      begin
        ClipRoot := TSVGBasic(GetRoot.FindByID(ClipURI));
        if Assigned(ClipRoot) then
        begin
          TGP := GetGPMatrix(ClipRoot.Matrix);
          Graphics.SetTransform(TGP);
          TGP.Free;
        end;
      end;
      try
        Graphics.SetClip(FClipPath);
      except
      end;
      Graphics.ResetTransform;
    end;

    TGP := GetGPMatrix(Matrix);
    Graphics.SetTransform(TGP);
    TGP.Free;

    SF := TGPStringFormat.Create(TGPStringFormat.GenericTypographic);
    SF.SetFormatFlags(StringFormatFlagsMeasureTrailingSpaces);

    Brush := GetFillBrush;
    if Assigned(Brush) and (Brush.GetLastStatus = OK) then
    try
      Font := GetFont;
      try
        KerningText.AddToGraphics(Graphics, FText, Font, MakePoint(X, Y - FFontHeight), SF, Brush);
      finally
        Font.Free;
      end;
    finally
      Brush.Free;
    end;

    SF.Free;
  finally
    Graphics.ResetTransform;
    Graphics.ResetClip;
  end;
{$ELSE}
  inherited;
{$ENDIF}
end;

procedure TSVGCustomText.ParseNode(const Node: PXMLNode);
const
  TAB = #8;
var
  Text: TSVGText;
  TSpan: TSVGTSpan;
  TextPath: TSVGTextPath;
begin
  if Node.NodeName = '#text' then
  begin
    Text := TSVGTSpan.Create(Self);
    Text.Assign(Self);
    Text.FText := GetXmlText(Node, True);
    Text.SetSize;
    Text.ConstructPath;
    Exit;
  end;

  if Node.NodeName = 'text' then
  begin
    Text := TSVGTSpan.Create(Self);
    Text.Assign(Self);
    FillChar(Text.FMatrix, SizeOf(Text.FMatrix), 0);
    Text.ReadIn(Node);
  end;

  if Node.NodeName = 'tspan' then
  begin
    TSpan := TSVGTSpan.Create(Self);
    TSpan.Assign(Self);
    FillChar(TSpan.FMatrix, SizeOf(TSpan.FMatrix), 0);
    TSpan.ReadIn(Node);
  end;

  if Node.NodeName = 'textPath' then
  begin
    TextPath := TSVGTextPath.Create(Self);
    TextPath.Assign(Self);
    FillChar(TextPath.FMatrix, SizeOf(TextPath.FMatrix), 0);
    TextPath.ReadIn(Node);
  end;
end;

procedure TSVGCustomText.ReadIn(const Node: PXMLNode);
begin
  inherited;

  FHasX := HasAttribute(Node, 'x');
  FHasY := HasAttribute(Node, 'y');

  LoadLength(Node, 'dx', FDX);
  LoadLength(Node, 'dy', FDY);
end;


procedure TSVGCustomText.ReadTextNodes(const Node: PXMLNode);
var
  C: Integer;
begin
  if Node.nodeType = 3 then
  begin
    FText := Node.xml;
    SetSize;
    ConstructPath;
  end
  else
  begin
    ConstructPath;
    for C := 0 to Node.childNodes.length - 1 do
      ParseNode(Node.childNodes[C]);
  end;
end;
{$ENDIF}
// TSVGClipPath

procedure TSVGClipPath.PaintToPath(Path: TFlattenedPath);
begin
end;

procedure TSVGClipPath.PaintToGraphics(Graphics: TBitmap32);
begin
end;

procedure TSVGClipPath.Clear;
begin
  inherited;
  if Assigned(FClipPath) then
    FreeAndNil(FClipPath);
end;

procedure TSVGClipPath.ConstructClipPath;

  procedure AddPath(SVG: TSVGBasic);
  var
    C: Integer;
  begin
    FClipPath.ClosePath;
    SVG.PaintToPath(FClipPath);

    for C := 0 to SVG.Count - 1 do
      AddPath(TSVGBasic(SVG[C]));
    FClipPath.ClosePath;
  end;

begin
  if Assigned(FClipPath) then
    FreeAndNil(FClipPath);
  FClipPath := TFlattenedPath.Create;
  AddPath(Self);
end;

destructor TSVGClipPath.Destroy;
begin
  if Assigned(FClipPath) then
    FreeAndNil(FClipPath);
  inherited;
end;

function TSVGClipPath.GetClipPath: TFlattenedPath;
begin
  if not Assigned(FClipPath) then
    ConstructClipPath;
  Result := FClipPath;
end;

function TSVGClipPath.New(Parent: TSVGObject): TSVGObject;
begin
  Result := TSVGClipPath.Create(Parent);
end;

procedure TSVGClipPath.ReadIn(const Node: PXMLNode);
begin
  inherited;
  ReadChildren(Node);
  Display := 0;
end;

{ TSVGTextPath }
{$IFDEF GPTEXT}
procedure TSVGTextPath.Clear;
begin
  inherited;
  FOffset := 0;
  FPathRef := '';
  FMethod := tpmAlign;
  FSpacing := tpsAuto;
end;

procedure TSVGTextPath.ConstructPath;
var
  GuidePath: TSVGPath;
  Position: TFloat;
  Offset: TFloat;
  X, Y: TFloat;

  procedure RenderTextElement(const Element: TSVGCustomText);
  var
    C: Integer;
    FF: TGPFontFamily;
    FontStyle: TFontStyle;
    SF: TGPStringFormat;
    PT: TGPPathText;
    Matrix: TFloatMatrix;
    Size: TFloat;
  begin
    FreeAndNil(Element.FUnderlinePath);
    FreeAndNil(Element.FStrikeOutPath);
    FreeAndNil(Element.FPath);
    if Element.FText <> '' then
    begin
      FF := GetFontFamily(Element.GetFontName);

      FontStyle := FontStyleRegular;
      if Element.FFontWeight = FW_BOLD then
        FontStyle := FontStyle or FontStyleBold;

      if Element.GetFontStyle = 1 then
        FontStyle := FontStyle or FontStyleItalic;

      SF := TGPStringFormat.Create(TGPStringFormat.GenericTypographic);
      SF.SetFormatFlags(StringFormatFlagsMeasureTrailingSpaces);

      PT := TGPPathText.Create(GuidePath.FPath);

      if Element.FMatrix[2, 2] = 1 then
        Matrix := GetGPMatrix(Element.FMatrix)
      else
        Matrix := nil;

      X := X + Element.FDX;
      Y := Y + Element.FDY;
      if (X <> 0) or (Y <> 0) then
      begin
        if not Assigned(Matrix) then
          Matrix := TFloatMatrix.Create;
        Matrix.Translate(X, Y);
      end;

      PT.AdditionalMatrix := Matrix;
      Element.FPath := TFlattenedPath.Create;

      Size := Element.GetFontSize;
      Position := Position +
        PT.AddPathText(Element.FPath, Trim(Element.FText), Offset + Position,
          FF, FontStyle, Size, SF);

      PT.Free;

      Matrix.Free;

      SF.Free;
      FF.Free;
    end;

    for C := 0 to Element.Count - 1 do
      if Element[C] is TSVGCustomText then
        RenderTextElement(TSVGCustomText(Element[C]));
  end;

begin
  inherited;

  GuidePath := nil;
  if FPathRef <> '' then
  begin
    if FPathRef[1] = '#' then
      GuidePath := TSVGPath(GetRoot.FindByID(Copy(FPathRef, 2, MaxInt)));
  end;

  if GuidePath = nil then
    Exit;

  Offset := 0;
  if FOffsetIsPercent and (FOffset <> 0) then
    Offset := TGPPathText.GetPathLength(GuidePath.FPath) / 100 * FOffset;

  X := FDX;
  Y := FDY;
  RenderTextElement(Self);
end;

procedure TSVGTextPath.PaintToGraphics(Graphics: TBitmap32);
begin
  inherited;

end;

procedure TSVGTextPath.ReadIn(const Node: PXMLNode);
var
  Value: WideString;
begin
  inherited;

  Value := Style.Values['startOffset'];
  if Value <> '' then
  begin
    FOffsetIsPercent := False;
    if Copy(Value, Length(Value), 1) = '%' then
    begin
      FOffsetIsPercent := True;
      Value := Copy(Value, 1, Length(Value) - 1);
    end;
    FOffset := ParseLength(Value);
  end;

  Value := Style.Values['method'];
  if Value = 'stretch' then
    FMethod := tpmStretch;

  Value := Style.Values['spacing'];
  if Value = 'exact' then
    FSpacing := tpsExact;

  LoadString(Node, 'xlink:href', FPathRef);

  ReadTextNodes(Node);
end;

procedure TSVGTextPath.ReadTextNodes(const Node: PXMLNode);
var
  C: Integer;
begin
  if (Node.nodeType = 3) then
  begin
    FText := Node.xml;
    SetSize;
  end
  else
  begin
    for C := 0 to Node.childNodes.length - 1 do
      ParseNode(Node.childNodes[C]);
  end;
  ConstructPath;
end;

{ TSVGText }

procedure TSVGText.ReadIn(const Node: PXMLNode);
begin
  inherited;

  ReadTextNodes(Node);
end;

{ TSVGTSpan }

procedure TSVGTSpan.ReadTextNodes(const Node: PXMLNode);
begin
  FText := GetXmlText(Node, True);
  SetSize;
  ConstructPath;

  // Again with only child
  if (Node.childNodes.length = 1) then
    ReadTextNodes(Node.childNodes[0]);
end;
{$ENDIF}
procedure PatchINT3;
var 
  NOP: Byte;
  NTDLL: THandle;
  BytesWritten: DWORD;
  Address: Pointer;
begin
  if Win32Platform <> VER_PLATFORM_WIN32_NT then
    Exit;
  NTDLL := GetModuleHandle('NTDLL.DLL');
  if NTDLL = 0 then
    Exit;
  Address := GetProcAddress(NTDLL, 'DbgBreakPoint');
  if Address = nil then
    Exit;
  try
    if Char(Address^) <> #$CC then
      Exit;

    NOP := $90;
    if WriteProcessMemory(GetCurrentProcess, Address, @NOP, 1, BytesWritten) and
       (BytesWritten = 1) then
      FlushInstructionCache(GetCurrentProcess, Address, 1);
  except
    //Do not panic if you see an EAccessViolation here, it is perfectly harmless!
    on EAccessViolation do ;
    else raise;
  end;
end;


{ TSVGSymbol }

function TSVGSymbol.New(Parent: TSVGObject): TSVGObject;
begin
  Result := TSVGSymbol.Create(Parent);
end;

procedure TSVGSymbol.ReadIn(const Node: PXMLNode);
var LViewBox : WideString;
begin
  inherited;
  LViewBox := Node.GetAttribute('viewBox');
  if LViewBox <> '' then
    FViewBox := ParseDRect(LViewBox);

  ReadChildren(Node);
end;

procedure TSVGSymbol.SetViewBox(const Value: TFRect);
begin
  FViewBox := Value;
end;


initialization
  {$WARN SYMBOL_PLATFORM OFF}
// nur wenn ein Debugger vorhanden, den Patch ausführen
  if DebugHook <> 0 then
    PatchINT3;
  {$WARN SYMBOL_PLATFORM ON}
end.