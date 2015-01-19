unit uMain;

interface

{$DEFINE CLIPPER_621}
{$DEFINE GR32_CLIPPER}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  GR32, GR32_Polygons, GR32_Image, GR32_RangeBars, GR32_Transforms,
  {$IFDEF CLIPPER_621}  clipper, {$ENDIF}
  {$IFDEF GR32_CLIPPER} GR32_Clipper, {$ENDIF}
  StdCtrls, ExtCtrls;

type
  TRendererMethod = procedure (Buffer : TBitmap32; const Points:TArrayOfArrayOfFloatPoint;
    ALineWidth: TFloat; AJoinStyle: TJoinStyle;  AEndStyle: TEndStyle; AMiterLimit: TFloat;
    Transformation : TTransformation) of object; 
  TfrmMain = class(TForm)
    Panel2: TPanel;
    lbZoom: TLabel;
    gau1: TGaugeBar;
    Panel1: TPanel;
    imgView1: TImgView32;
    pnlRight: TPanel;
    cbbRenderer: TComboBox;
    Label1: TLabel;
    PnlBitmapLayer: TPanel;
    LblOpacity: TLabel;
    PnlBitmapLayerHeader: TPanel;
    gbrFillOpacity: TGaugeBar;
    chkDebugRoute: TCheckBox;
    BtnLayerRescale: TButton;
    BtnLayerResetScale: TButton;
    chkNodes: TCheckBox;
    pnlStroke: TPanel;
    Label2: TLabel;
    Panel4: TPanel;
    gbrStrokeBig: TGaugeBar;
    Label3: TLabel;
    gbrStrokeSmall: TGaugeBar;
    procedure gau1Change(Sender: TObject);
    procedure imgView1PaintStage(Sender: TObject; Buffer: TBitmap32;
      StageNum: Cardinal);
    procedure FormCreate(Sender: TObject);
    procedure gau1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Rebuild(Sender: TObject);
    procedure gbrFillOpacityMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure gbrStrokeBigMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure gbrStrokeSmallMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    FStrokeWidth : array[0..1] of TFloat;

    FFillOpacity : Cardinal;
    FTransform : TAffineTransformation;
    procedure SetFillOpacity(const Value: Cardinal);
  protected
    FRenderer : array of TRendererMethod;
    procedure DoRender;

    procedure DoRender_GR32(Buffer : TBitmap32; const Points:TArrayOfArrayOfFloatPoint;
      ALineWidth: TFloat; AJoinStyle: TJoinStyle;  AEndStyle: TEndStyle; AMiterLimit: TFloat;
      Transformation : TTransformation) ;

    {$IFDEF CLIPPER_621}
    procedure DoRender_CLIPPER_621(Buffer : TBitmap32; const Points:TArrayOfArrayOfFloatPoint;
      ALineWidth: TFloat; AJoinStyle: TJoinStyle;  AEndStyle: TEndStyle; AMiterLimit: TFloat;
      Transformation : TTransformation) ;
    {$ENDIF}

    {.$IFDEF GR32_CLIPPER}
    procedure DoRender_GR32_Clipper(Buffer : TBitmap32; const Points:TArrayOfArrayOfFloatPoint;
      ALineWidth: TFloat; AJoinStyle: TJoinStyle;  AEndStyle: TEndStyle; AMiterLimit: TFloat;
      Transformation : TTransformation) ;
    {.$ENDIF}


  public
    { Public declarations }
    property FillOpacity : Cardinal read FFillOpacity write SetFillOpacity;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  acc_data, GR32_VectorUtils,
  Math
  ;
  
{$R *.dfm}



const
  ZOOM_LEVEL : array[0..12] of TFloat = (0.05, 0.10, 0.20, 0.5, 0.75, 1, 1.30, 1.60, 2, 3,4,6,8);
  svgWidth  = 1850 div 4;
  svgHeight = 1540 div 4;


procedure Dots(Buffer: TBitmap32;
  const Points: TArrayOfArrayOfFloatPoint; Transformation: TTransformation; AColor: TColor32 = clLime32);
var i,j : Integer;
  C : TArrayOfFloatPoint;
begin
  for i := 0 to Length(Points)-1 do
  for j := 0 to Length(Points[i])-1 do
  begin
    C := Circle(Points[i,j], 0.8,6);
    PolygonFS( Buffer, C, AColor and $c0ffffff, pfWinding, Transformation);
  end;

end;
  
procedure TfrmMain.FormCreate(Sender: TObject);

  procedure RegisterRenderer(AMethod : TRendererMethod; ATitle : string);
  var i : Integer;
  begin
    i := Length(FRenderer);
    SetLength(FRenderer, i+1);
    FRenderer[i] := AMethod;
    cbbRenderer.Items.Add(ATitle);
  end;

begin

  FTransform := TAffineTransformation.Create;

// RENDERER

  // GR32_Polygons using GR32_VectorUtils
  RegisterRenderer(DoRender_GR32, 'GR32 Polygon');

  {$IFDEF CLIPPER_621}
  RegisterRenderer(DoRender_CLIPPER_621, 'clipper.pas v621');
  {$ENDIF}

  //GR32_CLIPPER
  {$IFDEF GR32_CLIPPER}
  RegisterRenderer(DoRender_GR32_Clipper, 'GR32_Clipper');
  {$ENDIF}

  // by default, PST_CLEAR_BACKGND is executed at this stage,
  // which, in turn, calls ExecClearBackgnd method of ImgView.
  // Here I substitute PST_CLEAR_BACKGND with PST_CUSTOM, so force ImgView
  // to call the OnPaintStage event instead of performing default action.
  with ImgView1.PaintStages[0]^ do
  begin
    if Stage = PST_CLEAR_BACKGND then Stage := PST_CUSTOM;
  end;
  FStrokeWidth[1] := 27.63601;
  FStrokeWidth[0] := 8.736;
  FillOpacity := gbrFillOpacity.Position;
  cbbRenderer.ItemIndex := 0;
  Rebuild(Self);  
end;

procedure TfrmMain.imgView1PaintStage(Sender: TObject; Buffer: TBitmap32;
  StageNum: Cardinal);
const            //0..1
  Colors: array [Boolean] of TColor32 = ($FFFFFFFF, $FFd0d0d0);
var
  R: TRect;
  I, J: Integer;
  OddY: Integer;
  TilesHorz, TilesVert: Integer;
  TileX, TileY: Integer;
  TileHeight, TileWidth: Integer;
begin
  TileHeight := 8;
  TileWidth := 8;

  TilesHorz := Buffer.Width div TileWidth;
  TilesVert := Buffer.Height div TileHeight;
  TileY := 0;

  for J := 0 to TilesVert do
  begin
    TileX := 0;
    OddY := J and $1;
    for I := 0 to TilesHorz do
    begin
      R.Left := TileX;
      R.Top := TileY;
      R.Right := TileX + TileWidth;
      R.Bottom := TileY + TileHeight;
      Buffer.FillRectS(R, Colors[I and $1 = OddY]);
      Inc(TileX, TileWidth);
    end;
    Inc(TileY, TileHeight);
  end;
  Buffer.FrameRectS(imgView1.GetBitmapRect, clTrBlue32);
end;

procedure TfrmMain.Rebuild;
var W,H : Integer;
begin
  imgView1.BeginUpdate;
  try
    W := Round(SVGWidth * ZOOM_LEVEL[ gau1.Position]);
    H := Round(SVGHeight * ZOOM_LEVEL[gau1.Position]);
    (*
    SVG.PaintTo(imgView1.Bitmap,
      FloatRect( (imgView1.Bitmap.Width - W) / 2, (imgView1.Bitmap.Height - H) / 2, W, H), nil, 0);
    *)
    imgView1.Bitmap.SetSize(W, H);
    imgView1.Bitmap.Clear($38FFFFFF);

    //SVG.PaintTo( imgView1.Bitmap, FloatRect( imgView1.Bitmap.BoundsRect), nil, 0  );
    DoRender();

  finally
    imgView1.EndUpdate;
    imgView1.Invalidate;
  end;
end;


procedure TfrmMain.gau1Change(Sender: TObject);
begin
  lbZoom.Caption := Format('Zoom: %.0f%%',[ZOOM_LEVEL[gau1.Position]*100]);
end;

procedure TfrmMain.gau1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FTransform.Clear;
  FTransform.Scale(ZOOM_LEVEL[gau1.Position]);
  Rebuild(Sender);
end;

procedure TfrmMain.DoRender;
const
  LJoinStyle : array[0..3] of TJoinStyle = (jsMiter, jsRound, jsMiter, jsRound);
  LLineWidth : array[0..3] of TFloat = (8.736, 27.63601, 8.736, 27.63601);
var i : Integer;
begin
  for i := 0 to Length(AccData) -1 do
  begin
    FRenderer[cbbRenderer.ItemIndex](imgView1.Bitmap, AccData[i],
      //LLineWidth[i],
      FStrokeWidth[i mod 2],
      LJoinStyle[i], esButt, 4, FTransform );

        //original path
    PolyPolylineFS( imgView1.Bitmap, AccData[i], clTrBlack32, //
      False, 2, jsMiter, esButt, 4, FTransform
      //Assigned(Brush), 1, Self.StrokeLineJoin  ,Self.StrokeLineCap, self.StrokeMiterLimit, TGP
      );
    Dots(imgView1.Bitmap, AccData[i], FTransform, ClYellow32);
  end;
//  FRenderer[cbbRenderer.ItemIndex]();
end;

procedure TfrmMain.DoRender_GR32(Buffer : TBitmap32; const Points:TArrayOfArrayOfFloatPoint;
      ALineWidth: TFloat; AJoinStyle: TJoinStyle;  AEndStyle: TEndStyle; AMiterLimit: TFloat;
      Transformation : TTransformation);
var
  Dst : TArrayOfArrayOfFloatPoint;
begin
    PolyPolylineFS( Buffer, Points, clBlue32 and FillOpacity, True, ALineWidth,
      AJoinStyle, AEndStyle, AMiterLimit, Transformation);

  Dst := BuildPolyPolyLine(Points, True, ALineWidth, AJoinStyle  , AEndStyle, AMiterLimit  );
  //PolyPolylineFS( Graphics, Dst, clTrRed32, True,1,jsMiter, esButt, 4, TGP);

  if chkDebugRoute.Checked then
    PolyPolylineFS(Buffer, Dst, clBlue32, True, 1, jsMiter, esButt, 2, Transformation);

  if chkNodes.Checked then
    Dots(Buffer, Dst, Transformation);

end;


{$IFDEF CLIPPER_621}

//taken from Clipper demo
function AAFloatPoint2AAPoint(const a: TArrayOfArrayOfFloatPoint;
  decimals: integer = 0): TPaths;
var
  i,j,decScale: integer;
begin
  decScale := round(power(10,decimals));
  setlength(result, length(a));
  for i := 0 to high(a) do
  begin
    setlength(result[i], length(a[i]));
    for j := 0 to high(a[i]) do
    begin
      result[i][j].X := round(a[i][j].X *decScale);
      result[i][j].Y := round(a[i][j].Y *decScale);
    end;
  end;
end;
//------------------------------------------------------------------------------

function AAPoint2AAFloatPoint(const a: TPaths;
  decimals: integer = 0): TArrayOfArrayOfFloatPoint;
var
  i,j,decScale: integer;
begin
  decScale := round(power(10,decimals));
  setlength(result, length(a));
  for i := 0 to high(a) do
  begin
    setlength(result[i], length(a[i]));
    for j := 0 to high(a[i]) do
    begin
      result[i][j].X := a[i][j].X /decScale;
      result[i][j].Y := a[i][j].Y /decScale;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TfrmMain.DoRender_CLIPPER_621(Buffer : TBitmap32; const Points:TArrayOfArrayOfFloatPoint;
      ALineWidth: TFloat; AJoinStyle: TJoinStyle;  AEndStyle: TEndStyle; AMiterLimit: TFloat;
      Transformation : TTransformation) ;
  const
    ClipperJoinTypeOfGR32PolygonJoinStyle : array[GR32_Polygons.TJoinStyle] of clipper.TJoinType =
      (clipper.jtMiter, clipper.jtSquare, clipper.jtRound);
    ClipperEndTypeOfGR32PolygonEndStyle : array[GR32_Polygons.TEndStyle] of clipper.TEndType =
      (clipper.etOpenButt, clipper.etOpenSquare, clipper.etOpenRound);

  function Grow({const src:TArrayOfArrayOfFloatPoint; Growth: TFloat; JoinStyle: TJoinStyle;
    EndStyle: TEndStyle; AMiterLimit: TFloat}):TArrayOfArrayOfFloatPoint ;
  var solI,solutionI : TPaths;
  begin
    solutionI := AAFloatPoint2AAPoint(Points);
    with TClipperOffset.Create( ) do
      try
        MiterLimit := AMiterLimit;
        AddPaths( solutionI, ClipperJoinTypeOfGR32PolygonJoinStyle[AJoinStyle], ClipperEndTypeOfGR32PolygonEndStyle[AEndStyle]);
        Execute( solI, ALineWidth/2);
      finally
        Free;
      end;
      Result := AAPoint2AAFloatPoint(solI);
  end;

var
    Dst: TArrayOfArrayOfFloatPoint;
begin
  //CLIPPER
  Dst := Grow(); //AccData[1], GetStrokeWidth(), StrokeLineJoin  ,Self.StrokeLineCap, self.StrokeMiterLimit);
  PolyPolygonFS(Buffer, Dst, clGreen32 and FillOpacity, pfAlternate, Transformation);
  
  //if chkDebugRoute.Checked then
    PolyPolylineFS(Buffer, Dst, clGreen32 , True,1, jsMiter, esButt, 2, Transformation);
  if chkNodes.Checked then
    Dots(Buffer, Dst, Transformation);
end;
{$ENDIF}




procedure TfrmMain.DoRender_GR32_Clipper(Buffer: TBitmap32;
  const Points: TArrayOfArrayOfFloatPoint; ALineWidth: TFloat;
  AJoinStyle: TJoinStyle; AEndStyle: TEndStyle; AMiterLimit: TFloat;
  Transformation: TTransformation);

  const
    GR32ClipperJoinTypeOfGR32PolygonJoinStyle : array[GR32_Polygons.TJoinStyle] of GR32_Clipper.TJoinType =
      (GR32_Clipper.jtMiter, GR32_Clipper.jtSquare, GR32_Clipper.jtRound);


var
    Dst: TArrayOfArrayOfFloatPoint;
begin
  // GR32_CLIPPER
  Dst := InflatePolygons(Points, ALineWidth/2, GR32ClipperJoinTypeOfGR32PolygonJoinStyle[AJoinStyle], AMiterLimit, False);
  PolyPolygonFS(Buffer, Dst, clYellow32 and FillOpacity, pfWinding, Transformation);

  if chkDebugRoute.Checked then
  PolyPolylineFS(Buffer, Dst, clTrRed32, True, 2, jsMiter, esButt, 2, Transformation);
  if chkNodes.Checked then
    Dots(Buffer, Dst, Transformation);
end;

procedure TfrmMain.SetFillOpacity(const Value: Cardinal);
begin
  FFillOpacity := Value;

  if Value < $100 then
    FFillOpacity := (Value shl 24) or $ffffff;
end;

procedure TfrmMain.gbrFillOpacityMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FillOpacity := gbrFillOpacity.Position;
  Rebuild(Sender);
end;

procedure TfrmMain.gbrStrokeBigMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FStrokeWidth[1] := gbrStrokeBig.Position /2;
  Rebuild(self);
end;

procedure TfrmMain.gbrStrokeSmallMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FStrokeWidth[0] := gbrStrokeSmall.Position /2;
  Rebuild(self);
end;

end.
