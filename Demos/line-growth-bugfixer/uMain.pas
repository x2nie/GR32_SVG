unit uMain;

interface

{$DEFINE CLIPPER_621}
{$DEFINE GR32_CLIPPER}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  GR32, GR32_Polygons, GR32_Image, GR32_Layers, GR32_RangeBars, GR32_Transforms,
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
    chkNodes: TCheckBox;
    pnlStroke: TPanel;
    Panel4: TPanel;
    Label3: TLabel;
    gbrStrokeSmall: TGaugeBar;
    chkNumber: TCheckBox;
    Panel3: TPanel;
    Panel5: TPanel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    chkNormal: TCheckBox;
    lblStrokeWidth: TLabel;
    chkStrokeClosed: TCheckBox;
    rgJoinStyle: TRadioGroup;
    rgEndStyle: TRadioGroup;
    procedure gau1Change(Sender: TObject);
    procedure imgView1PaintStage(Sender: TObject; Buffer: TBitmap32;
      StageNum: Cardinal);
    procedure FormCreate(Sender: TObject);
    procedure gau1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Rebuild(Sender: TObject);
    procedure gbrFillOpacityMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure gbrStrokeSmallMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgView1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; Layer: TCustomLayer);
    procedure imgView1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure gbrStrokeSmallChange(Sender: TObject);
  private
    { Private declarations }
    FMyPolygon,
    FMyNormals,
    FPositives
     : TArrayOfArrayOfFloatPoint;
    FStrokeWidth : array[0..1] of TFloat;

    FFillOpacity : Cardinal;
    FTransform : TAffineTransformation;
    FNodeLocked: Boolean;
    procedure SetFillOpacity(const Value: Cardinal);
    function GetMyNormal: PArrayOfArrayOfFloatPoint;
    function GetMyPolygon: PArrayOfArrayOfFloatPoint;
    procedure SetNodeLocked(const Value: Boolean);
  protected
    FRenderer : array of TRendererMethod;
    procedure DoRender;

    procedure DoRender_GR32(Buffer : TBitmap32; const Points:TArrayOfArrayOfFloatPoint;
      ALineWidth: TFloat; AJoinStyle: TJoinStyle;  AEndStyle: TEndStyle; AMiterLimit: TFloat;
      Transformation : TTransformation) ;
    property NodeLocked : Boolean read FNodeLocked write SetNodeLocked;
  public
    { Public declarations }
    property FillOpacity : Cardinal read FFillOpacity write SetFillOpacity;
    property MyNormals : PArrayOfArrayOfFloatPoint read GetMyNormal;
    property MyPolygon : PArrayOfArrayOfFloatPoint read GetMyPolygon;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  acc_single_data,
  GR32_VectorUtils,  GR32_Geometry,
  Math
  , uInfo;
  
{$R *.dfm}



const
  ZOOM_LEVEL : array[0..12] of TFloat = (0.25, 0.50, 0.75, 1, 1.50, 2, 3,4,6,8,10,12,15);
  svgWidth  = 1850 div 8;
  svgHeight = 1540 div 11;


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

procedure ShowNormals(Buffer: TBitmap32;
  const Points, Normals: TArrayOfArrayOfFloatPoint;
  LineWidth : TFloat;
  Transformation: TTransformation; AColor: TColor32 = clLime32);
var i,j,w,h,x,y : Integer;
  P, P0 : TPoint;
  PF : TFloatPoint;
  R : TRect;
  flip : Boolean; //flip flop
  s : string;
  clBg : TColor32;
  AALevel : Integer;
begin
  Buffer.Font.Size := 10;
  h := Buffer.TextHeight('IjgMh9');
  clBg := clTrRed32;
  AALevel := 1;

  P0 := Point(0,0);
  for i := 0 to Length(Points)-1 do
  for j := 0 to Length(Points[i])-1 do
  begin
    with Normals[i,j] do
      if (X = 0) and (Y = 0) then
        Continue;
    //if flip then
    begin
      PF.X := Points[i,j].X + Normals[i,j].X * LineWidth /2;
      PF.Y := Points[i,j].Y + Normals[i,j].Y * LineWidth /2;
      
      //P := Transformation.Transform(Point( OffsetPoint( Points[i,j], Normals[i,j]) ));
      P := Point(Transformation.Transform( PF ));
      
      //draw line
      P0 := Point(Transformation.Transform( Points[i,j] ));
      Buffer.LineS(P.X, P.Y, P0.X, P0.Y, clRed32);

      //if Distance(P, P0) > 32 then
      begin
        s := IntToStr(J);
        w := Buffer.TextWidth(s);
        x := P.X - w div 2;
        y := P.Y - h div 2;
        R := MakeRect(x,y, x+w, y+h);
        //InflateRect(R,2,2);
        //Buffer.FillRectS(R, clTrWhite32);
        P0 := P;
        P.X := X;
        P.Y := Y;

        //top left
        P := OffsetPoint(P, -1, -1);
        Buffer.RenderText(P.X,P.Y, s, AALevel, clBg);

        //top right
        P := OffsetPoint(P, 2, 0);
        Buffer.RenderText(P.X,P.Y, s, AALevel, clBg);

        //bot right
        P := OffsetPoint(P, 0, 2);
        Buffer.RenderText(P.X,P.Y, s, AALevel, clBg);

        //bot left
        P := OffsetPoint(P, -2, 0);
        Buffer.RenderText(P.X,P.Y, s, AALevel, clBg);

        //center
        P := OffsetPoint(P, 1, -1);

        Buffer.RenderText(P.X,P.Y, s, 1, clWhite32);
      end;
    end;
    flip := not flip;
  end;
end;

procedure GetNearestPoint(P: TFloatPoint; const Points: TArrayOfArrayOfFloatPoint;
  Distance: TFloat; out ii,jj : Integer);
var
  i,j : Integer;  
begin
  for i := 0 to Length(Points)-1 do
  for j := 0 to Length(Points[i])-1 do
  begin
    if Abs(P.X - Points[i,j].X) < Distance then
    if Abs(P.Y - Points[i,j].Y) < Distance then
    begin
      ii := i;
      jj := j;
      //Break;
      Exit;

    end;
  end;

  ii := -1;
  jj := -1;
end;      

procedure Numbers(Buffer: TBitmap32;
  const Points: TArrayOfArrayOfFloatPoint; Transformation: TTransformation; AColor: TColor32 = clLime32);
var i,j,w,h,x,y : Integer;
  P, P0 : TPoint;
  R : TRect;
  flip : Boolean; //flip flop
  s : string;
  clBg : TColor32;
  AALevel : Integer;
begin
  Buffer.Font.Size := 10;
  h := Buffer.TextHeight('IjgMh9');
  clBg := clWhite32;
  AALevel := 1;

  P0 := Point(0,0);
  for i := 0 to Length(Points)-1 do
  for j := 0 to Length(Points[i])-1 do
  begin
    //if flip then
    begin
      P := Transformation.Transform(Point(Points[i,j]));
      if Distance(P, P0) > 32 then
      begin
        s := IntToStr(J);
        w := Buffer.TextWidth(s);
        x := P.X - w div 2;
        y := P.Y - h div 2;
        R := MakeRect(x,y, x+w, y+h);
        //InflateRect(R,2,2);
        //Buffer.FillRectS(R, clTrWhite32);
        P0 := P;
        P.X := X;
        P.Y := Y;

        //top left
        P := OffsetPoint(P, -1, -1);
        Buffer.RenderText(P.X,P.Y, s, AALevel, clBg);

        //top right
        P := OffsetPoint(P, 2, 0);
        Buffer.RenderText(P.X,P.Y, s, AALevel, clBg);

        //bot right
        P := OffsetPoint(P, 0, 2);
        Buffer.RenderText(P.X,P.Y, s, AALevel, clBg);

        //bot left
        P := OffsetPoint(P, -2, 0);
        Buffer.RenderText(P.X,P.Y, s, AALevel, clBg);

        //center
        P := OffsetPoint(P, 1, -1);

        Buffer.RenderText(P.X,P.Y, s, 1, clBlack32);
      end;
    end;
    flip := not flip;
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

var i : Integer;
begin
  FStrokeWidth[1] := 27.63601;
  FStrokeWidth[0] := 8.736;

  FMyPolygon := AccData[0];
  SetLength( FMyNormals, Length(FMyPolygon));
  SetLength( FPositives, Length(FMyPolygon));
  for I := 0 to High(FMyPolygon) do
  begin
      FMyNormals[i] := BuildNormals(FMyPolygon[I]);
      FPositives[i] := Grow( FMyPolygon[i], FMyNormals[i], FStrokeWidth[0] /2 );
  end;



  FTransform := TAffineTransformation.Create;

// RENDERER

  // GR32_Polygons using GR32_VectorUtils
  RegisterRenderer(DoRender_GR32, 'GR32 Polygon');


  // by default, PST_CLEAR_BACKGND is executed at this stage,
  // which, in turn, calls ExecClearBackgnd method of ImgView.
  // Here I substitute PST_CLEAR_BACKGND with PST_CUSTOM, so force ImgView
  // to call the OnPaintStage event instead of performing default action.
  with ImgView1.PaintStages[0]^ do
  begin
    if Stage = PST_CLEAR_BACKGND then Stage := PST_CUSTOM;
  end;
  FillOpacity := gbrFillOpacity.Position;
  cbbRenderer.ItemIndex := 0;
  gau1Change(Sender);
  Rebuild(Self);
  NodeLocked := False; 
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




procedure TfrmMain.DoRender;
const
  LJoinStyle : array[0..3] of TJoinStyle = (jsMiter, jsRound, jsMiter, jsRound);
  LLineWidth : array[0..3] of TFloat = (8.736, 27.63601, 8.736, 27.63601);
var i, first,last : Integer;
begin
  first := 0;
  last := Length(AccData) -1;


  last := 0;
  for i := first to last do
  begin
    FRenderer[cbbRenderer.ItemIndex](imgView1.Bitmap, AccData[i],
      //LLineWidth[i],
      FStrokeWidth[i mod 2],
      //LJoinStyle[i],
      TJoinStyle(rgJoinStyle.ItemIndex),
      ///esButt,
      TEndStyle(rgEndStyle.ItemIndex),
      4, FTransform );

        //original path
    PolyPolylineFS( imgView1.Bitmap, AccData[i], clTrBlack32, //
      False, 2, jsMiter, esButt, 4, FTransform
      //Assigned(Brush), 1, Self.StrokeLineJoin  ,Self.StrokeLineCap, self.StrokeMiterLimit, TGP
      );

    // positives
    PolyPolylineFS( imgView1.Bitmap, FPositives, clAqua32 and clTrWhite32, //
      False, 1,
      TJoinStyle(rgJoinStyle.ItemIndex),
      TEndStyle(rgEndStyle.ItemIndex),
      4, FTransform
      //Assigned(Brush), 1, Self.StrokeLineJoin  ,Self.StrokeLineCap, self.StrokeMiterLimit, TGP
      );

    if chkNormal.Checked then
      ShowNormals(imgView1.Bitmap, accData[i], FMyNormals, FStrokeWidth[0], FTransform);

    Dots(imgView1.Bitmap, AccData[i], FTransform, ClYellow32);

    if chkNumber.Checked then
      Numbers(imgView1.Bitmap, AccData[i], FTransform, ClYellow32);
  end;
//  FRenderer[cbbRenderer.ItemIndex]();
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

procedure TfrmMain.DoRender_GR32(Buffer : TBitmap32; const Points:TArrayOfArrayOfFloatPoint;
      ALineWidth: TFloat; AJoinStyle: TJoinStyle;  AEndStyle: TEndStyle; AMiterLimit: TFloat;
      Transformation : TTransformation);
var
  Dst : TArrayOfArrayOfFloatPoint;
begin
    //PolyPolylineFS( Buffer, Points, clBlue32 and FillOpacity, True, ALineWidth,
      //AJoinStyle, AEndStyle, AMiterLimit, Transformation);

  Dst := BuildPolyPolyLine(Points, chkStrokeClosed.Checked, ALineWidth, AJoinStyle  , AEndStyle, AMiterLimit  );
  //PolyPolylineFS( Graphics, Dst, clTrRed32, True,1,jsMiter, esButt, 4, TGP);

  PolyPolygonFS(Buffer, Dst, clBlue32  and FillOpacity, pfWinding, Transformation);

  if chkDebugRoute.Checked then
    PolyPolylineFS(Buffer, Dst, clBlue32, True, 1, jsMiter, esButt, 2, Transformation);

  if chkNodes.Checked then
    Dots(Buffer, Dst, Transformation);
  if chkNumber.Checked then
    Numbers(Buffer, Dst, Transformation);
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

procedure TfrmMain.gbrStrokeSmallMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i : Integer;  
begin
  FStrokeWidth[0] := gbrStrokeSmall.Position /2;

  for I := 0 to High(FMyPolygon) do
  begin
      FMyNormals[i] := BuildNormals(FMyPolygon[I]);
      FPositives[i] := Grow( FMyPolygon[i], FMyNormals[i], FStrokeWidth[0] /2 );
  end;
  Rebuild(self);
end;

procedure TfrmMain.imgView1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer; Layer: TCustomLayer);

  procedure SetInfo(lb : TLabel; i,j : Integer);
  var p : TFloatPoint;
  begin
    p := FMyPolygon[i,j];
    with FMyNormals[i,j] do
      lb.Caption := Format('%.3d Normal: %f, %f   (%f, %f)', [j, x,y, p.X, p.Y ] );
  end;
  
var
  pb : TFloatPoint;
  i,j : Integer;
var
  Scale, ShiftX, ShiftY: TFloat;
begin
  if NodeLocked then
    Exit;

    
    imgView1.Layers.GetViewportShift(ShiftX, ShiftY);
//    imgView1.Layers.GetViewportScale(ScaleX, ScaleY);

    Scale := ZOOM_LEVEL[gau1.Position];


    pb.X := (X - ShiftX) / Scale ;
    pb.Y := (Y - ShiftY) / Scale;


  GetNearestPoint(pb, self.FMyPolygon, 20/scale , i,j);

  frmInfo.SetIJ(i,j);

  if i > -1 then
  begin
    frmInfo.Caption := Format('Info - Polygon: %d    Node: %d',[ i,j]);
    //with FMyNormals[i,j] do
      //frmInfo.lbNormal.Caption := Format('%.3d Normal: x=%f, y=%f', [j, x,y ] ) ;
      SetInfo(frmInfo.lbNormal, i,j);

    if j < Length(FMyNormals[i]) -1 then
      //with FMyNormals[i,j+1] do
      //frmInfo.lbNormal1.Caption := Format('%.3d Normal: x=%f, y=%f', [j+1, x,y ] )
      SetInfo(frmInfo.lbNormal1, i,j+1)
    else
      frmInfo.lbNormal1.Caption := '-';

    if j > 0 then
      //with FMyNormals[i,j-1] do
      //frmInfo.lbNormal_1.Caption := Format('%.3d Normal: x=%f, y=%f', [j-1, x,y ] )
      SetInfo(frmInfo.lbNormal_1, i,j-1)
    else
      frmInfo.lbNormal_1.Caption := '-';


  end
  else
  begin
    frmInfo.Caption := 'Info';
    frmInfo.lbNormal.Caption := '-';
    frmInfo.lbNormal1.Caption := '-';
    frmInfo.lbNormal_1.Caption := '-';
  end;

  //Caption := Format('x:%f, y:%f   %d~%d',[pb.X, pb.Y, i,j]);
end;

function TfrmMain.GetMyNormal: PArrayOfArrayOfFloatPoint;
begin
  Result := @FMyNormals;
end;

function TfrmMain.GetMyPolygon: PArrayOfArrayOfFloatPoint;
begin
  Result := @FMyPolygon;
end;

procedure TfrmMain.SetNodeLocked(const Value: Boolean);
begin
  FNodeLocked := Value;
  if Value then
  begin
    imgView1.Cursor := crDefault;
  end
  else
  begin
    imgView1.Cursor := crHandPoint;
  end;    
end;

procedure TfrmMain.imgView1Click(Sender: TObject);
begin
  NodeLocked := not NodeLocked;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  SetBounds(0,0, Screen.WorkAreaWidth, Screen.WorkAreaHeight);
end;

procedure TfrmMain.gbrStrokeSmallChange(Sender: TObject);
begin
  lblStrokeWidth.Caption := FloatToStr(gbrStrokeSmall.Position /2);
end;

end.
