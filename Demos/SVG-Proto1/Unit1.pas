unit Unit1;

interface

uses
  Windows, SysUtils, Controls, Forms, XPMan,
  Dialogs, StdCtrls, Graphics,
  SVG, SVGPaint, ActnList, ImgList, //Slider, AngleChooser,Printers, ShellDropper
  ComCtrls, ToolWin, Classes,
  ExtCtrls, StdActns, GR32, GR32_Image, GR32_RangeBars;

type
  TForm1 = class(TForm)
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    Panel2: TPanel;
    Label1: TLabel;
    ActionList1: TActionList;
    ImageList1: TImageList;
    Action1: TAction;
    Label2: TLabel;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    ToolButton2: TToolButton;
    PrintDialog1: TPrintDialog;
    ToolButton3: TToolButton;
    PrintDlg1: TPrintDlg;
    imgView1: TImgView32;
    gau1: TGaugeBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure Action1Execute(Sender: TObject);
    procedure AngleChooser1Change(Sender: TObject);
    procedure Slider2Change(Sender: TObject);
    procedure Slider1Change(Sender: TObject);
    procedure PrintDlg1Accept(Sender: TObject);
    procedure gau1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgView1PaintStage(Sender: TObject; Buffer: TBitmap32;
      StageNum: Cardinal);
//    procedure ShellDropper1DragEnter(Sender: TObject; const DropRec: TDropRec;
  //    var Accept: Boolean);
    //procedure ShellDropper1Drop(Sender: TObject; const DropRec: TDropRec);
  private
    SVG: TSVG;
    procedure LoadFile(const S: AnsiString);
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation


{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);

begin
  //Panel1.DoubleBuffered := True;
  SVG := TSVG.Create;
  imgView1.Bitmap.SetSize( imgView1.Height - 100, imgView1.Height - 100);
  // by default, PST_CLEAR_BACKGND is executed at this stage,
  // which, in turn, calls ExecClearBackgnd method of ImgView.
  // Here I substitute PST_CLEAR_BACKGND with PST_CUSTOM, so force ImgView
  // to call the OnPaintStage event instead of performing default action.
  with ImgView1.PaintStages[0]^ do
  begin
    if Stage = PST_CLEAR_BACKGND then Stage := PST_CUSTOM;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  SVG.Free;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
var
  W, H: Integer;
begin
  if SVG.Count > 0 then
  begin
    W := Round(SVG.Width * {Slider1.Percent}100 * 2 / 50);
    H := Round(SVG.Height * {Slider1.Percent}100 * 2 / 50);
    //SVG.PaintTo(PaintBox1.Canvas.Handle,
      //MakeRect((PaintBox1.Width - W) / 2, (PaintBox1.Height - H) / 2, W, H), nil, 0);
  end;
end;

type
  TDeviceCaps = record
    Name: WideString;
    Index: Integer;
    Width,
    Height,
    DPIX,
    DPIY,
    OffsetX,
    OffsetY: Integer;
    PhysicalWidth,
    PhysicalHeight: Integer;
    PixelWidth,
    PixelHeight: Real;
    Orientation: TPrinterOrientation;
  end;

function GetPrinterCaps: TDeviceCaps;
var
  DC: THandle;
  Caps: TDeviceCaps;
begin
{  DC := Printer.Handle;
  Caps.Orientation    := Printer.Orientation;
  Caps.Index          := Printer.PrinterIndex;
  Caps.Name           := Printer.Printers[Caps.Index];
  Caps.Width          := GetDeviceCaps(DC, HorzSize);  // mm
  Caps.Height         := GetDeviceCaps(DC, VertSize);  // mm
  Caps.DPIX           := GetDeviceCaps(DC, LogPixelSX); // DPI
  Caps.DPIY           := GetDeviceCaps(DC, LogPixelSY); // DPI
  Caps.OffsetX        := GetDeviceCaps(DC, PhysicalOffsetX);  // Pixel
  Caps.OffsetY        := GetDeviceCaps(DC, PhysicalOffsetY);  // Pixel
  Caps.PhysicalWidth  := GetDeviceCaps(DC, PhysicalWidth);  // Pixel
  Caps.PhysicalHeight := GetDeviceCaps(DC, PhysicalHeight);  // Pixel
  Caps.PixelWidth     := (Caps.Width / Printer.PageWidth);
  Caps.PixelHeight    := (Caps.Height / Printer.PageHeight);
  Result := Caps;}
end;

function GetPageRect(const Width, Height: Integer;
  const Caps: TDeviceCaps; Center: Boolean = True): TRect;
begin
  Result.Left := -Caps.OffsetX;
  Result.Top := -Caps.OffsetY;
  Result.Right := Result.Left + Round(Width / Caps.PixelWidth);
  Result.Bottom := Result.Top + Round(Height / Caps.PixelHeight);

  if Center then
  begin
    if Result.Right < Caps.PhysicalWidth then
    begin
      Result.Left := (Caps.PhysicalWidth - Result.Right) div 2;
      Inc(Result.Right, Result.Left);
    end;

    if Result.Bottom < Caps.PhysicalHeight then
    begin
      Result.Top := (Caps.PhysicalHeight - Result.Bottom) div 2;
      Inc(Result.Bottom, Result.Top);
    end;
  end;
end;

procedure TForm1.PrintDlg1Accept(Sender: TObject);
var
  R: TRect;
  //Bounds: TGPRectF;
  //Graphics: TGPGraphics;
  Caps: TDeviceCaps;
begin
  Caps := GetPrinterCaps;
  R := GetPageRect(Round(SVG.Width), Round(SVG.Height), Caps, False);
{  Printer.BeginDoc;
  Bounds.X        := R.Left;
  Bounds.Y        := R.Top;
  Bounds.Width    := R.Right - R.Left;
  Bounds.Height   := R.Bottom - R.Top;

  Graphics := TGPGraphics.Create(Printer.Canvas.Handle);
  try
    Graphics.SetPageUnit(UnitPixel);
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    SVG.PaintTo(Graphics, Bounds, nil, 0);
  finally
    Graphics.Free;
  end;
  Printer.EndDoc;}
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
{  if WheelDelta > 0 then
  begin
    if Slider1.Percent - 5 < 0.1 then
      Slider1.Percent := 0.1
    else
      Slider1.Percent := Slider1.Percent - 5;
  end;

  if WheelDelta < 0 then
  begin
    if Slider1.Percent + 5 > 100 then
      Slider1.Percent := 100
    else
      Slider1.Percent := Slider1.Percent + 5;
  end;

  Label1.Caption := 'Zoom: ' + FloatToStr(Slider1.Percent * 2) + '%';
  PaintBox1.Invalidate;}
end;

procedure TForm1.LoadFile(const S: AnsiString);
var
  W, H: Integer;
begin

{  AngleChooser1.Angle := 0;
  Slider1.Percent := 25;
  Label1.Caption := 'Zoom: ' + FloatToStr(Slider1.Percent * 2) + '%';
  Slider2.Percent := 100;
  Label2.Caption := 'Opacity: ' + Slider2.PercentString + '%';
 } SetCurrentDir(ExtractFilePath(S));
  SVG.LoadFromFile(S);
  Caption := ExtractFileName(S) + ' - SVG-Viewer';

  if SVG.Count > 0 then
  begin
    imgView1.BeginUpdate;


      W := Round(SVG.Width * {Slider1.Percent}50 * 2/2 / 50);
      H := Round(SVG.Height * {Slider1.Percent}50 * 2/2 / 50);
      (*
      SVG.PaintTo(imgView1.Bitmap,
        FloatRect( (imgView1.Bitmap.Width - W) / 2, (imgView1.Bitmap.Height - H) / 2, W, H), nil, 0);
      *)
      if W or H = 0 then
      begin
        W := 100;
        H := 100;
      end;  
    imgView1.Bitmap.SetSize(W, H);
    imgView1.Bitmap.Clear($00FFFFFF);

    SVG.PaintTo( imgView1.Bitmap, FloatRect( imgView1.Bitmap.BoundsRect), nil, 0  );

    imgView1.EndUpdate;
    imgView1.Invalidate;
  end;
end;

procedure TForm1.Action1Execute(Sender: TObject);
begin
  if OpenDialog1.Execute then
    LoadFile(OpenDialog1.FileName);
end;

procedure TForm1.AngleChooser1Change(Sender: TObject);
begin
//  SVG.Angle := AngleChooser1.Angle / 180 * Pi;
//  PaintBox1.Invalidate;
end;

procedure TForm1.Slider2Change(Sender: TObject);
begin
//  Label2.Caption := 'Opacity: ' + Slider2.PercentString + '%';
//  SVG.SVGOpacity := Slider2.Percent / 100;
//  PaintBox1.Invalidate;
end;

{procedure TForm1.ShellDropper1DragEnter(Sender: TObject;
  const DropRec: TDropRec; var Accept: Boolean);
begin
  Accept := (DropRec.Files.Count = 1) and (LowerCase(ExtractFileExt(DropRec.Files[0])) = '.svg');
end;

procedure TForm1.ShellDropper1Drop(Sender: TObject; const DropRec: TDropRec);
begin
  LoadFile(DropRec.Files[0]);
end;}

procedure TForm1.Slider1Change(Sender: TObject);
begin
{  if Slider1.Percent = 0 then
    Slider1.Percent := 0.1;
  Label1.Caption := 'Zoom: ' + FloatToStr(Slider1.Percent * 2) + '%';
  PaintBox1.Invalidate;}
end;

procedure TForm1.gau1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  W,H : Integer;
begin
  if SVG.Count > 0 then
  begin
    imgView1.BeginUpdate;


      W := Round(SVG.Width * gau1.Position * 2 / 50);
      H := Round(SVG.Height * gau1.Position * 2 / 50);
      (*
      SVG.PaintTo(imgView1.Bitmap,
        FloatRect( (imgView1.Bitmap.Width - W) / 2, (imgView1.Bitmap.Height - H) / 2, W, H), nil, 0);
      *)
    imgView1.Bitmap.SetSize(W, H);
    imgView1.Bitmap.Clear($00FFFFFF);

    SVG.PaintTo( imgView1.Bitmap, FloatRect( imgView1.Bitmap.BoundsRect), nil, 0  );

    imgView1.EndUpdate;
    imgView1.Invalidate;
  end;


end;

procedure TForm1.imgView1PaintStage(Sender: TObject; Buffer: TBitmap32;
  StageNum: Cardinal);
const            //0..1
  Colors: array [Boolean] of TColor32 = ($FFFFFFFF, $FFB0B0B0);
var
  R: TRect;
  I, J: Integer;
  OddY: Integer;
  TilesHorz, TilesVert: Integer;
  TileX, TileY: Integer;
  TileHeight, TileWidth: Integer;
begin
  TileHeight := 13;
  TileWidth := 13;

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

end;

end.
