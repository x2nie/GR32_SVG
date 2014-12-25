unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, GR32_Image, ExtCtrls;

type
  TForm1 = class(TForm)
    btn1: TButton;
    dlgOpenSvg: TOpenDialog;
    pnl1: TPanel;
    img1: TImage32;
    procedure btn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses
 GR32, SVG;
{$R *.dfm}

procedure TForm1.btn1Click(Sender: TObject);
var s : TSVG;
  R : TFloatRect;
begin
  if dlgOpenSvg.Execute then
  begin
    s := TSVG.Create;
    s.LoadFromFile(dlgOpenSvg.FileName);
    img1.BeginUpdate;
    R := floatrect(img1.Bitmap.BoundsRect);
    InflateRect(R, -50, -50);
    s.PaintTo(img1.Bitmap, R,nil, 0);
    img1.EndUpdate;
    img1.Invalidate;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  img1.Bitmap.SetSize(img1.Width, img1.Height);
  img1.Bitmap.Clear(clWhite32);
end;

end.
 