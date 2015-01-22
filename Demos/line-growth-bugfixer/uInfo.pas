unit uInfo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, ExtCtrls,
  GR32,
  VirtualTrees, ComCtrls, Tabs;

type
  TfrmInfo = class(TForm)
    ts1: TTabSet;
    pg1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    vt1: TVirtualStringTree;
    lbNormal_1: TLabel;
    lbNormal: TLabel;
    lbNormal1: TLabel;
    procedure vt1GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure vt1PaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure ts1Change(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
  private
    { Private declarations }
    FI, FJ : Integer;
    FPolygon, FNormals : PArrayOfFloatPoint;
  public
    { Public declarations }
    procedure SetIJ(i,j: Integer);
  end;

var
  frmInfo: TfrmInfo;

implementation

uses uMain;

{$R *.dfm}

{ TfrmInfo }

procedure TfrmInfo.SetIJ(i, j: Integer);
var
  Node : PVirtualNode;
begin
  if i < 0 then
  begin
    vt1.RootNodeCount := 0;
  end
  else
  begin
    if FI <> i then
    begin
      FPolygon := @frmMain.MyPolygon^[i];
      FNormals := @frmMain.MyNormals^[i];
      vt1.RootNodeCount := Length(FPolygon^);
    end;

    if FJ <> j then
    begin
      FJ := j;
      Node := vt1.GetFirstChild(nil);
      while Assigned(Node) and (Node.Index < j) do
        Node := Node.NextSibling;
      vt1.ScrollIntoView(Node, True);
    end;  
  end;
end;

procedure TfrmInfo.vt1GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
var
  F : TFloat;
  i : Integer;
begin
  i := Node.Index;
  if Column = 0 then
    CellText := IntToStr(Node.Index)
  else
  begin

    case Column of
      1 : F := FNormals^[i].X;
      2 : F := FNormals^[i].Y;
      3 : F := FPolygon^[i].X;
      4 : F := FPolygon^[i].Y;
    end;

    CellText := FloatToStrF(F, ffNumber	, 5,2);
  end;

end;

procedure TfrmInfo.vt1PaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  F : TFloat;
  i : Integer;
begin
  i := Node.Index;
  if Column in [1,2] then
  begin
    case Column of
      1 : F := FNormals^[i].X;
      2 : F := FNormals^[i].Y;
    end;
    if F < 0 then
      TargetCanvas.Font.Color := clRed;
  end;

  if (Column <= 2) and (i = FJ) then
    TargetCanvas.Font.Style := [fsBold];
end;

procedure TfrmInfo.ts1Change(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);
begin
  pg1.ActivePageIndex := ts1.TabIndex;
end;

end.
