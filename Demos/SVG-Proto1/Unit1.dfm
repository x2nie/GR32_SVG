object Form1: TForm1
  Left = 187
  Top = 132
  Width = 928
  Height = 480
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnl1: TPanel
    Left = 0
    Top = 0
    Width = 920
    Height = 41
    Align = alTop
    Caption = 'pnl1'
    TabOrder = 0
    object btn1: TButton
      Left = 440
      Top = 8
      Width = 75
      Height = 25
      Caption = 'btn1'
      TabOrder = 0
      OnClick = btn1Click
    end
  end
  object img1: TImage32
    Left = 0
    Top = 41
    Width = 920
    Height = 412
    Align = alClient
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baTopLeft
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 1
  end
  object dlgOpenSvg: TOpenDialog
    Filter = 'SVG (*.svg)|*.svg'
    Left = 328
    Top = 48
  end
end
