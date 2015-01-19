object frmMain: TfrmMain
  Left = 311
  Top = 165
  Width = 928
  Height = 480
  Caption = 'LineGrow Benchmark'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 920
    Height = 52
    Align = alTop
    TabOrder = 0
    object lbZoom: TLabel
      Left = 32
      Top = 30
      Width = 59
      Height = 13
      Caption = 'Zoom: 100%'
    end
    object Label1: TLabel
      Left = 224
      Top = 31
      Width = 44
      Height = 13
      Caption = 'Renderer'
    end
    object gau1: TGaugeBar
      Left = 32
      Top = 8
      Width = 145
      Height = 16
      Backgnd = bgPattern
      Max = 12
      ShowArrows = False
      ShowHandleGrip = True
      Style = rbsMac
      Position = 5
      OnChange = gau1Change
      OnMouseUp = gau1MouseUp
    end
    object cbbRenderer: TComboBox
      Left = 224
      Top = 7
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
      OnChange = Rebuild
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 52
    Width = 920
    Height = 401
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    object imgView1: TImgView32
      Left = 0
      Top = 0
      Width = 789
      Height = 401
      Align = alClient
      Bitmap.DrawMode = dmBlend
      Bitmap.CombineMode = cmMerge
      Bitmap.ResamplerClassName = 'TNearestResampler'
      BitmapAlign = baCustom
      Centered = False
      Scale = 1.000000000000000000
      ScaleMode = smScale
      ScrollBars.ShowHandleGrip = True
      ScrollBars.Style = rbsDefault
      ScrollBars.Size = 16
      ScrollBars.Visibility = svAuto
      OverSize = 0
      TabOrder = 0
      OnPaintStage = imgView1PaintStage
    end
    object pnlRight: TPanel
      Left = 789
      Top = 0
      Width = 131
      Height = 401
      Align = alRight
      TabOrder = 1
      object PnlBitmapLayer: TPanel
        Left = 1
        Top = 1
        Width = 129
        Height = 168
        Align = alTop
        TabOrder = 0
        object LblOpacity: TLabel
          Left = 8
          Top = 24
          Width = 39
          Height = 13
          Caption = 'Opacity:'
        end
        object PnlBitmapLayerHeader: TPanel
          Left = 1
          Top = 1
          Width = 127
          Height = 16
          Align = alTop
          BevelOuter = bvNone
          Caption = 'Fill Properties'
          Color = clBtnShadow
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindow
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
        end
        object gbrFillOpacity: TGaugeBar
          Left = 16
          Top = 40
          Width = 105
          Height = 12
          Backgnd = bgPattern
          HandleSize = 16
          Max = 255
          ShowArrows = False
          ShowHandleGrip = True
          Style = rbsMac
          Position = 128
          OnMouseUp = gbrFillOpacityMouseUp
        end
        object chkDebugRoute: TCheckBox
          Left = 16
          Top = 64
          Width = 97
          Height = 17
          Caption = '&Debug Route'
          Checked = True
          State = cbChecked
          TabOrder = 2
          OnClick = Rebuild
        end
        object BtnLayerRescale: TButton
          Left = 16
          Top = 112
          Width = 105
          Height = 17
          Caption = 'Rescale'
          TabOrder = 3
        end
        object BtnLayerResetScale: TButton
          Left = 16
          Top = 136
          Width = 105
          Height = 17
          Caption = 'Scale to 100%'
          TabOrder = 4
        end
        object CbxCropped: TCheckBox
          Left = 16
          Top = 88
          Width = 97
          Height = 17
          Caption = '&Cropped'
          TabOrder = 5
        end
      end
    end
  end
end
