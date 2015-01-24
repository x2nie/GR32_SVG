object frmMain: TfrmMain
  Left = 256
  Top = 31
  Width = 928
  Height = 681
  Caption = 'LineGrow Benchmark'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 920
    Height = 52
    Align = alTop
    BevelOuter = bvNone
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
      Position = 3
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
    Height = 602
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    object imgView1: TImgView32
      Left = 0
      Top = 0
      Width = 789
      Height = 602
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
      OnClick = imgView1Click
      OnMouseMove = imgView1MouseMove
      OnPaintStage = imgView1PaintStage
    end
    object pnlRight: TPanel
      Left = 789
      Top = 0
      Width = 131
      Height = 602
      Align = alRight
      TabOrder = 1
      object PnlBitmapLayer: TPanel
        Left = 1
        Top = 89
        Width = 129
        Height = 96
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
          Left = 112
          Top = 16
          Width = 105
          Height = 17
          Caption = 'Rescale'
          TabOrder = 3
          Visible = False
        end
      end
      object pnlStroke: TPanel
        Left = 1
        Top = 185
        Width = 129
        Height = 336
        Align = alTop
        TabOrder = 1
        object Label3: TLabel
          Left = 8
          Top = 25
          Width = 31
          Height = 13
          Caption = 'Width:'
        end
        object lblStrokeWidth: TLabel
          Left = 117
          Top = 25
          Width = 3
          Height = 13
          Alignment = taRightJustify
          Caption = '-'
        end
        object Panel4: TPanel
          Left = 1
          Top = 1
          Width = 127
          Height = 16
          Align = alTop
          BevelOuter = bvNone
          Caption = 'Stroke Properties'
          Color = clBtnShadow
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindow
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
        end
        object gbrStrokeSmall: TGaugeBar
          Tag = 1
          Left = 16
          Top = 41
          Width = 105
          Height = 12
          Backgnd = bgPattern
          HandleSize = 16
          Max = 64
          ShowArrows = False
          ShowHandleGrip = True
          Style = rbsMac
          Position = 17
          OnChange = gbrStrokeSmallChange
          OnMouseUp = gbrStrokeSmallMouseUp
        end
        object chkNodes: TCheckBox
          Left = 11
          Top = 61
          Width = 97
          Height = 17
          Caption = '&Nodes'
          Checked = True
          State = cbChecked
          TabOrder = 2
          OnClick = Rebuild
        end
        object chkNumber: TCheckBox
          Left = 11
          Top = 85
          Width = 97
          Height = 17
          Caption = 'N&umber'
          Checked = True
          State = cbChecked
          TabOrder = 3
          OnClick = Rebuild
        end
        object chkNormal: TCheckBox
          Left = 11
          Top = 109
          Width = 97
          Height = 17
          Caption = 'No&rmal'
          TabOrder = 4
          OnClick = Rebuild
        end
        object chkStrokeClosed: TCheckBox
          Left = 11
          Top = 133
          Width = 97
          Height = 17
          Caption = '&Close'
          TabOrder = 5
          OnClick = Rebuild
        end
        object rgJoinStyle: TRadioGroup
          Left = 8
          Top = 168
          Width = 113
          Height = 73
          Caption = ' JoinStyle '
          Ctl3D = True
          ItemIndex = 0
          Items.Strings = (
            'Miter'
            'Bevel'
            'Round')
          ParentCtl3D = False
          TabOrder = 6
          OnClick = Rebuild
        end
        object rgEndStyle: TRadioGroup
          Left = 8
          Top = 256
          Width = 113
          Height = 73
          Caption = 'EndStyle'
          ItemIndex = 0
          Items.Strings = (
            'Butt'
            'Square'
            'Round')
          TabOrder = 7
          OnClick = Rebuild
        end
      end
      object Panel3: TPanel
        Left = 1
        Top = 1
        Width = 129
        Height = 88
        Align = alTop
        TabOrder = 2
        object Panel5: TPanel
          Left = 1
          Top = 1
          Width = 127
          Height = 16
          Align = alTop
          BevelOuter = bvNone
          Caption = 'Draw'
          Color = clBtnShadow
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindow
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
        end
        object CheckBox1: TCheckBox
          Left = 11
          Top = 28
          Width = 97
          Height = 17
          Caption = '&Nodes'
          Checked = True
          State = cbChecked
          TabOrder = 1
          OnClick = Rebuild
        end
        object CheckBox2: TCheckBox
          Left = 11
          Top = 52
          Width = 97
          Height = 17
          Caption = 'N&umber'
          Checked = True
          State = cbChecked
          TabOrder = 2
          OnClick = Rebuild
        end
      end
    end
  end
end
