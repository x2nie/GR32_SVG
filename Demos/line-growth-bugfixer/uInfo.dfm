object frmInfo: TfrmInfo
  Left = 253
  Top = 191
  Width = 410
  Height = 150
  BorderStyle = bsSizeToolWin
  Caption = 'Info'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Visible = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ts1: TTabSet
    Left = 0
    Top = 102
    Width = 402
    Height = 21
    Align = alBottom
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Tabs.Strings = (
      '  Full  '
      ' Simple ')
    TabIndex = 0
    OnChange = ts1Change
  end
  object pg1: TPageControl
    Left = 0
    Top = 0
    Width = 402
    Height = 102
    ActivePage = TabSheet2
    Align = alClient
    Style = tsButtons
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
      TabVisible = False
      object lbNormal_1: TLabel
        Left = 16
        Top = 16
        Width = 56
        Height = 14
        Caption = 'lbNormal'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
      end
      object lbNormal: TLabel
        Left = 16
        Top = 40
        Width = 56
        Height = 14
        Caption = 'lbNormal'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lbNormal1: TLabel
        Left = 16
        Top = 64
        Width = 56
        Height = 14
        Caption = 'lbNormal'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'TabSheet2'
      ImageIndex = 1
      TabVisible = False
      object vt1: TVirtualStringTree
        Left = 0
        Top = 0
        Width = 394
        Height = 92
        Align = alClient
        Header.AutoSizeIndex = 0
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'MS Sans Serif'
        Header.Font.Style = []
        Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
        Header.Style = hsFlatButtons
        Indent = 0
        TabOrder = 0
        TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toThemeAware, toUseBlendedImages, toUseBlendedSelection]
        TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect]
        OnGetText = vt1GetText
        OnPaintText = vt1PaintText
        Columns = <
          item
            Alignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coFixed, coAllowFocus]
            Position = 0
            WideText = 'Node'
          end
          item
            Alignment = taRightJustify
            Position = 1
            Width = 80
            WideText = 'Normal X'
          end
          item
            Alignment = taRightJustify
            Position = 2
            Width = 80
            WideText = 'Normal Y'
          end
          item
            Alignment = taRightJustify
            Position = 3
            Width = 80
            WideText = 'X'
          end
          item
            Alignment = taRightJustify
            Position = 4
            Width = 80
            WideText = 'Y'
          end>
      end
    end
  end
end
