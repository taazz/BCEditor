object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'BCEditor Demo'
  ClientHeight = 530
  ClientWidth = 706
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 106
  TextHeight = 14
  object SplitterVertical: TSplitter
    Left = 223
    Top = 0
    Height = 530
  end
  object PanelLeft: TPanel
    Left = 0
    Top = 0
    Width = 223
    Height = 530
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object SplitterHorizontal: TSplitter
      Left = 0
      Top = 390
      Width = 223
      Height = 3
      Cursor = crVSplit
      Align = alBottom
    end
    object ListBoxColors: TListBox
      AlignWithMargins = True
      Left = 3
      Top = 393
      Width = 220
      Height = 134
      Margins.Top = 0
      Margins.Right = 0
      Align = alBottom
      ItemHeight = 14
      TabOrder = 0
      OnClick = ListBoxColorsClick
    end
    object ListBoxHighlighters: TListBox
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 220
      Height = 387
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alClient
      ItemHeight = 14
      TabOrder = 1
      OnClick = ListBoxHighlightersClick
    end
  end
  object BCEditor: TBCEditor
    AlignWithMargins = True
    Left = 226
    Top = 3
    Width = 477
    Height = 524
    Margins.Left = 0
    Align = alClient
    TabOrder = 1
  end
end
