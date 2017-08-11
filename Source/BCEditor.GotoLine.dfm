object GotoLineDialog: TGotoLineDialog
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'GotoLineDialog'
  ClientHeight = 104
  ClientWidth = 285
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 106
  TextHeight = 14
  object FGroupBox: TGroupBox
    Left = 9
    Top = 4
    Width = 266
    Height = 50
    Caption = 'FGroupBox'
    TabOrder = 0
    object FLLine: TLabel
      Left = 9
      Top = 20
      Width = 34
      Height = 14
      Caption = 'FLLine'
      FocusControl = FLine
    end
    object FLine: TEdit
      Left = 178
      Top = 17
      Width = 78
      Height = 22
      Alignment = taRightJustify
      NumbersOnly = True
      TabOrder = 0
      Text = '  '
    end
  end
  object FBOk: TButton
    Left = 107
    Top = 68
    Width = 80
    Height = 27
    Caption = 'FBOk'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object FBCancel: TButton
    Left = 194
    Top = 68
    Width = 81
    Height = 27
    Cancel = True
    Caption = 'FBCancel'
    ModalResult = 2
    TabOrder = 2
  end
end
