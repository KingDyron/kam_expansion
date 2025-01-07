object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'KMR Animation Interpolator'
  ClientHeight = 629
  ClientWidth = 753
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 453
    Width = 33
    Height = 13
    Caption = 'Errors:'
  end
  object btnProcess: TButton
    Left = 8
    Top = 146
    Width = 105
    Height = 25
    Caption = 'Process'
    TabOrder = 0
    OnClick = btnProcessClick
  end
  object Memo1: TMemo
    Left = 8
    Top = 177
    Width = 737
    Height = 270
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object memoErrors: TMemo
    Left = 8
    Top = 472
    Width = 737
    Height = 146
    Lines.Strings = (
      'memoErrors')
    TabOrder = 2
  end
  object chkSerfCarry: TCheckBox
    Left = 8
    Top = 31
    Width = 97
    Height = 17
    Caption = 'Serf carry'
    TabOrder = 3
  end
  object chkUnitActions: TCheckBox
    Left = 8
    Top = 8
    Width = 97
    Height = 17
    Caption = 'Unit actions'
    TabOrder = 4
  end
  object chkUnitThoughts: TCheckBox
    Left = 8
    Top = 54
    Width = 97
    Height = 17
    Caption = 'Unit thoughts'
    TabOrder = 5
  end
  object chkTrees: TCheckBox
    Left = 8
    Top = 77
    Width = 97
    Height = 17
    Caption = 'Trees'
    TabOrder = 6
  end
  object chkHouseActions: TCheckBox
    Left = 8
    Top = 100
    Width = 97
    Height = 17
    Caption = 'House actions'
    TabOrder = 7
  end
  object chkBeasts: TCheckBox
    Left = 8
    Top = 123
    Width = 97
    Height = 17
    Caption = 'House beasts'
    TabOrder = 8
  end
end
