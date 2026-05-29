object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Form3'
  ClientHeight = 85
  ClientWidth = 310
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object Button1: TButton
    Left = 8
    Top = 16
    Width = 285
    Height = 25
    Caption = '.tkmjson > json'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 47
    Width = 285
    Height = 25
    Caption = '.json > .tkmjson'
    TabOrder = 1
    OnClick = Button2Click
  end
  object OpenDialog1: TOpenDialog
  end
end
