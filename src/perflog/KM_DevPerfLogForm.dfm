object FormPerfLogs: TFormPerfLogs
  Left = 227
  Top = 108
  Align = alClient
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'PerfLog'
  ClientHeight = 389
  ClientWidth = 245
  Color = clBtnFace
  ParentFont = True
  FormStyle = fsStayOnTop
  Position = poScreenCenter
  TextHeight = 15
  object Label1: TLabel
    Left = 104
    Top = 144
    Width = 145
    Height = 15
    Caption = 'Filled in dynamically on init'
    Visible = False
  end
  object Label2: TLabel
    Left = 182
    Top = 6
    Width = 54
    Height = 15
    Caption = 'Scale (ms)'
  end
  object lblPerflogSaveThreshold: TLabel
    Left = 8
    Top = 83
    Width = 109
    Height = 15
    Caption = 'Save Threshold (ms):'
  end
  object cbStackedGFX: TCheckBox
    Left = 95
    Top = 22
    Width = 81
    Height = 17
    Caption = 'Stacked GFX'
    TabOrder = 0
    OnClick = DoChange
  end
  object seScale: TSpinEdit
    Left = 182
    Top = 22
    Width = 49
    Height = 22
    MaxValue = 1000
    MinValue = 1
    TabOrder = 1
    Value = 30
    OnChange = DoChange
    OnKeyDown = seScaleKeyDown
  end
  object cbStackedCPU: TCheckBox
    Left = 95
    Top = 6
    Width = 81
    Height = 17
    Caption = 'Stacked CPU'
    TabOrder = 2
    OnClick = DoChange
  end
  object cbSmoothLines: TCheckBox
    Left = 8
    Top = 46
    Width = 81
    Height = 17
    Caption = 'Smooth lines'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = DoChange
  end
  object sePerfLogSaveThreshold: TSpinEdit
    Left = 116
    Top = 80
    Width = 40
    Height = 22
    MaxValue = 300
    MinValue = 1
    TabOrder = 4
    Value = 20
  end
  object btnPerfLogExport: TButton
    Left = 169
    Top = 80
    Width = 68
    Height = 22
    Caption = 'Export'
    TabOrder = 5
    OnClick = DoExport
  end
  object cbEnabled: TCheckBox
    Left = 8
    Top = 6
    Width = 81
    Height = 17
    Caption = 'Enabled'
    TabOrder = 6
    OnClick = DoChange
  end
  object cbClearOnGameStart: TCheckBox
    Left = 95
    Top = 46
    Width = 122
    Height = 17
    Caption = 'Clear on game start'
    Checked = True
    State = cbChecked
    TabOrder = 7
    OnClick = DoChange
  end
end
