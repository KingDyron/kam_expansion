object fmRXXEditor: TfmRXXEditor
  Left = 72
  Top = 90
  BorderStyle = bsSingle
  Caption = 'RXX Editor'
  ClientHeight = 517
  ClientWidth = 473
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 48
    Width = 48
    Height = 13
    Caption = 'Contents:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 232
    Top = 48
    Width = 53
    Height = 13
    Caption = 'Main image'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 232
    Top = 296
    Width = 55
    Height = 13
    Caption = 'Mask image'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label4: TLabel
    Left = 360
    Top = 24
    Width = 24
    Height = 13
    Caption = 'Pivot'
  end
  object btnAdd: TButton
    Left = 16
    Top = 480
    Width = 81
    Height = 25
    Caption = 'Add Image ...'
    Enabled = False
    TabOrder = 0
    OnClick = btnAddClick
  end
  object btnSaveRXX: TButton
    Left = 104
    Top = 16
    Width = 81
    Height = 25
    Caption = 'Save RXX ...'
    Enabled = False
    TabOrder = 1
    OnClick = btnSaveRXXClick
  end
  object lbSpritesList: TListBox
    Left = 16
    Top = 64
    Width = 209
    Height = 409
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 2
    OnClick = lbSpritesListClick
  end
  object btnLoadRXX: TButton
    Left = 16
    Top = 16
    Width = 81
    Height = 25
    Caption = 'Load RXX ...'
    TabOrder = 3
    OnClick = btnLoadRXXClick
  end
  object btnDelete: TButton
    Left = 104
    Top = 480
    Width = 81
    Height = 25
    Caption = 'Delete image'
    Enabled = False
    TabOrder = 4
    OnClick = btnDeleteClick
  end
  object btnReplace: TButton
    Left = 232
    Top = 256
    Width = 105
    Height = 25
    Caption = 'Replace image ...'
    Enabled = False
    TabOrder = 5
    OnClick = btnReplaceClick
  end
  object btnExport: TButton
    Left = 344
    Top = 256
    Width = 97
    Height = 25
    Caption = 'Export image ...'
    Enabled = False
    TabOrder = 6
    OnClick = btnExportClick
  end
  object Panel1: TPanel
    Left = 232
    Top = 64
    Width = 242
    Height = 162
    BevelOuter = bvLowered
    TabOrder = 7
    object imgMain: TImage
      Left = 1
      Top = 1
      Width = 240
      Height = 160
      Align = alClient
      Proportional = True
      Stretch = True
      Transparent = True
      ExplicitLeft = -1
      ExplicitTop = 2
    end
  end
  object Panel2: TPanel
    Left = 232
    Top = 312
    Width = 242
    Height = 162
    BevelOuter = bvLowered
    TabOrder = 8
    object imgMask: TImage
      Left = 1
      Top = 1
      Width = 240
      Height = 160
      Align = alClient
      Proportional = True
      Stretch = True
      Transparent = True
      ExplicitLeft = 233
      ExplicitTop = 33
    end
  end
  object edtPivotX: TSpinEdit
    Left = 360
    Top = 40
    Width = 57
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 9
    Value = 0
    OnChange = PivotChange
  end
  object edtPivotY: TSpinEdit
    Left = 416
    Top = 40
    Width = 57
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 10
    Value = 0
    OnChange = PivotChange
  end
  object chkHasMask: TCheckBox
    Left = 408
    Top = 296
    Width = 65
    Height = 15
    Caption = 'Has mask'
    Enabled = False
    TabOrder = 11
  end
  object chbImageStretch: TCheckBox
    Left = 232
    Top = 232
    Width = 89
    Height = 18
    Cursor = crHandPoint
    Caption = 'Draw stretched'
    Checked = True
    State = cbChecked
    TabOrder = 12
    OnClick = chbImageStretchClick
  end
  object chbOverload: TCheckBox
    Left = 191
    Top = 8
    Width = 123
    Height = 18
    Cursor = crHandPoint
    Caption = 'Overload from folder'
    Checked = True
    State = cbChecked
    TabOrder = 13
    OnClick = chbImageStretchClick
  end
  object chbOnlyFolder: TCheckBox
    Left = 191
    Top = 24
    Width = 123
    Height = 18
    Cursor = crHandPoint
    Caption = 'Only from folder'
    TabOrder = 14
    OnClick = chbImageStretchClick
  end
  object OpenDialog1: TOpenDialog
    OnShow = OpenDialog1Show
    Filter = 'Supported images (*.bmp;*.png)|*.bmp;*.png'
    Options = [ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select images'
    Left = 40
    Top = 80
  end
  object SaveDialog1: TSaveDialog
    OnShow = SaveDialog1Show
    DefaultExt = '*.rxx'
    Filter = 'RXX packages (*.rxx)|*.rxx'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 40
    Top = 136
  end
  object SelectDirectoryDialog1: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    OkButtonLabel = 'Select'
    Options = [fdoPickFolders, fdoForceFileSystem, fdoPathMustExist]
    Title = 'Select Directory'
    Left = 144
    Top = 80
  end
end
