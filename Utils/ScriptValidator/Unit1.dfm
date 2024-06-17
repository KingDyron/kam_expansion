object Form1: TForm1
  Left = 192
  Top = 148
  Anchors = [akLeft, akTop, akRight, akBottom]
  Caption = 'Script Validator'
  ClientHeight = 401
  ClientWidth = 779
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    779
    401)
  PixelsPerInch = 96
  TextHeight = 18
  object Label1: TLabel
    Left = 8
    Top = 15
    Width = 65
    Height = 18
    Caption = 'Script file:'
  end
  object Label2: TLabel
    Left = 8
    Top = 78
    Width = 45
    Height = 18
    Caption = 'Result:'
  end
  object Edit1: TEdit
    Left = 8
    Top = 39
    Width = 573
    Height = 26
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = Edit1Change
    ExplicitWidth = 387
  end
  object btnBrowseFile: TButton
    Left = 682
    Top = 8
    Width = 89
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Browse File'
    TabOrder = 1
    OnClick = btnBrowseFileClick
    ExplicitLeft = 496
  end
  object btnValidate: TButton
    Left = 587
    Top = 39
    Width = 184
    Height = 26
    Anchors = [akTop, akRight]
    Caption = 'Validate'
    TabOrder = 2
    OnClick = btnValidateClick
    ExplicitLeft = 401
  end
  object Memo1: TMemo
    Left = 8
    Top = 104
    Width = 763
    Height = 289
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 3
    ExplicitWidth = 577
    ExplicitHeight = 233
  end
  object btnValidateAll: TButton
    Left = 587
    Top = 71
    Width = 184
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Validate all KMR scripts'
    TabOrder = 4
    OnClick = btnValidateAllClick
    ExplicitLeft = 401
  end
  object btnBrowsePath: TButton
    Left = 587
    Top = 8
    Width = 89
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Browse Path'
    TabOrder = 5
    OnClick = btnBrowsePathClick
    ExplicitLeft = 401
  end
  object OpenDialog: TOpenDialog
    Filter = 'KaM Remake script files (*.script)|*.script'
    Left = 40
    Top = 120
  end
  object FileOpenDlg: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    OkButtonLabel = 'Select'
    Options = [fdoPickFolders, fdoForceFileSystem, fdoPathMustExist]
    Title = 'Select scripts directory'
    Left = 120
    Top = 120
  end
end
