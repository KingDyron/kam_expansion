object Form1: TForm1
  Left = 192
  Top = 148
  Anchors = [akLeft, akTop, akRight, akBottom]
  Caption = 'Script Validator'
  ClientHeight = 400
  ClientWidth = 775
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    775
    400)
  TextHeight = 18
  object Label1: TLabel
    Left = 8
    Top = 15
    Width = 62
    Height = 18
    Caption = 'Script file:'
  end
  object Label2: TLabel
    Left = 8
    Top = 78
    Width = 44
    Height = 18
    Caption = 'Result:'
  end
  object Edit1: TEdit
    Left = 8
    Top = 39
    Width = 565
    Height = 26
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = Edit1Change
  end
  object btnBrowseFile: TButton
    Left = 674
    Top = 8
    Width = 89
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Browse File'
    TabOrder = 1
    OnClick = btnBrowseFileClick
  end
  object btnValidate: TButton
    Left = 579
    Top = 39
    Width = 184
    Height = 26
    Anchors = [akTop, akRight]
    Caption = 'Validate'
    TabOrder = 2
    OnClick = btnValidateClick
  end
  object Memo1: TMemo
    Left = 8
    Top = 104
    Width = 755
    Height = 288
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object btnValidateAll: TButton
    Left = 579
    Top = 71
    Width = 184
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Validate all KMR scripts'
    TabOrder = 4
    OnClick = btnValidateAllClick
  end
  object btnBrowsePath: TButton
    Left = 579
    Top = 8
    Width = 89
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Browse Path'
    TabOrder = 5
    OnClick = btnBrowsePathClick
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
