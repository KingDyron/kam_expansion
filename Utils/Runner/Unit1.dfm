object Form2: TForm2
  Left = 244
  Top = 169
  Caption = 'Runner'
  ClientHeight = 641
  ClientWidth = 1097
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    1097
    641)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 127
    Width = 35
    Height = 13
    Caption = 'Cycles:'
  end
  object Label2: TLabel
    Left = 187
    Top = 256
    Width = 15
    Height = 13
    Alignment = taRightJustify
    Caption = '     '
  end
  object Label4: TLabel
    Left = 103
    Top = 127
    Width = 72
    Height = 13
    Caption = 'Duration (min):'
  end
  object Label5: TLabel
    Left = 187
    Top = 236
    Width = 15
    Height = 13
    Alignment = taRightJustify
    Caption = '     '
  end
  object Label6: TLabel
    Left = 184
    Top = 276
    Width = 18
    Height = 13
    Alignment = taRightJustify
    Caption = '      '
  end
  object Label7: TLabel
    Left = 8
    Top = 168
    Width = 28
    Height = 13
    Caption = 'Seed:'
  end
  object Label8: TLabel
    Left = 184
    Top = 296
    Width = 18
    Height = 13
    Alignment = taRightJustify
    Caption = '      '
  end
  object Label9: TLabel
    Left = 8
    Top = 215
    Width = 18
    Height = 13
    Caption = '      '
  end
  object Label10: TLabel
    Left = 8
    Top = 256
    Width = 18
    Height = 13
    Caption = '      '
  end
  object Label11: TLabel
    Left = 8
    Top = 276
    Width = 18
    Height = 13
    Caption = '      '
  end
  object Label12: TLabel
    Left = 184
    Top = 316
    Width = 18
    Height = 13
    Alignment = taRightJustify
    Caption = '      '
  end
  object btnRun: TButton
    Left = 8
    Top = 412
    Width = 89
    Height = 38
    Caption = 'Run'
    Enabled = False
    TabOrder = 0
    OnClick = btnRunClick
  end
  object seCycles: TSpinEdit
    Left = 8
    Top = 144
    Width = 89
    Height = 22
    MaxValue = 1000000
    MinValue = 1
    TabOrder = 1
    Value = 1
  end
  object ListBox1: TListBox
    Left = 8
    Top = 8
    Width = 185
    Height = 113
    ItemHeight = 13
    TabOrder = 2
    OnClick = ListBox1Click
  end
  object Memo2: TMemo
    Left = 8
    Top = 496
    Width = 185
    Height = 133
    BorderStyle = bsNone
    Color = clBtnFace
    Lines.Strings = (
      'Runner'
      ''
      'Tool to run a game in pure simulation '
      'mode to test distribution of results '
      'and help catch bugs related to that.'
      ''
      'For example:'
      '  TestStone runs a stone mining that '
      'in theory should yeild same amount '
      'of stone each run')
    ReadOnly = True
    TabOrder = 3
  end
  object PageControl1: TPageControl
    Left = 208
    Top = 8
    Width = 881
    Height = 625
    ActivePage = TabSheet3
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 4
    OnChange = PageControl1Change
    object TabSheet1: TTabSheet
      Caption = 'Results'
      OnResize = TabSheetResize
      DesignSize = (
        873
        597)
      object Image1: TImage
        Left = 40
        Top = 8
        Width = 825
        Height = 569
        Anchors = [akLeft, akTop, akRight, akBottom]
        ExplicitWidth = 633
        ExplicitHeight = 273
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Distribution'
      ImageIndex = 1
      OnResize = TabSheetResize
      DesignSize = (
        873
        597)
      object Image2: TImage
        Left = 40
        Top = 8
        Width = 825
        Height = 569
        Anchors = [akLeft, akTop, akRight, akBottom]
        ExplicitWidth = 633
        ExplicitHeight = 273
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Times'
      ImageIndex = 2
      OnResize = TabSheetResize
      DesignSize = (
        873
        597)
      object Image3: TImage
        Left = 40
        Top = 8
        Width = 825
        Height = 569
        Anchors = [akLeft, akTop, akRight, akBottom]
        ExplicitWidth = 633
        ExplicitHeight = 273
      end
      object Label3: TLabel
        Left = 720
        Top = 8
        Width = 47
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Threshold'
        ExplicitLeft = 528
      end
      object TrackBar1: TTrackBar
        Left = 712
        Top = 24
        Width = 150
        Height = 33
        Anchors = [akTop, akRight]
        Max = 25
        Position = 10
        TabOrder = 0
        OnChange = TrackBar1Change
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Log'
      ImageIndex = 3
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 873
        Height = 597
        Align = alClient
        TabOrder = 0
      end
    end
    object Render: TTabSheet
      Caption = 'Render'
      ImageIndex = 4
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 873
        Height = 597
        Align = alClient
        Caption = 'Panel1'
        TabOrder = 0
      end
    end
  end
  object chkRender: TCheckBox
    Left = 8
    Top = 456
    Width = 57
    Height = 17
    Caption = 'Render'
    TabOrder = 5
  end
  object seDuration: TSpinEdit
    Left = 103
    Top = 144
    Width = 89
    Height = 22
    MaxValue = 1000000
    MinValue = 0
    TabOrder = 6
    Value = 100
  end
  object seSeed: TSpinEdit
    Left = 8
    Top = 187
    Width = 89
    Height = 22
    MaxValue = 2000000000
    MinValue = 0
    TabOrder = 7
    Value = 0
  end
  object rgAIType: TRadioGroup
    Left = 106
    Top = 172
    Width = 96
    Height = 53
    Caption = 'AI type'
    ItemIndex = 1
    Items.Strings = (
      'Classic'
      'Advanced')
    TabOrder = 8
  end
  object btnStop: TButton
    Left = 103
    Top = 412
    Width = 89
    Height = 38
    Caption = 'Stop'
    Enabled = False
    TabOrder = 9
    OnClick = btnStopClick
  end
  object btnPause: TButton
    Left = 104
    Top = 456
    Width = 89
    Height = 21
    Caption = 'Pause'
    Enabled = False
    TabOrder = 10
    Visible = False
    OnClick = btnPauseClick
  end
  object rgMaps: TRadioGroup
    Left = 1
    Top = 305
    Width = 72
    Height = 96
    Caption = 'Maps type'
    ItemIndex = 1
    Items.Strings = (
      'Classic'
      'MP 8P'
      'Fight'
      'Coop')
    TabOrder = 11
  end
  object rgTeams: TRadioGroup
    Left = 79
    Top = 328
    Width = 113
    Height = 73
    Caption = 'Teams'
    ItemIndex = 2
    Items.Strings = (
      'FFA'
      'Random alliances'
      'Random teams')
    TabOrder = 12
  end
end
