object Form1: TForm1
  Left = 159
  Top = 134
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 
    #1058#1077#1089#1090' '#1088#1072#1089#1095#1105#1090#1072' '#1082#1088#1072#1090#1095#1072#1081#1096#1077#1075#1086' '#1087#1091#1090#1080'. '#1041#1080#1073#1083#1080#1086#1090#1077#1082#1072' "Crystal Path Finding"' +
    ' (cpf)'
  ClientHeight = 647
  ClientWidth = 1064
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBox1: TPaintBox
    Left = 8
    Top = 40
    Width = 900
    Height = 600
    OnMouseDown = PaintBox1MouseDown
    OnMouseMove = PaintBox1MouseMove
    OnMouseUp = PaintBox1MouseUp
    OnPaint = PaintBox1Paint
  end
  object lbDistance: TLabel
    Left = 8
    Top = 16
    Width = 151
    Height = 21
    Caption = #1056#1072#1089#1089#1090#1086#1103#1085#1080#1077': 100.01'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clRed
    Font.Height = -19
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
  end
  object GroupBox5: TGroupBox
    Left = 916
    Top = 532
    Width = 140
    Height = 40
    TabOrder = 6
    object cbSectorTest: TCheckBox
      Left = 8
      Top = 4
      Width = 97
      Height = 17
      Caption = ' SectorTest'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = cbSectorTestClick
    end
    object cbCaching: TCheckBox
      Left = 8
      Top = 20
      Width = 97
      Height = 17
      Hint = #1059#1084#1085#1099#1081' '#1088#1072#1089#1095#1105#1090' '#1074#1077#1089#1086#1074
      Caption = ' Caching'
      Checked = True
      Enabled = False
      ParentShowHint = False
      ShowHint = True
      State = cbChecked
      TabOrder = 1
      OnClick = OnMapOptionChanged
    end
  end
  object GroupBox1: TGroupBox
    Left = 916
    Top = 108
    Width = 140
    Height = 333
    Caption = '      '#1058#1072#1081#1083#1099'  '
    ParentShowHint = False
    ShowHint = False
    TabOrder = 0
    object pbTile0: TPaintBox
      Left = 10
      Top = 16
      Width = 60
      Height = 60
      OnClick = OnTileClick
      OnPaint = OnTilePaint
    end
    object pbTile1: TPaintBox
      Tag = 1
      Left = 10
      Top = 95
      Width = 60
      Height = 60
      OnClick = OnTileClick
      OnPaint = OnTilePaint
    end
    object pbTile2: TPaintBox
      Tag = 2
      Left = 10
      Top = 174
      Width = 60
      Height = 60
      OnClick = OnTileClick
      OnPaint = OnTilePaint
    end
    object pbTile3: TPaintBox
      Tag = 3
      Left = 10
      Top = 253
      Width = 60
      Height = 60
      OnClick = OnTileClick
      OnPaint = OnTilePaint
    end
    object lbTile0: TLabel
      Left = 75
      Top = 66
      Width = 17
      Height = 13
      Caption = 'text'
    end
    object lbTile1: TLabel
      Left = 75
      Top = 144
      Width = 17
      Height = 13
      Caption = 'text'
    end
    object lbTile2: TLabel
      Left = 75
      Top = 223
      Width = 17
      Height = 13
      Caption = 'text'
    end
    object lbTile3: TLabel
      Left = 75
      Top = 303
      Width = 17
      Height = 13
      Caption = 'text'
    end
    object sbTile0: TScrollBar
      Left = 11
      Top = 80
      Width = 120
      Height = 10
      Max = 200
      PageSize = 0
      TabOrder = 0
      OnChange = OnTileWeightChange
    end
    object cbUseWeights: TCheckBox
      Left = 8
      Top = -2
      Width = 62
      Height = 17
      Hint = #1048#1089#1087#1086#1083#1100#1079#1086#1074#1072#1090#1100' '#1074#1077#1089#1072' '#1090#1072#1081#1083#1086#1074
      Checked = True
      ParentShowHint = False
      ShowHint = True
      State = cbChecked
      TabOrder = 1
      OnClick = cbUseWeightsClick
    end
    object sbTile1: TScrollBar
      Tag = 1
      Left = 11
      Top = 158
      Width = 120
      Height = 10
      Max = 200
      PageSize = 0
      TabOrder = 2
      OnChange = OnTileWeightChange
    end
    object sbTile2: TScrollBar
      Tag = 2
      Left = 11
      Top = 237
      Width = 120
      Height = 10
      Max = 200
      PageSize = 0
      TabOrder = 3
      OnChange = OnTileWeightChange
    end
    object sbTile3: TScrollBar
      Tag = 3
      Left = 11
      Top = 317
      Width = 120
      Height = 10
      Max = 200
      PageSize = 0
      TabOrder = 4
      OnChange = OnTileWeightChange
    end
  end
  object GroupBox3: TGroupBox
    Left = 916
    Top = 34
    Width = 140
    Height = 73
    Caption = '  '#1055#1088#1072#1074#1072#1103' '#1082#1085#1086#1087#1082#1072' '#1084#1099#1096#1080'  '
    TabOrder = 1
    object pbClear: TPaintBox
      Tag = -1
      Left = 19
      Top = 21
      Width = 45
      Height = 45
      Hint = #1055#1088#1077#1087#1103#1090#1089#1090#1074#1080#1077
      ParentShowHint = False
      ShowHint = True
      OnClick = OnTileClick
      OnPaint = OnTilePaint
    end
    object pbExclude: TPaintBox
      Tag = -2
      Left = 70
      Top = 21
      Width = 45
      Height = 45
      Hint = 'Excluded points'
      ParentShowHint = False
      ShowHint = True
      OnClick = OnTileClick
      OnPaint = OnTilePaint
    end
  end
  object GroupBox4: TGroupBox
    Left = 916
    Top = 574
    Width = 140
    Height = 67
    Caption = '  '#1058#1077#1089#1090' '#1089#1082#1086#1088#1086#1089#1090#1080'  '
    TabOrder = 2
    object seIterationsCount: TSpinEdit
      Left = 11
      Top = 18
      Width = 120
      Height = 22
      Hint = #1050#1086#1083#1080#1095#1077#1089#1090#1074#1086' '#1088#1072#1089#1095#1105#1090#1086#1074
      MaxValue = 1000000
      MinValue = 1
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Value = 1000
    end
    object btnTestSpeed: TButton
      Left = 10
      Top = 42
      Width = 122
      Height = 22
      Caption = #1047#1072#1087#1091#1089#1082
      TabOrder = 1
      OnClick = btnTestSpeedClick
    end
  end
  object btnRandom: TButton
    Left = 834
    Top = 13
    Width = 75
    Height = 25
    Caption = 'Random'
    TabOrder = 3
    OnClick = btnRandomClick
  end
  object btnClear: TButton
    Left = 755
    Top = 13
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 4
    OnClick = btnClearClick
  end
  object rgMapKind: TRadioGroup
    Left = 916
    Top = 442
    Width = 140
    Height = 88
    Caption = '  '#1056#1077#1078#1080#1084' '#1082#1072#1088#1090#1099'  '
    ItemIndex = 0
    Items.Strings = (
      'mmSimple'
      'mmDiagonal'
      'mmDiagonalEx'
      'mmHexagonal')
    ParentShowHint = False
    ShowHint = False
    TabOrder = 5
    OnClick = rgMapKindClick
  end
end
