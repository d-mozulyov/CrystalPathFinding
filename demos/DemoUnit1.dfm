object MainForm: TMainForm
  Left = 159
  Top = 134
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'CrystalPathFinding (CPF) library test'
  ClientHeight = 652
  ClientWidth = 1072
  Color = clBtnFace
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pbMap: TPaintBox
    Left = 8
    Top = 44
    Width = 900
    Height = 600
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    OnDblClick = pbMapDblClick
    OnMouseDown = pbMapMouseDown
    OnMouseMove = pbMapMouseMove
    OnMouseUp = pbMapMouseUp
    OnPaint = pbMapPaint
  end
  object gbTileWeigths: TGroupBox
    Left = 916
    Top = 91
    Width = 149
    Height = 380
    Caption = '        Tile weights '
    ParentShowHint = False
    ShowHint = False
    TabOrder = 6
    object pbTile1: TPaintBox
      Tag = 1
      Left = 11
      Top = 21
      Width = 60
      Height = 60
      OnClick = OnTileClick
      OnPaint = OnTilePaint
    end
    object pbTile2: TPaintBox
      Tag = 2
      Left = 11
      Top = 111
      Width = 60
      Height = 60
      OnClick = OnTileClick
      OnPaint = OnTilePaint
    end
    object pbTile3: TPaintBox
      Tag = 3
      Left = 11
      Top = 201
      Width = 60
      Height = 60
      OnClick = OnTileClick
      OnPaint = OnTilePaint
    end
    object pbTile4: TPaintBox
      Tag = 4
      Left = 11
      Top = 291
      Width = 60
      Height = 60
      OnClick = OnTileClick
      OnPaint = OnTilePaint
    end
    object lbTile1: TLabel
      Left = 76
      Top = 70
      Width = 22
      Height = 13
      Caption = '1.00'
    end
    object lbTile2: TLabel
      Left = 76
      Top = 160
      Width = 22
      Height = 13
      Caption = '1.00'
    end
    object lbTile3: TLabel
      Left = 76
      Top = 250
      Width = 22
      Height = 13
      Caption = '1.00'
    end
    object lbTile4: TLabel
      Left = 76
      Top = 340
      Width = 22
      Height = 13
      Caption = '1.00'
    end
    object sbTile1: TScrollBar
      Tag = 1
      Left = 12
      Top = 88
      Width = 125
      Height = 13
      Max = 200
      PageSize = 0
      Position = 20
      TabOrder = 1
      OnChange = OnTileWeightChange
    end
    object cbUseWeights: TCheckBox
      Left = 13
      Top = -1
      Width = 84
      Height = 17
      Hint = 'Custom tile weights'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = cbUseWeightsClick
    end
    object sbTile2: TScrollBar
      Tag = 2
      Left = 12
      Top = 178
      Width = 125
      Height = 13
      Max = 200
      PageSize = 0
      Position = 20
      TabOrder = 2
      OnChange = OnTileWeightChange
    end
    object sbTile3: TScrollBar
      Tag = 3
      Left = 12
      Top = 268
      Width = 125
      Height = 13
      Max = 200
      PageSize = 0
      Position = 20
      TabOrder = 3
      OnChange = OnTileWeightChange
    end
    object sbTile4: TScrollBar
      Tag = 4
      Left = 12
      Top = 358
      Width = 125
      Height = 13
      Max = 200
      PageSize = 0
      Position = 20
      TabOrder = 4
      OnChange = OnTileWeightChange
    end
  end
  object gbBarrierMode: TGroupBox
    Left = 916
    Top = 10
    Width = 149
    Height = 77
    Caption = 'Barrier (right mouse button)'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    object pbBarrier: TPaintBox
      Left = 19
      Top = 21
      Width = 45
      Height = 45
      Hint = 'Barriers'
      ParentShowHint = False
      ShowHint = True
      OnClick = OnTileClick
      OnPaint = OnTilePaint
    end
    object pbExclude: TPaintBox
      Tag = -1
      Left = 77
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
  object gbPerformanceTest: TGroupBox
    Left = 916
    Top = 556
    Width = 149
    Height = 90
    Caption = ' Performance test '
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 8
    object seIterationsCount: TSpinEdit
      Left = 11
      Top = 32
      Width = 126
      Height = 22
      Hint = 'Count of times'
      MaxValue = 1000000
      MinValue = 1
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Value = 10000
    end
    object btnPerformanceTest: TButton
      Left = 10
      Top = 60
      Width = 128
      Height = 25
      Caption = 'Run'
      TabOrder = 1
      OnClick = btnPerformanceTestClick
    end
  end
  object cbTestingMode: TComboBox
    Left = 139
    Top = 16
    Width = 224
    Height = 22
    Hint = 'Start points and caching'
    Style = csDropDownList
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemIndex = 0
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    Text = 'One start point'
    OnChange = cbTestingModeChange
    Items.Strings = (
      'One start point'
      'One start point - caching'
      'Many start points'
      'Many start points (standard)'
      'Many start points (standard) - caching')
  end
  object cbMapKind: TComboBox
    Left = 8
    Top = 16
    Width = 125
    Height = 22
    Hint = 'Map kind'
    Style = csDropDownList
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemIndex = 0
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    Text = 'mkSimple'
    OnChange = cbMapKindChange
    Items.Strings = (
      'mkSimple'
      'mkDiagonal'
      'mkDiagonalEx'
      'mkHexagonal')
  end
  object btnSave: TButton
    Left = 834
    Top = 15
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 4
    OnClick = btnSaveClick
  end
  object btnRandom: TButton
    Left = 755
    Top = 15
    Width = 75
    Height = 25
    Caption = 'Random'
    TabOrder = 3
    OnClick = btnRandomClick
  end
  object btnClear: TButton
    Left = 676
    Top = 15
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 2
    OnClick = btnClearClick
  end
  object gpOptions: TGroupBox
    Left = 916
    Top = 475
    Width = 149
    Height = 66
    TabOrder = 7
    object cbSectorTest: TCheckBox
      Left = 11
      Top = 14
      Width = 126
      Height = 17
      Caption = 'SectorTest'
      TabOrder = 0
      OnClick = cbSectorTestClick
    end
    object cbSameDiagonalWeight: TCheckBox
      Left = 11
      Top = 36
      Width = 126
      Height = 17
      Caption = 'SameDiagonalWeight'
      Enabled = False
      TabOrder = 1
      OnClick = cbSameDiagonalWeightClick
    end
  end
end
