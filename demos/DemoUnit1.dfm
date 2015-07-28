object MainForm: TMainForm
  Left = 159
  Top = 134
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'CrystalPathFinding (cpf) library test'
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
  object pbMap: TPaintBox
    Left = 8
    Top = 40
    Width = 900
    Height = 600
    OnDblClick = pbMapDblClick
    OnMouseDown = pbMapMouseDown
    OnMouseMove = pbMapMouseMove
    OnMouseUp = pbMapMouseUp
    OnPaint = pbMapPaint
  end
  object lbDistance: TLabel
    Left = 8
    Top = 16
    Width = 123
    Height = 21
    Caption = 'Distance: 100.01'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clRed
    Font.Height = -19
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
  end
  object gbOptions: TGroupBox
    Left = 916
    Top = 532
    Width = 140
    Height = 40
    TabOrder = 6
    object cbSectorTest: TCheckBox
      Left = 8
      Top = 20
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
      Top = 4
      Width = 97
      Height = 17
      Caption = ' Caching'
      Checked = True
      ParentShowHint = False
      ShowHint = True
      State = cbChecked
      TabOrder = 1
      OnClick = OnMapOptionChanged
    end
  end
  object gbTileWeigths: TGroupBox
    Left = 916
    Top = 108
    Width = 140
    Height = 333
    Caption = '      Tile weights  '
    ParentShowHint = False
    ShowHint = False
    TabOrder = 0
    object pbTile1: TPaintBox
      Tag = 1
      Left = 10
      Top = 16
      Width = 60
      Height = 60
      OnClick = OnTileClick
      OnPaint = OnTilePaint
    end
    object pbTile2: TPaintBox
      Tag = 2
      Left = 10
      Top = 95
      Width = 60
      Height = 60
      OnClick = OnTileClick
      OnPaint = OnTilePaint
    end
    object pbTile3: TPaintBox
      Tag = 3
      Left = 10
      Top = 174
      Width = 60
      Height = 60
      OnClick = OnTileClick
      OnPaint = OnTilePaint
    end
    object pbTile4: TPaintBox
      Tag = 4
      Left = 10
      Top = 253
      Width = 60
      Height = 60
      OnClick = OnTileClick
      OnPaint = OnTilePaint
    end
    object lbTile1: TLabel
      Left = 75
      Top = 66
      Width = 17
      Height = 13
      Caption = 'text'
    end
    object lbTile2: TLabel
      Left = 75
      Top = 144
      Width = 17
      Height = 13
      Caption = 'text'
    end
    object lbTile3: TLabel
      Left = 75
      Top = 223
      Width = 17
      Height = 13
      Caption = 'text'
    end
    object lbTile4: TLabel
      Left = 75
      Top = 303
      Width = 17
      Height = 13
      Caption = 'text'
    end
    object sbTile1: TScrollBar
      Tag = 1
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
      Hint = 'Custom tile weights'
      Checked = True
      ParentShowHint = False
      ShowHint = True
      State = cbChecked
      TabOrder = 1
      OnClick = cbUseWeightsClick
    end
    object sbTile2: TScrollBar
      Tag = 2
      Left = 11
      Top = 158
      Width = 120
      Height = 10
      Max = 200
      PageSize = 0
      TabOrder = 2
      OnChange = OnTileWeightChange
    end
    object sbTile3: TScrollBar
      Tag = 3
      Left = 11
      Top = 237
      Width = 120
      Height = 10
      Max = 200
      PageSize = 0
      TabOrder = 3
      OnChange = OnTileWeightChange
    end
    object sbTile4: TScrollBar
      Tag = 4
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
  object gbBarrierMode: TGroupBox
    Left = 916
    Top = 34
    Width = 140
    Height = 73
    Caption = ' Barrier mode (right button) '
    TabOrder = 1
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
  object gbSpeedTest: TGroupBox
    Left = 916
    Top = 574
    Width = 140
    Height = 67
    Caption = ' Speed test '
    TabOrder = 2
    object seIterationsCount: TSpinEdit
      Left = 11
      Top = 18
      Width = 120
      Height = 22
      Hint = 'Count of times'
      MaxValue = 1000000
      MinValue = 1
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Value = 1000
    end
    object btnSpeedTest: TButton
      Left = 10
      Top = 42
      Width = 122
      Height = 22
      Caption = 'Run'
      TabOrder = 1
      OnClick = btnSpeedTestClick
    end
  end
  object btnRandom: TButton
    Left = 755
    Top = 13
    Width = 75
    Height = 25
    Caption = 'Random'
    TabOrder = 3
    OnClick = btnRandomClick
  end
  object btnClear: TButton
    Left = 676
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
    Caption = ' Map mode'
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
  object btnSave: TButton
    Left = 834
    Top = 13
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 7
    OnClick = btnSaveClick
  end
end
