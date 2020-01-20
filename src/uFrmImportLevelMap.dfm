object FrmImportLevelMap: TFrmImportLevelMap
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Import Level Map'
  ClientHeight = 170
  ClientWidth = 318
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 15
  object ImgIcon: TImage
    Left = 16
    Top = 16
    Width = 32
    Height = 32
  end
  object CbSupersample: TSpTBXComboBox
    Left = 217
    Top = 94
    Width = 88
    Height = 23
    Style = csDropDownList
    ItemHeight = 15
    TabOrder = 0
    OnChange = CbSupersampleChange
  end
  object LbInfo: TSpTBXLabel
    Left = 64
    Top = 10
    Width = 237
    Height = 44
    Caption = 'Select supersample value of importing level map.'
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 10040064
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    Wrapping = twWrap
  end
  object lbSupersample: TSpTBXLabel
    Left = 140
    Top = 96
    Width = 71
    Height = 19
    Caption = 'Supersample:'
    AutoSize = False
    Alignment = taRightJustify
  end
  object EdImgSize: TSpTBXEdit
    Left = 217
    Top = 65
    Width = 88
    Height = 23
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 3
    Text = '1000x1000 px'
  end
  object LbImgSize: TSpTBXLabel
    Left = 140
    Top = 67
    Width = 71
    Height = 19
    Caption = 'Image Size:'
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    Alignment = taRightJustify
  end
  object SpTBXPanel1: TSpTBXPanel
    Left = 0
    Top = 129
    Width = 318
    Height = 41
    Caption = 'SpTBXPanel1'
    Color = clBtnFace
    Align = alBottom
    TabOrder = 5
    DesignSize = (
      318
      41)
    object BtnOK: TSpTBXButton
      Left = 145
      Top = 8
      Width = 75
      Height = 25
      Caption = 'OK'
      Anchors = [akTop, akRight]
      TabOrder = 0
      Default = True
      ModalResult = 1
    end
    object BtnCancel: TSpTBXButton
      Left = 230
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Cancel'
      Anchors = [akTop, akRight]
      TabOrder = 1
      Cancel = True
      ModalResult = 2
    end
  end
end
