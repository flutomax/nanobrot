object FrmSaveAdvanced: TFrmSaveAdvanced
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Advanced Options'
  ClientHeight = 189
  ClientWidth = 390
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 15
  object ImgIcon: TImage
    Left = 16
    Top = 16
    Width = 32
    Height = 32
  end
  object CkIncludeLevelMaps: TSpTBXCheckBox
    Left = 64
    Top = 88
    Width = 189
    Height = 23
    Caption = 'Include Level Maps as .nbm file'
    TabOrder = 0
    OnClick = CkIncludeLevelMapsClick
  end
  object SpTBXPanel1: TSpTBXPanel
    Left = 0
    Top = 148
    Width = 390
    Height = 41
    Caption = 'SpTBXPanel1'
    Color = clBtnFace
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      390
      41)
    object BtnOK: TSpTBXButton
      Left = 217
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
      Left = 302
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
  object lbCompression: TSpTBXLabel
    Left = 64
    Top = 119
    Width = 153
    Height = 19
    Caption = 'Level maps file compression:'
    AutoSize = False
    Alignment = taRightJustify
    FocusControl = CbCompression
  end
  object CbCompression: TSpTBXComboBox
    Left = 223
    Top = 119
    Width = 154
    Height = 23
    Style = csDropDownList
    ItemHeight = 15
    TabOrder = 3
    Items.Strings = (
      'None'
      'Zlib (fast)'
      'Zlib (default)'
      'Zlib (best)'
      'LZMA (super)')
  end
  object LbInfo: TSpTBXLabel
    Left = 64
    Top = 10
    Width = 313
    Height = 63
    Caption = 
      'You can also save level maps, saving time for their repeated cal' +
      'culations.'#13#10'Maps can also be saved as compressed files.'
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 10040064
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    Wrapping = twWrap
  end
end
