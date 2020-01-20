object FrmAbout: TFrmAbout
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'About Nanobrot'
  ClientHeight = 281
  ClientWidth = 533
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  DesignSize = (
    533
    281)
  PixelsPerInch = 96
  TextHeight = 13
  object imAppIcon: TImage
    Left = 12
    Top = 12
    Width = 256
    Height = 256
  end
  object LinkLabel1: TLinkLabel
    Left = 290
    Top = 71
    Width = 228
    Height = 69
    Caption = 
      #169'2019-2020 Vasily Makarov <a href="http://stone-voices.ru">stone' +
      '-voices.ru</a>'#13#10'Original C++ source codes:'#13#10#169'2013-2017 Karl Runm' +
      'o'#13#10#169'2017-2019 Claude Heiland-Allen <a href="https://mathr.co.uk/' +
      '">mathr.co.uk</a>'#13#10'License: GNU AGPL v3+'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 2105376
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnLinkClick = LinkLabel1LinkClick
  end
  object btOK: TSpTBXButton
    Left = 445
    Top = 248
    Width = 75
    Height = 25
    Caption = 'OK'
    Anchors = [akTop, akRight]
    TabOrder = 1
    Default = True
    ModalResult = 1
  end
  object LbLogo: TSpTBXLabel
    Left = 290
    Top = -4
    Width = 201
    Height = 66
    Caption = 'Nanobrot'
    Font.Charset = ANSI_CHARSET
    Font.Color = 2105376
    Font.Height = -43
    Font.Name = 'Comic Sans MS'
    Font.Style = [fsBold]
    ParentFont = False
    Visible = False
  end
  object lbVersion: TSpTBXLabel
    Left = 290
    Top = 52
    Width = 48
    Height = 19
    Caption = 'Version'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 2105376
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LinkLabel2: TLinkLabel
    Left = 289
    Top = 140
    Width = 192
    Height = 56
    Caption = 
      'Third party components and libraries:'#13#10#8226' SpTBXLib <a href="http:' +
      '//www.silverpointdevelopment.com/sptbxlib/index.htm">silverpoint' +
      'development.com</a>'#13#10#8226' Graphics32 <a href="http://www.graphics32' +
      '.org/">graphics32.org</a>'#13#10#8226' LZMA SDK <a href="https://www.7-zip' +
      '.org/">www.7-zip.org</a>'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 2105376
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    OnLinkClick = LinkLabel1LinkClick
  end
  object LinkLabel3: TLinkLabel
    Left = 290
    Top = 196
    Width = 165
    Height = 43
    Caption = 
      'Color maps from:'#13#10#8226' Apophysis <a href="http://www.apophysis.org/' +
      '">www.apophysis.org</a>'#13#10#8226' Jason Rample <a href="http://softolog' +
      'y.com.au/">softology.com.au</a>'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 2105376
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    OnLinkClick = LinkLabel1LinkClick
  end
end
