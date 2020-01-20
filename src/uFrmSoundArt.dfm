object FrmSoundArt: TFrmSoundArt
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Sound Art Export'
  ClientHeight = 372
  ClientWidth = 706
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PnDock: TSpTBXPanel
    Left = 0
    Top = 26
    Width = 169
    Height = 327
    Align = alLeft
    TabOrder = 0
    object PnExportSettings: TSpTBXPanel
      Left = 2
      Top = 2
      Width = 165
      Height = 323
      Color = clCream
      Align = alClient
      TabOrder = 0
      Borders = False
      object LbGpHeader: TSpTBXLabel
        Left = 0
        Top = 0
        Width = 165
        Height = 25
        Caption = 'Export Settings'
        Color = clCream
        Align = alTop
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        Alignment = taCenter
      end
      object GpGeneralSettings: TGridPanel
        AlignWithMargins = True
        Left = 0
        Top = 25
        Width = 165
        Height = 152
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alTop
        BevelOuter = bvNone
        Color = clCream
        ColumnCollection = <
          item
            SizeStyle = ssAbsolute
            Value = 80.000000000000000000
          end
          item
            Value = 100.000000000000000000
          end>
        ControlCollection = <
          item
            Column = 0
            Control = LbImgWidth
            Row = 0
          end
          item
            Column = 0
            Control = LbImgHeight
            Row = 1
          end
          item
            Column = 1
            Control = EdImgWidth
            Row = 0
          end
          item
            Column = 1
            Control = EdImgHeight
            Row = 1
          end
          item
            Column = 0
            Control = LbSweepAngle
            Row = 3
          end
          item
            Column = 1
            Control = EdSweepAngle
            Row = 3
          end
          item
            Column = 0
            Control = LbStartAngle
            Row = 2
          end
          item
            Column = 1
            Control = EdStartAngle
            Row = 2
          end
          item
            Column = 0
            Control = LbSupersample
            Row = 4
          end
          item
            Column = 1
            Control = EdSupersample
            Row = 4
          end
          item
            Column = 0
            Control = LbFlipVertical
            Row = 5
          end
          item
            Column = 1
            Control = CkFlipVertical
            Row = 5
          end>
        Padding.Top = 1
        Padding.Right = 5
        Padding.Bottom = 2
        ParentBackground = False
        RowCollection = <
          item
            SizeStyle = ssAbsolute
            Value = 24.000000000000000000
          end
          item
            SizeStyle = ssAbsolute
            Value = 24.000000000000000000
          end
          item
            SizeStyle = ssAbsolute
            Value = 24.000000000000000000
          end
          item
            SizeStyle = ssAbsolute
            Value = 24.000000000000000000
          end
          item
            SizeStyle = ssAbsolute
            Value = 24.000000000000000000
          end
          item
            SizeStyle = ssAbsolute
            Value = 24.000000000000000000
          end
          item
            Value = 100.000000000000000000
          end>
        ShowCaption = False
        TabOrder = 1
        object LbImgWidth: TSpTBXLabel
          Left = 0
          Top = 2
          Width = 75
          Height = 21
          Caption = 'Width:'
          Align = alClient
          ParentColor = True
          Alignment = taRightJustify
          FocusControl = EdImgWidth
        end
        object LbImgHeight: TSpTBXLabel
          Left = 0
          Top = 26
          Width = 75
          Height = 21
          Caption = 'Height:'
          Align = alClient
          ParentColor = True
          Alignment = taRightJustify
          FocusControl = EdImgHeight
        end
        object EdImgWidth: TSpTBXSpinEdit
          Left = 80
          Top = 2
          Width = 75
          Height = 21
          Hint = 'Image width value'
          Align = alClient
          Alignment = taLeftJustify
          TabOrder = 1
          AsInteger = 1000
          SpinButton.Left = 57
          SpinButton.Top = 0
          SpinButton.Width = 14
          SpinButton.Height = 17
          SpinButton.Align = alRight
          SpinOptions.MaxValue = 65536.000000000000000000
          SpinOptions.MinValue = 10.000000000000000000
          SpinOptions.Postfix = ' px'
          SpinOptions.Value = 1000.000000000000000000
          OnValueChanged = EdImgWidthValueChanged
        end
        object EdImgHeight: TSpTBXSpinEdit
          Left = 80
          Top = 26
          Width = 75
          Height = 21
          Hint = 'Image height value'
          Align = alClient
          Alignment = taLeftJustify
          TabOrder = 3
          AsInteger = 500
          SpinButton.Left = 57
          SpinButton.Top = 0
          SpinButton.Width = 14
          SpinButton.Height = 17
          SpinButton.Align = alRight
          SpinOptions.MaxValue = 65536.000000000000000000
          SpinOptions.MinValue = 10.000000000000000000
          SpinOptions.Postfix = ' px'
          SpinOptions.Value = 500.000000000000000000
          OnValueChanged = EdImgWidthValueChanged
        end
        object LbSweepAngle: TSpTBXLabel
          Left = 0
          Top = 74
          Width = 75
          Height = 21
          Caption = 'Sweep Angle:'
          Align = alClient
          ParentColor = True
          Alignment = taRightJustify
          FocusControl = EdSweepAngle
        end
        object EdSweepAngle: TSpTBXSpinEdit
          Left = 80
          Top = 74
          Width = 75
          Height = 21
          Hint = 'Image height value'
          Align = alClient
          Alignment = taLeftJustify
          TabOrder = 7
          AsInteger = 360
          SpinButton.Left = 57
          SpinButton.Top = 0
          SpinButton.Width = 14
          SpinButton.Height = 17
          SpinButton.Align = alRight
          SpinOptions.Decimal = 4
          SpinOptions.MaxValue = 360.000000000000000000
          SpinOptions.MinValue = 0.010000000000000000
          SpinOptions.Postfix = #176
          SpinOptions.Value = 360.000000000000000000
          SpinOptions.ValueType = spnFloat
          OnValueChanged = EdSweepAngleValueChanged
        end
        object LbStartAngle: TSpTBXLabel
          Left = 0
          Top = 50
          Width = 75
          Height = 21
          Caption = 'Start Angle:'
          Align = alClient
          ParentColor = True
          Alignment = taRightJustify
          FocusControl = EdStartAngle
        end
        object EdStartAngle: TSpTBXSpinEdit
          Left = 80
          Top = 50
          Width = 75
          Height = 21
          Hint = 'Image height value'
          Align = alClient
          Alignment = taLeftJustify
          TabOrder = 5
          SpinButton.Left = 57
          SpinButton.Top = 0
          SpinButton.Width = 14
          SpinButton.Height = 17
          SpinButton.Align = alRight
          SpinOptions.Decimal = 4
          SpinOptions.MaxValue = 360.000000000000000000
          SpinOptions.MinValue = -360.000000000000000000
          SpinOptions.Postfix = #176
          SpinOptions.ValueType = spnFloat
          OnValueChanged = EdStartAngleValueChanged
        end
        object LbSupersample: TSpTBXLabel
          Left = 0
          Top = 98
          Width = 75
          Height = 21
          Caption = 'Supersample:'
          Align = alClient
          ParentColor = True
          Alignment = taRightJustify
          FocusControl = EdSupersample
        end
        object EdSupersample: TSpTBXSpinEdit
          Left = 80
          Top = 98
          Width = 75
          Height = 21
          Hint = 'Image height value'
          Align = alClient
          Alignment = taLeftJustify
          TabOrder = 9
          AsInteger = 2
          SpinButton.Left = 57
          SpinButton.Top = 0
          SpinButton.Width = 14
          SpinButton.Height = 17
          SpinButton.Align = alRight
          SpinOptions.MaxValue = 16.000000000000000000
          SpinOptions.MinValue = 1.000000000000000000
          SpinOptions.Postfix = 'x'
          SpinOptions.Value = 2.000000000000000000
          OnValueChanged = EdSupersampleValueChanged
        end
        object LbFlipVertical: TSpTBXLabel
          Left = 0
          Top = 122
          Width = 75
          Height = 21
          Caption = 'Flip Vertical:'
          Align = alClient
          ParentColor = True
          Alignment = taRightJustify
          FocusControl = CkFlipVertical
        end
        object CkFlipVertical: TSpTBXCheckBox
          Left = 80
          Top = 122
          Width = 75
          Height = 21
          Caption = 'False'
          Align = alClient
          TabOrder = 11
          OnClick = CkFlipVerticalClick
          OnDrawCaption = CkFlipVerticalDrawCaption
        end
      end
    end
  end
  object SpTBXDock1: TSpTBXDock
    Left = 0
    Top = 0
    Width = 706
    Height = 26
    AllowDrag = False
    object SpTBXToolbar1: TSpTBXToolbar
      Left = 0
      Top = 0
      DockPos = 0
      FullSize = True
      Images = FrmMain.IlMain16
      TabOrder = 0
      Caption = 'SpTBXToolbar1'
      object smPreset: TSpTBXSubmenuItem
        Caption = 'Presets'
        DisplayMode = nbdmImageAndText
        ImageIndex = 24
        Images = FrmMain.IlMain16
        Options = [tboDropdownArrow]
        OnPopup = smPresetPopup
        object SpTBXItem3: TSpTBXItem
          Action = cmdPresetAdd
          Images = FrmMain.IlMain16
        end
        object SpTBXItem4: TSpTBXItem
          Action = cmdPresetManager
          Images = FrmMain.IlMain16
        end
        object SpTBXSeparatorItem2: TSpTBXSeparatorItem
        end
      end
      object SpTBXSeparatorItem5: TSpTBXSeparatorItem
      end
      object SpTBXItem2: TSpTBXItem
        Action = cmdFileExportImage
        Images = FrmMain.IlMain16
      end
      object SpTBXItem5: TSpTBXItem
        Action = cmdFileExportLevelMap
      end
      object SpTBXSeparatorItem1: TSpTBXSeparatorItem
      end
      object SpTBXItem1: TSpTBXItem
        Action = cmdRender
        DisplayMode = nbdmImageAndText
        Images = FrmMain.IlMain16
      end
    end
  end
  object StatusBar: TSpTBXStatusBar
    Left = 0
    Top = 353
    Width = 706
    Height = 19
    Images = FrmMain.IlSmall
    OnDrawDockBackground = StatusBarDrawDockBackground
    object SbLbReady: TSpTBXLabelItem
      Caption = 'Ready'
      Wrapping = twEndEllipsis
      ImageIndex = 0
      CustomWidth = 150
      CustomHeight = 15
    end
    object SpTBXSeparatorItem3: TSpTBXSeparatorItem
    end
    object SbiTime: TSpTBXLabelItem
      ImageIndex = 1
      CustomHeight = 15
    end
    object SbLbTime: TSpTBXLabelItem
      Wrapping = twEndEllipsis
      CustomWidth = 350
      CustomHeight = 15
    end
    object SpTBXSeparatorItem4: TSpTBXSeparatorItem
    end
    object SbiInfo: TSpTBXLabelItem
      ImageIndex = 2
      CustomHeight = 15
    end
    object SbLbInfo: TSpTBXLabelItem
      Wrapping = twEndEllipsis
      CustomHeight = 15
      MinWidth = 150
    end
  end
  object PnImgView: TSpTBXPanel
    Left = 169
    Top = 26
    Width = 537
    Height = 327
    Caption = 'PnImgView'
    Align = alClient
    TabOrder = 3
    object ImgView: TImgView32
      Left = 2
      Top = 2
      Width = 533
      Height = 323
      Align = alClient
      Bitmap.ResamplerClassName = 'TLinearResampler'
      BitmapAlign = baCustom
      Scale = 1.000000000000000000
      ScaleMode = smOptimal
      ScrollBars.ShowHandleGrip = True
      ScrollBars.Style = rbsDefault
      ScrollBars.Size = 17
      ScrollBars.Visibility = svHidden
      OverSize = 0
      TabOrder = 0
    end
  end
  object ActionList1: TActionList
    Images = FrmMain.IlMain16
    Left = 395
    Top = 108
    object cmdRender: TAction
      Category = 'View'
      Caption = 'Render'
      Hint = 'Render fractal'
      ImageIndex = 0
      ShortCut = 120
      OnExecute = cmdRenderExecute
      OnUpdate = cmdRenderUpdate
    end
    object cmdFileExportImage: TAction
      Category = 'File'
      Caption = 'Export Image...'
      Hint = 'Export Image'
      ImageIndex = 6
      OnExecute = cmdFileExportImageExecute
      OnUpdate = cmdEnabledUpdate
    end
    object cmdPresetManager: TAction
      Category = 'Preset'
      Caption = 'Preset Manager...'
      Hint = 'Show preset manager'
      ImageIndex = 26
      OnExecute = cmdPresetManagerExecute
      OnUpdate = cmdEnabledUpdate
    end
    object cmdPresetAdd: TAction
      Category = 'Preset'
      Caption = 'Add Preset...'
      Hint = 'Add Preset'
      ImageIndex = 25
      OnExecute = cmdPresetAddExecute
      OnUpdate = cmdEnabledUpdate
    end
    object cmdFileExportLevelMap: TAction
      Category = 'File'
      Caption = 'Export Level Map...'
      Hint = 'Export Level Map'
      ImageIndex = 21
      OnExecute = cmdFileExportLevelMapExecute
      OnUpdate = cmdEnabledUpdate
    end
  end
  object DlgSaveImg: TFileSaveDialog
    DefaultExtension = '.png'
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'Portable Network Graphics (*.png)'
        FileMask = '*.png'
      end
      item
        DisplayName = 'All Files (*.*)'
        FileMask = '*.*'
      end>
    Options = [fdoOverWritePrompt]
    Left = 487
    Top = 108
  end
  object DlgSaveMap: TFileSaveDialog
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'Nanobrot map file (*.nbm)'
        FileMask = '*.nbm'
      end
      item
        DisplayName = 'All Files (*.*)'
        FileMask = '*.*'
      end>
    Options = [fdoOverWritePrompt]
    Left = 572
    Top = 108
  end
end
