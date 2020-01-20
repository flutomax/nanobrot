object FrmMain: TFrmMain
  Left = 0
  Top = 0
  Caption = 'Nanobrot'
  ClientHeight = 607
  ClientWidth = 984
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  ShowHint = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object DkTop: TSpTBXDock
    Left = 0
    Top = 0
    Width = 984
    Height = 60
    BoundLines = [blBottom]
    object TbFile: TSpTBXToolbar
      Left = 0
      Top = 25
      Images = IlMainN
      TabOrder = 0
      Caption = 'File'
      object SpTBXItem10: TSpTBXItem
        Action = cmdFileNew
      end
      object SpTBXItem11: TSpTBXItem
        Action = cmdFileOpen
      end
      object SpTBXItem43: TSpTBXItem
        Action = cmdFileRevert
      end
      object SpTBXSeparatorItem10: TSpTBXSeparatorItem
      end
      object SpTBXItem13: TSpTBXItem
        Action = cmdFileSave
      end
      object SpTBXItem12: TSpTBXItem
        Action = cmdFileSaveAs
      end
      object SpTBXItem14: TSpTBXItem
        Action = cmdFileExportImage
      end
      object SpTBXSeparatorItem12: TSpTBXSeparatorItem
      end
      object SpTBXItem45: TSpTBXItem
        Action = cmdFilePrint
      end
    end
    object TbMainMenu: TSpTBXToolbar
      Left = 0
      Top = 0
      CloseButton = False
      DockMode = dmCannotFloatOrChangeDocks
      DragHandleStyle = dhNone
      FullSize = True
      ProcessShortCuts = True
      ShrinkMode = tbsmWrap
      TabOrder = 1
      Caption = 'Main Menu'
      Customizable = False
      MenuBar = True
      object miFile: TSpTBXSubmenuItem
        Caption = 'File'
        object SpTBXItem1: TSpTBXItem
          Action = cmdFileNew
          Images = IlMain16
        end
        object SpTBXItem2: TSpTBXItem
          Action = cmdFileOpen
          Images = IlMain16
        end
        object SpTBXItem44: TSpTBXItem
          Action = cmdFileRevert
          Images = IlMain16
        end
        object SpTBXSeparatorItem11: TSpTBXSeparatorItem
        end
        object SpTBXItem6: TSpTBXItem
          Action = cmdFileSave
          Images = IlMain16
        end
        object SpTBXItem5: TSpTBXItem
          Action = cmdFileSaveAs
          Images = IlMain16
        end
        object SpTBXSeparatorItem1: TSpTBXSeparatorItem
        end
        object SpTBXItem4: TSpTBXItem
          Action = cmdFileImport
          Images = IlMain16
        end
        object SpTBXItem3: TSpTBXItem
          Action = cmdFileExport
          Images = IlMain16
        end
        object SpTBXSeparatorItem2: TSpTBXSeparatorItem
        end
        object SpTBXItem8: TSpTBXItem
          Action = cmdFileExportImage
          Images = IlMain16
        end
        object SpTBXItem7: TSpTBXItem
          Action = cmdFileShowLocation
          Images = IlMain16
        end
        object SpTBXSeparatorItem13: TSpTBXSeparatorItem
        end
        object SpTBXItem46: TSpTBXItem
          Action = cmdFilePrint
          Images = IlMain16
        end
        object msRfTop: TSpTBXSeparatorItem
        end
        object msRfBot: TSpTBXSeparatorItem
        end
        object SpTBXItem9: TSpTBXItem
          Action = cmdFileExit
          Images = IlMain16
        end
      end
      object SpTBXSubmenuItem2: TSpTBXSubmenuItem
        Caption = 'Edit'
        object SpTBXItem42: TSpTBXItem
          Action = cmdEditUndo
          Images = IlMain16
        end
        object SpTBXItem41: TSpTBXItem
          Action = cmdEditRedo
          Images = IlMain16
        end
        object SpTBXSeparatorItem9: TSpTBXSeparatorItem
        end
        object SpTBXItem19: TSpTBXItem
          Action = cmdEditCut
          Images = IlMain16
        end
        object SpTBXItem18: TSpTBXItem
          Action = cmdEditCopy
          Images = IlMain16
        end
        object SpTBXItem17: TSpTBXItem
          Action = cmdEditPaste
          Images = IlMain16
        end
        object SpTBXItem16: TSpTBXItem
          Action = cmdEditDelete
          Images = IlMain16
        end
        object SpTBXItem15: TSpTBXItem
          Action = cmdEditSelectAll
        end
      end
      object SpTBXSubmenuItem3: TSpTBXSubmenuItem
        Caption = 'View'
        object SpTBXItem21: TSpTBXItem
          Action = cmdRender
          Images = IlMain16
        end
        object SpTBXItem20: TSpTBXItem
          Action = cmdViewRecoloring
        end
        object SpTBXSeparatorItem5: TSpTBXSeparatorItem
        end
        object SmiMagnification: TSpTBXSubmenuItem
          Caption = 'Magnification'
          ImageIndex = 13
          Images = IlMain16
          OnPopup = SmiMagnificationPopup
          object SpTBXItem25: TSpTBXItem
            Action = cmdView6
            GroupIndex = 1
            RadioItem = True
          end
          object SpTBXItem39: TSpTBXItem
            Action = cmdView12
            GroupIndex = 1
            RadioItem = True
          end
          object SpTBXItem38: TSpTBXItem
            Action = cmdView25
            GroupIndex = 1
            RadioItem = True
          end
          object SpTBXItem37: TSpTBXItem
            Action = cmdView50
            GroupIndex = 1
            RadioItem = True
          end
          object SpTBXItem36: TSpTBXItem
            Action = cmdView66
            GroupIndex = 1
            RadioItem = True
          end
          object SpTBXItem35: TSpTBXItem
            Action = cmdView100
            GroupIndex = 1
            RadioItem = True
          end
          object SpTBXItem34: TSpTBXItem
            Action = cmdView150
            GroupIndex = 1
            RadioItem = True
          end
          object SpTBXItem33: TSpTBXItem
            Action = cmdView200
            GroupIndex = 1
            RadioItem = True
          end
          object SpTBXItem32: TSpTBXItem
            Action = cmdView300
            GroupIndex = 1
            RadioItem = True
          end
          object SpTBXItem31: TSpTBXItem
            Action = cmdView400
            GroupIndex = 1
            RadioItem = True
          end
          object SpTBXItem30: TSpTBXItem
            Action = cmdView800
            GroupIndex = 1
            RadioItem = True
          end
          object SpTBXItem29: TSpTBXItem
            Action = cmdView1600
            GroupIndex = 1
            RadioItem = True
          end
        end
        object SpTBXSeparatorItem14: TSpTBXSeparatorItem
        end
        object SpTBXSubmenuItem5: TSpTBXSubmenuItem
          Caption = 'Toolbars'
          ImageIndex = 22
          Images = IlMain16
          object SpTBXItem50: TSpTBXItem
            Caption = 'File'
            Control = TbFile
          end
          object SpTBXItem51: TSpTBXItem
            Caption = 'Edit'
            Control = TbEdit
          end
          object SpTBXItem52: TSpTBXItem
            Caption = 'View'
            Control = TbView
          end
          object SpTBXItem53: TSpTBXItem
            Caption = 'Tools'
            Control = TbTools
          end
          object SpTBXItem56: TSpTBXItem
            Caption = 'Statusbar'
            Control = StatusBar
          end
          object SpTBXSeparatorItem15: TSpTBXSeparatorItem
          end
          object SpTBXItem59: TSpTBXItem
            Action = cmdViewSmallIcons
          end
          object SpTBXItem54: TSpTBXItem
            Action = cmdViewCustomizerReset
          end
        end
      end
      object SpTBXSubmenuItem4: TSpTBXSubmenuItem
        Caption = 'Tools'
        object SpTBXItem49: TSpTBXItem
          Action = cmdToolOptions
          Images = IlMain16
        end
        object SpTBXItem58: TSpTBXItem
          Action = cmdToolSoundArtExport
          Images = IlMain16
        end
        object SpTBXSeparatorItem16: TSpTBXSeparatorItem
        end
        object SpTBXItem55: TSpTBXItem
          Action = cmdToolsUpdateColorMaps
          Images = IlMain16
        end
      end
      object SpTBXSubmenuItem1: TSpTBXSubmenuItem
        Caption = 'Help'
        object SpTBXItem60: TSpTBXItem
          Action = cmdHelpHomePage
          Images = IlMain16
        end
        object SpTBXItem57: TSpTBXItem
          Action = cmdHelpAbout
          Images = IlMain16
        end
      end
    end
    object TbEdit: TSpTBXToolbar
      Left = 239
      Top = 25
      DockPos = 159
      Images = IlMainN
      TabOrder = 2
      Caption = 'Edit'
      object SpTBXItem40: TSpTBXItem
        Action = cmdEditUndo
      end
      object SpTBXItem28: TSpTBXItem
        Action = cmdEditRedo
      end
      object SpTBXSeparatorItem8: TSpTBXSeparatorItem
      end
      object SpTBXItem24: TSpTBXItem
        Action = cmdEditCut
      end
      object SpTBXItem23: TSpTBXItem
        Action = cmdEditCopy
      end
      object SpTBXItem22: TSpTBXItem
        Action = cmdEditPaste
      end
    end
    object TbView: TSpTBXToolbar
      Left = 410
      Top = 25
      DockPos = 248
      Images = IlMainN
      TabOrder = 3
      Caption = 'View'
      object SiMaginification: TSpTBXSubmenuItem
        Hint = 'Maginification'
        ImageIndex = 13
        Options = [tboDropdownArrow]
        CustomWidth = 42
        LinkSubitems = SmiMagnification
      end
      object SpTBXSeparatorItem6: TSpTBXSeparatorItem
      end
      object SpTBXItem26: TSpTBXItem
        Action = cmdViewRecoloring
      end
      object SpTBXLabelItem1: TSpTBXLabelItem
      end
      object SpTBXItem27: TSpTBXItem
        Action = cmdRender
        DisplayMode = nbdmImageAndText
      end
      object SpTBXMRUListItem1: TSpTBXMRUListItem
      end
    end
    object TbTools: TSpTBXToolbar
      Left = 577
      Top = 25
      DockPos = 534
      Images = IlMainN
      TabOrder = 4
      Caption = 'Tools'
      object SpTBXItem47: TSpTBXItem
        Action = cmdToolOptions
      end
      object SpTBXLabelItem2: TSpTBXLabelItem
      end
      object SpTBXItem48: TSpTBXItem
        Action = cmdToolSoundArtExport
      end
    end
  end
  object StatusBar: TSpTBXStatusBar
    Left = 0
    Top = 588
    Width = 984
    Height = 19
    Images = IlSmall
    OnDrawDockBackground = StatusBarDrawDockBackground
    object SbLbReady: TSpTBXLabelItem
      Caption = 'Ready'
      Wrapping = twEndEllipsis
      ImageIndex = 0
      Images = IlSmall
      CustomWidth = 150
      CustomHeight = 15
    end
    object SpTBXSeparatorItem3: TSpTBXSeparatorItem
    end
    object SbiTime: TSpTBXLabelItem
      ImageIndex = 1
      Images = IlSmall
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
      Images = IlSmall
      CustomHeight = 15
    end
    object SbLbInfo: TSpTBXLabelItem
      Wrapping = twEndEllipsis
      CustomHeight = 15
      MinWidth = 150
    end
    object SpTBXRightAlignSpacerItem1: TSpTBXRightAlignSpacerItem
      CustomWidth = 192
      CustomHeight = 15
    end
    object SpTBXSeparatorItem7: TSpTBXSeparatorItem
    end
    object SbiZoom: TSpTBXLabelItem
      ImageIndex = 3
      Images = IlSmall
      CustomHeight = 15
    end
    object SbLbZoom: TSpTBXLabelItem
      Caption = '100%'
      Wrapping = twEndEllipsis
      CustomWidth = 50
      CustomHeight = 15
    end
  end
  object PnMain: TSpTBXPanel
    Left = 0
    Top = 60
    Width = 984
    Height = 528
    Caption = 'PnMain'
    Align = alClient
    Padding.Left = 2
    Padding.Top = 2
    Padding.Right = 2
    TabOrder = 2
    Borders = False
    object PnLeftDk: TSpTBXPanel
      Left = 2
      Top = 2
      Width = 220
      Height = 526
      Align = alLeft
      TabOrder = 0
      object PsLeftDk: TSpTBXPageScroller
        Left = 2
        Top = 2
        Width = 216
        Height = 522
        Align = alClient
        Color = clCream
        ParentColor = False
        TabOrder = 0
        object PnFractalProperties: TSpTBXPanel
          Left = 0
          Top = 79
          Width = 216
          Height = 221
          Color = 8972543
          Align = alTop
          TabOrder = 1
          Borders = False
          object LbFpHeader: TSpTBXLabel
            Left = 0
            Top = 0
            Width = 216
            Height = 25
            Caption = 'Fractal Settings'
            Align = alTop
            AutoSize = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentColor = True
            ParentFont = False
            Alignment = taCenter
          end
          object GpFractalProperties: TGridPanel
            AlignWithMargins = True
            Left = 0
            Top = 25
            Width = 216
            Height = 196
            Margins.Left = 0
            Margins.Top = 0
            Margins.Right = 0
            Margins.Bottom = 0
            Align = alClient
            BevelOuter = bvNone
            Color = 8972543
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
                Control = LbCenterX
                Row = 0
              end
              item
                Column = 0
                Control = LbCenterY
                Row = 1
              end
              item
                Column = 1
                Control = EdCenterX
                Row = 0
              end
              item
                Column = 1
                Control = EdCenterY
                Row = 1
              end
              item
                Column = 0
                Control = LbZoom
                Row = 2
              end
              item
                Column = 1
                Control = EdZoom
                Row = 2
              end
              item
                Column = 0
                Control = LbMaxIters
                Row = 4
              end
              item
                Column = 0
                Control = LbPeriod
                Row = 3
              end
              item
                Column = 0
                Control = LbOrderM
                Row = 5
              end
              item
                Column = 0
                Control = LbOrderN
                Row = 6
              end
              item
                Column = 0
                Control = LbJitter
                Row = 7
              end
              item
                Column = 1
                Control = EdIterations
                Row = 4
              end
              item
                Column = 1
                Control = EdOrderM
                Row = 5
              end
              item
                Column = 1
                Control = EdOrderN
                Row = 6
              end
              item
                Column = 1
                Control = CbJitter
                Row = 7
              end
              item
                Column = 1
                Control = EdPeriod
                Row = 3
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
              end>
            ShowCaption = False
            TabOrder = 1
            object LbCenterX: TSpTBXLabel
              Left = 0
              Top = 2
              Width = 75
              Height = 21
              Caption = 'Center X:'
              Align = alClient
              ParentColor = True
              OnClick = EdLbClick
              Alignment = taRightJustify
              FocusControl = EdCenterX
            end
            object LbCenterY: TSpTBXLabel
              Left = 0
              Top = 26
              Width = 75
              Height = 21
              Caption = 'Center Y:'
              Align = alClient
              ParentColor = True
              OnClick = EdLbClick
              Alignment = taRightJustify
              FocusControl = EdCenterY
            end
            object EdCenterX: TSpTBXEdit
              Left = 80
              Top = 2
              Width = 126
              Height = 21
              Hint = 'X-coordinate value of center image'
              Align = alClient
              TabOrder = 1
              OnChange = EdCenterXChange
              OnKeyPress = EdFloatKeyPress
            end
            object EdCenterY: TSpTBXEdit
              Left = 80
              Top = 26
              Width = 126
              Height = 21
              Hint = 'Y-coordinate value of center image'
              Align = alClient
              TabOrder = 3
              OnChange = EdCenterYChange
              OnKeyPress = EdFloatKeyPress
            end
            object LbZoom: TSpTBXLabel
              Left = 0
              Top = 50
              Width = 75
              Height = 21
              Caption = 'Zoom:'
              Align = alClient
              ParentColor = True
              OnClick = EdLbClick
              Alignment = taRightJustify
              FocusControl = EdZoom
            end
            object EdZoom: TSpTBXEdit
              Left = 80
              Top = 50
              Width = 126
              Height = 21
              Hint = 'Zoom value'
              Align = alClient
              TabOrder = 5
              OnChange = EdZoomChange
              OnKeyPress = EdFloatKeyPress
            end
            object LbMaxIters: TSpTBXLabel
              Left = 0
              Top = 98
              Width = 75
              Height = 21
              Caption = 'Iterations:'
              Align = alClient
              ParentColor = True
              OnClick = EdLbClick
              Alignment = taRightJustify
              FocusControl = EdIterations
            end
            object LbPeriod: TSpTBXLabel
              Left = 0
              Top = 74
              Width = 75
              Height = 21
              Caption = 'Period:'
              Align = alClient
              ParentColor = True
              OnClick = EdLbClick
              Alignment = taRightJustify
              FocusControl = EdPeriod
            end
            object LbOrderM: TSpTBXLabel
              Left = 0
              Top = 122
              Width = 75
              Height = 21
              Caption = 'Order M:'
              Align = alClient
              ParentColor = True
              OnClick = EdLbClick
              Alignment = taRightJustify
              FocusControl = EdOrderM
            end
            object LbOrderN: TSpTBXLabel
              Left = 0
              Top = 146
              Width = 75
              Height = 21
              Caption = 'Order N:'
              Align = alClient
              ParentColor = True
              OnClick = EdLbClick
              Alignment = taRightJustify
              FocusControl = EdOrderN
            end
            object LbJitter: TSpTBXLabel
              Left = 0
              Top = 170
              Width = 75
              Height = 21
              Caption = 'Jitter:'
              Align = alClient
              ParentColor = True
              OnClick = EdLbClick
              Alignment = taRightJustify
              FocusControl = CbJitter
            end
            object EdIterations: TSpTBXSpinEdit
              Left = 80
              Top = 98
              Width = 126
              Height = 21
              Hint = 'Maximum number of iterations'
              Align = alClient
              Alignment = taLeftJustify
              NumbersOnly = True
              TabOrder = 9
              SpinButton.Left = 108
              SpinButton.Top = 0
              SpinButton.Width = 14
              SpinButton.Height = 17
              SpinButton.Align = alRight
              SpinOptions.MaxValue = 2147483647.000000000000000000
              OnValueChanged = EdMaxItersChange
            end
            object EdOrderM: TSpTBXSpinEdit
              Left = 80
              Top = 122
              Width = 126
              Height = 21
              Hint = 'Order M value'
              Align = alClient
              Alignment = taLeftJustify
              NumbersOnly = True
              TabOrder = 11
              AsInteger = 4
              SpinButton.Left = 108
              SpinButton.Top = 0
              SpinButton.Width = 14
              SpinButton.Height = 17
              SpinButton.Align = alRight
              SpinOptions.MaxValue = 127.000000000000000000
              SpinOptions.MinValue = 1.000000000000000000
              SpinOptions.Value = 4.000000000000000000
              OnValueChanged = EdOrderMChange
            end
            object EdOrderN: TSpTBXSpinEdit
              Left = 80
              Top = 146
              Width = 126
              Height = 21
              Hint = 'Order N value'
              Align = alClient
              Alignment = taLeftJustify
              NumbersOnly = True
              TabOrder = 13
              AsInteger = 4
              SpinButton.Left = 108
              SpinButton.Top = 0
              SpinButton.Width = 14
              SpinButton.Height = 17
              SpinButton.Align = alRight
              SpinOptions.MaxValue = 127.000000000000000000
              SpinOptions.Value = 4.000000000000000000
              OnValueChanged = EdOrderNChange
            end
            object CbJitter: TSpTBXComboBox
              Left = 80
              Top = 170
              Width = 126
              Height = 21
              Hint = 'Jitter type selector'
              Align = alClient
              Style = csDropDownList
              ItemHeight = 13
              TabOrder = 15
              OnChange = CbJitterChange
              Items.Strings = (
                'None'
                'Random'
                'BurtleHash'
                'Gaussian')
            end
            object EdPeriod: TSpTBXButtonEdit
              Left = 80
              Top = 74
              Width = 126
              Height = 21
              Hint = 'Atom domain period value (Use button to search this)'
              Align = alClient
              NumbersOnly = True
              TabOrder = 7
              OnChange = EdPeriodChange
              EditButton.Left = 108
              EditButton.Top = 0
              EditButton.Width = 14
              EditButton.Height = 17
              EditButton.Action = cmdToolFindPeriod
              EditButton.Align = alRight
              EditButton.ShowAccelChar = False
              EditButton.Wrapping = twEndEllipsis
              EditButton.BitmapTransparent = False
              EditButton.DropDownArrow = False
              EditButton.Images = IlSmall
              EditButton.ImageIndex = 4
            end
          end
        end
        object PnGeneralProperties: TSpTBXPanel
          Left = 0
          Top = 0
          Width = 216
          Height = 79
          Color = clMoneyGreen
          Align = alTop
          TabOrder = 0
          Borders = False
          object LbGpHeader: TSpTBXLabel
            Left = 0
            Top = 0
            Width = 216
            Height = 25
            Caption = 'Image Size'
            Align = alTop
            AutoSize = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentColor = True
            ParentFont = False
            Alignment = taCenter
          end
          object GpGeneralSettings: TGridPanel
            AlignWithMargins = True
            Left = 0
            Top = 25
            Width = 216
            Height = 54
            Margins.Left = 0
            Margins.Top = 0
            Margins.Right = 0
            Margins.Bottom = 0
            Align = alClient
            BevelOuter = bvNone
            Color = clMoneyGreen
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
                SizeStyle = ssAuto
              end
              item
                SizeStyle = ssAuto
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
              OnClick = EdLbClick
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
              OnClick = EdLbClick
              Alignment = taRightJustify
              FocusControl = EdImgHeight
            end
            object EdImgWidth: TSpTBXSpinEdit
              Left = 80
              Top = 2
              Width = 126
              Height = 21
              Hint = 'Image width value'
              Align = alClient
              Alignment = taLeftJustify
              TabOrder = 2
              AsInteger = 500
              SpinButton.Left = 108
              SpinButton.Top = 0
              SpinButton.Width = 14
              SpinButton.Height = 17
              SpinButton.Align = alRight
              SpinOptions.MaxValue = 65536.000000000000000000
              SpinOptions.MinValue = 10.000000000000000000
              SpinOptions.Postfix = ' px'
              SpinOptions.Value = 500.000000000000000000
              OnValueChanged = EdImgWidthChange
            end
            object EdImgHeight: TSpTBXSpinEdit
              Left = 80
              Top = 26
              Width = 126
              Height = 21
              Hint = 'Image height value'
              Align = alClient
              Alignment = taLeftJustify
              TabOrder = 3
              AsInteger = 500
              SpinButton.Left = 108
              SpinButton.Top = 0
              SpinButton.Width = 14
              SpinButton.Height = 17
              SpinButton.Align = alRight
              SpinOptions.MaxValue = 65536.000000000000000000
              SpinOptions.MinValue = 10.000000000000000000
              SpinOptions.Postfix = ' px'
              SpinOptions.Value = 500.000000000000000000
              OnValueChanged = EdImgWidthChange
            end
          end
        end
        object PnRenderingProperties: TSpTBXPanel
          Left = 0
          Top = 300
          Width = 216
          Height = 222
          Color = clSkyBlue
          Align = alTop
          TabOrder = 2
          Borders = False
          object LbRenderProperties: TSpTBXLabel
            Left = 0
            Top = 0
            Width = 216
            Height = 25
            Caption = 'Render Settings'
            Align = alTop
            AutoSize = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentColor = True
            ParentFont = False
            Alignment = taCenter
          end
          object GpRenderProperties: TGridPanel
            AlignWithMargins = True
            Left = 0
            Top = 25
            Width = 216
            Height = 197
            Margins.Left = 0
            Margins.Top = 0
            Margins.Right = 0
            Margins.Bottom = 0
            Align = alClient
            BevelOuter = bvNone
            Color = clSkyBlue
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
                Control = LbSupersample
                Row = 0
              end
              item
                Column = 0
                Control = LbOutColoring
                Row = 1
              end
              item
                Column = 1
                Control = EdSupersample
                Row = 0
              end
              item
                Column = 1
                Control = EdMapRange
                Row = 2
              end
              item
                Column = 0
                Control = LbMapRange
                Row = 2
              end
              item
                Column = 0
                Control = LbMapOffset
                Row = 3
              end
              item
                Column = 1
                Control = EdMapOffset
                Row = 3
              end
              item
                Column = 1
                Control = CbOutColoring
                Row = 1
              end
              item
                Column = 0
                Control = lbShowSlopes
                Row = 4
              end
              item
                Column = 1
                Control = CkShowSlopes
                Row = 4
              end
              item
                Column = 0
                Control = LbSlopePower
                Row = 5
              end
              item
                Column = 1
                Control = EdSlopePower
                Row = 5
              end
              item
                Column = 0
                Control = LbSlopeRatio
                Row = 6
              end
              item
                Column = 1
                Control = EdSlopeRatio
                Row = 6
              end
              item
                Column = 0
                Control = LbSlopeAngle
                Row = 7
              end
              item
                Column = 1
                Control = EdSlopeAngle
                Row = 7
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
              end>
            ShowCaption = False
            TabOrder = 1
            object LbSupersample: TSpTBXLabel
              Left = 0
              Top = 2
              Width = 75
              Height = 21
              Caption = 'Supersample:'
              Align = alClient
              ParentColor = True
              OnClick = EdLbClick
              Alignment = taRightJustify
              FocusControl = EdSupersample
            end
            object LbOutColoring: TSpTBXLabel
              Left = 0
              Top = 26
              Width = 75
              Height = 21
              Caption = 'Coloring:'
              Align = alClient
              ParentColor = True
              OnClick = EdLbClick
              Alignment = taRightJustify
              FocusControl = CbOutColoring
            end
            object EdSupersample: TSpTBXSpinEdit
              Left = 80
              Top = 2
              Width = 126
              Height = 21
              Hint = 'Image supersampling value of render'
              Align = alClient
              Alignment = taLeftJustify
              TabOrder = 1
              AsInteger = 1
              SpinButton.Left = 108
              SpinButton.Top = 0
              SpinButton.Width = 14
              SpinButton.Height = 17
              SpinButton.Align = alRight
              SpinOptions.MaxValue = 16.000000000000000000
              SpinOptions.MinValue = 1.000000000000000000
              SpinOptions.Postfix = 'x'
              SpinOptions.Value = 1.000000000000000000
              OnValueChanged = EdSupersampleChange
            end
            object EdMapRange: TSpTBXSpinEdit
              Left = 80
              Top = 50
              Width = 126
              Height = 21
              Hint = 'Color map range (Value in c.u.)'
              Align = alClient
              Alignment = taLeftJustify
              TabOrder = 5
              AsInteger = 200
              SpinButton.Left = 108
              SpinButton.Top = 0
              SpinButton.Width = 14
              SpinButton.Height = 17
              SpinButton.Align = alRight
              SpinOptions.MaxValue = 2147483647.000000000000000000
              SpinOptions.MinValue = 1.000000000000000000
              SpinOptions.Value = 200.000000000000000000
              SpinOptions.ValueType = spnFloat
              OnValueChanged = EdMapRangeChange
            end
            object LbMapRange: TSpTBXLabel
              Left = 0
              Top = 50
              Width = 75
              Height = 21
              Caption = 'Map Range:'
              Align = alClient
              ParentColor = True
              OnClick = EdLbClick
              Alignment = taRightJustify
              FocusControl = EdMapRange
            end
            object LbMapOffset: TSpTBXLabel
              Left = 0
              Top = 74
              Width = 75
              Height = 21
              Caption = 'Map Offset:'
              Align = alClient
              ParentColor = True
              OnClick = EdLbClick
              Alignment = taRightJustify
              FocusControl = EdMapOffset
            end
            object EdMapOffset: TSpTBXSpinEdit
              Left = 80
              Top = 74
              Width = 126
              Height = 21
              Hint = 'Color map offset (Value in c.u.)'
              Align = alClient
              Alignment = taLeftJustify
              TabOrder = 7
              SpinButton.Left = 108
              SpinButton.Top = 0
              SpinButton.Width = 14
              SpinButton.Height = 17
              SpinButton.Align = alRight
              SpinOptions.MaxValue = 2147483647.000000000000000000
              SpinOptions.MinValue = -2147483648.000000000000000000
              SpinOptions.ValueType = spnFloat
              OnValueChanged = EdMapOffsetChange
            end
            object CbOutColoring: TSpTBXComboBox
              Left = 80
              Top = 26
              Width = 126
              Height = 21
              Hint = 'Coloring method selector'
              Align = alClient
              Style = csDropDownList
              ItemHeight = 13
              TabOrder = 3
              OnChange = CbOutColoringChange
              Items.Strings = (
                'Iterations'
                'Potential'
                'ArcTan'
                'Square Root'
                'Cubic Root'
                'Logarithm'
                'Stretched'
                'Distance Linear'
                'Distance Sqrt'
                'Distance Log'
                'Distance Estimation Linear'
                'Distance Estimation Sqrt'
                'Distance Estimation Log')
            end
            object lbShowSlopes: TSpTBXLabel
              Left = 0
              Top = 98
              Width = 75
              Height = 21
              Caption = 'Show Slopes:'
              Align = alClient
              ParentColor = True
              OnClick = EdLbClick
              Alignment = taRightJustify
              FocusControl = CkShowSlopes
            end
            object CkShowSlopes: TSpTBXCheckBox
              Left = 80
              Top = 98
              Width = 126
              Height = 21
              Hint = 'Apply 3D-like shadows'
              Caption = 'True'
              Align = alClient
              TabOrder = 9
              OnClick = CkShowSlopesClick
            end
            object LbSlopePower: TSpTBXLabel
              Left = 0
              Top = 122
              Width = 75
              Height = 21
              Caption = 'Depth:'
              Align = alClient
              ParentColor = True
              OnClick = EdLbClick
              Alignment = taRightJustify
              FocusControl = EdSlopePower
            end
            object EdSlopePower: TSpTBXSpinEdit
              Left = 80
              Top = 122
              Width = 126
              Height = 21
              Hint = 'Slope shadow depth'
              Align = alClient
              Alignment = taLeftJustify
              TabOrder = 11
              SpinButton.Left = 108
              SpinButton.Top = 0
              SpinButton.Width = 14
              SpinButton.Height = 17
              SpinButton.Align = alRight
              SpinOptions.Decimal = 0
              SpinOptions.Increment = 5.000000000000000000
              SpinOptions.MaxValue = 100.000000000000000000
              SpinOptions.ValueType = spnFloat
              OnValueChanged = EdSlopePowerValueChanged
            end
            object LbSlopeRatio: TSpTBXLabel
              Left = 0
              Top = 146
              Width = 75
              Height = 21
              Caption = 'Strength:'
              Align = alClient
              ParentColor = True
              OnClick = EdLbClick
              Alignment = taRightJustify
              FocusControl = EdSlopeRatio
            end
            object EdSlopeRatio: TSpTBXSpinEdit
              Left = 80
              Top = 146
              Width = 126
              Height = 21
              Hint = 'Slope shadow strength'
              Align = alClient
              Alignment = taLeftJustify
              TabOrder = 13
              SpinButton.Left = 108
              SpinButton.Top = 0
              SpinButton.Width = 14
              SpinButton.Height = 17
              SpinButton.Align = alRight
              SpinOptions.Decimal = 0
              SpinOptions.Increment = 5.000000000000000000
              SpinOptions.MaxValue = 100.000000000000000000
              SpinOptions.Postfix = '%'
              SpinOptions.ValueType = spnFloat
              OnValueChanged = EdSlopeRatioValueChanged
            end
            object LbSlopeAngle: TSpTBXLabel
              Left = 0
              Top = 170
              Width = 75
              Height = 21
              Caption = 'Angle:'
              Align = alClient
              ParentColor = True
              OnClick = EdLbClick
              Alignment = taRightJustify
              FocusControl = EdSlopeAngle
            end
            object EdSlopeAngle: TSpTBXSpinEdit
              Left = 80
              Top = 170
              Width = 126
              Height = 21
              Hint = 'Slope shadow angle'
              Align = alClient
              Alignment = taLeftJustify
              TabOrder = 15
              SpinButton.Left = 108
              SpinButton.Top = 0
              SpinButton.Width = 14
              SpinButton.Height = 17
              SpinButton.Align = alRight
              SpinOptions.Decimal = 0
              SpinOptions.Increment = 5.000000000000000000
              SpinOptions.MaxValue = 360.000000000000000000
              SpinOptions.MinValue = -360.000000000000000000
              SpinOptions.Postfix = #176
              SpinOptions.ValueType = spnFloat
              OnValueChanged = EdSlopeAngleValueChanged
            end
          end
        end
      end
    end
    object PnImageView: TSpTBXPanel
      Left = 227
      Top = 2
      Width = 543
      Height = 526
      Align = alClient
      TabOrder = 1
      object ImgView: TImgView32
        Left = 2
        Top = 2
        Width = 539
        Height = 522
        Align = alClient
        Bitmap.ResamplerClassName = 'TNearestResampler'
        BitmapAlign = baCustom
        Scale = 1.000000000000000000
        ScaleMode = smScale
        ScrollBars.ShowHandleGrip = True
        ScrollBars.Style = rbsDefault
        ScrollBars.Size = 17
        SizeGrip = sgNone
        OverSize = 0
        TabOrder = 0
      end
    end
    object PnRightDk: TSpTBXPanel
      Left = 770
      Top = 2
      Width = 212
      Height = 526
      Caption = 'PnRightDk'
      Align = alRight
      TabOrder = 2
      object PnRightCntr: TSpTBXPanel
        Left = 2
        Top = 2
        Width = 208
        Height = 522
        Color = 14135039
        Align = alClient
        Padding.Left = 2
        Padding.Top = 2
        Padding.Right = 2
        Padding.Bottom = 2
        TabOrder = 0
        Borders = False
        object LblColorMaps: TSpTBXLabel
          Left = 2
          Top = 2
          Width = 204
          Height = 25
          Caption = 'Color Maps'
          Align = alTop
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentColor = True
          ParentFont = False
          Alignment = taCenter
        end
        object LbColorMaps: TSpTBXListBox
          Left = 2
          Top = 27
          Width = 204
          Height = 493
          Style = lbVirtualOwnerDraw
          Align = alClient
          ItemHeight = 40
          TabOrder = 1
          OnClick = LbColorMapsChange
          OnData = LbColorMapsData
          OnDataFind = LbColorMapsDataFind
          OnDrawItem = LbColorMapsDrawItem
        end
      end
    end
    object SpMain: TSpTBXSplitter
      Left = 222
      Top = 2
      Height = 526
      Cursor = crSizeWE
      MinSize = 150
      ResizeStyle = rsPattern
    end
  end
  object ActionList1: TActionList
    Images = IlMain16
    Left = 288
    Top = 88
    object cmdRender: TAction
      Category = 'View'
      Caption = 'Render'
      Hint = 'Render fractal'
      ImageIndex = 0
      ShortCut = 120
      OnExecute = cmdRenderExecute
      OnUpdate = cmdRenderUpdate
    end
    object cmdFileNew: TAction
      Category = 'File'
      Caption = 'New'
      Hint = 'New fractal'
      ImageIndex = 2
      ShortCut = 16462
      OnExecute = cmdFileNewExecute
      OnUpdate = cmdEnabledUpdate
    end
    object cmdViewRecoloring: TAction
      Category = 'View'
      Caption = 'Recoloring Mode'
      Hint = 'Recoloring mode'
      ImageIndex = 7
      OnExecute = cmdViewRecoloringExecute
      OnUpdate = cmdViewRecoloringUpdate
    end
    object cmdEditUndo: TAction
      Category = 'Edit'
      Caption = 'Undo'
      ImageIndex = 8
      ShortCut = 16474
      OnExecute = cmdEditUndoExecute
      OnUpdate = cmdEditUndoUpdate
    end
    object cmdEditRedo: TAction
      Category = 'Edit'
      Caption = 'Redo'
      ImageIndex = 9
      ShortCut = 16473
      OnExecute = cmdEditRedoExecute
      OnUpdate = cmdEditRedoUpdate
    end
    object cmdEditCut: TEditCut
      Category = 'Edit'
      Caption = 'Cut'
      Hint = 'Cut|Cuts the selection and puts it on the Clipboard'
      ImageIndex = 10
      ShortCut = 16472
    end
    object cmdEditCopy: TEditCopy
      Category = 'Edit'
      Caption = 'Copy'
      Hint = 'Copy|Copies the selection and puts it on the Clipboard'
      ImageIndex = 11
      ShortCut = 16451
    end
    object cmdEditPaste: TEditPaste
      Category = 'Edit'
      Caption = 'Paste'
      Hint = 'Paste|Inserts Clipboard contents'
      ImageIndex = 12
      ShortCut = 16470
    end
    object cmdEditSelectAll: TEditSelectAll
      Category = 'Edit'
      Caption = 'Select All'
      Hint = 'Select All|Selects the entire document'
      ShortCut = 16449
    end
    object cmdEditDelete: TEditDelete
      Category = 'Edit'
      Caption = 'Delete'
      Hint = 'Delete|Erases the selection'
      ImageIndex = 19
      ShortCut = 46
    end
    object cmdFileOpen: TAction
      Category = 'File'
      Caption = 'Open...'
      Hint = 'Open fractal'
      ImageIndex = 3
      ShortCut = 16463
      OnExecute = cmdFileOpenExecute
      OnUpdate = cmdEnabledUpdate
    end
    object cmdFileSaveAs: TAction
      Category = 'File'
      Caption = 'Save As...'
      Hint = 'Save Fractal as'
      ImageIndex = 5
      ShortCut = 32851
      OnExecute = cmdFileSaveAsExecute
      OnUpdate = cmdEnabledUpdate
    end
    object cmdFileSave: TAction
      Category = 'File'
      Caption = 'Save'
      Hint = 'Save Fractal'
      ImageIndex = 4
      ShortCut = 16467
      OnExecute = cmdFileSaveExecute
      OnUpdate = cmdFileSaveUpdate
    end
    object cmdFileExportImage: TAction
      Category = 'File'
      Caption = 'Export Image...'
      Hint = 'Export current fractal image'
      ImageIndex = 6
      ShortCut = 16461
      OnExecute = cmdFileExportImageExecute
      OnUpdate = cmdEnabledUpdate
    end
    object cmdFileRevert: TAction
      Category = 'File'
      Caption = 'Revert'
      Hint = 'Revert to saved version of file'
      ImageIndex = 15
      ShortCut = 123
      OnExecute = cmdFileRevertExecute
      OnUpdate = cmdFileSaveUpdate
    end
    object cmdFileExit: TFileExit
      Category = 'File'
      Caption = 'Exit'
      Hint = 'Exit|Quits the application'
      ImageIndex = 18
      ShortCut = 32883
    end
    object cmdView6: TAction
      Tag = 6
      Category = 'View'
      AutoCheck = True
      Caption = '6%'
      GroupIndex = 1
      OnExecute = cmdView100Execute
    end
    object cmdView12: TAction
      Tag = 12
      Category = 'View'
      AutoCheck = True
      Caption = '12%'
      GroupIndex = 1
      OnExecute = cmdView100Execute
    end
    object cmdView25: TAction
      Tag = 25
      Category = 'View'
      AutoCheck = True
      Caption = '25%'
      GroupIndex = 1
      OnExecute = cmdView100Execute
    end
    object cmdView50: TAction
      Tag = 50
      Category = 'View'
      AutoCheck = True
      Caption = '50%'
      GroupIndex = 1
      OnExecute = cmdView100Execute
    end
    object cmdView66: TAction
      Tag = 66
      Category = 'View'
      AutoCheck = True
      Caption = '66%'
      GroupIndex = 1
      OnExecute = cmdView100Execute
    end
    object cmdView100: TAction
      Tag = 100
      Category = 'View'
      AutoCheck = True
      Caption = '100%'
      Checked = True
      GroupIndex = 1
      OnExecute = cmdView100Execute
    end
    object cmdView150: TAction
      Tag = 150
      Category = 'View'
      AutoCheck = True
      Caption = '150%'
      GroupIndex = 1
      OnExecute = cmdView100Execute
    end
    object cmdView200: TAction
      Tag = 200
      Category = 'View'
      AutoCheck = True
      Caption = '200%'
      HelpContext = 1
      OnExecute = cmdView100Execute
    end
    object cmdView400: TAction
      Tag = 400
      Category = 'View'
      AutoCheck = True
      Caption = '400%'
      GroupIndex = 1
      OnExecute = cmdView100Execute
    end
    object cmdView800: TAction
      Tag = 800
      Category = 'View'
      AutoCheck = True
      Caption = '800%'
      GroupIndex = 1
      OnExecute = cmdView100Execute
    end
    object cmdView1600: TAction
      Tag = 1600
      Category = 'View'
      AutoCheck = True
      Caption = '1600%'
      GroupIndex = 1
      OnExecute = cmdView100Execute
    end
    object cmdFileImport: TAction
      Category = 'File'
      Caption = 'Import...'
      Hint = 'Import file'
      ImageIndex = 20
      ShortCut = 16457
      OnExecute = cmdFileImportExecute
      OnUpdate = cmdEnabledUpdate
    end
    object cmdView300: TAction
      Tag = 300
      Category = 'View'
      AutoCheck = True
      Caption = '300%'
      GroupIndex = 1
      OnExecute = cmdView100Execute
    end
    object cmdFileExport: TAction
      Category = 'File'
      Caption = 'Export...'
      Hint = 'Export file'
      ImageIndex = 21
      ShortCut = 16453
      OnExecute = cmdFileExportExecute
      OnUpdate = cmdEnabledUpdate
    end
    object cmdFileShowLocation: TAction
      Category = 'File'
      Caption = 'Show File Location...'
      Hint = 'Show image file location in explorer'
      ImageIndex = 22
      OnExecute = cmdFileShowLocationExecute
      OnUpdate = cmdFileShowLocationUpdate
    end
    object cmdToolFindPeriod: TAction
      Category = 'Tools'
      Hint = 'Find period'
      ImageIndex = 0
      OnExecute = cmdToolFindPeriodExecute
      OnUpdate = cmdEnabledUpdate
    end
    object cmdFilePrint: TAction
      Category = 'File'
      Caption = 'Print...'
      Hint = 'Print current fractal image'
      ImageIndex = 16
      ShortCut = 16464
      OnExecute = cmdFilePrintExecute
      OnUpdate = cmdEnabledUpdate
    end
    object cmdToolOptions: TAction
      Category = 'Tools'
      Caption = 'Options...'
      Hint = 'Show options dialog'
      ImageIndex = 14
      ShortCut = 121
      OnExecute = cmdToolOptionsExecute
      OnUpdate = cmdEnabledUpdate
    end
    object cmdViewCustomizerReset: TAction
      Category = 'View'
      Caption = 'Reset'
      Hint = 'Reset toolbars'
      OnExecute = cmdViewCustomizerResetExecute
    end
    object cmdToolsUpdateColorMaps: TAction
      Category = 'Tools'
      Caption = 'Refresh Color Maps'
      Hint = 'Refresh Color Maps List'
      ImageIndex = 30
      ShortCut = 116
      OnExecute = cmdToolsUpdateColorMapsExecute
      OnUpdate = cmdEnabledUpdate
    end
    object cmdHelpHomePage: TAction
      Category = 'Help'
      Caption = 'Home Page...'
      Hint = 'Project home page'
      ImageIndex = 32
      OnExecute = cmdHelpHomePageExecute
    end
    object cmdHelpAbout: TAction
      Category = 'Help'
      Caption = 'About...'
      Hint = 'About Nanobrot'
      ImageIndex = 31
      ShortCut = 112
      OnExecute = cmdHelpAboutExecute
    end
    object cmdToolSoundArtExport: TAction
      Category = 'Tools'
      Caption = 'Sound Art Export...'
      Hint = 'Generate image for Sound Art app'
      ImageIndex = 17
      ShortCut = 122
      OnExecute = cmdToolSoundArtExportExecute
      OnUpdate = cmdEnabledUpdate
    end
    object cmdViewSmallIcons: TAction
      Category = 'View'
      Caption = 'Small Icons'
      Hint = 'Small icons on toolbars'
      OnExecute = cmdViewSmallIconsExecute
      OnUpdate = cmdViewSmallIconsUpdate
    end
  end
  object IlMainN: TImageList
    ColorDepth = cd32Bit
    DrawingStyle = dsTransparent
    Height = 24
    Width = 24
    Left = 372
    Top = 88
    Bitmap = {
      494C010112006000280218001800FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
      00000000000036000000280000006000000078000000010020000000000000B4
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000060606291717174E2626
      2663272727641A1A1A520808082F000000010000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000060601660B0B02880B0B02880B0B02880B0B02880B0B02880B0B
      02880B0B02880B0B02880B0B02880B0B02880B0B02880B0B0288060601660000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000072B2B2B698A847BBECDBFA0F4C2AD7EFFA089
      54FFA08954FFBCA777FFD2C1A0F8948F83C73636367601010113000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000D0D0485FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FEFFFEFEFDFFFEFEFDFFFEFEFCFFFDFDFBFFFDFDFAFFFEFEFBFF0D0D04850000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000013131346A9A396D0C7B181FFA48645FF876114FF6B5214FFB7AF
      9CFFB9B19EFF725C24FF7B5606FFA28341FFC1A974FFBCB1A0DD1F1F1F5A0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000F0F0680FFFFFFFFFF8F3EFFFFAB6EFFFFCCA6FFFFD9BEFFFFE9
      D8FFC6C7A8FF658445FFAD7A47FFB88551FFB78450FFFCFCF8FF0F0F06800000
      0000000000000000000000000000000000000000000000000000000000000000
      00003030306FD3C094FF7D5D13FF856118FF6C4901FF604000FF8C7A4FFFFFFF
      FFFFFFFFFFFF9F926EFF5B3B00FF6B4901FF846018FF7B5A0FFFC3AD79FF4848
      4888000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000002020264030303860303
      03860303038601010181FFFFFFFFFF903FFFFFB27BFFFFD0ADFFFFD9BEFFFFE6
      D3FFA9B289FF5B7A3AFFA36F3CFFB07C49FFB07C49FFFCFCF7FF010101810303
      0386030303860303038602020264000000000000000000000000000000002B2B
      2B69C8B27CFF8D7949FF938663FF64470FFF9A8A63FFDDDBD6FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFE7E7E6FFA19470FF644812FF8D7D55FF908054FFB8A0
      64FF484848880000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000707077EC5BFBFFFC4B8
      B8FFC7BABAFF626262FFFFFFFFFFFF8F3CFFFFAB6EFFFFC9A2FFFFDCC3FFBFBB
      94FF597A37FF476825FF9B6734FFA97542FFA97542FFFBFBF6FF626262FFCCBB
      BBFFCCBBBBFFCCBBBBFF0707077E0000000000000000000000000B0B0B36D4C3
      99FF85703BFFFCFFFFFFFFFFFFFFCFCCC6FFFFFFFFFFF2F1EEFFB6AD98FFFFFF
      FFFFFFFFFFFFBCB39BFFE5E2DBFFFFFFFFFFD0CDC7FFFFFFFFFFFFFFFFFF8A78
      4BFFC2AD7AFF1A1A1A5300000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000A0A0A78C4C4C4FFB8B0
      B0FFC0B8B8FF565656FF565656FF565656FF565656FF565656FF565656FF5656
      56FF565656FF565656FF565656FF565656FF565656FF565656FF565656FFCCBB
      BBFFCCBBBBFFCCBBBBFF0A0A0A78000000000000000000000000A3A096CD9270
      29FF76622FFFE5E3DBFFFFFFFFFFFFFFFFFFC0B9A4FF705619FF877447FFE7E8
      E5FFEBEBE9FF948459FF6B4F12FFB2A88DFFFFFFFFFFFFFFFFFFEBE9E3FF8473
      46FF825F15FFCBC3B2E70000000D000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000A0A0A75C8C8C8FFBAB8
      B8FFB7B1B1FF6E6A6AFF978C8CFF958989FF958888FF958888FF958787FF9687
      87FF968787FF978787FF978787FF988787FF988787FF988787FF706767FFCCBB
      BBFFCCBBBBFFCCBBBBFF0A0A0A7500000000000000001818184FD2BD92FF8E6B
      27FF5F3F00FFBEB8A5FFFFFFFFFFFFFFFFFFE2E0D8FFA79A77FFA99567FF9A84
      54FF998254FFA59162FFA4966FFFDAD7CCFFFFFFFFFFFFFFFFFFD1CEC2FF6345
      00FF87631DFFC5AD79FF31313171000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000B0B0B73D9D9D9FFCCCC
      CCFFC9C8C8FF1B1B1BFF615555FF5C4E4EFF5C4E4EFF5C4E4EFF5C4E4EFF5C4E
      4EFF5C4E4EFF5C4E4EFF5C4E4EFF5C4E4EFF5C4E4EFF5C4E4EFF1B1B1BFFD9CC
      CCFFD9CCCCFFD9CCCCFF0B0B0B73000000000000000072706BACAD9052FF714E
      04FF918052FFFFFFFFFFBCB39BFFC4BEAAFFD0CAB9FF98814DFFA28651FFC2AF
      8CFFC7B593FFA18650FF9E8654FFBFB69CFFCDC9B9FFB3A98EFFFFFFFFFFA092
      6DFF6A4700FFA58644FF969186C7000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000B0B0B71D0D0D0FFB7B7
      B7FFABABABFF272727FF5E5757FF514747FF514747FF514747FF514747FF5147
      47FF514747FF514747FF514747FF514747FF514747FF514747FF272727FFA9A2
      A2FFB5ADADFFC5B8B8FF0B0B0B71000000000000000DBCB19EE28C6820FF4C2A
      00FFD3CFC1FFF7F8F6FF705516FF9D8D61FFA18D60FFA88D59FFFBFAFAFFFFFF
      FFFFFFFFFFFFFFFFFFFFB39C6EFF9D8553FFA59670FF6C5110FFE5E2DAFFEAEA
      E6FF593800FF7C570EFFD7C8A9F9050505250000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000B0B0B69CCCCCCF8C4C4
      C4FFC4C4C4FF353535FF646363FF4F4A4AFF474141FF463F3FFF463F3FFF463F
      3FFF463F3FFF463F3FFF463F3FFF463F3FFF463F3FFF474040FF353535FFC0B3
      B3FF65FF65FFB9ACACF80B0B0B69000000000808082FC7B590FA7B6533FF9081
      5FFFFEFFFFFFDCD8CDFF95865BFFB3A17BFF95763BFFF6F3F0FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFBFAF9FFA1854EFFA59164FF97885FFFC5BDA9FFFFFF
      FFFF9D916CFF745E36FFBFAB7CFF1515154B0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000080808569A9A9ADCCACA
      CAFFCACACAFF434343FF6D6D6DFF5B5B5BFF575656FF494747FF3E3B3BFF3C39
      39FF3C3939FF3C3939FF3C3939FF3C3939FF3C3939FF403D3DFF434343FFB7AE
      AEFFBAB0B0FF857E7EDC080808560000000010101040AE9865FFB4AB91FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF9D895BFFB9A37AFFFCFCFBFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFC5B391FF9A8454FFFCFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFC3BDACFFA18951FF2020205B0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000404043F616161BED3D3
      D3FFD2D2D2FF525252FF757575FF656565FF656565FF656565FF616161FF5656
      56FF484747FF3F3E3EFF393838FF353434FF353434FF434343FF525252FFBDB8
      B8FFC9C6C6FF595959BE0404043F000000000F0F0F3EB29D6BFFA69B7EFFFFFF
      FFFFFFFFFFFFF6F4F1FFEBECEAFFA18E61FFB59E73FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFC1AE8AFF9B8656FFE3E3DFFFF4F3EFFFFFFF
      FFFFFFFFFFFFB0A997FFA58E57FF1F1F1F5A0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000010101283939399FDDDD
      DDFFD8D8D8FF606060FF7B7B7BFF6E6E6EFF6E6E6EFF6E6E6EFF6E6E6EFF6E6E
      6EFF6E6E6EFF6E6E6EFF6E6E6EFF6E6E6EFF6E6E6EFF747474FF606060FFD8D8
      D8FFDADADAFF3737379F010101280000000006060629C7B896F6785E25FF7460
      32FFF9F9FAFFDAD6CBFF826D3AFFB5A580FF96783FFFEAE4DBFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFEFEBE4FFA0854FFFAB986CFF85713FFFC0B8A2FFFFFF
      FFFF857243FF6C5322FFC8B489FF131313460000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000141E1E1E7FE9E9
      E9FFE5E5E5FF6C6C6CFF838383FF7D7D7DFF7D7D7DFF7D7D7DFF7D7D7DFF7D7D
      7DFF7D7D7DFF7D7D7DFF7C7C7CFF7C7C7CFF7B7B7BFF7E7E7EFF6C6C6CFFE2E2
      E2FFE4E4E4FF1D1D1D7F000000140000000000000007B1A897DA98742DFF5934
      00FFC6BFACFFFFFFFFFF7C652CFF9B8C63FFA28E62FF9A7A3EFFEEE9E1FFFFFF
      FFFFFFFFFFFFF6F4F0FFA78C56FF98804DFFA79A77FF755C21FFF6F5F2FFDDDB
      D1FF5F3E00FF8D671CFFD0C3A8F30303031C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000020707074D0D0D
      0D670D0D0D670101018C747474FF747474FF747474FF747474FF747474FF7474
      74FF747474FF747474FF747474FF747474FF747474FF747474FF0101018C0D0D
      0D670D0D0D670707074D0000000200000000000000005F5E5B9CB1965BFF7451
      09FF86703CFFFAFAF9FFD2CCBEFFD6D3C7FFE3E0D7FFA28F63FF95773DFFB59E
      72FFB8A379FF94753AFFA39063FFD3CEBEFFDCDAD0FFCAC4B3FFFFFFFFFF9280
      52FF6D4A00FFA88B4BFF84807AB9000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000014140C6BFBFBF6FFF6F6F0FFF5F5EEFFF4F4EDFFF3F3EAFFEFEF
      E5FFEBEBDFFFE8E8D9FFE6E6D5FFE5E5D4FFE5E5D4FFF2F2E1FF14140C6B0000
      000000000000000000000000000000000000000000000D0D0D3BDCC9A3FF916E
      2AFF5D3D00FFBBB4A0FFFFFFFFFFFFFFFFFFD2CEC0FF97875BFFB6A67EFFA28F
      64FF9F8A60FFB4A37EFF98875AFFC9C3B2FFFFFFFFFFFFFFFFFFCECBBEFF6143
      00FF89651FFFCEB88BFF2020205B000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000014140D6AFAFAF4FFF5F5EEFFF4F4EDFFF3F3EAFFEFEFE5FFEBEB
      DFFFE8E8D9FFE6E6D5FFE5E5D4FFE5E5D4FFE5E5D4FFF2F2E1FF14140D6A0000
      00000000000000000000000000000000000000000000000000007C7B78B29978
      33FF7C6B3DFFF8F8F7FFFFFFFFFFFFFFFFFFCCC6B4FF7B632BFF847043FFF1F4
      F5FFF6F9FBFF918054FF745B22FFBEB59EFFFFFFFFFFFFFFFFFFFDFDFCFF8E80
      5AFF856319FFABA79DD100000001000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000014140D69FAFAF3FFF4F4EDFFF3F3EAFFEFEFE5FFEBEBDFFFE8E8
      D9FFE6E6D5FFE5E5D4FFE5E5D4FFE5E5D4FFE5E5D4FFF2F2E1FF14140D690000
      00000000000000000000000000000000000000000000000000000303031ECFC3
      A8F28B733AFFE3E3E1FFFFFFFFFFB0A89AFFFFFFFFFFFFFFFFFFD7D3CBFFFFFF
      FFFFFFFFFFFFD9D5C8FFFFFFFFFFFFFFFFFFB1A99AFFF7F7F7FFF4F6F9FF8872
      3DFFD8C7A0FF0B0B0B3600000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000014140D68F9F9F2FFF3F3EAFFEFEFE5FFEBEBDFFFE8E8D9FFE6E6
      D5FFE5E5D4FFE5E5D4FFE5E5D4FFA4A493FFA4A493FFA4A493FF1010087C0000
      0000000000000000000000000000000000000000000000000000000000001212
      1245DDCCA2FF8D763EFF796738FF5D3C00FF887141FFBBB4A0FFF3F3EFFFFFFF
      FFFFFFFFFFFFF7F7F7FFC3BCADFF8D784BFF5F3E02FF75612EFF88733FFFCFBA
      8AFF242424600000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000014140D68F9F9F1FFEFEFE5FFEBEBDFFFE8E8D9FFE6E6D5FFE5E5
      D4FFE5E5D4FFE5E5D4FFE5E5D4FFB6B6A5FFFFFFFFFF14140D68010101200000
      0000000000000000000000000000000000000000000000000000000000000000
      000013131346C3B79DEA9A7A36FF926F27FF75520AFF563200FF796633FFFFFF
      FFFFFFFFFFFF8E8056FF4D2900FF735008FF8F6D24FF96752EFFD1BF9BF82222
      225E000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000014140D67FBFBF1FFF6F6E9FFF4F4E5FFF3F3E2FFF2F2E1FFF2F2
      E1FFF2F2E1FFF2F2E1FFF2F2E1FFC2C2B1FF14140D6702020125000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000040404216E6E6AA8DBC9A3FFB2975CFF97742CFF795F21FFA69A
      7AFFA79B7CFF7B642BFF8F6B21FFAF9356FFD4C298FF84827CB8090909320000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000B0B074D14140D6614140D6614140D6614140D6614140D661414
      0D6614140D6614140D6614140D6614140D660202012400000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000F0F0F3E57565395A59E8ED3C0B090F1B4A0
      70FFB5A06FFFBFB08DF4ACA392D863605D9F1515154A00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000008050505270D0D
      0D3A0D0D0D3B0707072B0000000C000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B4B4B4FF949494FF878787FF8787
      87FF878787FF878787FF878787FF878787FF878787FF878787FF878787FF8787
      87FF878787FF878787FF878787FF0000000000000000000000021008007D1E10
      00940503004B0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000416
      2B930E2D50BB0003073C00000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000101011F383935A85151
      4CC01C1D1A790000000000000000000000000000000015121060000000000000
      00000000000000000000000000000000000002020220181412602A241F804139
      319F433A329F2C262180231F1B70020202200000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B6B6B6FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF878787FF00000000000000001409008D633200FF6F40
      00FF563300E90100002400000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000912671C53
      9DFF285DABFF0F3563D500000010000000000000000000000000000000000000
      00000000000000000000000000000000000000000002484843B5B4B6ACFFADAF
      A6FFA0A298FF18181669000000000000000000000000B19E8DFF161210600000
      000000000000000000000A0807405C4F45BFBAA898FFCABDAFFFD3C8BBFFDDD2
      C7FFDFD4C9FFD4C7B9FFCEBEB0FFC1AE9CFF7A6D5FCF1B181560000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B6B6B6FFFFFFFFFFFAFAFAFFFAFA
      FAFFFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFFAFA
      FAFFFAFAFAFFFFFFFFFF878787FF000000000B05006A884600FF965600FF965A
      00FF895100FF6E4100FF01000024000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000004000812671D589FFF3674
      C7FF154699FF133C83FF0B305FD60000000F0000000000000000000000000000
      0000000000000000000000000000000000003F403CA9D8DACCFF4B4C47AD1A1A
      1973999A91F1AAACA2FF060606380000000000000000B19E8DFFB19E8DFF1613
      106000000010584A3FBFC0B1A2FFE0D8CFFFE3DBD1FFDED5CAFFDCD2C5FFDACF
      C2FFDACEC1FFDBCFC2FFDDD1C5FFE0D5CAFFDBCFC1FFC7B5A3FF6B5E53BF0302
      0220000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B6B6B6FFFFFFFFFFFAFAFAFFDBCD
      BFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCD
      BFFFFAFAFAFFFFFFFFFF878787FF000000003C2100ADAA6500FFBD6F00FFB069
      00FF955800FF854E00FF6D4100FF010000210000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000040A4D0E4C92FF3D83D0FF366B
      B2FF2B65B2FF134594FF0C337BFF062B5AD40000000E00000000000000000000
      000000000000000000000000000000000014BDBFB3FF909288E2000000000000
      00000707073FB4B5ABFF30302D910000000000000000B19E8DFFDAD1C6FFB29F
      8EFF7D6C5EDFD8CEC3FFE4DDD4FFDDD4C9FFDBD2C6FFDFD6CBFFE1D9CEFFE1D9
      CFFFE1D8CEFFE1D7CDFFDDD2C6FFD9CCBEFFD9CBBDFFDED4C7FFD5C8B9FFAA99
      88EF060605300000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B6B6B6FFFFFFFFFFFBFBFBFFFBFB
      FBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFB
      FBFFFBFBFBFFFFFFFFFF878787FF0000000007040056A96A00FFC97C00FFBD6F
      00FFAC6700FF965900FF854F00FF693F00FB0100002400000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000102260B2F5DCC3181CAFF184E
      9AFF08307AFF2661B0FF164998FF093078FF072A59D30000000C000000000000
      000000000000000000000000000015151367C4C6BAFF7B7C74DA000000000000
      00000A0A094ABBBDB2FF2C2D298B0000000000000000B19E8DFFDBD3C9FFDAD1
      C6FFDED6CDFFE1DAD0FFDCD3C7FFE2DAD0FFE5DFD6FFCEC2B5FFC0B0A1FFBBA9
      98FFBCAB99FFC4B4A4FFD2C4B7FFE1D7CCFFDFD4C8FFD8CABCFFDCD1C3FFD7C9
      BAFFAB9A8AEF060605300000000000000000467DA4FF467DA4FF457095FF456F
      94FF456F94FF456F94FF446F94FF446F94FFB6B6B6FFFFFFFFFFFCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFC
      FCFFFCFCFCFFFFFFFFFF878787FF00000000000000000D08006EB87600FFC87B
      00FFBC6D00FFAD6700FF975900FF854F00FF6D4100FF01010025000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000A0D3360CD3485
      CFFF2058A3FF083079FF2664B2FF194D9DFF082F77FF052956D20000000C0000
      0000000000000000000000000000696A64D4C2C5BAFFB6B8AEFF464742B31D1D
      1B78B9BBB0FFBBBDB1FF0404042F0000000000000000B19E8DFFD6CDC3FFD1C6
      BAFFDAD1C7FFD9D0C4FFE4DED5FFD1C6BAFF867769DF201C1870020202200000
      00000000000002020220191613608D7D6EDFD5C9BBFFE3D9CFFFD9CCBEFFDBCF
      C1FFD5C7B8FF7E7163CF00000010000000003084B3FF96CAE3FF95CAE3FF95CA
      E3FF94C9E3FF94C9E3FF93C8E3FF92C8E2FFB6B6B6FFFFFFFFFFFDFDFDFFDBCD
      BFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCD
      BFFFFDFDFDFFFFFFFFFF878787FF00000000000000000000000010090070BC7C
      00FFC97D00FFBA6C00FFAE6800FF965800FF854E00FF6E4100FF010100250504
      03295048409B8C837ACAA49A92DA9A9187D36E655BB41C19155F000000000000
      0000000000000000000000000000000000000000000000000000000000070E33
      60CD388BD3FF245CA6FF083079FF2967B5FF1A4F9EFF092F77FF052854D10000
      000B00000000000000003F3F3BAABABDB2FFB7B9AFFFC2C4BAFFCECFC5FFC3C5
      B9FFAEB0A6FA0E0E0D54000000000000000000000000B19E8DFFD3C9BFFFCABE
      B1FFCEC3B6FFDAD1C7FFCDC0B3FF5B4F43BF0000001000000000000000000000
      0000000000000000000000000000000000002D282380C7B7A6FFE3D9CFFFD9CC
      BEFFDDD1C4FFCBBAA9FF2F2A2580000000003084B3FF98CBE3FF5FAED3FF4BA5
      CEFF4AA4CEFF49A4CEFF48A3CEFF46A2CEFFB6B6B6FFFFFFFFFFFEFEFEFFFEFE
      FEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFE
      FEFFFEFEFEFFFFFFFFFF878787FF000000000000000000000000000000000F09
      0070C68700FFCB8100FFB86A00FFAD6800FF955800FF844E00FF8F6127FFB69E
      82F3E0D6CCFFDCDADBFFD7D7DBFFD8D8DAFFE0DAD6FFCEBBA5FB594835AE0201
      011F000000000000000000000000000000000000000000000000000000000000
      00070E3361CD3F91D9FF2962ABFF062F78FF2B6BB9FF1D52A1FF082E76FF001D
      4ECB000001192D2B2487B3B5ABFFBEC0B6FFC8CABEFFC1C3B7FF555651BA1919
      176B0000001200000000000000000000000000000000B19E8DFFCFC6BBFFC6B9
      ACFFCABEB1FFD1C6BAFFD9D0C5FFB5A291FF1613116000000000000000000000
      00000000000000000000000000000000000000000000221E1A70CDBEAEFFE0D6
      CBFFD8CABCFFD9CDBFFFA89786EF000000103084B3FF9ACCE3FF50A7CFFF4FA7
      CFFF4EA6CFFF4DA5CEFF4BA5CEFF4AA4CEFFB6B6B6FFFFFFFFFFFEFEFEFFFEFE
      FEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFE
      FEFFFEFEFEFFFFFFFFFF878787FF000000000000000000000000000000000000
      00000F090070CE8E00FFD08500FFB66800FFAF6B07FFBB9A71FFEBECEFFFC4C3
      C9FFA19683FFA59572FFAD9B72FFA99972FFA19376FFAEA8A5FFE0E3EDFFD6C3
      AEFF1C140B650000000000000000000000000000000000000000000000000000
      0000000000070E3361CD4394DBFF2F67AFFF052E77FF2D6EBAFF1C52A0FF0027
      79FF1C4270EF95978BFFC2BFACFFD4D5C8FFABADA4F70A0A0A4A000000000000
      00000000000000000000000000000000000000000000B19E8DFFD0C7BCFFCFC6
      BBFFD3C9BFFFD6CDC3FFDBD3C9FFDAD1C6FFB5A392FF17141160000000000000
      000000000000000000000000000000000000000000000000000065584DBFDED3
      C8FFDCD0C3FFDCD0C3FFCABAAAFF191613603084B3FF9CCDE3FF53A9CFFF53A8
      CFFF52A8CFFF50A7CFFF4FA7CFFF4DA6CEFFB6B6B6FFFFFFFFFFFFFFFFFFDBCD
      BFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCD
      BFFFFFFFFFFFFFFFFFFF878787FF000000000000000000000000000000000000
      0000000000000F080070D69700FFD28A05FFBFA586FFEEF1FCFF9D9589FF9D84
      44FFBCA253FFD8C481FFE3D49FFFDECC8FFFC9B062FFAA8F43FF92805CFFC5C6
      CFFFEAE2DAFF231A0F7200000000000000000000000000000000000000000000
      000000000000000000070E3060CD4A9ADFFF376FB6FF002671FF1D5DADFF2464
      B9FF06439AFF697783FFACA898FF77766ACB0404032C00000000000000000000
      00000000000000000000000000000000000000000000B19E8DFFB19E8DFFB19E
      8DFFB19E8DFFB19E8DFFB19E8DFFB19E8DFFB19E8DFFB19E8DFF171411600000
      00000000000000000000000000000000000000000000000000000B090840C8B8
      A9FFE0D6CBFFDACDC0FFD2C4B6FF453D369F3084B3FF9ECEE3FF57AACFFF56AA
      CFFF55A9CFFF54A9CFFF52A8CFFF51A7CFFFB6B6B6FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF878787FF000000000000000000000000000000000000
      000000000000000000000F090070B59160FFF8FCFFFF8C806AFFA3811BFFE3C9
      66FFEEDF9AFFF0E7B6FFF5EFCEFFF2E9BFFFEDE0A2FFECD780FFC4A334FF7E65
      29FFC1C2CCFFE4D9CDFF06040232000000000000000000000000000000000000
      0000000000000000000000000007082D61CD4291DBFF4C85C9FF609EDAFF60AA
      F1FF0D3984FF183265FF143F77FF0E18248C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008C7D
      70DFE3DAD1FFD8CBBDFFD9CDC1FF665A4FBF3084B3FFA0CFE4FF5AABD0FF59AB
      CFFF58ABCFFF57AACFFF55A9CFFF54A9CFFFB6B6B6FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF878787FF000000000000000000000000000000000000
      000000000000000000001A130B63F3F6FBFF9E9790FF957004FFE5CA58FFE3CF
      78FFE7D78EFFEADDA2FFEDE1ACFFEBDFA6FFE7D894FFE3D17DFFE4CE68FFC6A1
      20FF7A6431FFDBDEEBFFA18769E5000000000000000000000000000000000000
      000000000000000000000000000000000000052047B73587D7FF81CBFFFF4071
      B0FF00114BFF002669FF1055A9FF003F97FF0000001400000000000000000000
      00000000000000000000000000000000000051A7D9FF51A7D9FF51A7D9FF51A7
      D9FF51A7D9FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006155
      4ABFE4DCD2FFDED4C8FFD7CBBEFFBCAA99FF3084B3FFA2CFE4FF5CADD0FF5CAC
      D0FF5BACD0FF5AABD0FF58ABCFFF56AACFFFB6B6B6FFFFFFFFFFFFFFFFFFDBCD
      BFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCD
      BFFFFFFFFFFFFEFEFEFF8A8A8AFF000000000000000000000000000000000000
      0000000000000503022C8B745AE5E4E7F6FF7B6014FFD8B329FFDBC356FFDEC8
      68FFE1CE7AFFE4D389FFE6D68FFFE5D48CFFE2D07EFFDFC96DFFDCC45AFFE1C4
      46FF946F00FF928A81FFF2EAE1FF231F1B680000000000000000000000000000
      000000000000000000000000000000000000343637A36481A2FF426898FF0820
      5EFF0E3D81FF114FA9FF1850A1FF050B135D0000000000000000000000000000
      0000000000000000000000000000000000005CA1C6EFADE6FEFF93DEFCFF86D6
      FBFF265A77BF0000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000BBAB
      9BFFBBAB9BFFBBAB9BFFBBAB9BFFBBAB9BFF3084B3FFA3D0E4FF5FAED0FF5EAD
      D0FF5DADD0FF5CADD0FF5BACD0FF59ABCFFFB6B6B6FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFD
      FDFFF0F0F0FFE6E6E6FFA5A5A5FF000000000000000000000000000000000000
      000000000000271C1078CBBDB0FFAEA8A3FF8D6800FFDDBD32FFD8BC46FFD9C1
      56FFDCC666FFDFCA70FFE0CC74FFE0CB72FFDEC769FFDBC25BFFD7BD4BFFD5B9
      39FFC09803FF836E3BFFE5E9F7FF857A71C60000000000000000000000000000
      00000000000000000000000000014B4A43B4CDCCB9FFF5E8C7FF848E98FF154B
      95FF84C9FEFF3979CAFF4D5E73FF33312AA60000000000000000000000000000
      0000000000000000000000000000000000002B5B79BFB2E7FEFF75D5F5FF8BD9
      FBFF4694BFEF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003084B3FFA4D1E4FF61AFD0FF60AF
      D0FF5FAED0FF5EAED0FF5DADD0FF5BACD0FFB6B6B6FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE0E0E0FFB6B6
      B6FFCBCBCBFFBEBEBEFF787878CF000000000000000000000000000000000000
      0000000000003F2D1A98ECE5DCFF91856FFF9C7300FFD6B529FFD4B73BFFD6BB
      48FFD9C054FFDBC35CFFDBC45FFFDBC35DFFD9C157FFD7BD4CFFD4B83EFFD3B5
      30FFD1A809FF7E6316FFCED1E3FFAFA59ADC0000000000000000000000000D10
      0D561A1D146921211B745F5F5ECBC6C8BBFFC8CABDFFECE8CEFF7A8999FA1450
      A6FF468DDDFF232D3BA4AEA384FF979483FF43413AC100000000000000000000
      0000000000000000000000000000000000001833438FA3DCF7FF87DAF8FF89DB
      FBFF63B8E5FF060F145000000000000000000000000000000000000000000000
      00000000000008161D603A9CCFFF3A9CCFFF3A9CCFFF3A9CCFFF3A9CCFFF3A9C
      CFFF3A9CCFFF3A9CCFFF3A9CCFFF000000003084B3FFA6D1E4FF63B0D0FF62AF
      D0FF61AFD0FF60AED0FF5FAED0FF5DADD0FFB6B6B6FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD5D5D5FFF4F4
      F4FFFFFFFFFF787878CF00000010000000000000000000000000000000000000
      0000000000003D2C1996EEE6DBFF908571FF996E00FFD6B62AFFD3B638FFD5B9
      42FFD7BC4BFFD8BF51FFD9BF54FFD8BF52FFD7BD4DFFD5BA45FFD3B63BFFD3B5
      33FFCEA501FF795D0FFFD1D5E9FFACA297DB00000000000000074D4F48B5CCC5
      C0FFBCB8BBFFBBBDB8FFCED1C2FFC6C8BBFFD4D6C9FFC3C3B1FC0404042E0207
      0E540005125B0000000017171266AFB0A4FF9C9C90FF3C3C38C0000000000000
      000000000000000000000000000000000000070F155084C7EBFF9BE0FAFF7CD8
      F7FF7CCEF6FF377DA3DF00000010000000000000000000000000000000000000
      0000000000000000000008161D603BB0E2FF38CFFFFF37CDFEFF37CBFDFF37C9
      FCFF37C7FCFF37C5FDFF3A9CCFFF000000003084B3FFA6D2E4FF65B1D1FF64B0
      D0FF63B0D0FF62AFD0FF60AED0FF5FAED0FFB6B6B6FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9F9F9FFCFCFCFFFFFFF
      FFFF787878CF0000001000000000000000000000000000000000000000000000
      00000000000023190E72D0C0B1FFB4B1B4FF7E5700FFDFBE31FFD7BB42FFD6BA
      46FFD8BD4DFFD9BF51FFD8BF53FFD8BF52FFD8BE4EFFD7BB49FFD5B93FFFD9BD
      44FFB88D00FF77622FFFF2F6FFFF7F766BC10000000E3D3E3AA7C8CABDFFE3E5
      D6FFD7D9CBFFC6C8BBFFC6C8BBFFCED0C3FFC7C9BBFE0808073F000000000000
      00000000000000000000000000001515146BBDBEB0FFAAAA9DFF373833BD0000
      000000000000000000000000000000000000000000004785A8DFB7E9FEFF79D6
      F5FF82D8FAFF63BBEAFF1B3E529F000000000000000000000000000000000000
      000000000000000000000000000009161D603CB0E3FF3FCDFFFF43CDF8FF45CC
      F6FF46CAF4FF4AC9FBFF3A9CCFFF000000003084B3FFA7D2E4FF66B1D1FF65B1
      D1FF64B0D1FF63B0D0FF62AFD0FF60AED0FFB6B6B6FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEAEAEAFFCACACAFF7878
      78CF000000100000000000000000000000000000000000000000000000000000
      00000000000003020122766048DBFFFFFFFF634500FFD3AD1AFFE2CE6FFFD9C0
      55FFDBC35DFFDCC460FFDCC461FFDCC461FFDBC35EFFDAC158FFDCC55FFFE9CD
      54FF7D5400FF9B9797FFEFE5D9FF181512581717166BB6B8ACFFEBEDDEFFCDCF
      C1FFCCCEC0FFD3D5C7FFC4C6B9FFD1D3C6FF454540A700000000000000000000
      00000000000000000000000000000000000014151368C1C2B4FFCCCCBEFF5C5D
      56C305050537000000000000000000000000000000000B171E6085C7EBFFA1E2
      FCFF6CD3F5FF79D3FAFF58B4E6FF204B63AF0000001000000000000000000000
      0000000000000000000000000000010203203279A1DF52BCEDFF67D5FCFF6FD6
      F7FF75D6F5FF75D4FCFF3A9CCFFF000000003084B3FFA8D2E4FF67B2D1FF67B1
      D1FF66B1D1FF64B0D0FF5FABCCFF5CA8CAFFB6B6B6FFB6B6B6FFB6B6B6FFB6B6
      B6FFB6B6B6FFB6B6B6FFB6B6B6FFB6B6B6FFB1B1B1FF9E9E9EFFA0A0A0EF0606
      0630000000000000000000000000000000000000000000000000000000000000
      00000000000000000000110C0750FBF9F6FFB1ADA9FF714800FFF1D86FFFF0E3
      A7FFE2CF7EFFE3D080FFE3D081FFE3D081FFE2CF7CFFE7D891FFFBECA7FFB38A
      04FF665223FFFFFFFFFF8A7256DA0000000053534FB3EBEDDDFFC8C9BDFB0303
      032C09090946B2B4A8F2CFD1C4FFC7C9BDFF1F1F1E7800000000000000000000
      0000000000000000000000000000000000000000000014141368EEF0E0FFFFFF
      F9FFC7C9BCFF31312E96000000000000000000000000000000002B5B79BFA4DC
      F7FF92DDFAFF65D0F4FF74D3FAFF5ABAEDFF3F93C0EF1A3E529F040A0D400102
      032001020320060F14501A3D529F3B92BFEF4DBAEEFF5FD1FCFF68D4F6FF70D5
      FAFF75D5F6FF75D4FBFF3A9CCFFF000000003084B3FFA9D2E4FF68B2D1FF67B2
      D1FF67B1D1FF74ADC6FF669DB6FF6197B0FF5C91ABFF588DA7FF3B81A5FF59AB
      CFFF56AACFFF9CCDE3FF2B5279FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000684F35D5FFFFFFFF978C7EFF7D5700FFDFC6
      74FFFFFFDDFFFFFEE8FFFDF9E1FFFEFBE5FFFFFFE9FFF9E9B0FFA88316FF6A51
      1EFFF2F5FFFFDBCAB7FF0302012500000000A1A299F0C6C8BAFF0707063C0000
      0000000000004D4E49BBCCCEC0FFD5D7C9FF3334309400000000000000000000
      00000000000000000000000000000000000000000000000000004C4E49B6F6F8
      E7FFFFFFF0FFD9DBCCFF1E1E1C7900000000000000000000000000000010386F
      90CF9FDBF7FF8DDCFAFF61CFF4FF67D1F9FF63C9F7FF52BAEEFF46ABE2FF42A7
      DFFF41A8DFFF41ABE2FF46B9EEFF51C7F9FF5BD0F9FF62D1F6FF6AD2FBFF6CCE
      F9FF77D5FDFF7AD6FDFF3A9CCFFF000000003084B3FFA9D3E4FF85C0D9FF78BA
      D6FF77BAD6FF959595FF959595FF959595FF959595FF959595FF4586A8FF6BB4
      D5FF7ABCD9FF9CCDE3FF2B5279FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000005937C60EBFFFFFFFFC4C0C1FF8D70
      31FFA78B41FFC4AD69FFCEBA7AFFCBB573FFB69C56FF937428FF9C8C71FFFFFF
      FFFFE0D3C6FF150F095900000000000000001414135E0A0A0949000000000000
      00001717166BC2C3B7FFEEF0E0FFD3D5C6FF0D0D0C4E00000000000000000000
      00000000000000000000000000000000000000000000000000000101011FBEBF
      B2FFF4F6E5FFEEF0DFFFBEBFB2FF090908440000000000000000000000000000
      0010386F90CF96D6F4FF99DFFDFF70D4F7FF56CCF2FF5ACDF6FF59CDF9FF57CD
      FAFF54CCFAFF52CCF9FF54CDF7FF59CEF4FF62D0F8FF6AD2FDFF64C4F1FF397F
      A5DF5DB6E5FF86DAFEFF3A9CCFFF000000003084B3FFA9D3E4FFA9D3E4FFA8D2
      E4FFA8D2E4FF585858FF7C7C7CFF7C7C7CFF7C7C7CFF585858FF6599B3FFA0CF
      E4FF9ECEE3FF9CCDE3FF2C5A84FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000045F4C35C5EEE8E2FFFFFF
      FFFFE1DBD2FFC4B388FFC1AD78FFC0AE7CFFD2C5ACFFF9FBFFFFFFFFFFFFAB96
      7EF7100B074E0000000000000000000000000000000000000000000000002C2C
      2989D0D1C4FFECEEDEFF9FA095E6151514640000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000F0F
      0E58A7A89EF4E9EBE0FFB9BAAFFF080807400000000000000000000000000000
      0000000000101833438F70BDE6FF96DAF9FF8EDCFDFF74D4F9FF65D0F6FF5DCE
      F5FF5CCEF5FF60CFF7FF67D1FAFF6FD3FDFF6ECEF9FF56B2E2FF1632438F0000
      00100A161E6065B9E6FF3A9CCFFF00000000467DA4FF3084B3FF3084B3FF3084
      B3FF3084B3FF7C7C7CFF9F9F9FFF9F9F9FFF9F9F9FFF7C7C7CFF1E608BFF3084
      B3FF3084B3FF3083B2FF2E709DFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000100B064D6955
      40C9C8B8A7FDF1E8DEFFF7F2EDFFF5EEE7FFDBD0C6FF9D8972E8302417890000
      0009000000000000000000000000000000000000000000000000000000003636
      329874756EC8373733920707063C000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000F0F0E5861625EBB191A186B000000000000000000000000000000000000
      00000000000000000000010203201D3F539F529AC4EF71C1EBFF7ACBF2FF79CD
      F4FF75CCF4FF6FC8F2FF64BDEAFF4998C3EF1D3F539F01020320000000000000
      0000000000000A171E603A9CCFFF000000000000000000000000000000000000
      000000000000808080FF808080FF808080FF808080FF808080FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000E130E08553223148744311C9E3D2C199621180D6E0504022E000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000040A0D400E1F29701329
      3680132936800E1F2970040A0D40000000000000000000000000000000000000
      000000000000000000000B171E60000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000030100200000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000301002000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000001E110960CB6D38FFA44E27FF2611098000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000AEAEAEFFA6A6A6FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5A5FFA5A5
      A5FFA5A5A5FF878787FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000361D0D800D07
      0340000000000000000000000000000000000000000000000000000000000000
      00000B0602402F180B8000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A86233DF8F512ACF512C169FA14B25FF0F060350000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B6B6B6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF878787FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000010C068
      2EEF030100200000000000000000000000000000000000000000000000000201
      0020A55226EF0000001000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000E08746FF03020120000000004422129F472010AF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B6B6B6FFFFFFFFFFF8F8F8FFF8F8F8FFF8F8F8FFF8F8
      F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8
      F8FFFFFFFFFF878787FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006838
      19AF7C431DBF0000000000000000000000000000000000000000000000006834
      18BF582C14AF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000E48D4AFF00000000000000000B060340863D1EEF000000000000
      000000000000000000000000000000000000000000000000000000000000AEAE
      AEFF8C8C8CFF878787FFB6B6B6FFFFFFFFFFF9F9F9FFF9F9F9FFF9F9F9FFF9F9
      F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9
      F9FFFFFFFFFF878787FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000001F10
      0760DC7634FF1E10076000000000000000000000000000000000190D0660B95B
      2AFF1A0D06600000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000004C22
      109F6F3118BF31160B8000000000000000000000000000000000000000000000
      000000000000E8944EFF160E0750000000001A0E076075351BDF000000000000
      000000000000000000000000000000000000000000000000000000000000B6B6
      B6FFFFFFFFFFFFFFFFFFB6B6B6FFFFFFFFFFFBFBFBFFDBCDBFFFDBCDBFFFDBCD
      BFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCDBFFFFBFB
      FBFFFFFFFFFF878787FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0010DB7636FFA75928DF000000000000000000000000000000008D4521DFBC5D
      2CFF000000100000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000001C0D0660C95F
      2FFF9A4924DFAF5329EF833D1DCF000000100000000000000000000000000000
      000000000000EC9A52FFCB8245EF39241380A4582DEF3B1B0D9F000000000000
      000000000000000000000000000000000000000000000000000000000000B6B6
      B6FFFFFFFFFFFAFAFAFFB6B6B6FFFFFFFFFFFCFCFCFFFCFCFCFFFCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFC
      FCFFFFFFFFFF878787FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008E4C21CFDD773BFF1E1007600000000000000000190C0660C26030FF793C
      1CCF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000341A0D809F51
      29DF0000000000000010874321CF864021CF0000001000000000000000000000
      000000000000F0A056FFE6944EFFCA743DFF79391CDF02010020000000000000
      000000000000000000000000000000000000000000000000000000000000B6B6
      B6FFFFFFFFFFF9F9F9FFB6B6B6FFFFFFFFFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
      FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
      FDFFFFFFFFFF878787FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000532C139FE07C41FF8B4A21CF0000000000000000773A1BCFCA6534FF4723
      109F000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000029170B70A65E
      30DF000000000000000003010020D06C36FF874321CF00000010000000000000
      000003020120E2954FFF8C4221EF0F0603500000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B6B6
      B6FFFFFFFFFFFBFBFBFFB6B6B6FFFFFFFFFFFDFDFDFFDBCDBFFFDBCDBFFFDBCD
      BFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCDBFFFFDFD
      FDFFFFFFFFFF878787FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000351C0D80E48345FFD77237FF0301002002010020BC5E30FFCF6736FF2E16
      0B80000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000010C87C
      41EF2C1C0E700000000000000010D8783EFFD26E38FF874321CF000000100000
      000023190D60D98C4AFF552613BF000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B6B6
      B6FFFFFFFFFFFCFCFCFFB6B6B6FFFFFFFFFFFEFEFEFFFEFEFEFFFEFEFEFFFEFE
      FEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFE
      FEFFFFFFFFFF878787FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000341C0C80E17D41FFDC793DFF1D0F0760190C0560C7673AFFCC6939FF2D16
      0B80000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000002116
      0C60F1A357FF9D6938CFB1713CDFE08746FFD8783EFFD06C36FFB1552AEF3E1F
      118FA87D4EDFB07248FF1D0D0670000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B6B6
      B6FFFFFFFFFFFDFDFDFFB6B6B6FFFFFFFFFFFEFEFEFFFEFEFEFFFEFEFEFFFEFE
      FEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFE
      FEFFFFFFFFFF878787FF0000000000000000B85E33FFB85E33FFB85E33FFB85E
      33FFB85E33FFB85E33FFB85E33FFB85E33FFB85E33FFB85E33FFB85E33FFB85E
      33FF000000000000000000000000000000000000000000000000000000000000
      0000341B0C80DD793EFFE17E42FF40220F8F371A0C8FD07244FFCB6C3EFF2D16
      0A80000000000000000000000000000000000000000000000000000000000000
      0000CE6B33FFCE6B33FFCE6B33FFCE6B33FFCE6B33FFCE6B33FFCE6B33FFCE6B
      33FFCE6B33FFCE6B33FFCE6B33FFCE6B33FF0000000000000000000000000000
      00000F0B064074512CAF875A30BF3A24138007040230773F20BFCD6633FFCEA6
      8BFFC2AD9BFFC5B19FFF554B42AF000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B6B6
      B6FFFFFFFFFFFDFDFDFFB6B6B6FFFFFFFFFFFFFFFFFFDBCDBFFFDBCDBFFFDBCD
      BFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCDBFFFFFFF
      FFFFFFFFFFFF878787FF0000000000000000B95E31FFDE8355FFDE7E50FFDF7B
      4AFFDF7B4AFFDF7B4AFFDF7B4AFFDF7B4AFFDF7B4AFFDA6D38FFAE592AEF0603
      0130000000000000000000000000000000000000000000000000000000000000
      0000331B0C80E07C41FFE58145FF723B1BBF632F17BFD87C4FFFD17345FF2D16
      0A80000000000000000000000000000000000000000000000000000000000000
      000006030130AD582AEFDB713AFFE2763DFFE3783EFFE4793FFFE47B40FFE57C
      41FFE57C41FFE57D42FFE67E43FFCE6B33FF0000000000000000000000000000
      00000000000000000000000000000000000000000000000000000D060340BBA4
      8FFFC6B5A5FFBDAB9AFFD1C0B1FF443C369F0000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B6B6
      B6FFFFFFFFFFFEFEFEFFB6B6B6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF878787FF0000000000000000B85E33FFE38B5EFFDA7846FFDB79
      46FFDB7742FFDA7139FFD8682DFFE06E34FFDB6E37FFAD572AEF060301300000
      0000000000000000000000000000000000000000000000000000000000000000
      00004E28139FDF7A40FFE68045FF713A1ABF632F17BFDC8356FFD2774BFF4522
      109F000000000000000000000000000000000000000000000000000000000000
      00000000000006030130AE592AEFDE733AFFE5783CFFE07838FFE07938FFE17B
      39FFE27C3AFFE27D3BFFE88043FFCC6932FF0000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000CEB9
      A6FFC2B1A0FFCFBEADFFFAF3ECFFD3C5B8FF453E379F00000000000000000000
      000000000000000000000000000000000000000000000000000000000000B6B6
      B6FFFFFFFFFFFEFEFEFFB6B6B6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF878787FF0000000000000000B75E34FFE38E63FFD67340FFD773
      3FFFD7723DFFD8723CFFE07640FFDA6D38FFAA5529EF06030130000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00009B5025DFE57D43FFE47D43FF703A1ABF622F16BFDB845AFFDC8356FF8A44
      21DF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000006030130AE582AEFDE733AFFE3773BFFDD7534FFDD76
      34FFDE7735FFDE7835FFE77C40FFCB6832FF0000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000D3C0
      AEFFF9F2ECFFDBC9B8FFE7D9CBFFFCF9F5FFD5C9BFFF463F3A9F000000000000
      000000000000000000000000000000000000000000000000000000000000B6B6
      B6FFFFFFFFFFFFFFFFFFB6B6B6FFFFFFFFFFFFFFFFFFDBCDBFFFDBCDBFFFDBCD
      BFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCDBFFFFFFF
      FFFFFFFFFFFF8B8B8BFF0000000000000000B65F35FFE49267FFD6713EFFD670
      3DFFD66F3AFFDA733EFFE17945FFC66432FF2412087000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000C06
      0340CE6B34FFE77C41FFDB743CFF5D2F15AF512712AFD27B52FFE2885CFFBE64
      39FF0B0502400000000000000000000000000000000000000000000000000000
      000000000000000000000000000025120870CB6732FFE3753BFFDE7334FFDC73
      31FFDC7432FFDD7533FFE67A3EFFC96731FF0000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000D4C3
      B2FFFDFAF7FFC7B9ACFF908479CFE8DED3FFFFFEFDFFD6CBC2FF4F4740AB0000
      000000000000000000000000000000000000000000000000000000000000B6B6
      B6FFFFFFFFFFFFFFFFFFB6B6B6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFEFEFEFFA8A8A8FF0000000000000000B55F35FFE4946CFFD5703DFFD670
      3DFFD87441FFD56B35FFDD7541FFDA7341FFA75428EF06030130000000000000
      0000000000000000000000000000000000000000000000000000000000006F38
      1ABFDD7842FFE7834AFFD5723CFF31190C802B150A80C77249FFE28B5FFFD47D
      53FF633017BF0000000000000000000000000000000000000000000000000000
      0000000000000000000006030130AA5529EFDD723CFFE07439FFDA7233FFDE75
      37FFDC7637FFDC7738FFE67C43FFC86631FF0000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000D5C6
      B7FFFFFEFDFFC5B9ADFF0000001091877ECFE9E0D5FFFDFBF8FFD4C7BCFF453E
      379F00000000000000000000000000000000000000000000000000000000B6B6
      B6FFFFFFFFFFFFFFFFFFB6B6B6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8F8F8FFF2F2F2FFE8E8E8FFDADA
      DAFFC4C4C4FFA8A8A8FF0000000000000000B45F36FFE4966FFFD5703FFFE089
      5CFFE2895BFFD56D38FFD46933FFDE7643FFD36E3DFFA55327EF060301300000
      00000000000000000000000000000000000000000000000000003F210F92CE6B
      36FFE7844DFFE88751FFCB6934FF0603013006030130BA643CFFE18C63FFE18A
      5FFFC26B40FF3A1D0E9200000000000000000000000000000000000000000000
      00000000000006030130A75427EFD56D39FFE27740FFDB763BFFDD773CFFE47A
      42FFE37A42FFDD793CFFE57C43FFC66530FF0000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000D5C6
      B7FFFEFDFCFFC5B8ADFF000000000000001090877CCFE7DBCEFFFBF6F1FFCBBB
      ACFF1613115B000000000000000000000000000000000000000000000000B6B6
      B6FFFFFFFFFFFFFFFFFFB6B6B6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE4E4E4FFFFFFFFFFFFFFFFFFF4F4
      F4FFC4C4C4FF787878CF0000000000000000B35E37FFE39872FFE08F65FFD98A
      62FFD17D54FFDF8354FFD36832FFD3662FFFDE7440FFD66F3DFFBF5F2EFF2F17
      0B8000000010000000000000000000000000000000104C2712A1C96733FFE37C
      47FFE6834CFFDF7B46FF944B23DF0000000000000000864121DFD7875FFFE08A
      5FFFDE895EFFBD653AFF4421109F000000100000000000000000000000000000
      00102E170B80BF5F2DFFD76D39FFE1753EFFD97339FFDA743AFFE2773FFFD66F
      3AFFDD733EFFE37841FFE57942FFC5632FFF0000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000D4C3
      B3FFFDFAF6FFC4B6A9FF0000000000000000000000108F8378CFE6D7C8FFECE1
      D6FF7F7163D2000000000000000000000000000000000000000000000000B6B6
      B6FFFFFFFFFFFFFFFFFFB6B6B6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE6E6E6FFFFFFFFFFF4F4F4FFCCCC
      CCFF787878CF000000100000000000000000B25E37FFE39A74FFD98F69FF9F52
      2DEFA0532DEFD9855AFFDF8152FFD3652FFFD2642CFFDC713DFFDC713EFFC965
      33FFA75327EF582C14AF2F180B806A3419BFC2612EFFD16B37FFE37944FFE37D
      47FFE57E4AFFCB6936FF1B0D06600000000000000000180B0560BF6C44FFE08E
      65FFDD8357FFDF895EFFCA7349FFB85D31FF653117BF2D160A80552A13AFA352
      28EFC96737FFDD723FFFDF723CFFD87038FFD97138FFE0753DFFDB713CFFA954
      29EFAA5529EFDC723DFFE37841FFC3622FFF0000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000D3C0
      AFFFFBF6F0FFC3B3A6FF000000000000000000000000000000108E8174CFE4D2
      C2FFD0BBA7FF0000000C0000000000000000000000000000000000000000B6B6
      B6FFFFFFFFFFFFFFFFFFB6B6B6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEEEEEEFFF4F4F4FFCCCCCCFF7878
      78CF00000010000000000000000000000000B15D37FFD8926EFF9C512EEF0603
      013011080450B65D32FFD88459FFDE7E50FFD46832FFD16129FFD66932FFDE70
      3BFFDC6D38FFD66A35FFD36935FFDA6E39FFE0733EFFE17640FFDE7741FFE47B
      46FFD36E3BFF6A3418BF00000000000000000000000000000000602D16BFCB7A
      55FFE18C62FFD9794BFFE08659FFDF875BFFD88054FFD17548FFD6784AFFDE7C
      4CFFE07947FFDA703BFFD66E36FFD86F38FFDF733CFFDA6F3BFFBF5F2DFF1A0D
      066006030130A85429EFDB713CFFC1612EFF0000000000000000000000000000
      000000000000000000000000000000000000000000000000000003030320D2BE
      ABFFFAF2EBFFC2B1A2FF00000000000000000000000000000000000000108D7F
      71CFD5BDA6FF0606052F0000000000000000000000000000000000000000B6B6
      B6FFFFFFFFFFFFFFFFFFB6B6B6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCDCDCDFFCCCCCCFF787878CF0000
      001000000000000000000000000000000000AC552EFF9A502EEF060201300000
      0000000000000B0502409E4F2AEFCD774EFFDF8457FFDA7443FFD3652EFFD162
      2AFFD5672FFFD86932FFDA6B35FFDA6D36FFD96F38FFDC733DFFE17642FFD46D
      3BFF7C3D1DCF0000001000000000000000000000000000000000000000107238
      1DCFCE7E59FFDF8B62FFD9794BFFD6703EFFD97545FFDA7645FFD97442FFD76F
      3BFFD46A35FFD66D38FFDD733FFFE07440FFCF6736FFA55127EF0B0502400000
      00000000000006030130A85429EFBD5E2DFF0000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B6A4
      93EFECDFD3FFC1AE9FFF00000000000000000000000000000000000000000000
      00108D7D6ECF030303230000000000000000000000000000000000000000B6B6
      B6FFFFFFFFFFFFFFFFFFB6B6B6FFB6B6B6FFB6B6B6FFB6B6B6FFB6B6B6FFB6B6
      B6FFB6B6B6FFB6B6B6FFB6B6B6FFB1B1B1FF9E9E9EFFA0A0A0EF060606300000
      000000000000000000000000000000000000924321EF06020130000000000000
      0000000000000000000002010020632F17BFBD653AFFD47B50FFDE8052FFDD77
      47FFDA713EFFD96D38FFD96C37FFDD6F39FFE0723DFFD96F3CFFC66332FF793C
      1CCF000000100000000000000000000000000000000000000000000000000000
      0010723619CFBF6C44FFD7875FFFE08A60FFDD8052FFDA7848FFDA7746FFDC79
      48FFDF7C4CFFDF7B4CFFD47142FFC06031FF663117BF02010020000000000000
      0000000000000000000006030130A25025EF0000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000332D
      2880D5C2B1FFC0AD9BFF00000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B6B6
      B6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFEEEEEEFFF4F4F4FFCCCCCCFF787878CF00000010000000000000
      0000000000000000000000000000000000000502013000000000000000000000
      0000000000000000000000000000000000000B050240632F17BFB75C30FFC368
      3CFFCF7043FFD06E3FFFD06B3AFFCA6434FFC05E2EFF8D4521DF190C06600000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000190C0662864121DFBA643CFFC77249FFD0794FFFD0774DFFCF75
      4AFFC3683CFFB85C2FFF653117BF0B0502400000000000000000000000000000
      0000000000000000000000000000060301300000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007C6B5CCFB39D89FF00000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B6B6
      B6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFCDCDCDFFCCCCCCFF787878CF0000001000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000101108
      04502C160A802C160A802D160A80221107700603013000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000006030130210F07702B150A802C150A802C15
      0A80110804500000001000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000010877666DF00000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B6B6
      B6FFB6B6B6FFB6B6B6FFB6B6B6FFB6B6B6FFB6B6B6FFB6B6B6FFB6B6B6FFB6B6
      B6FFB1B1B1FF9E9E9EFFA0A0A0EF060606300000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0075000000EA0101018A0101018A0101018A0101018A0101018A0101018A0101
      018A0101018A0101018A0101018A000000EA000000EA000000EA000000EA0000
      00EA000000EA000000EA000000AF000000000000000000000081000000AB0000
      00AB14140D6614140D6614140D6614140D6614140D6614140D6614140D661414
      0D6614140D6614140D6614140D6614140D6614140D6614140D6614140D661414
      0D66000000AB000000AB00000081000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000210E0D0DC1332F2FC3151515BB232121BC332E2EBB312D2DBF2120
      20E4222222E7282828E90F0F0FC70000000000000000000000020B0F125B0D16
      1D790C12176D0303042E06080943111F267C1427329015293292132027830F14
      17630203032A0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000720000
      00E3474747FFB9A8A8FFB4A4A4FF434343FF525252FF434343FFB9A8A8FFB4A4
      A4FFB4A4A4FFB4A4A4FFB9A8A8FF434343FF545454FF545454FF545454FF5252
      52FF5D5D5DFF626262FF000000E30000000000000000000000AD848484FF8080
      80FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFBFBFFF7F7F7FFF3F3F3FFF7F7
      F7FFFAFAFAFFFDFDFDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF808080FF848484FF000000AD000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000F0F0FBC5B5A5AFFB0A4A4FF494949FF847C7CFFC7B8B8FFB7AAAAFF5857
      57FF585858FF616161FF282828E400000000000000000A0D0F4F1E4970E01B56
      8EFF063F75FF072B55FF042F50FF21749FFF3991C2FF3688B6FF297DAFFF2277
      A8FF2B77A1FF325B71D81016196B000000070000000000000000000000000000
      000000000000000000000000000000000000000000000000006E000000DB6767
      67FF414141FFB7A9A9FFAEA0A0FF414141FF4E4E4EFF414141FFB7A9A9FFAEA0
      A0FFAEA0A0FFAEA0A0FFB7A9A9FF414141FF545454FF545454FF545454FF4E4E
      4EFF565656FF5F5F5FFF000000DB0000000000000000000000AE000000002A2A
      2AD7FEFEFDFFFDFDFBFFFDFDFBFFFDFDFBFFF6F6F4FFEDEDEBFFE5E5E3FFE7E7
      E5FFEBEBE9FFF3F3F1FFF9F9F7FFFDFDFBFFFDFDFBFFFDFDFBFFFDFDFBFFFEFE
      FDFF757575FF7E7E7EFF000000AE000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000282828DF5E5D5DFFA9A4A4FF3E3D3DFF7B7878FFC2BCBCFFB1ACACFF5150
      50FF515151FF575757FF242424D80000000000000003000000000000000F316D
      93E8124E83FF043362FF000A31FF023251FF1972A7FF2271A4FF165A8CFF0859
      95FF005CA2FF0263A9FF1F76A9FF39799AFD1116196D00000000000000000000
      00000000000000000000000000000000000000000000000000D2696969FF5858
      58FF3E3E3EFFBAAFAFFFB1A6A6FF3E3E3EFF4A4A4AFF3E3E3EFFBAAFAFFFB1A6
      A6FFB1A6A6FFB1A6A6FFBAAFAFFF3E3E3EFF545454FF545454FF545454FF4A4A
      4AFF585858FF616161FF000000D20000000000000000000000AF7D7D7DFF7474
      74FFFDFDFAFF2F3332FF2F3332FF2F3332FF255E9CFF497AB5FF5F89B9FF94AB
      C4FFC8CED2FFE2E2DEFFEEEEEAFFF8F8F4FFFBFBF7FFFBFBF7FFFBFBF7FFFDFD
      FAFF747474FF7D7D7DFF000000AF00000000000000005C4225B7B38148FFB381
      48FFB38148FFB38148FFB38148FFB38148FFB38148FFB38148FFB38148FFB381
      48FF484037FF595959FFA9A9A9FFA9A8A8FFADACACFFB5B4B4FFABAAAAFF4949
      49FF494949FF535353FF252525D6000000000000000000000000406C83CE1D96
      CAFF106D9EFF0A3461FF707371FF4F4944FF0D4F72FF0A6AA7FF2B6F9AFFA2A1
      8DFFC4B394FF516F7DFF004F90FF1A74B1FF2174A3FF2B5264D4000001160000
      00000000000000000000000000000000000000000000000000CC626262FF5959
      59FF3C3C3CFFBCB5B5FFB3ACACFF3C3C3CFF454545FF3C3C3CFFBCB5B5FFB3AC
      ACFFB3ACACFFB3ACACFFBCB5B5FF3C3C3CFF545454FF545454FF545454FF4545
      45FF595959FF626262FF000000CC0000000000000000000000B17B7B7BFF7272
      72FFFCFCF7FF2F3332FF2F3332FFF8F8F3FF4E80B9FFC0E1F6FFA6D4F0FF82B8
      DFFF3776B7FFD3D3CFFFDDDDD9FFEBEBE6FFF5F5F0FFF8F8F3FFF8F8F3FFFCFC
      F7FF727272FF7B7B7BFF000000B10000000000000000B38148FFFAFAF9FFFAFA
      F9FFFAFAF9FFFAFAF9FFFAFAF9FFFAFAF9FFFAFAF9FFFAFAF9FFFAFAF9FFFAFA
      F9FF575757FF5B5B5BFF484849FF4D4D4EFF4C4C4DFF4A4A4CFF4A4A4BFF4343
      44FF414142FF5A5A5BFF272727D50000000000000000233137871392C8FF288D
      BEFF797CCAFFB270CCFFFDE1F3FFBDB597FF413D39FF033D62FF005595FF4984
      A7FFEAD19CFFFFE997FF999785FF004884FF0C5894FF2279AFFF1C668CFF0809
      093B0000000000000000000000000000000000000000000000C9646464FF5B5B
      5BFF393939FFBFBBBBFFB6B2B2FF252525FF282828FF252525FFBFBBBBFFB6B2
      B2FFB6B2B2FFB6B2B2FFBFBBBBFF393939FF545454FF545454FF545454FF3F3F
      3FFF5B5B5BFF646464FF000000C90000000000000000000000B27A7A7AFF7171
      71FFFAFAF3FFF5F5EEFFF5F5EEFFF5F5EEFF8AA9CBFFD4E6F4FFD7E9FAFFCBE3
      F9FF9FD9F4FF4389C2FFD1D1CBFFDADAD4FFE8E8E1FFF2F2EBFFF5F5EEFFFAFA
      F3FF717171FF7A7A7AFF000000B20000000000000000B38148FFFAFAF9FF9B64
      50FF93543DFF925339FF925237FF915235FF8F5235FF8A5335FF855435FF7E55
      35FF3F3833FF8D8D8AFFB0B0A7FFADADA4FFACACA4FFACACA4FFADADA5FFAFAF
      A6FFB1B1A9FF8D8D8AFF222223D300000000020303223B99C1F630A2D2FFA87D
      D0FFF484E5FFF28BF1FFEE83ECFFFFEBFAFFAEA88DFF46403CFF053C5BFF004F
      97FF005CA8FFDCC8A2FFFFE290FF77877EFF005696FF0E538AFF1060A5FF226F
      97FF0A0A0A3C00000000000000000000000000000000000000C7666666FF5D5D
      5DFF373737FFC2C0C0FFB9B7B7FFB9B7B7FFB9B7B7FFB9B7B7FFB9B7B7FFB9B7
      B7FFB9B7B7FFB9B7B7FFC2C0C0FF373737FF545454FF545454FF545454FF3A3A
      3AFF5D5D5DFF666666FF000000C70000000000000000000000B4787878FF6F6F
      6FFFF9F9EFFFF1F1E8FFF1F1E8FFF1F1E8FFCBD7DDFF7AA4D1FFF9FCFEFFBCE3
      F9FF37BDE8FF509ACCFF448AC2FFCDCDC6FFD7D7CFFFE4E4DCFFEEEEE5FFF9F9
      EFFF6F6F6FFF787878FF000000B40000000000000000B38148FFFAFAF9FF9051
      39FFD76D38FFD66B36FFD56B35FFCF6D35FFC57335FFB77933FFA38333FF8A8B
      33FF3C4130FFBBBBB4FFFFFFEEFFF5F5E5FFF4F4E5FFF4F4E5FFF4F4E5FFF5F5
      E5FFFFFFEEFFBBBBB4FF1F1F20D000000000141E22692DAFDDFF6C96D3FF9F89
      D5FFC289DEFFB392DEFF5F91D3FF8892D8FFFFF5FAFFAF9F8BFF5B5957FF1D49
      5FFF346680FFDDC6A3FFFFE9A7FFD4B27CFF1A5F90FF1F77A0FF27778BFF0051
      9BFF1E6287F70303031F000000000000000000000000000000C5686868FF5F5F
      5FFF353535FFC8C8C8FFC3C3C3FFC3C3C3FFC3C3C3FFC3C3C3FFC3C3C3FFC3C3
      C3FFC3C3C3FFC3C3C3FFC8C8C8FF353535FF545454FF545454FF545454FF3636
      36FF5F5F5FFF686868FF000000C50000000000000000000000B6767676FF6D6D
      6DFFF7F7ECFFEEEEE3FFEEEEE3FFEEEEE3FFEEEEE3FF5C89BCFFC7E4F4FF43D0
      F4FF00C3F2FF24B8E6FF509ACCFF488CC3FFCBCBC1FFD4D4CAFFE1E1D7FFF4F4
      E9FF6D6D6DFF767676FF000000B60000000000000000B38148FFFAFAF9FF9253
      38FFD66B35FFD46B35FFCB7035FFBB7735FFA38333FF858E32FF679435FF589C
      2CFF374C2FFFBCBCB7FFFAFAEEFFEFEFE5FFEFEFE5FFEFEFE5FFEFEFE5FFEFEF
      E5FFFAFAEEFFBCBCB7FF212121CF000000001A303A883EB1E5FF4FA7DDFF1D9C
      D1FF2D91CCFF37A2D3FF1EB0CDFF3590CCFFA78FD4FFFCF9EFFF675D56FF342C
      25FF9C7C55FFBDA177FFE5C390FFB2A282FF0C5399FF01519EFF70D3AAFF3A94
      92FF004B9AFF2C4955BB000000000000000000000000000000C26A6A6AFF6161
      61FF333333FF323232FF323232FF323232FF323232FF323232FF323232FF3232
      32FF323232FF323232FF323232FF333333FF333333FF333333FF333333FF3333
      33FF616161FF6A6A6AFF000000C20000000000000000000000B8747474FF6B6B
      6BFFF6F6E8FFEBEBDEFFEBEBDEFFEBEBDEFFEBEBDEFFEBEBDEFF568AC0FFC1EA
      F7FF27CBF3FF00C3F2FF24B8E6FF509ACCFF4A8EC5FFC8C8BDFFD2D2C6FFE9E9
      DCFF696969FF747474FF000000B80000000000000000B38149FFFAFAF9FF9252
      36FFD46B35FFC87035FFB27C33FF8F8A32FF6D9433FF5A9A33FF53A723FF3CD4
      0AFF395138FFC0C0BDFFFFFFFAFFF7F7F0FFF7F7F0FFF7F7F0FFF7F7F0FFF7F7
      F0FFFFFFFAFFC1C1BEFF232324D400000000070809354EAEDBFF3C94BAEF1E30
      3A8B1C313A8B2E88AFEB3FA8DAFF47AAD5FF1180BDFF5D9FC8FFC2B2A9FF3229
      27FF313131FF20516BFF095180FF24759CFF43A5AEFF13609DFF55ABABFF9EFF
      AFFF186E92FF025493FF111314560000000000000000000000C06C6C6CFF6363
      63FF636363FF636363FF636363FF636363FF636363FF636363FF636363FF6363
      63FF636363FF636363FF636363FF636363FF636363FF636363FF636363FF6363
      63FF636363FF6C6C6CFF000000C00000000000000000000000BA727272FF6969
      69FFF4F4E6FFE9E9DAFFE9E9DAFFE9E9DAFFE9E9DAFFE9E9DAFFE9E9DAFF5A8B
      C2FFC1EAF8FF27CBF3FF00C3F2FF24B8E6FF509ACCFF4D90C6FFC7C7BAFFDADA
      CDFF636363FF707070FF000000BA0000000000000000B38149FFFAFAF9FF9052
      35FFC97035FFAF7E33FF808E33FF5C9832FF51A723FF4DB910FF3FD816FF68BF
      6BFF423D41FFCCCCCEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFB5B5B6F71515159F00000000000000000000000F000000090000
      000000000000000000074A8BA9DD39A6D8FF2F94C7FF0479BCFF80B2CDFFCEB9
      ACFF29221EFF272F37FF00367BFF2A8F94FFABFFB2FF89EEB4FF85E4B3FFA1FF
      B0FF3BA292FF004FA0FF2C4F61C70000000000000000000000BE6E6E6EFF6565
      65FF656565FF656565FF656565FF656565FF656565FF656565FF656565FF6565
      65FF656565FF656565FF656565FF656565FF656565FF656565FF656565FF6565
      65FF656565FF6E6E6EFF000000BE0000000000000000000000BC707070FF6767
      67FFD3D3C7FFF3F3E3FFF3F3E3FFF3F3E3FFF3F3E3FFF3F3E3FFF3F3E3FFF3F3
      E3FF5F8FC4FFC1EAF7FF27CBF3FF00C3F2FF24B8E6FF509ACCFF5092C7FFB4B4
      AAFF5B5B5BFF6A6A6AFF000000BD0000000000000000B38149FFFAFAF9FF8B53
      35FFB57A33FF7E8E33FF5A9A31FF4CB91AFF49D425FF3CE82FFF5FCA71FF4C17
      45FF131113FF61625AFF8A8379FFA3A791FF99C17DFFA7AF83FFA89E84FFD2D2
      C4FFBEAE8CFF42423DB809090997000000000000000000000000000000000000
      0000000000000000000013181A5933A4D8FF3294C9FF3096CCFF1288C5FF93B4
      C6FFC9B3A8FF291E17FF222433FF227085FF7DE2A0FF9EFFB5FF90F7B3FF5ED0
      9EFF1D7A9AFF105FA1FF1C6E9CFF090A0B4000000000000000BC707070FF6767
      67FFD3D3C7FFF3F3E3FFF3F3E3FFF3F3E3FFF3F3E3FFF3F3E3FFF3F3E3FFF3F3
      E3FFF3F3E3FFF3F3E3FFF3F3E3FFF3F3E3FFF3F3E3FFF3F3E3FFF3F3E3FFD3D3
      C7FF676767FF707070FF000000BC0000000000000000000000BE6E6E6EFF6565
      65FF656565FF656565FF656565FF656565FF656565FF656565FF656565FF6565
      65FF656565FF6393C4FFC1EAF8FF27CBF3FF00C3F2FF24B8E6FF509ACCFF5394
      C8FF565656FF626262FF000000C20000000400000000B38149FFFAFAF9FF8355
      35FF8B8A33FF55A229FF40CA0EFF44DC35FF91618FFF674661FF823A70FF0000
      00FF000000FF000000FF000000FF353D35FF53C929FF679530FF715A34FFFAFA
      F9FFB38149FF0000000000000000000000000000000000000000000000000000
      00000000000000000000111516542DA0D7FF2392CCFF339BD0FF269BD6FF0B7B
      BBFFA2B7C3FFC7B2A6FF251810FF182237FF298294FF80E6A1FF74E6A5FF0D6E
      9BFF004A95FF116699FF237CB0FF161E217100000000000000BA727272FF6969
      69FFF4F4E6FFE9E9DAFFE9E9DAFFE9E9DAFFE9E9DAFFE9E9DAFFE9E9DAFFE9E9
      DAFFE9E9DAFFE9E9DAFFE9E9DAFFE9E9DAFFE9E9DAFFE9E9DAFFE9E9DAFFF4F4
      E6FF696969FF727272FF000000BA0000000000000000000000C06C6C6CFF6363
      63FF636363FF636363FF636363FF636363FF636363FF636363FF636363FF6363
      63FF636363FF636363FF6794C6FFC2EAF8FF27CBF3FF00C3F2FF24B8E6FF509A
      CCFF6195C0FF5C5C5CFF000000C70000000E00000000B38149FFFAFAF9FF7B56
      35FF62A02BFF48CC30FF65BF62FF97659BFF210A1DFF000000FF050405FF0000
      00FF000000FF000000FF020001FF3D793AFF49CF1CFF659532FF6F5B34FFFAFA
      F9FFB38149FF0000000000000000000000000000000000000000000000000000
      0000000000000000000A5698B7E83BADE2FF172228710101011A26485AAC1A90
      CFFF1888C2FFAABCC5FFBDA99DFF221209FF09203FFF1475A1FF248C94FF2480
      AFFF5299D1FF3589BAFF1D7DB1FF10222C8F00000000000000B8747474FF6B6B
      6BFFF6F6E8FFEBEBDEFFEBEBDEFFEBEBDEFFEBEBDEFFEBEBDEFFEBEBDEFFEBEB
      DEFFEBEBDEFFEBEBDEFFEBEBDEFFEBEBDEFFEBEBDEFFEBEBDEFFEBEBDEFFF6F6
      E8FF6B6B6BFF747474FF000000B80000000000000000000000C26A6A6AFF6161
      61FF333333FF333333FF333333FF333333FF333333FF323232FF323232FF3232
      32FF323232FF323232FF323232FF6B98C8FFC2EAF8FF27CBF3FF00C3F2FF3BB1
      D6FF92979BFF8C97A1FF000000CB0000001C00000000B38249FFFAFAF9FF7D55
      35FF70972FFF4ABE26FF49D12FFF73A879FF531C4CFF160212FF2B1326FF0000
      00FF000000FF000000FF000000FF335A33FF4DCD22FF659531FF6F5B34FFFAFA
      F9FFB38249FF0000000000000000000000000000000000000000000000000000
      00000101021C2D7AA2F03FB7EDFF438CB1ED000000040000000000000000375E
      72BA1C97D5FF1081BEFFADBFC9FFB9A397FF1A0900FF022E50FF0B6BBAFF93C2
      EFFFC5F1FFFF94CFF8FF2482B9FF0E24319600000000000000B6767676FF6D6D
      6DFFF7F7ECFFEEEEE3FFEEEEE3FFEEEEE3FFEEEEE3FFEEEEE3FFEEEEE3FFEEEE
      E3FFEEEEE3FFEEEEE3FFEEEEE3FFEEEEE3FFEEEEE3FFEEEEE3FFEEEEE3FFF7F7
      ECFF6D6D6DFF767676FF000000B60000000000000000000000C5686868FF5F5F
      5FFF363636FF545454FF545454FF545454FF353535FFC8C8C8FFC3C3C3FFC3C3
      C3FFC3C3C3FFC3C3C3FFC3C3C3FFC3C3C3FF6E9AC9FFC2EBF8FF3DC1E1FF8C9A
      9DFFD7D7D7FFC9C9C9FF8D98A1FF0000002500000000B38249FFFAFAF9FF8754
      35FF9E8333FF629531FF4EAF1AFF3DD91AFF7AA776FF70AA74FF926E8AFF0D02
      0BFF000000FF000000FF000000FF444841FF51C829FF6B9431FF745934FFFAFA
      F9FFB38249FF0000000000000000000000000000000000000000000000000000
      0000131D226A47BEF3FF55BAE9FF3298CCFF12171A6600000000000000002636
      3E8E29A9DDFF28A0D6FF1D91CBFFA5BBCAFFAB9188FF0E0000FF1A597DFF67B9
      F0FF8FC2EAFFC5EDFFFF65B1E2FF0B1D278600000000000000B4787878FF6F6F
      6FFFF9F9EFFFF1F1E8FFF1F1E8FFF1F1E8FFF1F1E8FFF1F1E8FFF1F1E8FFF1F1
      E8FFF1F1E8FFF1F1E8FFF1F1E8FFF1F1E8FFF1F1E8FFF1F1E8FFF1F1E8FFF9F9
      EFFF6F6F6FFF787878FF000000B40000000000000000000000C7666666FF5D5D
      5DFF3A3A3AFF545454FF545454FF545454FF373737FFC2C0C0FFB9B7B7FFB9B7
      B7FFB9B7B7FFB9B7B7FFB9B7B7FFB9B7B7FFB9B7B7FF789BC2FF9A9D9EFFD7D7
      D7FFF5F5F5FFC2C2C2FF5B86BDFF2562BAED00000000B38249FFFAFAF9FF8D52
      35FFC07635FF958832FF649535FF53A626FF44CA12FF35E10AFF41E144FF7A4A
      7EFF2A1026FF080307FF492546FF739E62FF4ABB20FF7E8D33FF7C5734FFFAFA
      F9FFB38249FF0000000000000000000000000000000000000000000000000000
      0000090A0B3940B0E2FF4DB1E1FF4BB0E3FF3491C1FF1C2E389B18252C893C9C
      C8FE3389C7FF2F81BCFF32A9DEFF2AA2D6FFBDD9DFFFA28579FF070000FF015E
      90FFA2DFFFFF9DCEF6FF298AC4FF0F16196200000000000000B27A7A7AFF7171
      71FFFAFAF3FFF5F5EEFFF5F5EEFFF5F5EEFFF5F5EEFFF5F5EEFFF5F5EEFFF5F5
      EEFFF5F5EEFFF5F5EEFFF5F5EEFFF5F5EEFFF5F5EEFFF5F5EEFFF5F5EEFFFAFA
      F3FF717171FF7A7A7AFF000000B20000000000000000000000C9646464FF5B5B
      5BFF3F3F3FFF545454FF545454FF545454FF393939FFBFBBBBFFB6B2B2FFB6B2
      B2FFB6B2B2FFBFBBBBFF252525FF282828FF252525FFB6B2B2FF8D99A6FFC4C4
      C4FFC2C2C2FF4E8BC3FF0963EBFF0A5CD9F600000000B38249FFFAFAF9FF9152
      35FFCF6D35FFBC7735FF978832FF6D9433FF5A9A31FF55A423FF49C410FF46DF
      32FF749474FF65265FFF5CD253FF48C71DFF5E9A2FFF9E8533FF855435FFFAFA
      F9FFB38249FF0000000000000000000000000000000000000000000000000000
      0000000000002B3B438C37AAE1FF46A7D8FF55B9E9FF48B3E2FF39AEDDFF42A0
      D9FF3D42B5FF3840B0FF257BBEFF69DBF2FFBDFFFFFFCBE4ECFF8D6D61FF1A14
      13FF66B8EAFF35A4DFFF00669FFF0506072F00000000000000B17B7B7BFF7272
      72FFFCFCF7FFF8F8F3FFF8F8F3FFF8F8F3FFF8F8F3FFF8F8F3FFF8F8F3FFF8F8
      F3FFF8F8F3FFF8F8F3FFF8F8F3FFF8F8F3FFF8F8F3FFF8F8F3FFF8F8F3FFFCFC
      F7FF727272FF7B7B7BFF000000B10000000000000000000000CC626262FF5959
      59FF454545FF545454FF545454FF545454FF3C3C3CFFBCB5B5FFB3ACACFFB3AC
      ACFFB3ACACFFBCB5B5FF3C3C3CFF454545FF3C3C3CFFB3ACACFFBCB5B5FF8D99
      A7FF6890C3FF0963EBFF0963EBFF05429ED300000000B3824AFFFAFAF9FF9252
      37FFD66B35FFCE6D35FFBF7635FFA48233FF838E32FF659533FF589B30FF49B8
      13FF4AD02FFF7AAC6EFF44C319FF5C9A2DFF8E8A33FFBE7633FF8C5235FFFAFA
      F9FFB3824AFF0000000000000000000000000000000000000000000000000000
      00000000000000000000264352A237A7E0FF4FB6E4FF469CD4FF42AFDBFF428E
      CDFF4742C7FF4C4BCCFF2F5DB5FF33A5D1FF7AD1EEFFB0FFFFFFC1E8F2FF8E66
      57FF0A1D22FF0088CCFF264B61AF0000000000000000000000AF7D7D7DFF7474
      74FFFDFDFAFFFBFBF7FFFBFBF7FFFBFBF7FFFBFBF7FFFBFBF7FFFBFBF7FFFBFB
      F7FFFBFBF7FFFBFBF7FFFBFBF7FFFBFBF7FFFBFBF7FFFBFBF7FFFBFBF7FFFDFD
      FAFF747474FF7D7D7DFF000000AF0000000000000000000000D2616161FF5858
      58FF4A4A4AFF545454FF545454FF545454FF3E3E3EFFBAAFAFFFB1A6A6FFB1A6
      A6FFB1A6A6FFBAAFAFFF3E3E3EFF4A4A4AFF3E3E3EFFB1A6A6FFBAAFAFFF3E3E
      3EFF316FC9FF0E64E5FF074EB9F700040A3A00000000B3824AFFFAFAF9FF9253
      39FFD76B36FFD66B35FFD06D35FFC57335FFB57A33FF9C8532FF7E8E33FF6495
      33FF55A729FF4DB123FF659731FF928833FFBB7733FFCF6D35FF915235FFFAFA
      F9FFB3824AFF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000223139854ABAE9FF3C9ED8FF4186C9FF416B
      C4FF4570CFFF4652C7FF396BBCFF29A1D2FF369CD1FFB6F7FFFF9BF9FFFFA6DE
      EFFF835E4EFF05283CFF040505270000000000000000000000AE7E7E7EFF7575
      75FFFEFEFDFFFDFDFBFFFDFDFBFFFDFDFBFFFDFDFBFFFDFDFBFFFDFDFBFFFDFD
      FBFFFDFDFBFFFDFDFBFFFDFDFBFFFDFDFBFFFDFDFBFFFDFDFBFFFDFDFBFFFEFE
      FDFF2A2A2AD700000000000000AE0000000000000000000000DB5F5F5FFF5656
      56FF4E4E4EFF545454FF545454FF545454FF414141FFB7A9A9FFAEA0A0FFAEA0
      A0FFAEA0A0FFB7A9A9FF414141FF4E4E4EFF414141FFAEA0A0FFB7A9A9FF4141
      41FF676767FF000000DB0000006E0000000000000000B3824AFFFAFAF9FF9152
      3BFFD76D3CFFD76B36FFD66B35FFD36B35FFCC6E35FFC37335FFB67933FFA482
      33FF978533FF988533FFAC7E33FFC37435FFD06D35FFD66B35FF904E33FFFAFA
      F9FFB3824AFF0000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000007090A39438AA9E240ACE2FF3CAE
      E2FF3D86CDFF3A57B9FF379ACDFF46B3E0FFA9F0FCFFCEFFFFFFACF9FFFF32C7
      FAFF7AC1E3FE725549FF1D1A197B0000000000000000000000AD848484FF8080
      80FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF808080FF848484FF000000AD0000000000000000000000E3626262FF5D5D
      5DFF525252FF545454FF545454FF545454FF434343FFB9A8A8FFB4A4A4FFB4A4
      A4FFB4A4A4FFB9A8A8FF434343FF525252FF434343FFB4A4A4FFB9A8A8FF4747
      47FF000000E300000072000000000000000000000000B3824AFFFAFAF9FF9C65
      53FF935540FF92543AFF925237FF925236FF915235FF8F5235FF8B5335FF8954
      35FF865435FF885435FF8A5335FF8F5235FF915236FF925237FF995F48FFFAFA
      F9FFB3824AFF0000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000C11124D3967
      7AC24089C8FF3FA8DBFF3CB2E4FF3AAADFFF74D6F5FF81DFFBFF48B8E2FF3477
      98D800010115171717554A4745FF060606310000000000000081000000AB0000
      00AB14140D6614140D6614140D6614140D6614140D6614140D6614140D661414
      0D6614140D6614140D6614140D6614140D6614140D6614140D6614140D661414
      0D66000000AB000000AB000000810000000000000000000000AF000000EA0000
      00EA000000EA000000EA000000EA000000EA000000EA0101018A0101018A0101
      018A0101018A0101018A0101018A0101018A0101018A0101018A0101018A0000
      00EA0000007500000000000000000000000000000000B3824AFFFAFAF9FFFAFA
      F9FFFAFAF9FFFAFAF9FFFAFAF9FFFAFAF9FFFAFAF9FFFAFAF9FFFAFAF9FFFAFA
      F9FFFAFAF9FFFAFAF9FFFAFAF9FFFAFAF9FFFAFAF9FFFAFAF9FFFAFAF9FFFAFA
      F9FFB3824AFF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000101140C12155419272E7C243D49981B354396101F2775050708360000
      0000000000000000000301010117000000030000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000066492AC0B3824AFFB382
      4AFFB3824AFFB3824AFFB3824AFFB3824AFFB3824AFFB3824AFFB3824AFFB382
      4AFFB3824AFFB3824AFFB3824AFFB3824AFFB3824AFFB3824AFFB3824AFFB382
      4AFF66492AC00000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000081431FCFCA6A
      34FF4C27129F0000001000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000010030318600A0A429F0E0E60BF0F0F
      60BF0F0F60BF0F0F60BF0B0B429F040418600000001000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000001B2F378F61A0B8FF3C6577D40000000D00000000000000000000
      00000000000000000010080E1150223A449F315463BF253E48BB0A1013730000
      0016000000000000000000000000000000000000000000000000CA6A34FFE288
      4DFFDB8147FFB4602EEF26140970000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000001006062B80141497EF0F0FABFF0E0EB2FF0E0EB8FF1111
      BEFF1515C2FF1717C4FF1919C0FF1A1AB8FF191998EF07072A80000000100000
      0000000000000000000000000000000000000000000000000000000000000000
      0000060601660B0B02880B0B02880B0B02880B0B02880B0B02880B0B02880B0B
      02880B0B02880B0B02880B0B02880B0B02880B0B02880B0B02880B0B02880606
      0166000000000000000000000000000000000000000000000000000000000000
      0000223A449F63A3BBFF97DDEBFF5997B0FF0000003C0000000C05090B401B2F
      378F3B6374CF5A99B2FF5EA0BAFF63A7C3FF66ACC9FF61A3BCFF69AAC0FF4779
      8DE9000000140000000000000000000000000000000000000000CA6A34FFE080
      45FFE18346FFE48F53FFDA8145FF874720CF0D07034000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000630151598EF0C0CA8FF0808B4FF0C0CBDFF0F0FC2FF1515C6FF2323
      CDFF2727D1FF2727D5FF2121D7FF2727DBFF2626D8FF2020C3FF161683DF0101
      0630000000000000000000000000000000000000000000000000000000000000
      00000D0D0485FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFEFE
      FDFFFEFEFDFFFEFEFCFFFDFDFBFFFDFDFAFFFDFDF9FFFCFCF8FFFEFEF9FF0D0D
      0485000000000000000000000000000000000000000000000000000000103154
      63BF64ABC3FF7DCDE2FF9DE3EFFF5997B0FF2A4651CF47778CFF5C9CB5FF62A5
      C1FF66ACC9FF6AB4D2FF6AB4D3FF65B3C7FF40A880FF64BAB7FF9CE2EFFF5997
      B0FF020404370000000500000000000000000000000000000000CA6933FFDF7C
      41FFDA6F2FFFDF7B3BFFE79355FFE79759FFD77A3DFF512A139F000000100000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000303
      18601919ADFF0606ADFF0707B8FF0A0ABCFF2424C6FF5858D6FF6F6FDAFF6969
      D6FF6B6BD7FF7676E0FF7474E6FF4F4FE1FF3131E1FF3131E9FF2C2CDDFF2020
      B2FF040417600000000000000000000000000000000000000000000000000000
      00000F0F0680FFFFFFFFFFFFFFFFFFFFFFFFFEFEFEFFFEFEFDFFFDFDFCFFFDFD
      FBFFFCFCFAFFFBFBF9FFFBFBF8FFFAFAF7FFF9F9F5FFF8F8F4FFFCFCF7FF0F0F
      0680000000000000000000000000000000000000000000000000437482CF66AE
      C6FF77CDE4FF81D1E6FF9EE4F0FF5997B0FF4F859BFF528BA2FF64AAC7FF6BB5
      D4FF6BB6D6FF6BB8D7FF66B7CDFF24A848FF22BB46FF46B27CFF9DE3F0FF5997
      B0FF000000100000000C00000002000000000000000000000000C96932FFDF7A
      3FFFDA6F2EFFDC7433FFDF7A37FFE68E4CFFECA060FFE69656FFBD6933EF1E10
      0760000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000006302A2A
      B8FF2323BCFF0505B7FF0707B9FF4646CCFF5151C7FF2929B3FF181897EF0F0F
      60BF0F0F60BF191996EF2D2DB3FF6B6BD6FF8787EEFF3B3BEAFF3C3CF3FF3333
      E6FF2020B1FF0101063000000000000000000000000000000000000000000000
      00001010087CFFFFFFFFFFFFFFFFFEFEFEFFFEFEFDFFFDFDFCFFFDFDFBFFFCFC
      FAFFFBFBF9FFFBFBF8FFFAFAF7FFF9F9F5FFF8F8F4FFF8F8F3FFFBFBF5FF1010
      087C0000000000000000000000000000000000000000000000006CBACFFF79D2
      E7FF78CFE5FF82D3E7FF9FE5F0FF5997B0FF548FA7FF5591AAFF66AECCFF6CB9
      D9FF6CBBDBFF67BAD1FF26AC4EFF29C153FF24BD4AFF47B37DFF9EE4F0FF5997
      B0FF000000010000000000000000000000000000000000000000C96832FFDE78
      3CFFD96E2EFFDC7332FFDF7936FFE2803AFFE68943FFED9D5AFFEFA866FFE491
      50FF78401DBF0704013000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000010181889DF4E4E
      CDFF0404B5FF0505B7FF3333C3FF2929B4FF0E0E61BF01010A40000000000000
      0000000000000000000001010A40151578CF2E2EE1FF3434EBFF3939F1FF3B3B
      F3FF2C2CDEFF161683DF00000010000000000000000000000000000000000000
      000012120A78FFFFFFFFFEFEFEFFFEFEFDFFFDFDFCFFFDFDFBFFFCFCFAFFFBFB
      F9FFFBFBF8FFFAFAF7FFF9F9F5FFF8F8F4FFF8F8F3FFF7F7F1FFFBFBF5FF1212
      0A7800000000000000000000000000000000000000000000000071C2D6FF7AD4
      E8FF79D2E7FF83D6E9FFA1E6F1FF5997B0FF5795AEFF5797B1FF67B3D3FF6DBD
      DFFF68BDD5FF28B052FF2EC75EFF2BC458FF24BD4AFF48B580FFA0E5F1FF5997
      B0FF000000000000000000000000000000000000000000000000C96731FFDD75
      3AFFD96D2DFFDB7231FFDE7835FFE17D39FFE4843DFFE78A42FFEB964EFFF1AA
      67FFF0AA68FFDD8342FF4223108F000000100000000000000000000000000000
      0000000000000000000000000000000000000000000006062C804F4FC9FF0D0D
      B7FF0303B5FF0909B5FF1818AEFF040421700000000000000000000000000000
      00000000000000000000010107302222C1EF2C2CE2FF3030E7FF3434EBFF3C3C
      EAFF3131E8FF2121C4FF07072A80000000000000000000000000000000000000
      000012120A77FFFFFFFFFEFEFDFFFDFDFCFFFDFDFBFFFCFCFAFFFBFBF9FFFBFB
      F8FFFAFAF7FFF9F9F5FFF8F8F4FFF8F8F3FFF7F7F1FFF6F6F0FFFAFAF3FF1212
      0A7700000000000000000000000000000000000000000000000074C7DBFF7BD6
      EAFF7AD4E9FF85D8EAFFA3E7F2FF5997B0FF599BB6FF5A9DB9FF69B7D8FF68C0
      D8FF29B355FF30C962FF31CA62FF2BC458FF24BD4AFF15A92BFF2FAA4BFF4995
      91FF000000000000000000000000000000000000000000000000C86530FFDD72
      37FFD86B2CFFDA702FFFDD7533FFE07B37FFE2813BFFE5873FFFE88C43FFEA91
      47FFEFA25BFFF3B06CFFEA9E5CFFBC6630EF1D0F076000000000000000000000
      000000000000000000000000000000000000000000002121A0EF4646C9FF0202
      B3FF0404B4FF1616AFFF04042170000000000000000000000000000000000000
      000000000000010107301C1CBBEF2424D9FF2828DDFF2C2CE2FF3535E2FF8989
      EEFF3333E1FF2424D6FF191998EF000000000000000000000000000000000000
      000012120A75FFFFFEFFFDFDFCFFFDFDFBFFFCFCFAFFFBFBF9FFFBFBF8FFFAFA
      F7FFF9F9F5FFF8F8F4FFF8F8F3FFF7F7F1FFF6F6F0FFF5F5EEFFFAFAF2FF1212
      0A7500000000000000000000000000000000000000000000000075CADDFF7CD8
      EBFF7BD7EAFF86DAECFFA5E8F2FF5997B0FF5CA0BDFF5DA3C0FF64B8D0FF2BB5
      58FF30C962FF32CB66FF31CA62FF2BC458FF24BD4AFF1CB539FF13AC27FF0798
      0FFF001301600000000000000000000000000000000000000000C7642FFFDC6F
      34FFD7692AFFD96E2EFFDC7332FFDE7835FFE17D39FFE3833DFFE68840FFE88C
      43FFEA9046FFEC964CFFF0A661FFF0AB69FFDF8A4BFF753D1BBF070301300000
      000000000000000000000000000000000000020211504040C3FF1F1FBBFF0101
      B3FF0D0DB1FF0E0E61BF00000000000000000000000000000000000000000000
      0000000007301717B5EF1D1DD2FF2121D5FF2424D9FF2222C1EF151578CF6C6C
      D6FF5454E2FF2626DAFF1A1AB5FF020210500000000000000000000000000000
      000013130B74FEFEFDFFFDFDFBFFFCFCFAFFFBFBF9FFFBFBF8FFFAFAF7FFF9F9
      F5FFF8F8F4FFF8F8F3FFF7F7F1FFF6F6F0FFF5F5EEFFF4F4EDFFF9F9F1FF1313
      0B7400000000000000000000000000000000000000000000000075CADDFF7DDB
      EDFF7CD9ECFF80D9ECFF94E1F0FF62A7BFFF5998B1FF5C9FBBFF3AB567FF30C9
      62FF32CB66FF32CB66FF31CA62FF2BC458FF24BD4AFF1CB539FF13AC27FF08A2
      12FF019303FF0008004000000000000000000000000000000000C6622EFFDB6B
      31FFD66729FFD86B2CFFDA7030FFDD7533FFDF7A36FFE17E3AFFE3833DFFE587
      40FFE78A42FFE88D44FFE98F45FFEB964EFFEFA664FFE99D5DFFD5793DFF3F21
      0F8F000000100000000000000000000000000707378F6666D2FF1515B7FF0101
      B2FF1515AFFF0202115000000000000000000000000000000000000000000000
      06301212AFEF1717CBFF1A1ACEFF1D1DD2FF1C1CBAEF01010730020210503535
      B7FF7878E7FF2424D7FF1919BDFF0808358F0000000000000000000000000000
      000013130B73FEFEFDFFFCFCFAFFFBFBF9FFFBFBF8FFFAFAF7FFF9F9F5FFF8F8
      F4FFF8F8F3FFF7F7F1FFF6F6F0FFF5F5EEFFF4F4EDFFF3F3EBFFF9F9F0FF1313
      0B7300000000000000000000000000000000000000000000000075CADDFF7EDD
      EEFF7EDCEEFF7DDBEDFF7DDAECFF88DDEEFFA4E5EFFF5997B0FF8AE09FFF80DF
      A0FF32CB66FF32CB66FF31CA62FF2BC458FF24BD4AFF1CB539FF13AC27FF08A2
      12FF009A00FF006A02DF00000000000000000000000000000000C6632FFFDA69
      2FFFD46427FFD7692AFFD96D2DFFDB7131FFDD7634FFDF7A36FFE17E39FFE382
      3CFFE4853EFFE5873FFFE68840FFE68841FFE68840FFE99552FFEBA161FFDF8C
      4FFFB25E2CEF130A045000000000000000000D0D63BF7A7AD9FF3939C4FF1212
      B6FF1818AFFF0000000000000000000000000000000000000000000006300E0E
      AAEF1212C5FF1414C8FF1717CBFF1717B4EF0000073000000000000000001C1C
      ABFF8383E4FF2929D5FF1616C2FF0F0F60BF0000000000000000000000000000
      000013130C72FEFEFCFFFBFBF9FFFBFBF8FFFAFAF7FFF9F9F5FFF8F8F4FFF8F8
      F3FFF7F7F1FFF6F6F0FFF5F5EEFFF4F4EDFFF3F3EBFFF2F2E8FFF8F8EEFF1313
      0C7200000000000000000000000000000000000000000000000075CADDFF80DF
      F0FF80DEEFFF7EDDEFFF7EDCEEFF8AE0EFFFABECF4FF5997B0FF4EC698FFC2F5
      CDFF7EDFA0FF32CB66FF31CA62FF2BC458FF24BD4AFF1CB539FF13AC27FF08A2
      12FF009A00FF009300FF00080040000000000000000000000000C56431FFDD74
      3EFFD46529FFD56628FFD76A2BFFD96E2EFFDB7231FFDD7533FFDF7936FFE07C
      38FFE17E3AFFE2813BFFE3823CFFE3823CFFE3823CFFE2813BFFE2833EFFE795
      55FFE6975BFFCA6B34FF03010020000000000D0D63BF7676D7FF3E3EC5FF3232
      C0FF101073CF00000000000000000000000000000000000006300A0AA6EF0D0D
      C0FF0F0FC2FF1212C5FF1212AFEF000007300000000000000000000000001212
      71CF6E6ED8FF2929D2FF1313BFFF0F0F60BF0000000000000000000000000000
      000013130C70FDFDFBFFFBFBF8FFFAFAF7FFF9F9F5FFF8F8F4FFF8F8F3FFF7F7
      F1FFF6F6F0FFF5F5EEFFF4F4EDFFF3F3EBFFF2F2E8FFF0F0E5FFF7F7EBFF1313
      0C7000000000000000000000000000000000000000000000000075CADDFF81E2
      F2FF81E1F1FF80E0F0FF80DFF0FF8BE2F1FFADEDF5FF5997B0FF66B9DDFF50CA
      9CFFC2F5CCFF7EDF9FFF31CA62FF2BC458FF22AE3DFF93E79CFF40C04FFF08A2
      12FF009A00FF009900FF00210080000000000000000000000000C46432FFDD76
      42FFD77038FFD66B30FFD56628FFD76A2BFFD96E2EFFDB7130FFDC7432FFDD76
      34FFDF7936FFDF7A37FFE07B38FFE07B38FFE07B38FFDF7A37FFDF7A38FFE38A
      4BFFE7975BFFCA6D36FF03010020000000000D0D63BF7777D8FF3F3FC6FF3333
      C0FF101073CF000000000000000000000000000006300606A2EF0909BBFF0B0B
      BEFF0D0DC0FF0E0EAAEF00000630000000000000000000000000000000001212
      71CF6D6DD6FF2525CEFF1010BBFF0F0F60BF0000000000000000000000000000
      000013130C6FFDFDFAFFFAFAF7FFF9F9F5FFF8F8F4FFF8F8F3FFF7F7F1FFF6F6
      F0FFF5F5EEFFF4F4EDFFF3F3EBFFF2F2E8FFF0F0E5FFEEEEE2FFF6F6E9FF1313
      0C6F00000000000000000000000000000000000000000000000075CADDFF82E4
      F3FF82E3F2FF81E2F2FF81E2F2FF8DE4F2FFB0EEF6FF5997B0FF68BEE3FF6EC9
      EFFF54D1A5FFBEF5CAFF7ADD99FF2BC458FF1DAF3CFF39BB53FF9AF5AFFF50C4
      56FF009A00FF009900FF00210080000000000000000000000000C36433FFDD78
      45FFD66F38FFD77138FFD87137FFD76C2FFFD76A2CFFD86C2DFFDA6F2FFFDB71
      30FFDC7332FFDD7533FFDD7534FFDD7634FFDD7533FFE08141FFE69458FFDD8A
      4FFFAF5D2DEF1B0E066000000000000000000D0D63BF8080DCFF4343C6FF3939
      C3FF1818AFFF0000000000000000010106300A0AA1EF0606B8FF0808BAFF0909
      BBFF0A0AA6EF0000063000000000000000000000000000000000000000001B1B
      ACFF7C7CDFFF1212C5FF0D0DB6FF0F0F60BF0000000000000000000000000000
      000013130C6EFDFDF9FFF9F9F5FFF8F8F4FFF8F8F3FFF7F7F1FFF6F6F0FFF5F5
      EEFFF4F4EDFFF3F3EBFFF2F2E8FFF0F0E5FFEEEEE2FFECECDEFFF5F5E7FF1313
      0C6E00000000000000000000000000000000000000000000000075CADDFF83E6
      F5FF83E5F4FF82E5F4FF82E4F3FF8EE7F4FFB2F0F7FF5997B0FF6AC2E7FF6FCB
      F3FF74D4FDFF54D2A6FFB4F5C3FF71DA8EFF24BD4AFF319661FF0F341B8087EC
      9CFF25AC26FF009900FF00210080000000000000000000000000C26333FFDC79
      47FFD56D37FFD66F38FFD77138FFD8733AFFD9743AFFD97135FFD96F31FFD96D
      2EFFD96E2EFFDA6F2FFFDA7030FFDC7535FFE28649FFE0894EFFCD713AFF5C2F
      15AF030100200000000000000000000000000707378F6B6BD4FF4A4AC7FF3F3F
      C6FF1C1CB2FF02021150020206302F2FACEF3434C4FF2A2AC2FF1919BEFF0C0C
      A4EF000006300000000000000000000000000000000000000000020210503232
      B7FF5C5CD7FF0F0FC2FF0E0EAEFF0808358F0000000000000000000000000000
      000014140C6DFCFCF8FFF8F8F4FFF8F8F3FFF7F7F1FFF6F6F0FFF5F5EEFFF4F4
      EDFFF3F3EBFFF2F2E8FFF0F0E5FFEEEEE2FFECECDEFFEAEADCFFF4F4E5FF1414
      0C6D00000000000000000000000000000000000000000000000075CADDFF84E8
      F6FF84E8F6FF83E7F5FF83E6F5FF8FE9F5FFB5F1F7FF5997B0FF6CC6ECFF70CE
      F6FF74D5FFFF74D5FFFF52D2A5FFA2F5B7FF65D57CFF319661FF000000001D72
      37BF65CC66FF009900FF00210080000000000000000000000000C16334FFDC7A
      49FFD46B36FFD56D37FFD66F38FFD77038FFD87239FFD9733AFFDA753BFFDA76
      3CFFDA753AFFDB7539FFDF7D42FFE48A50FFD2743DFF954C24DF0C0603400000
      000000000000000000000000000000000000020211504D4DC8FF5C5CCDFF4141
      C6FF2B2BBBFF151576CF3232ACEF3737C4FF3535C4FF3434C4FF2C2CADEF0101
      06300000000000000000000000000000000000000000000000000E0E61BF6868
      D0FF3535CBFF1F1FC2FF1818ADFF020210500000000000000000000000000000
      000014140D6CFCFCF7FFF8F8F3FFF7F7F1FFF6F6F0FFF5F5EEFFF4F4EDFFF3F3
      EBFFF2F2E8FFF0F0E5FFEEEEE2FFECECDEFFEAEADCFFE8E8D9FFF3F3E3FF1414
      0D6C00000000000000000000000000000000000000000000000075CADDFF85EA
      F7FF85EAF7FF85E9F7FF84E9F6FF91EBF7FFB7F3F8FF5997B0FF6EC8F0FF71CF
      F8FF74D5FFFF74D5FFFF74D5FFFF50D2A4FF74E48FFF36966BFF000000000D33
      1A8058C659FF009600FF00120060000000000000000000000000BF6234FFDC7B
      4BFFD36A35FFD46B36FFD56D37FFD66E37FFD76F38FFD77038FFD87239FFD973
      3AFFDD7941FFE2854DFFDA7E48FFAF5C2EEF30180B8000000000000000000000
      000000000000000000000000000000000000000000002222A1EF7C7CDAFF4545
      C7FF3E3EC5FF3C3CC4FF3B3BC5FF3939C4FF3737C4FF2F2FACEF010106300000
      00000000000000000000000000000000000000000000050521703434B8FF6969
      D7FF2E2EC7FF2424BAFF161697EF000000000000000000000000000000000000
      000014140C6BFBFBF6FFF7F7F1FFF6F6F0FFF5F5EEFFF4F4EDFFF3F3EBFFF2F2
      E8FFF0F0E5FFEEEEE2FFECECDEFFEAEADCFFE8E8D9FFE7E7D7FFF3F3E2FF1414
      0C6B00000000000000000000000000000000000000000000000075CADDFF86ED
      F9FF86ECF8FF85EBF8FF85EBF8FF92EDF8FFBAF4F9FF5997B0FF6FCBF3FF71D1
      FAFF74D5FFFF74D5FFFF74D5FFFF74D5FFFFA8EFE7FF5997B0FF000000002251
      2C9F25AC26FF009001FF00020020000000000000000000000000BE6234FFDB7C
      4DFFD26935FFD36A35FFD46A36FFD56C36FFD56D37FFD66E37FFD8713AFFDE79
      43FFDF7C47FFCB6C38FF592D15AF030100200000000000000000000000000000
      0000000000000000000000000000000000000000000006062C806363D2FF5E5E
      CDFF4242C7FF4040C6FF3E3EC6FF3B3BC5FF3232ACEF02020630000000000000
      000000000000000000000000000000000000040421701A1AAFFF4D4DCBFF2F2F
      C5FF2E2EC4FF1C1CADFF06062B80000000000000000000000000000000000000
      000014140D6AFBFBF5FFF6F6F0FFF5F5EEFFF4F4EDFFF3F3EBFFF2F2E8FFF0F0
      E5FFEEEEE2FFECECDEFFEAEADCFFE8E8D8FFE3E3D3FFDFDFCEFFE2E2D1FF1414
      0D6A00000000000000000000000000000000000000000000000075CADDFF87EE
      FAFF87EEFAFF86EEF9FF86EDF9FF94EFF9FFBDF6FAFF5997B0FF70CDF6FF72D1
      FBFF74D5FFFF74D5FFFF74D5FFFF74D5FFFFBCF6FAFF5997B0FF000000001383
      1BDF009801FF002A018F00000000000000000000000000000000BD6135FFDB7D
      50FFD26734FFD26835FFD36935FFD36A35FFD56C37FFDA733FFFDF7643FFCF6C
      39FF914922DF0B06024000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000001018188ADF8282
      DDFF5252CAFF4242C7FF4040C6FF3C3CC4FF151576CF01010B40000000000000
      0000000000000000000001010B400E0E61BF1C1CB1FF2D2DC0FF3131C5FF3131
      C5FF2424B6FF141484DF00000010000000000000000000000000000000000000
      000014140D69FAFAF4FFF5F5EEFFF4F4EDFFF3F3EBFFF2F2E8FFF0F0E5FFEEEE
      E2FFECECDEFFEAEADCFFE8E8D9FF9C9C8BFF9C9C8BFF9C9C8BFF9C9C8BFF1010
      077E00000000000000000000000000000000000000000000000075CADDFF88F0
      FBFF87F0FBFF87F0FBFF87EFFBFF95F1FBFFBFF8FBFF5997B0FF72D1FAFF73D4
      FEFF74D5FFFF74D5FFFF74D5FFFF74D5FFFFBFF7FBFF5A99B2FF023B059F07A0
      10FF015C04CF0000000000000000000000000000000000000000BB6135FFDB7E
      51FFD16634FFD16734FFD26834FFD7713FFFDE7B4BFFD47141FFA8542AEF2E17
      0B80000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000006302F2F
      BCFF7C7CDAFF5252CAFF4242C7FF3E3EC5FF2B2BBBFF1A1AB0FF15159AEF0E0E
      62BF0E0E62BF15159AEF1C1CB1FF2727B9FF3333C3FF3434C4FF3434C4FF3939
      C3FF1C1CAFFF0000063000000000000000000000000000000000000000000000
      000014140D68FAFAF3FFF4F4EDFFF3F3EBFFF2F2E8FFF0F0E5FFEEEEE2FFECEC
      DEFFEAEADCFFE8E8D9FFE7E7D7FFBBBBAAFFFFFFFFFFFFFFFFFF14140D680202
      012500000000000000000000000000000000000000000000000075CADDFF88F2
      FCFF88F2FCFF88F1FCFF88F1FCFFA5F5FCFFC2FAFCFF5997B0FF74D5FFFF74D5
      FFFF74D5FFFF74D5FFFF74D5FFFF79D7FFFFC2F9FCFF4F9F9CFF0C9C19FF012D
      038F000000000000000000000000000000000000000000000000BA6035FFDA7E
      53FFD16634FFD36B3BFFDB7B4DFFDB7D50FFC46637FF663117BF020100200000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000303
      19602F2FBCFF8383DDFF5F5FCEFF4646C7FF4141C6FF3F3FC6FF3737C1FF3333
      C0FF3232C0FF3333C0FF3939C4FF3838C4FF3737C4FF3E3EC7FF6767D6FF2E2E
      B9FF030318600000000000000000000000000000000000000000000000000000
      000014140D68FAFAF2FFF3F3EBFFF2F2E8FFF0F0E5FFEEEEE2FFECECDEFFEAEA
      DCFFE8E8D9FFE7E7D7FFE6E6D5FFCBCBBAFFFFFFFFFF14140D68020201250000
      000000000000000000000000000000000000000000000000000075CADDFF89F4
      FDFF8DF4FDFF9FF6FDFFBEFAFDFFC0F7FAFF9BD8E3FF5B9CB6FF74D5FFFF74D5
      FFFF89DFFFFF89DFFFFF9DE8FEFFB6F4FDFFC5FBFDFF64A9C5FF000200200000
      0000000000000000000000000000000000000000000000000000B95F35FFDB83
      57FFD77649FFDD865BFFCA6F43FF8A4321DF1108045000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000006301C1C9FEF6464D2FF7D7DDAFF5E5ECDFF4B4BC7FF4343C6FF3F3F
      C6FF3E3EC5FF3C3CC5FF4040C6FF4B4BCAFF6868D5FF5F5FCFFF1C1C9DEF0000
      0630000000000000000000000000000000000000000000000000000000000000
      000014140D67FCFCF4FFF9F9F0FFF8F8EEFFF7F7EBFFF6F6E9FFF5F5E7FFF4F4
      E5FFF3F3E3FFF3F3E2FFF2F2E1FFD9D9C8FF14140D6702020125000000000000
      000000000000000000000000000000000000000000000000000075CADDFFA9F9
      FEFFC4FCFEFFC3FAFCFF9FE3EDFF79CCDEFF8ED1DFFFB6EEF5FFC8FDFEFFC8FD
      FEFFC8FDFEFFC8FDFEFFB6EFF5FFB0EAF2FF99D8E7FF447487CF000000000000
      0000000000000000000000000000000000000000000000000000B75E35FFDE8B
      61FFD27A51FFB75C31FF2C150A80000000100000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000001006062C802222A1EF5D5DCFFF7575D8FF8282DDFF7A7A
      D9FF7979D9FF8080DCFF7171D7FF5B5BCEFF2323A1EF06062C80000000100000
      0000000000000000000000000000000000000000000000000000000000000000
      00000B0B074D14140D6614140D6614140D6614140D6614140D6614140D661414
      0D6614140D6614140D6614140D6614140D660202012400000000000000000000
      000000000000000000000000000000000000000000000000000075CADDFFC1F8
      FBFF96DEEAFF74C9DCFF7DCBDEFFA2E0ECFF9BD9E8FF9BD9E8FF83C6DCFF83C6
      DCFF6AB3D1FF6AB3D1FF46768ACF3B6475BF2137418F00000010000000000000
      000000000000000000000000000000000000000000000000000072361CCFB65E
      35FF602D15BF0201002000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000010030319600808449F0D0D63BF0D0D
      63BF0D0D63BF0D0D63BF0909449F030318600000001000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000599BA8DF74C7
      DCFF70BFD7FF5E9FB8EF3B6475BF315462AF1B2D34801B2D3480060B0D40060B
      0D40000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000060000000780000000100010000000000A00500000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000}
  end
  object IlSmall: TImageList
    ColorDepth = cd32Bit
    Height = 12
    Width = 12
    Left = 536
    Top = 88
    Bitmap = {
      494C010105000800F4000C000C00FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000300000001800000001002000000000000012
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000A9000000E30000
      00E3000000B00000000000000000000000B0000000E3000000E3000000A90000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000CF666666FF5252
      52FF030303E80000000000000000030303E8656565FF525151FF000000CF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000C74C4C4CFF3839
      39FF060606E40000000000000000060606E44B4B4BFF393838FF000000C70000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000C1727272FF5E5F
      5FFF080808E00000000000000000080808E0717171FF5E5E5EFF000000C10000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000BC717171FF494A
      4AFF0B0B0BDB0B0B0BDB0B0B0BDB0B0B0BDB6F6F6FFF4A4848FF000000BC0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000B77E7E7EFF6A6A
      6AFF323232FF6D6D6DFF6D6D6DFF323232FF7D7D7DFF6A6969FF000000B70000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000860D0D0DD13C3C
      3CFF0D0D0DD10D0D0DD10D0D0DD10D0D0DD13C3C3CFF0D0D0DD1000000860000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000AF7676
      76FF0D0D0DCA00000000000000000D0D0DCA767676FF000000AF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000810000
      00AC01010184000000000000000001010184000000AC00000081000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000E0E
      0E743D3D3DCA5E5E5ED9626262D7444444C61010107200000000000000000000
      0000000000010204075900000027000000000100003806010364060103650300
      0151000000020000000001040554000000290000000000000000000000000000
      00020502003812080065130900670603003E0000000400000000000000000000
      00000000001C1F10009406030046000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000154B4B
      4BF0646464FF6A6A6AFF6C6C6CFF686868FF4D4D4DED00000015000000000000
      0000000000003590B5DB47BADEF33E1624BAC7404CFFE06571FFE16772FFD656
      63FF62151ECE2E4961C756FBFFFF0102033C0000000000000000000000183C1D
      00ABB35A00FDD16800FFD36900FFB95D00FF472400B801000022000000000000
      00005D3300D3B86C00FF844F00FF0704004E0000000000000000000000000000
      00000000000000000000000000000000000000000000000000150706067F8B85
      85FBA79F9FFFA8A2A2FFA8A2A2FFA79F9FFF8B8585FA0706067F000000150000
      000000000000040C0F60C18BA9FFF56566FFEEC4C0FFE7E2DEFFE4D9D5FFEED4
      D0FFF58887FFE46F81FF334D60C7000000000000000000000014673400D8C464
      00FFD78C37FFE6B274FFE7B172FFD89140FFC66400FF7A3D00E5010000210000
      00000F09006DD98700FFB76C00FF804C00FF0704004F00000000000000000000
      000000000000000000000000000000000000000000270C0B0B88C4B4B4F5AE8E
      86FFDDAF21FFB58B49FFB58B49FFDDAF21FFAE8E86FFC4B4B4F50C0B0B880000
      0027000000002C090E9FFA6A6DFFE2E1DFFFE2FFFFFFDDFFFFFFCFF8F9FFE3FF
      FFFFDCF9F8FFF79793FF942830E40000001A000000002C15009AB05A00FFBA5E
      00FFC77822FFE6DFD8FFE9E9E9FFC9802FFFBC5E00FFB25B00FF3D1E00B10000
      000000000000190F0078DA8A00FFB76B00FF7D4A00FF463A2DA896887CD9A192
      82E57F7368C8090806390000000000000000000000764E4949CE95817BFF7051
      47FFEEBD08FFAB842AFFAB842AFFEEBD08FF705147FF95817BFF4E4949CE0000
      007600000027D75C68FFEBC4C2FFD7FFFFFFD2F0F2FFD4F1F3FFE8FFFFFFD6F7
      FAFFD6FFFFFFDCE9E8FFFE858DFF19090C850100001B793B00EEA65300FFB35D
      02FFB95800FFDEC5AFFFE4D6C9FFB95A00FFB25A00FFA55000FF8A4300F90301
      002D0000000000000000170F0078DF9000FFBE9F7CFFC0B7A8FFBBA979FFC5B5
      89FFB4A374FFC6C0BAFF3C32288F000000004E4E4EE2767171EE534845FF3224
      1FFFC9A007FF715815FF715815FFC9A007FF32241FFF534845FF767171EE4E4E
      4EE204010251F27D86FFD7DDDCFFC9F0F0FFCEE6E8FF889494FFB4CDCEFFDCFA
      FDFFCBF0F4FFC7F1F2FFFFAFB3FF3B191DAE07030041A26124FFB27133FFBA76
      35FFB97026FFEAD7C5FFF2E8DEFFBD762EFFB8732EFFA9621DFF98500AFF0E06
      005C000000000000000001000014755E46D9C0B496FFBFA131FFF5E7A2FFFCF5
      C9FFECD77DFFAB8C27FFBDB7B0FF13100C520101015A5A5959D39F9D9DFF3736
      36FF353434FF353434FF353434FF353434FF373636FF9F9D9DFF5A5959D30101
      015A04020253F68A90FFCED5D4FFB3E7E7FFEDFFFFFF7C8A8AFF6A7476FFE1FF
      FFFFC0E5EBFFB8E4E6FFFFB8BCFF3D1C20B00602003CAC713AFFBB824BFFB87B
      42FFCDA37AFFF9F4EFFFEEE0D3FFBB7D43FFBB814AFFBC834DFFB37A46FF0D06
      00570000000000000000271B1077ACA39BFDB5941FFFE9CE51FFE1CF7BFFE3D1
      84FFE1CD73FFDBB929FFA48934FF93877FD703030347575757C7D8D8D8FFCECE
      CEFFC3BFBFFFBBB2B2FFB7ADADFFBCB5B5FFCAC7C7FFD8D8D8FF575757C70303
      03470100002BE77071F8DCDEE2FFC7FFFFFF9BB1B2FF7A8687FFC8F3F5FFC7F1
      F7FFC0F7FFFFC4F2F7FFFFACAFFF2611128D00000014855629E8C6905EFFC08A
      56FFC49263FFD0AB89FFCEA784FFC08955FFC18B58FFC58F5CFF9D6A3CF40200
      002400000000000000003D2C1996BAB09CFFA78000FFDDC03FFFD8BE4FFFD9C0
      55FFD6BC49FFDFBD28FFA2820EFFA99C95ED0101012A2B2B2B94E7E8E8FCD2D2
      D2FFD6D4D4FFC7C0C0FFC2B9B9FFCBC5C5FFCCC9C9FFE7E8E8FC2B2B2B940101
      012A00000013A55F74EEFFB0AEFF9DC8D1FF729DA0FFBBECF2FFC0F1F5FFB7F5
      FEFFA0F1FBFFDBBDBFFFFB858DFF0200013F0000000026150786C99667FFCB98
      6AFFC58F5FFFEDDFD2FFF4ECE5FFC79364FFCA9769FFCB996AFF37220E9D0000
      00000000000000000000241A0E73B0A59EFCB4921BFFEACE51FFD7BE4EFFD8BE
      4FFFDBC356FFD8B629FFA38830FF928680D40000000604040436636363B8E0E1
      E1FFEFEFEFFFE8E7E7FFE6E5E5FFECEAEAFFDDDDDDFF636363B8040404360000
      00060926389965BFF7FFF2797EFFF29A9AFFB8D1DBFFA4D8E4FF94C3D0FFACD6
      E1FFD7B0B4FFFF8886FF9DA6D2FF0000000C000000000000000979614AC7D0A0
      76FFD1A177FFD9B18FFFDAB493FFD1A177FFD0A177FF8D7053D7010100130000
      000000000000000000000000000E403429A8DED1B0FFBD9F37FFF1E2A1FFF4E9
      B3FFE7D488FFB09334FFD1CCC5FF0E0B08490000000000000001070707405A5A
      5AB0D6D5D5F1EBE8E8FFE8E5E5FFD2CFCFF25E5E5EB507070740000000010000
      0000214558B473FFFFFF7EBED0FFEF9FA4FFFFA1A8FFE5B0B7FFE6B5B9FFF4A7
      AFFFFF999BFFA9A8B0FF71FFFFFF1D485DB30000000000000000010000104A42
      3B92C6A88DF3DAB494FFDAB493FFCCAD90F7584E45A002020218000000000000
      0000000000000000000000000000000000006E6054CDDCD4C4FFD3C49BFFD4C5
      A4FFD1C29AFFD7CFC6FF2921187F000000000000000000000000000000010505
      053444444498777777C5777777C5444444980505053400000001000000000000
      0000000101294E9CA9D85CE5E5F209151B72160909605731319F532F2D9F3116
      157D181D227E4CDDE5F20C1A1C58091218A80000000000000000000000000000
      00000303031F1615154D1717164F040404240000000000000000000000000000
      0000000000000000000000000000000000000000000017141162887D74CFA89B
      8CE06E645CBE0504032F0000000000000000424D3E000000000000003E000000
      2800000030000000180000000100010000000000C00000000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000}
  end
  object DlgFindPeriod: TTaskDialog
    Buttons = <>
    Caption = 'Period Search'
    CommonButtons = [tcbCancel]
    DefaultButton = tcbCancel
    Flags = [tfAllowDialogCancellation, tfShowMarqueeProgressBar, tfCallbackTimer]
    FooterIcon = 3
    RadioButtons = <>
    Text = 'This is text'
    Title = 'A period search is performed. Please wait.'
    OnTimer = DlgFindPeriodTimer
    Left = 456
    Top = 144
  end
  object DlgSaveFractal: TFileSaveDialog
    DefaultExtension = '.nbr'
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'Nanobrot Files (*.nbr)'
        FileMask = '*.nbr'
      end
      item
        DisplayName = 'All Files (*.*)'
        FileMask = '*.*'
      end>
    Options = []
    OnExecute = DlgSaveFractalExecute
    OnFileOkClick = DlgSaveFractalFileOkClick
    Left = 372
    Top = 144
  end
  object IlMain16: TImageList
    ColorDepth = cd32Bit
    DrawingStyle = dsTransparent
    Left = 457
    Top = 88
    Bitmap = {
      494C0101210060005C0110001000FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000009000000001002000000000000090
      0000000000000000000000000000000000000000000000000023000000330000
      0033000000330000003300000033000000330000002300000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000003F4F58C07B9AAAFF809B
      AAFF829BA9FF829BA9FF809BAAFF7A9AAAFF3D4F5ABF00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007C9BABFFBFD4E2FF2066
      94FF3EAAE2FF3EAAE2FF206694FFBFD4E3FF779CB6FF00000033000000330000
      0033000000330000003300000033000000230000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007E9CADFFC7D9E4FF2269
      9FFF43ABDFFF43ABDFFF22699FFFC5D9E6FF769BB5FFBC7A00FFB67A06FFB57A
      08FFB57A08FFB57B08FFB67D0DFF60440CC00000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000849FADFFCBDEE8FF1F6B
      A9FF42ABDEFF42ABDEFF1F6BA9FFC9DDE8FF7798ACFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFB67D0DFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000338DA3B0FFD0E3ECFF5E98
      C7FF1D6DB4FF1D6DB4FF5E98C7FFCEE1EBFF7E9BABFFFFFFFFFFF0D9B7FFEDD9
      BAFFECD8B9FFEAD6B7FFFFFFFFFFB57A08FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000002C74A8FF4D809FFFD8EAF1FFD2E6
      F0FFD2EAF5FFD2EAF5FFD2E6F0FFD5E8F0FF3F779BFF075FA1FFF2D6ABFFD0AB
      7EFFCEAA7EFFE8D0A9FFFFFFFFFFB47A08FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000002972ACFF679BBDFFDAEF
      FCFFC3610DFFC3610DFFD9EEFBFF5F96BBFF1064A5FFFFFAF8FFEBCF9DFFE8D1
      A4FFE7D0A4FFE4CB9CFFFFFFFFFFB47A08FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000002472B2FF669F
      C8FFE1F7FFFFE1F7FFFF619BC5FF1064A9FFFAF1EBFFEDECEEFFE4C68EFFC9A2
      6CFFC8A26CFFE1C48EFFFFFFFFFFB47A08FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000001C72
      C0FF6AA3D2FF68A1CDFF1166AFFFF2E9E1FFE5E2E0FFE0E1E5FFDFBF7CFFDFC2
      85FFDFC285FFDDBD7CFFFFFFFFFFB57A08FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000CB84
      00FF116EC2FF0E67B6FFE8DFD8FFDCD8D5FFD6D5D5FFD4D6DBFFD9B365FFD9B5
      6BFFD8B56AFFD7B365FFFFFFFFFFB57A08FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000BC7E
      09FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFB57B0AFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B67E
      0EFFF9E2BFFFE9B96FFFE9BE78FFE8BE78FFE8BD78FFE8BD78FFE7BD78FFE7BD
      78FFE7BC76FFE6B66DFFF7E0BEFFB57D0EFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B680
      12FFF2D8AEFFCD8407FFD08A12FFD38F1CFFD49323FFD6962AFFD99A33FFDB9E
      3AFFDCA241FFDFA648FFF1D7ACFFB68012FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B882
      16FFEED09CFFECCE99FFECCF9AFFECCE9AFFECCE99FFECCE99FFEBCD98FFEBCD
      98FFEBCD97FFEBCD96FFEECF9AFFB88216FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000583F
      0DB0B88216FFB78115FFB78115FFB78115FFB78115FFB78114FFB78114FFB681
      14FFB68114FFB78114FFB88216FF583F0DB00000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000226
      0099034200CC0226009900000000000000000000000000000000000000000000
      00000F100E4626341C8D305615C73E7C1AF03E7C1AF0305615C726341C8D0F10
      0E46000000000000000000000000000000000000000000000000000000000000
      00000000000000000000130A005C613101CC130A005C00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000002A000000780000002A00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000753
      00CC2ADF19FF075300CC00000000000000000000000000000000030303202131
      168C407F1BF3418B11FF3A8D00FF3E7B00FF3E7B00FF407D07FF428412FF407F
      1BF32131168C0303032000000000000000000000000000000000000000000000
      000000000000140A005C633303CCFFBB19FF633303CC140A005C000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000002A01010178EFEFEFFF010101780000002A000000000000
      0000000000000000000000000000000000000000000000000000000000000002
      053500162C93002346B9002850C500274FC4001E3CAB05481DCB096608DF095D
      02D23CE22BFF085900CC085900CC043200990000000003030320284316A34097
      15FF3B8800FF3B8800FF3A8D00FF397000FF397000FF397000FF397000FF3E7B
      00FF448717FF2A3F19A303030320000000000000000000000000000000000000
      0000150B015C683706CCFEBF27FFFEB101FFFEBC21FF683706CC150B015C0000
      0000000000000000000000000000000000000000002603030366050505750505
      05750505057505050575F0F0F0FFE1E1E1FFF0F0F0FF05050575050505750505
      0575050505750303036600000026000000000000000000000000000000000021
      44AE49789FE181B2D9F698CCF1FF90C2E9FC76A8CDF21F9225FA51E740FF51E7
      40FF51E740FF51E740FF51E740FF0A5E00CC000000002035168C409715FF3991
      00FF3A8D00FF3A8D00FFCCDBDDFFCCDBDDFFCCDBDDFFCCDBDDFF397000FF3E7B
      00FF3E7B00FF428412FF2131168C00000000000000000000000000000000150C
      015C6B3A08CCF9C03DFFF5AB0DFFF5AB0DFFF5AB0DFFF8BA30FF6B3A08CC150C
      015C0000000000000000000000000000000007070762929292D7EBEBEBFFEAEA
      EAFFEAEAEAFFEAEAEAFFE3E3E3FFE3E3E3FFE3E3E3FFEAEAEAFFEAEAEAFFEAEA
      EAFFEBEBEBFF929292D707070762000000000000000000000000000000000021
      42AB8ABBDFF88EC1EDFF8EC1ECFF92C5EFFF96C9F1FF46AE60FF2BA62FFF2BA4
      2FFE65EB54FF0F8316F00D7209DE063800990F110E463A9116F3399600FF3996
      00FF399100FF399100FF399100FFCEDEE1FFCCDBDDFF397000FF3C8300FF3E7B
      00FF3E7B00FF3E7B00FF3E7B15F30F100E460000000000000000160C025C703F
      0CCCF3C255FFE9A31DFFE9A31DFFE9A31DFFE9A31DFFE9A31DFFEFB742FF703F
      0CCC160C025C0000000000000000000000000D0D0D6FEDEDEDFFE6E6E6FFE6E6
      E6FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6
      E6FFE6E6E6FFEDEDEDFF0D0D0D6F000000000000000000000000000000000010
      207A214A71CC95C6EDFD91C4EEFF75A7CDF16898BEEC71A3CBF190C3EEFF2BA9
      2EFF74EE63FF2BA82DFD092D50AE0004083926391D8D3D9E10FF399A02FF399A
      02FF399600FF399600FF399100FFD0E0E3FFCEDEE1FF397000FF3B8800FF3C83
      00FF3E7B00FF3E7B00FF407D07FF26341C8D00000000170E035C75440FCCF1C5
      6BFFE7B04BFFE6AF4AFFE1A43CFFDC9A2FFFE0A33AFFE4AB44FFE4AA43FFE9B5
      53FF75440FCC170E035C00000000000000000E0E0E6EEFEFEFFFE9E9E9FF8080
      80FF747474FF6C6C6CFFC6C6C6FFE9E9E9FFE9E9E9FF6D6D6DFF5B5B5BFF5B5B
      5BFFE9E9E9FFEFEFEFFF0E0E0E6E000000000000000000000000000000000000
      00080115298B255077D19DD0F5FF0A2F54BC00000000082E53BC93C6EFFF42B0
      5CFF2DA822FF4B9016FF576978E1011830892C681FC73FA115FF3D9E10FF3D9E
      10FF3B9C09FF399A02FF399600FFD2E2E5FFD0E0E3FF397000FF3B8B00FF3B88
      00FF3C8300FF3E7B00FF3E7B00FF305312C70000000043280A99794913CC7949
      13CC794913CC794913CCDEAB57FFD1943DFFDDA954FF794913CC794913CC7949
      13CC794913CC43280A9900000000000000000F0F0F6DF1F1F1FFECECECFF6A6A
      6AFFECECECFF6A6A6AFFECECECFFECECECFFECECECFF949494FFC0C0C0FFECEC
      ECFFECECECFFF1F1F1FF0F0F0F6D000000000003073C01203FAC011F3DAA000C
      186B0003073C01274CBD97CAF2FF3D6A94E00A325ACA3A6893E08BBEEAFF86B9
      E7FFB88551FFBE8B57FFA87441FF253647BE3E962BF043A522FF43A522FF40A3
      1BFF3FA115FF3D9E10FF399A02FFD2E2E5FFD2E2E5FF397000FF3A8D00FF3B8B
      00FF3B8800FF3C8300FF3E7B00FF3B7410F00000000000000000000000000000
      0000000000007E4E17CCDDAD60FFCC9043FFDCA95CFF7E4E17CC000000000000
      0000000000000000000000000000000000001010106CF3F3F3FFEFEFEFFF9898
      98FF8A8A8AFF7A7A7AFFEFEFEFFF7A7A7AFFEFEFEFFFDEDEDEFF818181FFE4E4
      E4FFEFEFEFFFF3F3F3FF1010106C00000000011C37985685ABE383B6DEF83E6A
      92D9082B4EB02E587FCE8FC1EDFF89BCE9FF8EC1EDFF89BCE9FF83B6E5FF83B6
      E5FFCE9B67FFF7C491FFF6C390FFB5824EFF40962DF048AA33FF46AA30FF44A7
      29FF43A522FF40A31BFF399A02FFD5E4E7FFD2E2E5FF397000FF399100FF3A8D
      00FF3B8800FF3C8300FF3E7B00FF376D00F00000000000000000000000000000
      00000000000083521BCCF1C174FFDFA356FFE4B366FF83521BCC000000000000
      0000000000000000000000000000000000001111116BF6F6F6FFF2F2F2FFF2F2
      F2FFF2F2F2FF8B8B8BFFF2F2F2FFF2F2F2FFF2F2F2FFF2F2F2FFCCCCCCFFB5B5
      B5FFF2F2F2FFF6F6F6FF1111116B00000000011B378D8DC0EAFC80B3E4FF87BA
      E8FF8EC1EDFF8CBFECFF80B3E4FF80B3E4FF80B3E4FF80B3E4FF80B3E4FF80B3
      E4FFA0AFBDFFD9A672FFD3A06CFF293849B4336D2FC74BAF41FF4CAE3EFF48AC
      38FF46AA30FF44A729FFDCE9ECFFD7E5E8FFD5E4E7FF399600FF399600FF3991
      00FF3B8B00FF3B8800FF3C8300FF2D5009C70000000000000000000000000000
      00000000000088571ECCF5C77AFFEEB265FFF5C679FF88571ECC000000000000
      0000000000000000000000000000000000001111116AF8F8F8FFF5F5F5FFB7B7
      B7FFA3A3A3FFA7A7A7FFF5F5F5FFF5F5F5FFF5F5F5FF979797FF979797FFA3A3
      A3FFF5F5F5FFF8F8F8FF1111116A0000000001172D80436F97D28ABDEAFF7DB1
      E2FF7DB1E2FF5B7CD3FF1515AFFF5879D0FF7DB1E2FF7DB1E2FF5AA6CCFF148C
      9DFF58A3C9FF7EB2E3FF5685ADDE001327772C3F2D8D50B552FF53B34DFF4DB1
      47FF4CAE3EFF48AA33FF48AA33FF48AA33FF397000FF397000FF399600FF3991
      00FF3A8D00FF3B8800FF3C8300FF29351D8D0000000000000000000000000000
      0000000000008C5B21CCF8CD81FFF2B96CFFF7CB7EFF8C5B21CC000000000000
      00000000000000000000000000000000000012121269FAFAFAFFF8F8F8FFF8F8
      F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8
      F8FFF8F8F8FFFAFAFAFF121212690000000000010220021C368B376288C98CBE
      EAFD7DB1E2FF2F2FC9FF3939CCFF1F1FB9FF597BD1FF7AAEE0FF28A0B1FF34A9
      BAFF1B93A4FF64AFD2FF1E4469B50008104D0F1110464BA754F353B85CFF50B5
      52FF4DB147FF4CAE3EFF4CAE3EFFCCDBDDFFCCDBDDFF397000FF399A02FF3996
      00FF399100FF3B8B00FF367C00F30F100E460000000000000000000000000000
      000000000000905F24CCFCD589FFF8C77AFFFCD488FF905F24CC000000000000
      0000000000000000000000000000000000000E0E0E5B9F9F9FD4FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFBFBFBFFFBFBFBFFFBFBFBFFFCFCFCFFFCFCFCFFFCFC
      FCFFFCFCFCFF9F9F9FD40E0E0E5B00000000000000000001011A001428760F31
      52A35483ABDC4545DFFF7D7DF9FF7B7BF8FF2C2CC6FF78ACDFFF38B0C1FF7BE5
      F6FF7AE4F5FF269EAFFF011A34870000000F000000002A3E2F8C56BB66FF53B8
      5CFF50B552FF4DB147FF53B34DFFDCE9ECFFD9E7EAFF3B9C09FF399A02FF3996
      00FF399100FF3A8D00FF2435188C000000000000000000000000000000000000
      000000000000926227CCFFE094FFFEDB8FFFFFDF93FF926227CC000000000000
      000000000000000000000000000000000000020202220E0E0E5A131313671313
      13671313136713131367FEFEFEFFFDFDFDFFFEFEFEFF13131367131313671313
      1367131313670E0E0E5A02020222000000000000000000000000000000030005
      0B3E00172F7F142F6CB75050EAFF4A4AE4FF6C8DDDFA82B5E1FA60A8CBF340B8
      C9FF3CB4C5FF0A3956AC00010322000000000000000003030320315339A356BB
      66FF53B85CFF50B552FF53B34DFF48AC38FF48AC38FF48A523FF3B9C09FF3996
      00FF399100FF284316A303030320000000000000000000000000000000000000
      00000000000054381699956428CC956428CC956428CC54381699000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000202022414141467FFFFFFFF1414146702020224000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000040003062F000C185B00162B7A011C388A011C398B00172E7E0012
      236E00070E460000000B00000000000000000000000000000000030303202A3E
      2F8C4EA95DF350B552FF4DB147FF48AC38FF44A729FF40A31BFF3B9C09FF3488
      00F32435188C0303032000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000002020224141414660202022400000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000F1110462C3F2D8D3C7239C7409832F03C9425F0356A22C7283B1F8D0F11
      0E460000000000000000000000000000000000000000000000001B1B1B964F4F
      4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F
      4FFF4F4F4FFF4F4F4FFF4F4F4FFF1B1B1B9600000000000000001B1B1B964F4F
      4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF805E45FFC775
      37FFC77537FFC77537FF805E45FF1B1B1B960000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000535353FFFFFF
      FFFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
      FDFFFDFDFDFFFDFDFDFFFFFFFFFF535353FF0000000000000000535353FFFFFF
      FFFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFE7C7AEFFCA7A3DFFCA7A
      3DFFDAA377FFCA7A3DFFCA7B3DFF826148FF622E1CD48F4229FF8F4229FF8F42
      29FF8F4229FF8F4229FF8F4229FF8F4229FF8F4229FF8F4229FF8F4229FF8F42
      29FF8F4229FF8F4229FF622E1CD4000000000000000000000000000000000000
      000000000000361C0199613101CC613101CC613101CC361C0199000000000000
      0000000000000000000000000000000000002A11098E7C331AF2873A20FFBD90
      80FFF3F3F3FFF3F3F3FFF2F2F2FFF2F2F2FFF2F2F2FFF1F1F1FFF1F1F1FFF0F0
      F0FFF0F0F0FFEFEFEFFFFDFDFDFF575757FF2A11098E7C331AF2873A20FFBD90
      80FFF3F3F3FFF3F3F3FFF2F2F2FFF2F2F2FFF2F2F2FFCE8347FFCF8447FFCF83
      46FFFFFFFFFFCE8347FFCE8347FFCF8347FF93472CFFFFFFFFFFFFFFFFFFFEFE
      FEFFFDFDFDFFFCFCFCFFFBFBFBFFF9F9F9FFF8F8F8FFF6F6F6FFF4F4F4FFF2F2
      F2FFF1F1F1FFEFEFEFFF93472CFF000000000000000000000000000000000000
      000000000000633303CCFFBA15FFFFB60BFFFFB810FF633303CC000000000000
      000000000000000000000000000000000000843C22F2DFDFDFFFC67334FF974B
      30FFBC9080FFF2F2F2FFF2F2F2FFF1F1F1FFF1F1F1FFF0F0F0FFF0F0F0FFEFEF
      EFFFEFEFEFFFEEEEEEFFFDFDFDFF5D5D5DFF843C22F2DFDFDFFFC67334FF974B
      30FFBC9080FFF2F2F2FFF2F2F2FFF1F1F1FFF1F1F1FFD48C51FFE1AF86FFFFFF
      FFFFFFFFFFFFFFFFFFFFE1AF87FFD48D51FF984D31FFFEFDFDFFFEFEFEFFFCFC
      FCFFFAFAFAFFF8F8F8FFF5F5F5FFF3F3F3FFF0F0F0FFEDEDEDFFEAEAEAFFE7E7
      E7FFE4E4E4FFEDEDEDFF984D31FF000000000000000000000000000000000000
      000000000000683706CCFEB815FFFEB101FFFEB711FF683706CC000000000000
      0000000000000000000000000000000000008B4629F2E7C1A2FFCE8244FFCE82
      44FF9F5637FFC19684FFF1F1F1FFF0F0F0FFF0F0F0FFEFEFEFFFEFEFEFFFEEEE
      EEFFEEEEEEFFEDEDEDFFFDFDFDFF626262FF8B4629F2E7C1A2FFCE8244FFCE82
      44FF9F5637FFC19684FFF1F1F1FFF0F0F0FFF0F0F0FFD8965CFFD8965DFFD995
      5DFFFFFFFFFFD8955CFFD8965DFFD8955CFF9D5336FFFCFBFBFFFCFCFCFFFAFA
      FAFFF8F8F8FFF5F5F5FFF3F3F3FFF0F0F0FFEDEDEDFFEAEAEAFFE7E7E7FFE4E4
      E4FFE1E1E1FFECECECFF9D5336FF000000000000000000000000000000000000
      0000000000006B3A08CCF7B524FFF5AB0DFFF7B320FF6B3A08CC000000000000
      000000000000000000000000000000000000331C118E965132F2EBC8ABFFD691
      55FFD69155FFAA6240FFC79D89FFEFEFEFFFEFEFEFFFEEEEEEFFEEEEEEFFEDED
      EDFFEDEDEDFFECECECFFFDFDFDFF696969FF331C118E965132F2EBC8ABFFD691
      55FFD69155FFAA6240FFC79D89FFEFEFEFFFEFEFEFFFDEBEA5FFDD9D65FFDD9D
      65FFE7BB94FFDD9E66FFDD9E66FF8F6E55FFA35A3BFFFCFAFAFFFAFAFAFFF8F8
      F8FFF5F5F5FFF3F3F3FFF0F0F0FFEDEDEDFFEAEAEAFFE7E7E7FFE4E4E4FFE1E1
      E1FFDFDFDFFFEAEAEAFFA35A3BFF000000000000000000000000000000000000
      000000000000703F0CCCEEB136FFE9A31DFFEDAF32FF703F0CCC000000000000
      00000000000000000000000000000000000000000000351F138EAC6641FFEFCF
      B3FFDE9F65FFEFCFB3FFAC6741FFA4A4A4FFEEEEEEFFEDEDEDFFEDEDEDFFECEC
      ECFFEBEBEBFFEBEBEBFFFDFDFDFF6F6F6FFF00000000351F138EAC6641FFEFCF
      B3FFDE9F65FFEFCFB3FFAC6741FFA4A4A4FFEEEEEEFFEDEDEDFFDEBDA5FFE0A3
      6CFFE0A36CFFE0A36CFFE7C7AEFF6F6F6FFFA96240FFFBFAF9FFF8F8F8FFF5F5
      F5FFF3F3F3FFF0F0F0FFEDEDEDFFEAEAEAFFE7E7E7FFE4E4E4FFE1E1E1FFDFDF
      DFFFDCDCDCFFE9E9E9FFA96240FF000000000000000000000000000000000000
      00000000000075440FCCE5AD49FFDC9A2FFFE4AA45FF75440FCC000000000000
      00000000000000000000000000000000000000000000000000009B725CFFBB77
      50FFF1D4B8FFB9764FFFC49D86FF767676FF767676FF767676FFA7A7A7FFEBEB
      EBFFEAEAEAFFEAEAEAFFFDFDFDFF767676FF00000000000000009B725CFFBB77
      50FFF1D4B8FFB9764FFFC49D86FF767676FF767676FF767676FFA7A7A7FFEBEB
      EBFFEAEAEAFFEAEAEAFFFDFDFDFF767676FFAF6945FFFAF9F8FFF5F5F5FFF3F3
      F3FFF0F0F0FFEDEDEDFFEAEAEAFFE7E7E7FFE4E4E4FFE1E1E1FFDFDFDFFFDCDC
      DCFFDADADAFFE8E8E8FFAF6945FF000000000000000000000000000000000000
      000000000000794913CCDEAB57FFD1943DFFDDA954FF794913CC000000000000
      00000000000000000000000000000000000000000000000000007C7C7CFFD9B3
      9BFFBA774FFFD3AD94FFDFDFDFFFE1E1E1FFEFEFEFFFF2F2F2FF7E7E7EFFABAB
      ABFFE9E9E9FFE9E9E9FFFDFDFDFF7C7C7CFF00000000000000007C7C7CFFD9B3
      9BFFBA774FFFD3AD94FFDFDFDFFFE1E1E1FFEFEFEFFFF2F2F2FF7E7E7EFFABAB
      ABFFE9E9E9FFE9E9E9FFFDFDFDFF7C7C7CFFB46F4AFFF8F7F6FFF3F3F3FFF0F0
      F0FFEDEDEDFFEAEAEAFFE7E7E7FFE4E4E4FFE1E1E1FFDFDFDFFFDCDCDCFFDADA
      DAFFD8D8D8FFE6E6E6FFB46F4AFF0000000000000000472C0D997E4E17CC7E4E
      17CC7E4E17CC7E4E17CCDDAD60FFCC9043FFDCA95CFF7E4E17CC7E4E17CC7E4E
      17CC7E4E17CC472C0D9900000000000000000000000000000000848484FFFDFD
      FDFFB1B1B1FF878787FFEBEBEBFFF1F1F1FF878787FF878787FFFAFAFAFF8787
      87FFE8E8E8FFE7E7E7FFFDFDFDFF848484FF0000000000000000848484FFFDFD
      FDFFB1B1B1FF878787FFEBEBEBFFF1F1F1FF878787FF878787FFFAFAFAFF8787
      87FFE8E8E8FFE7E7E7FFFDFDFDFF848484FFB9754FFFF7F6F5FFF0F0F0FFEDED
      EDFFEAEAEAFFE7E7E7FFE4E4E4FFE1E1E1FFDFDFDFFFDCDCDCFFDADADAFFD8D8
      D8FFD6D6D6FFE6E6E6FFB9754FFF00000000000000001A10055C83521BCCFCDB
      8FFFF5C679FFF5C578FFEDB76AFFDFA356FFDEA75AFFE0AF62FFDDAB5EFFEBC3
      76FF83521BCC1A10055C000000000000000000000000000000008A8A8AFFFDFD
      FDFFEBEBEBFF8F8F8FFFF2F2F2FF8F8F8FFFB4B4B4FFB4B4B4FF8F8F8FFFBCBC
      BCFFFDFDFDFFFDFDFDFFFDFDFDFF8A8A8AFF00000000000000008A8A8AFFFDFD
      FDFFEBEBEBFF8F8F8FFFF2F2F2FF8F8F8FFFB4B4B4FFB4B4B4FF8F8F8FFFBCBC
      BCFFFDFDFDFFFDFDFDFFFDFDFDFF8A8A8AFFBD7A52FFF6F6F6FFF4F4F4FFF2F2
      F2FFF1F1F1FFEFEFEFFFEDEDEDFFECECECFFEAEAEAFFE9E9E9FFE8E8E8FFE6E6
      E6FFE6E6E6FFE6E6E6FFBD7A52FF0000000000000000000000001B11065C8857
      1ECCFDDE92FFEEB265FFEEB265FFEEB265FFEEB265FFEEB265FFFAD68AFF8857
      1ECC1B11065C0000000000000000000000000000000000000000909090FFFDFD
      FDFFEAEAEAFF969696FFF9F9F9FF969696FFB8B8B8FFE7E7E7FFFDFDFDFFE0E0
      E0FFE5E5E5FFEBEBEBFF909090FF313131960000000000000000909090FFFDFD
      FDFFEAEAEAFF969696FFF9F9F9FF969696FFB8B8B8FFE7E7E7FFFDFDFDFFE0E0
      E0FFE5E5E5FFEBEBEBFF909090FF31313196B97851FDC07E55FFC07E55FFC07E
      55FFC07E55FFC07E55FFC07E55FFC07E55FFC07E55FFC07E55FFC07E55FFC07E
      55FFC07E55FFC07E55FFBC7952FF000000000000000000000000000000001C12
      065C8C5B21CCFEE195FFF2B96CFFF2B96CFFF2B96CFFFDDD91FF8C5B21CC1C12
      065C000000000000000000000000000000000000000000000000959595FFFDFD
      FDFFE9E9E9FFBBBBBBFF9C9C9CFFFCFCFCFF9C9C9CFFE6E6E6FFFDFDFDFFE5E5
      E5FFEBEBEBFF959595FF33333396000000000000000000000000959595FFFDFD
      FDFFE9E9E9FFBBBBBBFF9C9C9CFFFCFCFCFF9C9C9CFFE6E6E6FFFDFDFDFFE5E5
      E5FFEBEBEBFF959595FF3333339600000000B5744EFBE7B891FFE8B992FFE8B9
      92FFE8B992FFE8B992FFE8B992FFE8B992FFE8B992FFE8B992FFE8B992FFE8B9
      92FFE8B992FFE8B992FFBB7850FF000000000000000000000000000000000000
      00001D13075C905F24CCFFE397FFF8C77AFFFFE296FF905F24CC1D13075C0000
      00000000000000000000000000000000000000000000000000009A9A9AFFFDFD
      FDFFE8E8E8FFE7E7E7FFBEBEBEFFA1A1A1FFBDBDBDFFE5E5E5FFFDFDFDFFEBEB
      EBFF9A9A9AFF35353596000000000000000000000000000000009A9A9AFFFDFD
      FDFFE8E8E8FFE7E7E7FFBEBEBEFFA1A1A1FFBDBDBDFFE5E5E5FFFDFDFDFFEBEB
      EBFF9A9A9AFF353535960000000000000000B2734EF7E7B790FFE7B891FFE7B8
      91FFE7B891FFE7B891FFE7B891FFE7B891FFE7B891FFE7B891FFF1DCCEFFE7B8
      91FFF1DCCEFFE7B891FFB87751FB000000000000000000000000000000000000
      0000000000001D14075C926227CCFFE599FF926227CC1D14075C000000000000
      00000000000000000000000000000000000000000000000000009E9E9EFFFDFD
      FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFF9E9E
      9EFF3636369600000000000000000000000000000000000000009E9E9EFFFDFD
      FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFF9E9E
      9EFF363636960000000000000000000000003B271A8EB47650F7BA7A53FBBA7A
      53FBBA7A53FBBA7A53FBBA7A53FBBA7A53FBBA7A53FBBA7A53FBBA7A53FBBA7A
      53FBBA7A53FBBA7A53FB3E291B92000000000000000000000000000000000000
      000000000000000000001E14085C956428CC1E14085C00000000000000000000
      000000000000000000000000000000000000000000000000000037373796A1A1
      A1FFA1A1A1FFA1A1A1FFA1A1A1FFA1A1A1FFA1A1A1FFA1A1A1FFA1A1A1FF3737
      379600000000000000000000000000000000000000000000000037373796A1A1
      A1FFA1A1A1FFA1A1A1FFA1A1A1FFA1A1A1FFA1A1A1FFA1A1A1FFA1A1A1FF3737
      3796000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000B252525B52222228A2222
      228A2222228A2222228A161616E3161616E3252525A900000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000B252525B52222228A2222228A2222
      228A2222228A161616E3161616E3252525A9070B0D5818272DA018272DA01827
      2DA018272DA018272DA018272DA018272DA018272DA018272DA018272DA01827
      2DA016242A9A0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000262626A5202020CFBBADADFF3232
      32FFB6A9A9FFBBADADFF414141FF5C5C5CFF202020CF00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000262626A5202020CFBBADADFF323232FFB6A9
      A9FFBBADADFF414141FF5C5C5CFF202020CF18272DA090CFE9FF90CFE9FF90CF
      E9FF90CFE9FF90CFE9FF90CFE9FF618FA1FF7BB2C9FF82B8D0F37AB0C6EE82BA
      D1F28CC8E1F918272DA000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000222222C7505050FFBDB7B7FF2121
      21FFB4AEAEFFBDB7B7FF3C3C3CFF595959FF222222C700000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000222222C7505050FFBDB7B7FF212121FFB4AE
      AEFFBDB7B7FF3C3C3CFF595959FF222222C718272DA091CFEAFF91D1EAFF91D1
      EAFF91D1EAFF93CFEAFF91D0E9FF618FA1FF7CB3CBFF94D4EEFF92D1EBFE8FA6
      ADFF93D2EBFE8FCCE4FC18272DA0000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000242424C1525252FFC7C6C6FFC3C2
      C2FFC3C2C2FFC7C6C6FF373737FF575757FF1A1A1ADE1E1E1E771E1E1E771E1E
      1E771E1E1E771E1E1E771E1E1E7714141459141414591E1E1E771E1E1E771E1E
      1E771E1E1E771E1E1E771E1E1E771D1D1DDE313131FF878686FFC3C2C2FFC3C2
      C2FFC7C6C6FF373737FF575757FF242424C118272DA094D2EDFF94D3EDFF94D3
      EDFF93D3EEFF94D3EDFF94D3EDFF618FA1FF7DB4CCFFA4BAC0FFE59464FF5B57
      54FF95D4EFFF90CDE9FD18272DA00000000000000006000000160000001A0000
      001A0000001A0000001A0000001A0000001A0000001A0000001A0000001A0000
      001A0000001A0000001A0000001600000006242424BC555555FF323232FF3232
      32FF323232FF323232FF333333FF565656FF3C3C3CFFE7E7E7FFE7E7E7FFE7E7
      E7FFE7E7E7FFE7E7E7FFEBEBEBFF2020207420202074EBEBEBFFE7E7E7FFE7E7
      E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFEBEBEBFF2A2A2AFF323232FF3232
      32FF323232FF333333FF565656FF242424BC18272DA095D6F1FF95D6F1FF95D6
      F1FF95D6F1FF95D6F1FF95D6F1FF898F84FFD49058FFDD8D57FFCD7C51FF607D
      89FF95D4EFFF92D0EBFE18272DA0000000000202025B05050575050505750505
      0575050505750505057505050575050505750505057505050575050505750505
      05750505057505050575050505750202025B252525B7767676FFD6D6CCFFF6F6
      E9FFF6F6E9FFF6F6E9FFD6D6CCFF767676FF404040FFE3E3E3FFE3E3E3FFE3E3
      E3FFE3E3E3FFE3E3E3FFEAEAEAFF2020206F2020206FEAEAEAFFE3E3E3FFE3E3
      E3FFE3E3E3FFE3E3E3FFE3E3E3FFE3E3E3FFEAEAEAFF979792FFF6F6E9FFF6F6
      E9FFF6F6E9FFD6D6CCFF767676FF252525B718272DA099DAF5FF99DAF5FF99DA
      F5FF99DAF5FF99DAF5FF99DAF5FFE8C197FFD6923DFFCF8C4EFFCD8452FFCB7D
      52FF664A3CFF8AC1D8FF18272DA0000000000B0B0B70ECECECFFF0F0F0FFF0F0
      F0FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFECECECFF0B0B0B70252525B27B7B7BFFFAFAF3FFF5F5
      EEFFF5F5EEFFF5F5EEFFFAFAF3FF7B7B7BFF454545FFE6E6E6FFE6E6E6FFE6E6
      E6FFE6E6E6FFE6E6E6FFEDEDEDFF2121216E2121216EEDEDEDFFE6E6E6FFE6E6
      E6FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6E6FFEDEDEDFFB0B0ACFFF5F5EEFFF5F5
      EEFFF5F5EEFFFAFAF3FF7B7B7BFF252525B218272DA09CDDF9FF9CDDF9FF9CDD
      F9FF9DDDF9FF9CDDF9FF9DDDF9FF7E9DA1FFF0CB95FFD9973DFFE09850FFDA8B
      4AFFDE8450FF7E5642FF1C2A2FAC000000000E0E0E6EEBEBEBFFB8B8B8FFBDBD
      BDFFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4
      E4FFE4E4E4FFE4E4E4FFEBEBEBFF0E0E0E6E262626AF848484FFFFFFFEFFFEFE
      FDFFFEFEFDFFFEFEFDFFFFFFFEFF848484FF494949FFE9E9E9FFE9E9E9FFE9E9
      E9FFE9E9E9FFE9E9E9FFEFEFEFFF2121216D2121216DEFEFEFFFE9E9E9FFE9E9
      E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFEFEFEFFFB6B6B5FFFEFEFDFFFEFE
      FDFFFEFEFDFFFFFFFEFF848484FF262626AF17242A9B9FE2FEFF9FE2FDFF9FE2
      FDFF9FE2FDFF9FE2FDFF9FE1FEFF618FA1FFCBD5C3FFF3BA64FF7E9575FFBDC8
      C7FFE7AD7AFFE18549FF3E3632ED000000000F0F0F6CEFEFEFFFEFEFEFFFEFEF
      EFFFEAEAEAFFEAEAEAFFEAEAEAFFEAEAEAFFEAEAEAFFEAEAEAFFEAEAEAFFEAEA
      EAFFEAEAEAFFEAEAEAFFEFEFEFFF0F0F0F6C20202081262626AC20201D662020
      1D6620201D6620201D6645453DA74E4E4EFF747474FFECECECFFBC8F61FFCFB4
      99FFECECECFFECECECFFF1F1F1FF2020206C2020206CF1F1F1FFECECECFFECEC
      ECFFECECECFFECECECFFECECECFFECECECFFF1F1F1FF42423EA720201D662020
      1D6620201D6620201D66262626AC2020208115222796A1E6FFFFA1E6FFFFA1E6
      FFFFA1E6FFFFA1E6FFFFA1E5FFFFA1E6FFFF6492A5FFEFD4A6FF79ADABFF9DDF
      FAFF666B64CBF6D3A3FFB3825CFF000000191111116BF4F4F4FFBEBEBEFFC5C5
      C5FFF0F0F0FFF0F0F0FFF0F0F0FFF0F0F0FFF0F0F0FFF0F0F0FFF0F0F0FFF0F0
      F0FFF0F0F0FFF0F0F0FFF4F4F4FF1111116B0000000000000000000000000000
      000000000000000000002121216BF3F3F3FFEFEFEFFFEFEFEFFFB67839FFB678
      39FFD6BB9EFFEFEFEFFFF3F3F3FF2121216B2121216BF3F3F3FFEFEFEFFFEFEF
      EFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFF3F3F3FF2121216B000000000000
      0000463626991C18155C0000000000000000121F248FA5E9FFFFA5E9FFFFA5E9
      FFFFA5E9FFFFA5E9FFFFA3E9FFFFA5E9FFFFA4E9FFFF7A9EA6FF9EDBF1FF9EE1
      FCFF121F248F2D261D70FDD79FFF0606065A1212126AF8F8F8FFF8F8F8FFF8F8
      F8FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FFF8F8F8FF1212126A0000000000000000000000000000
      000000000000000000006E5335C3C38644FFC28643FFC28643FFC28643FFFFC5
      37FFC28643FFDFC4A6FFF8F8F8FF2020206920202069F8F8F8FFF5F5F5FFF5F5
      F5FFF5F5F5FFF5F5F5FFF5F5F5FFF5F5F5FFF8F8F8FF20202069000000000000
      0000764F27CC764F27CC1D19155C00000000111B2087A6EDFFFFA7EDFFFFA7ED
      FFFFA7EDFFFFA7EDFFFFA6EDFFFFA7EDFFFFA6ECFFFF71A4B8FFA1E5FFFFA0E4
      FFFF111B20870807062E92836CC3110E0A7412121268FCFCFCFFC3C3C3FFCCCC
      CCFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFB
      FBFFFBFBFBFFFBFBFBFFFCFCFCFF121212680000000000000000000000000000
      00000000000000000000986933E1FFE392FFFFD569FFFFD15CFFFFD15CFFFFD1
      5CFFFFD872FFCE924CFFFAFAFAFF2121216921212169FAFAFAFFF8F8F8FFF8F8
      F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFD2A572FF905F2CE17D562CCC7D56
      2CCC7D562CCCFFC537FF7D562CCC1D1A165C0E181C7EA9EFFFFFA9F1FFFFA9F1
      FFFFAAEFFFFFA9EFFFFFAAF1FFFFAAF1FFFFA9EFFFFF77ADC2FFA2E7FFFFA1E5
      FFFF0E181C7E000000000B09073C0807054413131367FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF131313670000000000000000000000000000
      00000000000000000000785C3DC3DB9F56FFDB9E56FFDB9E56FFDB9E56FFFFE5
      97FFDB9E56FFECD1B1FFFCFCFCFF2020206820202068FCFCFCFFFBFBFBFFFBFB
      FBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFCF934DFFFFE392FFFFD569FFFFD1
      5CFFFFD15CFFFFD15CFFFFD872FF855E32CC0C141874AAF1FFFFAAF1FFFFAAF3
      FFFFAAF3FFFFAAF1FFFFAAF1FFFFAAF1FFFFAAF1FFFF7EB7CEFFA2E7FFFFA2E7
      FFFF0C1418740000000000000000000000000B0B0B4D14141466141414661414
      1466141414661414146614141466141414661414146614141466141414661414
      14661414146614141466141414660B0B0B4D0000000000000000000000000000
      0000000000000000000020202067FEFEFEFFFDFDFDFFFDFDFDFFE5A95EFFE5A9
      5EFFF2D7B6FFFDFDFDFFFEFEFEFF2020206720202067FEFEFEFFFDFDFDFFFDFD
      FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFE3B681FFA2743BE18C6537CC8C65
      37CC8C6537CCFFE597FF8C6537CC1E1B175C0A101469ABF3FFFFABF3FFFFABF3
      FFFFABF3FFFFABF3FFFFABF3FFFFABF3FFFFABF3FFFF86C0D8FFA2E7FFFFA2E7
      FFFF0A1014690000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000020202067FFFFFFFFFFFFFFFFFFFFFFFFF1C48BFFF7DC
      BAFFFFFFFFFFFFFFFFFFFFFFFFFF2020206720202067FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF20202067000000000000
      0000936D3DCC936D3DCC1F1C175C00000000080E0F5FABF3FFFFABF3FFFFABF3
      FFFFABF3FFFFABF3FFFFABF3FFFFABF3FFFFABF3FFFF8CC9E2FFA2E7FFFFA2E7
      FFFF080E0F5F0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001313134D202020662020206620202066202020662020
      20662020206620202066202020661313134D1313134D20202066202020662020
      206620202066202020662020206620202066202020661313134D000000000000
      0000574631991F1C185C00000000000000000102032B060A0C53060A0B52060A
      0B52060A0B52060A0B52060A0B52060A0C53060A0B52060A0B52060A0B52060A
      0B520102032B0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000606
      01650B0B03870B0B03870B0B03870B0B03870B0B03870B0B03870B0B03870B0B
      0387060601650000000000000000000000000000000000000000000000000000
      0000000000000B0B0B37343331775350479A5451489C3937347C0F0F0E3F0000
      0003000000000000000000000000000000000000000000000000000000110000
      0011000000110000001100000011000000110000001100000011000000110000
      0011000000110000001100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000E0E
      0682FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFEFDFFFEFEFDFFFDFDFBFFFEFE
      FBFF0E0E06820000000000000000000000000000000000000000000000000606
      0628635F56A6B09D74F29C8044FFB2A481FFB9AC8DFF997D43FFB29D72F6706B
      5FB20A0A0A350000000000000000000000000000000011155CBB0F2094FF0F20
      9CFF0F289CFF0F28A5FF0F28A5FF0F30A5FF0F30A5FF0F28A5FF0F289CFF0F28
      9CFF0F2094FF0F2094FF080D53BB0000000000000000222665C76C7ADAFC080A
      3DA2000000000000000000000000000000000000000000000000000000000000
      0000000000000000000200000000000000000303036304040485040404850101
      0188FFFFFFFFFF964AFFFFC79EFFD6C8A6FF628040FF87662FFFA87441FFFCFC
      F8FF0101018804040485040404850303036300000000000000000E0E0E3E9F93
      79E18E7337FF826425FF988557FFF0EFEBFFFAFAF9FF9E8D62FF846628FF8A6E
      30FFA89A79EC181817500000000000000000000000110F209CFF000094FF000F
      A5FF0018B5FF0028C5FF0030D6FF0039DEFF0039DEFF0030D6FF0028C5FF0018
      B5FF000FA5FF000094FF0F2094FF0000001100000A487983E6FF6980FFFF2338
      D3FC000000140000000000000000000000000000000000000000000000000000
      000001012B8B0001287A00000000000000000909097AC5BDBDFFC5BABAFF5858
      58FF585858FF585858FF585858FF585858FF585858FF585858FF585858FF5858
      58FF585858FFCCBBBBFFCCBBBBFF0909097A00000000040404229E9277E0C5BC
      A3FFD5D0C2FFD3CDBDFFDFDBD1FFF4F2EEFFF8F7F5FFDDD9CEFFD8D3C5FFD1CB
      BBFFCDC5B2FFAA9B7BED0909093200000000000000110F189CFF00079CFF000F
      ADFF0020BDFF3051D6FF8CA5E6FFB5C5E6FFB5C5E6FF8CA5E6FF3051D6FF0020
      BDFF000FADFF00079CFF0F1894FF00000011010215644C53DEFF2435EBFF091E
      D7FE0000011D0000000000000000000000000000000000000000000000000000
      0223060EB4F800000E4F00000000000000000A0A0A75C4C2C2FFB8B0B0FF6E6A
      6AFF968A8AFF958888FF968787FF968787FF968787FF978787FF978787FF9787
      87FF706868FFCCBBBBFFCCBBBBFF0A0A0A750000000056524D99917436FFBFB6
      9EFFFFFFFFFFEBE8E2FF978659FFBEB297FFC3B9A0FF968456FFDFDCD1FFFFFF
      FFFFCEC7B5FF8A6D2FFF706A5FB10000000500000011070F9CFF00079CFF0018
      ADFF7A94DEFFEFEFEFFFE6E6E6FFD6D6D6FFD6D6D6FFDEDEDEFFE6E6E6FF7A8C
      DEFF0018ADFF00079CFF07188CFF00000011000000161014B8FB050DDBFF0311
      D8FF01022686000000000000000000000000000000000000000000000000050B
      7ED9060FA4EE0000000000000000000000000A0A0A6DB8B8B8FFA5A5A5FF1F1F
      1FFF5F5353FF594C4CFF594C4CFF594C4CFF594C4CFF594C4CFF594C4CFF594C
      4CFF1F1F1FFF9D9D9DFFAFAFAFFF0A0A0A6D05050527AB9A79E9826322FFD0C9
      B7FFDAD5C8FFD6D0C0FFAA966CFFB4A178FFB5A27AFFA99569FFD1CAB7FFD8D3
      C5FFD9D4C6FF836527FFB6A275F70C0C0C3A00000011070F9CFF00079CFF6A72
      CEFFEFEFEFFFE6E6E6FF728CC5FF2859CEFF2859CEFF728CC5FFDEDEDEFFEFEF
      EFFF6A72CEFF00079CFF070F8CFF000000110000000000013EA60001CFFF0208
      D8FF030ABAFB0000022600000000000000000000000000000000010229880B20
      DFFF0001166500000000000000000000000007070758969696DDBEBEBEFF3030
      30FF5B5555FF4C4343FF4C4343FF4C4343FF4C4343FF4C4343FF4C4343FF4D45
      45FF303030FF65FF65FF887E7EDD0707075822222160A78F59FF917B4BFFE7E5
      DEFF99885BFFA79367FFD3C6ADFFFFFFFFFFFFFFFFFFDACFBBFFA89468FF9785
      58FFE2DFD6FF9D8B60FF9C8147FF343331770000001107079CFF070F9CFFE6E6
      EFFFEFEFEFFF496AC5FF0030D6FF0F49DEFF0F49DEFF0030D6FF4962C5FFEFEF
      EFFFDEDEEFFF070F9CFF070F8CFF000000110000000000000002010284E30000
      D0FF0005D5FF02057FDF00000000000000000000000000000E570B27E5FF0610
      8ADF0000000000000000000000000000000004040440616161BFC9C9C9FF4343
      43FF666565FF484545FF3F3B3BFF3E3A3AFF3E3A3AFF3E3A3AFF3E3A3AFF4743
      43FF434343FFBBB1B1FF534F4FBF040404403B393580B5A783FFF5F4F1FFF9F8
      F6FFCEC6B3FFAF9A6FFFFCFBFAFFFFFFFFFFFFFFFFFFFFFFFFFFB5A27AFFC6BC
      A4FFF7F5F2FFF7F6F4FFB8AB8BFF4D4B43950000001107079CFF4949ADFFEFEF
      EFFFC5C5DEFF0F30BDFF0F39C5FFC5CEDEFFC5CEDEFF0730C5FF0020B5FFBDC5
      DEFFEFEFEFFF4141ADFF07078CFF000000110000000000000000000001170001
      9AEF0000D1FF0002CEFF01023FA70000000000000C4F071BCEFC0A24DDFE0000
      032E0000000000000000000000000000000001010129393939A0DADADAFF5757
      57FF757575FF656565FF5E5E5EFF515050FF434242FF3B3A3AFF363535FF4948
      48FF575757FFBFB9B9FF333333A0010101293937337DB0A078FFE4E1D8FFF4F2
      EFFFC3B9A1FFAE996DFFFBFAF7FFFFFFFFFFFFFFFFFFFFFFFFFFB4A077FFBCB1
      95FFF0EEE9FFE9E6DFFFB0A17CFF4C484393000000112020A5FF8484C5FFEFEF
      EFFFB5B5DEFF4959C5FF4959CEFFDEDEE6FFDEDEE6FF4962CEFF5162C5FFB5B5
      DEFFEFEFEFFF7A7ABDFF070794FF000000110000000000000000000000000000
      011D010296EC0000D2FF0000C4FF01024CB40209BCFB0413E1FF01022F930000
      000000000000000000000000000000000000000000141E1E1E7FE8E8E8FF6767
      67FF838383FF7C7C7CFF7C7C7CFF7C7C7CFF7C7C7CFF7C7C7CFF7B7B7BFF8080
      80FF676767FFE4E4E4FF1D1D1D7F000000141E1D1C59AC935DFF89713CFFE7E5
      DEFF9C8B60FFA9966CFFCABB9DFFFAF8F4FFFAF8F5FFD1C3AAFFAA966CFF9988
      5CFFE4E1D9FF958151FF9F844AFF2E2D2B6F000000112828B5FF8484C5FFEFEF
      EFFFCECEE6FF5962BDFF5962C5FFE6E6E6FFDEE6E6FF5962C5FF5962BDFFCECE
      E6FFE6E6E6FF8484C5FF4949B5FF000000110000000000000000000000000000
      00000000000A020270D50000CCFF0000CCFF0002D1FF010364CB000000000000
      000000000000000000000000000000000000000000020707074D0D0D0D670101
      018D747474FF747474FF747474FF747474FF747474FF747474FF747474FF7474
      74FF0101018D0D0D0D670707074D000000020303031EA5967AE2836321FFC7BE
      A8FFE8E5DEFFDDD8CCFFA8966BFFAD986CFFAE996EFFA79367FFD7D1C1FFE7E4
      DBFFD2CBBAFF826323FFB2A07AF10808082F000000003030B5FF7272BDFFE6E6
      E6FFEFEFEFFF8484CEFF6A6ABDFFE6E6EFFFE6E6EFFF6A6ABDFF848CCEFFEFEF
      EFFFDEDEDEFF7272BDFF4949B5FF000000000000000000000000000000000000
      00000000011D02056FD00106CFFF0002D2FF0000C9FF0101298A000000000000
      0000000000000000000000000000000000000000000000000000000000001414
      0D6BFAFAF4FFF5F5EDFFF2F2EAFFEEEEE2FFE8E8D9FFE6E6D5FFE5E5D4FFF2F2
      E1FF14140D6B0000000000000000000000000000000042403C86977C41FFC6BE
      AAFFFFFFFFFFEBE9E3FF9B8A5FFFC6BEA6FFCCC5B1FF99885CFFE1DDD3FFFFFF
      FFFFD5D0C1FF907539FF5D5952A000000001000000003939BDFF7A7AC5FFADAD
      CEFFE6E6E6FFD6D6E6FF7A7AC5FFE6E6EFFFE6E6EFFF7A7AC5FFD6D6E6FFE6E6
      E6FFADADC5FF7A7AC5FF5151BDFF000000000000000000000000000000000203
      35920918C8FA0616E4FF040FDEFF0106A6F30103BCFC0507CCFF01023CA10000
      0000000000000000000000000000000000000000000000000000000000001414
      0D69FAFAF3FFF2F2EAFFEEEEE2FFE8E8D9FFE6E6D5FFA4A493FFA4A493FFA4A4
      93FF1010087C00000000000000000000000000000000010101158C8370CDBFB3
      96FFC2B9A3FFC5BCA5FFE4E1D9FFF8F7F5FFFBFBF9FFE4E1D9FFCCC4B0FFBEB4
      9CFFC3B99FFF9D9279DE0404042200000000000000004141C5FF8C8CCEFF8C8C
      C5FFADADC5FFA5A5C5FF8C8CCEFFA5A5C5FFA5A5C5FF8C8CCEFFA5A5C5FFADAD
      C5FF8C8CC5FF8C8CCEFF5151BDFF000000000000000005051E71373FB1EC3C56
      F7FF1C3BFAFF0C25F0FF040B9DEB000000140000073C0608A0ED2224D4FF1010
      6ACA000000060000000000000000000000000000000000000000000000001414
      0D68F9F9F0FFEEEEE2FFE8E8D9FFE6E6D5FFE5E5D4FFB6B6A5FFFFFFFFFF1414
      0D68010101200000000000000000000000000000000000000000060606288880
      6ECB987C41FF836322FF88703AFFE8E5DFFFF3F2F0FF8D7745FF826322FF9376
      3AFF958A73D80B0B0B370000000000000000000000005959CEFF9C9CD6FF9C9C
      D6FF9C9CD6FF9C9CD6FF9C9CD6FF9C9CD6FF9C9CD6FF9C9CD6FF9C9CD6FF9C9C
      D6FF9C9CD6FF9C9CD6FF6262C5FF000000006062A5E6A2ABF7FF8D9DFFFF697E
      FFFF3954F9FF060F7FD60000001000000000000000000000000206074AAE3E3F
      CFFE3B3CB1F40303176500000000000000000000000000000000000000001414
      0D67FBFBEFFFF4F4E5FFF3F3E2FFF2F2E1FFF2F2E1FFC2C2B1FF14140D670202
      0125000000000000000000000000000000000000000000000000000000000101
      0115413F3C859D9075DDA9925CFDB1A27AFFB5A783FFA9915CFFA39476E34E4B
      46920303031E00000000000000000000000000000000383880BB7272DEFF6A6A
      DEFF6A6ADEFF6A6ADEFF6A6ADEFF6A6ADEFF6A6ADEFF6A6ADEFF6A6ADEFF6A6A
      D6FF6A6AD6FF7272DEFF38387CBB0000000051518CD8B6BBF5FFA3ACFBFF616B
      D7FA080A39980000000000000000000000000000000000000000000000000000
      063822227DD45859C3FA2E2E83DB00000F540000000000000000000000000B0B
      074D14140D6614140D6614140D6614140D6614140D6614140D66020201240000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000303031E1C1B1B563634317A3634317B1F1F1E5C050505250000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000325404173C36A6CA9DF0A0A
      287F000000000000000000000000000000000000000000000000000000000000
      0000000000120C0D3A9837388FDE040429840000000000000000000000000000
      00000000000000000002131313771919198A1818188A1717178A1616168A1616
      168A1515158A1414148A1313138A0A0A0A66000000001715135A26211C790101
      0113000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000822344AA81D30
      4AAB0000000B0000000000000000000000000000000000000000000000000404
      042663635EC0666862C4070707320000000000000000B19E8DFF000000000000
      00000000000002020220201C18702B2621802C272280221E1A70060505300000
      00000000000000000000000000000000000002080C4A144063CF1E6197FF1E61
      97FF1E6197FF226194FF567187FFF7F7F7FFF0F0F0FFF0F0F0FFF0F0F0FFF0F0
      F0FFF0F0F0FFF0F0F0FFE9E9E9FA12121284211D186C773C00FF7A4000FF543E
      1DC9010101170000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000011021831AC225FB1FF1245
      94FF051E41B50000000B000000000000000000000000000000000000000F7B7C
      74DA40413CB0363633A975776FDD0000001400000000B19E8DFF3C342C9F0202
      01205B4F43BFB9A998FFCBBDAEFFCEC1B2FFCFC1B2FFCDBEAEFFC3B2A0FF695E
      53BF06050530000000000000000000000000123857C260A4D7FF63A7DAFF62A5
      D9FF60A3D8FF609ED1FF738DA3FFEFEFEFFFE7E7E7FFE7E7E7FFE7E7E7FFE7E7
      E7FFE6E6E6FFE6E6E6FFE2E2E2FA13131384493821A7AD6100FFAE6800FF8449
      00FF563E1CCE0202011800000000000000000000000000000000000000000000
      000000000000000000000000000000000000030404331B5EAAFF2A63ADFF1B4E
      9BFF114290FF041B3BB40000000A00000000000000000000000008080856C4C6
      B9FF0000000B00000000A4A69BF50202023500000000B19E8DFFBBAC9DFFA996
      84FFD3C8BBFFDBD1C5FFDAD1C4FFDAD0C3FFD9CFC1FFD9CEC0FFD8CCBEFFD6C9
      BAFFC2AF9DFF1A17156000000000000000001E6197FF66AADCFF468DCFFF448A
      CEFF4186CDFF4283C5FF6684A0FFF0F0F0FFB3B3B3FFB3B3B3FFB3B3B3FFB3B3
      B3FFB3B3B3FFB2B2B2FFE3E3E3FA14141484080807338B5B0EE9BB6E00FFAC67
      00FF844900FF563E1CCE02020118000000000000000000000000000000000000
      0000000000000000000000000000000000000000000201060E69266CBCFF194E
      99FF1A4F9DFF134593FF041937B300000009000000000000000250514CBDBEC0
      B6FF41413DAE4A4B47B57C7C75D90000001100000000B19E8DFFC8BCAFFFCEC4
      B7FFD5CBBFFFD7CDC0FFBDAD9DFFB5A391FFB8A593FFC1B1A0FFD4C7B9FFD8CC
      BEFFD8CBBCFFC1AF9CFF06050530000000001E6197FF67ADDCFF4892D1FF468E
      D0FF448ACEFF4587C7FF6A87A2FFF0F0F0FFE8E8E8FFE8E8E8FFE7E7E7FFE7E7
      E7FFE7E7E7FFE7E7E7FFE3E3E3FA141414840000000009090836915F0CECBD70
      00FFAC6600FF844900FF543A12CC0F0D0B45302D2877534E499A4B4541921F1C
      18600101011700000000000000000000000000000000000000000209116C2D74
      C4FF1D529DFF1D53A0FF144796FF011634B20000001129292690BDBFB5FFC3C5
      BAFFAEB0A4F5454541AF010101260000000000000000B19E8DFFC2B5A7FFC8BC
      AFFFCBC0B3FF927F6EEF0A09074000000000000000000605043064584EBFCABB
      ABFFD8CCBEFFD6C9BAFF7A6D60CF000000001E6197FF69B0DEFF4B96D3FF4992
      D2FF468ED0FF488BC9FF6D8AA4FFF1F1F1FFB5B5B5FFB4B4B4FFB4B4B4FFB3B3
      B3FFB3B3B3FFB3B3B3FFE3E3E3FA1515158400000000000000000A0909379866
      0DECBF7200FFAA6401FFB59366FFC7BDB2FFBEB4A1FFBCB096FFBCB198FFC0B6
      A7FFA69D93E7221E1B6600000000000000000000000000000000000000000309
      116C3379C7FF2156A0FF1D53A0FF0C4192FF254166DEADAC9DFFCDCDBDFF3333
      309A0000001700000000000000000000000000000000B19E8DFFBCAEA0FFC2B5
      A7FFC8BCAFFFBDAE9EFF3F362F9F000000000000000000000000000000006356
      4BBFD4C7B9FFD8CCBEFFC4B3A1FF060504301E6197FF6BB2DFFF4E9BD5FF4C97
      D3FF4993D1FF4A90CBFF6E8DA6FFF1F1F1FFE9E9E9FFE9E9E9FFE8E8E8FFE8E8
      E8FFE8E8E8FFE7E7E7FFE3E3E3FA161616840000000000000000000000000A09
      08378E5C0CE2D3B386FFC2BFB8FFB4A166FFD1BD7AFFE3D5A3FFDECE95FFC6B0
      69FFB0A175FFC6BCB1FF2724216E000000000000000000000000000000000000
      00000309116C367AC8FF2F66AFFF3F81CCFF1F5096FF526782FF272D31A60000
      00000000000000000000000000000000000000000000B19E8DFFB19E8DFFB19E
      8DFFB19E8DFFB19E8DFFB19E8DFFB19E8DFF0000000000000000000000000202
      0220BEAD9BFFD9CEC0FFCEC0B1FF2C2722801E6197FF6EB4E0FF509ED7FF4E9B
      D6FF4C97D4FF4D94CDFF7190A9FFF1F1F1FFB6B6B6FFB5B5B5FFB5B5B5FFB5B5
      B5FFB4B4B4FFB4B4B4FFE4E4E4FA171717840000000000000000000000000000
      0000423B3290C1BDB4FFAD923DFFDEC665FFEBDB96FFF0E5B1FFEEE1A7FFE7D5
      84FFCFB349FFA5935CFFC5BEB5F70605052B0000000000000000000000000000
      0000000000000309116B4A8CD2FF538DCDFF0A2D6DFF083F8CFF011B3DB00000
      0000000000000000000000000000000000000D181F602C637EBF296380BF1833
      438F000000000000000000000000000000000000000000000000000000000000
      000087796ADFD9CFC1FFD6CABCFF51473EAF1E6197FF71B6E1FF55A2D7FF519F
      D7FF4E9CD5FF5098CFFF7393ABFFF8F8F8FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2
      F2FFF2F2F2FFF1F1F1FFEAEAEAFA181818840000000000000000000000000000
      00099E9288DCA2915AFFCEAE30FFDEC65FFFE2CE78FFE5D487FFE4D283FFE0CA
      6DFFDDC351FFB5951FFFB3A994FF423B348D0000000000000000000000000000
      0000000000012F2E2B97A2ADB0FF395C8BFF346EB6FF3164A9FF111419840000
      000000000000000000000000000000000000172A378040BFEEFF37CAF8FF3E9A
      C5EF000000000000000000000000000000000000000000000000000000000000
      0000B19E8DFFB19E8DFFB19E8DFFB19E8DFF1E6197FF74B8E2FF5AA6D9FF56A3
      D8FF519FD7FF519DD5FF5F8AA8FF6287A0FF6286A0FF61859FFF67869EFF4866
      80FF1E1E1E8A1E1E1E8A1D1D1D8A0E0E0E630000000000000000000000000404
      0422C4BAB2FAA08528FFD4B42BFFD8BD49FFDCC35BFFDEC764FFDDC662FFDAC0
      53FFD6BA3FFFC4A115FFA39263FF817A74C100000000000000020C0E0B5C1C1D
      18744F4F4AB8C9CBBEFFD7D5C1FF435E83E529558DDA3C4247BEA09C8AFF0B0B
      0A640000000000000000000000000000000011202A7042BCECFF37CAF7FF44B6
      E6FF040A0D40000000000000000000000000173C519F3CA1D3FF3DABDDFF3FAC
      DEFF43ADDFFF46AEE0FF4EA8DBFF000000001E6197FF78BAE3FF5FA9DBFF58A4
      D9FF519FD7FF509ED7FF509ED7FF509ED7FF509ED7FF509ED7FF60A2D8FF1E61
      97FF000000000000000000000000000000000000000000000000000000000303
      0320C7BDB5FA9C8021FFD4B329FFD6BA42FFD9BE4DFFDAC154FFDAC052FFD8BC
      48FFD6B93CFFC29E10FFA19161FF817973BF000000062F302C9CD8D9CDFFCDCF
      C3FFCACCBFFFCFD1C3FF3434309900000015000000140000000744443FB6ABAC
      9FFF0B0B0A63000000000000000000000000010203204DB4E5FF37C8F5FF39C9
      F7FF326E8FCF00000010000000000000000000000000193D519F3CC0EFFF38D3
      FFFF38D2FEFF38D0FDFF4CB0E2FF000000001E6197FF7ABCE4FF63ADDDFF60AA
      DCFF5CA7DAFF5AA6D9FF5AA6D9FF5AA6D9FF5AA6D9FF509ED7FF60A2D8FF1E61
      97FF000000000000000000000000000000000000000000000000000000000000
      0007A0948ADB9D8B51FFCBAA2CFFDEC55BFFDCC35BFFDDC55FFFDCC45EFFDBC2
      58FFDFC658FFAC8A12FFB6AD99FF3E3731871616155ACBCDBFFFAAACA0E9C0C2
      B6F9CBCDC0FF96988EE40000000B0000000000000000000000000000000A4646
      41B4C9CABBFF1A1A187B0000000C0000000000000000335E7CBF3DC0EFFF37CA
      F7FF3BC5F3FF3980A7DF0A161E6000000010000000100A161E6045A8DAFF39CE
      FBFF38D0FDFF38CFFBFF4DB1E3FF000000001E6197FF7DBEE4FF67B1DEFF489A
      DAFF4296DCFF4195DCFF4095DCFF4094DCFF3F94DBFF4F9DD6FF6AB1DEFF1E61
      97FF000000000000000000000000000000000000000000000000000000000000
      0000302B2776CDC7BFFFA0862EFFE1CC77FFEEE09FFFECDD9CFFEDDE9DFFEEDE
      9CFFCBB04FFFA08E57FFCEC7C0F304040324676763AE919289DE000000191414
      1272D1D3C6FF7F8078D600000000000000000000000000000000000000000000
      000A787971CEFBFDEBFF71726BCB0000001200000000010203204A9AC6EF39C5
      F3FF37CAF7FF39CAF7FF41BCEBFF45B1E2FF45B1E2FF40BCECFF39CDFAFF38CF
      FCFF38CEFBFF37CCF9FF4FB1E3FF00000000174B75E06FB0D9FE7CBEE4FF4C9C
      DFFFB4EEFDFF73D4F0FF73D4F0FFB4EEFDFF499ADEFF6CB3E0FF66AAD5F91C58
      89F3000000000000000000000000000000000000000000000000000000000000
      00000000000778716CB6D0CABEFFB4A166FFD0BE84FFE0D2A5FFDDCE9EFFC4B0
      70FFB6A87DFFDED4CCFF211E1B60000000000D0D0C4D000000230000000F4D4D
      48BADFE1D1FF383834A500000000000000000000000000000000000000000000
      000004040343D7D9CAFFE6E7D7FF2B2B28970000000000000000030607304A9A
      C6EF3DC1F0FF37CAF7FF37CBF8FF37CDFAFF37CEFAFF38CEFAFF37CDFAFF3EC4
      F2FF51ACDFFF43BDECFF52B1E3FF00000000010305320F2F49B21E6197FF3573
      A3FFB5EFFEFF7EDBF3FF7EDBF3FFB5EFFEFF2C6CA0FF1E6197FF0C283FA5030A
      0F53000000000000000000000000000000000000000000000000000000000000
      0000000000000000000947413C8CC8BEB6EEDBD0BDFFDBD0B5FFDACFB7FFDED3
      C5FFA39A93D518161453000000000000000000000000000000001D1E1C728687
      80C6212120730000000A00000000000000000000000000000000000000000000
      0000000000000E0E0E53767671BD0F0F0F550000000000000000000000000000
      00102341569F51B1E3FF46BAEBFF3FC1F0FF3FC2F0FF45BCECFF4EB0E3FF2141
      559F000000102341569F5AACDFFF0000000000000000000000000002042A1E61
      97FF1E6197FF1E6197FF1E6197FF1E6197FF1B5686F000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000404032425211E6747423F8B3D3A36831513
      11500000000C0000000000000000000000000000000000000000000000040000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000010050A0D40162A3780162A3780050A0D40000000100000
      000000000000000000002441569F000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000070402300000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000007714421D7A96D39F77E4D27E00000
      0014000000000000000000000000000000000000000000000000000000000000
      0000B1B1B1FF8F8F8FFF878787FF878787FF878787FF878787FF878787FF8787
      87FF878787FF878787FF878787FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00004327128F0D07034000000000000000000000000000000000010101021212
      1213000000010000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00180101002000000000000000005F371BC6E7B161FF99753EE5DDAA60FF7345
      23D8000000000000000000000000000000000000000000000000000000000000
      0000B6B6B6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF878787FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000003020120A55E2DDF000000100000000000000000000000007F70688F1616
      1617000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000D090356AA81
      44ECD6A358FE684321CB00000018824F28E1C99952FA0000001E3427139CAF71
      3CFC0000001000000000000000000000000000000000B1B1B1FF8F8F8FFF8787
      87FFB6B6B6FFFFFFFFFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFB
      FBFFFCFCFCFFFFFFFFFF878787FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008E4F26CF351D0E8000000000000000001B1B1B1CA87253DA0808
      0809000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000936937E7B187
      49E8674C25C5ECB564FF774D25D8915A2DEBB18648F30000000018110876B676
      3EFF0000001B00000000000000000000000000000000B6B6B6FFFFFFFFFFFFFF
      FFFFB6B6B6FFFFFFFFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCD
      BFFFDBCDBFFFFFFFFFFF878787FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000004224118FBD6532EF0000001000000001B85A29FF4847464E0000
      0001000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000AA7B41F2543F
      20B60000000033261197E7AF64FFC89F72FEE5B065FF533418C17D5F33D39F65
      34F40000000800000000000000000000000000000000B6B6B6FFFFFFFFFFFBFB
      FBFFB6B6B6FFFFFFFFFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
      FDFFFDFDFDFFFFFFFFFF878787FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000027150970DB743DFF1C0E066028282829C66131FF262626270000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000462F16ACD2A2
      5AFD0604013E0A07024FE4AC5EFFDCBC9AFFEFCDA4FFEFB665FFD8A55BFF0F0A
      04640000000000000000000000000000000000000000B6B6B6FFFFFFFFFFDBCD
      BFFFB6B6B6FFFFFFFFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCD
      BFFFDBCDBFFFFFFFFFFF878787FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000C060340DD773DFF602F15AFAC6B4CE2C05F30FF1D1D1D1E0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000018825B
      2FDBDCA95EFFBC8F4AF3EAB263FFD8B998FFF8EBE1FF5E492EBC050300410000
      00000000000000000000000000000000000000000000B6B6B6FFFFFFFFFFFDFD
      FDFFB6B6B6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF878787FF00000000D05E28FFD05E28FFD05E28FFD05E
      28FFD05E28FFD05E28FFD05E28FF833A16CF0000000000000000000000000000
      00000000000014090450DE773DFF733719BFBB5E32FFBC5E2FFF1D1D1D1E0000
      000000000000000000000000000007070708303030323636363A3636363A3636
      363A3636363A3636363A3636363A0D0D0D0E0000000000000000000000000100
      001A462E16AB77542CD2402A11A4786041CDF0E0D0FF6C5538C5000000040000
      00000000000000000000000000000000000000000000B6B6B6FFFFFFFFFFDBCD
      BFFFB6B6B6FFFFFFFFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCDBFFFDBCD
      BFFFDBCDBFFFFFFFFFFF878787FF00000000D05E28FFDA7544FFD8703BFFD970
      39FFDD6E35FFDC6831FF843916CF000000100000000000000000000000000000
      00000000000032170A80E47A40FF87401DCFC4683CFFBC6033FF262626270000
      00000000000000000000000000000F0F0F10CA6631FFE3763CFFE4793EFFE47A
      40FFE67C41FFE67D42FFCA672EFF121212130000000000000000000000000000
      000000000000000000020000000035291A8BF5E9DDFFE2CDB3FF473925A00000
      00000000000000000000000000000000000000000000B6B6B6FFFFFFFFFFFFFF
      FFFFB6B6B6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF888888FF00000000D05E28FFD6713FFFD36A34FFD76E
      38FFDF7846FF9A431CDF00000010000000000000000000000000000000000000
      000000000000843C19CFE5793FFF713216BFCC7348FFCD7246FF363636390202
      0203000000000000000000000000000000000F0F0F10CB6631FFE07636FFDE77
      34FFDF7936FFE07B37FFC7652DFF121212130000000000000000000000000000
      000000000000000000000000000032271889F1E2D3FFC1A683F6F5E9DCFF1C15
      096C0000000000000000000000000000000000000000B6B6B6FFFFFFFFFFDBCD
      BFFFB6B6B6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC8C8C8FFC8C8
      C8FFC8C8C8FFC8C8C8FFA8A8A8FF00000000D05E28FFD6703FFFD77341FFD56C
      37FFDD7946FFD16433FF06030130000000000000000000000000000000000000
      000013080350D56834FFE37840FF5D2A13AFC26A41FFE08658FF7F70689B1717
      17180000000000000000000000000000000001010102937969ADE27339FFDB71
      30FFDC7331FFDD7432FFC4632DFF121212130000000000000000000000000000
      00000000000000000000000000002E231584EDDECEFF160F0463827159CBE1D0
      BDFC0705013B00000000000000000000000000000000B6B6B6FFFFFFFFFFFFFF
      FFFFB6B6B6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB6B6B6FFFFFF
      FFFFFFFFFFFFFAFAFAFFB6B6B6FF00000000D05E28FFDB8255FFDF8A61FFDD80
      52FFD56B36FFDE7A49FFB7592EEF261007700000000000000000000000000C05
      0240B45327EFE37740FFD86B37FF1B0C0560AD5126FFDA784AFFBF673CFF3D3D
      3D421717171803030304000000010A0A0A0B6E66627BDA6F3BFFDC763AFFDC77
      3BFFE0793EFFDD793CFFC2612CFF121212130000000000000000000000000000
      0000000000000000000000000000271E117BE6D4BFFF020100240101001D9D89
      71DCA48E74E101000018000000000000000000000000B6B6B6FFFFFFFFFFFFFF
      FFFFB6B6B6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB6B6B6FFFFFF
      FFFFFAFAFAFFC4C4C4FF4646469F00000000D05E28FFDD8D66FFB04F28EFD778
      4EFFDC7E4FFFD46B35FFDE7948FFD76D3CFF9C4621DF723217BF873C1BCFD465
      32FFE0733DFFE0733DFFAF4D23EF000000103F3D3D41DC8C64FFDC7C4FFFBF67
      3CFF857268A53939393D4C4B4A53B36339F3DB6F3AFFD97137FFD97238FFE074
      3EFFC76330FFDE763CFFBF5F2BFF121212130000000000000000000000000000
      0000000000000000000000000000221A0E74DEC8ADFF0101001E000000000302
      0027A79175E45F4C33B6000000000000000000000000B6B6B6FFFFFFFFFFFFFF
      FFFFB6B6B6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB6B6B6FFFAFA
      FAFFC4C4C4FF4646469F0000000000000000D05E28FF843D21CF000000103E1B
      0C8FD47146FFDE8356FFD7703DFFD76E39FFDC733FFFDE713CFFDD703AFFDB6F
      39FFDE713BFFCF5F2EFF120703500000000000000000A95E3BF0DF885CFFD46A
      38FFE18759FFDD8354FFDE7D4FFFDC7440FFD66D35FFD76E36FFDF713CFFA973
      55D814141415C5622FFFBC5D2AFF121212130000000000000000000000000000
      00000000000000000000000000001E170C6EC9B194F80000000F000000000000
      00000403012D564225B5000000000000000000000000B6B6B6FFFFFFFFFFFFFF
      FFFFB6B6B6FFC8C8C8FFC8C8C8FFC8C8C8FFC8C8C8FFC8C8C8FFB6B6B6FFBFBF
      BFFF4646469F000000000000000000000000853C1ACC00000010000000000000
      0000130803509D4B28DFDA794EFFDD7D4FFFDB7544FFDA713EFFDC6F3BFFD866
      34FFB04C23EF1207035000000000000000000000000000000000AA613EF0DC8C
      64FFD77241FFD26732FFD36833FFD66C37FFDF7847FFCA6636FF7E70688E0101
      0102000000000F0F0F10B95A29FF0C0C0C0D0000000000000000000000000000
      00000000000000000000000000001B140A69A28A6AE400000009000000000000
      00000000000000000002000000000000000000000000B6B6B6FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFB6B6B6FFFAFAFAFFC4C4C4FF4646469F0000
      0000000000000000000000000000000000000100001200000000000000000000
      000000000000000000001C0B04605F2913AF72341ABF713218BF6F2F15BF240F
      0670000000100000000000000000000000000000000000000000000000003F3D
      3D41AD5126FFB86036FFBF663BFFB5592CFF8E766AA604040405000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000402002A3F2E179E00000005000000000000
      00000000000000000000000000000000000000000000B6B6B6FFC8C8C8FFC8C8
      C8FFC8C8C8FFC8C8C8FFC8C8C8FFB6B6B6FFBFBFBFFF4646469F000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000510000
      00E70101018A0101018A0101018A0101018A0101018A0101018A000000E70000
      00E7000000E7000000E7000000AD000000000000000020202081262626AC2020
      1D6620201D6620201D6620201D6620201D6620201D6620201D6620201D662020
      1D6620201D66262626AC20202081000000000000000000000000000000000000
      00000000000000000000000000000000000000000000010101663E3939D72B29
      29D04F4848CF443F3FDA2E2E2EEB252525DF0000000410151A6B1B3A58D21627
      38BD1A3A4EC82F7AA0F12F7199F31F5474D9193240A50A0C0D4E000000000000
      000000000000000000000000000000000000000000000000004C000000DB2B2B
      2BF7B9ABABFF424242FF424242FFB5A7A7FFB5A7A7FFB9ABABFF424242FF5454
      54FF515151FF676767FF000000DB0000000000000000262626AE848484FFFFFF
      FFFFEFEFEEFFF9F9F8FFFFFFFEFFFFFFFEFFFFFFFEFFFFFFFEFFFFFFFEFFFFFF
      FEFFFFFFFFFF848484FF262626AE000000000000000000000000000000000000
      000000000000000000000000000000000000000000001E1E1ECF959090FF6F6D
      6DFFC4BDBDFF959191FF505050FF464646F900000000070809391E80B4FF0029
      57FF001B30FF15618EFF2179B3FF467A97FF2C749FFF005CA1FF27546CD30809
      0A440000000000000000000000000000000000000000000000CE656565FF3E3E
      3EFFBAB1B1FF3E3E3EFF3E3E3EFFB1A8A8FFB1A8A8FFBAB1B1FF3E3E3EFF5454
      54FF494949FF696969FF000000CE0000000000000000252525B07D7D7DFFFEFE
      FBFF4C4C4BFFC3C3C1FF000000FF528898FFBFD6D9FFFBFBF8FFFBFBF8FFFBFB
      F8FFFEFEFBFF7D7D7DFF252525B000000000000000005C4225B7B38148FFB381
      48FFB38148FFB38148FFB38148FFB38148FFB38148FF4F4438FF6D6D6DFF7D7C
      7DFF828283FF626263FF3F3F40FF3F3F40F2000000003A86A2E32388C7FF8970
      C1FFD9ADAFFF42544AFF00417BFF5A8BA4FFE8CB97FF858D86FF005499FF156A
      A0FF1720248100000000000000000000000000000000000000C9636363FF3A3A
      3AFFC0BBBBFF262626FF262626FFB7B2B2FFB7B2B2FFC0BBBBFF3A3A3AFF5454
      54FF414141FF6B6B6BFF000000C90000000000000000252525B27B7B7BFFFBFB
      F5FFF6F6F0FFF6F6F0FF7BB2C1FFD9F4FFFF3E9DACFFB5D3D5FFF6F6F0FFF6F6
      F0FFFBFBF5FF7B7B7BFF252525B20000000000000000B38148FFFAFAF9FFFAFA
      F9FFFAFAF9FFFAFAF9FFFAFAF9FFFAFAF9FFFAFAF9FF686868FFA1A19AFFA4A4
      9BFFA0A198FFA4A59DFFACACA4FF545452F0111A1C642E9ED6FFC27DD6FFFF85
      F2FFE6B6FFFFE4C8B9FF414A43FF003883FF6192AAFFFFF89AFF697E81FF0055
      95FF005798FF232C338A000000000000000000000000000000C5666666FF3636
      36FFC8C7C7FFC3C2C2FFC3C2C2FFC3C2C2FFC3C2C2FFC8C7C7FF363636FF5454
      54FF393939FF6E6E6EFF000000C50000000000000000252525B5787878FFF8F8
      EFFFF1F1E7FFF1F1E7FFCDE0DEFF45B1C0FF78E6F7FF3D9BAAFFB1CFCEFFF1F1
      E7FFF8F8EFFF787878FF252525B50000000000000000B38148FFFAFAF9FFA75D
      3EFFC56437FFBE6435FFAC6C34FF8C7230FF5D5F48FF656563FFEFEFE4FFFAFA
      EDFFF6F6E9FFF7F7E9FFFEFEF1FF676765F0266077BC46A9E7FF6195DCFF599E
      DBFF2996CEFFB9BDF6FFD4B5A7FF232724FF7A7C6CFFF6D696FF8E908CFF004C
      9DFF349299FF035D93FF0D0E0F4B0000000000000000000000C16A6A6AFF3333
      33FF323232FF323232FF323232FF323232FF323232FF323232FF333333FF3333
      33FF333333FF727272FF000000C10000000000000000252525B8747474FFF6F6
      E9FFECECDFFFECECDFFFECECDFFFB2DAD6FF44B0BEFF78E6F7FF3C9AA8FFADCC
      C8FFF6F6E9FF747474FF252525B80000000000000000B38148FFFAFAF9FFAF5B
      36FFD97134FFAD8034FF7C8F31FF51AE1CFF57824CFF706E6FFFF7F7F5FFFFFF
      FEFFFEFEFBFFFFFFFCFFFFFFFFFF696969EC10151654244453A50E181B661C3B
      449943B5E0FF1A8AC9FF71ABD0FF8D6C59FF252623FF0C4B7CFF3C9CA7FF449D
      ADFF85E6B0FF44B29DFF185187F10000000000000000000000BE6D6D6DFF6464
      64FF646464FF646464FF646464FF646464FF646464FF646464FF646464FF6464
      64FF646464FF6D6D6DFF000000BE0000000000000000252525BB717171FFD4D4
      C9FFF4F4E4FFF4F4E4FFF4F4E4FFF4F4E4FFB8E0DAFF46B2BFFF78E6F7FF3E9C
      A9FFA0AFAAFF717171FF252525BB0000000000000000B38148FFFAFAF9FFA95D
      35FFA38234FF5AA328FF48D021FF4EBC3DFF525552FF6B6969FFD0D2C5FFD9DA
      CCFFDEDED2FFCEC4ACFF84847CC8292927AF0000000000000000000000000000
      00002E47539E1E96D1FF0D8DD0FF95BFD5FF825F4EFF00062DFF42A799FFBDFF
      BBFF9BFFB4FF379C96FF00519BFF0D10115300000000000000BB717171FFD4D4
      C9FFF4F4E4FFF4F4E4FFF4F4E4FFF4F4E4FFF4F4E4FFF4F4E4FFF4F4E4FFF4F4
      E4FFD4D4C9FF717171FF000000BB0000000000000000242424BE6D6D6DFF6464
      64FF646464FF646464FF646464FF646464FF646464FF4B7379FF1D899AFF78E6
      F7FF434343FF5C5C5CFF242424BE0000000000000000B38149FFFAFAF9FF8869
      33FF5BB12AFF55B342FF596554FF3C2C3CFF050004FF171515FF509139FF7093
      39FFFAFAF9FFB38249FF00000000000000000000000000000000000000000000
      0000283F4A9C2E91C3F22D7294DA1391D4FF9FC0D1FF805745FF031F35FF54BF
      A8FF43B2A0FF00569FFF1270ACFF182C379800000000000000B8747474FFF6F6
      E9FFECECDFFFECECDFFFECECDFFFECECDFFFECECDFFFECECDFFFECECDFFFECEC
      DFFFF6F6E9FF747474FF000000B80000000000000000242424C1727272FF3333
      33FF333333FF333333FF323232FF323232FF323232FF323232FF254D53FF3B3B
      3BFFF7F7F7FF3D3D55FF1E1E3BD20000000000000000B38149FFFAFAF9FF776F
      31FF4CCB2DFF6B986AFF330934FF10000EFF000000FF000000FF3A9526FF6491
      2BFFFAFAF9FFB38249FF00000000000000000000000000000000000000001C25
      2A782CA4DDFF06070836000000001E3845971398DBFFA1C0CFFF734937FF0014
      3FFF2C8DC8FFA2D3F7FF60ABDAFF0C3248AE00000000000000B5787878FFF8F8
      EFFFF1F1E7FFF1F1E7FFF1F1E7FFF1F1E7FFF1F1E7FFF1F1E7FFF1F1E7FFF1F1
      E7FFF8F8EFFF787878FF000000B50000000000000000232323C56E6E6EFF3939
      39FF545454FF363636FFC8C7C7FFC3C2C2FFC3C2C2FFC3C2C2FFC3C2C2FFB8B7
      B7FF363651FF8080FFFF0D0D84F10000000000000000B38149FFFAFAF9FF9A62
      35FF7A942DFF49BB26FF5BC448FF5A7A58FF170018FF1A0B1CFF539C3CFF6E8C
      2FFFFAFAF9FFB3824AFF00000000000000000000000000000000000000014BB4
      E4FF3EB7F1FF2437429D000000040F15185F1FA5DBFF26A4DEFF9BBCCEFF5134
      27FF114E78FFA7E5FFFFA7D9FFFF1631429E00000000000000B27B7B7BFFFBFB
      F5FFF6F6F0FFF6F6F0FFF6F6F0FFF6F6F0FFF6F6F0FFF6F6F0FFF6F6F0FFF6F6
      F0FFFBFBF5FF7B7B7BFF000000B20000000000000000212121C96B6B6BFF4141
      41FF545454FF3A3A3AFFC0BBBBFFB7B2B2FFB7B2B2FF262626FF262626FFC0BB
      BBFF363650FF1F1FB8FF1D1D3FD70000000000000000B38149FFFAFAF9FFAB5C
      35FFC17735FF7D8D31FF51AA20FF48CB25FF5D8A56FF56924BFF5DB42CFF9573
      33FFFAFAF9FFB3824AFF00000000000000000000000000000000000000002534
      3A843AB0EAFF40A8DCFF3B92B4F740A0CFFF3B58BBFF2072BFFF6FE7FFFFE0EA
      DFFF372521FF458CBAFF3FB0F1FF0E161B6900000000000000B07D7D7DFFFEFE
      FBFFFBFBF8FFFBFBF8FFFBFBF8FFFBFBF8FFFBFBF8FFFBFBF8FFFBFBF8FFFBFB
      F8FFFEFEFBFF7D7D7DFF000000B00000000000000000202020CE696969FF4949
      49FF545454FF3E3E3EFFBAB1B1FFB1A8A8FFB1A8A8FF3E3E3EFF3E3E3EFFBAB1
      B1FF3E3E3EFF656565FF202020CE0000000000000000B3824AFFFAFAF9FFB05D
      37FFE16F35FFCB7435FFA68235FF78932EFF5FB52EFF6BAA31FFA88431FFBE65
      34FFFAFAF9FFB3824AFF00000000000000000000000000000000000000000000
      0000263C489C42B9F0FF44B3EAFF407DCBFF4B3DC6FF3759BDFF39ABD3FFA3FA
      FFFFC6DBD2FF3E3F40FF10648FE40000000800000000000000AE848484FFFFFF
      FFFFFFFFFEFFFFFFFEFFFFFFFEFFFFFFFEFFFFFFFEFFFFFFFEFFFFFFFEFFFFFF
      FEFFFFFFFFFF848484FF000000AE00000000000000001A1A1ADB676767FF5151
      51FF545454FF424242FFB9ABABFFB5A7A7FFB5A7A7FF424242FF424242FFB9AB
      ABFF303030F71A1A1ADB0F0F0F4C0000000000000000B3824AFFFAFAF9FFA05F
      47FFAF5D38FFAD5C36FFA95C35FF9D6135FF8F6534FF986334FFAB5C35FFA65A
      3AFFFAFAF9FFB3824AFF00000000000000000000000000000000000000000000
      000000000000161E216C459BC9F53995DEFF3A69CDFF3375C7FF48B6DDFFBBFC
      FFFF92FFFFFF5FA5C0FF504541DE000000100000000000000081000000AC1414
      0D6614140D6614140D6614140D6614140D6614140D6614140D6614140D661414
      0D6614140D66000000AC000000810000000000000000252525AD131313E71313
      13E7131313E7131313E72222228A2222228A2222228A2222228A1E1E1E821A1A
      1A79101010D611111151000000000000000000000000B3824AFFFAFAF9FFFAFA
      F9FFFAFAF9FFFAFAF9FFFAFAF9FFFAFAF9FFFAFAF9FFFAFAF9FFFAFAF9FFFAFA
      F9FFFAFAF9FFB3824AFF00000000000000000000000000000000000000000000
      000000000000000000000000000F17252976305C77C63D9AC5F45ABBE3FF7DCA
      DFF830647AC2050809372421207B121211580000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000066492AC0B3824AFFB382
      4AFFB3824AFFB3824AFFB3824AFFB3824AFFB3824AFFB3824AFFB3824AFFB382
      4AFFB3824AFF66492AC000000000000000000000000000000000000000000000
      0000000000000000000000000000000000080A0F104E182E3683193B4D99172F
      3A89080C0E4A00000000010101150707073400000000693418BFC0602CFF3119
      0B80000000100000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000010050521700C0C50AF0F0F60BF0F0F60BF0D0D4FAF050520700000
      00100000000000000000000000000000000000000000ADADADFF8F8F8FFF8787
      87FF878787FF878787FF878787FF878787FF878787FF878787FF878787FF8787
      87FF878787FF878787FF00000000000000000000000000000000091013505095
      B0EF5BAAC9FF0203042D00000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BD5E2CFFE0854BFFD57A
      41FF9A5024DF140A045000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000404
      2170141496EF0E0EACFF0E0EB6FF1111BFFF1717C6FF1C1CC7FF1D1DC0FF1A1A
      99EF0505207000000000000000000000000000000000B6B6B6FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF878787FF0000000000000000000000102B4F5FAF60B0CEFF70C1
      DBFF60AFCDFF0912156F00000022000000000000000000000010023104AF0126
      029F0000000000000000000000000000000000000000BE5F2CFFDF7D41FFE081
      42FFE59153FFD6793DFF763E1CBF030100200000000000000000000000000000
      00000000000000000000000000000000000000000000000000000909439F1515
      ACFF0606B2FF1616C1FF4F4FD2FF5E5ED3FF6565D6FF6262DFFF3E3EE0FF2C2C
      E2FF2323BDFF0B0B419F000000000000000000000000B6B6B6FFFFFFFFFFF9F9
      F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFFAFA
      FAFFFFFFFFFF878787FF00000000000000005095B0EF6ABDD7FF77CCE3FF76CA
      E2FF5BAAC9FF4887A0FF427B93FF4D8FAAFF509CB1FF178232FF17A72FFF0879
      11FF0305063000000000000000000000000000000000BF602CFFDF793EFFDB72
      31FFE17E3CFFEA9958FFE79757FFDC7B3BFF371E0D8000000010000000000000
      00000000000000000000000000000000000000000000040421703939BFFF1111
      B8FF2121BFFF4A4AC3FF1515469F02021050020210501515459F6666D8FF3F3F
      ECFF3A3AF2FF2323BDFF050520700000000000000000B6B6B6FFFFFFFFFFFAFA
      FAFFFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFFBFB
      FBFFFFFFFFFF878787FF00000000000000005BAAC9FF79D2E7FF78D0E6FF77CE
      E4FF5BAAC9FF5696B0FF5590A8FF5AA0B2FF1E903CFF1DAF3AFF1BB538FF097D
      13FF3C8F85FF00000000000000000000000000000000BF5F2CFFDE753AFFDA70
      2FFFDF7936FFE3833DFFEA944EFFF0A764FFE89552FFA65928DF140B05500000
      000000000000000000000000000000000000000000102121A0EF2F2FC2FF0808
      B4FF161675CF0000063000000000000000000000000004041E602929DEFF3030
      E6FF4A4AEDFF2D2DE3FF1A1A98EF0000001000000000B6B6B6FFFFFFFFFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFDFD
      FDFFFFFFFFFF878787FF00000000000000005BAAC9FF7BD6EAFF7AD4E8FF79D2
      E7FF5BAAC9FF599BB6FF4B9897FF1E963CFF22B646FF22BB45FF1BB538FF13A7
      28FF0A8A15FF025106DF000300300000000000000000BE5F2CFFDC7036FFD96D
      2DFFDD7633FFE17E39FFE58740FFE99047FFEFA15AFFEFA866FFDB8041FF753D
      1BBF03010020000000000000000000000000030318604848C6FF0B0BB5FF1313
      B0FF0000063000000000000000000000000003031C601E1ED2FF2323D8FF2929
      DEFF6F6FDCFF4141E1FF1D1DBDFF0404186000000000B6B6B6FFFFFFFFFFFDFD
      FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFEFE
      FEFFFFFFFFFF878787FF00000000000000005BAAC9FF7CD9ECFF7CD7EBFF7BD6
      EAFF5BAAC9FF5BA9C8FF2EAB43FF46C867FF28C152FF22BB45FF1BB538FF15AF
      2CFF0FA920FF099F13FF036206EF0003003000000000BC5E2BFFDB6B31FFD769
      2AFFDB7130FFDE7835FFE2803AFFE5863FFFE78B43FFE99149FFEEA25FFFE494
      55FFCE7036FF311A0C8000000000000000000909449F5F5FCFFF1010B5FF0909
      439F00000000000000000000000002021B601414C8FF1919CDFF1E1ED2FF0404
      1E601515459F6666E0FF1C1CC5FF0B0B429F00000000B6B6B6FFFFFFFFFFFEFE
      FEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFE
      FEFFFFFFFFFF878787FF00000000000000005BAAC9FF7EDDEEFF7DDBEDFF7DDA
      EDFF7DD9ECFF76D0E5FF2FB147FFBBF6C0FF47CC69FF22BB45FF1BB538FF15AF
      2CFF0FA920FF09A314FF049709FF013803BF00000000BB5C2BFFDC7039FFD566
      28FFD86C2CFFDB7231FFDE7835FFE17D39FFE3823CFFE3833DFFE3833DFFE58B
      47FFE69658FFD1783FFF25120870000000000D0D63BF6D6DD5FF3636BFFF1212
      3080000000000000000001011A600D0DBFFF1010C3FF1414C8FF03031C600000
      0000070721707B7BDEFF1616C4FF0F0F60BF00000000B6B6B6FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF878787FF00000000000000005BAAC9FF81E1F1FF80E0F0FF80DE
      EFFF7EDEEFFF7EDDEEFF4EAAABFF3CB752FFBAF5BFFF4CCC67FF1BB538FF0F93
      1FFF0C951AFF09A314FF049E08FF037406FF00000000B85B2AFFDD7744FFD76F
      36FFD66A2DFFD86B2CFFDB7030FFDD7533FFDE7835FFDF7936FFDF7936FFE081
      3FFFE59457FFCF753EFF24120870000000000D0D63BF7070D6FF3F3FC2FF1111
      308000000000000019600707B9FF0A0ABCFF0D0DBFFF02021B60000000000000
      0000070721706E6ED9FF1111BDFF0F0F60BF00000000B6B6B6FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF878787FF00000000000000005BAAC9FF82E4F3FF82E3F3FF82E3
      F2FF81E2F2FF81E2F1FF5BAAC9FF58B9BFFF3CB653FFAAEDAFFF5DD06EFF0F95
      20FF0432089F098F13FF049E08FF047909FF00000000B65929FFDC7947FFD66E
      37FFD77139FFD97338FFD97033FFD97031FFDA6F2FFFDC7333FFE18446FFDC85
      4BFFC56732FF3B1E0D8F00000010000000000808449F6868D2FF4343C6FF0B0B
      449F04041A602E2EC2FF2222C0FF1212BCFF01011A6000000000000000000000
      00001515469F4F4FD1FF0F0FB4FF0A0A429F00000000B6B6B6FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF878787FF00000000000000005BAAC9FF84E8F6FF83E7F5FF83E6
      F5FF83E6F4FF83E6F4FF5BAAC9FF6BC4EAFF5DC2CBFF24AB3EFFA9EDAEFF3EAE
      49FF00000000054A0CBF049E08FF057E0BFF00000000B35729FFDC7B4BFFD46B
      36FFD66E37FFD77038FFD87239FFD9743AFFDE7C44FFDF844CFFCB6C36FF6B36
      19BF06030130000000000000000000000000030319605B5BCEFF4D4DC9FF3131
      BEFF3737C4FF3838C4FF3131C3FF04041A600000000000000000000000000000
      06305D5DC9FF3434C9FF1C1CB0FF0303186000000000B6B6B6FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF8A8A8AFF00000000000000005BAAC9FF85EBF8FF85EAF7FF85EA
      F7FF85E9F7FF85E9F6FF5BAAC9FF6EC9F0FF6FCDF5FF62CCD7FF2AAD4BFF2AA4
      4CFF0000000005440AAF049E08FF06700EEF00000000B05428FFDB7C4EFFD369
      35FFD46B36FFD56D37FFD8703AFFDF7944FFCF6F3BFFA85429EF1A0D06600000
      000000000000000000000000000000000000000000102222A1EF6F6FD5FF4343
      C7FF3F3FC6FF3737C4FF04041A60000000000000000000000000000006303333
      A4EF4646CAFF2626BCFF161697EF0000001000000000B6B6B6FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF888888FF878787FF8787
      87FF878787FF9D9D9DFF00000000000000005BAAC9FF87EEFAFF86EEF9FF86ED
      F9FF86EDF9FF86EDF9FF5BAAC9FF6FCCF5FF71CFF8FF74D5FFFF74D5FFFF5DAE
      CEFF010902400B9A17FF089511FF0321068000000000AD5227FFDB7D51FFD167
      34FFD36A37FFDA7544FFD4703FFFBD5E2DFF3A1D0D8F00000010000000000000
      00000000000000000000000000000000000000000000040422704646C5FF6969
      D3FF4343C7FF3636BFFF0B0B449F06062270060622700B0B449F2C2CB8FF3636
      C3FF3838C4FF1E1EB0FF040421700000000000000000B6B6B6FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA5A5A5FFFFFFFFFFFDFD
      FDFFDDDDDDFFA0A0A0EF00000000000000005BAAC9FF88F1FCFF88F1FCFF88F0
      FBFF87F0FBFF87F0FBFF5BAAC9FF71D0F9FF73D3FDFF74D5FFFF74D5FFFF50AD
      B0FF11A023FF0D9E1AFF085010BF0000000000000000AA4F26FFDA8054FFD673
      45FFD77A4EFFBF6235FF643117BF060301300000000000000000000000000000
      00000000000000000000000000000000000000000000000000000808449F4646
      C5FF7777D9FF4E4EC9FF4343C6FF3C3CC1FF3A3AC0FF3D3DC4FF3C3CC5FF5E5E
      D2FF3A3ABFFF0909439F000000000000000000000000B6B6B6FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB6B6B6FFFDFDFDFFDDDD
      DDFFA0A0A0EF0606063000000000000000005BAAC9FF89F4FDFF89F3FDFF89F3
      FDFF86EEF9FF77D7E9FF5BAAC9FF8BDFFFFF8BE0FFFF8BE0FFFFABEFFFFF51B0
      B2FF13A528FF05310B8F000000000000000000000000A64C25FFDE8B61FFC76F
      45FF9A4B25EF180C056000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000404
      22702222A1EF6363D2FF7272D6FF7272D7FF6F6FD7FF6969D4FF5F5FCFFF2323
      A1EF0404217000000000000000000000000000000000B6B6B6FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB6B6B6FFDDDDDDFFA0A0
      A0EF060606300000000000000000000000005BAAC9FF8AF5FFFF77D9EAFF69C1
      DAFF5BAAC9FF7EC5DAFFA9E4EEFFCCFFFFFFCCFFFFFFCCFFFFFFB8F1F7FF549D
      B9EF00000000000000000000000000000000000000004C2211AFA54B25FF401E
      0E9F000000100000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000010040422700A0A52AF0D0D63BF0D0D63BF0A0A52AF040421700000
      00100000000000000000000000000000000000000000B6B6B6FFB6B6B6FFB6B6
      B6FFB6B6B6FFB6B6B6FFB6B6B6FFB6B6B6FFB6B6B6FFB6B6B6FFA0A0A0EF0606
      0630000000000000000000000000000000003C7084CF5BAAC9FF5DADCDFF5FB1
      D1FF60B4D5FF61B5D6FF61B5D6FF61B5D6FF61B5D6FF62B7D8FF56A1BDEF0D19
      1E6000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000900000000100010000000000800400000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFF000000000000C7FF00000000
      0000CFFF0000000000008FFF0000000000000FFF0000000000001FFF00000000
      00001FFF0000000000001E000000000000001E000000000000000F0000000000
      00000F0000000000000000000000000000000000000000000000800000000000
      0000C008000000000000E03F0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000}
  end
  object AppEvents: TApplicationEvents
    OnHint = AppEventsHint
    Left = 536
    Top = 144
  end
  object Customizer: TSpTBXCustomizer
    Left = 537
    Top = 208
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
        DisplayName = 'JPEG Image File (*.jpg;*.jpeg;*.jpe;*.jfif)'
        FileMask = '*.jpg;*.jpeg;*.jpe;*.jfif'
      end
      item
        DisplayName = 'TIFF Images (*.tif;*.tiff)'
        FileMask = '*.tif;*.tiff'
      end
      item
        DisplayName = 'GIF Images (*.gif)'
        FileMask = '*.gif'
      end
      item
        DisplayName = 'Windows Bitmaps (*.bmp)'
        FileMask = '*.bmp'
      end
      item
        DisplayName = 'Windows Media Photo (*.wdp)'
        FileMask = '*.wdp'
      end
      item
        DisplayName = 'All Files (*.*)'
        FileMask = '*.*'
      end>
    Options = [fdoOverWritePrompt]
    Left = 457
    Top = 208
  end
  object DlgOpenFractal: TFileOpenDialog
    DefaultExtension = '.nbr'
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'Nanobrot Files (*.nbr)'
        FileMask = '*.nbr'
      end
      item
        DisplayName = 'All Files (*.*)'
        FileMask = '*.*'
      end>
    Options = [fdoPathMustExist, fdoFileMustExist]
    Left = 289
    Top = 144
  end
  object DlgImport: TFileOpenDialog
    DefaultExtension = '.kfr'
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'Kalle'#39's fraktaler parametrs file (*.kfr)'
        FileMask = '*.kfr'
      end
      item
        DisplayName = 'Kalle'#39's fraktaler map file (*.kfb)'
        FileMask = '*.kfb'
      end
      item
        DisplayName = 'Nanobrot map file (*.nbm)'
        FileMask = '*.nbm'
      end
      item
        DisplayName = 'All Files (*.*)'
        FileMask = '*.*'
      end>
    Options = [fdoPathMustExist, fdoFileMustExist]
    Left = 289
    Top = 208
  end
  object DlgExport: TFileSaveDialog
    DefaultExtension = '.kfr'
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'Kalle'#39's fraktaler parametrs file (*.kfr)'
        FileMask = '*.kfr'
      end
      item
        DisplayName = 'Kalle'#39's fraktaler map file (*.kfb)'
        FileMask = '*.kfb'
      end
      item
        DisplayName = 'Nanobrot map file (*.nbm)'
        FileMask = '*.nbm'
      end
      item
        DisplayName = 'All Files (*.*)'
        FileMask = '*.*'
      end>
    Options = [fdoOverWritePrompt]
    Left = 373
    Top = 208
  end
end
