object FrmPresetManager: TFrmPresetManager
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Preset Manager'
  ClientHeight = 202
  ClientWidth = 281
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object SpTBXPanel1: TSpTBXPanel
    Left = 190
    Top = 0
    Width = 91
    Height = 202
    Caption = 'SpTBXPanel1'
    Align = alRight
    TabOrder = 0
    DesignSize = (
      91
      202)
    object SpTBXButton1: TSpTBXButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Action = cmdAdd
      TabOrder = 0
    end
    object BtClose: TSpTBXButton
      Left = 8
      Top = 171
      Width = 75
      Height = 25
      Hint = 'Close Preset Manager'
      Caption = 'Close'
      Anchors = [akLeft, akBottom]
      TabOrder = 1
      Cancel = True
      ModalResult = 8
    end
    object SpTBXButton2: TSpTBXButton
      Left = 8
      Top = 37
      Width = 75
      Height = 25
      Action = cmdDelete
      TabOrder = 2
    end
    object SpTBXButton3: TSpTBXButton
      Left = 8
      Top = 66
      Width = 75
      Height = 25
      Action = cmdRename
      TabOrder = 3
    end
    object SpTBXButton4: TSpTBXButton
      Left = 8
      Top = 95
      Width = 75
      Height = 25
      Action = cmdUpdate
      TabOrder = 4
    end
    object SpTBXButton5: TSpTBXButton
      Left = 8
      Top = 124
      Width = 25
      Height = 25
      Action = cmdMoveUp
      TabOrder = 5
      Images = FrmMain.IlMain16
      ImageIndex = 27
      OnDrawCaption = DrawButtonCaption
    end
    object SpTBXButton6: TSpTBXButton
      Left = 33
      Top = 124
      Width = 25
      Height = 25
      Action = cmdMoveDown
      TabOrder = 6
      Images = FrmMain.IlMain16
      ImageIndex = 28
      OnDrawCaption = DrawButtonCaption
    end
    object SpTBXButton7: TSpTBXButton
      Left = 58
      Top = 124
      Width = 25
      Height = 25
      Action = cmdSort
      TabOrder = 7
      Images = FrmMain.IlMain16
      ImageIndex = 29
      OnDrawCaption = DrawButtonCaption
    end
  end
  object LbPresets: TSpTBXListBox
    AlignWithMargins = True
    Left = 4
    Top = 4
    Width = 182
    Height = 194
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alClient
    ItemHeight = 18
    MultiSelect = True
    PopupMenu = PmPresets
    TabOrder = 1
    OnDrawItem = LbPresetsDrawItem
  end
  object ActionList1: TActionList
    Images = FrmMain.IlMain16
    Left = 116
    Top = 60
    object cmdAdd: TAction
      Caption = 'Add...'
      Hint = 'Add Preset'
      ShortCut = 45
      OnExecute = cmdExecute
    end
    object cmdDelete: TAction
      Tag = 1
      Caption = 'Delete...'
      Hint = 'Delete selected Preset'
      ShortCut = 46
      OnExecute = cmdDeleteExecute
      OnUpdate = cmdDoUpdate
    end
    object cmdRename: TAction
      Tag = 2
      Caption = 'Rename...'
      Hint = 'Rename selected Preset'
      ShortCut = 113
      OnExecute = cmdExecute
      OnUpdate = cmdDoUpdate
    end
    object cmdUpdate: TAction
      Tag = 3
      Caption = 'Update...'
      Enabled = False
      Hint = 'Update Preset'
      ShortCut = 116
      OnExecute = cmdUpdateExecute
    end
    object cmdMoveUp: TAction
      Tag = 4
      Caption = 'Move Up'
      Hint = 'Move selected Preset(s) up'
      ImageIndex = 27
      ShortCut = 16422
      OnExecute = cmdExecute
      OnUpdate = cmdMoveUpUpdate
    end
    object cmdMoveDown: TAction
      Tag = 5
      Caption = 'Move Down'
      Hint = 'Move selected Preset(s) down'
      ImageIndex = 28
      ShortCut = 16424
      OnExecute = cmdExecute
      OnUpdate = cmdMoveDownUpdate
    end
    object cmdSort: TAction
      Tag = 6
      Caption = 'Sort '
      Hint = 'Sort Presets'
      ImageIndex = 29
      OnExecute = cmdExecute
    end
  end
  object PmPresets: TSpTBXPopupMenu
    Left = 116
    Top = 128
    object SpTBXItem1: TSpTBXItem
      Action = cmdAdd
    end
    object SpTBXItem2: TSpTBXItem
      Action = cmdDelete
    end
    object SpTBXItem3: TSpTBXItem
      Action = cmdRename
    end
    object SpTBXItem4: TSpTBXItem
      Action = cmdUpdate
    end
    object SpTBXSeparatorItem1: TSpTBXSeparatorItem
    end
    object SpTBXItem7: TSpTBXItem
      Action = cmdMoveUp
    end
    object SpTBXItem6: TSpTBXItem
      Action = cmdMoveDown
    end
    object SpTBXItem5: TSpTBXItem
      Action = cmdSort
    end
  end
end
