unit uFrmPresetManager;

{
  Nanobrot

  Copyright (C) 2019 - 2020 Vasily Makarov

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Affero General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>.

}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Actions, ActnList, SpTBXItem, SpTBXControls, SpTBXEditors,
  SpTBXSkins, ImageList, ImgList, TB2Item, Vcl.Menus;

type

  TFrmPresetManager = class;
  TPresetManagerCmd = (pmcAdd, pmcDelete, pmcRename, pmcUpdate, pmcMoveUp,
    pmcMoveDown, pmcSort);
  TPresetManagerEvent = procedure(Sender: TFrmPresetManager;
    const aCmd: TPresetManagerCmd) of object;

  TFrmPresetManager = class(TForm)
    ActionList1: TActionList;
    SpTBXPanel1: TSpTBXPanel;
    SpTBXButton1: TSpTBXButton;
    LbPresets: TSpTBXListBox;
    cmdAdd: TAction;
    BtClose: TSpTBXButton;
    SpTBXButton2: TSpTBXButton;
    cmdDelete: TAction;
    SpTBXButton3: TSpTBXButton;
    cmdRename: TAction;
    SpTBXButton4: TSpTBXButton;
    cmdUpdate: TAction;
    SpTBXButton5: TSpTBXButton;
    SpTBXButton6: TSpTBXButton;
    cmdMoveUp: TAction;
    cmdMoveDown: TAction;
    SpTBXButton7: TSpTBXButton;
    cmdSort: TAction;
    PmPresets: TSpTBXPopupMenu;
    SpTBXItem1: TSpTBXItem;
    SpTBXItem2: TSpTBXItem;
    SpTBXItem3: TSpTBXItem;
    SpTBXItem4: TSpTBXItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    SpTBXItem5: TSpTBXItem;
    SpTBXItem6: TSpTBXItem;
    SpTBXItem7: TSpTBXItem;
    procedure cmdExecute(Sender: TObject);
    procedure cmdDoUpdate(Sender: TObject);
    procedure cmdDeleteExecute(Sender: TObject);
    procedure cmdUpdateExecute(Sender: TObject);
    procedure LbPresetsDrawItem(Sender: TObject; ACanvas: TCanvas;
      var ARect: TRect; Index: Integer; const State: TOwnerDrawState;
      const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
    procedure cmdMoveUpUpdate(Sender: TObject);
    procedure cmdMoveDownUpdate(Sender: TObject);
    procedure DrawButtonCaption(Sender: TObject; ACanvas: TCanvas;
      ClientAreaRect: TRect; State: TSpTBXSkinStatesType;
      var ACaption: WideString; var CaptionRect: TRect;
      var CaptionFormat: Cardinal; IsTextRotated: Boolean;
      const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
  private
    fCmdEvent: TPresetManagerEvent;
    function GetSelectedPresets: string;
    function GetFirstSelectedIndex: Integer;
    function GetLastSelectedIndex: Integer;
  public
    procedure UpdatePresetList(aPresets: TStrings);
    procedure SelectPresets(aIndexes: TList);
    property SelectedPresets: string read GetSelectedPresets;
  end;

var
  FrmPresetManager: TFrmPresetManager;

  procedure ShowPresetManager(aPresets: TStrings; Index: Integer; aCmd: TPresetManagerEvent);

implementation

{$R *.dfm}

uses Math, uMisc, uMain;

procedure ShowPresetManager(aPresets: TStrings; Index: Integer; aCmd: TPresetManagerEvent);
begin
  Assert(Assigned(aCmd));
  Application.CreateForm(TFrmPresetManager, FrmPresetManager);
  with FrmPresetManager do
  try
    fCmdEvent := aCmd;
    UpdatePresetList(aPresets);
    if (Index >= 0) and (Index < aPresets.Count) then
    begin
      LbPresets.Selected[Index] := true;
      cmdUpdate.Enabled := true;
    end;
    ShowModal;
  finally
    Release;
  end;
end;


procedure SpDrawListboxItem(L: TSpTBXListBox; Index: Integer; ARect: TRect;
  State: TOwnerDrawState; IL: TCustomImageList; ImageIndex: Integer);
var
  s: string;
  TextR, ILRect: TRect;
  Y: Integer;
  ACanvas: TCanvas;
begin
  ACanvas := L.Canvas;
  Inc(ARect.Left, 2);
  if Assigned(IL) then begin
    ILRect := Bounds(ARect.Left, ARect.Top + ((L.ItemHeight - IL.Height) div 2), IL.Width, IL.Height);
    // Draw icon shadow
    if odSelected in State then begin
      OffsetRect(ILRect, 1, 1);
      SpDrawIconShadow(ACanvas, ILRect, IL, ImageIndex);
      OffsetRect(ILRect, -2, -2);
    end;
    SpDrawImageList(ACanvas, ILRect, IL, ImageIndex, True, True);
  end;
  // Draw the caption
  Inc(ARect.Left, L.ItemHeight);
  s := L.Items[Index];
  TextR := ARect;
  Y := SpDrawXPText(ACanvas, s, TextR, DT_CALCRECT);
  ARect.Top := ARect.Top + ((L.ItemHeight - Y) div 2);
  SpDrawXPText(ACanvas, s, ARect, 0);
end;

procedure TFrmPresetManager.cmdDeleteExecute(Sender: TObject);
begin
  if QuestionDialog('Preset Manager',
  Format('Do you want to delete the Preset(s) "%s"?', [SelectedPresets])) = mrYes then
    fCmdEvent(Self, pmcDelete);
end;

procedure TFrmPresetManager.cmdDoUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := LbPresets.SelCount > 0;
end;

procedure TFrmPresetManager.cmdExecute(Sender: TObject);
begin
  if Assigned(fCmdEvent) then
    fCmdEvent(Self, TPresetManagerCmd(TAction(Sender).Tag));
end;

procedure TFrmPresetManager.cmdMoveDownUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (LbPresets.SelCount > 0)
    and InRange(GetLastSelectedIndex, 0, LbPresets.Count - 2);
end;

procedure TFrmPresetManager.cmdMoveUpUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (LbPresets.SelCount > 0)
    and InRange(GetFirstSelectedIndex, 1, LbPresets.Count - 1);
end;


procedure TFrmPresetManager.cmdUpdateExecute(Sender: TObject);
begin
  if QuestionDialog('Preset Manager',
  Format('Would you like to update the stored parameters for the "%s" preset with the current settings?', [SelectedPresets])) = mrYes then
    fCmdEvent(Self, pmcUpdate);
end;

function TFrmPresetManager.GetFirstSelectedIndex: Integer;
var
  i: Integer;
begin
  result := -1;
  with LbPresets do
    for i := 0 to Count - 1 do
      if Selected[i] then
        Exit(i);
end;

function TFrmPresetManager.GetLastSelectedIndex: Integer;
var
  i: Integer;
begin
  result := -1;
  with LbPresets do
    for i := Count - 1 downto 0 do
      if Selected[i] then
        Exit(i);
end;

function TFrmPresetManager.GetSelectedPresets: string;
var
  i: Integer;
begin
  result := '';
  with LbPresets do
    for i := 0 to Count - 1 do
      if Selected[i] then
        if result.IsEmpty then
          result := Items[i]
        else
          result := Format('%s, %s', [result, Items[i]]);
end;

procedure TFrmPresetManager.LbPresetsDrawItem(Sender: TObject; ACanvas: TCanvas;
  var ARect: TRect; Index: Integer; const State: TOwnerDrawState;
  const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
begin
  if PaintStage = pstPostPaint then
    Exit;
  PaintDefault := false;
  SpDrawListboxItem(Sender as TSpTBXListBox, Index, ARect, State,
    FrmMain.IlMain16, 24);
end;

procedure TFrmPresetManager.SelectPresets(aIndexes: TList);
var
  i, j: Integer;
begin
  if aIndexes.Count = 0 then
    Exit;
  for i := 0 to aIndexes.Count - 1 do
  begin
    j := Integer(IntPtr(aIndexes[i]));
    if j > -1 then
      LbPresets.Selected[j] := true;
  end;
end;

procedure TFrmPresetManager.DrawButtonCaption(Sender: TObject;
  ACanvas: TCanvas; ClientAreaRect: TRect; State: TSpTBXSkinStatesType;
  var ACaption: WideString; var CaptionRect: TRect; var CaptionFormat: Cardinal;
  IsTextRotated: Boolean; const PaintStage: TSpTBXPaintStage;
  var PaintDefault: Boolean);
begin
  ACaption := '';
end;

procedure TFrmPresetManager.UpdatePresetList(aPresets: TStrings);
begin
  with LbPresets.Items do
  try
    BeginUpdate;
    Clear;
    Assign(aPresets);
  finally
    EndUpdate;
  end;
end;

end.
