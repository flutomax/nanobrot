unit uFrmSoundArt;

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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus,
  StdCtrls, SpTBXEditors, ExtCtrls, SpTBXControls, TB2Dock, TB2Toolbar, TB2Item,
  SpTBXItem, SpTBXSkins, Actions, ActnList, uBase, uProgressBarEx, uSoundArtExport,
  GR32_Image, uNanobrot, uColorMap, uMisc, uFrmPresetManager;

type
  TInputQueryArray = array[0..0] of string;

  TFrmSoundArt = class(TForm)
    PnDock: TSpTBXPanel;
    SpTBXDock1: TSpTBXDock;
    SpTBXToolbar1: TSpTBXToolbar;
    PnExportSettings: TSpTBXPanel;
    LbGpHeader: TSpTBXLabel;
    GpGeneralSettings: TGridPanel;
    LbImgWidth: TSpTBXLabel;
    LbImgHeight: TSpTBXLabel;
    EdImgWidth: TSpTBXSpinEdit;
    EdImgHeight: TSpTBXSpinEdit;
    LbSweepAngle: TSpTBXLabel;
    EdSweepAngle: TSpTBXSpinEdit;
    StatusBar: TSpTBXStatusBar;
    SbLbReady: TSpTBXLabelItem;
    SpTBXSeparatorItem3: TSpTBXSeparatorItem;
    SbiTime: TSpTBXLabelItem;
    SbLbTime: TSpTBXLabelItem;
    SpTBXSeparatorItem4: TSpTBXSeparatorItem;
    SbiInfo: TSpTBXLabelItem;
    SbLbInfo: TSpTBXLabelItem;
    PnImgView: TSpTBXPanel;
    ImgView: TImgView32;
    ActionList1: TActionList;
    cmdRender: TAction;
    SpTBXItem1: TSpTBXItem;
    LbStartAngle: TSpTBXLabel;
    EdStartAngle: TSpTBXSpinEdit;
    DlgSaveImg: TFileSaveDialog;
    cmdFileExportImage: TAction;
    SpTBXItem2: TSpTBXItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    LbSupersample: TSpTBXLabel;
    EdSupersample: TSpTBXSpinEdit;
    LbFlipVertical: TSpTBXLabel;
    CkFlipVertical: TSpTBXCheckBox;
    smPreset: TSpTBXSubmenuItem;
    SpTBXItem3: TSpTBXItem;
    SpTBXItem4: TSpTBXItem;
    SpTBXSeparatorItem2: TSpTBXSeparatorItem;
    cmdPresetAdd: TAction;
    cmdPresetManager: TAction;
    cmdFileExportLevelMap: TAction;
    DlgSaveMap: TFileSaveDialog;
    SpTBXItem5: TSpTBXItem;
    SpTBXSeparatorItem5: TSpTBXSeparatorItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure EdLbClick(Sender: TObject);
    procedure EdImgWidthValueChanged(Sender: TObject);
    procedure EdSweepAngleValueChanged(Sender: TObject);
    procedure StatusBarDrawDockBackground(Sender: TObject; ACanvas: TCanvas;
      ARect: TRect; const PaintStage: TSpTBXPaintStage;
      var PaintDefault: Boolean);
    procedure cmdRenderExecute(Sender: TObject);
    procedure cmdRenderUpdate(Sender: TObject);
    procedure EdStartAngleValueChanged(Sender: TObject);
    procedure cmdFileExportImageExecute(Sender: TObject);
    procedure cmdEnabledUpdate(Sender: TObject);
    procedure EdSupersampleValueChanged(Sender: TObject);
    procedure CkFlipVerticalDrawCaption(Sender: TObject; ACanvas: TCanvas;
      ClientAreaRect: TRect; State: TSpTBXSkinStatesType;
      var ACaption: WideString; var CaptionRect: TRect;
      var CaptionFormat: Cardinal; IsTextRotated: Boolean;
      const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
    procedure CkFlipVerticalClick(Sender: TObject);
    procedure cmdPresetAddExecute(Sender: TObject);
    procedure smPresetPopup(Sender: TTBCustomItem; FromLink: Boolean);
    procedure PresetClick(Sender: TObject);
    procedure cmdPresetManagerExecute(Sender: TObject);
    procedure cmdFileExportLevelMapExecute(Sender: TObject);
  private
    fSAE: TSoundArtExport;
    fProgressBar: TProgressBarEx;
    fSavedCtrl: TSavedElement;
    fPresetIndex: Integer;
    function AllowChange: Boolean;
    function GetPresetName(const aName: string = ''): string;
    procedure DoMessage(Sender: TObject; const aStr: string);
    procedure DoProgress(Sender: TObject; Stage: TProgressStage;
      Progress: Integer; const Msg: string);
    procedure DoColorMapRequest(ColorMap: TColorMap; const aName: string);
    procedure DoImageResize(Sender: TObject);
    procedure DoRender(Sender: TObject);
    procedure DoDocChange(Sender: TObject);
    procedure DoDocLoad(Sender: TObject);
    procedure DoImgViewScaleChange(Sender: TObject);
    procedure DoCheckPresetName(Sender: TObject; const Values: array of string; var CanClose: Boolean);
    procedure DoPresetMngrCmd(Sender: TFrmPresetManager; const aCmd: TPresetManagerCmd);
  public
    property SAE: TSoundArtExport read fSAE;
  end;

var
  FrmSoundArt: TFrmSoundArt;

  procedure ShowSynth;

implementation

{$R *.dfm}

uses
  StrUtils, uMain;

procedure ShowSynth;
begin
  Application.CreateForm(TFrmSoundArt, FrmSoundArt);
  with FrmSoundArt do
  try
    ShowModal;
  finally
    Release;
  end;
end;

function TFrmSoundArt.AllowChange: Boolean;
begin
  result := ([csLoading, csDestroying] * ComponentState = []) and
    not SAE.Runned;
end;

procedure TFrmSoundArt.FormCreate(Sender: TObject);
begin
  ImgView.Bitmap.SetSize(1000, 500);
  fPresetIndex := -1;
  fSAE := TSoundArtExport.Create;
  fSAE.Bitmap := ImgView.Bitmap;
  fSAE.OnLoad := DoDocLoad;
  fSAE.OnChange := DoDocChange;
  fSAE.OnWork := DoRender;
  fSAE.OnResize := DoImageResize;
  fSAE.OnMessage := DoMessage;
  fSAE.OnProgress := DoProgress;
  fSAE.OnColorMapRequest := DoColorMapRequest;

  fProgressBar := TProgressBarEx.Create(self);
  fProgressBar.Parent := StatusBar;
  fProgressBar.SetBounds(3, 3, 150, StatusBar.Height - 5);
  fProgressBar.Position := 0;
  fProgressBar.Visible := false;
  PrepareStatusbar(Statusbar);
end;

procedure TFrmSoundArt.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fSAE);
end;

procedure TFrmSoundArt.FormShow(Sender: TObject);
begin
  SAE.Assign(FrmMain.Nanobrot);
  SAE.SetImgSize(Trunc(EdImgWidth.Value), Trunc(EdImgHeight.Value));
end;

procedure TFrmSoundArt.StatusBarDrawDockBackground(Sender: TObject;
  ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage;
  var PaintDefault: Boolean);
begin
  if PaintStage = pstPostPaint then
    Exit;
  DrawStatusbarBG(Sender as TSpTBXCustomStatusBar, ARect, ACanvas);
  PaintDefault := false;
end;

procedure TFrmSoundArt.DoColorMapRequest(ColorMap: TColorMap;
  const aName: string);
begin
  FrmMain.ColorMapManager.LoadColorMap(ColorMap, aName);
end;

procedure TFrmSoundArt.DoDocChange(Sender: TObject);
begin

end;

procedure TFrmSoundArt.DoDocLoad(Sender: TObject);
begin
  EdImgWidth.Value := SAE.ImgWidth;
  EdImgHeight.Value := SAE.ImgHeight;
  EdStartAngle.Value := SAE.StartAngle;
  EdSweepAngle.Value := SAE.SweepAngle;
  EdSupersample.Value := SAE.ImgSupersample;
  CkFlipVertical.Checked := SAE.FlipVertical;
end;

procedure TFrmSoundArt.DoImageResize(Sender: TObject);
begin
  ImgView.Bitmap.Clear;
  ImgView.Bitmap.SetSize(SAE.ImgWidth, SAE.ImgHeight);
  EdImgWidth.Value := SAE.ImgWidth;
  EdImgHeight.Value := SAE.ImgHeight;
end;

procedure TFrmSoundArt.DoImgViewScaleChange(Sender: TObject);
begin

end;

procedure TFrmSoundArt.DoMessage(Sender: TObject; const aStr: string);
begin
  SbLbInfo.Caption := aStr;
  Application.ProcessMessages;
end;


procedure TFrmSoundArt.DoProgress(Sender: TObject; Stage: TProgressStage;
  Progress: Integer; const Msg: string);
begin
  fProgressBar.Position := Progress;
  SbLbReady.Caption := IfThen(Stage = psEnding, 'Ready');
  SbLbTime.Caption := Msg;
  case Stage of
    psStarting: fProgressBar.Show;
    psRunning: fProgressBar.Refresh;
    psEnding: fProgressBar.Hide;
  end;
  Application.ProcessMessages;
end;


procedure TFrmSoundArt.DoRender(Sender: TObject);
begin
  if SAE.Runned then
    fSavedCtrl := TSavedElement.Create(Screen.ActiveControl);
  UpdateParamsCtrls(PnExportSettings, not SAE.Runned);
  if not SAE.Runned then
  begin
    fSavedCtrl.Extract;
    ImgView.ForceFullInvalidate;
  end;
end;

procedure TFrmSoundArt.EdImgWidthValueChanged(Sender: TObject);
begin
  if AllowChange then
    SAE.SetImgSize(Trunc(EdImgWidth.Value), Trunc(EdImgHeight.Value));
end;

procedure TFrmSoundArt.EdLbClick(Sender: TObject);
var
  L: TSpTBXLabel;
begin
  L := Sender as TSpTBXLabel;
  if Assigned(L.FocusControl) and (L.FocusControl.CanFocus) then
    L.FocusControl.SetFocus;
end;


procedure TFrmSoundArt.EdStartAngleValueChanged(Sender: TObject);
begin
  if AllowChange then
    SAE.StartAngle := EdStartAngle.Value;
end;

procedure TFrmSoundArt.EdSweepAngleValueChanged(Sender: TObject);
begin
  if AllowChange then
    SAE.SweepAngle := EdSweepAngle.Value;
end;

procedure TFrmSoundArt.EdSupersampleValueChanged(Sender: TObject);
begin
  if AllowChange then
    SAE.ImgSupersample := Trunc(EdSupersample.Value);
end;

procedure TFrmSoundArt.CkFlipVerticalClick(Sender: TObject);
begin
  if AllowChange then
    SAE.FlipVertical := CkFlipVertical.Checked;
end;

procedure TFrmSoundArt.cmdRenderExecute(Sender: TObject);
begin
  if SAE.Runned then begin
    SAE.Stop;
    fProgressBar.Hide;
    SbLbTime.Caption := '';
    SbLbInfo.Caption := '';
    SbLbReady.Caption := 'Stopping...';
    exit;
  end;
  SAE.Start;
end;

procedure TFrmSoundArt.cmdRenderUpdate(Sender: TObject);
begin
  if SAE.Runned then begin
    cmdRender.ImageIndex := 1;
    cmdRender.Caption := 'Cancel';
    cmdRender.Hint := 'Cancel current job';
    cmdRender.ShortCut := TextToShortCut('Esc');
  end else begin
    cmdRender.ImageIndex := 0;
    cmdRender.Caption := 'Render';
    cmdRender.Hint := 'Render fractal';
    cmdRender.ShortCut := TextToShortCut('F9');
  end;
end;


procedure TFrmSoundArt.CkFlipVerticalDrawCaption(Sender: TObject;
  ACanvas: TCanvas; ClientAreaRect: TRect; State: TSpTBXSkinStatesType;
  var ACaption: WideString; var CaptionRect: TRect; var CaptionFormat: Cardinal;
  IsTextRotated: Boolean; const PaintStage: TSpTBXPaintStage;
  var PaintDefault: Boolean);
begin
  ACaption := BoolToStr(TSpTBXCheckBox(Sender).Checked, True);
end;

procedure TFrmSoundArt.cmdEnabledUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not SAE.Runned;
end;

procedure TFrmSoundArt.cmdFileExportImageExecute(Sender: TObject);
begin
  Application.ProcessMessages;
  with DlgSaveImg do
    if Length(FileName) > 0 then
      FileName := ExtractFileName(FileName);
  if not DlgSaveImg.Execute(Handle) then
    Exit;
  if not AccessFileOnWrite(DlgSaveImg.FileName) then
    Exit;
  SbLbInfo.Caption := 'Saving Image...';
  Screen.Cursor := crHourGlass;
  try
    ExportImage(DlgSaveImg.FileName, ImgView.Bitmap);
    SbLbInfo.Caption := 'Image saved';
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFrmSoundArt.cmdFileExportLevelMapExecute(Sender: TObject);
begin
  Application.ProcessMessages;
  with DlgSaveMap do
    if Length(FileName) > 0 then
      FileName := ExtractFileName(FileName);
  if not DlgSaveMap.Execute(Handle) then
    Exit;
  if not AccessFileOnWrite(DlgSaveMap.FileName) then
    Exit;
  SbLbInfo.Caption := 'Saving Maps file...';
  Screen.Cursor := crHourGlass;
  try
    SAE.ExportLevelMap(DlgSaveMap.FileName);
    SbLbInfo.Caption := 'Maps file saved';
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFrmSoundArt.PresetClick(Sender: TObject);
begin
  if Sender is TSpTBXItem then
    if SAE.LoadPreset(TSpTBXItem(Sender).Caption) then
      fPresetIndex := TSpTBXItem(Sender).Tag;
end;

procedure TFrmSoundArt.smPresetPopup(Sender: TTBCustomItem; FromLink: Boolean);
var
  i: integer;
  s: TStrings;
  m: TSpTBXItem;
begin
  for i := smPreset.Count - 1 downto 3 do
    smPreset.Delete(i);
  s := TStringList.Create;
  try
    SAE.ReadPresets(s);
    for i := 0 to s.Count - 1 do
    begin
      m := TSpTBXItem.Create(smPreset);
      m.Caption := s[i];
      m.Tag := i;
      m.GroupIndex := 1;
      m.RadioItem := true;
      m.Checked := i = fPresetIndex;
      m.Enabled := not SAE.Runned;
      m.OnClick := PresetClick;
      smPreset.Add(m);
    end;
  finally
    s.Free;
  end;
end;


procedure TFrmSoundArt.DoCheckPresetName(Sender: TObject;
  const Values: array of string; var CanClose: Boolean);
var
  s: string;
  i: integer;
begin
  s := Values[0].Trim;
  if s.IsEmpty then
  begin
    CanClose := false;
    MessageDlg('Preset Name can not be empty.', mtWarning, [mbOk], 0);
  end else begin
    CanClose := not SAE.PresetExists(s);
    if not CanClose then
      MessageDlg(Format('Preset "%s" already exist.' + #13#10 +
        'Enter other Name or rename existance.', [s]), mtWarning, [mbOk], 0);
  end;
  if not CanClose and Assigned(Screen.ActiveForm) then
    with Screen.ActiveForm do
      for i := 0 to ControlCount - 1 do
          if Controls[i] is TEdit then
          begin
            if TEdit(Controls[i]).CanFocus then
              TEdit(Controls[i]).SetFocus;
            break;
          end;
end;


function TFrmSoundArt.GetPresetName(const aName: string): string;
const
  aPrompts: TInputQueryArray = ('Preset Name:');
var
  aValues: TInputQueryArray;
begin
  result := '';
  aValues[0] := aName;
  if not InputQuery('Add Preset', aPrompts, aValues, DoCheckPresetName) then
    Exit;
  result := aValues[0].Trim;
end;

procedure TFrmSoundArt.cmdPresetAddExecute(Sender: TObject);
begin
  if SAE.UpdatePreset(GetPresetName) then
    SAE.UpdatePresets;
end;


procedure TFrmSoundArt.DoPresetMngrCmd(Sender: TFrmPresetManager;
  const aCmd: TPresetManagerCmd);
var
  s: TStrings;
  el: TList;
  pn, old: string;
  i: Integer;
  result: Boolean;
begin
  result := false;
  el := TList.Create;
  s := TStringList.Create;
  try
    SAE.ReadPresets(s);
    case aCmd of
      pmcAdd:
      begin
        pn := GetPresetName;
        result := SAE.UpdatePreset(pn);
        if result then
        begin
          i := SAE.PresetIndex(pn);
          if i >= 0 then
            el.Add(Pointer(IntPtr(i)));
        end;
      end;
      
      pmcDelete: 
      begin
        for i := s.Count - 1 downto 0 do
          if Sender.LbPresets.Selected[i] then
            SAE.DeletePreset(s[i]);
        result := true;
      end;
      
      pmcRename: 
      begin
        result := Sender.LbPresets.SelCount > 0;
        if result then
        begin
          old := s[Sender.LbPresets.ItemIndex];
          pn := GetPresetName(old);
          result := not pn.IsEmpty;
          if result then
          begin
            SAE.RenamePreset(old, pn);
            i := SAE.PresetIndex(pn);
            if i >= 0 then
              el.Add(Pointer(IntPtr(i)));
          end;
        end;
      end;

      pmcUpdate:
      begin
        pn := s[fPresetIndex];
        result := SAE.UpdatePreset(pn);
        if result then
        begin
          i := SAE.PresetIndex(pn);
          if i >= 0 then
            el.Add(Pointer(IntPtr(i)));
        end;
      end;

      pmcMoveUp, pmcMoveDown:
      begin
        for i := 0 to s.Count - 1 do
          if Sender.LbPresets.Selected[i] then
            el.Add(Pointer(IntPtr(i)));
        result := SAE.MovePresets(el, aCmd = pmcMoveUp);
      end;

      pmcSort:
      begin
        result := SAE.SortPresets;
      end;

    end;
    if result then
    begin
      SAE.ReadPresets(s);
      SAE.UpdatePresets;
      Sender.UpdatePresetList(s);
      Sender.SelectPresets(el);
    end;
  finally
    el.Free;
    s.Free;
  end;
end;

procedure TFrmSoundArt.cmdPresetManagerExecute(Sender: TObject);
var
  s: TStrings;
begin
  s := TStringList.Create;
  try
    SAE.ReadPresets(s);
    ShowPresetManager(s, fPresetIndex, DoPresetMngrCmd);
  finally
    s.Free;
  end;
end;


end.
