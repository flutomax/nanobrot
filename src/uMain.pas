unit uMain;

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
  StdCtrls, ExtCtrls, ComCtrls, CommCtrl, Menus, Actions, ActnList, XPMan, GR32,
  GR32_Image, ToolWin, ImageList, ImgList, ExtDlgs, StdActns, ShlObj, TB2Item,
  TB2Dock, TB2Toolbar, SpTBXControls, SpTBXEditors, SpTBXItem, SpTBXDkPanels,
  SpTBXPageScroller, uBase, uProgressBarEx, uNanobrot, uColorMap, uMisc,
  AppEvnts, SpTBXCustomizer, SpTBXMDIMRU;

type

  TFrmMain = class(TForm)
    ActionList1: TActionList;
    IlMainN: TImageList;
    cmdRender: TAction;
    ImgView: TImgView32;
    cmdFileExportImage: TAction;
    cmdFileNew: TAction;
    cmdFileExit: TFileExit;
    GpGeneralSettings: TGridPanel;
    LbImgWidth: TSpTBXLabel;
    LbImgHeight: TSpTBXLabel;
    GpFractalProperties: TGridPanel;
    LbCenterX: TSpTBXLabel;
    LbCenterY: TSpTBXLabel;
    EdCenterX: TSpTBXEdit;
    EdCenterY: TSpTBXEdit;
    LbZoom: TSpTBXLabel;
    EdZoom: TSpTBXEdit;
    LbMaxIters: TSpTBXLabel;
    LbPeriod: TSpTBXLabel;
    LbOrderM: TSpTBXLabel;
    LbOrderN: TSpTBXLabel;
    LbJitter: TSpTBXLabel;
    GpRenderProperties: TGridPanel;
    LbSupersample: TSpTBXLabel;
    cmdViewRecoloring: TAction;
    LbRenderProperties: TSpTBXLabel;
    LbFpHeader: TSpTBXLabel;
    LbGpHeader: TSpTBXLabel;
    cmdEditCut: TEditCut;
    cmdEditCopy: TEditCopy;
    cmdEditPaste: TEditPaste;
    cmdEditSelectAll: TEditSelectAll;
    cmdEditDelete: TEditDelete;
    cmdFileSaveAs: TAction;
    cmdFileSave: TAction;
    cmdFileOpen: TAction;
    LbOutColoring: TSpTBXLabel;
    cmdView100: TAction;
    cmdView50: TAction;
    cmdView25: TAction;
    cmdView12: TAction;
    cmdView200: TAction;
    cmdView400: TAction;
    cmdView800: TAction;
    cmdView1600: TAction;
    IlSmall: TImageList;
    cmdFileImport: TAction;
    cmdView6: TAction;
    cmdView66: TAction;
    cmdView150: TAction;
    cmdView300: TAction;
    LblColorMaps: TSpTBXLabel;
    cmdFileShowLocation: TAction;
    cmdFileExport: TAction;
    PnRightCntr: TSpTBXPanel;
    SpMain: TSpTBXSplitter;
    PnGeneralProperties: TSpTBXPanel;
    PnFractalProperties: TSpTBXPanel;
    PnRenderingProperties: TSpTBXPanel;
    PnLeftDk: TSpTBXPanel;
    PnImageView: TSpTBXPanel;
    PnRightDk: TSpTBXPanel;
    DkTop: TSpTBXDock;
    TbFile: TSpTBXToolbar;
    TbMainMenu: TSpTBXToolbar;
    miFile: TSpTBXSubmenuItem;
    SpTBXItem1: TSpTBXItem;
    SpTBXItem2: TSpTBXItem;
    SpTBXItem3: TSpTBXItem;
    SpTBXItem4: TSpTBXItem;
    SpTBXItem5: TSpTBXItem;
    SpTBXItem6: TSpTBXItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    SpTBXSeparatorItem2: TSpTBXSeparatorItem;
    SpTBXItem7: TSpTBXItem;
    SpTBXItem8: TSpTBXItem;
    msRfTop: TSpTBXSeparatorItem;
    msRfBot: TSpTBXSeparatorItem;
    SpTBXItem9: TSpTBXItem;
    SpTBXItem10: TSpTBXItem;
    SpTBXItem11: TSpTBXItem;
    SpTBXItem12: TSpTBXItem;
    SpTBXItem13: TSpTBXItem;
    SpTBXItem14: TSpTBXItem;
    SpTBXSubmenuItem2: TSpTBXSubmenuItem;
    SpTBXSubmenuItem3: TSpTBXSubmenuItem;
    SpTBXItem15: TSpTBXItem;
    SpTBXItem16: TSpTBXItem;
    SpTBXItem17: TSpTBXItem;
    SpTBXItem18: TSpTBXItem;
    SpTBXItem19: TSpTBXItem;
    SpTBXItem20: TSpTBXItem;
    SpTBXItem21: TSpTBXItem;
    SpTBXSeparatorItem5: TSpTBXSeparatorItem;
    SmiMagnification: TSpTBXSubmenuItem;
    TbEdit: TSpTBXToolbar;
    SpTBXItem22: TSpTBXItem;
    SpTBXItem23: TSpTBXItem;
    SpTBXItem24: TSpTBXItem;
    TbView: TSpTBXToolbar;
    SiMaginification: TSpTBXSubmenuItem;
    SpTBXItem26: TSpTBXItem;
    SpTBXSeparatorItem6: TSpTBXSeparatorItem;
    SpTBXItem27: TSpTBXItem;
    StatusBar: TSpTBXStatusBar;
    SbLbReady: TSpTBXLabelItem;
    SbiTime: TSpTBXLabelItem;
    SpTBXSeparatorItem3: TSpTBXSeparatorItem;
    SbLbTime: TSpTBXLabelItem;
    SpTBXSeparatorItem4: TSpTBXSeparatorItem;
    SbiInfo: TSpTBXLabelItem;
    SbLbInfo: TSpTBXLabelItem;
    SpTBXRightAlignSpacerItem1: TSpTBXRightAlignSpacerItem;
    SpTBXSeparatorItem7: TSpTBXSeparatorItem;
    SbiZoom: TSpTBXLabelItem;
    SbLbZoom: TSpTBXLabelItem;
    SpTBXItem25: TSpTBXItem;
    SpTBXItem39: TSpTBXItem;
    SpTBXItem38: TSpTBXItem;
    SpTBXItem37: TSpTBXItem;
    SpTBXItem36: TSpTBXItem;
    SpTBXItem35: TSpTBXItem;
    SpTBXItem34: TSpTBXItem;
    SpTBXItem33: TSpTBXItem;
    SpTBXItem32: TSpTBXItem;
    SpTBXItem31: TSpTBXItem;
    SpTBXItem30: TSpTBXItem;
    SpTBXItem29: TSpTBXItem;
    SpTBXSubmenuItem1: TSpTBXSubmenuItem;
    EdImgWidth: TSpTBXSpinEdit;
    EdImgHeight: TSpTBXSpinEdit;
    LbColorMaps: TSpTBXListBox;
    EdIterations: TSpTBXSpinEdit;
    EdOrderM: TSpTBXSpinEdit;
    EdOrderN: TSpTBXSpinEdit;
    EdSupersample: TSpTBXSpinEdit;
    EdMapRange: TSpTBXSpinEdit;
    LbMapRange: TSpTBXLabel;
    LbMapOffset: TSpTBXLabel;
    EdMapOffset: TSpTBXSpinEdit;
    CbOutColoring: TSpTBXComboBox;
    CbJitter: TSpTBXComboBox;
    PnMain: TSpTBXPanel;
    DlgFindPeriod: TTaskDialog;
    EdPeriod: TSpTBXButtonEdit;
    cmdToolFindPeriod: TAction;
    SpTBXLabelItem1: TSpTBXLabelItem;
    lbShowSlopes: TSpTBXLabel;
    CkShowSlopes: TSpTBXCheckBox;
    LbSlopePower: TSpTBXLabel;
    EdSlopePower: TSpTBXSpinEdit;
    LbSlopeRatio: TSpTBXLabel;
    EdSlopeRatio: TSpTBXSpinEdit;
    LbSlopeAngle: TSpTBXLabel;
    EdSlopeAngle: TSpTBXSpinEdit;
    PsLeftDk: TSpTBXPageScroller;
    cmdEditUndo: TAction;
    cmdEditRedo: TAction;
    SpTBXSeparatorItem8: TSpTBXSeparatorItem;
    SpTBXItem28: TSpTBXItem;
    SpTBXItem40: TSpTBXItem;
    SpTBXSeparatorItem9: TSpTBXSeparatorItem;
    SpTBXItem41: TSpTBXItem;
    SpTBXItem42: TSpTBXItem;
    DlgSaveFractal: TFileSaveDialog;
    IlMain16: TImageList;
    SpTBXSubmenuItem4: TSpTBXSubmenuItem;
    cmdFileRevert: TAction;
    SpTBXSeparatorItem10: TSpTBXSeparatorItem;
    SpTBXItem43: TSpTBXItem;
    SpTBXItem44: TSpTBXItem;
    SpTBXSeparatorItem11: TSpTBXSeparatorItem;
    cmdFilePrint: TAction;
    SpTBXSeparatorItem12: TSpTBXSeparatorItem;
    SpTBXItem45: TSpTBXItem;
    SpTBXItem46: TSpTBXItem;
    SpTBXSeparatorItem13: TSpTBXSeparatorItem;
    AppEvents: TApplicationEvents;
    TbTools: TSpTBXToolbar;
    SpTBXItem47: TSpTBXItem;
    SpTBXLabelItem2: TSpTBXLabelItem;
    SpTBXItem48: TSpTBXItem;
    cmdToolOptions: TAction;
    SpTBXItem49: TSpTBXItem;
    Customizer: TSpTBXCustomizer;
    SpTBXSeparatorItem14: TSpTBXSeparatorItem;
    SpTBXSubmenuItem5: TSpTBXSubmenuItem;
    SpTBXItem50: TSpTBXItem;
    SpTBXItem51: TSpTBXItem;
    SpTBXItem52: TSpTBXItem;
    SpTBXItem53: TSpTBXItem;
    cmdViewCustomizerReset: TAction;
    SpTBXSeparatorItem15: TSpTBXSeparatorItem;
    SpTBXItem54: TSpTBXItem;
    cmdToolsUpdateColorMaps: TAction;
    SpTBXItem55: TSpTBXItem;
    SpTBXItem56: TSpTBXItem;
    cmdHelpAbout: TAction;
    SpTBXItem57: TSpTBXItem;
    cmdToolSoundArtExport: TAction;
    SpTBXItem58: TSpTBXItem;
    SpTBXSeparatorItem16: TSpTBXSeparatorItem;
    DlgSaveImg: TFileSaveDialog;
    DlgOpenFractal: TFileOpenDialog;
    DlgImport: TFileOpenDialog;
    DlgExport: TFileSaveDialog;
    SpTBXMRUListItem1: TSpTBXMRUListItem;
    cmdViewSmallIcons: TAction;
    SpTBXItem59: TSpTBXItem;
    cmdHelpHomePage: TAction;
    SpTBXItem60: TSpTBXItem;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure EdLbClick(Sender: TObject);
    procedure EdFloatKeyPress(Sender: TObject; var Key: Char);
    procedure cmdRenderExecute(Sender: TObject);
    procedure cmdRenderUpdate(Sender: TObject);
    procedure cmdFileExportImageExecute(Sender: TObject);
    procedure cmdFileNewExecute(Sender: TObject);
    procedure LbColorMapsChange(Sender: TObject);
    procedure cmdViewRecoloringExecute(Sender: TObject);
    procedure cmdFileSaveAsExecute(Sender: TObject);
    procedure EdCenterXChange(Sender: TObject);
    procedure EdZoomChange(Sender: TObject);
    procedure EdMaxItersChange(Sender: TObject);
    procedure EdPeriodChange(Sender: TObject);
    procedure EdOrderMChange(Sender: TObject);
    procedure EdOrderNChange(Sender: TObject);
    procedure CbJitterChange(Sender: TObject);
    procedure EdSupersampleChange(Sender: TObject);
    procedure EdMapRangeChange(Sender: TObject);
    procedure EdMapOffsetChange(Sender: TObject);
    procedure EdCenterYChange(Sender: TObject);
    procedure cmdFileSaveExecute(Sender: TObject);
    procedure cmdFileSaveUpdate(Sender: TObject);
    procedure cmdFileOpenExecute(Sender: TObject);
    procedure cmdEnabledUpdate(Sender: TObject);
    procedure CbOutColoringChange(Sender: TObject);
    procedure cmdView100Execute(Sender: TObject);
    procedure StatusBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure cmdFileImportExecute(Sender: TObject);
    procedure cmdViewRecoloringUpdate(Sender: TObject);
    procedure EdImgWidthChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure LbColorMapsData(Control: TWinControl; Index: Integer;
      var Data: string);
    function LbColorMapsDataFind(Control: TWinControl;
      FindString: string): Integer;
    procedure cmdFileShowLocationExecute(Sender: TObject);
    procedure cmdFileShowLocationUpdate(Sender: TObject);
    procedure cmdFileExportExecute(Sender: TObject);
    procedure SmiMagnificationPopup(Sender: TTBCustomItem; FromLink: Boolean);
    procedure LbColorMapsDrawItem(Sender: TObject; ACanvas: TCanvas;
      var ARect: TRect; Index: Integer; const State: TOwnerDrawState;
      const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
    procedure StatusBarDrawDockBackground(Sender: TObject; ACanvas: TCanvas;
      ARect: TRect; const PaintStage: TSpTBXPaintStage;
      var PaintDefault: Boolean);
    procedure cmdToolFindPeriodExecute(Sender: TObject);
    procedure DlgFindPeriodTimer(Sender: TObject; TickCount: Cardinal;
      var Reset: Boolean);
    procedure CkShowSlopesClick(Sender: TObject);
    procedure EdSlopePowerValueChanged(Sender: TObject);
    procedure EdSlopeRatioValueChanged(Sender: TObject);
    procedure EdSlopeAngleValueChanged(Sender: TObject);
    procedure cmdEditUndoExecute(Sender: TObject);
    procedure cmdEditUndoUpdate(Sender: TObject);
    procedure cmdEditRedoExecute(Sender: TObject);
    procedure cmdEditRedoUpdate(Sender: TObject);
    procedure DlgSaveFractalExecute(Sender: TObject);
    procedure DlgSaveFractalFileOkClick(Sender: TObject; var CanClose: Boolean);
    procedure cmdFileRevertExecute(Sender: TObject);
    procedure cmdFilePrintExecute(Sender: TObject);
    procedure AppEventsHint(Sender: TObject);
    procedure cmdToolOptionsExecute(Sender: TObject);
    procedure cmdViewCustomizerResetExecute(Sender: TObject);
    procedure cmdToolsUpdateColorMapsExecute(Sender: TObject);
    procedure cmdHelpAboutExecute(Sender: TObject);
    procedure cmdToolSoundArtExportExecute(Sender: TObject);
    procedure cmdViewSmallIconsExecute(Sender: TObject);
    procedure cmdViewSmallIconsUpdate(Sender: TObject);
    procedure cmdHelpHomePageExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    fNanobrot: TNanobrot;
    fMRUList: TMRUList;
    fColorMapManager: TColorMapManager;
    fIniFileName: string;
    fProgressBar: TProgressBarEx;
    fSavedCtrl: TSavedElement;
    fFileAssociation: Boolean;
    fSmallIcons: Boolean;
    fStarted: Boolean;
    function AllowChange: Boolean;
    procedure CheckFileSave;
    procedure DoMessage(Sender: TObject; const aStr: string);
    procedure DoProgress(Sender: TObject; Stage: TProgressStage;
      Progress: Integer; const Msg: string);
    procedure DoPeriodProgress(Sender: TObject; Stage: TProgressStage;
      Progress: Integer; const Msg: string);
    procedure DoColorMapManagerLoad(Sender: TObject);
    procedure DoColorMapRequest(ColorMap: TColorMap; const aName: string);
    procedure DoImageResize(Sender: TObject);
    procedure DoWork(Sender: TObject);
    procedure DoRecoloring(Sender: TObject);
    procedure DoDocChange(Sender: TObject);
    procedure DoDocLoad(Sender: TObject);
    procedure DoRecentFileClick(Sender: TObject; const aFileName: string);
    procedure DoImgViewScaleChange(Sender: TObject);
    procedure DoFindPeriod(Sender: TObject);
    procedure DoDlgSaveClickButton(const pfdc: IFileDialogCustomize; dwIDCtl: DWORD);
    procedure UpdateShowSlopes;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure SmallIconsOnToolbar(small: Boolean);
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFractal(const aFileName: string);
    procedure SaveFractal(const aFileName: string);
    property ColorMapManager: TColorMapManager read fColorMapManager;
    property Nanobrot: TNanobrot read fNanobrot;
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.dfm}

uses
  ShellApi, StrUtils, Math, IniFiles, GR32_Resamplers, uFrmSaveAdvanced,
  uFrmPrint, uFrmOptions, uFrmAbout, uFrmSoundArt, uHelpers;

const
  def_ext_io: array[1..4] of string = ('.kfr', '.kfb', '.nbm', '');
  def_ext_im: array[1..7] of string = ('.png', '.jpg', '.tif', '.gif', '.bmp', '.wdp', '');


constructor TFrmMain.Create(AOwner: TComponent);
begin
  inherited;
  fStarted := false;
  fIniFileName := ChangeFileExt(Application.ExeName, '.ini');
  fColorMapManager := TColorMapManager.Create(self);
  fColorMapManager.OnLoad := DoColorMapManagerLoad;
  fNanobrot := TNanobrot.Create;
  fNanobrot.OnLoad := DoDocLoad;
  fNanobrot.OnChange := DoDocChange;
  fNanobrot.OnWork := DoWork;
  fNanobrot.OnRecoloring := DoRecoloring;
  fNanobrot.OnResize := DoImageResize;
  fNanobrot.OnMessage := DoMessage;
  fNanobrot.OnProgress := DoProgress;
  fNanobrot.OnColorMapRequest := DoColorMapRequest;
  fNanobrot.OnPeriodProgress := DoPeriodProgress;
  fNanobrot.OnFindPeriod := DoFindPeriod;
  fProgressBar := TProgressBarEx.Create(self);
  fProgressBar.Parent := StatusBar;
  fProgressBar.SetBounds(3, 3, 150, StatusBar.Height - 5);
  fProgressBar.Position := 0;
  fProgressBar.Visible := false;
  fMRUList := TMRUList.Create(self);
  fMRUList.MIRecent := miFile;
  fMRUList.TopSepar := msRfTop;
  fMRUList.BotSepar := msRfBot;
  fMRUList.OnRecent := DoRecentFileClick;
  ImgView.Bitmap.SetSize(500, 500);
  ImgView.OnScaleChange := DoImgViewScaleChange;
  ImgView.Layers.MouseEvents := false;
  fNanobrot.Bitmap := ImgView.Bitmap;
  PrepareStatusbar(Statusbar);
  EdPeriod.EditButton.Caption := '';
end;

destructor TFrmMain.Destroy;
begin
  FreeAndNil(fNanobrot);
  inherited;
end;

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  DragAcceptFiles(Handle, true);
  LoadSettings;
  Nanobrot.LoadDefault;
  LbColorMaps.ItemIndex := ColorMapManager.List.IndexOf(DEF_COLORMAP);
  if ParamCount > 0 then
    LoadFractal(Paramstr(1));
end;

procedure TFrmMain.FormShow(Sender: TObject);
var
  ini: TIniFileEx;
begin
  if fStarted then
    Exit;
  // on first run
  if not FileExists(fIniFileName) then
  begin
    CenterWindow(Handle);
    fFileAssociation := true;
    SetFileAssociation(fFileAssociation);
  end else begin
    ini := TIniFileEx.Create(fIniFileName);
    try
      Customizer.Load(ini);
    finally
      ini.Free;
    end;
  end;
  fMRUList.UpdateRecentFiles;
  fStarted := true;
end;

procedure TFrmMain.WMDropFiles(var Msg: TWMDropFiles);
var
  cFileName: array[0..MAX_PATH] of Char;
begin
  try
    if DragQueryFile(Msg.Drop, 0, cFileName, MAX_PATH) > 0 then
    begin
      Application.BringToFront;
      CheckFileSave;
      LoadFractal(cFileName);
      Msg.Result := 0;
    end;
  finally
    DragFinish(Msg.Drop);
  end;
end;

procedure TFrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Nanobrot.Stop;
  SaveSettings;
end;

procedure TFrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Nanobrot.Runned then
    if QuestionDialog('Rendering in progress',
        'Do you want to stop rendering and quits the application?') = mrNo then
      exit;
  try
    CheckFileSave;
  except
    CanClose := false;
  end;
end;

procedure TFrmMain.LoadSettings;
var
  ini: TIniFileEx;
  w, h: Integer;
begin
  ini := TIniFileEx.Create(fIniFileName);
  try
    PnLeftDk.Width := ini.ReadInteger('General', 'SplitterPos', PnLeftDk.Width);
    Application.ShowHint := ini.ReadBool('General', 'ShowHints', true);
    Nanobrot.SoundOnDone := ini.ReadBool('General', 'SoundOnDone', true);
    fFileAssociation := ini.ReadBool('General', 'FileAssociation', false);
    Customizer.SaveFormState := ini.ReadBool('General', 'SaveWindowPos', true);
    StatusBar.Visible := ini.ReadBool('General', 'StatusBar', true);
    SmallIconsOnToolbar(ini.ReadBool('General', 'SmallIcons', false));
    Nanobrot.RasterizerType := TRasterizerType(ini.ReadInteger('General', 'RasterizerTypeIndex', 1));
    DlgSaveImg.FileTypeIndex := ini.ReadInteger('Dialogs', 'SaveImgIndex', DlgSaveImg.FileTypeIndex);
    DlgExport.FileTypeIndex := ini.ReadInteger('Dialogs', 'ExportIndex', DlgExport.FileTypeIndex);
    DlgImport.FileTypeIndex := ini.ReadInteger('Dialogs', 'ImportIndex', DlgImport.FileTypeIndex);
    Nanobrot.Level.IncludeLevelMaps := ini.ReadBool('Dialogs', 'IncludeLevelMap', false);
    w := ini.ReadInteger('Image', 'Width', 500);
    h := ini.ReadInteger('Image', 'Height', 500);
    Nanobrot.SetSize(w, h);
    ColorMapManager.Directory := ini.ReadString('ColorMapManager', 'Path', '');
    fMRUList.LoadFromIni(ini);
  finally
    ini.Free;
  end;
end;

procedure TFrmMain.SaveSettings;
var
  ini: TIniFileEx;
begin
  ini := TIniFileEx.Create(fIniFileName);
  try
    ini.WriteBool('General', 'FileAssociation', fFileAssociation);
    ini.WriteBool('General', 'SaveWindowPos', Customizer.SaveFormState);
    ini.WriteBool('General', 'ShowHints', Application.ShowHint);
    ini.WriteBool('General', 'SoundOnDone', Nanobrot.SoundOnDone);
    ini.WriteInteger('General', 'SplitterPos', PnLeftDk.Width);
    ini.WriteBool('General', 'StatusBar', StatusBar.Visible);
    ini.WriteBool('General', 'SmallIcons', fSmallIcons);
    ini.WriteInteger('General', 'RasterizerTypeIndex', Ord(Nanobrot.RasterizerType));
    ini.WriteInteger('Dialogs', 'SaveImgIndex', DlgSaveImg.FileTypeIndex);
    ini.WriteInteger('Dialogs', 'ExportIndex', DlgExport.FileTypeIndex);
    ini.WriteInteger('Dialogs', 'ImportIndex', DlgImport.FileTypeIndex);
    ini.WriteBool('Dialogs', 'IncludeLevelMap', Nanobrot.Level.IncludeLevelMaps);
    ini.WriteFloat('Image', 'Width', EdImgWidth.Value);
    ini.WriteFloat('Image', 'Height', EdImgHeight.Value);
    ini.WriteString('ColorMapManager', 'Path', ColorMapManager.Directory);
    fMRUList.SaveToIni(ini);
    Customizer.Save(ini);
    ini.UpdateFile;
  finally
    ini.Free;
  end;
end;

procedure TFrmMain.SmallIconsOnToolbar(small: Boolean);
var
  i: TCustomImageList;
begin
  if fSmallIcons = small then
    Exit;
  fSmallIcons := small;
  if fSmallIcons then
    i := IlMain16
  else
    i := IlMainN;
  TbFile.Images := i;
  TbEdit.Images := i;
  TbView.Images := i;
  TbTools.Images := i;
  DkTop.ArrangeToolbars;
end;

procedure TFrmMain.SmiMagnificationPopup(Sender: TTBCustomItem;
  FromLink: Boolean);
var
  i: Integer;
begin
  for i := 0 to SmiMagnification.Count - 1 do
    if SmiMagnification.Items[i] is TSpTBXItem then
      TSpTBXItem(SmiMagnification.Items[i]).CustomHeight :=
        IfThen(FromLink, GetSystemMetrics(SM_CYMENUSIZE) + 2, -1);
end;

procedure TFrmMain.LbColorMapsDrawItem(Sender: TObject; ACanvas: TCanvas;
  var ARect: TRect; Index: Integer; const State: TOwnerDrawState;
  const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
begin
  PaintDefault := false;
  if PaintStage = pstPrePaint then
    Exit;
  ColorMapManager.DrawListBoxItem(LbColorMaps, Index, ARect, State);
end;

procedure TFrmMain.DoRecentFileClick(Sender: TObject; const aFileName: string);
begin
  Application.ProcessMessages;
  LoadFractal(aFileName);
end;

procedure TFrmMain.DoDocChange(Sender: TObject);
const
  sCh: array[boolean] of string = ('', '*');
begin
  Caption := Format('Nanobrot - %s%s', [sCh[Nanobrot.Modified], ExtractFileName(Nanobrot.FileName)]);
end;

procedure TFrmMain.DoDocLoad(Sender: TObject);
begin
  EdCenterX.Text := Nanobrot.CenterX;
  EdCenterY.Text := Nanobrot.CenterY;
  EdZoom.Text := Nanobrot.Zoom;
  EdIterations.Value := Nanobrot.Iterations;
  EdPeriod.Text := Nanobrot.Period.ToString;
  EdOrderM.Value := Nanobrot.OrderM;
  EdOrderN.Value := Nanobrot.OrderN;
  CbJitter.ItemIndex := Nanobrot.Jitter;
  EdSupersample.Value := Nanobrot.Supersample;
  CbOutColoring.ItemIndex := Nanobrot.OutColoring;
  LbColorMaps.ItemIndex := ColorMapManager.List.IndexOf(Nanobrot.ColorMapFn);
  EdMapRange.Value := Nanobrot.ColorMapRange;
  EdMapOffset.Value := Nanobrot.ColorMapOffset;
  CkShowSlopes.Checked := Nanobrot.Slopes;
  EdSlopePower.Value := Nanobrot.SlopePower;
  EdSlopeRatio.Value := Nanobrot.SlopeRatio;
  EdSlopeAngle.Value := Nanobrot.SlopeAngle;
  UpdateShowSlopes;
end;

procedure TFrmMain.DoColorMapManagerLoad(Sender: TObject);
begin
  LbColorMaps.Clear;
  LbColorMaps.Count := ColorMapManager.List.Count;
end;

procedure TFrmMain.DoColorMapRequest(ColorMap: TColorMap; const aName: string);
begin
  ColorMapManager.LoadColorMap(ColorMap, aName);
end;

procedure TFrmMain.DoImageResize(Sender: TObject);
begin
  ImgView.Bitmap.Clear;
  ImgView.Bitmap.SetSize(Nanobrot.Width, Nanobrot.Height);
  EdImgWidth.Value := Nanobrot.Width;
  EdImgHeight.Value := Nanobrot.Height;
  cmdViewRecoloring.Update;
end;

procedure TFrmMain.DoImgViewScaleChange(Sender: TObject);
begin
  if ImgView.Scale < 1 then begin
    if ImgView.Bitmap.ResamplerClassName <> 'TLinearResampler' then
      TLinearResampler.Create(ImgView.Bitmap);
  end else begin
    if ImgView.Bitmap.ResamplerClassName <> 'TNearestResampler' then
      TNearestResampler.Create(ImgView.Bitmap);
  end;
  SbLbZoom.Caption := Format('%.0f%%', [ImgView.Scale * 100]);
  Application.ProcessMessages;
end;

procedure TFrmMain.DoMessage(Sender: TObject; const aStr: string);
begin
  SbLbInfo.Caption := aStr;
  Application.ProcessMessages;
end;

procedure TFrmMain.DoProgress(Sender: TObject; Stage: TProgressStage;
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

procedure TFrmMain.DoWork(Sender: TObject);
begin
  if Nanobrot.Runned then
    fSavedCtrl := TSavedElement.Create(Screen.ActiveControl);
  UpdateParamsCtrls(PnLeftDk, not Nanobrot.Runned);
  UpdateParamsCtrls(PnRightDk, not Nanobrot.Runned);
  UpdateShowSlopes;
  fMRUList.Enabled := not Nanobrot.Runned;
  if not Nanobrot.Runned then
    fSavedCtrl.Extract;
end;

procedure TFrmMain.DoRecoloring(Sender: TObject);
begin
  if Assigned(fSavedCtrl.Control) then
    Exit;
  if Nanobrot.Runned then
    fSavedCtrl := TSavedElement.Create(Screen.ActiveControl);
  fMRUList.Enabled := not Nanobrot.Runned;
  UpdateParamsCtrls(PnRenderingProperties, not Nanobrot.Runned);
  UpdateParamsCtrls(PnRightDk, not Nanobrot.Runned);
  if not Nanobrot.Runned then
    fSavedCtrl.Extract;
end;

function TFrmMain.AllowChange: Boolean;
begin
  result := ([csLoading, csDestroying] * ComponentState = []) and
    not Nanobrot.Runned;
end;

procedure TFrmMain.EdCenterXChange(Sender: TObject);
begin
  if AllowChange then
    Nanobrot.CenterX := Trim(EdCenterX.Text);
end;

procedure TFrmMain.EdCenterYChange(Sender: TObject);
begin
  if AllowChange then
  Nanobrot.CenterY := Trim(EdCenterY.Text);
end;

procedure TFrmMain.EdZoomChange(Sender: TObject);
begin
  if AllowChange then
    Nanobrot.Zoom := Trim(EdZoom.Text);
end;

procedure TFrmMain.EdMaxItersChange(Sender: TObject);
begin
  if AllowChange then
    Nanobrot.Iterations := Trunc(EdIterations.Value);
end;

procedure TFrmMain.EdPeriodChange(Sender: TObject);
begin
  if AllowChange then
    Nanobrot.Period := StrToIntDef(EdPeriod.Text, 0);
end;

procedure TFrmMain.EdOrderMChange(Sender: TObject);
begin
  if AllowChange then
    Nanobrot.OrderM := Trunc(EdOrderM.Value);
end;

procedure TFrmMain.EdOrderNChange(Sender: TObject);
begin
  if AllowChange then
    Nanobrot.OrderN := Trunc(EdOrderN.Value);
end;

procedure TFrmMain.AppEventsHint(Sender: TObject);
begin
  if not Nanobrot.Runned then
    SbLbInfo.Caption := Application.Hint;
end;

procedure TFrmMain.CbJitterChange(Sender: TObject);
begin
  if AllowChange then
    Nanobrot.Jitter := CbJitter.ItemIndex;
end;

procedure TFrmMain.EdSupersampleChange(Sender: TObject);
begin
  if AllowChange then
    Nanobrot.Supersample := Trunc(EdSupersample.Value);
end;

procedure TFrmMain.CbOutColoringChange(Sender: TObject);
begin
  if AllowChange then
    Nanobrot.OutColoring := CbOutColoring.ItemIndex;
end;

procedure TFrmMain.LbColorMapsChange(Sender: TObject);
begin
  if AllowChange then
    with LbColorMaps do
      if ItemIndex >= 0 then
        Nanobrot.ColorMapFn := Items[ItemIndex];
end;

procedure TFrmMain.EdMapRangeChange(Sender: TObject);
begin
  if AllowChange then
    Nanobrot.ColorMapRange := EdMapRange.Value;
end;

procedure TFrmMain.EdMapOffsetChange(Sender: TObject);
begin
  if AllowChange then
    Nanobrot.ColorMapOffset := EdMapOffset.Value;
end;

procedure TFrmMain.CkShowSlopesClick(Sender: TObject);
begin
  if AllowChange then
    Nanobrot.Slopes := CkShowSlopes.Checked;
  UpdateShowSlopes;
end;

procedure TFrmMain.EdSlopePowerValueChanged(Sender: TObject);
begin
  if AllowChange then
    Nanobrot.SlopePower := EdSlopePower.Value;
end;

procedure TFrmMain.EdSlopeRatioValueChanged(Sender: TObject);
begin
  if AllowChange then
    Nanobrot.SlopeRatio := EdSlopeRatio.Value;
end;

procedure TFrmMain.EdSlopeAngleValueChanged(Sender: TObject);
begin
  if AllowChange then
    Nanobrot.SlopeAngle := EdSlopeAngle.Value;
end;

procedure TFrmMain.EdFloatKeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in ['0'..'9', '-', '+', '.', 'E', 'e', #8]) then
    Key := #0;
end;

procedure TFrmMain.EdImgWidthChange(Sender: TObject);
begin
  if AllowChange then
    Nanobrot.SetSize(Trunc(EdImgWidth.Value), Trunc(EdImgHeight.Value));
end;

procedure TFrmMain.EdLbClick(Sender: TObject);
var
  L: TSpTBXLabel;
begin
  L := Sender as TSpTBXLabel;
  if Assigned(L.FocusControl) and (L.FocusControl.CanFocus) then
    L.FocusControl.SetFocus;
end;

procedure TFrmMain.LbColorMapsData(Control: TWinControl; Index: Integer;
  var Data: string);
begin
  Data := ColorMapManager.List[Index];
end;

function TFrmMain.LbColorMapsDataFind(Control: TWinControl;
  FindString: string): Integer;
begin
  result := ColorMapManager.List.IndexOf(FindString);
end;

procedure TFrmMain.LoadFractal(const aFileName: string);
begin
  Nanobrot.LoadFromFile(aFileName);
  if FileExists(aFileName) then
    fMRUList.AddToRecent(aFileName);
end;

procedure TFrmMain.SaveFractal(const aFileName: string);
begin
  if not AccessFileOnWrite(aFileName) then
    Exit;
  Application.ProcessMessages;
  Nanobrot.SaveToFile(aFileName);

  if FileExists(aFileName) then
    fMRUList.AddToRecent(aFileName);
end;

procedure TFrmMain.StatusBarDrawDockBackground(Sender: TObject;
  ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage;
  var PaintDefault: Boolean);
begin
  if PaintStage = pstPostPaint then
    Exit;
  DrawStatusbarBG(Sender as TSpTBXCustomStatusBar, ARect, ACanvas);
  PaintDefault := false;
end;

procedure TFrmMain.StatusBarDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
var
  x: Integer;
  r: TRect;
begin
  x := IfThen(Panel.Index = 0, 2, 16);
  StatusBar.Canvas.FillRect(Rect);
  //DrawStatuspanel(Rect, StatusBar.Canvas);
  if Panel.Index > 0 then
    IlSmall.Draw(StatusBar.Canvas, Rect.Left + 1, Rect.Top + 2, Panel.Index - 1);
  r := Rect;
  Inc(r.Left, x);
  SetBkMode(StatusBar.Canvas.Handle, TRANSPARENT);
  DrawText(StatusBar.Canvas.Handle, PChar(Panel.Text), Length(Panel.Text), r,
    DT_VCENTER or DT_SINGLELINE or DT_END_ELLIPSIS);

 //StatusBar.Canvas.TextOut(Rect.Left + x, Rect.Top, Panel.Text);
end;

procedure TFrmMain.UpdateShowSlopes;
var
  e: Boolean;
begin
  e := CkShowSlopes.Checked;
  CkShowSlopes.Caption := BoolToStr(e, true);
  e := e and not Nanobrot.Runned;
  LbSlopePower.Enabled := e;
  EdSlopePower.Enabled := e;
  LbSlopeRatio.Enabled := e;
  EdSlopeRatio.Enabled := e;
  LbSlopeAngle.Enabled := e;
  EdSlopeAngle.Enabled := e;
end;

procedure TFrmMain.cmdEditUndoExecute(Sender: TObject);
begin
  Nanobrot.Undo;
end;

procedure TFrmMain.cmdEditUndoUpdate(Sender: TObject);
begin
  Nanobrot.UpdateUndo(cmdEditUndo);
end;

procedure TFrmMain.cmdEditRedoExecute(Sender: TObject);
begin
  Nanobrot.Redo;
end;

procedure TFrmMain.cmdEditRedoUpdate(Sender: TObject);
begin
  Nanobrot.UpdateRedo(cmdEditRedo);
end;

procedure TFrmMain.cmdEnabledUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not Nanobrot.Runned;
end;

procedure TFrmMain.cmdRenderExecute(Sender: TObject);
begin
  if Nanobrot.Runned then begin
    Nanobrot.Stop;
    fProgressBar.Hide;
    SbLbTime.Caption := '';
    SbLbInfo.Caption := '';
    SbLbReady.Caption := 'Stopping...';
    exit;
  end;
  Nanobrot.Start;
end;

procedure TFrmMain.cmdRenderUpdate(Sender: TObject);
begin
  if Nanobrot.Runned then begin
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

procedure TFrmMain.cmdView100Execute(Sender: TObject);
begin
  ImgView.Scale := TAction(Sender).Tag * 0.01;
end;

procedure TFrmMain.cmdViewCustomizerResetExecute(Sender: TObject);
begin
  Customizer.Reset;
end;

procedure TFrmMain.cmdViewRecoloringExecute(Sender: TObject);
begin
  Nanobrot.RecoloringMode := not Nanobrot.RecoloringMode;
end;

procedure TFrmMain.cmdViewRecoloringUpdate(Sender: TObject);
begin
  cmdViewRecoloring.Checked := Nanobrot.RecoloringMode;
  cmdViewRecoloring.Enabled := not Nanobrot.Runned;
end;

procedure TFrmMain.cmdViewSmallIconsExecute(Sender: TObject);
begin
  SmallIconsOnToolbar(not cmdViewSmallIcons.Checked);
end;

procedure TFrmMain.cmdViewSmallIconsUpdate(Sender: TObject);
begin
  cmdViewSmallIcons.Checked := fSmallIcons;
end;

procedure TFrmMain.CheckFileSave;
begin
  if not Nanobrot.Modified then
    Exit;
  case QuestionDialog('Current fractal settings is not saved',
    Format('Save changes to "%s"?', [Nanobrot.FileName]), mbYesNoCancel) of
    mrYes: cmdFileSaveExecute(Self);
    mrNo: {Nothing};
    mrCancel: Abort;
  end;
end;

procedure TFrmMain.cmdFileNewExecute(Sender: TObject);
begin
  CheckFileSave;
  Nanobrot.LoadDefault;
end;

procedure TFrmMain.cmdFileOpenExecute(Sender: TObject);
begin
  CheckFileSave;
  Application.ProcessMessages;
  if not DlgOpenFractal.Execute(Handle) then
    Exit;
  LoadFractal(DlgOpenFractal.FileName);
end;

procedure TFrmMain.cmdFilePrintExecute(Sender: TObject);
begin
  ShowPrinPreview;
end;

procedure TFrmMain.cmdFileRevertExecute(Sender: TObject);
begin
  Nanobrot.RevertToSaved;
end;

procedure TFrmMain.DoDlgSaveClickButton(const pfdc: IFileDialogCustomize;
  dwIDCtl: DWORD);
begin
  if dwIDCtl = dwDialogAdvancedID then
    ShowFrmSaveAdvanced(Nanobrot.Level);
end;

procedure TFrmMain.DlgSaveFractalExecute(Sender: TObject);
var
  c: IFileDialogCustomize;
  d: IFileDialogEvents;
  cookie: DWORD;
begin
  if Supports(DlgSaveFractal.Dialog, IFileDialogCustomize, c) then
  begin
    c.AddPushButton(dwDialogAdvancedID, 'Advanced...');
    c.MakeProminent(dwDialogAdvancedID);
    d := TFileDialogHelper.Create;
    TFileDialogHelper(d).OnDialogClick := DoDlgSaveClickButton;
    if Succeeded(DlgSaveFractal.Dialog.Advise(d, cookie)) then
    begin
      FileDialog := DlgSaveFractal.Dialog;
      FileDialogEvents := d;
      FileDialogEventsCookie := cookie;
    end;
  end;
end;

procedure TFrmMain.DlgSaveFractalFileOkClick(Sender: TObject;
  var CanClose: Boolean);
var
  msg, lmf: string;
begin
  if FileExists(DlgSaveFractal.FileName) then
    msg := Format('"%s"', [DlgSaveFractal.FileName]);
  if Nanobrot.Level.AllowSaveLevelMaps then
  begin
    lmf := ChangeFileExt(DlgSaveFractal.FileName, '.nbm');
    if FileExists(lmf) then
      msg := Format('%s,'#13#10'"%s"', [msg, lmf]);
  end;
  if Length(msg) > 0 then
  begin
    msg := Format('%s already exist.'#13#10'Do you want to replace them?', [msg]);
    CanClose := MessageDlg(msg, mtWarning, mbYesNo, 0) = mrYes;
  end;
end;

procedure TFrmMain.cmdFileSaveAsExecute(Sender: TObject);
begin
  Application.ProcessMessages;
  DlgSaveFractal.FileName := ExtractFileName(Nanobrot.FileName);
  FileDialog := nil;
  FileDialogEvents := nil;
  FileDialogEventsCookie := 0;
  try
    if not DlgSaveFractal.Execute(Handle) then
      Exit;
  finally
    if (FileDialog <> nil) and (FileDialogEventsCookie <> 0) then
      FileDialog.Unadvise(FileDialogEventsCookie);
    FileDialog := nil;
    FileDialogEvents := nil;
    FileDialogEventsCookie := 0;
  end;
  Application.ProcessMessages;
  SaveFractal(DlgSaveFractal.FileName);
end;

procedure TFrmMain.cmdFileSaveExecute(Sender: TObject);
begin
  if not FileExists(Nanobrot.FileName) then
    cmdFileSaveAs.Execute
  else
    SaveFractal(Nanobrot.FileName);
end;

procedure TFrmMain.cmdFileSaveUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Nanobrot.Modified and (not Nanobrot.Runned);
end;

procedure TFrmMain.cmdFileShowLocationExecute(Sender: TObject);
begin
  ShowFileLocation(DlgSaveImg.FileName);
end;

procedure TFrmMain.cmdFileShowLocationUpdate(Sender: TObject);
begin
  cmdFileShowLocation.Enabled := FileExists(DlgSaveImg.FileName);
end;

procedure TFrmMain.cmdHelpAboutExecute(Sender: TObject);
begin
  ShowAbout;
end;


procedure TFrmMain.cmdFileExportImageExecute(Sender: TObject);
begin
  Application.ProcessMessages;
  DlgSaveImg.FileName := ChangeFileExt(ExtractFileName(Nanobrot.FileName),
    def_ext_im[DlgSaveImg.FileTypeIndex]);
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

procedure TFrmMain.cmdFileImportExecute(Sender: TObject);
var
  ext: string;
begin
  Application.ProcessMessages;
  DlgImport.FileName := ExtractFileName(DlgImport.FileName);
  if not DlgImport.Execute(Handle) then
    Exit;
  ext := ExtractFileExt(DlgImport.FileName);
  Screen.Cursor := crHourGlass;
  try
    if SameText(ext, def_ext_io[1]) then
      Nanobrot.ImportKallesParams(DlgImport.FileName)
    else
    if SameText(ext, def_ext_io[2]) or SameText(ext, def_ext_io[3]) then
      Nanobrot.ImportLevelMap(DlgImport.FileName)
    else
      MessageDlg('Can''t export. Format not supported.',mtError, [mbCancel], 0);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFrmMain.cmdFileExportExecute(Sender: TObject);
var
  ext: string;
begin
  Application.ProcessMessages;
  DlgExport.FileName := ChangeFileExt(ExtractFileName(Nanobrot.FileName),
    def_ext_io[DlgExport.FileTypeIndex]);
  if not DlgExport.Execute(Handle) then
    Exit;
  if not AccessFileOnWrite(DlgExport.FileName) then
    Exit;
  ext := ExtractFileExt(DlgExport.FileName);

  Screen.Cursor := crHourGlass;
  try
    if SameText(ext, def_ext_io[1]) then
      Nanobrot.ExportKallesParams(DlgExport.FileName)
    else
    if SameText(ext, def_ext_io[2]) or SameText(ext, def_ext_io[3]) then
      Nanobrot.ExportLevelMap(DlgExport.FileName)
    else
      MessageDlg('Can''t export. Format not supported.',mtError, [mbCancel], 0);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFrmMain.cmdToolFindPeriodExecute(Sender: TObject);
begin
  Nanobrot.FindPeriod;
end;

procedure TFrmMain.cmdToolSoundArtExportExecute(Sender: TObject);
begin
  ShowSynth;
end;

procedure TFrmMain.cmdToolOptionsExecute(Sender: TObject);
begin
  Application.CreateForm(TFrmOptions, FrmOptions);
  with FrmOptions do
  try
    ckShowHints.Checked := Application.ShowHint;
    ckSaveFormState.Checked := Customizer.SaveFormState;
    ckSoundOnDone.Checked := Nanobrot.SoundOnDone;
    ckFileAssociation.Checked := fFileAssociation;
    edColorMapsPath.Text := ColorMapManager.Directory;
    edUndoLimit.Value := Nanobrot.UndoLimit;
    rgRasterizerType.ItemIndex := Ord(Nanobrot.RasterizerType);
    if ShowModal = mrOK then
    begin
      Application.ShowHint := ckShowHints.Checked;
      Nanobrot.SoundOnDone := ckSoundOnDone.Checked;
      Customizer.SaveFormState := ckSaveFormState.Checked;
      if ckFileAssociation.Checked <> fFileAssociation then
      begin
        fFileAssociation := ckFileAssociation.Checked;
        SetFileAssociation(fFileAssociation);
      end;
      ColorMapManager.Directory := EdColorMapsPath.Text;
      Nanobrot.UndoLimit := Trunc(edUndoLimit.Value);
      Nanobrot.RasterizerType := TRasterizerType(rgRasterizerType.ItemIndex);
    end;
  finally
    Release;
  end;
end;

procedure TFrmMain.cmdToolsUpdateColorMapsExecute(Sender: TObject);
begin
  ColorMapManager.Refresh;
end;

procedure TFrmMain.DoFindPeriod(Sender: TObject);
begin
  EdPeriod.Text := Nanobrot.Period.ToString;
end;

procedure TFrmMain.DoPeriodProgress(Sender: TObject; Stage: TProgressStage;
  Progress: Integer; const Msg: string);
begin
  case Stage of
    psStarting:
      if DlgFindPeriod.Execute then
        if DlgFindPeriod.ModalResult = mrCancel then
           Nanobrot.CancelPeriodSearch;
    psRunning: DlgFindPeriod.Text := Format('Current atom domain period: %d', [Progress]);
    psEnding: DlgFindPeriod.ModalResult := mrOK;
  end;
end;

procedure TFrmMain.DlgFindPeriodTimer(Sender: TObject; TickCount: Cardinal;
  var Reset: Boolean);
begin
  Reset := true;
  if not Nanobrot.IsPeriodSearch then
    SendMessage(DlgFindPeriod.Handle, TDM_CLICK_BUTTON, mrCancel, 0);
end;

procedure TFrmMain.cmdHelpHomePageExecute(Sender: TObject);
begin
  ShellExecute(Handle, 'open', 'https://github.com/flutomax/nanobrot/', nil, nil, SW_SHOWDEFAULT);
end;



end.
