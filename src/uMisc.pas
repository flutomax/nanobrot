unit uMisc;

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
  Windows, Classes, SysUtils, Controls, StdCtrls, Menus, IniFiles, ImgList,
  Dialogs, Registry, ShellAPI, ShlObj, Graphics, GR32, SpTBXItem, Wincodec;

const
  sRecentFiles = 'Recent Files';

type

  TRecentMenuItem = class(TSpTBXItem)
  private
    fFileName: string;
  public
    property FileName: string read fFileName;
  end;
  TRecentMenuItemClass = class of TRecentMenuItem;

  TOnRecentFileEvent = procedure(Sender: TObject; const aFileName: string) of object;

  TMRUList = class(TComponent)
  private
    fRecent: TStrings;
    fMaxRecent: Integer;
    fMIRecent: TSpTBXSubmenuItem;
    fTopSepar: TSpTBXSeparatorItem;
    fBotSepar: TSpTBXSeparatorItem;
    fMinimizeWidth: Integer;
    fEnabled: Boolean;
    fOnRecent: TOnRecentFileEvent;
    function CreateMenuItem(aOwner: TComponent): TRecentMenuItem;
    function CreateMenuCaption(aIndex: Integer; const aFileName: string): string;
    procedure DoOnRecentClick(Sender: TObject);
    procedure SetMaxRecent(AValue: Integer);
    procedure SetTopSepar(const aValue: TSpTBXSeparatorItem);
    procedure SetBotSepar(const aValue: TSpTBXSeparatorItem);
    procedure SetMIRecent(const aValue: TSpTBXSubmenuItem);
    procedure SetRecent(const aValue: TStrings);
    procedure SetEnabled(const Value: Boolean);
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure AddToRecent(const aFileName: string);
    procedure RemoveFromRecent(const aFileName: string);
    procedure UpdateRecentFiles;
    procedure LoadFromIni(Ini: TCustomIniFile; const aSection: string = sRecentFiles);
    procedure SaveToIni(Ini: TCustomIniFile; const aSection: string = sRecentFiles);
  published
    property MaxRecent: Integer read fMaxRecent write SetMaxRecent default 10;
    property MinimizeWidth: Integer read fMinimizeWidth write fMinimizeWidth default 200;
    property MIRecent: TSpTBXSubmenuItem read fMIRecent write SetMIRecent;
    property TopSepar: TSpTBXSeparatorItem read fTopSepar write SetTopSepar;
    property BotSepar: TSpTBXSeparatorItem read fBotSepar write SetBotSepar;
    property Enabled: Boolean read fEnabled write SetEnabled default true;
    property OnRecent: TOnRecentFileEvent read fOnRecent write fOnRecent;
  end;

  TSavedElement = record
    Control: TWincontrol;
    SelStart: Integer;
    SelLength: Integer;
    constructor Create(aControl: TWincontrol);
    function Extract: TWincontrol;
  end;

  TFileDialogClickEvent = procedure(const pfdc: IFileDialogCustomize;
    dwIDCtl: DWORD) of object;

  TFileDialogHelper = class(TInterfacedObject, IFileDialogEvents, IFileDialogControlEvents)
  protected
    fOnDialogCheck: TFileDialogClickEvent;
  public
    function OnFileOk(const pfd: IFileDialog): HResult; stdcall;
    function OnFolderChanging(const pfd: IFileDialog;
      const psiFolder: IShellItem): HResult; stdcall;
    function OnFolderChange(const pfd: IFileDialog): HResult; stdcall;
    function OnSelectionChange(const pfd: IFileDialog): HResult; stdcall;
    function OnShareViolation(const pfd: IFileDialog; const psi: IShellItem;
      out pResponse: DWORD): HResult; stdcall;
    function OnTypeChange(const pfd: IFileDialog): HResult; stdcall;
    function OnOverwrite(const pfd: IFileDialog; const psi: IShellItem;
      out pResponse: DWORD): HResult; stdcall;
    function OnItemSelected(const pfdc: IFileDialogCustomize; dwIDCtl: DWORD;
      dwIDItem: DWORD): HResult; stdcall;
    function OnButtonClicked(const pfdc: IFileDialogCustomize;
      dwIDCtl: DWORD): HResult; stdcall;
    function OnCheckButtonToggled(const pfdc: IFileDialogCustomize;
      dwIDCtl: DWORD; bChecked: BOOL): HResult; stdcall;
    function OnControlActivating(const pfdc: IFileDialogCustomize;
      dwIDCtl: DWORD): HResult; stdcall;
    property OnDialogClick: TFileDialogClickEvent read fOnDialogCheck write fOnDialogCheck;
  end;

  TFileAssociation = class(TComponent)
  private
    fRegistry: TRegistry;
    fRegisterForAllUsers: boolean;
    fRegisterFileAssociation: boolean;
    fAddApplicationToDefaultPrograms: boolean;
    fAddExtensionToDefaultPrograms: boolean;
    fUnReg: boolean;
    fApplicationName: string;
    FApplicationDescription: string;
    fExtension: string;
    fExtensionName: string;
    fExtensionIcon: string;
    fAction: string;
    fActionName: string;
    fActionText: string;
    fActionIcon: string;
    procedure SetRoot;
    procedure SetAction(AValue: string);
    procedure SetActionIcon(AValue: string);
    procedure SetActionName(AValue: string);
    procedure SetActionText(AValue: string);
    procedure SetApplicationDescription(AValue: string);
    procedure SetApplicationName(AValue: string);
    procedure SetExtension(AValue: string);
    procedure SetExtensionIcon(AValue: string);
    procedure SetExtensionName(AValue: string);
    procedure SetRegisterForAllUsers(AValue: boolean);
    procedure SetUnReg(AValue: boolean);
    procedure SetAddApplicationToDefaultPrograms(AValue: boolean);
    procedure SetAddExtensionToDefaultPrograms(AValue: boolean);
    procedure SetRegisterFileAssociation(AValue: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function StrNoSpaces(const s: string): string;
    function WriteStringValue(SubKey: string; ValueName: string;
      ValueData: string): boolean;
    function DeleteValue(SubKey: string; ValueName: string): boolean;
    function WriteString(SubKey: string; ValueName: string; ValueData: string): boolean;
    function WriteFileAssociationClass: boolean;
    function WriteFileAssociationClassCommand: boolean;
    function WriteFileAssociation: boolean;
    function WriteDefaultPrograms: boolean;
    function WriteDefaultProgramsAddExt: boolean;
    function Execute: boolean;
    procedure ClearIconCache;
  published
    property ApplicationName: string read fApplicationName write SetApplicationName;
    property ApplicationDescription: string
      read FApplicationDescription write SetApplicationDescription;
    property Extension: string read fExtension write SetExtension;
    property ExtensionName: string read fExtensionName write SetExtensionName;
    property ExtensionIcon: string read fExtensionIcon write SetExtensionIcon;
    property Action: string read fAction write SetAction;
    property ActionName: string read fActionName write SetActionName;
    property ActionText: string read fActionText write SetActionText;
    property ActionIcon: string read fActionIcon write SetActionIcon;
  published
    property RegisterForAllUsers: boolean read fRegisterForAllUsers
      write SetRegisterForAllUsers default True;
    property RegisterFileAssociation: boolean
      read fRegisterFileAssociation write SetRegisterFileAssociation default True;
    property AddApplicationToDefaultPrograms: boolean read fAddApplicationToDefaultPrograms
      write SetAddApplicationToDefaultPrograms default True;
    property AddExtensionToDefaultPrograms: boolean
      read fAddExtensionToDefaultPrograms write SetAddExtensionToDefaultPrograms default True;
    property UnReg: boolean read fUnReg write SetUnReg default False;
  end;

  function FormatSize(const ASize: UInt64): string;
  function MiniMizeName(FileName: string; Canvas: TCanvas; MaxWidth: Integer): string;
  function AccessFileOnWrite(const aFileName: string): Boolean;
  function GetDefaultSystemIcon(ASiid: Integer): HICON;
  function QuestionDialog(const aTitle, aText: string; Buttons: TMsgDlgButtons = mbYesNo): Integer;
  function GetExponentOfNumber(const aNumber: AnsiString): Integer;
  procedure CenterWindow(Wnd: HWnd);
  procedure DrawDisabledImagelist(src, dst: TCustomImageList);
  procedure DrawSplitter(R: TRect; Canvas: TCanvas);
  procedure DrawStatusbarBG(Sb: TSpTBXCustomStatusBar; R: TRect; Canvas: TCanvas);
  procedure PrepareStatusbar(Sb: TSpTBXCustomStatusBar);
  procedure ExportImage(const aFileName: string; bmp: TBitmap32);
  procedure UpdateParamsCtrls(const aControl: TWinControl; enabled: Boolean);
  procedure ZeroDivideError;
  procedure PlayDoneSound;
  procedure ShowFileLocation(const s: string);
  procedure SetFileAssociation(aValue: Boolean);

const
  dwDialogAdvancedID: DWORD = 1973;

var
  FileDialog: IFileDialog = nil;
  FileDialogEvents: IFileDialogEvents = nil;
  FileDialogEventsCookie: DWORD = 0;

implementation

uses
  Consts, Math, Forms, CommCtrl, StrUtils, SysConst, ActiveX, MMSystem,
  TB2Item, SpTBXSkins, SpTBXEditors;

type

  TSpTBXStatusBarAccess = class(TSpTBXCustomStatusBar);
  TSpTBXCustomItemAccess = class(TSpTBXCustomItem);

  TWICImageAdapter = class
  private
    class var fImagingFactory: IWICImagingFactory;
    class function GetImagingFactory: IWICImagingFactory; static;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveBitmap32(const aFileName: string; ImageFormat: TWICImageFormat; bmp: TBitmap32);
  end;

const

  KeyMaxRecent    = 'MaxRecent';
  KeyCount        = 'Count';
  KeyFile         = 'File%d';

  TaskDlgButtons: array[TMsgDlgBtn] of Cardinal = (
    TDCBF_YES_BUTTON, TDCBF_NO_BUTTON, TDCBF_OK_BUTTON,
    TDCBF_CANCEL_BUTTON, 0, TDCBF_RETRY_BUTTON,
    0, 0, 0, 0, 0, TDCBF_CLOSE_BUTTON);

var
  bmSbarGrad: TBitmap;

function StrFormatByteSizeW(ASize: UInt64; szBuf: PWideChar;
  uiBufSize: UINT): PWideChar; stdcall; external 'shlwapi.dll';

procedure InitModule;
const
  gk: array[0..18] of TColor = ($00FFEBD1, $00FFE9CD, $00FFE8CB, $00FFE6C7,
    $00FFE1BC, $00FFE2BE, $00FFCF94, $00FFD198, $00FFD29A, $00FFD29A, $00FFD49E,
    $00FFD6A3, $00FFD8A7, $00FFDAAB, $00FFDCAF, $00FFDEB4, $00FFDFB8, $00FFE0BA,
    $00FFE1BC);
var
 y: Integer;
begin
  bmSbarGrad := TBitmap.Create;
  bmSbarGrad.SetSize(1, 19);
  bmSbarGrad.PixelFormat := pf24bit;
  for y := 0 to bmSbarGrad.Height - 1 do
    bmSbarGrad.Canvas.Pixels[0, y] := gk[y];
end;

procedure FreeModule;
begin
  FreeAndNil(bmSbarGrad);
end;

function FormatSize(const ASize: UInt64): string;
var
  Buf: string;
begin
  SetLength(Buf, 1024);
  if StrFormatByteSizeW(ASize, PChar(Buf), Length(Buf)) <> nil then
    Result := Buf
  else
    Result := UIntToStr(ASize) + ' Bytes';
end;


function MiniMizeName(FileName: string; Canvas: TCanvas; MaxWidth: Integer): string;

  procedure RemoveFirstDir(var Dir: string);
  var p: Integer;
  begin
    p:= Pos(PathDelim,Dir);
    if (p > 0) then
    begin
      Dir := Copy(Dir,p+1,Length(Dir)-p);
    end;
  end;

var
  Drive, Dir, Fn,
  ComposedName: string;
  TWidth: Integer;
begin
  Result := FileName;
  if Pos(PathDelim, FileName) = 0 then Exit;
  if Canvas.TextWidth(FileName) <= MaxWidth then Exit;
  Drive := ExtractFileDrive(FileName);
  Fn := ExtractFileName(FileName);
  Dir := ExtractFilePath(FileName);
  if (Length(Drive) > 0) then System.Delete(Dir, 1, Length(Drive));
  While (Length(Dir) > 0) and (Dir[1] in ['/','\']) do
  begin
    Drive := Drive + Dir[1];
    System.Delete(Dir,1,1);
  end;
  if (Length(Dir) = 0) then
  begin
    Result := Fn;
    Exit;
  end;
  repeat
    RemoveFirstDir(Dir);
    ComposedName := Drive+'...'+PathDelim+Dir+Fn;
    TWidth := Canvas.TextWidth(ComposedName);
  until (Length(Dir) = 0) or (TWidth <= MaxWidth);
  if (TWidth <= MaxWidth) then Result := ComposedName else Result := Fn;
end;

procedure CenterWindow(Wnd: HWnd);
var
  Rect: TRect;
  Monitor: TMonitor;
begin
  GetWindowRect(Wnd, Rect);
  if Application.MainForm <> nil then
  begin
    if Assigned(Screen.ActiveForm) then
      Monitor := Screen.ActiveForm.Monitor
    else
      Monitor := Application.MainForm.Monitor;
  end
  else
    Monitor := Screen.PrimaryMonitor;
  SetWindowPos(Wnd, 0,
    Monitor.Left + ((Monitor.Width - Rect.Right + Rect.Left) div 2),
    Monitor.Top + ((Monitor.Height - Rect.Bottom + Rect.Top) div 3),
    0, 0, SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER);
end;

procedure DrawDisabledImagelist(src, dst: TCustomImageList);
var
  i, x, y: integer;
  p: PRGBQuad;
  Image, Mask: TBitmap;
  ARect: TRect;
begin
  Assert((src.Width = dst.Width) and (src.Height = dst.Height) and (src.Count > 0));
  dst.Clear;

  ARect := Rect(0, 0, src.Width, src.Height);
  dst.BeginUpdate;
  try
    Image := TBitmap.Create;
    try
      Image.PixelFormat := pf32Bit;
      Image.SetSize(src.Width, src.Height);
      Mask := TBitmap.Create;
      try
        with Mask do
        begin
          Monochrome := True;
          Height := src.Height;
          Width := src.Width;
        end;
        for i := 0 to src.Count - 1 do
          begin
            with Image.Canvas do
            begin
              FillRect(ARect);
              ImageList_Draw(src.Handle, I, Handle, 0, 0, ILD_NORMAL);
            end;
            for y := 0 to Image.Height - 1 do begin
              p := Image.Scanline[y];
              for x := 0 to Image.Width - 1 do begin
                p.rgbReserved := p.rgbReserved div 3;
                Inc(p);
              end;
            end;
            with Mask.Canvas do
            begin
              FillRect(ARect);
              ImageList_Draw(src.Handle, I, Handle, 0, 0, ILD_MASK);
            end;
            dst.Add(Image, Mask);
          end;
      finally
        Mask.Free;
      end;
    finally
      Image.Free;
    end;
  finally
    dst.EndUpdate;
  end;
end;

procedure DrawSplitter(R: TRect; Canvas: TCanvas);
var
  i, X, Y, DY: integer;
  Brush: TBitmap;
begin
  //Canvas.Brush.Color := Color;
  Canvas.FillRect(R);
  DY := 4;
  X := (R.Left+R.Right) div 2;
  Y := (R.Top+R.Bottom) div 2;
  dec(X, 1);
  dec(Y, DY * 4);
  Brush := TBitmap.Create;
  try
    Brush.SetSize(3, 3);
    R := Brush.Canvas.ClipRect;
    DrawEdge(Brush.Canvas.Handle, R, BDR_SUNKENINNER, BF_RECT);
    for i := 0 to 8 do
    begin
      Canvas.Draw(X, Y, Brush);
      inc(Y, DY);
    end;
  finally
    Brush.Free;
  end;
end;

procedure DrawStatusbarBG(Sb: TSpTBXCustomStatusBar; R: TRect; Canvas: TCanvas);
const
  C1 = $00ad6000;
  C2 = $00ffc985;
  C3 = $00FFAF4B;
  C4 = $00FFE5C5;
var
  g: TRect;
  i, n, x: Integer;
  v: TTBItemViewer;
begin
  // Draw Background
  SetStretchBltMode(Canvas.Handle, HALFTONE);
  StretchBlt(Canvas.Handle, R.Left, R.Top, R.Width, R.Height,
    bmSbarGrad.Canvas.Handle, 0, 0, bmSbarGrad.Width, bmSbarGrad.Height, SRCCOPY);
  // Draw Size Grip
  g := Sb.Toolbar.GetGripRect;
  // When it's called by the Toolbar the Gripper position should be corrected
  if (not IsRectEmpty(G)) and (R.Left = -2) and (R.Top = -2) then
    OffsetRect(g, -2, -2);
  if not IsRectEmpty(g) then begin
    // Draw 3 cells at the bottom
    g.Left := g.Right - 12;
    g.Top := g.Bottom - 4;
    SpDrawXPGrip(Canvas, g, C1, C2);
    // Draw 2 cells at the top
    g.Bottom := g.Top;
    g.Top := g.Bottom - 4;
    g.Left := g.Left + 4;
    SpDrawXPGrip(Canvas, g, C1, C2);
    // Draw 1 cell at the top
    g.Bottom := g.Top;
    g.Top := g.Bottom - 4;
    g.Left := g.Left + 4;
    SpDrawXPGrip(Canvas, g, C1, C2);
  end;
  // Draw Separators
  n := SaveDC(Canvas.Handle);
  try
    for i := 0 to Sb.Toolbar.View.ViewerCount - 1 do begin
      v := Sb.Toolbar.View.Viewers[i];
      if not (v is TSpTBXSeparatorItemViewer) then
        Continue;
      R := v.BoundsRect;
      if IsRectEmpty(R) then
        Continue;
      Inc(R.Top, 2);
      x := (R.Right + R.Left) div 2;
      Canvas.Pen.Color := C3;
      Canvas.MoveTo(x, R.Top);
      Canvas.LineTo(x, R.Bottom);
      Inc(x);
      Canvas.Pen.Color := C4;
      Canvas.MoveTo(x, R.Top);
      Canvas.LineTo(x, R.Bottom);
    end;
  finally
    RestoreDC(Canvas.Handle, n);
  end;
end;

procedure PrepareStatusbar(Sb: TSpTBXCustomStatusBar);
var
  i: Integer;
begin
  for i := 0 to Sb.Items.Count - 1 do
  begin
    if Sb.Items[i] is TSpTBXSeparatorItem then
      TSpTBXSeparatorItem(Sb.Items[i]).Blank := true;
    if Sb.Items[i] is TSpTBXCustomLabelItem then
      TSpTBXCustomItemAccess(Sb.Items[i]).CustomHeight := 15;
  end;
end;

function AccessFileOnWrite(const aFileName: string): Boolean;
const
  sErrWriteAccs = 'File "%s" access denided.';
var
  f: file;
  a: Integer;
begin
  AssignFile(f, aFileName);
  repeat
    {$I-}
    Rewrite(f, 1);
    {$I+}
    result := IOResult = 0;
    if not result then
      a := MessageDlg(Format(sErrWriteAccs,[aFileName]), mtError, [mbCancel, mbRetry], 0);
  until result or (a = mrCancel);
  if result then
    CloseFile(f);
end;

function GetDefaultSystemIcon(ASiid: Integer): HICON;
var
  sInfo: TSHStockIconInfo;
begin
  sInfo.cbSize := SizeOf(TSHStockIconInfo);
  if S_OK = SHGetStockIconInfo(ASiid, SHGSI_ICON or SHGSI_LARGEICON, sInfo) then
    Result := sInfo.hIcon
  else
    Result := 0;
end;

function GetExponentOfNumber(const aNumber: AnsiString): Integer;
var
  ep: Integer;
  s: AnsiString;
begin
  ep := Pos('e', aNumber);
  if ep = 0 then
    ep := Pos('E', aNumber);
  if ep = 0 then
  begin
    for result := 0 to Length(aNumber) - 1 do
      if aNumber[result - 1] = '.' then
        break;
    Dec(result);
  end else begin
    s := Copy(aNumber, ep + 1, MaxInt);
    result := StrToIntDef(s, 0);
  end;
end;

function QuestionDialog(const aTitle, aText: string; Buttons: TMsgDlgButtons): Integer;
var
  TaskDialog: TTaskDialogConfig;
  CommonButton: TMsgDlgBtn;
begin
  FillChar(TaskDialog,SizeOf(TaskDialog), 0);
  TaskDialog.cbSize := SizeOf(TaskDialog);
  TaskDialog.hwndParent := GetActiveWindow;
  TaskDialog.dwFlags := TDF_ALLOW_DIALOG_CANCELLATION;
  TaskDialog.pszWindowTitle := PChar(SMsgDlgConfirm);
  TaskDialog.pszMainInstruction := PChar(aTitle);
  TaskDialog.pszContent := PChar(aText);
  TaskDialog.pszMainIcon := LPCWSTR(MAKEINTRESOURCEW(Word(IDI_QUESTION)));
  for CommonButton:=Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
    if CommonButton in Buttons then begin
      TaskDialog.dwCommonButtons:=TaskDialog.dwCommonButtons or TaskDlgButtons[CommonButton];
    end;
  TaskDialog.nDefaultButton := IDYES;
  TaskDialogIndirect(TaskDialog, @result, nil, nil);
end;

function FileExtToImageFormat(const ext: string): TWICImageFormat;
const
  et: array[0..9] of string = (
    '.bmp', '.png', '.jpg', '.jpeg', '.jpe', '.jfif', '.gif',
    '.tif', '.tiff', '.wdp');
  wt: array[0..9] of TWICImageFormat = (
    wifBmp, wifPng, wifJpeg, wifJpeg, wifJpeg, wifJpeg, wifGif,
    wifTiff, wifTiff, wifWMPhoto);
var
  i: Integer;
begin
  i := IndexText(ext, et);
  if InRange(i, 0, 9) then
    result := wt[i]
  else
    result := wifOther;
end;

procedure ExportImage(const aFileName: string; bmp: TBitmap32);
var
  wf: TWICImageFormat;
  w: TWICImageAdapter;
begin
  wf := FileExtToImageFormat(ExtractFileExt(aFileName));
  if wf = wifOther then begin
    MessageDlg('Can''t export image. Format not supported.',mtError, [mbCancel], 0);
    exit;
  end;

  w := TWICImageAdapter.Create;
  try
    w.SaveBitmap32(aFileName, wf, bmp);
  finally
    w.Free;
  end;
end;

procedure UpdateParamsCtrls(const aControl: TWinControl; enabled: Boolean);
var
  i: integer;
begin
  for i:=0 to aControl.ControlCount - 1 do begin
    aControl.Controls[i].Enabled := enabled;
    if aControl.Controls[i] is TWinControl then
      UpdateParamsCtrls(aControl.Controls[i] as TWinControl, enabled);
    // fug fix on TSpTBXSpinButton repeatig on disabled parent control
    if aControl.Controls[i] is TSpTBXSpinEdit then
      TSpTBXSpinEdit(aControl.Controls[i]).SpinButton.Repeating := enabled;
  end;
end;

procedure ZeroDivideError;
begin
  raise EZeroDivide.Create(SDivByZero);
end;

procedure PlayDoneSound;
var
  h: THandle;
  p: Pointer;
begin
  h := LoadResource(hInstance, FindResource(hInstance, 'SNDDONE', RT_RCDATA));
  p := LockResource(h);
  SndPlaySound(p, SND_MEMORY or SND_ASYNC);
  FreeResource(h);
end;

procedure ShowFileLocation(const s: string);
var
  IIDL: PItemIDList;
  si: TStartupInfo;
  pi: TProcessInformation;
  cmd: string;
begin
  IIDL := ILCreateFromPath(PChar(s));
  if IIDL <> nil then
  begin
    try
      SHOpenFolderAndSelectItems(IIDL, 0, nil, 0);
    finally
      ILFree(IIDL);
    end;
  end else begin
    FIllChar(si, SizeOf(si), 0);
    FIllChar(pi, SizeOf(pi), 0);
    si.cb := SizeOf(si);
    si.dwFlags := STARTF_USESHOWWINDOW;
    si.wShowWindow := SW_SHOWDEFAULT;
    cmd := Format('explorer.exe /e, /select, "%s"', [s]);
    CreateProcess(nil, PChar(cmd), nil, nil, false, 0, nil, nil, si, pi);
    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);
  end;
end;

procedure SetFileAssociation(aValue: Boolean);
var
  a: TFileAssociation;
begin
  a:=TFileAssociation.Create(nil);
  try
    a.UnReg := not aValue;
    a.ApplicationName := 'Nanobrot';
    a.ApplicationDescription := 'Nanobrot Fractal Generator';
    a.Extension := '.nbr';
    a.ExtensionName := 'Nanobrot Fractal File';
    a.ExtensionIcon := Format('"%s",1', [Application.ExeName]);
    a.Action := Format('"%s" "%%1"', [Application.ExeName]);
    a.ActionName := 'Open';
    a.ActionIcon := Format('"%s",0' ,[Application.ExeName]);
    a.RegisterForAllUsers := true; // you can change it to False and register for current user only
    if a.Execute then
      a.ClearIconCache;
  finally
    a.Free;
  end;
end;

{ TMRUList }

constructor TMRUList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fRecent := TStringList.Create;
  fMaxRecent := 10;
  fMinimizeWidth := 200;
  fEnabled := true;
end;

destructor TMRUList.Destroy;
begin
  FreeAndNil(fRecent);
  inherited Destroy;
end;

procedure TMRUList.Clear;
begin
  fRecent.Clear;
  UpdateRecentFiles;
end;

procedure TMRUList.UpdateRecentFiles;
var
  i, a, b, n: integer;
  m: TRecentMenuItem;
begin
  if (not Assigned(fMIRecent)) or (not Assigned(fTopSepar)) or
    (not Assigned(fBotSepar)) then
    exit;
  a := fMIRecent.IndexOf(fTopSepar) + 1;
  b := fMIRecent.IndexOf(fBotSepar) - 1;
  for i := b downto a do
    fMIRecent.Delete(i);
  n := 0;
  for i := 0 to fRecent.Count - 1 do
  begin
    m := CreateMenuItem(self.Owner);
    m.Caption := CreateMenuCaption(i, fRecent[i]);
    m.fFileName := fRecent[i];
    m.OnClick := DoOnRecentClick;
    fMIRecent.Insert(a + i, m);

    Inc(n);
  end;
  fBotSepar.Visible := n > 0;
end;

procedure TMRUList.AddToRecent(const aFileName: string);
var
  i: integer;
  s: string;
begin
  s := ExpandFileName(aFileName);
  i := fRecent.IndexOf(s);
  if i > -1 then
  begin
    if i > 0 then
      fRecent.Exchange(0, i);
  end
  else
  begin
    while fRecent.Count >= fMaxRecent do
      fRecent.Delete(fRecent.Count - 1);
    fRecent.Insert(0, s);
  end;
  UpdateRecentFiles;
end;

procedure TMRUList.RemoveFromRecent(const aFileName: string);
var
  i: integer;
begin
  i := fRecent.IndexOf(ExpandFileName(aFileName));
  if i > -1 then
    fRecent.Delete(i);
end;

procedure TMRUList.LoadFromIni(Ini: TCustomIniFile; const aSection: string);
var
  i, c: integer;
  s: string;
begin
  fRecent.Clear;
  fMaxRecent := Ini.ReadInteger(aSection, KeyMaxRecent, 10);
  c := Ini.ReadInteger(aSection, KeyCount, 0);
  for i := 1 to c do
  begin
    s := Ini.ReadString(aSection, Format(KeyFile, [i]), '');
    if s <> '' then
      fRecent.Add(s);
  end;
end;

procedure TMRUList.SaveToIni(Ini: TCustomIniFile; const aSection: string);
var
  i: integer;
begin
  Ini.EraseSection(aSection);
  Ini.WriteInteger(aSection, KeyMaxRecent, fMaxRecent);
  Ini.WriteInteger(aSection, KeyCount, fRecent.Count);
  for i := 0 to fRecent.Count - 1 do
    Ini.WriteString(aSection, Format(KeyFile, [i + 1]), fRecent[i]);
  Ini.UpdateFile;
end;

function TMRUList.CreateMenuItem(aOwner: TComponent): TRecentMenuItem;
begin
  Result := TRecentMenuItem.Create(aOwner);
end;

function TMRUList.CreateMenuCaption(aIndex: integer; const aFileName: string): string;
begin
  if (fMinimizeWidth > 0) and Assigned(Application.MainForm) then
    Result := Format('%d. %s', [aIndex + 1, MiniMizeName(aFileName,
      Application.MainForm.Canvas, fMinimizeWidth)])
  else
    Result := Format('%d. %s', [aIndex + 1, aFileName]);
end;

procedure TMRUList.DoOnRecentClick(Sender: TObject);
var
  s: string;
begin
  if Assigned(Sender) and (Sender is TRecentMenuItem) then
    s := (Sender as TRecentMenuItem).FileName;
  if (s <> '') and Assigned(fOnRecent) then
    fOnRecent(self, s);
end;

procedure TMRUList.SetMaxRecent(AValue: integer);
var
  i: integer;
begin
  if fMaxRecent = AValue then
    Exit;
  fMaxRecent := AValue;
  for i := fRecent.Count - 1 downto fMaxRecent do
    fRecent.Delete(i);
  UpdateRecentFiles;
end;

procedure TMRUList.SetTopSepar(const aValue: TSpTBXSeparatorItem);
begin
  if fTopSepar = aValue then
    exit;
  if Assigned(fTopSepar) then
    fTopSepar.RemoveFreeNotification(Self);
  fTopSepar := aValue;
  if Assigned(fTopSepar) then
    fTopSepar.FreeNotification(Self);
  UpdateRecentFiles;
end;

procedure TMRUList.SetBotSepar(const aValue: TSpTBXSeparatorItem);
begin
  if fBotSepar = aValue then
    exit;
  if Assigned(fBotSepar) then
    fBotSepar.RemoveFreeNotification(Self);
  fBotSepar := aValue;
  if Assigned(fBotSepar) then
    fBotSepar.FreeNotification(Self);
  UpdateRecentFiles;
end;

procedure TMRUList.SetMIRecent(const aValue: TSpTBXSubmenuItem);
begin
  if fMIRecent = aValue then
    exit;
  if Assigned(fMIRecent) then
    fMIRecent.RemoveFreeNotification(Self);
  fMIRecent := aValue;
  if Assigned(fMIRecent) then
    fMIRecent.FreeNotification(Self);
  UpdateRecentFiles;
end;

procedure TMRUList.SetRecent(const aValue: TStrings);
begin
  if fRecent = aValue then
    exit;
  fRecent.Assign(aValue);
  UpdateRecentFiles;
end;

procedure TMRUList.Loaded;
begin
  inherited Loaded;
  if (fRecent.Count > 0) and Assigned(fMIRecent) then
    UpdateRecentFiles;
end;

procedure TMRUList.SetEnabled(const Value: Boolean);
var
  a, b, i: Integer;
begin
  if fEnabled = Value then
    Exit;
  fEnabled := Value;
  a := fMIRecent.IndexOf(fTopSepar) + 1;
  b := fMIRecent.IndexOf(fBotSepar) - 1;
  for i := b downto a do
    fMIRecent.Items[i].Enabled := fEnabled;
end;



{ TSavedElement }

constructor TSavedElement.Create(aControl: TWincontrol);
begin
  Control := aControl;
  if Control = nil then
    Exit;
  if Control is TEdit then begin
    SelStart := TEdit(Control).SelStart;
    SelLength := TEdit(Control).SelLength;
  end;
  Control.Enabled := false;
end;

function TSavedElement.Extract: TWincontrol;
begin
  result := Control;
  if (result = nil) or (result is TCustomForm) then
    Exit;
  result.Enabled := true;
  Screen.ActiveForm.FocusControl(result);
  if result is TEdit then begin
    TEdit(result).SelStart := SelStart;
    TEdit(result).SelLength := SelLength;
  end;
  Control := nil;
end;


{ TWICImageAdapter }

constructor TWICImageAdapter.Create;
begin
  if fImagingFactory = nil then
    CoCreateInstance(CLSID_WICImagingFactory, nil, CLSCTX_INPROC_SERVER or
      CLSCTX_LOCAL_SERVER, IUnknown, fImagingFactory)
  else
    fImagingFactory._AddRef;
end;

destructor TWICImageAdapter.Destroy;
begin
  if fImagingFactory._Release = 0 then
    Pointer(fImagingFactory) := nil;
  inherited;
end;

class function TWICImageAdapter.GetImagingFactory: IWICImagingFactory;
begin
  Result := fImagingFactory;
end;

procedure TWICImageAdapter.SaveBitmap32(const aFileName: string;
  ImageFormat: TWICImageFormat; bmp: TBitmap32);
var
  Data: TMemoryStream;
  Size: UInt;
  WicBitmap: IWICBitmap;
  LStreamAdapter: IStream;
  LStream: IWICStream;
  Encoder: IWICBitmapEncoder;
  Frame: IWICBitmapFrameEncode;
  Props: IPropertyBag2;
  EncoderContainerFormat: TGUID;
  PixelFormat: TGUID;
  Palette: IWICPalette;
begin
  case ImageFormat of
    wifBmp:     EncoderContainerFormat := GUID_ContainerFormatBmp;
    wifPng:     EncoderContainerFormat := GUID_ContainerFormatPng;
    wifJpeg:    EncoderContainerFormat := GUID_ContainerFormatJpeg;
    wifGif:     EncoderContainerFormat := GUID_ContainerFormatGif;
    wifTiff:    EncoderContainerFormat := GUID_ContainerFormatTiff;
    wifWMPhoto: EncoderContainerFormat := GUID_ContainerFormatWmp;
    wifOther: ;
  end;
  Data := TMemoryStream.Create;
  try
    Size := bmp.Width * bmp.Height * 4;
    fImagingFactory.CreateBitmapFromMemory(bmp.Width, bmp.Height,
    GUID_WICPixelFormat32bppBGR, bmp.Width * 4, Size, PByte(bmp.Bits), WicBitmap);
    LStreamAdapter := TStreamAdapter.Create(Data);
    fImagingFactory.CreateStream(LStream);
    LStream.InitializeFromIStream(LStreamAdapter);
    FImagingFactory.CreateEncoder(EncoderContainerFormat, guid_null, Encoder);
    Encoder.Initialize(LStream, WICBitmapEncoderNoCache);
    Encoder.CreateNewFrame(Frame, Props);
    Frame.Initialize(Props);
    WicBitmap.GetPixelFormat(PixelFormat);
    Frame.SetPixelFormat(PixelFormat);
    Frame.SetSize(bmp.Width, bmp.Height);
    FImagingFactory.CreatePalette(Palette);
    WicBitmap.CopyPalette(Palette);
    Frame.SetPalette(Palette);
    Frame.WriteSource(WicBitmap, nil);
    Frame.Commit;
    Encoder.Commit;
    Data.Position := 0;
    Data.SaveToFile(aFileName);
  finally
    Data.Free;
  end;
end;

{ TSaveFileDialogHelper }

function TFileDialogHelper.OnFileOk(const pfd: IFileDialog): HResult;
begin
  Result := E_NOTIMPL;
end;

function TFileDialogHelper.OnFolderChange(const pfd: IFileDialog): HResult;
begin
  Result := E_NOTIMPL;
end;

function TFileDialogHelper.OnFolderChanging(const pfd: IFileDialog;
  const psiFolder: IShellItem): HResult;
begin
  Result := E_NOTIMPL;
end;

function TFileDialogHelper.OnOverwrite(const pfd: IFileDialog;
  const psi: IShellItem; out pResponse: DWORD): HResult;
begin
  Result := E_NOTIMPL;
end;

function TFileDialogHelper.OnSelectionChange(const pfd: IFileDialog): HResult;
begin
  Result := E_NOTIMPL;
end;

function TFileDialogHelper.OnShareViolation(const pfd: IFileDialog;
  const psi: IShellItem; out pResponse: DWORD): HResult;
begin
  Result := E_NOTIMPL;
end;

function TFileDialogHelper.OnTypeChange(const pfd: IFileDialog): HResult;
begin
  Result := E_NOTIMPL;
end;

function TFileDialogHelper.OnItemSelected(const pfdc: IFileDialogCustomize; dwIDCtl: DWORD; dwIDItem: DWORD): HResult;
begin
  Result := E_NOTIMPL;
end;

function TFileDialogHelper.OnButtonClicked(const pfdc: IFileDialogCustomize; dwIDCtl: DWORD): HResult;
begin
  if Assigned(fOnDialogCheck) then
    fOnDialogCheck(pfdc, dwIDCtl);
  Result := S_OK;
end;

function TFileDialogHelper.OnCheckButtonToggled(const pfdc: IFileDialogCustomize; dwIDCtl: DWORD; bChecked: BOOL): HResult;
begin
  Result := E_NOTIMPL;
end;

function TFileDialogHelper.OnControlActivating(const pfdc: IFileDialogCustomize; dwIDCtl: DWORD): HResult;
begin
  Result := E_NOTIMPL;
end;

{ TFileAssociation }

constructor TFileAssociation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fRegistry := TRegistry.Create;
  AddApplicationToDefaultPrograms := True;
  AddExtensionToDefaultPrograms := True;
  RegisterFileAssociation := True;
  UnReg := False;
  RegisterForAllUsers := True;
end;

destructor TFileAssociation.Destroy;
begin
  fRegistry.Free;
  inherited Destroy;
end;

procedure TFileAssociation.SetRoot;
begin
  if RegisterForAllUsers then
    fRegistry.RootKey := HKEY_LOCAL_MACHINE
  else
    fRegistry.RootKey := HKEY_CURRENT_USER;
end;

procedure TFileAssociation.SetAction(AValue: string);
begin
  if fAction = AValue then
    Exit;
  fAction := AValue;
end;

procedure TFileAssociation.SetActionIcon(AValue: string);
begin
  if fActionIcon = AValue then
    Exit;
  fActionIcon := AValue;
end;

procedure TFileAssociation.SetActionName(AValue: string);
begin
  if fActionName = AValue then
    Exit;
  fActionName := AValue;
end;

procedure TFileAssociation.SetApplicationDescription(AValue: string);
begin
  if FApplicationDescription = AValue then
    Exit;
  FApplicationDescription := AValue;
end;

procedure TFileAssociation.SetApplicationName(AValue: string);
begin
  if fApplicationName = AValue then
    Exit;
  fApplicationName := AValue;
end;

procedure TFileAssociation.SetExtension(AValue: string);
begin
  if fExtension = AValue then
    Exit;
  fExtension := AValue;
end;

procedure TFileAssociation.SetExtensionIcon(AValue: string);
begin
  if fExtensionIcon = AValue then
    Exit;
  fExtensionIcon := AValue;
end;

procedure TFileAssociation.SetExtensionName(AValue: string);
begin
  if fExtensionName = AValue then
    Exit;
  fExtensionName := AValue;
end;

procedure TFileAssociation.SetRegisterForAllUsers(AValue: boolean);
begin
  fRegisterForAllUsers := AValue;
  SetRoot;
end;

procedure TFileAssociation.SetUnReg(AValue: boolean);
begin
  if fUnReg = AValue then
    Exit;
  fUnReg := AValue;
end;

procedure TFileAssociation.SetAddApplicationToDefaultPrograms(AValue: boolean);
begin
  if fAddApplicationToDefaultPrograms = AValue then
    Exit;
  fAddApplicationToDefaultPrograms := AValue;
end;

procedure TFileAssociation.SetAddExtensionToDefaultPrograms(AValue: boolean);
begin
  if fAddExtensionToDefaultPrograms = AValue then
    Exit;
  fAddExtensionToDefaultPrograms := AValue;
end;

procedure TFileAssociation.SetRegisterFileAssociation(AValue: boolean);
begin
  if fRegisterFileAssociation = AValue then
    Exit;
  fRegisterFileAssociation := AValue;
end;

function TFileAssociation.Execute: boolean;
var
  b1, b2, b3, b4, b5: boolean;
begin
  b1 := WriteFileAssociationClass;
  b2 := WriteFileAssociationClassCommand;

  if RegisterFileAssociation then
    b3 := WriteFileAssociation;

  if RegisterForAllUsers then
  begin
    if AddApplicationToDefaultPrograms then
      b4 := WriteDefaultPrograms;
    if AddExtensionToDefaultPrograms then
      b5 := WriteDefaultProgramsAddExt;
  end;

  Result := b1 and b2 and b3 and b4 and b5;
end;

function TFileAssociation.WriteStringValue(SubKey: string; ValueName: string;
  ValueData: string): boolean;
begin
  Result := fRegistry.OpenKey(SubKey, True);

  if Result then
  begin
    fRegistry.WriteString(ValueName, ValueData);
    fRegistry.CloseKey;
  end;
end;

function TFileAssociation.DeleteValue(SubKey: string; ValueName: string): boolean;
begin
  Result := fRegistry.OpenKey(SubKey, True);
  if Result then
  begin
    fRegistry.DeleteValue(ValueName);
    fRegistry.DeleteKey(ValueName);
    fRegistry.CloseKey;
  end;
end;

procedure TFileAssociation.SetActionText(AValue: string);
begin
  if fActionText = AValue then
    Exit;
  fActionText := AValue;
end;

procedure TFileAssociation.ClearIconCache;
begin
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
end;

function TFileAssociation.StrNoSpaces(const s: string): string;
begin
  Result := StringReplace(s, ' ', '', [rfReplaceAll]);
end;

function TFileAssociation.WriteString(SubKey: string; ValueName: string;
  ValueData: string): boolean;
begin
  if not UnReg then
    Result := WriteStringValue(SubKey, ValueName, ValueData)
  else
    Result := DeleteValue(SubKey, ValueName);
end;

function TFileAssociation.WriteFileAssociationClass: boolean;
var
  b1, b2: boolean;
  sub: string;
begin
  sub := 'Software\Classes\' + StrNoSpaces(ApplicationName) +
    '.AssocFile.' + StrNoSpaces(ExtensionName);

  b1 := WriteString(sub, '', ExtensionName);
  b2 := WriteString(sub + '\DefaultIcon', '', ExtensionIcon);

  Result := b1 and b2;
end;

function TFileAssociation.WriteFileAssociationClassCommand: boolean;
var
  b1, b2, b3: boolean;
  sub: string;
begin
  sub := 'Software\Classes\' + StrNoSpaces(ApplicationName) +
    '.AssocFile.' + StrNoSpaces(ExtensionName) + '\Shell\' + StrNoSpaces(ActionName);

  b1 := WriteString(sub, '', ActionText);
  b2 := WriteString(sub, 'Icon', ActionIcon);
  b3 := WriteString(sub + '\Command', '', Action);

  Result := b1 and b2 and b3;
end;

function TFileAssociation.WriteFileAssociation: boolean;
begin
  Result := WriteString('Software\Classes\' + Extension, '',
    StrNoSpaces(ApplicationName) + '.AssocFile.' + StrNoSpaces(ExtensionName));
end;

function TFileAssociation.WriteDefaultPrograms: boolean;
var
  b1, b2, b3, b4: boolean;
  sub: string;
begin
  sub := 'Software\' + StrNoSpaces(ApplicationName) + '\Capabilities';

  b1 := WriteString(sub, '', '');
  b2 := WriteString(sub, 'ApplicationName', ApplicationName);
  b3 := WriteString(sub, 'ApplicationDescription', ApplicationDescription);
  b4 := WriteString('Software\RegisteredApplications',
    StrNoSpaces(ApplicationName), sub);

  Result := b1 and b2 and b3 and b4;
end;

function TFileAssociation.WriteDefaultProgramsAddExt: boolean;
begin
  Result := WriteString('Software\' + StrNoSpaces(ApplicationName) +
    '\Capabilities\FileAssociations', Extension, StrNoSpaces(ApplicationName) +
    '.AssocFile.' + StrNoSpaces(ExtensionName));
end;

initialization
  InitModule;

finalization
  FreeModule;

end.
