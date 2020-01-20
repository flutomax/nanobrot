unit uPrintDialogs;

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
  Windows, Classes, Messages, SysUtils, Graphics, CommDlg, Dialogs;

const
  // Internal events
  CM_PAINTINIT = WM_USER + 10;
  CM_PAINTPAGE = WM_USER + 11;

  // masks for separation of parameters from TDlgPaintEvent.aFlags
  PRINTER_MASK = $00000002;
  ORIENT_MASK = $00000004;
  PAPER_MASK = $00000008;

type

  TPageSetupDialogEx = class;
  TPageSetupFlagsEx =
    (poDefaultMinMargins, poMargins, poMinMargins, poDisableMargins,
    poDisableOrientation, poDisablePagePainting, poDisablePaper, poDisablePrinter,
    poHundredthsOfMillimeters, poThousandthsOfInches, poNoWarning);
  TPageOptionsEx = set of TPageSetupFlagsEx;

  TDlgPaintWhat =
    (pwFullPage, pwMinimumMargins,
    pwMargins, pwGreekText,
    pwEnvStamp, pwYAFullPage);

  TMarginSizeEx = class(TPersistent)
  private
    fMargin: TRect;
    procedure AssignError;
    function GetValue(Index: integer): integer;
    procedure SetValue(Index: integer; Value: integer);
    procedure SetRect(Value: TRect);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    function IsNull: boolean;
    function MarginsEqu(AMargin: TMarginSizeEx): boolean;
    property AsRect: TRect read fMargin write SetRect;
  published
    property Left: integer index 0 read GetValue write SetValue stored False;
    property Top: integer index 1 read GetValue write SetValue stored False;
    property Right: integer index 2 read GetValue write SetValue stored False;
    property Bottom: integer index 3 read GetValue write SetValue stored False;
  end;


  TDlgPaintEvent = procedure(Sender: TPageSetupDialogEx; Paper, Flags: integer;
    PageSetupRec: TPageSetupDlg; PaintWhat: TDlgPaintWhat; Canvas: TCanvas;
    Rect: TRect; var NoDefaultPaint: boolean) of object;

  TPageSetupDialogEx = class(TCommonDialog)
  private
    fHeader: string;
    fCopies: integer;
    fHelpHeader: integer;
    fHelpCopies: integer;
    fOptions: TPageOptionsEx;
    fFlags: DWORD;
    fMargin: TMarginSizeEx;
    fMinMargin: TMarginSizeEx;
    fPaperSize: TPoint;
    fInitPaper: integer;
    fInitFlags: integer;
    fPageSetupRec: TPageSetupDlg;
    fPaintWhat: TDlgPaintWhat;
    fOnPrinter: TNotifyEvent;
    fOnPaint: TDlgPaintEvent;
    procedure SetOptions(Value: TPageOptionsEx);
    function DoExecute(Show: boolean): boolean;
    procedure ReadMargin(AMargin: TMarginSizeEx; Reader: TReader);
    procedure WriteMargin(AMargin: TMarginSizeEx; Writer: TWriter);
    procedure ReadValues(AReader: TReader);
    procedure WriteValues(AWriter: TWriter);
    procedure ReadMinValues(AReader: TReader);
    procedure WriteMinValues(AWriter: TWriter);
    procedure SetEditText(EditId: integer; Text: string);
    function GetEditText(EditId: integer): string;
    function HandleHelp(const Msg: TWMHelp): Boolean;
    function HandleCommand(const Msg: TWMCommand): Boolean;
    function HandlePaintInit(const Msg: TMessage): Boolean;
    function HandlePaintPage(const Msg: TMessage): Boolean;
  protected
    procedure DefineProperties(AFiler: TFiler); override;
    function DoPrinter: boolean; virtual;
    function DoPaint(InitPaper, InitFlags: integer; PageSetupRec: TPageSetupDlg;
      PaintWhat: TDlgPaintWhat; Canvas: TCanvas; Rect: TRect): boolean; virtual;
    function TaskModalDialog(DialogFunc: Pointer; var DialogData): Bool; override;
    procedure DoShow; override;
    procedure DoClose; override;
    function MessageHook(var Msg: TMessage): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: boolean; override;
    procedure GetDefaults; virtual;
    property PaperSize: TPoint read fPaperSize write fPaperSize;
  published
    property Margin: TMarginSizeEx read fMargin;
    property MinMargin: TMarginSizeEx read fMinMargin;
    property PageHeader: string read fHeader write fHeader;
    property PageCopies: integer read fCopies write fCopies;
    property HelpContextHeader: integer read fHelpHeader write fHelpHeader default 0;
    property HelpContextCopies: integer read fHelpCopies write fHelpCopies default 0;
    property Options: TPageOptionsEx read fOptions write SetOptions default
      [poDefaultMinMargins, poMargins, poHundredthsOfMillimeters];
    property OnPaint: TDlgPaintEvent read fOnPaint write fOnPaint;
    property OnPrinter: TNotifyEvent read fOnPrinter write fOnPrinter;
  end;

  TAddPrinterDialog = class(TCommonDialog)
  published
    function Execute: boolean; override;
  end;

  TPrinterPropertiesDialog = class(TCommonDialog)
  private
    fObjectName: TFileName;
    fInitialTab: string;
    fPrinterIndex: integer;
  public
    function Execute(ParentWnd: HWND): boolean; override;
  published
    property PrinterIndex: integer read fPrinterIndex write fPrinterIndex;
    property ObjectName: TFileName read fObjectName;
    property InitialTab: string read fInitialTab write fInitialTab;
  end;

implementation

{$R PrintPreview.res}

uses
  Controls, Forms, Printers, ShlObj, ShellAPI, ActiveX, uFrmPrint, uMisc;

type
  TSHObjectPropertiesProc = function(Owner: HWND; Flags: UINT;
    ObjectName: Pointer; InitialTabName: Pointer): longbool; stdcall;

  THackCommonDialog = class(TComponent)
  private
    fCtl3D: Boolean;
    fHelpContext: THelpContext;
    fHandle: HWnd;
    fRedirector: TWinControl;
    fTemplateModule: HINST;
    fOnClose: TNotifyEvent;
    fOnShow: TNotifyEvent;
    fDefWndProc: IntPtr;
    fObjectInstance: Pointer;
  end;

const
  // dialog controls
  DLGHEADER = 30;
  DLGCOPIES = 31;
  DLGHEADERLABEL = 32;
  DLGCOPIESLABEL = 33;
  OPF_PRINTERNAME = $01;
  OPF_PATHNAME = $02;

var
  CreationControl: TCommonDialog = nil;
  PageSetupControl: TPageSetupDialogEx = nil;

function OSCheck(RetVal: boolean): boolean;
begin
  if not RetVal then
    RaiseLastOSError;
  Result := RetVal;
end;


function ItemIDListCreate(const Allocator: IMalloc; const Size: integer): PItemIDList;
begin
  Result := Allocator.Alloc(Size);
  if Result <> nil then
    FillChar(Result^, Size, 0);
end;

function ItemIDListGetNextItem(const ItemIDList: PItemIDList): PItemIDList;
begin
  if ItemIDList = nil then
    Result := nil
  else
    Result := PItemIDList(cardinal(ItemIDList) + ItemIDList.mkid.cb);
end;

function ItemIDListGetSize(const ItemIDList: PItemIDList): cardinal;
var
  TempItemIDList: PItemIDList;
begin
  Result := 0;
  TempItemIDList := ItemIDList;
  if TempItemIDList <> nil then
  begin
    while TempItemIDList.mkid.cb > 0 do
    begin
      Inc(Result, TempItemIDList.mkid.cb);
      TempItemIDList := ItemIDListGetNextItem(TempItemIDList);
    end;
    Inc(Result, 2 * SizeOf(byte));
  end;
end;

function ItemIDListsConcatenate(const Allocator: IMalloc;
  const List1, List2: PItemIDList): PItemIDList;
var
  List1Length: cardinal;
  List2Length: cardinal;
  NewItemIDList: PItemIDList;
begin
  List1Length := 0;
  if List1 <> nil then
    List1Length := ItemIDListGetSize(List1) - 2 * SizeOf(byte);
  List2Length := ItemIDListGetSize(List2);
  NewItemIDList := ItemIDListCreate(Allocator, List1Length + List2Length);
  if NewItemIDList <> nil then
  begin
    if List1 <> nil then
      CopyMemory(NewItemIDList, List1, List1Length);
    CopyMemory(Pointer(cardinal(NewItemIDList) + List1Length), List2, List2Length);
  end;
  Result := NewItemIDList;
end;

function GetPrinterItemIDList(const DesktopFolder: IShellFolder): PItemIDList;
begin
  Result := nil;
  if DesktopFolder <> nil then
    if Failed(SHGetSpecialFolderLocation(0, CSIDL_PRINTERS, Result)) then
      Result := nil;
end;

function GetAddPrinterItem(const Allocator: IMalloc): PItemIDList;
var
  DesktopFolder: IShellFolder;
  EnumIDList: IEnumIDList;
  hOK: HRESULT;
  PrinterItemIDList: PItemIDList;
  PrintersFolder: IShellFolder;
  Retrieved: integer;
  TempItemIDList: PItemIDList;
begin
  Result := nil;
  if Allocator <> nil then
    if Succeeded(SHGetDesktopFolder(DesktopFolder)) then
    begin
      PrinterItemIDList := GetPrinterItemIDList(DesktopFolder);
      if PrinterItemIDList <> nil then
      begin
        hOK := DesktopFolder.BindToObject(PrinterItemIDList, nil,
          IID_IShellFolder, Pointer(PrintersFolder));
        if Succeeded(hOK) then
          if Succeeded(PrintersFolder.EnumObjects(0, SHCONTF_FOLDERS or
            SHCONTF_NONFOLDERS, EnumIDList)) then
          begin
            hOK := EnumIDList.Next(1, TempItemIDList, cardinal(Retrieved));
            if (Retrieved > 0) and Succeeded(hOK) then
              Result := ItemIDListsConcatenate(Allocator, PrinterItemIDList,
                TempItemIDList);
          end;
      end;
    end;
end;


// Generic dialog hook. Centers the dialog on the screen in response to
// the WM_INITDIALOG message

function DialogHook(Wnd: HWND; Msg: UINT; AWParam: WPARAM;
  ALParam: LPARAM): UINT_PTR; stdcall;
begin
  Result := 0;
  if Msg = WM_INITDIALOG then
  begin
    SetClassLong(Wnd, GCL_HICON, Application.Icon.Handle);
    CenterWindow(Wnd);
    THackCommonDialog(CreationControl).fHandle := Wnd;
    THackCommonDialog(CreationControl).fDefWndProc :=
      SetWindowLong(Wnd, GWL_WNDPROC, IntPtr(THackCommonDialog(
      CreationControl).fObjectInstance));
    CallWindowProc(THackCommonDialog(CreationControl).fObjectInstance,
      Wnd, Msg, AWParam, ALParam);
    CreationControl := nil;
  end;
end;

function PageDrawHook(Wnd: HWND; Msg: UINT; AWParam: WPARAM;
  ALParam: LPARAM): UINT_PTR; stdcall;
const
  PagePaintWhat: array [WM_PSD_FULLPAGERECT..WM_PSD_YAFULLPAGERECT] of TDlgPaintWhat =
    (pwFullPage, pwMinimumMargins, pwMargins,
    pwGreekText, pwEnvStamp, pwYAFullPage);
var
  m: TMessage;
begin
  case Msg of
    WM_PSD_PAGESETUPDLG:
    begin
      m.Msg := CM_PAINTINIT;
      m.WParam := AWParam;
      m.LParam := ALParam;
      Result := Ord(PageSetupControl.MessageHook(m));
    end;
    WM_PSD_FULLPAGERECT, WM_PSD_MINMARGINRECT, WM_PSD_MARGINRECT,
    WM_PSD_GREEKTEXTRECT, WM_PSD_ENVSTAMPRECT, WM_PSD_YAFULLPAGERECT:
    begin
      m.Msg := CM_PAINTPAGE;
      m.WParam := AWParam;
      m.LParam := ALParam;
      PageSetupControl.fPaintWhat := PagePaintWhat[Msg];
      Result := Ord(PageSetupControl.MessageHook(m));
    end;
    else
      Result := 0;
  end;
end;

{ Printer dialog routines }

procedure GetPrinter(var DeviceMode, DeviceNames: THandle);
var
  Device, Driver, Port: array [0..79] of char;
  DevNames: PDevNames;
  Offset: PChar;
begin
  Printer.GetPrinter(Device, Driver, Port, DeviceMode);
  if DeviceMode <> 0 then
  begin
    DeviceNames := GlobalAlloc(GHND, SizeOf(TDevNames) + StrLen(Device) +
      StrLen(Driver) + StrLen(Port) + 3);
    DevNames := PDevNames(GlobalLock(DeviceNames));
    try
      Offset := PChar(DevNames) + SizeOf(TDevNames);
      with DevNames^ do
      begin
        wDriverOffset := UINT_PTR(Offset) - UINT_PTR(DevNames);
        Offset := StrECopy(Offset, Driver) + 1;
        wDeviceOffset := UINT_PTR(Offset) - UINT_PTR(DevNames);
        Offset := StrECopy(Offset, Device) + 1;
        wOutputOffset := UINT_PTR(Offset) - UINT_PTR(DevNames);
        StrCopy(Offset, Port);
      end;
    finally
      GlobalUnlock(DeviceNames);
    end;
  end;
end;

procedure SetPrinter(DeviceMode, DeviceNames: THandle);
var
  DevNames: PDevNames;
begin
  if DeviceNames = 0 then
    Exit;

  DevNames := PDevNames(GlobalLock(DeviceNames));
  try
    with DevNames^ do
      Printer.SetPrinter(PChar(DevNames) + wDeviceOffset,
        PChar(DevNames) + wDriverOffset,
        PChar(DevNames) + wOutputOffset, DeviceMode);
  finally
    GlobalUnlock(DeviceNames);
    GlobalFree(DeviceNames);
  end;
end;

function CopyData(Handle: THandle): THandle;
var
  Src, Dest: PChar;
  Size: integer;
begin
  if Handle <> 0 then
  begin
    Size := GlobalSize(Handle);
    Result := GlobalAlloc(GHND, Size);
    if Result <> 0 then
      try
        Src := GlobalLock(Handle);
        Dest := GlobalLock(Result);
        if (Src <> nil) and (Dest <> nil) then
          Move(Src^, Dest^, Size);
      finally
        GlobalUnlock(Handle);
        GlobalUnlock(Result);
      end;
  end
  else
    Result := 0;
end;

procedure TMarginSizeEx.AssignError;
begin
  raise ERangeError.Create('Illegal value');
end;

procedure TMarginSizeEx.AssignTo(Dest: TPersistent);
begin
  if Dest is TMarginSizeEx then
    with Dest as TMarginSizeEx do
      fMargin := Self.fMargin
  else
    inherited AssignTo(Dest);
end;

function TMarginSizeEx.IsNull: boolean;
begin
  with fMargin do
    Result := (Left = 0) and (Top = 0) and (Right = 0) and (Bottom = 0);
end;

function TMarginSizeEx.MarginsEqu(AMargin: TMarginSizeEx): boolean;
begin
  Result := (fMargin.Left = AMargin.Left) and (fMargin.Top = AMargin.Top) and
    (fMargin.Right = AMargin.Right) and (fMargin.Bottom = AMargin.Bottom);
end;

function TMarginSizeEx.GetValue(Index: integer): integer;
begin
  case Index of
    0: Result := fMargin.Left;
    1: Result := fMargin.Top;
    2: Result := fMargin.Right;
    else
      Result := fMargin.Bottom;
  end;
end;

procedure TMarginSizeEx.SetValue(Index: integer; Value: integer);
begin
  if Value < 0 then
    AssignError;
  case Index of
    0:
      fMargin.Left := Value;
    1:
      fMargin.Top := Value;
    2:
      fMargin.Right := Value;
    else
      fMargin.Bottom := Value;
  end;
end;

procedure TMarginSizeEx.SetRect(Value: TRect);
begin
  with Value do
    if (Left < 0) or (Top < 0) or (Right < 0) or (Bottom < 0) then
      AssignError;
  fMargin := Value;
end;

{ TPageSetupDialogEx }

constructor TPageSetupDialogEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fMargin := TMarginSizeEx.Create;
  fMinMargin := TMarginSizeEx.Create;
  fCopies := 1;
  Options := [poDefaultMinMargins, poMargins, poHundredthsOfMillimeters];
  Template := MakeIntResource(14);
end;

destructor TPageSetupDialogEx.Destroy;
begin
  fMargin.Free;
  fMinMargin.Free;
  inherited Destroy;
end;

procedure TPageSetupDialogEx.DefineProperties(AFiler: TFiler);

// Rule 1
  function DoWriteMargin1: boolean;
  begin
    if AFiler.Ancestor <> nil then
      Result := not TPageSetupDialogEx(AFiler.Ancestor).fMargin.MarginsEqu(fMargin)
    else
      Result := (fMargin <> nil) and (not fMargin.IsNull);
  end;

  // Rule 2
  function DoWriteMargin2: boolean;
  begin
    if AFiler.Ancestor <> nil then
      Result := not TPageSetupDialogEx(AFiler.Ancestor).fMinMargin.MarginsEqu(fMinMargin)
    else
      Result := (fMinMargin <> nil) and (not fMinMargin.IsNull);
  end;

begin
  inherited DefineProperties(AFiler);
  with AFiler do
  begin
    DefineProperty('MarginData', ReadValues, WriteValues, DoWriteMargin1);
    DefineProperty('MinMarginData', ReadMinValues, WriteMinValues, DoWriteMargin2);
  end;
end;

// Reading from stream

procedure TPageSetupDialogEx.ReadMargin(AMargin: TMarginSizeEx; Reader: TReader);
begin
  with AMargin, Reader do
  begin
    ReadListBegin;
    Left := ReadInteger;
    Top := ReadInteger;
    Right := ReadInteger;
    Bottom := ReadInteger;
    ReadListEnd;
  end;
end;

// Writing to stream

procedure TPageSetupDialogEx.WriteMargin(AMargin: TMarginSizeEx; Writer: TWriter);
begin
  with AMargin, Writer do
  begin
    WriteListBegin;
    WriteInteger(Left);
    WriteInteger(Top);
    WriteInteger(Right);
    WriteInteger(Bottom);
    WriteListEnd;
  end;
end;

procedure TPageSetupDialogEx.ReadValues(AReader: TReader);
begin
  ReadMargin(fMargin, AReader);
end;

procedure TPageSetupDialogEx.WriteValues(AWriter: TWriter);
begin
  WriteMargin(fMargin, AWriter);
end;

procedure TPageSetupDialogEx.ReadMinValues(AReader: TReader);
begin
  ReadMargin(fMinMargin, AReader);
end;

procedure TPageSetupDialogEx.WriteMinValues(AWriter: TWriter);
begin
  WriteMargin(fMinMargin, AWriter);
end;

// Processing Help commands

function TPageSetupDialogEx.HandleHelp(const Msg: TWMHelp): Boolean;

  procedure ShowHelp(ContextID: integer);
  var
    Pt: TSmallPoint;
  begin
    Pt := PointToSmallPoint(Msg.HelpInfo^.MousePos);
    Application.HelpCommand(HELP_SETPOPUP_POS, LongInt(Pt));
    Application.HelpCommand(HELP_CONTEXTPOPUP, ContextID);
  end;

begin
  case Msg.HelpInfo^.iCtrlId of
    DLGHEADER, DLGHEADERLABEL:
      if fHelpHeader <> 0 then
        ShowHelp(fHelpHeader);
    DLGCOPIES, DLGCOPIESLABEL:
      if fHelpCopies <> 0 then
        ShowHelp(fHelpCopies)
    else
      if HelpContext <> 0 then
        Application.HelpContext(HelpContext)
  end;
  result := true;
end;

// Processing [Printer] button

function TPageSetupDialogEx.HandleCommand(const Msg: TWMCommand): Boolean;
const
  IDPRINTERBTN = $0402;
begin
  Result := (not ((Msg.ItemID = IDPRINTERBTN) and (Msg.NotifyCode = BN_CLICKED) and
    DoPrinter))
end;

function TPageSetupDialogEx.HandlePaintInit(const Msg: TMessage): Boolean;
begin
  fInitPaper := LoWord(Msg.WParam);
  fInitFlags := HiWord(Msg.WParam);
  fPageSetupRec := PPageSetupDlg(Msg.LParam)^;
  Result := not Assigned(fOnPaint);
end;

function TPageSetupDialogEx.HandlePaintPage(const Msg: TMessage): Boolean;
var
  PaintRect: TRect;
  Canvas: TCanvas;
begin
  if Msg.LParam <> 0 then
    PaintRect := PRect(Msg.LParam)^
  else
    PaintRect := Rect(0, 0, 0, 0);

  Canvas := TCanvas.Create;
  Canvas.Handle := HDC(Msg.WParam);
  try
    Result := DoPaint(fInitPaper, fInitFlags, fPageSetupRec, fPaintWhat, Canvas,
      PaintRect);
  finally
    Canvas.Free;
  end;
end;


function TPageSetupDialogEx.MessageHook(var Msg: TMessage): Boolean;
begin
  case Msg.Msg of
    WM_HELP: Result := HandleHelp(TWMHelp(Msg));
    WM_COMMAND: Result := HandleCommand(TWMCommand(Msg));
    CM_PAINTINIT: Result := HandlePaintInit(Msg);
    CM_PAINTPAGE: Result := HandlePaintPage(Msg);
    else Result := inherited MessageHook(Msg);
  end;
end;


function TPageSetupDialogEx.DoPrinter: boolean;
begin
  Result := Assigned(fOnPrinter);
  if Result then
    fOnPrinter(Self);
end;

function TPageSetupDialogEx.DoPaint(InitPaper, InitFlags: integer;
  PageSetupRec: TPageSetupDlg; PaintWhat: TDlgPaintWhat; Canvas: TCanvas;
  Rect: TRect): boolean;
begin
  Result := False;
  if Assigned(fOnPaint) then
    fOnPaint(Self, InitPaper, InitFlags, PageSetupRec, PaintWhat, Canvas, Rect, Result);
end;

// Show modal dialog

function TPageSetupDialogEx.TaskModalDialog(DialogFunc: Pointer; var DialogData): Bool;
type
  TDialogFunc = function(var ADialogData): Bool; stdcall;
  TDialogStruct = record
    cbSize: Integer;
    hwndOwner: HWND;
  end;
var
  ActiveWindow: HWND;
  FocusState: TFocusState;
  WindowList: Pointer;
  {$IFDEF CPUX86}
  FPUControlWord: word;
  {$ENDIF}
begin
  ActiveWindow := Application.ActiveFormHandle;
  WindowList := DisableTaskWindows(TDialogStruct(DialogData).hwndOwner);
  FocusState := SaveFocusState;
  try
    Application.HookMainWindow(MessageHook);
    {$IFDEF CPUX86}
    asm
      // Avoid FPU control word change in NETRAP.dll, NETAPI32.dll, etc
      FNSTCW  FPUControlWord
    end;
    {$ENDIF}
    try
      CreationControl := Self;
      PageSetupControl := Self;
      Result := TDialogFunc(DialogFunc)(DialogData);
    finally
      PageSetupControl := nil;
      {$IFDEF CPUX86}
      asm
        FNCLEX
        FLDCW FPUControlWord
      end;
      {$ENDIF}
      Application.UnhookMainWindow(MessageHook);
    end;
  finally
    EnableTaskWindows(WindowList);
    SetActiveWindow(ActiveWindow);
    RestoreFocusState(FocusState);
  end;
end;

function TPageSetupDialogEx.DoExecute(Show: boolean): boolean;
var
  PageDlgRec: TPageSetupDlg;
  DevHandle: THandle;
  Err: integer;
begin
  // fill record
  FillChar(PageDlgRec, SizeOf(PageDlgRec), 0);
  with PageDlgRec do
  begin
    lStructSize := SizeOf(PageDlgRec);
    hwndOwner := Application.Handle;
    Flags := fFlags;
    rtMinMargin := Rect(fMinMargin.Left, fMinMargin.Top, fMinMargin.Right,
      fMinMargin.Bottom);
    rtMargin := Rect(fMargin.Left, fMargin.Top, fMargin.Right, fMargin.Bottom);
    hInstance := SysInit.HInstance;
    if Show then
    begin
      lpfnPageSetupHook := DialogHook;
      Flags := fFlags or PSD_ENABLEPAGESETUPHOOK;
      GetPrinter(DevHandle, hDevNames);
      hDevMode := CopyData(DevHandle);
    end
    else
      Flags := Flags or PSD_RETURNDEFAULT;
    if Template <> nil then
    begin
      Flags := Flags or PSD_ENABLEPAGESETUPTEMPLATE;
      lpPageSetupTemplateName := Template;
    end;
    if Assigned(fOnPaint) then
    begin
      Flags := Flags or PSD_ENABLEPAGEPAINTHOOK;
      lpfnPagePaintHook := PageDrawHook;
    end;

    if Show then
      Result := TaskModalDialog(@PageSetupDlg, PageDlgRec)
    else
      Result := PageSetupDlg(PageDlgRec);
    Err := CommDlgExtendedError;

    if Result then
      SetPrinter(hDevMode, hDevNames)
    else
    begin
      if hDevMode <> 0 then
        GlobalFree(hDevMode);
      if hDevNames <> 0 then
        GlobalFree(hDevNames);
    end;
    OSCheck(Err = 0);

    fMargin.AsRect := rtMargin;
    fPaperSize := ptPaperSize;
  end;
end;

function TPageSetupDialogEx.Execute: boolean;
begin
  Result := DoExecute(True);
end;

// Get default margin values

procedure TPageSetupDialogEx.GetDefaults;
begin
  DoExecute(False);
end;

procedure TPageSetupDialogEx.SetOptions(Value: TPageOptionsEx);
const
  WinFlags: array [TPageSetupFlagsEx] of DWORD =
    (PSD_DEFAULTMINMARGINS, PSD_MARGINS, PSD_MINMARGINS,
    PSD_DISABLEMARGINS, PSD_DISABLEORIENTATION,
    PSD_DISABLEPAGEPAINTING, PSD_DISABLEPAPER, PSD_DISABLEPRINTER,
    PSD_INHUNDREDTHSOFMILLIMETERS, PSD_INTHOUSANDTHSOFINCHES,
    PSD_NOWARNING);
var
  I: TPageSetupFlagsEx;
begin
  if (poDefaultMinMargins in Value) and not (poDefaultMinMargins in fOptions) then
    Value := Value - [poMinMargins];
  if (poMinMargins in Value) and not (poMinMargins in fOptions) then
    Value := Value - [poDefaultMinMargins];
  if (poHundredthsOfMillimeters in Value) and not
    (poHundredthsOfMillimeters in fOptions) then
    Value := Value - [poThousandthsOfInches];
  if (poThousandthsOfInches in Value) and not (poThousandthsOfInches in fOptions) then
    Value := Value - [poHundredthsOfMillimeters];
  fOptions := Value;

  // set flags
  fFlags := 0;
  for I := Low(TPageSetupFlagsEx) to High(TPageSetupFlagsEx) do
    if I in fOptions then
      fFlags := fFlags or WinFlags[I];
end;

procedure TPageSetupDialogEx.DoClose;
var
  s: string[100];
begin
  fHeader := GetEditText(DLGHEADER);
  s := GetEditText(DLGCOPIES);
  fCopies := StrToIntDef(s, 1);
  if fCopies < 1 then
    fCopies := 1;
  inherited DoClose;
end;

procedure TPageSetupDialogEx.DoShow;
var
  s: string[100];
begin
  SetEditText(DLGHEADER, fHeader);
  s := IntToStr(fCopies);
  SetEditText(DLGCOPIES, s);
  inherited DoShow;
end;

function TPageSetupDialogEx.GetEditText(EditId: integer): string;
var
  Len: integer;
begin
  Len := SendDlgItemMessage(Handle, EditId, WM_GETTEXTLENGTH, 0, 0);
  SetLength(Result, Len);
  GetDlgItemText(Handle, EditId, PChar(Result), Len + 1);
end;

procedure TPageSetupDialogEx.SetEditText(EditId: integer; Text: string);
begin
  SetDlgItemText(Handle, EditId, PChar(Text));
end;

{ TAddPrinterDialog }

function TAddPrinterDialog.Execute: boolean;
var
  AddPrinterItemIDList: PItemIDList;
  Allocator: IMalloc;
  ShellExecuteInfo: TShellExecuteInfo;
begin
  Result := False;
  if CoGetMalloc(MEMCTX_TASK, Allocator) = S_OK then
  begin
    AddPrinterItemIDList := GetAddPrinterItem(Allocator);
    try
      if AddPrinterItemIDList <> nil then
      begin
        FillChar(ShellExecuteInfo, SizeOf(TShellExecuteInfo), 0);
        with ShellExecuteInfo do
        begin
          cbSize := SizeOf(TShellExecuteInfo);
          fMask := SEE_MASK_INVOKEIDLIST or SEE_MASK_FLAG_NO_UI;
          lpIDList := AddPrinterItemIDList;
          nShow := SW_SHOWDEFAULT;
        end;
        // (rom) now reports success
        Result := ShellExecuteEx(@ShellExecuteInfo);
      end;
    finally
      Allocator.Free(AddPrinterItemIDList);
    end;
  end;
end;

{ TPrinterPropertiesDialog }

function TPrinterPropertiesDialog.Execute(ParentWnd: HWND): boolean;
var
  ShellHandle: THandle;
  ObjectNameBuffer: Pointer;
  TabNameBuffer: Pointer;
begin
  fPrinterIndex := Printer.PrinterIndex;
  fObjectName := Printer.Printers[fPrinterIndex];
  GetMem(ObjectNameBuffer, (Length(ObjectName) + 1) * SizeOf(widechar));
  try
    if SysUtils.Win32Platform = VER_PLATFORM_WIN32_NT then
    begin
      StringToWideChar(ObjectName, PWideChar(ObjectNameBuffer), Length(ObjectName) + 1);
    end
    else
    begin
      StrPCopy(PChar(ObjectNameBuffer), ObjectName);
    end;
    GetMem(TabNameBuffer, (Length(InitialTab) + 1) * SizeOf(widechar));
    try
      if SysUtils.Win32Platform = VER_PLATFORM_WIN32_NT then
      begin
        StringToWideChar(InitialTab, PWideChar(TabNameBuffer), Length(InitialTab) + 1);
      end
      else
      begin
        StrPCopy(PChar(TabNameBuffer), InitialTab);
      end;

      Result := SHObjectProperties(ParentWnd, OPF_PRINTERNAME,
          ObjectNameBuffer, TabNameBuffer);

    finally
      FreeMem(TabNameBuffer);
    end;
  finally
    FreeMem(ObjectNameBuffer);
  end;
end;

end.
