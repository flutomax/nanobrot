unit uPrintPreview;

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
  Windows, WinSpool, Messages, Classes, Graphics, Controls, SysUtils, Forms,
  Dialogs, StdCtrls, ExtCtrls, Menus, Printers, CommDlg, uPrintDialogs;

const
  crHand = 10;
  crGrab = 11;

var
  AllowTransparentDIB: boolean = False;

type

  TPaperType = (pLetter, pLetterSmall, pTabloid, pLedger, pLegal, pStatement,
    pExecutive, pA3, pA4, pA4Small, pA5, pB4, pB5, pFolio, pQuatro, p10x14,
    p11x17, pNote, pEnv9, pEnv10, pEnv11, pEnv12, pEnv14, pCSheet, pDSheet,
    pESheet, pEnvDL, pEnvC5, pEnvC3, pEnvC4, pEnvC6, pEnvC65, pEnvB4, pEnvB5,
    pEnvB6, pEnvItaly, pEnvMonarch, pEnvPersonal, pFanfoldUSStd, pFanfoldGermanStd,
    pFanfoldGermanLegal, pB4ISO, pJapanesePostcard, p9x11, p10x11, p15x11,
    pEnvInvite, pLetterExtra, pLegalExtra, pTabloidExtra, pA4Extra, pLetterTransverse,
    pA4Transverse, pLetterExtraTransverse, pAPlus, pBPlus, pLetterPlus, pA4Plus,
    pA5Transverse, pB5Transverse, pA3Extra, pA5Extra, pB5Extra, pA2, pA3Transverse,
    pA3ExtraTransverse, pCustom);
  TUpdateSeverity = (usNone, usRedraw, usRecreate);
  TUnits = (mmPixel, mmLoMetric, mmHiMetric, mmLoEnglish, mmHiEnglish,
    mmTWIPS, mmPoints);
  TGrayscaleOption = (gsPreview, gsPrint);
  TGrayscaleOptions = set of TGrayscaleOption;
  TPreviewState = (psReady, psCreating, psPrinting, psEditing, psReplacing,
    psInserting, psLoading, psSaving);
  TZoomState = (zsZoomOther, zsZoomToWidth, zsZoomToHeight, zsZoomToFit);
  TPageProcessingChoice = (pcAccept, pcIgnore, pcCancellAll);
  TVertAlign = (vaTop, vaCenter, vaBottom);  //rmk
  THorzAlign = (haLeft, haCenter, haRight);

  TPagePaintEvent = procedure(Sender: TObject; Canvas: TCanvas;
    const Rect: TRect) of object;
  TPagePreviewChangeEvent = procedure(Sender: TObject;
    Severity: TUpdateSeverity) of object;
  TPreviewPageProcessingEvent = procedure(Sender: TObject; PageNo: integer;
    var Choice: TPageProcessingChoice) of object;
  TPreviewPageDrawEvent = procedure(Sender: TObject; PageNo: integer;
    Canvas: TCanvas) of object;
  TPreviewProgressEvent = procedure(Sender: TObject; Done, Total: integer) of object;

  EPrintPreviewError = class(Exception);
  EPreviewLoadError = class(EPrintPreviewError);

  TPagePreviewOptions = class(TPersistent)
  private
    fPageColor: TColor;
    fBorderColor: TColor;
    fBorderWidth: TBorderWidth;
    fShadowColor: TColor;
    fShadowWidth: TBorderWidth;
    fCursor: TCursor;
    fDragCursor: TCursor;
    fGrabCursor: TCursor;
    fPopupMenu: TPopupMenu;
    fHint: string;
    fOnChange: TPagePreviewChangeEvent;
    procedure SeTPageColor(Value: TColor);
    procedure SetBorderColor(Value: TColor);
    procedure SetBorderWidth(Value: TBorderWidth);
    procedure SetShadowColor(Value: TColor);
    procedure SetShadowWidth(Value: TBorderWidth);
    procedure SetCursor(Value: TCursor);
    procedure SetDragCursor(Value: TCursor);
    procedure SetGrabCursor(Value: TCursor);
    procedure SetPopupMenu(Value: TPopupMenu);
    procedure SetHint(const Value: string);
  protected
    procedure DoChange(Severity: TUpdateSeverity);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure CalcDimensions(PageWidth, PageHeight: integer;
      out PageRect, BoxRect: TRect);
    procedure Draw(Canvas: TCanvas; const BoxRect: TRect);
    property OnChange: TPagePreviewChangeEvent read fOnChange write fOnChange;
  published
    property BorderColor: TColor read fBorderColor write SetBorderColor default clBlack;
    property BorderWidth: TBorderWidth read fBorderWidth write SetBorderWidth default 1;
    property Cursor: TCursor read fCursor write SetCursor default crDefault;
    property DragCursor: TCursor read fDragCursor write SetDragCursor default crHand;
    property GrabCursor: TCursor read fGrabCursor write SetGrabCursor default crGrab;
    property Hint: string read FHint write SetHint;
    property PageColor: TColor read fPageColor write SeTPageColor default clWhite;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property ShadowColor: TColor read fShadowColor write SetShadowColor default clBtnShadow;
    property ShadowWidth: TBorderWidth read fShadowWidth write SetShadowWidth default 3;
  end;

  TPagePreview = class(TCustomControl)
  private
    fPreservePageSize: boolean;
    fPageColor: TColor;
    fBorderColor: TColor;
    fBorderWidth: TBorderWidth;
    fShadowColor: TColor;
    fShadowWidth: TBorderWidth;
    fShowCaption: boolean;
    fAlignment: TAlignment;
    fWordWrap: boolean;
    fCaptionHeight: integer;
    fPageRect: TRect;
    fOffScreen: TBitmap;
    fIsOffScreenPrepared: boolean;
    fIsOffScreenReady: boolean;
    fLastVisibleRect: TRect;
    fLastVisiblePageRect: TRect;
    fPageCanvas: TCanvas;
    fOnResize: TNotifyEvent;
    fOnPaint: TPagePaintEvent;
    fOnMouseEnter: TNotifyEvent;
    fOnMouseLeave: TNotifyEvent;
    procedure SeTPageWidth(Value: integer);
    function GeTPageWidth: integer;
    procedure SeTPageHeight(Value: integer);
    function GeTPageHeight: integer;
    function GeTPageSize: TPoint;
    procedure SeTPageSize(const Value: TPoint);
    procedure SeTPageColor(Value: TColor);
    procedure SetBorderColor(Value: TColor);
    procedure SetBorderWidth(Value: TBorderWidth);
    procedure SetShadowColor(Value: TColor);
    procedure SetShadowWidth(Value: TBorderWidth);
    procedure SetShowCaption(Value: boolean);
    procedure SetAlignment(Value: TAlignment);
    procedure SetWordWrap(Value: boolean);
    procedure UpdateCaptionHeight;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure BiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
  protected
    procedure Paint; override;
    procedure DrawPage(Canvas: TCanvas); virtual;
    function ActualWidth(Value: integer): integer; virtual;
    function ActualHeight(Value: integer): integer; virtual;
    function LogicalWidth(Value: integer): integer; virtual;
    function LogicalHeight(Value: integer): integer; virtual;
    procedure InvalidateAll; virtual;
    property CaptionHeight: integer read fCaptionHeight;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Invalidate; override;
    function ClientToPage(const Pt: TPoint): TPoint;
    function PageToClient(const Pt: TPoint): TPoint;
    procedure SetBoundsEx(ALeft, ATop, APageWidth, APageHeight: integer);
    property PageSize: TPoint read GeTPageSize write SeTPageSize;
    property PageRect: TRect read fPageRect;
  published
    property Align;
    property Alignment: TAlignment read fAlignment write SetAlignment default taCenter;
    property BiDiMode;
    property BorderColor: TColor read fBorderColor write SetBorderColor default clBlack;
    property BorderWidth: TBorderWidth read fBorderWidth write SetBorderWidth default 1;
    property Caption;
    property Color;
    property Cursor;
    property DragCursor;
    property DragMode;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property PageColor: TColor read fPageColor write SeTPageColor default clWhite;
    property PageWidth: integer read GeTPageWidth write SeTPageWidth;
    property PageHeight: integer read GeTPageHeight write SeTPageHeight;
    property PreservePageSize: boolean read fPreservePageSize
      write fPreservePageSize default True;
    property ShadowColor: TColor read fShadowColor write SetShadowColor default
      clBtnShadow;
    property ShadowWidth: TBorderWidth read fShadowWidth write SetShadowWidth default 3;
    property ShowCaption: boolean read fShowCaption write SetShowCaption default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property WordWrap: boolean read fWordWrap write SetWordWrap default True;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter: TNotifyEvent read fOnMouseEnter write fOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read fOnMouseLeave write fOnMouseLeave;
    property OnResize: TNotifyEvent read fOnResize write fOnResize;
    property OnPaint: TPagePaintEvent read fOnPaint write fOnPaint;
  end;

  TPrintPreview = class(TScrollBox)
  private
    fPageView: TPagePreview;
    fPageViewOptions: TPagePreviewOptions;
    fBitmap: TBitmap;
    fPrintJobTitle: string;
    fPageHeader: string;
    fIniFileName: string;
    fPageCanvas: TCanvas;
    fUnits: TUnits;
    fDeviceExt: TPoint;
    fLogicalExt: TPoint;
    fPageExt: TPoint;
    fMarginRect: TRect;
    fOrientation: TPrinterOrientation;
    fPaperType: TPaperType;
    fState: TPreviewState;
    fZoom: integer;
    fZoomState: TZoomState;
    fZoomSavePos: boolean;
    fZoomMin: integer;
    fZoomMax: integer;
    fZoomStep: integer;
    fLastZoom: integer;
    fUsePrinterOptions: boolean;
    fDirectPrint: boolean;
    fDirectPrinting: boolean;
    fDirectPrintPageCount: integer;
    fOldMousePos: TPoint;
    fCanScrollHorz: boolean;
    fCanScrollVert: boolean;
    fIsDragging: boolean;
    fFormName: string;
    fVirtualFormName: string;
    fGrayscale: TGrayscaleOptions;
    fGrayBrightness: integer;
    fGrayContrast: integer;
    fShowPrintableArea: boolean;
    fShowMarginArea: boolean;
    fPrintableAreaColor: TColor;
    fMarginAreaColor: TColor;
    fPageMetafile: TMetafile;
    fWheelAccumulator: integer;
    fReferenceDC: HDC;
    fOnBeginDoc: TNotifyEvent;
    fOnEndDoc: TNotifyEvent;
    fOnNewPage: TNotifyEvent;
    fOnEndPage: TNotifyEvent;
    fOnStateChange: TNotifyEvent;
    fOnPageChange: TNotifyEvent;
    fOnBeforePrint: TNotifyEvent;
    fOnAfterPrint: TNotifyEvent;
    fOnZoomChange: TNotifyEvent;
    fOnAnnotation: TPreviewPageDrawEvent;
    fOnBackground: TPreviewPageDrawEvent;
    fOnPrintAnnotation: TPreviewPageDrawEvent;
    fOnPrintBackground: TPreviewPageDrawEvent;
    procedure SetBitmap(Value: TBitmap);
    procedure SetPageViewOptions(Value: TPagePreviewOptions);
    procedure SetUnits(Value: TUnits);
    procedure SetPaperType(Value: TPaperType); overload;
    procedure SetPaperType(const Value: string); overload;
    function GetPaperType: string;
    function GetPageWidth: integer;
    procedure SetPageWidth(Value: integer);
    function GetPageHeight: integer;
    procedure SetPageHeight(Value: integer);
    procedure SetGrayscale(Value: TGrayscaleOptions);
    procedure SetGrayBrightness(Value: integer);
    procedure SetGrayContrast(Value: integer);
    function GetFormName: string;
    procedure SetFormName(const Value: string);
    function GetPageBounds: TRect;
    function GetPrinterPageBounds: TRect;
    function GetPrinterPhysicalPageBounds: TRect;
    function GetOrientation: string;
    procedure SetOrientation(Value: TPrinterOrientation); overload;
    procedure SetOrientation(const Value: string); overload;
    procedure SetZoomState(Value: TZoomState);
    procedure SetZoom(Value: integer);
    procedure SetZoomMin(Value: integer);
    procedure SetZoomMax(Value: integer);
    function GetCanvas: TCanvas;
    function GetPrinterInstalled: boolean;
    function GetPrinter: TPrinter;
    procedure SetShowPrintableArea(Value: boolean);
    procedure SetShowMarginArea(Value: boolean);
    procedure SetPrintableAreaColor(Value: TColor);
    procedure SetMarginAreaColor(Value: TColor);
    procedure SetDirectPrint(Value: boolean);
    procedure SetPageHeader(Value: string);
    function GetIsDummyFormName: boolean;
    function GetSystemDefaultUnits: TUnits;
    function GetUserDefaultUnits: TUnits;
    function IsZoomStored: boolean;
    procedure PageClick(Sender: TObject);
    procedure PageDblClick(Sender: TObject);
    procedure PageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure PageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PageViewOptionsChanged(Sender: TObject; Severity: TUpdateSeverity);
    procedure PaintPage(Sender: TObject; Canvas: TCanvas; const Rect: TRect);
    procedure UpdateBitmap;
    procedure CNKeyDown(var Message: TWMKey); message CN_KEYDOWN;
    procedure WMMouseWheel(var Message: TMessage); message WM_MOUSEWHEEL;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMHScroll(var Message: TWMScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMScroll); message WM_VSCROLL;
  protected
    procedure Loaded; override;
    procedure Resize; override;
    procedure CreateWnd; override;
    procedure DoPageChange; virtual;
    procedure ChangeState(NewState: TPreviewState);
    procedure PreviewPage(Canvas: TCanvas; const Rect: TRect); virtual;
    procedure PrintPage(Canvas: TCanvas; const Rect: TRect); virtual;
    function FindPaperTypeBySize(APageWidth, APageHeight: integer): TPaperType;
    function FindPaperTypeByID(ID: integer): TPaperType;
    function GetPaperTypeSize(APageType: TPaperType;
      out APageWidth, APageHeight: integer; OutUnits: TUnits): boolean;
    procedure SetPageSize(AWidth, AHeight: integer); overload;
    procedure SetPageSize(const Value: TPoint); overload;
    procedure SetPageSizeOrientation(AWidth, AHeight: integer;
      AOrientation: TPrinterOrientation; redraw: boolean = True);
    procedure ResetPrinterDC;
    procedure InitializePrinting; virtual;
    procedure FinalizePrinting(Succeeded: boolean); virtual;
    function GetVisiblePageRect: TRect;
    procedure SetVisiblePageRect(const Value: TRect);
    procedure UpdateZoomEx(X, Y: integer); virtual;
    function CalculateViewSize(const Space: TPoint): TPoint; virtual;
    procedure UpdateExtends; virtual;
    procedure CreateMetafileCanvas(out AMetafile: TMetafile;
      out ACanvas: TCanvas); virtual;
    procedure CloseMetafileCanvas(var AMetafile: TMetafile;
      var ACanvas: TCanvas); virtual;
    procedure CreatePrinterCanvas(out ACanvas: TCanvas); virtual;
    procedure ClosePrinterCanvas(var ACanvas: TCanvas); virtual;
    procedure ScaleCanvas(ACanvas: TCanvas); virtual;
    function HorzPixelsPerInch: integer; virtual;
    function VertPixelsPerInch: integer; virtual;
    function LoadPageInfo(Stream: TStream): boolean; virtual;
    procedure SavePageInfo(Stream: TStream); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ConvertPoints(var Points; NumPoints: integer; InUnits, OutUnits: TUnits);
    function ConvertXY(X, Y: integer; InUnits, OutUnits: TUnits): TPoint;
    function ConvertX(X: integer; InUnits, OutUnits: TUnits): integer;
    function ConvertY(Y: integer; InUnits, OutUnits: TUnits): integer;
    function BoundsFrom(AUnits: TUnits; ALeft, ATop, AWidth, AHeight: integer): TRect;
    function RectFrom(AUnits: TUnits; ALeft, ATop, ARight, ABottom: integer): TRect;
    function PointFrom(AUnits: TUnits; X, Y: integer): TPoint;
    function XFrom(AUnits: TUnits; X: integer): integer;
    function YFrom(AUnits: TUnits; Y: integer): integer;
    function ScreenToPreview(X, Y: integer): TPoint;
    function PreviewToScreen(X, Y: integer): TPoint;
    function ScreenToPage(const Pt: TPoint): TPoint;
    function PageToScreen(const Pt: TPoint): TPoint;
    function ClientToPage(const Pt: TPoint): TPoint;
    function PageToClient(const Pt: TPoint): TPoint;
    function PaintGraphic(X, Y: integer; Graphic: TGraphic): TPoint;
    function PaintGraphicEx(const Rect: TRect; Graphic: TGraphic;
      Proportinal, ShrinkOnly, Center: boolean): TRect;
    function PaintGraphicEx2(const Rect: TRect; Graphic: TGraphic;   //rmk
      VertAlign: TVertAlign; HorzAlign: THorzAlign): TRect;          //rmk
    procedure BeginDoc;
    procedure EndDoc;
    procedure NewPage;
    procedure Print;
    procedure UpdateZoom;
    procedure SetPrinterOptions;
    procedure GetPrinterOptions;
    procedure SetPageSetupParameters(PageSetupDialog: TPageSetupDialogEx);
    function GetPageSetupParameters(PageSetupDialog: TPageSetupDialogEx): TRect;
    function FetchFormNames(FormNames: TStrings): boolean;
    function GetFormSize(const AFormName: string;
      out FormWidth, FormHeight: integer): boolean;
    function AddNewForm(const AFormName: string; FormWidth, FormHeight: DWORD): boolean;
    function RemoveForm(const AFormName: string): boolean;
    procedure DrawPage(Canvas: TCanvas; const Rect: TRect; Gray: boolean); virtual;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    function IsPageCustom: boolean;
    function IsPageRotated: boolean;
    procedure AdvancedProperties;
    property Bitmap: TBitmap read fBitmap write SetBitmap;
    property Canvas: TCanvas read GetCanvas;
    property State: TPreviewState read fState;
    property PageSize: TPoint read fPageExt write SetPageSize;
    property PageDevicePixels: TPoint read fDeviceExt;
    property PageLogicalPixels: TPoint read fLogicalExt;
    property PageBounds: TRect read GetPageBounds;
    property PrinterPageBounds: TRect read GetPrinterPageBounds;
    property PrinterPhysicalPageBounds: TRect read GetPrinterPhysicalPageBounds;
    property PrinterInstalled: boolean read GetPrinterInstalled;
    property Printer: TPrinter read GetPrinter;
    property PageViewControl: TPagePreview read fPageView;
    property FormName: string read GetFormName write SetFormName;
    property IsDummyFormName: boolean read GetIsDummyFormName;
    property SystemDefaultUnits: TUnits read GetSystemDefaultUnits;
    property UserDefaultUnits: TUnits read GetUserDefaultUnits;
    property CanScrollHorz: boolean read fCanScrollHorz;
    property CanScrollVert: boolean read fCanScrollVert;
    procedure ShowAddPrinterDialog;
    procedure ShowPageSetupDialog;
    procedure ShowPrinterPropertys;
    procedure Redraw;
    procedure DlgPaint(Sender: TPageSetupDialogEx; Paper, Flags: integer;
      PageSetupRec: TPageSetupDlg; PaintWhat: TDlgPaintWhat; Canvas: TCanvas;
      Rect: TRect; var NoDefaultPaint: boolean);
  published
    property Align default alClient;
    property DirectPrint: boolean read fDirectPrint write SetDirectPrint default False;
    property Grayscale: TGrayscaleOptions read fGrayscale write SetGrayscale default [];
    property GrayBrightness: integer
      read fGrayBrightness write SetGrayBrightness default 0;
    property GrayContrast: integer read fGrayContrast write SetGrayContrast default 0;
    property Units: TUnits read fUnits write SetUnits default mmHiMetric;
    property Orientation: TPrinterOrientation
      read fOrientation write SetOrientation default poPortrait;
    property PaperType: TPaperType read fPaperType write SetPaperType default pA4;
    property PageView: TPagePreviewOptions read fPageViewOptions
      write SetPageViewOptions;
    property PageWidth: integer read GetPageWidth write SetPageWidth stored IsPageCustom;
    property PageHeight: integer
      read GetPageHeight write SetPageHeight stored IsPageCustom;
    property ParentFont default False;
    property PageHeader: string read fPageHeader write SetPageHeader;
    property MarginAreaColor: TColor read fMarginAreaColor write SetMarginAreaColor;
    property PrintableAreaColor: TColor read fPrintableAreaColor
      write SetPrintableAreaColor default clSilver;
    property PrintJobTitle: string read fPrintJobTitle write fPrintJobTitle;
    property ShowMarginArea: boolean
      read fShowMarginArea write SetShowMarginArea default False;
    property ShowPrintableArea: boolean read fShowPrintableArea
      write SetShowPrintableArea default False;
    property TabStop default True;
    property UsePrinterOptions: boolean read fUsePrinterOptions
      write fUsePrinterOptions default False;
    property ZoomState: TZoomState read fZoomState write SetZoomState default
      zsZoomToFit;
    property Zoom: integer read fZoom write SetZoom stored IsZoomStored;
    property ZoomMin: integer read fZoomMin write SetZoomMin default 10;
    property ZoomMax: integer read fZoomMax write SetZoomMax default 1000;
    property ZoomSavePos: boolean read fZoomSavePos write fZoomSavePos default True;
    property ZoomStep: integer read fZoomStep write fZoomStep default 10;
    property OnBeginDoc: TNotifyEvent read fOnBeginDoc write fOnBeginDoc;
    property OnEndDoc: TNotifyEvent read fOnEndDoc write fOnEndDoc;
    property OnNewPage: TNotifyEvent read fOnNewPage write fOnNewPage;
    property OnEndPage: TNotifyEvent read fOnEndPage write fOnEndPage;
    property OnStateChange: TNotifyEvent read fOnStateChange write fOnStateChange;
    property OnZoomChange: TNotifyEvent read fOnZoomChange write fOnZoomChange;
    property OnPageChange: TNotifyEvent read fOnPageChange write fOnPageChange;
    property OnBeforePrint: TNotifyEvent read fOnBeforePrint write fOnBeforePrint;
    property OnAfterPrint: TNotifyEvent read fOnAfterPrint write fOnAfterPrint;
    property OnAnnotation: TPreviewPageDrawEvent read fOnAnnotation write fOnAnnotation;
    property OnBackground: TPreviewPageDrawEvent read fOnBackground write fOnBackground;
    property OnPrintAnnotation: TPreviewPageDrawEvent
      read fOnPrintAnnotation write fOnPrintAnnotation;
    property OnPrintBackground: TPreviewPageDrawEvent
      read fOnPrintBackground write fOnPrintBackground;
  end;

  TPaperSizeInfo = record
    ID: smallint;
    Width, Height: integer;
    Units: TUnits;
  end;

const
  // Paper Sizes
  PaperSizes: array[TPaperType] of TPaperSizeInfo = (
    (ID: DMPAPER_LETTER; Width: 08500; Height: 11000;
    Units: mmHiEnglish),
    (ID: DMPAPER_LETTER; Width: 08500; Height: 11000;
    Units: mmHiEnglish),
    (ID: DMPAPER_TABLOID; Width: 11000; Height: 17000;
    Units: mmHiEnglish),
    (ID: DMPAPER_LEDGER; Width: 17000; Height: 11000;
    Units: mmHiEnglish),
    (ID: DMPAPER_LEGAL; Width: 08500; Height: 14000;
    Units: mmHiEnglish),
    (ID: DMPAPER_STATEMENT; Width: 05500; Height: 08500;
    Units: mmHiEnglish),
    (ID: DMPAPER_EXECUTIVE; Width: 07250; Height: 10500;
    Units: mmHiEnglish),
    (ID: DMPAPER_A3; Width: 02970; Height: 04200;
    Units: mmLoMetric),
    (ID: DMPAPER_A4; Width: 02100; Height: 02970;
    Units: mmLoMetric),
    (ID: DMPAPER_A4SMALL; Width: 02100; Height: 02970;
    Units: mmLoMetric),
    (ID: DMPAPER_A5; Width: 01480; Height: 02100;
    Units: mmLoMetric),
    (ID: DMPAPER_B4; Width: 02500; Height: 03540;
    Units: mmLoMetric),
    (ID: DMPAPER_B5; Width: 01820; Height: 02570;
    Units: mmLoMetric),
    (ID: DMPAPER_FOLIO; Width: 08500; Height: 13000;
    Units: mmHiEnglish),
    (ID: DMPAPER_QUARTO; Width: 02150; Height: 02750;
    Units: mmLoMetric),
    (ID: DMPAPER_10X14; Width: 10000; Height: 14000;
    Units: mmHiEnglish),
    (ID: DMPAPER_11X17; Width: 11000; Height: 17000;
    Units: mmHiEnglish),
    (ID: DMPAPER_NOTE; Width: 08500; Height: 11000;
    Units: mmHiEnglish),
    (ID: DMPAPER_ENV_9; Width: 03875; Height: 08875;
    Units: mmHiEnglish),
    (ID: DMPAPER_ENV_10; Width: 04125; Height: 09500;
    Units: mmHiEnglish),
    (ID: DMPAPER_ENV_11; Width: 04500; Height: 10375;
    Units: mmHiEnglish),
    (ID: DMPAPER_ENV_12; Width: 04750; Height: 11000;
    Units: mmHiEnglish),
    (ID: DMPAPER_ENV_14; Width: 05000; Height: 11500;
    Units: mmHiEnglish),
    (ID: DMPAPER_CSHEET; Width: 17000; Height: 22000;
    Units: mmHiEnglish),
    (ID: DMPAPER_DSHEET; Width: 22000; Height: 34000;
    Units: mmHiEnglish),
    (ID: DMPAPER_ESHEET; Width: 34000; Height: 44000;
    Units: mmHiEnglish),
    (ID: DMPAPER_ENV_DL; Width: 01100; Height: 02200;
    Units: mmLoMetric),
    (ID: DMPAPER_ENV_C5; Width: 01620; Height: 02290;
    Units: mmLoMetric),
    (ID: DMPAPER_ENV_C3; Width: 03240; Height: 04580;
    Units: mmLoMetric),
    (ID: DMPAPER_ENV_C4; Width: 02290; Height: 03240;
    Units: mmLoMetric),
    (ID: DMPAPER_ENV_C6; Width: 01140; Height: 01620;
    Units: mmLoMetric),
    (ID: DMPAPER_ENV_C65; Width: 01140; Height: 02290;
    Units: mmLoMetric),
    (ID: DMPAPER_ENV_B4; Width: 02500; Height: 03530;
    Units: mmLoMetric),
    (ID: DMPAPER_ENV_B5; Width: 01760; Height: 02500;
    Units: mmLoMetric),
    (ID: DMPAPER_ENV_B6; Width: 01760; Height: 01250;
    Units: mmLoMetric),
    (ID: DMPAPER_ENV_ITALY; Width: 01100; Height: 02300;
    Units: mmLoMetric),
    (ID: DMPAPER_ENV_MONARCH; Width: 03875; Height: 07500;
    Units: mmHiEnglish),
    (ID: DMPAPER_ENV_PERSONAL; Width: 03625; Height: 06500;
    Units: mmHiEnglish),
    (ID: DMPAPER_FANFOLD_US; Width: 14875; Height: 11000;
    Units: mmHiEnglish),
    (ID: DMPAPER_FANFOLD_STD_GERMAN; Width: 08500; Height: 12000;
    Units: mmHiEnglish),
    (ID: DMPAPER_FANFOLD_LGL_GERMAN; Width: 08500; Height: 13000;
    Units: mmHiEnglish),
    (ID: DMPAPER_ISO_B4; Width: 02500; Height: 03530;
    Units: mmLoMetric),
    (ID: DMPAPER_JAPANESE_POSTCARD; Width: 01000; Height: 01480;
    Units: mmLoMetric),
    (ID: DMPAPER_9X11; Width: 09000; Height: 11000;
    Units: mmHiEnglish),
    (ID: DMPAPER_10X11; Width: 10000; Height: 11000;
    Units: mmHiEnglish),
    (ID: DMPAPER_15X11; Width: 15000; Height: 11000;
    Units: mmHiEnglish),
    (ID: DMPAPER_ENV_INVITE; Width: 02200; Height: 02200;
    Units: mmLoMetric),
    (ID: DMPAPER_LETTER_EXTRA; Width: 09500; Height: 12000;
    Units: mmHiEnglish),
    (ID: DMPAPER_LEGAL_EXTRA; Width: 09500; Height: 15000;
    Units: mmHiEnglish),
    (ID: DMPAPER_TABLOID_EXTRA; Width: 11690; Height: 18000;
    Units: mmHiEnglish),
    (ID: DMPAPER_A4_EXTRA; Width: 09270; Height: 12690;
    Units: mmHiEnglish),
    (ID: DMPAPER_LETTER_TRANSVERSE; Width: 08500; Height: 11000;
    Units: mmHiEnglish),
    (ID: DMPAPER_A4_TRANSVERSE; Width: 02100; Height: 02970;
    Units: mmLoMetric),
    (ID: DMPAPER_LETTER_EXTRA_TRANSVERSE; Width: 09500; Height: 12000;
    Units: mmHiEnglish),
    (ID: DMPAPER_A_PLUS; Width: 02270; Height: 03560;
    Units: mmLoMetric),
    (ID: DMPAPER_B_PLUS; Width: 03050; Height: 04870;
    Units: mmLoMetric),
    (ID: DMPAPER_LETTER_PLUS; Width: 08500; Height: 12690;
    Units: mmHiEnglish),
    (ID: DMPAPER_A4_PLUS; Width: 02100; Height: 03300;
    Units: mmLoMetric),
    (ID: DMPAPER_A5_TRANSVERSE; Width: 01480; Height: 02100;
    Units: mmLoMetric),
    (ID: DMPAPER_B5_TRANSVERSE; Width: 01820; Height: 02570;
    Units: mmLoMetric),
    (ID: DMPAPER_A3_EXTRA; Width: 03220; Height: 04450;
    Units: mmLoMetric),
    (ID: DMPAPER_A5_EXTRA; Width: 01740; Height: 02350;
    Units: mmLoMetric),
    (ID: DMPAPER_B5_EXTRA; Width: 02010; Height: 02760;
    Units: mmLoMetric),
    (ID: DMPAPER_A2; Width: 04200; Height: 05940;
    Units: mmLoMetric),
    (ID: DMPAPER_A3_TRANSVERSE; Width: 02970; Height: 04200;
    Units: mmLoMetric),
    (ID: DMPAPER_A3_EXTRA_TRANSVERSE; Width: 03220; Height: 04450;
    Units: mmLoMetric),
    (ID: DMPAPER_USER; Width: 0; Height: 0;
    Units: mmPixel));

procedure StretchDrawGraphic(Canvas: TCanvas; const Rect: TRect; Graphic: TGraphic);

implementation

uses
  Math, TypInfo, IniFiles, SpTBXSkins, GR32_LowLevel, uHelpers;

const
  TextAlignFlags: array[TAlignment] of DWORD = (DT_LEFT, DT_RIGHT, DT_CENTER);
  TextWordWrapFlags: array[boolean] of DWORD = (DT_END_ELLIPSIS, DT_WORDBREAK);

  sPrinting = 'Printing';

type
  TStreamHeader = packed record
    Signature: array[0..3] of AnsiChar;
    Version: word;
  end;

const
  PageInfoHeader: TStreamHeader = (Signature: 'DAPI'; Version: $0550);
  PageListHeader: TStreamHeader = (Signature: 'DAPL'; Version: $0550);

{ TPagePreviewOptions }

constructor TPagePreviewOptions.Create;
begin
  inherited Create;
  fBorderColor := clBlack;
  fBorderWidth := 1;
  fCursor := crDefault;
  fDragCursor := crHand;
  fGrabCursor:= crGrab;
  fPageColor := clWhite;
  fShadowColor := clBtnShadow;
  fShadowWidth := 3;
end;

procedure TPagePreviewOptions.Assign(Source: TPersistent);
begin
  if Source is TPagePreviewOptions then
  begin
    BorderColor := TPagePreviewOptions(Source).BorderColor;
    BorderWidth := TPagePreviewOptions(Source).BorderWidth;
    ShadowColor := TPagePreviewOptions(Source).ShadowColor;
    ShadowWidth := TPagePreviewOptions(Source).ShadowWidth;
    Cursor := TPagePreviewOptions(Source).Cursor;
    DragCursor := TPagePreviewOptions(Source).DragCursor;
    GrabCursor := TPagePreviewOptions(Source).GrabCursor;
    Hint := TPagePreviewOptions(Source).Hint;
    PageColor := TPagePreviewOptions(Source).PageColor;
    PopupMenu := TPagePreviewOptions(Source).PopupMenu;
  end
  else
    inherited Assign(Source);
end;

procedure TPagePreviewOptions.AssignTo(Dest: TPersistent);
begin
  if Dest is TPagePreviewOptions then
    Dest.Assign(Self)
  else if Dest is TPagePreview then
  begin
    TPagePreview(Dest).PageColor := PageColor;
    TPagePreview(Dest).BorderColor := BorderColor;
    TPagePreview(Dest).BorderWidth := BorderWidth;
    TPagePreview(Dest).ShadowColor := ShadowColor;
    TPagePreview(Dest).ShadowWidth := ShadowWidth;
    TPagePreview(Dest).Cursor := Cursor;
    TPagePreview(Dest).PopupMenu := PopupMenu;
    TPagePreview(Dest).Hint := Hint;
  end
  else
    inherited AssignTo(Dest);
end;

procedure TPagePreviewOptions.DoChange(Severity: TUpdateSeverity);
begin
  if Assigned(FOnChange) then
    FOnChange(self, Severity);
end;

procedure TPagePreviewOptions.SeTPageColor(Value: TColor);
begin
  if PageColor <> Value then
  begin
    fPageColor := Value;
    DoChange(usRedraw);
  end;
end;

procedure TPagePreviewOptions.SetBorderColor(Value: TColor);
begin
  if BorderColor <> Value then
  begin
    fBorderColor := Value;
    DoChange(usRedraw);
  end;
end;

procedure TPagePreviewOptions.SetBorderWidth(Value: TBorderWidth);
begin
  if BorderWidth <> Value then
  begin
    fBorderWidth := Value;
    DoChange(usRecreate);
  end;
end;

procedure TPagePreviewOptions.SetShadowColor(Value: TColor);
begin
  if ShadowColor <> Value then
  begin
    fShadowColor := Value;
    DoChange(usRedraw);
  end;
end;

procedure TPagePreviewOptions.SetShadowWidth(Value: TBorderWidth);
begin
  if ShadowWidth <> Value then
  begin
    fShadowWidth := Value;
    DoChange(usRecreate);
  end;
end;

procedure TPagePreviewOptions.SetCursor(Value: TCursor);
begin
  if Cursor <> Value then
  begin
    fCursor := Value;
    DoChange(usNone);
  end;
end;

procedure TPagePreviewOptions.SetDragCursor(Value: TCursor);
begin
  if DragCursor <> Value then
  begin
    fDragCursor := Value;
    DoChange(usNone);
  end;
end;

procedure TPagePreviewOptions.SetGrabCursor(Value: TCursor);
begin
  if GrabCursor <> Value then
  begin
    fGrabCursor:= Value;
    DoChange(usNone);
  end;
end;

procedure TPagePreviewOptions.SetHint(const Value: string);
begin
  if Hint <> Value then
  begin
    FHint := Value;
    DoChange(usNone);
  end;
end;

procedure TPagePreviewOptions.SetPopupMenu(Value: TPopupMenu);
begin
  if PopupMenu <> Value then
  begin
    FPopupMenu := Value;
    DoChange(usNone);
  end;
end;

procedure TPagePreviewOptions.CalcDimensions(PageWidth, PageHeight: integer;
  out PageRect, BoxRect: TRect);
begin
  PageRect.Left := BorderWidth;
  PageRect.Right := PageRect.Left + PageWidth;
  PageRect.Top := BorderWidth;
  PageRect.Bottom := PageRect.Top + PageHeight;
  BoxRect.Left := 0;
  BoxRect.Top := 0;
  BoxRect.Right := BorderWidth + PageWidth + BorderWidth + ShadowWidth;
  BoxRect.Bottom := BorderWidth + PageHeight + BorderWidth + ShadowWidth;
end;

procedure TPagePreviewOptions.Draw(Canvas: TCanvas; const BoxRect: TRect);
var
  R: TRect;
begin
  if ShadowWidth > 0 then
  begin
    R.Left := BoxRect.Right - ShadowWidth;
    R.Right := BoxRect.Right;
    R.Top := 0;
    R.Bottom := ShadowWidth;
    Canvas.FillRect(R);
    R.Left := 0;
    R.Right := ShadowWidth;
    R.Top := BoxRect.Bottom - ShadowWidth;
    R.Bottom := BoxRect.Bottom;
    Canvas.FillRect(R);
    Canvas.Brush.Color := ShadowColor;
    Canvas.Brush.Style := bsSolid;
    R.Left := BoxRect.Right - ShadowWidth;
    R.Right := BoxRect.Right;
    R.Top := BoxRect.Top + ShadowWidth;
    R.Bottom := BoxRect.Bottom;
    Canvas.FillRect(R);
    R.Left := ShadowWidth;
    R.Top := BoxRect.Bottom - ShadowWidth;
    Canvas.FillRect(R);
  end;
  if BorderWidth > 0 then
  begin
    Canvas.Pen.Width := BorderWidth;
    Canvas.Pen.Style := psInsideFrame;
    Canvas.Pen.Color := BorderColor;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(BoxRect.Left, BoxRect.Top,
      BoxRect.Right - ShadowWidth, BoxRect.Bottom - ShadowWidth);
  end;
end;

{ TPagePreview }

constructor TPagePreview.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csDisplayDragImage];
  fOffScreen := TBitmap.Create;
  fPageCanvas := TCanvas.Create;
  fPreservePageSize := True;
  fBorderColor := clBlack;
  fBorderWidth := 1;
  fPageColor := clWhite;
  fShadowColor := clBtnShadow;
  fShadowWidth := 3;
  fShowCaption := False;
  fAlignment := taCenter;
  fWordWrap := True;
  Width := 100;
  Height := 150;
end;

destructor TPagePreview.Destroy;
begin
  fOffScreen.Free;
  fPageCanvas.Free;
  inherited Destroy;
end;

procedure TPagePreview.Invalidate;
begin
  fIsOffScreenReady := False;
  if WindowHandle <> 0 then
    InvalidateRect(WindowHandle, @PageRect, False);
end;

procedure TPagePreview.InvalidateAll;
begin
  fIsOffScreenPrepared := False;
  if WindowHandle <> 0 then
    InvalidateRect(WindowHandle, nil, False);
end;

procedure TPagePreview.Paint;
var
  OffDC: HDC;
  VisibleRect: TRect;
  VisiblePageRect: TRect;
  SavedDC: integer;
begin
  if IntersectRect(VisibleRect, Canvas.ClipRect, ClientRect) then
  begin
    if not fIsOffScreenPrepared or (VisibleRect.Left < fLastVisibleRect.Left) or
      (VisibleRect.Top < fLastVisibleRect.Top) or
      (VisibleRect.Right > fLastVisibleRect.Right) or
      (VisibleRect.Bottom > fLastVisibleRect.Bottom) then
    begin
      fOffScreen.Width := VisibleRect.Right - VisibleRect.Left;
      fOffScreen.Height := VisibleRect.Bottom - VisibleRect.Top;
      OffDC := fOffScreen.Canvas.Handle;
      SetWindowOrgEx(OffDC, VisibleRect.Left, VisibleRect.Top, nil);
      DrawPage(fOffScreen.Canvas);
      SetWindowOrgEx(OffDC, 0, 0, nil);
      fLastVisibleRect := VisibleRect;
      fIsOffScreenPrepared := True;
      fIsOffScreenReady := False;
    end;
    if IntersectRect(VisiblePageRect, VisibleRect, PageRect) then
    begin
      if not fIsOffScreenReady or (VisiblePageRect.Left <
        fLastVisiblePageRect.Left) or (VisiblePageRect.Top < fLastVisiblePageRect.Top) or
        (VisiblePageRect.Right > fLastVisiblePageRect.Right) or
        (VisiblePageRect.Bottom > fLastVisiblePageRect.Bottom) then
      begin
        OffDC := fOffScreen.Canvas.Handle;
        SelectClipRgn(OffDC, 0);
        SetWindowOrgEx(OffDC, fLastVisibleRect.Left, fLastVisibleRect.Top, nil);
        with VisiblePageRect do
          IntersectClipRect(OffDC, Left, Top, Right, Bottom);
        with fOffScreen.Canvas do
        begin
          Brush.Color := PageColor;
          Brush.Style := bsSolid;
          FillRect(VisiblePageRect);
        end;
        if Assigned(fOnPaint) then
        begin
          SavedDC := SaveDC(OffDC);
          fPageCanvas.Handle := OffDC;
          try
            fOnPaint(Self, fPageCanvas, PageRect);
          finally
            fPageCanvas.Handle := 0;
            RestoreDC(OffDC, SavedDC);
          end;
        end;
        SetWindowOrgEx(OffDC, 0, 0, nil);
        fLastVisiblePageRect := VisiblePageRect;
        fIsOffScreenReady := True;
      end;
    end;
    Canvas.Draw(fLastVisibleRect.Left, fLastVisibleRect.Top, fOffScreen);
  end;
end;

procedure TPagePreview.DrawPage(Canvas: TCanvas);
var
  Rect: TRect;
  Flags: DWORD;
begin
  Canvas.Pen.Mode := pmCopy;
  if ShowCaption and (Caption <> '') then
  begin
    Rect.Left := 0;
    Rect.Top := Height - CaptionHeight;
    Rect.Right := Width - ShadowWidth + 1;
    Rect.Bottom := Height;
    if RectVisible(Canvas.Handle, Rect) then
    begin
      Canvas.Brush.Color := Color;
      Canvas.Brush.Style := bsSolid;
      Canvas.Font.Assign(Font);
      Canvas.FillRect(Rect);
      InflateRect(Rect, 0, -1);
      Flags := TextAlignFlags[Alignment] or TextWordWrapFlags[WordWrap] or
        DT_NOPREFIX;
      Flags := DrawTextBiDiModeFlags(Flags);
      DrawText(Canvas.Handle, PChar(Caption), Length(Caption), Rect, Flags);
    end;
  end;
  if ShadowWidth > 0 then
  begin
    Canvas.Brush.Color := Color;
    Canvas.Brush.Style := bsSolid;
    Rect.Left := Width - ShadowWidth;
    Rect.Right := Width;
    Rect.Top := 0;
    Rect.Bottom := ShadowWidth;
    Canvas.FillRect(Rect);
    Rect.Left := 0;
    Rect.Right := ShadowWidth;
    Rect.Top := Height - CaptionHeight - ShadowWidth;
    Rect.Bottom := Height - CaptionHeight;
    Canvas.FillRect(Rect);
    Canvas.Brush.Color := ShadowColor;
    Canvas.Brush.Style := bsSolid;
    Rect.Left := Width - ShadowWidth;
    Rect.Top := ShadowWidth;
    Rect.Right := Width;
    Rect.Bottom := Height - CaptionHeight;
    Canvas.FillRect(Rect);
    Rect.Left := ShadowWidth;
    Rect.Top := Height - CaptionHeight - ShadowWidth;
    Canvas.FillRect(Rect);
  end;
  if BorderWidth > 0 then
  begin
    Canvas.Pen.Width := BorderWidth;
    Canvas.Pen.Style := psInsideFrame;
    Canvas.Pen.Color := BorderColor;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(0, 0, Width - ShadowWidth, Height - CaptionHeight - ShadowWidth);
  end;
end;

procedure TPagePreview.UpdateCaptionHeight;
var
  Rect: TRect;
  Flags: DWORD;
  NewCaptionHeight: integer;
  SavedSize: TPoint;
  DC: HDC;
begin
  if ShowCaption then
  begin
    Flags := TextAlignFlags[Alignment] or TextWordWrapFlags[WordWrap] or
      DT_NOPREFIX or DT_CALCRECT;
    Flags := DrawTextBiDiModeFlags(Flags);
    Rect.Left := 0;
    Rect.Right := Width - ShadowWidth;
    Rect.Top := 0;
    Rect.Bottom := 0;
    Dec(Rect.Right, ShadowWidth);
    if HandleAllocated then
      DC := Canvas.Handle
    else
    begin
      DC := CreateCompatibleDC(0);
      SelectObject(DC, Font.Handle);
    end;
    DrawText(DC, PChar(Caption), Length(Caption), Rect, Flags);
    if HandleAllocated then
      DeleteDC(DC);
    NewCaptionHeight := Rect.Bottom - Rect.Top + 2;
  end
  else
    NewCaptionHeight := 0;
  if CaptionHeight <> NewCaptionHeight then
  begin
    SavedSize := PageSize;
    fCaptionHeight := NewCaptionHeight;
    if PreservePageSize then
      PageSize := SavedSize
    else
      InvalidateAll;
  end;
end;

function TPagePreview.ClientToPage(const Pt: TPoint): TPoint;
begin
  Result.X := Pt.X - BorderWidth;
  Result.Y := Pt.Y - BorderWidth;
end;

function TPagePreview.PageToClient(const Pt: TPoint): TPoint;
begin
  Result.X := Pt.X + BorderWidth;
  Result.Y := Pt.Y + BorderWidth;
end;

procedure TPagePreview.SetBoundsEx(ALeft, ATop, APageWidth, APageHeight: integer);
begin
  fPageRect.Left := BorderWidth;
  fPageRect.Top := BorderWidth;
  fPageRect.Right := fPageRect.Left + APageWidth;
  fPageRect.Bottom := fPageRect.Top + APageHeight;
  SetBounds(ALeft, ATop, ActualWidth(APageWidth), ActualHeight(APageHeight));
end;

function TPagePreview.ActualWidth(Value: integer): integer;
begin
  Result := Value + 2 * fBorderWidth + fShadowWidth;
end;

function TPagePreview.ActualHeight(Value: integer): integer;
begin
  Result := Value + 2 * fBorderWidth + fShadowWidth + CaptionHeight;
end;

function TPagePreview.LogicalWidth(Value: integer): integer;
begin
  Result := Value - 2 * fBorderWidth - fShadowWidth;
end;

function TPagePreview.LogicalHeight(Value: integer): integer;
begin
  Result := Value - 2 * fBorderWidth - fShadowWidth - CaptionHeight;
end;

procedure TPagePreview.SeTPageWidth(Value: integer);
begin
  ClientWidth := ActualWidth(Value);
end;

function TPagePreview.GeTPageWidth: integer;
begin
  Result := LogicalWidth(Width);
end;

procedure TPagePreview.SeTPageHeight(Value: integer);
begin
  ClientHeight := ActualHeight(Value);
end;

function TPagePreview.GeTPageHeight: integer;
begin
  Result := LogicalHeight(ClientHeight);
end;

procedure TPagePreview.SeTPageSize(const Value: TPoint);
begin
  SetBoundsEx(Left, Top, Value.X, Value.Y);
end;

function TPagePreview.GeTPageSize: TPoint;
begin
  Result.X := LogicalWidth(Width);
  Result.Y := LogicalHeight(Height);
end;

procedure TPagePreview.SeTPageColor(Value: TColor);
begin
  if PageColor <> Value then
  begin
    fPageColor := Value;
    InvalidateAll;
  end;
end;

procedure TPagePreview.SetBorderColor(Value: TColor);
begin
  if BorderColor <> Value then
  begin
    fBorderColor := Value;
    InvalidateAll;
  end;
end;

procedure TPagePreview.SetBorderWidth(Value: TBorderWidth);
var
  SavedSize: TPoint;
begin
  if BorderWidth <> Value then
  begin
    SavedSize := PageSize;
    fBorderWidth := Value;
    if PreservePageSize then
      PageSize := SavedSize
    else
      InvalidateAll;
  end;
end;

procedure TPagePreview.SetShadowColor(Value: TColor);
begin
  if ShadowColor <> Value then
  begin
    fShadowColor := Value;
    InvalidateAll;
  end;
end;

procedure TPagePreview.SetShadowWidth(Value: TBorderWidth);
var
  SavedSize: TPoint;
begin
  if ShadowWidth <> Value then
  begin
    SavedSize := PageSize;
    fShadowWidth := Value;
    if PreservePageSize then
      PageSize := SavedSize
    else
      InvalidateAll;
  end;
end;

procedure TPagePreview.SetShowCaption(Value: boolean);
begin
  if ShowCaption <> Value then
  begin
    fShowCaption := Value;
    UpdateCaptionHeight;
  end;
end;

procedure TPagePreview.SetAlignment(Value: TAlignment);
begin
  if Alignment <> Value then
  begin
    fAlignment := Value;
    if ShowCaption then
      InvalidateAll;
  end;
end;

procedure TPagePreview.SetWordWrap(Value: boolean);
begin
  if WordWrap <> Value then
  begin
    fWordWrap := Value;
    if ShowCaption then
      UpdateCaptionHeight;
  end;
end;

procedure TPagePreview.WMSize(var Message: TWMSize);
begin
  inherited;
  fPageRect.Left := BorderWidth;
  fPageRect.Top := BorderWidth;
  fPageRect.Right := fPageRect.Left + LogicalWidth(Width);
  fPageRect.Bottom := fPageRect.Top + LogicalHeight(Height);
  InvalidateAll;
  if Assigned(OnResize) then
    OnResize(Self);
end;

procedure TPagePreview.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TPagePreview.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if Assigned(fOnMouseEnter) then
    fOnMouseEnter(Self);
end;

procedure TPagePreview.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Assigned(fOnMouseLeave) then
    fOnMouseLeave(Self);
end;

procedure TPagePreview.CMColorChanged(var Message: TMessage);
begin
  inherited;
  InvalidateAll;
end;

procedure TPagePreview.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if ShowCaption then
  begin
    UpdateCaptionHeight;
    InvalidateAll;
  end;
end;

procedure TPagePreview.CMTextChanged(var Message: TMessage);
begin
  inherited;
  if ShowCaption then
  begin
    UpdateCaptionHeight;
    InvalidateAll;
  end;
end;

procedure TPagePreview.BiDiModeChanged(var Message: TMessage);
begin
  inherited;
  if ShowCaption then
    InvalidateAll
  else
    Invalidate;
end;

{ TPrintPreview }

procedure TransparentStretchDIBits(dstDC: HDC; dstX, dstY: integer;
  dstW, dstH: integer; srcX, srcY: integer; srcW, srcH: integer;
  bmpBits: Pointer; var bmpInfo: TBitmapInfo; mskBits: Pointer;
  var mskInfo: TBitmapInfo; Usage: DWORD);
var
  MemDC: HDC;
  MemBmp: HBITMAP;
  Save: THandle;
  crText, crBack: TColorRef;
  memInfo: pBitmapInfo;
  memBits: Pointer;
  HeaderSize: DWORD;
  ImageSize: DWORD;
begin
  MemDC := CreateCompatibleDC(0);
  try
    MemBmp := CreateCompatibleBitmap(dstDC, srcW, srcH);
    try
      Save := SelectObject(MemDC, MemBmp);
      SetStretchBltMode(MemDC, ColorOnColor);
      StretchDIBits(MemDC, 0, 0, srcW, srcH, 0, 0, srcW, srcH, mskBits,
        mskInfo, Usage, SrcCopy);
      StretchDIBits(MemDC, 0, 0, srcW, srcH, 0, 0, srcW, srcH, bmpBits,
        bmpInfo, Usage, SrcErase);
      if Save <> 0 then
        SelectObject(MemDC, Save);
      GetDIBSizes(MemBmp, HeaderSize, ImageSize);
      GetMem(memInfo, HeaderSize);
      try
        GetMem(memBits, ImageSize);
        try
          GetDIB(MemBmp, 0, memInfo^, memBits^);
          crText := SetTextColor(dstDC, RGB(0, 0, 0));
          crBack := SetBkColor(dstDC, RGB(255, 255, 255));
          SetStretchBltMode(dstDC, ColorOnColor);
          StretchDIBits(dstDC, dstX, dstY, dstW, dstH, srcX, srcY,
            srcW, srcH, mskBits, mskInfo, Usage, SrcAnd);
          StretchDIBits(dstDC, dstX, dstY, dstW, dstH, srcX, srcY,
            srcW, srcH, memBits, memInfo^, Usage, SrcInvert);
          SetTextColor(dstDC, crText);
          SetBkColor(dstDC, crBack);
        finally
          FreeMem(memBits, ImageSize);
        end;
      finally
        FreeMem(memInfo, HeaderSize);
      end;
    finally
      DeleteObject(MemBmp);
    end;
  finally
    DeleteDC(MemDC);
  end;
end;

procedure DrawBitmapAsDIB(DC: HDC; Bitmap: TBitmap; const Rect: TRect);
var
  BitmapHeader: pBitmapInfo;
  BitmapImage: Pointer;
  HeaderSize: DWORD;
  ImageSize: DWORD;
  MaskBitmapHeader: pBitmapInfo;
  MaskBitmapImage: Pointer;
  maskHeaderSize: DWORD;
  MaskImageSize: DWORD;
begin
  GetDIBSizes(Bitmap.Handle, HeaderSize, ImageSize);
  GetMem(BitmapHeader, HeaderSize);
  try
    GetMem(BitmapImage, ImageSize);
    try
      GetDIB(Bitmap.Handle, Bitmap.Palette, BitmapHeader^, BitmapImage^);
      if AllowTransparentDIB and Bitmap.Transparent then
      begin
        GetDIBSizes(Bitmap.MaskHandle, MaskHeaderSize, MaskImageSize);
        GetMem(MaskBitmapHeader, MaskHeaderSize);
        try
          GetMem(MaskBitmapImage, MaskImageSize);
          try
            GetDIB(Bitmap.MaskHandle, 0, MaskBitmapHeader^, MaskBitmapImage^);
            TransparentStretchDIBits(
              DC,                              // handle of destination device context
              Rect.Left, Rect.Top,
              // upper-left corner of destination rectagle
              Rect.Right - Rect.Left,          // width of destination rectagle
              Rect.Bottom - Rect.Top,          // height of destination rectagle
              0, 0,                            // upper-left corner of source rectangle
              Bitmap.Width, Bitmap.Height,     // width and height of source rectangle
              BitmapImage,                     // address of bitmap bits
              BitmapHeader^,                   // bitmap data
              MaskBitmapImage,                 // address of mask bitmap bits
              MaskBitmapHeader^,               // mask bitmap data
              DIB_RGB_COLORS                   // usage: the color table contains literal RGB values
              );
          finally
            FreeMem(MaskBitmapImage, MaskImageSize)
          end;
        finally
          FreeMem(MaskBitmapHeader, maskHeaderSize);
        end;
      end
      else
      begin
        SetStretchBltMode(DC, ColorOnColor);
        StretchDIBits(
          DC,                                  // handle of destination device context
          Rect.Left, Rect.Top,
          // upper-left corner of destination rectagle
          Rect.Right - Rect.Left,              // width of destination rectagle
          Rect.Bottom - Rect.Top,              // height of destination rectagle
          0, 0,                                // upper-left corner of source rectangle
          Bitmap.Width, Bitmap.Height,         // width and height of source rectangle
          BitmapImage,                         // address of bitmap bits
          BitmapHeader^,                       // bitmap data
          DIB_RGB_COLORS,
          // usage: the color table contains literal RGB values
          SrcCopy                              // raster operation code: copy source pixels
          );
      end;
    finally
      FreeMem(BitmapImage, ImageSize)
    end;
  finally
    FreeMem(BitmapHeader, HeaderSize);
  end;
end;

procedure StretchDrawGraphic(Canvas: TCanvas; const Rect: TRect; Graphic: TGraphic);
var
  Bitmap: TBitmap;
begin
  if Graphic is TBitmap then
    DrawBitmapAsDIB(Canvas.Handle, TBitmap(Graphic), Rect)
  else
  begin
    Bitmap := TBitmap.Create;
    try
      Bitmap.Canvas.Brush.Color := clWhite;
      Bitmap.Width := Graphic.Width;
      Bitmap.Height := Graphic.Height;
      Bitmap.Canvas.Draw(0, 0, Graphic);
      Bitmap.Transparent := Graphic.Transparent;
      DrawBitmapAsDIB(Canvas.Handle, Bitmap, Rect)
    finally
      Bitmap.Free;
    end;
  end;
end;

procedure ConvertBitmapToGrayscale(Bitmap: TBitmap; Brightness, Contrast: integer);
// If we consider RGB values in range [0,1] and contrast and brightness in
// rannge [-1,+1], the formula of this function became:
// Gray = Red * 0.30 + Green * 0.59 + Blue * 0.11
// GrayBC = 0.5 + (Gray - 0.5) * (1 + Contrast) + Brighness
// FinalGray = Confine GrayBC in range [0,1]
var
  Pixel: PRGBQuad;
  TransPixel: TRGBQuad;
  X, Y: integer;
  Gray: integer;
  Offset: integer;
  Scale: integer;
begin
  Bitmap.PixelFormat := pf32bit;
  TransPixel.rgbRed := GetRValue(Bitmap.TransparentColor);
  TransPixel.rgbGreen := GetGValue(Bitmap.TransparentColor);
  TransPixel.rgbBlue := GetBValue(Bitmap.TransparentColor);
  if Bitmap.Transparent then
    TransPixel.rgbReserved := 0
  else
    TransPixel.rgbReserved := 255;
  Scale := 100 + Contrast;
  Offset := 128 + (255 * Brightness - 128 * Scale) div 100;
  Pixel := Bitmap.ScanLine[Bitmap.Height - 1];
  for Y := 0 to Bitmap.Height - 1 do
  begin
    for X := 0 to Bitmap.Width - 1 do
    begin
      if PDWORD(Pixel)^ <> PDWORD(@TransPixel)^ then
        with Pixel^ do
        begin
          Gray := Offset + (rgbRed * 30 + rgbGreen * 59 + rgbBlue * 11) *
            Scale div 10000;
          if Gray > 255 then
            Gray := 255
          else if Gray < 0 then
            Gray := 0;
          rgbRed := Gray;
          rgbGreen := Gray;
          rgbBlue := Gray;
        end;
      Inc(Pixel);
    end;
  end;
end;


function ConvertUnits(Value, DPI: integer; InUnits, OutUnits: TUnits): integer;
begin
  Result := Value;
  case InUnits of
    mmLoMetric:
      case OutUnits of
        mmLoMetric: Result := Value;
        mmHiMetric: Result := Value * 10;
        mmLoEnglish: Result := Round(Value * 100 / 254);
        mmHiEnglish: Result := Round(Value * 1000 / 254);
        mmPoints: Result := Round(Value * 72 / 254);
        mmTWIPS: Result := Round(Value * 1440 / 254);
        mmPixel: Result := Round(Value * DPI / 254);
      end;
    mmHiMetric:
      case OutUnits of
        mmLoMetric: Result := Value div 10;
        mmHiMetric: Result := Value;
        mmLoEnglish: Result := Round(Value * 100 / 2540);
        mmHiEnglish: Result := Round(Value * 1000 / 2540);
        mmPoints: Result := Round(Value * 72 / 2540);
        mmTWIPS: Result := Round(Value * 1440 / 2540);
        mmPixel: Result := Round(Value * DPI / 2540);
      end;
    mmLoEnglish:
      case OutUnits of
        mmLoMetric: Result := Round(Value * 254 / 100);
        mmHiMetric: Result := Round(Value * 2540 / 100);
        mmLoEnglish: Result := Value;
        mmHiEnglish: Result := Value * 10;
        mmPoints: Result := Round(Value * 72 / 100);
        mmTWIPS: Result := Round(Value * 1440 / 100);
        mmPixel: Result := Round(Value * DPI / 100);
      end;
    mmHiEnglish:
      case OutUnits of
        mmLoMetric: Result := Round(Value * 254 / 1000);
        mmHiMetric: Result := Round(Value * 2540 / 1000);
        mmLoEnglish: Result := Value div 10;
        mmHiEnglish: Result := Value;
        mmPoints: Result := Round(Value * 72 / 1000);
        mmTWIPS: Result := Round(Value * 1440 / 1000);
        mmPixel: Result := Round(Value * DPI / 1000);
      end;
    mmPoints:
      case OutUnits of
        mmLoMetric: Result := Round(Value * 254 / 72);
        mmHiMetric: Result := Round(Value * 2540 / 72);
        mmLoEnglish: Result := Round(Value * 100 / 72);
        mmHiEnglish: Result := Round(Value * 1000 / 72);
        mmPoints: Result := Value;
        mmTWIPS: Result := Value * 20;
        mmPixel: Result := Round(Value * DPI / 72);
      end;
    mmTWIPS:
      case OutUnits of
        mmLoMetric: Result := Round(Value * 254 / 1440);
        mmHiMetric: Result := Round(Value * 2540 / 1440);
        mmLoEnglish: Result := Round(Value * 100 / 1440);
        mmHiEnglish: Result := Round(Value * 1000 / 1440);
        mmPoints: Result := Value div 20;
        mmTWIPS: Result := Value;
        mmPixel: Result := Round(Value * DPI / 1440);
      end;
    mmPixel:
      case OutUnits of
        mmLoMetric: Result := Round(Value * 254 / DPI);
        mmHiMetric: Result := Round(Value * 2540 / DPI);
        mmLoEnglish: Result := Round(Value * 100 / DPI);
        mmHiEnglish: Result := Round(Value * 1000 / DPI);
        mmPoints: Result := Round(Value * 72 / DPI);
        mmTWIPS: Result := Round(Value * 1440 / DPI);
        mmPixel: Result := Value;
      end;
  end;
end;

constructor TPrintPreview.Create(AOwner: TComponent);
var
  ini: TIniFileEx;
  s, po, spt: string;
  pri: Integer;
  ps: TRect;
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls] + [csDisplayDragImage];
  Align := alClient;
  TabStop := True;
  ParentFont := False;
  Font.Name := 'Arial';
  Font.Size := 8;
  fIniFileName := ChangeFileExt(Application.ExeName, '.ini');
  ini := TIniFileEx.Create(fIniFileName);
  try
    s := ini.ReadString(sPrinting, 'MarginRect', '2500, 2500, 18500, 27200');
    SpStringToRect(s, fMarginRect);
    fShowMarginArea := ini.ReadBool(sPrinting, 'ShowMarginArea', false);
    fShowPrintableArea := ini.ReadBool(sPrinting, 'ShowPrintableArea', false);
    spt := ini.ReadString(sPrinting, 'PaperType', 'pA4');
    s := ini.ReadString(sPrinting, 'PageSize', '21000, 29700') + ', 0, 0';
    SpStringToRect(s, ps);
    po := ini.ReadString(sPrinting, 'Orientation', 'poPortrait');
    pri := ini.ReadInteger(sPrinting, 'PrinterIndex', 0);
    fPageHeader := ini.ReadString(sPrinting, 'PageHeader', '');
  finally
    ini.Free;
  end;
  if (pri < Printer.Printers.Count) and (pri >= 0) then
  begin
    Printer.PrinterIndex := pri;
  end else begin
    if PrinterInstalled then
    begin
      pri := 0;
      Printer.PrinterIndex := pri;
    end;
  end;
  fBitmap := nil;
  fPrintableAreaColor := $00808080;
  fMarginAreaColor := clTeal;
  fPageViewOptions := TPagePreviewOptions.Create;
  fPageViewOptions.BorderColor := $00606060;
  fPageViewOptions.OnChange := PageViewOptionsChanged;
  fPageView := TPagePreview.Create(Self);
  with fPageView do
  begin
    Parent := Self;
    TabStop := False;
    Visible := False;
    OnPaint := PaintPage;
    OnClick := PageClick;
    OnDblClick := PageDblClick;
    OnMouseDown := PageMouseDown;
    OnMouseMove := PageMouseMove;
    OnMouseUp := PageMouseUp;
  end;
  fPageViewOptions.AssignTo(fPageView);
  fState := psReady;
  fZoom := 100;
  fZoomMin := 10;
  fZoomMax := 1000;
  fZoomStep := 10;
  fZoomSavePos := True;
  fZoomState := zsZoomToFit;
  fUnits := mmHiMetric;
  SetPaperType(spt);
  SetPageSize(ps.Location);
  SetOrientation(po);
  UpdateExtends;
end;

destructor TPrintPreview.Destroy;
var
  ini: TIniFileEx;
begin
  ini := TIniFileEx.Create(fIniFileName);
  try
    ini.WriteBool(sPrinting, 'ShowMarginArea', fShowMarginArea);
    ini.WriteBool(sPrinting, 'ShowPrintableArea', fShowPrintableArea);
    ini.WriteString(sPrinting, 'PaperType', GetPaperType);
    ini.WriteString(sPrinting, 'Orientation', GetOrientation);
    ini.WriteString(sPrinting, 'PageHeader', fPageHeader);
    if fOrientation = poPortrait then
    begin
      ini.WriteString(sPrinting, 'PageSize', Format('%d, %d', [fPageExt.X, fPageExt.Y]));
      ini.WriteString(sPrinting, 'MarginRect', SpRectToString(fMarginRect));
    end else begin
      ini.WriteString(sPrinting, 'PageSize', Format('%d, %d', [fPageExt.Y, fPageExt.X]));
      with fMarginRect do
        ini.WriteString(sPrinting, 'MarginRect', Format('%d, %d, %d, %d',
          [Top, Left, Bottom, Right]));
    end;
    ini.WriteInteger(sPrinting, 'PrinterIndex', Printer.PrinterIndex);
  finally
    ini.UpdateFile;
    ini.Free;
  end;
  if assigned(fPageMetafile) then
  begin
    fPageMetafile.Free;
    fPageMetafile := nil;
    fBitmap := nil;
  end;
  fPageView.Free;
  fPageViewOptions.Free;
  inherited Destroy;
end;

procedure TPrintPreview.CreateWnd;
begin
  inherited;
  UpdateZoom;
  fPageView.Visible := True;
  Update;
end;

procedure TPrintPreview.Loaded;
begin
  inherited Loaded;
  UpdateExtends;
  UpdateZoom;
end;

function TPrintPreview.ConvertX(X: integer; InUnits, OutUnits: TUnits): integer;
begin
  Result := ConvertUnits(X, HorzPixelsPerInch, InUnits, OutUnits);
end;

function TPrintPreview.ConvertY(Y: integer; InUnits, OutUnits: TUnits): integer;
begin
  Result := ConvertUnits(Y, VertPixelsPerInch, InUnits, OutUnits);
end;

function TPrintPreview.ConvertXY(X, Y: integer; InUnits, OutUnits: TUnits): TPoint;
begin
  Result.X := ConvertUnits(X, HorzPixelsPerInch, InUnits, OutUnits);
  Result.Y := ConvertUnits(Y, VertPixelsPerInch, InUnits, OutUnits);
end;

procedure TPrintPreview.ConvertPoints(var Points; NumPoints: integer;
  InUnits, OutUnits: TUnits);
var
  pPoints: PPoint;
begin
  pPoints := @Points;
  while NumPoints > 0 do
  begin
    with pPoints^ do
    begin
      X := ConvertUnits(X, HorzPixelsPerInch, InUnits, OutUnits);
      Y := ConvertUnits(Y, VertPixelsPerInch, InUnits, OutUnits);
    end;
    Inc(pPoints);
    Dec(NumPoints);
  end;
end;

function TPrintPreview.BoundsFrom(AUnits: TUnits;
  ALeft, ATop, AWidth, AHeight: integer): TRect;
begin
  Result := RectFrom(AUnits, ALeft, ATop, ALeft + AWidth, ATop + AHeight);
end;

function TPrintPreview.RectFrom(AUnits: TUnits;
  ALeft, ATop, ARight, ABottom: integer): TRect;
begin
  Result.TopLeft := PointFrom(AUnits, ALeft, ATop);
  Result.BottomRight := PointFrom(AUnits, ARight, ABottom);
end;

function TPrintPreview.PointFrom(AUnits: TUnits; X, Y: integer): TPoint;
begin
  Result := ConvertXY(X, Y, AUnits, fUnits);
end;

function TPrintPreview.XFrom(AUnits: TUnits; X: integer): integer;
begin
  Result := ConvertX(X, AUnits, fUnits);
end;

function TPrintPreview.YFrom(AUnits: TUnits; Y: integer): integer;
begin
  Result := ConvertY(Y, AUnits, fUnits);
end;

function TPrintPreview.ScreenToPreview(X, Y: integer): TPoint;
begin
  Result.X := ConvertX(MulDiv(X, HorzPixelsPerInch, Screen.PixelsPerInch),
    mmPixel, fUnits);
  Result.Y := ConvertY(MulDiv(Y, VertPixelsPerInch, Screen.PixelsPerInch),
    mmPixel, fUnits);
end;

function TPrintPreview.PreviewToScreen(X, Y: integer): TPoint;
begin
  Result.X := MulDiv(ConvertX(X, fUnits, mmPixel), Screen.PixelsPerInch,
    HorzPixelsPerInch);
  Result.Y := MulDiv(ConvertY(Y, fUnits, mmPixel), Screen.PixelsPerInch,
    VertPixelsPerInch);
end;

function TPrintPreview.ScreenToPage(const Pt: TPoint): TPoint;
begin
  Result := fPageView.ScreenToClient(Pt);
  Result := fPageView.ClientToPage(Result);
  Result.X := MulDiv(Result.X, 100, fZoom);
  Result.Y := MulDiv(Result.Y, 100, fZoom);
  Result := ScreenToPreview(Result.X, Result.Y);
end;

function TPrintPreview.PageToScreen(const Pt: TPoint): TPoint;
begin
  Result := PreviewToScreen(Pt.X, Pt.Y);
  Result.X := MulDiv(Result.X, fZoom, 100);
  Result.Y := MulDiv(Result.Y, fZoom, 100);
  Result := fPageView.PageToClient(Result);
  Result := fPageView.ClientToScreen(Result);
end;

function TPrintPreview.ClientToPage(const Pt: TPoint): TPoint;
begin
  Result := ScreenToPage(ClientToScreen(Pt));
end;

function TPrintPreview.PageToClient(const Pt: TPoint): TPoint;
begin
  Result := ScreenToClient(PageToScreen(Pt));
end;

function TPrintPreview.PaintGraphic(X, Y: integer; Graphic: TGraphic): TPoint;
var
  Rect: TRect;
begin
  Rect.Left := X;
  Rect.Top := Y;
  Rect.BottomRight := ScreenToPreview(Graphic.Width, Graphic.Height);
  Result := PaintGraphicEx(Rect, Graphic, False, False, False).BottomRight;
end;

function TPrintPreview.PaintGraphicEx(const Rect: TRect; Graphic: TGraphic;
  Proportinal, ShrinkOnly, Center: boolean): TRect;
var
  gW, gH: integer;
  rW, rH: integer;
  W, H: integer;
begin
  with ScreenToPreview(Graphic.Width, Graphic.Height) do
  begin
    gW := X;
    gH := Y;
  end;
  rW := Rect.Right - Rect.Left;
  rH := Rect.Bottom - Rect.Top;
  if not ShrinkOnly or (gW > rW) or (gH > rH) then
  begin
    if Proportinal then
    begin
      if (rW / gW) < (rH / gH) then
      begin
        H := MulDiv(gH, rW, gW);
        W := rW;
      end
      else
      begin
        W := MulDiv(gW, rH, gH);
        H := rH;
      end;
    end
    else
    begin
      W := rW;
      H := rH;
    end;
  end
  else
  begin
    W := gW;
    H := gH;
  end;
  if Center then
  begin
    Result.Left := Rect.Left + (rW - W) div 2;
    Result.Top := Rect.Top + (rH - H) div 2;
  end
  else
    Result.TopLeft := Rect.TopLeft;
  Result.Right := Result.Left + W;
  Result.Bottom := Result.Top + H;
  StretchDrawGraphic(Canvas, Result, Graphic);
end;

//rmk
function TPrintPreview.PaintGraphicEx2(const Rect: TRect; Graphic: TGraphic;
  VertAlign: TVertAlign; HorzAlign: THorzAlign): TRect;
var
  gW, gH: integer;
  rW, rH: integer;
  W, H: integer;
begin
  with ScreenToPreview(Graphic.Width, Graphic.Height) do
  begin
    gW := X;
    gH := Y;
  end;
  rW := Rect.Right - Rect.Left;
  rH := Rect.Bottom - Rect.Top;

  if (gW > rW) or (gH > rH) then
  begin
    if (rW / gW) < (rH / gH) then
    begin
      H := MulDiv(gH, rW, gW);
      W := rW;
    end
    else
    begin
      W := MulDiv(gW, rH, gH);
      H := rH;
    end;
  end
  else
  begin
    W := gW;
    H := gH;
  end;

  case VertAlign of
    vaTop: Result.Top := Rect.Top;
    vaCenter: Result.Top := Rect.Top + (rH - H) div 2;
    vaBottom: Result.Top := Rect.Bottom - H;
    else
      Result.Top := Rect.Top + (rH - H) div 2;
  end;

  case HorzAlign of
    haLeft: Result.Left := Rect.Left;
    haCenter: Result.Left := Rect.Left + (rW - W) div 2;
    haRight: Result.Left := Rect.Right - W;
    else
      Result.Left := Rect.Left + (rW - W) div 2;
  end;

  Result.Right := Result.Left + W;
  Result.Bottom := Result.Top + H;

  StretchDrawGraphic(Canvas, Result, Graphic);
end;

procedure TPrintPreview.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TPrintPreview.WMPaint(var Message: TWMPaint);
var
  DC: HDC;
  PaintStruct: TPaintStruct;
begin
  DC := Message.DC;
  if Message.DC = 0 then
    DC := BeginPaint(WindowHandle, PaintStruct);
  try
    if fPageView.Visible then
      with fPageView.BoundsRect do
        ExcludeClipRect(DC, Left, Top, Right, Bottom);
    FillRect(DC, PaintStruct.rcPaint, Brush.Handle);
  finally
    if Message.DC = 0 then
      EndPaint(WindowHandle, PaintStruct);
  end;
end;

procedure TPrintPreview.CNKeyDown(var Message: TWMKey);
var
  Key: word;
  Shift: TShiftState;
begin
  with Message do
  begin
    Key := CharCode;
    Shift := KeyDataToShiftState(KeyData);
  end;
  if (Key = VK_HOME) and (Shift = []) then
    Perform(WM_HSCROLL, SB_LEFT, 0)
  else if (Key = VK_HOME) and (Shift = [ssCtrl]) then
    Perform(WM_VSCROLL, SB_TOP, 0)
  else if (Key = VK_END) and (Shift = []) then
    Perform(WM_HSCROLL, SB_RIGHT, 0)
  else if (Key = VK_END) and (Shift = [ssCtrl]) then
    Perform(WM_VSCROLL, SB_BOTTOM, 0)
  else if (Key = VK_LEFT) and (Shift = [ssShift]) then
    Perform(WM_HSCROLL, MakeLong(SB_THUMBPOSITION, HorzScrollBar.Position - 1), 0)
  else if (Key = VK_LEFT) and (Shift = []) then
    Perform(WM_HSCROLL, SB_LINELEFT, 0)
  else if (Key = VK_LEFT) and (Shift = [ssCtrl]) then
    Perform(WM_HSCROLL, SB_PAGELEFT, 0)
  else if (Key = VK_RIGHT) and (Shift = [ssShift]) then
    Perform(WM_HSCROLL, MakeLong(SB_THUMBPOSITION, HorzScrollBar.Position + 1), 0)
  else if (Key = VK_RIGHT) and (Shift = []) then
    Perform(WM_HSCROLL, SB_LINERIGHT, 0)
  else if (Key = VK_RIGHT) and (Shift = [ssCtrl]) then
    Perform(WM_HSCROLL, SB_PAGERIGHT, 0)
  else if (Key = VK_UP) and (Shift = [ssShift]) then
    Perform(WM_VSCROLL, MakeLong(SB_THUMBPOSITION, VertScrollBar.Position - 1), 0)
  else if (Key = VK_UP) and (Shift = []) then
    Perform(WM_VSCROLL, SB_LINEUP, 0)
  else if (Key = VK_UP) and (Shift = [ssCtrl]) then
    Perform(WM_VSCROLL, SB_PAGEUP, 0)
  else if (Key = VK_DOWN) and (Shift = [ssShift]) then
    Perform(WM_VSCROLL, MakeLong(SB_THUMBPOSITION, VertScrollBar.Position + 1), 0)
  else if (Key = VK_DOWN) and (Shift = []) then
    Perform(WM_VSCROLL, SB_LINEDOWN, 0)
  else if (Key = VK_DOWN) and (Shift = [ssCtrl]) then
    Perform(WM_VSCROLL, SB_PAGEDOWN, 0)
  else if (Key = VK_ADD) and (Shift = []) then
    Zoom := Zoom + ZoomStep
  else if (Key = VK_SUBTRACT) and (Shift = []) then
    Zoom := Zoom - ZoomStep
  else
    inherited;
end;

procedure TPrintPreview.WMMouseWheel(var Message: TMessage);
var
  IsNeg: boolean;
  Rect: TRect;
  Pt: TPoint;
  I: integer;
begin
  GetWindowRect(WindowHandle, Rect);
  Pt.X := Message.LParamLo;
  Pt.Y := Message.LParamHi;
  if PtInRect(Rect, Pt) then
  begin
    Message.Result := 1;
    Inc(fWheelAccumulator, smallint(Message.WParamHi));
    while Abs(fWheelAccumulator) >= WHEEL_DELTA do
    begin
      IsNeg := fWheelAccumulator < 0;
      fWheelAccumulator := Abs(fWheelAccumulator) - WHEEL_DELTA;
      if IsNeg then
      begin
        fWheelAccumulator := -fWheelAccumulator;
        case LoWord(Message.WParam) of
          MK_CONTROL: Zoom := Zoom - ZoomStep;
          0: for I := 0 to 4 do
              Perform(WM_VSCROLL, SB_LINEDOWN, 0); // klin
          else
            Message.Result := 0;
        end;
      end
      else
      begin
        case LoWord(Message.WParam) of
          MK_CONTROL: Zoom := Zoom + ZoomStep;
          0: for I := 0 to 4 do
              Perform(WM_VSCROLL, SB_LINEUP, 0); // klin
          else
            Message.Result := 0;
        end;
      end;
    end;
  end;
end;

procedure TPrintPreview.WMHScroll(var Message: TWMScroll);
begin
  inherited;
  Update;
end;

procedure TPrintPreview.WMVScroll(var Message: TWMScroll);
begin
  inherited;
  Update;
end;

procedure TPrintPreview.PageClick(Sender: TObject);
begin
  Click;
end;

procedure TPrintPreview.PageDblClick(Sender: TObject);
begin
  DblClick;
end;

procedure TPrintPreview.PageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  Pt: TPoint;
begin
  if not Focused and Enabled then
    SetFocus;
  if (Sender = fPageView) and (fCanScrollHorz or fCanScrollVert) then
  begin
    fIsDragging := True;
    fPageView.Cursor := fPageViewOptions.GrabCursor;
    fPageView.Perform(WM_SETCURSOR, fPageView.Handle, HTCLIENT);
  end;
  Pt.X := X;
  Pt.Y := Y;
  fOldMousePos := Pt;
  MapWindowPoints(fPageView.Handle, Handle, Pt, 1);
  MouseDown(Button, Shift, Pt.X, Pt.Y);
end;

procedure TPrintPreview.PageMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
var
  Delta: TPoint;
  Pt: TPoint;
begin
  Pt.X := X;
  Pt.Y := Y;
  MapWindowPoints(fPageView.Handle, Handle, Pt, 1);
  MouseMove(Shift, Pt.X, Pt.Y);
  if ssLeft in Shift then
  begin
    if fCanScrollHorz then
    begin
      Delta.X := X - fOldMousePos.X;
      if not (AutoScroll and HorzScrollBar.Visible) then
      begin
        if fPageView.Left + Delta.X < ClientWidth - HorzScrollBar.Margin -
        fPageView.Width then
          Delta.X := ClientWidth - HorzScrollBar.Margin - fPageView.Width -
            fPageView.Left
        else if fPageView.Left + Delta.X > HorzScrollBar.Margin then
          Delta.X := HorzScrollBar.Margin - fPageView.Left;
        fPageView.Left := fPageView.Left + Delta.X;
      end
      else
        HorzScrollBar.Position := HorzScrollBar.Position - Delta.X;
    end;
    if fCanScrollVert then
    begin
      Delta.Y := Y - fOldMousePos.Y;
      if not (AutoScroll and VertScrollBar.Visible) then
      begin
        if fPageView.Top + Delta.Y < ClientHeight - VertScrollBar.Margin -
        fPageView.Height then
          Delta.Y := ClientHeight - VertScrollBar.Margin - fPageView.Height -
            fPageView.Top
        else if fPageView.Top + Delta.Y > VertScrollBar.Margin then
          Delta.Y := VertScrollBar.Margin - fPageView.Top;
        fPageView.Top := fPageView.Top + Delta.Y;
      end
      else
        VertScrollBar.Position := VertScrollBar.Position - Delta.Y;
    end;
    if (fCanScrollHorz and (Delta.X <> 0)) or (fCanScrollVert and (Delta.Y <> 0)) then
    begin
      Update;
    end;
  end;
end;

procedure TPrintPreview.PageMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  Pt: TPoint;
begin
  Pt.X := X;
  Pt.Y := Y;
  MapWindowPoints(fPageView.Handle, Handle, Pt, 1);
  MouseUp(Button, Shift, Pt.X, Pt.Y);
  if fIsDragging then
  begin
    fIsDragging := False;
    fPageView.Cursor := fPageViewOptions.DragCursor;
  end;
end;

function TPrintPreview.GetSystemDefaultUnits: TUnits;
var
  Data: array[0..1] of char;
begin
  GetLocaleInfo(LOCALE_SYSTEM_DEFAULT, LOCALE_IMEASURE, Data, 2);
  if Data[0] = '0' then
    Result := mmHiMetric
  else
    Result := mmHiEnglish;
end;

function TPrintPreview.GetUserDefaultUnits: TUnits;
var
  Data: array[0..1] of char;
begin
  GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_IMEASURE, Data, 2);
  if Data[0] = '0' then
    Result := mmHiMetric
  else
    Result := mmHiEnglish;
end;

procedure TPrintPreview.SetPageSetupParameters(PageSetupDialog: TPageSetupDialogEx);
var
  OutUnit: TUnits;
  pbr: TRect;
  p: TPoint;
begin
  pbr := PrinterPhysicalPageBounds;
  OutUnit := UserDefaultUnits;
  PageSetupDialog.Margin.Left := ConvertX(fMarginRect.Left, fUnits, OutUnit);
  PageSetupDialog.Margin.Right :=
    ConvertX(PageBounds.Right - fMarginRect.Right, fUnits, OutUnit);
  PageSetupDialog.Margin.Top := ConvertY(fMarginRect.Top, fUnits, OutUnit);
  PageSetupDialog.Margin.Bottom :=
    ConvertY(PageBounds.Bottom - fMarginRect.Bottom, fUnits, OutUnit);
  if Printer.Orientation = Orientation then
  begin
    p.X := ConvertX(PageWidth, fUnits, OutUnit);
    p.Y := ConvertY(PageHeight, fUnits, OutUnit);
    PageSetupDialog.PaperSize := p;
  end
  else
  begin
    Printer.Orientation := Orientation;
    p.X := ConvertX(PageHeight, fUnits, OutUnit);
    p.Y := ConvertY(PageWidth, fUnits, OutUnit);
    PageSetupDialog.PaperSize := p;
  end;
end;

function TPrintPreview.GetPageSetupParameters(PageSetupDialog:
  TPageSetupDialogEx): TRect;
var
  InUnit: TUnits;
  NewWidth, NewHeight: integer;
begin
  InUnit := UserDefaultUnits;
  NewWidth := ConvertX(PageSetupDialog.PaperSize.X, InUnit, fUnits);
  NewHeight := ConvertY(PageSetupDialog.PaperSize.Y, InUnit, fUnits);
  SetPageSizeOrientation(NewWidth, NewHeight, Printer.Orientation, False);
  Result := PageBounds;
  Inc(Result.Left, ConvertX(PageSetupDialog.Margin.Left, InUnit, fUnits));
  Inc(Result.Top, ConvertY(PageSetupDialog.Margin.Top, InUnit, fUnits));
  Dec(Result.Right, ConvertX(PageSetupDialog.Margin.Right, InUnit, fUnits));
  Dec(Result.Bottom, ConvertX(PageSetupDialog.Margin.Bottom, InUnit, fUnits));
  fMarginRect := Result;
  UpdateBitmap;
  DoPageChange;
end;

procedure TPrintPreview.SetPrinterOptions;
var
  DeviceMode: THandle;
  DevMode: PDeviceMode;
  Device, Driver, Port: array[0..MAX_PATH] of char;
  DriverInfo2: PDriverInfo2;
  DriverInfo2Size: DWORD;
  hPrinter: THandle;
  PageSize: TPoint;
begin
  if PrinterInstalled then
  begin
    Printer.GetPrinter(Device, Driver, Port, DeviceMode);
    DevMode := PDevMode(GlobalLock(DeviceMode));
    try
      with DevMode^ do
      begin
        dmFields := dmFields and not (DM_FORMNAME or
          DM_PAPERSIZE or DM_PAPERWIDTH or DM_PAPERLENGTH);
        if not IsDummyFormName then
        begin
          dmFields := dmFields or DM_FORMNAME;
          StrPLCopy(dmFormName, FormName, CCHFORMNAME);
        end;
        if PaperType = pCustom then
        begin
          PageSize := ConvertXY(PageWidth, PageHeight, Units, mmLoMetric);
          if fOrientation = poLandscape then
            Swap(PageSize.X, PageSize.Y);
          dmFields := dmFields or DM_PAPERSIZE;
          dmPaperSize := DMPAPER_USER;
          dmFields := dmFields or DM_PAPERWIDTH;
          dmPaperWidth := PageSize.X;
          dmFields := dmFields or DM_PAPERLENGTH;
          dmPaperLength := PageSize.Y;
        end
        else
        begin
          dmFields := dmFields or DM_PAPERSIZE;
          dmPaperSize := PaperSizes[PaperType].ID;
        end;
        dmFields := dmFields or DM_ORIENTATION;
        case fOrientation of
          poPortrait: dmOrientation := DMORIENT_PORTRAIT;
          poLandscape: dmOrientation := DMORIENT_LANDSCAPE;
        end;
      end;
    finally
      GlobalUnlock(DeviceMode);
    end;
    ResetDC(Printer.Handle, DevMode^);
    OpenPrinter(Device, hPrinter, nil);
    try
      GetPrinterDriver(hPrinter, nil, 2, nil, 0, DriverInfo2Size);
      GetMem(DriverInfo2, DriverInfo2Size);
      try
        GetPrinterDriver(hPrinter, nil, 2, DriverInfo2, DriverInfo2Size,
          DriverInfo2Size);
        StrPCopy(Driver, ExtractFileName(StrPas(DriverInfo2^.PDriverPath)));
      finally
        FreeMem(DriverInfo2, DriverInfo2Size);
      end;
    finally
      ClosePrinter(hPrinter);
    end;
    Printer.SetPrinter(Device, Driver, Port, DeviceMode);
  end;
end;

procedure TPrintPreview.GetPrinterOptions;
var
  DeviceMode: THandle;
  Device, Driver, Port: array[0..MAX_PATH] of char;
  NewWidth, NewHeight: integer;
  NewOrientation: TPrinterOrientation;
  NewPageType: TPaperType;
begin
  if PrinterInstalled then
  begin
    Printer.GetPrinter(Device, Driver, Port, DeviceMode);
    with PDevMode(GlobalLock(DeviceMode))^ do
      try
        NewOrientation := Orientation;
        if (dmFields and DM_ORIENTATION) = DM_ORIENTATION then
          case dmOrientation of
            DMORIENT_PORTRAIT: NewOrientation := poPortrait;
            DMORIENT_LANDSCAPE: NewOrientation := poLandscape;
          end;
        NewPageType := pCustom;
        if (dmFields and DM_PAPERSIZE) = DM_PAPERSIZE then
          NewPageType := FindPaperTypeByID(dmPaperSize);
        if NewPageType = pCustom then
        begin
          NewWidth := ConvertUnits(GetDeviceCaps(Printer.Handle, PHYSICALWIDTH),
            GetDeviceCaps(Printer.Handle, LOGPIXELSX), mmPixel, Units);
          NewHeight := ConvertUnits(GetDeviceCaps(Printer.Handle, PHYSICALHEIGHT),
            GetDeviceCaps(Printer.Handle, LOGPIXELSY), mmPixel, Units);
        end
        else
        begin
          GetPaperTypeSize(NewPageType, NewWidth, NewHeight, Units);
          if NewOrientation = poLandscape then
            Swap(NewWidth, NewHeight);
        end;
        SetPageSizeOrientation(NewWidth, NewHeight, NewOrientation);
        if (dmFields and DM_FORMNAME) = DM_FORMNAME then
        begin
          fFormName := StrPas(dmFormName);
          fVirtualFormName := '';
        end;
      finally
        GlobalUnlock(DeviceMode);
      end;
  end;
end;

procedure TPrintPreview.ResetPrinterDC;
var
  DeviceMode: THandle;
  DevMode: PDeviceMode;
  Device, Driver, Port: array[0..MAX_PATH] of char;
begin
  if PrinterInstalled then
  begin
    Printer.GetPrinter(Device, Driver, Port, DeviceMode);
    DevMode := PDevMode(GlobalLock(DeviceMode));
    try
      ResetDC(Printer.Canvas.Handle, DevMode^);
    finally
      GlobalUnlock(DeviceMode);
    end;
  end;
end;

procedure TPrintPreview.InitializePrinting;
begin
  if Assigned(fOnBeforePrint) then
    fOnBeforePrint(Self);
  if not UsePrinterOptions then
    SetPrinterOptions;
  Printer.Title := PrintJobTitle;
  Printer.BeginDoc;
  if not UsePrinterOptions then
    ResetPrinterDC;
end;

procedure TPrintPreview.FinalizePrinting(Succeeded: boolean);
begin
  if not Succeeded and Printer.Printing then
    Printer.Abort;
  if Printer.Printing then
    Printer.EndDoc;
  Printer.Title := '';
  if Assigned(fOnAfterPrint) then
    fOnAfterPrint(Self);
end;

function TPrintPreview.FetchFormNames(FormNames: TStrings): boolean;
var
  DeviceMode: THandle;
  Device, Driver, Port: array[0..MAX_PATH] of char;
  hPrinter: THandle;
  pFormsInfo, pfi: PFormInfo1;
  BytesNeeded: DWORD;
  FormCount: DWORD;
  I: integer;
begin
  Result := False;
  FormNames.BeginUpdate;
  try
    FormNames.Clear;
    if PrinterInstalled then
    begin
      Printer.GetPrinter(Device, Driver, Port, DeviceMode);
      OpenPrinter(Device, hPrinter, nil);
      try
        BytesNeeded := 0;
        EnumForms(hPrinter, 1, nil, 0, BytesNeeded, FormCount);
        if BytesNeeded > 0 then
        begin
          FormCount := BytesNeeded div SizeOf(TFormInfo1);
          GetMem(pFormsInfo, BytesNeeded);
          try
            if EnumForms(hPrinter, 1, pFormsInfo, BytesNeeded, BytesNeeded,
              FormCount) then
            begin
              Result := True;
              pfi := pFormsInfo;
              for I := 0 to FormCount - 1 do
              begin
                if (pfi^.Size.cx > 10) and (pfi^.Size.cy > 10) then
                  FormNames.Add(pfi^.pName);
                Inc(pfi);
              end;
            end;
          finally
            FreeMem(pFormsInfo);
          end;
        end;
      finally
        ClosePrinter(hPrinter);
      end;
    end;
  finally
    FormNames.EndUpdate;
  end;
end;

function TPrintPreview.GetFormSize(const AFormName: string;
  out FormWidth, FormHeight: integer): boolean;
var
  DeviceMode: THandle;
  Device, Driver, Port: array[0..MAX_PATH] of char;
  hPrinter: THandle;
  pFormInfo: PFormInfo1;
  BytesNeeded: DWORD;
begin
  Result := False;
  if PrinterInstalled then
  begin
    Printer.GetPrinter(Device, Driver, Port, DeviceMode);
    OpenPrinter(Device, hPrinter, nil);
    try
      BytesNeeded := 0;
      GetForm(hPrinter, PChar(AFormName), 1, nil, 0, BytesNeeded);
      if BytesNeeded > 0 then
      begin
        GetMem(pFormInfo, BytesNeeded);
        try
          if GetForm(hPrinter, PChar(AFormName), 1, pFormInfo, BytesNeeded,
            BytesNeeded) then
          begin
            with ConvertXY(pFormInfo.Size.cx div 10, pFormInfo.Size.cy div 10,
                mmHiMetric, Units) do
            begin
              FormWidth := X;
              FormHeight := Y;
            end;
            Result := True;
          end;
        finally
          FreeMem(pFormInfo);
        end;
      end;
    finally
      ClosePrinter(hPrinter);
    end;
  end;
end;

function TPrintPreview.AddNewForm(const AFormName: string;
  FormWidth, FormHeight: DWORD): boolean;
var
  DeviceMode: THandle;
  Device, Driver, Port: array[0..MAX_PATH] of char;
  hPrinter: THandle;
  FormInfo: TFormInfo1;
begin
  Result := False;
  if PrinterInstalled then
  begin
    Printer.GetPrinter(Device, Driver, Port, DeviceMode);
    OpenPrinter(Device, hPrinter, nil);
    try
      with FormInfo do
      begin
        Flags := 0;
        pName := PChar(AFormName);
        with ConvertXY(FormWidth, FormHeight, Units, mmHiMetric) do
        begin
          Size.cx := X * 10;
          Size.cy := Y * 10;
        end;
        SetRect(ImageableArea, 0, 0, Size.cx, Size.cy);
      end;
      if AddForm(hPrinter, 1, @FormInfo) then
      begin
        if CompareText(AFormName, fVirtualFormName) = 0 then
          fVirtualFormName := '';
        Result := True;
      end;
    finally
      ClosePrinter(hPrinter);
    end;
  end;
end;

function TPrintPreview.RemoveForm(const AFormName: string): boolean;
var
  DeviceMode: THandle;
  Device, Driver, Port: array[0..MAX_PATH] of char;
  hPrinter: THandle;
begin
  Result := False;
  if PrinterInstalled then
  begin
    Printer.GetPrinter(Device, Driver, Port, DeviceMode);
    OpenPrinter(Device, hPrinter, nil);
    try
      if DeleteForm(hPrinter, PChar(AFormName)) then
      begin
        if CompareText(AFormName, fFormName) = 0 then
        begin
          fVirtualFormName := fFormName;
          fFormName := '';
        end;
        Result := True;
      end;
    finally
      ClosePrinter(hPrinter);
    end;
  end;
end;

function TPrintPreview.GetFormName: string;
var
  DeviceMode: THandle;
  Device, Driver, Port: array[0..MAX_PATH] of char;
  hPrinter: THandle;
  PageSize: TPoint;
  mmPageSize: TPoint;
  mmFormSize: TPoint;
  pForms, pf: PFormInfo1;
  BytesNeeded: DWORD;
  FormCount: DWORD;
  IsRotated: boolean;
  Metric: boolean;
  I: integer;
begin
  Result := fFormName;
  if fFormName = '' then
  begin
    if (fVirtualFormName = '') and PrinterInstalled then
    begin
      IsRotated := (Orientation = poLandscape);
      if PaperType <> pCustom then
        GetPaperTypeSize(PaperType, PageSize.X, PageSize.Y, mmHiMetric)
      else if IsRotated then
        PageSize := ConvertXY(PageHeight, PageWidth, Units, mmHiMetric)
      else
        PageSize := ConvertXY(PageWidth, PageHeight, Units, mmHiMetric);
      mmPageSize.X := Round(PageSize.X / 100);
      mmPageSize.Y := Round(PageSize.Y / 100);
      Printer.GetPrinter(Device, Driver, Port, DeviceMode);
      OpenPrinter(Device, hPrinter, nil);
      try
        BytesNeeded := 0;
        EnumForms(hPrinter, 1, nil, 0, BytesNeeded, FormCount);
        if BytesNeeded > 0 then
        begin
          FormCount := BytesNeeded div SizeOf(TFormInfo1);
          GetMem(pForms, BytesNeeded);
          try
            if EnumForms(hPrinter, 1, pForms, BytesNeeded, BytesNeeded, FormCount) then
            begin
              pf := pForms;
              for I := 0 to FormCount - 1 do
              begin
                mmFormSize.X := Round(pf^.Size.cx / 1000);
                mmFormSize.Y := Round(pf^.Size.cy / 1000);
                if (mmFormSize.X = mmPageSize.X) and (mmFormSize.Y = mmPageSize.Y) then
                begin
                  fFormName := pf^.pName;
                  fVirtualFormName := '';
                  Result := fFormName;
                  Exit;
                end
                else if (mmFormSize.X = mmPageSize.Y) and
                  (mmFormSize.Y = mmPageSize.X) then
                  fVirtualFormName := pf^.pName;
                Inc(pf);
              end;
            end;
          finally
            FreeMem(pForms);
          end;
        end;
      finally
        ClosePrinter(hPrinter);
      end;
      if fVirtualFormName <> '' then
        IsRotated := not IsRotated
      else
      begin
        Metric := True;
        case Units of
          mmLoEnglish, mmHiEnglish:
            Metric := False;
          mmLoMetric, mmHiMetric:
            Metric := True;
          else
            case UserDefaultUnits of
              mmLoEnglish, mmHiEnglish:
                Metric := False;
              mmLoMetric, mmHiMetric:
                Metric := True;
            end;
        end;
        if IsRotated then
          Swap(mmPageSize.X, mmPageSize.Y);
        if Metric then
          fVirtualFormName := Format('%umm x %umm', [mmPageSize.X, mmPageSize.Y])
        else
          with ConvertXY(PageSize.X, PageSize.Y, mmHiMetric, mmHiEnglish) do
            fVirtualFormName :=
              Format('%g" x %g"', [Round(X / 100) / 10, Round(Y / 100) / 10]);
      end;
      if IsRotated then
        fVirtualFormName := fVirtualFormName + ' Повернуто';
    end;
    Result := fVirtualFormName;
  end;
end;

procedure TPrintPreview.SetFormName(const Value: string);
var
  FormWidth, FormHeight: integer;
begin
  if (CompareText(fFormName, Value) <> 0) and (fState = psReady) and
    GetFormSize(Value, FormWidth, FormHeight) and (FormWidth <> 0) and
    (FormHeight <> 0) then
  begin
    if Orientation = poPortrait then
      SetPageSize(FormWidth, FormHeight)
    else
      SetPageSize(FormHeight, FormWidth);
    fFormName := Value;
    fVirtualFormName := '';
  end;
end;

function TPrintPreview.GetIsDummyFormName: boolean;
begin
  Result := (CompareText(FormName, fVirtualFormName) = 0);
end;

function TPrintPreview.FindPaperTypeBySize(APageWidth, APageHeight: integer): TPaperType;
var
  Page: TPaperType;
  InputSize: TPoint;
  PaperSize: TPoint;
begin
  Result := pCustom;
  InputSize := ConvertXY(APageWidth, APageHeight, Units, mmHiMetric);
  InputSize.X := Round(InputSize.X / 100);
  InputSize.Y := Round(InputSize.Y / 100);
  for Page := Low(TPaperType) to High(TPaperType) do
  begin
    PaperSize := ConvertXY(PaperSizes[Page].Width, PaperSizes[Page].Height,
      PaperSizes[Page].Units, mmHiMetric);
    PaperSize.X := Round(PaperSize.X / 100);
    PaperSize.Y := Round(PaperSize.Y / 100);
    if (PaperSize.X = InputSize.X) and (PageSize.Y = InputSize.Y) then
    begin
      Result := Page;
      Exit;
    end;
  end;
end;

function TPrintPreview.FindPaperTypeByID(ID: integer): TPaperType;
var
  Page: TPaperType;
begin
  Result := pCustom;
  for Page := Low(TPaperType) to High(TPaperType) do
    if PaperSizes[Page].ID = ID then
    begin
      Result := Page;
      Exit;
    end;
end;

function TPrintPreview.GetPaperType: string;
begin
  result := GetEnumName(TypeInfo(TPaperType), Ord(fPaperType));
end;

function TPrintPreview.GetPaperTypeSize(APageType: TPaperType;
  out APageWidth, APageHeight: integer; OutUnits: TUnits): boolean;
begin
  Result := False;
  if APageType <> pCustom then
  begin
    APageWidth := ConvertX(PaperSizes[APageType].Width,
      PaperSizes[APageType].Units, OutUnits);
    APageHeight := ConvertY(PaperSizes[APageType].Height,
      PaperSizes[APageType].Units, OutUnits);
    Result := True;
  end;
end;

procedure TPrintPreview.Resize;
begin
  inherited Resize;
  UpdateZoom;
end;

function TPrintPreview.GetVisiblePageRect: TRect;
begin
  Result := fPageView.PageRect;
  MapWindowPoints(fPageView.Handle, Handle, Result, 2);
  IntersectRect(Result, Result, ClientRect);
  MapWindowPoints(Handle, fPageView.Handle, Result, 2);
  OffsetRect(Result, -fPageView.BorderWidth, -fPageView.BorderWidth);
  Result.Left := MulDiv(Result.Left, 100, Zoom);
  Result.Top := MulDiv(Result.Top, 100, Zoom);
  Result.Right := MulDiv(Result.Right, 100, Zoom);
  Result.Bottom := MulDiv(Result.Bottom, 100, Zoom);
end;

procedure TPrintPreview.SetVisiblePageRect(const Value: TRect);
var
  OldZoom: integer;
  Space: TPoint;
  W, H: integer;
begin
  OldZoom := fLastZoom;
  Space.X := ClientWidth - 2 * HorzScrollBar.Margin;
  Space.Y := ClientHeight - 2 * VertScrollBar.Margin;
  W := fPageView.ActualWidth(Value.Right - Value.Left);
  H := fPageView.ActualHeight(Value.Bottom - Value.Top);
  if Space.X / W < Space.Y / H then
    fZoom := MulDiv(100, Space.X, W)
  else
    fZoom := MulDiv(100, Space.Y, H);
  UpdateZoomEx(Value.Left, Value.Top);
  if OldZoom = fZoom then
  begin
    if fZoomState <> zsZoomOther then
    begin
      fZoomState := zsZoomOther;
      if Assigned(fOnZoomChange) then
        fOnZoomChange(Self);
    end;
  end;
end;

function TPrintPreview.CalculateViewSize(const Space: TPoint): TPoint;
begin
  with fPageView do
  begin
    case fZoomState of
      zsZoomOther:
      begin
        Result.X := ActualWidth(MulDiv(fLogicalExt.X, fZoom, 100));
        Result.Y := ActualHeight(MulDiv(fLogicalExt.Y, fZoom, 100));
      end;
      zsZoomToWidth:
      begin
        Result.X := Space.X;
        Result.Y := ActualHeight(MulDiv(LogicalWidth(Result.X),
          fLogicalExt.Y, fLogicalExt.X));
      end;
      zsZoomToHeight:
      begin
        Result.Y := Space.Y;
        Result.X := ActualWidth(MulDiv(LogicalHeight(Result.Y),
          fLogicalExt.X, fLogicalExt.Y));
      end;
      zsZoomToFit:
      begin
        if (fLogicalExt.Y / fLogicalExt.X) < (Space.Y / Space.X) then
        begin
          Result.X := Space.X;
          Result.Y := ActualHeight(MulDiv(LogicalWidth(Result.X),
            fLogicalExt.Y, fLogicalExt.X));
        end
        else
        begin
          Result.Y := Space.Y;
          Result.X := ActualWidth(MulDiv(LogicalHeight(Result.Y),
            fLogicalExt.X, fLogicalExt.Y));
        end;
      end;
    end;
    if fZoomState <> zsZoomOther then
      fZoom := Round((100 * LogicalHeight(Result.Y)) / fLogicalExt.Y);
  end;
end;

{$WARNINGS OFF}
procedure TPrintPreview.UpdateZoomEx(X, Y: integer);
var
  Space: TPoint;
  Position: TPoint;
  ViewPos: TPoint;
  ViewSize: TPoint;
  Percent: TPoint;
  ha: boolean;
begin
  ha := HandleAllocated;
  if not Ha or (csLoading in ComponentState) or (csDesigning in ComponentState) then
    Exit;

  Space.X := ClientWidth - 2 * HorzScrollBar.Margin;
  Space.Y := ClientHeight - 2 * VertScrollBar.Margin;

  if (Space.X <= 0) or (Space.Y <= 0) then
    Exit;
  Position.X := MulDiv(HorzScrollbar.Position, 100, HorzScrollBar.Range - Space.X);
  if Position.X < 0 then
    Position.X := 0;
  Position.Y := MulDiv(VertScrollbar.Position, 100, VertScrollbar.Range - Space.Y);
  if Position.Y < 0 then
    Position.Y := 0;

  if AutoScroll then
  begin
    if HorzScrollBar.Visible and (GetWindowLong(WindowHandle, GWL_STYLE) and
      SB_HORZ <> 0) then
      Inc(Space.Y, GetSystemMetrics(SM_CYHSCROLL));
    if VertScrollBar.Visible and (GetWindowLong(WindowHandle, GWL_STYLE) and
      SB_VERT <> 0) then
      Inc(Space.X, GetSystemMetrics(SM_CXVSCROLL));
  end;

  SendMessage(WindowHandle, WM_SETREDRAW, 0, 0);

  try

    DisableAutoRange;

    try

      HorzScrollbar.Position := 0;
      VertScrollbar.Position := 0;

      ViewSize := CalculateViewSize(Space);

      fCanScrollHorz := (ViewSize.X > Space.X);
      fCanScrollVert := (ViewSize.Y > Space.Y);

      if AutoScroll then
      begin
        if fCanScrollHorz then
        begin
          Dec(Space.Y, GetSystemMetrics(SM_CYHSCROLL));
          fCanScrollVert := (fPageView.Height > Space.Y);
          if fCanScrollVert then
            Dec(Space.X, GetSystemMetrics(SM_CXVSCROLL));
          ViewSize := CalculateViewSize(Space);
        end
        else if fCanScrollVert then
        begin
          Dec(Space.X, GetSystemMetrics(SM_CXVSCROLL));
          fCanScrollHorz := (fPageView.Width > Space.X);
          if fCanScrollHorz then
            Dec(Space.Y, GetSystemMetrics(SM_CYHSCROLL));
          ViewSize := CalculateViewSize(Space);
        end;
      end;

      ViewPos.X := HorzScrollBar.Margin;
      if not fCanScrollHorz then
        Inc(ViewPos.X, (Space.X - ViewSize.X) div 2);

      ViewPos.Y := VertScrollBar.Margin;
      if not fCanScrollVert then
        Inc(ViewPos.Y, (Space.Y - ViewSize.Y) div 2);

      fPageView.SetBounds(ViewPos.X, ViewPos.Y, ViewSize.X, ViewSize.Y);

    finally
      EnableAutoRange;
    end;
    if fCanScrollHorz then
    begin
      if X >= 0 then
        HorzScrollbar.Position := MulDiv(X, HorzScrollBar.Range, fLogicalExt.X)
      else if fZoomSavePos then
        HorzScrollbar.Position :=
          MulDiv(Position.X, HorzScrollBar.Range - Space.X, 100);
      if fCanScrollVert then
      begin
        if Y >= 0 then
          VertScrollBar.Position := MulDiv(Y, VertScrollBar.Range, fLogicalExt.Y)
        else if fZoomSavePos then
          VertScrollbar.Position :=
            MulDiv(Position.Y, VertScrollbar.Range - Space.Y, 100);
      end;
    end;

  finally
    SendMessage(WindowHandle, WM_SETREDRAW, 1, 0);
    Invalidate;
  end;

  fIsDragging := False;
  if fCanScrollHorz or fCanScrollVert then
    fPageView.Cursor := fPageViewOptions.DragCursor
  else
    fPageView.Cursor := fPageViewOptions.Cursor;

  if (ViewSize.X <> fPageView.Width) or (ViewSize.Y <> fPageView.Height) then
  begin
    Percent.X := (MulDiv(100, fPageView.Width, fLogicalExt.X) div fZoomStep) * fZoomStep;
    Percent.Y := (MulDiv(100, fPageView.Height, fLogicalExt.Y) div
      fZoomStep) * fZoomStep;
    if Percent.X < Percent.Y then
      fZoom := Percent.X
    else
      fZoom := Percent.Y;
    UpdateZoomEx(X, Y);
  end
  else
  begin
    if fLastZoom <> fZoom then
    begin
      fLastZoom := fZoom;
      Update;
      if Assigned(fOnZoomChange) then
        fOnZoomChange(Self);
    end;
  end;
end;

{$WARNINGS ON}

procedure TPrintPreview.UpdateZoom;
begin
  UpdateZoomEx(-1, -1);
end;

procedure TPrintPreview.ChangeState(NewState: TPreviewState);
begin
  if fState <> NewState then
  begin
    fState := NewState;
    if Assigned(fOnStateChange) then
      fOnStateChange(Self);
  end;
end;

procedure TPrintPreview.PaintPage(Sender: TObject; Canvas: TCanvas; const Rect: TRect);
var
  sx, sy: double;
begin
  PreviewPage(Canvas, Rect);
  sx := Rect.Width / fPageExt.X;
  sy := Rect.Height / fPageExt.Y;
  if fShowPrintableArea then
  begin
    with Canvas, PrinterPageBounds do
    begin
      Pen.Mode := pmMask;
      Pen.Width := 0;
      Pen.Style := psDot;
      Pen.Color := FPrintableAreaColor;
      MoveTo(Round(sx * Left), Rect.Top);
      LineTo(Round(sx * Left), Rect.Bottom);
      MoveTo(Round(sx * Right), Rect.Top);
      LineTo(Round(sx * Right), Rect.Bottom);
      MoveTo(Rect.Left, Round(sy * Top));
      LineTo(Rect.Right, Round(sy * Top));
      MoveTo(Rect.Left, Round(sy * Bottom));
      LineTo(Rect.Right, Round(sy * Bottom));
    end;
  end;
  if fShowMarginArea then
  begin
    with Canvas, fMarginRect do
    begin
      Pen.Mode := pmMask;
      Pen.Width := 0;
      Pen.Style := psDot;
      Pen.Color := fMarginAreaColor;
      Rectangle(Round(sx * Left), Round(sy * Top), Round(sx * Right), Round(sy * Bottom));
    end;
  end;
end;


procedure TPrintPreview.PreviewPage(Canvas: TCanvas; const Rect: TRect);
begin
  DrawPage(Canvas, Rect, gsPreview in fGrayscale);
end;

procedure TPrintPreview.PrintPage(Canvas: TCanvas; const Rect: TRect);
begin
  SetStretchBltMode(Canvas.Handle, HALFTONE);
  Canvas.StretchDraw(Rect, fPageMetafile);
end;

procedure TPrintPreview.DrawPage(Canvas: TCanvas; const Rect: TRect; Gray: boolean);
begin
  if fPageMetafile = nil then
    exit;
  SetStretchBltMode(Canvas.Handle, HALFTONE);
  Canvas.StretchDraw(Rect, fPageMetafile);
end;

procedure TPrintPreview.PageViewOptionsChanged(Sender: TObject;
  Severity: TUpdateSeverity);
begin
  fPageViewOptions.AssignTo(fPageView);
  if Severity = usRecreate then
    UpdateZoom;
end;

function TPrintPreview.HorzPixelsPerInch: integer;
begin
  if fReferenceDC <> 0 then
    Result := GetDeviceCaps(fReferenceDC, LOGPIXELSX)
  else
    Result := Screen.PixelsPerInch;
end;

function TPrintPreview.VertPixelsPerInch: integer;
begin
  if fReferenceDC <> 0 then
    Result := GetDeviceCaps(fReferenceDC, LOGPIXELSY)
  else
    Result := Screen.PixelsPerInch;
end;

procedure TPrintPreview.SetPageViewOptions(Value: TPagePreviewOptions);
begin
  fPageViewOptions.Assign(Value);
end;

procedure TPrintPreview.SetUnits(Value: TUnits);
begin
  if fUnits <> Value then
  begin
    if fPaperType <> pCustom then
    begin
      GetPaperTypeSize(fPaperType, fPageExt.X, fPageExt.Y, Value);
      if fOrientation = poLandscape then
        Swap(fPageExt.X, fPageExt.Y);
    end
    else
      ConvertPoints(fPageExt, 1, fUnits, Value);
    if Assigned(fPageCanvas) then
    begin
      fPageCanvas.Pen.Width := ConvertX(fPageCanvas.Pen.Width, fUnits, Value);
      ScaleCanvas(fPageCanvas);
    end;
    fUnits := Value;
  end;
end;

procedure TPrintPreview.DoPageChange;
begin
  fFormName := '';
  fVirtualFormName := '';
  UpdateExtends;
  UpdateZoom;
  if Assigned(fOnPageChange) then
    fOnPageChange(Self);
end;

procedure TPrintPreview.SetPaperType(Value: TPaperType);
begin
  if (fPaperType <> Value) and (fState = psReady) then
  begin
    fPaperType := Value;
    if fPaperType <> pCustom then
    begin
      with PaperSizes[fPaperType] do
        fPageExt := ConvertXY(Width, Height, Units, fUnits);
      if fOrientation = poLandscape then
        Swap(fPageExt.X, fPageExt.Y);
      UpdateBitmap;
      DoPageChange;
    end;
  end;
end;

procedure TPrintPreview.SetPaperType(const Value: string);
var
  v: Integer;
begin
  v := GetEnumValue(TypeInfo(TPaperType), Value);
  if v < 0 then
    SetPaperType(pA4)
  else
    SetPaperType(TPaperType(v))
end;

procedure TPrintPreview.SetPageSize(AWidth, AHeight: integer);
begin
  if AWidth < 1 then
    AWidth := 1;
  if AHeight < 1 then
    AHeight := 1;
  if ((fPageExt.X <> AWidth) or (fPageExt.Y <> AHeight)) and (fState = psReady) then
  begin
    fPageExt.X := AWidth;
    fPageExt.Y := AHeight;
    if fOrientation = poLandscape then
      fPaperType := FindPaperTypeBySize(fPageExt.Y, fPageExt.X)
    else
      fPaperType := FindPaperTypeBySize(fPageExt.X, fPageExt.Y);
    DoPageChange;
  end;
end;

procedure TPrintPreview.SetPageSize(const Value: TPoint);
begin
  SetPageSize(Value.X, Value.Y);
end;

procedure TPrintPreview.SetOrientation(Value: TPrinterOrientation);
begin
  if (fOrientation <> Value) and (fState = psReady) then
  begin
    fOrientation := Value;
    Swap(fPageExt.X, fPageExt.Y);
    Swap(fMarginRect.Left, fMarginRect.Top);
    Swap(fMarginRect.Right, fMarginRect.Bottom);
    Printer.Orientation := fOrientation;
    UpdateBitmap;
    DoPageChange;
  end;
end;

procedure TPrintPreview.SetOrientation(const Value: string);
var
  v: Integer;
begin
  v := GetEnumValue(TypeInfo(TPrinterOrientation), Value);
  if v < 0 then
    SetOrientation(poPortrait)
  else
    SetOrientation(TPrinterOrientation(v))
end;

function TPrintPreview.GetOrientation: string;
begin
  result := GetEnumName(TypeInfo(TPrinterOrientation), Ord(fOrientation));
end;

procedure TPrintPreview.SetPageSizeOrientation(AWidth, AHeight: integer;
  AOrientation: TPrinterOrientation; redraw: boolean);
begin
  if AWidth < 1 then
    AWidth := 1;
  if AHeight < 1 then
    AHeight := 1;
  if (fOrientation <> AOrientation) or ((AOrientation = fOrientation) and
    ((fPageExt.X <> AWidth) or (fPageExt.Y <> AHeight))) or
    ((AOrientation <> fOrientation) and ((fPageExt.X <> AHeight) or
    (fPageExt.Y <> AWidth))) then
  begin
    fPageExt.X := AWidth;
    fPageExt.Y := AHeight;
    fOrientation := AOrientation;
    if fOrientation = poPortrait then
      fPaperType := FindPaperTypeBySize(fPageExt.X, fPageExt.Y)
    else
      fPaperType := FindPaperTypeBySize(fPageExt.Y, fPageExt.X);
    if redraw then
      DoPageChange;
  end;
end;

function TPrintPreview.GetPageWidth: integer;
begin
  Result := fPageExt.X;
end;

procedure TPrintPreview.SetPageWidth(Value: integer);
begin
  SetPageSize(Value, fPageExt.Y);
end;

function TPrintPreview.GetPageHeight: integer;
begin
  Result := fPageExt.Y;
end;

procedure TPrintPreview.SetPageHeight(Value: integer);
begin
  SetPageSize(fPageExt.X, Value);
end;

function TPrintPreview.GetPageBounds: TRect;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.BottomRight := fPageExt;
end;

function TPrintPreview.GetPrinterPageBounds: TRect;
var
  Offset: TPoint;
  Size: TPoint;
  DPI: TPoint;
begin
  if PrinterInstalled then
  begin
    DPI.X := GetDeviceCaps(Printer.Handle, LOGPIXELSX);
    DPI.Y := GetDeviceCaps(Printer.Handle, LOGPIXELSY);
    Offset.X := GetDeviceCaps(Printer.Handle, PHYSICALOFFSETX);
    Offset.Y := GetDeviceCaps(Printer.Handle, PHYSICALOFFSETY);
    Offset.X := ConvertUnits(Offset.X, DPI.X, mmPixel, Units);
    Offset.Y := ConvertUnits(Offset.Y, DPI.Y, mmPixel, Units);
    Size.X := GetDeviceCaps(Printer.Handle, HORZRES);
    Size.Y := GetDeviceCaps(Printer.Handle, VERTRES);
    Size.X := ConvertUnits(Size.X, DPI.X, mmPixel, Units);
    Size.Y := ConvertUnits(Size.Y, DPI.Y, mmPixel, Units);
    SetRect(Result, Offset.X, Offset.Y, Offset.X + Size.X, Offset.Y + Size.Y);
  end
  else
    Result := PageBounds;
end;

function TPrintPreview.GetPrinterPhysicalPageBounds: TRect;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := 0;
  Result.Bottom := 0;
  if PrinterInstalled then
  begin
    if UsePrinterOptions then
    begin
      Result.Right := GetDeviceCaps(Printer.Handle, PHYSICALWIDTH);
      Result.Bottom := GetDeviceCaps(Printer.Handle, PHYSICALHEIGHT);
    end
    else
    begin
      Result.Right := ConvertUnits(fPageExt.X,
        GetDeviceCaps(Printer.Handle, LOGPIXELSX), fUnits, mmPixel);
      Result.Bottom := ConvertUnits(fPageExt.Y,
        GetDeviceCaps(Printer.Handle, LOGPIXELSY), fUnits, mmPixel);
    end;
    OffsetRect(Result, -GetDeviceCaps(Printer.Handle, PHYSICALOFFSETX),
      -GetDeviceCaps(Printer.Handle, PHYSICALOFFSETY));
  end;
end;

function TPrintPreview.IsPageCustom: boolean;
begin
  Result := (fPaperType = pCustom);
end;

function TPrintPreview.IsPageRotated: boolean;
begin
  Result := (fOrientation = poLandscape);
end;

procedure TPrintPreview.SetZoom(Value: integer);
var
  OldZoom: integer;
begin
  if Value < fZoomMin then
    Value := fZoomMin
  else if Value > fZoomMax then
    Value := fZoomMax;
  if (fZoom <> Value) or (fZoomState <> zsZoomOther) then
  begin
    OldZoom := fZoom;
    fZoom := Value;
    fZoomState := zsZoomOther;
    UpdateZoom;
    if (OldZoom = fZoom) and Assigned(fOnZoomChange) then
      fOnZoomChange(Self);
  end;
end;

function TPrintPreview.IsZoomStored: boolean;
begin
  Result := (fZoomState = zsZoomOther) and (fZoom <> 100);
end;

procedure TPrintPreview.SetZoomMin(Value: integer);
begin
  if (fZoomMin <> Value) and (Value >= 1) and (Value <= fZoomMax) then
  begin
    fZoomMin := Value;
    if (fZoomState = zsZoomOther) and (fZoom < fZoomMin) then
      Zoom := fZoomMin;
  end;
end;

procedure TPrintPreview.SetZoomMax(Value: integer);
begin
  if (fZoomMax <> Value) and (Value >= fZoomMin) then
  begin
    fZoomMax := Value;
    if (fZoomState = zsZoomOther) and (fZoom > fZoomMax) then
      Zoom := fZoomMax;
  end;
end;

procedure TPrintPreview.SetZoomState(Value: TZoomState);
var
  OldZoom: integer;
begin
  if fZoomState <> Value then
  begin
    OldZoom := fZoom;
    fZoomState := Value;
    UpdateZoom;
    if (OldZoom = fZoom) and Assigned(fOnZoomChange) then
      fOnZoomChange(Self);
  end;
end;

procedure TPrintPreview.SetGrayscale(Value: TGrayscaleOptions);
begin
  if Grayscale <> Value then
  begin
    fGrayscale := Value;
    fPageView.Repaint;
  end;
end;

procedure TPrintPreview.SetGrayBrightness(Value: integer);
begin
  if Value < -100 then
    Value := -100
  else if Value > 100 then
    Value := 100;
  if GrayBrightness <> Value then
  begin
    fGrayBrightness := Value;
    if gsPreview in Grayscale then
    begin
      fPageView.Repaint;
    end;
  end;
end;

procedure TPrintPreview.SetGrayContrast(Value: integer);
begin
  if Value < -100 then
    Value := -100
  else if Value > 100 then
    Value := 100;
  if GrayContrast <> Value then
  begin
    fGrayContrast := Value;
    if gsPreview in Grayscale then
    begin
      fPageView.Repaint;
    end;
  end;
end;

procedure TPrintPreview.SetShowPrintableArea(Value: boolean);
begin
  if fShowPrintableArea = Value then
    exit;
  fShowPrintableArea := Value;
  fPageView.Refresh;
end;

procedure TPrintPreview.SetShowMarginArea(Value: boolean);
begin
  if fShowMarginArea = Value then
    exit;
  fShowMarginArea := Value;
  fPageView.Refresh;
end;

procedure TPrintPreview.SetPrintableAreaColor(Value: TColor);
begin
  if fPrintableAreaColor = Value then
    exit;
  fPrintableAreaColor := Value;
  fPageView.Refresh;
end;

procedure TPrintPreview.SetMarginAreaColor(Value: TColor);
begin
  if fMarginAreaColor = Value then
    exit;
  fMarginAreaColor := Value;
  fPageView.Refresh;
end;

procedure TPrintPreview.SetDirectPrint(Value: boolean);
begin
  if fDirectPrint <> Value then
  begin
    fDirectPrint := Value;
    if fDirectPrint and PrinterInstalled then
      fReferenceDC := Printer.Handle
    else
      fReferenceDC := 0;
    UpdateExtends;
  end;
end;

function TPrintPreview.GetCanvas: TCanvas;
begin
  if Assigned(fPageCanvas) then
    Result := fPageCanvas
  else
    Result := Printer.Canvas;
end;

function TPrintPreview.GetPrinterInstalled: boolean;
begin
  Result := (Printer.Printers.Count > 0);
end;

function TPrintPreview.GetPrinter: TPrinter;
begin
  Result := Printers.Printer;
end;

procedure TPrintPreview.ScaleCanvas(ACanvas: TCanvas);
var
  FontSize: integer;
  LogExt, DevExt: TPoint;
begin
  LogExt := fPageExt;
  DevExt.X := ConvertUnits(LogExt.X, GetDeviceCaps(ACanvas.Handle, LOGPIXELSX),
    fUnits, mmPixel);
  DevExt.Y := ConvertUnits(LogExt.Y, GetDeviceCaps(ACanvas.Handle, LOGPIXELSY),
    fUnits, mmPixel);
  SetMapMode(ACanvas.Handle, MM_ANISOTROPIC);
  SetWindowExtEx(ACanvas.Handle, LogExt.X, LogExt.Y, nil);
  SetViewPortExtEx(ACanvas.Handle, DevExt.X, DevExt.Y, nil);
  SetViewportOrgEx(ACanvas.Handle, -GetDeviceCaps(ACanvas.Handle, PHYSICALOFFSETX),
    -GetDeviceCaps(ACanvas.Handle, PHYSICALOFFSETY), nil);
  FontSize := ACanvas.Font.Size;
  ACanvas.Font.PixelsPerInch :=
    MulDiv(GetDeviceCaps(ACanvas.Handle, LOGPIXELSY), LogExt.Y, DevExt.Y);
  ACanvas.Font.Size := FontSize;
end;

procedure TPrintPreview.UpdateExtends;
begin
  fDeviceExt.X := ConvertX(fPageExt.X, fUnits, mmPixel);
  fDeviceExt.Y := ConvertX(fPageExt.Y, fUnits, mmPixel);
  fLogicalExt.X := MulDiv(fDeviceExt.X, Screen.PixelsPerInch, HorzPixelsPerInch);
  fLogicalExt.Y := MulDiv(fDeviceExt.Y, Screen.PixelsPerInch, VertPixelsPerInch);
end;

procedure TPrintPreview.CreateMetafileCanvas(out AMetafile: TMetafile;
  out ACanvas: TCanvas);
begin
  AMetafile := TMetafile.Create;
  try
    AMetafile.Width := fDeviceExt.X;
    AMetafile.Height := fDeviceExt.Y;
    ACanvas := TMetafileCanvas.CreateWithComment(AMetafile, fReferenceDC,
      Format('%s', [ClassName]), PrintJobTitle);
    if ACanvas.Handle = 0 then
    begin
      ACanvas.Free;
      ACanvas := nil;
      OutOfMemoryError;
    end;
  except
    AMetafile.Free;
    AMetafile := nil;
    raise;
  end;
  ACanvas.Font.Assign(Font);
  ScaleCanvas(ACanvas);
  SetBkColor(ACanvas.Handle, RGB(255, 255, 255));
  SetBkMode(ACanvas.Handle, TRANSPARENT);
end;

procedure TPrintPreview.CloseMetafileCanvas(var AMetafile: TMetafile;
  var ACanvas: TCanvas);
begin
  ACanvas.Free;
  ACanvas := nil;
  if AMetafile.Handle = 0 then
  begin
    AMetafile.Free;
    AMetafile := nil;
    OutOfMemoryError;
  end;
end;

procedure TPrintPreview.CreatePrinterCanvas(out ACanvas: TCanvas);
begin
  ACanvas := TCanvas.Create;
  try
    ACanvas.Handle := Printer.Handle;
    ScaleCanvas(ACanvas);
  except
    ACanvas.Free;
    ACanvas := nil;
    raise;
  end;
end;

procedure TPrintPreview.ClosePrinterCanvas(var ACanvas: TCanvas);
begin
  ACanvas.Handle := 0;
  ACanvas.Free;
  ACanvas := nil;
end;

procedure TPrintPreview.BeginDoc;
begin
  if fState = psReady then
  begin
    fPageCanvas := nil;
    if not fDirectPrint then
    begin
      ChangeState(psCreating);
      if UsePrinterOptions then
        GetPrinterOptions;
      fDirectPrinting := False;
      fReferenceDC := 0;
    end
    else
    begin
      ChangeState(psPrinting);
      fDirectPrinting := True;
      fDirectPrintPageCount := 0;
      if UsePrinterOptions then
        GetPrinterOptions
      else
        SetPrinterOptions;
      Printer.Title := PrintJobTitle;
      Printer.BeginDoc;
      fReferenceDC := Printer.Handle;
    end;
    UpdateExtends;
    if Assigned(fOnBeginDoc) then
      fOnBeginDoc(Self);
    NewPage;
  end;
end;

procedure TPrintPreview.EndDoc;
begin
  if ((fState = psCreating) and not fDirectPrinting) or
    ((fState = psPrinting) and fDirectPrinting) then
  begin
    if Assigned(fOnEndPage) then
      fOnEndPage(Self);
    if not fDirectPrinting then
    begin
      CloseMetafileCanvas(fPageMetafile, fPageCanvas);
    end
    else
    begin
      Inc(fDirectPrintPageCount);
      ClosePrinterCanvas(fPageCanvas);
      Printer.EndDoc;
      fDirectPrinting := False;
    end;
    if Assigned(fOnEndDoc) then
      fOnEndDoc(Self);
    ChangeState(psReady);
  end;
end;

procedure TPrintPreview.NewPage;
begin
  if ((fState = psCreating) and not fDirectPrinting) or
    ((fState = psPrinting) and fDirectPrinting) then
  begin
    if Assigned(fPageCanvas) and Assigned(fOnEndPage) then
      fOnEndPage(Self);
    if not fDirectPrinting then
    begin
      if Assigned(fPageCanvas) then
      begin
        CloseMetafileCanvas(fPageMetafile, fPageCanvas);
        try
        finally
          fPageMetafile.Free;
          fPageMetafile := nil;
        end;
      end;
      CreateMetafileCanvas(fPageMetafile, fPageCanvas);
    end
    else
    begin
      if Assigned(fPageCanvas) then
      begin
        Inc(fDirectPrintPageCount);
        Printer.NewPage;
      end
      else
        CreatePrinterCanvas(fPageCanvas);
      fPageCanvas.Font.Assign(Font);
    end;
    if Assigned(fOnNewPage) then
      fOnNewPage(Self);
  end;
end;

function TPrintPreview.LoadPageInfo(Stream: TStream): boolean;
var
  Header: TStreamHeader;
  Data: integer;
begin
  Result := False;
  Stream.ReadBuffer(Header, SizeOf(Header));
  if CompareMem(@Header.Signature, @PageInfoHeader.Signature,
    SizeOf(Header.Signature)) then
  begin
    Stream.ReadBuffer(Data, SizeOf(Data));
    fOrientation := TPrinterOrientation(Data);
    Stream.ReadBuffer(Data, SizeOf(Data));
    fPaperType := TPaperType(Data);
    Stream.ReadBuffer(Data, SizeOf(Data));
    fPageExt.X := ConvertX(Data, mmHiMetric, fUnits);
    Stream.ReadBuffer(Data, SizeOf(Data));
    fPageExt.Y := ConvertY(Data, mmHiMetric, fUnits);
    UpdateExtends;
    Result := True;
  end;
end;

procedure TPrintPreview.SavePageInfo(Stream: TStream);
var
  Data: integer;
begin
  Stream.WriteBuffer(PageInfoHeader, SizeOf(PageInfoHeader));
  Data := Ord(fOrientation);
  Stream.WriteBuffer(Data, SizeOf(Data));
  Data := Ord(fPaperType);
  Stream.WriteBuffer(Data, SizeOf(Data));
  Data := ConvertX(fPageExt.X, fUnits, mmHiMetric);
  Stream.WriteBuffer(Data, SizeOf(Data));
  Data := ConvertY(fPageExt.Y, fUnits, mmHiMetric);
  Stream.WriteBuffer(Data, SizeOf(Data));
end;

procedure TPrintPreview.LoadFromStream(Stream: TStream);
begin
  ChangeState(psLoading);
  try
    if not LoadPageInfo(Stream) then
      raise EPreviewLoadError.Create('Loading Error');
  finally
    ChangeState(psReady);
  end;
end;

procedure TPrintPreview.SaveToStream(Stream: TStream);
begin
  ChangeState(psSaving);
  try
    SavePageInfo(Stream);
  finally
    ChangeState(psReady);
  end;
end;

procedure TPrintPreview.LoadFromFile(const FileName: string);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TPrintPreview.SaveToFile(const FileName: string);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
  try
    SaveToStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TPrintPreview.Print;
var
  I: integer;
  PageRect: TRect;
  Succeeded: boolean;
begin
  if (fState = psReady) and PrinterInstalled then
  begin
    ChangeState(psPrinting);
    try
      Succeeded := False;
      InitializePrinting;
      try
        PageRect := PrinterPhysicalPageBounds;
        Printer.NewPage;
        PrintPage(Printer.Canvas, PageRect);
        Succeeded := True;
      finally
        FinalizePrinting(Succeeded);
      end;
    finally
      ChangeState(psReady);
    end;
  end;
end;

procedure TPrintPreview.SetBitmap(Value: TBitmap);
begin
  fBitmap := Value;
  UpdateBitmap;
  fPageView.Repaint;
end;

procedure TPrintPreview.UpdateBitmap;
var
  k: double;
  L, x: integer;
  r: TRect;
begin
  if fBitmap = nil then
    exit;
  BeginDoc;
  try
    x := 0;
    L := Length(PageHeader);
    if L > 0 then
      Inc(x, 30);
    if (fBitmap.Width = 0) or (fBitmap.Height = 0) then
      exit;
    Canvas.Brush.Color:=clWhite;
    Canvas.FillRect(Canvas.ClipRect);
    r := fMarginRect;
    k := r.Width / fBitmap.Width;
    r.Bottom := Round(r.Top + (fBitmap.Height + x) * k);
    if r.Bottom > fMarginRect.Bottom then
    begin
      r := fMarginRect;
      k := r.Height / (fBitmap.Height + x);
      r.Right := Round(r.Left + fBitmap.Width * k);
    end;
    x := Round(x * k);
    SetStretchBltMode(Canvas.Handle,HALFTONE);
    StretchBlt(Canvas.Handle, r.Left, r.Top + x, r.Width, r.Height,
      fBitmap.Canvas.Handle, 0, 0, fBitmap.Width, fBitmap.Height, SrcCopy);
    if L > 0 then
    begin
      Canvas.Font.Size := 11;
      DrawText(Canvas.Handle, PChar(PageHeader), L, r, DT_SINGLELINE or DT_END_ELLIPSIS);
    end;
  finally
    EndDoc;
  end;
end;

procedure TPrintPreview.AdvancedProperties;
var
  DeviceMode, h: THandle;
  DevMode: PDeviceMode;
  Device, Driver, Port: array[0..MAX_PATH] of char;
  x: integer;
  po: TPrinterOrientation;
begin
  if Printer.Printers.Count > 0 then
  begin
    Printer.GetPrinter(Device, Driver, Port, DeviceMode);
    DevMode := PDevMode(GlobalLock(DeviceMode));
    try
      if not OpenPrinter(Device, h, nil) then
      begin
        h := 0;
        raise EPrintPreviewError.CreateFmt('OpenPrinter exception : %s',
          [SysErrorMessage(GetlastError)]);
      end;
      try
        x := DocumentProperties(Application.Handle, h, Device, DevMode^, DevMode^, DM_OUT_BUFFER or
          DM_IN_BUFFER or DM_IN_PROMPT);
      finally
        ClosePrinter(h);
      end;
      if (x = ID_OK) then
      begin
        po := TPrinterOrientation(DevMode.dmOrientation - 1);
        if fOrientation <> po then
        begin
          Printer.Orientation := po;
          Swap(fPageExt.X, fPageExt.Y);
          SetPageSizeOrientation(fPageExt.X, fPageExt.Y, Printer.Orientation, False);
          UpdateBitmap;
          DoPageChange;
        end;
      end;
    finally
      GlobalUnlock(DeviceMode);
    end;
  end;
end;

procedure TPrintPreview.ShowAddPrinterDialog;
begin
  with TAddPrinterDialog.Create(nil) do
    try
      Execute;
    finally
      Free;
    end;
end;

procedure TPrintPreview.ShowPageSetupDialog;
var
  dlg: TPageSetupDialogEx;
begin
  dlg := TPageSetupDialogEx.Create(nil);
  try
    SetPageSetupParameters(dlg);
    dlg.PageHeader := PageHeader;
    dlg.PageCopies := Printer.Copies;
    dlg.OnPaint := DlgPaint;
    if not dlg.Execute then
      exit;
    Printer.Copies := dlg.PageCopies;
    PageHeader := Trim(dlg.PageHeader);
    GetPageSetupParameters(dlg);
  finally
    dlg.Free;
  end;
  fPageView.Invalidate;
end;

procedure TPrintPreview.DlgPaint(Sender: TPageSetupDialogEx; Paper, Flags: integer;
  PageSetupRec: TPageSetupDlg; PaintWhat: TDlgPaintWhat; Canvas: TCanvas;
  Rect: TRect; var NoDefaultPaint: boolean);
var
  k, sx, sy: double;
  L, T: integer;
  r, ir: TRect;
begin
  NoDefaultPaint := false;
  if (fBitmap = nil) then
    exit;
  with PageSetupRec.rtMargin, PageSetupRec.ptPaperSize do
  begin
    sx := Rect.Width / X;
    sy := Rect.Height / Y;
    SetRect(r, Round(sx * Left), Round(sy * Top), Round(sx * (X - Right)),
      Round(sy * (Y - Bottom)));
  end;
  ir := r;
  T := 0;
  L := Length(PageHeader);
  if L > 0 then
    Inc(T, 5);

  if (fBitmap.Width = 0) or (fBitmap.Height = 0) then
    exit;
  Canvas.Brush.Color := clWhite;
  Canvas.FillRect(Canvas.ClipRect);
  k := ir.Width / fBitmap.Width;
  ir.Bottom := Round(ir.Top + (fBitmap.Height + T) * k);
  if ir.Bottom > r.Bottom then
  begin
    ir := r;
    k := ir.Height / (fBitmap.Height + T);
    ir.Right := Round(ir.Left + fBitmap.Width * k);
  end;
  SetStretchBltMode(Canvas.Handle, HALFTONE);
  StretchBlt(Canvas.Handle, ir.Left, ir.Top + T, ir.Width, ir.Height - T,
    fBitmap.Canvas.Handle, 0, 0, fBitmap.Width, fBitmap.Height, SrcCopy);
  if L > 0 then
  begin
    Canvas.Font.Size := 2;
    DrawText(Canvas.Handle, PChar(PageHeader), L, r, DT_SINGLELINE or DT_END_ELLIPSIS);
  end;

  with Canvas do
  begin
    Pen.Mode := pmMask;
    Pen.Width := 0;
    Pen.Style := psDot;
    Pen.Color := fMarginAreaColor;
    Rectangle(r);
  end;
  NoDefaultPaint := true;

end;

procedure TPrintPreview.ShowPrinterPropertys;
var
  form: TCustomForm;
  wnd: HWND;
begin
  form := GetParentForm(self);
  if Assigned(form) and form.HandleAllocated then
    wnd := form.Handle
  else
    wnd := Application.Handle;
  with TPrinterPropertiesDialog.Create(nil) do
    try
      Execute(Wnd);
    finally
      Free;
    end;
end;

procedure TPrintPreview.Redraw;
begin
  fPageView.Refresh;
end;

procedure TPrintPreview.SetPageHeader(Value: string);
begin
  if fPageHeader = Value then
    exit;
  fPageHeader := Value;
  UpdateBitmap;
  Redraw;
  //DoPageChange;
end;

initialization
  Screen.Cursors[crHand] := LoadCursor(hInstance, 'CURSOR_HAND');
  Screen.Cursors[crGrab] := LoadCursor(hInstance, 'CURSOR_GRAB');

end.
