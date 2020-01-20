unit uNanobrot;

{
  Nanobrot
  Copyright (C) 2019 - 2020 Vasily Makarov

  Original C++ source codes:
  Copyright (C) 2013-2017 Karl Runmo
  Copyright (C) 2017-2018 Claude Heiland-Allen


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

{$I Nanobrot.inc}

uses
  Windows, Messages, Classes, SysUtils, Diagnostics, TimeSpan, GR32,
  GR32_Resamplers, GR32_Rasterizers, uDecNumber, uFloatExp, uHelpers,
  uColorMap, uBase, uMisc;

type

  TProgressStage = (psStarting, psRunning, psEnding);
  TJitterType = (jtNone, jtRandom, jtBurtleHash, jtGaussian);
  TOutColoring = (ocIterations, ocPotential, ocArcTan, ocSquareRoot,
  ocCubicRoot, ocLogarithm, ocStretched, ocDistanceLinear, ocDistanceSqrt,
  ocDistanceLog, ocDistanceEstimationLinear, ocDistanceEstimationSqrt,
  ocDistanceEstimationLog);
  TStringEvent = procedure(Sender: TObject; const aStr: string) of object;
  TProgressEvent = procedure(Sender: TObject; Stage: TProgressStage;
    Progress: Integer; const Msg: string) of object;
  TColorMapRequestEvent = procedure(ColorMap: TColorMap; const aName: string) of object;
  TRasterizerProgressEvent = procedure(Sender: TObject; Progress: Integer) of object;

  TParamType = (ptfWidth, ptHeight, ptCenterX, ptCenterY, ptZoom, ptIterations,
    ptPeriod, ptOrderM, ptOrderN, ptJitter, ptSupersample, ptColorMapFn, ptOutColoring);

  TCalcCallback = function(obj: Pointer; value: Int64): Boolean; cdecl;

  TLevel = record
    Iters: Integer;
	  Trans: Single;
  end;

  ENanobrot = class(Exception);
  TNanobrot = class;
  TBiPoly = class;

  //Polynomial
  TPoly = record
  private
    fM: Integer;
    fTab: TFloatExpComplexArray128;
  public
    constructor Create(M: Integer); overload;
    constructor Create(p: TPoly); overload;
    function Eval(const u: TFloatExpComplex): TFloatExpComplex;
    function EvalD(const u: TFloatExpComplex): TFloatExpComplex;
    function EvalR(const u: TFloatExp): TFloatExp;
    function GetRoot: TFloatExpComplex;
    function Shift(const s: TFloatExpComplex): TPoly;
  end;

  TTmpPoly = record
  private
    fM: Integer;
    fB: TFloatExpComplexArray128;
  public
    constructor Create(p: TBiPoly);
    function GetRoot: TFloatExpComplex;
    function Eval(const u: TFloatExpComplex): TFloatExpComplex;
    function EvalD(const u: TFloatExpComplex): TFloatExpComplex;
  end;

  TUniPoly = record
  private
    fM: Integer;
    fB: TFloatExpComplexArray128;
    fDbdc: TFloatExpComplexArray128;
  public
    constructor Create(p: TBiPoly; const c: TFloatExpComplex);
    procedure Eval(var z, dc: TFloatExpComplex); overload;
    procedure Eval(var z, dz, dc, dzdz, dcdz: TFloatExpComplex); overload;
    procedure EvalDz(var z, dz: TFloatExpComplex);
  end;

  TBiPoly = class(TPersistent)
  private
    fM: Integer;
    fN: Integer;
    fPeriod: Integer;
    fEscRadius: TFloatExp;
    fShift: TFloatExpComplex;
    fTab: TFloatExpComplexMatrix128;
    fTTab: TFloatExpComplexMatrix128;
    procedure MCopy; {$IFDEF DOINLINE} inline; {$ENDIF}
    function CSqrc(k, l: Integer): TFloatExpComplex; {$IFDEF DOINLINE} inline; {$ENDIF}
  public
    constructor Create; overload;
    constructor Create(bpc: TBiPoly); overload;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    procedure Reset;
    procedure SetOrder(const aM, aN: Integer);  {$IFDEF DOINLINE} inline; {$ENDIF}
    procedure Sqr; {$IFDEF DOINLINE} inline; {$ENDIF}
    procedure CStep(const z: TFloatExpComplex); {$IFDEF DOINLINE} inline; {$ENDIF}
    procedure SetEscRadius(const multiplier: TFloatExp); {$IFDEF DOINLINE} inline; {$ENDIF}
    function Eval(const u, v: TFloatExpComplex): TFloatExpComplex; {$IFDEF DOINLINE} inline; {$ENDIF}
    function EvalDc(const u, v: TFloatExpComplex): TFloatExpComplex; {$IFDEF DOINLINE} inline; {$ENDIF}
    function EvalDz(const u, v: TFloatExpComplex): TFloatExpComplex; {$IFDEF DOINLINE} inline; {$ENDIF}
    function GetRadius: TFloatExp;
    function GetSAPoly: TPoly;
  end;

  TRef = class
  private
    fList: TList;
    function GetItem(const Index: Integer): TFloatExpComplex; {$IFDEF DOINLINE} inline; {$ENDIF}
    function GetEmpty: Boolean;
    function GetCount: Integer; {$IFDEF DOINLINE} inline; {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; {$IFDEF DOINLINE} inline; {$ENDIF}
    procedure Add(const Item: TFloatExpComplex); {$IFDEF DOINLINE} inline; {$ENDIF}
    property Item[const Index: Integer]: TFloatExpComplex read GetItem; default;
    property Empty: Boolean read GetEmpty;
    property Count: Integer read GetCount;
  end;

  TFractalSampler = class(TCustomSampler)
  private
    fNB: TNanobrot;
  public
    constructor Create(aNB: TNanobrot);
    function GetSampleFloat(X, Y: TFloat): TColor32; override;
  end;

  TRasterizerType = (rtRegular, rtProgressive, rtSwizzling);

  TRegularRasterizerEx = class(TRegularRasterizer)
  private
    fOnProgress: TRasterizerProgressEvent;
  protected
    procedure DoRasterize(Dst: TCustomBitmap32; DstRect: TRect); override;
  public
    constructor Create; override;
  published
    property OnProgress: TRasterizerProgressEvent read fOnProgress write fOnProgress;
  end;

  TProgressiveRasterizerEx = class(TProgressiveRasterizer)
  private
    fOnProgress: TRasterizerProgressEvent;
  protected
    procedure DoRasterize(Dst: TCustomBitmap32; DstRect: TRect); override;
  published
    property OnProgress: TRasterizerProgressEvent read fOnProgress write fOnProgress;
  end;

  TSwizzlingRasterizerEx = class(TSwizzlingRasterizer)
  private
    fOnProgress: TRasterizerProgressEvent;
  protected
    procedure DoRasterize(Dst: TCustomBitmap32; DstRect: TRect); override;
  published
    property OnProgress: TRasterizerProgressEvent read fOnProgress write fOnProgress;
  end;

  TJobThread = class(TThread)
  protected
    fNB: TNanobrot;
    fMsg: string;
    fMessage: string;
    fStage: TProgressStage;
    fProgress: Integer;
    fStopwatch: TStopwatch;
    fPrevSpan: TTimeSpan;
    fLastTime: Cardinal;
    fException: Exception;
    procedure RasterizerProgress(Sender: TObject; Progress: Integer);
    procedure DoMessage; virtual;
    procedure DoProgress; virtual;
    procedure DoHandleException; virtual;
    procedure Rasterize;
    procedure HandleException; virtual;
  public
    constructor Create(aMB: TNanobrot);
    procedure NotifyProgress(Stage: TProgressStage; Progress: Integer);
    procedure NotifyMessage(const aStr: string);
  published
    property Terminated;
  end;

  TRenderThread = class(TJobThread)
  protected
    procedure Execute; override;
  end;

  TRecoloringThread = class(TJobThread)
  protected
    procedure Execute; override;
  end;

  TFindPeriodThread = class(TJobThread)
  private
    fPeriod: Integer;
  protected
    procedure DoProgress; override;
    procedure DoDone;
    procedure Execute; override;
  end;

  TLevelMapsFileThread = class(TJobThread)
  protected
    fImport: Boolean;
    fLevelMapsFileName: string;
  public
    property Import: Boolean read fImport write fImport;
    property LevelMapsFileName: string read fLevelMapsFileName write fLevelMapsFileName;
  end;

  TSaveLevelMapsThread = class(TLevelMapsFileThread)
  protected
    procedure Execute; override;
  end;

  TLoadLevelMapsThread = class(TLevelMapsFileThread)
  protected
    procedure DoDefineParams;
    procedure Execute; override;
  public
    procedure DefineParams;
  end;

  TNanobrot = class(TComandPersistent)
  private
    fWidth: Integer;
    fHeight: Integer;
    fFileName: string;
    fCenterX: AnsiString;
    fCenterY: AnsiString;
    fZoom: AnsiString;
    fZoomExp: Integer;
    fIterations: Integer;
    fPeriod: Integer;
    fOrderM: Integer;
    fOrderN: Integer;
    fJitter: Integer;
    fSupersample: Integer;
    fSupersampleRec: Double;
    fColorMapFn: string;
    fOutColoring: Integer;
    fSlopes: Boolean;
    fSlopeAngle: Single;
    fSlopePower: Single;
    fSlopeRatio: Single;
    fSlopeCoeff: Double;
    fModified: Boolean;
    fC: TCDecNumberComplex;
    fZ: TCDecNumberComplex;
    fZlo: TFloatExpComplex;
    fP: TBiPoly;
    fRef: TRef;
    fRef1: TRef;
    fBout: TFloatExp;
    fRadius: TFloatExp;
    fRadRatio: TFloatExp;
    fDiametr: TFloatExpComplex;
    fNucleusPos: TFloatExpComplex;
    fPixelScale: TFloatExp;
    fJobThread: TJobThread;
    fFindPeriodThread: TFindPeriodThread;
    fRasterizerType: TRasterizerType;
    fRasterizerProgress: TRasterizerProgressEvent;
    fBmp: TBitmap32;
    fCancelled: Boolean;
    fCalcNeeded: Boolean;
    fRecoloringMode: Boolean;
    fOldRecoloringMode: Boolean;
    fColorMap: TColorMap;
    fExportColorMap: Boolean;
    fSoundOnDone: Boolean;
    fSlopeX: Double;
    fSlopeY: Double;
    fHNmbLib: THandle;
    fUsesNmbLib: Boolean;
    fMbPtr: Pointer;
    fTmpIncludeLevelMaps: Boolean;
    fTmpCompression: TCompressionType;
    fOnWork: TNotifyEvent;
    fOnRecoloring: TNotifyEvent;
    fOnResize: TNotifyEvent;
    fOnMessage: TStringEvent;
    fOnProgress: TProgressEvent;
    fOnColorMapRequest: TColorMapRequestEvent;
    fOnLoad: TNotifyEvent;
    fOnFindPeriod: TNotifyEvent;
    fOnPeriodProgress: TProgressEvent;
    procedure Init;
    procedure InitLib;
    procedure UnloadLib;
    procedure Clear;
    procedure SetCenterX(const Value: AnsiString);
    procedure SetCenterY(const Value: AnsiString);
    procedure SetZoom(const Value: AnsiString);
    procedure SetIterations(Value: Integer);
    procedure SetPeriod(Value: Integer);
    procedure SetOrderM(Value: Integer);
    procedure SetOrderN(Value: Integer);
    procedure SetJitter(Value: Integer);
    procedure SetSupersample(Value: Integer);
    procedure SetOutColoring(Value: Integer);
    procedure SetColorMapFn(const Value: string);
    procedure SetMapRange(Value: Double);
    procedure SetMapOffset(Value: Double);
    procedure SetRecoloringMode(Value: Boolean);
    procedure SetSlopes(Value: Boolean);
    procedure SetSlopeAngle(Value: Single);
    procedure SetSlopePower(Value: Single);
    procedure SetSlopeRatio(Value: Single);
    procedure SetUndoLimit(const Value: Integer);
    procedure UpdateSupersample;
    procedure UpdateSlopeAngle;
    procedure UpdateSlopeCoeff;
    procedure Calculate;
    procedure PreCalculate;
    procedure JobTerminate(Sender: TObject);
    procedure LoadLevelMapsTerminate(Sender: TObject);
    procedure FindPeriodTerminate(Sender: TObject);
    procedure UpdateRasterizer;
    procedure SaveLevelMaps(const aFileName: string);
    procedure LoadLevelMaps(const aFileName: string; import: Boolean = false);
    procedure SetRasterizerType(const Value: TRasterizerType);
    function Perturbation(d0, d, dd: TFloatExpComplex; N: Integer): boolean;  {$IFDEF DOINLINE} inline; {$ENDIF}
    function IteratePt(const d0: TFloatExpComplex): Boolean;  {$IFDEF DOINLINE} inline; {$ENDIF}
    function IteratePtDE2(const d0: TFloatExpComplex): Boolean; {$IFDEF DOINLINE} inline; {$ENDIF}
    function GetColorMapOffset: Double;
    function GetColorMapRange: Double;
    function ApplySlopes(C: TColor32; sX, sY: TFloat): TColor32;
    function GetUndoLimit: Integer;
  protected
    fSampler: TFractalSampler;
    fSuperSampler: TSuperSampler;
    fRasterizer: TRasterizer;
    fStoreLevel: Boolean;
    fLevel: TLevelStorage;
    fInternalRecoloring: Boolean;
    fInternalRecoloringNeed: Boolean;
    procedure DoLoad;
    procedure DoLoadLevelMap(Sender: TObject);
    procedure DoChande;
    procedure DoColormapRequest;
    procedure DoReset;
    procedure DoResize; virtual;
    procedure DoMessage(const aStr: string);
    procedure DoProgress(Stage: TProgressStage; Progress: Integer; const Msg: string);
    procedure DoFindPeriod(const aPeriod: Integer);
    procedure DoPeriodProgress(const Current: Integer);
    procedure DoExecuteComand(const aCmdID: TComandID; const aData: Variant); override;
    procedure RasterizerProgress(Sender: TObject; Progress: Integer);
    procedure InternalRecoloring;
    procedure CalcSample(X, Y: TFloat); {$IFDEF DOINLINE} inline; {$ENDIF}
    function GetCurrentColor(X, Y: TFloat): TColor32; {$IFDEF DOINLINE} inline; {$ENDIF}
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function Runned: Boolean; override;
    function GetSampleFloat(X, Y: TFloat): TColor32; {$IFDEF DOINLINE} inline; {$ENDIF}
    procedure Start;
    procedure Stop;
    procedure Recoloring;
    procedure RevertToSaved;
    procedure LoadDefault;
    procedure LoadFromFile(const aFileName: string);
    procedure SaveToFile(const aFileName: string);
    procedure ImportLevelMap(const aFileName: string);
    procedure ExportLevelMap(const aFileName: string);
    procedure ImportKallesParams(const aFileName: string);
    procedure ExportKallesParams(const aFileName: string);
    procedure SetSize(aWidth, aHeight: Integer);
    procedure FindPeriod;
    procedure CancelPeriodSearch;
    function IsPeriodSearch: Boolean;
    property Modified: Boolean read fModified;
    property Cancelled: Boolean read fCancelled;
    property ColorMap: TColorMap read fColorMap;
    property FileName: string read fFileName;
    property Width: Integer read fWidth;
    property Height: Integer read fHeight;
    property Level: TLevelStorage read fLevel;
  published
    property Bitmap: TBitmap32 read fBmp write fBmp;
    property RecoloringMode: Boolean read fRecoloringMode write SetRecoloringMode;
    property CenterX: AnsiString read fCenterX write SetCenterX;
    property CenterY: AnsiString read fCenterY write SetCenterY;
    property Zoom: AnsiString read fZoom write SetZoom;
    property Iterations: Integer read fIterations write SetIterations;
    property Period: Integer read fPeriod write SetPeriod;
    property OrderM: Integer read fOrderM write SetOrderM;
    property OrderN: Integer read fOrderN write SetOrderN;
    property Jitter: Integer read fJitter write SetJitter;
    property Supersample: Integer read fSupersample write SetSupersample;
    property OutColoring: Integer read fOutColoring write SetOutColoring;
    property ColorMapFn: string read fColorMapFn write SetColorMapFn;
    property ColorMapRange: Double read GetColorMapRange write SetMapRange;
    property ColorMapOffset: Double read GetColorMapOffset write SetMapOffset;
    property Slopes: Boolean read fSlopes write SetSlopes;
    property SlopeAngle: Single read fSlopeAngle write SetSlopeAngle;
    property SlopePower: Single read fSlopePower write SetSlopePower;
    property SlopeRatio: Single read fSlopeRatio write SetSlopeRatio;
    property ExportColorMap: Boolean read fExportColorMap write fExportColorMap;
    property RasterizerType: TRasterizerType read fRasterizerType write SetRasterizerType;
    property SoundOnDone: Boolean read fSoundOnDone write fSoundOnDone;
    property UndoLimit: Integer read GetUndoLimit write SetUndoLimit;
    property OnLoad: TNotifyEvent read fOnLoad write fOnLoad;
    property OnWork: TNotifyEvent read fOnWork write fOnWork;
    property OnResize: TNotifyEvent read fOnResize write fOnResize;
    property OnRecoloring: TNotifyEvent read fOnRecoloring write fOnRecoloring;
    property OnMessage: TStringEvent read fOnMessage write fOnMessage;
    property OnProgress: TProgressEvent read fOnProgress write fOnProgress;
    property OnColorMapRequest: TColorMapRequestEvent read fOnColorMapRequest write fOnColorMapRequest;
    property OnFindPeriod: TNotifyEvent read fOnFindPeriod write fOnFindPeriod;
    property OnPeriodProgress: TProgressEvent read fOnPeriodProgress write fOnPeriodProgress;
  end;


var
  fpZero: TFloatExp;
  fpHalf: TFloatExp;
  fpOne: TFloatExp;
  fpTenM6: TFloatExp;
  fpTenM1: TFloatExp;
  fpTwo: TFloatExp;
  fcZero: TFloatExpComplex;
  fcOne: TFloatExpComplex;
  fcTwo: TFloatExpComplex;
  dLn2Rec: Double;

  nmb_create: function(): Pointer; cdecl;
	nmb_delete: procedure(nmb: Pointer); cdecl;
  nmb_calc: procedure(nmb: Pointer; owner: Pointer; callback: TCalcCallback); cdecl;
	nmb_iterate: function(nmb: Pointer; const cX, cY: Double): Boolean; cdecl;
	nmb_get_level: procedure(nmb: Pointer; var level: Tlevel); cdecl;
  nmb_set_re: procedure(nmb: Pointer; re: PAnsiChar); cdecl;
	nmb_set_im: procedure(nmb: Pointer; im: PAnsiChar); cdecl;
  nmb_set_zoom: procedure(nmb: Pointer; zoom: PAnsiChar); cdecl;
	nmb_set_bm: procedure(nmb: Pointer; bm: Integer); cdecl;
	nmb_set_bn: procedure(nmb: Pointer; bn: Integer); cdecl;
	nmb_set_maxiters: procedure(nmb: Pointer; maxiters: Integer); cdecl;
	nmb_set_period: procedure(nmb: Pointer; period: Integer); cdecl;
  nmb_find_period_setup: procedure(nmb: Pointer); cdecl;
  nmb_find_period_cycle: procedure(nmb: Pointer; var re, im: TFloatExp); cdecl;
  nmb_get_radius: procedure(nmb: Pointer; var radius: TFloatExp); cdecl;

const
  dInfinity: Double  =  1.0 / 0.0;
  dPiD4 = Pi / 4;

  function BurtleHash(a: Cardinal): Cardinal; {$IFDEF DOINLINE} inline; {$ENDIF}
  function DitherJitter(x, y, c: Integer): Double; {$IFDEF DOINLINE} inline; {$ENDIF}
  procedure GaussianJitter(s: Double; var x, y: Double); {$IFDEF DOINLINE} inline; {$ENDIF}

implementation

uses
  Forms, Dialogs, IniFiles, StrUtils, Math, GR32_Math, GR32_LowLevel,
  uFrmImportLevelMap;

type
  TCustomBitmap32Access = class(TCustomBitmap32);

const
  sFractalSettings = 'FractalSettings';
  sRenderSettings = 'RenderSettings';
  sLevelmaps = 'LevelMaps';
  dTwoPi = 2 * Pi;
  d1d3 = 1 / 3;



function BurtleHash(a: Cardinal): Cardinal;
begin
  a := (a + $7ed55d16) + (a shl 12);
  a := (a xor $c761c23c) xor (a shr 19);
  a := (a + $165667b1) + (a shl 5);
  a := (a + $d3a2646c) xor (a shl 9);
  a := (a + $fd7046c5) + (a shl 3);
  a := (a xor $b55a4f09) xor (a shr 16);
  result := a;
end;

// uniform in [0,1)
function DitherJitter(x, y, c: Integer): Double;
begin
  result := BurtleHash(x + BurtleHash(y + BurtleHash(c))) / $100000000;
end;

procedure GaussianJitter(s: Double; var x, y: Double);
var
  r, t, u, v: Double;
begin
  u := DitherJitter(Round(x), Round(y), 0);
	v := DitherJitter(Round(x), Round(y), 1);

  r := IfThen((0 < u) and (u < 1), sqrt(-2 * ln(u)), 0);
	t := dTwoPi * v;
  SinCos(t, y, x);
  s := s * 0.5;
	x := s * r * x;
	y := s * r * y;
end;

function RenderTimeToStr(const aTime: TTimeSpan): string;
begin
  with aTime do
    if Days > 0 then
      result := Format('%.2d:%.2d:%.2d:%.2d.%.3d',
        [Days, Hours, Minutes, Seconds, Milliseconds])
    else
    if Hours > 0 then
      result := Format('%.2d:%.2d:%.2d.%.3d',
        [Hours, Minutes, Seconds, Milliseconds])
    else
      result := Format('%.2d:%.2d.%.3d',
        [Minutes, Seconds, Milliseconds]);
end;

function CalcCalback(obj: Pointer; value: Int64): Boolean; cdecl;
begin
  TJobThread(obj).NotifyProgress(psRunning, Integer(value));
  result := TJobThread(obj).Terminated;
end;


{ TPoly }


constructor TPoly.Create(M: Integer);
var
  i: Integer;
begin
  fM := M;
  for i := 0 to 127 do
    fTab[i] := fpZero;
end;

constructor TPoly.Create(p: TPoly);
var
  i: Integer;
begin
  Create(p.fM);
  for i := 0 to fM do
    fTab[i] := p.fTab[i];
end;


function TPoly.Eval(const u: TFloatExpComplex): TFloatExpComplex;
var
  i: Integer;
  ui: TFloatExpComplex;
begin
  result := fTab[fM];
	ui := fcOne;
  for i := fM - 1 downto 0 do
    result := fTab[i] + u * result;
end;

function TPoly.EvalD(const u: TFloatExpComplex): TFloatExpComplex;
var
  i: Integer;
  ui: TFloatExpComplex;
begin
  result := fTab[fM] * fM;
	ui := fcOne;
  for i := fM - 1 downto 0 do
    result := fTab[i] * i + u * result;
end;

function TPoly.EvalR(const u: TFloatExp): TFloatExp;
begin
  result := u * u * fTab[2].Abs;
end;

function TPoly.GetRoot: TFloatExpComplex;
var
  i: Integer;
  num, den, delta: TFloatExpComplex;
begin
  result := fcZero;
  for i := 0 to 19 do
  begin
		num := Eval(result);
		den := EvalD(result);
		delta := num / den;
    num := result;
		result := result - delta;
    if (result.Re = num.Re) and (result.Im = num.Im) then
      break;
  end;
end;

function TPoly.Shift(const s: TFloatExpComplex): TPoly;
var
  i, j: Integer;
  v: TFloatExp;
  bino: array[0..127] of TFloatExp;
begin
  result := TPoly.Create(fM);
  // now is not impement
  {
  for i := 0 to fM do
    bino[i] := fpOne;
  for i := 0 to fM do
  begin
		v := fpZero;
    for j := fM - i downto 0 do
      v := fTab[j + i] * bino[j] + s * v;
    for j := 1 to fM do
      bino[j] := bino[j] + bino[j - 1];
    result.fTab[i] := v;
  end;
  }
end;



{ TBiPoly }

constructor TBiPoly.Create;
begin
  fM := 0;
  fN := 0;
  fPeriod := 0;
  fShift := fcZero;
  fEscRadius := fpZero;
  Clear;
  fTab[1, 0] := fcOne;
end;

constructor TBiPoly.Create(bpc: TBiPoly);
begin
  Create;
  Assign(bpc);
end;

procedure TBiPoly.Assign(Source: TPersistent);
var
  i, j: Integer;
begin
  if Source is TBiPoly then begin
    fM := TBiPoly(Source).fM;
    fN := TBiPoly(Source).fN;
    fPeriod := TBiPoly(Source).fPeriod;
    fEscRadius := TBiPoly(Source).fEscRadius;
    fShift := TBiPoly(Source).fShift;
    for i := 0 to fm do
      for j := 0 to fn do
        fTab[i, j] := TBiPoly(Source).fTab[i, j];
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TBiPoly.Clear;
var
  i, j: Integer;
begin
  for i := 0 to 127 do
    for j := 0 to 127 do
    begin
      fTab[i, j] := fcZero;
      fTTab[i, j] := fcZero;
    end;
end;

procedure TBiPoly.Reset;
var
  i, j: Integer;
  par1: TFloatExpComplex;
begin
  //save first param
	par1 := fTab[0, 0];
  for i := 0 to fm do
    for j := 0 to fn do
    begin
      fTab[i, j] := fcZero;
      fTTab[i, j] := fcZero;
    end;
  fTab[1, 0] := fcOne;
  //restore 1st param
  fTab[0, 0] := par1;
end;

procedure TBiPoly.SetEscRadius(const multiplier: TFloatExp);
begin
  fEscRadius := multiplier * getRadius();
end;

procedure TBiPoly.SetOrder(const aM, aN: Integer);
begin
  fM := aM;
  fN := aN;
  Clear;
  fTab[1, 0] := fcOne;
end;

function TBiPoly.CSqrc(k, l: Integer): TFloatExpComplex;
var
  i, j: Integer;
begin
  result := fcZero;
  for i := 0 to k do
    for j := 0 to l do
      result := result + (fTTab[i,j] * fTTab[k-i, l-j]);
end;

procedure TBiPoly.MCopy;
var
  i, j: Integer;
begin
  for i := 0 to fM do
    for j := 0 to fN do
      fTTab[i, j] := fTab[i, j];
end;

procedure TBiPoly.Sqr;
var
  i, j: Integer;
begin
  MCopy;
  for i := 0 to fM do
    for j := 0 to fN do
      fTab[i, j] := CSqrc(i,j);
end;

procedure TBiPoly.CStep(const z: TFloatExpComplex);
begin
  Sqr;
	fTab[0, 0] := z;
	fTab[0, 1] := fTab[0, 1] + fcOne;
end;


function TBiPoly.Eval(const u, v: TFloatExpComplex): TFloatExpComplex;
var
  i, j: Integer;
  ui, u2, vj: TFloatExpComplex;
begin
  result := 0;
  ui := 1;
  u2 := u * u;
  i := 0;
  while i <= fM do begin
    vj := ui;
    for j := 0 to fN do begin
      result := result + fTab[i,j] * vj;
      vj := vj * v;
    end;
    ui := ui * u2;
    Inc(i,2);
  end;
end;

function TBiPoly.EvalDc(const u, v: TFloatExpComplex): TFloatExpComplex;
var
  i, j: Integer;
  ui, u2, vj: TFloatExpComplex;
begin
  result := 0;
  ui := 1;
  u2 := u * u;
  i := 0;
  while i <= fM do begin
    vj := ui;
    for j := 1 to fN do begin
      result := result + j * fTab[i,j] * vj;
      vj := vj * v;
    end;
    ui := ui * u2;
    Inc(i,2);
  end;
end;

function TBiPoly.EvalDz(const u, v: TFloatExpComplex): TFloatExpComplex;
var
  i, j: Integer;
  ui, u2, vj: TFloatExpComplex;
begin
  result := 0;
  ui := u;
  u2 := u * u;
  i := 2;
  while i <= fM do begin
    vj := ui * i;
    for j := 0 to fN do begin
      result := result + fTab[i, j] * vj;
      vj := vj * v;
    end;
    ui := ui * u2;
    Inc(i, 2);
  end;
end;

function TBiPoly.GetRadius: TFloatExp;
var
  i, j: Integer;
  rr: TFloatExp;
  den: TFloatExpComplex;
begin
  result := fTab[0, 1].Abs / fTab[0, 2].Abs;
  //return abs(tab[0][1])/abs(tab[0][2]);
  {
  result := fpZero;
	for i := 0 to 9 do begin
    den := fcZero;
    rr := fpOne;
    for j := 2 to fN do begin
      den := den + fTab[0, j] * rr;
      rr := rr * result;
    end;
    result := fTab[0, 1].Abs / den.Abs;
  end;
  result := fpHalf * result;
  }
end;

function TBiPoly.GetSAPoly: TPoly;
var
  i: Integer;
begin
  result := TPoly.Create(fN);
  for i := 0 to fN do
    result.fTab[i] := fTab[0, i];
end;

{ TTmpPoly }

constructor TTmpPoly.Create(p: TBiPoly);
var
  i: Integer;
begin
  fM := p.fN;
  for i := 0 to fM do
    fB[i] := p.fTab[0, i];
end;

function TTmpPoly.Eval(const u: TFloatExpComplex): TFloatExpComplex;
var
  i: Integer;
  ui: TFloatExpComplex;
begin
  result := 0;
  ui := 1;
  for i := 0 to fM - 1 do begin
    result := result + fB[i] * ui;
			ui := ui * u;
  end;
end;

function TTmpPoly.EvalD(const u: TFloatExpComplex): TFloatExpComplex;
var
  i: Integer;
  ui: TFloatExpComplex;
begin
  result := 0;
  ui := 1;
  for i := 1 to fM - 1 do begin
    result := result + i * fB[i] * ui;
			ui := ui * u;
  end;
end;

function TTmpPoly.GetRoot: TFloatExpComplex;
var
  i: Integer;
  num, den,
  delta: TFloatExpComplex;
begin
  result := fcZero;
  for i := 0 to 29 do begin
		num := eval(result);
		den := evalD(result);
		delta := num / den;
    num := result;
    result := result - delta;
    if (result.Re = num.Re) and (result.Im = num.Im) then
      break;
	end;
end;


{ TUniPoly }

constructor TUniPoly.Create(p: TBiPoly; const c: TFloatExpComplex);
var
  i, j: Integer;
  s, ds, cj, cj1: TFloatExpComplex;
begin
  fM := p.fM;
  i := 0;
  while i <= fM do begin
    s := 0; ds := 0; cj := fcOne; cj1 := 0;
			for j := 0 to p.fN do begin
				s := s + p.fTab[i, j] * cj;
				ds := ds + j * p.fTab[i, j] * cj1;
				cj := cj * c;
				cj1 := cj1 * c;
				if j = 0 then
          cj1 := fcOne;
			end;
			fB[i] := s;
			fDbdc[i] := ds;
    Inc(i, 2);
  end;
end;

procedure TUniPoly.Eval(var z, dc: TFloatExpComplex);
var
  i: Integer;
  zs, dcs, zi, zi1, z2: TFloatExpComplex;
begin
  zs := 0; dcs := 0; zi := fcOne; zi1 := 0; i := 0;
  while i <= fM do begin
    dcs := dcs + i * fB[i] * zi1 * dc + fDbdc[i] * zi;
    zs := zs + fB[i] * zi;
    z2 := z * z;
    zi := zi * z2;
    zi1 := zi1 * z2;
    if i = 0 then
      zi1 := z;
    Inc(i, 2);
  end;
  z := zs;
  dc := dcs;
end;

procedure TUniPoly.Eval(var z, dz, dc, dzdz, dcdz: TFloatExpComplex);
var
  i: Integer;
  zs, dzs, dcs, dzdzs, dcdzs,
  zi, zi1, zi2, z2: TFloatExpComplex;
begin
  zs := 0; dzs := 0; dcs := 0; dzdzs := 0; dcdzs := 0;
  zi := fcOne; zi1 := 0; zi2 := 0; i := 0;
  while i <= fM do begin
    dcdzs := dcdzs + TFloatExpComplex(i) * TFloatExpComplex(i - 1) * fB[i] *
      zi2 * dz * dc + i * fB[i] * zi1 * dcdz + i * fDbdc[i] * zi1 * dz;
		dzdzs := dzdzs + TFloatExpComplex(i) * TFloatExpComplex(i - 1) * fB[i] *
      zi2 * dz * dz + i * fB[i] * zi1 * dzdz;
		dcs := dcs + i * fB[i] * zi1 * dc + fDbdc[i] * zi;
		dzs := dzs + i * fB[i] * zi1 * dz;
		zs := zs + fB[i] * zi;
    z2 := z * z;
		zi := zi * z2;
		zi1 := zi1 * z2;
		zi2 := zi2 * z2;
		if i = 0 then begin
      zi1 := z;
      zi2 := fcOne;
    end;
    Inc(i, 2);
  end;
  z := zs;
	dz := dzs;
	dc := dcs;
	dzdz := dzdzs;
	dcdz := dcdzs;
end;

procedure TUniPoly.EvalDz(var z, dz: TFloatExpComplex);
var
  i, j: Integer;
  zs, dzs, zi, zi1, z2: TFloatExpComplex;
begin
  zs := 0; dzs := 0; zi := fcOne; zi1 := 0; i := 0;
  while i <= fM do begin
    dzs := dzs + i * fB[i] * zi1 * dz;
    zs := zs + fB[i] * zi;
    z2 := z * z;
    zi := zi * z2;
		zi1 := zi1 * z2;
    if i = 0 then
      zi1 := z;
    Inc(i, 2);
  end;
  z := zs;
  dz := dzs;
end;


{ TRef }

constructor TRef.Create;
begin
  fList := TList.Create;
end;

destructor TRef.Destroy;
begin
  Clear;
  FreeAndNil(fList);
  inherited;
end;

procedure TRef.Clear;
var
  i: Integer;
begin
  for i:=fList.Count-1 downto 0 do
    FreeMem(fList[i]);
  fList.Clear;
end;

procedure TRef.Add(const Item: TFloatExpComplex);
var
  p: PFloatExpComplex;
begin
  GetMem(p, SizeOf(TFloatExpComplex));
  p^ := Item;
  fList.Add(p);
end;

function TRef.GetCount: Integer;
begin
  result := fList.Count;
end;

function TRef.GetEmpty: Boolean;
begin
  result := fList.Count = 0;
end;

function TRef.GetItem(const Index: Integer): TFloatExpComplex;
begin
  result := PFloatExpComplex(fList[Index mod fList.Count])^;
end;



{ TFractalSampler }

constructor TFractalSampler.Create(aNB: TNanobrot);
begin
  fNB := aNB;
end;

function TFractalSampler.GetSampleFloat(X, Y: TFloat): TColor32;
begin
  try
    Result := fNB.GetSampleFloat(x, y);
  except
    on E: EAbort do
      exit;
    else
      raise ENanobrot.CreateFmt('Error calculate in x=%g, y=%g.', [x, y]);
  end;
end;

{ TCalcThread }

constructor TJobThread.Create(aMB: TNanobrot);
begin
  inherited Create(true);
  fNB := aMB;
  fLastTime := 0;
  fStopwatch := TStopwatch.Create;
  fPrevSpan := fStopwatch.Elapsed;
  FreeOnTerminate := true;
end;

procedure TJobThread.HandleException;
begin
  fException:=Exception(ExceptObject);
  try
    if not (fException is EAbort) then
      Synchronize(DoHandleException);
  finally
    fException := nil;
  end;
end;

procedure TJobThread.DoHandleException;
begin
  if GetCapture <> 0 then
    SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
  if fException is Exception then
    Application.ShowException(fException)
  else
    SysUtils.ShowException(fException,nil);
end;

procedure TJobThread.DoMessage;
begin
  fNB.DoMessage(fMessage);
end;

procedure TJobThread.DoProgress;
begin
  fNB.DoProgress(fStage, fProgress, fMsg);
end;

procedure TJobThread.RasterizerProgress(Sender: TObject; Progress: Integer);
begin
  if Terminated then
    Abort;
  NotifyProgress(psRunning, Progress);
end;

procedure TJobThread.NotifyMessage(const aStr: string);
begin
  fMessage := aStr;
  Synchronize(DoMessage);
end;

procedure TJobThread.NotifyProgress(Stage: TProgressStage; Progress: Integer);
var
  Elapsed: TTimeSpan;
  Remaining: TTimeSpan;
  p, ms: Double;
begin
  p := 0.01;
  fMsg := '';
  fStage := Stage;
  fProgress := Progress;

  case fStage of
    psStarting:
    begin
      fStopwatch.Reset;
      fStopwatch.Start;
      fMsg := 'Elapsed Time 00:00:00.000   Time Remaining ??:??:??.???';
    end;

    psRunning:
    begin
      if fProgress>0 then
        p := fProgress;
      Elapsed := fStopwatch.Elapsed;
      ms := Elapsed.TotalMilliseconds * 1000 / p;
      Remaining := Remaining.FromMilliseconds(ms);
      Remaining := Remaining.Subtract(Elapsed);
      fMsg := Format('Elapsed Time %s   Time Remaining %s',
        [RenderTimeToStr(Elapsed), RenderTimeToStr(Remaining)]);
    end;

    psEnding:
    begin
      if fPrevSpan.Ticks>0 then begin
        fPrevSpan := fPrevSpan.Add(fStopwatch.Elapsed);
        fMsg :=Format('Process Time %s',
          [RenderTimeToStr(fPrevSpan)]);
      end;
      fPrevSpan := fStopwatch.Elapsed;
      fStopwatch.Stop;
    end;
  end;
  if (fStage = psRunning) and (GetTickCount - fLastTime < 200) then
    exit;
  Synchronize(DoProgress);
  fLastTime := GetTickCount;
end;

procedure TJobThread.Rasterize;
begin
  fException:=nil;
  try
    NotifyProgress(psStarting, 0);
    fNB.fRasterizerProgress := RasterizerProgress;
    fNB.fRasterizer.Lock;
    try
      fNB.fRasterizer.Rasterize(fNB.fBmp);
    finally
      fNB.fRasterizer.Unlock;
      fNB.fRasterizerProgress := nil;
      NotifyProgress(psEnding, 0);
      NotifyMessage(IfThen(fNB.Cancelled, 'Cancelled', 'Done'));
    end;
  except
    HandleException;
  end;
end;


{ TRenderThread }

procedure TRenderThread.Execute;
begin
  fNB.Calculate;
  if Terminated then
    Exit;
  NotifyMessage('Rendering image...');
  Rasterize;
end;


{ TRecoloringThread }

procedure TRecoloringThread.Execute;
begin
  if Terminated then
    Exit;
  NotifyMessage('Recoloring image...');
  Rasterize;
end;

{ TFindPeriodThread }

procedure TFindPeriodThread.DoDone;
begin
  fNB.DoFindPeriod(fPeriod);
end;

procedure TFindPeriodThread.DoProgress;
begin
  fNB.DoPeriodProgress(fProgress);
end;

procedure TFindPeriodThread.Execute;
const
  maxperiod = 100000000;
var
  i: Integer;
  fpSPAlive, stopRecSSAs: Boolean;
  SA: TPoly;
  SSA: TBiPoly;
  fp, fpSP: TBiPoly;
  smallest_ref_rad_so_far,
  zmag, er: TFloatExp;
  z: TCDecNumberComplex;
  zlo, HCcentre: TFloatExpComplex;
begin
  NotifyMessage('Searcing Period...');
  if fNb.fUsesNmbLib then
    // setup dll solver
    with fNb do
    begin
      nmb_set_re(fMbPtr, PAnsiChar(fCenterX));
      nmb_set_im(fMbPtr, PAnsiChar(fCenterY));
      nmb_set_zoom(fMbPtr, PAnsiChar(fZoom));
      nmb_find_period_setup(fMbPtr);
    end;
  fPeriod := 0;
  fpSPAlive := false;
  stopRecSSAs := false;
  smallest_ref_rad_so_far := 1000;
  er := fNB.fRadius * 0.01;
  z := 0;
  zlo := fcZero;
  fp := TBiPoly.Create;
  try
    fp.SetOrder(fNB.OrderM, fNB.OrderN);
    fpSP := TBiPoly.Create(fp);
    try
      for i := 0 to maxperiod - 1 do
      begin
        if fNb.fUsesNmbLib then
          nmb_find_period_cycle(fNb.fMbPtr, zlo.Re, zlo.Im)
        else begin
          z := z * z + fNB.fC;
		      zlo := z;
        end;
		    fp.CStep(zlo);
	    	if fpSPAlive then
			    fpSP.CStep(zlo);
        zmag := zlo.Abs;
        if (zmag < smallest_ref_rad_so_far) and (not stopRecSSAs) then
        begin
			    //compute the SSA for current hyperbolic component and store it in SSAs:
          SA := fp.GetSAPoly;
          //compute the corresponding HC center
          HCcentre := SA.GetRoot;
          //get shifted SSA
          SSA := TBiPoly.Create(fp);
          try
			      SSA.fPeriod := i + 1;
			      SSA.SetEscRadius(fpTenM1);
            if SSA.fEscRadius > er then
            // Store the shifted SSA into SSAs
				    // We don't need HC with too low periods. They may be slower than using perturbation.
				    // ToDO: For now, we also accept non-cardioid centers. We need to add the info about
            // if it is a cadioid or a disc. Cardioids are useful for interior checking and DE
            // and their escape radius is much bigger than discs.
            begin
              if i >= 10 then
              begin
                fPeriod := SSA.fPeriod;
					      //Prepare next "super perturbation" biPoly
					      fpSP.Assign(fp);
					      //reset it
					      fpSP.Reset;
					      //Tell outer loop to compute fpSP
					      fpSPAlive := true;
              end;
            end
            else
              stopRecSSAs := true;
            smallest_ref_rad_so_far := zmag;
          finally
            SSA.Free;
          end;
        end;
        if (fp.GetRadius * fpTenM1 < er) then
        begin
          if fPeriod = 0 then
            fPeriod := i + 1;
			    break;
        end;
        if Terminated then
          break;
        if (i mod 100) = 0 then
        begin
          fProgress := i;
          if (GetTickCount - fLastTime >= 200) then
          begin
            Synchronize(DoProgress);
            fLastTime := GetTickCount;
          end;
        end;
      end; // for
    finally
      fpSP.Free;
    end;
  finally
    fp.Free;
  end;
  NotifyMessage(IfThen(Terminated, 'Searcing cancelled', 'Searcing done'));
  Synchronize(DoDone);
end;


{ TSaveLevelMapsThread }

procedure TSaveLevelMapsThread.Execute;
begin
  if Terminated then
    Exit;
  fException:=nil;
    try
    NotifyMessage('Saving level maps...');
    try
      fNb.Level.SaveToFile(fLevelMapsFileName, self);
    finally
      NotifyProgress(psEnding, 0);
      NotifyMessage(IfThen(fNB.Cancelled, 'Cancelled', 'Done'));
    end;
  except
    HandleException;
  end;
end;


{ TLoadLevelMapsThread }

procedure TLoadLevelMapsThread.DefineParams;
begin
  Synchronize(DoDefineParams);
end;

procedure TLoadLevelMapsThread.DoDefineParams;
begin
  with fNb do
  begin
    BeginUpdate;
    try
      SetSupersample(Level.Supersample);
      SetSize(Level.Width, Level.Height);
      DoLoad;
      DoReset;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TLoadLevelMapsThread.Execute;
begin
  if Terminated then
    Exit;
  fException := nil;
  try
    NotifyMessage('Loading level maps...');
    try
      fNb.Level.LoadFromFile(fLevelMapsFileName, self);
    finally
      NotifyProgress(psEnding, 0);
      NotifyMessage(IfThen(fNB.Cancelled, 'Cancelled', 'Done'));
    end;
  except
    HandleException;
  end;
end;

{ TRegularRasterizerEx }

constructor TRegularRasterizerEx.Create;
begin
  inherited Create;
  UpdateRowCount := 1;
end;

procedure TRegularRasterizerEx.DoRasterize(Dst: TCustomBitmap32; DstRect: TRect);
var
  I, J, UpdateCount: Integer;
  P: PColor32;
  GetSample: TGetSampleInt;
begin
  GetSample := Sampler.GetSampleInt;
  UpdateCount := 0;
  for J := DstRect.Top to DstRect.Bottom - 1 do
  begin
    P := @Dst.Bits[DstRect.Left + J * Dst.Width];
    for I := DstRect.Left to DstRect.Right - 1 do
    begin
      AssignColor(P^, GetSample(I, J));
      Inc(P);
    end;
    Inc(UpdateCount);
    if UpdateCount = UpdateRowCount then
    begin
      Dst.Changed(Rect(DstRect.Left, J - UpdateCount, DstRect.Right, J));
      UpdateCount := 0;
    end;
    if Assigned(fOnProgress) then
      fOnProgress(self, MulDiv(1000, j + 1, DstRect.Bottom));
  end;
  with DstRect do
    Dst.Changed(Rect(Left, Bottom - UpdateCount - 1, Right, Bottom));
end;


{ TProgressiveRasterizerEx }

procedure TProgressiveRasterizerEx.DoRasterize(Dst: TCustomBitmap32;
  DstRect: TRect);
var
  I, J, Shift, W, H, B, Wk, Hk, X, Y, G, S, P,
  Step: Integer;
  DoUpdate: Boolean;
  GetSample: TGetSampleInt;
  Bits: PColor32Array;

  procedure IntFillRect(X1, Y1, X2, Y2: Integer; C: TColor32);
  var
    Y: Integer;
    P: PColor32Array;
  begin
    for Y := Y1 to Y2 - 1 do
    begin
      P := Pointer(@Bits[Y * W]);
      FillLongword(P[X1], X2 - X1, C);
    end;
  end;

begin
  GetSample := Sampler.GetSampleInt;
  Bits := Dst.Bits;
  DoUpdate := (TCustomBitmap32Access(Dst).UpdateCount = 0);
  W := DstRect.Right - DstRect.Left;
  H := DstRect.Bottom - DstRect.Top;
  J := DstRect.Top;
  Step := 1 shl Steps;
  // Calculate progress
  S := Step;
  G := Ceil(W / S) * Ceil(H / S);
  while S > 1 do
  begin
    S := S div 2;
    Wk := W div S - 1;
    Hk := H div S;
    for J := 0 to Hk do
    begin
      if Odd(J) then
        Inc(G, Wk + 1)
      else
        Inc(G, Ceil(Wk / 2));
      Inc(G);
    end;
  end;
  // Rastirezation
  P := 0;
  Shift := Steps;
  J := DstRect.Top;
  while J < DstRect.Bottom do
  begin
    I := DstRect.Left;
    B := Min(J + Step, DstRect.Bottom);
    while I < DstRect.Right - Step do
    begin
      IntFillRect(I, J, I + Step, B, GetSample(I, J));
      Inc(P);
      Inc(I, Step);
    end;

    IntFillRect(I, J, DstRect.Right, B, GetSample(I, J));
    Inc(P);
    if DoUpdate and UpdateRows then
      Dst.Changed(Rect(DstRect.Left, J, DstRect.Right, B), AREAINFO_RECT);
    Inc(J, Step);
    if Assigned(fOnProgress) then
      fOnProgress(self, MulDiv(1000, P, G));
  end;
  if DoUpdate and (not UpdateRows) then
    Dst.Changed(DstRect, AREAINFO_RECT);


  Shift := Steps;
  while Step > 1 do
  begin
    Dec(Shift);
    Step := Step div 2;
    Wk := W div Step - 1;
    Hk := H div Step;
    for J := 0 to Hk do
    begin
      Y := DstRect.Top + J shl Shift;
      B := Min(Y + Step, DstRect.Bottom);
      if Odd(J) then
        for I := 0 to Wk do
        begin
          X := DstRect.Left + I shl Shift;
          IntFillRect(X, Y, X + Step, B, GetSample(X, Y));
          Inc(P);
        end
      else
        for I := 0 to Wk do
          if Odd(I) then
          begin
            X := DstRect.Left + I shl Shift;
            IntFillRect(X, Y, X + Step, B, GetSample(X, Y));
            Inc(P);
          end;
      X := DstRect.Left + Wk shl Shift;
      IntFillRect(X, Y, DstRect.Right, B, GetSample(X, Y));
      Inc(P);
      if UpdateRows and DoUpdate then
        Dst.Changed(Rect(DstRect.Left, Y, DstRect.Right, B), AREAINFO_RECT);
      if Assigned(fOnProgress) then
        fOnProgress(self, MulDiv(1000, P, G));
    end;
    if DoUpdate and (not UpdateRows) then
      Dst.Changed(DstRect, AREAINFO_RECT);
    if Assigned(fOnProgress) then
      fOnProgress(self, MulDiv(1000, P, G));
  end;
end;


{ TSwizzlingRasterizerEx }

procedure TSwizzlingRasterizerEx.DoRasterize(Dst: TCustomBitmap32;
  DstRect: TRect);
var
  I, L, T, W, H, Size, RowSize, D: Integer;
  P1, P2, PBlock: TPoint;
  GetSample: TGetSampleInt;
  ForwardBuffer: array of Integer;

  function GetDstCoord(P: TPoint): TPoint;
  var
    XI, YI: Integer;
  begin
    Result := P;
    Inc(Result.X);
    Inc(Result.Y);

    XI := ForwardBuffer[Result.X];
    YI := ForwardBuffer[Result.Y];

    if XI <= YI then
      Dec(Result.Y, 1 shl XI)
    else
      Dec(Result.X, 1 shl (YI + 1));

    if Result.Y >= H then
    begin
      Result.Y := P.Y + 1 shl YI;
      Result.X := P.X;
      Result := GetDstCoord(Result);
    end;

    if Result.X >= W then
    begin
      Result.X := P.X + 1 shl XI;
      Result.Y := P.Y;
      Result := GetDstCoord(Result);
    end;
  end;

begin
  W := DstRect.Right - DstRect.Left;
  H := DstRect.Bottom - DstRect.Top;
  L := DstRect.Left; T := DstRect.Top;
  Size := NextPowerOf2(Max(W, H));

  SetLength(ForwardBuffer, Size + 1);

  I := 2;
  while I <= Size do
  begin
    ForwardBuffer[I] := ForwardBuffer[I shr 1] + 1;
    Inc(I, 2);
  end;

  Size := W * H - 1;
  GetSample := Sampler.GetSampleInt;

  D := 1 shl BlockSize;
  PBlock := Point(L + D, T + D);
  P1 := Point(-1, 0);

  RowSize := Dst.Width;
  for I := 0 to Size do
  begin
    P1 := GetDstCoord(P1);
    P2.X := L + P1.X;
    P2.Y := T + P1.Y;

    AssignColor(Dst.Bits[P2.X + P2.Y * RowSize], GetSample(P2.X, P2.Y));
    if Assigned(fOnProgress) then
      fOnProgress(self, MulDiv(1000, I, Size));
    // Invalidate the current block
    if (P2.X >= PBlock.X) or (P2.Y >= PBlock.Y) then
    begin
      Dst.Changed(Rect(PBlock.X - D, PBlock.Y - D, PBlock.X, PBlock.Y));
      PBlock.X := P2.X + D;
      PBlock.Y := P2.Y + D;
    end;
  end;
  Dst.Changed(Rect(PBlock.X - D, PBlock.Y - D, PBlock.X, PBlock.Y));
end;


{ TNanoMB }


constructor TNanobrot.Create;
begin
  inherited;
  fBmp := nil;
  fJobThread := nil;
  fMbPtr := nil;
  fFindPeriodThread := nil;
  fWidth := 0;
  fHeight := 0;
  fZoomExp := 0;
  fZ := 0;
  fZlo := fcZero;
  fC.Re := 0;
  fC.Im := 0;
  fBout := fpZero;
  fRadius := fpTwo;
  fCancelled := false;
  fRecoloringMode := false;
  fInternalRecoloring := false;
  fInternalRecoloringNeed := false;
  fCalcNeeded := true;
  fExportColorMap := true;
  fSoundOnDone := true;
  fStoreLevel := true;
  fPixelScale := fpZero;
  fUsesNmbLib := false;
  fHNmbLib := 0;
  fSampler := TFractalSampler.Create(self);
  fSuperSampler := TSuperSampler.Create(fSampler);
  fRasterizerType := rtProgressive;
  UpdateRasterizer;
  fColorMap := TColorMap.Create;
  fLevel := TLevelStorage.Create;
  fLevel.ColorMap := fColorMap;
  fLevel.OnLoad := DoLoadLevelMap;
  fRef := TRef.Create;
  fRef1 := TRef.Create;
  fp := TBiPoly.Create;
  Init;
  {$IFDEF USE_NMB_LIB}
  InitLib;
  {$ENDIF}
end;

destructor TNanobrot.Destroy;
begin
  FreeAndNil(fP);
  FreeAndNil(fRef);
  FreeAndNil(fRef1);
  FreeAndNil(fSampler);
  FreeAndNil(fSuperSampler);
  FreeAndNil(fRasterizer);
  FreeAndNil(fColorMap);
  FreeAndNil(fLevel);
  {$IFDEF USE_NMB_LIB}
  UnloadLib;
  {$ENDIF}
  inherited;
end;

procedure TNanobrot.Init;
begin
  fFileName := sDefFilename;
  fModified := false;
  BeginUpdate;
  try
    CenterX := '-0.75';
    CenterY := '0';
    Zoom := '1';
    Iterations := 200;
    Period := 1;
    OrderM := 4;
    OrderN := 4;
    Jitter := 0;
    Supersample := 1;
    ColorMapFN := DEF_COLORMAP;
    ColorMapRange := 200;
    ColorMapOffset := 0;
    OutColoring := 1;
    Slopes := false;
    SlopeAngle := 45;
    SlopePower := 50;
    SlopeRatio := 50;
    UpdateSlopeCoeff;
  finally
    EndUpdate;
  end;
end;

procedure TNanobrot.InitLib;
var
  path: string;
begin
  path := ExtractFilePath(Application.ExeName) + 'nmblib.dll';
  if not FileExists(path) then
    raise EFileNotFoundException.CreateFmt('Library "%s" not found.' + #13#10 +
      'Uses native codes computing.', [path]);
  fHNmbLib := LoadLibrary(PChar(path));
  if fHNmbLib <> 0 then
  begin
    @nmb_create := GetProcAddress(fHNmbLib, PChar('nmb_create'));
    @nmb_delete := GetProcAddress(fHNmbLib, PChar('nmb_delete'));
    @nmb_calc := GetProcAddress(fHNmbLib, PChar('nmb_calc'));
    @nmb_iterate := GetProcAddress(fHNmbLib, PChar('nmb_iterate'));
    @nmb_get_level := GetProcAddress(fHNmbLib, PChar('nmb_get_level'));
    @nmb_set_re := GetProcAddress(fHNmbLib, PChar('nmb_set_re'));
    @nmb_set_im := GetProcAddress(fHNmbLib, PChar('nmb_set_im'));
    @nmb_set_zoom := GetProcAddress(fHNmbLib, PChar('nmb_set_zoom'));
    @nmb_set_bm := GetProcAddress(fHNmbLib, PChar('nmb_set_bm'));
    @nmb_set_bn := GetProcAddress(fHNmbLib, PChar('nmb_set_bn'));
    @nmb_set_maxiters := GetProcAddress(fHNmbLib, PChar('nmb_set_maxiters'));
    @nmb_set_period := GetProcAddress(fHNmbLib, PChar('nmb_set_period'));
    @nmb_find_period_setup := GetProcAddress(fHNmbLib, PChar('nmb_find_period_setup'));
    @nmb_find_period_cycle := GetProcAddress(fHNmbLib, PChar('nmb_find_period_cycle'));
    @nmb_get_radius := GetProcAddress(fHNmbLib, PChar('nmb_get_radius'));

    if (@nmb_create = nil) or
       (@nmb_delete = nil) or
       (@nmb_calc = nil) or
       (@nmb_iterate = nil) or
       (@nmb_get_level = nil) or
       (@nmb_set_re = nil) or
       (@nmb_set_im = nil) or
       (@nmb_set_zoom = nil) or
       (@nmb_set_bm = nil) or
       (@nmb_set_bn = nil) or
       (@nmb_set_maxiters = nil) or
       (@nmb_set_period = nil) or
       (@nmb_find_period_setup = nil) or
       (@nmb_find_period_cycle = nil) or
       (@nmb_get_radius = nil) then
    begin
      // if something went wrong during linking, free library & reset handle
      FreeLibrary(fHNmbLib);
      fHNmbLib := 0;
    end;
  end;
  fUsesNmbLib := fHNmbLib <> 0;
  if fUsesNmbLib then
    fMbPtr := nmb_create;
end;

procedure TNanobrot.UnloadLib;
begin
  if fUsesNmbLib then begin
    nmb_delete(fMbPtr);
    FreeLibrary(fHNmbLib);
  end;
  fMbPtr := nil;
  fHNmbLib := 0;
  fUsesNmbLib := false;
end;

procedure TNanobrot.UpdateRasterizer;
begin
  if Assigned(fRasterizer) then
    FreeAndNil(fRasterizer);
  case fRasterizerType of
    rtRegular:
    begin
      fRasterizer := TRegularRasterizerEx.Create;
      TRegularRasterizerEx(fRasterizer).OnProgress := RasterizerProgress;
    end;
    rtProgressive:
    begin
      fRasterizer := TProgressiveRasterizerEx.Create;
      TProgressiveRasterizerEx(fRasterizer).OnProgress := RasterizerProgress;
    end;
    rtSwizzling:
    begin
      fRasterizer := TSwizzlingRasterizerEx.Create;
      TSwizzlingRasterizerEx(fRasterizer).OnProgress := RasterizerProgress;
    end;
  end;

  fRasterizer.Sampler := fSuperSampler;
end;

procedure TNanobrot.RasterizerProgress(Sender: TObject; Progress: Integer);
begin
  if Assigned(fRasterizerProgress) then
    fRasterizerProgress(Sender, Progress);
end;

procedure TNanobrot.SetRasterizerType(const Value: TRasterizerType);
begin
  if (fRasterizerType = Value) or Runned then
    Exit;
  fRasterizerType := Value;
  UpdateRasterizer;
end;


procedure TNanobrot.SetSize(aWidth, aHeight: Integer);
begin
  if UpdateCount > 0 then
    Exit;
  aWidth := EnsureRange(aWidth, 10, MaxWord);
  aHeight := EnsureRange(aHeight, 10, MaxWord);
  if (fWidth = aWidth) and (fHeight = aHeight) then
    Exit;
  fWidth := aWidth;
  fHeight := aHeight;
  DoResize;
end;

procedure TNanobrot.SetCenterX(const Value: AnsiString);
begin
  if fCenterX = Value then
    Exit;
  AddComand(cidCenterX, fCenterX, Value);
  fCenterX := Value;
  fCalcNeeded := true;
  DoChande;
end;

procedure TNanobrot.SetCenterY(const Value: AnsiString);
begin
  if fCenterY = Value then
    Exit;
  AddComand(cidCenterY, fCenterY, Value);
  fCenterY := Value;
  fCalcNeeded := true;
  DoChande;
end;

procedure TNanobrot.SetZoom(const Value: AnsiString);
begin
  if fZoom = Value then
    Exit;
  AddComand(cidZoom, fZoom, Value);
  fZoom := Value;
  fZoomExp := GetExponentOfNumber(Value);
  fCalcNeeded := true;
  UpdateSlopeCoeff;
  DoChande;
end;

procedure TNanobrot.SetIterations(Value: Integer);
begin
  if fIterations = Value then
    Exit;
  AddComand(cidIterations, fIterations, Value);
  fIterations := Value;
  fLevel.MaxIters := Value;
  DoChande;
end;

procedure TNanobrot.SetPeriod(Value: Integer);
begin
  if fPeriod = Value then
    Exit;
  AddComand(cidPeriod, fPeriod, Value);
  fPeriod := Value;
  fCalcNeeded := true;
  DoChande;
end;

procedure TNanobrot.SetOrderM(Value: Integer);
begin
  Value := EnsureRange(Value, 1, MAXCHAR);
  if fOrderM = Value then
    Exit;
  AddComand(cidOrderM, fOrderM, Value);
  fOrderM := Value;
  fCalcNeeded := true;
  DoChande;
end;

procedure TNanobrot.SetOrderN(Value: Integer);
begin
  Value := EnsureRange(Value, 1, MAXCHAR);
  if fOrderN = Value then
    Exit;
  AddComand(cidOrderN, fOrderN, Value);
  fOrderN := Value;
  fCalcNeeded := true;
  DoChande;
end;

procedure TNanobrot.SetJitter(Value: Integer);
begin
  Value := EnsureRange(Value, 0, Ord(High(TJitterType)));
  if fJitter = Value then
    Exit;
  AddComand(cidJitter, fJitter, Value);
  fJitter := Value;
  DoChande;
end;

procedure TNanobrot.SetSupersample(Value: Integer);
begin
  Value := EnsureRange(Value, 1, 16);
  if fSupersample = Value then
    Exit;
  AddComand(cidSupersample, fSupersample, Value);
  fSupersample := Value;
  UpdateSupersample;
  DoChande;
end;

function TNanobrot.GetUndoLimit: Integer;
begin
  result := fHistory.UndoLimit;
end;

procedure TNanobrot.SetUndoLimit(const Value: Integer);
begin
  fHistory.UndoLimit := Value;
end;

procedure TNanobrot.SetOutColoring(Value: Integer);
begin
  Value := EnsureRange(Value, 0, Ord(High(TOutColoring)));
  if fOutColoring = Value then
    Exit;
  AddComand(cidOutColoring, fOutColoring, Value);
  fOutColoring := Value;
  DoChande;
  if fRecoloringMode then
    Recoloring;
end;

procedure TNanobrot.SetColorMapFn(const Value: string);
begin
  if fColorMapFn = Value then
    Exit;
  AddComand(cidColorMapFn, fColorMapFn, Value);
  fColorMapFn := Value;
  DoColorMapRequest;
  DoChande;
  if fRecoloringMode then
    Recoloring;
end;

procedure TNanobrot.SetMapRange(Value: Double);
begin
  Value := EnsureRange(Value, 1, MaxSingle);
  if fColorMap.Range = Value then
    Exit;
  AddComand(cidColorMapRange, fColorMap.Range, Value);
  fColorMap.Range := Value;
  DoChande;
  if fRecoloringMode then
    Recoloring;
end;

procedure TNanobrot.SetMapOffset(Value: Double);
begin
  Value := EnsureRange(Value, -2147483648, 2147483647);
  if fColorMap.Offset = Value then
    Exit;
  AddComand(cidColorMapOffset, fColorMap.Offset, Value);
  fColorMap.Offset := Value;
  DoChande;
  if fRecoloringMode then
    Recoloring;
end;

function TNanobrot.GetColorMapOffset: Double;
begin
  result := fColorMap.Offset;
end;

function TNanobrot.GetColorMapRange: Double;
begin
  result := fColorMap.Range;
end;

procedure TNanobrot.SetSlopes(Value: Boolean);
begin
  if fSlopes = Value then
    Exit;
  AddComand(cidSlopes, fSlopes, Value);
  fSlopes := Value;
  UpdateSlopeCoeff;
  DoChande;
  if fRecoloringMode then
    Recoloring;
end;

procedure TNanobrot.SetSlopeAngle(Value: Single);
begin
  Value := EnsureRange(Value, -360, 360);
  if Value = fSlopeAngle then
    Exit;
  AddComand(cidSlopeAngle, fSlopeAngle, Value);
  fSlopeAngle := Value;
  UpdateSlopeAngle;
  DoChande;
  if fRecoloringMode then
    Recoloring;
end;

procedure TNanobrot.SetSlopePower(Value: Single);
begin
  Value := EnsureRange(Value, 1, 100);
  if Value = fSlopePower then
    Exit;
  AddComand(cidSlopePower, fSlopePower, Value);
  fSlopePower := Value;
  UpdateSlopeCoeff;
  DoChande;
  if fRecoloringMode then
    Recoloring;
end;

procedure TNanobrot.SetSlopeRatio(Value: Single);
begin
  Value := EnsureRange(Value, 0, 100);
  if Value = fSlopeAngle then
    Exit;
  AddComand(cidSlopeRatio, fSlopeRatio, Value);
  fSlopeRatio := Value;
  DoChande;
  if fRecoloringMode then
    Recoloring;
end;

procedure TNanobrot.SetRecoloringMode(Value: Boolean);
begin
  fRecoloringMode := Value;
end;

procedure TNanobrot.UpdateSlopeAngle;
var
  a: Double;
begin
  a := DegToRad(fSlopeAngle); // image is flipped on Y-coordinate
  SinCos(a, fSlopeY, fSlopeX);
end;

procedure TNanobrot.UpdateSlopeCoeff;
begin
  fSlopeCoeff := fSlopePower * (fZoomExp * 1.75 + 1) * (fWidth / 640);
end;

procedure TNanobrot.UpdateSupersample;
begin
  fSupersampleRec := 1 / fSupersample;
  fSuperSampler.SamplingX := fSupersample;
  fSuperSampler.SamplingY := fSupersample;
end;

procedure TNanobrot.DoLoad;
begin
  if Assigned(fOnLoad) then
    fOnLoad(self);
end;

procedure TNanobrot.DoLoadLevelMap(Sender: TObject);
begin
  BeginUpdate;
  try
    fRecoloringMode := true;
    fWidth := Level.Width;
    fHeight := Level.Height;
    fSupersample := Level.Supersample;
    UpdateSupersample;
    if Assigned(fOnResize) then
      fOnResize(self);
  finally
    EndUpdate;
  end;
end;

procedure TNanobrot.DoChande;
begin
  fModified := true;
  Changed;
end;

procedure TNanobrot.DoColormapRequest;
begin
  if Assigned(fOnColorMapRequest) then
    fOnColorMapRequest(fColorMap, fColorMapFn);
end;

procedure TNanobrot.DoExecuteComand(const aCmdID: TComandID;
  const aData: Variant);
begin
  BeginUpdate;
  try
    case aCmdId of
      cidCenterX: CenterX := aData;
      cidCenterY: CenterY := aData;
      cidZoom: Zoom := aData;
      cidIterations: Iterations := aData;
      cidPeriod: Period := aData;
      cidOrderM: OrderM := aData;
      cidOrderN: OrderN := aData;
      cidJitter: Jitter := aData;
      cidSupersample: Supersample := aData;
      cidOutColoring: OutColoring := aData;
      cidColorMapFn: ColorMapFn := aData;
      cidColorMapRange: ColorMapRange := aData;
      cidColorMapOffset: ColorMapOffset := aData;
      cidSlopes: Slopes := aData;
      cidSlopeAngle: SlopeAngle := aData;
      cidSlopePower: SlopePower := aData;
      cidSlopeRatio: SlopeRatio := aData;
    end;
    DoLoad;
  finally
    EndUpdate;
  end;
  if RecoloringMode and (aCmdId in [cidOutColoring..cidSlopeRatio]) then
    Recoloring;
end;

procedure TNanobrot.DoReset;
begin
  fModified := false;
  Changed;
end;

procedure TNanobrot.DoMessage(const aStr: string);
begin
  if Assigned(fOnMessage) then
    fOnMessage(self, aStr);
end;

procedure TNanobrot.DoProgress(Stage: TProgressStage; Progress: Integer;
  const Msg: string);
begin
  if Assigned(fOnProgress) then
    fOnProgress(self, Stage, Progress, Msg);
end;

procedure TNanobrot.DoResize;
begin
  BeginUpdate;
  try
    fLevel.SetParams(fWidth, fHeight, fSupersample);
    if Assigned(fOnResize) then
      fOnResize(self);
  finally
    EndUpdate;
  end;
end;

procedure TNanobrot.Clear;
begin
  fLevel.Clear;
  fRef1.Clear;
  fRef.Clear;
  fBmp.Clear;
  fCalcNeeded := true;
  DoReset;
end;

procedure TNanobrot.LoadDefault;
begin
  BeginUpdate;
  try
    fHistory.DeleteAllHistoryStates;
    Init;
    DoLoad;
  finally
    EndUpdate;
  end;
  if Assigned(fOnColorMapRequest) then
    fOnColorMapRequest(fColorMap, fColorMapFn);
  Clear;
end;

procedure TNanobrot.RevertToSaved;
begin
  LoadFromFile(fFileName);
end;

procedure TNanobrot.LoadFromFile(const aFileName: string);
var
  f: TIniFileEx;
  LevelMapsFileName: string;
  maploaded: Boolean;
begin
  maploaded := false;
  BeginUpdate;
  try
    fHistory.DeleteAllHistoryStates;
    Clear;
    f := TIniFileEx.Create(aFileName);
    try
      CenterX := f.ReadString(sFractalSettings, 'CenterX', '0');
      CenterY := f.ReadString(sFractalSettings, 'CenterY', '0');
      Zoom := f.ReadString(sFractalSettings, 'Zoom', '1');
      Iterations := f.ReadInteger(sFractalSettings, 'Iterations', 200);
      Period := f.ReadInteger(sFractalSettings, 'Period', 2500);
      OrderM := f.ReadInteger(sFractalSettings, 'OrderM', 4);
      OrderN := f.ReadInteger(sFractalSettings, 'OrderN', 4);
      Jitter := f.ReadInteger(sFractalSettings, 'Jitter', 0);
      Supersample := f.ReadInteger(sRenderSettings, 'Supersample', 2);
      OutColoring := f.ReadInteger(sRenderSettings, 'OutColoring', 0);
      ColorMapFn := f.ReadString(sRenderSettings, 'ColorMapFileName', DEF_COLORMAP);
      ColorMapRange := f.ReadFloat(sRenderSettings, 'ColorMapRange', 100);
      ColorMapOffset := f.ReadFloat(sRenderSettings, 'ColorMapOffset', 0);
      Slopes := f.ReadBool(sRenderSettings, 'Slopes', false);
      SlopeAngle := f.ReadFloat(sRenderSettings, 'SlopeAngle', 45);
      SlopePower := f.ReadFloat(sRenderSettings, 'SlopePower', 50);
      SlopeRatio := f.ReadFloat(sRenderSettings, 'SlopeRatio', 50);
      LevelMapsFileName := f.ReadString(sLevelmaps ,'FileName', '');
      if Length(LevelMapsFileName) > 0 then
        LevelMapsFileName := ExtractFilePath(aFileName) + LevelMapsFileName;
    finally
      f.Free;
    end;
    fFileName := aFileName;
    DoLoad;
  finally
    EndUpdate;
  end;
  if FileExists(LevelMapsFileName) then
    LoadLevelMaps(LevelMapsFileName);
  DoReset;
end;

procedure TNanobrot.LoadLevelMapsTerminate(Sender: TObject);
begin
  if Sender = fJobThread then
    fJobThread := nil;
  if fCancelled then
  begin
    if Assigned(fOnWork) then
      fOnWork(self);
    Exit;
  end;
  fRecoloringMode := true;
  Recoloring;
  if TLoadLevelMapsThread(Sender).Import and (fFileName = sDefFilename) then
    fFileName := ChangeFileExt(TLoadLevelMapsThread(Sender).LevelMapsFileName, '.nbr');
end;

procedure TNanobrot.LoadLevelMaps(const aFileName: string; import: Boolean);
begin
  fCancelled := false;
  fJobThread := TLoadLevelMapsThread.Create(Self);
  TLoadLevelMapsThread(fJobThread).LevelMapsFileName := aFileName;
  TLoadLevelMapsThread(fJobThread).Import := import;
  fJobThread.OnTerminate := LoadLevelMapsTerminate;
  fJobThread.Start;
  if Assigned(fOnWork) then
    fOnWork(self);
end;

procedure TNanobrot.SaveLevelMaps(const aFileName: string);
begin
  fCancelled := false;
  fJobThread := TSaveLevelMapsThread.Create(Self);
  TSaveLevelMapsThread(fJobThread).LevelMapsFileName := aFileName;
  fJobThread.OnTerminate := JobTerminate;
  fJobThread.Start;
  if Assigned(fOnWork) then
    fOnWork(self);
end;

procedure TNanobrot.SaveToFile(const aFileName: string);
var
  f: TIniFileEx;
  WriteLevelMaps: Boolean;
  LevelMapsFileName: string;
begin
  WriteLevelMaps := fLevel.AllowSaveLevelMaps and not Cancelled;
  f := TIniFileEx.Create(aFileName);
  try
    f.WriteString(sFractalSettings, 'CenterX', fCenterX);
    f.WriteString(sFractalSettings, 'CenterY', fCenterY);
    f.WriteString(sFractalSettings, 'Zoom', fZoom);
    f.WriteInteger(sFractalSettings, 'Iterations', fIterations);
    f.WriteInteger(sFractalSettings, 'Period', fPeriod);
    f.WriteInteger(sFractalSettings, 'OrderM', fOrderM);
    f.WriteInteger(sFractalSettings, 'OrderN', fOrderN);
    f.WriteInteger(sFractalSettings, 'Jitter', fJitter);
    f.WriteInteger(sRenderSettings, 'Supersample', fSupersample);
    f.WriteInteger(sRenderSettings, 'OutColoring', fOutColoring);
    f.WriteString(sRenderSettings, 'ColorMapFileName', fColorMapFn);
    f.WriteFloat(sRenderSettings, 'ColorMapRange', fColorMap.Range);
    f.WriteFloat(sRenderSettings, 'ColorMapOffset', fColorMap.Offset);
    f.WriteBool(sRenderSettings, 'Slopes', fSlopes);
    f.WriteFloat(sRenderSettings, 'SlopeAngle', fSlopeAngle);
    f.WriteFloat(sRenderSettings, 'SlopePower', fSlopePower);
    f.WriteFloat(sRenderSettings, 'SlopeRatio', fSlopeRatio);
    if WriteLevelMaps then
    begin
      LevelMapsFileName := ChangeFileExt(aFileName, '.nbm');
      if AccessFileOnWrite(LevelMapsFileName) then
        f.WriteString(sLevelmaps, 'FileName', ExtractFileName(LevelMapsFileName))
      else
        WriteLevelMaps := false;
    end;
    if not WriteLevelMaps then
      f.EraseSection(sLevelmaps);
    f.UpdateFile;
  finally
    f.Free;
  end;
  if WriteLevelMaps then
  begin
    fTmpIncludeLevelMaps := fLevel.IncludeLevelMaps;
    fTmpCompression := fLevel.Compression;
    SaveLevelMaps(LevelMapsFileName);
  end;
  fFileName := aFileName;
  DoReset;
end;

function TNanobrot.Runned: Boolean;
begin
  Runned := Assigned(fJobThread);
end;

procedure TNanobrot.PreCalculate;
var
  z: TCDecNumber;
begin
  // Check on zero
  z := fZoom;
  if z.IsZero then begin
    MessageDlg('Zoom value can''t be a zero!', mtError, [mbOK], 0);
    Exit;
  end;
  if not fUsesNmbLib then
  begin
    fZoomExp := z.GetExponent;
    z := 2 / z;
    fRadius := z;
  end;
  fC.Create(fCenterX, fCenterY);
end;

procedure TNanobrot.Start;
begin
  if Runned then
    exit;
  Assert(fBmp <> nil);
  if Period <=0 then
    raise ENanobrot.Create('Period can''t be zero or less!');
  PreCalculate;
  fOldRecoloringMode := fRecoloringMode;
  fRecoloringMode := false;
  fInternalRecoloring := false;
  fInternalRecoloringNeed := fSlopes or (fOutColoring > Ord(ocStretched));
  fCancelled := false;
  fZ := 0;
  fZlo := fcZero;

  fJobThread := TRenderThread.Create(Self);
  fJobThread.OnTerminate := JobTerminate;
  fJobThread.Start;
  if Assigned(fOnWork) then
    fOnWork(self);
end;

procedure TNanobrot.Stop;
begin
  if not Runned then
    Exit;
  fJobThread.Terminate;
  fCancelled := true;
end;


procedure TNanobrot.InternalRecoloring;
begin
  fCancelled := false;
  fJobThread := TRecoloringThread.Create(Self);
  fJobThread.OnTerminate := JobTerminate;
  fJobThread.Start;
end;

procedure TNanobrot.Recoloring;
begin
  if Runned or (fLevel.Empty) or (not fRecoloringMode) or (UpdateCount > 0) then
    exit;
  fRecoloringMode := true;
  InternalRecoloring;
  if Assigned(fOnRecoloring) then
    fOnRecoloring(self);
end;

procedure TNanobrot.JobTerminate(Sender: TObject);
begin
  if Sender = fJobThread then
    fJobThread := nil;
  if Assigned(fOnWork) then
    fOnWork(self);

  if Sender is TRenderThread then
  begin
    fRecoloringMode := fOldRecoloringMode;
    if not fCancelled then
    begin
      if fSoundOnDone and not fInternalRecoloringNeed then
        PlayDoneSound;
      if fInternalRecoloringNeed then
      begin
        fInternalRecoloring := true;
        InternalRecoloring;
      end;
    end;
  end
  else
  if Sender is TRecoloringThread then
  begin
    if fSoundOnDone and fInternalRecoloringNeed then
      PlayDoneSound;
    fInternalRecoloring := false;
    fInternalRecoloringNeed := false;
  end
  else
  if Sender is TSaveLevelMapsThread then
  begin
    fLevel.IncludeLevelMaps := fTmpIncludeLevelMaps;
    fLevel.Compression := fTmpCompression;
  end;
end;

function TNanobrot.Perturbation(d0, d, dd: TFloatExpComplex; N: Integer): boolean;
var
  i: Integer;
  r, v: Double;
  zn: TFloatExpComplex;
begin
  result := false;
  for i := N to fIterations - 1 do begin
    if fJobThread.Terminated then
      Abort;
		zn := fRef1[i];
		dd := fcTwo * dd * (d + zn) + fcOne;
		d := d * (fcTwo * zn + d) + d0;
    r := (zn + d).Norm;
    if r > 1e10 then
    begin
      v := i + 1 - ln(ln(r)) * dLn2Rec;
      fLevel.Iters := Trunc(v);
      fLevel.Trans := 1 - Frac(v);
			Exit(true);
		end;
  end;
end;

function TNanobrot.IteratePt(const d0: TFloatExpComplex): Boolean;
var
  i: Integer;
  d, dd: TFloatExpComplex;
begin
  i := 0;
  d := fcZero;
  dd := fcZero;
  if d0.Abs < fBout then begin
		while (i < fIterations) and (d.Norm < fBout) do begin
			dd := dd * fP.EvalDz(d, d0) + fP.EvalDc(d, d0);
			d := fP.Eval(d, d0);
      Inc(i, fPeriod);
		end;
	end
  else
    dd := fcOne;
	if i > fIterations then
		Exit(false);
  result := Perturbation(d0, d, dd, i);
end;

function TNanobrot.IteratePtDE2(const d0: TFloatExpComplex): Boolean;
var
  i, n, si, step: Integer;
  d, dd, w, w0, dw,
  w0_next, delta, d0_: TFloatExpComplex;
  d_norm, de: TFloatExp;
  up: TUniPoly;
  converged: Boolean;
begin
  i := 0;
  d := 0;
  dd := 0;
  si := 0;
  d_norm := dInfinity;
  if d0.Abs < fBout then begin
    up := TUniPoly.Create(fP, d0);
    while (i < fIterations) and (d.Norm < fBout) do begin
      up.eval(d, dd);
			Inc(i, period);
      Inc(si);
			if d.Norm < d_norm then begin
        w0 := d;
        converged := false;
        for step := 0 to 15 do begin
					w := w0;
					dw := fcOne;
					for n := 0 to si - 1 do
						up.EvalDz(w, dw);

					w0_next := w0 - (w - w0) / (dw - fcOne);
					delta := (w0_next - w0);
					w0 := w0_next;
          if (delta.Re.Abs < w0.Re.Abs * fpTenM6)
          and (delta.Im.Abs < w0.Im.Abs * fpTenM6) then begin
            converged := true;
						break;
          end;
				end;
        if converged and (dw.Norm < fpOne) then begin
					// is interior, calculate interior DE
          fLevel.Iters := fIterations;
          fLevel.Trans := 0;
			    Exit(false);
				end;
      end;
    end;
  end;
  if i > fIterations then
		Exit(false);
  d0_ := d0 - fNucleusPos;
  d := d - fRef1[i];
  result := Perturbation(d0_, d, dd, i);
end;

function TNanobrot.ApplySlopes(C: TColor32; sX, sY: TFloat): TColor32;
var
  diff, d1: Double;
  s: TColor32Entry absolute C;
begin
  diff := fLevel.GetSlope(sX, sY, fSlopeX, fSlopeY);
  diff := Power(diff, fSlopeCoeff);
  if diff > 1 then
  begin
		diff := (ArcTan(diff) - dPiD4) / dPiD4;
		diff := diff * fSlopeRatio * 0.01;
    d1 := 1 - diff;
		s.R := Clamp(Round(d1 * s.R));
		s.G := Clamp(Round(d1 * s.G));
		s.B := Clamp(Round(d1 * s.B));
	end else begin
    diff := IfThen( diff = 0, MaxInt, 1 / diff);
    diff := (ArcTan(diff) - dPiD4) / dPiD4;
    diff := diff * fSlopeRatio * 0.01;
    d1 := 1 - diff;
    s.R := Clamp(Round(d1 * s.R + diff * 255));
		s.G := Clamp(Round(d1 * s.G + diff * 255));
		s.B := Clamp(Round(d1 * s.B + diff * 255));
	end;
  result := s.ARGB;
end;


procedure TNanobrot.CalcSample(X, Y: TFloat);
var
  cX, cY: Double;
  dc: TFloatExpComplex;
  lv: TLevel;
begin
  if fRecoloringMode or fInternalRecoloring then begin
    fLevel.Restore(X, Y);
  end
  else begin
    case TJitterType(fJitter) of
      jtNone:
      begin
        cX := fWidth * (X / fWidth - 0.5) / fHeight;
        cY := Y / fHeight - 0.5;
      end;
      jtRandom:
      begin
        cX := fWidth * ((x + Random / fSupersample) / fWidth - 0.5) / fHeight;
        cY := (y + Random / fSupersample) / fHeight - 0.5;
      end;
      jtBurtleHash:
      begin
        cX := fWidth * ((x + DitherJitter(Round(x), Round(y), 0) * fSupersampleRec) / fWidth - 0.5) / fHeight;
        cY := (y + DitherJitter(Round(x), Round(y), 1) * fSupersampleRec) / fHeight - 0.5;
      end;
      jtGaussian:
      begin
        cX := X;
        cY := Y;
        GaussianJitter(fSupersampleRec, cX, cY);
        cX := fWidth * ((x + cX) / fWidth - 0.5) / fHeight;
        cY := (y + cY) / fHeight - 0.5;
      end;
    end;

    if fJobThread.Terminated then
      Exit;
    if fUsesNmbLib then
    begin
      if nmb_iterate(fMbPtr, cX, cY) then
      begin
        nmb_get_level(fMbPtr, lv);
        fLevel.Iters := lv.Iters;
        fLevel.Trans := lv.Trans;
      end
      else
        fLevel.Iters := -1
    end else begin
      dc := fDiametr * TFloatExpComplex.Create(cX, cY);
      if not IteratePtDE2(dc) then
        fLevel.Iters := -1;
    end;
    // store value to level map
    if fStoreLevel then
      fLevel.Store(X, Y);
  end;
end;


function TNanobrot.GetCurrentColor(X, Y: TFloat): TColor32;
var
  k, p: Double;
  ix, iy: Integer;
  m: TOutColoring;
begin
  if fLevel.Iters < 0 then begin
    result := fColorMap.GetLastColor;
    Exit;
  end;

  p := fLevel.Iters + (1 - fLevel.Trans);
  m := TOutColoring(fOutColoring);
  if (not fInternalRecoloring) and (m > ocStretched) then
    m := ocPotential;

  case m of
    ocIterations: k := fLevel.Iters;
    ocPotential: k := p;
    ocArcTan: k := ArcTan(p);
    ocSquareRoot: k := Sqrt(Max(0, p));
    ocCubicRoot: k := Power(Max(0, p), d1d3);
    ocLogarithm: k := Ln(Max(1, p));
    ocStretched: k := 1024 * (p + 1) / fIterations;
    ocDistanceLinear..ocDistanceEstimationLog:
    begin
      k := fLevel.GetDistance(X, Y);
      case TOutColoring(fOutColoring) of
        ocDistanceSqrt, ocDistanceEstimationSqrt: k := Sqrt(Max(0, k));
        ocDistanceLog, ocDistanceEstimationLog: k := Ln(Max(1, k + 1));
      end;

      if k > 1024 then
        k := 1024;
      if (TOutColoring(fOutColoring) in [ocDistanceEstimationLinear..ocDistanceEstimationLog])
        and (k > 1) then
          k := p;
    end;
  end;

  k := EnsureRange(k, 0, 10000000);
  result := fColorMap.GetColor(k);
  if fSlopes then
    result := ApplySlopes(result, X, Y);
end;

function TNanobrot.GetSampleFloat(X, Y: TFloat): TColor32;
begin
  CalcSample(X, Y);
  result := GetCurrentColor(X, Y);
end;

procedure TNanobrot.Assign(Source: TPersistent);
begin
  if Source is TNanobrot then begin
    BeginUpdate;
    try
      CenterX := TNanobrot(Source).CenterX;
      CenterY := TNanobrot(Source).CenterY;
      Zoom := TNanobrot(Source).Zoom;
      Iterations := TNanobrot(Source).Iterations;
      Period := TNanobrot(Source).Period;
      OrderM := TNanobrot(Source).OrderM;
      OrderN := TNanobrot(Source).OrderN;
      Jitter := TNanobrot(Source).Jitter;
      Supersample := TNanobrot(Source).Supersample;
      fSupersampleRec := TNanobrot(Source).fSupersampleRec;
      ColorMapFn := TNanobrot(Source).ColorMapFn;
      ColorMapRange := TNanobrot(Source).ColorMapRange;
      ColorMapOffset := TNanobrot(Source).ColorMapOffset;
      OutColoring := TNanobrot(Source).OutColoring;
      Slopes := TNanobrot(Source).Slopes;
      SlopeAngle := TNanobrot(Source).SlopeAngle;
      SlopePower := TNanobrot(Source).SlopePower;
      SlopeRatio := TNanobrot(Source).SlopeRatio;
      SoundOnDone := TNanobrot(Source).SoundOnDone;
      DoLoad;
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TNanobrot.Calculate;
var
  i: Integer;
  tp: TTmpPoly;
  zlo1: TFloatExpComplex;
begin
  if fStoreLevel then
    fLevel.SetParams(fWidth, fHeight, fSupersample);
  if fCalcNeeded then begin
    fJobThread.NotifyMessage('Calculate atom domain...');
    fJobThread.NotifyProgress(psStarting, 0);

    // end addition
    // prepare lib
    if fUsesNmbLib then
    begin
      nmb_set_re(fMbPtr, PAnsiChar(fCenterX));
      nmb_set_im(fMbPtr, PAnsiChar(fCenterY));
      nmb_set_zoom(fMbPtr, PAnsiChar(fZoom));
	    nmb_set_bm(fMbPtr, fOrderM);
	    nmb_set_bn(fMbPtr, fOrderN);
	    nmb_set_maxiters(fMbPtr, fIterations);
	    nmb_set_period(fMbPtr, fPeriod);
      nmb_calc(fMbPtr, fJobThread, CalcCalback);
      nmb_get_radius(fMbPtr, fRadius);
    end else begin
      fRef.Clear;
      fRef1.Clear;
      fP.SetOrder(fOrderM, fOrderN);
      for i := 0 to fPeriod - 1 do begin
        fRef.Add(fZlo);
  	    fZ := fZ * fZ + fC;
  	    fZlo := fZ;
  	    fP.CStep(fZlo);
        if fJobThread.Terminated then
          Abort;
        fJobThread.NotifyProgress(psRunning, Trunc(1000 * (i + 1)/ fPeriod));
      end;
      fBout := fP.GetRadius;
      // added in last version of NanoMB
      tp := TTmpPoly.Create(fP);
      fNucleusPos := tp.GetRoot;
      zlo1 := 0;
      for i := 0 to fPeriod - 1 do begin
        fZlo := fRef[i];
  		  fRef1.Add(fZlo + zlo1);
  		  zlo1 := zlo1 * (zlo1 + 2 * fZlo) + fNucleusPos;
      end;
    end;
    fRadRatio := fHeight / fRadius;
    fPixelScale := 60 * fRadRatio;
    fDiametr := fcTwo * TFloatExpComplex(fRadius);
    fJobThread.NotifyProgress(psEnding, 0);
    fCalcNeeded := false;
    UpdateSlopeCoeff;
  end;
end;

procedure TNanobrot.ImportLevelMap(const aFileName: string);
var
  fs: TFileStream;
  sig: array[0..2] of AnsiChar;
  w, h, s: Integer;
begin
  s := 1;
  BeginUpdate;
  try
    fHistory.DeleteAllHistoryStates;
    if SameText(ExtractFileExt(aFileName), '.nbm') then
    begin
      LoadLevelMaps(aFileName, true);
    end else begin
      fs := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyNone);
      try
        fs.Read(sig, 3);
        if sig <> 'KFB' then
          raise ENanobrot.CreateFmt('File "%s" is not Kalle''s fraktaler map file!', [aFileName]);
        fs.Read(w, 4);
        fs.Read(h, 4);
        if not (InRange(w, 1, MaxWord) and InRange(h, 1, MaxWord)) then
          raise ENanobrot.CreateFmt('Can''t import file "%s", because map sizes (%dx%d) is out of range!',
            [aFileName, w, h]);
        if ShowImportLevelMapDialog(w, h, s) then
        begin
          fWidth := w;
          fHeight := h;
          fRecoloringMode := true;
          fSupersample := s;
          UpdateSupersample;
          fLevel.SetParams(fWidth, fHeight, fSupersample);
          fLevel.ImportFromStream(fs);
        end
        else
          Exit;
      finally
        fs.Free;
      end;
    end;
    DoLoad;
    DoResize;
  finally
    EndUpdate;
  end;
  if fFileName = sDefFilename then
    fFileName := ChangeFileExt(aFileName, '.nbr');
  DoChande;
  Recoloring;
end;

procedure TNanobrot.ExportLevelMap(const aFileName: string);
var
  fs: TFileStream;
  sig: array[0..2] of AnsiChar;
begin
  if SameText(ExtractFileExt(aFileName), '.nbm') then
  begin
    fTmpIncludeLevelMaps := fLevel.IncludeLevelMaps;
    fTmpCompression := fLevel.Compression;
    fLevel.IncludeLevelMaps := true;
    fLevel.Compression := ctZlibDef;
    SaveLevelMaps(aFileName);
    Exit;
  end;
  sig := 'KFB';
  fs := TFileStream.Create(aFileName, fmCreate);
  try
    fs.Write(sig, 3);
    fLevel.ExportColorMap := fExportColorMap;
    fLevel.ExportToStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TNanobrot.ImportKallesParams(const aFileName: string);
var
  sl: TStringList;
  i: integer;
begin
  BeginUpdate;
  try
    fHistory.DeleteAllHistoryStates;
    sl := TStringList.Create;
    try
      sl.NameValueSeparator := ':';
      sl.LoadFromFile(aFileName);
      CenterX := Trim(sl.Values['Re']);
      CenterY := Trim(sl.Values['Im']);
      Zoom := Trim(sl.Values['Zoom']);
      Iterations := StrToIntDef(Trim(sl.Values['Iterations']), 200);
    finally
      sl.Free;
    end;
    DoLoad;
  finally
    EndUpdate;
  end;
  if fFileName = sDefFilename then
    fFileName := ChangeFileExt(aFileName, '.nbr');
  Clear;
  DoChande;
end;

procedure TNanobrot.ExportKallesParams(const aFileName: string);
var
  sl: TStringList;
  i: integer;
  s: string;
  rgb: TRGB;
begin
  sl := TStringList.Create;
  try
    sl.Add('Re: ' + fCenterX);
    sl.Add('Im: ' + fCenterY);
    sl.Add('Zoom: ' + fZoom);
    sl.Add('Iterations: ' + fIterations.ToString);
    if fExportColorMap then begin
      s := '';
      for i := 0 to fColorMap.ColorsCount - 1 do
      begin
        rgb := fColorMap.RGB[i];
        s := Format('%s%d,%d,%d,', [s, rgb[0], rgb[1], rgb[2]]);
      end;
      sl.Add('Colors: ' + s);
    end;
    sl.SaveToFile(aFileName);
  finally
    sl.Free;
  end;
end;

procedure TNanobrot.DoFindPeriod(const aPeriod: Integer);
begin
  SetPeriod(aPeriod);
  if Assigned(fOnFindPeriod) then
    fOnFindPeriod(self);
end;

procedure TNanobrot.FindPeriodTerminate(Sender: TObject);
begin
  fFindPeriodThread := nil;
  if Assigned(fOnPeriodProgress) then
    fOnPeriodProgress(self, psEnding, 0, '');
end;

procedure TNanobrot.DoPeriodProgress(const Current: Integer);
begin
  if Assigned(fOnPeriodProgress) then
    fOnPeriodProgress(self, psRunning, Current, '');
end;

function TNanobrot.IsPeriodSearch: Boolean;
begin
  result := Assigned(fFindPeriodThread);
end;

procedure TNanobrot.FindPeriod;
begin
  if IsPeriodSearch then
    Exit;
  PreCalculate;
  fFindPeriodThread := TFindPeriodThread.Create(self);
  with fFindPeriodThread do
  begin
    OnTerminate := FindPeriodTerminate;
    Start;
  end;
  if Assigned(fOnPeriodProgress) then
    fOnPeriodProgress(self, psStarting, 0, '');
end;

procedure TNanobrot.CancelPeriodSearch;
begin
  if not IsPeriodSearch then
    Exit;
  fFindPeriodThread.Terminate;
end;

initialization
  fpTenM6 := 1e-6;
  fpTenM1 := 0.1;
  fpZero := 0;
  fpHalf := 0.5;
  fpOne := 1;
  fpTwo := 2;
  fcZero := 0;
  fcOne := 1;
  fcTwo := 2;
  dLn2Rec := 1 / Ln(2);


end.


