unit uHelpers;

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

{$I Nanobrot.inc}

uses
  Windows, Classes, SysUtils, IniFiles, GR32, uColorMap, uLZMA, uBase;

type

  TCompressionType = (ctNone, ctZLibFast, ctZlibDef, ctZLibBest, ctLZMA);

  TIncludeLevelMaps = record
    Enabled: Boolean;
    Compression: TCompressionType;
  end;

  // 16-bit float unsigned, range 0..1
  // for reduce file size
  TShortFloat = record
  private
    fVal: Word;
  public
    class operator Implicit(const Value: Single): TShortFloat;
    class operator Implicit(const Value: TShortFloat): Single;
  end;

  PShortFloatArray = ^TShortFloatArray;
  TShortFloatArray = array[0..0] of TShortFloat;

  TTemporaryFileStream = class(THandleStream)
  public
    constructor Create(size: Int64 = 0);
    destructor Destroy; override;
  end;

  TMemoryMap = class(TCustomMap)
  protected
    fElementSize: Integer;
    fColumns: TPointerList;
    procedure ChangeSize(var Width, Height: Integer; NewWidth, NewHeight: Integer); override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure ToEmpty;
    function Empty: Boolean; override;
  end;

  TIntegerMap = class(TMemoryMap)
  private
    function GetValue(X, Y: Integer): Integer;
    procedure SetValue(X, Y: Integer; const Value: Integer);
  public
    constructor Create; overload; override;
    procedure Clear(FillValue: Integer = 0);
    property Value[X, Y: Integer]: Integer read GetValue write SetValue; default;
  end;

  TSingleMap = class(TMemoryMap)
  private
    fBuffer: PShortFloatArray;
    fBufferSize: Integer;
    function GetValue(X, Y: Integer): Single;
    procedure SetValue(X, Y: Integer; const Value: Single);
  protected
    procedure ChangeSize(var Width, Height: Integer; NewWidth, NewHeight: Integer); override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure Clear(FillValue: Single = 0);
    procedure ReadColunm(Stream: TStream; const X: Integer);
    procedure WriteColunm(Stream: TStream; const X: Integer);
    property Value[X, Y: Integer]: Single read GetValue write SetValue; default;
  end;

  TLevelStorage = class
  private
    fWidth: Integer;
    fHeight: Integer;
    fWorkWidth: Integer;
    fWorkHeight: Integer;
    fMaxIters: Integer;
    fIters: Integer;
    fTrans: Single;
    fOffset: TFixed;
    fDistance: TFixed;
    fScale: Single;
    fSupersample: Integer;
    fItersMap: TIntegerMap;
    fTransMap: TSingleMap;
    fColorMap: TColorMap;
    fClean: Boolean;
    fExportColorMap: Boolean;
    fJobThread: TObject;
    fCmprsProgressMax: Int64;
    fCmprsStream: TStream;
    fIncludeLevelMaps: Boolean;
    fCompression: TCompressionType;
    fOnLoad: TNotifyEvent;
    procedure UpdateParams;
    procedure RealToMap(X, Y: Single; var iX, iY: Integer); {$IFDEF DOINLINE} inline; {$ENDIF}
    procedure LZMAProgress(const Action: TLZMAProgressAction; const Value: int64);
    procedure ZLibProgress(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Store(X, Y: TFloat);  {$IFDEF DOINLINE} inline; {$ENDIF}
    procedure Restore(X, Y: TFloat); {$IFDEF DOINLINE} inline; {$ENDIF}
    procedure SetParams(NewWidth, NewHeight, NewSuperSample: Integer);
    procedure ImportFromStream(Stream: TStream);
    procedure ExportToStream(Stream: TStream);
    procedure SaveToFile(const FileName: string; JobThread: TObject);
    function LoadFromFile(const FileName: string; JobThread: TObject): Boolean;
    function GetSlope(sX, sY: TFloat; const SlopeX, SlopeY: Double): Double; {$IFDEF DOINLINE} inline; {$ENDIF}
    function GetDistance(sX, sY: TFloat): Double; {$IFDEF DOINLINE} inline; {$ENDIF}
    function AllowSaveLevelMaps: Boolean;
    function Empty: Boolean;
    property Clean: Boolean read fClean;
    property ColorMap: TColorMap read fColorMap write fColorMap;
  published
    property Width: Integer read fWidth;
    property Height: Integer read fHeight;
    property Supersample: Integer read fSupersample;
    property MaxIters: Integer read fMaxIters write fMaxIters;
    property Iters: Integer read fIters write fIters;
    property Trans: Single read fTrans write fTrans;
    property ExportColorMap: Boolean read fExportColorMap write fExportColorMap;
    property IncludeLevelMaps: Boolean read fIncludeLevelMaps write fIncludeLevelMaps;
    property Compression: TCompressionType read fCompression write fCompression;
    property OnLoad: TNotifyEvent read fOnLoad write fOnLoad;
  end;

  TLevelFileHeader = packed record
    ID: array[0..3] of AnsiChar;
    Version: Word;
    Width: Integer;
    Height: Integer;
    Supersample: Integer;
    Flags: Cardinal;
  end;

  TReorderSectionsProc = procedure(List: TStringList) of object;

  TIniFileEx = class(TMemIniFile)
  private
    fIndex1: Integer;
    fIndex2: Integer;
    procedure SortSectionsProc(List: TStringList);
    procedure ExchangeSectionsProc(List: TStringList);
    procedure ReorderSections(proc: TReorderSectionsProc);
  public
    procedure RenameSection(const OldName, NewName: string);
    procedure ExchangeSections(Index1, Index2: Integer);
    procedure SortSections;
    function SectionIndex(const aName: string): Integer;
    procedure WriteBool(const Section, Ident: string; Value: Boolean); override;
    function ReadBool(const Section, Ident: string; Default: Boolean): Boolean; override;
  end;

implementation

uses
  Math, GR32_LowLevel, ZLib, uNanobrot;

const
  MAXWORDREC = 1 / MAXWORD;

{ TShortFloat }

class operator TShortFloat.Implicit(const Value: Single): TShortFloat;
begin
  Result.fVal := Round(EnsureRange(Value, 0, 1) * MAXWORD);
end;

class operator TShortFloat.Implicit(const Value: TShortFloat): Single;
begin
  result := MAXWORDREC * Value.fVal;
end;

{ TTemporaryFileStream }

constructor TTemporaryFileStream.Create(size: Int64);
var
  TempPath: array[0..MAX_PATH] of Char;
  TempFile: array[0..MAX_PATH] of Char;
  TempHandle: THandle;
  dwLow: Integer;
  dwHigh: Integer;
begin
  dwLow := LongWord(Size);
  dwHigh := ((Size shr 32) and $FFFFFFFF);
  GetTempPath(High(TempPath), TempPath);
  GetTempFileName(TempPath, '~NB_', 0, TempFile);
  TempHandle:=CreateFile(TempFile, GENERIC_READ or GENERIC_WRITE, 0, nil,
    CREATE_ALWAYS, FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_RANDOM_ACCESS or
    FILE_FLAG_DELETE_ON_CLOSE, 0);
  Assert(TempHandle <> INVALID_HANDLE_VALUE, 'Unable to create temporary file');
  // set predefined size
  if (Size > 0) and (SetFilePointer(TempHandle, dwLow, @dwHigh, FILE_BEGIN) = LongWord(Size)) then
    SetEndOfFile(TempHandle);
  inherited Create(TempHandle);
end;

destructor TTemporaryFileStream.Destroy;
begin
  FileClose(Handle);
  inherited Destroy;
end;

{ TMemoryMap }

constructor TMemoryMap.Create;
begin
  fColumns := nil;
  fElementSize := 0;
  inherited Create;
end;

destructor TMemoryMap.Destroy;
begin
  ToEmpty;
  inherited;
end;

procedure TMemoryMap.ChangeSize(var Width, Height: Integer; NewWidth,
  NewHeight: Integer);
var
  x: Integer;
begin
  SetLength(fColumns, NewWidth);
  for x := 0 to NewWidth - 1 do
    ReallocMem(fColumns[x], NewHeight * fElementSize);
  inherited;
end;

procedure TMemoryMap.ToEmpty;
var
  x: Integer;
begin
  for x := 0 to Width - 1 do
    FreeMem(fColumns[x]);
  fColumns := nil;
end;

function TMemoryMap.Empty: Boolean;
begin
  Result := Length(fColumns) = 0;
end;


{ TIntegerMap }

constructor TIntegerMap.Create;
begin
  inherited Create;
  fElementSize := sizeof(Integer);
end;

procedure TIntegerMap.Clear(FillValue: Integer);
var
  x: Integer;
begin
  for x := 0 to Width - 1 do
    FillLongword(fColumns[x]^, Height, FillValue);
  Changed;
end;

function TIntegerMap.GetValue(X, Y: Integer): Integer;
begin
  Result := PIntegerArray(fColumns[x])^[Y];
end;

procedure TIntegerMap.SetValue(X, Y: Integer; const Value: Integer);
begin
  PIntegerArray(fColumns[x])^[Y] := Value;
end;


{ TSingleMap }

constructor TSingleMap.Create;
begin
  inherited Create;
  fBuffer := nil;
  fBufferSize := 0;
  fElementSize := sizeof(Single);
end;

destructor TSingleMap.Destroy;
begin
  if Assigned(fBuffer) then
    FreeMem(fBuffer);
  inherited;
end;

procedure TSingleMap.ChangeSize(var Width, Height: Integer; NewWidth,
  NewHeight: Integer);
begin
  inherited;
  fBufferSize := NewHeight * sizeof(TShortFloat);
  ReallocMem(fBuffer, fBufferSize);
end;

procedure TSingleMap.Clear(FillValue: Single);
var
  x, y: Integer;
begin
  for x := 0 to Width - 1 do
    for y := 0 to Height - 1 do
      PSingleArray(fColumns[x])^[Y] := FillValue;
end;

function TSingleMap.GetValue(X, Y: Integer): Single;
begin
  Result := PSingleArray(fColumns[x])^[Y];
end;

procedure TSingleMap.SetValue(X, Y: Integer; const Value: Single);
begin
  PSingleArray(fColumns[x])^[Y] := Value;
end;

procedure TSingleMap.ReadColunm(Stream: TStream; const X: Integer);
var
  Y: Integer;
begin
  Stream.Read(fBuffer^, fBufferSize);
  for Y := 0 to Height - 1 do
    PSingleArray(fColumns[X])^[Y] := fBuffer^[Y];
end;

procedure TSingleMap.WriteColunm(Stream: TStream; const X: Integer);
var
  Y: Integer;
begin
  for Y := 0 to Height - 1 do
    fBuffer^[Y] := PSingleArray(fColumns[X])^[Y];
  Stream.Write(fBuffer^, fBufferSize);
end;

{ TLevelStorage }

constructor TLevelStorage.Create;
begin
  fJobThread := nil;
  fWorkWidth := 0;
  fWorkHeight := 0;
  fMaxIters := 0;
  fExportColorMap := true;
  fIncludeLevelMaps := false;
  fCompression := ctNone;
  fClean := true;
  fItersMap := TIntegerMap.Create;
  fTransMap := TSingleMap.Create;
end;

destructor TLevelStorage.Destroy;
begin
  FreeAndNil(fItersMap);
  FreeAndNil(fTransMap);
  inherited;
end;

function TLevelStorage.Empty: Boolean;
begin
  result := fItersMap.Empty or fTransMap.Empty;
end;

function TLevelStorage.AllowSaveLevelMaps: Boolean;
begin
  result := fIncludeLevelMaps and not fClean;
end;

procedure TLevelStorage.Clear;
begin
  fItersMap.Clear;
  fTransMap.Clear;
  fClean := true;
end;

procedure TLevelStorage.UpdateParams;
begin
  fWorkWidth := fSupersample * fWidth;
  fWorkHeight := fSupersample * fHeight;
  fItersMap.SetSize(fWorkWidth, fWorkHeight);
  fTransMap.SetSize(fWorkWidth, fWorkHeight);
  fDistance := Fixed(1 / fSupersample);
  fOffset := Fixed(((1 / fSupersample) - 1) * 0.5);
  fScale := FixedToFloat * fSupersample;
end;

procedure TLevelStorage.SetParams(NewWidth, NewHeight, NewSuperSample: Integer);
begin
  if (fWidth = NewWidth) and (fHeight = NewHeight)
    and (fSupersample = NewSuperSample) then
    Exit;
  fWidth := NewWidth;
  fHeight := NewHeight;
  fSupersample := NewSuperSample;
  Clear;
  UpdateParams;
end;

procedure TLevelStorage.RealToMap(X, Y: Single; var iX, iY: Integer);
var
  p: TFixedPoint;
begin
  p := FixedPoint(X, Y);
  iX := EnsureRange(Round((p.X - fOffset) * fScale), 0, fWorkWidth - 1);
  iY := EnsureRange(Round((p.Y - fOffset) * fScale), 0, fWorkHeight - 1);
end;

procedure TLevelStorage.Store(X, Y: TFloat);
var
  ix, iy: Integer;
begin
  RealToMap(X, Y, ix, iy);
  //Assert(InRange(ix, 0, fWorkWidth - 1) and InRange(iy, 0, fWorkHeight - 1));
  fItersMap[ix, iy] := fIters;
  fTransMap[ix, iy] := fTrans;
  fClean := false;
end;

procedure TLevelStorage.Restore(X, Y: TFloat);
var
  ix, iy: Integer;
begin
  RealToMap(X, Y, ix, iy);
  //Assert(InRange(ix, 0, fWorkWidth - 1) and InRange(iy, 0, fWorkHeight - 1));
  fIters := fItersMap[ix, iy];
  fTrans := fTransMap[ix, iy];
end;

function TLevelStorage.GetSlope(sX, sY: TFloat; const SlopeX, SlopeY: Double): Double;
var
  x, y: Integer;
  p1, p2, diffx, diffy: Double;
begin
  RealToMap(sX, sY, x, y);
  if x > 0 then
  begin
		p1 := fItersMap[x - 1, y] + 1 - fTransMap[x - 1, y];
		p2 := fItersMap[x, y] + 1 - fTransMap[x, y];
	end
	else
  if x < fWorkWidth - 1 then
  begin
    p1 := fItersMap[x, y] + 1 - fTransMap[x, y];
    p2 := fItersMap[x + 1, y] + 1 - fTransMap[x + 1, y];
	end else begin
    p1 := fItersMap[x, y] + 1 - fTransMap[x, y];
    p2 := p1;
  end;
  diffx := p1 - p2;
  if y > 0 then
  begin
    p1 := fItersMap[x, y - 1] + 1 - fTransMap[x, y - 1];
    p2 := fItersMap[x, y] + 1 - fTransMap[x, y];
	end
  else
  if y < fWorkHeight - 1 then
  begin
    p1 := fItersMap[x, y] + 1 - fTransMap[x, y];
    p2 := fItersMap[x, y + 1] + 1 - fTransMap[x, y + 1];
	end else begin
    p1 := fItersMap[x, y] + 1 - fTransMap[x, y];
    p2 := p1;
  end;
	diffy := p1 - p2;
	result := Max(0, diffx * SlopeX + diffy * SlopeY);
	p1 := Max(1, fItersMap[x, y] + 1 - fTransMap[x, y]);
	result := (p1 + result) / p1;
end;

function TLevelStorage.GetDistance(sX, sY: TFloat): Double;
var
  x, y: Integer;
  p1, p2, a: Double;
begin
  result := 0;
  RealToMap(sX, sY, x, y);
  if x > 0 then
  begin
		p1 := fItersMap[x - 1, y] + 1 - fTransMap[x - 1, y];
		p2 := fItersMap[x, y] + 1 - fTransMap[x, y];
	end
	else
  if x < fWorkWidth - 1 then
  begin
    p1 := fItersMap[x, y] + 1 - fTransMap[x, y];
    p2 := fItersMap[x + 1, y] + 1 - fTransMap[x + 1, y];
	end else begin
    p1 := fItersMap[x, y] + 1 - fTransMap[x, y];
    p2 := p1;
  end;
  result := result + abs(p1 - p2) * 1.414;
  if (x > 0) and (y > 0) then
  begin
    p1 := fItersMap[x - 1, y - 1] + 1 - fTransMap[x - 1, y - 1];
    p2 := fItersMap[x, y] + 1 - fTransMap[x, y];
	end
  else
  if (x < fWorkWidth - 1) and (y < fWorkHeight - 1) then
  begin
    p1 := fItersMap[x, y] + 1 - fTransMap[x, y];
    p2 := fItersMap[x + 1, y + 1] + 1 - fTransMap[x + 1, y + 1];
	end else begin
    p1 := fItersMap[x, y] + 1 - fTransMap[x, y];
    p2 := p1;
  end;
  result := result + abs(p1 - p2);
	if y > 0 then
  begin
    p1 := fItersMap[x, y - 1] + 1 - fTransMap[x, y - 1];
    p2 := fItersMap[x, y] + 1 - fTransMap[x, y];
  end
  else
  if y < fWorkHeight - 1 then
  begin
    p1 := fItersMap[x, y] + 1 - fTransMap[x, y];
    p2 := fItersMap[x, y + 1] + 1 - fTransMap[x, y + 1];
  end else begin
    p1 := fItersMap[x, y] + 1 - fTransMap[x, y];
    p2 := p1;
  end;
  result := result + abs(p1 - p2) * 1.414;
	if (y > 0) and (x < fWorkWidth - 1) then
  begin
    p1 := fItersMap[x + 1, y - 1] + 1 - fTransMap[x + 1, y - 1];
    p2 := fItersMap[x, y] + 1 - fTransMap[x, y];
	end
  else
  if (x > 0) and (y < fWorkHeight - 1)  then
  begin
    p1 := fItersMap[x, y] + 1 - fTransMap[x, y];
    p2 := fItersMap[x - 1, y + 1] + 1 - fTransMap[x - 1, y + 1];
  end else begin
    p1 := fItersMap[x, y] + 1 - fTransMap[x, y];
    p2 := p1;
  end;
  result := result + abs(p1 - p2);
end;

// Compatible with Kalle''s fraktaler map file
procedure TLevelStorage.ImportFromStream(Stream: TStream);
var
  n, s, x: Integer;
  sm: TSingleMap;
begin
  s := fWorkHeight * 4;
  for x := 0 to fWorkWidth - 1 do
    Stream.Read(fItersMap.fColumns[x]^, s);
  Stream.Read(n, 4);
  Stream.Read(n, 4);
  Stream.Seek(n * 3, soCurrent);
  Stream.Read(n, 4);
  for x := 0 to fWorkWidth - 1 do
    Stream.Read(fTransMap.fColumns[x]^, s);
end;

procedure TLevelStorage.ExportToStream(Stream: TStream);
const
  def_colors: array[0..1] of TRGB = ((0, 0, 0), ($FF, $FF, $FF));
var
  n, s, x: Integer;
  rgb: TRGB;
begin
  Stream.Write(fWorkWidth, 4);
  Stream.Write(fWorkHeight, 4);
  s := fWorkHeight * 4;
  for x := 0 to fWorkWidth - 1 do
    Stream.Write(fItersMap.fColumns[x]^, s);
  n := 1;
  Stream.Write(n, 4);
  if fExportColorMap then begin
    n := ColorMap.ColorsCount;
    Stream.Write(n, 4);
    for x := 0 to fWorkWidth - 1 do begin
      rgb := ColorMap.RGB[x];
      Stream.Write(rgb, 3);
    end;
  end else begin
    n := 2;
    Stream.Write(n, 4);
    Stream.Write(def_colors, 6);
  end;
  Stream.Write(fMaxIters, 4);
  for x := 0 to fWorkWidth - 1 do
    Stream.Write(fTransMap.fColumns[x]^, s);
end;


procedure TLevelStorage.ZLibProgress(Sender: TObject);
begin
  if TJobThread(fJobThread).Terminated then
    Abort;
  TJobThread(fJobThread).NotifyProgress(psRunning,
    Trunc(1000 * fCmprsStream.Position / fCmprsProgressMax));
end;

procedure TLevelStorage.LZMAProgress(const Action: TLZMAProgressAction;
  const Value: int64);
begin
  if TJobThread(fJobThread).Terminated then
    Abort;
  case Action of
    lpAMax: fCmprsProgressMax := Value;
    lpAPos: if fCmprsProgressMax > 0 then
      TJobThread(fJobThread).NotifyProgress(psRunning,
        Trunc(1000 * Value / fCmprsProgressMax));
  end;
end;

procedure TLevelStorage.SaveToFile(const FileName: string; JobThread: TObject);
const
  cmprs: array[TCompressionType] of TZCompressionLevel =
    (zcNone, zcFastest, zcDefault, zcMax, zcNone);
var
  fs: TFileStream;
  ts: THandleStream;
  zs: TZCompressionStream;
  i, m, s, x, p: Integer;
  h: TLevelFileHeader;
  e: TLZMAEncoder;
  v: Byte;
begin
  if (not AllowSaveLevelMaps) or (fWidth = 0) or (fHeight = 0) then
    Exit;
  fJobThread := JobThread;
  TJobThread(fJobThread).NotifyProgress(psStarting, 0);
  FillChar(h, sizeof(h), 0);
  h.ID := 'NBLM';
  h.Version := 10;
  h.Width := fWidth;
  h.Height := fHeight;
  h.Supersample := fSupersample;
  h.Flags := Cardinal(fCompression);
  fs := TFileStream.Create(FileName, fmCreate);
  try
    fs.Write(h, sizeof(h));
    if fCompression = ctNone then
      ts := fs
    else
      ts := TTemporaryFileStream.Create;
    try
      s := fWorkHeight * 4;
      m := fWorkWidth * 2;
      p := 1;

      for x := 0 to fWorkWidth - 1 do begin
        ts.Write(fItersMap.fColumns[x]^, s);

        //TJobThread(fJobThread).NotifyProgress(psRunning, MulDiv(1000, p, m));
        inc(p);
      end;

      for x := 0 to fWorkWidth - 1 do begin
        fTransMap.WriteColunm(ts, x);
        //TJobThread(fJobThread).NotifyProgress(psRunning, MulDiv(1000, p, m));
        inc(p);
      end;

      ts.Seek(0, 0);
      case fCompression of
        ctLZMA:
        begin
          e:=TLZMAEncoder.Create;
          try
            e.OnProgress := LZMAProgress;
            e.WriteCoderProperties(fs);
            for i := 0 to 7 do
            begin
              v := (ts.Size shr (8 * i)) and $FF;
              fs.Write(v, 1);
            end;
            e.Code(ts, fs, -1, -1);
          finally
            e.Free;
          end;
        end;

        ctZLibFast..ctZLibBest:
        begin
          fCmprsProgressMax := ts.Size;
          fCmprsStream := fs;
          zs := TZCompressionStream.Create(fs, cmprs[fCompression], 15);
          try
            zs.OnProgress := ZLibProgress;
            zs.CopyFrom(ts, ts.Size);
          finally
            zs.Free;
          end;
          fCmprsStream := nil;
          fCmprsProgressMax := 0;
        end;
      end;
    finally
      if fCompression <> ctNone then
        ts.Free;
    end;
  finally
    fs.Free;
  end;
  fJobThread := nil;
end;

function TLevelStorage.LoadFromFile(const FileName: string; JobThread: TObject): Boolean;
var
  fs: TFileStream;
  ts: THandleStream;
  zs: TZDecompressionStream;
  hdr: TLevelFileHeader;
  ct: TCompressionType;
  p: TLZMAProperties;
  d: TLZMADecoder;
  i, x, z, w, h, s: Integer;
  o: Int64;
  v: Byte;
begin
  result := false;
  fJobThread := JobThread;
  TJobThread(fJobThread).NotifyProgress(psStarting, 0);
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    if fs.Read(hdr, sizeof(hdr)) <> sizeof(hdr) then
      raise EReadError.CreateFmt(sInvalidFile, [FileName]);
    if hdr.ID <> 'NBLM' then
      raise EReadError.CreateFmt(sInvalidFile, [FileName]);
    if hdr.Version <> 10 then
      raise EReadError.CreateFmt(sUnsupported, [FileName]);
    w := hdr.Width;
    h := hdr.Height;
    s := hdr.Supersample;
    ct := TCompressionType(Byte(hdr.Flags));
    SetParams(w, h, s);
    TLoadLevelMapsThread(fJobThread).DefineParams;
    // read maps
    if ct = ctNone then
      ts := fs
    else
      ts := TTemporaryFileStream.Create;
    try
      case ct of
        ctZLibFast..ctZLibBest:
        begin
          fCmprsProgressMax := fs.Size;
          fCmprsStream := fs;
          zs := TZDecompressionStream.Create(fs);
          try
            zs.OnProgress := ZLibProgress;
            ts.CopyFrom(zs, 0);
            ts.Seek(0, 0);
          finally
            zs.Free;
          end;
        end;

        ctLZMA:
        begin
          if fs.Read(p,kPropertiesSize) <> kPropertiesSize then
            raise EReadError.CreateFmt(sInvalidFile, [FileName]);
          d := TLZMADecoder.Create;
          try
            d.OnProgress := LZMAProgress;
            if not d.SetDecoderProperties(p) then
              raise EReadError.CreateFmt(sInvalidFile, [FileName]);
            o := 0;
            for i := 0 to 7 do begin
              if fs.Read(v, 1) <> 1 then
                EReadError.CreateFmt(sInvalidFile, [FileName]);
              o := o or v shl (8 * i);
            end;
            if not d.Code(fs, ts, o) then
              raise Exception.Create('Error in data stream');
            ts.Seek(0, 0);
          finally
            d.Free;
          end;
        end;
      end;  // case


    z := fWorkHeight * 4;
    for x := 0 to fWorkWidth - 1 do begin
      ts.Read(fItersMap.fColumns[x]^, z);
      //TJobThread(fJobThread).NotifyProgress(psRunning, 10);
    end;

    for x := 0 to fWorkWidth - 1 do begin
      fTransMap.ReadColunm(ts, x);
      //TJobThread(fJobThread).NotifyProgress(psRunning, 20);
    end;

    finally
      if ct <> ctNone then
        ts.Free;
    end;
  finally
    fs.Free;
  end;
  fCompression := ct;
  fClean := false;
  if Assigned(fOnLoad) then
    fOnLoad(self);
  result := true;
end;



{ TIniFileEx }



function TIniFileEx.ReadBool(const Section, Ident: string;
  Default: Boolean): Boolean;
begin
  result := StrToBoolDef(ReadString(Section, Ident, ''), Default);
end;

procedure TIniFileEx.WriteBool(const Section, Ident: string; Value: Boolean);
begin
  WriteString(Section, Ident, BoolToStr(Value, true));
end;

procedure TIniFileEx.RenameSection(const OldName, NewName: string);
var
  build: TStrings;
  s: string;
  i: integer;
begin
  if not SectionExists(OldName) or NewName.IsEmpty then
    exit;
  build := TStringList.Create;
  try
    GetStrings(build);
    for i := 0 to build.Count - 1 do
    begin
      s := build[i].Trim;
    if (s <> '') and (s[1] <> ';') then
      if (s[1] = '[') and (s[s.Length] = ']') then
        if OldName = s.Substring(1, s.Length - 2).Trim then
        begin
          build[i] := Format('[%s]', [NewName]);
          break;
        end;
    end;
    SetStrings(build);
  finally
    build.Free;
  end;
end;

procedure TIniFileEx.SortSectionsProc(List: TStringList);
begin
  List.Sort;
end;

procedure TIniFileEx.ExchangeSectionsProc(List: TStringList);
begin
  List.Exchange(fIndex1, fIndex2);
end;

procedure TIniFileEx.ReorderSections(proc: TReorderSectionsProc);
var
  sect, vals, build: TStringList;
  i, j: Integer;
begin
  build := TStringList.Create;
  try
    sect := TStringList.Create;
    try
      ReadSections(sect);
      proc(sect);
      vals := TStringList.Create;
      try
        build.BeginUpdate;
        try
          for i := 0 to sect.Count - 1 do
          begin
            build.Add(Format('[%s]', [sect[i]]));
            vals.Clear;
            ReadSectionValues(sect[i], vals);
            for j := 0 to vals.Count - 1 do
              build.Add(vals[j]);
            build.Add('');
          end;
        finally
          build.EndUpdate;
        end;
      finally
        vals.Free;
      end;
    finally
      sect.Free;
    end;
    SetStrings(build);
  finally
    build.Free;
  end;
end;

procedure TIniFileEx.ExchangeSections(Index1, Index2: Integer);
begin
  fIndex1 := Index1;
  fIndex2 := Index2;
  ReorderSections(ExchangeSectionsProc);
end;

function TIniFileEx.SectionIndex(const aName: string): Integer;
var
  sect: TStringList;
begin
  sect := TStringList.Create;
  try
    ReadSections(sect);
    result := sect.IndexOf(aName);
  finally
    sect.Free;
  end;
end;

procedure TIniFileEx.SortSections;
begin
  ReorderSections(SortSectionsProc);
end;

end.
