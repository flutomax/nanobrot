unit uColorMap;

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
  Windows, SysUtils, Classes, Controls, StdCtrls, Graphics, GR32;

const

  DEF_COLORMAP = 'Default.map';

type

  TRGB = array[0..2] of byte;
  TPalette = TArrayOfColor32;

  TColorMap = class
  private
    fRange: Double;
    fRecRange: Double;
    fOffset: Double;
    fPalette: TPalette;
    fOnChange: TNotifyEvent;
    procedure SetOffset(const Value: Double);
    procedure SetRange(const Value: Double);
    function GetColorsCount: Integer; inline;
    function GetRGB(index: Integer): TRGB; inline;
  protected
    procedure Changed; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function GetColor(Value: Double): TColor32; inline;
    function GetFirstColor: TColor32; inline;
    function GetLastColor: TColor32; inline;
    function LoadFromFile(const aFileName: string): Boolean; inline;
    procedure GetBitmap(const Horizontal: boolean; var Bmp: TBitmap32); inline;
    property RGB[index: integer]: TRGB read GetRGB;
  published
    property ColorsCount: Integer read GetColorsCount;
    property Range: Double read fRange write SetRange;
    property Offset: Double read fOffset write SetOffset;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  end;

  TColorMapManager = class(TComponent)
  private
    fList: TStrings;
    fDirectory: string;
    fOnLoad: TNotifyEvent;
    function CreateColorMapBitmap(Index: Integer; Rect: TRect): TBitmap32;
    procedure LoadList(warning: Boolean = true);
    procedure SetDirectory(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Refresh;
    procedure FillList(aList: TStrings);
    procedure LoadColorMap(ColorMap: TColorMap; const Name: string);
    procedure DrawListBoxItem(Control: TCustomListBox; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
  published
    property Directory: string read fDirectory write SetDirectory;
    property List: TStrings read fList;
    property OnLoad: TNotifyEvent read fOnLoad write fOnLoad;
  end;

implementation

uses
  Forms, Dialogs, RTLConsts, Math, GR32_LowLevel, GR32_Blend, GR32_Resamplers;

const
  K_BYTE = 255;

function MixColors(const TargetColor, BaseColor: TColor32;
  const Amount: Byte): TColor32; inline;
var
  Base: TColor32Entry absolute BaseColor;
  Target: TColor32Entry absolute TargetColor;
  Res: TColor32Entry absolute Result;
begin
  Res.R := (Target.R - Base.R) * Amount div 255 + Base.R;
  Res.G := (Target.G - Base.G) * Amount div 255 + Base.G;
  Res.B := (Target.B - Base.B) * Amount div 255 + Base.B;
  Res.A := $FF;
end;

{ TColorMap }

constructor TColorMap.Create;
begin
  fPalette := nil;
  fOffset := 0;
  Range := 100;
end;

destructor TColorMap.Destroy;
begin
  fPalette := nil;
  inherited;
end;

procedure TColorMap.Changed;
begin
  if Assigned(fOnChange) then
    fOnChange(self);
end;

procedure TColorMap.SetOffset(const Value: Double);
begin
  fOffset := Value;
end;

procedure TColorMap.SetRange(const Value: Double);
begin
  fRange := Value;
  fRecRange := IfThen(fRange > 0, 1 / fRange, 0);
end;

function TColorMap.GetColorsCount: Integer;
begin
  result := Length(fPalette);
end;

function TColorMap.GetFirstColor: TColor32;
begin
  result := fPalette[0];
end;

function TColorMap.GetLastColor: TColor32;
begin
  result := fPalette[High(fPalette)];
end;

function TColorMap.GetRGB(index: Integer): TRGB;
var
  c: TColor32Entry;
begin
  index := EnsureRange(index, Low(fPalette), High(fPalette));
  c.ARGB := fPalette[index];
  result[0] := c.R;
  result[1] := c.G;
  result[2] := c.B;
end;

function TColorMap.LoadFromFile(const aFileName: string): Boolean;
var
  f: TStringList;
  s: PChar;
  i, j: integer;
  r: TRGB;
begin
  result:=false;
  if not FileExists(aFileName) then
    exit;
  f := TStringList.Create;
  try
    f.LoadFromFile(aFileName);
    if f.Count < 2 then
      exit;
    SetLength(fPalette, f.Count);
    for i := 0 to f.Count - 1 do begin
      s := PChar(f[i]);
      for j := 0 to 2 do begin
        if s^ = #0 then
          break;
        while not (s^ in ['0'..'9']) do
          Inc(s);
        r[j] := 0;
        while s^ in ['0'..'9'] do begin
		      r[j] := 10 * r[j] + Ord(s^) - Ord('0');
		      Inc(s);
	      end;
      end;
      fPalette[i] := Color32(r[0], r[1], r[2]);
    end;
  finally
    f.Free;
  end;
  Changed;
  result := true;
end;

procedure TColorMap.GetBitmap(const Horizontal: boolean; var Bmp: TBitmap32);
var
  x, y: Integer;
begin
  if Horizontal then
  begin
    bmp.SetSize(ColorsCount, 1);
    TLinearResampler.Create(bmp);
    for x := 0 to bmp.Width - 1 do begin
      bmp[x, 0] := fPalette[x];
    end;
  end else begin
    bmp.SetSize(1, ColorsCount);
    TLinearResampler.Create(bmp);
    for y := 0 to bmp.Height - 1 do begin
      bmp[0, y] := fPalette[y];
    end;
  end;
end;

function TColorMap.GetColor(Value: Double): TColor32;
var
  i, w: Integer;
  n, v: Double;
begin
  n := fRange + 1;
  v := Value + fOffset;
  Value := v - n * Int(v / n); // fmod
  if Value >= fRange then begin
    Result := GetLastColor;
    exit;
  end;
  if Value = 0 then begin
    Result := fPalette[0];
    exit;
  end;
  n := Frac(Value * fRecRange);
  n := n * (ColorsCount - 1);
  i := Trunc(n);
  n := n - i;
  w := Clamp(Round(n * K_BYTE));
  Result := MixColors(fPalette[i+1], fPalette[i], w);
  EMMS;
end;


{ TColorMapManager }

constructor TColorMapManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fList := TStringList.Create;
  TStringList(fList).CaseSensitive := false;
  fDirectory := ExtractFilePath(Application.ExeName) + 'ColorMaps\';
end;

destructor TColorMapManager.Destroy;
var
  i: Integer;
begin
  for i := fList.Count - 1 downto 0 do
    fList.Objects[i].Free;
  FreeAndNil(fList);
  inherited;
end;

procedure TColorMapManager.FillList(aList: TStrings);
begin
  aList.Clear;
  aList.AddStrings(fList);
end;

procedure TColorMapManager.LoadList(warning: Boolean);
var
  si: TSearchRec;
  mfn: string;
begin
  fList.Clear;
  if not DirectoryExists(fDirectory) then begin
    MessageDlg(Format('Directory "%s" not found! Color maps failed.', [fDirectory]), mtError, [mbOK], 0);
    Exit;
  end;

  mfn := fDirectory + 'index.lst';
  if not FileExists(mfn) then begin
    if warning then
      MessageDlg(Format('Can''t load maps. %s: %s'#13#10 +
      'Problem was be fixed now.',[SFileNotFound, mfn]), mtWarning, [mbOK], 0);
    Screen.Cursor := crHourGlass;
    try
      if FindFirst(fDirectory + '*.map', faAnyFile, si) = 0 then begin
        repeat
          if (si.Name='.') or (si.Name='..') or (si.Name='') then
            continue;
          fList.Add(si.Name);
        until FindNext(si) <> 0;
        FindClose(si);
      end;
      if fList.Count = 0 then begin
        MessageDlg(Format('Color maps not found in Directory "%s"!', [fDirectory]), mtError, [mbOK], 0);
        Exit;
      end;

      TStringList(fList).Sort;
      fList.SaveToFile(mfn);
    finally
      Screen.Cursor := crDefault;
    end;
  end else begin
    fList.LoadFromFile(mfn);
  end;
  if Assigned(fOnLoad) then
    fOnLoad(self);
end;

procedure TColorMapManager.Refresh;
var
  f: string;
begin
  f := fDirectory + 'index.lst';
  if FileExists(f) then
    DeleteFile(f);
  LoadList(false);
end;

procedure TColorMapManager.SetDirectory(const Value: string);
begin
  if (fDirectory = Value) and (fList.Count > 0) then
    Exit;
  if DirectoryExists(Value) then
    fDirectory := IncludeTrailingBackslash(Value);
  Refresh;
end;

procedure TColorMapManager.LoadColorMap(ColorMap: TColorMap; const Name: string);
begin
  if fList.IndexOf(Name) < 0 then begin
    MessageDlg(Format('Can''t load map: "%s".',[Name]), mtWarning, [mbOK], 0);
    Exit;
  end;
  ColorMap.LoadFromFile(fDirectory + Name);
end;

function TColorMapManager.CreateColorMapBitmap(Index: Integer; Rect: TRect): TBitmap32;
var
  map: TColorMap;
begin
  result := TBitmap32.Create;
  result.SetSize(Rect.Width, Rect.Height);
  result.DrawMode := dmBlend;
  map := TColorMap.Create;
  try
    if map.LoadFromFile(fDirectory + fList[index]) then
      map.GetBitmap(true, result)
    else
      result.Clear(clWhite32);
  finally
    map.Free;
  end;
end;

procedure TColorMapManager.DrawListBoxItem(Control: TCustomListBox; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  bmp, mp: TBitmap32;
  R, Tr: TRect;
  Pc: TColor;
  c: TCanvas;
begin
  c := Control.Canvas;
  bmp := TBitmap32.Create;
  try
    bmp.SetSize(Rect.Width, Rect.Height);
    if odSelected in State then
      c.Brush.Color := IfThen(odDisabled in State, clLtGray, clHighlight);
    Pc := Color32(c.Brush.Color);
    bmp.Clear(Pc);

    if fList.Objects[Index] = nil then
      fList.Objects[Index] := CreateColorMapBitmap(Index, Rect);
    mp := TBitmap32(fList.Objects[Index]);
    R := bmp.BoundsRect;
    Tr := Classes.Rect(2, 24, R.Right - 2, R.Bottom);
    R.Bottom := 24;
    InflateRect(R, -4, -4);
    mp.ResetAlpha(IfThen(odDisabled in State, $55, $FF));
    mp.DrawTo(bmp, R, mp.BoundsRect);
    InflateRect(R, 1, 1);
    if odDisabled in State then
      c.Font.Color := clGrayText;
    Pc := Color32(c.Font.Color);
    bmp.LineS(R.Left + 1, R.Top, R.Right - 1, R.Top, Pc);
    bmp.LineS(R.Right - 1, R.Top + 1, R.Right - 1, R.Bottom - 1, Pc);
    bmp.LineS(R.Left + 1, R.Bottom - 1, R.Right - 1, R.Bottom - 1, Pc);
    bmp.LineS(R.Left, R.Top + 1, R.Left, R.Bottom - 1, Pc);
    bmp.Font := c.Font;
    bmp.Textout(Tr, DT_TOP or DT_CENTER or DT_END_ELLIPSIS or DT_NOPREFIX or
      DT_SINGLELINE, fList[Index]);

    bmp.DrawTo(c.Handle, Rect.Left, Rect.Top);
  finally
    bmp.Free;
  end;
  if odFocused in State then
    c.DrawFocusRect(Rect);
end;


end.
