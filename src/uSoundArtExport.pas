unit uSoundArtExport;

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
  Windows, Classes, SysUtils, IniFiles, Forms, uNanobrot, GR32, GR32_Resamplers,
  GR32_Rasterizers, uHelpers;

type

  TSoundArtExport = class;

  TRoundRasterizer = class(TRegularRasterizer)
  private
    fOwner: TSoundArtExport;
    fOnProgress: TRasterizerProgressEvent;
  protected
    procedure DoRasterize(Dst: TCustomBitmap32; DstRect: TRect); override;
  public
    constructor Create(aOwner: TSoundArtExport);
  published
    property OnProgress: TRasterizerProgressEvent read fOnProgress write fOnProgress;
  end;

  TSoundArtExport = class(TNanobrot)
  private
    fParent: TNanobrot;
    fStartAngle: Double;
    fSweepAngle: Double;
    fImgHeight: Integer;
    fImgWidth: Integer;
    fImgSupersample: Integer;
    fFlipVertical: Boolean;
    fPresetList: TIniFileEx;
    procedure Load;
    procedure SetSweepAngle(const Value: Double);
    procedure SetStartAngle(const Value: Double);
    procedure SetImgSupersample(Value: Integer);
  protected
    procedure DoResize; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetImgSize(aWidth, aHeight: Integer);
    procedure Reset;
    procedure DeletePreset(const aName: string);
    procedure RenamePreset(const OldName, NewName: string);
    procedure ReadPresets(Strings: TStrings);
    procedure UpdatePresets;
    function UpdatePreset(const aName: string): Boolean;
    function MovePresets(List: TList; MoveUp: Boolean): Boolean;
    function SortPresets: Boolean;
    function LoadPreset(const aName: string): Boolean;
    function PresetExists(const aName: string): Boolean;
    function PresetIndex(const aName: string): Integer;
  published
    property StartAngle: Double read fStartAngle write SetStartAngle;
    property SweepAngle: Double read fSweepAngle write SetSweepAngle;
    property ImgWidth: Integer read fImgWidth;
    property ImgHeight: Integer read fImgHeight;
    property ImgSupersample: Integer read fImgSupersample write SetImgSupersample;
    property FlipVertical: Boolean read fFlipVertical write fFlipVertical;
  end;


implementation

uses
  RTLConsts, Math;



{ TRoundRasterizer }

constructor TRoundRasterizer.Create(aOwner: TSoundArtExport);
begin
  inherited Create;
  fOwner := aOwner;
  UpdateRowCount := 1;
end;

procedure TRoundRasterizer.DoRasterize(Dst: TCustomBitmap32; DstRect: TRect);
var
  xi, yi, w, h, i, j, p, UpdateCount: Integer;
  a, d, g, s, c, fx, fy, ofs, x, y, x0, y0, ss: Single;
  ast, k, sa: Double;
  Buffer: TBufferEntry;
  sc: TFixed;
begin
  k := fOwner.Height / fOwner.ImgHeight;
  x0 := fOwner.Width * 0.5;
  y0 := fOwner.Height * 0.5;
  w := fOwner.ImgWidth;
  h := fOwner.ImgHeight;
  p := fOwner.fImgSupersample;
  ss := 1 / p;
  sc := Fixed(1 / (p * p));
  ofs := (ss - 1) * 0.5;
  sa := fOwner.SweepAngle / fOwner.ImgWidth;
  ast := ss * sa;
  UpdateCount := 0;
  for yi := 0 to h - 1 do
  begin
    for xi := 0 to w - 1 do
    begin
      Buffer := EMPTY_ENTRY;
      fy := IfThen(fOwner.FlipVertical, h - 1 - yi, yi) + ofs;
      for j := 1 to p do
      begin
        fx := xi + ofs;
        a := sa * xi;
        for i := 1 to p do
        begin
          g := k * fy * 0.5;
          if fOwner.fInternalRecoloring then
          begin
            fOwner.fLevel.Restore(fx, fy);
          end else begin
            d := DegToRad(a + fOwner.StartAngle);
            sincos(d, s, c);
            x := x0 + g * c;
            y := y0 + g * s;
            fOwner.CalcSample(x, y);
            fOwner.fLevel.Store(fx, fy);
            a := a + ast
          end;
          IncBuffer(Buffer, fOwner.GetCurrentColor(fx, fy));
          fx := fx + ss;
        end;
        fy := fy + ss;
      end;
      MultiplyBuffer(Buffer, sc);
      Dst[xi, yi] := BufferToColor32(Buffer, 16);
    end;
    Inc(UpdateCount);
    if UpdateCount = UpdateRowCount then
    begin
      Dst.Changed(Rect(DstRect.Left, yi - UpdateCount, DstRect.Right, yi));
      UpdateCount := 0;
    end;
    if Assigned(fOnProgress) then
      fOnProgress(self, MulDiv(1000, yi + 1, DstRect.Bottom));
  end;
  Dst.Changed;
end;


{ TSoundArtExport }

constructor TSoundArtExport.Create;
begin
  inherited;
  fPresetList := TIniFileEx.Create(ExtractFilePath(Application.ExeName) + 'SoundArtExport.ini');
  fStoreLevel := false;
  Reset;
  fRasterizer.Free;
  fRasterizer := TRoundRasterizer.Create(self);
  fRasterizer.Sampler := fSuperSampler;
  TRoundRasterizer(fRasterizer).OnProgress := RasterizerProgress;
end;

destructor TSoundArtExport.Destroy;
begin
  FreeAndNil(fPresetList);
  inherited;
end;

procedure TSoundArtExport.Assign(Source: TPersistent);
var
  SourceName: string;
begin
  if Source is TNanobrot then
  begin
    fParent := Source as TNanobrot;
    inherited;
    Load;
  end else begin
    if Source <> nil then
      SourceName := Source.ClassName
    else
      SourceName := 'nil';
    raise EConvertError.CreateResFmt(@SAssignError, [SourceName, ClassName]);
  end;
end;

procedure TSoundArtExport.SetStartAngle(const Value: Double);
begin
  fStartAngle := EnsureRange(Value, -360, 360);
end;

procedure TSoundArtExport.SetSweepAngle(const Value: Double);
begin
  fSweepAngle := EnsureRange(Value, 0.01, 360);
end;

procedure TSoundArtExport.SetImgSize(aWidth, aHeight: Integer);
begin
  if UpdateCount > 0 then
    Exit;
  aWidth := EnsureRange(aWidth, 10, MaxWord);
  aHeight := EnsureRange(aHeight, 10, MaxWord);
  if (fImgWidth = aWidth) and (fImgHeight = aHeight) then
    Exit;
  fImgWidth := aWidth;
  fImgHeight := aHeight;
  DoResize;
end;

procedure TSoundArtExport.SetImgSupersample(Value: Integer);
begin
  Value := EnsureRange(Value, 1, 16);
  if fImgSupersample = Value then
    Exit;
  fImgSupersample := Value;
  DoResize;
end;

procedure TSoundArtExport.Load;
begin
  SetSize(fParent.Width, fParent.Height);
  Supersample := 1;
  DoColormapRequest;
end;

procedure TSoundArtExport.ReadPresets(Strings: TStrings);
begin
  fPresetList.ReadSections(Strings);
end;

procedure TSoundArtExport.Reset;
begin
  fImgWidth := 1000;
  fImgHeight := 500;
  fStartAngle := 0;
  fSweepAngle := 360;
  fImgSupersample := 2;
  fFlipVertical := false;
  DoLoad;
end;

procedure TSoundArtExport.DoResize;
begin
  BeginUpdate;
  try
    fLevel.SetParams(fImgWidth, fImgHeight, fImgSupersample);
    if Assigned(OnResize) then
      OnResize(self);
  finally
    EndUpdate;
  end;
end;

function TSoundArtExport.LoadPreset(const aName: string): Boolean;
var
  w, h: Integer;
begin
  result := false;
  if not PresetExists(aName) then
    Exit;
  BeginUpdate;
  try
    w := fPresetList.ReadInteger(aName, 'Width', 1000);
    h := fPresetList.ReadInteger(aName, 'Height', 500);
    StartAngle := fPresetList.ReadFloat(aName, 'StartAngle', 0);
    SweepAngle := fPresetList.ReadFloat(aName, 'SweepAngle', 360);
    ImgSupersample := fPresetList.ReadInteger(aName, 'Supersample', 2);
    FlipVertical := fPresetList.ReadBool(aName, 'FlipVertical', false);
  finally
    EndUpdate;
  end;
  SetImgSize(w, h);
  DoLoad;
  result := true;
end;

function TSoundArtExport.PresetExists(const aName: string): Boolean;
begin
  result := fPresetList.SectionExists(aName);
end;

function TSoundArtExport.PresetIndex(const aName: string): Integer;
begin
  result := fPresetList.SectionIndex(aName);
end;

function TSoundArtExport.UpdatePreset(const aName: string): Boolean;
begin
  result := false;
  if aName.IsEmpty then
    Exit;
  fPresetList.WriteInteger(aName, 'Width', ImgWidth);
  fPresetList.WriteInteger(aName, 'Height', ImgHeight);
  fPresetList.WriteFloat(aName, 'StartAngle', StartAngle);
  fPresetList.WriteFloat(aName, 'SweepAngle', SweepAngle);
  fPresetList.WriteInteger(aName, 'Supersample', ImgSupersample);
  fPresetList.WriteBool(aName, 'FlipVertical', FlipVertical);
  result := true;
end;

procedure TSoundArtExport.UpdatePresets;
begin
  fPresetList.UpdateFile;
end;

procedure TSoundArtExport.DeletePreset(const aName: string);
begin
  fPresetList.EraseSection(aName);
end;

procedure TSoundArtExport.RenamePreset(const OldName, NewName: string);
begin
  fPresetList.RenameSection(OldName, NewName);
end;

function TSoundArtExport.MovePresets(List: TList; MoveUp: Boolean): Boolean;
var
  i, j, n: Integer;
begin
  result := false;
  if List.Count = 0 then
    Exit;
  if MoveUp then
    for i := 0 to List.Count - 1 do
    begin
      j := Integer(IntPtr(List[i]));
      n := j - 1;
      fPresetList.ExchangeSections(j, n);
      List[i] := Pointer(IntPtr(n));
    end
  else
    for i := List.Count - 1 downto 0 do
    begin
      j := Integer(IntPtr(List[i]));
      n := j + 1;
      fPresetList.ExchangeSections(j, n);
      List[i] := Pointer(IntPtr(n));
    end;
  result := true;
end;

function TSoundArtExport.SortPresets: Boolean;
begin
  fPresetList.SortSections;
  result := true;
end;


end.
