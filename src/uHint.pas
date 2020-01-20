unit uHint;

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
  Classes, Types, Windows, Graphics, Messages, Controls, CommCtrl, ImgList, Forms,
  SysUtils, SpTBXSkins;

const
  ROP_DSPDxax = $00E20746;
  ILD_SCALE = $2000;

type

  THintPaintMode = (pmNormal, pmHot, pmDown, pmChecked, pmCheckedHot, pmCheckedDown);
  TArrowPos = (NE, NW, SE, SW);


  TNBHint = class(TComponent)
  private
    fHintFont: TFont;
    fHintWidth: integer;
    fShadowOpacity: integer;
    fShadowSize: integer;
    fBidiMode: TBidiMode;
    fPaintMode: THintPaintMode;
    fArrowColor: TColor;
    fShadow: boolean;
    fBar: boolean;
    fUseWideCaption: boolean;
    fOnShowHint: TShowHintEvent;
    procedure SetHintFont(Value: TFont);
    procedure GetHintInfo(var HintStr: string; var CanShow: boolean;
      var HintInfo: THintInfo);
    procedure SetBidiMode(const Value: TBidiMode);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property PaintMode: THintPaintMode read fPaintMode write fPaintMode default pmHot;
    property MaxHintWidth: integer read fHintWidth write fHintWidth default 200;
    property Font: TFont read fHintFont write SetHintFont;
    property BidiMode: TBidiMode read fBidiMode write SetBidiMode default bdLeftToRight;
    property OverrideArrowColor: TColor
      read fArrowColor write fArrowColor default clNone;
    property ShadowSize: integer read fShadowSize write fShadowSize default 3;
    property ShadowOpacity: integer read fShadowOpacity write fShadowOpacity default 75;
    property UseShadow: boolean read fShadow write fShadow default True;
    property UseWideCaption: boolean
      read fUseWideCaption write fUseWideCaption default True;
    property MenuBarAppearence: boolean read fBar write fBar default False;
    property OnShowHint: TShowHintEvent read fOnShowHint write fOnShowHint;
  end;

  TNBHintWindow = class(THintWindow)
  private
    fArrowPos: TArrowPos;
    fArrowPoint: TPoint;
    fRgn: HRgn;
    function GetHintStr: string;
  protected
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure Paint; override;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ActivateHint(HintRect: TRect; const AHint: string); override;
  end;

  TNBHintWindowClass = class of TNBHintWindow;

implementation

uses Themes, Math;

resourcestring
  SInvalidParameters = 'Invalid parameters';
  SInvalidImageDimensions = 'Invalid image dimensions';


var
  FHint: TNBHint;
  HintControl: TControl; // control the tooltip belongs to


constructor TNBHint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHint := Self;
  if not (csDesigning in ComponentState) then
  begin
    HintWindowClass := TNBHintWindow;
    with Application do
    begin
      ShowHint := not ShowHint;
      ShowHint := not ShowHint;
      OnShowHint := GetHintInfo;
      HintShortPause := 25;
      HintPause := 500;
      HintHidePause := 5000;
    end;
  end;
  fHintWidth := 200;
  fHintFont := TFont.Create;
  fHintFont.Name := 'Tahoma';
  fPaintMode := pmHot;
  fArrowColor := clNone;
  fBidiMode := bdLeftToRight;
  fShadowOpacity := 75;
  fShadowSize := 3;
  fShadow := True;
  fUseWideCaption := True;
  fBar := False;
end;

destructor TNBHint.Destroy;
begin
  fHintFont.Free;
  inherited Destroy;
end;

procedure TNBHint.SetHintFont(Value: TFont);
begin
  fHintFont.Assign(Value);
end;

procedure TNBHint.GetHintInfo(var HintStr: string; var CanShow: boolean;
  var HintInfo: THintInfo);
begin
  if Assigned(fOnShowHint) then
    fOnShowHint(HintStr, CanShow, HintInfo);
  HintControl := HintInfo.HintControl;
end;

procedure TNBHint.SetBidiMode(const Value: TBidiMode);
begin
  if fBidiMode <> Value then
    fBidiMode := Value;
end;

{ TNBHintWindow }

constructor TNBHintWindow.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque];
end;

destructor TNBHintWindow.Destroy;
begin
  DeleteObject(fRgn);
  inherited;
end;

procedure TNBHintWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style - WS_BORDER;
end;

function TNBHintWindow.GetHintStr: string;
var
  s: string;
  i: integer;
begin
  s := Caption;
  if FHint.UseWideCaption then
  begin
    Result := GetShortHint(Application.Hint);
    if Length(Result) < Length(s) then
    begin
      Result := Result + Copy(s, Length(Result) + 1, Length(s) - Length(Result));
    end;
  end
  else
    Result := s;
end;

procedure TNBHintWindow.Paint;
var
  ArrowRect, TextRect, R, TempRect: TRect;
  s: WideString;
begin
  // Set the Rect's
  s := GetHintStr;
  case FArrowPos of
    NW, SW:
    begin
      ArrowRect := Rect(ClientRect.Left + 1, ClientRect.Top + 1,
        ClientRect.Left + 15, ClientRect.Bottom - 1);
      TextRect := Rect(ClientRect.Left + 15, ClientRect.Top + 1,
        ClientRect.Right - 1, ClientRect.Bottom - 1);
    end;
    NE, SE:
    begin
      ArrowRect := Rect(ClientRect.Right - 15, ClientRect.Top + 1,
        ClientRect.Right - 1, ClientRect.Bottom - 1);
      TextRect := Rect(ClientRect.Left + 1, ClientRect.Top + 1,
        ClientRect.Right - 15, ClientRect.Bottom - 1);
    end;
  end;
  //back
  R := ClientRect;

  SpDrawXPButton(Canvas, R, True, False, False, True, False, False);
  Canvas.Brush.Color := clWindow;
  Canvas.FillRect(TextRect);
  if CurrentSkin.SkinName = 'Default' then
  begin
    Canvas.Brush.Color := clHotLight;
    Canvas.FillRect(ArrowRect);
  end;
  if FHint.fArrowColor <> clNone then
    Canvas.Pen.Color := FHint.fArrowColor
  else
    case SkinManager.GetSkinType of
      sknNone: Canvas.Pen.Color := clWindowFrame;
      sknWindows: Canvas.Pen.Color := ThemeServices.ColorToRGB(clWindowFrame);
      sknSkin: Canvas.Pen.Color := CurrentSkin.Options(skncWindow).Borders.Color1;
      sknDelphiStyle: Canvas.Pen.Color :=
          TStyleManager.ActiveStyle.ColorToRGB(clWindowFrame);
    end;
  //Canvas.Pen.Color:=CurrentSkin.Options(skncWindow,sknsNormal).Borders.Color1;
  Canvas.Brush.Style := bsClear;
  Canvas.RoundRect(R.Left, R.Top, R.Right, r.Bottom, 6, 6);
  Canvas.Brush.Style := bsSolid;
  if FHint.fArrowColor <> clNone then
    Canvas.Pen.Color := FHint.fArrowColor
  else if CurrentSkin.SkinName = 'Default' then
    Canvas.Pen.Color := clWindow
  else
    Canvas.Pen.Color := CurrentSkin.Options(skncButton, sknsNormal).TextColor;

  //Canvas.Pen.Color:=CurrentSkin.Options(skncWindowTitleBar,sknsNormal).TextColor;

  // DrawArrow
  case FArrowPos of
    NW: FArrowPoint := Point(ArrowRect.Left + 2, ArrowRect.Top + 2);
    NE: FArrowPoint := Point(ArrowRect.Right - 3, ArrowRect.Top + 2);
    SW: FArrowPoint := Point(ArrowRect.Left + 2, ArrowRect.Bottom - 3);
    SE: FArrowPoint := Point(ArrowRect.Right - 3, ArrowRect.Bottom - 3);
  end;
  case FArrowPos of
    NW: Canvas.Polyline([Point(FArrowPoint.x, FArrowPoint.y),
        Point(FArrowPoint.x, FArrowPoint.y + 6),
        Point(FArrowPoint.x + 1, FArrowPoint.y + 6),
        Point(FArrowPoint.x + 1, FArrowPoint.y), Point(
        FArrowPoint.x + 6, FArrowPoint.y), Point(FArrowPoint.x + 6, FArrowPoint.y + 1),
        Point(FArrowPoint.x + 2, FArrowPoint.y + 1),
        Point(FArrowPoint.x + 2, FArrowPoint.y + 4),
        Point(FArrowPoint.x + 5, FArrowPoint.y + 7),
        Point(FArrowPoint.x + 6, FArrowPoint.y + 7),
        Point(FArrowPoint.x + 3, FArrowPoint.y + 4),
        Point(FArrowPoint.x + 3, FArrowPoint.y + 3),
        Point(FArrowPoint.x + 6, FArrowPoint.y + 6),
        Point(FArrowPoint.x + 7, FArrowPoint.y + 6),
        Point(FArrowPoint.x + 3, FArrowPoint.y + 2),
        Point(FArrowPoint.x + 4, FArrowPoint.y + 2),
        Point(FArrowPoint.x + 7, FArrowPoint.y + 5),
        Point(FArrowPoint.x + 7, FArrowPoint.y + 6)]);
    NE: Canvas.Polyline([Point(FArrowPoint.x, FArrowPoint.y),
        Point(FArrowPoint.x, FArrowPoint.y + 6),
        Point(FArrowPoint.x - 1, FArrowPoint.y + 6),
        Point(FArrowPoint.x - 1, FArrowPoint.y), Point(
        FArrowPoint.x - 6, FArrowPoint.y), Point(FArrowPoint.x - 6, FArrowPoint.y + 1),
        Point(FArrowPoint.x - 2, FArrowPoint.y + 1),
        Point(FArrowPoint.x - 2, FArrowPoint.y + 4),
        Point(FArrowPoint.x - 5, FArrowPoint.y + 7),
        Point(FArrowPoint.x - 6, FArrowPoint.y + 7),
        Point(FArrowPoint.x - 3, FArrowPoint.y + 4),
        Point(FArrowPoint.x - 3, FArrowPoint.y + 3),
        Point(FArrowPoint.x - 6, FArrowPoint.y + 6),
        Point(FArrowPoint.x - 7, FArrowPoint.y + 6),
        Point(FArrowPoint.x - 3, FArrowPoint.y + 2),
        Point(FArrowPoint.x - 4, FArrowPoint.y + 2),
        Point(FArrowPoint.x - 7, FArrowPoint.y + 5),
        Point(FArrowPoint.x - 7, FArrowPoint.y + 6)]);
    SW: Canvas.Polyline([Point(FArrowPoint.x, FArrowPoint.y),
        Point(FArrowPoint.x, FArrowPoint.y - 6),
        Point(FArrowPoint.x + 1, FArrowPoint.y - 6),
        Point(FArrowPoint.x + 1, FArrowPoint.y), Point(
        FArrowPoint.x + 6, FArrowPoint.y), Point(FArrowPoint.x + 6, FArrowPoint.y - 1),
        Point(FArrowPoint.x + 2, FArrowPoint.y - 1),
        Point(FArrowPoint.x + 2, FArrowPoint.y - 4),
        Point(FArrowPoint.x + 5, FArrowPoint.y - 7),
        Point(FArrowPoint.x + 6, FArrowPoint.y - 7),
        Point(FArrowPoint.x + 3, FArrowPoint.y - 4),
        Point(FArrowPoint.x + 3, FArrowPoint.y - 3),
        Point(FArrowPoint.x + 6, FArrowPoint.y - 6),
        Point(FArrowPoint.x + 7, FArrowPoint.y - 6),
        Point(FArrowPoint.x + 3, FArrowPoint.y - 2),
        Point(FArrowPoint.x + 4, FArrowPoint.y - 2),
        Point(FArrowPoint.x + 7, FArrowPoint.y - 5),
        Point(FArrowPoint.x + 7, FArrowPoint.y - 6)]);
    SE: Canvas.Polyline([Point(FArrowPoint.x, FArrowPoint.y),
        Point(FArrowPoint.x, FArrowPoint.y - 6),
        Point(FArrowPoint.x - 1, FArrowPoint.y - 6),
        Point(FArrowPoint.x - 1, FArrowPoint.y), Point(
        FArrowPoint.x - 6, FArrowPoint.y), Point(FArrowPoint.x - 6, FArrowPoint.y - 1),
        Point(FArrowPoint.x - 2, FArrowPoint.y - 1),
        Point(FArrowPoint.x - 2, FArrowPoint.y - 4),
        Point(FArrowPoint.x - 5, FArrowPoint.y - 7),
        Point(FArrowPoint.x - 6, FArrowPoint.y - 7),
        Point(FArrowPoint.x - 3, FArrowPoint.y - 4),
        Point(FArrowPoint.x - 3, FArrowPoint.y - 3),
        Point(FArrowPoint.x - 6, FArrowPoint.y - 6),
        Point(FArrowPoint.x - 7, FArrowPoint.y - 6),
        Point(FArrowPoint.x - 3, FArrowPoint.y - 2),
        Point(FArrowPoint.x - 4, FArrowPoint.y - 2),
        Point(FArrowPoint.x - 7, FArrowPoint.y - 5),
        Point(FArrowPoint.x - 7, FArrowPoint.y - 6)]);
  end;
  // DrawHintText
  Canvas.brush.Style := bsClear;
  InflateRect(TextRect, -3, -1);
  if BidiMode = bdRightToLeft then
    DrawText(Canvas.handle, PChar(s), Length(s), TextRect, DT_RIGHT or
      DT_WORDBREAK or DT_NOPREFIX)
  else
    DrawText(Canvas.handle, PChar(s), Length(s), TextRect, DT_WORDBREAK or DT_NOPREFIX);
end;

procedure TNBHintWindow.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Msg.Result := 1;
end;

procedure TNBHintWindow.ActivateHint(HintRect: TRect; const AHint: string);
var
  curWidth: byte;
  Pnt: TPoint;
  HintHeight, HintWidth: integer;
  NordWest, NordEast, SouthWest, SouthEast: TRect;
  P: array [0..7] of TPoint;
  s: string;
begin
  DoubleBuffered := True;
  Caption := AHint;
  s := GetHintStr;
  if FHint <> nil then
  begin                 //little correction DS
    Canvas.Font.Assign(FHint.fHintFont);

    // Calculate width and height
    HintRect.Right := HintRect.Left + FHint.fHintWidth - 22;

    if BidiMode = bdRightToLeft then
      DrawText(Canvas.Handle, @s[1], Length(s), HintRect, DT_RIGHT or
        DT_CALCRECT or DT_WORDBREAK or DT_NOPREFIX)
    else
      DrawText(Canvas.Handle, @s[1], Length(s), HintRect, DT_CALCRECT or
        DT_WORDBREAK or DT_NOPREFIX);

    DrawText(Canvas.Handle, @s[1], Length(s), HintRect, DT_CALCRECT or
      DT_WORDBREAK or DT_NOPREFIX);
    Inc(HintRect.Right, 22);
    Inc(HintRect.Bottom, 6);

    // Divide the screen in 4 pices
    NordWest := Rect(0, 0, Screen.Width div 2, Screen.Height div 2);
    NordEast := Rect(Screen.Width div 2, 0, Screen.Width, Screen.Height div 2);
    SouthWest := Rect(0, Screen.Height div 2, Screen.Width div 2, Screen.Height);
    SouthEast := Rect(Screen.Width div 2, Screen.Height div 2,
      Screen.Width, Screen.Height);

    GetCursorPos(Pnt);

    if PtInRect(NordWest, Pnt) then
      FArrowPos := NW
    else
    if PtInRect(NordEast, Pnt) then
      FArrowPos := NE
    else
    if PtInRect(SouthWest, Pnt) then
      FArrowPos := SW
    else
      FArrowPos := SE;

    // Calculate the position of the hint
    if FArrowPos = NW then
      curWidth := 12
    else
      curWidth := 5;

    HintHeight := HintRect.Bottom - HintRect.Top;
    HintWidth := HintRect.Right - HintRect.Left;

    case FArrowPos of
      NW: HintRect := Rect(Pnt.x + curWidth, Pnt.y + curWidth, Pnt.x +
          HintWidth + curWidth, Pnt.y + HintHeight + curWidth);
      NE: HintRect := Rect(Pnt.x - HintWidth - curWidth, Pnt.y +
          curWidth, Pnt.x - curWidth, Pnt.y + HintHeight + curWidth);
      SW: HintRect := Rect(Pnt.x + curWidth, Pnt.y - HintHeight -
          curWidth, Pnt.x + HintWidth + curWidth, Pnt.y - curWidth);
      SE: HintRect := Rect(Pnt.x - HintWidth - curWidth, Pnt.y -
          HintHeight - curWidth, Pnt.x - curWidth, Pnt.y - curWidth);
    end;

    BoundsRect := HintRect;
    HintRect.TopLeft := ScreenToClient(HintRect.TopLeft);
    HintRect.BottomRight := ScreenToClient(HintRect.BottomRight);
    p[0].X := HintRect.Left + 2;
    p[0].Y := HintRect.Top;
    p[1].X := HintRect.Right - 2;
    p[1].Y := HintRect.Top;
    p[2].X := HintRect.Right;
    p[2].Y := HintRect.Top + 2;
    p[3].X := HintRect.Right;
    p[3].Y := HintRect.Bottom - 3;
    p[4].X := HintRect.Right - 3;
    p[4].Y := HintRect.Bottom;
    p[5].X := HintRect.Left + 2;
    p[5].Y := HintRect.Bottom;
    p[6].X := HintRect.Left;
    p[6].Y := HintRect.Bottom - 3;
    p[7].X := HintRect.Left;
    p[7].Y := HintRect.Top + 2;
    fRgn := CreatePolygonRgn(P, 8, WINDING);
    Pnt := ClientToScreen(Point(0, 0));

    SetWindowPos(Handle, HWND_TOPMOST, Pnt.X, Pnt.Y, 0, 0, SWP_SHOWWINDOW or
      SWP_NOACTIVATE or SWP_NOSIZE);
    SetWindowRgn(Handle, fRgn, True);
  end;
end;



initialization
  HintWindowClass := TNBHintWindow;
  fHint := TNBHint.Create(Application);

end.
