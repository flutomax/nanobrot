unit uFrmAbout;

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
  ExtCtrls, SpTBXSkins, SpTBXItem, SpTBXControls, GR32;

type
  TFrmAbout = class(TForm)
    LinkLabel1: TLinkLabel;
    imAppIcon: TImage;
    btOK: TSpTBXButton;
    LbLogo: TSpTBXLabel;
    lbVersion: TSpTBXLabel;
    LinkLabel2: TLinkLabel;
    LinkLabel3: TLinkLabel;
    procedure LinkLabel1LinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fBmp: TBitmap32;
    procedure MakeBackground;
  public
    { Public declarations }
  end;

var
  FrmAbout: TFrmAbout;

  procedure ShowAbout;

implementation

{$R *.dfm}

uses
  ShellAPI, GR32_VectorUtils, GR32_Polygons, GR32_ColorGradients, GR32_Blurs;

procedure ShowAbout;
begin
  Application.CreateForm(TFrmAbout, FrmAbout);
  with FrmAbout do
  try
    ShowModal;
  finally
    Release;
  end;
end;

function ExeVersion: string;
var
  Info: Pointer;
  FileInfo: PVSFixedFileInfo;
  InfoSize, FileInfoSize, Handle: Cardinal;
begin
  InfoSize := GetFileVersionInfoSize(PChar(Application.ExeName), Handle);
  if InfoSize = 0 then
    exit;
  GetMem(Info, InfoSize);
  try
    GetFileVersionInfo(PChar(Application.ExeName), 0, InfoSize, Info);
    VerQueryValue(Info, '\', Pointer(FileInfo), FileInfoSize);
    with FileInfo^ do
      Result := Format('Version: %d.%d.%d.%d (Win%d)', [
        HiWord(dwFileVersionMS), LoWord(dwFileVersionMS),
        HiWord(dwFileVersionLS), LoWord(dwFileVersionLS),
        {$IFDEF CPUX64}64{$ELSE}32{$ENDIF}]
      );
  finally
    FreeMem(Info);
  end;
end;

procedure TFrmAbout.FormCreate(Sender: TObject);
begin
  fBmp := TBitmap32.Create;
  fBmp.SetSize(ClientWidth, ClientHeight);
  imAppIcon.Picture.Icon.Handle := LoadImage(hInstance, 'MAINICON', IMAGE_ICON,
    256, 256, LR_LOADTRANSPARENT);
  lbVersion.Caption := ExeVersion;
  MakeBackground;
end;

procedure TFrmAbout.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fBmp);
end;

procedure TFrmAbout.MakeBackground;
const
  offs = 2;
var
  GradLUT: TColor32LookupTable;
  Gradient: TColor32Gradient;
  GradFiller: TLinearGradientPolygonFiller;
  Polygon: TArrayOfFloatPoint;
begin
  Polygon := Rectangle(FloatRect(fBmp.BoundsRect));
  GradLUT := TColor32LookupTable.Create;
  Gradient := TColor32Gradient.Create;
  try
    Gradient.StartColor := $FF4169E1;
    Gradient.EndColor := clWhite32;
    Gradient.FillColorLookUpTable(GradLUT);
    GradFiller := TLinearGradientPolygonFiller.Create(GradLUT);
    try
      GradFiller.StartPoint := FloatPoint(fBmp.Width * 0.5, 0);
      GradFiller.EndPoint := FloatPoint(fBmp.Width * 0.5, fBmp.Height - 1);
      PolygonFS(fBmp, Polygon, GradFiller);
    finally
      FreeAndNil(GradFiller);
    end;
  finally
    FreeAndNil(GradLUT);
    FreeAndNil(Gradient);
  end;

  fBmp.Font := LbLogo.Font;
  // Make shadow
  fBmp.RenderText(LbLogo.Left + offs, LbLogo.Top + offs, LbLogo.Caption, 4, $90000000);
  GaussianBlur(fBmp, 7);
  fBmp.RenderText(LbLogo.Left, LbLogo.Top, LbLogo.Caption, 4, $FFF0F8FF);
end;

procedure TFrmAbout.FormPaint(Sender: TObject);
begin
  fBmp.DrawTo(Canvas.Handle);
end;

procedure TFrmAbout.LinkLabel1LinkClick(Sender: TObject; const Link: string;
  LinkType: TSysLinkType);
begin
  ShellExecute(0, 'Open', PChar(Link), nil, nil, SW_SHOWNORMAL);
end;

end.
