unit uFrmImportLevelMap;

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
  ExtCtrls, SpTBXItem, SpTBXControls, StdCtrls, SpTBXEditors;

type
  TFrmImportLevelMap = class(TForm)
    CbSupersample: TSpTBXComboBox;
    LbInfo: TSpTBXLabel;
    ImgIcon: TImage;
    lbSupersample: TSpTBXLabel;
    EdImgSize: TSpTBXEdit;
    LbImgSize: TSpTBXLabel;
    SpTBXPanel1: TSpTBXPanel;
    BtnOK: TSpTBXButton;
    BtnCancel: TSpTBXButton;
    procedure CbSupersampleChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  public
    InitWidth: Integer;
    IhitHeight: Integer;
    procedure Setup;
    procedure GetParams(var w, h, s: Integer);
  end;

var
  FrmImportLevelMap: TFrmImportLevelMap;

  function ShowImportLevelMapDialog(var w, h, s: Integer): Boolean;

implementation

{$R *.dfm}

uses ShellAPI, uMisc;

function ShowImportLevelMapDialog(var w, h, s: Integer): Boolean;
begin
  Application.CreateForm(TFrmImportLevelMap, FrmImportLevelMap);
  with FrmImportLevelMap do
  try
    InitWidth := w;
    IhitHeight := h;
    Setup;
    result := ShowModal = mrOK;
    if result then
      GetParams(w, h, s);
  finally
    Release;
  end;
end;

{ TFrmImportLevelMap }

procedure TFrmImportLevelMap.Setup;
var
  i: Integer;
begin
  ImgIcon.Picture.Icon.Handle := GetDefaultSystemIcon(SIID_INFO);
  CbSupersample.Items.BeginUpdate;
  try
    CbSupersample.Items.Clear;
    for i := 1 to 16 do
    begin
      if (InitWidth mod i = 0) and (IhitHeight mod i = 0) then
        CbSupersample.Items.AddObject(Format('%dx', [i]), TObject(IntPtr(i)));
    end;
  finally
    CbSupersample.Items.EndUpdate;
  end;
  CbSupersample.ItemIndex := 0;
  CbSupersampleChange(nil);
end;


procedure TFrmImportLevelMap.FormDestroy(Sender: TObject);
begin
  DestroyIcon(ImgIcon.Picture.Icon.Handle);
end;

procedure TFrmImportLevelMap.GetParams(var w, h, s: Integer);
begin
  with CbSupersample do
    s := IntPtr(Items.Objects[ItemIndex]);
  w := InitWidth div s;
  h := IhitHeight div s;
end;

procedure TFrmImportLevelMap.CbSupersampleChange(Sender: TObject);
var
  w, h, s: Integer;
begin
  GetParams(w, h, s);
  EdImgSize.Text := Format('%dx%d px', [w, h]);
end;

end.
