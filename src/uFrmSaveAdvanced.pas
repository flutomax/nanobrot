unit uFrmSaveAdvanced;

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
  StdCtrls, ExtCtrls, SpTBXControls, SpTBXEditors, SpTBXItem, uHelpers;

type
  TFrmSaveAdvanced = class(TForm)
    CkIncludeLevelMaps: TSpTBXCheckBox;
    SpTBXPanel1: TSpTBXPanel;
    BtnOK: TSpTBXButton;
    BtnCancel: TSpTBXButton;
    lbCompression: TSpTBXLabel;
    CbCompression: TSpTBXComboBox;
    LbInfo: TSpTBXLabel;
    ImgIcon: TImage;
    procedure CkIncludeLevelMapsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmSaveAdvanced: TFrmSaveAdvanced;

  function ShowFrmSaveAdvanced(Storage: TLevelStorage): Boolean;

implementation

{$R *.dfm}

uses ShellAPI, uMisc;

function ShowFrmSaveAdvanced(Storage: TLevelStorage): Boolean;
begin
  Application.CreateForm(TFrmSaveAdvanced, FrmSaveAdvanced);
  with FrmSaveAdvanced do
  try
    CkIncludeLevelMaps.Checked := Storage.IncludeLevelMaps;
    CbCompression.Enabled := CkIncludeLevelMaps.Checked;
    CbCompression.ItemIndex := Ord(Storage.Compression);
    result := ShowModal = mrOK;
    if result then
    begin
      Storage.IncludeLevelMaps := CkIncludeLevelMaps.Checked;
      Storage.Compression := TCompressionType(CbCompression.ItemIndex);
    end;
  finally
    Release;
  end;
end;

procedure TFrmSaveAdvanced.CkIncludeLevelMapsClick(Sender: TObject);
begin
  CbCompression.Enabled := CkIncludeLevelMaps.Checked;
end;

procedure TFrmSaveAdvanced.FormCreate(Sender: TObject);
begin
  ImgIcon.Picture.Icon.Handle := GetDefaultSystemIcon(SIID_INFO);
end;

procedure TFrmSaveAdvanced.FormDestroy(Sender: TObject);
begin
  DestroyIcon(ImgIcon.Picture.Icon.Handle);
end;

end.
