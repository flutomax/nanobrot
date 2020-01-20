unit uFrmOptions;

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
  StdCtrls, ExtCtrls, SpTBXItem, SpTBXControls, SpTBXEditors,
  Vcl.Imaging.pngimage;

type
  TFrmOptions = class(TForm)
    pnBottom: TSpTBXPanel;
    btCancel: TSpTBXButton;
    btOK: TSpTBXButton;
    DlgSelectFolder: TFileOpenDialog;
    rgRasterizerType: TSpTBXRadioGroup;
    ckShowHints: TSpTBXCheckBox;
    ckSoundOnDone: TSpTBXCheckBox;
    ckFileAssociation: TSpTBXCheckBox;
    ckSaveFormState: TSpTBXCheckBox;
    gbUndoLimit: TSpTBXGroupBox;
    edUndoLimit: TSpTBXSpinEdit;
    gbPathColorMaps: TSpTBXGroupBox;
    edColorMapsPath: TSpTBXButtonEdit;
    ImgIcon: TImage;
    LbInfo: TSpTBXLabel;
    procedure EdColorMapsPathSubEditButton0Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmOptions: TFrmOptions;


implementation

{$R *.dfm}


procedure TFrmOptions.EdColorMapsPathSubEditButton0Click(Sender: TObject);
var
  dir: string;
begin
  dir := ExcludeTrailingBackslash(EdColorMapsPath.Text);
  DlgSelectFolder.FileName := ExtractFileName(dir);
  DlgSelectFolder.DefaultFolder := ExtractFileDir(dir);
  if not DlgSelectFolder.Execute(Handle) then
    Exit;
  EdColorMapsPath.Text := IncludeTrailingBackslash(DlgSelectFolder.FileName);
end;

end.
