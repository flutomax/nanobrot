program Nanobrot;

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

{$R *.res}
{$R *.dres}

uses
  Forms,
  Windows,
  SysUtils,
  uMain in 'uMain.pas' {FrmMain},
  uDecNumber in 'uDecNumber.pas',
  uFloatExp in 'uFloatExp.pas',
  uNanobrot in 'uNanobrot.pas',
  uProgressBarEx in 'uProgressBarEx.pas',
  uColorMap in 'uColorMap.pas',
  uMisc in 'uMisc.pas',
  uHelpers in 'uHelpers.pas',
  uBase in 'uBase.pas',
  uLZMA in 'uLZMA.pas',
  uFrmSaveAdvanced in 'uFrmSaveAdvanced.pas' {FrmSaveAdvanced},
  uHint in 'uHint.pas',
  uPrintPreview in 'uPrintPreview.pas',
  uPrintDialogs in 'uPrintDialogs.pas',
  uFrmPrint in 'uFrmPrint.pas' {FrmPrint},
  uFrmOptions in 'uFrmOptions.pas' {FrmOptions},
  uFrmAbout in 'uFrmAbout.pas' {FrmAbout},
  uFrmSoundArt in 'uFrmSoundArt.pas' {FrmSoundArt},
  uSoundArtExport in 'uSoundArtExport.pas',
  uFrmImportLevelMap in 'uFrmImportLevelMap.pas' {FrmImportLevelMap},
  uFrmPresetManager in 'uFrmPresetManager.pas' {FrmPresetManager};

{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}

var
  VerInfo: TOSVersioninfo;
begin
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(VerInfo);
  if VerInfo.dwMajorVersion < 6 then
  begin
    MessageBox(0, 'The program cannot be run on this operating system. Windows Vista or newer required.',
    'Nanobrot - Error', MB_ICONSTOP or MB_TASKMODAL);
    ExitProcess(0);
  end;

  Randomize;
  FormatSettings := TFormatSettings.Create('en_US');
  Application.Initialize;
  Application.MainFormOnTaskbar := False;
  Application.Title := 'Nanobrot';
  Application.CreateForm(TFrmMain, FrmMain);
  Application.CreateForm(TFrmPrint, FrmPrint);
  Application.Run;
end.
