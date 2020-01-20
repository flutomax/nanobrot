unit uProgressBarEx;

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
  Windows, Messages, SysUtils, Classes, Forms, Graphics, Controls, ComCtrls,
  StdCtrls, ShlObj;

type

  TProgressKind = (pkDefault, pkGauge, pkText);

  TProgressBarEx = class(TProgressBar)
  private
    fPosition: Integer;
    fPercentDone: Integer;
    fSavedPd: Integer;
    f32BitMode: Boolean;
    fKind: TProgressKind;
    fLabel: TLabel;
    fFormHandle: HWND;
    fTaskBarList: ITaskBarList3;
    procedure SetupTaskBarList;
    procedure RepaintLabel;
    procedure SetKind(const Value: TProgressKind);
    procedure SetPosition(const Value: Integer);
    procedure CmTextChanged(var msg: TMessage); message CM_TEXTCHANGED;
    procedure CmVisibleChanged(var msg: TMessage); message CM_VISIBLECHANGED;
  protected
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Kind: TProgressKind read fKind write SetKind;
    property Position: Integer read fPosition write SetPosition default 0;
    property Text;
  end;

implementation

uses
  Math, Consts, CommCtrl, ComObj;

const
  Limit16 = 65535;

procedure ProgressLimitError;
begin
  raise Exception.CreateResFmt({$IFNDEF CLR}@{$ENDIF}SOutOfRange, [0, Limit16]);
end;

{ TProgressBarEx }

constructor TProgressBarEx.Create(AOwner: TComponent);
begin
  f32BitMode := InitCommonControl(ICC_PROGRESS_CLASS);
  inherited Create(AOwner);
  fTaskBarList := nil;
  fLabel := TLabel.Create(self);
  font.Style := [fsBold];
  fKind:=pkDefault;
  fPosition := 0;
  fPercentDone := 0;
  fSavedPd := -1;
  fFormHandle := 0;
  DoubleBuffered := true;
  Max := 1000;
  SetupTaskBarList;
end;

destructor TProgressBarEx.Destroy;
begin
  FreeAndNil(fLabel);
  fTaskBarList := nil;
  inherited;
end;

procedure TProgressBarEx.SetupTaskBarList;
var
  tbList: ITaskBarList;
  hr: HRESULT;
begin
   // Works only under Windows 7 and higher
  if CheckWin32Version(6, 1) then
  try
    tbList := CreateComObject(CLSID_TaskBarList) as ITaskBarList;
    tbList.HrInit;
    hr := tbList.QueryInterface(IID_ITaskBarList3, fTaskBarList);
    if hr <> S_OK then begin
      fTaskBarList := nil;
      tbList._Release;
    end;
  except
    fTaskBarList := nil;
  end;
end;

procedure TProgressBarEx.SetKind(const Value: TProgressKind);
begin
  if fKind=Value then
    Exit;
  fKind := Value;
  Invalidate;
end;

procedure TProgressBarEx.SetParent(AParent: TWinControl);
var
  form: TCustomForm;
begin
  inherited;
  if csDestroying in ComponentState then
    Exit;
  fLabel.Parent := self;
  fLabel.AutoSize := false;
  fLabel.Transparent := true;
  fLabel.Align := alClient;
  fLabel.Alignment := taCenter;
  fLabel.Layout := tlCenter;
  if Application.MainFormOnTaskbar then
  begin
    form := GetParentForm(AParent);
    if Assigned(form) and form.HandleAllocated then
      fFormHandle := form.Handle
    else
      fFormHandle := Application.Handle
  end
  else
    fFormHandle := Application.Handle
end;

procedure TProgressBarEx.SetPosition(const Value: Integer);
begin
  if Max = 0 then
    Exit;
  if not f32BitMode and ((Value < 0) or (Value > Limit16)) then
    ProgressLimitError;
  fPosition := Value;
  fPercentDone := Round(Int64(Value) * 1000 / Max);
  if HandleAllocated and (fKind<>pkText) then begin
    SendMessage(Handle, PBM_SETPOS, Value, 0);
    if fKind=pkDefault then
      RepaintLabel;
  end;
  if Assigned(fTaskBarList) then
    fTaskBarList.SetProgressValue(fFormHandle, Value, Max);
end;

procedure TProgressBarEx.RepaintLabel;
begin
  if fPercentDone <> fSavedPd then begin
    fLabel.Caption := FormatFloat('##0.0%', fPercentDone * 0.1);
    Application.Title := 'Nanobrot - '+ fLabel.Caption;
    fSavedPd := fPercentDone;
  end;
end;

procedure TProgressBarEx.CmTextChanged(var msg: TMessage);
begin
  fLabel.Caption := Text;
end;

procedure TProgressBarEx.CmVisibleChanged(var msg: TMessage);
begin
  inherited;
  Application.Title := 'Nanobrot';
  if Assigned(fTaskBarList) then
    fTaskBarList.SetProgressState(fFormHandle,
      IfThen(Visible, TBPF_NORMAL, TBPF_NOPROGRESS));
end;



end.
