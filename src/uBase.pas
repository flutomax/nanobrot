unit uBase;

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
  SysUtils, Classes, ActnList, GR32;

const
  sInvalidFile = 'Can''t load file "%s".'#13#10'Invalid file format or file is corrupted.';
  sUnsupported = 'Can''t load file "%s".'#13#10'The file is an unsupported version.';
  sDefFilename = 'Noname.nbr';

type

  TComandID = (cidCenterX, cidCenterY, cidZoom, cidIterations, cidPeriod,
    cidOrderM, cidOrderN, cidJitter, cidSupersample, cidOutColoring,
    cidColorMapFn, cidColorMapRange, cidColorMapOffset, cidSlopes,
    cidSlopeAngle, cidSlopePower, cidSlopeRatio);

  TComandState = (csNone, csUndo, csRedo);

  THistory = class;

  TComandPersistent = class(TNotifiablePersistent)
  protected
    fHistory: THistory;
    procedure DoExecuteComand(const aCmdID: TComandID; const aData: Variant); virtual; abstract;
    procedure AddComand(const aCmdID: TComandID; const aOldData, aNewData: Variant);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Runned: Boolean; virtual;
    procedure Undo;
    procedure Redo;
    procedure UpdateUndo(const Target: TAction);
    procedure UpdateRedo(const Target: TAction);
  end;

  TComand = class
  protected
    fCmdID: TComandID;
    fState: TComandState;
    fName: string;
    fEnabled: boolean;
    fOldData: Variant;
    fNewData: Variant;
    fOwner: TComandPersistent;
  public
    constructor Create(aOwner: TComandPersistent; const aCmdID: TComandID;
      const aOldData, aNewData: Variant);
    procedure Rollback; virtual;
    procedure Execute; virtual;
    property State: TComandState read fState;
    property Name: string read fName;
    property Enabled: boolean read fEnabled write fEnabled;
  end;

  THistory = class
  private
    fUndoLimit: integer;
    fCurrentStateIndex: integer;
    fState: TComandState;
    fHistory: TList;
    fUpdate: boolean;
    function GetComandCount: integer;
    function GetCurrentComandName: string;
    function GetNextComandName: string;
  public
    constructor Create;
    destructor Destroy; override;
    function AddComand(const cmd: TComand): integer;
    procedure DeselectAllHistoryStates;
    procedure DeleteHistoryStates(const AStartIndex, AEndIndex: integer);
    procedure DeleteAllHistoryStates;
    function SelectHistoryStateByIndex(const AIndex: integer): boolean;
    procedure Undo;
    procedure Redo;
    procedure UpdateUndo(const Target: TAction);
    procedure UpdateRedo(const Target: TAction);
    property Updated: boolean read fUpdate;
    property UndoLimit: Integer read fUndoLimit write fUndoLimit;
  end;

implementation

uses
  Controls, Forms;

const
  S_UNDO = 'Undo ';
  S_REDO = 'Redo ';
  S_CANNOTUNDO = 'Can''t Undo';
  S_CANNOTREDO = 'Can''t Redo';

  S_CMDNAMES: array[TComandID] of string = (
    'Center X', 'Center Y', 'Zoom', 'Iterations', 'Period', 'Order M',
    'Order N', 'Jitter', 'Supersample',  'Coloring', 'Color Map Filename',
    'Color Map Range', 'Color Map Offset', 'Slopes', 'Slope Angle',
    'Slope Power', 'Slope Ratio');


{ TComandPersistent }

constructor TComandPersistent.Create;
begin
  fHistory := THistory.Create;
end;

destructor TComandPersistent.Destroy;
begin
  FreeAndNil(fHistory);
  inherited;
end;

procedure TComandPersistent.Undo;
begin
  fHistory.Undo;
end;

procedure TComandPersistent.Redo;
begin
  fHistory.Redo;
end;

function TComandPersistent.Runned: Boolean;
begin
  result := false;
end;

procedure TComandPersistent.UpdateUndo(const Target: TAction);
begin
  fHistory.UpdateUndo(Target);
  Target.Enabled := Target.Enabled and not Runned;
end;

procedure TComandPersistent.UpdateRedo(const Target: TAction);
begin
  fHistory.UpdateRedo(Target);
  Target.Enabled := Target.Enabled and not Runned;
end;

procedure TComandPersistent.AddComand(const aCmdID: TComandID; const aOldData,
  aNewData: Variant);
var
  cmd: TComand;
begin
  if (UpdateCount > 0) or fHistory.Updated then
    Exit; // Обломингос с добавлением!
  cmd := TComand.Create(self, aCmdID, aOldData, aNewData);
  fHistory.AddComand(cmd);
end;


{ TComand }

constructor TComand.Create(aOwner: TComandPersistent; const aCmdID: TComandID;
  const aOldData, aNewData: Variant);
begin
  fOwner := aOwner;
  fCmdID := aCmdID;
  fState := csUndo;
  fOldData := aOldData;
  fNewData := aNewData;
  fName := S_CMDNAMES[fCmdID];
end;

procedure TComand.Execute;
begin
  fState := csUndo;
  fOwner.DoExecuteComand(fCmdID, fNewData);
end;

procedure TComand.Rollback;
begin
  fState := csRedo;
  fOwner.DoExecuteComand(fCmdID, fOldData);
end;

{ THistory }

constructor THistory.Create;
begin
  fHistory := TList.Create;
  fUndoLimit := 100;
  fCurrentStateIndex := -1;
  fUpdate := false;
  fState := csNone;
end;

destructor THistory.Destroy;
begin
  DeleteAllHistoryStates;
  fHistory.Free;
  inherited Destroy;
end;

function THistory.GetComandCount: integer;
begin
  result := fHistory.Count;
end;

function THistory.SelectHistoryStateByIndex(const AIndex: integer): boolean;
begin
  result := false;
  if fHistory.Count = 0 then
    exit;
  if (AIndex >= 0) and (AIndex < fHistory.Count) then
  begin
    DeselectAllHistoryStates;
    fCurrentStateIndex := AIndex;
    result := true;
  end;
end;

function THistory.AddComand(const cmd: TComand): integer;
var
  Diff: integer;
begin
  result := -1;
  if (cmd = nil) then
    exit;
  if fUpdate then
  begin
    // ignore Comand
    cmd.Free;
    exit;
  end;
  if fCurrentStateIndex < (fHistory.Count - 1) then
    DeleteHistoryStates(fCurrentStateIndex + 1, fHistory.Count - 1);
  fHistory.Add(cmd);
  // delete superfluous history states in the list
  Diff := fHistory.Count - fUndoLimit;
  if Diff > 0 then
    DeleteHistoryStates(0, diff - 1);
  SelectHistoryStateByIndex(fHistory.Count - 1);
  result := fHistory.Count - 1;
  fCurrentStateIndex := result;
  fState := cmd.State;
end;

procedure THistory.DeleteAllHistoryStates;
var
  i: integer;
  cmd: TComand;
begin
  if fHistory.Count = 0 then
    exit;
  for i := fHistory.Count - 1 downto 0 do
  begin
    cmd := TComand(fHistory[i]);
    FreeAndNil(cmd);
  end;
  fHistory.Clear;
  fCurrentStateIndex := -1;
  fUpdate := false;
end;

procedure THistory.DeleteHistoryStates(const AStartIndex, AEndIndex: integer);
var
  i: integer;
  cmd: TComand;
begin
  if fHistory.Count = 0 then
    exit;
  if (AStartIndex >= 0) and (AStartIndex < fHistory.Count)
    and (AEndIndex >= 0) and (AEndIndex < fHistory.Count) and
    (AStartIndex <= AEndIndex) then
  begin
    Screen.Cursor := crHourGlass;
    try
      for i := AEndIndex downto AStartIndex do
      begin
        cmd := TComand(fHistory[i]);
        FreeAndNil(cmd);
        fHistory.Delete(i);
      end;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure THistory.DeselectAllHistoryStates;
begin
  if fHistory.Count = 0 then
    exit;
  fCurrentStateIndex := -1;
end;

procedure THistory.Undo;
var
  cmd: TComand;
begin
  fUpdate := true;
  try
    if (fCurrentStateIndex >= 0) and (fCurrentStateIndex < fHistory.Count) then
    begin
      cmd := TComand(fHistory[fCurrentStateIndex]);
      cmd.Rollback;
      cmd.Enabled := false;
      fState := cmd.State;
      Dec(fCurrentStateIndex);
    end;
  finally
    fUpdate := false;
  end;
end;

procedure THistory.Redo;
var
  cmd: TComand;
begin
  fUpdate := true;
  try
    Inc(fCurrentStateIndex);
    if (fCurrentStateIndex >= 0) and (fCurrentStateIndex < fHistory.Count) then
    begin
      cmd := TComand(fHistory[fCurrentStateIndex]);
      cmd.Execute;
      cmd.Enabled := true;
      fState := cmd.State;
    end;
    if fCurrentStateIndex >= fHistory.Count then
      fCurrentStateIndex := fHistory.Count - 1;
  finally
    fUpdate := false;
  end;
end;

function THistory.GetCurrentComandName: string;
begin
  if (fCurrentStateIndex >= 0) and (fCurrentStateIndex < fHistory.Count) then
    result := TComand(fHistory[fCurrentStateIndex]).Name
  else
    result := '';
end;

function THistory.GetNextComandName: string;
begin
  if (fCurrentStateIndex + 1 >= 0) and (fCurrentStateIndex + 1 < fHistory.Count) then
    result := TComand(fHistory[fCurrentStateIndex + 1]).Name
  else
    result := '';
end;

procedure THistory.UpdateUndo(const Target: TAction);
begin
  if (fCurrentStateIndex >= 0) and (fCurrentStateIndex < fHistory.Count) then
  begin
    Target.Caption := S_UNDO + GetCurrentComandName;
    Target.Enabled := true;
  end
  else
  begin
    Target.Caption := S_CANNOTUNDO;
    Target.Enabled := false;
  end;
  Target.Hint := Target.Caption;
end;

procedure THistory.UpdateRedo(const Target: TAction);
begin
  if (fCurrentStateIndex < fHistory.Count - 1) then
  begin
    Target.Caption := S_REDO + GetNextComandName;
    Target.Enabled := true;
  end
  else
  begin
    Target.Caption := S_CANNOTREDO;
    Target.Enabled := false;
  end;
  Target.Hint := Target.Caption;
end;



end.
