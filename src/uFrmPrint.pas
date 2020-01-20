unit uFrmPrint;

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
  TB2Dock, TB2Toolbar, SpTBXItem, TB2Item, StdCtrls, SpTBXEditors, Menus,
	AppEvnts, ActnList, Actions, ImageList, ImgList, uPrintPreview, uHelpers;

type
  TFrmPrint = class(TForm)
    Statusbar: TSpTBXStatusBar;
    SpTBXDock1: TSpTBXDock;
    SpTBXToolbar1: TSpTBXToolbar;
    ActionList1: TActionList;
    cbZoom: TSpTBXComboBox;
    TBControlItem1: TTBControlItem;
    SpTBXItem1: TSpTBXItem;
    cmdPrint: TAction;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    cmdPrinterSetup: TAction;
    cmdPageSetup: TAction;
    SpTBXItem2: TSpTBXItem;
    cmdZoomIn: TAction;
    cmdZoomOut: TAction;
    SpTBXItem4: TSpTBXItem;
    SpTBXItem5: TSpTBXItem;
    LblIPrint: TSpTBXLabelItem;
    lblPrinter: TSpTBXLabelItem;
    SpTBXSeparatorItem2: TSpTBXSeparatorItem;
    SpTBXLabelItem3: TSpTBXLabelItem;
    lblZoom: TSpTBXLabelItem;
    cmdZoomFit: TAction;
    SpTBXItem6: TSpTBXItem;
    pmPrev: TSpTBXPopupMenu;
    SpTBXItem7: TSpTBXItem;
    SpTBXItem8: TSpTBXItem;
    SpTBXItem9: TSpTBXItem;
    SpTBXItem10: TSpTBXItem;
    SpTBXItem11: TSpTBXItem;
    SpTBXSeparatorItem3: TSpTBXSeparatorItem;
    SpTBXSeparatorItem4: TSpTBXSeparatorItem;
    SpTBXItem12: TSpTBXItem;
    cmdShowPrintSize: TAction;
    ddMenuPrinters: TSpTBXSubmenuItem;
    SpTBXSeparatorItem5: TSpTBXSeparatorItem;
    SpTBXLabelItem2: TSpTBXLabelItem;
    SbLbInfo: TSpTBXLabelItem;
    AppEvents: TApplicationEvents;
    SpTBXSeparatorItem6: TSpTBXSeparatorItem;
    SpTBXItem3: TSpTBXItem;
    cmdPrinterAdd: TAction;
    cmdCloseWnd: TAction;
    SpTBXSeparatorItem7: TSpTBXSeparatorItem;
    SpTBXItem13: TSpTBXItem;
    cmdPrinterPropertys: TAction;
    SpTBXItem14: TSpTBXItem;
    pmPrinter: TSpTBXPopupMenu;
    SpTBXItem15: TSpTBXItem;
    SpTBXItem16: TSpTBXItem;
    SpTBXSeparatorItem8: TSpTBXSeparatorItem;
    SpTBXItem17: TSpTBXItem;
    SpTBXSeparatorItem9: TSpTBXSeparatorItem;
    LblPageSize: TSpTBXLabelItem;
    SpTBXLabelItem5: TSpTBXLabelItem;
    cmdPageOrientPortrait: TAction;
    cmdPageOrentLandscape: TAction;
    SpTBXSeparatorItem10: TSpTBXSeparatorItem;
    SpTBXSubmenuItem1: TSpTBXSubmenuItem;
    SpTBXItem18: TSpTBXItem;
    SpTBXItem19: TSpTBXItem;
    SpTBXSeparatorItem11: TSpTBXSeparatorItem;
    SpTBXItem20: TSpTBXItem;
    SpTBXItem21: TSpTBXItem;
    SpTBXSubmenuItem2: TSpTBXSubmenuItem;
    SpTBXItem22: TSpTBXItem;
    SpTBXItem23: TSpTBXItem;
    SpTBXSeparatorItem12: TSpTBXSeparatorItem;
    cmdShowMargins: TAction;
    SpTBXItem24: TSpTBXItem;
    SpTBXItem25: TSpTBXItem;
    SpTBXSeparatorItem13: TSpTBXSeparatorItem;
    IlPrint: TImageList;
    IlSmall: TImageList;
    SpTBXRightAlignSpacerItem1: TSpTBXRightAlignSpacerItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cmdPageSetupExecute(Sender: TObject);
    procedure DlgPageSetupShow(Sender: TObject);
    procedure cbZoomChange(Sender: TObject);
    procedure cmdPrinterSetupExecute(Sender: TObject);
    procedure cmdPrintExecute(Sender: TObject);
    procedure cmdZoomInExecute(Sender: TObject);
    procedure cmdZoomInUpdate(Sender: TObject);
    procedure cmdZoomOutExecute(Sender: TObject);
    procedure cmdZoomOutUpdate(Sender: TObject);
    procedure cmdZoomFitExecute(Sender: TObject);
    procedure cmdZoomFitUpdate(Sender: TObject);
    procedure cmdShowPrintSizeExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure AppEventsHint(Sender: TObject);
    procedure cmdPrinterAddExecute(Sender: TObject);
    procedure cmdCloseWndExecute(Sender: TObject);
    procedure cmdPrinterPropertysExecute(Sender: TObject);
    procedure cmdPageOrientPortraitExecute(Sender: TObject);
    procedure cmdShowMarginsExecute(Sender: TObject);
    procedure cmdPrintUpdate(Sender: TObject);
    procedure StatusbarDrawDockBackground(Sender: TObject;
      ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage;
      var PaintDefault: Boolean);
  private
    fBmp: TBitmap;
    fZoomUpdate: boolean;
    procedure ZoomChange(Sender: TObject);
    procedure mnCurrPrinterClick(Sender: TObject);
    procedure StateChange(Sender: TObject);
    procedure PageChange(Sender: TObject);
  protected
    procedure WMGetSysCommand(var Message: TMessage); message WM_SYSCOMMAND;
  public
    pp: TPrintPreview;
  end;

var
  FrmPrint: TFrmPrint;

  procedure ShowPrinPreview;

implementation

uses
  Printers, uMain, uMisc;

{$R *.dfm}

procedure ShowPrinPreview;
begin
  FrmPrint.BoundsRect := FrmMain.BoundsRect;
  FrmPrint.WindowState := FrmMain.WindowState;
  FrmPrint.ShowHint := FrmMain.ShowHint;
  FrmPrint.Statusbar.Visible := FrmMain.Statusbar.Visible;
  FrmPrint.Show;
end;

procedure TFrmPrint.WMGetSysCommand(var Message: TMessage);
begin
  case Message.wParam of
    SC_MINIMIZE: Application.Minimize;
    else
      inherited;
  end;

end;

procedure TFrmPrint.FormCreate(Sender: TObject);
var
  i: integer;
  t: TSpTBXItem;
  s: string;
begin
  fZoomUpdate := False;
  PrepareStatusbar(Statusbar);
  pp := TPrintPreview.Create(self);
  pp.Align := alClient;
  pp.Parent := self;
  pp.PopupMenu := pmPrev;
  pp.OnZoomChange := ZoomChange;
  pp.OnStateChange := StateChange;
  pp.OnPageChange := PageChange;

  for i := 0 to pp.Printer.Printers.Count - 1 do
  begin
    t := TSpTBXItem.Create(self);
    t.Caption := pp.Printer.Printers[i];
    t.Hint := 'Printer: ' + pp.Printer.Printers[i];
    t.Name := Format('mnCurPrinter%d', [i]);
    t.Tag := i;
    t.GroupIndex := 4;
    t.AutoCheck := True;
    t.RadioItem := True;
    if i = pp.Printer.PrinterIndex then
      t.Checked := True
    else
      t.Checked := False;
    t.OnClick := mnCurrPrinterClick;
    ddMenuPrinters.Insert(i, t);
  end;

  fBmp := TBitmap.Create;
  cbZoom.ItemIndex := 10;
  cmdShowPrintSize.Checked := pp.ShowPrintableArea;
  cmdShowMargins.Checked := pp.ShowMarginArea;
end;

procedure TFrmPrint.FormDestroy(Sender: TObject);
begin
  fBmp.Free;

end;

procedure TFrmPrint.FormShow(Sender: TObject);
var
  s: string;
begin
  cbZoom.ItemIndex := 10;
  with FrmMain.ImgView.Bitmap do
  begin
    fBmp.SetSize(Width, Height);
    DrawTo(fBmp.Canvas.Handle);
  end;
  pp.Bitmap := fBmp;
  FrmMain.Hide;
  s := ExtractFileName(FrmMain.Nanobrot.FileName);
  Caption := Format('Print [%s]', [s]);
  Application.Title := Caption;
  pp.PrintJobTitle := s;
  PageChange(nil);
end;

procedure TFrmPrint.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FrmMain.WindowState := WindowState;
  FrmMain.Show;
  if WindowState <> wsMaximized then
    FrmMain.BoundsRect := BoundsRect;
end;

procedure TFrmPrint.cmdPageSetupExecute(Sender: TObject);
begin
  pp.ShowPageSetupDialog;
end;

procedure TFrmPrint.DlgPageSetupShow(Sender: TObject);
var
  h: hWnd;
  r: TRect;
begin
  h := TCommonDialog(Sender).Handle;
  SetClassLong(h, GCL_HICON, LoadIcon(hInstance, 'MAINICON'));
  GetWindowRect(h, r);
  r.left := Left + Width div 2 - (r.right - r.left) div 2;
  r.top := Top + Height div 2 - (r.Bottom - r.Top) div 2;
  SetWindowPos(h, 0, r.left, r.top, 0, 0, SWP_NOSIZE);
end;

procedure TFrmPrint.cbZoomChange(Sender: TObject);
begin
  if fZoomUpdate then
    exit;
  fZoomUpdate := True;
  try
    case cbZoom.ItemIndex of
      0: pp.Zoom := 500;
      1: pp.Zoom := 200;
      2: pp.Zoom := 150;
      3: pp.Zoom := 100;
      4: pp.Zoom := 75;
      5: pp.Zoom := 50;
      6: pp.Zoom := 25;
      7: pp.Zoom := 10;
      8: pp.ZoomState := zsZoomToWidth;
      9: pp.ZoomState := zsZoomToHeight;
      10: pp.ZoomState := zsZoomToFit;
    end;
  finally
    fZoomUpdate := False;
  end;
end;

procedure TFrmPrint.cmdPrinterSetupExecute(Sender: TObject);
begin
  pp.AdvancedProperties;
end;

procedure TFrmPrint.cmdPrintExecute(Sender: TObject);
begin
  pp.Print;
end;

procedure TFrmPrint.cmdZoomInExecute(Sender: TObject);
begin
  fZoomUpdate := True;
  try
    pp.Zoom := pp.Zoom + pp.ZoomStep;
  finally
    fZoomUpdate := False;
  end;
end;

procedure TFrmPrint.cmdZoomInUpdate(Sender: TObject);
begin
  cmdZoomin.Enabled := pp.Zoom < pp.ZoomMax;
end;

procedure TFrmPrint.cmdZoomOutExecute(Sender: TObject);
begin
  fZoomUpdate := True;
  try
    pp.Zoom := pp.Zoom - pp.ZoomStep;
  finally
    fZoomUpdate := False;
  end;
end;

procedure TFrmPrint.cmdZoomOutUpdate(Sender: TObject);
begin
  cmdZoomOut.Enabled := pp.Zoom > pp.ZoomMin;
end;

procedure TFrmPrint.ZoomChange(Sender: TObject);
var
  i: integer;
begin
  lblZoom.Caption := Format('%d%%', [pp.Zoom]);
  i := cbZoom.Items.IndexOf(lblZoom.Caption);
  if i = -1 then
    case pp.ZoomState of
      zsZoomToWidth: cbZoom.ItemIndex := 8;
      zsZoomToHeight: cbZoom.ItemIndex := 9;
      zsZoomToFit: cbZoom.ItemIndex := 10;
      else
      begin
        cbZoom.Items[11] := lblZoom.Caption;
        cbZoom.ItemIndex := 11;
      end
    end
  else
    cbZoom.ItemIndex := i;
  //cbZoom.Text:=lblZoom.Caption;
end;

procedure TFrmPrint.cmdZoomFitExecute(Sender: TObject);
begin
  fZoomUpdate := True;
  try
    pp.ZoomState := zsZoomToFit;
  finally
    fZoomUpdate := False;
  end;
end;

procedure TFrmPrint.cmdZoomFitUpdate(Sender: TObject);
begin
  cmdZoomFit.Enabled := pp.ZoomState <> zsZoomToFit;
end;

procedure TFrmPrint.cmdShowPrintSizeExecute(Sender: TObject);
begin
  pp.ShowPrintableArea := cmdShowPrintSize.Checked;
end;

procedure TFrmPrint.mnCurrPrinterClick(Sender: TObject);
begin
  pp.Printer.PrinterIndex := TSpTBXCustomItem(Sender).Tag;
  StateChange(nil);
  pp.Redraw;
end;

procedure TFrmPrint.AppEventsHint(Sender: TObject);
begin
  SbLbInfo.Caption := Application.Hint;
end;

procedure TFrmPrint.cmdPrinterAddExecute(Sender: TObject);
begin
  pp.ShowAddPrinterDialog;
end;

procedure TFrmPrint.cmdCloseWndExecute(Sender: TObject);
begin
  Close;
end;

procedure TFrmPrint.cmdPrinterPropertysExecute(Sender: TObject);
begin
  pp.ShowPrinterPropertys;
end;

procedure TFrmPrint.StatusbarDrawDockBackground(Sender: TObject;
  ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage;
  var PaintDefault: Boolean);
begin
  if PaintStage = pstPostPaint then
    Exit;
  DrawStatusbarBG(Sender as TSpTBXCustomStatusBar, ARect, ACanvas);
  PaintDefault := false;
end;

procedure TFrmPrint.StateChange(Sender: TObject);
var
  s: string;
begin
  if not pp.PrinterInstalled then
  begin
    lblPrinter.Caption := 'Printer unavailable';
    exit;
  end;
  case pp.State of
    psCreating: s := 'Creating';
    psReady: s := 'Ready';
    psPrinting: s := 'Printing';
    else
      s := '';
  end;
  lblPrinter.Caption := Format('Printer "%s", %s',
    [Trim(pp.Printer.Printers[pp.Printer.PrinterIndex]), s]);
end;

procedure TFrmPrint.PageChange(Sender: TObject);
begin
  if pp.Printer.Printers.Count = 0 then
    exit;
  LblPageSize.Caption := Format('%s - [%dx%d mm]',
    [pp.FormName, pp.PageWidth div 100, pp.PageHeight div 100]);
  cmdPageOrientPortrait.Checked := pp.Orientation = poPortrait;
  cmdPageOrentLandscape.Checked := pp.Orientation = poLandscape;
end;

procedure TFrmPrint.cmdPageOrientPortraitExecute(Sender: TObject);
begin
  pp.Orientation := TPrinterOrientation(TAction(Sender).Tag);
end;

procedure TFrmPrint.cmdShowMarginsExecute(Sender: TObject);
begin
  pp.ShowMarginArea := cmdShowMargins.Checked;
end;

procedure TFrmPrint.cmdPrintUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := pp.PrinterInstalled;
end;

end.
