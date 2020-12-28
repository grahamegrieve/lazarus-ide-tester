unit idetester_options;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, LCLIntf,
  idetester_strings;

type
  { TIDETesterSettings }

  TIDETesterSettings = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    btnOk: TButton;
    btnCancel: TButton;
    Button1: TButton;
    chkAutoSave: TCheckBox;
    edtParameters: TEdit;
    editWaitTime: TEdit;
    edtTester: TEdit;
    lblTester: TLabel;
    lblhelp: TLabel;
    lblTimeout: TLabel;
    lblExecutionParameters: TLabel;
    odProject: TOpenDialog;
    pnlBottom: TPanel;
    pnlKillTime: TPanel;
    pnlParameters: TPanel;
    pnlTester: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure edtTesterChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lblhelpClick(Sender: TObject);
  private

  public

  end;

var
  IDETesterSettings: TIDETesterSettings;

implementation

{$R *.lfm}

procedure switchButtons(btn1, btn2 : TControl);
var
  l : integer;
begin
  l := btn1.left;
  btn1.left := btn2.left;
  btn2.left := l;
end;

{ TIDETesterSettings }

procedure TIDETesterSettings.lblhelpClick(Sender: TObject);
begin
  OpenURL(helpUrl);
end;

procedure TIDETesterSettings.FormCreate(Sender: TObject);
begin
  btnOk.Caption := rs_IdeTester_Caption_OK;
  btnCancel.Caption := rs_IdeTester_Caption_Cancel;
  lblhelp.caption := rs_IdeTester_Caption_Help;
  lblTester.caption := rs_IdeTester_Caption_Options_Label_Tester;
  lblExecutionParameters.caption := rs_IdeTester_Caption_Options_Label_Parameters;
  lblTimeout.Caption := rs_IdeTester_Caption_Options_Label_Timeout;
  chkAutoSave.Caption := rs_IdeTester_Caption_Options_Label_AutoSave;
  {$IFNDEF WINDOWS}
  switchButtons(btnOk, btnCancel);
  {$ENDIF}
end;

procedure TIDETesterSettings.FormShow(Sender: TObject);
var
  h : integer;
begin
  h := pnlBottom.Height;
  if pnlTester.Visible then
    inc(h, pnlTester.Height);
  if pnlParameters.Visible then
    inc(h, pnlParameters.Height);
  if pnlKillTime.Visible then
    inc(h, pnlKillTime.Height);
  Height := h;
end;

procedure TIDETesterSettings.Button1Click(Sender: TObject);
begin
  if odProject.execute then
    edtTester.Text := odProject.filename;
end;

procedure TIDETesterSettings.edtTesterChange(Sender: TObject);
begin
  chkAutoSave.enabled := edtTester.text <> '';
end;

end.

