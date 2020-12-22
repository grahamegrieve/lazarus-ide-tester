unit idetester_options;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, LCLIntf,
  idetester_strings;

type
  { TIDETesterSettings }

  TIDETesterSettings = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    edtParameters: TEdit;
    editWaitTime: TEdit;
    lblhelp: TLabel;
    lblTimeout: TLabel;
    lblExecutionParameters: TLabel;
    Panel1: TPanel;
    pnlKillTime: TPanel;
    pnlParameters: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure lblhelpClick(Sender: TObject);
  private

  public

  end;

var
  IDETesterSettings: TIDETesterSettings;

implementation

{$R *.lfm}

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
  lblTimeout.caption := rs_IdeTester_Caption_Options_Label_Parameters;
  lblExecutionParameters.Caption := rs_IdeTester_Caption_Options_Label_Timeout;
end;

end.

