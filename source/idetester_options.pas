unit idetester_options;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type
  { TIDETesterSettings }

  TIDETesterSettings = class(TForm)
    Button1: TButton;
    Button2: TButton;
    edtParameters: TEdit;
    editWaitTime: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
  private

  public

  end;

var
  IDETesterSettings: TIDETesterSettings;

implementation

{$R *.lfm}

end.

