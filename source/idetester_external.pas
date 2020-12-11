unit idetester_external;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  idetester_base;

type
  { TTestNodeId }
  TTestNodeId = class (TObject)
  private
    FTestClassName: String;
    FTestName: String;
  public
    property testClassName : String read FTestClassName write FTestClassName;
    property testName : String read FTestName write FTestName;
  end;

(*
  { TTestSessionExternal }

  TTestSessionExternal = class (TTestSession)
  private
    FSkipList : TTestNodeList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure skipTest(node : TTestNode); override;
  end;


  { TTestEngineExternal }

  TTestEngineExternal = class (TTestEngine)
  public
    procedure loadAllTests(factory : TNodeFactory); override;
    function threadMode : TTestEngineThreadMode; override;
    function canTerminate : boolean; override;
    function doesReload : boolean; override;
    function canDebug : boolean; override;

    function prepareToRunTests : TTestSession; override;
    procedure runTest(session : TTestSession; node : TTestNode; debug : boolean); override;
    procedure terminateTests; override;
    procedure finishTestRun(session : TTestSession); override;
  end;
*)

implementation

end.

