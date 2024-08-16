unit ActionTimerUnit;

{$mode ObjFPC}{$H+}

interface

uses
  // system
  Classes, SysUtils, ExtCtrls,
  // main
  ActionClass;

type

  { TActionTimer }

  TActionTimer = class(TTimer)
  private
    FActionList: TActionArr;
  public
    constructor Create(AActionList: TActionArr; AInterval: Integer);
    destructor Destroy(); override;
    procedure Execute(Sender: TObject);
  end;

procedure CreateNewTimer(AActionList: TActionArr; AInterval: Integer);
procedure StartAllTimer(DoImmediate: Boolean = false);
procedure StopAllTimer();
procedure ClearTimerList();

var
  ActionTimerList: array of TActionTimer;

implementation

uses
  // system
  Dialogs,
  // main
  ActionFunctionMapUnit, Utils;
// end uses

procedure CreateNewTimer(AActionList: TActionArr; AInterval: Integer);
begin
  SetLength(ActionTimerList, Length(ActionTimerList) + 1);
  ActionTimerList[Length(ActionTimerList) - 1] := TActionTimer.Create(AActionList, AInterval);
end;

procedure StartAllTimer(DoImmediate: Boolean = false);
var
  i: Integer;
begin
  for i := 0 to Length(ActionTimerList) - 1 do
  begin
    if (DoImmediate) then
      ActionTimerList[i].Execute(nil);
    ActionTimerList[i].Enabled := true;
  end;
end;

procedure StopAllTimer();
var
  i: Integer;
begin
  for i := 0 to Length(ActionTimerList) - 1 do
  begin
    ActionTimerList[i].Enabled := false;
  end;
end;

procedure ClearTimerList();
var
  i: Integer;
begin
  for i := 0 to Length(ActionTimerList) - 1 do
  begin
    ActionTimerList[i].Enabled := false;
    FreeObject(ActionTimerList[i]);
  end;
  SetLength(ActionTimerList, 0);
end;

{ TActionTimer }

constructor TActionTimer.Create(AActionList: TActionArr; AInterval: Integer);
begin
  inherited Create(nil);

  Interval := AInterval;
  Enabled := false;
  SetLength(FActionList, 0);
  CopyActionsToEnd(AActionList, FActionList);
  OnTimer := @Execute;
end;

destructor TActionTimer.Destroy();
begin
  SetLength(FActionList, 0);
  inherited Destroy();
end;

procedure TActionTimer.Execute(Sender: TObject);
var
  i: Integer;
  Func: TActionFunction;
begin
  for i := 0 to Length(FActionList) - 1 do
    ActionFunctionClass.ExecuteAction(FActionList[i]);
end;

end.

