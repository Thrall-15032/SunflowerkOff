unit ActionExecutorUnit;

{$mode ObjFPC}{$H+}

interface

uses
  // system
  Classes, SysUtils, Windows, ComCtrls,
  // main
  ActionClass, ActionTimerUnit;

type
  TExecuteResult = (exeProcessNotFound, exeSuccessful, exeBreaked);

  { TActionExecutorClass }

  TActionExecutorClass = class
  private
    FProcessHandle: HWND;
    FActionGuid: TGuid;
    FWindowRect: TRect;
    FExecutionList: TActionArr;
    function GetWindowPosition(): TPoint;
    function GetWindowSize(): TPoint;
    procedure DoWindowManagement(AWindowParams: TGameParams);
    function UpdateWindowRect(): Boolean;
    procedure ParseTreeView(ATreeView: TTreeView);
    function ParseRecursive(ARootNode: TTreeNode): TActionArr;
  public
    constructor Create();
    destructor Destroy(); override;
    function ParseAndExecute(
      ATreeViewGuid: TGUID; AWindowParams: TGameParams; ATreeView: TTreeView
    ): TExecuteResult;
    property ProcessHandle: HWND read FProcessHandle write FProcessHandle;
    property ExecutionList: TActionArr read FExecutionList;
  end;

var
  ActionExecutorClass: TActionExecutorClass;

implementation

uses
  // system
  Dialogs, Forms,
  // main
  ActionKindUnit, ActionFunctionMapUnit, KeyboardHookUnit, MouseFunctionUnit, Utils;
// end uses

{ TActionExecutorClass }

constructor TActionExecutorClass.Create();
begin
  FWindowRect := TRect.Create(0, 0, 0, 0);
  FActionGuid := TGuid.NewGuid();
end;

destructor TActionExecutorClass.Destroy();
begin

end;

function TActionExecutorClass.GetWindowPosition(): TPoint;
var
  RectTmp: TRect;
begin
  GetWindowRect(FProcessHandle, RectTmp);
  Result := TPoint.Create(RectTmp.TopLeft);
end;

function TActionExecutorClass.GetWindowSize(): TPoint;
var
  RectTmp: TRect;
begin
  GetWindowRect(FProcessHandle, RectTmp);
  Result := TPoint.Create(
    RectTmp.BottomRight.X - RectTmp.TopLeft.X,
    RectTmp.BottomRight.Y - RectTmp.TopLeft.Y
  );
end;

procedure TActionExecutorClass.DoWindowManagement(AWindowParams: TGameParams);
begin
  if (AWindowParams.DoMaximize) then
    ShowWindow(FProcessHandle, SW_MAXIMIZE)
  else
  begin
    if (IsIconic(FProcessHandle)) then
      ShowWindow(FProcessHandle, SW_RESTORE)
    else
      ShowWindow(FProcessHandle, SW_SHOW);
  end;
  BringWindowToTop(ActionExecutorClass.FProcessHandle);

  if ((AWindowParams.DoWindowPosition) AND (AWindowParams.DoWindowSize)) then
  begin
    SetWindowPos(
      FProcessHandle, HWND_TOP,
      AWindowParams.NewWindowPosition.X, AWindowParams.NewWindowPosition.Y,
      AWindowParams.NewWindowSize.X, AWindowParams.NewWindowSize.Y,
      SWP_SHOWWINDOW
    );
  end
  else
  begin
    if (AWindowParams.DoWindowPosition) then
    begin
      SetWindowPos(
        FProcessHandle, HWND_TOP,
        AWindowParams.NewWindowPosition.X, AWindowParams.NewWindowPosition.Y,
        0, 0, SWP_SHOWWINDOW + SWP_NOSIZE
      );
    end;
    if (AWindowParams.DoWindowSize) then
    begin
      SetWindowPos(
        FProcessHandle, HWND_TOP, 0, 0,
        AWindowParams.NewWindowSize.X, AWindowParams.NewWindowSize.Y,
        SWP_SHOWWINDOW + SWP_NOMOVE
      );
    end;
  end;

  if (AWindowParams.DoStartDisplayCenter) then
  begin
    DoMouseMove(TPoint.Create(Screen.Width div 2, Screen.Height div 2), 10, 0);
  end;
  if (AWindowParams.DoStartWindowCenter) then
  begin
    GetWindowRect(FProcessHandle, FWindowRect);
    DoMouseMove(FWindowRect.CenterPoint, 10, 0);
  end;
end;

function TActionExecutorClass.UpdateWindowRect(): Boolean;
var
  RectTmp: TRect;
begin
  GetWindowRect(FProcessHandle, RectTmp);
  Result := NOT(EqualRect(RectTmp, FWindowRect));
  FWindowRect := TRect.Create(RectTmp);
end;

procedure TActionExecutorClass.ParseTreeView(ATreeView: TTreeView);
var
  i: Integer;
  ActionArrTmp: TActionArr;
  NodeTmp: TTreeNode;
begin
  for i := 0 to Length(FExecutionList) - 1 do
    if (Assigned(FExecutionList[i].InnerActionList)) then
      FreePointer(PActionArr(FExecutionList[i].InnerActionList));
  SetLength(FExecutionList, 0);
  ClearTimerList();

  NodeTmp := ATreeView.Items.GetFirstNode;
  while (Assigned(NodeTmp)) do
  begin
    if (NodeTmp.StateIndex = 1) then
      ActionArrTmp := ParseRecursive(NodeTmp);
    CopyActionsToEnd(ActionArrTmp, FExecutionList);
    NodeTmp := NodeTmp.GetNextSibling();
  end;
end;

function TActionExecutorClass.ParseRecursive(ARootNode: TTreeNode): TActionArr;
var
  Child: TTreeNode;
  Data: TAction;
  ActionArrTmp: TActionArr;
  ActionArrPointer: PActionArr;
begin
  Child := ARootNode.GetFirstChild();
  SetLength(Result, 0);
  while (Assigned(Child)) do
  begin
    if (Child.StateIndex = 0) then
    begin
      Child := ARootNode.GetNextChild(Child);
      Continue;
    end;

    Data := TAction(Child.Data^);
    if (Data.Kind = actChapter) then
    begin
      ActionArrTmp := ParseRecursive(Child);
      CopyActionsToEnd(ActionArrTmp, Result);
      Child := ARootNode.GetNextChild(Child);
      Continue;
    end;
    if (Data.Kind = actTimer) then
    begin
      ActionArrTmp := ParseRecursive(Child);
      CreateNewTimer(ActionArrTmp, Data.Interval);

      Child := ARootNode.GetNextChild(Child);
      Continue;
    end;

    // upgrade relative coordinates to absolute coordinates
    Data.PointRes := TPoint.Create(ParsePointRes(Data, FWindowRect));
    Data.ScreenRectRes := ParseRectRes(Data, FWindowRect);

    if ((Data.Kind = actInfinityLoop) OR (Data.Kind = actLoop)) then
    begin
      ActionArrTmp := ParseRecursive(Child);
      New(ActionArrPointer);
      CopyActionsToPointer(ActionArrTmp, ActionArrPointer);
      Data.InnerActionList := ActionArrPointer;
    end;

    if (ActionFunctionClass.IsFunctionExist(Data.Kind)) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := Data;
    end;
    Child := ARootNode.GetNextChild(Child);
  end;
end;

function TActionExecutorClass.ParseAndExecute(
  ATreeViewGuid: TGUID;
  AWindowParams: TGameParams;
  ATreeView: TTreeView
): TExecuteResult;
var
  i: Integer;
  IsNewRect: Boolean;
begin
  if (FProcessHandle <= 0) then
  begin
    Result := exeProcessNotFound;
    Exit;
  end;
  DoWindowManagement(AWindowParams);

  // That var need for guaranty call UpdateWindowRect()
  IsNewRect := UpdateWindowRect();
  if (NOT(IsEqualGUID(ATreeViewGuid, FActionGuid)) OR IsNewRect) then
  begin
    FActionGuid := ATreeViewGuid;
    ParseTreeView(ATreeView);
  end;
  KeyboardHookClass.ClearExecutionFlag();
  StartAllTimer(true);

  for i := 0 to Length(FExecutionList) - 1 do
  begin
    ActionFunctionClass.ExecuteAction(FExecutionList[i]);
    if (KeyboardHookClass.IsStopExecution) then
    begin
      StopAllTimer();
      Result := exeBreaked;
      Exit;
    end;
    SmartSleep(0);
  end;
end;

end.

