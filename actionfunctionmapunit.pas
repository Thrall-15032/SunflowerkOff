unit ActionFunctionMapUnit;

{$mode ObjFPC}{$H+}

interface

uses
  // system
  Classes, SysUtils, fgl, Graphics,
  // main
  ActionClass, ActionKindUnit;
// end uses

type
  TPauseSignalKind = (pseBegin, pseEnd);

  TActionFunction = procedure (AAction: TAction);

  TActionFunctionMap = specialize TFPGMap<TActionKind, TActionFunction>;

  { TActionFunctionClass }

  TActionFunctionClass = class
  private
    FActionFunctionMap: TActionFunctionMap;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure ExecuteAction(AAction: TAction);
    function IsFunctionExist(AActionKind: TActionKind): Boolean;
  end;

var
  ActionFunctionClass: TActionFunctionClass;

implementation

uses
  // system
  Forms, Windows, Win32Extra,
  // main
  KeyboardHookUnit, KeyFunctionUnit, MouseFunctionUnit, Utils, SettingsUnit;
// end uses

procedure PauseSignal(AMessage: string; APauseSignalKind: TPauseSignalKind);
var
  ScreenDC: HDC;
  BitmapDC: Graphics.TBitmap;
begin

  case APauseSignalKind of
    pseBegin:
    begin
      if (AMessage = '') then
        Exit;

    end;
    pseEnd:
    begin

    end;
  end;
end;

procedure PauseExecute(AAction: TAction);
var
  ScreenDC: HDC;
  BitmapDC: Graphics.TBitmap;
  Position: TPoint;
  PRect: TRect;
  TmpStyle: TTextStyle;
  //Tmp: TPAINTSTRUCT;
begin
  KeyboardHookClass.ClearContinueFlag();
  KeyboardHookClass.ClearExecutionFlag();
  Beep(47000, 200);
  ScreenDC := GetDC(0);
  BitmapDC  := Graphics.TBitmap.Create();
  BitmapDC.Canvas.Handle := ScreenDC;
  //BitmapDC.Canvas.Brush.Color := clNone;
  //BitmapDC.Canvas.Brush.Style := bsClear;
  BitmapDC.Canvas.Font.Name := AppSettings.FontName;
  BitmapDC.Canvas.Font.Size := AppSettings.FontSize;
  BitmapDC.Canvas.Font.Color := AppSettings.FontColor;
  BitmapDC.Canvas.Font.Bold := true;
  Position := AppSettings.GetPosition(
    BitmapDC.Canvas.TextWidth(AAction.KeyList),
    BitmapDC.Canvas.TextHeight(AAction.KeyList)
  );
  PRect := TRect.Create(Position, BitmapDC.Canvas.TextWidth(AAction.KeyList), BitmapDC.Canvas.TextHeight(AAction.KeyList));
  TmpStyle.Opaque := false;
  //Tmp.rcPaint := TRect.Create(Position, BitmapDC.Canvas.TextWidth(AAction.KeyList), BitmapDC.Canvas.TextHeight(AAction.KeyList));
  //Tmp.fRestore := false;
  //Tmp.fErase := true;
  //BeginPaint(ScreenDC, Tmp);
  SetBkMode(ScreenDC, OPAQUE);
  while (true) do
  begin
    //AlphaBlend();
    BitmapDC.Canvas.TextOut(Position.X, Position.Y, AAction.KeyList);
    //BitmapDC.Canvas.TextRect(PRect, 0, 0, AAction.KeyList, TmpStyle);
    if (KeyboardHookClass.IsContinueExecution) OR (KeyboardHookClass.IsStopExecution) then
    begin
      Beep(47000, 200);
      FreeObject(BitmapDC);
      //EndPaint(ScreenDC, Tmp);
      ReleaseDC(0, ScreenDC);
      Exit;
    end;
    SmartSleep(0);
  end;
end;

procedure LoopExecute(AAction: TAction);
var
  i, j: Integer;
  ActionListTmp: TActionArr;
begin
  ActionListTmp := PActionArr(AAction.InnerActionList)^;
  for i := 0 to AAction.LoopCount - 1 do
  begin
    if (KeyboardHookClass.IsStopExecution) then
      Exit;
    for j := 0 to Length(ActionListTmp) - 1 do
      ActionFunctionClass.ExecuteAction(ActionListTmp[j]);
    SmartSleep(0);
  end;
end;

procedure InfinityLoopExecute(AAction: TAction);
var
  i: Integer;
  ActionListTmp: TActionArr;
begin
  ActionListTmp := PActionArr(AAction.InnerActionList)^;
  while (true) do
  begin
    if (KeyboardHookClass.IsStopExecution) then
      Exit;
    for i := 0 to Length(ActionListTmp) - 1 do
      ActionFunctionClass.ExecuteAction(ActionListTmp[i]);
    SmartSleep(0);
  end;
end;

{ TActionFunctionClass }

constructor TActionFunctionClass.Create();
begin
  FActionFunctionMap := TActionFunctionMap.Create();
  FActionFunctionMap.Add(actLoop, @LoopExecute);
  FActionFunctionMap.Add(actInfinityLoop, @InfinityLoopExecute);
  FActionFunctionMap.Add(actPause, @PauseExecute);
  FActionFunctionMap.Add(actKeyDownOne, @KeyDownOne);
  FActionFunctionMap.Add(actKeyUpOne, @KeyUpOne);
  FActionFunctionMap.Add(actKeyPressOne, @KeyPressOne);
  FActionFunctionMap.Add(actKeyPressMany, @KeyPressMany);
  FActionFunctionMap.Add(actMouseMove, @MouseMove);
  FActionFunctionMap.Add(actLookForEmpty, @LookForEmpty);
  FActionFunctionMap.Add(actLookForPixel, @LookForPixel);
  FActionFunctionMap.Add(actMousePressLeft, @MousePressLeft);
  FActionFunctionMap.Add(actMousePressRight, @MousePressRight);
  FActionFunctionMap.Add(actMouseFloodLeft, @MouseFloodLeft);
  FActionFunctionMap.Add(actMouseFloodRight, @MouseFloodRight);
end;

destructor TActionFunctionClass.Destroy();
begin
  FreeObject(FActionFunctionMap);
  inherited Destroy();
end;

procedure TActionFunctionClass.ExecuteAction(AAction: TAction);
var
  Func: TActionFunction;
begin
  Func := FActionFunctionMap.KeyData[AAction.Kind];
  Func(AAction);
end;

function TActionFunctionClass.IsFunctionExist(AActionKind: TActionKind): Boolean;
begin
  Result := (FActionFunctionMap.IndexOf(AActionKind) >= 0);
end;

end.

