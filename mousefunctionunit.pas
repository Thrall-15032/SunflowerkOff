unit MouseFunctionUnit;

{$mode ObjFPC}{$H+}

interface

uses
  // system
  Classes, Controls,
  SysUtils,
  // main
  ActionClass;
// end uses

type
  TLoopFunction = function (x: Integer; y: Integer; AAction: TAction): Boolean;

procedure DoMouseMove(APoint: TPoint; Interval: Integer; Sleep: Integer);
procedure DoMouseClick(AMouseButton: TMouseButton; Interval: Integer; Sleep: Integer);
procedure DoRectLoop(AAction: TAction; AFunc: TLoopFunction);

procedure LookForEmpty(AAction: TAction);
procedure LookForPixel(AAction: TAction);
procedure MouseMove(AAction: TAction);
procedure MouseMoveWindowCenter(AAction: TAction);
procedure MouseMoveDisplayCenter(AAction: TAction);
procedure MousePressLeft(AAction: TAction);
procedure MousePressRight(AAction: TAction);
procedure MouseFloodLeft(AAction: TAction);
procedure MouseFloodRight(AAction: TAction);

implementation

uses
  // system
  MouseAndKeyInput, Forms, Windows,  Graphics,
  Dialogs,
  // main
  ActionKindUnit, Utils;
// end uses

procedure DoMouseMove(APoint: TPoint; Interval: Integer; Sleep: Integer);
begin
  MouseAndKeyInput.MouseInput.Move([], APoint.X, APoint.Y, Interval);
  SmartSleep(Sleep);
end;

procedure DoMouseClick(AMouseButton: TMouseButton; Interval: Integer; Sleep: Integer);
begin
  MouseAndKeyInput.MouseInput.Down(AMouseButton, []);
  SmartSleep(Interval);
  MouseAndKeyInput.MouseInput.Up(AMouseButton, []);
  SmartSleep(Sleep);
end;

procedure DoRectLoop(AAction: TAction; AFunc: TLoopFunction);
var
  i, j: Integer;
  fi, p, p_k, fi_k: Double;
  OutSpiral: Integer;
  BoolResult: Boolean;
begin
  case AAction.LookKind of
    lookHorizontal:
    begin
      j := AAction.ScreenRectRes.Top;
      while (j < AAction.ScreenRectRes.Bottom) do
      begin
        i := AAction.ScreenRectRes.Left;
        while (i < AAction.ScreenRectRes.Right) do
        begin
          if (Assigned(AFunc)) then
          begin
            BoolResult := AFunc(i, j, AAction);
            if (BoolResult) then
              Exit;
          end;
          i := i + AAction.ScreenStep;
        end;
        j := j + AAction.ScreenStep;
      end;
    end;
    lookVertical:
    begin
      i := AAction.ScreenRectRes.Left;
      while (i < AAction.ScreenRectRes.Right) do
      begin
        j := AAction.ScreenRectRes.Top;
        while (j < AAction.ScreenRectRes.Bottom) do
        begin
          if (Assigned(AFunc)) then
          begin
            BoolResult := AFunc(i, j, AAction);
            if (BoolResult) then
              Exit;
          end;
          j := j + AAction.ScreenStep;
        end;
        i := i + AAction.ScreenStep;
      end;
    end;
    lookSpiral:
    begin
      fi := 0.5;
      p_k := AAction.ScreenStep / 6;
      fi_k := 0.7;
      while (true) do
      begin
        p := p_k * fi;
        i := AAction.ScreenRectRes.CenterPoint.X + Round(p * Cos(fi));
        j := AAction.ScreenRectRes.CenterPoint.Y + Round(p * Sin(fi));

        if (AAction.ScreenRectRes.Contains(TPoint.Create(i, j))) then
        begin
          OutSpiral := 0;
          if (Assigned(AFunc)) then
          begin
            BoolResult := AFunc(i, j, AAction);
            if (BoolResult) then
              Exit;
          end;
        end
        else
          Inc(OutSpiral);
        if (OutSpiral > 200) then
          Exit;

        fi := fi + fi_k;
        fi_k := fi_k * 0.977;
        if (fi_k < 0.05) then
          fi_k := fi_k + fi_k;
      end;
    end;
  end;
end;

function LookForEmptyFunc(x: Integer; y: Integer; AAction: TAction): Boolean;
begin
  Result := false;
  DoMouseMove(TPoint.Create(x, y), 0, 0);
end;

procedure LookForEmpty(AAction: TAction);
begin
  DoRectLoop(AAction, @LookForEmptyFunc);
end;

function LookForPixelFunc(x: Integer; y: Integer; AAction: TAction): Boolean;
var
  ScreenDC: HDC;
  PointColor: TColor;
begin
  Result := false;
  ScreenDC := GetDC(0);
  DoMouseMove(TPoint.Create(x, y), 0, 0);
  PointColor := GetPixel(ScreenDC, x, y);
  ReleaseDC(0, ScreenDC);
  Result := (PointColor = AAction.PixelColor);
end;

procedure LookForPixel(AAction: TAction);
begin
  DoRectLoop(AAction, @LookForPixelFunc);
end;

procedure MouseMove(AAction: TAction);
begin
  DoMouseMove(AAction.PointRes, AAction.Interval, AAction.Sleep);
end;

procedure MouseMoveWindowCenter(AAction: TAction);
begin
  DoMouseMove(AAction.PointRes, AAction.Interval, AAction.Sleep);
end;

procedure MouseMoveDisplayCenter(AAction: TAction);
begin
  DoMouseMove(TPoint.Create(Screen.Width div 2, Screen.Height div 2),
    AAction.Interval, AAction.Sleep);
end;

procedure MousePressLeft(AAction: TAction);
begin
  DoMouseClick(mbLeft, AAction.Interval, AAction.Sleep);
end;

procedure MousePressRight(AAction: TAction);
begin
  DoMouseClick(mbRight, AAction.Interval, AAction.Sleep);
end;

function MouseFloodLeftFunc(x: Integer; y: Integer; AAction: TAction): Boolean;
begin
  Result := false;
  DoMouseMove(TPoint.Create(x, y), 0, 0);
  DoMouseClick(mbLeft, AAction.Interval, AAction.Sleep);
end;

procedure MouseFloodLeft(AAction: TAction);
begin
  DoRectLoop(AAction, @MouseFloodLeftFunc);
end;

function MouseFloodRightFunc(x: Integer; y: Integer; AAction: TAction): Boolean;
begin
  Result := false;
  DoMouseMove(TPoint.Create(x, y), 0, 0);
  DoMouseClick(mbRight, AAction.Interval, AAction.Sleep);
end;

procedure MouseFloodRight(AAction: TAction);
begin
  DoRectLoop(AAction, @MouseFloodRightFunc);
end;

end.

