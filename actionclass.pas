unit ActionClass;

{$mode ObjFPC}{$H+}

interface

uses
  // system
  Classes, SysUtils,
  // main
  ActionKindUnit, Utils;
// end uses

type
  TPointStr = record
    X: string;
    Y: string;
  end;

  TAction = record
    // technical property
    StringListIndex: Integer;
    InnerActionList: Pointer;
    LoopCount: Integer;
    // game execute property
    Kind: TActionKind;
    Key: char;
    KeyList: string;
    PointKind: TPointKind;
    PointRes: TPoint;
    PointStr: TPointStr;
    LookKind: TLookKind;
    RectKind: TRectKind;
    ScreenRectRes: TRect;
    ScreenRectStr: string;
    ScreenStep: Integer;
    PixelColor: Integer;
    Interval: Integer;
    Sleep: Integer;
  end;
  PAction = ^TAction;
  TActionArr = array of TAction;
  PActionArr = ^TActionArr;

  TGameParams = record
    DoMaximize: Boolean;
    DoStartDisplayCenter: Boolean;
    DoStartWindowCenter: Boolean;
    DoWindowPosition: Boolean;
    NewWindowPosition: TPoint;
    DoWindowSize: Boolean;
    NewWindowSize: TPoint;
  end;

function ActionToStr(AAction: TAction): string;
procedure CopyActionsToEnd(ASrcList: TActionArr; var AResult: TActionArr);
procedure CopyActionsToPointer(ASrcList: TActionArr; var AResult: PActionArr);

function GetEmptyAction(): TAction;
function ParseAction(AStr: string): TAction;
procedure ParseActionPointer(AStr: string; var AAction: PAction);
function ParseParams(AStr: string): TGameParams;
function IsTrueParams(AGameParams: TGameParams): Boolean;
function ParamsToStr(AGameParams: TGameParams): string;
function ParsePointRes(AAction: TAction; ARect: TRect): TPoint;
function ParseRectRes(AAction: TAction; ARect: TRect): TRect;

implementation

uses
  // system
  Forms, TypInfo, StrUtils;
  // main

// end uses

function ActionToStr(AAction: TAction): string;
begin
  Result := 'Kind = ' + GetEnumName(TypeInfo(TActionKind), integer(AAction.Kind));
  Result := Result + #10 + 'Key = ' + AAction.Key;
  Result := Result + #10 + 'KeyList = ' + AAction.KeyList;
  Result := Result + #10 + 'PointKind = ' +
    GetEnumName(TypeInfo(TPointKind), Integer(AAction.PointKind));
  Result := Result + #10 + 'PointStr = ' +
    AAction.PointStr.X + ':' + AAction.PointStr.Y;
  Result := Result + #10 + 'PointRes = ' +
    IntToStr(AAction.PointRes.X) + ':' + IntToStr(AAction.PointRes.Y);
  Result := Result + #10 + 'LookKind = ' +
    GetEnumName(TypeInfo(TLookKind), Integer(AAction.LookKind));
  Result := Result + #10 + 'RectKind = ' +
    GetEnumName(TypeInfo(TRectKind), Integer(AAction.RectKind));
  Result := Result + #10 + 'ScreenRectStr = ' + AAction.ScreenRectStr;
  Result := Result + #10 + 'ScreenRectRes = ' +
    IntToStr(AAction.ScreenRectRes.Left) + ':' +
    IntToStr(AAction.ScreenRectRes.Top) + ' ... ' +
    IntToStr(AAction.ScreenRectRes.Right) + ':' +
    IntToStr(AAction.ScreenRectRes.Bottom);
  Result := Result + #10 + 'ScreenStep = ' + IntToStr(AAction.ScreenStep);
  Result := Result + #10 + 'PixelColor = ' + IntToHex(AAction.PixelColor);
  Result := Result + #10 + 'Interval = ' + IntToStr(AAction.Interval);
  Result := Result + #10 + 'Sleep = ' + IntToStr(AAction.Sleep);
  Result := Result + #10 + 'StringListIndex = ' + IntToStr(AAction.StringListIndex);
  Result := Result + #10 + 'InnerActionList = ' + IntToStr(Integer(AAction.InnerActionList));
  Result := Result + #10 + 'LoopCount = ' + IntToStr(AAction.LoopCount);
end;

procedure CopyActionsToEnd(ASrcList: TActionArr; var AResult: TActionArr);
var
  i: Integer;
begin
  SetLength(AResult, Length(AResult) + Length(ASrcList));
  for i := 0 to Length(ASrcList) - 1 do
    AResult[Length(AResult) - Length(ASrcList) + i] := ASrcList[i];
end;

procedure CopyActionsToPointer(ASrcList: TActionArr; var AResult: PActionArr);
var
  i: Integer;
begin
  SetLength(AResult^, Length(ASrcList));
  for i := 0 to Length(ASrcList) - 1 do
    AResult^[i] := ASrcList[i];
end;

function GetEmptyAction(): TAction;
begin
  Result.Kind := actUnknown;
  Result.Key := #0;
  Result.KeyList := '';
  Result.PointKind:= pntUnknown;
  Result.PointRes.X := 0;
  Result.PointRes.Y := 0;
  Result.PointStr.X := '';
  Result.PointStr.Y := '';
  Result.LookKind := lookUnknown;
  Result.RectKind := rectUnknown;
  Result.ScreenRectRes.Left := 0;
  Result.ScreenRectRes.Right := 0;
  Result.ScreenRectRes.Top := 0;
  Result.ScreenRectRes.Bottom := 0;
  Result.ScreenRectStr := '0-0-0-0';
  Result.ScreenStep := 0;
  Result.PixelColor := 0;
  Result.LoopCount := 0;
  Result.Interval := 0;
  Result.Sleep := 0;
end;

function ParseAction(AStr: string): TAction;
var
  index: Integer;
  ArrTmp: StringArr;
begin
  try
    Result := GetEmptyAction();

    ArrTmp := StrSplit(AStr);
    if (ArrTmp[0][1] = '=') then
    begin
      Result.Kind := actChapter;
      // using KeyList for chapter name
      Result.KeyList := RemoveSpecSymbols(ArrTmp[0]);
      Exit;
    end;
    Result.Kind := ActionKindHelper.StrToActionKind(ArrTmp[0]);

    case Result.Kind of
      actLoop:
      begin
        Result.LoopCount := StrToInt(ArrTmp[1]);
      end;
      actTimer:
      begin
        Result.Interval := StrToInt(ArrTmp[1]);
      end;
      actPause:
      begin
        if (Length(ArrTmp) > 1) then
          Result.KeyList := ArrTmp[1];
      end;
      actKeyPressOne, actKeyDownOne, actKeyUpOne:
      begin
        Result.Key := ArrTmp[1][1];
        Result.Interval := StrToInt(ArrTmp[2]);
        Result.Sleep := StrToInt(ArrTmp[3]);
      end;
      actKeyPressMany:
      begin
        Result.KeyList := ArrTmp[1];
        Result.Interval := StrToInt(ArrTmp[2]);
        Result.Sleep := StrToInt(ArrTmp[3]);
      end;
      actMousePressLeft, actMousePressRight:
      begin
        Result.Interval := StrToInt(ArrTmp[1]);
        Result.Sleep := StrToInt(ArrTmp[2]);
      end;
      actMouseMove:
      begin
        index := 2;
        Result.PointKind := ActionKindHelper.StrToPointKind(ArrTmp[1]);
        if (Result.PointKind = pntCoordinates) then
        begin
          Result.PointStr.X := ArrTmp[2];
          Result.PointStr.Y := ArrTmp[3];
          index := 4;
        end;
        Result.Interval := StrToInt(ArrTmp[index]);
        Result.Sleep := StrToInt(ArrTmp[index + 1]);
      end;
      actMouseFloodLeft, actMouseFloodRight:
      begin
        Result.LookKind := ActionKindHelper.StrToLookKind(ArrTmp[1]);
        Result.RectKind := ActionKindHelper.StrToRectKind(ArrTmp[2]);
        index := 3;
        if (Result.RectKind = rectCustom) then
        begin
          Result.ScreenRectStr := ArrTmp[3];
          index := 4;
        end;
        Result.ScreenStep := StrToInt(ArrTmp[index]);
        Result.Interval := StrToInt(ArrTmp[index + 1]);
        Result.Sleep := StrToInt(ArrTmp[index + 2]);
      end;
      actLookForEmpty:
      begin
        Result.LookKind := ActionKindHelper.StrToLookKind(ArrTmp[1]);
        Result.RectKind := ActionKindHelper.StrToRectKind(ArrTmp[2]);
        if ((Result.LookKind = lookUnknown) OR (Result.RectKind = rectUnknown)) then
        begin
          Result.Kind := actBroken;
          Exit;
        end;
        index := 3;
        if (Result.RectKind = rectCustom) then
        begin
          Result.ScreenRectStr := ArrTmp[3];
          index := 4;
        end;
        Result.ScreenStep := StrToInt(ArrTmp[index]);
      end;
      actLookForPixel:
      begin
        Result.LookKind := ActionKindHelper.StrToLookKind(ArrTmp[1]);
        Result.RectKind := ActionKindHelper.StrToRectKind(ArrTmp[2]);
        index := 3;
        if (Result.RectKind = rectCustom) then
        begin
          Result.ScreenRectStr := ArrTmp[3];
          index := 4;
        end;
        Result.PixelColor := Hex2Dec(ArrTmp[index]);
        Result.ScreenStep := StrToInt(ArrTmp[index + 1]);
      end;
    end;
  except
    Result.Kind := actBroken;
  end;
end;

procedure ParseActionPointer(AStr: string; var AAction: PAction);
var
  ActionTmp: TAction;
begin
  ActionTmp := ParseAction(AStr);
  AAction^.Kind := ActionTmp.Kind;
  AAction^.Key := ActionTmp.Key;
  AAction^.KeyList := ActionTmp.KeyList;
  AAction^.PointKind := ActionTmp.PointKind;
  AAction^.PointRes.Create(ActionTmp.PointRes);
  AAction^.PointStr.X := ActionTmp.PointStr.X;
  AAction^.PointStr.Y := ActionTmp.PointStr.Y;
  AAction^.LookKind := ActionTmp.LookKind;
  AAction^.RectKind := ActionTmp.RectKind;
  AAction^.ScreenRectRes.Create(ActionTmp.ScreenRectRes);
  AAction^.ScreenRectStr := ActionTmp.ScreenRectStr;
  AAction^.ScreenStep := ActionTmp.ScreenStep;
  AAction^.PixelColor := ActionTmp.PixelColor;
  AAction^.Interval := ActionTmp.Interval;
  AAction^.Sleep := ActionTmp.Sleep;
  AAction^.LoopCount := ActionTmp.LoopCount;
end;

function ParseParams(AStr: string): TGameParams;
var
  i: Integer;
  ArrTmp: StringArr;
  PointTmp: TPoint;
begin
  Result.DoMaximize := false;
  Result.DoStartDisplayCenter := false;
  Result.DoStartWindowCenter := false;
  Result.DoWindowPosition := false;
  Result.DoWindowSize := false;

  ArrTmp := StrSplit(AStr);
  i := 0;
  while (i <= Length(ArrTmp) - 1) do
  begin
    if ((ArrTmp[i] = 'Maximize') OR (ArrTmp[i] = 'StartDisplayCenter') OR (ArrTmp[i] = 'StartWindowCenter')) then
    begin
      Result.DoMaximize := Result.DoMaximize OR (ArrTmp[i] = 'Maximize');
      Result.DoStartDisplayCenter := Result.DoStartDisplayCenter OR (ArrTmp[i] = 'StartDisplayCenter');
      Result.DoStartWindowCenter := Result.DoStartWindowCenter OR (ArrTmp[i] = 'StartWindowCenter');
      Inc(i);
      Continue;
    end;
    if ((ArrTmp[i] = 'WindowPosition') OR (ArrTmp[i] = 'WindowSize')) then
    begin
      PointTmp := TPoint.Create(StrToInt(ArrTmp[i + 1]), StrToInt(ArrTmp[i + 2]));
      if (ArrTmp[i] = 'WindowPosition') then
      begin
        Result.DoWindowPosition := true;
        Result.NewWindowPosition := PointTmp;
      end;
      if (ArrTmp[i] = 'WindowSize') then
      begin
        Result.DoWindowSize := true;
        Result.NewWindowSize := PointTmp;
      end;
      Inc(i, 3);
    end;
  end;
end;

function IsTrueParams(AGameParams: TGameParams): Boolean;
begin
  Result := AGameParams.DoMaximize
    OR AGameParams.DoStartDisplayCenter OR AGameParams.DoStartWindowCenter
    OR AGameParams.DoWindowPosition OR AGameParams.DoWindowSize;
end;

function ParamsToStr(AGameParams: TGameParams): string;
begin
  Result := ''; // TODO

end;

function ParsePointRes(AAction: TAction; ARect: TRect): TPoint;
var
  FormatSettings: TFormatSettings;
  Percent: Real;
  X, Y: Integer;
begin
  FormatSettings.DecimalSeparator := '.';
  case (AAction.PointKind) of
    pntDisplayCenter:
    begin
      Result := TPoint.Create(Screen.Width div 2, Screen.Height div 2);
    end;
    pntWindowCenter:
    begin
      Result := TPoint.Create(ARect.CenterPoint);
    end;
    pntCoordinates:
    begin
      if (CountCharInStr(AAction.PointStr.X, '%') > 0) then
      begin
        Percent := StrToFloat(RemovePercentChar(AAction.PointStr.X), FormatSettings);
        X := ARect.TopLeft.X + Round(ARect.Width * Percent / 100);
      end
      else
        X := ARect.TopLeft.X + StrToInt(AAction.PointStr.X);

      if (CountCharInStr(AAction.PointStr.Y, '%') > 0) then
      begin
        Percent := StrToFloat(RemovePercentChar(AAction.PointStr.Y), FormatSettings);
        Y := ARect.TopLeft.Y + Round(ARect.Height * Percent / 100);
      end
      else
        Y := ARect.TopLeft.Y + StrToInt(AAction.PointStr.Y);

      Result := TPoint.Create(X, Y);
    end;
  end;
end;

function ParseRectRes(AAction: TAction; ARect: TRect): TRect;
var
  StrArrTmp: StringArr;
begin
  case (AAction.RectKind) of
    rectFullScreen:
    begin
      Result := TRect.Create(0, 0, Screen.Width, Screen.Height);
    end;
    rectWindow:
    begin
      Result := TRect.Create(ARect);
    end;
    rectCustom:
    begin
      StrArrTmp := StrSplit(AAction.ScreenRectStr, '-');
      Result := TRect.Create(
        StrToInt(StrArrTmp[0]), StrToInt(StrArrTmp[2]),
        StrToInt(StrArrTmp[1]), StrToInt(StrArrTmp[3])
      );
    end;
  end;
end;

end.

