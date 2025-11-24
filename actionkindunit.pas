unit ActionKindUnit;

{$mode ObjFPC}{$H+}

interface

uses
  // system
  Classes, SysUtils, fgl;
  // main

// end uses

type
  TActionKind = (
    actUnknown, actBroken,
    actChapter, actLoop, actInfinityLoop, actTimer, actPause,
    actKeyDownOne, actKeyUpOne, actKeyPressOne, actKeyPressMany,
    actMouseMove, actLookForEmpty, actLookForPixel,
    actMousePressLeft, actMouseDownLeft, actMouseUpLeft,
    actMousePressRight, actMouseDownRight, actMouseUpRight,
    actMouseFloodLeft, actMouseFloodRight
  );

  TPointKind = (pntUnknown, pntCoordinates, pntDisplayCenter, pntWindowCenter);

  TLookKind = (
    lookUnknown, lookHorizontalLeftRight, lookHorizontalPingPong,
    lookVerticalLeftRight, lookVerticalPingPong, lookSpiral
  );

  TRectKind = (rectUnknown, rectFullScreen, rectWindow, rectCustom);

  TKindRecord = record
    Code: string;
    Caption: string;
  end;

  TStringActionKindMap = specialize TFPGMap<string, TActionKind>;
  TActionKindStringMap = specialize TFPGMap<TActionKind, TKindRecord>;
  TStringPointKindMap = specialize TFPGMap<string, TPointKind>;
  TPointKindStringMap = specialize TFPGMap<TPointKind, TKindRecord>;
  TStringLookKindMap = specialize TFPGMap<string, TLookKind>;
  TLookKindStringMap = specialize TFPGMap<TLookKind, TKindRecord>;
  TStringRectKindMap = specialize TFPGMap<string, TRectKind>;
  TRectKindStringMap = specialize TFPGMap<TRectKind, TKindRecord>;

  { TActionKindHolder }

  TActionKindHolder = class
  public
    FActionKind: TActionKind;
    constructor Create(AActionKind: TActionKind);
  end;

  { TPointKindHolder }

  TPointKindHolder = class
  public
    FPointKind: TPointKind;
    constructor Create(APointKind: TPointKind);
  end;

  { TActionKindHelper }

  TActionKindHelper = class
  private
    FStringActionKindMap: TStringActionKindMap;
    FActionKindStringMap: TActionKindStringMap;
    FStringPointKindMap: TStringPointKindMap;
    FPointKindStringMap: TPointKindStringMap;
    FStringLookKindMap: TStringLookKindMap;
    FLookKindStringMap: TLookKindStringMap;
    FStringRectKindMap: TStringRectKindMap;
    FRectKindStringMap: TRectKindStringMap;
    function MakeRecord(ACode: string; ACaption: string): TKindRecord;
  public
    constructor Create();
    destructor Destroy(); override;
    function StrToActionKind(AStr: string): TActionKind;
    function ActionKindToStr(AActionKind: TActionKind): TKindRecord;
    function StrToPointKind(AStr: string): TPointKind;
    function PointKindToStr(APointKind: TPointKind): TKindRecord;
    function StrToLookKind(AStr: string): TLookKind;
    function LookKindToStr(ALookKind: TLookKind): TKindRecord;
    function StrToRectKind(AStr: string): TRectKind;
    function RectKindToStr(ARectKind: TRectKind): TKindRecord;
  end;

var
  ActionKindHelper: TActionKindHelper;

implementation

uses
  // system

  // main
  Utils;
// end uses

{ TActionKindHolder }

constructor TActionKindHolder.Create(AActionKind: TActionKind);
begin
  FActionKind := AActionKind;
end;

{ TPointKindHolder }

constructor TPointKindHolder.Create(APointKind: TPointKind);
begin
  FPointKind := APointKind;
end;

{ TActionKindHelper }

constructor TActionKindHelper.Create();
begin
  FStringActionKindMap := TStringActionKindMap.Create();
  FActionKindStringMap := TActionKindStringMap.Create();
  FStringPointKindMap := TStringPointKindMap.Create();
  FPointKindStringMap := TPointKindStringMap.Create();
  FStringLookKindMap := TStringLookKindMap.Create();
  FLookKindStringMap := TLookKindStringMap.Create();
  FStringRectKindMap := TStringRectKindMap.Create();
  FRectKindStringMap := TRectKindStringMap.Create();

  FStringActionKindMap.Add('Unknown', actUnknown);
  FStringActionKindMap.Add('Broken', actBroken);
  FStringActionKindMap.Add('Chapter', actChapter);
  FStringActionKindMap.Add('Loop', actLoop);
  FStringActionKindMap.Add('InfinityLoop', actInfinityLoop);
  FStringActionKindMap.Add('Timer', actTimer);
  FStringActionKindMap.Add('Pause', actPause);
  FStringActionKindMap.Add('KeyPressOne', actKeyPressOne);
  FStringActionKindMap.Add('KeyDownOne', actKeyDownOne);
  FStringActionKindMap.Add('KeyUpOne', actKeyUpOne);
  FStringActionKindMap.Add('KeyPressMany', actKeyPressMany);
  FStringActionKindMap.Add('MouseMove', actMouseMove);
  FStringActionKindMap.Add('LookForEmpty', actLookForEmpty);
  FStringActionKindMap.Add('LookForPixel', actLookForPixel);
  FStringActionKindMap.Add('MousePressLeft', actMousePressLeft);
  FStringActionKindMap.Add('MouseDownLeft', actMouseDownLeft);
  FStringActionKindMap.Add('MouseUpLeft', actMouseUpLeft);
  FStringActionKindMap.Add('MousePressRight', actMousePressRight);
  FStringActionKindMap.Add('MouseDownRight', actMouseDownRight);
  FStringActionKindMap.Add('MouseUpRight', actMouseUpRight);
  FStringActionKindMap.Add('MouseFloodLeft', actMouseFloodLeft);
  FStringActionKindMap.Add('MouseFloodRight', actMouseFloodRight);

  // TODO: add localization captions
  FActionKindStringMap.Add(actUnknown, MakeRecord('Unknown', 'Unknown'));
  FActionKindStringMap.Add(actBroken, MakeRecord('Broken', 'Broken'));
  FActionKindStringMap.Add(actChapter, MakeRecord('Chapter', 'Chapter'));
  FActionKindStringMap.Add(actLoop, MakeRecord('Loop', 'Loop'));
  FActionKindStringMap.Add(actInfinityLoop, MakeRecord('InfinityLoop', 'InfinityLoop'));
  FActionKindStringMap.Add(actTimer, MakeRecord('Timer', 'Timer'));
  FActionKindStringMap.Add(actPause, MakeRecord('Pause', 'Pause'));
  FActionKindStringMap.Add(actKeyPressOne, MakeRecord('KeyPressOne', 'KeyPressOne'));
  FActionKindStringMap.Add(actKeyDownOne, MakeRecord('KeyDownOne', 'KeyDownOne'));
  FActionKindStringMap.Add(actKeyUpOne, MakeRecord('KeyUpOne', 'KeyUpOne'));
  FActionKindStringMap.Add(actKeyPressMany, MakeRecord('KeyPressMany', 'KeyPressMany'));
  FActionKindStringMap.Add(actMouseMove, MakeRecord('MouseMove', 'MouseMove'));
  FActionKindStringMap.Add(actLookForEmpty, MakeRecord('LookForEmpty', 'LookForEmpty'));
  FActionKindStringMap.Add(actLookForPixel, MakeRecord('LookForPixel', 'LookForPixel'));
  FActionKindStringMap.Add(actMousePressLeft, MakeRecord('MousePressLeft', 'MousePressLeft'));
  FActionKindStringMap.Add(actMouseDownLeft, MakeRecord('MouseDownLeft', 'MouseDownLeft'));
  FActionKindStringMap.Add(actMouseUpLeft, MakeRecord('MouseUpLeft', 'MouseUpLeft'));
  FActionKindStringMap.Add(actMousePressRight, MakeRecord('MousePressRight', 'MousePressRight'));
  FActionKindStringMap.Add(actMouseDownRight, MakeRecord('MouseDownRight', 'MouseDownRight'));
  FActionKindStringMap.Add(actMouseUpRight, MakeRecord('MouseUpRight', 'MouseUpRight'));
  FActionKindStringMap.Add(actMouseFloodLeft, MakeRecord('MouseFloodLeft', 'MouseFloodLeft'));
  FActionKindStringMap.Add(actMouseFloodRight, MakeRecord('MouseFloodRight', 'MouseFloodRight'));

  FStringPointKindMap.Add('Unknown', pntUnknown);
  FStringPointKindMap.Add('DisplayCenter', pntDisplayCenter);
  FStringPointKindMap.Add('WindowCenter', pntWindowCenter);
  FStringPointKindMap.Add('Coordinates', pntCoordinates);

  // TODO: add localization captions
  FPointKindStringMap.Add(pntUnknown, MakeRecord('Unknown', 'Unknown'));
  FPointKindStringMap.Add(pntDisplayCenter, MakeRecord('DisplayCenter', 'DisplayCenter'));
  FPointKindStringMap.Add(pntWindowCenter, MakeRecord('WindowCenter', 'WindowCenter'));
  FPointKindStringMap.Add(pntCoordinates, MakeRecord('Coordinates', 'Coordinates'));

  FStringLookKindMap.Add('Unknown', lookUnknown);
  FStringLookKindMap.Add('HorizontalLeftRight', lookHorizontalLeftRight);
  FStringLookKindMap.Add('HorizontalPingPong', lookHorizontalPingPong);
  FStringLookKindMap.Add('VerticalLeftRight', lookVerticalLeftRight);
  FStringLookKindMap.Add('VerticalPingPong', lookVerticalPingPong);
  FStringLookKindMap.Add('Spiral', lookSpiral);

  // TODO: add localization captions
  FLookKindStringMap.Add(lookUnknown, MakeRecord('Unknown', 'Unknown'));
  FLookKindStringMap.Add(lookHorizontalLeftRight, MakeRecord('HorizontalLeftRight', 'HorizontalLeftRight'));
  FLookKindStringMap.Add(lookHorizontalPingPong, MakeRecord('HorizontalPingPong', 'HorizontalPingPong'));
  FLookKindStringMap.Add(lookVerticalLeftRight, MakeRecord('VerticalLeftRight', 'VerticalLeftRight'));
  FLookKindStringMap.Add(lookVerticalPingPong, MakeRecord('VerticalPingPong', 'VerticalPingPong'));
  FLookKindStringMap.Add(lookSpiral, MakeRecord('Spiral', 'Spiral'));

  FStringRectKindMap.Add('Unknown', rectUnknown);
  FStringRectKindMap.Add('FullScreen', rectFullScreen);
  FStringRectKindMap.Add('WindowRect', rectWindow);
  FStringRectKindMap.Add('CustomRect', rectCustom);

  // TODO: add localization captions
  FRectKindStringMap.Add(rectUnknown, MakeRecord('Unknown', 'Unknown'));
  FRectKindStringMap.Add(rectFullScreen, MakeRecord('FullScreen', 'FullScreen'));
  FRectKindStringMap.Add(rectWindow, MakeRecord('WindowRect', 'WindowRect'));
  FRectKindStringMap.Add(rectCustom, MakeRecord('CustomRect', 'CustomRect'));
end;

destructor TActionKindHelper.Destroy();
begin
  FreeObject(FStringActionKindMap);
  FreeObject(FActionKindStringMap);
  FreeObject(FStringPointKindMap);
  FreeObject(FPointKindStringMap);
  FreeObject(FStringLookKindMap);
  FreeObject(FLookKindStringMap);
  FreeObject(FStringRectKindMap);
  FreeObject(FRectKindStringMap);
  inherited Destroy();
end;

function TActionKindHelper.MakeRecord(ACode: string; ACaption: string): TKindRecord;
begin
  Result.Code := ACode;
  Result.Caption := ACaption;
end;

function TActionKindHelper.StrToActionKind(AStr: string): TActionKind;
begin
  Result := actBroken;
  if (FStringActionKindMap.IndexOf(AStr) >= 0) then
    Result := FStringActionKindMap.KeyData[AStr];
end;

function TActionKindHelper.ActionKindToStr(AActionKind: TActionKind): TKindRecord;
begin
  if (FActionKindStringMap.IndexOf(AActionKind) >= 0) then
    Result := FActionKindStringMap.KeyData[AActionKind];
end;

function TActionKindHelper.StrToPointKind(AStr: string): TPointKind;
begin
  Result := pntUnknown;
  if (FStringPointKindMap.IndexOf(AStr) >= 0) then
    Result := FStringPointKindMap.KeyData[AStr];
end;

function TActionKindHelper.PointKindToStr(APointKind: TPointKind): TKindRecord;
begin
  if (FPointKindStringMap.IndexOf(APointKind) >= 0) then
    Result := FPointKindStringMap.KeyData[APointKind];
end;

function TActionKindHelper.StrToLookKind(AStr: string): TLookKind;
begin
  Result := lookUnknown;
  if (FStringLookKindMap.IndexOf(AStr) >= 0) then
    Result := FStringLookKindMap.KeyData[AStr];
end;

function TActionKindHelper.LookKindToStr(ALookKind: TLookKind): TKindRecord;
begin
  if (FLookKindStringMap.IndexOf(ALookKind) >= 0) then
    Result := FLookKindStringMap.KeyData[ALookKind];
end;

function TActionKindHelper.StrToRectKind(AStr: string): TRectKind;
begin
  Result := rectUnknown;
  if (FStringRectKindMap.IndexOf(AStr) >= 0) then
    Result := FStringRectKindMap.KeyData[AStr];
end;

function TActionKindHelper.RectKindToStr(ARectKind: TRectKind): TKindRecord;
begin
  if (FRectKindStringMap.IndexOf(ARectKind) >= 0) then
    Result := FRectKindStringMap.KeyData[ARectKind];
end;

end.

