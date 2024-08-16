unit KeyFunctionUnit;

{$mode ObjFPC}{$H+}

interface

uses
  // system
  Windows,
  // main
  ActionClass;
// end uses

const
  KEY_DOWN = 0;
  KEY_UP = KEYEVENTF_KEYUP;

procedure DoKeyEvent(AKey: char; AEvent: Integer);

procedure KeyDownOne(AAction: TAction);
procedure KeyUpOne(AAction: TAction);
procedure KeyPressOne(AAction: TAction);
procedure KeyPressMany(AAction: TAction);

implementation

uses
  // system
  MouseAndKeyInput, Dialogs, SysUtils,
  // main
  Utils;
// end uses

procedure DoKeyEvent(AKey: char; AEvent: Integer);
var
  KeyCode: DWord;
  KeyRecord: TINPUT;
begin
  KeyCode := Ord(AKey);
  { keybd_event(KeyCode, MapVirtualKey(KeyCode, 0), KEYEVENTF_EXTENDEDKEY or AEvent, 0); }
  { MouseAndKeyInput.KeyInput.Down(KeyCode); }

  KeyRecord._Type := INPUT_KEYBOARD;
  KeyRecord.ki.wVk := KeyCode;
  KeyRecord.ki.wScan := MapVirtualKey(KeyCode, 0);
  KeyRecord.ki.dwFlags := AEvent;
  KeyRecord.ki.ExtraInfo := 0;
  KeyRecord.ki.Time := 0;
  SendInput(1, @KeyRecord, SizeOf(KeyRecord));
end;

procedure KeyDownOne(AAction: TAction);
begin
  DoKeyEvent(AAction.Key, KEY_DOWN);
  SmartSleep(AAction.Sleep);
end;

procedure KeyUpOne(AAction: TAction);
begin
  DoKeyEvent(AAction.Key, KEY_UP);
  SmartSleep(AAction.Sleep);
end;

procedure KeyPressOne(AAction: TAction);
begin
  DoKeyEvent(AAction.Key, KEY_DOWN);
  SmartSleep(AAction.Interval);
  DoKeyEvent(AAction.Key, KEY_UP);
  SmartSleep(AAction.Sleep);
end;

procedure KeyPressMany(AAction: TAction);
var
  i: Integer;
begin
  for i := 0 to Length(AAction.KeyList) - 1 do
    DoKeyEvent(AAction.KeyList[i], KEY_DOWN);
  SmartSleep(AAction.Interval);
  for i := Length(AAction.KeyList) - 1 downto 0 do
    DoKeyEvent(AAction.KeyList[i], KEY_UP);
  SmartSleep(AAction.Sleep);
end;

end.

