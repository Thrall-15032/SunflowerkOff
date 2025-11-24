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
  KeyUpper: Char;
  KeyCode: DWord;
  KeyRecord: TINPUT;
  KeyRecordList: array of TINPUT;
begin
  KeyUpper := UpperCase(AKey)[1];
  KeyCode := Ord(KeyUpper);
  { keybd_event(KeyCode, MapVirtualKey(KeyCode, 0), KEYEVENTF_EXTENDEDKEY or AEvent, 0); }
  { MouseAndKeyInput.KeyInput.Down(KeyCode); }
  if (KeyUpper = AKey) then
  begin
    SetLength(KeyRecordList, 3);
    KeyRecordList[0]._Type := INPUT_KEYBOARD;
    KeyRecordList[0].ki.wVk := VK_SHIFT;
    KeyRecordList[0].ki.wScan := MapVirtualKey(VK_SHIFT, 0);
    KeyRecordList[0].ki.dwFlags := KEY_DOWN;
    KeyRecordList[0].ki.ExtraInfo := 0;
    KeyRecordList[0].ki.Time := 0;
    KeyRecordList[1]._Type := INPUT_KEYBOARD;
    KeyRecordList[1].ki.wVk := KeyCode;
    KeyRecordList[1].ki.wScan := MapVirtualKey(KeyCode, 0);
    KeyRecordList[1].ki.dwFlags := AEvent;
    KeyRecordList[1].ki.ExtraInfo := 0;
    KeyRecordList[1].ki.Time := 0;
    KeyRecordList[2]._Type := INPUT_KEYBOARD;
    KeyRecordList[2].ki.wVk := VK_SHIFT;
    KeyRecordList[2].ki.wScan := MapVirtualKey(VK_SHIFT, 0);
    KeyRecordList[2].ki.dwFlags := KEY_UP;
    KeyRecordList[2].ki.ExtraInfo := 0;
    KeyRecordList[2].ki.Time := 0;
    SendInput(3, @KeyRecordList[0], SizeOf(KeyRecordList[0]));
  end
  else
  begin
    KeyRecord._Type := INPUT_KEYBOARD;
    KeyRecord.ki.wVk := KeyCode;
    KeyRecord.ki.wScan := MapVirtualKey(KeyCode, 0);
    KeyRecord.ki.dwFlags := AEvent;
    KeyRecord.ki.ExtraInfo := 0;
    KeyRecord.ki.Time := 0;
    SendInput(1, @KeyRecord, SizeOf(KeyRecord));
  end;
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

