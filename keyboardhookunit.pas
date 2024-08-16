unit KeyboardHookUnit;

{$mode ObjFPC}{$H+}

interface

uses
  // system
  Windows;
  // main


type

  TKeyboardDllHookRecord = packed record
    vkCode: DWord;
    scanCode: DWord;
    flags: DWord;
    time: DWord;
    dwExtraInfo: DWord;
  end;
  PKeyboardDllHookRecord = ^TKeyboardDllHookRecord;

  { TKeyboardHookClass }

  TKeyboardHookClass = class
  private
    FHookHandler: HHOOK;
    FIsStopExecution: Boolean;
    FIsContinueExecution: Boolean;
  public
    constructor Create();
    destructor Destroy();
    procedure ClearExecutionFlag();
    procedure ClearContinueFlag();
    procedure StopExecution();
    property IsStopExecution: Boolean read FIsStopExecution;
    property IsContinueExecution: Boolean read FIsContinueExecution;
  end;

const
  WH_KEYBOARD_LL = 13;

var
  KeyboardHookClass: TKeyboardHookClass;

implementation

function HookFunc(nCode: Integer; wParam: WPARAM; lParam: LPARAM): Int64; stdcall;
var
  VirtualKey: Integer;
  HookRecord: PKeyboardDllHookRecord;
begin
  HookRecord := PKeyboardDllHookRecord(Pointer(lParam));
  if nCode = HC_ACTION then
  begin
    VirtualKey := HookRecord^.vkCode;
    if ((wParam = WM_KEYUP) and (VirtualKey = VK_F4)) then
      KeyboardHookClass.FIsStopExecution := true;
    if ((wParam = WM_KEYUP) and (VirtualKey = VK_F3)) then
      KeyboardHookClass.FIsContinueExecution := true;
  end;
  Result := CallNextHookEx(KeyboardHookClass.FHookHandler, nCode, wParam, lParam);
end;

{ TKeyboardHookClass }

constructor TKeyboardHookClass.Create();
begin
  FHookHandler := SetWindowsHookEx(WH_KEYBOARD_LL, @HookFunc, HInstance, 0);
end;

destructor TKeyboardHookClass.Destroy();
begin
  UnhookWindowsHookEx(FHookHandler);
end;

procedure TKeyboardHookClass.StopExecution();
begin
  FIsStopExecution := true;
end;

procedure TKeyboardHookClass.ClearExecutionFlag();
begin
  FIsStopExecution := false;
end;

procedure TKeyboardHookClass.ClearContinueFlag();
begin
  FIsContinueExecution := false;
end;

end.

