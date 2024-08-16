unit WindowListFormUnit;

{$mode ObjFPC}{$H+}

interface

uses
  // system
  Classes, ExtCtrls, Forms, LConvEncoding, StdCtrls, SysUtils, Windows;
  // main

type

  { TWindowListForm }

  TWindowListForm = class(TForm)
    btnChoose: TButton;
    lboxActiveWindow: TListBox;
    Timer1: TTimer;
    procedure btnChooseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private

  public

  end;

var
  WindowListForm: TWindowListForm;

function EnumWindowsProc(WHandle: HWND; LParM: LParam): LongBool; StdCall; Export;

implementation

uses
  // system

  // main
  GameDataClassUnit, LocalizationClassUnit;

{$R *.lfm}

{ TWindowListForm }

procedure TWindowListForm.FormCreate(Sender: TObject);
begin
  btnChoose.Caption := Localization.ChooseWindowButton;
end;

function EnumWindowsProc(WHandle: HWND; LParM: LParam): LongBool; StdCall;
var
  CharTitle: array[0..256] of char;
  StrTitle: string;
  CharLen: Integer;
  BoolOut: Boolean;
begin
  Result := True;
  CharLen := GetWindowText(wHandle, CharTitle, 256);
  StrTitle := ConvertEncodingToUTF8(CharTitle, 'cp' + IntToStr(GetACP()), BoolOut);
  if IsWindowVisible(wHandle) AND (StrTitle <> '') then
  begin
    WindowListForm.lboxActiveWindow.Items.Add(StrTitle);
  end;
end;

procedure TWindowListForm.Timer1Timer(Sender: TObject);
begin
  lboxActiveWindow.Clear;
  EnumWindows(@EnumWindowsProc, 0);
  Timer1.Enabled := false;
end;

procedure TWindowListForm.btnChooseClick(Sender: TObject);
var
  index: Integer;
begin
  index := lboxActiveWindow.ItemIndex;
  if (index < 0) then
    Exit
  else
  begin
    GameDataClass.GameProcess := lboxActiveWindow.Items.Strings[index];
    Close();
  end;
end;

end.

