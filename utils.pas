unit Utils;

{$mode ObjFPC}{$H+}

interface

type
  CharArr = array of char;
  StringArr = array of string;

procedure FreeObject(AObj: TObject);
procedure FreePointer(APoint: Pointer);
function GetGamesPath(): string;
function GetGameName(AGameName: string; APath: string; AMask: string): string;
function CountCharInStr(AStr: string; AChar: char; IsBreakSearch: Boolean = false): Integer;
function CountHashTag(AStr: string): Integer;
function RemoveChar(AStr: string; AChar: char): string;
function RemoveSpecSymbols(AStr: string): string;
function RemovePercentChar(AStr: string): string;
function StrSplit(AStr: string; ADelimiter: char = ','): StringArr;
procedure SmartSleep(dt: DWord);

implementation

uses
  // system
  Classes, Forms, SysUtils, RegExpr;
  // main

// end uses

procedure FreeObject(AObj: TObject);
begin
  if (Assigned(AObj)) then
    FreeAndNil(AObj);
end;

procedure FreePointer(APoint: Pointer);
begin
  if (Assigned(APoint)) then
    FreeMemAndNil(APoint);
end;

function GetGamesPath(): string;
begin
  Result := ExtractFileDir(ParamStr(0)) + PathDelim + 'games';
end;

function GetGameName(AGameName: string; APath: string; AMask: string): string;
begin
  Result := StringReplace(AGameName, APath + PathDelim, '', [rfIgnoreCase]);
  Result := LeftStr(Result, Length(Result) - 4);
end;

function CountCharInStr(AStr: string; AChar: char; IsBreakSearch: Boolean = false): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(AStr) do
  begin
    if (AStr[i] = AChar) then
      Inc(Result)
    else
      if (IsBreakSearch) then
        Exit;
  end;
end;

function CountHashTag(AStr: string): Integer;
begin
  Result := CountCharInStr(AStr, '#', true);
end;

function RemoveChar(AStr: string; AChar: char): string;
begin
  Result := AStr;
  Result := StringReplace(Result, AChar, '', [rfReplaceAll]);
end;

function RemoveSpecSymbols(AStr: string): string;
begin
  Result := RemoveChar(AStr, '#');
  Result := RemoveChar(AStr, '=');
end;

function RemovePercentChar(AStr: string): string;
begin
  Result := RemoveChar(AStr, '%');
end;

function StrSplit(AStr: string; ADelimiter: char = ','): StringArr;
var
  i: Integer;
  StringListTmp: TStringList;
begin
  StringListTmp := TStringList.Create();
  StringListTmp.Delimiter := ADelimiter;
  StringListTmp.StrictDelimiter := true;
  StringListTmp.DelimitedText := AStr;
  SetLength(Result, StringListTmp.Count);
  for i := 0 to StringListTmp.Count - 1 do
    Result[i] := StringListTmp.Strings[i];
  FreeObject(StringListTmp);
end;

procedure SmartSleep(dt: DWord);
var
  tc : DWord;
begin
  tc := GetTickCount();
  repeat
    Application.ProcessMessages();
  until ((GetTickCount() > tc + dt) OR (Application.Terminated));
  // while ((GetTickCount() < tc + dt) AND (Not(Application.Terminated))) do
  //   Application.ProcessMessages;
end;

end.

