unit GameDataClassUnit;

{$mode ObjFPC}{$H+}

interface

uses
  // system
  Classes, ComCtrls, fgl, SysUtils,
  // main
  ActionClass, Utils;
// end uses

type
  TGameDataList = specialize TFPGMap<string, string>;

  { TGameDataClass }

  TGameDataClass = class
  private
    FGameDataList: TGameDataList;
    FCurrentGame: TStringList;
    FCurrentFileName: string;
    FOldGameName: string;
    FCurrentGameName: string;
    FOldGameProcess: string;
    FCurrentGameProcess: string;
    FOldGameParams: TGameParams;
    FCurrentGameParams: TGameParams;
    procedure setCurrentGameName(ANewGameName: string);
    procedure setCurrentGameProcess(ANewGameProcess: string);
    procedure InitVars();
    function FilterByHashTag(const s: string): Boolean;
  public
    constructor Create();
    destructor Destroy(); override;
    function GetList(): StringArr;
    procedure LoadFile(AGameName: string; TreeView: TTreeView);
    function IsChangedProcess(): Boolean;
    function IsChangedParams(): Boolean;
    procedure SaveChanges(AStringList: TStringList = nil; ANewFile: string = '');
    property GameName: string read FCurrentGameName write setCurrentGameName;
    property GameProcess: string read FCurrentGameProcess write setCurrentGameProcess;
    property GameParams: TGameParams read FCurrentGameParams write FCurrentGameParams;
  end;

var
  GameDataClass: TGameDataClass;

const
  DataFilesMask = '*.txt';

implementation

uses
  // system
  Dialogs, FileUtil,
  // main
  ActionKindUnit;
// end uses

{ TGameDataClass }

constructor TGameDataClass.Create();
begin
  InitVars();
end;

destructor TGameDataClass.Destroy();
begin
  FreeObject(FGameDataList);
  FreeObject(FCurrentGame);
  inherited Destroy();
end;

procedure TGameDataClass.InitVars();
begin
  FreeObject(FCurrentGame);
  FCurrentGame := TStringList.Create();
  FOldGameName := '';
  FCurrentGameName := '';
  FOldGameProcess := '';
  FCurrentGameProcess := '';
  FCurrentFileName := '';
  FOldGameParams.DoMaximize := false;
  FOldGameParams.DoStartWindowCenter:= false;
  FOldGameParams.DoStartDisplayCenter := false;
  FOldGameParams.DoWindowPosition := false;
  FOldGameParams.NewWindowPosition := TPoint.Create(0, 0);
  FOldGameParams.DoWindowSize:= false;
  FOldGameParams.NewWindowSize := TPoint.Create(0, 0);
  FCurrentGameParams.DoMaximize := false;
  FCurrentGameParams.DoStartWindowCenter:= false;
  FCurrentGameParams.DoStartDisplayCenter := false;
  FCurrentGameParams.DoWindowPosition := false;
  FCurrentGameParams.NewWindowPosition := TPoint.Create(0, 0);
  FCurrentGameParams.DoWindowSize:= false;
  FCurrentGameParams.NewWindowSize := TPoint.Create(0, 0);
end;

function TGameDataClass.GetList(): StringArr;
var
  i: Integer;
  CurDir: string;
  StrTmp: string;
  FilesList: TStringList;
begin
  FreeObject(FGameDataList);
  FGameDataList := TGameDataList.Create();

  CurDir := GetGamesPath();
  FilesList := TStringList.Create();
  FileUtil.FindAllFiles(FilesList, CurDir, DataFilesMask, False, 0);
  if (FilesList.Count = 0) then
  begin
    Result := [];
    FreeObject(FilesList);
    Exit;
  end;

  SetLength(Result, FilesList.Count);
  for i := 0 to FilesList.Count - 1 do
  begin
    StrTmp := GetGameName(FilesList[i], CurDir, DataFilesMask);
    Result[i] := StrTmp;
    FGameDataList.Add(StrTmp, FilesList[i]);
  end;
  FreeObject(FilesList);
end;

procedure TGameDataClass.LoadFile(AGameName: string; TreeView: TTreeView);
var
  i, j: Integer;
  StrTmp: string;
  ActionTmp: PAction;
  OneLine: string;
  HashCount: Integer;
  Nodes: array of TTreeNode;
begin
  InitVars();
  FCurrentFileName := FGameDataList.KeyData[AGameName];
  FCurrentGame.LoadFromFile(FCurrentFileName);

  for i := 0 to FCurrentGame.Count - 1 do
  begin
    OneLine := FCurrentGame.Strings[i];
    if (OneLine = '') then
      Continue;
    if (LeftStr(OneLine, 1) = '%') then
      Continue;
    if (LeftStr(OneLine, 4) = 'Name') then
    begin
      FCurrentGameName := StringReplace(OneLine, 'Name=', '', [rfIgnoreCase]);
      Continue;
    end;
    if (LeftStr(OneLine, 7) = 'Process') then
    begin
      FCurrentGameProcess := StringReplace(OneLine, 'Process=', '', [rfIgnoreCase]);
      Continue;
    end;
    if (LeftStr(OneLine, 6) = 'Params') then
    begin
      StrTmp := StringReplace(OneLine, 'Params=', '', [rfIgnoreCase]);
      FOldGameParams := ParseParams(StrTmp);
      FCurrentGameParams := ParseParams(StrTmp);
      Continue;
    end;
    HashCount := CountHashTag(OneLine);
    StrTmp := RightStr(OneLine, Length(OneLine) - HashCount);

    new(ActionTmp);
    ParseActionPointer(StrTmp, ActionTmp);
    if (ActionTmp^.Kind = actUnknown) then
    begin
      // Log unknown command
    end;

    if (HashCount = 0) then
      Continue;
    if (HashCount = 1) then
    begin
      SetLength(Nodes, 1);
      Nodes[0] := TreeView.Items.AddObject(nil, RemoveSpecSymbols(OneLine), ActionTmp);
      Nodes[0].StateIndex := 1;
    end
    else
    begin
      if (HashCount - 1 > Length(Nodes)) then
        Exit;
      SetLength(Nodes, HashCount);
      Nodes[HashCount - 1] := TreeView.Items.AddChildObject(Nodes[HashCount - 2], RemoveSpecSymbols(OneLine), ActionTmp);
      Nodes[HashCount - 1].StateIndex := 1;
    end;
  end;
  FOldGameName := FCurrentGameName;
  FOldGameProcess := FCurrentGameProcess;
end;

function TGameDataClass.IsChangedProcess(): Boolean;
begin
  Result := (FOldGameName <> FCurrentGameName) OR (FOldGameProcess <> FCurrentGameProcess);
end;

function TGameDataClass.IsChangedParams(): Boolean;
begin
  Result := ((FOldGameParams.DoMaximize <> FCurrentGameParams.DoMaximize)
    OR (FOldGameParams.DoStartDisplayCenter <> FCurrentGameParams.DoStartDisplayCenter)
    OR (FOldGameParams.DoStartWindowCenter <> FCurrentGameParams.DoStartWindowCenter)
    OR (FOldGameParams.DoWindowPosition <> FCurrentGameParams.DoWindowPosition)
    OR (NOT(PointsEqual(FOldGameParams.NewWindowPosition, FCurrentGameParams.NewWindowPosition)))
    OR (FOldGameParams.DoWindowSize <> FCurrentGameParams.DoWindowSize)
    OR (NOT(PointsEqual(FOldGameParams.NewWindowSize, FCurrentGameParams.NewWindowSize)))
  );
end;

procedure TGameDataClass.SaveChanges(AStringList: TStringList = nil; ANewFile: string = '');
var
  i: Integer;
  IndexName, IndexProcess, IndexParams: Integer;
  OneLine: string;
begin
  IndexName := -1;
  IndexProcess := -1;
  IndexParams := -1;
  for i := 0 to FCurrentGame.Count - 1 do
  begin
    OneLine := FCurrentGame.Strings[i];
    if (LeftStr(OneLine, 4) = 'Name') then
    begin
      FCurrentGame.Strings[i] := 'Name=' + FCurrentGameName;
      IndexName := i;
      Continue;
    end;
    if (LeftStr(OneLine, 7) = 'Process') then
    begin
      FCurrentGame.Strings[i] := 'Process=' + FCurrentGameProcess;
      IndexProcess := i;
      Continue;
    end;
    if (LeftStr(OneLine, 6) = 'Params') then
    begin
      FCurrentGame.Strings[i] := 'Params=' + ParamsToStr(FCurrentGameParams);
      IndexParams := i;
      Continue;
    end;
  end;

  if (AStringList <> nil) then
  begin
    FCurrentGame.AddStrings(FCurrentGame.Filter(@FilterByHashTag), true);
    FCurrentGame.AddStrings(AStringList);
  end;

  if ((ANewFile <> '') AND (FCurrentFileName = '')) then
    FCurrentGame.SaveToFile(ANewFile)
  else
    FCurrentGame.SaveToFile(FCurrentFileName);
end;

procedure TGameDataClass.setCurrentGameName(ANewGameName: string);
begin
  if (FCurrentGameName <> ANewGameName) then
    FCurrentGameName := ANewGameName;
end;

procedure TGameDataClass.setCurrentGameProcess(ANewGameProcess: string);
begin
  if (FCurrentGameProcess <> ANewGameProcess) then
    FCurrentGameProcess := ANewGameProcess;
end;

function TGameDataClass.FilterByHashTag(const s: string): Boolean;
begin
  Result := ((Length(s) > 0) AND (s[1] <> '#'));
end;

end.

