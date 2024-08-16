unit MainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  // system
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, Buttons, Menus, Windows,
  // main
  ActionClass;
// end uses


type

  { TMainForm }

  TMainForm = class(TForm)
    btnCollapse: TBitBtn;
    btnFindProcess: TBitBtn;
    btnMoveUp: TBitBtn;
    btnMoveDown: TBitBtn;
    btnUncheck: TBitBtn;
    btnSave: TBitBtn;
    btnNone: TBitBtn;
    btnAdd: TBitBtn;
    btnEdit: TBitBtn;
    btnDelete: TBitBtn;
    btnStart: TButton;
    btnStop: TButton;
    chbDoMaximize: TCheckBox;
    chbDoStartDisplayCenter: TCheckBox;
    chbDoStartWindowCenter: TCheckBox;
    chbDoWindowPosition: TCheckBox;
    chbDoWindowSize: TCheckBox;
    cmbGameList: TComboBox;
    edtWindowPosX: TEdit;
    edtWindowPosY: TEdit;
    edtWindowSizeX: TEdit;
    edtWindowSizeY: TEdit;
    edtGameName: TEdit;
    edtGameProcess: TEdit;
    ImageList1: TImageList;
    lblGameName: TLabel;
    lblGameProcess: TLabel;
    lblProcessTest: TLabel;
    lblChooseGame: TLabel;
    menCollapseChapters: TMenuItem;
    menExpandAll: TMenuItem;
    menExpandChapters: TMenuItem;
    menCollapseAll: TMenuItem;
    menCheckAll: TMenuItem;
    menCheckActions: TMenuItem;
    menUncheckAll: TMenuItem;
    menUncheckChapters: TMenuItem;
    pageProcess: TPageControl;
    pnlManagement: TPanel;
    popupCollapseExpand: TPopupMenu;
    popupCheckUncheck: TPopupMenu;
    dlgSaveNewFile: TSaveDialog;
    tabProcess: TTabSheet;
    tabParams: TTabSheet;
    TreeView1: TTreeView;
    procedure btnAddClick(Sender: TObject);
    procedure btnCollapseClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnFindProcessClick(Sender: TObject);
    procedure btnMoveDownClick(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnUncheckClick(Sender: TObject);
    procedure chbDoMaximizeChange(Sender: TObject);
    procedure chbDoStartDisplayCenterChange(Sender: TObject);
    procedure chbDoStartWindowCenterChange(Sender: TObject);
    procedure chbDoWindowPositionChange(Sender: TObject);
    procedure chbDoWindowSizeChange(Sender: TObject);
    procedure cmbGameListChange(Sender: TObject);
    procedure edtGameNameChange(Sender: TObject);
    procedure edtGameProcessChange(Sender: TObject);
    procedure edtWindowPosChange(Sender: TObject);
    procedure edtWindowSizeChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure menCheckActionsClick(Sender: TObject);
    procedure menCheckAllClick(Sender: TObject);
    procedure menCollapseAllClick(Sender: TObject);
    procedure menCollapseChaptersClick(Sender: TObject);
    procedure menExpandAllClick(Sender: TObject);
    procedure menExpandChaptersClick(Sender: TObject);
    procedure menUncheckAllClick(Sender: TObject);
    procedure menUncheckChaptersClick(Sender: TObject);
    procedure TreeView1AdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure TreeView1Click(Sender: TObject);
  private
    FIsTreeWasChanged: Boolean;
    FTreeViewGuid: TGUID;
    procedure InitForm();
    procedure CheckProcess();
    procedure CheckUnsaved();
    procedure SaveAllChanges(ANewFile: string = '');
    procedure ToggleTreeViewCheckBoxes(Node: TTreeNode);
  public
  end;

var
  MainForm: TMainForm;

implementation

uses
  //system
  Themes, RegExpr,
  //main
  ActionKindUnit,
  ActionExecutorUnit, KeyboardHookUnit, ActionEditFormUnit, ActionFunctionMapUnit,
  SettingsUnit,
  Utils, WindowListFormUnit, GameDataClassUnit, LocalizationClassUnit;
// end uses

{$R *.lfm}

{ TMainForm }

procedure TMainForm.InitForm();
var
  games: array of string;
begin
  lblChooseGame.Caption := Localization.ChooseGameLabel;
  lblGameName.Caption := Localization.GameNameLabel;
  edtGameName.Text := '';
  lblGameProcess.Caption := Localization.GameProcessLabel;
  edtGameProcess.Text := '';
  btnStart.Caption := Localization.StartButton;
  btnStart.Enabled := false;
  pageProcess.ActivePageIndex := 0;
  tabProcess.Caption := Localization.ProcessTab;
  tabParams.Caption := Localization.ParamsTab;
  menCheckAll.Caption := Localization.CheckAllMenu;
  menCheckActions.Caption := Localization.CheckActionsMenu;
  menUncheckAll.Caption := Localization.UncheckAllMenu;
  menUncheckChapters.Caption := Localization.UncheckChaptersMenu;
  menExpandAll.Caption := Localization.ExpandAllMenu;
  menExpandChapters.Caption := Localization.ExpandChaptersMenu;
  menCollapseAll.Caption := Localization.CollapseAllMenu;
  menCollapseChapters.Caption := Localization.CollapseChaptersMenu;
  chbDoMaximize.Caption := Localization.GameDoMaximizeLabel;
  chbDoStartDisplayCenter.Caption := Localization.GameDoStartDisplayCenterLabel;
  chbDoStartWindowCenter.Caption := Localization.GameDoStartWindowCenterLabel;
  chbDoWindowPosition.Caption := Localization.GameDoWindowPositionLabel;
  chbDoWindowSize.Caption := Localization.GameDoWindowSizeLabel;
  edtWindowPosX.Text := '';
  edtWindowPosY.Text := '';
  edtWindowSizeX.Text := '';
  edtWindowSizeY.Text := '';

  FTreeViewGuid := TGUID.NewGuid();
  games := GameDataClass.getList();
  cmbGameList.Items.AddStrings(games);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  AppSettings := TAppSettings.Create();
  Localization := TLocalization.Create();
  GameDataClass := TGameDataClass.Create();
  KeyboardHookClass := TKeyboardHookClass.Create();
  ActionKindHelper := TActionKindHelper.Create();
  ActionExecutorClass := TActionExecutorClass.Create();
  ActionFunctionClass := TActionFunctionClass.Create();
  InitForm();
  SetPriorityClass(GetCurrentThreadID(), THREAD_PRIORITY_HIGHEST);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeObject(ActionFunctionClass);
  FreeObject(ActionExecutorClass);
  FreeObject(ActionKindHelper);
  FreeObject(KeyboardHookClass);
  FreeObject(GameDataClass);
  FreeObject(Localization);
  FreeObject(AppSettings);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CheckUnsaved();
  CanClose := true;
end;

procedure TMainForm.CheckUnsaved();
var
  MsgBoxResult: TModalResult;
begin
  if (GameDataClass.IsChangedProcess() OR GameDataClass.IsChangedParams()) then
  begin
    if (cmbGameList.Text <> '') then
    begin
      MsgBoxResult := MessageDlg(
        '',
        Localization.UnsavedChangesMessage + ' (' + cmbGameList.Text + RemoveChar(DataFilesMask, '*') + ')',
        mtInformation, [mbYes, mbNo], 0
      );
      if (MsgBoxResult = mrYes) then
        SaveAllChanges();
    end
    else
    begin
      MsgBoxResult := MessageDlg(
        '',
        Localization.UnsavedToNewFileMessage,
        mtInformation, [mbYes, mbNo], 0
      );
      if ((MsgBoxResult = mrYes) AND dlgSaveNewFile.Execute()) then
      begin
        SaveAllChanges(dlgSaveNewFile.FileName);
      end;
    end;
  end;
end;

procedure TMainForm.SaveAllChanges(ANewFile: string = '');
var
  i, depth: Integer;
  ActionTmp: TAction;
  Prefix: string;
  StrList: TStringList;
begin
  StrList := TStringList.Create();
  for i := 0 to TreeView1.Items.Count - 1 do
  begin
    ActionTmp := TAction(TreeView1.Items.Item[i].Data^);
    depth := CountCharInStr(TreeView1.Items.Item[i].GetTextPath(), '/') + 1;
    Prefix := StringOfChar('#', depth);
    if (ActionTmp.Kind = actChapter) then
      Prefix := Prefix + '=';
    StrList.Add(Prefix + TreeView1.Items.Item[i].Text);
  end;
  GameDataClass.SaveChanges(StrList, ANewFile);
  FreeObject(StrList);
end;

procedure TMainForm.cmbGameListChange(Sender: TObject);
begin
  TreeView1.Items.Clear();
  if (cmbGameList.ItemIndex = -1) then
    Exit;

  FTreeViewGuid := TGUID.NewGuid();
  FIsTreeWasChanged := false;
  GameDataClass.LoadFile(cmbGameList.Text, TreeView1);
  edtGameName.Text := GameDataClass.GameName;
  edtGameProcess.Text := GameDataClass.GameProcess;
  lblProcessTest.Caption := '';
  btnStart.Enabled := false;
  CheckProcess();
end;

procedure TMainForm.edtGameNameChange(Sender: TObject);
begin
  GameDataClass.GameName := edtGameName.Text;
  if (GameDataClass.IsChangedProcess()) then
    tabProcess.Caption := Localization.ProcessTab + '*'
  else
    tabProcess.Caption := Localization.ProcessTab;
end;

procedure TMainForm.edtGameProcessChange(Sender: TObject);
begin
  GameDataClass.GameProcess := edtGameProcess.Text;
  if (GameDataClass.IsChangedProcess()) then
    tabProcess.Caption := Localization.ProcessTab + '*'
  else
    tabProcess.Caption := Localization.ProcessTab;
end;

procedure TMainForm.chbDoMaximizeChange(Sender: TObject);
var
  GameParamsTmp: TGameParams;
begin
  GameParamsTmp := GameDataClass.GameParams;
  GameParamsTmp.DoMaximize := chbDoMaximize.Checked;
  GameDataClass.GameParams := GameParamsTmp;
  if (GameDataClass.IsChangedParams()) then
    tabParams.Caption := Localization.ParamsTab + '*'
  else
    tabParams.Caption := Localization.ParamsTab;
end;

procedure TMainForm.chbDoStartDisplayCenterChange(Sender: TObject);
var
  GameParamsTmp: TGameParams;
begin
  GameParamsTmp := GameDataClass.GameParams;
  GameParamsTmp.DoStartDisplayCenter := chbDoStartDisplayCenter.Checked;
  GameDataClass.GameParams := GameParamsTmp;
  if (GameDataClass.IsChangedParams()) then
    tabParams.Caption := Localization.ParamsTab + '*'
  else
    tabParams.Caption := Localization.ParamsTab;
end;

procedure TMainForm.chbDoStartWindowCenterChange(Sender: TObject);
var
  GameParamsTmp: TGameParams;
begin
  GameParamsTmp := GameDataClass.GameParams;
  GameParamsTmp.DoStartWindowCenter := chbDoStartWindowCenter.Checked;
  GameDataClass.GameParams := GameParamsTmp;
  if (GameDataClass.IsChangedParams()) then
    tabParams.Caption := Localization.ParamsTab + '*'
  else
    tabParams.Caption := Localization.ParamsTab;
end;

procedure TMainForm.chbDoWindowPositionChange(Sender: TObject);
var
  GameParamsTmp: TGameParams;
begin
  GameParamsTmp := GameDataClass.GameParams;
  GameParamsTmp.DoWindowPosition := chbDoWindowPosition.Checked;
  GameDataClass.GameParams := GameParamsTmp;
  if (GameDataClass.IsChangedParams()) then
    tabParams.Caption := Localization.ParamsTab + '*'
  else
    tabParams.Caption := Localization.ParamsTab;
end;

procedure TMainForm.chbDoWindowSizeChange(Sender: TObject);
var
  GameParamsTmp: TGameParams;
begin
  GameParamsTmp := GameDataClass.GameParams;
  GameParamsTmp.DoWindowSize := chbDoWindowSize.Checked;
  GameDataClass.GameParams := GameParamsTmp;
  if (GameDataClass.IsChangedParams()) then
    tabParams.Caption := Localization.ParamsTab + '*'
  else
    tabParams.Caption := Localization.ParamsTab;
end;

procedure TMainForm.edtWindowPosChange(Sender: TObject);
var
  GameParamsTmp: TGameParams;
begin
  edtWindowPosX.Text := ReplaceRegExpr('[^0-9]', edtWindowPosX.Text, '');
  edtWindowPosY.Text := ReplaceRegExpr('[^0-9]', edtWindowPosY.Text, '');
  if ((edtWindowPosX.Text = '') OR (edtWindowPosY.Text = '')) then
    Exit;

  GameParamsTmp := GameDataClass.GameParams;
  GameParamsTmp.NewWindowPosition := TPoint.Create(StrToInt(edtWindowPosX.Text), StrToInt(edtWindowPosY.Text));
  GameDataClass.GameParams := GameParamsTmp;
  if (GameDataClass.IsChangedParams()) then
    tabParams.Caption := Localization.ParamsTab + '*'
  else
    tabParams.Caption := Localization.ParamsTab;
end;

procedure TMainForm.edtWindowSizeChange(Sender: TObject);
var
  GameParamsTmp: TGameParams;
begin
  edtWindowSizeX.Text := ReplaceRegExpr('[^0-9]', edtWindowSizeX.Text, '');
  edtWindowSizeY.Text := ReplaceRegExpr('[^0-9]', edtWindowSizeY.Text, '');
  if ((edtWindowSizeX.Text = '') OR (edtWindowSizeY.Text = '')) then
    Exit;

  GameParamsTmp := GameDataClass.GameParams;
  GameParamsTmp.NewWindowSize := TPoint.Create(StrToInt(edtWindowSizeX.Text), StrToInt(edtWindowSizeY.Text));
  GameDataClass.GameParams := GameParamsTmp;
  if (GameDataClass.IsChangedParams()) then
    tabParams.Caption := Localization.ParamsTab + '*'
  else
    tabParams.Caption := Localization.ParamsTab;
end;

procedure TMainForm.CheckProcess();
var
  wideChars: array[0..256] of WideChar;
  MsgBoxResult: TModalResult;
  FoundHandle: HWND;
begin
  if (Length(GameDataClass.GameProcess) = 0) then
    Exit;

  FillChar(wideChars, SizeOf(wideChars), #0);
  StringToWideChar(GameDataClass.GameProcess, wideChars, SizeOf(wideChars));
  FoundHandle := FindWindowW(nil, wideChars);
  if (FoundHandle <> 0) then
  begin
    ActionExecutorClass.ProcessHandle := FoundHandle;
    lblProcessTest.Font.Color := clLime;
    lblProcessTest.Caption := Localization.ProcessFoundLabel;
    btnStart.Enabled := true;
  end
  else
  begin
    ActionExecutorClass.ProcessHandle := -1;
    lblProcessTest.Font.Color := clRed;
    lblProcessTest.Caption := Localization.ProcessNotFoundLabel;
    btnStart.Enabled := false;
    //MsgBoxResult := MessageDlg(
    //  Localization.ProcessNotFoundLabel,
    //  Localization.ProcessNotFoundMessage,
    //  mtWarning, [mbYes, mbNo], 0
    //);
    //if (MsgBoxResult = mrYes) then
    //begin
    //  lblProcessTest.Caption := '';
    //  WindowListForm := TWindowListForm.Create(MainForm);
    //  WindowListForm.ShowModal();
    //  lblGameProcess.Caption := GameDataClass.GameProcess;
    //end;
  end;
end;

procedure TMainForm.btnStartClick(Sender: TObject);
var
  ExecuteResult: TExecuteResult;
begin
  ExecuteResult := ActionExecutorClass.ParseAndExecute(FTreeViewGuid, GameDataClass.GameParams, TreeView1);
  if (ExecuteResult = exeBreaked) then
  begin
    MessageDlg(
      '',
      Localization.ExecutionBreakedMessage,
      mtInformation, [mbOK], 0
    );
  end;
end;

procedure TMainForm.btnStopClick(Sender: TObject);
begin
  KeyboardHookClass.StopExecution();
end;

procedure TMainForm.btnSaveClick(Sender: TObject);
begin
  if (NOT(FIsTreeWasChanged)) then
  begin
    ShowMessage('No Changes');
    Exit;
  end;
  SaveAllChanges();
end;

procedure TMainForm.btnAddClick(Sender: TObject);
var
  ActionTmp: TAction;
  ParentNode, NewNode: TTreeNode;
  IsSubling: Boolean;
  EditResult: TModalResult;
begin
  if (Not(Assigned(TreeView1.Selected))) then
    Exit;

  ParentNode := TreeView1.Selected;
  IsSubling := false;
  ActionTmp := TAction(ParentNode.Data^);
  if ((ActionTmp.Kind <> actChapter)
    AND (ActionTmp.Kind <> actLoop)
    AND (ActionTmp.Kind <> actInfinityLoop)
    AND (ActionTmp.Kind <> actTimer)) then
  begin
    ParentNode := ParentNode.Parent;
    IsSubling := true;
  end;

  NewNode := TreeView1.Items.AddChildObject(ParentNode, '', nil);
  NewNode.StateIndex := 1;
  if (IsSubling) then
    NewNode.MoveTo(TreeView1.Selected, naInsertBehind);
  ActionEditForm := TActionEditForm.Create(MainForm, NewNode);
  EditResult := ActionEditForm.ShowModal();
  if (EditResult = mrClose) then
    TreeView1.Items.Delete(NewNode);
  if (EditResult = mrOK) then
  begin
    FIsTreeWasChanged := true;
    FTreeViewGuid := TGUID.NewGuid();
  end;
end;

procedure TMainForm.btnEditClick(Sender: TObject);
var
  EditResult: TModalResult;
begin
  if (NOT(Assigned(TreeView1.Selected))) then
    Exit;
  ActionEditForm := TActionEditForm.Create(MainForm, TreeView1.Selected);
  EditResult := ActionEditForm.ShowModal();
  if (EditResult = mrOK) then
  begin
    FIsTreeWasChanged := true;
    FTreeViewGuid := TGUID.NewGuid();
  end;
end;

procedure TMainForm.btnDeleteClick(Sender: TObject);
begin
  if (NOT(Assigned(TreeView1.Selected)) OR TreeView1.Selected.HasChildren) then
    Exit;
  TreeView1.Selected.Delete();
end;

procedure TMainForm.btnMoveDownClick(Sender: TObject);
begin
  if (NOT(Assigned(TreeView1.Selected)) OR (TreeView1.Selected.Index = TreeView1.Selected.GetLastSibling.Index)) then
    Exit;
  TreeView1.Selected.Index := TreeView1.Selected.Index + 1;
end;

procedure TMainForm.btnMoveUpClick(Sender: TObject);
begin
  if (NOT(Assigned(TreeView1.Selected)) OR (TreeView1.Selected.Index = 0)) then
    Exit;
  TreeView1.Selected.Index := TreeView1.Selected.Index - 1;
end;

procedure TMainForm.btnCollapseClick(Sender: TObject);
var
  pnt: TPoint;
begin
  if GetCursorPos(pnt) then
    popupCollapseExpand.Popup(pnt.X, pnt.Y);
end;

procedure TMainForm.menCollapseAllClick(Sender: TObject);
begin
  TreeView1.FullCollapse();
end;

procedure TMainForm.menCollapseChaptersClick(Sender: TObject);
var
  i: Integer;
  ActionTmp: TAction;
begin
  for i := 0 to TreeView1.Items.Count - 1 do
  begin
    ActionTmp := TAction(TreeView1.Items.Item[i].Data^);
    if (ActionTmp.Kind <> actChapter) then
      TreeView1.Items.Item[i].Parent.Collapse(false);
  end;
end;

procedure TMainForm.menExpandAllClick(Sender: TObject);
begin
  TreeView1.FullExpand();
end;

procedure TMainForm.menExpandChaptersClick(Sender: TObject);
var
  i: Integer;
  ActionTmp: TAction;
begin
  for i := 0 to TreeView1.Items.Count - 1 do
  begin
    ActionTmp := TAction(TreeView1.Items.Item[i].Data^);
    if ((ActionTmp.Kind = actChapter) AND (Assigned(TreeView1.Items.Item[i].Parent))) then
      TreeView1.Items.Item[i].Parent.Expand(false);
  end;
end;

procedure TMainForm.btnUncheckClick(Sender: TObject);
var
  pnt: TPoint;
begin
  if GetCursorPos(pnt) then
    popupCheckUncheck.Popup(pnt.X, pnt.Y);
end;

procedure TMainForm.menCheckAllClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to TreeView1.Items.Count - 1 do
    TreeView1.Items.Item[i].StateIndex := 1;
  FTreeViewGuid := TGUID.NewGuid();
end;

procedure TMainForm.menCheckActionsClick(Sender: TObject);
var
  i: Integer;
  ActionTmp: TAction;
begin
  for i := 0 to TreeView1.Items.Count - 1 do
  begin
    ActionTmp := TAction(TreeView1.Items.Item[i].Data^);
    if (ActionTmp.Kind <> actChapter) then
      TreeView1.Items.Item[i].StateIndex := 1;
  end;
  FTreeViewGuid := TGUID.NewGuid();
end;

procedure TMainForm.menUncheckAllClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to TreeView1.Items.Count - 1 do
    TreeView1.Items.Item[i].StateIndex := 0;
  FTreeViewGuid := TGUID.NewGuid();
end;

procedure TMainForm.menUncheckChaptersClick(Sender: TObject);
var
  i: Integer;
  ActionTmp: TAction;
begin
  for i := 0 to TreeView1.Items.Count - 1 do
  begin
    ActionTmp := TAction(TreeView1.Items.Item[i].Data^);
    if (ActionTmp.Kind = actChapter) then
      TreeView1.Items.Item[i].StateIndex := 0;
  end;
  FTreeViewGuid := TGUID.NewGuid();
end;

procedure TMainForm.btnFindProcessClick(Sender: TObject);
begin
  WindowListForm := TWindowListForm.Create(MainForm);
  WindowListForm.ShowModal();
  edtGameProcess.Caption := GameDataClass.GameProcess;
  CheckProcess();
end;

procedure TMainForm.ToggleTreeViewCheckBoxes(Node: TTreeNode);
begin
  if Assigned(Node) then
  begin
    FTreeViewGuid := TGUID.NewGuid();
    if Node.StateIndex = 0 then
      Node.StateIndex := 1
    else
    if Node.StateIndex = 1 then
      Node.StateIndex := 0;
  end;
end;

procedure TMainForm.TreeView1Click(Sender: TObject);
var
  ClickPoint: TPoint;
  ActionTmp: TAction;
begin
  GetCursorPos(ClickPoint);
  ClickPoint := TreeView1.ScreenToClient(ClickPoint);
  if (htOnStateIcon in TreeView1.GetHitTestInfoAt(ClickPoint.X, ClickPoint.Y)) then
  begin
    ToggleTreeViewCheckBoxes(TreeView1.Selected);
    Exit;
  end;
  if ((htOnLabel in TreeView1.GetHitTestInfoAt(ClickPoint.X, ClickPoint.Y))
    AND (Assigned(TreeView1.Selected)) AND (Assigned(TreeView1.Selected.Data))) then
  begin
    ActionTmp := TAction(TreeView1.Selected.Data^);
    TreeView1.Hint := ActionToStr(ActionTmp);
    Exit;
  end;
  if (ComCtrls.htNowhere in TreeView1.GetHitTestInfoAt(ClickPoint.X, ClickPoint.Y)) then
  begin
    TreeView1.Selected := nil;
  end;
end;


procedure TMainForm.TreeView1AdvancedCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
  var PaintImages, DefaultDraw: Boolean);
var
  ActionTmp: TAction;
  NodeRect: TRect;
begin
  if ((Stage <> cdPostPaint) OR (NOT(Assigned(Node.Data)))) then
    Exit;

  ActionTmp := TAction(Node.Data^);
  if (ActionTmp.Kind = actBroken) then
  begin
    Sender.Canvas.Font.Assign(Sender.Font);
    Sender.Canvas.Font.Color := clRed;
    Sender.Canvas.Brush.Style := bsClear;
    NodeRect := Node.DisplayRect(true);
    NodeRect.Offset(ScaleX(6, 120), ScaleY(2, 120));
    Sender.Canvas.TextOut(NodeRect.Left, NodeRect.Top, Node.Text);
  end;
end;

end.

