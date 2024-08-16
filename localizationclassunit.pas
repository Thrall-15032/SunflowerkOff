unit LocalizationClassUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TLocalization }

  TLocalization = class
  private
    CHOOSE_GAME_LABEL: string;
    GAME_NAME_LABEL: string;
    GAME_PROCESS_LABEL: string;
    GAME_DO_MAXIMIZE_LABEL: string;
    GAME_DO_START_DISPLAY_CENTER_LABEL: string;
    GAME_DO_START_WINDOW_CENTER_LABEL: string;
    GAME_DO_WINDOW_POSITION_LABEL: string;
    GAME_DO_WINDOW_SIZE_LABEL: string;
    CHOOSE_WINDOW_BUTTON: string;
    PROCESS_TAB: string;
    PARAMS_TAB: string;
    TEST_BUTTON: string;
    START_BUTTON: string;
    SAVE_BUTTON: string;
    CANCEL_BUTTON: string;
    CHECK_ALL_MENU: string;
    CHECK_ACTIONS_MENU: string;
    UNCHECK_ALL_MENU: string;
    UNCHECK_CHAPTERS_MENU: string;
    EXPAND_ALL_MENU: string;
    EXPAND_CHAPTERS_MENU: string;
    COLLAPSE_ALL_MENU: string;
    COLLAPSE_CHAPTERS_MENU: string;
    ENTER_CHAPTER_NAME_LABEL: string;
    ENTER_LOOP_COUNT_LABEL: string;
    ENTER_MANY_KEYS_LABEL: string;
    ENTER_ONE_KEY_LABEL: string;
    PROCESS_FOUND_LABEL: string;
    PROCESS_NOT_FOUND_LABEL: string;
    PROCESS_NOT_FOUND_MESSAGE: string;
    UNSAVED_CHANGES_MESSAGE: string;
    UNSAVED_TO_NEW_FILE_MESSAGE: string;
    EXECUTION_BREAKED_MESSAGE: string;
    ACTION_CHILD_ACTION_ERROR: string;
    function getChooseGameLavel(): string;
    function getGameNameLabel(): string;
    function getGameProcessLabel(): string;
    function getGameDoMaximizeLabel(): string;
    function getGameDoStartDisplayCenterLabel(): string;
    function getGameDoStartWindowCenterLabel(): string;
    function getGameDoWindowPositionLabel(): string;
    function getGameDoWindowSizeLabel(): string;
    function getChooseWindowButton(): string;
    function getProcessTab(): string;
    function getParamsTab(): string;
    function getTestButton(): string;
    function getStartButton(): string;
    function getSaveButton(): string;
    function getCancelButton(): string;
    function getCheckAllMenu(): string;
    function getCheckActionsMenu(): string;
    function getUncheckAllMenu(): string;
    function getUncheckChaptersMenu(): string;
    function getExpandAllMenu(): string;
    function getExpandChaptersMenu(): string;
    function getCollapseAllMenu(): string;
    function getCollapseChaptersMenu(): string;
    function getEnterChapterNameLabel(): string;
    function getEnterLoopCountLabel(): string;
    function getEnterManyKeysLabel(): string;
    function getEnterOneKeyLabel(): string;
    function getProcessFoundLabel(): string;
    function getProcessNotFoundLabel(): string;
    function getProcessNotFoundMessage(): string;
    function getUnsavedChangesMessage(): string;
    function getUnsavedToNewFileMessage(): string;
    function getExecutionBreakedMessage(): string;
    function getActionChildActionError(): string;
  public
    property ChooseGameLabel: string read getChooseGameLavel;
    property GameNameLabel: string read getGameNameLabel;
    property GameProcessLabel: string read getGameProcessLabel;
    property GameDoMaximizeLabel: string read getGameDoMaximizeLabel;
    property GameDoStartDisplayCenterLabel: string read getGameDoStartDisplayCenterLabel;
    property GameDoStartWindowCenterLabel: string read getGameDoStartWindowCenterLabel;
    property GameDoWindowPositionLabel: string read getGameDoWindowPositionLabel;
    property GameDoWindowSizeLabel: string read getGameDoWindowSizeLabel;
    property ChooseWindowButton: string read getChooseWindowButton;
    property ProcessTab: string read getProcessTab;
    property ParamsTab: string read getParamsTab;
    property TestButton: string read getTestButton;
    property StartButton: string read getStartButton;
    property SaveButton: string read getSaveButton;
    property CancelButton: string read getCancelButton;
    property CheckAllMenu: string read getCheckAllMenu;
    property CheckActionsMenu: string read getCheckActionsMenu;
    property UncheckAllMenu: string read getUncheckAllMenu;
    property UncheckChaptersMenu: string read getUncheckChaptersMenu;
    property ExpandAllMenu: string read getExpandAllMenu;
    property ExpandChaptersMenu: string read getExpandChaptersMenu;
    property CollapseAllMenu: string read getCollapseAllMenu;
    property CollapseChaptersMenu: string read getCollapseChaptersMenu;
    property EnterChapterNameLabel: string read getEnterChapterNameLabel;
    property EnterLoopCountLabel: string read getEnterLoopCountLabel;
    property EnterManyKeysLabel: string read getEnterManyKeysLabel;
    property EnterOneKeyLabel: string read getEnterOneKeyLabel;
    property ProcessFoundLabel: string read getProcessFoundLabel;
    property ProcessNotFoundLabel: string read getProcessNotFoundLabel;
    property ProcessNotFoundMessage: string read getProcessNotFoundMessage;
    property UnsavedChangesMessage: string read getUnsavedChangesMessage;
    property UnsavedToNewFileMessage: string read getUnsavedToNewFileMessage;
    property ExecutionBreakedMessage: string read getExecutionBreakedMessage;
    property ActionChildActionError: string read getActionChildActionError;
  end;

var
  Localization: TLocalization;

implementation

{ TLocalization }

function TLocalization.getChooseGameLavel(): string;
begin
  Result := 'Choose the game:';
  if (Length(self.CHOOSE_GAME_LABEL) > 0) then
    Result := self.CHOOSE_GAME_LABEL;
end;

function TLocalization.getGameNameLabel(): string;
begin
  Result := 'Name:';
  if (Length(self.GAME_NAME_LABEL) > 0) then
    Result := self.GAME_NAME_LABEL;
end;

function TLocalization.getGameProcessLabel(): string;
begin
  Result := 'Process:';
  if (Length(self.GAME_PROCESS_LABEL) > 0) then
    Result := self.GAME_PROCESS_LABEL;
end;

function TLocalization.getGameDoMaximizeLabel(): string;
begin
  Result := 'Maximize game';
  if (Length(self.GAME_DO_MAXIMIZE_LABEL) > 0) then
    Result := self.GAME_DO_MAXIMIZE_LABEL;
end;

function TLocalization.getGameDoStartDisplayCenterLabel(): string;
begin
  Result := 'Move mouse to Display center';
  if (Length(self.GAME_DO_START_DISPLAY_CENTER_LABEL) > 0) then
    Result := self.GAME_DO_START_DISPLAY_CENTER_LABEL;
end;

function TLocalization.getGameDoStartWindowCenterLabel(): string;
begin
  Result := 'Move mouse to Window center';
  if (Length(self.GAME_DO_START_WINDOW_CENTER_LABEL) > 0) then
    Result := self.GAME_DO_START_WINDOW_CENTER_LABEL;
end;

function TLocalization.getGameDoWindowPositionLabel(): string;
begin
  Result := 'Change Window position';
  if (Length(self.GAME_DO_WINDOW_POSITION_LABEL) > 0) then
    Result := self.GAME_DO_WINDOW_POSITION_LABEL;
end;

function TLocalization.getGameDoWindowSizeLabel(): string;
begin
  Result := 'Change Window size';
  if (Length(self.GAME_DO_WINDOW_SIZE_LABEL) > 0) then
    Result := self.GAME_DO_WINDOW_SIZE_LABEL;
end;

function TLocalization.getChooseWindowButton(): string;
begin
  Result := 'Choose';
  if (Length(self.CHOOSE_WINDOW_BUTTON) > 0) then
    Result := self.CHOOSE_WINDOW_BUTTON;
end;

function TLocalization.getProcessTab(): string;
begin
  Result := 'Process';
  if (Length(self.PROCESS_TAB) > 0) then
    Result := self.PROCESS_TAB;
end;

function TLocalization.getParamsTab(): string;
begin
  Result := 'Params';
  if (Length(self.PARAMS_TAB) > 0) then
    Result := self.PARAMS_TAB;
end;

function TLocalization.getTestButton(): string;
begin
  Result := 'TEST';
  if (Length(self.TEST_BUTTON) > 0) then
    Result := self.TEST_BUTTON;
end;

function TLocalization.getStartButton(): string;
begin
  Result := 'START';
  if (Length(self.START_BUTTON) > 0) then
    Result := self.START_BUTTON;
end;

function TLocalization.getSaveButton(): string;
begin
  Result := 'SAVE';
  if (Length(self.SAVE_BUTTON) > 0) then
    Result := self.SAVE_BUTTON;
end;

function TLocalization.getCancelButton(): string;
begin
  Result := 'CANCEL';
  if (Length(self.CANCEL_BUTTON) > 0) then
    Result := self.CANCEL_BUTTON;
end;

function TLocalization.getCheckAllMenu(): string;
begin
  Result := 'Check All';
  if (Length(self.CHECK_ALL_MENU) > 0) then
    Result := self.CHECK_ALL_MENU;
end;

function TLocalization.getCheckActionsMenu(): string;
begin
  Result := 'Check Actions';
  if (Length(self.CHECK_ACTIONS_MENU) > 0) then
    Result := self.CHECK_ACTIONS_MENU;
end;

function TLocalization.getUncheckAllMenu(): string;
begin
  Result := 'Uncheck All';
  if (Length(self.UNCHECK_ALL_MENU) > 0) then
    Result := self.UNCHECK_ALL_MENU;
end;

function TLocalization.getUncheckChaptersMenu(): string;
begin
  Result := 'Uncheck Chapters';
  if (Length(self.UNCHECK_CHAPTERS_MENU) > 0) then
    Result := self.UNCHECK_CHAPTERS_MENU;
end;

function TLocalization.getExpandAllMenu(): string;
begin
  Result := 'Expand All';
  if (Length(self.EXPAND_ALL_MENU) > 0) then
    Result := self.EXPAND_ALL_MENU;
end;

function TLocalization.getExpandChaptersMenu(): string;
begin
  Result := 'Expand Chapters';
  if (Length(self.EXPAND_CHAPTERS_MENU) > 0) then
    Result := self.EXPAND_CHAPTERS_MENU;
end;

function TLocalization.getCollapseAllMenu(): string;
begin
  Result := 'Collapse All';
  if (Length(self.COLLAPSE_ALL_MENU) > 0) then
    Result := self.COLLAPSE_ALL_MENU;
end;

function TLocalization.getCollapseChaptersMenu(): string;
begin
  Result := 'Collapse Chapters';
  if (Length(self.COLLAPSE_CHAPTERS_MENU) > 0) then
    Result := self.COLLAPSE_CHAPTERS_MENU;
end;

function TLocalization.getEnterChapterNameLabel(): string;
begin
  Result := 'Enter chapter name:';
  if (Length(self.ENTER_CHAPTER_NAME_LABEL) > 0) then
    Result := self.ENTER_CHAPTER_NAME_LABEL;
end;

function TLocalization.getEnterLoopCountLabel(): string;
begin
  Result := 'Enter loop count:';
  if (Length(self.ENTER_LOOP_COUNT_LABEL) > 0) then
    Result := self.ENTER_LOOP_COUNT_LABEL;
end;

function TLocalization.getEnterManyKeysLabel(): string;
begin
  Result := 'Enter one key or several keys for pressing:';
  if (Length(self.ENTER_MANY_KEYS_LABEL) > 0) then
    Result := self.ENTER_MANY_KEYS_LABEL;
end;

function TLocalization.getEnterOneKeyLabel(): string;
begin
  Result := 'Enter one key for pressing:';
  if (Length(self.ENTER_ONE_KEY_LABEL) > 0) then
    Result := self.ENTER_ONE_KEY_LABEL;
end;

function TLocalization.getProcessFoundLabel(): string;
begin
  Result := 'PROCESS FOUND';
  if (Length(self.PROCESS_FOUND_LABEL) > 0) then
    Result := self.PROCESS_FOUND_LABEL;
end;

function TLocalization.getProcessNotFoundLabel(): string;
begin
  Result := 'PROCESS NOT FOUND';
  if (Length(self.PROCESS_NOT_FOUND_LABEL) > 0) then
    Result := self.PROCESS_NOT_FOUND_LABEL;
end;

function TLocalization.getProcessNotFoundMessage(): string;
begin
  Result := 'Window of game is not found.' + #10 + 'Want you choose window from active window list?';
  if (Length(self.PROCESS_NOT_FOUND_MESSAGE) > 0) then
    Result := self.PROCESS_NOT_FOUND_MESSAGE;
end;

function TLocalization.getUnsavedChangesMessage(): string;
begin
  Result := 'Save changes to file?';
  if (Length(self.UNSAVED_CHANGES_MESSAGE) > 0) then
    Result := self.UNSAVED_CHANGES_MESSAGE;
end;

function TLocalization.getUnsavedToNewFileMessage(): string;
begin
  Result := 'Save changes to new file?';
  if (Length(self.UNSAVED_TO_NEW_FILE_MESSAGE) > 0) then
    Result := self.UNSAVED_TO_NEW_FILE_MESSAGE;
end;

function TLocalization.getExecutionBreakedMessage(): string;
begin
  Result := 'Execution was breaked';
  if (Length(self.EXECUTION_BREAKED_MESSAGE) > 0) then
    Result := self.EXECUTION_BREAKED_MESSAGE;
end;

function TLocalization.getActionChildActionError(): string;
begin
  Result := 'You can not add action as child to simple action';
  if (Length(self.ACTION_CHILD_ACTION_ERROR) > 0) then
    Result := self.ACTION_CHILD_ACTION_ERROR;
end;

end.

