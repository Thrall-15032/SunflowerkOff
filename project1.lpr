program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainFormUnit, LocalizationClassUnit, GameDataClassUnit, Utils,
  ActionClass, WindowListFormUnit, lazmouseandkeyinput, KeyboardHookUnit,
  ActionExecutorUnit, KeyFunctionUnit, MouseFunctionUnit, ActionTimerUnit, 
ActionFunctionMapUnit, ActionEditFormUnit, ActionKindUnit, SettingsUnit;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='SunflowerkOff';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

