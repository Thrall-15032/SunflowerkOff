unit ActionEditFormUnit;

{$mode ObjFPC}{$H+}

interface

uses
  // system
  Classes, ComCtrls, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls,
  // main
  ActionClass, ActionKindUnit;
// end uses


type
  { TActionEditForm }

  TActionEditForm = class(TForm)
    btnSave: TButton;
    btnCancel: TButton;
    cmbActionKind: TComboBox;
    cmbPointKind: TComboBox;
    edtName: TEdit;
    edtXvalue: TEdit;
    edtYvalue: TEdit;
    edtInterval: TEdit;
    edtSleep: TEdit;
    edtAction: TEdit;
    Image1: TImage;
    lblName: TLabel;
    lblInterval: TLabel;
    lblSleep: TLabel;
    pnlName: TPanel;
    pnlTiming: TPanel;
    pnlPointKind: TPanel;
    pnlXstyle: TPanel;
    pnlYstyle: TPanel;
    rbnXpercent: TRadioButton;
    rbnXpixel: TRadioButton;
    rbnYpercent: TRadioButton;
    rbnYpixel: TRadioButton;
    procedure cmbActionKindChange(Sender: TObject);
    procedure RefreshResult(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
  private
    FTreeNode: ^TTreeNode;
    procedure RefreshForm(AActionKind: TActionKind);
    function IsCoordinateBroken(): Boolean;
    function IsTimingBroken(): Boolean;
  public
    constructor Create(TheOwner: TComponent; ATreeNode: TTreeNode);
    destructor Destroy(); override;
  end;

var
  ActionEditForm: TActionEditForm;

implementation

uses
  // system
  DialogRes, LCLType, RegExpr,
  // main
  LocalizationClassUnit, Utils;
// end uses

{$R *.lfm}

{ TActionEditForm }

constructor TActionEditForm.Create(TheOwner: TComponent; ATreeNode: TTreeNode);
var
  i: Integer;
  ActionTmp: TAction;
  IsPercent: Boolean;
  ActionKindTmp: TActionKind;
  PointKindTmp: TPointKind;
  StringListTmp: TStringList;
begin
  inherited Create(TheOwner);
  pnlName.Enabled := false;
  pnlPointKind.Enabled := false;
  pnlTiming.Enabled := false;
  // TODO add localization MORE
  btnSave.Caption := Localization.SaveButton;
  btnCancel.Caption := Localization.CancelButton;
  Image1.Picture.Assign(GetDialogIcon(idDialogWarning));
  Image1.Proportional := true;
  Image1.Visible := false;

  FTreeNode := @ATreeNode;
  edtAction.Text := FTreeNode^.Text;
  if (Assigned(FTreeNode^.Data)) then
    ActionTmp := TAction(FTreeNode^.Data^)
  else
    ActionTmp := GetEmptyAction();

  i := 0;
  for ActionKindTmp := actChapter to High(TActionKind) do
  begin
    cmbActionKind.Items.AddObject(
      ActionKindHelper.ActionKindToStr(ActionKindTmp).Caption,
      TActionKindHolder.Create(ActionKindTmp)
    );
    if (ActionTmp.Kind = ActionKindTmp) then
      cmbActionKind.ItemIndex := i;
    Inc(i);
  end;
  i := 0;
  for PointKindTmp := pntCoordinates to High(TPointKind) do
  begin
    cmbPointKind.Items.AddObject(
      ActionKindHelper.PointKindToStr(PointKindTmp).Caption,
      TPointKindHolder.Create(PointKindTmp)
    );
    if (ActionTmp.PointKind = PointKindTmp) then
      cmbPointKind.ItemIndex := i;
    Inc(i);
  end;

  edtName.Text := '';
  if ((ActionTmp.Kind = actChapter) OR (ActionTmp.Kind = actKeyPressMany)) then
    edtName.Text := ActionTmp.KeyList;
  if ((ActionTmp.Kind = actKeyPressOne) OR (ActionTmp.Kind = actKeyDownOne) OR (ActionTmp.Kind = actKeyUpOne)) then
    edtName.Text := ActionTmp.Key;
  if (ActionTmp.Kind = actLoop) then
    edtName.Text := IntToStr(ActionTmp.LoopCount);

  edtInterval.Text := IntToStr(ActionTmp.Interval);
  edtSleep.Text := IntToStr(ActionTmp.Sleep);

  if (ActionTmp.PointKind = pntCoordinates) then
  begin
    IsPercent := (CountCharInStr(ActionTmp.PointStr.X, '%') > 0);
    rbnXpercent.Checked := IsPercent;
    rbnXpixel.Checked := NOT(IsPercent);
    edtXvalue.Text := RemovePercentChar(ActionTmp.PointStr.X);
    IsPercent := (CountCharInStr(ActionTmp.PointStr.Y, '%') > 0);
    rbnYpercent.Checked := IsPercent;
    rbnYpixel.Checked := NOT(IsPercent);
    edtYvalue.Text := RemovePercentChar(ActionTmp.PointStr.Y);
  end;
  RefreshForm(ActionTmp.Kind);
  RefreshResult(TheOwner);
end;

destructor TActionEditForm.Destroy();
var
  i: Integer;
  ObjTmp: TObject;
begin
  for i := 0 to cmbActionKind.Items.Count - 1 do
  begin
    ObjTmp := cmbActionKind.Items.Objects[i];
    FreeObject(ObjTmp);
  end;
  for i := 0 to cmbPointKind.Items.Count - 1 do
  begin
    ObjTmp := cmbPointKind.Items.Objects[i];
    FreeObject(ObjTmp);
  end;
  inherited Destroy();
end;

function TActionEditForm.IsCoordinateBroken(): Boolean;
var
  HasNoNumber: TRegExpr;
begin
  HasNoNumber := TRegExpr.Create('[^0-9|.]');
  Result := (edtXvalue.Text = '') OR HasNoNumber.Exec(edtXvalue.Text) OR
    (edtYvalue.Text = '') OR HasNoNumber.Exec(edtYvalue.Text);
  FreeObject(HasNoNumber);
end;

function TActionEditForm.IsTimingBroken(): Boolean;
var
  HasNoNumber: TRegExpr;
begin
  HasNoNumber := TRegExpr.Create('[^0-9]');
  Result := (edtInterval.Text = '') OR HasNoNumber.Exec(edtInterval.Text) OR
    (edtSleep.Text = '') OR HasNoNumber.Exec(edtSleep.Text);
  FreeObject(HasNoNumber);
end;

procedure TActionEditForm.RefreshResult(Sender: TObject);
var
  StrTmp: string;
  IsBroken: Boolean;
  HasNoNumber: TRegExpr;
  SelectedActionKind: TActionKind;
  SelectedPointKind: TPointKind;
  CoordinatesKind: string;
begin
  HasNoNumber := TRegExpr.Create('[^0-9]');
  IsBroken := false;
  if (cmbActionKind.ItemIndex < 0) then
  begin
    SelectedActionKind := actBroken;
    IsBroken := true;
  end
  else
    SelectedActionKind := TActionKindHolder(cmbActionKind.Items.Objects[cmbActionKind.ItemIndex]).FActionKind;

  if (cmbPointKind.ItemIndex >= 0) then
    SelectedPointKind := TPointKindHolder(cmbPointKind.Items.Objects[cmbPointKind.ItemIndex]).FPointKind;

  StrTmp := ActionKindHelper.ActionKindToStr(SelectedActionKind).Code + ',';
  case SelectedActionKind of
    actChapter:
    begin
      StrTmp := '=' + edtName.Text;
      IsBroken := (edtName.Text = '');
    end;
    actLoop:
    begin
      StrTmp := StrTmp + edtName.Text;
      IsBroken := (edtName.Text = '') OR HasNoNumber.Exec(edtName.Text);
    end;
    actInfinityLoop, actPause:
    begin
      StrTmp := ActionKindHelper.ActionKindToStr(SelectedActionKind).Code;
    end;
    actKeyDownOne, actKeyUpOne, actKeyPressOne:
    begin
      if (Length(edtName.Text) > 1) then
        edtName.Text := edtName.Text[1];
      StrTmp := StrTmp + edtName.Text + ',';
      StrTmp := StrTmp + edtInterval.Text + ',';
      StrTmp := StrTmp + edtSleep.Text;
      IsBroken := (edtName.Text = '') OR IsTimingBroken();
    end;
    actKeyPressMany:
    begin
      StrTmp := StrTmp + edtName.Text + ',';
      StrTmp := StrTmp + edtInterval.Text + ',';
      StrTmp := StrTmp + edtSleep.Text;
      IsBroken := (edtName.Text = '') OR IsTimingBroken();
    end;


    actMouseMove:
    begin
      StrTmp := StrTmp + ActionKindHelper.PointKindToStr(SelectedPointKind).Code + ',';
      if (SelectedPointKind = pntCoordinates) then
      begin
        CoordinatesKind := '';
        if (rbnXpercent.Checked) then CoordinatesKind := '%';
        StrTmp := StrTmp + edtXvalue.Text + CoordinatesKind + ',';
        CoordinatesKind := '';
        if (rbnYpercent.Checked) then CoordinatesKind := '%';
        StrTmp := StrTmp + edtYvalue.Text + CoordinatesKind + ',';
        IsBroken := IsCoordinateBroken();
      end;
      StrTmp := StrTmp + edtInterval.Text + ',';
      StrTmp := StrTmp + edtSleep.Text;
      IsBroken := IsBroken OR IsTimingBroken();
    end;
    actMousePressLeft, actMousePressRight:
    begin
      StrTmp := StrTmp + edtInterval.Text + ',';
      StrTmp := StrTmp + edtSleep.Text;
      IsBroken := IsTimingBroken();
    end;
  end;

  edtAction.Text := StrTmp;
  Image1.Visible := IsBroken;
  FreeObject(HasNoNumber);
end;

procedure TActionEditForm.RefreshForm(AActionKind: TActionKind);
begin
  lblName.Caption := '';
  case AActionKind of
    actChapter:
    begin
      pnlName.Enabled := true;
      pnlPointKind.Enabled := false;
      pnlTiming.Enabled := false;
      lblName.Caption := Localization.EnterChapterNameLabel;
    end;
    actLoop:
    begin
      pnlName.Enabled := true;
      pnlPointKind.Enabled := false;
      pnlTiming.Enabled := false;
      lblName.Caption := Localization.EnterLoopCountLabel;
    end;
    actInfinityLoop, actPause:
    begin
      pnlName.Enabled := false;
      pnlPointKind.Enabled := false;
      pnlTiming.Enabled := false;
    end;
    actKeyPressMany:
    begin
      pnlName.Enabled := true;
      pnlPointKind.Enabled := false;
      pnlTiming.Enabled := true;
      lblName.Caption := Localization.EnterManyKeysLabel;
    end;
    actKeyDownOne, actKeyUpOne, actKeyPressOne:
    begin
      pnlName.Enabled := true;
      pnlPointKind.Enabled := false;
      pnlTiming.Enabled := true;
      lblName.Caption := Localization.EnterOneKeyLabel;
    end;
    actMouseMove:
    begin
      pnlName.Enabled := false;
      pnlPointKind.Enabled := true;
      pnlTiming.Enabled := true;
    end;
    actMousePressLeft, actMousePressRight:
    begin
      pnlName.Enabled := false;
      pnlPointKind.Enabled := false;
      pnlTiming.Enabled := true;
    end;
  end;
end;

procedure TActionEditForm.cmbActionKindChange(Sender: TObject);
var
  SelectedActionKind: TActionKind;
begin
  if (cmbActionKind.ItemIndex >= 0) then
  begin
    SelectedActionKind := TActionKindHolder(cmbActionKind.Items.Objects[cmbActionKind.ItemIndex]).FActionKind;
    RefreshForm(SelectedActionKind);
  end;

  RefreshResult(Sender);
end;

procedure TActionEditForm.btnSaveClick(Sender: TObject);
var
  ActionTmp: PAction;
begin
  if (edtAction.Text = FTreeNode^.Text) then
  begin
    ModalResult := mrClose;
    // Close();
  end;

  FTreeNode^.Text := RemoveSpecSymbols(edtAction.Text);
  new(ActionTmp);
  ParseActionPointer(edtAction.Text, ActionTmp);
  FTreeNode^.Data := ActionTmp;
  ModalResult := mrOK; // this like Close();
end;

procedure TActionEditForm.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrClose; // this like Close();
end;

end.

