unit KM_GUIMapEdPlayerGoals;
{$I KaM_Remake.inc}
interface
uses
   Classes, Math, SysUtils,
   KM_Controls, KM_ControlsBase, KM_ControlsList,
   KM_Defaults,
   KM_InterfaceGame, KM_AIGoals,
   KM_GUIMapEdPlayerGoalPopUp;


type
  TKMMapEdPlayerGoals = class
  private
    procedure Goals_Add(Sender: TObject);
    procedure Goals_Del(Sender: TObject);
    procedure Goals_Edit(aIndex: Integer);
    procedure Goals_ListClick(Sender: TObject);
    procedure Goals_ListDoubleClick(Sender: TObject);
    procedure Goals_ClipBoard(Sender: TObject);
    procedure Goals_OnDone(Sender: TObject);
    procedure Goals_Refresh;
  protected
    Panel_Goals: TKMPanel;
      ColumnBox_Goals: TKMColumnBox;
      Button_GoalsAdd: TKMButton;
      Button_GoalsDel: TKMButton;
      Button_GoalsCopy: TKMButton;
      Button_GoalsPaste: TKMButton;

  public
    GoalPopUp: TKMMapEdPlayerGoal;

    constructor Create(aParent: TKMPanel);

    procedure Show;
    function Visible: Boolean;
    procedure Hide;
  end;


implementation
uses
  KM_Game,
  KM_HandsCollection, KM_ResTexts, KM_RenderUI, KM_ResFonts, KM_Hand, KM_MapTypes;


{ TKMMapEdPlayerGoals }
constructor TKMMapEdPlayerGoals.Create(aParent: TKMPanel);
begin
  inherited Create;

  Panel_Goals := TKMPanel.Create(aParent, 0, 28, aParent.Width, 400);
  with TKMLabel.Create(Panel_Goals, 0, PAGE_TITLE_Y, Panel_Goals.Width, 0, gResTexts[TX_MAPED_GOALS], fntOutline, taCenter) do
    Anchors := [anLeft, anTop, anRight];
  ColumnBox_Goals := TKMColumnBox.Create(Panel_Goals, 9, 30, Panel_Goals.Width - 9, 230, fntGame, bsGame);
  ColumnBox_Goals.SetColumns(fntOutline,
    [gResTexts[TX_MAPED_GOALS_TYPE],
     gResTexts[TX_MAPED_GOALS_CONDITION],
     gResTexts[TX_MAPED_GOALS_PLAYER]], [0, 25, 155], True);
  ColumnBox_Goals.Anchors := [anLeft, anTop, anRight];
  ColumnBox_Goals.OnClick := Goals_ListClick;
  ColumnBox_Goals.OnDoubleClick := Goals_ListDoubleClick;
  ColumnBox_Goals.ColumnIdForScroll := 2;

  Button_GoalsAdd := TKMButton.Create(Panel_Goals, 9, 270, 25, 25, '+', bsGame);
  Button_GoalsAdd.OnClick := Goals_Add;
  Button_GoalsDel := TKMButton.Create(Panel_Goals, 39, 270, 25, 25, 'X', bsGame);
  Button_GoalsDel.OnClick := Goals_Del;

  Button_GoalsPaste := TKMButton.Create(Panel_Goals,  Panel_Goals.Width - 35, 270, 25, 25, '/\', bsGame);
  Button_GoalsPaste.OnClick := Goals_ClipBoard;
  Button_GoalsPaste.Hint := gResTexts[674];
  Button_GoalsCopy := TKMButton.Create(Panel_Goals, Button_GoalsPaste.left - 30, 270, 25, 25, '\/', bsGame);
  Button_GoalsCopy.OnClick := Goals_ClipBoard;
  Button_GoalsCopy.Hint := gResTexts[673];
end;


//Add a dummy goal and let mapmaker edit it
procedure TKMMapEdPlayerGoals.Goals_Add(Sender: TObject);
var
  G: TKMGoal;
begin
  FillChar(G, SizeOf(G), #0);
  G.GoalType := gltVictory;
  G.GoalCondition := gcBuildings;
  G.Disabled := False;
  gMySpectator.Hand.AI.Goals.AddGoal(G);

  Goals_Refresh;
  ColumnBox_Goals.ItemIndex := gMySpectator.Hand.AI.Goals.Count - 1;

  //Edit the attack we have just appended
  Goals_Edit(ColumnBox_Goals.ItemIndex);
end;


procedure TKMMapEdPlayerGoals.Goals_Del(Sender: TObject);
var
  I: Integer;
begin
  I := ColumnBox_Goals.ItemIndex;
  if InRange(I, 0, gMySpectator.Hand.AI.Goals.Count - 1) then
    gMySpectator.Hand.AI.Goals.Delete(I);
  Goals_Refresh;
end;

procedure TKMMapEdPlayerGoals.Goals_ClipBoard(Sender: TObject);
begin
  if Sender = Button_GoalsCopy then
  begin
    gGame.MapEditor.ClipBoard.Goals := gMySpectator.Hand.AI.Goals.GoalsArray;
    Goals_Refresh;
  end else
  if Sender = Button_GoalsPaste then
  begin
    gMySpectator.Hand.AI.Goals.AddGoals(gGame.MapEditor.ClipBoard.Goals);
    Goals_Refresh;
  end;

end;

procedure TKMMapEdPlayerGoals.Goals_Edit(aIndex: Integer);
begin
  Assert(InRange(aIndex, 0, gMySpectator.Hand.AI.Goals.Count - 1));

  GoalPopUp.Show(gMySpectator.HandID, aIndex);
  GoalPopUp.fOnDone := Goals_OnDone;
  GoalPopUp.OnListChange := Goals_Refresh;
end;


procedure TKMMapEdPlayerGoals.Goals_ListClick(Sender: TObject);
var
  I: Integer;
begin
  I := ColumnBox_Goals.ItemIndex;
  Button_GoalsDel.Enabled := InRange(I, 0, gMySpectator.Hand.AI.Goals.Count - 1);
end;


procedure TKMMapEdPlayerGoals.Goals_ListDoubleClick(Sender: TObject);
var
  I: Integer;
begin
  I := ColumnBox_Goals.ItemIndex;

  //Check if user double-clicked on an existing item (not on an empty space)
  if InRange(I, 0, gMySpectator.Hand.AI.Goals.Count - 1) then
    Goals_Edit(I);
end;


procedure TKMMapEdPlayerGoals.Goals_OnDone(Sender: TObject);
begin
  Goals_Refresh;
end;


procedure TKMMapEdPlayerGoals.Goals_Refresh;
const
  TYP: array [TKMGoalType] of string = ('-', 'V', 'S');
  TYPE_HINT: array [TKMGoalType] of Integer = (
    TX_MAPED_GOALS_TYPE_NONE, TX_MAPED_GOALS_TYPE_VICTORY, TX_MAPED_GOALS_TYPE_SURVIVE);
var
  I, Index, TopIndex: Integer;
  G: TKMGoal;
begin
  TopIndex := ColumnBox_Goals.TopIndex; //Save index and TopIndex to restore after refresh
  Index := ColumnBox_Goals.ItemIndex;
  ColumnBox_Goals.Clear;

  for I := 0 to gMySpectator.Hand.AI.Goals.Count - 1 do
  begin
    G := gMySpectator.Hand.AI.Goals[I];
    ColumnBox_Goals.AddItem(MakeListRow([TYP[G.GoalType],
                                         gResTexts[GOAL_CONDITION_LIBX[G.GoalCondition, G.GoalType]],
                                         IntToStr(G.HandIndex + 1)],
                                        [gResTexts[TYPE_HINT[G.GoalType]],
                                         gResTexts[GOAL_CONDITION_LIBX[G.GoalCondition, G.GoalType]],
                                         Format(gResTexts[TX_PLAYER_X], [G.HandIndex + 1])]));
  end;

  Goals_ListClick(nil);

  //Try to restore previous selected element
  if Index >= ColumnBox_Goals.RowCount then
    Index := ColumnBox_Goals.RowCount - 1;

  ColumnBox_Goals.ItemIndex := Index;
  ColumnBox_Goals.TopIndex := TopIndex;

  ColumnBox_Goals.JumpToSelected;

  Button_GoalsDel.Enabled := ColumnBox_Goals.IsSelected;
  Button_GoalsPaste.Enabled := gGame.MapEditor.ClipBoard.HasGoals;
end;


procedure TKMMapEdPlayerGoals.Hide;
begin
  Panel_Goals.Hide;
end;


procedure TKMMapEdPlayerGoals.Show;
begin
  Goals_Refresh;
  Panel_Goals.Show;
end;


function TKMMapEdPlayerGoals.Visible: Boolean;
begin
  Result := Panel_Goals.Visible;
end;


end.
