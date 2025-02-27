unit KM_GUIGameGoalsPopUp;
{$I KaM_Remake.inc}
interface
uses
  KM_Controls, KM_ControlsBase, KM_ControlsEdit, KM_ControlsWaresRow,
  KM_CommonClasses, KM_CommonTypes,
  KM_ResTypes, KM_ControlsList,
  System.Classes;


type
  TKMGUIGameGoalsPopUp = class
  private
    fIsSurvival : Boolean;
    fOnShow : TNotifyEvent;
    procedure ClosePanel(Sender : TObject);
    procedure RefreshList;
    procedure SelectListItem(Sender : TObject);
    procedure ButtonClick(Sender : TObject);
  protected
    Pin_Open : TKMButtonFlatPin;
    Panel_Goals : TKMPanel;
      Image_Background,
      Image_ClosePanel: TKMImage;
      Button_Victory, Button_Survive : TKMButton;
      Columnbox_Goals : TKMColumnBox;
      Label_GoalLine3,
      Label_GoalLine2,
      Label_GoalLine1 : TKMLabel;
      Shape_PlayerColor : TKMFlatButtonShape;
      Edit_PlayerName,
      Edit_BuildingType : TKMEdit;
      Icon_Houses : TKMIconsRow;

  public
    constructor Create(aParent: TKMPanel; aOnShow : TNotifyEvent);

    procedure Show;
    procedure Hide;
    function Visible: Boolean;
    procedure UpdateState;
  end;


implementation
uses  KM_HandsCollection,
      KM_Pics,
      KM_AIGoals, KM_MapTypes,
      KM_Defaults, KM_GameParams,
      KM_Resource,
      KM_ResFonts, KM_ResTexts,
      KM_RenderUI, KM_ResHouses,
      KM_CommonUtils, SysUtils, Math;

constructor TKMGUIGameGoalsPopUp.Create(aParent: TKMPanel; aOnShow : TNotifyEvent);
begin
  inherited Create;
  fIsSurvival := false;
  fOnShow := aOnShow;
  Pin_Open := TKMButtonFlatPin.Create(aParent, 198, 70, 25, 32, 794);
  Pin_Open.OnClick := ClosePanel;
  Pin_Open.Hint := gResTexts[1948];
  Panel_Goals := TKMPanel.Create(aParent, 240, 0, 400, 650);

  Image_Background := TKMImage.Create(Panel_Goals, 0, 0, Panel_Goals.Width, Panel_Goals.Height, 18, rxGuiMain);
  Image_Background.ImageStretch;

  TKMBevel.Create(Panel_Goals, 20, 40, Panel_Goals.Width - 40, Panel_Goals.Height - 100);

  Image_ClosePanel := TKMImage.Create(Panel_Goals, Panel_Goals.Width - 60, 10, 30, 30, 52);
  Image_ClosePanel.OnClick := ClosePanel;

  Button_Victory := TKMButton.Create(Panel_Goals, 32, 47, 150, 25, gResTexts[444], bsGame);
  Button_Victory.OnClick := ButtonClick;

  Button_Survive := TKMButton.Create(Panel_Goals, Button_Victory.Right + 35, 47, 150, 25, gResTexts[443], bsGame);
  Button_Survive.OnClick := ButtonClick;

  Columnbox_Goals := TKMColumnBox.Create(Panel_Goals, 30, 80, Panel_Goals.Width - 60, 250, fntOutline, bsGame);
  Columnbox_Goals.SetColumns(fntMetal, ['', ''], [0, Columnbox_Goals.Width - 30]);
  Columnbox_Goals.ShowHeader := false;
  Columnbox_Goals.AutoFocusable := false;
  Columnbox_Goals.ShowLines := true;
  Columnbox_Goals.OnClick := SelectListItem;
  Columnbox_Goals.OnChange := SelectListItem;

  Label_GoalLine1 := TKMLabel.Create(Panel_Goals, 30, Columnbox_Goals.Bottom + 10, Panel_Goals.Width - 60, 200, '', fntMetal, taLeft);
  Label_GoalLine1.WordWrap := true;
  Label_GoalLine1.Hitable := false;
  Label_GoalLine2 := TKMLabel.Create(Panel_Goals, 40, Columnbox_Goals.Bottom + 30, Panel_Goals.Width - 60, 20, '', fntOutLine, taLeft);
  Label_GoalLine2.Hitable := false;
  Label_GoalLine3 := TKMLabel.Create(Panel_Goals, 40, Columnbox_Goals.Bottom + 50, Panel_Goals.Width - 60, 20, gResTexts[1934], fntMetal, taLeft);
  Label_GoalLine3.Hitable := false;

  Edit_PlayerName := TKMEdit.Create(Panel_Goals, 40, Label_GoalLine3.Bottom, 130, 20, fntMetal, false);
  Edit_PlayerName.Selectable := false;
  Edit_PlayerName.Hitable := false;
  Edit_PlayerName.AutoFocusable := false;
  Edit_PlayerName.ReadOnly := true;

  Shape_PlayerColor := TKMFlatButtonShape.Create(Panel_Goals, Edit_PlayerName.Right, Edit_PlayerName.Top, 20, 20, '', fntMetal, $ffffffff);
  Shape_PlayerColor.Hitable := false;

  Edit_BuildingType := TKMEdit.Create(Panel_Goals, Columnbox_Goals.Right - 140, Label_GoalLine2.Top - 2, 140, 20, fntMetal, false);
  Edit_BuildingType.Selectable := false;
  Edit_BuildingType.Hitable := false;
  Edit_BuildingType.AutoFocusable := false;
  Edit_BuildingType.ReadOnly := true;
  Icon_Houses := TKMIconsRow.Create(Panel_Goals, Edit_BuildingType.Left, Edit_BuildingType.Bottom + 5, 37, 37);
  Icon_Houses.MaxCountInRow := 4;
  //Icon_Houses.StillSize := true;
  Icon_Houses.BackBevel := 0.5;
  //Icon_Houses.Width := 37 * 4;
  //Icon_Houses.Height := 37 * 5;

  Hide;
end;


procedure TKMGUIGameGoalsPopUp.Show;
begin
  Panel_Goals.Show;
  if Assigned(fOnShow) then
    fOnShow(Self);
  RefreshList;
end;


procedure TKMGUIGameGoalsPopUp.Hide;
begin
  Panel_Goals.Hide;
end;


function TKMGUIGameGoalsPopUp.Visible: Boolean;
begin
  Result := Panel_Goals.Visible;
end;

procedure TKMGUIGameGoalsPopUp.ClosePanel(Sender: TObject);
begin
  if Sender = Pin_Open then
  begin
    if Visible then
      Hide
    else
      Show;

  end else
    Hide;
end;

procedure TKMGUIGameGoalsPopUp.RefreshList;
var I, ItemIndex, TopIndex : Integer;
  G : TKMGoal;
begin
  ItemIndex := Columnbox_Goals.ItemIndex;
  TopIndex := Columnbox_Goals.TopIndex;

  Columnbox_Goals.Clear;

 for I := 0 to gMySpectator.Hand.AI.Goals.Count - 1 do
  begin
    G := gMySpectator.Hand.AI.Goals[I];

    if (G.GoalType = gltVictory) and (not fIsSurvival) then
    begin
      Columnbox_Goals.AddItem(
                                MakeListRow([gResTexts[GOAL_CONDITION_LIBX[G.GoalCondition, G.GoalType]], ''],
                                             [icWhite, icWhite], [MakePic(rxGuiMain, 0), MakePic(rxGuiMain, IfThen(G.Disabled, 33, 32))],
                                             I
                                            )
                              );
    end else
    if (G.GoalType = gltSurvive) and fIsSurvival then
    begin
      Columnbox_Goals.AddItem(
                                MakeListRow([gResTexts[GOAL_CONDITION_LIBX[G.GoalCondition, G.GoalType]], ''],
                                             [icWhite, icWhite], [MakePic(rxGuiMain, 0), MakePic(rxGuiMain, IfThen(G.Disabled, 32, 33))],
                                             I
                                            )
                              );

    end;

  end;
  //just testing here
  {for I := 0 to High(gMySpectator.Hand.MessageStack) do
  begin
    with gMySpectator.Hand.MessageStack[I] do
      Columnbox_Goals.AddItem(
                              MakeListRow([ Copy(Text, 1, 25) + '...', ''],
                                           [icWhite, 0], [MakePic(rxGuiMain, 0), MakePic(rxGuiMain, IfThen(G.Disabled, 33, 32))],
                                           I
                                          )
                            );
  end;}

  if ItemIndex >= Columnbox_Goals.RowCount then
    ItemIndex := Columnbox_Goals.RowCount - 1;

  Columnbox_Goals.ItemIndex := ItemIndex;
  Columnbox_Goals.TopIndex := TopIndex;
  Columnbox_Goals.JumpToSelected;
  SelectListItem(Columnbox_Goals);
end;

procedure TKMGUIGameGoalsPopUp.SelectListItem(Sender: TObject);
  function HousesToIcons(aArray : TKMHouseTypeArray) : TKMWordArray;
  var I : Integer;
  begin
    SetLength(Result, Length(aArray));
    for I := 0 to High(aArray) do
      Result[I] := gRes.Houses[aArray[I]].GuiIcon;

  end;
var aIndex : Integer;
  G : TKMGoal;
  aTime : Single;
begin
  //hide evrything first
  Label_GoalLine1.Hide;
  Label_GoalLine2.Hide;
  Shape_PlayerColor.Hide;
  Edit_PlayerName.Hide;
  Edit_BuildingType.Hide;
  Icon_Houses.Hide;
  Label_GoalLine3.Hide;
  Edit_PlayerName.Unfocus;
  Edit_BuildingType.Unfocus;

  if not Columnbox_Goals.IsSelected then
    Exit;

  aIndex := Columnbox_Goals.SelectedItem.Tag;
  G := gMySpectator.Hand.AI.Goals[aIndex];

  if G.GoalCondition in [gcTime] then
  begin
    Label_GoalLine1.Show;
    aTime := G.GoalTime / 24 / 60 / 60/ 10;
    If G.GoalType = gltVictory then
      Label_GoalLine1.Caption := gResTexts[1931] + TimeToString(aTime)
    else
      Label_GoalLine1.Caption := gResTexts[1930] + TimeToString(aTime);

    Exit;
  end;
  Label_GoalLine1.Visible := not (G.GoalCondition in [gcFindPlace, gcRevealPlace]);
  Label_GoalLine2.Visible := Label_GoalLine1.Visible;
  if Label_GoalLine2.Visible then
  begin
    case G.GoalCondition of
      gcBuildings,
      gcMilitaryAssets,
      gcEconomyBuildings,
      gcBuildingsType,
      gcAllBuildings: If G.GoalType = gltVictory then
                        Label_GoalLine1.Caption := gResTexts[1928]
                      else
                        Label_GoalLine1.Caption := gResTexts[1932];

      gcTroops,
      gcSerfsAndSchools,
      gcAllUnits: If G.GoalType = gltVictory then
                    Label_GoalLine1.Caption := gResTexts[1929]
                  else
                    Label_GoalLine1.Caption := gResTexts[1933];
    end;

    Label_GoalLine2.Caption := gResTexts[GOAL_CONDITION_LIBX[G.GoalCondition, G.GoalType]];
    Edit_BuildingType.Visible := G.GoalCondition = gcBuildingsType;
    Icon_Houses.Visible := G.GoalCondition in [gcBuildingsType, gcBuildings, gcMilitaryAssets];
    if ((G.HandIndex >= 0) and (G.HandIndex <> gMySpectator.HandID)) or (G.GoalType = gltSurvive) then
    begin
      Label_GoalLine3.Show;
      Shape_PlayerColor.Show;
      Edit_PlayerName.Show;
      Shape_PlayerColor.ShapeColor := gHands[G.HandIndex].FlagColor;
      Shape_PlayerColor.Caption := IntToStr(G.HandIndex + 1);
      Edit_PlayerName.SetTextSilently(gHands[G.HandIndex].OwnerName);
      if Edit_BuildingType.Visible then
        Edit_BuildingType.Text := gResTexts[HOUSE_VICTORY_ORDER[G.BuldingsType].TextID];

      If Icon_Houses.Visible then
      begin
        case G.GoalCondition of
          gcBuildingsType : Icon_Houses.SetIcons(HousesToIcons(HOUSE_VICTORY_ORDER[G.BuldingsType].H));
          gcBuildings : Icon_Houses.SetIcons(HousesToIcons([htStore, htSchool, htBarracks, htTownHall, htPalace]) );
          gcMilitaryAssets : Icon_Houses.SetIcons(HousesToIcons([htBarracks, htCoalMine, htWeaponWorkshop, htArmorWorkshop, htStables,
                                                                 htIronMine, htIronSmithy ,htWeaponSmithy, htArmorSmithy, htTownHall,
                                                                 htSiegeWorkshop, htPalace, htWallTower]));
        end;

      end;

    end;
  end;

end;

procedure TKMGUIGameGoalsPopUp.ButtonClick(Sender : TObject);
begin
  if Sender = Button_Survive then
    fIsSurvival := true
  else
  if Sender = Button_Victory then
    fIsSurvival := false;
  RefreshList;

end;

procedure TKMGUIGameGoalsPopUp.UpdateState;
begin
  RefreshList;

end;

end.


