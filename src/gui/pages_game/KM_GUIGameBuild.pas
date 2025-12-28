unit KM_GUIGameBuild;
{$I KaM_Remake.inc}
interface
uses
  StrUtils, SysUtils, Classes,
  KM_CommonClasses,
  KM_Controls, KM_ControlsBase,
  KM_Defaults, KM_ControlsScroll, KM_ControlsWaresRow,
  KM_InterfaceGame, KM_ResHouses;


type
  TKMGUIGameBuild = class
  private
    fLastRoadType : TKMLockFieldType;
    fSelectedType : Byte;
    procedure Build_ButtonClick(Sender: TObject; Shift: TShiftState);
    procedure BuildType_Click(Sender: TObject);
    procedure Build_ButtonOver(Sender : TObject; Shift: TShiftState);
    //procedure Build_ButtonOver(Sender: TObject; X, Y: Integer; Shift : TShiftState);
  protected
    Panel_Build: TKMScrollPanel;
      Panel_Cost : TKMPanel;
        Bevel_Build: TKMBevel;
        Label_Build: TKMLabel;
        Image_Build_Selected: TKMImage;
        Row_Cost : TKMCostsRowMulti;

      Button_BuildField: array[TKMLockFieldType] of TKMButtonFlat;

      Button_BuildTypes: array of TKMButton;
      Label_BuildType :TKMLabel;

      Label_BuildTypes: array of TKMLabel;
      Button_Build: array of TKMButtonFlat;

      Icons_UnlockingHouses : TKMIconsRow;
      DecorationCost : TKMVWaresButtonsMulti;
  public
    constructor Create(aParent: TKMPanel);
    procedure PlanRoad;
    procedure PlanField;
    procedure PlanWine;
    procedure ErasePlan;
    procedure Show;
    procedure Hide;
    function Visible: Boolean;
    procedure UpdateHotkeys;
    procedure UpdateState;
    procedure SaveToStream(aSaveStream : TKMemoryStream);
    procedure LoadFromStream(aSaveStream : TKMemoryStream);
  end;
Const BUTTON_BUILD_TAG2 = 2;

implementation
uses
  KM_CommonTypes,
  KM_GameSettings,
  KM_RenderUI, KM_Cursor, KM_HandsCollection, KM_ResTexts, KM_Resource, KM_ResFonts,
  KM_Utils, Math, KM_HandTypes,
  KM_ResTypes, KM_ResMapElements,
  KM_UtilsExt;


{ TKMGUIGameBuild }
constructor TKMGUIGameBuild.Create(aParent: TKMPanel);
var
  I, J, K, buttonsCount, top: Integer;
  //RT : TKMRoadType;
  FT : TKMLockFieldType;
begin

  inherited Create;
  fSelectedType := 0;

  fLastRoadType := lftRoadStone;

  Panel_Build := TKMScrollPanel.Create(aParent, TB_PAD, 44 + 100, TB_WIDTH + 10, aParent.Height - 50, [saVertical], bsMenu, ssCommon);
  Panel_Build.AnchorsStretch;
  Panel_Build.DoScrollUpdate := false;
  Panel_Build.OnMouseOver := Build_ButtonOver;
  Panel_Build.ScrollV_PadTop := 30;
  SetLength(Button_BuildTypes, 3);//include structures and decorations

  for I := 0 to High(Button_BuildTypes) do
  begin
    Button_BuildTypes[I] := TKMButton.Create(Panel_Build, 34 + 7 + 33 * I, 0, 33, 30, 0, rxGui, bsPaper);
    Button_BuildTypes[I].OnClick := BuildType_Click;

    if I = 0 then
    begin
      Button_BuildTypes[I].TexID := 741;
      Button_BuildTypes[I].Hint := gResTexts[268];
    end else
    if I = 1 then
    begin
      Button_BuildTypes[I].TexID := 39;
      Button_BuildTypes[I].Hint := gResTexts[2035];
    end else
    if I = 2 then
    begin
      Button_BuildTypes[I].TexID := 666;
      Button_BuildTypes[I].Hint := gResTexts[2036];
    end;

    Button_BuildTypes[I].MobilHint := true;
    Button_BuildTypes[I].Tag := I;
  end;
  top := Button_BuildTypes[0].Bottom + 3;
  Label_BuildType := TKMLabel.Create(Panel_Build, 0, top, Panel_Build.Width - 25, 20, '', fntOutline, taCenter);


    for FT := low(TKMLockFieldType) to High(TKMLockFieldType) do
    begin
      Button_BuildField[FT] := TKMButtonFlat.Create(Panel_Build, 37 * (byte(FT) mod 5), 37 + 37 * (byte(FT) div 5), 33, 33, LOCK_FIELD_GUI[FT]);
      Button_BuildField[FT].OnClickShift   := Build_ButtonClick;
      Button_BuildField[FT].Tag       := byte(FT);
      case FT of
        lftRoadStone: Button_BuildField[FT].Hint := gResTexts[1992];
        lftRoadWooden: Button_BuildField[FT].Hint := gResTexts[1993];
        lftRoadClay: Button_BuildField[FT].Hint := gResTexts[1994];
        lftRoadExclusive: Button_BuildField[FT].Hint := gResTexts[1995];
        lftPalisade: Button_BuildField[FT].Hint := gResTexts[1633];
        lftField: Button_BuildField[FT].Hint := gResTexts[TX_BUILD_FIELD];
        lftGrassField: Button_BuildField[FT].Hint := gResTexts[1990];
        lftVegetablesField: Button_BuildField[FT].Hint := gResTexts[1991];
        lftWineField: Button_BuildField[FT].Hint := gResTexts[TX_BUILD_WINE];
        lftRemove: Button_BuildField[FT].Hint := gResTexts[TX_BUILD_DEMOLISH];
      end;
    end;

  buttonsCount := 0;
  //check how many buttons we need
  K := 0;
  for I := 0 to high(HOUSE_GUI_TAB_ORDER) do
  begin
    for J := 0 to High(HOUSE_GUI_TAB_ORDER[I].H) do
      Inc(K, length(HOUSE_GUI_TAB_ORDER[I].H[J]));
    buttonsCount := max(buttonsCount, K);
  end;

  SetLength(Button_Build, buttonsCount);
  SetLength(Label_BuildTypes, length(HOUSE_GUI_TAB_ORDER) );

  for I := 0 to high(Label_BuildTypes) do
  begin
    Label_BuildTypes[I] := TKMLabel.Create(Panel_Build, 0, 0, Panel_Build.Width - 9, 17, '', fntOutLine, taCenter);
    Label_BuildTypes[I].Caption := gResTexts[HOUSE_GUI_TAB_ORDER[I].TextID];
  end;

  for I := 0 to high(Button_Build) do
  begin
    Button_Build[I] := TKMButtonFlat.Create(Panel_Build, 0, 0,33,33, 0);
    Button_Build[I].OnClickShift := Build_ButtonClick;
    Button_Build[I].OnMouseOver := Build_ButtonOver;
  end;

  Panel_Cost := TKMPanel.Create(aParent, TB_PAD, 44, TB_WIDTH, 100);

  Bevel_Build := TKMBevel.Create(Panel_Cost, 0, 0, TB_WIDTH, 0);
  Bevel_Build.Color.SetColor(0.05, 0.1, 0.05);
  Bevel_Build.BackAlpha := 0.4;

  Label_Build := TKMLabel.Create(Panel_Cost, 0, 0, TB_WIDTH, 0, '', fntOutline, taCenter);

  Image_Build_Selected := TKMImage.Create(Panel_Cost, 1, 20, 32, 32, 335);
  Image_Build_Selected.ImageCenter;

  Row_Cost := TKMCostsRowMulti.Create(Panel_Cost, 35, 20, 45 * 3, 21);
  Row_Cost.MobilHint := true;
  Row_Cost.WarePlan.Reset;

  Panel_Cost.Height := 60;
  Bevel_Build.Height := Panel_Cost.Height;

  Panel_Build.Top := Panel_Cost.Bottom + 3;

  Panel_Build.Height := Panel_Build.Height - Panel_Build.Top;
  Icons_UnlockingHouses := TKMIconsRow.Create(Panel_Build.MasterPanel, 0, 0, 37, 37);
  Icons_UnlockingHouses.Hitable := false;
  Icons_UnlockingHouses.MaxCountInRow := 5;
  DecorationCost := TKMVWaresButtonsMulti.Create(Panel_Build, 0, 600, Panel_Build.Width, 300);
  DecorationCost.Hide;

end;


procedure TKMGUIGameBuild.PlanRoad;
begin
  //Button_BuildRoad[rtStone].Down := True;
  //Build_ButtonClick(Button_BuildRoad[rtStone]);
  //Button_BuildField[lftRoadStone].Down := True;
  Build_ButtonClick(Button_BuildField[fLastRoadType], []);
end;


procedure TKMGUIGameBuild.PlanField;
begin
  //Button_BuildField.Down := True;
  //Build_ButtonClick(Button_BuildField);

  Button_BuildField[lftField].Down := True;
  Build_ButtonClick(Button_BuildField[lftField], []);
end;


procedure TKMGUIGameBuild.PlanWine;
begin
  //Button_BuildWine.Down := True;
  //Build_ButtonClick(Button_BuildWine);
  Button_BuildField[lftWineField].Down := True;
  Build_ButtonClick(Button_BuildField[lftWineField], []);
end;


procedure TKMGUIGameBuild.ErasePlan;
begin
  {Button_BuildCancel.Down := True;
  Build_ButtonClick(Button_BuildCancel);}
  Button_BuildField[lftRemove].Down := True;
  Build_ButtonClick(Button_BuildField[lftRemove], []);
end;


procedure TKMGUIGameBuild.Build_ButtonClick(Sender: TObject; Shift: TShiftState);

  procedure SetCost(aCursor: TKMCursorMode; aTag, aTexId, aWood, aStone, aTile: Integer; const aCaption: UnicodeString); overload;
  begin
    gCursor.Mode := aCursor;
    if aCursor <> cmRoad then
      gCursor.Tag1 := aTag;

    if aCursor = cmRoad then
      gCursor.RoadType := TKMRoadType(aTag)
    else
      gCursor.RoadType := rtNone;
    {Row_Cost[0].WareCount := IfThen(aWood <> 0, aWood, 0);
    Row_Cost[1].WareCount := IfThen(aStone <> 0, aStone, 0);
    Row_Cost[2].WareCount := IfThen(aTile <> 0, aTile, 0);}
    Row_Cost.WarePlan.Reset;
    Row_Cost.WarePlan.AddWare(wtTimber, aWood, true);
    Row_Cost.WarePlan.AddWare(wtStone, aStone, true);
    Row_Cost.WarePlan.AddWare(wtTile, aTile, true);

    Label_Build.Caption := aCaption;
    Image_Build_Selected.TexID := aTexId;
  end;

  procedure SetCost(aCursor: TKMCursorMode; aTag, aTexID : Integer; aWares : TKMWareTypeArray; aCounts : TIntegerArray; aCaption : UnicodeString); overload
  var I : Integer;
  begin
    gCursor.Mode := aCursor;
    if aCursor <> cmRoad then
      gCursor.Tag1 := aTag;

    if aCursor = cmRoad then
      gCursor.RoadType := TKMRoadType(aTag)
    else
      gCursor.RoadType := rtNone;

    Row_Cost.WarePlan.Reset;
    for I := 0 to High(aWares) do
      if aCounts[I] > 0 then
      Row_Cost.WarePlan.AddWare(aWares[I], aCounts[I]);
    Label_Build.Caption := aCaption;
    Image_Build_Selected.TexID := aTexId;

  end;

var
  I: Integer;
  house: TKMHouseType;
  houseSpec: TKMHouseSpec;
  FT : TKMLockFieldType;

  procedure SetHouseCost(aHouseType : TKMHouseType);
  var wood, stone, tile : Byte;
  begin
    houseSpec := gRes.Houses[aHouseType];
    wood := gMySpectator.Hand.GetHouseWoodCost(aHouseType);
    stone := gMySpectator.Hand.GetHouseStoneCost(aHouseType);
    tile := gMySpectator.Hand.GetHouseTileCost(aHouseType);


    SetCost(cmHouses, Byte(aHouseType), houseSpec.GUIIcon, wood, stone, tile, houseSpec.HouseName);
  end;
begin
  if Sender = nil then
  begin
    gCursor.Mode := cmNone;
    Exit;
  end;

  //Release all buttons (houses and fields)
  for I := 0 to Panel_Build.ChildPanel.ChildCount - 1 do
    if Panel_Build.ChildPanel.Childs[I] is TKMButtonFlat then
      TKMButtonFlat(Panel_Build.ChildPanel.Childs[I]).Down := False;

  for I := 1 to high(Button_Build) do
      Button_Build[I].Down := False;

  //Press the button
  TKMButtonFlat(Sender).Down := True;

  //Reset building mode and see if it needs to be changed
  SetCost(cmNone, 0, 0, 0, 0, 0, '');

  for FT := Low(TKMLockFieldType) to High(TKMLockFieldType) do
    if (Sender = Button_BuildField[FT]) and (Button_BuildField[FT].Visible) and (Button_BuildField[FT].Enabled) then
    begin
      case FT of
        lftRoadStone:         SetCost(cmRoad, 1, LOCK_FIELD_GUI[FT], 0, 1, 0, gResTexts[1992]);
        lftRoadWooden:        SetCost(cmRoad, 2, LOCK_FIELD_GUI[FT], 1 * byte(not gMySpectator.Hand.BuildDevUnlocked(26)), 0, 0, gResTexts[1993]);
        lftRoadClay:          SetCost(cmRoad, 3, LOCK_FIELD_GUI[FT], 0, 0, 1, gResTexts[1994]);
        lftRoadExclusive:     SetCost(cmRoad, 4, LOCK_FIELD_GUI[FT], 1 * byte(not gMySpectator.Hand.BuildDevUnlocked(9)), 1, 1, gResTexts[1995]);
        lftPalisade:          SetCost(cmPalisade, 0, LOCK_FIELD_GUI[FT], 1, 0, 0, gResTexts[1633]);
        lftField:             SetCost(cmField, 0, LOCK_FIELD_GUI[FT], 0, 0, 0, gResTexts[TX_BUILD_FIELD]);
        lftGrassField:        SetCost(cmGrassLand, 0, LOCK_FIELD_GUI[FT], 0, 0, 0, gResTexts[1990]);
        lftVegetablesField:   SetCost(cmVegeField, 0, LOCK_FIELD_GUI[FT], 0, 0, 0, gResTexts[1991]);
        lftWineField:         SetCost(cmWine, 0, LOCK_FIELD_GUI[FT], 1, 0, 0, gResTexts[TX_BUILD_WINE]);
        lftRemove:            SetCost(cmErase, 0, LOCK_FIELD_GUI[FT], 0, 0, 0, gResTexts[TX_BUILD_DEMOLISH]);
      end;
      if ssRight in Shift then
        if FT in [lftRoadStone, lftRoadWooden, lftRoadClay, lftRoadExclusive] then
          fLastRoadType := FT;
      Exit;
    end;



  if (Sender is TKMButtonFlat) and (TKMButtonFlat(Sender).Tag2 = 2) then
  begin
    with gDecorations[TKMButtonFlat(Sender).Tag] do
      SetCost(cmDecorations, TKMButtonFlat(Sender).Tag, GUIIcon, 0, 0, 0, gResTexts[TextID]);
    with gDecorations[TKMButtonFlat(Sender).Tag] do
    begin
      SetLength(DecorationCost.WarePlan, length(Cost) );
      for I := 0 to High(Cost) do
      begin
        DecorationCost.WarePlan[I].W := Cost[I].W;
        DecorationCost.WarePlan[I].C := Cost[I].C;

      If gMySpectator.Hand.BuildDevUnlocked(1) then
        DecorationCost.WarePlan[I].C := Round(DecorationCost.WarePlan[I].C * 0.9);
      end;
    end;
  end else
  if (Sender is TKMButtonFlat) and (TKMButtonFlat(Sender).Tag2 = 1) then //bridges
  begin
    with gRes.Structures[TKMButtonFlat(Sender).Tag] do
    begin
      SetCost(cmBridges, TKMButtonFlat(Sender).Tag, GUIIcon, 0, 0, 0, gResTexts[TextID]);
      Row_Cost.WarePlan.Reset;
      for I := 0 to Cost.Count - 1 do
        Row_Cost.WarePlan.AddWare(Cost[I].W, Cost[I].C);

    end;
    gCursor.MapEdDir := 0;
  end else
  begin
    house := TKMHouseType(TKMButton(Sender).Tag);
    SetHouseCost(house);
    //houseSpec := gRes.Houses[house];
    //SetCost(cmHouses, Byte(house), houseSpec.GUIIcon, houseSpec.WoodCost, houseSpec.StoneCost, houseSpec.TileCost, houseSpec.HouseName);
  end;
end;

procedure TKMGUIGameBuild.BuildType_Click(Sender: TObject);
begin
  fSelectedType := TKMButton(Sender).Tag;

  UpdateState;
end;

procedure TKMGUIGameBuild.Build_ButtonOver(Sender: TObject; Shift: TShiftState);
var I : Integer;
    H, H2 : TKMHouseType;
    icons : TKMWordArray;
begin
  Icons_UnlockingHouses.Hide;
  if fSelectedType > 0 then
    Exit;

  for I := 0 to High(Button_Build) do
    if Sender = Button_Build[I] then
    if Button_Build[I].TexID = 41 then
    begin
      H := TKMHouseType(TKMButtonFlat(Sender).Tag);
      if H = htNone then
        Exit;

      icons := [];
      for H2 in gRes.Houses[H].ReleasedBy do
      begin
        if H2 in [htNone, htAny] then
          Continue;
        SetLength(icons, length(icons) + 1);
        icons[high(icons)] := gRes.Houses[H2].GUIIcon;
      end;
      Icons_UnlockingHouses.SetIcons(icons);
      //Icons_UnlockingHouses.SetIcons()
      Icons_UnlockingHouses.Left := Button_Build[I].AbsRight;
      Icons_UnlockingHouses.Top := Button_Build[I].AbsTop - 37;
      Icons_UnlockingHouses.Caption := 'Unlocks after';
      Icons_UnlockingHouses.Show;
    end;

end;

procedure TKMGUIGameBuild.Show;
var FT : TKMLockFieldType;
  HasAny : Boolean;
begin
  //UpdateState;
  gCursor.Mode := cmNone;
  HasAny := false;
  Panel_Build.Show;
  Panel_Cost.Show;
  if gMySpectator.Hand.Locks.FieldLocked(fLastRoadType) then
  begin
    for FT := lftRoadStone to lftRoadExclusive do
      if not gMySpectator.Hand.Locks.FieldLocked(FT) then
        if Button_BuildField[FT].Visible and Button_BuildField[FT].Enabled then
        begin
          fLastRoadType := FT;
          HasAny := true;
          Break;
        end;
  end else
    HasAny := true;
  if HasAny then
    Build_ButtonClick(Button_BuildField[fLastRoadType], []);

  UpdateState;
end;


procedure TKMGUIGameBuild.Hide;
var oldVisible : Boolean;
begin
  //Reset cursor
  oldVisible := Visible;
  Icons_UnlockingHouses.Hide;

  Panel_Build.Hide;
  Panel_Cost.Hide;
  if oldVisible <> Panel_Build.Visible then
    Build_ButtonClick(nil, []);
end;


function TKMGUIGameBuild.Visible: Boolean;
begin
  Result := Panel_Build.Visible;
end;



procedure TKMGUIGameBuild.UpdateHotkeys;
begin
  Button_BuildField[lftRoadStone].Hint := GetHintWHotkey(TX_BUILD_ROAD_HINT, kfPlanRoad);
  Button_BuildField[lftField].Hint := GetHintWHotkey(TX_BUILD_FIELD_HINT, kfPlanField);
  Button_BuildField[lftWineField].Hint := GetHintWHotkey(TX_BUILD_WINE_HINT, kfPlanWine);
  Button_BuildField[lftRemove].Hint := GetHintWHotkey(TX_BUILD_CANCEL_HINT, kfErasePlan);
end;


procedure TKMGUIGameBuild.UpdateState;
var
  I, J, K, L: Integer;

var H : TKMHouseType;
  lastVisible, lastID, top : Integer;
  hasAny : Boolean;
  FT : TKMLockFieldType;
begin
  //standard
  hasAny := false;
  for I := 0 to High(Button_Build) do
  begin
    Button_Build[I].Hide;
    Button_Build[I].Tag2 := 0;
    Button_Build[I].RX := rxGui;

    if Panel_Build.MasterControl.CtrlOver = Button_Build[I] then
      hasAny := true;
  end;
  if not hasAny then
    Icons_UnlockingHouses.Hide;
  DecorationCost.Hide;

  top := 0;
  for I := 0 to byte(high(TKMLockFieldType)) div 5 do
  begin
    J := 0;
    //check each 5 icons
    for FT := TKMLockFieldType(I * 5) to TKMLockFieldType(I * 5 + 4) do
    begin
      if (FT = lftPalisade) and not gMySpectator.Hand.Locks.FieldLocked(FT) then
      begin
        if gMySpectator.Hand.Locks.HouseCanBuild(htWall) then
        begin
          Button_BuildField[FT].Top := top;
          Button_BuildField[FT].Left := 37 * (J mod 5);
          Button_BuildField[FT].Show;
          Button_BuildField[FT].TexID := LOCK_FIELD_GUI[FT];
          Button_BuildField[FT].Enable;
          Inc(J);
        end else
        begin
          Button_BuildField[FT].Top := top;
          Button_BuildField[FT].Left := 37 * (J mod 5);
          Button_BuildField[FT].Show;
          Button_BuildField[FT].Disable;
          Button_BuildField[FT].TexID := 41;
          Inc(J);
        end;

      end else
      if not gMySpectator.Hand.Locks.FieldLocked(FT) then
      begin
        Button_BuildField[FT].Top := top;
        Button_BuildField[FT].Left := 37 * (J mod 5);
        Button_BuildField[FT].Show;
        Inc(J);
      end else
        Button_BuildField[FT].Hide;

      Button_BuildField[FT].BackBevelColor := IfThen(fLastRoadType = FT, $4000FF00, $00000000);
    end;

    if J > 0 then
      Inc(top, 37);
  end;
  for I := 0 to High(Label_BuildTypes) do
    Label_BuildTypes[I].Hide;

  for I := 0 to High(Button_BuildTypes) do
    Button_BuildTypes[I].Top := top;

  top := Button_BuildTypes[0].Bottom + 3;
  Label_BuildType.Top := top;
  Label_BuildType.Show;
  inc(top, 17);

  if fSelectedType = 0 then
  begin
    dec(top, 17);
    Label_BuildType.Hide;
    K := 0;
    for I := 0 to High(HOUSE_GUI_TAB_ORDER) do
    begin
      hasAny := false;
      for L := 0 to High(HOUSE_GUI_TAB_ORDER[I].H) do
        for J := 0 to High(HOUSE_GUI_TAB_ORDER[I].H[L]) do
        begin
          H := HOUSE_GUI_TAB_ORDER[I].H[L, J];
          if H = htSign then
            Continue;
          if gMySpectator.Hand.Locks.HouseLock[H] <> hlNotVisible then
          begin
            hasAny := true;
            Break;
          end;
        end;
      if not hasAny then
        Continue;
      lastVisible := 0;

      Label_BuildTypes[I].Top := top;
      Label_BuildTypes[I].Show;
      Inc(top, 17);


      for L := 0 to High(HOUSE_GUI_TAB_ORDER[I].H) do
        for J := 0 to High(HOUSE_GUI_TAB_ORDER[I].H[L]) do
        begin
          H := HOUSE_GUI_TAB_ORDER[I].H[L, J];
          if H = htSign then //Sign is allowed only in MapEd
            Continue;
          if gMySpectator.Hand.Locks.HouseLock[H] = hlNotVisible then
            Continue;

          Button_Build[K].Left := lastVisible mod 5 * 37;
          Button_Build[K].Top := top + lastVisible div 5 * 37;
          Button_Build[K].Show;
          Button_Build[K].BackBevelColor := $40000000;
          Button_Build[K].DownColor := $FFFFFFFF;

          if gMySpectator.Hand.Locks.HouseCanBuild(H) then
          begin
            Button_Build[K].Hint := gRes.Houses[H].HouseName;
            Button_Build[K].TexID := gRes.Houses[H].GUIIcon;
            Button_Build[K].Tag := ord(H);
            Button_Build[K].Down := gCursor.Tag1 = Button_Build[K].Tag;
            Button_Build[K].OnClickShift := Build_ButtonClick;
          end else
          begin
            Button_Build[K].Hint := gResTexts[TX_HOUSE_NOT_AVAILABLE];
            Button_Build[K].TexID := 41;
            Button_Build[K].Tag := ord(H);
            Button_Build[K].OnClickShift := nil;
            Button_Build[K].Down := false;
          end;
          if (gMySpectator.Hand.Locks.HouseLock[H] = hlBlocked) or not gMySpectator.Hand.CanBuildHouse(H) then
            Button_Build[K].Disable
          else
            Button_Build[K].Enable;

          Inc(lastVisible);
          Inc(K);

        end;

      top := Button_Build[Max(K - 1, 0)].Bottom + 3;
      Inc(top, 4);

    end;

  end else
  if fSelectedType = 1 then //structures
  begin
    Label_BuildType.Caption := gResTexts[2035];
    lastVisible := 0;
    for I := 0 to gRes.Structures.Count - 1 do
    begin
      if gMySpectator.Hand.Locks.Structures[I] = ulNotVisible then
        Continue;

      Button_Build[lastVisible].Left := lastVisible mod 5 * 37;
      Button_Build[lastVisible].Top := top + lastVisible div 5 * 37;
      Button_Build[lastVisible].Tag2 := 1;
      Button_Build[lastVisible].Tag := I;
      Button_Build[lastVisible].Enabled := true;
      Button_Build[lastVisible].TexID := gRes.Structures[I].GuiIcon;
      Button_Build[lastVisible].Hint := gResTexts[gRes.Structures[I].TextID];
      Button_Build[lastVisible].OnClickShift := Build_ButtonClick;
      Button_Build[lastVisible].Show;
      Button_Build[lastVisible].BackBevelColor := $40000000;
      Button_Build[lastVisible].DownColor := $FFFFFFFF;

      if gMySpectator.Hand.Locks.Structures[I] = ulBlocked then
      begin
        Button_Build[lastVisible].Hint := gResTexts[TX_HOUSE_NOT_AVAILABLE];
        Button_Build[lastVisible].TexID := 41;
        Button_Build[lastVisible].Tag := I;
        Button_Build[lastVisible].OnClickShift := nil;
        Button_Build[lastVisible].Down := false;
      end;

      Inc(lastVisible);
    end;
  end else
  if fSelectedType = 2 then //decorations
  begin
    DecorationCost.Show;
    Label_BuildType.Caption := gResTexts[2036];
    lastVisible := 0;
    for I := 0 to High(gDecorations) do
    begin
      if gMySpectator.Hand.Locks.Decoration[I] = ulNotVisible then
        Continue;

      Button_Build[lastVisible].Left := lastVisible mod 5 * 37;
      Button_Build[lastVisible].Top := top + lastVisible div 5 * 37;
      Button_Build[lastVisible].Tag2 := 2;
      Button_Build[lastVisible].Tag := I;

      case gDecorations[I].DType of
        dtObject : Button_Build[I].RX := rxGui;
        else Button_Build[lastVisible].RX := rxTiles;
      end;
      case gDecorations[I].DType of
        dtObject : Button_Build[lastVisible].TexID := gDecorations[I].GuiIcon;
        dtTile : Button_Build[lastVisible].TexID := gDecorations[I].ID + 1;
        dtTileOverlay : Button_Build[lastVisible].TexID := gRes.Tileset.Overlay[gDecorations[I].ID].TileID + 1;
      end;
      case gDecorations[I].DType of
        dtObject : Button_Build[lastVisible].Hint := gResTexts[gDecorations[I].TextID];
        dtTile : Button_Build[lastVisible].Hint := '';
        dtTileOverlay : Button_Build[lastVisible].Hint := gResTexts[gRes.Tileset.Overlay[gDecorations[I].ID].Hint];
      end;


      Button_Build[lastVisible].Hint := gResTexts[gDecorations[I].TextID];
      Button_Build[lastVisible].Show;
      Button_Build[lastVisible].OnClickShift := Build_ButtonClick;
      If not gMySpectator.Hand.HasVWaresDec(gDecorations[I].Cost) then
      begin
        Button_Build[lastVisible].BackBevelColor := $400000FF;
        Button_Build[lastVisible].DownColor := $FF0000FF;
      end else
      begin
        Button_Build[lastVisible].BackBevelColor := $40000000;
        Button_Build[lastVisible].DownColor := $FFFFFFFF;
      end;
      //Button_build[lastVisible].Enabled := gMySpectator.Hand.HasVWaresDec(gDecorations[I].Cost);

      if gMySpectator.Hand.Locks.Decoration[I] = ulBlocked then
      begin
        Button_Build[lastVisible].Hint := gResTexts[TX_HOUSE_NOT_AVAILABLE];
        Button_Build[lastVisible].TexID := 41;
        Button_Build[lastVisible].Tag := -1;
        Button_Build[lastVisible].OnClickShift := nil;
        Button_Build[lastVisible].Down := false;
      end;

      Inc(lastVisible);
    end;
    DecorationCost.Top := Button_Build[lastVisible - 1].Bottom + 5;

  end;




  if Panel_Build.Visible then
    Panel_Build.UpdateScrolls;

end;


procedure TKMGUIGameBuild.SaveToStream(aSaveStream : TKMemoryStream);
begin
  Exit;
  aSaveStream.Write(fLastRoadType, sizeOf(fLastRoadType));
end;

procedure TKMGUIGameBuild.LoadFromStream(aSaveStream : TKMemoryStream);
begin
  Exit;
  aSaveStream.Read(fLastRoadType, sizeOf(fLastRoadType));
end;

end.
