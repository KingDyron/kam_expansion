unit KM_GUIGameBuild;
{$I KaM_Remake.inc}
interface
uses
  StrUtils, SysUtils, Classes,
  KM_Controls, KM_ControlsBase,
  KM_Defaults, KM_ControlsScroll, KM_ControlsWaresRow,
  KM_InterfaceGame, KM_ResHouses;


type
  TKMGUIGameBuild = class
  private
    procedure Build_ButtonClick(Sender: TObject);
    //procedure Build_ButtonOver(Sender: TObject; X, Y: Integer; Shift : TShiftState);
  protected
    Panel_Build: TKMScrollPanel;
      Panel_Cost : TKMPanel;
        Bevel_Build: TKMBevel;
        Label_Build: TKMLabel;
        Image_Build_Selected: TKMImage;
        Row_Cost : array[0..2] of TKMWaresRow;

      {Button_BuildRoad:array[rtStone..high(TKMRoadType)] of TKMButtonFlat;
      Button_BuildField: TKMButtonFlat;
      Button_BuildGrassLand: TKMButtonFlat;
      Button_BuildWine: TKMButtonFlat;
      Button_BuildPalisade: TKMButtonFlat;
      Button_BuildCancel: TKMButtonFlat;}
      Button_BuildField: array[TKMLockFieldType] of TKMButtonFlat;
      Label_BuildType: array of TKMLabel;
      Button_Build: array of TKMButtonFlat;
      Button_BuildBridge: array of TKMButtonFlat;
      //Button_BuildWalls: array [1..high(GUIHouseWallsOrder)] of TKMButtonFlat;
      //Button_BuildAdd: array [1..high(GUIHouseAddOrder)] of TKMButtonFlat;
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
  end;
Const BUTTON_BUILD_TAG2 = 2;

implementation
uses
  KM_GameSettings,
  KM_RenderUI, KM_Cursor, KM_HandsCollection, KM_ResTexts, KM_Resource, KM_ResFonts,
  KM_Utils, Math, KM_HandTypes,
  KM_ResTypes,
  KM_UtilsExt;


{ TKMGUIGameBuild }
constructor TKMGUIGameBuild.Create(aParent: TKMPanel);
var
  I, J, lastID, top: Integer;
  W : TKMWareType;
  //RT : TKMRoadType;
  FT : TKMLockFieldType;
begin

  inherited Create;

  Panel_Build := TKMScrollPanel.Create(aParent, TB_PAD, 44 + 100, TB_WIDTH + 25, aParent.MasterParent.Height - 350, [saVertical], bsMenu, ssCommon);

  Panel_Build.DoScrollUpdate := false;

    for FT := low(TKMLockFieldType) to High(TKMLockFieldType) do
    begin
      Button_BuildField[FT] := TKMButtonFlat.Create(Panel_Build, 37 * (byte(FT) mod 5), 37 + 37 * (byte(FT) div 5), 33, 33, LOCK_FIELD_GUI[FT]);
      Button_BuildField[FT].OnClick   := Build_ButtonClick;
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
    {for RT := rtStone to High(Button_BuildRoad) do
    begin
      Button_BuildRoad[RT]           := TKMButtonFlat.Create(Panel_Build,   -37 + byte(RT) * 37, 0, 33, 33, ROAD_GUI_PIC[RT]);
      Button_BuildRoad[RT].OnClick   := Build_ButtonClick;
      Button_BuildRoad[RT].Tag       := byte(RT);

      //if RT <> rtStone then
      //  Button_BuildRoad[RT].Hide;
    end;

    Button_BuildField       := TKMButtonFlat.Create(Panel_Build,  0, 37, 33, 33, 337);
    Button_BuildGrassLand   := TKMButtonFlat.Create(Panel_Build,  37, 37, 33, 33, 132);
    Button_BuildWine        := TKMButtonFlat.Create(Panel_Build,  74, 37, 33, 33, 336);
    Button_BuildPalisade    := TKMButtonFlat.Create(Panel_Build, 111, 37, 33, 33, 373);
    Button_BuildCancel      := TKMButtonFlat.Create(Panel_Build, 148, 37, 33, 33, 340);

    Button_BuildField.OnClick         := Build_ButtonClick;
    Button_BuildGrassLand.OnClick     := Build_ButtonClick;
    Button_BuildWine.OnClick          := Build_ButtonClick;
    Button_BuildCancel.OnClick        := Build_ButtonClick;
    Button_BuildPalisade.OnClick        := Build_ButtonClick;


    Button_BuildPalisade.Disable;}

  SetLength(Button_Build, 0);//just in case
  SetLength(Label_BuildType, length(HOUSE_GUI_TAB_ORDER));

  top := Button_BuildField[high(TKMLockFieldType)].Bottom + 3;

  J := 0;
  for I := 0 to high(HOUSE_GUI_TAB_ORDER) do
  begin
    lastID := 0;
    if I > 0 then
      top := Button_Build[J - 1].Bottom + 3;

    Label_BuildType[I] := TKMLabel.Create(Panel_Build, 0, top - 3, Panel_Build.Width, 20, gResTexts[HOUSE_GUI_TAB_ORDER[I].TextID], fntOutline, taCenter);
    Label_BuildType[I].Hitable := false;

    Inc(Top, 15);
    while (lastID < length(HOUSE_GUI_TAB_ORDER[I].H)) do
    begin
      SetLength(Button_Build, Length(Button_Build) + 1);//add new elements
      Button_Build[J] := TKMButtonFlat.Create(Panel_Build, lastID mod 5 * 37, top + (lastID div 5) * 37,33,33,gRes.Houses[HOUSE_GUI_TAB_ORDER[I].H[lastID]].GUIIcon);
      Button_Build[J].OnClick := Build_ButtonClick;
      Button_Build[J].Tag2 := BUTTON_BUILD_TAG2;
      Button_Build[J].Tag := Byte(HOUSE_GUI_TAB_ORDER[I].H[lastID]);
      Button_Build[J].Hint := gRes.Houses[HOUSE_GUI_TAB_ORDER[I].H[lastID]].HouseName;
      Inc(J);
      Inc(lastID);
    end;
  end;

  top := Button_Build[J - 1].Bottom + 10;
  SetLength(Button_BuildBridge, gRes.Bridges.Count);
  for I := 0 to High(Button_BuildBridge) do
  begin
    Button_BuildBridge[I] := TKMButtonFlat.Create(Panel_Build, I mod 5 * 37, top + I div 5 * 37,33,33, gRes.Bridges[I].GuiIcon);
    Button_BuildBridge[I].OnClick := Build_ButtonClick;
    Button_BuildBridge[I].Tag2 := 1;
    Button_BuildBridge[I].Tag := I;
    //Button_BuildBridge[I].Hint := gRes.Houses[HOUSE_GUI_TAB_ORDER[J].H[lastID]].HouseName;
  end;

  Panel_Cost := TKMPanel.Create(aParent, TB_PAD, 44, TB_WIDTH, aParent.MasterParent.Height - 350);

  Bevel_Build := TKMBevel.Create(Panel_Cost, 0, 0, TB_WIDTH, 0);
  Bevel_Build.Color.SetColor(0.05, 0.1, 0.05);
  Bevel_Build.BackAlpha := 0.4;

  Label_Build := TKMLabel.Create(Panel_Cost, 0, 0, TB_WIDTH, 0, '', fntOutline, taCenter);

  Image_Build_Selected := TKMImage.Create(Panel_Cost, 45, 40, 32, 32, 335);
  Image_Build_Selected.ImageCenter;

  for I := Low(Row_Cost) to High(Row_Cost) do
  begin

    case I of
      0: W := wtTimber;
      1: W := wtStone;
      2: W := wtTile;
      else
        W := wtNone;
    end;
    Row_Cost[I] := TKMWaresRow.Create(Panel_Cost, TB_WIDTH - 89, 25 + 23 * I, 50);
    Row_Cost[I].RX := rxGui;
    Row_Cost[I].TexID := gRes.Wares[W].GUIIcon;
    Row_Cost[I].Caption := gRes.Wares[W].Title;
    Row_Cost[I].Hint := gRes.Wares[W].Title;
    Row_Cost[I].Hitable := false;
    Row_Cost[I].Spacing := 12;
    Row_Cost[I].WareCntAsNumber := true;
    Row_Cost[I].ShowName := false;
    Row_Cost[I].TxtOffset := -25;
    Row_Cost[I].TextOffset := 15;
  end;
  Panel_Cost.Height := Row_Cost[high(Row_Cost)].Bottom + 3;
  Bevel_Build.Height := Panel_Cost.Height;

  Panel_Build.Top := Panel_Cost.Bottom + 3;

  Panel_Build.Height := Panel_Build.Height - Panel_Build.Top;

end;


procedure TKMGUIGameBuild.PlanRoad;
begin
  //Button_BuildRoad[rtStone].Down := True;
  //Build_ButtonClick(Button_BuildRoad[rtStone]);
  Button_BuildField[lftRoadStone].Down := True;
  Build_ButtonClick(Button_BuildField[lftRoadStone]);
end;


procedure TKMGUIGameBuild.PlanField;
begin
  //Button_BuildField.Down := True;
  //Build_ButtonClick(Button_BuildField);

  Button_BuildField[lftField].Down := True;
  Build_ButtonClick(Button_BuildField[lftField]);
end;


procedure TKMGUIGameBuild.PlanWine;
begin
  //Button_BuildWine.Down := True;
  //Build_ButtonClick(Button_BuildWine);
  Button_BuildField[lftWineField].Down := True;
  Build_ButtonClick(Button_BuildField[lftWineField]);
end;


procedure TKMGUIGameBuild.ErasePlan;
begin
  {Button_BuildCancel.Down := True;
  Build_ButtonClick(Button_BuildCancel);}
  Button_BuildField[lftRemove].Down := True;
  Build_ButtonClick(Button_BuildField[lftRemove]);
end;


procedure TKMGUIGameBuild.Build_ButtonClick(Sender: TObject);

  procedure SetCost(aCursor: TKMCursorMode; aTag, aTexId, aWood, aStone, aTile: Integer; const aCaption: UnicodeString);
  begin
    gCursor.Mode := aCursor;
    if aCursor <> cmRoad then
      gCursor.Tag1 := aTag;

    if aCursor = cmRoad then
      gCursor.RoadType := TKMRoadType(aTag)
    else
      gCursor.RoadType := rtNone;
    Row_Cost[0].WareCount := IfThen(aWood <> 0, aWood, 0);
    Row_Cost[1].WareCount := IfThen(aStone <> 0, aStone, 0);
    Row_Cost[2].WareCount := IfThen(aTile <> 0, aTile, 0);


    Label_Build.Caption := aCaption;
    Image_Build_Selected.TexID := aTexId;
  end;

var
  I: Integer;
  house: TKMHouseType;
  houseSpec: TKMHouseSpec;
  FT : TKMLockFieldType;
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

  for I := 0 to high(Button_BuildBridge) do
      Button_BuildBridge[I].Down := False;

  //Press the button
  TKMButtonFlat(Sender).Down := True;

  //Reset building mode and see if it needs to be changed
  SetCost(cmNone, 0, 0, 0, 0, 0, '');

 { if Button_BuildCancel.Down then
    SetCost(cmErase, 0, 340, 0, 0, 0, gResTexts[TX_BUILD_DEMOLISH])
  else
  if Button_BuildRoad[rtStone].Down then
    SetCost(cmRoad, 1, 335, 0, 1, 0, gResTexts[TX_BUILD_ROAD])
  else
  if Button_BuildRoad[rtWooden].Down then
    SetCost(cmRoad, 2, 822, 1, 0, 0, gResTexts[TX_BUILD_ROAD])
  else
  if Button_BuildRoad[rtClay].Down then
    SetCost(cmRoad, 3, 823, 0, 0, 1, gResTexts[TX_BUILD_ROAD])
  else
  if Button_BuildRoad[rtExclusive].Down then
    SetCost(cmRoad, 4, 824, 1, 1, 1, gResTexts[TX_BUILD_ROAD])
  else
  if Button_BuildField.Down then
    SetCost(cmField, 0, 337, 0, 0, 0, gResTexts[TX_BUILD_FIELD])
  else
  if Button_BuildGrassLand.Down then
    SetCost(cmGrassLand, 0, 132, 0, 0, 0, '')
  else
  if Button_BuildWine.Down then
    SetCost(cmWine, 0, 336, 1, 0, 0, gResTexts[TX_BUILD_WINE])
  else
  if Button_BuildPalisade.Down then
    SetCost(cmPalisade, 0, 373, 1, 0, 0, gResTexts[1633])
  else}
  for FT := Low(TKMLockFieldType) to High(TKMLockFieldType) do
    if (Sender = Button_BuildField[FT]) and (Button_BuildField[FT].Visible) and (Button_BuildField[FT].Enabled) then
    begin
      case FT of
        lftRoadStone:         SetCost(cmRoad, 1, LOCK_FIELD_GUI[FT], 0, 1, 0, gResTexts[1992]);
        lftRoadWooden:        SetCost(cmRoad, 2, LOCK_FIELD_GUI[FT], 1, 0, 0, gResTexts[1993]);
        lftRoadClay:          SetCost(cmRoad, 3, LOCK_FIELD_GUI[FT], 0, 0, 1, gResTexts[1994]);
        lftRoadExclusive:     SetCost(cmRoad, 4, LOCK_FIELD_GUI[FT], 1, 1, 1, gResTexts[1995]);
        lftPalisade:          SetCost(cmPalisade, 0, LOCK_FIELD_GUI[FT], 1, 0, 0, gResTexts[1633]);
        lftField:             SetCost(cmField, 0, LOCK_FIELD_GUI[FT], 0, 0, 0, gResTexts[TX_BUILD_FIELD]);
        lftGrassField:        SetCost(cmGrassLand, 0, LOCK_FIELD_GUI[FT], 0, 0, 0, gResTexts[1990]);
        lftVegetablesField:   SetCost(cmVegeField, 0, LOCK_FIELD_GUI[FT], 0, 0, 0, gResTexts[1991]);
        lftWineField:         SetCost(cmWine, 0, LOCK_FIELD_GUI[FT], 1, 0, 0, gResTexts[TX_BUILD_WINE]);
        lftRemove:            SetCost(cmErase, 0, LOCK_FIELD_GUI[FT], 0, 0, 0, gResTexts[TX_BUILD_DEMOLISH]);
      end;
      Exit;
    end;



  if (Sender is TKMButtonFlat) and (TKMButtonFlat(Sender).Tag2 = 1) then
  begin
    with gRes.Bridges[TKMButtonFlat(Sender).Tag] do
      SetCost(cmBridges, TKMButtonFlat(Sender).Tag, GUIIcon, 0, 0, 0, gResTexts[TextID]);
    gCursor.MapEdDir := 0;
  end else
  begin
    house := TKMHouseType(TKMButton(Sender).Tag);
    houseSpec := gRes.Houses[house];
    SetCost(cmHouses, Byte(house), houseSpec.GUIIcon, houseSpec.WoodCost, houseSpec.StoneCost, houseSpec.TileCost, houseSpec.HouseName);
  end;
end;

{procedure TKMGUIGameBuild.Build_ButtonOver(Sender: TObject; X, Y: Integer; Shift : TShiftState);
var ctrlDown : TKMControl;
begin
  if Sender = nil then
    Exit;
  if not (Sender is TKMButtonFlat) then
    Exit;

  if not (TKMButtonFlat(Sender).Tag2 = BUTTON_BUILD_TAG2) then
    Exit;
  ctrlDown := TKMButtonFlat(Sender).MasterPanel.MasterControl.CtrlDown;
  if ctrlDown = nil then
    Exit;
  if (ctrlDown is TKMButtonFlat) and (TKMButtonFlat(ctrlDown).Tag2 = BUTTON_BUILD_TAG2) then
  begin
    TKMButtonFlat(Sender).HighLightColor := $400707FF;
    TKMButtonFlat(Sender).Down := true;
  end;
end;}

procedure TKMGUIGameBuild.Show;
var FT : TKMLockFieldType;
begin
  Panel_Build.Show;
  Panel_Cost.Show;
  //UpdateState;
  gCursor.Mode := cmNone;
  for FT := lftRoadStone to lftRoadExclusive do
    if not gMySpectator.Hand.Locks.FieldLocked(FT) then
      if Button_BuildField[FT].Visible and Button_BuildField[FT].Enabled then
      begin
        Build_ButtonClick(Button_BuildField[FT]);
        Break;
      end;
  UpdateState;
end;


procedure TKMGUIGameBuild.Hide;
var oldVisible : Boolean;
begin
  //Reset cursor
  oldVisible := Visible;

  Panel_Build.Hide;
  Panel_Cost.Hide;
  if oldVisible <> Panel_Build.Visible then
    Build_ButtonClick(nil);

end;


function TKMGUIGameBuild.Visible: Boolean;
begin
  Result := Panel_Build.Visible;
end;



procedure TKMGUIGameBuild.UpdateHotkeys;
begin
  {Button_BuildRoad[rtStone].Hint   := GetHintWHotkey(TX_BUILD_ROAD_HINT,   kfPlanRoad);
  Button_BuildField.Hint  := GetHintWHotkey(TX_BUILD_FIELD_HINT,  kfPlanField);
  Button_BuildWine.Hint   := GetHintWHotkey(TX_BUILD_WINE_HINT,   kfPlanWine);
  Button_BuildCancel.Hint := GetHintWHotkey(TX_BUILD_CANCEL_HINT, kfErasePlan);}
  Button_BuildField[lftRoadStone].Hint := GetHintWHotkey(TX_BUILD_ROAD_HINT, kfPlanRoad);
  Button_BuildField[lftField].Hint := GetHintWHotkey(TX_BUILD_FIELD_HINT, kfPlanField);
  Button_BuildField[lftWineField].Hint := GetHintWHotkey(TX_BUILD_WINE_HINT, kfPlanWine);
  Button_BuildField[lftRemove].Hint := GetHintWHotkey(TX_BUILD_CANCEL_HINT, kfErasePlan);
end;


procedure TKMGUIGameBuild.UpdateState;
var
  I, J, K: Integer;

var H : TKMHouseType;
  lastVisible, lastID, top : Integer;
  hasAny : Boolean;
  FT : TKMLockFieldType;
begin
  //standard
  for I := 0 to High(Button_Build) do
    Button_Build[I].Hide;
  for I := 0 to High(Label_BuildType) do
    Label_BuildType[I].Hide;

  top := 0;
  for I := 0 to byte(high(TKMLockFieldType)) div 5 do
  begin
    J := 0;
    lastId := 0;
    //check each 5 icons
    for FT := TKMLockFieldType(I * 5) to TKMLockFieldType(I * 5 + 4) do
      if (FT = lftPalisade) and not gMySpectator.Hand.Locks.FieldLocked(FT) then
      begin
        if gMySpectator.Hand.Locks.HouseCanBuild(htWall) then
        begin
          Button_BuildField[FT].Top := top;
          Button_BuildField[FT].Left := 37 * (J mod 5);
          Button_BuildField[FT].Show;
          Button_BuildField[FT].TexID := LOCK_FIELD_GUI[FT];
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

    if J > 0 then
      Inc(top, 37);
  end;

  {for FT := Low(TKMLockFieldType) to High(TKMLockFieldType) do
  begin
    if byte(FT) + 1 mod 5 = 0 then
    begin
      lastId := 0;
      Inc(J, 5);
    end;
    if (FT = lftPalisade) and not gMySpectator.Hand.Locks.FieldLocked(FT) then
    begin
      if gMySpectator.Hand.Locks.HouseCanBuild(htWall) then
      begin
        Button_BuildField[FT].Top := top +  37 * (byte(FT) div 5);
        Button_BuildField[FT].Left := 37 * (lastId mod 5);
        Button_BuildField[FT].Show;
        Button_BuildField[FT].TexID := LOCK_FIELD_GUI[FT];
        Inc(J);
        Inc(lastId);
      end else
      begin
        Button_BuildField[FT].Top := top +  37 * (byte(FT) div 5);
        Button_BuildField[FT].Left := 37 * (lastId mod 5);
        Button_BuildField[FT].Show;
        Button_BuildField[FT].Disable;
        Button_BuildField[FT].TexID := 41;
        Inc(J);
        Inc(lastId);
      end;

    end else
    if not gMySpectator.Hand.Locks.FieldLocked(FT) then
    begin
      Button_BuildField[FT].Top := top + 37 * (byte(FT) div 5);
      Button_BuildField[FT].Left := 37 * (lastId mod 5);
      Button_BuildField[FT].Show;
      Inc(J);
      Inc(lastId);
    end else
      Button_BuildField[FT].Hide;
  end;}

  J := 0;
  for I := 0 to High(HOUSE_GUI_TAB_ORDER) do
  begin
    hasAny := false;
    for K := 0 to High(HOUSE_GUI_TAB_ORDER[I].H) do
      if gMySpectator.Hand.Locks.HouseLock[HOUSE_GUI_TAB_ORDER[I].H[K]] <> hlNotVisible then
      begin
        hasAny := true;
        break;
      end;

    if hasAny then
    begin
      Label_BuildType[I].Top := top - 3;
      Label_BuildType[I].Show;
      Inc(top, 17);
    end else
      Continue;

    lastID := 0;
    lastVisible := 0;
    while lastID < length(HOUSE_GUI_TAB_ORDER[I].H) do
    begin
      H := HOUSE_GUI_TAB_ORDER[I].H[lastID];
      if gMySpectator.Hand.Locks.HouseLock[H] <> hlNotVisible then
      begin
        Button_Build[J].Tag := Byte(H);
        Button_Build[J].Hint := gRes.Houses[H].HouseName;
        //Button_Build[J].TexID := gRes.Houses[H].GUIIcon;
        Button_Build[J].Top := top + lastVisible div 5 * 37;
        Button_Build[J].Left := lastVisible mod 5 * 37;
        Button_Build[J].Show;

        Inc(J);
        Inc(lastVisible);
      end;
      Inc(lastID);

    end;
    if J > 0 then
      top := Button_Build[J - 1].Bottom;
    
  end;

  top := Button_Build[J - 1].Bottom + 3;
  SetLength(Button_BuildBridge, gRes.Bridges.Count);
  for I := 0 to High(Button_BuildBridge) do
  begin
    Button_BuildBridge[I].Top := top + I div 5 * 37;
    Button_BuildBridge[I].Left := I mod 5 * 37;
    Button_BuildBridge[I].Hide;
    //Button_BuildBridge[I].Hint := gRes.Houses[HOUSE_GUI_TAB_ORDER[J].H[lastID]].HouseName;
  end;


    {case gMySpectator.Hand.Locks.HouseLock[H] of
      hlNone,
      hlDefault,
      hlBlocked,
      hlGranted,
      hlNotVisible : ;
    end;}


  {if gMySpectator.Hand.Locks.HouseCanBuild(htWall) then
  begin
    Button_BuildPalisade.TexID := 373;
    Button_BuildPalisade.Enable;
  end
  else
    Button_BuildPalisade.TexID := 41;
  }
  for I := 0 to high(Button_Build) do
    if Button_Build[I].Visible then
    begin
      H := TKMHouseType(Button_Build[I].Tag);
      if gMySpectator.Hand.Locks.HouseCanBuild(H) then
      begin
        Button_Build[I].Enabled := gMySpectator.Hand.CanBuildHouse(H);
        Button_Build[I].TexID := gRes.Houses[H].GUIIcon;
        Button_Build[I].OnClick := Build_ButtonClick;
        Button_Build[I].Hint := gRes.Houses[H].HouseName + gResTexts[TX_BUILD_HOUSE_POSTFIX];
      end
      else
      begin
        Button_Build[I].OnClick := nil;
        Button_Build[I].TexID := 41;
        Button_Build[I].Hint := gResTexts[TX_HOUSE_NOT_AVAILABLE]; //Building not available
      end;
    end;

  if Panel_Build.Visible then
    Panel_Build.UpdateScrolls;

end;

end.
