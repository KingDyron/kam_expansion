unit KM_GUIGameStats;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math, StrUtils, SysUtils,
  KM_Controls, KM_ControlsBase, KM_ControlsScroll,
  KM_Defaults, KM_Houses,
  KM_InterfaceGame, KM_ResHouses, KM_CommonTypes;


type
  TKMHouseStatIcon = class(TKMControl)
    public
      HighlightOnMouseOver : Boolean;
      TexID : Word;
      ColorQT, ColorWip : Cardinal;
      SQt, SWip : String;
      Constructor Create(aPanel: TKMPanel; X, Y, aTexID: Integer; aHouseType : Byte);
      procedure Paint;override;
  end;
  TKMUnitStatIcon = class(TKMControl)
    public
      HighlightOnMouseOver : Boolean;
      TexID : Word;
      FlagColor : Cardinal;
      ColorQT, ColorWip : Cardinal;
      SQt, SWip : String;
      Constructor Create(aPanel: TKMPanel; X, Y, aTexID: Integer; aUnitType : Byte);
      procedure Paint;override;
  end;

  TKMGUIGameStats = class
  private
    fOnShowStats: TNotifyEvent;
    fHouseSketch: TKMHouseSketchEdit;
    fLastGroupUID: Cardinal;
    fLastUnitUIDs: array [CITIZEN_MIN..CITIZEN_MAX] of Cardinal;
    fLastHouseUIDs: array [HOUSE_MIN..HOUSE_MAX] of Cardinal;
    fSetViewportEvent: TPointFEvent;
    fLastTick : Cardinal;
    procedure House_Stat_Clicked(Sender: TObject);
    procedure Unit_Stat_Clicked(Sender: TObject);
    function DoReset : Boolean;
  protected
    Panel_Stats: TKMScrollPanel;
      Panel_StatBlock: array [0..STATS_LINES_CNT-1] of TKMPanel;
        Stat_Units: array[CITIZEN_MIN..CITIZEN_MAX] of TKMUnitStatIcon;
        Stat_Houses: array[CITIZEN_MIN..CITIZEN_MAX] of array of TKMHouseStatIcon;
        Stat_Army : TKMUnitStatIcon;
      Button_ShowStats: TKMButtonFlat;
  public
    constructor Create(aParent: TKMPanel; aOnShowStats: TNotifyEvent; aSetViewportEvent: TPointFEvent);
    destructor Destroy; override;

    procedure Show;
    procedure Hide;
    procedure UpdateState;
    function Visible: Boolean;
    procedure ResetUIDs;
  end;


implementation
uses
  KM_Entity,
  KM_RenderUI, KM_HandsCollection, KM_ResTexts, KM_Resource, KM_ResFonts, KM_ResUnits,
  KM_Hand, KM_Pics, KM_Points,
  KM_Units, KM_UnitGroup,
  KM_GameParams,
  KM_ResTypes,
  KM_UtilsExt;


{ TKMGUIGameStats }

constructor TKMHouseStatIcon.Create(aPanel: TKMPanel; X, Y, aTexID: Integer; aHouseType : Byte);
const
  HOUSE_W = 30;
var
  HT : TKMHouseType;
begin
  Inherited Create(aPanel, X, Y, HOUSE_W, 30);
  HT := TKMHouseType(aHouseType);
  Hint := gRes.Houses[HT].HouseName;
  Tag := aHouseType;
  TexID := aTexID;
  SQt := '-';
  SWip := '';
  ColorQT := clStatsUnitDefault;
  ColorWip := clStatsUnitDefault;
end;

procedure TKMHouseStatIcon.Paint;
var light : Single;
begin
  Inherited;
  light := 0;
  If HighlightOnMouseOver and (csOver in State) then
    light := 0.25;
  TKMRenderUI.WritePicture(AbsLeft, AbsTop, Width, Height, [], rxGui, TexID, Enabled, $FFFFFFFF, light);
  TKMRenderUI.WriteText(AbsLeft - 2, AbsTop, Width, SWip, fntGrey, taRight, ColorWip);
  TKMRenderUI.WriteText(AbsLeft, AbsTop + 16, Width, SQt, fntGrey, taRight, ColorQT);
end;

constructor TKMUnitStatIcon.Create(aPanel: TKMPanel; X, Y, aTexID: Integer; aUnitType : Byte);
const
  UNIT_W = 26;
var
  UT : TKMUnitType;
begin
  Inherited Create(aPanel, X, Y, UNIT_W, 30);
  UT := TKMUnitType(aUnitType);
  Hint := gRes.Units[UT].GUIName;
  Tag := aUnitType;
  TexID := aTexID;
  SQt := '-';
  SWip := '';
  ColorQT := clStatsUnitDefault;
  ColorWip := clStatsUnitDefault;
end;

procedure TKMUnitStatIcon.Paint;
var light : Single;
begin
  Inherited;
  light := 0;
  If HighlightOnMouseOver and (csOver in State) then
    light := 0.25;
  TKMRenderUI.WritePicture(AbsLeft, AbsTop, Width, Height, [], rxGui, TexID, Enabled, FlagColor, light);
  TKMRenderUI.WriteText(AbsLeft - 2, AbsTop, Width, SWip, fntGrey, taRight, ColorWip);
  TKMRenderUI.WriteText(AbsLeft, AbsTop + 16, Width, SQt, fntGrey, taRight, ColorQT);
end;

constructor TKMGUIGameStats.Create(aParent: TKMPanel; aOnShowStats: TNotifyEvent; aSetViewportEvent: TPointFEvent);
const
  HOUSE_W = 32;
  UNIT_W = 26;
var
  I, K, Row, C, gapX: Integer;
  UT: TKMUnitType;
  offX: Integer;
  aHouses : TKMHouseTypeArray;

begin
  inherited Create;

  fOnShowStats := aOnShowStats;
  fSetViewportEvent := aSetViewportEvent;
  fHouseSketch := TKMHouseSketchEdit.Create;
  ResetUIDs;

  Panel_Stats := TKMScrollPanel.Create(aParent, TB_PAD, 44, TB_WIDTH, aParent.Height - 50, [saVertical], bsMenu, ssCommon);
  Panel_Stats.ScrollV.Left := Panel_Stats.ScrollV.Left + 20;
  Panel_Stats.AnchorsStretch;
  //Panel_Stats.Anchors := [anLeft, anTop, anBottom];

  TKMBevel.Create(Panel_Stats, 5, (HOUSE_W+2), TB_WIDTH - 5, HOUSE_W * 3 + 3 );

  Stat_Units[utSerf] := TKMUnitStatIcon.Create(Panel_Stats, 5, HOUSE_W, gRes.Units[utSerf].GUIIcon, byte(utSerf));
  Stat_Units[utBuilder] := TKMUnitStatIcon.Create(Panel_Stats, 5 + UNIT_W, HOUSE_W, gRes.Units[utBuilder].GUIIcon, byte(utBuilder));
  Stat_Units[utHouseBuilder] := TKMUnitStatIcon.Create(Panel_Stats, 5, HOUSE_W * 2, gRes.Units[utHouseBuilder].GUIIcon, byte(utHouseBuilder));
  Stat_Units[utMountedSerf] := TKMUnitStatIcon.Create(Panel_Stats, 5 + UNIT_W, HOUSE_W*2, gRes.Units[utMountedSerf].GUIIcon, byte(utMountedSerf));
  Stat_Units[utFeeder] := TKMUnitStatIcon.Create(Panel_Stats, 5, HOUSE_W*3, gRes.Units[utFeeder].GUIIcon, byte(utFeeder));

  Stat_Army := TKMUnitStatIcon.Create(Panel_Stats, 5 + UNIT_W, HOUSE_W*3 + 2, 665, 0);
  Stat_Army.Hint := gResTexts[266];
  Stat_Army.Tag := -1;

  SetLength(Stat_Houses[utSerf], length(StatNonWorkerHouse));
  for I := 0 to High(StatNonWorkerHouse) do
  begin
    Stat_Houses[utSerf][I] := TKMHouseStatIcon.Create(Panel_Stats, 10 + HOUSE_W * (I mod 3 + 2), (HOUSE_W+2) + (I div 3) * HOUSE_W, gRes.Houses[ StatNonWorkerHouse[I] ].GUIIcon, byte(StatNonWorkerHouse[I])  );
  end;
    

  Row := 4;

  for I := 0 to high(StatUnitOrder) do
  begin
    UT := StatUnitOrder[I];
    offX := I + Row;
    aHouses := gRes.Units[UT].WorkerOfHouses;
    case Length(aHouses) of
      1, 2, 3, 4 : C := Length(aHouses);
      5, 6, 9 : C := 3;
      else C := 4;
    end;

    TKMBevel.Create(Panel_Stats, 5, offX*(HOUSE_W+2), TB_WIDTH - 5, (high(aHouses) div C + 1) * HOUSE_W );

    Stat_Units[UT] := TKMUnitStatIcon.Create(Panel_Stats, 5, offX*(HOUSE_W+2) + (high(aHouses) div C) * 16, gRes.Units[UT].GUIIcon, byte(UT));

    SetLength(Stat_Houses[UT], length(aHouses));
    if high(aHouses) >= C then
      Inc(Row, high(aHouses) div C);

    gapX := ((4 * HOUSE_W) - (C * HOUSE_W)) div 2;
    for K := 0 to High(aHouses) do
      Stat_Houses[UT][K] := TKMHouseStatIcon.Create(Panel_Stats, gapX + 10 + HOUSE_W * (K mod C + 1), offX*(HOUSE_W+2) + (K div C) * HOUSE_W, gRes.Houses[ aHouses[K] ].GUIIcon, byte(aHouses[K])  );

  end;
  for UT := CITIZEN_MIN to CITIZEN_MAX do
    Stat_Units[UT].Tag := byte(UT);

    Button_ShowStats  := TKMButtonFlat.Create(Panel_Stats, TB_WIDTH - 30, 0, 30, 30, 669, rxGui);
    Button_ShowStats.OnClick := fOnShowStats;
    Button_ShowStats.Hint := gResTexts[TX_GAME_MENU_SHOW_STATS_HINT];
end;


destructor TKMGUIGameStats.Destroy;
begin
  FreeAndNil(fHouseSketch);
  inherited
end;


procedure TKMGUIGameStats.UpdateState;
var
  K: Integer;
  HT: TKMHouseType;
  UT: TKMUnitType;
  uqty, qty, uwipQty, wipQty, hTotalConstrOpenedQty: Integer;
  doHighlight: Boolean;
  citizensNeeded : array[CITIZEN_MIN..CITIZEN_MAX] of Word;
  P : TKMHand;
  house : TKMHouse;
  I : Integer;
begin

  //Serfs and builder are together so they need to be counted seperately
  qty := gMySpectator.Hand.Stats.GetArmyCount;
  Stat_Army.SQT := qty.ToString;
  Stat_Army.FlagColor := gMySpectator.Hand.FlagColor;
  IF qty > 0 then
  begin
    Stat_Army.OnClick := Unit_Stat_Clicked;
    Stat_Army.HighlightOnMouseOver := true;
  end else
  begin
    Stat_Army.OnClick := nil;
    Stat_Army.HighlightOnMouseOver := false;
  end;

  FillChar(citizensNeeded, SizeOf(citizensNeeded), 0);
  P := gMySpectator.Hand;

  //Citizens
  //Count overall unit requirement (excluding Barracks and ownerless houses)
  for I := 0 to P.Houses.Count - 1 do
  begin
    house := P.Houses[I];
    if house.IsValid(htAny, false, true) then
    if not house.IsClosedForWorker then
    if not (house.HouseType in [htBarracks, htStore, htInn, htMarket]) then
    if gRes.Houses[house.HouseType].CanHasWorker then
      for K := 0 to high(house.HSpec.Workers) do
        begin
          UT := house.HSpec.Workers[K].UnitType;
          Inc(citizensNeeded[UT], 1);
        end;
  end;

  for UT := CITIZEN_MIN to CITIZEN_MAX do
    If Stat_Units[UT] <> nil then
    begin
      uqty := gMySpectator.Hand.Stats.GetUnitQty(UT);
      uwipQty := gMySpectator.Hand.Stats.GetUnitTraining(UT) - gMySpectator.Hand.Stats.GetUnitDismissing(UT);
      Stat_Units[UT].SQt := IfThen(uqty > 0, IntToKStr(uqty, 1000), '-');
      Stat_Units[UT].SWip := IfThen(uwipQty > 0, '+' + IntToStr(uwipQty), '');
      Stat_Units[UT].FlagColor := gMySpectator.Hand.FlagColor;

      Stat_Units[UT].OnClick := nil;
      Stat_Units[UT].HighlightOnMouseOver := false;
      If uqty > 0 then
      begin
        Stat_Units[UT].OnClick := Unit_Stat_Clicked;
        Stat_Units[UT].HighlightOnMouseOver := true;
      end;

      hTotalConstrOpenedQty := citizensNeeded[UT];

      for K := 0 to High(Stat_Houses[UT]) do
      begin
        HT := TKMHouseType(Stat_Houses[UT][K].Tag);
        qty := gMySpectator.Hand.Stats.GetHouseQty(HT);

        {if HT <> htBarracks then
          hTotalConstrOpenedQty := hTotalConstrOpenedQty + gMySpectator.Hand.Stats.GetHouseOpenedQty(HT);

        if HT = htWallTower then
          hTotalConstrOpenedQty := hTotalConstrOpenedQty + gMySpectator.Hand.Stats.GetHouseOpenedQty(HT);}

        Stat_Houses[UT][K].TexID := gRes.Houses[HT].GUIIcon;
        wipQty := gMySpectator.Hand.Stats.GetHouseWip(HT);
        Stat_Houses[UT][K].SQt := IfThen(qty > 0, IntToStr(qty), '-');
        Stat_Houses[UT][K].SWip := IfThen(wipQty > 0, '+' +IntToStr(wipQty), '');

        if qty > 0 then
        begin
          Stat_Houses[UT][K].OnClick := House_Stat_Clicked;
          Stat_Houses[UT][K].HighlightOnMouseOver := true;
        end
        else
        begin
          Stat_Houses[UT][K].OnClick := nil;
          Stat_Houses[UT][K].HighlightOnMouseOver := false;
        end;
      end;
      IF UT in [utSerf, utFeeder, utMountedSerf, utBuilder, utHouseBuilder] then
        doHighlight := false
      else
        doHighlight := hTotalConstrOpenedQty > uqty + uwipQty;

      if doHighlight then
        Stat_Units[UT].ColorWip := clStatsUnitMissingHL
      else
        Stat_Units[UT].ColorWip := clStatsUnitDefault;

      if doHighlight then
        Stat_Units[UT].ColorQT := clStatsUnitMissingHL
      else
        Stat_Units[UT].ColorQT := clStatsUnitDefault;

      Stat_Units[UT].SQt := IfThen(not doHighlight and (uQty  = 0), '-', IntToKStr(uqty, 1000));

      IF not (UT in [utSerf, utFeeder, utMountedSerf, utBuilder, utHouseBuilder]) then
        if  uqty + uwipQty > hTotalConstrOpenedQty then
          Stat_Units[UT].SQt := IntToKStr(uQty, 1000) + '^';

      if uwipQty > 0 then
        Stat_Units[UT].SWip := '+' + IntToStr(uwipQty)
      else
      if uwipQty < 0 then
        Stat_Units[UT].SWip := IntToStr(uwipQty)
      else
        Stat_Units[UT].SWip := '';
    end;
end;


procedure TKMGUIGameStats.ResetUIDs;
var
  HT: TKMHouseType;
  UT : TKMUnitType;
begin
  for HT := Low(fLastHouseUIDs) to High(fLastHouseUIDs) do
    fLastHouseUIDs[HT] := 0;
  for UT := Low(fLastUnitUIDs) to High(fLastUnitUIDs) do
    fLastUnitUIDs[UT] := 0;
  fLastGroupUID := 0;
end;


function TKMGUIGameStats.DoReset: Boolean;
begin
  Result := gGameParams.Tick > fLastTick;
end;
procedure TKMGUIGameStats.House_Stat_Clicked(Sender: TObject);
var
  HT: TKMHouseType;
begin
  Assert(Sender is TKMHouseStatIcon);
  if not Assigned(fSetViewportEvent) then Exit;

  If DoReset then
  begin
    FillChar(fLastGroupUID, Sizeof(fLastGroupUID), #0);
    FillChar(fLastUnitUIDs, Sizeof(fLastUnitUIDs), #0);
    FillChar(fLastHouseUIDs, Sizeof(fLastHouseUIDs), #0);
  end;
  fLastTick := gGameParams.Tick + 600;

  HT := TKMHouseType(TKMHouseStatIcon(Sender).Tag);

  gMySpectator.Hand.GetNextHouseWSameType(HT, fLastHouseUIDs[HT], false, fHouseSketch, [hstHouse, hstHousePlan]);
  if not fHouseSketch.IsEmpty then
  begin
    gMySpectator.Highlight := fHouseSketch;
    fSetViewportEvent(KMPointF(fHouseSketch.Entrance)); //center viewport on that house
    fLastHouseUIDs[HT] := fHouseSketch.UID;
  end;
end;

procedure TKMGUIGameStats.Unit_Stat_Clicked(Sender: TObject);
var
  UT: TKMUnitType;
  U : TKMUnit;
  G : TKMUnitGroup;
begin
  Assert(Sender is TKMUnitStatIcon);
  if not Assigned(fSetViewportEvent) then Exit;

  If DoReset then
  begin
    FillChar(fLastGroupUID, Sizeof(fLastGroupUID), #0);
    FillChar(fLastUnitUIDs, Sizeof(fLastUnitUIDs), #0);
    FillChar(fLastHouseUIDs, Sizeof(fLastHouseUIDs), #0);
  end;

  fLastTick := gGameParams.Tick + 600;

  If sender = Stat_Army then
  begin
    G := gMySpectator.Hand.GetNextGroupWSameType(utAny, fLastGroupUID);
    If (G <> nil) and not G.IsDead then
    begin
      gMySpectator.Highlight := G;
      fSetViewportEvent(G.FlagBearer.PositionF); //center viewport on that house
      fLastGroupUID := G.UID;
    end;
    Exit;
  end;
  UT := TKMUnitType(TKMUnitStatIcon(Sender).Tag);

  U := gMySpectator.Hand.GetNextUnitWSameType(UT, fLastUnitUIDs[UT]);

  if (U <> nil) and not U.IsDeadOrDying then
  begin
    gMySpectator.Highlight := U;
    fSetViewportEvent(U.PositionF); //center viewport on that house
    fLastUnitUIDs[UT] := U.UID;
  end;
end;


procedure TKMGUIGameStats.Show;
begin
  UpdateState;
  Panel_Stats.Show;
end;


procedure TKMGUIGameStats.Hide;
begin
  Panel_Stats.Hide;
end;


function TKMGUIGameStats.Visible: Boolean;
begin
  Result := Panel_Stats.Visible;
end;


end.
