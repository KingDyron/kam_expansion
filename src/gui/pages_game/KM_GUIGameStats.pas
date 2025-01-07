unit KM_GUIGameStats;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math, StrUtils, SysUtils,
  KM_Controls, KM_ControlsBase, KM_ControlsScroll,
  KM_Defaults, KM_Houses,
  KM_InterfaceGame, KM_ResHouses, KM_CommonTypes;


type
  TKMHouseStatIcon = class
    public
      SPic : TKMImage;
      SQt, SWip : TKMLabel;
      Constructor Create(aPanel: TKMPanel; X, Y, aTexID: Integer; aHouseType : Byte);
  end;
  TKMUnitStatIcon = class
    public
      SPic : TKMImage;
      SQt, SWip : TKMLabel;
      Constructor Create(aPanel: TKMPanel; X, Y, aTexID: Integer; aUnitType : Byte);
  end;

  TKMGUIGameStats = class
  private
    fOnShowStats: TNotifyEvent;
    fHouseSketch: TKMHouseSketchEdit;
    fLastHouseUIDs: array [HOUSE_MIN..HOUSE_MAX] of Cardinal;
    fSetViewportEvent: TPointFEvent;
    procedure House_Stat_Clicked(Sender: TObject);
    procedure ResetUIDs;
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
    procedure Resize;
    procedure UpdateState;
    function Visible: Boolean;
  end;


implementation
uses
  KM_Entity,
  KM_RenderUI, KM_HandsCollection, KM_ResTexts, KM_Resource, KM_ResFonts, KM_ResUnits,
  KM_Hand, KM_Pics, KM_Points,
  KM_ResTypes;


{ TKMGUIGameStats }

constructor TKMHouseStatIcon.Create(aPanel: TKMPanel; X, Y, aTexID: Integer; aHouseType : Byte);
const
  HOUSE_W = 30;
var
  HT : TKMHouseType;

begin
  HT := TKMHouseType(aHouseType);

  SPic := TKMImage.Create(aPanel, X, Y, HOUSE_W, 30, aTexID);
  SPic.ImageCenter;
  SPic.Hint := gRes.Houses[HT].HouseName;
  SPic.Tag := aHouseType;

  SWip := TKMLabel.Create(aPanel, X + HOUSE_W, Y, '', fntGrey, taRight);
  SWip.Hitable := false;

  SQt := TKMLabel.Create(aPanel, X + HOUSE_W - 2, Y+16, '-', fntGrey, taRight);
  SQt.Hitable := false;
end;

constructor TKMUnitStatIcon.Create(aPanel: TKMPanel; X, Y, aTexID: Integer; aUnitType : Byte);
const
  UNIT_W = 26;
var
  UT : TKMUnitType;
begin
  UT := TKMUnitType(aUnitType);

  SPic := TKMImage.Create(aPanel, X, Y, UNIT_W, 30, aTexID);
  SPic.ImageCenter;
  SPic.Hint := gRes.Units[UT].GUIName;
  SPic.Tag := aUnitType;

  SWip := TKMLabel.Create(aPanel, X + UNIT_W, Y , '', fntGrey, taRight);
  SWip.Hitable := false;

  SQt := TKMLabel.Create(aPanel, X + UNIT_W - 2, Y + 16, '-', fntGrey, taRight);
  SQt.Hitable := false;
end;

constructor TKMGUIGameStats.Create(aParent: TKMPanel; aOnShowStats: TNotifyEvent; aSetViewportEvent: TPointFEvent);
const
  HOUSE_W = 30;
  UNIT_W = 26;
var
  I, K, Row: Integer;
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

  TKMBevel.Create(Panel_Stats, 5, (HOUSE_W+2), TB_WIDTH - 5, 90 );

  Stat_Units[utSerf] := TKMUnitStatIcon.Create(Panel_Stats, 5, HOUSE_W, gRes.Units[utSerf].GUIIcon, byte(utSerf));
  Stat_Units[utBuilder] := TKMUnitStatIcon.Create(Panel_Stats, 5 + UNIT_W, HOUSE_W, gRes.Units[utBuilder].GUIIcon, byte(utBuilder));

  Stat_Army := TKMUnitStatIcon.Create(Panel_Stats, 5 + (UNIT_W div 2), HOUSE_W*2 + 2, 665, 0);

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

    TKMBevel.Create(Panel_Stats, 5, offX*(HOUSE_W+2), TB_WIDTH - 5, (high(aHouses) div 4 + 1) * 30 );

    Stat_Units[UT] := TKMUnitStatIcon.Create(Panel_Stats, 5, offX*(HOUSE_W+2) + (high(aHouses) div 4) * 15, gRes.Units[UT].GUIIcon, byte(UT));

    SetLength(Stat_Houses[UT], length(aHouses));
    if high(aHouses) >= 4 then
      Inc(Row, high(aHouses) div 4);

    for K := 0 to High(aHouses) do
      Stat_Houses[UT][K] := TKMHouseStatIcon.Create(Panel_Stats, 10 + HOUSE_W * (K mod 4 + 1), offX*(HOUSE_W+2) + (K div 4) * HOUSE_W, gRes.Houses[ aHouses[K] ].GUIIcon, byte(aHouses[K])  );

  end;

    Button_ShowStats  := TKMButtonFlat.Create(Panel_Stats, TB_WIDTH - 30, 0, 30, 30, 669, rxGui);
    Button_ShowStats.OnClick := fOnShowStats;
    Button_ShowStats.Hint := gResTexts[TX_GAME_MENU_SHOW_STATS_HINT];
end;


destructor TKMGUIGameStats.Destroy;
var U : TKMUnitType;
  I : Integer;
begin
  FreeAndNil(fHouseSketch);
  for U := CITIZEN_MIN to CITIZEN_MAX do
  begin
    Stat_Units[U].Free;
    for I := 0 to High(Stat_Houses[U]) do
      Stat_Houses[U][I].Free;

  end;

  inherited
end;


//Resize stats page in a way to display data in more readable form
//Try to keep items in corresponding pairs and stack them when dont fit otherwise
procedure TKMGUIGameStats.Resize;
{const
  PAD_X = 4;
  PAD_Y = 4;
var
  I, K: Integer;
  rows: Integer;
  offX, nextWidth, lineHeight: Integer;
  needToCompact: Boolean;}
begin
  {lineHeight := Panel_StatBlock[0].Height + PAD_Y;
  //How many rows could fit
  rows := Panel_Stats.Height div (lineHeight);

  //Reposition ShowStats button
  if rows >= 12 then
    Button_ShowStats.Top := 0
  else if rows = 11 then
    Button_ShowStats.Top := lineHeight
  else
    Button_ShowStats.Top := 2 * lineHeight;

  //Adjoin rows till they fit
  K := 0;
  offX := 0;
  for I := 0 to High(StatPlan) do
  begin
    Panel_StatBlock[I].Left := offX;
    Panel_StatBlock[I].Top := K * lineHeight;

    Inc(offX, PAD_X + Panel_StatBlock[I].Width);

    //Return caret
    if I <> High(StatPlan) then
    begin
      needToCompact := (Length(StatPlan) - I) > (rows - K);
      nextWidth := Panel_StatBlock[I].Width + PAD_X;
      if not needToCompact or (offX + nextWidth > TB_WIDTH) then
      begin
        offX := 0;
        Inc(K);
      end;
    end;
  end; }
end;


procedure TKMGUIGameStats.UpdateState;
var
  I, K: Integer;
  HT: TKMHouseType;
  UT: TKMUnitType;
  uqty, qty, uwipQty, wipQty, hTotalConstrOpenedQty: Integer;
  doHighlight: Boolean;
begin

  //Serfs and builder are together so they need to be counted seperately
  Stat_Units[utSerf].SPic.FlagColor := gMySpectator.Hand.FlagColor;
  Stat_Units[utBuilder].SPic.FlagColor := gMySpectator.Hand.FlagColor;
  uqty := gMySpectator.Hand.Stats.GetUnitQty(utSerf);
  uwipQty := gMySpectator.Hand.Stats.GetUnitTraining(utSerf) - gMySpectator.Hand.Stats.GetUnitDismissing(utSerf);
  Stat_Units[utSerf].SQt.Caption := IfThen(uqty > 0, IntToStr(uqty), '-');

  Stat_Units[utSerf].SWip.Caption := IfThen(uwipQty > 0, '+' + IntToStr(uwipQty), '');

  uqty := gMySpectator.Hand.Stats.GetUnitQty(utBuilder);
  uwipQty := gMySpectator.Hand.Stats.GetUnitTraining(utBuilder) - gMySpectator.Hand.Stats.GetUnitDismissing(utBuilder);
  Stat_Units[utBuilder].SQt.Caption := IfThen(uqty > 0, IntToStr(uqty), '-');
  Stat_Units[utBuilder].SWip.Caption := IfThen(uwipQty > 0, '+' + IntToStr(uwipQty), '');

  Stat_Army.SQT.Caption := IntToStr(gMySpectator.Hand.Stats.GetArmyCount);
  Stat_Army.SPic.FlagColor := gMySpectator.Hand.FlagColor;

  for K := 0 to High(Stat_Houses[utSerf]) do
  begin
    HT := TKMHouseType(Stat_Houses[utSerf][K].SPic.Tag);
    qty := gMySpectator.Hand.Stats.GetHouseQty(HT);
    wipQty := gMySpectator.Hand.Stats.GetHouseWip(HT);
    Stat_Houses[utSerf][K].SQt.Caption := IfThen(qty > 0, IntToStr(qty), '-');
    Stat_Houses[utSerf][K].SWip.Caption := IfThen(wipQty > 0, '+' +IntToStr(wipQty), '');

    if qty > 0 then
    begin
      Stat_Houses[utSerf][K].SPic.OnClick := House_Stat_Clicked;
      Stat_Houses[utSerf][K].SPic.HighlightOnMouseOver := true;
    end
    else
    begin
      Stat_Houses[utSerf][K].SPic.OnClick := nil;
      Stat_Houses[utSerf][K].SPic.HighlightOnMouseOver := false;
    end;
  end;
  //now update for the rest of the units
  for I := 0 to high(StatUnitOrder) do
  begin
    hTotalConstrOpenedQty := 0;

    UT := StatUnitOrder[I];
    uqty := gMySpectator.Hand.Stats.GetUnitQty(UT);
    uwipQty := gMySpectator.Hand.Stats.GetUnitTraining(UT) - gMySpectator.Hand.Stats.GetUnitDismissing(UT);

    Stat_Units[UT].SPic.FlagColor := gMySpectator.Hand.FlagColor;

    for K := 0 to High(Stat_Houses[UT]) do
    begin
      HT := TKMHouseType(Stat_Houses[UT][K].SPic.Tag);
      qty := gMySpectator.Hand.Stats.GetHouseQty(HT);

      if HT <> htBarracks then
        hTotalConstrOpenedQty := hTotalConstrOpenedQty + gMySpectator.Hand.Stats.GetHouseOpenedQty(HT);

      if HT = htWallTower then
        hTotalConstrOpenedQty := hTotalConstrOpenedQty + gMySpectator.Hand.Stats.GetHouseOpenedQty(HT);

      Stat_Houses[UT][K].SPic.TexID := gRes.Houses[HT].GUIIcon;
      wipQty := gMySpectator.Hand.Stats.GetHouseWip(HT);
      Stat_Houses[UT][K].SQt.Caption := IfThen(qty > 0, IntToStr(qty), '-');
      Stat_Houses[UT][K].SWip.Caption := IfThen(wipQty > 0, '+' +IntToStr(wipQty), '');

      if qty > 0 then
      begin
        Stat_Houses[UT][K].SPic.OnClick := House_Stat_Clicked;
        Stat_Houses[UT][K].SPic.HighlightOnMouseOver := true;
      end
      else
      begin
        Stat_Houses[UT][K].SPic.OnClick := nil;
        Stat_Houses[UT][K].SPic.HighlightOnMouseOver := false;
      end;
    end;
    doHighlight := hTotalConstrOpenedQty > uqty + uwipQty;

    if doHighlight then
      Stat_Units[UT].SWip.FontColor := clStatsUnitMissingHL
    else
      Stat_Units[UT].SWip.FontColor := clStatsUnitDefault;

    if doHighlight then
      Stat_Units[UT].SQt.FontColor := clStatsUnitMissingHL
    else
      Stat_Units[UT].SQt.FontColor := clStatsUnitDefault;

    Stat_Units[UT].SQt.Caption := IfThen(not doHighlight and (uQty  = 0), '-', IntToStr(uQty));

    if  uqty + uwipQty > hTotalConstrOpenedQty then
      Stat_Units[UT].SQt.Caption := IntToStr(uQty) + '^';

    if uwipQty > 0 then
      Stat_Units[UT].SWip.Caption := '+' + IntToStr(uwipQty)
    else
    if uwipQty < 0 then
      Stat_Units[UT].SWip.Caption := IntToStr(uwipQty)
    else
      Stat_Units[UT].SWip.Caption := '';
      
  end;

end;


procedure TKMGUIGameStats.ResetUIDs;
var
  HT: TKMHouseType;
begin
  for HT := Low(fLastHouseUIDs) to High(fLastHouseUIDs) do
    fLastHouseUIDs[HT] := 0;
end;


procedure TKMGUIGameStats.House_Stat_Clicked(Sender: TObject);
var
  HT: TKMHouseType;
begin
  Assert(Sender is TKMImage);
  if not Assigned(fSetViewportEvent) then Exit;

  HT := TKMHouseType(TKMImage(Sender).Tag);

  gMySpectator.Hand.GetNextHouseWSameType(HT, fLastHouseUIDs[HT], fHouseSketch, [hstHouse, hstHousePlan]);
  if not fHouseSketch.IsEmpty then
  begin
    gMySpectator.Highlight := fHouseSketch;
    fSetViewportEvent(KMPointF(fHouseSketch.Entrance)); //center viewport on that house
    fLastHouseUIDs[HT] := fHouseSketch.UID;
  end;
end;


procedure TKMGUIGameStats.Show;
begin
  UpdateState;
  Resize;
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
