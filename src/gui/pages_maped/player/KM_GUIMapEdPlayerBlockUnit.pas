unit KM_GUIMapEdPlayerBlockUnit;
{$I KaM_Remake.inc}
interface
uses
  Classes,
  KM_Defaults,
  KM_ResTypes,
  KM_Controls, KM_ControlsBase, KM_ControlsScroll,
  KM_Pics, KM_InterfaceGame;

type
  TKMButtonFlatBlockArray = array of TKMButtonFlatBlock;

  TKMMapEdPlayerBlockUnit = class
  private
    fCounter : byte;
    procedure Player_BlockUnitClick(Sender: TObject; X, Y : Integer; Shift: TShiftState);
    procedure Player_Block_Over(Sender: TObject; Shift: TShiftState);
    procedure Player_BlockRefresh;
    procedure CreateButtons(var aTop : Integer; aCaption : String; var aButtons : TKMButtonFlatBlockArray; aUnitArr : TKMUnitTypeArray);
    function ButtonToHouseType(aButton : TObject) : TKMHouseType;
  protected
    Panel_BlockUnit: TKMScrollPanel;
      Button_BlockUnit,
      Button_BlockBarracksWarriors,
      Button_BlockTHWarriors,
      Button_BlockSiegeMachines,
      Button_BlockPalaceUnits,
      Button_BlockShips: TKMButtonFlatBlockArray;

  public
    constructor Create(aParent: TKMPanel);
    procedure Show;
    function Visible: Boolean;
    procedure Hide;
    procedure UpdatePlayerColor;
  end;


implementation
uses
  KM_HandsCollection,
  KM_RenderUI,
  KM_Resource, KM_ResTexts, KM_ResUnits, KM_ResFonts,
  KM_HandTypes;


{ TKMMapEdPlayerBlockUnit }
constructor TKMMapEdPlayerBlockUnit.Create(aParent: TKMPanel);
var
  I: Integer;
  top : Integer;
begin
  inherited Create;
  fCounter := 0;
  Panel_BlockUnit := TKMScrollPanel.Create(aParent, 9, 28, aParent.Width - 9, 400, [saVertical], bsMenu, ssCommon);
  Panel_BlockUnit.Height := Panel_BlockUnit.MasterPanel.Height - Panel_BlockUnit.AbsTop - 25;
  Panel_BlockUnit.AnchorsStretch;

  with TKMLabel.Create(Panel_BlockUnit, 0, PAGE_TITLE_Y, Panel_BlockUnit.Width, 0, gResTexts[TX_MAPED_BLOCK_UNITS], fntOutline, taCenter) do
    Anchors := [anLeft, anTop, anRight];
  top := 30;
  CreateButtons(top, '', Button_BlockUnit, SCHOOL_GAME_ORDER);
  CreateButtons(top, gResTexts[TX_MAPED_BLOCK_UNITS_IN_BARRACKS], Button_BlockBarracksWarriors, BARRACKS_GAME_ORDER);
  CreateButtons(top, gResTexts[TX_MAPED_BLOCK_UNITS_IN_TOWNHALL], Button_BlockTHWarriors, TH_GAME_ORDER);
  CreateButtons(top, gResTexts[1841], Button_BlockSiegeMachines, SIEGE_GAME_ORDER);
  CreateButtons(top, gResTexts[1882], Button_BlockPalaceUnits, PALACE_UNITS_ORDER);
  CreateButtons(top, gResTexts[2126], Button_BlockShips, SHIPYARD_ORDER);

end;

procedure TKMMapEdPlayerBlockUnit.CreateButtons(var aTop: Integer; aCaption : String; var aButtons: TKMButtonFlatBlockArray; aUnitArr: TKMUnitTypeArray);
var I : Integer;
  UT : TKMUnitType;
begin
  SetLength(aButtons, length(aUnitArr));

  TKMLabel.Create(Panel_BlockUnit, 9, aTop, Panel_BlockUnit.Width - 9, 0, aCaption, fntMetal, taLeft);
  aTop := aTop + 17;
  for I := 0 to High(aButtons) do
  begin
    UT := aUnitArr[I];
    aButtons[I] := TKMButtonFlatBlock.Create(Panel_BlockUnit, 9 + I mod 5 * 37, aTop + I div 5 * 37,
                                          33, 33,
                                          gRes.Units[UT].GUIIcon, rxGui);
    aButtons[I].Hint := gRes.Units[UT].GUIName;
    aButtons[I].Tag := Byte(UT);
    aButtons[I].Tag2 := BUTTON_BLOCK_Unit_TAG_2;
    aButtons[I].Tag3 := fCounter;
    aButtons[I].OnMouseDown := Player_BlockUnitClick;
    aButtons[I].OnMouseOver := Player_Block_Over;
  end;
  aTop := aButtons[high(aButtons)].Bottom + 3;
  Inc(fCounter);
end;

function TKMMapEdPlayerBlockUnit.ButtonToHouseType(aButton: TObject): TKMHouseType;
begin
  Result := htNone;
  if not (aButton is TKMButtonFlatBlock) then
    Exit;
  case TKMButtonFlatBlock(aButton).Tag3 of
    0 : Result := htSchool;
    1 : Result := htBarracks;
    2 : Result := htTownhall;
    3 : Result := htSiegeWorkshop;
    4 : Result := htPalace;
    5 : Result := htShipYard;
  end;
end;


procedure TKMMapEdPlayerBlockUnit.Player_BlockUnitClick(Sender: TObject; X, Y : Integer; Shift: TShiftState);
var
  I: Integer;
  U: TKMUnitType;
  UL : TKMHandUnitLock;
  HT : TKMHouseType;
begin
  I := TKMButtonFlat(Sender).Tag;
  U := TKMUnitType(I);
  HT := ButtonToHouseType(Sender);
  UL := gMySpectator.Hand.Locks.GetUnitBlocked(U, HT);
  if ssRight in Shift then
    gMySpectator.Hand.Locks.SetUnitBlocked(U, HT, TKMHandUnitLock( (1 + Ord(UL) + 1) mod 3))
  else
    gMySpectator.Hand.Locks.SetUnitBlocked(U, HT, TKMHandUnitLock( (1 + Ord(UL)) mod 3));


  Player_BlockRefresh;
end;

procedure TKMMapEdPlayerBlockUnit.Player_BlockRefresh;
  procedure RefreshButton(aButtons : TKMButtonFlatBlockArray);
  var
    I: Integer;
    UT: TKMUnitType;
    HT : TKMHouseType;
    UL : TKMHandUnitLock;
  begin
    HT := ButtonToHouseType(aButtons[0]);
    for I := 0 to High(aButtons) do
    begin
      UT := TKMUnitType(aButtons[I].Tag);
      UL := gMySpectator.Hand.Locks.GetUnitBlocked(UT, HT);
      case UL of
        ulUnlocked : aButtons[I].SetBlock(0);
        ulBlocked : aButtons[I].SetBlock(2);
        ulNotVisible : aButtons[I].SetBlock(4);
      end;
    end;

  end;
begin
  RefreshButton(Button_BlockUnit);
  RefreshButton(Button_BlockBarracksWarriors);
  RefreshButton(Button_BlockTHWarriors);
  RefreshButton(Button_BlockSiegeMachines);
  RefreshButton(Button_BlockPalaceUnits);
  RefreshButton(Button_BlockShips);
end;


procedure TKMMapEdPlayerBlockUnit.UpdatePlayerColor;
var
  I: Integer;
  col: Cardinal;
begin
  col := gMySpectator.Hand.FlagColor;

  for I := Low(Button_BlockUnit) to High(Button_BlockUnit) do
    Button_BlockUnit[I].FlagColor := col;
  for I := Low(Button_BlockBarracksWarriors) to High(Button_BlockBarracksWarriors) do
    Button_BlockBarracksWarriors[I].FlagColor := col;
  for I := Low(Button_BlockTHWarriors) to High(Button_BlockTHWarriors) do
    Button_BlockTHWarriors[I].FlagColor := col;

  for I := Low(Button_BlockSiegeMachines) to High(Button_BlockSiegeMachines) do
    Button_BlockSiegeMachines[I].FlagColor := col;

  for I := 0 to High(Button_BlockPalaceUnits) do
    Button_BlockPalaceUnits[I].FlagColor := col;
end;

procedure TKMMapEdPlayerBlockUnit.Player_Block_Over(Sender: TObject; Shift: TShiftState);
var ctrlDown : TKMControl;
  ctrlOver : TKMControl;
  UT1, UT2 : TKMUnitType;
  HT1, HT2 : TKMHouseType;
begin
  if Sender = nil then
    Exit;

  if not (sender is TKMButtonFlat) then
    Exit;

  if not (TKMButtonFlat(Sender).Tag2 = BUTTON_BLOCK_Unit_TAG_2) then
    Exit;

  ctrlDown := TKMButtonFlat(Sender).MasterPanel.MasterControl.CtrlDown;
  ctrlOver := TKMControl(Sender);
  if (ctrlDown = nil) or not (ctrlDown is TKMButtonFlat) then
    Exit;
  if ctrlDown = ctrlOver then
    Exit;

  if (TKMButtonFlat(ctrlDown).Tag2 = BUTTON_BLOCK_Unit_TAG_2) then
  begin
    UT1 := TKMUnitType(ctrlDown.Tag);
    UT2 := TKMUnitType(ctrlOver.Tag);
    HT1 := ButtonToHouseType(ctrlDown);
    HT2 := ButtonToHouseType(ctrlOver);
    if gMySpectator.Hand.Locks.GetUnitBlocked(UT1, HT1) <> gMySpectator.Hand.Locks.GetUnitBlocked(UT2, HT2) then
    begin
      //gMySpectator.Hand.Locks.HouseLock[H2] :=gMySpectator.Hand.Locks.HouseLock[H1];
      gMySpectator.Hand.Locks.SetUnitBlocked(UT2, HT2, gMySpectator.Hand.Locks.GetUnitBlocked(UT1, HT1));
      Player_BlockRefresh;
    end;
  end;


end;


procedure TKMMapEdPlayerBlockUnit.Show;
begin
  Player_BlockRefresh;
  Panel_BlockUnit.Show;
end;


procedure TKMMapEdPlayerBlockUnit.Hide;
begin
  Panel_BlockUnit.Hide;
end;


function TKMMapEdPlayerBlockUnit.Visible: Boolean;
begin
  Result := Panel_BlockUnit.Visible;
end;

end.
