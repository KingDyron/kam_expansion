unit KM_GUIMapEdPlayerBlockHouse;
{$I KaM_Remake.inc}
interface
uses
   Classes,
   KM_Controls, KM_ControlsBase, KM_ControlsScroll,
   KM_Pics, KM_InterfaceGame, KM_Defaults;

type
  TKMMapEdPlayerBlockHouse = class
  private
    fLevels : Boolean;
    procedure SwitchPage(aPage : Byte);
    procedure Player_BlockHouseClick(Sender: TObject; X, Y : Integer; Shift: TShiftState);
    procedure Player_BlockHouseRefresh;
    procedure Player_ChangeToLvl(Sender : TObject);
    procedure Player_ChangePanels(Sender : TObject);
    procedure Player_BlockHouseOver(Sender : TObject; X, Y : Integer; Shift : TShiftState);
    procedure Player_BlockFieldOver(Sender : TObject; X, Y : Integer; Shift : TShiftState);


    procedure Player_StructureRefresh;
    procedure Player_BlockStructOver(Sender : TObject; Shift : TShiftState);
    procedure Player_BlockDecOver(Sender : TObject; Shift : TShiftState);
    procedure Player_DecorationRefresh;
    procedure Player_BlockStructClick(Sender: TObject; X, Y : Integer; Shift: TShiftState);
    procedure Player_BlockDecClick(Sender: TObject; X, Y : Integer; Shift: TShiftState);
  protected
    Panel_BlockHouse: TKMPanel;
    Button_SwitchToHouses,
    Button_SwitchToLvl,
    Button_SwitchToStructures,
    Button_SwitchToDecorations: TKMButton;
    Label_BlockHouse: array of TKMlabel;
    {Button_BlockHouse: array of TKMButtonFlat;
    Image_BlockHouse: array of TKMImage;}
    Button_BlockField: array[TKMLockFieldType] of TKMButtonFlatBlock;
    Panel_Houses : TKMScrollPanel;
      Button_BlockHouse: array of TKMButtonFlatBlock;
    Panel_Structures : TKMScrollPanel;
      Button_BlockStruct: array of TKMButtonFlatBlock;
    Panel_Decorations : TKMScrollPanel;
      Button_BlockDec: array of TKMButtonFlatBlock;
  public
    constructor Create(aParent: TKMPanel);

    procedure Show;
    function Visible: Boolean;
    procedure Hide;
  end;


implementation
uses
  KM_HandsCollection, KM_ResTexts, KM_HandLocks,
  KM_Resource, KM_ResHouses, KM_RenderUI, KM_ResFonts,
  KM_ResTypes, KM_HandTypes,
  KM_UtilsExt, KM_CommonUtils,
  KM_ResMapElements,
  SysUtils, Math;

{ TKMMapEdPlayerBlockHouse }
constructor TKMMapEdPlayerBlockHouse.Create(aParent: TKMPanel);
var
  I, J, K, L, top, lastID: Integer;
  H : TKMHouseType;
  FT : TKMLockFieldType;
begin
  inherited Create;

  Panel_BlockHouse := TKMPanel.Create(aParent, 0, 28, aParent.Width, aParent.MasterParent.Height - 350);
  Panel_BlockHouse.Height := Panel_BlockHouse.MasterPanel.Height - Panel_BlockHouse.AbsTop;

  with TKMLabel.Create(Panel_BlockHouse, 0, PAGE_TITLE_Y, Panel_BlockHouse.Width, 0, gResTexts[TX_MAPED_BLOCK_HOUSES], fntOutline, taCenter) do
    Anchors := [anLeft, anTop, anRight];

  fLevels := false;



  SetLength(Label_BlockHouse, length(HOUSE_GUI_TAB_ORDER));
  SetLength(Button_BlockHouse, 0);
  //SetLength(Image_BlockHouse, 0);
  top := 30;

  for FT := Low(TKMLockFieldType) to High(TKMLockFieldType) do
  begin
    Button_BlockField[FT] := TKMButtonFlatBlock.Create(Panel_BlockHouse, 9 + byte(FT) mod 5 * 37, top + (byte(FT) div 5) * 37,33,33, LOCK_FIELD_GUI[FT]);
    Button_BlockField[FT].Tag := byte(FT);
    Button_BlockField[FT].Tag2 := BUTTON_BLOCK_FIELD_TAG_2;
    Button_BlockField[FT].OnMouseDown := Player_BlockHouseClick;

    case FT of
      lftRoadStone: Button_BlockField[FT].Hint := gResTexts[1992];
      lftRoadWooden: Button_BlockField[FT].Hint := gResTexts[1993];
      lftRoadClay: Button_BlockField[FT].Hint := gResTexts[1994];
      lftRoadExclusive: Button_BlockField[FT].Hint := gResTexts[1995];
      lftPalisade: Button_BlockField[FT].Hint := gResTexts[1633];
      lftField: Button_BlockField[FT].Hint := gResTexts[TX_BUILD_FIELD];
      lftGrassField: Button_BlockField[FT].Hint := gResTexts[1990];
      lftVegetablesField: Button_BlockField[FT].Hint := gResTexts[1991];
      lftWineField: Button_BlockField[FT].Hint := gResTexts[TX_BUILD_WINE];
      lftRemove: Button_BlockField[FT].Hint := gResTexts[TX_BUILD_DEMOLISH];
    end;

  end;

  top := Button_BlockField[high(TKMLockFieldType)].Bottom + 3;


  Button_SwitchToHouses := TKMButton.Create(Panel_BlockHouse, 9, top, 33, 33, 741, rxGui, bsGame);
  Button_SwitchToHouses.OnClick := Player_ChangeToLvl;
  Button_SwitchToHouses.Hint := gResTexts[268];

  Button_SwitchToLvl := TKMButton.Create(Panel_BlockHouse, 44, top, 33, 33, 748, rxGui, bsGame);
  Button_SwitchToLvl.OnClick := Player_ChangeToLvl;
  Button_SwitchToLvl.Hint := gResTexts[1859];

  Button_SwitchToStructures := TKMButton.Create(Panel_BlockHouse, 79, top, 33, 33, 39, rxGui, bsGame);
  Button_SwitchToStructures.OnClick := Player_ChangePanels;
  Button_SwitchToStructures.Hint := gResTexts[2035];

  Button_SwitchToDecorations := TKMButton.Create(Panel_BlockHouse, 114, top, 33, 33, 666, rxGui, bsGame);
  Button_SwitchToDecorations.OnClick := Player_ChangePanels;
  Button_SwitchToDecorations.Hint := gResTexts[2036];

  top := Button_SwitchToLvl.Bottom + 3;
  J := 0;
  Panel_BlockHouse.MasterControl.AddMouseMoveCtrlSub(Player_BlockHouseOver);
  Panel_BlockHouse.MasterControl.AddMouseMoveCtrlSub(Player_BlockFieldOver);

  Panel_Houses := TKMScrollPanel.Create(Panel_BlockHouse, 0, top, Panel_BlockHouse.Width + 20, Panel_BlockHouse.Height - top, [saVertical], bsMenu, ssCommon);
  Panel_Houses.ScrollV.Left := Panel_Houses.ScrollV.Left;
  Panel_Houses.Padding.SetBottom(10);
  Panel_Houses.ScrollV_PadTop := 10;
  Panel_Houses.ScrollV_PadBottom := 10;
  Panel_Houses.ScrollV_PadLeft := -20;

  Panel_Structures := TKMScrollPanel.Create(Panel_BlockHouse, 0, top, Panel_BlockHouse.Width + 20, Panel_BlockHouse.Height - top, [saVertical], bsMenu, ssCommon);
  Panel_Structures.ScrollV.Left := Panel_Structures.ScrollV.Left;
  Panel_Structures.Padding.SetBottom(10);
  Panel_Structures.ScrollV_PadTop := 10;
  Panel_Structures.ScrollV_PadBottom := 10;
  Panel_Structures.ScrollV_PadLeft := -20;

  Panel_Decorations := TKMScrollPanel.Create(Panel_BlockHouse, 0, top, Panel_BlockHouse.Width + 20, Panel_BlockHouse.Height - top, [saVertical], bsMenu, ssCommon);
  Panel_Decorations.ScrollV.Left := Panel_Decorations.ScrollV.Left;
  Panel_Decorations.Padding.SetBottom(10);
  Panel_Decorations.ScrollV_PadTop := 10;
  Panel_Decorations.ScrollV_PadBottom := 10;
  Panel_Decorations.ScrollV_PadLeft := -20;
  top := 0;
  for I := 0 to high(HOUSE_GUI_TAB_ORDER) do
  begin
    if I > 0 then
      top := Button_BlockHouse[J - 1].Bottom + 3;

    Label_BlockHouse[I] := TKMLabel.Create(Panel_Houses, 0, top - 3, Panel_Houses.Width, 20, gResTexts[HOUSE_GUI_TAB_ORDER[I].TextID], fntOutline, taCenter);
    Inc(Top, 15);
    LastID := 0;
    for K := 0 to High(HOUSE_GUI_TAB_ORDER[I].H) do
    begin

      for L := 0 to High(HOUSE_GUI_TAB_ORDER[I].H[K]) do
      begin
        H := HOUSE_GUI_TAB_ORDER[I].H[K, L];
        SetLength(Button_BlockHouse, Length(Button_BlockHouse) + 1);//add new element

        Button_BlockHouse[J] := TKMButtonFlatBlock.Create(Panel_Houses, 9 + lastID mod 5 * 37, top + (lastID div 5) * 37,33,33,gRes.Houses[H].GUIIcon);
        Button_BlockHouse[J].Hint := gRes.Houses[H].HouseName;
        Button_BlockHouse[J].Tag := byte(H);
        Button_BlockHouse[J].Tag2 := BUTTON_BLOCK_HOUSE_TAG_2;
        Button_BlockHouse[J].OnMouseDown := Player_BlockHouseClick;

        Inc(J);
        Inc(lastID);
      end;
    end;
    top := Button_BlockHouse[J - 1].Bottom + 3;
  end;
  SetLength(Button_BlockStruct, gRes.Structures.Count);

  for I := 0 to High(Button_BlockStruct) do
  begin
    Button_BlockStruct[I] := TKMButtonFlatBlock.Create(Panel_Structures, 9 + I mod 5 * 37, (I div 5) * 37,33,33,gRes.Structures[I].GUIIcon);
    Button_BlockStruct[I].Hint := gResTexts[gRes.Structures[I].TextID];
    Button_BlockStruct[I].Tag := I;
    Button_BlockStruct[I].Tag2 := BUTTON_BLOCK_STRUCT_TAG_2;
    Button_BlockStruct[I].OnMouseOver:= Player_BlockStructOver;
    Button_BlockStruct[I].OnMouseDown:= Player_BlockStructClick;
  end;

  SetLength(Button_BlockDec, length(gDecorations));

  for I := 0 to High(Button_BlockDec) do
  begin
    Button_BlockDec[I] := TKMButtonFlatBlock.Create(Panel_Decorations, 9 + I mod 5 * 37, (I div 5) * 37,33,33,gDecorations[I].GUIIcon);
    Button_BlockDec[I].Hint := gResTexts[gDecorations[I].TextID];
    Button_BlockDec[I].Tag := I;
    Button_BlockDec[I].Tag2 := BUTTON_BLOCK_DECOR_TAG_2;
    Button_BlockDec[I].OnMouseOver:= Player_BlockDecOver;
    Button_BlockDec[I].OnMouseDown:= Player_BlockDecClick;
  end;

  SwitchPage(0);
end;

procedure TKMMapEdPlayerBlockHouse.SwitchPage(aPage: Byte);
begin
  Panel_Houses.Hide;
  Panel_Structures.Hide;
  Panel_Decorations.Hide;
  case aPage of
    0 : Panel_Houses.Show;
    1 : Panel_Structures.Show;
    2 : Panel_Decorations.Show;
  end;

  Button_SwitchToHouses.ShowImageEnabled := not fLevels and Panel_Houses.Visible;
  Button_SwitchToLvl.ShowImageEnabled := fLevels and Panel_Houses.Visible;
  Button_SwitchToStructures.ShowImageEnabled := Panel_Structures.Visible;
  Button_SwitchToDecorations.ShowImageEnabled := Panel_Decorations.Visible;

end;

procedure TKMMapEdPlayerBlockHouse.Player_ChangeToLvl(Sender: TObject);
var I: Integer;
  H : TKMHouseType;

begin

  if Sender = Button_SwitchToLvl then
    fLevels := true
  else
  if Sender = Button_SwitchToHouses then
    fLevels := false;

  if (Sender = Button_SwitchToLvl) or (Sender = Button_SwitchToHouses) then
    SwitchPage(0);

  for I := 0 to high(Button_BlockHouse) do
  begin
    Button_BlockHouse[I].Enable;
    Button_BlockHouse[I].Caption := '';
    Button_BlockHouse[I].SetBlock(0);
  end;

  if fLevels then
  begin
    for I := 0 to High(Button_BlockHouse) do
    begin
      H := TKMHouseType(Button_BlockHouse[I].Tag);
      Button_BlockHouse[I].Disable;
      if length(gRes.Houses[H].Levels) > 0 then
      begin
        Button_BlockHouse[I].Enable;

        if gMySpectator.Hand.Locks.HouseMaxLvl[H] > 0 then
        begin
          if gMySpectator.Hand.Locks.HouseMaxLvl[H] = (length(gRes.Houses[H].Levels) + 1) then
            Button_BlockHouse[I].Caption := 'max'
          else
            Button_BlockHouse[I].Caption := IntToStr(gMySpectator.Hand.Locks.HouseMaxLvl[H] - 1)
        end else
          Button_BlockHouse[I].Caption := '--';

      end;
    end;
  end else
    Player_BlockHouseRefresh;

end;

procedure TKMMapEdPlayerBlockHouse.Player_ChangePanels(Sender: TObject);
begin
  if Sender = Button_SwitchToStructures then
    SwitchPage(1)
  else
  if Sender = Button_SwitchToDecorations then
    SwitchPage(2);
end;

procedure TKMMapEdPlayerBlockHouse.Player_BlockHouseClick(Sender: TObject; X, Y : Integer; Shift: TShiftState);
var
  I, J, K: Integer;
  H: TKMHouseType;
  locks: TKMHandLocks;
  FT : TKMLockFieldType;
begin
  locks := gMySpectator.Hand.Locks;

  for FT := Low(TKMLockFieldType) to High(TKMLockFieldType) do
    if Sender = Button_BlockField[FT] then
    begin
      locks.SetFieldLocked(FT, not locks.FieldLocked(FT));
      Player_BlockHouseRefresh;
      Exit;
    end;

  if fLevels then
  begin
    I := TKMButtonFlatBlock(Sender).Tag;
    H := TKMHouseType(I);
    K := -1;
    for J := 0 to High(Button_BlockHouse) do
      if Sender = Button_BlockHouse[J]  then
      begin
        K := J;
        Break;
      end;
    if K = -1 then
    Exit;

    if ssLeft in Shift then
    begin
      J := locks.HouseMaxLvl[H];
      IncLoop(J, 0, length(gRes.Houses[H].Levels) + 1);
      locks.HouseMaxLvl[H] := J;
      //locks.HouseMaxLvl[H] := EnsureRange(locks.HouseMaxLvl[H] + 1, 0, length(gRes.Houses[H].Levels) + 1)
    end else
    if ssRight in Shift then
    begin
      J := locks.HouseMaxLvl[H];
      IncLoop(J, 0, length(gRes.Houses[H].Levels) + 1, -1);
      locks.HouseMaxLvl[H] := J;
    end;
      //locks.HouseMaxLvl[H] := EnsureRange(locks.HouseMaxLvl[H] - 1, 0, length(gRes.Houses[H].Levels) + 1);

    if gMySpectator.Hand.Locks.HouseMaxLvl[H] > 0 then
    begin
      if gMySpectator.Hand.Locks.HouseMaxLvl[H] = (length(gRes.Houses[H].Levels) + 1) then
        Button_BlockHouse[K].Caption := 'max'
      else
        Button_BlockHouse[K].Caption := IntToStr(gMySpectator.Hand.Locks.HouseMaxLvl[H] - 1)
    end else
      Button_BlockHouse[K].Caption := '--';
  end else
  begin
    I := TKMButtonFlatBlock(Sender).Tag;
    H := TKMHouseType(I);

    locks := gMySpectator.Hand.Locks;

    // Circling through 4 house lock values
    if ssLeft in Shift then
      // Straight direction
      locks.HouseLock[H] := TKMHandHouseLock(1 + ((Ord(locks.HouseLock[H])) mod 4))
    else
    if ssRight in Shift then
      // Reverse direction
      locks.HouseLock[H] := TKMHandHouseLock(1 + ((Ord(locks.HouseLock[H]) + 2) mod 4));

    Player_BlockHouseRefresh;
  end;
end;


procedure TKMMapEdPlayerBlockHouse.Player_BlockHouseRefresh;
var
  I: Integer;
  H: TKMHouseType;
  locks: TKMHandLocks;
  FT : TKMLockFieldType;
begin
  locks := gMySpectator.Hand.Locks;
  for FT := Low(TKMLockFieldType) to High(TKMLockFieldType) do
    if locks.FieldLocked(FT) then
      Button_BlockField[FT].SetBlock(4)
    else
      Button_BlockField[FT].SetBlock(0);
  if fLevels then Exit;


  for I := 0 to high(Button_BlockHouse) do
  begin
    H := TKMHouseType(Button_BlockHouse[I].Tag);
    Button_BlockHouse[I].SetBlock(byte(locks.HouseLock[H]));
    {case locks.HouseLock[H] of
      hlBlocked:  Image_BlockHouse[I].TexID := 32;
      hlGranted:  Image_BlockHouse[I].TexID := 33;
      hlNotVisible:  Image_BlockHouse[I].TexID := 91;
      else        Image_BlockHouse[I].TexID := 0;
    end;}
  end;
end;

procedure TKMMapEdPlayerBlockHouse.Player_BlockHouseOver(Sender: TObject; X, Y: Integer; Shift : TShiftState);
var ctrlDown : TKMControl;
  H1, H2 : TKMHouseType;
  K : Integer;
begin
  if Sender = nil then
    Exit;
  if not (Sender is TKMButtonFlatBlock) then
    Exit;

  if not (TKMButtonFlatBlock(Sender).Tag2 = BUTTON_BLOCK_HOUSE_TAG_2) then
    Exit;
  ctrlDown := TKMButtonFlatBlock(Sender).MasterPanel.MasterControl.CtrlDown;
  if ctrlDown = nil then
    Exit;
  if (ctrlDown is TKMButtonFlatBlock) and (TKMButtonFlatBlock(ctrlDown).Tag2 = BUTTON_BLOCK_HOUSE_TAG_2) then
  begin
    H1 := TKMHouseType(ctrlDown.Tag);
    H2 := TKMHouseType(TKMButtonFlatBlock(Sender).Tag);
    if fLevels then
    begin
      if gMySpectator.Hand.Locks.HouseMaxLvl[H1] <> gMySpectator.Hand.Locks.HouseMaxLvl[H2] then
      begin
        gMySpectator.Hand.Locks.HouseMaxLvl[H2] := Min(gMySpectator.Hand.Locks.HouseMaxLvl[H1], length(gRes.Houses[H2].Levels) + 1);
        Player_BlockHouseRefresh;
        for K := 0 to High(Button_BlockHouse) do
          if Button_BlockHouse[K] = Sender then
          begin
            if gMySpectator.Hand.Locks.HouseMaxLvl[H2] > 0 then
            begin
              if gMySpectator.Hand.Locks.HouseMaxLvl[H2] = (length(gRes.Houses[H2].Levels) + 1) then
                Button_BlockHouse[K].Caption := 'max'
              else
                Button_BlockHouse[K].Caption := IntToStr(gMySpectator.Hand.Locks.HouseMaxLvl[H2] - 1)
            end else
              Button_BlockHouse[K].Caption := '--';
          end;
      end;

    end else
    if gMySpectator.Hand.Locks.HouseLock[H1] <> gMySpectator.Hand.Locks.HouseLock[H2] then
    begin
      gMySpectator.Hand.Locks.HouseLock[H2] :=gMySpectator.Hand.Locks.HouseLock[H1];
      Player_BlockHouseRefresh;
    end;
  end;
end;

procedure TKMMapEdPlayerBlockHouse.Player_BlockFieldOver(Sender: TObject; X, Y: Integer; Shift : TShiftState);
var ctrlDown : TKMControl;
  F1, F2 : TKMLockFieldType;
begin
  if Sender = nil then
    Exit;
  if not (Sender is TKMButtonFlatBlock) then
    Exit;

  if not (TKMButtonFlatBlock(Sender).Tag2 = BUTTON_BLOCK_FIELD_TAG_2) then
    Exit;
  ctrlDown := TKMButtonFlatBlock(Sender).MasterPanel.MasterControl.CtrlDown;
  if ctrlDown = nil then
    Exit;
  if (ctrlDown is TKMButtonFlatBlock) and (TKMButtonFlatBlock(ctrlDown).Tag2 = BUTTON_BLOCK_FIELD_TAG_2) then
  begin
    F1 := TKMLockFieldType(ctrlDown.Tag);
    F2 := TKMLockFieldType(TKMButtonFlatBlock(Sender).Tag);
    if gMySpectator.Hand.Locks.FieldLocked(F1) <> gMySpectator.Hand.Locks.FieldLocked(F2) then
    begin
      gMySpectator.Hand.Locks.SetFieldLocked(F2, gMySpectator.Hand.Locks.FieldLocked(F1));
      Player_BlockHouseRefresh;
    end;
  end;
end;

procedure TKMMapEdPlayerBlockHouse.Player_StructureRefresh;
var I : Integer;
begin
  for I := 0 to High(Button_BlockStruct) do
  case gMySpectator.Hand.Locks.Structures[I] of
    ulUnlocked : Button_BlockStruct[I].SetBlock(0);
    ulBlocked : Button_BlockStruct[I].SetBlock(2);
    ulNotVisible : Button_BlockStruct[I].SetBlock(4);
  end;
end;

procedure TKMMapEdPlayerBlockHouse.Player_BlockStructOver(Sender: TObject; Shift: TShiftState);
var ctrlDown, ctrlOver : TKMControl;
  id1, id2 : Integer;
begin
  if Sender = nil then
    Exit;
  if not (Sender is TKMButtonFlatBlock) then
    Exit;

  if not (TKMButtonFlatBlock(Sender).Tag2 = BUTTON_BLOCK_STRUCT_TAG_2) then
    Exit;
  ctrlDown := TKMButtonFlatBlock(Sender).MasterPanel.MasterControl.CtrlDown;
  ctrlOver := TKMControl(Sender);

  if (ctrlDown is TKMButtonFlatBlock) and (TKMButtonFlatBlock(ctrlDown).Tag2 = TKMButtonFlatBlock(Sender).Tag2) then
  begin
    id1 := ctrlDown.Tag;
    id2 := ctrlOver.Tag;

    if id1 = id2 then
      Exit;
    if gMySpectator.Hand.Locks.Structures[id1] <> gMySpectator.Hand.Locks.Structures[id2] then
      gMySpectator.Hand.Locks.Structures[id2] := gMySpectator.Hand.Locks.Structures[id1];
    Player_StructureRefresh;
  end;

end;


procedure TKMMapEdPlayerBlockHouse.Player_DecorationRefresh;
var I : Integer;
begin
  for I := 0 to High(Button_BlockDec) do
  case gMySpectator.Hand.Locks.Decoration[I] of
    ulUnlocked : Button_BlockDec[I].SetBlock(0);
    ulBlocked : Button_BlockDec[I].SetBlock(2);
    ulNotVisible : Button_BlockDec[I].SetBlock(4);
  end;
end;

procedure TKMMapEdPlayerBlockHouse.Player_BlockDecOver(Sender: TObject; Shift: TShiftState);
var ctrlDown, ctrlOver : TKMControl;
  id1, id2 : Integer;
begin
  if Sender = nil then
    Exit;
  if not (Sender is TKMButtonFlatBlock) then
    Exit;
  ctrlOver := TKMControl(Sender);

  if not (TKMButtonFlatBlock(Sender).Tag2 = BUTTON_BLOCK_DECOR_TAG_2) then
    Exit;
  ctrlDown := TKMButtonFlatBlock(Sender).MasterPanel.MasterControl.CtrlDown;

  if (ctrlDown is TKMButtonFlatBlock) and (TKMButtonFlatBlock(ctrlDown).Tag2 = TKMButtonFlatBlock(Sender).Tag2) then
  begin
    id1 := ctrlDown.Tag;
    id2 := ctrlOver.Tag;
    if id1 = id2 then
      Exit;
    if gMySpectator.Hand.Locks.Decoration[id1] <> gMySpectator.Hand.Locks.Decoration[id2] then
      gMySpectator.Hand.Locks.Decoration[id2] := gMySpectator.Hand.Locks.Decoration[id1];
    Player_DecorationRefresh;
  end;
end;

procedure TKMMapEdPlayerBlockHouse.Player_BlockStructClick(Sender: TObject; X: Integer; Y: Integer; Shift: TShiftState);
var id : Integer;
begin
  if not (Sender is TKMButtonFlatBlock) then
    Exit;
  id := TKMButtonFlatBlock(Sender).Tag;
  case gMySpectator.Hand.Locks.Structures[id] of
    ulUnlocked : gMySpectator.Hand.Locks.Structures[id] := ulBlocked;
    ulBlocked : gMySpectator.Hand.Locks.Structures[id] := ulNotVisible;
    ulNotVisible : gMySpectator.Hand.Locks.Structures[id] := ulUnlocked;
  end;
  Player_StructureRefresh;
end;

procedure TKMMapEdPlayerBlockHouse.Player_BlockDecClick(Sender: TObject; X: Integer; Y: Integer; Shift: TShiftState);
var id : Integer;
begin
  if not (Sender is TKMButtonFlatBlock) then
    Exit;
  id := TKMButtonFlatBlock(Sender).Tag;
  case gMySpectator.Hand.Locks.Decoration[id] of
    ulUnlocked : gMySpectator.Hand.Locks.Decoration[id] := ulBlocked;
    ulBlocked : gMySpectator.Hand.Locks.Decoration[id] := ulNotVisible;
    ulNotVisible : gMySpectator.Hand.Locks.Decoration[id] := ulUnlocked;
  end;
  Player_DecorationRefresh;
end;

procedure TKMMapEdPlayerBlockHouse.Hide;
begin
  Panel_BlockHouse.Hide;
end;


procedure TKMMapEdPlayerBlockHouse.Show;
begin
  Player_BlockHouseRefresh;
  Player_DecorationRefresh;
  Player_StructureRefresh;
  Panel_BlockHouse.Show;
end;


function TKMMapEdPlayerBlockHouse.Visible: Boolean;
begin
  Result := Panel_BlockHouse.Visible;
end;


end.
