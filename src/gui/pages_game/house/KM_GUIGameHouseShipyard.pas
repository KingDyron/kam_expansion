unit KM_GUIGameHouseShipyard;
{$I KaM_Remake.inc}
interface
uses
  StrUtils, SysUtils, Math, Classes,
  KM_Defaults,
  KM_ResTypes,
  KM_Controls, KM_ControlsBase, KM_ControlsSwitch, KM_ControlsWaresRow,
  KM_Houses, KM_HouseShipyard,
  Vcl.Controls;

const MAX_DOCKS = 16;
type
  TKMWareButton = class(TKMButtonFlat)
    private
      const
        BLOCK_BUTTON_SIZE = 15;
    public
      OnBlockWare : TNotifyEventShift;
      OnShiftClick : TNotifyEventShift;
      Blocked : Boolean;

    procedure MouseUp   (X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;
    constructor Create(aParent : TKMPanel; aX, aY : Integer; aWare : TKMWareType; ClickShift, BlockWare : TNotifyEventShift);
    procedure Paint; override;
  end;



  TKMGuiGameShipyard = class(TKMPanel)
    private
      procedure Refresh(Shipyard : TKMHouseShipyard);
      procedure WareClickShift(Sender : TObject; Shift : TShiftState);
      procedure WareBlock(Sender : TObject; Shift : TShiftState);
      procedure ButtonClickShift(Sender : TObject; Shift : TShiftState);
      procedure ButtonWheel(Sender : TObject; WheelSteps : Integer; var Handled : Boolean);
    protected
      Button_Ware : array[1..WARES_IN_OUT_COUNT] of TKMWareButton;
      Button_Shipyard : TKMButtonFlat;
      Button_Dock : array[0..MAX_DOCKS - 1] of TKMButtonFlat;
      Wares_Out : TKMWaresButtonsMulti;

      Ship_Bevel: array of TKMBevel;
      Ship_Image: array of TKMImage;
      Ship_Cost : array of TKMCostsRowMultiCol;
    public
      constructor Create(aParent: TKMPanel);
      procedure Show(aHouse : TKMHouse; aTop : Integer); Reintroduce;
  end;

implementation
uses
  KM_Points,
  KM_CommonTypes,
  KM_Game, KM_GameInputProcess,
  KM_HandsCollection,
  KM_RenderUI, KM_Cursor,
  KM_Resource, KM_ResTexts, KM_ResUnits, KM_ResFonts,
  KM_UnitWarrior,
  KM_UtilsExt;

const
  DOCKS_POSITIONS : array[0..MAX_DOCKS - 1] of TKMPoint = (
        (X: -1; Y: -3),
        (X: 0; Y: -3),
        (X: 1; Y: -3),
        (X: 2; Y: -3),

        (X: -1; Y: 2),
        (X: 0; Y: 2),
        (X: 1; Y: 2),
        (X: 2; Y: 2),

        (X: -2; Y: -2),
        (X: -2; Y: -1),
        (X: -2; Y: 0),
        (X: -2; Y: 1),

        (X: 3; Y: -2),
        (X: 3; Y: -1),
        (X: 3; Y: 0),
        (X: 3; Y: 1)
        );


constructor TKMGuiGameShipyard.Create(aParent: TKMPanel);
var I : integer;
  U : TKMUnitType;
begin
  Inherited Create(aParent, 0, 20, aParent.Width - 8, 300);

  for I := 1 to WARES_IN_OUT_COUNT do
  begin
    Button_Ware[I] := TKMWareButton.Create(self, -32 + 32 * I, 20, gRes.Houses[htShipYard].WareInput[I], WareClickShift, WareBlock);
    Button_Ware[I].Caption := '0';
    Button_Ware[I].Tag := I;
  end;


  Button_Shipyard := TKMButtonFlat.Create(self, 0, 120, 60, 60, 872);
  Button_Shipyard.SetPosCenterW;

  for I := 0 to MAX_DOCKS - 1 do
  begin
    Button_Dock[I] := TKMButtonFlat.Create(self,  Button_Shipyard.Left + DOCKS_POSITIONS[I].X * 30,
                                                  Button_Shipyard.Top + 30 + DOCKS_POSITIONS[I].Y * 30, 30, 30, 0);
    Button_Dock[I].OnClickShift := ButtonClickShift;
    Button_Dock[I].OnMouseWheel := ButtonWheel;
    Button_Dock[I].Hint := gResTexts[2377];
  end;

  Wares_Out := TKMWaresButtonsMulti.Create(self, 0, 250, Width, 60);

  SetLength(Ship_Bevel, length(SHIPYARD_ORDER) - 1);//skip utNone
  SetLength(Ship_Image, length(SHIPYARD_ORDER) - 1);//skip utNone
  SetLength(Ship_Cost, length(SHIPYARD_ORDER) - 1);//skip utNone

  for I := 0 to High(Ship_Bevel) do
  begin
    U := SHIPYARD_ORDER[I + 1];
    Ship_Bevel[I] := TKMBevel.Create(self, 0, 350 + I * 55, Width + 5, 53);
    Ship_Image[I] := TKMImage.Create(self, 5, 365 + I * 55, 40, 30, gRes.Units[U].GUIIcon);
    Ship_Image[I].AnchorsCenter;

    Ship_Cost[I] := TKMCostsRowMultiCol.Create(self, 40, 350 + I * 55, Width - 37, 53, 3);
  end;


end;

procedure TKMGuiGameShipyard.Show(aHouse : TKMHouse; aTop : Integer);
begin
  Inherited Show;
  Top := aTop;
  Refresh(TKMHouseShipyard(aHouse));
end;

procedure TKMGuiGameShipyard.Refresh(Shipyard: TKMHouseShipyard);

  function GetButtonDockAt(X, Y : Integer) : Integer;
  var I : Integer;
  begin
    Result := -1;
    for I := 0 to MAX_DOCKS - 1 do
      If (X = DOCKS_POSITIONS[I].X) and (Y = DOCKS_POSITIONS[I].Y) then
        Exit(I);

  end;

var I, J, K : integer;
  dock : TKMShipyardDockWithLoc;
begin
  J := Shipyard.DocksCount;
  for I := 0 to MAX_DOCKS - 1 do
  begin
    Button_Dock[I].TexID := 479;
    Button_Dock[I].Hitable := false;
  end;
  for I := 0 to J - 1 do
  begin
    dock := Shipyard.GetDock(I);
    dock.Position.Loc.X := dock.Position.Loc.X - Shipyard.Entrance.X;
    dock.Position.Loc.Y := dock.Position.Loc.Y - Shipyard.Entrance.Y;
    K := GetButtonDockAt(dock.Position.Loc.X, dock.Position.Loc.Y);
    If K = -1 then
      Continue;
    Button_Dock[K].Tag := I;
    Button_Dock[K].TexID := gRes.Units[dock.NextShip].GUIIcon;

    Button_Dock[K].Hitable := true;
  end;

  for I := 1 to WARES_IN_OUT_COUNT do
    begin
      Button_Ware[I].Blocked := Shipyard.GetAcceptWareIn(Shipyard.WareInput[I]) > 0;
      Button_Ware[I].Caption := IntToKStr(Shipyard.ResIn[I]);
    end;

  Wares_Out.WarePlan.CopyFrom(Shipyard.WaresOut);

  J := 0;
  for I := 0 to High(Ship_Bevel) do
    If gHands[Shipyard.Owner].Locks.UnitUnlocked(SHIPYARD_ORDER[I + 1], htShipYard) then
    begin
      Ship_Bevel[I].Show;
      Ship_Image[I].Show;
      Ship_Cost[I].Show;
      Ship_Bevel[I].Top := Wares_Out.Bottom + J*60;
      Ship_Image[I].Top := Wares_Out.Bottom + 15 + J*60;
      Ship_Cost[I].Top := Wares_Out.Bottom + J*60;
      Ship_Cost[I].WarePlan.Reset;
      Ship_Cost[I].WarePlan := Shipyard.GetShipCost(SHIPYARD_ORDER[I + 1]);
      for K := 0 to Ship_Cost[I].WarePlan.Count - 1 do
        If Shipyard.CheckWareIn(Ship_Cost[I].WarePlan[K].W) >= Ship_Cost[I].WarePlan[K].C then
          Ship_Cost[I].SetColor(K, icWhite)
        else
          Ship_Cost[I].SetColor(K, icRed);


      Inc(J);
    end else
    begin
      Ship_Bevel[I].Hide;
      Ship_Image[I].Hide;
      Ship_Cost[I].Hide;
    end;

end;

procedure TKMGuiGameShipyard.ButtonClickShift(Sender: TObject; Shift: TShiftState);
var I : integer;
  SY : TKMHouseShipyard;
begin
  I := TKMControl(Sender).Tag;
  SY := TKMHouseShipyard(gMySpectator.Selected);

  gGame.GameInputProcess.CmdHouse(gicHouseShipType, SY, I, IfThen(ssRight in Shift, -1, 1));

end;

procedure TKMGuiGameShipyard.ButtonWheel(Sender : TObject; WheelSteps : Integer; var Handled : Boolean);
var I : Integer;
  SY : TKMHouseShipyard;
begin

  Handled := true;

  I := TKMControl(Sender).Tag;
  SY := TKMHouseShipyard(gMySpectator.Selected);
  gGame.GameInputProcess.CmdHouse(gicHouseShipType, SY, I, EnsureRange(WheelSteps, -1, 1));
end;

procedure TKMGuiGameShipyard.WareClickShift(Sender : TObject; Shift : TShiftState);
begin
  WareBlock(Sender, Shift);
end;

procedure TKMGuiGameShipyard.WareBlock(Sender : TObject; Shift : TShiftState);
var I : Integer;
  SY : TKMHouseShipyard;
begin
  I := TKMControl(Sender).Tag;
  SY := TKMHouseShipyard(gMySpectator.Selected);
  if SY.GetAcceptWareIn(SY.WareInput[I]) > 0 then
    gGame.GameInputProcess.CmdHouse(gicHouseDeliveryToggle, SY, SY.WareInput[I], -1000)
  else
    gGame.GameInputProcess.CmdHouse(gicHouseDeliveryToggle, SY, SY.WareInput[I], 1000);
end;




procedure TKMWareButton.MouseUp(X: Integer; Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  If (X - AbsLeft <= BLOCK_BUTTON_SIZE) and (Y - AbsTop <= BLOCK_BUTTON_SIZE) then
    OnClickShift := OnBlockWare
  else
    OnClickShift := OnShiftClick;

  Inherited;
end;

constructor TKMWareButton.Create(aParent: TKMPanel; aX: Integer; aY: Integer; aWare : TKMWareType; ClickShift: TNotifyEventShift; BlockWare: TNotifyEventShift);
begin
  Inherited Create(aParent, aX, aY, 31, 35, gRes.Wares[aWare].GUIIcon);

  OnBlockWare := BlockWare;
  OnShiftClick := ClickShift;
  Hint := gRes.Wares[aWare].Title;
  TexOffsetX := 5;
end;

procedure TKMWareButton.Paint;
begin
  Inherited;

  TKMRenderUI.WriteBevel(AbsLeft, AbsTop, BLOCK_BUTTON_SIZE, BLOCK_BUTTON_SIZE);
  TKMRenderUI.WritePicture(AbsLeft, AbsTop, BLOCK_BUTTON_SIZE, BLOCK_BUTTON_SIZE, [], rxGuiMain,
                          IfThen(Blocked, 32, 33), Enabled)
end;



end.

