unit KM_GUIGameHouseShipyard;
{$I KaM_Remake.inc}
interface
uses
  StrUtils, SysUtils, Math, Classes,
  KM_Defaults,
  KM_ResTypes,
  KM_Controls, KM_ControlsBase, KM_ControlsSwitch,
  KM_Houses, KM_HouseShipyard;

const MAX_DOCKS = 16;
type
  TKMGuiGameShipyard = class(TKMPanel)
    private
      procedure Refresh(Shipyard : TKMHouseShipyard);
      procedure ButtonClickShift(Sender : TObject; Shift : TShiftState);
      procedure ButtonWheel(Sender : TObject; WheelSteps : Integer; var Handled : Boolean);
    protected
      Button_Shipyard : TKMButtonFlat;
      Button_Dock : array[0..MAX_DOCKS - 1] of TKMButtonFlat;
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
  KM_UnitWarrior;

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
begin
  Inherited Create(aParent, 0, 100, aParent.Width - 8, 300);

  Button_Shipyard := TKMButtonFlat.Create(self, 0, 70, 60, 60, 872);
  Button_Shipyard.SetPosCenterW;

  for I := 0 to MAX_DOCKS - 1 do
  begin
    Button_Dock[I] := TKMButtonFlat.Create(self,  Button_Shipyard.Left + DOCKS_POSITIONS[I].X * 30,
                                                  Button_Shipyard.Top + 30 + DOCKS_POSITIONS[I].Y * 30, 30, 30, 0);
    Button_Dock[I].OnClickShift := ButtonClickShift;
    Button_Dock[I].OnMouseWheel := ButtonWheel;
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

end.

