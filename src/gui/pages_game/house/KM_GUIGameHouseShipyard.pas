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
      procedure ButtonClicked(Sender : TObject);
      procedure ButtonClickShift(Sender : TObject; Shift : TShiftState);
    protected
      Button_Shipyard : TKMButtonFlat;
      Button_Dock : array[0..MAX_DOCKS - 1] of TKMButtonFlat;
    public
      constructor Create(aParent: TKMPanel);
      procedure Show(aHouse : TKMHouse; aTop : Integer); Reintroduce;
  end;

implementation
uses
  KM_CommonTypes,
  KM_Game, KM_GameInputProcess,
  KM_HandsCollection,
  KM_RenderUI, KM_Cursor,
  KM_Resource, KM_ResTexts, KM_ResUnits, KM_ResFonts,
  KM_UnitWarrior;


constructor TKMGuiGameShipyard.Create(aParent: TKMPanel);
var I : integer;
begin
  Inherited Create(aParent, 0, 100, aParent.Width - 8, 300);

  Button_Shipyard := TKMButtonFlat.Create(self, 0, 100, 60, 60, 872);
  Button_Shipyard.SetPosCenterW;

  for I := 0 to MAX_DOCKS - 1 do
  begin
    Button_Dock[I] := TKMButtonFlat.Create(self, 0, 0, 30, 30, gRes.Units[utBattleShip].GUIIcon);
  end;


end;

procedure TKMGuiGameShipyard.Show(aHouse : TKMHouse; aTop : Integer);
begin
  Inherited Show;
  Top := aTop;
  Refresh(TKMHouseShipyard(aHouse));
end;

procedure TKMGuiGameShipyard.Refresh(Shipyard: TKMHouseShipyard);
var I, J : integer;
  dock : TKMShipyardDockWithLoc;
begin
  J := Shipyard.DocksCount;
  for I := 0 to MAX_DOCKS - 1 do
  If I < J then
  begin
    dock := Shipyard.GetDock(I);
    dock.Position.Loc.X := Shipyard.Entrance.X - dock.Position.Loc.X;
    dock.Position.Loc.Y := Shipyard.Entrance.Y - dock.Position.Loc.Y;

    Button_Dock[I].Left := Button_Shipyard.Left - dock.Position.Loc.X * 30;
    Button_Dock[I].Top := Button_Shipyard.Top + 30 - dock.Position.Loc.Y * 30;

    Button_Dock[I].Show;
  end else
    Button_Dock[I].Hide;
end;

procedure TKMGuiGameShipyard.ButtonClicked(Sender: TObject);
begin

end;

procedure TKMGuiGameShipyard.ButtonClickShift(Sender: TObject; Shift: TShiftState);
begin

end;

end.

