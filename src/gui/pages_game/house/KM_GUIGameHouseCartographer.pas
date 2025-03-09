unit KM_GUIGameHouseCartographer;
{$I KaM_Remake.inc}
interface
uses
  StrUtils, SysUtils, Math, Classes,
  KM_Controls, KM_ControlsBase, KM_ControlsMinimapView,
  KM_MinimapCartographer,
  KM_Houses, KM_HouseCartographers;

type
  TKMGuiGameCartographer = class(TKMPanel)
    private
      procedure Button_ToggleMode(Sender : TObject);
    protected
      Button_Type: array[TKMCartographesMode] of TKMButton;
      Panel_Cartographer : TKMPanel;
        Minimap : TKMMinimapCartographerView;
        Button_Toggle : array[TKMCartographersPaintLayer] of TKMButtonFlat;
      Panel_Spy : TKMPanel;
    public
      constructor Create(aParent: TKMPanel);
      procedure Show(aHouse : TKMHouse; aTop : Integer); Reintroduce;

      procedure UpdateState(aGlobalTickCount : Cardinal); override;
  end;
implementation
uses
  KM_Game,
  KM_RenderUI,
  KM_ResTypes,
  KM_HandsCollection;
constructor TKMGuiGameCartographer.Create(aParent: TKMPanel);
var CPL : TKMCartographersPaintLayer;
    CM : TKMCartographesMode;
begin
  Inherited Create(aParent, 0, 100, aParent.Width, 400);

  for CM := Low(TKMCartographesMode) to High(TKMCartographesMode) do
  begin
    Button_Type[CM] := TKMButton.Create(self, 10 + ord(CM) * 30, 5, 30, 30, 1, rxGui, bsGame);
  end;

  Panel_Cartographer := TKMPanel.Create(self, 0, 45, Width - 5, Height - 45);

    Minimap := TKMMinimapCartographerView.Create(Panel_Cartographer, 0, 0, Panel_Cartographer.Width, Panel_Cartographer.Width);

    for CPL := Low(TKMCartographersPaintLayer) to High(TKMCartographersPaintLayer) do
    begin
      Button_Toggle[CPL] := TKMButtonFlat.Create(Panel_Cartographer, 17 + ord(CPL) * 30, Minimap.Bottom + 5, 28, 28, ord(Cpl) + 1);
      Button_Toggle[CPL].OnClick := Button_ToggleMode;
      Button_Toggle[CPL].Tag := ord(CPL);
    end;

  Panel_Spy := TKMPanel.Create(self, 0, 45, Width - 5, Height - 45);
  Panel_Spy.Hide;
end;

procedure TKMGuiGameCartographer.Show(aHouse : TKMHouse; aTop : Integer);
var H : TKMHouseCartographers;
begin
  Inherited Show;
  Panel_Cartographer.Hide;
  Panel_Spy.Hide;
  Top := aTop;
  H := TKMHouseCartographers(aHouse);
  If H.Mode = cmSpy then
  begin
    Panel_Spy.Show;
  end else
  begin
    Panel_Cartographer.Show;
    Minimap.SetMinimap(H.Minimap);
  end;
end;

procedure TKMGuiGameCartographer.UpdateState(aGlobalTickCount: Cardinal);
begin
  Inherited;

  If aGlobalTickCount mod 50 = 0 then//update minimap every 5 seconds
    If Minimap.Visible then
      Minimap.UpdateMinimap;
end;

procedure TKMGuiGameCartographer.Button_ToggleMode(Sender: TObject);
var H : TKMHouseCartographers;
begin
  H := TKMHouseCartographers(gMySpectator.Selected);
  H.ToggleLayer(TKMButtonFlat(Sender).Tag)
end;

end.
