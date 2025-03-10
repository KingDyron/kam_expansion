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
      procedure ChangeMode(Sender : TObject);
      procedure Button_ToggleMode(Sender : TObject);
    protected
      Button_Type: array[TKMCartographersMode] of TKMButton;
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
  KM_ResTypes, KM_ResTexts,
  KM_HandsCollection;

constructor TKMGuiGameCartographer.Create(aParent: TKMPanel);
const CPL_HINT : array[TKMCartographersPaintLayer] of Word = (1, 2, 3, 4, 5);
      CM_HINT : array[TKMCartographersMode] of Word = (1, 2);
var CPL : TKMCartographersPaintLayer;
    CM : TKMCartographersMode;
begin
  Inherited Create(aParent, 0, 100, aParent.Width, 400);

  for CM := Low(TKMCartographersMode) to High(TKMCartographersMode) do
  begin
    Button_Type[CM] := TKMButton.Create(self, 10 + ord(CM) * 30, 5, 30, 30, 1, rxGui, bsGame);
    Button_Type[CM].Hint := gResTexts[CM_HINT[CM]];
    Button_Type[CM].Tag := ord(CM);
    Button_Type[CM].OnClick := ChangeMode;
  end;

  Panel_Cartographer := TKMPanel.Create(self, 0, 45, Width - 5, Height - 45);

    Minimap := TKMMinimapCartographerView.Create(Panel_Cartographer, 0, 0, Panel_Cartographer.Width, Panel_Cartographer.Width);

    for CPL := Low(TKMCartographersPaintLayer) to High(TKMCartographersPaintLayer) do
    begin
      Button_Toggle[CPL] := TKMButtonFlat.Create(Panel_Cartographer, 17 + ord(CPL) * 30, Minimap.Bottom + 5, 28, 28, ord(Cpl) + 1);
      Button_Toggle[CPL].OnClick := Button_ToggleMode;
      Button_Toggle[CPL].Tag := ord(CPL);
      Button_Toggle[CPL].Hint := gResTexts[CPL_HINT[CPL]];
    end;

  Panel_Spy := TKMPanel.Create(self, 0, 45, Width - 5, Height - 45);
  Panel_Spy.Hide;
end;

procedure TKMGuiGameCartographer.Show(aHouse : TKMHouse; aTop : Integer);
var CPL : TKMCartographersPaintLayer;
    CM : TKMCartographersMode;
    H : TKMHouseCartographers;
begin
  Inherited Show;
  Panel_Cartographer.Hide;
  Panel_Spy.Hide;
  Top := aTop;
  H := TKMHouseCartographers(aHouse);
  for CM := Low(TKMCartographersMode) to High(TKMCartographersMode) do
    Button_Type[CM].ShowImageEnabled := H.Mode = CM;

  If H.Mode = cmSpy then
  begin
    Panel_Spy.Show;

  end else
  begin
    Panel_Cartographer.Show;
    Minimap.SetMinimap(H.Minimap);
    for CPL := Low(TKMCartographersPaintLayer) to High(TKMCartographersPaintLayer) do
      Button_Toggle[CPL].Down := H.Layer[CPL];
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

procedure TKMGuiGameCartographer.ChangeMode(Sender : TObject);
var H : TKMHouseCartographers;
begin
  H := TKMHouseCartographers(gMySpectator.Selected);
  H.Mode := TKMCartographersMode(TKMControl(Sender).Tag);
  //Show(H, self.Top);
end;

end.
