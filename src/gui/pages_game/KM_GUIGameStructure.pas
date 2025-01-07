unit KM_GUIGameStructure;
{$I KaM_Remake.inc}
interface
uses
  StrUtils, SysUtils, Classes,
  KM_CommonClasses,
  KM_Controls, KM_ControlsBase, KM_ControlsWaresRow, KM_ControlsProgressBar,
  KM_Defaults,
  KM_InterfaceGame,
  KM_Structure;


type
  TKMGUIGameStructure = class
  private
    fStructure : TKMStructure;
  protected
    Panel_Structure : TKMPanel;
      Label_StructureName : TKMLabel;
      WareCost_Cost, WareCost_Delivered : TKMCostsRowMulti;
      Bar_Progress : TKMPercentBar;
  public
    constructor Create(aParent: TKMPanel);
    procedure Show(aStructure : TKMStructure);
    procedure Hide;
    function Visible: Boolean;
    procedure UpdateState;
  end;

implementation
uses
  KM_CommonTypes,
  KM_GameSettings,
  KM_RenderUI, KM_Cursor, KM_HandsCollection, KM_ResTexts, KM_Resource, KM_ResFonts,
  KM_Utils, Math, KM_HandTypes,
  KM_ResTypes, KM_ResMapElements,
  KM_UtilsExt;


{ TKMGUIGameBuild }
constructor TKMGUIGameStructure.Create(aParent: TKMPanel);
begin
  inherited Create;

  Panel_Structure := TKMPanel.Create(aParent, 0, 44, aParent.Width, aParent.Height);
  Label_StructureName := TKMLabel.Create(Panel_Structure, 0, 0, Panel_Structure.Width - 25, 20, gResTexts[2039], fntOutline, taCenter);

  Bar_Progress := TKMPercentBar.Create(Panel_Structure, 10, 20, aParent.Width - 20 - 25, 25, fntMetal);
  Bar_Progress.Position := 0;
  Bar_Progress.Caption := gResTexts[2157];

  WareCost_Cost := TKMCostsRowMulti.Create(Panel_Structure, 10, 75, aParent.Width - 20 - 25, 21);
  WareCost_Cost.WarePlan.Clear;
  WareCost_Cost.Caption := gResTexts[2037];

  WareCost_Delivered := TKMCostsRowMulti.Create(Panel_Structure, 10, 115, aParent.Width - 20 - 25, 21);
  WareCost_Delivered.WarePlan.Clear;
  WareCost_Delivered.Caption := gResTexts[2038];


end;


procedure TKMGUIGameStructure.Show(aStructure: TKMStructure);
begin
  fStructure := aStructure;
  Panel_Structure.Show;
  UpdateState;
end;


procedure TKMGUIGameStructure.Hide;
begin
  Panel_Structure.Hide;
end;


function TKMGUIGameStructure.Visible: Boolean;
begin
  Result := Panel_Structure.Visible;
end;

procedure TKMGUIGameStructure.UpdateState;
begin
  if (fStructure = nil) or fStructure.IsDestroyed or fStructure.IsComplete then
  begin
    Hide;
    Exit;
  end;
  WareCost_Cost.WarePlan := fStructure.Cost;
  WareCost_Delivered.WarePlan := fStructure.Delivered;
  Bar_Progress.Position := fStructure.Progress;
  Label_StructureName.Caption := gResTexts[fStructure.Spec.TextID];

end;

end.
