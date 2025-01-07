unit KM_GUIMenuLoading;
{$I KaM_Remake.inc}
interface
uses
  Math, SysUtils,
  KM_Controls, KM_ControlsBase, KM_InterfaceDefaults, KM_InterfaceTypes;


type
  TKMMenuLoading = class(TKMMenuPageCommon)
  private
    fOnPageChange: TKMMenuChangeEventText;
  protected
    Panel_Loading: TKMPanel;
    Label_Loading: TKMLabel;
  public
    constructor Create(aParent: TKMPanel; aOnPageChange: TKMMenuChangeEventText);

    procedure AppendText(const aText: UnicodeString);
    procedure Show(const aText: UnicodeString);
  end;


implementation
uses
  KM_ResTexts, KM_RenderUI, KM_ResFonts;


{ TKMGUIMenuLoading }
constructor TKMMenuLoading.Create(aParent: TKMPanel; aOnPageChange: TKMMenuChangeEventText);
begin
  inherited Create(gpLoading);

  fOnPageChange := aOnPageChange;

  Panel_Loading := TKMPanel.Create(aParent,0,0,aParent.Width, aParent.Height);
  Panel_Loading.AnchorsStretch;
    with TKMLabel.Create(Panel_Loading, aParent.Width div 2, aParent.Height div 2 - 20, gResTexts[TX_MENU_LOADING], fntOutline, taCenter) do
      AnchorsCenter;
    Label_Loading := TKMLabel.Create(Panel_Loading, aParent.Width div 2, aParent.Height div 2+10, '...', fntGrey, taCenter);
    Label_Loading.AnchorsCenter;
end;


procedure TKMMenuLoading.AppendText(const aText: UnicodeString);
begin
  Label_Loading.Caption := Label_Loading.Caption + aText + '|';
end;


procedure TKMMenuLoading.Show(const aText: UnicodeString);
begin
  Label_Loading.Caption := aText;
  Panel_Loading.Show;
end;


end.
