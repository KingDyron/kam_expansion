unit KM_GUICommonDevelopment;
{$I KaM_Remake.inc}
interface
uses
  Classes,
  KM_Controls, KM_ControlsBase, KM_ControlsScroll,
  KM_CommonTypes, KM_Defaults, KM_ResTypes, KM_ResDevelopment;

type
  TKMDevButton = record
    Button_Tree : TKMButtonFlat;
    Next : array of TKMDevButton;
  end;
  TKMGUICommonDevelopment = class(TKMPanel)
  private
    fLastPage : TKMDevelopmentTreeType;
    procedure SwitchPage(Sender : TObject); virtual;
  protected
    Button_SwitchTree : array[TKMDevelopmentTreeType] of TKMButton;
      Tree : array[TKMDevelopmentTreeType] of record
        fCount : Word;
        Panel : TKMScrollPanel;
          Button_Tree : TKMDevButton;
      end;

  public
    constructor Create(aParent : TKMPanel; aLeft, aTop, aWidth, aHeight : Integer);

    procedure Paint; override;
  end;

implementation
uses
  SysUtils, Math, KM_UtilsExt,
  KM_Points,
  KM_ResSound, KM_InterfaceGame,
  KM_ResTexts, KM_RenderUI, KM_ResFonts, KM_Resource;

Const DISTANCE_BETWEEN_ROWS = 50;
constructor TKMGUICommonDevelopment.Create(aParent: TKMPanel; aLeft: Integer; aTop: Integer; aWidth: Integer; aHeight: Integer);
const
  TREE_TYPE_ICON : array[TKMDevelopmentTreeType] of Word = (39, 322);
var dtt : TKMDevelopmentTreeType;

  procedure CreateNext(aType : TKMDevelopmentTreeType; var aToButton : TKMDevButton;  aDevelopment : TKMDevelopment; aTop : Byte);
  var I : integer;
  begin
    with Tree[aType] do
    begin
      Inc(fCount);

      aToButton.Button_Tree := TKMButtonFlat.Create(Panel, 3 + aDevelopment.X * 34, aTop * DISTANCE_BETWEEN_ROWS, 31, 31, aDevelopment.GuiIcon);
      aToButton.Button_Tree.Hint := gRes.Development.GetText(aDevelopment.HintID);
      aToButton.Button_Tree.BackAlpha := 1;
    end;
    SetLength(aToButton.Next, length(aDevelopment.Next));
    for I := 0 to High(aDevelopment.Next) do
      CreateNext(aType, aToButton.Next[I], aDevelopment.Next[I], aTop + 1);
    {If Tree[aType].fCount > Length(Tree[aType].Button_Tree) then
      SetLength(Tree[aType].Button_Tree, Tree[aType].fCount + 32);}

    {with Tree[aType] do
    begin
      Button_Tree[fCount - 1] := TKMButtonFlat.Create(Panel, aDevelopment.X * 37, aTop * DISTANCE_BETWEEN_ROWS, 33, 33, aDevelopment.GuiIcon);
      Button_Tree[fCount - 1].Hint := gResTexts[aDevelopment.TextID];
    end;}


  end;

begin
  Inherited Create(aParent, aLeft, aTop, aWidth, aHeight);
  AnchorsStretch;
  fLastPage := dttEconomy;
  for dtt := Low(TKMDevelopmentTreeType) to High(TKMDevelopmentTreeType) do
  begin
    Button_SwitchTree[dtt] := TKMButton.Create(self, byte(dtt) * 37, 5, 33, 33, TREE_TYPE_ICON[dtt], rxGui, bsPaper);
    Button_SwitchTree[dtt].Tag := byte(dtt);
    Button_SwitchTree[dtt].OnClick := SwitchPage;

    Tree[dtt].Panel := TKMScrollPanel.Create(self, 3, 42, Width - 6, Height - 42 - 5, [saVertical], bsMenu, ssGame);
    Tree[dtt].Panel.AnchorsStretch;
    Tree[dtt].Panel.ChildPanel.AnchorsStretch;
    CreateNext(dtt, Tree[dtt].Button_Tree, gRes.Development[dtt].FirstItem, 0);
    Tree[dtt].Panel.Visible := fLastPage = dtt;
  end;
end;

procedure TKMGUICommonDevelopment.SwitchPage(Sender: TObject);
var dtt : TKMDevelopmentTreeType;
begin
  fLastPage := TKMDevelopmentTreeType(TKMControl(Sender).Tag);
  for dtt := Low(TKMDevelopmentTreeType) to High(TKMDevelopmentTreeType) do
  begin
    Button_SwitchTree[dtt].ShowImageEnabled := fLastPage = dtt;
    Tree[dtt].Panel.Visible := fLastPage = dtt;
  end;

end;

procedure TKMGUICommonDevelopment.Paint;
  procedure MakeLine(aFrom : TKMPoint; aTo : TKMDevButton);
  var I : Integer;
    cent : TKMPoint;
  begin
    cent := aTo.Button_Tree.AbsCenter;
    TKMRenderUI.WriteLine(aFrom.X, aFrom.Y, aFrom.X, (DISTANCE_BETWEEN_ROWS div 2) + aFrom.Y, $FFFFFFFF);//  \/
    TKMRenderUI.WriteLine(aFrom.X, (DISTANCE_BETWEEN_ROWS div 2) + aFrom.Y,
                          cent.X, cent.Y - (DISTANCE_BETWEEN_ROWS div 2), $FFFFFFFF);//  <>

    TKMRenderUI.WriteLine(cent.X, cent.Y, cent.X, cent.Y - (DISTANCE_BETWEEN_ROWS div 2), $FFFFFFFF);//  /\
    for I := 0 to high(aTo.Next) do
      MakeLine(cent, aTo.Next[I]);
  end;
var I : integer;
begin
  If Visible then
  begin
    TKMRenderUI.WriteBevel(AbsLeft, AbsTop, Width, Height, 1, 0.7);
    TKMRenderUI.SetupClipY(AbsTop + 42, AbsTop + Height - 5);
    for I := 0 to high(Tree[fLastPage].Button_Tree.Next) do
    begin
      MakeLine(Tree[fLastPage].Button_Tree.Button_Tree.AbsCenter,
                Tree[fLastPage].Button_Tree.Next[I]);
    end;
    TKMRenderUI.ReleaseClipY;
  end;

  Inherited;
end;


end.

