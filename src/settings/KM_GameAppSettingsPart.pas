unit KM_GameAppSettingsPart;
{$I KaM_Remake.inc}
interface
uses
  KM_IoXML;


type
  // Part of GameApp settings, which are stored in the XML
  TKMGameAppSettingsPart = class abstract
  private
    fRoot: TKMXmlNode;
  public
    procedure SaveToXML; virtual; abstract;
    procedure LoadFromXML; virtual; abstract;

    property Root: TKMXmlNode read fRoot write fRoot;
  end;


implementation


end.

