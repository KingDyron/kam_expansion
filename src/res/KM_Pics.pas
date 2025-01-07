unit KM_Pics;
{$I KaM_Remake.inc}
interface
uses
  KM_ResTypes;


type
  TKMPic = record
    RX: TRXType;
    ID: Word;
    HighlightOnMouseOver: Boolean;
  end;
const
  PIC_CLEAR : TKMPic = (RX: rxGui; ID: 0; HighlightOnMouseOver: false);

  function MakePic(aRX: TRXType; aIndex: Word; aHighlightOnMouseOver: Boolean = False): TKMPic;


implementation


function MakePic(aRX: TRXType; aIndex: Word; aHighlightOnMouseOver: Boolean = False): TKMPic;
begin
  Result.RX := aRX;
  Result.ID := aIndex;
  Result.HighlightOnMouseOver := aHighlightOnMouseOver;
end;


end.
