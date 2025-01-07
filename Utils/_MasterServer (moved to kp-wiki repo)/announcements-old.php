<?php
require_once("consts.php");
global $MAIN_VERSION;

$Lang = $_REQUEST["lang"];
$Rev = $_REQUEST["rev"];


if(($Rev == "r3374") || ($Rev == "r3252") || ($Rev == "r3311") || ($Rev == "r3812") || ($Rev == "r3870") || ($Rev == "r3967") || ($Rev == "r3985") || ($Rev == "r4125") || ($Rev == "r4297") || ($Rev == "r5057") || ($Rev == "r5116") || ($Rev == "r5349") || ($Rev == "r5459"))
{
  die('[$0000FF]THE SCRIPTING DEMO IS OUT![]||This release candidate is now redundant. Please download the release from www.kamremake.com||Thanks again for your help testing!');
}

//First see if they are up to date
if($Rev != $MAIN_VERSION)
{
	echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!|~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|";
	switch($Lang)
	{
		case 'tur':
			echo "Eski bir KaM Remake versiyonu kullan�yorsunuz! ".$Rev." versiyonunu kullan�yorsunuz fakat ".$MAIN_VERSION." versiyonu kullan�labilir durumda.||L�tfen www.kamremake.com adresinden g�ncellemeyi indiriniz.";
			break;
		case 'jpn':
			echo "Anata no KAM Remake no ba-jon wa jidaiokure desu yo!�@".$Rev." ga jikkou saremashite iru, shikashi ".$MAIN_VERSION." ba-jon wa yori saikin desu. Kono rinku (www.kamremake.com) kara appude-to ga daunro-do saremashite kudasai.";
			break;
		case 'bel':
			echo "���� ����� KaM Remake ��������! �� �������������� ".$Rev.", � ����� ������� ����� ".$MAIN_VERSION.". ||��� ����� �������� ��������� �� ������: www.kamremake.com";
			break;
		case 'nor':
			echo "Din versjon av KaM Remake er utdatert! Du kj�rer ".$Rev." men den mest nylige versjonen er ".$MAIN_VERSION.".||Vennligst last ned oppdateringen p�: www.kamremake.com";
			break;
		case 'ukr':
			echo "���� ".$Rev." ����� KaM Remake ��������! ���������� ���� ".$MAIN_VERSION." ����������� �� ����: www.kamremake.com";
			break;
		case 'rom':
			echo "Versiunea ta de KaM Remake este expirat�! Acum rulezi ".$Rev." dar cea mai recent� versiune este ".$MAIN_VERSION.".||Te rug�m s� descarci update-ul de la www.kamremake.com";
			break;
		case 'svk':
			echo "Va�a verzia KaM Remake je zastaral�! M�te spusten� verziu ".$Rev.", ale posledn� verzia je ".$MAIN_VERSION.".||Pros�m, stiahnite si aktualiz�ciu na str�nke: www.kamremake.com";
			break;
		case 'est':
			echo "Teie KaM Remake versioon on vana! Teie versioon on ".$Rev." , aga viimane versioon on ".$MAIN_VERSION.".||Palun laadige alla uuendus: www.kamremake.com";
			break;
		case 'bul':
			echo "�������� �� ������ � � ��-����� ������! ��� ���������� ".$Rev." ,�� �� ���������� ���������� ������, ����� � ".$MAIN_VERSION.".||���� ��������� ������� ��: www.kamremake.com";
			break;
		case 'spa':
			echo "�La version del kam Remake est� desactualizada! Est�s ejecutando ".$Rev." pero la versi�n m�s reciente es ".$MAIN_VERSION.".||Por favor bajate la actualizacion en: www.kamremake.com";
			//No puedes jugar en multijugador hasta que no actualices.
			break;
		case 'ita':
			echo "La tua versione di \"KaM Remake\" non � aggiornata! Stai utilizzando la versione ".$Rev.", mentre la pi� recente � ".$MAIN_VERSION.".||Puoi scaricare l'aggiornamento dal sito: www.kamremake.com.";
			//Non potrai giocare online prima di aver aggiornato il programma.
			break;
		case 'ptb':
			echo "Sua vers�o do KaM Remake est� desatualizada! Voc� est� executando ".$Rev." mas a vers�o mais recente � ".$MAIN_VERSION.".|| Por favor, baixe a atualiza��o em: www.kamremake.com";
			//Voc� n�o pode jogar online at� que atualize seu jogo.
			break;
		case 'hun':
			echo "A KaM Remake verzi�d t�l r�gi! Te a ".$Rev." verzi�t futtatod, mik�zben a ".$MAIN_VERSION." verzi� a leg�jabb.||K�rlek t�ltsd le a j�t�k friss�t�s�t a hivatalos oldalon: www.kamremake.com";
			//Nem j�tszhatsz interneten, am�g nem friss�ted a j�t�kodat.
			break;
		case 'rus':
			echo "���� ������ ���� ��������! �� ����������� ������ ".$Rev." ����� ��� ��������� ��������� ������ - ".$MAIN_VERSION.".||���������� �������� �� � �����: www.kamremake.com";
			//�� �� ������ ������ ������ ���� �� �������� ���� ������.
			break;
		case 'cze':
			echo "M�te zastaralou verzi KaM Remake! Pou��v�te verzi ".$Rev.", ale nejnov�j�� verze je ".$MAIN_VERSION.".||Pros�m, st�hn�te si aktualizaci na: www.kamremake.com";
			//Nem��ete hr�t online dokud neaktualizujete.
			break;
		case 'fre':
			echo "La version de KaM Remake que vous utilisez n'est pas mise � jour !|Vous avez la version ".$Rev." alors que la version la plus r�cente est la ".$MAIN_VERSION.".||Veuillez t�l�charger la mise � jour sur www.kamremake.com.";
			//Vous ne pouvez pas jouer en ligne tant que vous n'avez pas mis � jour votre version.
			break;
		case 'pol':
			echo "Twoja wersja KaM Remake jest nieaktualna! U�ywasz ".$Rev." ale najnowsz� jest ".$MAIN_VERSION.".||Prosz� pobra� aktualizacj� ze strony: www.kamremake.com";
			//Nie mo�esz gra� online zanim nie zaktualizujesz swojej wersji gry.
			break;
		case 'dut':
			echo "Uw KaM Remake versie is niet de nieuwste. U draait ".$Rev." maar de meest recente versie is ".$MAIN_VERSION.".||U kunt de nieuwste versie downloaden van: www.kamremake.com";
			//U kunt niet online spelen totdat u de nieuwste versie heeft ge�nstalleerd.
			break;
		case 'swe':
			echo "Du har inte den senaste versionen av KaM Remake! Du k�r ".$Rev.", medan den senaste versionen �r ".$MAIN_VERSION.".||Ladda ner uppdateringen h�r: www.kamremake.com";
			//Du kan inte spela online f�rr�n du har uppdaterat.
			break;
		case 'ger':
			echo "Deine Version des Remakes ist veraltet! Du hast ".$Rev.", die neuste ist ".$MAIN_VERSION.".||Bitte lade das neuste Update von www.kamremake.com runter.";
			//Solange du nicht die aktuelle Version hast, kannst du nicht online spielen.
			break;
		default:
			echo "Your KaM Remake version is out of date! You are running ".$Rev." but the most recent version is ".$MAIN_VERSION.".||Please download the update at: www.kamremake.com";
			//You cannot play online until you have updated.
	}
	echo "||~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!";
}
else
{
    echo 'Did you know we have an official forum? Read about the latest developments to the KaM Remake, discuss your ideas and get responses from developers, see the latest fan-made maps, and take part in community events like tournaments and beta testing. Visit our forum at:|[$F8A070]knightsandmerchants.net/forum[]||Don\'t forget to try out some of the special multiplayer missions which use dynamic scripts to create new game modes and objectives.||Enjoy playing KaM Remake? Please consider [$00EEFF]donating[] to help cover our running costs and support future development. Visit [$F8A070]www.kamremake.com/donations[]';
	//echo 'We have upgraded to a [$009BEE]new server[] so you should notice the server list is refreshing faster! Read more at [$F8A070]www.kamremake.com[]||Don\'t forget to try out some of the special multiplayer missions which use dynamic scripts to create new game modes and objectives.||Enjoy playing KaM Remake? Please consider [$00EEFF]donating[] to help cover our running costs and support future development. Visit [$F8A070]www.kamremake.com/donations[]';
	//echo 'The 2 vs 2 Powah Tournament has begun! Go to [$F8A070]www.kamremake.com[] to find out more!||Can you help translate our website [$F8A070]www.kamremake.com[]? We need volunteers to help translate it into any language. Please contact us through our website.';
	//echo 'Powah Tour (Florek, Mulberry, To) are organising a tournament! Go to [$F8A070]www.kamremake.com[] to find out more!||Can you help translate our website [$F8A070]www.kamremake.com[]? We need volunteers to help translate it into any language. Please contact us through our website.';
	//echo '[$FF6EAF]Welcome to the 4th multiplayer demo[] :)|Please report any issues.';
	//echo "We will hopefully be releasing an update to the KaM Remake soon! (around 29th of April if testing goes well)||Check the news story on our website for more information: www.kamremake.com";
	//echo "The recent '500 Internal Server Errors' seem to be resolved, it was a problem at the hosting provider.|Please let us know if you have problems refreshing the server list or these announcements.||Thanks for your patience :)";
	//echo "We're getting some '500 Internal Server Errors' on our master server which we will try to fix as soon as possible.||If you can't see any servers in the list, please press Refresh Server List to try again.||Updated 19/02/2012.";
	//echo "Happy New Year! :-)";
	//echo "A KaM Remake Christmas tournament is being organised!||Visit the forum to register: tinyurl.com/KAMCOMP";
	//echo "Welcome to the new version!||Have fun, report problems and spread the word :-)";
	//echo "Use our webchat to organise your games and stay in contact: www.kamremake.com/chat||Server admins: Don't forget to update your servers to r2460 if you haven't already.||Have fun :)";
	//echo "TO THE OWNERS OF THE FOLLOWING SERVERS:|KaM srv from Natodia|[PL] Reborn Army KaM Server||You need to update your servers to r2460 ASAP. (download at www.kamremake.com) Due to bugs in the old server versions there are \"ghost\" players on your server which failed to disconnect properly.|If anyone knows the owners of these servers, please ask them to update. Playing on these servers is not recommended as they are more likely to crash your game.";
	//echo 'There is a new Servers page on the website! Check it out at www.kamremake.com/servers||We have also released an update to the dedicated server that fixes crashes on Linux. Please update your servers as soon as possible to the new version r2460. Thanks to everyone who helped test this server fix.';
	//echo 'WE NEED YOUR HELP!|We are having difficulties with the Linux build of the dedicated server. The servers occasionally crash which stops all games running on them. The following servers are running a new unreleased fix (r2446) which we are testing for release:| - [DE] KaM Remake Server| - Linux r2446 Server| - Jecy\'s r2446 Dedicated Server|Please help us by playing in these servers as much as possible until further notice. This will help us assess whether the crashes are fixed. Thanks! :)';
	/*switch($Lang)
	{
		case 'ger':
			echo "Willkommen bei Knights and Merchants Remake Online!||Jeden Samstag um 21Uhr CET finden Wettk�mpfe statt. Seid dabei! Danke f�r Eure Unterst�tzung!";
			break;
		default:
			echo "Welcome to the Knights and Merchants Remake online!||Weekly matches are currently run every Saturday at 9pm Central European Time. Please join us if you can!|Thank you for your support!";
	}*/
}
