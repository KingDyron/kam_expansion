<?php
require_once("serverlib.php");
require_once("consts.php");
require_once("db.php");

if(!CheckVersion($_REQUEST["rev"])) {
	die("Invalid revision");
}

$con = db_connect();

$IP = $con->real_escape_string($_SERVER['REMOTE_ADDR']); //Use the client's IP as it appears to us
$Map = $con->real_escape_string($_REQUEST["map"]);
$CRC = $con->real_escape_string($_REQUEST["mapcrc"]);
$PlayerCount = $con->real_escape_string($_REQUEST["playercount"]);
$Rev = $con->real_escape_string($_REQUEST["rev"]);

if(($Map == "") || ($Rev == "") || ($PlayerCount == "")) {
	$con->close();
	die("Incorrect parameters");
}

db_init($con);
$Now = date("Y-m-d H:i:s");
$query = "INSERT INTO Games (Rev, Timestamp, Map, MapCRC, Players, IP) VALUES ('$Rev', '$Now', '$Map', '$CRC', $PlayerCount, '$IP')";

if(!$con->query($query)) error_log("Error adding game: ".mysqli_error($con));

$con->close();
die('success');
