-- MySQL dump 10.13  Distrib 5.6.30, for DragonFly (x86_64)
--
-- Host: localhost    Database: adabase_examples
-- ------------------------------------------------------
-- Server version	5.6.30

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Current Database: `adabase_examples`
--

CREATE DATABASE /*!32312 IF NOT EXISTS*/ `adabase_examples` /*!40100 DEFAULT CHARACTER SET latin1 */;

USE `adabase_examples`;

--
-- Table structure for table `all_types`
--

DROP TABLE IF EXISTS `all_types`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `all_types` (
  `id_nbyte3` mediumint(8) unsigned NOT NULL AUTO_INCREMENT,
  `nbyte0` tinyint(1) DEFAULT NULL,
  `nbyte1` tinyint(3) unsigned DEFAULT NULL,
  `nbyte2` smallint(5) unsigned DEFAULT NULL,
  `nbyte4` int(10) unsigned DEFAULT NULL,
  `nbyte8` bigint(20) unsigned DEFAULT NULL,
  `byte1` tinyint(4) DEFAULT NULL,
  `byte2` smallint(6) DEFAULT NULL,
  `byte3` mediumint(9) DEFAULT NULL,
  `byte4` int(11) DEFAULT NULL,
  `byte5` bigint(20) DEFAULT NULL,
  `real9` float DEFAULT NULL,
  `real18` double DEFAULT NULL,
  `exact_decimal` decimal(5,2) DEFAULT NULL,
  `bit_type` bit(12) DEFAULT NULL,
  `my_date` date DEFAULT NULL,
  `my_datetime` datetime DEFAULT NULL,
  `my_timestamp` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  `my_time` time DEFAULT NULL,
  `my_year` year(4) DEFAULT NULL,
  `fixed_string` char(16) DEFAULT NULL,
  `variable_string` varchar(50) DEFAULT NULL,
  `my_tinytext` tinytext,
  `my_text` text,
  `my_mediumtext` mediumtext,
  `my_longtext` longtext,
  `my_binary` binary(4) DEFAULT NULL,
  `my_varbinary` varbinary(6) DEFAULT NULL,
  `my_tinyblob` tinyblob,
  `my_mediumblob` mediumblob,
  `my_blob` blob,
  `my_longblob` longblob,
  `enumtype` enum('red','green','blue','pink') DEFAULT NULL,
  `settype` set('red','green','purple','blue','black','white','yellow') DEFAULT NULL,
  PRIMARY KEY (`id_nbyte3`)
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `all_types`
--

LOCK TABLES `all_types` WRITE;
/*!40000 ALTER TABLE `all_types` DISABLE KEYS */;
INSERT INTO `all_types` VALUES (1,1,129,33000,2200000000,6600000000,-120,-30000,8000000,-2000000000,-7700000000,7.984,53.12342343423,123.45,'“','1993-01-15','1972-08-01 08:00:05','2016-05-03 20:17:51','04:30:02',1988,'ABCD1234EFGH5678','The quick brown fox jumped over the lazy dogs','You\'re going to need a bigger boat.','Take your stinkin\' paws off me, you damn dirty ape!','Toto, I\'ve a feeling we\'re not in Kansas anymore.','I ate his liver with some fava beans and a nice chianti.','a$&%','dog','abcdefg','1234(*&^#==','[```;^%^&123@#12','jason.bourne','red','blue,black,white,yellow');
/*!40000 ALTER TABLE `all_types` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `fruits`
--

DROP TABLE IF EXISTS `fruits`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `fruits` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `fruit` varchar(45) DEFAULT NULL,
  `color` varchar(45) DEFAULT NULL,
  `calories` smallint(5) unsigned NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=30 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `fruits`
--

LOCK TABLES `fruits` WRITE;
/*!40000 ALTER TABLE `fruits` DISABLE KEYS */;
INSERT INTO `fruits` VALUES (1,'apple','red',95),(2,'apricot','orange',30),(3,'avocado','green',150),(4,'banana','yellow',107),(5,'blackberry','black',1),(6,'cherry','cherry',3),(7,'clementine','orange',24),(8,'fig','purple',10),(9,'grape','purple',3),(10,'grapefruit','yellow',100),(11,'kiwi','green',34),(12,'lemon','yellow',20),(13,'mango','orange',40),(14,'honeydew melon','yellow',36),(15,'cantaloupe','orange',25),(16,'nectarine','orange',25),(17,'olive','black',7),(18,'orange','orange',65),(19,'peach','peach',35),(20,'pear','yellow',45),(21,'pineapple','yellow',50),(22,'plum','purple',25),(23,'prunes','purple',9),(24,'raisin','purple',1),(25,'raspberry','red',1),(26,'strawberry','red',3),(27,'tangerine','orange',26),(28,'tomato','red',9),(29,'cherry tomato','red',2);
/*!40000 ALTER TABLE `fruits` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `funny_names`
--

DROP TABLE IF EXISTS `funny_names`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `funny_names` (
  `id` int(10) unsigned NOT NULL,
  `surname` varchar(45) DEFAULT NULL,
  `first_name` varchar(45) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `funny_names`
--

LOCK TABLES `funny_names` WRITE;
/*!40000 ALTER TABLE `funny_names` DISABLE KEYS */;
INSERT INTO `funny_names` VALUES (1,'BjÃ¶rnstjerna','KÃ¶l'),(2,'LÃ¥ng','SÃ¶derlund'),(3,'NÃšÃ‘EZ','JosÃ© Antonio'),(4,'Ø¨Ù‡Ø®ÛŽØ±Ø¨ÛŽÙ†','ØªØ±Ø­ÙŠØ¨');
/*!40000 ALTER TABLE `funny_names` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `nhl_schedule`
--

DROP TABLE IF EXISTS `nhl_schedule`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `nhl_schedule` (
  `yyyswww` mediumint(8) unsigned NOT NULL DEFAULT '0',
  `away_team` tinyint(3) unsigned NOT NULL DEFAULT '0',
  `home_team` tinyint(3) unsigned NOT NULL DEFAULT '0',
  `gametime` int(10) unsigned NOT NULL DEFAULT '0',
  `home_score` tinyint(3) unsigned NOT NULL DEFAULT '0',
  `away_score` tinyint(3) unsigned NOT NULL DEFAULT '0',
  `event_code` smallint(5) unsigned NOT NULL DEFAULT '0',
  `status` enum('unplayed','complete') NOT NULL DEFAULT 'complete',
  `meeting` tinyint(3) unsigned NOT NULL DEFAULT '0',
  `mfc_index` mediumint(8) unsigned NOT NULL,
  PRIMARY KEY (`event_code`),
  KEY `yyyswww~gametime` (`yyyswww`,`gametime`),
  KEY `mfc_index` (`mfc_index`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `nhl_schedule`
--

LOCK TABLES `nhl_schedule` WRITE;
/*!40000 ALTER TABLE `nhl_schedule` DISABLE KEYS */;
INSERT INTO `nhl_schedule` VALUES (1085003,17,27,1223670600,4,5,9894,'complete',1,200802),(1085001,3,14,1223121600,1,2,9902,'complete',1,200801),(1085001,5,9,1223131500,3,4,9908,'complete',1,200801),(1085001,14,3,1223208000,2,1,9915,'complete',2,200801),(1085001,9,5,1223217000,1,3,9928,'complete',2,200801),(1085002,10,18,1223578800,2,3,9952,'complete',1,200802),(1085002,6,22,1223589600,4,5,9963,'complete',1,200802),(1085002,21,25,1223591400,6,0,9968,'complete',1,200802),(1085002,26,30,1223591400,4,1,9974,'complete',1,200802),(1085003,2,1,1223665200,2,1,10007,'complete',1,200802),(1085009,23,12,1225551600,1,3,10008,'complete',1,200805),(1085009,27,6,1225566000,5,1,10015,'complete',1,200805),(1085003,16,3,1223665200,4,2,10018,'complete',1,200802),(1085009,15,7,1225566000,5,0,10020,'complete',1,200805),(1085003,13,12,1223665200,6,4,10021,'complete',1,200802),(1085003,8,7,1223667000,2,1,10024,'complete',1,200802),(1085003,15,11,1223667000,7,4,10026,'complete',1,200802),(1085009,3,10,1225566000,5,2,10030,'complete',2,200805),(1085003,19,20,1223670600,5,2,10032,'complete',1,200802),(1085009,11,1,1225566000,6,1,10037,'complete',2,200805),(1085009,8,2,1225566000,4,5,10042,'complete',1,200805),(1085003,8,10,1223751600,1,6,10047,'complete',1,200802),(1085009,16,17,1225566000,3,4,10048,'complete',1,200805),(1085003,18,9,1223751600,2,3,10052,'complete',1,200802),(1085003,20,2,1223751600,5,2,10056,'complete',1,200802),(1085009,9,14,1225567800,3,2,10059,'complete',1,200805),(1085003,3,4,1223751600,3,4,10060,'complete',1,200802),(1085003,16,15,1223751600,4,2,10064,'complete',1,200802),(1085003,11,13,1223751600,3,2,10070,'complete',1,200802),(1085009,13,19,1225569600,3,2,10071,'complete',1,200805),(1085003,1,5,1223753400,1,2,10073,'complete',1,200802),(1085003,12,14,1223753400,3,4,10076,'complete',1,200802),(1085009,5,20,1225571400,3,6,10079,'complete',1,200805),(1085003,27,19,1223755200,3,1,10084,'complete',1,200802),(1085009,24,29,1225576800,2,3,10086,'complete',1,200805),(1085003,6,24,1223757000,4,3,10090,'complete',1,200802),(1085009,21,28,1225576800,2,3,10094,'complete',1,200805),(1085003,25,21,1223762400,4,5,10097,'complete',2,200802),(1085009,23,4,1225630800,4,5,10100,'complete',1,200805),(1085003,17,29,1223762400,3,1,10101,'complete',1,200802),(1085003,28,30,1223762400,3,1,10105,'complete',1,200802),(1085009,10,12,1225645200,6,4,10109,'complete',1,200805),(1085009,13,11,1225645200,5,3,10115,'complete',2,200805),(1085009,30,22,1225656000,3,5,10123,'complete',1,200805),(1085003,22,23,1223841600,3,2,10125,'complete',1,200802),(1085009,21,26,1225656000,3,2,10126,'complete',1,200805),(1085009,18,25,1225663200,2,3,10131,'complete',2,200805),(1085003,29,26,1223841600,2,4,10134,'complete',1,200802),(1085003,30,28,1223845200,0,1,10137,'complete',2,200802),(1085010,7,1,1225738800,0,2,10142,'complete',1,200806),(1085010,17,2,1225738800,4,3,10148,'complete',1,200806),(1085004,20,10,1223902800,4,5,10150,'complete',1,200803),(1085004,7,2,1223906400,1,7,10155,'complete',1,200803),(1085010,22,16,1225744200,6,2,10156,'complete',1,200806),(1085004,1,3,1223924400,4,1,10161,'complete',1,200803),(1085004,8,4,1223924400,3,5,10164,'complete',1,200803),(1085010,2,3,1225825200,1,2,10165,'complete',2,200806),(1085004,25,15,1223924400,5,1,10170,'complete',1,200803),(1085010,12,10,1225827000,4,5,10171,'complete',2,200806),(1085004,18,12,1223924400,1,3,10173,'complete',1,200803),(1085004,19,16,1223929800,2,3,10178,'complete',1,200803),(1085010,15,9,1225827000,2,1,10179,'complete',1,200806),(1085010,29,21,1225836000,2,4,10185,'complete',2,200806),(1085010,19,25,1225836000,4,0,10188,'complete',1,200806),(1085010,26,28,1225837800,0,1,10194,'complete',2,200806),(1085010,24,30,1225837800,3,1,10197,'complete',1,200806),(1085004,4,5,1224012600,3,2,10199,'complete',1,200803),(1085010,14,1,1225911600,4,3,10203,'complete',1,200806),(1085004,24,11,1224010800,2,4,10206,'complete',1,200803),(1085004,22,21,1224019800,5,4,10209,'complete',1,200803),(1085010,23,17,1225913400,5,4,10210,'complete',1,200806),(1085004,26,28,1224023400,6,3,10215,'complete',1,200803),(1085010,20,26,1225922400,5,2,10217,'complete',1,200806),(1085004,17,30,1224023400,5,2,10221,'complete',1,200803),(1085010,10,6,1225998000,5,2,10224,'complete',2,200806),(1085010,14,3,1225998000,5,2,10225,'complete',3,200806),(1085004,7,3,1224097200,1,3,10227,'complete',1,200803),(1085010,12,15,1225998000,3,2,10230,'complete',1,200806),(1085004,6,8,1224099000,4,3,10232,'complete',1,200803),(1085010,2,11,1225998000,4,3,10238,'complete',1,200806),(1085004,29,16,1224102600,4,1,10240,'complete',1,200803),(1085004,19,27,1224102600,6,4,10245,'complete',2,200803),(1085010,4,9,1225999800,4,1,10249,'complete',1,200806),(1085004,23,26,1224108000,2,3,10253,'complete',1,200803),(1085010,23,5,1225998000,5,4,10256,'complete',1,200806),(1085010,24,22,1226005200,1,3,10261,'complete',1,200806),(1085010,19,21,1226007000,7,6,10266,'complete',2,200806),(1085010,29,25,1226008800,1,0,10271,'complete',1,200806),(1085010,13,28,1226010600,3,2,10280,'complete',1,200806),(1085010,20,30,1226010600,5,4,10284,'complete',1,200806),(1085011,9,12,1226084400,2,1,10293,'complete',1,200806),(1085011,8,17,1226084400,4,3,10297,'complete',1,200806),(1085011,11,7,1226086200,4,5,10304,'complete',2,200806),(1085011,27,26,1226095200,2,5,10308,'complete',1,200806),(1085011,7,6,1226170800,3,1,10322,'complete',2,200806),(1085004,1,11,1224183600,0,1,10326,'complete',1,200803),(1085011,8,10,1226170800,6,3,10327,'complete',2,200806),(1085004,15,5,1224185400,3,4,10333,'complete',1,200803),(1085011,5,2,1226170800,3,4,10334,'complete',1,200806),(1085004,2,14,1224185400,3,4,10337,'complete',1,200803),(1085004,24,13,1224185400,2,6,10341,'complete',1,200803),(1085011,14,4,1226170800,1,2,10342,'complete',1,200806),(1085004,25,18,1224185400,3,4,10348,'complete',1,200803),(1085011,3,15,1226170800,3,1,10351,'complete',1,200806),(1085004,27,20,1224189000,6,1,10353,'complete',1,200803),(1085004,4,22,1224190800,5,2,10355,'complete',1,200803),(1085011,1,18,1226170800,3,1,10360,'complete',1,200806),(1085011,21,17,1226170800,3,1,10364,'complete',1,200806),(1085005,29,9,1224270000,6,3,10370,'complete',1,200803),(1085011,19,22,1226178000,1,0,10374,'complete',1,200806),(1085005,10,3,1224270000,1,0,10376,'complete',1,200803),(1085005,19,17,1224270000,5,3,10380,'complete',1,200803),(1085011,13,29,1226178000,4,1,10382,'complete',1,200806),(1085005,25,7,1224271800,5,2,10385,'complete',1,200803),(1085005,23,21,1224277200,3,4,10390,'complete',1,200803),(1085005,30,26,1224280800,4,0,10393,'complete',2,200803),(1085011,24,25,1226181600,2,0,10396,'complete',1,200806),(1085005,12,28,1224282600,4,3,10398,'complete',1,200803),(1085011,27,30,1226181600,2,1,10401,'complete',1,200806),(1085011,20,28,1226183400,5,3,10410,'complete',2,200806),(1085005,22,27,1224338400,4,5,10417,'complete',1,200803),(1085011,11,12,1226242800,2,5,10418,'complete',1,200806),(1085005,29,8,1224356400,4,1,10424,'complete',1,200803),(1085011,23,1,1226259000,1,2,10425,'complete',1,200806),(1085005,6,9,1224356400,2,4,10427,'complete',1,200803),(1085005,10,5,1224356400,4,1,10430,'complete',1,200803),(1085005,1,15,1224356400,3,4,10435,'complete',1,200803),(1085005,7,11,1224356400,3,2,10438,'complete',1,200803),(1085011,21,16,1226257200,6,1,10442,'complete',1,200806),(1085005,2,13,1224356400,2,0,10443,'complete',1,200803),(1085005,3,18,1224356400,5,4,10448,'complete',1,200803),(1085011,30,29,1226260800,4,2,10451,'complete',1,200806),(1085005,24,14,1224358200,0,1,10457,'complete',1,200803),(1085011,13,26,1226260800,1,3,10459,'complete',1,200806),(1085005,17,19,1224360000,6,3,10466,'complete',2,200803),(1085012,23,3,1226343600,2,3,10467,'complete',1,200807),(1085012,14,15,1226343600,4,2,10471,'complete',1,200807),(1085005,16,20,1224361800,4,3,10474,'complete',1,200803),(1085005,21,23,1224367200,3,2,10478,'complete',2,200803),(1085005,4,30,1224369000,5,4,10481,'complete',1,200803),(1085012,4,2,1226412000,1,3,10482,'complete',2,200807),(1085012,5,18,1226430000,6,7,10486,'complete',1,200807),(1085012,9,8,1226430000,4,0,10494,'complete',1,200807),(1085012,10,21,1226439000,4,3,10500,'complete',1,200807),(1085005,25,16,1224442800,4,2,10502,'complete',1,200803),(1085012,27,28,1226442600,3,2,10506,'complete',1,200807),(1085005,12,26,1224446400,1,3,10508,'complete',1,200803),(1085012,19,30,1226442600,3,4,10510,'complete',1,200807),(1085006,5,6,1224529200,1,2,10516,'complete',1,200804),(1085012,20,7,1226516400,4,3,10520,'complete',1,200807),(1085006,27,3,1224529200,1,2,10523,'complete',1,200804),(1085012,3,1,1226516400,2,5,10526,'complete',2,200807),(1085006,13,8,1224531000,3,1,10530,'complete',1,200804),(1085012,15,12,1226516400,1,5,10532,'complete',2,200807),(1085006,22,28,1224541800,3,4,10536,'complete',1,200804),(1085012,29,17,1226516400,2,5,10539,'complete',2,200807),(1085012,14,13,1226518200,4,0,10546,'complete',1,200807),(1085012,6,16,1226521800,1,2,10552,'complete',1,200807),(1085006,6,7,1224615600,3,2,10553,'complete',1,200804),(1085006,26,10,1224615600,2,3,10559,'complete',1,200804),(1085006,25,17,1224615600,4,2,10563,'complete',1,200804),(1085012,22,25,1226527200,1,2,10564,'complete',1,200807),(1085006,11,14,1224617400,3,2,10568,'complete',1,200804),(1085012,8,6,1226602800,6,1,10576,'complete',2,200807),(1085006,15,21,1224624600,2,1,10579,'complete',1,200804),(1085012,2,9,1226604600,1,3,10582,'complete',1,200807),(1085012,4,5,1226604600,5,4,10586,'complete',2,200807),(1085012,18,14,1226604600,3,4,10591,'complete',1,200807),(1085006,13,9,1224702000,1,3,10592,'complete',1,200804),(1085006,27,1,1224702000,5,0,10595,'complete',1,200804),(1085006,30,4,1224702000,6,7,10599,'complete',2,200804),(1085012,29,24,1226606400,4,0,10602,'complete',2,200807),(1085006,18,20,1224707400,3,4,10605,'complete',1,200804),(1085006,23,16,1224709200,3,0,10610,'complete',1,200804),(1085012,28,27,1226608200,2,3,10612,'complete',2,200807),(1085006,10,6,1224788400,2,4,10621,'complete',1,200804),(1085006,27,2,1224788400,3,5,10624,'complete',1,200804),(1085006,12,5,1224790200,4,1,10628,'complete',1,200804),(1085012,10,23,1226610000,2,5,10629,'complete',1,200807),(1085006,21,19,1224792000,3,5,10631,'complete',1,200804),(1085006,7,24,1224792000,3,4,10637,'complete',1,200804),(1085012,21,30,1226615400,6,1,10641,'complete',1,200807),(1085006,23,22,1224797400,4,1,10645,'complete',2,200804),(1085006,15,29,1224799200,2,1,10650,'complete',1,200804),(1085007,26,9,1224874800,3,4,10668,'complete',1,200804),(1085007,4,1,1224874800,3,6,10672,'complete',1,200804),(1085007,3,17,1224874800,1,3,10676,'complete',1,200804),(1085007,30,13,1224876600,4,3,10679,'complete',1,200804),(1085007,11,18,1224876600,5,3,10683,'complete',1,200804),(1085007,28,20,1224880200,0,4,10686,'complete',1,200804),(1085007,11,6,1224961200,5,4,10699,'complete',1,200804),(1085007,9,10,1224961200,3,2,10705,'complete',1,200804),(1085007,26,8,1224961200,4,6,10708,'complete',1,200804),(1085007,12,2,1224961200,3,4,10711,'complete',1,200804),(1085007,5,3,1224961200,3,2,10713,'complete',1,200804),(1085007,1,4,1224950400,3,2,10718,'complete',2,200804),(1085007,30,14,1224963000,0,3,10721,'complete',1,200804),(1085007,28,19,1224964800,5,4,10840,'complete',1,200804),(1085007,17,24,1224964800,2,1,10844,'complete',1,200804),(1085007,15,27,1224964800,5,6,10854,'complete',1,200804),(1085007,18,16,1224966600,5,6,10856,'complete',1,200804),(1085007,13,20,1224966600,4,0,10862,'complete',1,200804),(1085007,7,22,1224968400,2,1,10865,'complete',1,200804),(1085007,23,25,1224972000,6,3,10868,'complete',1,200804),(1085007,21,29,1224972000,1,4,10870,'complete',1,200804),(1085008,9,7,1225134000,2,5,10884,'complete',1,200805),(1085008,3,2,1225134000,2,4,10888,'complete',1,200805),(1085008,26,17,1225134000,2,3,10891,'complete',1,200805),(1085008,16,24,1225137600,3,2,10895,'complete',1,200805),(1085008,6,23,1225141200,0,1,10899,'complete',1,200805),(1085008,18,28,1225146600,3,4,10900,'complete',1,200805),(1085008,19,15,1225220400,4,3,10914,'complete',1,200805),(1085008,4,11,1225220400,0,7,10915,'complete',1,200805),(1085008,14,10,1225222200,2,3,10918,'complete',1,200805),(1085008,12,8,1225222200,3,2,10920,'complete',1,200805),(1085008,22,21,1225229400,3,0,10922,'complete',2,200805),(1085008,6,25,1225231200,0,1,10924,'complete',1,200805),(1085008,5,30,1225233000,2,1,10943,'complete',1,200805),(1085008,10,1,1225308600,5,6,10949,'complete',1,200805),(1085008,24,27,1225312200,4,2,10950,'complete',1,200805),(1085008,18,26,1225317600,5,4,10952,'complete',1,200805),(1085008,14,7,1225393200,2,5,10963,'complete',1,200805),(1085008,11,3,1225393200,3,2,10967,'complete',1,200805),(1085008,2,4,1225393200,3,2,10969,'complete',1,200805),(1085008,9,13,1225395000,1,2,10970,'complete',2,200805),(1085008,23,19,1225396800,3,1,10973,'complete',1,200805),(1085008,8,24,1225396800,1,2,10976,'complete',1,200805),(1085008,12,20,1225398600,0,1,10979,'complete',1,200805),(1085008,17,22,1225400400,2,4,10982,'complete',1,200805),(1085008,6,21,1225400400,3,2,10985,'complete',1,200805),(1085008,5,29,1225404000,4,1,10987,'complete',1,200805),(1085008,25,28,1225405800,0,4,10989,'complete',1,200805),(1085008,18,30,1225405800,4,2,10990,'complete',1,200805),(1085009,27,16,1225485000,5,2,11006,'complete',1,200805),(1085009,25,26,1225490400,6,7,11009,'complete',1,200805);
/*!40000 ALTER TABLE `nhl_schedule` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `nhl_teams`
--

DROP TABLE IF EXISTS `nhl_teams`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `nhl_teams` (
  `team_id` int(10) unsigned NOT NULL,
  `abbreviation` varchar(3) DEFAULT NULL,
  `city` varchar(20) DEFAULT NULL,
  `mascot` varchar(20) DEFAULT NULL,
  PRIMARY KEY (`team_id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `nhl_teams`
--

LOCK TABLES `nhl_teams` WRITE;
/*!40000 ALTER TABLE `nhl_teams` DISABLE KEYS */;
INSERT INTO `nhl_teams` VALUES (0,'-','-','-'),(1,'NJ','New Jersey','Devils'),(2,'NYI','New York','Islanders'),(3,'NYR','New York','Rangers'),(4,'PHI','Philadelphia','Flyers'),(5,'PIT','Pittsburg','Penguins'),(6,'BOS','Boston','Bruins'),(7,'BUF','Buffalo','Sabres'),(8,'MON','Montreal','Canadiens'),(9,'OTT','Ottawa','Senators'),(10,'TOR','Toronto','Maple Leafs'),(11,'ATL','Atlanta','Thrashers'),(12,'CAR','Carolina','Hurricanes'),(13,'FL','Florida','Panthers'),(15,'WAS','Washington','Capitols'),(16,'CHI','Chicago','Blackhawks'),(17,'CLM','Columbus','Blue Jackets'),(18,'DET','Detroit','Red Wings'),(19,'NAS','Nashville','Predators'),(20,'STL','St. Louis','Blues'),(21,'CAL','Calgary','Flames'),(22,'COL','Colorado','Avalanche'),(23,'EDM','Edmunton','Oilers'),(24,'MIN','Minnesota','Wild'),(25,'VAN','Vancouver','Canucks'),(26,'ANA','Anaheim','Ducks'),(27,'DAL','Dallas','Stars'),(28,'LA','Los Angeles','Kings'),(29,'PHO','Phoenix','Coyotes'),(30,'SJ','San Jose','Sharks'),(31,'WIN','Winnepeg','Jets');
/*!40000 ALTER TABLE `nhl_teams` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `spatial_plus`
--

DROP TABLE IF EXISTS `spatial_plus`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `spatial_plus` (
  `id` int(11) NOT NULL,
  `sp_point` point DEFAULT NULL,
  `sp_linestring` linestring DEFAULT NULL,
  `sp_polygon` polygon DEFAULT NULL,
  `sp_multi_point` multipoint DEFAULT NULL,
  `sp_multi_line_string` multilinestring DEFAULT NULL,
  `sp_multi_polygon` multipolygon DEFAULT NULL,
  `sp_geo_collection` geometrycollection DEFAULT NULL,
  `sp_geometry` geometry DEFAULT NULL,
  `sp_outer_ring` polygon DEFAULT NULL,
  `sp_coll2` geometrycollection DEFAULT NULL,
  `sp_geometry2` geometry DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `spatial_plus`
--

LOCK TABLES `spatial_plus` WRITE;
/*!40000 ALTER TABLE `spatial_plus` DISABLE KEYS */;
INSERT INTO `spatial_plus` VALUES (1,'\0\0\0\0\0\0\0\0\0\0\0\0\0@/Ý$•À','\0\0\0\0\0\0\0\0\0\0š™™™™™É¿ffffff-@®GázT5@\0\0\0\0\0\04@\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0','\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0€A@\0\0\0\0\0\0$@\0\0\0\0\0€F@\0\0\0\0\0€F@\0\0\0\0\0\0.@\0\0\0\0\0\0D@\0\0\0\0\0\0$@\0\0\0\0\0\04@\0\0\0\0\0€A@\0\0\0\0\0\0$@\0\0\0\0\0\0\0\0\04@\0\0\0\0\0\0>@\0\0\0\0\0€A@\0\0\0\0\0€A@\0\0\0\0\0\0>@\0\0\0\0\0\04@\0\0\0\0\0\04@\0\0\0\0\0\0>@','\0\0\0\0\0\0\0\0\0\0\0\0\0ffffffæ¿ffffffæ?\0\0\0333333ó?333333ó¿\0\0\0š™™™™™@ö(\\Âõ@\0\0\0{®Gáú#ÀŽX‹O\0\"À','\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0$@\0\0\0\0\0\0$@\0\0\0\0\0\04@\0\0\0\0\0\04@\0\0\0\0\0\0$@\0\0\0\0\0\0D@\0\0\0\0\0\0\0\0\0\0\0\0D@\0\0\0\0\0\0D@\0\0\0\0\0\0>@\0\0\0\0\0\0>@\0\0\0\0\0\0D@\0\0\0\0\0\04@\0\0\0\0\0\0>@\0\0\0\0\0\0$@','\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0D@\0\0\0\0\0\0D@\0\0\0\0\0\04@\0\0\0\0\0€F@\0\0\0\0\0€F@\0\0\0\0\0\0>@\0\0\0\0\0\0D@\0\0\0\0\0\0D@\0\0\0\0\0\0\0\0\0\0\0\0\0\0\04@\0\0\0\0\0€A@\0\0\0\0\0\0$@\0\0\0\0\0\0>@\0\0\0\0\0\0$@\0\0\0\0\0\0$@\0\0\0\0\0\0>@\0\0\0\0\0\0@\0\0\0\0\0€F@\0\0\0\0\0\04@\0\0\0\0\0\04@\0\0\0\0\0€A@\0\0\0\0\0\0\0\0\0>@\0\0\0\0\0\04@\0\0\0\0\0\04@\0\0\0\0\0\0.@\0\0\0\0\0\04@\0\0\0\0\0\09@\0\0\0\0\0\0>@\0\0\0\0\0\04@','\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0@\0\0\0\0\0\0@\0\0\0\0\0\0\0\0\0\0\0\0@\0\0\0\0\0\0@\0\0\0\0\0\0@\0\0\0\0\0\0$@\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0>@\0\0\0\0\0\0$@\0\0\0\0\0\0D@\0\0\0\0\0\0D@\0\0\0\0\0\04@\0\0\0\0\0\0D@\0\0\0\0\0\0$@\0\0\0\0\0\04@\0\0\0\0\0\0>@\0\0\0\0\0\0$@\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0>@\0\0\0\0\0\04@\0\0\0\0\0€F@\0\0\0\0\0\0D@\0\0\0\0\0\0$@\0\0\0\0\0\0D@\0\0\0\0\0\0>@\0\0\0\0\0\04@\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0.@\0\0\0\0\0\0@\0\0\0\0\0\0D@\0\0\0\0\0\0$@\0\0\0\0\0\0$@\0\0\0\0\0\04@\0\0\0\0\0\0@\0\0\0\0\0\0$@\0\0\0\0\0\0.@\0\0\0\0\0\0@\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0$@\0\0\0\0\0\0$@\0\0\0\0\0\04@\0\0\0\0\0\04@\0\0\0\0\0\0$@\0\0\0\0\0\0D@\0\0\0\0\0\0\0\0\0\0\0\0D@\0\0\0\0\0\0D@\0\0\0\0\0\0>@\0\0\0\0\0\0>@\0\0\0\0\0\0D@\0\0\0\0\0\04@\0\0\0\0\0\0>@\0\0\0\0\0\0$@\0\0\0\0\0\0\0\0\0ffffff&@333333E@\0\0\0ÍÌÌÌÌŒF@\0\0\0\0\0\0>@\0\0\0\0\0\0\0\0@4@\0\0\0\0\0\04@\0\0\0fffffæ>@–C‹lç;$@\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0>@\0\0\0\0\0\0$@\0\0\0\0\0\0D@\0\0\0\0\0\0D@\0\0\0\0\0\04@\0\0\0\0\0\0D@\0\0\0\0\0\0$@\0\0\0\0\0\04@\0\0\0\0\0\0>@\0\0\0\0\0\0$@','\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0à?\0\0\0\0\0\0\0@\0\0\0\0\0\0&@š™™™™™@\0\0\0\0\0\0(@333333 @','\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0>@\0\0\0\0\0\0$@\0\0\0\0\0\0D@\0\0\0\0\0\0D@\0\0\0\0\0\04@\0\0\0\0\0\0D@\0\0\0\0\0\0$@\0\0\0\0\0\04@\0\0\0\0\0\0>@\0\0\0\0\0\0$@','\0\0\0\0\0\0\0\0\0\0\0\0\03333332@333333À\0\0\0\0\0\0\0\0\0\0\0\0à?\0\0\0\0\0\0\0@\0\0\0\0\0\0&@š™™™™™@\0\0\0\0\0\0(@333333 @\0\0\0\0\0\0\0\0\0\n@\0\0\0\0\0\0@\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0ð?\0\0\0\0\0\0ð?\0\0\0\0\0\0@\0\0\0\0\0\0ð?\0\0\0\0\0\0@\0\0\0\0\0\0@\0\0\0\0\0\0ð?\0\0\0\0\0\0@\0\0\0\0\0\0ð?\0\0\0\0\0\0ð?\0\0\0\0\0\0\0\0\0\0@\0\0\0\0\0\0\0@\0\0\0\0\0\0@\0\0\0\0\0\0\0@\0\0\0\0\0\0@\0\0\0\0\0\0@\0\0\0\0\0\0\0@\0\0\0\0\0\0@\0\0\0\0\0\0\0@\0\0\0\0\0\0\0@\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0@\0\0\0\0\0\0@\0\0\0\0\0\0@\0\0\0\0\0\0\0@\0\0\0\0\0\0@\0\0\0\0\0\0@\0\0\0\0\0\0@\0\0\0\0\0\0@\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0@ffffff@\0\0\0333333@333333%@\0\0\0\0\0\0\0\0\0\0\0\0\0\0€A@\0\0\0\0\0\0$@\0\0\0\0\0€F@\0\0\0\0\0€F@\0\0\0\0\0\0.@\0\0\0\0\0\0D@\0\0\0\0\0\0$@\0\0\0\0\0\04@\0\0\0\0\0€A@\0\0\0\0\0\0$@\0\0\0\0\0\0\0\0\04@\0\0\0\0\0\0>@\0\0\0\0\0€A@\0\0\0\0\0€A@\0\0\0\0\0\0>@\0\0\0\0\0\04@\0\0\0\0\0\04@\0\0\0\0\0\0>@',NULL);
/*!40000 ALTER TABLE `spatial_plus` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Dumping routines for database 'adabase_examples'
--
/*!50003 DROP PROCEDURE IF EXISTS `multiple_rowsets` */;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = 'NO_ENGINE_SUBSTITUTION' */ ;
DELIMITER ;;
CREATE DEFINER=`root`@`%` PROCEDURE `multiple_rowsets`()
BEGIN
   SELECT * FROM fruits ORDER BY calories DESC LIMIT 8;
   SELECT fruit FROM fruits WHERE color = 'red';
   SELECT team_id, abbreviation FROM nhl_teams WHERE team_id > 28;
END ;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2016-06-15 10:31:16
