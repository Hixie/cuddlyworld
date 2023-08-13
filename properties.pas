{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit properties;

interface

const // keywords used as property names in "debug make" commands
   pnBackDescription = 'backDescription';
   pnBackSide = 'backSide';
   pnCannotMoveExcuse = 'cannotMoveExcuse';
   pnChild = 'child';
   pnDefiniteName = 'definiteName';
   pnDescription = 'description';
   pnDestination = 'destination';
   pnDirection = 'direction';
   pnDoor = 'door';
   pnFindDescription = 'findDescription';
   pnFrontDescription = 'frontDescription';
   pnFrontDirection = 'frontDirection';
   pnFrontSide = 'frontSide';
   pnGround = 'ground';
   pnIndefiniteName = 'indefiniteName';
   pnIngredients = 'ingredients';
   pnLandmark = 'landmark';
   pnMass = 'mass';
   pnMaxSize = 'maxSize';
   pnName = 'name';
   pnOpened = 'opened';
   pnPassageWay = 'passageWay';
   pnPattern = 'pattern';
   pnPileClass = 'pileClass';
   pnPosition = 'position';
   pnSize = 'size';
   pnSource = 'source';
   pnStairs = 'stairs';
   pnSurface = 'surface';
   pnUnderDescription = 'underDescription';
   pnWriting = 'writing';

const // keywords used as property types in "debug describe TFoo" commands
   // simple
   ptBoolean = 'boolean';
   ptString = 'string';
   ptPileClass = 'class:TPile';
   ptDoor = 'atom:TDoor';
   ptDoorSide = 'atom:TDoorSide';
   ptLocation = 'atom:TLocation';
   ptThing = 'atom:TThing';

   // special syntax
   ptIngredients = 'ingredients';
   ptPattern = 'pattern';

   // special syntax and allowed multiple times
   ptChild = 'child*';
   ptLandmark = 'landmark*';

   // enums (also update "debug describe enum" in parser.inc)
   ptDirection = 'enum:TCardinalDirection';
   ptMass = 'enum:TThingMass';
   ptSize = 'enum:TThingSize';
   ptThingPosition = 'enum:TThingPosition';

type
   TPropertyDescriber = class abstract
      procedure AddProperty(Name: UTF8String; PropertyType: UTF8String); virtual; abstract;
   end;

implementation

end.
