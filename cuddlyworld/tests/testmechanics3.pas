{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit testmechanics3;

//{$DEFINE PLAY_IN_TEST_EDEN}

interface

procedure TestMechanics3();

implementation

uses
   sysutils, storable, matcher, physics, player, locations, things, thingdim, grammarian, cuddlycamp, world, threshold, testmechanics;

procedure TestMechanics3();

   function InitTestWonderfulWorld: TWorld;
   var
      Song: TTestWorld;
      SkyBox, Land: TLocation;
      Sky: TThing;
      Thing, Thing2, Thing3: TThing;
   begin
      Song := TTestWorld.Create();

      { Sky Backdrop }
      Sky := TScenery.Create('skies of blue', '((sky/skies (of blue)?) (blue? sky/skies))@', 'The sky is blue.');
      (Sky as TScenery).Opened := True;
      Thing := TScenery.Create('clouds of white', '((cloud/clouds (of white)?) (white? cloud/clouds))@', 'The cloud is white.');
      Sky.Add(Thing, tpEmbedded);
      Thing := TScenery.Create('rainbow', 'rainbow/rainbows', 'The rainbow has many colours.');
      Sky.Add(Thing, tpEmbedded);
      Thing2 := TFeature.Create('colours', '(colour/colours color/colors)@', 'The colours of the rainbow are so pretty in the sky.');
      Thing.Add(Thing2, tpPartOfImplicit);
      Thing := TScenery.Create('sun', '(sun/suns star/stars)@', 'The sun is bright and blessed.');
      Sky.Add(Thing, tpEmbedded);
      SkyBox := TBackdrop.Create(Sky, tpAtImplicit);
      Song.AddLocation(SkyBox);

      { World }
      Land := TGroundLocation.Create('World', 'World', 'a world', 'The world is wonderful.', CreateEarthSurface());
      Land.AddLandmark(cdUp, Sky, [loVisibleFromFarAway]);
      Thing := TDescribedPhysicalThing.Create('trees of green', '((tree/trees (of green)?) (green tree/trees))@', 'The trees are green.', tmLudicrous, tsMassive);
      Land.GetSurface().Add(Thing, tpPlantedIn);
      Thing := TDescribedPhysicalThing.Create('rose bush', '(((blooming rose)% bush/bushes) ((blooming red)* rose/roses))@', 'The roses are red, and blooming for us.', tmLight, tsSmall);
      Land.GetSurface().Add(Thing, tpPlantedIn);
      Thing := TDescribedPhysicalThing.Create('people going by', '((person/people (going by)?) (friend/friends (shaking hands)?) ((babe/babes baby/babies)@ crying?))@', 'The people have faces. Some of the people appear to be friends; they are shaking hands. There are also some crying babies. Those are growing.', tmPonderous, tsMassive);
      Land.GetSurface().Add(Thing, tpOn);
      Thing2 := TFeature.Create('faces', 'face/faces', 'The faces of the people have many colours.');
      Thing.Add(Thing2, tpPartOfImplicit);
      Thing3 := TFeature.Create('colours', '(colour/colours color/colors)@', 'The colours of the faces of the people going by are as pretty as the colours of the rainbow.');
      Thing2.Add(Thing3, tpOnImplicit);
      Thing2 := TFeature.Create('hands', 'hand/hands', 'The friends shaking hands are saying "How do you do?" but really mean "I love you".');
      Thing.Add(Thing2, tpPartOfImplicit);

      Song.AddLocation(Land);
      Song.FStartLocation := Land.GetSurface();

      Result := Song;
   end;

   procedure RunTest(TestPlayer: TPlayer; Proxy: TTestProxy);
   begin

       // Things to test:
       // "take all"
       // "x colours of the rainbow" vs "x colours from the rainbow"
       // "find colours of the faces of the people going by" (vs "find colours from the faces from the people going by")
       // "enter bag", "drop bag", "enter bag", "exit", "take bag"

       Proxy.ExpectDone();
       Proxy.Test('End');
   end;

begin
   Writeln('MECHANICS III');
   RunMechanicsHarness(@InitTestWonderfulWorld, @RunTest, {$IFDEF PLAY_IN_TEST_EDEN} True {$ELSE} False {$ENDIF});
end;

end.