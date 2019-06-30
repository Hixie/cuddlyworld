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
      Thing := TScenery.Create('sun', '(sun/suns star/stars)@', 'The sun is giving us bright and blessed days.');
      Sky.Add(Thing, tpEmbedded);
      SkyBox := TBackdrop.Create(Sky, tpAtImplicit);
      Song.AddLocation(SkyBox);

      { World }
      Land := TGroundLocation.Create('World', 'the world', 'a world', 'The world is wonderful.', CreateEarthSurface());
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

   procedure RunTest(TestWorld: TWorld; TestPlayer: TPlayer; Proxy: TTestProxy);

      procedure RunCommand(Command: UTF8String; Responses: array of UTF8String);
      var
         Response: UTF8String;
      begin
         for Response in Responses do
            Proxy.ExpectString(Response);
         Proxy.ExpectString('');
         TestWorld.Perform(Command, TestPlayer);
         Proxy.ExpectDone();
      end;

   begin
      // The ones marked XXX are buggy.
      RunCommand('look', ['World', 'The world is wonderful. There are trees of green here. There is a rose bush here.', 'There are people going by here.']);
      RunCommand('x people', ['Which of the people do you want to examine first, the people going by or you?']);
      RunCommand('x people going by', ['The people have faces. Some of the people appear to be friends; they are shaking hands. There are also some crying babies. Those are growing.']);
      RunCommand('x the colors on the faces of the people going by', ['The colours of the faces of the people going by are as pretty as the colours of the rainbow.']);
      RunCommand('x the colors on the faces from the people going by', ['The colours of the faces of the people going by are as pretty as the colours of the rainbow.']);
      RunCommand('x the colors of the rainbow', ['The colours of the rainbow are so pretty in the sky.']);
      RunCommand('x the colors from the rainbow', ['The colours of the rainbow are so pretty in the sky.']);
      RunCommand('take all', ['People going by: Taken.', 'You fumble the people going by.', 'Colours: The colours are on the faces.']); // should this take the bush? XXX
      RunCommand('enter bag', ['That would prove rather challenging given where the bag of holding is relative to yourself.']);
      RunCommand('drop bag', ['Dropped.']);
      RunCommand('enter bag', ['In the bag of holding (at the world)', 'The bag has the name "Tester" embroidered around its rim.']);
      RunCommand('exit bag', ['I don''t understand how to exit "bag".']); // XXX
      RunCommand('exit', ['World', 'The world is wonderful. There are trees of green here. There is a rose bush here.', 'There are people going by here.', 'There is a bag of holding here.']);
      RunCommand('take bag', ['Taken.']);
   end;

begin
   Writeln('MECHANICS III');
   RunMechanicsHarness(@InitTestWonderfulWorld, @RunTest, {$IFDEF PLAY_IN_TEST_EDEN} True {$ELSE} False {$ENDIF});
end;

end.
