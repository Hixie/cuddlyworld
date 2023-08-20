{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit testmechanics7;

interface

//{$DEFINE PLAY_IN_TEST_STAIRLAND}

procedure TestMechanics7();

implementation

uses
   sysutils, storable, matcher, physics, player, locations, things, thingdim, grammarian, world, testmechanics;

procedure TestMechanics7();

   function InitTest: TWorld;
   var
      World: TTestWorld;
      TestRoom: TLocation;
      Index: Integer;
   begin
      World := TTestWorld.Create();

      { Locations }
      TestRoom := TGroundLocation.Create('Test Room', 'the test room', 'a test room', 'A virtual room of testiness.', CreateStoneSurface());
      TestRoom.Add(TStructure.Create('wall', 'wall/walls', 'The wall is virtual.', 'That would be a no.'), tpPartOfImplicit);

      { Objects }
      TestRoom.GetSurface().Add(TDescribedPhysicalThing.Create('big block', 'big? block/blocks', 'It is a big block.', tmPonderous, tsBig), tpOn);
      for Index := 1 to 15 do
      begin
         TestRoom.GetSurface().Add(TDescribedPhysicalThing.Create('coin ' + IntToStr(Index), '(coin/coins ' + IntToStr(Index) + ')&', 'It is a coin with a number on it. The number is ' + IntToStr(Index) + '.', tmLight, tsSmall), tpOn);
      end;

      { World }
      World.AddLocation(TestRoom);
      World.FStartLocation := TestRoom.GetSurface();

      Result := World;
   end;

   procedure RunTest(TestWorld: TWorld; TestPlayer: TPlayer; Proxy: TTestProxy);

      procedure RunCommand(Command: UTF8String; Responses: array of UTF8String);
      var
         Response: UTF8String;
      begin
         for Response in Responses do
            Proxy.ExpectString(Response);
         TestWorld.Perform(Command, TestPlayer);
         Proxy.ExpectDone();
      end;

   begin
      RunCommand('drop all', ['(the embroidered bag of holding labeled Tester)', 'Dropped.']);
      RunCommand('take big block', ['Taken.']);
      RunCommand('take coin 15', ['Taken.', 'You fumble the big block.']);
      RunCommand('climb big block', ['On the big block (at the test room)', 'It is a big block.', 'The big block is on the ground.']);
      RunCommand('take all', [
        'Coin 1: Taken.',
        'Coin 2: Taken.',
        'Coin 3: Taken.',
        'Coin 4: Taken.',
        'Coin 5: Taken.',
        'Coin 6: Taken.',
        'Coin 7: Taken.',
        'Coin 8: Taken.',
        'Coin 9: Taken.',
        'Coin 10: Taken.',
        'You fumble the coin 15.',
        'The coin 15 slides off the big block.',
        'Coin 11: Taken.',
        'You fumble the coin 1.',
        'The coin 1 slides off the big block.',
        'Coin 12: Taken.',
        'You fumble the coin 2.',
        'The coin 2 slides off the big block.',
        'Coin 13: Taken.',
        'You fumble the coin 3.',
        'The coin 3 slides off the big block.',
        'Coin 14: Taken.',
        'You fumble the coin 4.',
        'The coin 4 slides off the big block.',
        'Bag of holding: Taken.',
        'You fumble the coin 5.',
        'The coin 5 slides off the big block.',
        'Big block: Given your current position, that would be quite difficult.'
      ]);
      RunCommand('put coin 6 on me', ['Placed on you.']);
      RunCommand('put coin 7 on me', ['There is not enough room on you for the coin 7.']);
      RunCommand('put coin 1 on wall', ['(first taking the coin 1)', 'Taken.', 'That would be a no.']);
      RunCommand('push block on coin 5', ['Given your current position, that would be quite difficult.']);
   end;

begin
   Writeln('MECHANICS VII (moving things)');
   RunMechanicsHarness(@InitTest, @RunTest, False);
end;

end.