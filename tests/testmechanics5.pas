{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit testmechanics5;

interface

procedure TestMechanics5();

implementation

uses
   sysutils, storable, matcher, physics, player, locations, things, cuddlycamp, world, threshold, testmechanics;

procedure TestMechanics5();

   function InitTestWorld: TWorld;
   var
      Land: TLocation;
   begin
      Result := TTestWorld.Create();
      Land := TGroundLocation.Create('Land', 'the land', 'a land', 'The land is a land.', CreateEarthSurface());
      Result.AddLocation(Land);
      (Result as TTestWorld).FStartLocation := Land.GetSurface();
   end;

   procedure RunTest1(TestWorld: TWorld; TestPlayer: TPlayer; Proxy: TTestProxy);

      procedure RunCommand(Command: UTF8String; Responses: array of UTF8String);
      var
         Response: UTF8String;
      begin
         for Response in Responses do
            Proxy.ExpectString(Response);
         TestWorld.Perform(Command, TestPlayer);
         Proxy.ExpectDone();
      end;

      procedure Expect(Condition: Boolean);
      begin
         if (not Condition) then
         begin
            Writeln('Test failed.');
            Halt(1);
         end;
      end;

   begin
      TestWorld.AddPlayer(TPlayer.Create('PlayerA', 'password', pShe));
      TestWorld.AddPlayer(TPlayer.Create('PlayerB', 'password', pShe));
      TestWorld.AddPlayer(TPlayer.Create('PlayerC', 'password', pShe));
      RunCommand('find player', ['Which player do you mean, you, the other player named PlayerA, the other player named PlayerB, or the other player named PlayerC?']);
      RunCommand('find players', ['Which of the players do you want to find, you, the other player named PlayerA, the other player named PlayerB, or the other player named PlayerC? Let''s focus on one at a time.']);
      RunCommand('find the other player', ['Which other player do you mean, the other player named PlayerA, the other player named PlayerB, or the other player named PlayerC?']);
      RunCommand('find the other player named PlayerB', ['PlayerB is on the ground.']);
      RunCommand('find the male player', ['I can''t see any "male" here to find.']);
      RunCommand('find the player that is not female', ['You are on the ground.']);
      RunCommand('find the female player', ['Which female player do you mean, the other player named PlayerA, the other player named PlayerB, or the other player named PlayerC?']);
      TestWorld.AddPlayer(TPlayer.Create('PlayerD', 'password', pZe));
      RunCommand('examine PlayerD', ['PlayerD is a player. Zer eyes look into the distance, as if ze isn''t really here.', 'PlayerD is carrying:', '  A bag of holding.']);
      RunCommand('i', ['You are carrying:', '  A bag of holding.']);
      RunCommand('i do something weird', ['I don''t understand how to "do".']);
      RunCommand('i dance', ['You dance.']);
      RunCommand('i use', ['You use what?']);
      Expect(TestPlayer.Pronouns = pIt);
      RunCommand('i use she', ['Noted.']);
      Expect(TestPlayer.Pronouns = pShe);
      RunCommand('i use she pronouns', ['Noted.']);
      RunCommand('i use she her', ['Noted.']);
      RunCommand('i use she her pronouns', ['Noted.']);
      RunCommand('i use she/her', ['Noted.']);
      RunCommand('i use she/her pronouns', ['Noted.']);
      RunCommand('i use plural they/them pronouns.', ['Noted.']);
      Expect(TestPlayer.Pronouns = pPluralThey);
   end;

begin
   Writeln('MECHANICS V (players)');
   RunMechanicsHarness(@InitTestWorld, @RunTest1, False);
end;

end.