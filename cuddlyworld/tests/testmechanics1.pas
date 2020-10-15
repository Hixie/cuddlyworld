{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit testmechanics1;

interface

procedure TestMechanics1();

implementation

uses
   sysutils, storable, matcher, physics, player, locations, things, thingdim, grammarian, cuddlycamp, world, threshold, testmechanics;

procedure TestMechanics1();

   function InitTestEden: TWorld;
   var
      World: TTestWorld;
      SkyBox, Camp, Cliff, Cave1, Cave2, Cave3, Cave4, FlowerRoom, Kitchen, Olympus: TLocation;
      CampMountain, CampForest, Sky: TThing;
      CliffMountain, CliffForest, CliffCamp, CliffCliff: TThing;
      Thing, Thing2: TThing;
   begin
      World := TTestWorld.Create();

      { Sky Backdrop }
      Sky := TScenery.Create('sky', 'blue? sky/skies', 'The sky is clear.');
      Thing := TScenery.Create('sun', '(sun/suns star/stars)@', 'The sun is bright.');
      (Sky as TScenery).Opened := True;
      Sky.Add(Thing, tpEmbedded);
      SkyBox := TBackdrop.Create(Sky, tpAtImplicit);
      World.AddLocation(SkyBox);

      { Locations }
      Camp := TGroundLocation.Create('Camp Cuddlyfort', 'Camp Cuddlyfort', 'a camp', 'This is a camp nestled in a forest, under the shadow of a mountain to the north.', CreateEarthSurface());
      Cliff := TGroundLocation.Create('Foot of Cliff Face', 'the foot of the cliff face', 'a foot of a cliff face', 'The south side of a mountain rises out of the ground here, in a clear and well-defined way, as if to say "this far, no farther" to an enemy whose nature you cannot fathom. ' + 'The cliff to the north is a sheer rock face, essentially unclimbable, with a cave entrance. ' + ' Conspicuous is the absence of any vegetation anywhere on the cliff, at least as far as you can see. At the base of the cliff to the east and west is a dense forest.', CreateEarthSurface());
      Cave1 := TGroundLocation.Create('Cave one', 'the first cave', 'a cave', 'The cave is very well-lit from the south.', CreateEarthSurface());
      Cave2 := TGroundLocation.Create('Cave two', 'the second cave', 'a cave', 'The cave is somewhat well-lit from the south.', CreateEarthSurface());
      Cave3 := TGroundLocation.Create('Cave three', 'the third cave', 'a cave', 'The cave is lit from the south.', CreateEarthSurface());
      Cave4 := TGroundLocation.Create('Cave four', 'the fourth cave', 'a cave', 'The cave is brightly lit from an entrace to a white room to the west. There is also some dim light coming from the south.', CreateEarthSurface());
      FlowerRoom := TGroundLocation.Create('Flower room', 'the flower room', 'a flower room', 'The room has bright ambient lighting for no apparent reason. It is a bright white room, almost clinical in nature, but it unexpectedly conveys a sense of floweriness. An exit to the east appears to lead to a dimly lit cave, while another exit leads south. A third goes up, ascending towards the heavens.', CreateEarthSurface());
      Kitchen := TGroundLocation.Create('Kitchen', 'the kitchen', 'a kitchen', 'The room has bright ambient lighting for no apparent reason. It is a bright white room, almost clinical in nature, but it unexpectedly conveys a sense of being, or having once been, used for food preparation. An exit leads north.', CreateEarthSurface());
      Olympus := TGroundLocation.Create('Mount Olympus', 'Mount Olympus', 'a mountain top', 'The top of Olympus is more business-like than the legends would suggest: any ancient Greek stylings have been replaced by a modern, sleek, and understated decor.', CreateStoneSurface());

      { Camp }
      CampMountain := TScenery.Create('mountain', 'big? mountain/mountains', 'The mountain is big.');
      Camp.Add(CampMountain, tpAroundImplicit);
      CampForest := TScenery.Create('forest', '((forest/forests (of trees)?) tree/trees)@', 'The forest is dense and impassable.');
      Camp.Add(CampForest, tpAroundImplicit);
      Camp.GetSurface().Add(TPile.Create(['leaf'], ['leaves'], 'It appears someone has collected fallen leaves from the forest. Possibly the entire forest, given how big the pile is.', tmLight, tsGigantic), tpOn);
      Camp.GetSurface().Add(TDescribedPhysicalThing.Create('MacGuffin', 'MacGuffin/MacGuffins', 'The MacGuffin displays outward signs of being avian in nature.', tmHeavy, tsBig), tpOn);
      Camp.GetSurface().Add(TDescribedPhysicalThing.Create('penny', 'penny/pennies', 'The penny is a copper coin of little value.', tmLight, tsSmall), tpOn);
      Camp.AddLandmark(cdUp, Sky, [loVisibleFromFarAway]);

      { Cliff }
      CliffMountain := TScenery.Create('mountain', 'mountain/mountains', 'From here you cannot get a good sense of the size of the mountain. Its cliff face dominates your view.');
      (CliffMountain as TScenery).FindDescription := 'The mountain towers high above you.';
      Cliff.Add(CliffMountain, tpAtImplicit);
      CliffCliff := TScenery.Create('cliff', '((sheer rock cliff/cliffs)& (rock face/faces))@', 'The cliff consists of a sheer rock face with a cave entrace in it.');
      CliffMountain.Add(CliffCliff, tpPartOfImplicit);
      CliffForest := TScenery.Create('forest', '((forest/forests (of trees)?) tree/trees)@', 'The forest is dense and impassable.');
      Cliff.Add(CliffForest, tpAroundImplicit);
      CliffCamp := TScenery.Create('camp', 'camp/camps', 'The camp is hard to see from here.');
      Cliff.Add(CliffCamp, tpAroundImplicit);
      Cliff.GetSurface().Add(TDescribedPhysicalThing.Create('large pink balloon', '((large huge massive)@ pink balloon/balloons)&', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured pink.', tmLight, tsMassive), tpOn);
      Cliff.GetSurface().Add(TDescribedPhysicalThing.Create('large grey balloon', '((large huge massive)@ (grey gray)@ balloon/balloons)&', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured grey.', tmLight, tsMassive), tpOn);
      Cliff.GetSurface().Add(TDescribedPhysicalThing.Create('large black balloon', '((large huge massive)@ black balloon/balloons)&', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured black.', tmLight, tsMassive), tpOn);
      Cliff.GetSurface().Add(TDescribedPhysicalThing.Create('large white balloon', '((large huge massive)@ white balloon/balloons)&', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured white.', tmLight, tsMassive), tpOn);
      Cliff.GetSurface().Add(TDescribedPhysicalThing.Create('large violet balloon', '((large huge massive)@ (violet purple)@ balloon/balloons)&', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured violet.', tmLight, tsMassive), tpOn);
      Cliff.GetSurface().Add(TDescribedPhysicalThing.Create('large blue balloon', '((large huge massive)@ blue balloon/balloons)&', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured blue.', tmLight, tsMassive), tpOn);
      Cliff.GetSurface().Add(TDescribedPhysicalThing.Create('large green balloon', '((large huge massive)@ green balloon/balloons)&', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured green.', tmLight, tsMassive), tpOn);
      Cliff.GetSurface().Add(TDescribedPhysicalThing.Create('large yellow balloon', '((large huge massive)@ yellow balloon/balloons)&', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured yellow.', tmLight, tsMassive), tpOn);
      Cliff.GetSurface().Add(TDescribedPhysicalThing.Create('large orange balloon', '((large huge massive)@ orange balloon/balloons)&', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured orange.', tmLight, tsMassive), tpOn);
      Cliff.GetSurface().Add(TDescribedPhysicalThing.Create('large red balloon', '((large huge massive)@ red balloon/balloons)&', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured red.', tmLight, tsMassive), tpOn);
      Cliff.GetSurface().Add(TSpade.Create(), tpOn);
      Cliff.AddLandmark(cdUp, Sky, [loVisibleFromFarAway]);


      { Cave 1 }

      Thing := TBag.Create('brown sack', '(elongated brown (sack/sacks bag/bags)@)&', 'The sack is brown.', tsBig);
      Cave1.GetSurface().Add(Thing, tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('clove of garlic', '((clove/cloves of garlic) (garlic clove/cloves)&)@', 'There''s nothing special about the clove of garlic.', tmLight, tsSmall), tpIn);
      Thing.Add(TDescribedPhysicalThing.Create('lunch', 'lunch/lunches', 'There''s nothing special about the lunch.', tmLight, tsSmall), tpIn);
      Thing.Add(TDescribedPhysicalThing.Create('wooden spoon', '((wooden wood)@ (spoon/spoons utensil/utensils)@)&', 'The spoon is made of wood.', tmLight, tsSmall), tpIn);

      Thing := TDescribedPhysicalThing.Create('kitchen table', '(non-descript kitchen table/tables)&', 'The kitchen table is non-descript.', tmPonderous, tsMassive);
      Cave1.GetSurface().Add(Thing, tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('plastic knife', '(plastic (knife/knives utensil/utensils)@)&', 'The knife is made of plastic.', tmLight, tsSmall), tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('plastic fork', '(plastic (fork/forks utensil/utensils)@)&', 'The fork is made of plastic.', tmLight, tsSmall), tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('plastic spoon', '(plastic (spoon/spoons utensil/utensils)@)&', 'The spoon is made of plastic.', tmLight, tsSmall), tpOn);

      Thing := TDescribedPhysicalThing.Create('desk', '(non-descript (desk/desks table/tables)@)&', 'The desk is non-descript.', tmPonderous, tsMassive);
      Cave1.GetSurface().Add(Thing, tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('stainless steel knife', '(stainless steel (knife/knives utensil/utensils)@)&', 'The knife is made of stainless steel.', tmLight, tsSmall), tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('stainless steel fork', '(stainless steel (fork/forks utensil/utensils)@)&', 'The fork is made of stainless steel.', tmLight, tsSmall), tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('stainless steel spoon', '(stainless steel (spoon/spoons utensil/utensils)@)&', 'The spoon is made of stainless steel.', tmLight, tsSmall), tpOn);

      Thing := TDescribedPhysicalThing.Create('dining room table', '(non-descript dining room table/tables)&', 'The dining room table is non-descript.', tmPonderous, tsMassive);
      Cave1.GetSurface().Add(Thing, tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('silver knife', '(silver (knife/knives utensil/utensils)@)&', 'The knife is made of silver.', tmLight, tsSmall), tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('silver fork', '(silver (fork/forks utensil/utensils)@)&', 'The fork is made of silver.', tmLight, tsSmall), tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('silver spoon', '(silver (spoon/spoons utensil/utensils)@)&', 'The spoon is made of silver.', tmLight, tsSmall), tpOn);

      Cave1.GetSurface().Add(TSpade.Create(), tpOn);

      Thing := TPile.Create(['rock'], ['rocks'], 'The pile of rocks is boring and uninteresting.', tmHeavy, tsBig);
      Cave1.GetSurface().Add(Thing, tpOn);
      Thing.Add(TPile.Create(['diamond'], ['diamonds'], 'The pile of diamonds is the tiniest pile of diamonds you have ever seen.', tmLight, tsSmall), tpEmbedded);


      { Cave 2 }

      Thing := TBag.Create('brown sack', '(elongated brown (sack/sacks bag/bags)@)&', 'The sack is brown.', tsBig);
      Cave2.GetSurface().Add(Thing, tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('clove of garlic', '((clove/cloves of garlic) (garlic clove/cloves)&)@', 'There''s nothing special about the clove of garlic.', tmLight, tsSmall), tpIn);
      Thing.Add(TDescribedPhysicalThing.Create('lunch', 'lunch/lunches', 'There''s nothing special about the lunch.', tmLight, tsSmall), tpIn);
      Thing.Add(TDescribedPhysicalThing.Create('wooden spoon', '((wooden wood)@ (spoon/spoons utensil/utensils)@)&', 'The spoon is made of wood.', tmLight, tsSmall), tpIn);

      Thing := TDescribedPhysicalThing.Create('kitchen table', '(non-descript kitchen table/tables)&', 'The kitchen table is non-descript.', tmPonderous, tsMassive);
      Cave2.GetSurface().Add(Thing, tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('plastic knife', '(plastic (knife/knives utensil/utensils)@)&', 'The knife is made of plastic.', tmLight, tsSmall), tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('plastic fork', '(plastic (fork/forks utensil/utensils)@)&', 'The fork is made of plastic.', tmLight, tsSmall), tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('plastic spoon', '(plastic (spoon/spoons utensil/utensils)@)&', 'The spoon is made of plastic.', tmLight, tsSmall), tpOn);

      Thing := TDescribedPhysicalThing.Create('desk', '(non-descript (desk/desks table/tables)@)&', 'The desk is non-descript.', tmPonderous, tsMassive);
      Cave2.GetSurface().Add(Thing, tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('stainless steel knife', '(stainless steel (knife/knives utensil/utensils)@)&', 'The knife is made of stainless steel.', tmLight, tsSmall), tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('stainless steel fork', '(stainless steel (fork/forks utensil/utensils)@)&', 'The fork is made of stainless steel.', tmLight, tsSmall), tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('stainless steel spoon', '(stainless steel (spoon/spoons utensil/utensils)@)&', 'The spoon is made of stainless steel.', tmLight, tsSmall), tpOn);

      Thing := TDescribedPhysicalThing.Create('dining room table', '(non-descript dining room table/tables)&', 'The dining room table is non-descript.', tmPonderous, tsMassive);
      Cave2.GetSurface().Add(Thing, tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('silver knife', '(silver (knife/knives utensil/utensils)@)&', 'The knife is made of silver.', tmLight, tsSmall), tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('silver fork', '(silver (fork/forks utensil/utensils)@)&', 'The fork is made of silver.', tmLight, tsSmall), tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('silver spoon', '(silver (spoon/spoons utensil/utensils)@)&', 'The spoon is made of silver.', tmLight, tsSmall), tpOn);

      Cave2.GetSurface().Add(TSpade.Create(), tpOn);

      Thing := TPile.Create(['rock'], ['rocks'], 'The pile of rocks is boring and uninteresting.', tmHeavy, tsBig);
      Cave2.GetSurface().Add(Thing, tpOn);
      Thing.Add(TPile.Create(['diamond'], ['diamonds'], 'The pile of diamonds is the tiniest pile of diamonds you have ever seen.', tmLight, tsSmall), tpEmbedded);


      { Cave 3 }

      Thing := TBag.Create('brown sack', '(elongated brown (sack/sacks bag/bags)@)&', 'The sack is brown.', tsBig);
      Cave3.GetSurface().Add(Thing, tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('clove of garlic', '((clove/cloves of garlic) (garlic clove/cloves)&)@', 'There''s nothing special about the clove of garlic.', tmLight, tsSmall), tpIn);
      Thing.Add(TDescribedPhysicalThing.Create('lunch', 'lunch/lunches', 'There''s nothing special about the lunch.', tmLight, tsSmall), tpIn);
      Thing.Add(TDescribedPhysicalThing.Create('wooden spoon', '((wooden wood)@ (spoon/spoons utensil/utensils)@)&', 'The spoon is made of wood.', tmLight, tsSmall), tpIn);

      Cave3.GetSurface().Add(TSpade.Create(), tpOn);

      Thing := TDescribedPhysicalThing.Create('kitchen table', '(non-descript kitchen table/tables)&', 'The kitchen table is non-descript.', tmPonderous, tsMassive);
      Cave3.GetSurface().Add(Thing, tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('plastic knife', '(plastic (knife/knives utensil/utensils)@)&', 'The knife is made of plastic.', tmLight, tsSmall), tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('plastic fork', '(plastic (fork/forks utensil/utensils)@)&', 'The fork is made of plastic.', tmLight, tsSmall), tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('plastic spoon', '(plastic (spoon/spoons utensil/utensils)@)&', 'The spoon is made of plastic.', tmLight, tsSmall), tpOn);

      Thing := TDescribedPhysicalThing.Create('desk', '(non-descript (desk/desks table/tables)@)&', 'The desk is non-descript.', tmPonderous, tsMassive);
      Cave3.GetSurface().Add(Thing, tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('stainless steel knife', '(stainless steel (knife/knives utensil/utensils)@)&', 'The knife is made of stainless steel.', tmLight, tsSmall), tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('stainless steel fork', '(stainless steel (fork/forks utensil/utensils)@)&', 'The fork is made of stainless steel.', tmLight, tsSmall), tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('stainless steel spoon', '(stainless steel (spoon/spoons utensil/utensils)@)&', 'The spoon is made of stainless steel.', tmLight, tsSmall), tpOn);

      Thing := TDescribedPhysicalThing.Create('dining room table', '(non-descript dining room table/tables)&', 'The dining room table is non-descript.', tmPonderous, tsMassive);
      Cave3.GetSurface().Add(Thing, tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('silver knife', '(silver (knife/knives utensil/utensils)@)&', 'The knife is made of silver.', tmLight, tsSmall), tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('silver fork', '(silver (fork/forks utensil/utensils)@)&', 'The fork is made of silver.', tmLight, tsSmall), tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('silver spoon', '(silver (spoon/spoons utensil/utensils)@)&', 'The spoon is made of silver.', tmLight, tsSmall), tpOn);

      Thing := TPile.Create(['rock'], ['rocks'], 'The pile of rocks is boring and uninteresting.', tmHeavy, tsBig);
      Cave3.GetSurface().Add(Thing, tpOn);
      Thing.Add(TPile.Create(['diamond'], ['diamonds'], 'The pile of diamonds is the tiniest pile of diamonds you have ever seen.', tmLight, tsSmall), tpEmbedded);


      { Cave 4 }

      Cave4.GetSurface().Add(TDescribedPhysicalThing.Create('wooden spoon', '((wooden wood)@ (spoon/spoons utensil/utensils)@)&', 'The spoon is made of wood.', tmLight, tsSmall), tpOn);

      Thing := TDescribedPhysicalThing.Create('plastic table', '(plastic table/tables)&', 'The table is made of plastic.', tmPonderous, tsMassive);
      Cave4.GetSurface().Add(Thing, tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('plastic knife', '(plastic (knife/knives utensil/utensils)@)&', 'The knife is made of plastic.', tmLight, tsSmall), tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('plastic fork', '(plastic (fork/forks utensil/utensils)@)&', 'The fork is made of plastic.', tmLight, tsSmall), tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('plastic spoon', '(plastic (spoon/spoons utensil/utensils)@)&', 'The spoon is made of plastic.', tmLight, tsSmall), tpOn);

      Thing := TDescribedPhysicalThing.Create('stainless steel table', '(stainless steel table/tables)&', 'The table is made of stainless steel.', tmPonderous, tsMassive);
      Cave4.GetSurface().Add(Thing, tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('stainless steel knife', '(stainless steel (knife/knives utensil/utensils)@)&', 'The knife is made of stainless steel.', tmLight, tsSmall), tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('stainless steel fork', '(stainless steel (fork/forks utensil/utensils)@)&', 'The fork is made of stainless steel.', tmLight, tsSmall), tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('stainless steel spoon', '(stainless steel (spoon/spoons utensil/utensils)@)&', 'The spoon is made of stainless steel.', tmLight, tsSmall), tpOn);

      Thing := TDescribedPhysicalThing.Create('silver table', '(silver table/tables)&', 'The table is made of silver.', tmPonderous, tsMassive);
      Cave4.GetSurface().Add(Thing, tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('silver knife', '(silver (knife/knives utensil/utensils)@)&', 'The knife is made of silver.', tmLight, tsSmall), tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('silver fork', '(silver (fork/forks utensil/utensils)@)&', 'The fork is made of silver.', tmLight, tsSmall), tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('silver spoon', '(silver (spoon/spoons utensil/utensils)@)&', 'The spoon is made of silver.', tmLight, tsSmall), tpOn);

      Cave4.Add(TScenery.Create('cave paintings', 'cave? painting/paintings', 'The cave paintings are non-descript.'), tpAt);


      { Flower Room }

      Thing := TDescribedPhysicalThing.Create('red table', '(red table/tables)&', 'The table is red.', tmHeavy, tsBig);
      FlowerRoom.GetSurface().Add(Thing, tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('red vase', '(red vase/vases)&', 'The vase is red.', tmLight, tsSmall), tpOn);

      Thing := TDescribedPhysicalThing.Create('blue table', '(blue table/tables)&', 'The table is blue.', tmHeavy, tsBig);
      FlowerRoom.GetSurface().Add(Thing, tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('blue vase', '(blue vase/vases)&', 'The vase is blue.', tmLight, tsSmall), tpOn);


      { Kitchen }

      Thing := TDescribedPhysicalThing.Create('wooden table', '(brown wooden table/tables)&', 'The table is made of brown wood.', tmHeavy, tsBig);
      Kitchen.GetSurface().Add(Thing, tpOn);

      Thing2 := TDescribedPhysicalThing.Create('fruit plate', '(fruit plate/plates)&', 'The plate is intended to hold fruit.', tmLight, tsSmall);
      Thing.Add(Thing2, tpOn);
      Thing2.Add(TDescribedPhysicalThing.Create('red grapes', '(red (grape/grapes fruit)@)&', 'The grapes are red.', tmLight, tsSmall), tpOn);
      Thing2.Add(TDescribedPhysicalThing.Create('green grapes', '(green (grape/grapes fruit)@)&', 'The grapes are green.', tmLight, tsSmall), tpOn);
      Thing2.Add(TDescribedPhysicalThing.Create('red apple', '(red (apple/apples fruit)@)&', 'The apple is red.', tmLight, tsSmall), tpOn);
      Thing2.Add(TDescribedPhysicalThing.Create('green apple', '(green (apple/apples fruit)@)&', 'The apple is green.', tmLight, tsSmall), tpOn);
      Thing2.Add(TDescribedPhysicalThing.Create('yellow apple', '(yellow (apple/apples fruit)@)&', 'The apple is yellow.', tmLight, tsSmall), tpOn);
      Thing2.Add(TDescribedPhysicalThing.Create('red pepper', '(red (pepper/peppers fruit)@)&', 'The pepper is red.', tmLight, tsSmall), tpOn);
      Thing2.Add(TDescribedPhysicalThing.Create('green pepper', '(green (pepper/peppers fruit)@)&', 'The pepper is green.', tmLight, tsSmall), tpOn);
      Thing2.Add(TDescribedPhysicalThing.Create('yellow pepper', '(yellow (pepper/peppers fruit)@)&', 'The pepper is yellow.', tmLight, tsSmall), tpOn);
      Thing2.Add(TDescribedPhysicalThing.Create('orange', '(orange (orange/oranges fruit)@)&', 'The orange is orange.', tmLight, tsSmall), tpOn);
      Thing2.Add(TDescribedPhysicalThing.Create('apricot', '(orange (apricot/apricots fruit)@)&', 'The apricot is orange.', tmLight, tsSmall), tpOn);
      Thing2.Add(TDescribedPhysicalThing.Create('banana', '(yellow (banana/bananas fruit)@)&', 'The banana is yellow.', tmLight, tsSmall), tpOn);

      Thing2 := TContainer.Create('box', '((berry fruit)@ (box/boxes container/containers)@)&', 'The box is intended to hold berries.', tmLight, tsSmall);
      Thing.Add(Thing2, tpOn);
      Thing2.Add(TDescribedPhysicalThing.Create('strawberries', '(red (strawberry/strawberries berry/berries fruit)@)&', 'The strawberries are red.', tmLight, tsSmall), tpIn);
      Thing2.Add(TDescribedPhysicalThing.Create('blueberries', '(blue (blueberry/blueberries berry/berries fruit)@)&', 'The blueberries are blue.', tmLight, tsSmall), tpIn);

      Thing2 := TContainer.Create('crate', '(fruit (crate/crates container/containers)@)&', 'The crate is intended to hold fruit.', tmLight, tsSmall);
      Thing.Add(Thing2, tpOn);
      Thing2.Add(TDescribedPhysicalThing.Create('pineapple', '((prickly brown)# (pineapple/pineapples fruit)@)&', 'The pineapple is brown and prickly.', tmLight, tsSmall), tpIn);
      Thing2.Add(TDescribedPhysicalThing.Create('kiwi', '((furry brown)# (kiwi/kiwis fruit)@)&', 'The kiwi is brown and furry.', tmLight, tsSmall), tpIn);

      Thing := TBag.Create('garbage bag', '(black ((garbage bag/bags)& (trash bag/bags) trashbag/trashbags)@)&', 'The garbage bag is black.', tsBig);
      Kitchen.GetSurface().Add(Thing, tpOn);
      Thing.Add(TDescribedPhysicalThing.Create('rotten grapes', '((rotten red)# (grape/grapes fruit)@)&', 'The grapes is rotten.', tmLight, tsSmall), tpIn);
      Thing.Add(TDescribedPhysicalThing.Create('rotten kiwi', '((rotten furry brown)# (kiwi/kiwis fruit)@)&', 'The kiwi is rotten.', tmLight, tsSmall), tpIn);
      Thing.Add(TDescribedPhysicalThing.Create('rotten pineapple', '((rotten prickly brown)# (pineapple/pineapples fruit)@)&', 'The pineapple is rotten.', tmLight, tsSmall), tpIn);


      { Olympus }
      Thing := TDescribedPhysicalThing.Create('round table', '(round table/tables)&', 'The table, perfectly circular and so wide that you can''t reach from one side to the other, is made of a very dense, black glossy material. Etched in the table top are three large geometric shapes: a square, a triangle, and an oval.', tmLudicrous, tsMassive);
      Thing2 := TFeature.Create('square', '(((large (etched carved)@)# (square/squares (geometric shape/shapes)&)@)& (square (etching/etchings carving/carvings)@)&)@', 'The square is etched into the table.');
      Thing2.Add(TDescribedPhysicalThing.Create('Astorian people', '(Astorian people/peoples)&', 'The Astorian people are iconically represented for manipulation by the higher powers.', tmLight, tsSmall), tpOn);
      Thing2.Add(TDescribedPhysicalThing.Create('Dagian people', '(Dagian people/peoples)&', 'The Dagian people are iconically represented for manipulation by the higher powers.', tmLight, tsSmall), tpOn);
      Thing2.Add(TDescribedPhysicalThing.Create('Linian people', '(Linian people/peoples)&', 'The Linian people are iconically represented for manipulation by the higher powers.', tmLight, tsSmall), tpOn);
      Thing.Add(Thing2, tpPartOfImplicit);
      Thing2 := TFeature.Create('triangle', '(((large (etched carved)@)# (triangle/triangles (geometric shape/shapes)&)@)& (triangle (etching/etchings carving/carvings)@)&)@', 'The triangle is etched into the table.');
      Thing.Add(Thing2, tpPartOfImplicit);
      Thing2 := TFeature.Create('oval', '(((large (etched carved)@ round)# (oval/ovals (geometric shape/shapes)&)@)& (oval (etching/etchings carving/carvings)@)&)@', 'The oval is etched into the table.');
      Thing2.Add(TDescribedPhysicalThing.Create('Vatejian people', '(Vatejian people/peoples)&', 'The Vategian people are iconically represented for manipulation by the higher powers.', tmLight, tsSmall), tpOn);
      Thing.Add(Thing2, tpPartOfImplicit);
      Thing2 := TDescribedPhysicalThing.Create('Striped person', '(striped person/people)&', 'The person has a stripe as their only identifying feature.', tmLight, tsSmall);
      Thing2.Add(TFeature.Create('stripe', 'stripe/stripes', 'The stripe is an integral part of the person.'), tpPartOfImplicit);
      Thing.Add(Thing2, tpOn);
      Thing2 := TDescribedPhysicalThing.Create('Cloudy person', '(cloudy person/people)&', 'The person has a cloud as their only identifying feature.', tmLight, tsSmall);
      Thing2.Add(TFeature.Create('cloud', 'cloud/clouds', 'The cloud is an integral part of the person.'), tpPartOfImplicit);
      Thing.Add(Thing2, tpOn);
      Thing2 := TDescribedPhysicalThing.Create('Gloved person', '(gloved person/people)&', 'The person has a glove as their only identifying feature.', tmLight, tsSmall);
      Thing2.Add(TFeature.Create('glove', 'glove/gloves', 'The glove is an integral part of the person.'), tpPartOfImplicit);
      Thing.Add(Thing2, tpOn);
      Thing2 := TDescribedPhysicalThing.Create('Rich person', '(rich person/people)&', 'The person has riches as their only identifying feature.', tmLight, tsSmall);
      Thing2.Add(TFeature.Create('riches', 'richness/riches', 'The riches are an integral part of the person.'), tpPartOfImplicit);
      Thing.Add(Thing2, tpOn);
      Olympus.GetSurface().Add(Thing, tpOn);
      Olympus.AddLandmark(cdUp, Sky, [loVisibleFromFarAway]);


      { Connections }

      ConnectLocations(Camp, cdNorth, Cliff);
      ConnectLocations(Cliff, cdNorth, Cave1);
      ConnectLocations(Cave1, cdNorth, Cave2);
      ConnectLocations(Cave2, cdNorth, Cave3);
      ConnectLocations(Cave3, cdNorth, Cave4);
      ConnectLocations(Cave4, cdWest, FlowerRoom);
      ConnectLocations(FlowerRoom, cdSouth, Kitchen);
      Olympus.GetSurface().Add(TOpening.Create('opening', 'opening/openings', 'The opening is circular.', FlowerRoom, tsGigantic), tpSurfaceOpening);
      ConnectLocations(FlowerRoom, cdUp, Olympus);

      Camp.AddSurroundings(CampForest, cdCompassDirection - [cdNorth]);
      Cliff.AddSurroundings(CampForest, cdCompassDirection - [cdNorthEast, cdNorth, cdNorthWest]);

      World.AddLocation(Camp);
      World.AddLocation(Cliff);
      World.AddLocation(Cave1);
      World.AddLocation(Cave2);
      World.AddLocation(Cave3);
      World.AddLocation(Cave4);
      World.AddLocation(FlowerRoom);
      World.AddLocation(Kitchen);
      World.AddLocation(Olympus);
      World.FStartLocation := Camp.GetSurface();

      Result := World;
   end;

   procedure RunTest(TestWorld: TWorld; TestPlayer: TPlayer; Proxy: TTestProxy);
   begin

       { Basic look test }
       Proxy.Test('Login');
       Proxy.ExpectString('Camp Cuddlyfort');
       Proxy.SkipEverything();
       Proxy.StartRecording();
       TestPlayer.DoLook();
       Proxy.StopSkipping();
       Proxy.ExpectRecorded();
       TestWorld.Perform('look', TestPlayer);
       Proxy.ExpectDone();

       { Sanity tests }
       Proxy.Test('Sanity Tests');
       Proxy.ExpectString('You see nothing noteworthy when looking out.');
       TestWorld.Perform('look out', TestPlayer);
       Proxy.ExpectString('You cannot enter the penny. You are bigger than the penny.');
       TestWorld.Perform('enter penny', TestPlayer);

       { Basic parsing of things }
       Proxy.Test('Parsing of things');
       Proxy.ExpectString('I can''t see any "xyzzy" here to examine.');
       TestWorld.Perform('x xyzzy', TestPlayer);

       Proxy.ExpectString('I can''t see any "xyzzy" here to examine.');
       TestWorld.Perform('x the xyzzy', TestPlayer);

       Proxy.ExpectString('I was with you until you said "but xyzzy".');
       TestWorld.Perform('take all but xyzzy', TestPlayer);

       Proxy.ExpectString('I was with you until you said "but the xyzzy".');
       TestWorld.Perform('take all but the xyzzy one', TestPlayer);

       Proxy.ExpectString('I was with you until you said "that is xyzzy".');
       TestWorld.Perform('take all that is xyzzy', TestPlayer);

       Proxy.ExpectString('I was with you until you said "that is xyzzy".');
       TestWorld.Perform('take bag that is xyzzy', TestPlayer);

       Proxy.ExpectString('You used the term "and that is" in a way I don''t understand.');
       TestWorld.Perform('take bag and that is bag', TestPlayer);

       Proxy.ExpectString('I was with you until you said "that are xyzzy".');
       TestWorld.Perform('take all that are xyzzy', TestPlayer);

       Proxy.ExpectString('I was with you until you said "that are xyzzy".');
       TestWorld.Perform('take bag that are xyzzy', TestPlayer);

       Proxy.ExpectString('You used the term "and that are" in a way I don''t understand.');
       TestWorld.Perform('take bag and that are bag', TestPlayer);

       Proxy.ExpectString('You used the term "and on" in a way I don''t understand.');
       TestWorld.Perform('take bag and on ground', TestPlayer);

       Proxy.ExpectString('All 1234 what?');
       TestWorld.Perform('take all 1234', TestPlayer);

       Proxy.ExpectString('I was with you until you said "but but".');
       TestWorld.Perform('take all but but', TestPlayer);

       Proxy.ExpectString('I don''t understand your use of commas.');
       TestWorld.Perform('take ,', TestPlayer);

       Proxy.ExpectString('I can''t see any "," here to take.');
       TestWorld.Perform('TAKE THE,', TestPlayer);

       Proxy.ExpectSubstring('Pile of leaves: ');
       Proxy.ExpectString('MacGuffin: Taken.');
       Proxy.ExpectString('Penny: Taken.');
       Proxy.ExpectSubstring('Bag of holding: ');
       TestWorld.Perform('take all, and bag', TestPlayer);

       Proxy.ExpectString('MacGuffin: Dropped.');
       Proxy.ExpectString('Penny: Dropped.');
       TestWorld.Perform('drop all but bag', TestPlayer);

       Proxy.ExpectString('The bag has the name "Tester" embroidered around its rim.');
       TestWorld.Perform('x bag', TestPlayer);

       Proxy.ExpectString('Around the bag''s rim is embroidered the name "Tester".');
       TestWorld.Perform('x rim', TestPlayer);

       Proxy.ExpectString('Around the bag''s rim is embroidered the name "Tester".');
       TestWorld.Perform('x bag rim', TestPlayer);

       Proxy.ExpectString('I don''t understand how to examine things "bag".');
       TestWorld.Perform('x rim bag', TestPlayer);

       Proxy.ExpectString('Pile of leaves: The pile of leaves slips through your fingers.');
       Proxy.ExpectString('MacGuffin: You shake the MacGuffin.');
       TestWorld.Perform('shake the pile, some leaves, and a macguffin', TestPlayer);

       Proxy.ExpectString('What do you mean, "that is the leaf ones"?');
       TestWorld.Perform('take pile that is the leaf ones', TestPlayer);

       Proxy.ExpectString('The pile of leaves slips through your fingers.');
       TestWorld.Perform('take pile that is the leaf one', TestPlayer);

       Proxy.ExpectString('You used the term "that is" in a way I don''t understand.');
       TestWorld.Perform('take piles that is the leaf ones', TestPlayer);

       Proxy.ExpectString('You used the term "that is" in a way I don''t understand.');
       TestWorld.Perform('take piles that is the leaf one', TestPlayer);

       Proxy.ExpectString('I don''t understand how to choose the things that are all the particular "leaf one".');
       TestWorld.Perform('take piles that are the leaf one', TestPlayer);

       Proxy.ExpectString('(the pile of leaves)');
       Proxy.ExpectString('The pile of leaves slips through your fingers.');
       TestWorld.Perform('take piles that are the leaf ones', TestPlayer);

       Proxy.ExpectString('You used the term "that are" in a way I don''t understand.');
       TestWorld.Perform('take pile that are the leaf one', TestPlayer);

       Proxy.ExpectString('You used the term "that are" in a way I don''t understand.');
       TestWorld.Perform('take pile that are the leaf ones', TestPlayer);
       Proxy.ExpectDone();

       { Dig and cover test }
       Proxy.Test('Digging');
       Proxy.ExpectNoSubstring('I can''t see anything to move.');
       Proxy.WaitUntilString('');
       Proxy.ExpectString('Foot of Cliff Face');
       Proxy.ExpectNoSubstring('hole'); Proxy.AndAlso(); Proxy.ExpectNoSubstring('leaves');
       Proxy.WaitUntilString('');
       Proxy.ExpectString('(the ground with the spade)'); // dig
       Proxy.ExpectString('(first taking the spade)');
       Proxy.ExpectString('Taken.');
       Proxy.ExpectString('With much effort, you dig a huge hole.');
       Proxy.ExpectString('');
       Proxy.ExpectString('Foot of Cliff Face'); // look
       Proxy.ExpectSubstring('hole'); Proxy.AndAlso(); Proxy.ExpectNoSubstring('leaves');
       Proxy.WaitUntilString('');
       Proxy.ExpectSubstring('down'); Proxy.AndAlso(); {Proxy.ExpectSubstring('ground'); Proxy.AndAlso();} Proxy.ExpectSubstring('hole');
       Proxy.ExpectString('');
       Proxy.ExpectString('Hole in the ground'); // down
       Proxy.ExpectString('The hole is quite dirty.');
       Proxy.ExpectString('');
       Proxy.ExpectString('Hole in the ground'); // look
       Proxy.ExpectString('The hole is quite dirty.');
       Proxy.ExpectString('');
       Proxy.ExpectString('Looking out, you see:'); // look out
       Proxy.ExpectString('Foot of Cliff Face');
       Proxy.WaitUntilString('');
       Proxy.ExpectString('Looking up, you see a sky. The sky is clear.'); // look up
       Proxy.ExpectString('');
       Proxy.ExpectString('Foot of Cliff Face'); // look
       Proxy.WaitUntilString('');
       Proxy.ExpectSubstring('(first taking the '); Proxy.AndAlso(); Proxy.ExpectSubstring(' penny)');
       Proxy.ExpectString('Taken.');
       Proxy.ExpectString('Dropped on the hole.');
       Proxy.ExpectString('The penny falls into the hole.');
       Proxy.ExpectString('');
       Proxy.ExpectString('Placed on the hole.');
       Proxy.ExpectString('The spade falls into the hole.');
       Proxy.ExpectString('');
       Proxy.ExpectString('Moved onto the hole.');
       Proxy.ExpectString('The MacGuffin falls into the hole.');
       Proxy.ExpectString('');
       Proxy.ExpectString('(the pile of leaves)');
       Proxy.ExpectString('Moved onto the hole.');
       Proxy.ExpectString('');
       Proxy.SkipLine();
       Proxy.ExpectSubstring('On the hole is a pile of leaves'); { examine hole }
       Proxy.ExpectString('');
       Proxy.ExpectString('The hole is covered by a pile of leaves.'); { look in hole }
       Proxy.ExpectString('');
       Proxy.ExpectString('To look in the ground, you first need to dig a hole in it.'); { look in ground }
       Proxy.ExpectString('');
       Proxy.ExpectString('Foot of Cliff Face');
       Proxy.ExpectNoSubstring('hole'); Proxy.AndAlso(); Proxy.ExpectSubstring('leaves');
       Proxy.ExpectString('There is a large pink balloon here.');
       Proxy.ExpectString('There is a large grey balloon here.');
       Proxy.ExpectString('There is a large black balloon here.');
       Proxy.ExpectString('There is a large white balloon here.');
       Proxy.ExpectString('There is a large violet balloon here.');
       Proxy.ExpectString('There is a large blue balloon here.');
       Proxy.ExpectString('There is a large green balloon here.');
       Proxy.ExpectString('There is a large yellow balloon here.');
       Proxy.ExpectString('There is a large orange balloon here.');
       Proxy.ExpectString('There is a large red balloon here.');
       Proxy.ExpectString('There is a pile of earth here.');
       TestWorld.Perform('move all n; n; dig; l; l d; d; l; l out; l u; u; drop penny onto hole; move spade on to hole; push macguffin on hole; move leaves over hole; x hole; l in hole; look in ground; l', TestPlayer);
       Proxy.ExpectDone();

       { complex parsing }
       Proxy.Test('TScenery');
       Proxy.ExpectString('The camp cannot be moved.');
       TestWorld.Perform('move camp', TestPlayer);

       { complex parsing }
       Proxy.Test('Thing Seeker');
       Proxy.ExpectString('Which balloon do you mean, the large pink balloon, the large grey balloon, the large black balloon, the large white balloon, the large violet balloon, the large blue balloon, the large green balloon, the large yellow balloon, the large orange balloon, or the large red balloon?');
       TestWorld.Perform('examine balloon', TestPlayer);

       Proxy.ExpectString('Which of the balloons do you want to examine first, the large pink balloon, the large grey balloon, the large black balloon, the large white balloon, the large violet balloon, the large blue balloon, the large green balloon, the large yellow balloon, the large orange balloon, or the large red balloon?');
       TestWorld.Perform('examine balloons', TestPlayer);

       Proxy.ExpectString('Which balloon do you mean, the large pink balloon, the large grey balloon, the large black balloon, the large white balloon, the large violet balloon, the large blue balloon, the large green balloon, the large yellow balloon, the large orange balloon, or the large red balloon?');
       TestWorld.Perform('examine the balloon', TestPlayer);

       Proxy.ExpectSubstring('It is coloured pink.');
       TestWorld.Perform('examine the balloon that is pink', TestPlayer);

       Proxy.ExpectSubstring('It is coloured pink.');
       TestWorld.Perform('examine the pink that is a balloon', TestPlayer);

       Proxy.ExpectSubstring('Which balloon that is large do you mean, ');
       TestWorld.Perform('examine the balloon that is large', TestPlayer);

       Proxy.ExpectString('You used the term "that is" in a way I don''t understand.');
       TestWorld.Perform('examine the balloon that is pink that is pink', TestPlayer);

       Proxy.ExpectString('You used the term "that is" in a way I don''t understand.');
       TestWorld.Perform('examine balloons that is pink', TestPlayer);

       Proxy.ExpectSubstring(' balloon)');
       Proxy.ExpectSubstring('It is coloured');
       TestWorld.Perform('examine a balloon that is large', TestPlayer);

       Proxy.ExpectSubstring(' balloon)');
       Proxy.ExpectSubstring('It is coloured');
       TestWorld.Perform('examine any of the balloons', TestPlayer);

       Proxy.ExpectSubstring(' balloon)');
       Proxy.ExpectSubstring('It is coloured');
       TestWorld.Perform('examine one balloon', TestPlayer);

       Proxy.ExpectSubstring(' balloon)');
       Proxy.ExpectSubstring('It is coloured');
       TestWorld.Perform('examine some balloon', TestPlayer);

       Proxy.ExpectString('I don''t know how to examine multiple things at once.');
       TestWorld.Perform('examine some balloons', TestPlayer);

       Proxy.ExpectString('I don''t know how to examine multiple things at once.');
       TestWorld.Perform('examine all that is pink', TestPlayer);

       Proxy.ExpectString('I don''t know how to examine multiple things at once.');
       TestWorld.Perform('examine all that is balloon', TestPlayer);

       Proxy.ExpectString('I don''t know how to examine multiple things at once.');
       TestWorld.Perform('examine all that is pink', TestPlayer);

       Proxy.ExpectString('I don''t know how to examine multiple things at once.');
       TestWorld.Perform('examine pink and blue', TestPlayer);
       Proxy.ExpectDone();

       Proxy.Test('"that is" and "and that is" and so on');
       Proxy.ExpectString('(the large pink balloon)');
       Proxy.ExpectString('You shake the large pink balloon.');
       TestWorld.Perform('shake balloons that are large and that are pink and that are not blue', TestPlayer);

       Proxy.Test('"that is" and "and that is" and so on');
       Proxy.ExpectString('Large grey balloon: You shake the large grey balloon.');
       Proxy.ExpectString('Large black balloon: You shake the large black balloon.');
       TestWorld.Perform('shake balloons that are not purple and that are not red and that are not green and that are not blue and that are not pink and that are not orange and that are not yellow and that are not white', TestPlayer);

       { counting and parsing with numbers }
       Proxy.Test('Counting');
       Proxy.ExpectSubstring('(');
       Proxy.ExpectSubstring('balloon: Taken.');
       Proxy.ExpectSubstring('balloon: Taken.');
       Proxy.ExpectSubstring('balloon: Taken.');
       TestWorld.Perform('take three balloons', TestPlayer);

// disabled because there's a bug: 'drop any x' picks at all xes, not just those being held if there some being held
//         Proxy.ExpectSubstring('(');
//         Proxy.ExpectSubstring('pink balloon: Dropped.');
//         Proxy.ExpectSubstring('grey balloon: Dropped.');
//         TestWorld.Perform('drop any two balloons', TestPlayer);

       Proxy.ExpectString('About the two balloons... I count 10, not two.');
       TestWorld.Perform('drop the two balloons', TestPlayer);

       Proxy.SkipEverything();
       TestWorld.Perform('drop all balloons; look', TestPlayer);
       Proxy.StopSkipping();

       Proxy.ExpectString('I don''t understand how to choose the things that are all the particular "leaf one".');
       TestWorld.Perform('take piles that are the leaf one and the earth one', TestPlayer);

       Proxy.ExpectString('I don''t understand how to choose the things that are all the particular "earth one".');
       TestWorld.Perform('take piles that are leaf and that are the earth one', TestPlayer);

       Proxy.ExpectString('I don''t understand how to choose the things that are all a particular "earth one".');
       TestWorld.Perform('take piles that are an earth one', TestPlayer);

       Proxy.Test('"But"');
       Proxy.ExpectSubstring('Pile of leaves:');
       Proxy.ExpectSubstring('Pile of earth:');
       TestWorld.Perform('take all but balloons', TestPlayer);

       Proxy.ExpectString('You are carrying:');
       Proxy.ExpectString('  A bag of holding.');
       TestWorld.Perform('i', TestPlayer);

       Proxy.ExpectSubstring('blue');
       Proxy.ExpectSubstring('green');
       TestWorld.Perform('take all but piles and red and orange and yellow and balloon that is pink and violet and purple and white and black and gray', TestPlayer);

       Proxy.SkipEverything();
       TestWorld.Perform('drop all balloons; look', TestPlayer);
       Proxy.StopSkipping();

       Proxy.ExpectSubstring('white');
       Proxy.ExpectSubstring('violet');
       Proxy.ExpectSubstring('yellow');
       Proxy.ExpectSubstring('orange');
       Proxy.ExpectSubstring('red');
       Proxy.ExpectSubstring('black');
       TestWorld.Perform('take all balloons but pink ones, blue ones, the green one that is large, and the gray one', TestPlayer);

       Proxy.ExpectString('(the pile of earth)');
       Proxy.ExpectSubstring('slips');
       TestWorld.Perform('take all piles but the pile that is the pile of leaves', TestPlayer);

       Proxy.SkipEverything();
       TestWorld.Perform('drop all balloons', TestPlayer);
       Proxy.StopSkipping();

       Proxy.Test('"From"');
       Proxy.ExpectSubstring('Hole: ');
       Proxy.ExpectSubstring('Pile of earth: ');
       TestWorld.Perform('take all from ground but balloons', TestPlayer);

       Proxy.ExpectSubstring('(the pile of leaves)');
       Proxy.ExpectSubstring('slips');
       TestWorld.Perform('take all from hole but balloons', TestPlayer);

       Proxy.ExpectSubstring('Hole: ');
       Proxy.ExpectSubstring('Pile of earth: ');
       TestWorld.Perform('take all but balloons from ground', TestPlayer);

       Proxy.ExpectSubstring('(the pile of leaves)');
       Proxy.ExpectSubstring('slips');
       TestWorld.Perform('take all but balloons from hole', TestPlayer);

       Proxy.ExpectSubstring('Hole: ');
       Proxy.ExpectSubstring('Pile of earth:');
       Proxy.ExpectSubstring('Pile of leaves:');
       TestWorld.Perform('take all but balloons from ground and all but earth from hole', TestPlayer);

       Proxy.ExpectDone();

       { overfill test }
       Proxy.Test('Overfilling');
       Proxy.ExpectString('(the pile of leaves)');
       Proxy.ExpectString('Moved onto the ground.');
       Proxy.ExpectString('');
       Proxy.ExpectNoSubstring('overflowing');
       Proxy.WaitUntilString('  A spade.');
       Proxy.WaitUntilString('');
       Proxy.ExpectString('The hole is in the ground.');
       Proxy.ExpectString('');
       Proxy.ExpectSubstring('Moved');
       Proxy.ExpectSubstring('falls');
       Proxy.ExpectSubstring('Moved');
       Proxy.ExpectSubstring('falls');
       Proxy.WaitUntilString(''); { finish moving balloons }
       Proxy.ExpectSubstring('overflowing');
       Proxy.AndAlso();
       Proxy.ExpectNoSubstring('spade');
       Proxy.AndAlso();
       Proxy.ExpectSubstring('balloon');
       Proxy.WaitUntilString('  A spade.');
       Proxy.WaitUntilString('');
       Proxy.ExpectSubstring('Moved');
       Proxy.ExpectString('');
       Proxy.ExpectString('There is a hole under the pile of earth.');
       Proxy.ExpectString('');
       Proxy.ExpectString('(off the hole)');
       Proxy.ExpectString('Moved onto the ground.');
       Proxy.ExpectString('');
       Proxy.ExpectString('(out of the hole)');
       Proxy.ExpectString('Moved onto the ground.');
       Proxy.ExpectString('');
       Proxy.ExpectString('Taken.');
       Proxy.ExpectString('');
       Proxy.ExpectString('Moved onto the hole.');
       Proxy.ExpectString('You fill the hole with the pile of earth.');
       Proxy.ExpectString('');
       Proxy.ExpectString('I can''t find anything like a "hole" here.');
       TestWorld.Perform('move leaves onto ground; x hole; find hole; push balloons on hole; x hole; move soil on hole; find hole; move soil off; move pink out; get spade; move soil onto hole; find hole', TestPlayer);
       Proxy.ExpectDone();

       { and/then/etc tests }
       Proxy.Test('Continuation and Joins');
       Proxy.ExpectString('You say hello.');
       TestWorld.Perform('say hello', TestPlayer);

       Proxy.ExpectString('You say a.');
       Proxy.ExpectString('');
       Proxy.ExpectString('You say b.');
       TestWorld.Perform('say a and say b', TestPlayer);

       Proxy.ExpectString('You say a.');
       Proxy.ExpectString('');
       Proxy.ExpectString('You say b.');
       TestWorld.Perform('say a and, then, say b', TestPlayer);

       Proxy.ExpectString('You say a.');
       Proxy.ExpectString('');
       Proxy.ExpectString('You say b.');
       TestWorld.Perform('say a; then say b', TestPlayer);

       Proxy.ExpectString('You say a.');
       Proxy.ExpectString('');
       Proxy.ExpectString('You say b.');
       TestWorld.Perform('say a; say b', TestPlayer);

       Proxy.ExpectString('You say a.');
       Proxy.ExpectString('');
       Proxy.ExpectString('You say b.');
       TestWorld.Perform('say a then, say b', TestPlayer);

       Proxy.ExpectString('You say a.');
       Proxy.ExpectString('');
       Proxy.ExpectString('Then what?');
       TestWorld.Perform('say a then say b then', TestPlayer);

       Proxy.ExpectString('You say a.');
       Proxy.ExpectString('');
       Proxy.ExpectString('And what?');
       TestWorld.Perform('say a and say b and', TestPlayer);

       Proxy.SkipEverything();
       TestWorld.Perform('inventory; look', TestPlayer);
       Proxy.StopSkipping();

       Proxy.ExpectString('Pile of leaves: The pile of leaves slips through your fingers.');
       Proxy.ExpectString('Large pink balloon: Taken.');
       Proxy.ExpectString('');
       Proxy.ExpectString('(the ground with the spade)');
       Proxy.ExpectString('With much effort, you dig a huge hole.');
       Proxy.ExpectString('');
       Proxy.ExpectString('Bag of holding: Dropped in the hole.');
       Proxy.ExpectString('Spade: Dropped in the hole.');
       Proxy.ExpectString('Large pink balloon: Dropped in the hole.');
       TestWorld.Perform('take all and dig and drop all in hole in ground', TestPlayer);

       Proxy.ExpectString('And what?');
       TestWorld.Perform('take bag and spade and', TestPlayer);

       Proxy.ExpectString('And what?');
       TestWorld.Perform('take all and', TestPlayer);

       Proxy.ExpectString('Then what?');
       TestWorld.Perform('take all and then', TestPlayer);

       Proxy.ExpectString('Taken.');
       Proxy.ExpectString('');
       Proxy.ExpectString('I don''t understand how to "and".');
       TestWorld.Perform('take spade and and', TestPlayer);

       Proxy.ExpectString('I don''t understand your use of commas.');
       TestWorld.Perform('take bag, spade, ', TestPlayer);
       Proxy.ExpectDone();

       { more parsing tests }
       Proxy.Test('More thingseeker tests');
       Proxy.SkipEverything();
       TestWorld.Perform('north', TestPlayer);
       Proxy.StopSkipping();

       Proxy.ExpectString('Which utensil that is not a fork from a table that is not plastic and that is not a desk do you mean, the plastic knife or the plastic spoon?');
       TestWorld.Perform('shake the utensil that is not a fork from a table that is not plastic and that is not a desk', TestPlayer);

       Proxy.ExpectString('Wooden spoon: You shake the wooden spoon.');
       Proxy.ExpectString('Plastic spoon: You shake the plastic spoon.');
       Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
       Proxy.ExpectString('Silver spoon: You shake the silver spoon.');
       TestWorld.Perform('shake utensils that are some spoons', TestPlayer);

       Proxy.ExpectString('I don''t understand how to choose the things that are all a particular "spoon".');
       TestWorld.Perform('shake utensils that are some spoon', TestPlayer);

       Proxy.ExpectString('You used the term "that are" in a way I don''t understand.');
       TestWorld.Perform('shake utensil that are some spoon', TestPlayer);

       Proxy.ExpectString('You used the term "that are" in a way I don''t understand.');
       TestWorld.Perform('shake utensil that are some spoons', TestPlayer);

       Proxy.ExpectString('I was with you up to "spoon".');
       TestWorld.Perform('shake utensils that are not some spoon', TestPlayer);

       Proxy.ExpectString('I was with you up to "spoon".');
       TestWorld.Perform('shake utensils that are not a spoon', TestPlayer);

       Proxy.ExpectString('(the wooden spoon and the plastic spoon)');
       Proxy.ExpectString('Wooden spoon: You shake the wooden spoon.');
       Proxy.ExpectString('Plastic spoon: You shake the plastic spoon.');
       TestWorld.Perform('shake some spoons that are not the three silver utensils', TestPlayer);

       Proxy.ExpectString('I was with you up to "three silver utensils".');
       TestWorld.Perform('shake some spoons that are not three silver utensils', TestPlayer);

       Proxy.ExpectString('(the wooden spoon and the plastic spoon)');
       Proxy.ExpectString('Wooden spoon: You shake the wooden spoon.');
       Proxy.ExpectString('Plastic spoon: You shake the plastic spoon.');
       TestWorld.Perform('shake some spoons that are not all three silver utensils', TestPlayer);

       Proxy.ExpectString('About the three wooden utensils... I can only find one: the wooden spoon.');
       TestWorld.Perform('shake some spoons that are not the three wooden utensils', TestPlayer);

       Proxy.ExpectString('About the three wooden utensils... I can only find one: the wooden spoon.');
       TestWorld.Perform('shake some spoons that are not all three wooden utensils', TestPlayer);

       Proxy.ExpectString('I was with you up to "three wooden utensils".');
       TestWorld.Perform('shake some spoons that are not three wooden utensils', TestPlayer);

       Proxy.ExpectString('I was with you up to "eight wooden utensils".');
       TestWorld.Perform('shake all spoons that are not 8 wooden utensils', TestPlayer);

       Proxy.ExpectString('You used the term "that are not" in a way I don''t understand.');
       TestWorld.Perform('shake some spoon that are not the three silver utensils', TestPlayer);

       Proxy.ExpectString('You used the term "that are" and the number three in ways I really couldn''t make sense of.');
       TestWorld.Perform('shake some spoons that are the three silver utensils', TestPlayer);

       Proxy.ExpectString('You used the term "that are" and the number two in ways I really couldn''t make sense of.');
       TestWorld.Perform('shake the spoons and forks that are the two silver utensils and the two wooden utensils', TestPlayer);

       Proxy.ExpectString('About the two silver utensils... I count three, not two.');
       TestWorld.Perform('shake some spoons that are not the two silver utensils', TestPlayer);

       Proxy.ExpectString('You used the term "that are" and the number three in ways I really couldn''t make sense of.');
       TestWorld.Perform('shake spoons that are three silver utensils', TestPlayer);

       Proxy.ExpectString('You used the term "that are" and the number two in ways I really couldn''t make sense of.');
       TestWorld.Perform('shake spoons that are two silver utensils', TestPlayer);

       Proxy.ExpectString('I was with you up to "two silver utensils".');
       TestWorld.Perform('shake some spoons that are not two silver utensils', TestPlayer);

       Proxy.ExpectString('I was with you up to "two silver utensils".');
       TestWorld.Perform('shake all spoons that are not two silver utensils', TestPlayer);

       Proxy.ExpectString('About the two silver utensils... I count three, not two.');
       TestWorld.Perform('shake some spoons that are not all two silver utensils', TestPlayer);

       Proxy.ExpectString('(the plastic knife and the plastic spoon)');
       Proxy.ExpectString('Plastic knife: You shake the plastic knife.');
       Proxy.ExpectString('Plastic spoon: You shake the plastic spoon.');
       TestWorld.Perform('shake all utensils that are not forks from a table that is not plastic and that is not a desk', TestPlayer);

       Proxy.ExpectString('Stainless steel knife: You shake the stainless steel knife.');
       Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
       TestWorld.Perform('shake all of the knives but fork from desk and all of the steel but fork from desk', TestPlayer);

       Proxy.ExpectString('Stainless steel knife: You shake the stainless steel knife.');
       Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
       TestWorld.Perform('shake all of the knives from desk and all of the steel but fork from desk', TestPlayer);

       Proxy.ExpectString('Stainless steel knife: You shake the stainless steel knife.');
       Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
       TestWorld.Perform('shake all of the knives but fork and all of the steel but fork from desk', TestPlayer);

       Proxy.ExpectString('Stainless steel knife: You shake the stainless steel knife.');
       Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
       TestWorld.Perform('shake all of the knives and all of the steel but fork from desk', TestPlayer);

       Proxy.ExpectString('Plastic knife: You shake the plastic knife.');
       Proxy.ExpectString('Stainless steel knife: You shake the stainless steel knife.');
       Proxy.ExpectString('Silver knife: You shake the silver knife.');
       TestWorld.Perform('shake all utensils but some spoons and some forks', TestPlayer);

       Proxy.ExpectString('(the stainless steel knife, the stainless steel fork, and the stainless steel spoon)');
       Proxy.ExpectString('Stainless steel knife: You shake the stainless steel knife.');
       Proxy.ExpectString('Stainless steel fork: You shake the stainless steel fork.');
       Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
       TestWorld.Perform('shake all from one of the tables THAT IS NOT the kitchen table', TestPlayer);

       Proxy.ExpectString('Stainless steel knife: You shake the stainless steel knife.');
       Proxy.ExpectString('Stainless steel fork: You shake the stainless steel fork.');
       Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
       Proxy.ExpectString('Silver knife: You shake the silver knife.');
       Proxy.ExpectString('Silver fork: You shake the silver fork.');
       Proxy.ExpectString('Silver spoon: You shake the silver spoon.');
       TestWorld.Perform('shake all from all of the tables THAT ARE NOT the kitchen table', TestPlayer);

       Proxy.ExpectString('Plastic fork: Taken.');
       Proxy.ExpectString('Silver fork: Taken.');
       TestWorld.Perform('take forks from the table that is kitchen and from the table that is dining', TestPlayer);

       Proxy.ExpectString('Wooden spoon: You shake the wooden spoon.');
       Proxy.ExpectString('Plastic spoon: You shake the plastic spoon.');
       Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
       Proxy.ExpectString('Silver spoon: You shake the silver spoon.');
       Proxy.ExpectString('Stainless steel fork: You shake the stainless steel fork.');
       TestWorld.Perform('shake spoons and all forks from desk', TestPlayer);

       Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
       Proxy.ExpectString('Stainless steel fork: You shake the stainless steel fork.');
       TestWorld.Perform('shake all spoons and all forks from desk', TestPlayer);

       Proxy.ExpectString('Foot of Cliff Face');
       Proxy.WaitUntilString('');
       Proxy.ExpectString('Taken.');
       Proxy.ExpectString('');
       Proxy.ExpectString('Cave one');
       Proxy.WaitUntilString('');
       Proxy.ExpectString('You say "bring it on!".');
       TestWorld.Perform('south; take bag; north; say "bring it on!"', TestPlayer);
       Proxy.ExpectDone();

       Proxy.ExpectString('Placed in the brown sack.');
       Proxy.ExpectString('');
       Proxy.ExpectString('Placed in the bag of holding.');
       Proxy.ExpectString('');
       Proxy.ExpectString('(first taking the brown sack)');
       Proxy.ExpectString('Taken.');
       Proxy.ExpectString('Placed on the desk.');
       TestWorld.Perform('put plastic fork in sack and put silver fork in bag of holding; then put sack on desk', TestPlayer);

       Proxy.ExpectString('Taken.');
       TestWorld.Perform('take fork from sack from table', TestPlayer);

       Proxy.ExpectString('Placed in the brown sack.');
       TestWorld.Perform('put plastic fork in sack on table', TestPlayer);

       Proxy.ExpectString('You shake the plastic fork.');
       TestWorld.Perform('shake fork from bag from table', TestPlayer);

       Proxy.ExpectString('You shake the plastic fork.');
       TestWorld.Perform('shake fork from bag on table', TestPlayer);

       Proxy.ExpectString('You shake the plastic fork.');
       TestWorld.Perform('shake fork in bag from table', TestPlayer);

       Proxy.ExpectString('You shake the plastic fork.');
       TestWorld.Perform('shake fork in bag on table', TestPlayer);

       Proxy.ExpectString('The stainless steel fork is on the non-descript desk.');
       TestWorld.Perform('find steel fork', TestPlayer);

       Proxy.ExpectString('The silver fork is in the embroidered bag of holding labeled Tester.');
       TestWorld.Perform('find silver fork', TestPlayer);

       Proxy.ExpectString('The plastic fork is in the elongated brown sack.');
       TestWorld.Perform('find plastic fork', TestPlayer);

       Proxy.ExpectDone();

       Proxy.SkipEverything();
       TestWorld.Perform('drop all then look then north', TestPlayer);
       Proxy.StopSkipping();

       Proxy.ExpectString('(the plastic knife)');
       Proxy.ExpectString('You shake the plastic knife.');
       TestWorld.Perform('shake all knives but silver and stainless steel', TestPlayer);

       Proxy.ExpectString('(the plastic knife)');
       Proxy.ExpectString('You shake the plastic knife.');
       TestWorld.Perform('shake all knives but all silver and stainless steel', TestPlayer);

       Proxy.ExpectString('(the plastic knife)');
       Proxy.ExpectString('You shake the plastic knife.');
       TestWorld.Perform('shake all knives but all silver and all stainless steel', TestPlayer);

       Proxy.ExpectString('Plastic knife: You shake the plastic knife.');
       Proxy.ExpectString('Stainless steel knife: You shake the stainless steel knife.');
       Proxy.ExpectString('Stainless steel fork: You shake the stainless steel fork.');
       Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
       TestWorld.Perform('shake all knives but silver and all stainless steel', TestPlayer);

       Proxy.ExpectString('Plastic knife: You shake the plastic knife.');
       Proxy.ExpectString('Stainless steel knife: You shake the stainless steel knife.');
       Proxy.ExpectString('Silver knife: You shake the silver knife.');
       Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
       TestWorld.Perform('shake knives and all steel from desk but fork', TestPlayer);

       Proxy.ExpectString('Plastic knife: You shake the plastic knife.');
       Proxy.ExpectString('Stainless steel knife: You shake the stainless steel knife.');
       Proxy.ExpectString('Silver knife: You shake the silver knife.');
       Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
       TestWorld.Perform('shake knives and all steel but fork from desk', TestPlayer);

       Proxy.ExpectString('Stainless steel knife: You shake the stainless steel knife.');
       Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
       TestWorld.Perform('shake all knives but wooden plus all steel but fork from desk', TestPlayer);

       Proxy.ExpectString('Stainless steel knife: Taken.');
       Proxy.ExpectString('Stainless steel spoon: Taken.');
       TestWorld.Perform('take all knives and all steel but fork from desk', TestPlayer);

       Proxy.ExpectString('(the plastic knife, the plastic fork, and the plastic spoon)');
       Proxy.ExpectString('Plastic knife: Taken.');
       Proxy.ExpectString('Plastic fork: Taken.');
       Proxy.ExpectString('Plastic spoon: Taken.');
       TestWorld.Perform('take all from one of the tables', TestPlayer);

       Proxy.ExpectDone();

       Proxy.SkipEverything();
       TestWorld.Perform('drop all then look then north', TestPlayer);
       Proxy.StopSkipping();

       Proxy.ExpectString('I was with you until you said "that is all", but then I got confused.');
       TestWorld.Perform('shake the utensil that is all but fork and knife', TestPlayer);

       Proxy.ExpectString('I was with you until you said "that is all", but then I got confused.');
       TestWorld.Perform('shake the utensil that is all but some fork and a knife', TestPlayer);

       Proxy.ExpectString('I was with you until you said "that is all", but then I got confused.');
       TestWorld.Perform('shake the utensil that is all but some fork and two knives', TestPlayer);

       Proxy.ExpectString('Wooden spoon: You shake the wooden spoon.');
       Proxy.ExpectString('Plastic spoon: You shake the plastic spoon.');
       Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
       Proxy.ExpectString('Silver spoon: You shake the silver spoon.');
       TestWorld.Perform('shake the utensils that are all but fork and knife', TestPlayer);

       Proxy.ExpectString('(the wooden spoon, the plastic spoon, the stainless steel knife, the stainless steel fork, the stainless steel spoon, the silver knife, the silver fork, and the silver spoon)');
       Proxy.ExpectString('Wooden spoon: You shake the wooden spoon.');
       Proxy.ExpectString('Plastic spoon: You shake the plastic spoon.');
       Proxy.ExpectString('Stainless steel knife: You shake the stainless steel knife.');
       Proxy.ExpectString('Stainless steel fork: You shake the stainless steel fork.');
       Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
       Proxy.ExpectString('Silver knife: You shake the silver knife.');
       Proxy.ExpectString('Silver fork: You shake the silver fork.');
       Proxy.ExpectString('Silver spoon: You shake the silver spoon.');
       TestWorld.Perform('shake the utensils that are all but some fork and a knife', TestPlayer);

       Proxy.ExpectString('(the wooden spoon, the plastic spoon, the stainless steel fork, the stainless steel spoon, the silver knife, the silver fork, and the silver spoon)');
       Proxy.ExpectString('Wooden spoon: You shake the wooden spoon.');
       Proxy.ExpectString('Plastic spoon: You shake the plastic spoon.');
       Proxy.ExpectString('Stainless steel fork: You shake the stainless steel fork.');
       Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
       Proxy.ExpectString('Silver knife: You shake the silver knife.');
       Proxy.ExpectString('Silver fork: You shake the silver fork.');
       Proxy.ExpectString('Silver spoon: You shake the silver spoon.');
       TestWorld.Perform('shake the utensils that are all but some fork and two knives', TestPlayer);

       Proxy.ExpectString('(the plastic knife, the plastic fork, the plastic spoon, and the stainless steel knife)');
       Proxy.ExpectString('Plastic knife: Taken.');
       Proxy.ExpectString('Plastic fork: Taken.');
       Proxy.ExpectString('Plastic spoon: Taken.');
       Proxy.ExpectString('Stainless steel knife: Taken.');
       TestWorld.Perform('take four utensils from two of the tables', TestPlayer);

       Proxy.ExpectString('About the four forks from two tables... I can only find two: the stainless steel fork and the silver fork.');
       TestWorld.Perform('take four forks from two of the tables', TestPlayer);

       Proxy.ExpectSubstring('Clove of garlic: ');
       Proxy.ExpectSubstring('Lunch: ');
       Proxy.ExpectSubstring('Wooden spoon: ');
       TestWorld.Perform('shake all from all but table', TestPlayer);

       Proxy.ExpectDone();

       Proxy.SkipEverything();
       TestWorld.Perform('drop all then look', TestPlayer);
       Proxy.StopSkipping();

       Proxy.ExpectString('Taken.');
       Proxy.ExpectString('');
       Proxy.ExpectSubstring('You are carrying:');
       Proxy.ExpectString('  A silver spoon.');
       TestWorld.Perform('take the spoon from the table that is dining then inventory', TestPlayer);

       Proxy.ExpectSubstring('(the silver knife and the stainless steel fork)');
       Proxy.ExpectSubstring('shake');
       Proxy.ExpectSubstring('shake');
       TestWorld.Perform('shake some utensil from the dining room table and some utensil from the desk', TestPlayer);

       Proxy.ExpectDone();

       Proxy.SkipEverything();
       TestWorld.Perform('drop all then look then north', TestPlayer);
       Proxy.StopSkipping();

       Proxy.ExpectSubstring('Silver table: ');
       Proxy.ExpectSubstring('Silver knife: ');
       Proxy.ExpectSubstring('Silver spoon: ');
       TestWorld.Perform('shake all that is silver but fork', TestPlayer);

       Proxy.ExpectSubstring('Wooden spoon: ');
       Proxy.ExpectSubstring('Plastic knife: ');
       Proxy.ExpectSubstring('Plastic spoon: ');
       Proxy.ExpectSubstring('Stainless steel knife: ');
       Proxy.ExpectSubstring('Stainless steel spoon: ');
       Proxy.ExpectSubstring('Silver knife: ');
       Proxy.ExpectSubstring('Silver spoon: ');
       TestWorld.Perform('shake all the utensils that are not the three forks', TestPlayer);

       Proxy.ExpectDone();

       Proxy.SkipEverything();
       TestWorld.Perform('put silver fork on plastic table', TestPlayer);
       TestWorld.Perform('put plastic fork on steel table', TestPlayer);
       TestWorld.Perform('put steel fork on silver table', TestPlayer);
       Proxy.StopSkipping();

       Proxy.ExpectSubstring('Silver fork: ');
       Proxy.ExpectSubstring('Plastic fork: ');
       TestWorld.Perform('shake all forks from all tables but silver', TestPlayer);

       Proxy.ExpectSubstring('Plastic fork: ');
       Proxy.ExpectSubstring('Stainless steel fork: ');
       TestWorld.Perform('shake all forks from tables but silver', TestPlayer);

       Proxy.ExpectSubstring('Silver fork: ');
       Proxy.ExpectSubstring('Plastic fork: ');
       TestWorld.Perform('shake forks from all tables but silver', TestPlayer);

       Proxy.ExpectSubstring('You used the term "but" in a way I don''t understand.');
       TestWorld.Perform('shake forks from tables but silver', TestPlayer);

       Proxy.ExpectDone();

       Proxy.SkipEverything();
       TestWorld.Perform('put spoons on plastic table', TestPlayer);
       Proxy.StopSkipping();

       Proxy.ExpectString('Wooden spoon: You shake the wooden spoon.');
       Proxy.ExpectString('Plastic spoon: You shake the plastic spoon.');
       Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
       Proxy.ExpectString('Silver spoon: You shake the silver spoon.');
       TestWorld.Perform('shake spoons from silver table and from plastic table', TestPlayer);

       Proxy.ExpectDone();

       Proxy.SkipEverything();
       TestWorld.Perform('take all the spoons; push everything but the spoons south; drop the spoons; s; s; s; take a spade; n; n; n; l', TestPlayer);
       Proxy.StopSkipping();

       Proxy.ExpectString('With much effort, you dig a huge hole.');
       TestWorld.Perform('dig the ground with the spade that is metal', TestPlayer);

       Proxy.ExpectString('Which earth do you want to find first, the ground or the pile of earth?');
       TestWorld.Perform('find earth', TestPlayer);

       Proxy.ExpectString('Wooden spoon: Moved into the hole.');
       Proxy.ExpectString('Plastic spoon: Moved into the hole.');
       Proxy.ExpectString('Stainless steel spoon: Moved into the hole.');
       Proxy.ExpectString('Silver spoon: Moved into the hole.');
       Proxy.ExpectString('');
       Proxy.ExpectString('Moved into the hole.');
       Proxy.ExpectString('You fill the hole with the pile of earth.');
       TestWorld.Perform('push all the spoons into the hole, then push the pile of earth into the hole', TestPlayer);

       Proxy.ExpectString('With much effort, you dig a huge hole.');
       TestWorld.Perform('dig the ground with the spade that is metal', TestPlayer);

       Proxy.ExpectString('I don''t see anything to take here.');
       TestWorld.Perform('take all but pile', TestPlayer);

       Proxy.ExpectString('Wooden spoon: You shake the wooden spoon.');
       Proxy.ExpectString('Plastic spoon: You shake the plastic spoon.');
       Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
       Proxy.ExpectString('Silver spoon: You shake the silver spoon.');
       TestWorld.Perform('shake all spoons', TestPlayer);

       Proxy.ExpectString('Wooden spoon: You shake the wooden spoon.');
       Proxy.ExpectString('Plastic spoon: You shake the plastic spoon.');
       Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
       Proxy.ExpectString('Silver spoon: You shake the silver spoon.');
       TestWorld.Perform('shake all from pile', TestPlayer);

       Proxy.ExpectString('Wooden spoon: You shake the wooden spoon.');
       Proxy.ExpectString('Plastic spoon: You shake the plastic spoon.');
       Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
       Proxy.ExpectString('Silver spoon: You shake the silver spoon.');
       TestWorld.Perform('shake all from all that is soil', TestPlayer);

       Proxy.ExpectString('Hole: You cannot shake a hole. That is just silly.');
       Proxy.ExpectString('Pile of earth: The pile of earth slips through your fingers.');
       TestWorld.Perform('shake all from all that is earth but pile', TestPlayer);

       Proxy.ExpectString('(the wooden spoon)');
       Proxy.ExpectString('(on the ground)');
       Proxy.ExpectString('(first taking the wooden spoon)');
       Proxy.ExpectString('Taken.');
       Proxy.ExpectString('Placed.');
       TestWorld.Perform('put a spoon from pile of earth', TestPlayer);

       Proxy.ExpectString('(the wooden spoon)');
       Proxy.ExpectString('You shake the wooden spoon.');
       TestWorld.Perform('shake a spoon on ground', TestPlayer);

       Proxy.ExpectString('It''s not clear to what you are referring.');
       TestWorld.Perform('shake a spoon in ground', TestPlayer);

       Proxy.ExpectString('(the plastic spoon)');
       Proxy.ExpectString('You shake the plastic spoon.');
       TestWorld.Perform('shake a spoon in pile', TestPlayer);

       Proxy.ExpectString('It''s not clear to what you are referring.');
       TestWorld.Perform('shake a spoon on pile', TestPlayer);

       Proxy.ExpectString('It''s not clear to what you are referring.');
       TestWorld.Perform('shake a spoon on ground in pile', TestPlayer);

       Proxy.ExpectString('(the plastic spoon)');
       Proxy.ExpectString('You shake the plastic spoon.');
       TestWorld.Perform('shake a spoon in pile on ground', TestPlayer);

       Proxy.ExpectString('You are carrying:');
       Proxy.ExpectString('  A spade.');
       Proxy.ExpectString('');
       Proxy.ExpectString('Cave four');
       Proxy.ExpectString('The cave is brightly lit from an entrace to a white room to the west. There is also some dim light coming from the south. There are cave paintings here. There is a hole here.');
       Proxy.ExpectString('There is a pile of earth here.');
       Proxy.ExpectString('There is a wooden spoon here.');
       Proxy.ExpectString('');
       Proxy.ExpectString('The hole is quite dirty.');
       Proxy.ExpectString('The hole is empty.');
       Proxy.ExpectString('');
       Proxy.ExpectString('The pile of earth is quite dirty.');
       Proxy.ExpectString('A thorough search through the pile of earth reveals:');
       Proxy.ExpectString('  A plastic spoon.');
       Proxy.ExpectString('  A stainless steel spoon.');
       Proxy.ExpectString('  A silver spoon.');
       TestWorld.Perform('inventory; look; look in hole; look in pile', TestPlayer);

       Proxy.ExpectString('Hole in the ground');
       Proxy.ExpectString('The hole is quite dirty.');
       Proxy.ExpectString('');
       Proxy.ExpectString('You see nothing noteworthy when looking up.');
       Proxy.ExpectString('');
       Proxy.ExpectString('You are in the hole.');
       Proxy.ExpectString('');
       Proxy.ExpectString('Cave four');
       Proxy.ExpectString('The cave is brightly lit from an entrace to a white room to the west. There is also some dim light coming from the south. There are cave paintings here. There is a hole here.');
       Proxy.ExpectString('There is a pile of earth here.');
       Proxy.ExpectString('There is a wooden spoon here.');
       TestWorld.Perform('enter hole; look up; look down; exit up', TestPlayer);

       Proxy.ExpectString('Plastic spoon: You shake the plastic spoon.');
       Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
       Proxy.ExpectString('Silver spoon: You shake the silver spoon.');
       Proxy.ExpectString('Wooden spoon: You shake the wooden spoon.');
       TestWorld.Perform('shake spoons in pile and on ground', TestPlayer);

       Proxy.ExpectString('I don''t see any "bag".');
       TestWorld.Perform('shake a spoon in ground and on pile and from bag', TestPlayer);

       Proxy.ExpectString('I don''t see any "bag".');
       TestWorld.Perform('shake a spoon on ground and in pile and from bag', TestPlayer);

       Proxy.ExpectString('It''s not clear to what you are referring.');
       TestWorld.Perform('shake a spoon in ground and on pile and from the cave paintings', TestPlayer);

       Proxy.ExpectString('(the plastic spoon)');
       Proxy.ExpectString('You shake the plastic spoon.');
       TestWorld.Perform('shake a spoon on ground and in pile and from the cave paintings', TestPlayer);

       Proxy.ExpectString('Which spoon on ground and in pile and from the cave paintings do you mean, the plastic spoon, the stainless steel spoon, the silver spoon, or the wooden spoon?');
       TestWorld.Perform('shake the spoon on ground and in pile and from the cave paintings', TestPlayer);

       Proxy.ExpectString('Which spoon in pile and on ground and from the cave paintings do you mean, the plastic spoon, the stainless steel spoon, the silver spoon, or the wooden spoon?');
       TestWorld.Perform('shake the spoon in pile and on ground and from the cave paintings', TestPlayer);

       Proxy.ExpectString('About the two spoons but the stainless steel spoon... I count three, not two.');
       TestWorld.Perform('shake all two spoons but the stainless steel spoon', TestPlayer);

       Proxy.ExpectString('(the plastic spoon)');
       Proxy.ExpectString('You shake the plastic spoon.');
       TestWorld.Perform('shake plastic spoon in pile and from a spoon', TestPlayer);

       Proxy.ExpectString('Moved into the hole.'); // XXX
       Proxy.ExpectString('You fill the hole with the pile of earth.');
       TestWorld.Perform('push the pile of earth into the hole', TestPlayer);

       Proxy.ExpectString('(the ground)');
       Proxy.ExpectString('With much effort, you dig a huge hole.');
       TestWorld.Perform('dig with the spade that is metal', TestPlayer);
       Proxy.ExpectDone();

       Proxy.SkipEverything();
       TestWorld.Perform('drop all', TestPlayer);
       Proxy.StopSkipping();

       Proxy.ExpectString('Flower room');
       Proxy.ExpectString('The room has bright ambient lighting for no apparent reason. It is a bright white room, almost clinical in nature, but it unexpectedly conveys a sense of floweriness. An exit to the east appears to lead to a dimly lit cave, while another exit leads south. A third goes up, ascending towards the heavens.');
       Proxy.SkipEverything();
       TestWorld.Perform('west', TestPlayer);
       Proxy.StopSkipping();

       Proxy.ExpectString('Which table do you mean, the red table or the blue table?');
       TestWorld.Perform('shake the vase on table', TestPlayer);

       Proxy.ExpectString('Which table do you mean, the red table or the blue table?');
       TestWorld.Perform('shake vase on the table', TestPlayer);

       Proxy.ExpectString('Which table do you mean, the red table or the blue table?');
       TestWorld.Perform('shake the vase on the table', TestPlayer);

       Proxy.ExpectString('Which table do you mean, the red table or the blue table?');
       TestWorld.Perform('shake vase on table', TestPlayer);

       Proxy.ExpectString('Which table do you mean, the red table or the blue table?');
       TestWorld.Perform('shake the vases on table', TestPlayer);

       Proxy.ExpectString('Which table do you mean, the red table or the blue table?');
       TestWorld.Perform('shake vases on the table', TestPlayer);

       Proxy.ExpectString('Which table do you mean, the red table or the blue table?');
       TestWorld.Perform('shake the vases on the table', TestPlayer);

       Proxy.ExpectString('Which table do you mean, the red table or the blue table?');
       TestWorld.Perform('shake vases on table', TestPlayer);

       Proxy.ExpectString('Which vase on tables do you mean, the red vase or the blue vase?');
       TestWorld.Perform('shake the vase on tables', TestPlayer);

       Proxy.ExpectString('Which vase on the tables do you mean, the red vase or the blue vase?');
       TestWorld.Perform('shake vase on the tables', TestPlayer);

       Proxy.ExpectString('Which vase on the tables do you mean, the red vase or the blue vase?');
       TestWorld.Perform('shake the vase on the tables', TestPlayer);

       Proxy.ExpectString('Which vase on tables do you mean, the red vase or the blue vase?');
       TestWorld.Perform('shake vase on tables', TestPlayer);

       Proxy.ExpectString('Red vase: You shake the red vase.');
       Proxy.ExpectString('Blue vase: You shake the blue vase.');
       TestWorld.Perform('shake the vases on tables', TestPlayer);

       Proxy.ExpectString('Red vase: You shake the red vase.');
       Proxy.ExpectString('Blue vase: You shake the blue vase.');
       TestWorld.Perform('shake vases on the tables', TestPlayer);

       Proxy.ExpectString('Red vase: You shake the red vase.');
       Proxy.ExpectString('Blue vase: You shake the blue vase.');
       TestWorld.Perform('shake the vases on the tables', TestPlayer);

       Proxy.ExpectString('Red vase: You shake the red vase.');
       Proxy.ExpectString('Blue vase: You shake the blue vase.');
       TestWorld.Perform('shake vases on tables', TestPlayer);

       Proxy.ExpectString('You shake the red vase.');
       TestWorld.Perform('shake the red vase on table', TestPlayer);

       Proxy.ExpectString('Which table do you mean, the red table or the blue table?');
       TestWorld.Perform('shake red vase on the table', TestPlayer);

       Proxy.ExpectString('Which table do you mean, the red table or the blue table?');
       TestWorld.Perform('shake the red vase on the table', TestPlayer);

       Proxy.ExpectString('You shake the red vase.');
       TestWorld.Perform('shake red vase on table', TestPlayer);

       Proxy.ExpectString('You shake the red vase.');
       TestWorld.Perform('shake the red vase on tables', TestPlayer);

       Proxy.ExpectString('You shake the red vase.');
       TestWorld.Perform('shake red vase on the tables', TestPlayer);

       Proxy.ExpectString('You shake the red vase.');
       TestWorld.Perform('shake the red vase on the tables', TestPlayer);

       Proxy.ExpectString('You shake the red vase.');
       TestWorld.Perform('shake red vase on tables', TestPlayer);

       Proxy.ExpectString('(the red vase)');
       Proxy.ExpectString('You shake the red vase.');
       TestWorld.Perform('shake vases that are red', TestPlayer);

       Proxy.ExpectString('(the red vase)');
       Proxy.ExpectString('You shake the red vase.');
       TestWorld.Perform('shake all that are red and that are vases', TestPlayer);

       Proxy.ExpectString('(the blue vase)');
       Proxy.ExpectString('You shake the blue vase.');
       TestWorld.Perform('shake all from any of the tables THAT ARE NOT the red ones', TestPlayer);

       Proxy.ExpectString('(first taking the blue vase)');
       Proxy.ExpectString('Taken.');
       Proxy.ExpectString('Placed on the red table.');
       TestWorld.Perform('put blue vase on red table', TestPlayer);

       Proxy.ExpectString('You shake the blue vase.');
       TestWorld.Perform('shake blue from red THAT IS NOT vase', TestPlayer);
       Proxy.ExpectDone();

       Proxy.SkipEverything();
       TestWorld.Perform('south', TestPlayer);
       Proxy.StopSkipping();

       Proxy.ExpectString('Red grapes: You shake the red grapes.');
       Proxy.ExpectString('Green grapes: You shake the green grapes.');
       Proxy.ExpectString('Rotten grapes: You shake the rotten grapes.');
       Proxy.ExpectString('Red apple: You shake the red apple.');
       Proxy.ExpectString('Yellow apple: You shake the yellow apple.');
       TestWorld.Perform('shake grapes and all apples THAT ARE NOT green from plate on table', TestPlayer);

       Proxy.ExpectString('Red grapes: You shake the red grapes.');
       Proxy.ExpectString('Green grapes: You shake the green grapes.');
       Proxy.ExpectString('Red apple: You shake the red apple.');
       Proxy.ExpectString('Yellow apple: You shake the yellow apple.');
       TestWorld.Perform('shake grapes and apples THAT ARE NOT green from plate on table', TestPlayer);

       Proxy.ExpectDone();

       Proxy.ExpectString('Kitchen');
       Proxy.WaitUntilSubstring('On the fruit plate are green grapes.');
       Proxy.SkipEverything();
       TestWorld.Perform('look', TestPlayer);
       Proxy.StopSkipping();

       Proxy.ExpectString('Red grapes: You shake the red grapes.');
       Proxy.ExpectString('Green grapes: You shake the green grapes.');
       Proxy.ExpectString('Rotten grapes: You shake the rotten grapes.');
       Proxy.ExpectString('Red apple: You shake the red apple.');
       Proxy.ExpectString('Yellow apple: You shake the yellow apple.');
       Proxy.ExpectString('Banana: You shake the banana.');
       Proxy.ExpectString('Blueberries: You shake the blueberries.');
       Proxy.ExpectString('Pineapple: You shake the pineapple.');
       Proxy.ExpectString('Kiwi: You shake the kiwi.');
       TestWorld.Perform('shake grapes and all apples from plate but green plus bananas and all berries THAT ARE NOT strawberries from box and pineapple and kiwi from crate', TestPlayer);
       Proxy.ExpectDone();

       Proxy.ExpectString('Red grapes: You shake the red grapes.');
       Proxy.ExpectString('Green grapes: You shake the green grapes.');
       Proxy.ExpectString('Rotten grapes: You shake the rotten grapes.');
       Proxy.ExpectString('Red apple: You shake the red apple.');
       Proxy.ExpectString('Yellow apple: You shake the yellow apple.');
       Proxy.ExpectString('Banana: You shake the banana.');
       Proxy.ExpectString('Blueberries: You shake the blueberries.');
       Proxy.ExpectString('Pineapple: You shake the pineapple.');
       Proxy.ExpectString('Kiwi: You shake the kiwi.');
       TestWorld.Perform('shake grapes, and all of the apples from plate but green, plus bananas, and all of the berries that are not strawberries from box, and pineapple from crate, and kiwi from crate', TestPlayer);
       Proxy.ExpectDone();

       Proxy.WaitUntilString('Taken.');
       Proxy.WaitUntilString('Kitchen');
       Proxy.SkipEverything();
       TestWorld.Perform('north and east and south and take sack and south and south and take bag of holding and north and north and north and west and south', TestPlayer);
       Proxy.StopSkipping();

       Proxy.ExpectString('Which bag do you mean, the black garbage bag, the elongated brown sack, or the embroidered bag of holding labeled Tester?');
       TestWorld.Perform('examine bag', TestPlayer);

       Proxy.ExpectString('Placed in the garbage bag.');
       TestWorld.Perform('put brown sack in black bag', TestPlayer);

       Proxy.ExpectString('Which bag do you mean, the black garbage bag, the elongated brown sack, or the embroidered bag of holding labeled Tester?');
       TestWorld.Perform('examine bag', TestPlayer);

       Proxy.ExpectString('Taken.');
       TestWorld.Perform('take garbage bag', TestPlayer);

       Proxy.ExpectString('Which bag do you mean, the embroidered bag of holding labeled Tester, the black garbage bag, or the elongated brown sack?');
       TestWorld.Perform('examine bag', TestPlayer);

       Proxy.ExpectString('You are carrying:');
       Proxy.ExpectString('  A bag of holding.');
       Proxy.ExpectString('  A garbage bag.');
       Proxy.ExpectString('  The garbage bag contains:');
       Proxy.ExpectString('    Rotten grapes.');
       Proxy.ExpectString('    A rotten kiwi.');
       Proxy.ExpectString('    A rotten pineapple.');
       Proxy.ExpectString('    A brown sack.');
       Proxy.ExpectString('    The brown sack contains:');
       Proxy.ExpectString('      A clove of garlic.');
       Proxy.ExpectString('      A lunch.');
       Proxy.ExpectString('      A wooden spoon.');
       TestWorld.Perform('i', TestPlayer);

       Proxy.ExpectDone();

       Proxy.WaitUntilString('Mount Olympus');
       Proxy.SkipEverything();
       TestWorld.Perform('north; up', TestPlayer);
       Proxy.StopSkipping();

       Proxy.ExpectString('Looking up, you see a sky. The sky is clear.');
       TestWorld.Perform('look up', TestPlayer);

       Proxy.ExpectString('Looking down, you see a ground. The ground is a flat surface of stone. There is an opening there.');
       TestWorld.Perform('look down', TestPlayer);

       Proxy.ExpectString('The sky is above.');
       TestWorld.Perform('find sky', TestPlayer);

       Proxy.ExpectString('The opening is in the ground.');
       TestWorld.Perform('find opening', TestPlayer);

       Proxy.ExpectString('The sun is above, in the blue sky.');
       TestWorld.Perform('find sun', TestPlayer);

       Proxy.ExpectString('The opening is circular.');
       TestWorld.Perform('x opening', TestPlayer);

       Proxy.ExpectString('The sun is bright.');
       TestWorld.Perform('x sun', TestPlayer);

       Proxy.ExpectString('The sky contains:');
       Proxy.ExpectString('  A sun.');
       TestWorld.Perform('l in sky', TestPlayer);

       Proxy.ExpectString('You are under the sky.');
       TestWorld.Perform('l under sky', TestPlayer);

       Proxy.ExpectString('You are under the sun.');
       TestWorld.Perform('l under sun', TestPlayer);

       Proxy.ExpectString('The sky is too far away (above).');
       TestWorld.Perform('take sky', TestPlayer);

       Proxy.ExpectString('The sun is too far away (above).');
       TestWorld.Perform('take sun', TestPlayer);

       Proxy.ExpectDone();

       Proxy.ExpectString('(through the opening)');
       Proxy.ExpectString('Flower room');
       Proxy.SkipEverything();
       TestWorld.Perform('down', TestPlayer);
       Proxy.StopSkipping();

       Proxy.ExpectString('Looking up, you see:');
       Proxy.ExpectString('Mount Olympus');
       Proxy.SkipEverything();
       TestWorld.Perform('look up', TestPlayer);
       Proxy.StopSkipping();
     
       Proxy.ExpectString('That would prove rather challenging given where the garbage bag is relative to yourself.');
       TestWorld.Perform('enter garbage bag', TestPlayer);

       Proxy.ExpectString('Rotten grapes: (first taking the rotten grapes)');
       Proxy.ExpectString('Rotten grapes: Taken.');
       Proxy.ExpectString('Rotten grapes: Dropped.');
       Proxy.ExpectString('Rotten kiwi: (first taking the rotten kiwi)');
       Proxy.ExpectString('Rotten kiwi: Taken.');
       Proxy.ExpectString('Rotten kiwi: Dropped.');
       Proxy.ExpectString('Rotten pineapple: (first taking the rotten pineapple)');
       Proxy.ExpectString('Rotten pineapple: Taken.');
       Proxy.ExpectString('Rotten pineapple: Dropped.');
       Proxy.ExpectString('Brown sack: (first taking the brown sack)');
       Proxy.ExpectString('Brown sack: Taken.');
       Proxy.ExpectString('Brown sack: Dropped.');
       Proxy.ExpectString('Garbage bag: Dropped.');
       Proxy.ExpectString('');
       Proxy.ExpectString('You cannot enter the garbage bag. There is not enough room in the garbage bag for you.');
       TestWorld.Perform('drop all from garbage bag and garbage bag then enter garbage bag', TestPlayer);

       Proxy.ExpectString('The bag has the name "Tester" embroidered around its rim.');
       Proxy.ExpectString('The bag of holding is empty.');
       TestWorld.Perform('look in bag of holding', TestPlayer);

       Proxy.ExpectString('Dropped.');
       Proxy.ExpectString('');
       Proxy.ExpectString('In the bag of holding (at the flower room)');
       Proxy.ExpectString('The bag has the name "Tester" embroidered around its rim.');
       TestWorld.Perform('drop bag of holding then enter bag of holding', TestPlayer);

       Proxy.ExpectString('In the bag of holding (at the flower room)');
       Proxy.ExpectString('The bag has the name "Tester" embroidered around its rim.');
       TestWorld.Perform('look', TestPlayer);

       Proxy.ExpectDone();

       Proxy.Test('End');
   end;

begin
   Writeln('MECHANICS I');
   RunMechanicsHarness(@InitTestEden, @RunTest, {$IFDEF PLAY_IN_TEST_EDEN} True {$ELSE} False {$ENDIF});
end;

end.