# Term 1 Week 1
- Followed the apecs tutorial at https://github.com/jonascarpay/apecs/blob/master/examples/Shmup.md to help understand how apecs works, as well as have a code base to work off of
- Implemented quad-directional movement
- Began looking into how to implement boundary-box collision systems for the player

## T1W1 Summary & Experience

Overall, the beginning of dev with apecs has been really good. It is designed in such a way that streamlines the game dev process, not just the Haskell game dev process, by giving you easy-to-use tools for game drawing and updating components

Defining new components is very easy, and alleviates the lack of mutable state for all types of components for you

Adapting how you work with game dev code from a more GUI focused system with things like Unity or Unreal to a more programatical system took some time, but thanks to prior experience with Monogame, this was not a big issue, and on top of that apecs is designed to minimise this issue with said systems and functions like `cmap` which allow you to programatically change specific components within the game world

# Term 1 Week 2

- Issue arose from the lack of being made aware that rendering in Gloss is done centre of mass, rather than top-left. I.e., for a Square of size 10x10, it is rendered such that its center is placed at (0,0), instead of its top-left corner being at (0,0). This is different to many systems already out there (e.g. Monogame)
- Big issue with boundary box collision is to do with how apecs works under the hood. For boundary box collision, two attempts have been made: attempting to pre-emptively check for a collision in the event handler which was ineffective since it seems that it is only considered once the key is lifted; and doing a System' () function for it, which also has been ineffective
    - May need to split functions up into separate ones for checking a collision overlap, that may work
    - also look into how `clampPlayer` works to diagnose

- After implementing animation rendering, I learnt a lot:
    - Learnt about how to do clipping and also only loading things once from IO
        - Done by storing the loaded sprite as an Image and its RBG data and then cropping rectroactively to get the Picture for an animation frame
    - Also learnt more about apecs and how apecs allows for component updates and exposes the entity
        - Naturally, only one update to one component can occur from the result of cmap or the return value of cmapM
        - However using cmapM_, since we do not care about the return value, we can access the entity itself and use set to modify multiple components through side effects
        - Whilst this violates functional best practices, there is no alternative, so from now on code will make use of cmapM_ as little as possible

# Term 1 Week 3
- Since time is on my side, doing procedural generation early
    - Basing it off the gungeon procedural generation since it is not only functional and really well implemented, but also uses a tree-like structure
    - Sourcing information from https://www.boristhebrave.com/2019/07/28/dungeon-generation-in-enter-the-gungeon/, which is an article detailing the decompilation of Enter the Gungeon
- Completed an initial function for generating the abstract game tree. Since this did not interact with any of the apecs system, it was relatively simple and streamlined to implement. Knowledge from CS141 was very helpful, since we covered trees and manipulating trees, zipping, etc.
- Started implementation of tree -> map conversion, although need to devise how each room will be represented

for tree -> map conversion, most likely use a constrained BFS

for unit tests, could use it to "simulate" the game world and trivially test if inputs such as move right, up, etc. are handled appropriately.
if time does not permit, make sure to discuss it in the report, saying how a purely functional language helps to simulate these trivially

# Term 1 Week 4
- Started truly developing the tree to map conversion
- Overall it has not been too bad - has taught me a lot of what it means to work in a monadic state (e.g. IO) and how much power (and also issues it brings). E.g. when working in a monadic context you get the power of side effects, but that in itself is also a downside, especially when you have a function which takes a monadic function, e.g. `bfsM` since that means whoever uses the function can do any sort of side effect they like
- Broken it down into multiple functions, but a single entry function called `generateMap`
    - Works by first generating the Tree since it is in a monadic context, and then using a monadic BFS it applies a function which converts the current room in the tree it is at into an entity for `apecs`, but also applies a conditional check to ensure no overlaps occur between rooms by adjusting the position accordingly
- Breaking it down into functions as helped identify the errors in my code, although attempting to debug these has been extremely tricky
    - `bfsM` and the code for converting a game room entity into tiles works fine, but the actual positining code is flawed. Multiple attempts at differnet implementations have been made, but both attempts fundamentally use a single `cfoldM`. Fundamentally the position code without the actual intersection fixing is flawed, most likely to do with position calculations
- Also noticed that performance degrades as the program is ran for longer. Not sure what causes it - whether it is a apecs/gloss issue, or an issue with my code
- Also need to check if code is efficient. Theoretically since I am running the `initialize` function once the room gen is only done once, but want to clarify with supervisor

# Term 1 Week 5
Talking points for meeting:
- Discuss performance issues, sources, and how to solve (e.g. ensured it is not to do with the entities in the system, it is the drawing)
- ask about sources for the report (e.g. is the article on procedural generation appropriate even tho it is not a paper?)
- To fix performance issues, adjusted the sprite/animation system from using `Image PixelRGBA8` to instead create the `Picture`s instantly, and for animations splice the image once and store the `Picture`s in a Vector for O(1) lookup. This was extremely effective
    - A potential reason for this is that since the final `Picture`s in the original set up for animations were being computed each frame, and then discarded when the animation switched, Gloss or apecs could not cache them, resulting in substantial memory overhead as more and more Pictures were being created
- Also implemented occlusion culling, which simply checks if a sprite is outside the viewport and just does not draw it.
- Both of these in conjunction were very effective in fixing performance
- Added actual sprite images into the map generation. Was not that hard - just had to consider edge cases such as picking the correct sprite for a corner in each section of a room, etc.
- Also changed MoveDirection to use a Set instead of a Maybe to fix some bugs with animations when pressing multiple keys. Used a Set over a List or Vector since we have O(log n) insertion, deletion, and member checking compared to O(n)

# Term 1 Week 6
Talking points for meeting:
- Discuss best way to do the animation storing - currently it duplicates data which is bad, but also do not want to interact with IO more than is necessary since memory fetching is slow
- Discuss the best way to do the game space storing - when entering combat i envision the scene being switched to something else (e.g. a combat scene) with different background, different environment of input handling, etc. so from that what would be the best "stuff" to store to preserve the information of the game without using a lot of memory but also considering in transition times?
    - essentially want a near instant transition from dungeon to combat and vice versa, so should i store all the entities of the game map and only draw them when it is a game scene, or store the game rooms and then each time going into combat delete the tiles and then readd them into the component space?? not really sure
- also never done a turn based system before, so want to ask as well best way to store the e.g. turn order. was thinking a Vector with a pointer to who to use in the turn? so not only expandable to more than 2 enemies/player, but also get O(1) access with a vector
    - THIS ALSO BRINGS UP ANOTHER POINT of just generally how best is it to store things in apecs. basically compared to smth like c++ you dont have pointers or any explicit pass by reference (afaik everything is pass by value), so what is the best way to reduce redundant memory space (e.g. the animation stuff)

- spent a lot of time trying to fix the movement issues but nothing worked. can be seen in the commit history and what i reverted back to. for now just keeping it as it is and doing enemies. the closest i got was keeping a global set of the keys pressed and updating the player velocity exclusively each frame and then calculating whether collisions occurred for every entity which would need it, but this had edge cases of allowing the player to clip.

IORef - basically a reference
    - in base, called Data.IORef
    - look into it, and see if it is worth using over Global in apecs

    - would be nice to store scenes as a single thing and be able to flip between them almost instantly
    - may be good to use IORef here - map between scene names and IORef which points to apecs objects to be able to load

- One thing which may be good to do is separate boundary box collision from the sprite size
    - E.g. create a new BoundaryBox component which specifies the width and height of the Box from the entity's centre position

in global store keep track of current scene
separate out draw and step functions to pattern match on this scene and offload as necessary

currentScene <- get glob al
case currentScene of
	menu -> renderMenu
	...

# Term 1 Week 7
- Making use of the global store for sprites significantly reduced memory usage. Although the memory usage to begin with was low, it went from ~60MB down to ~35MB which is very good
- Making use of `Global` and `Unique` stores for game state and also the combat was extremely beneficial
- Turn based combat works by using a `Unique` store for the `CombatPlayer` and `CombatEnemy`
    - Combat player is a self-contained player entity only rendered in combat scenes. has no interaction with the dungeon player. Stores its own sprite and position components
    - Combat enemy is used to store the entity reference for the actual enemy in the world such that they can be deleted on a victory, and also stores its own position information, sprite information, etc.
- Fundamentally at the moment works by using four step functions, and stepping through each phase of a combat sequence, making use of animatiions which do not loop, and check when they have finished to progress the combat order
    - The `Animation` record was extended to have a looping property to allow animations to loop or not

## Meeting notes
- continue implementation of game, making it more complete and fleshed out
For progress report:
- come up with a first run at the conversation you want in the final report
- layout my experience with using a FP for games. what was hard, what was easy, the notion of an ECS
- updated timeline should definitely include 3d
- consider switching to sld2/glfw away from gloss to allow more control over the rendering and viewport, makes a nice story and discussion for progress report
- look into apecs paper for literature review
- pull from game programming haskell book
- use apecs paper for searching papers which reference it
- reference gungeon article about procedural generation
    - make use of diagrams, annotated pictures, etc. about possible generations from MY game and how it goes about that'

# Term 1 Week 9
- Split gloss and sdl2 code into separate app directories such that we can run the gloss or sdl version with the same apecs backend
- will need to change `package.yaml` to have two executables

- switching from gloss to sdl2 is very challenging since they fundamentally use different coordinate systems - gloss uses (0,0) as centre and +y = UP, whereas SDL uses (0,0) as top left and -y = UP

- to help with agnostic system, completely restarting sdl implementation, and fundamentally changing type systems to use an algebraic `RendererSystem` type which can be used to pattern match

# Winter Holidays
- Got SDL Renderer working and separated code to allow for both Gloss and SDL
- Important to note that SDL is much faster than Gloss. SDL is able to handle all tiles and walls wtih no occlusion culling at all, whereas Gloss would run at 1 FPS
    - Probably to do with Gloss' abstractions and vector drawing? Not sure. just important to note
    - Useful for why use SDL over gloss
- This was done by taking what was already there (so Gloss based code) and treating that as the absolute truth
    - This meant no changes were made to actual game logic since they all used the same
    - Only difference is at the rendering stage. each position is offset based on the fact that SDL uses top left as (0,0), and so every thing is offset by half of the viewport width and height, and also centered onto the player
        - look at `worldToScreen` function for this
    - This means we have a completely agnostic back end for game logic, and can plug and play with each renderer as necessary
- Completion of core gameplay loop (adding ladders) was very simple - reused transition logic, abstracted transition events into a data type to pattern match on and run event based on it, and just deleted all the game space components and recreated the game map
    - Did try storing the event inside the transition itself, however since the World data type is defined using Template Haskell (so at compile time), unable to store the `System World a` type inside the `Transition`, and so had to resort to this

# Term 2 Week 1
- Got Raylib working in isolation and got a very basic 3D demo in a separate executable - this is forming the basis of the 3D renderer
- Attempting to adapt this game, however, is causing issues. Since the game logic assumes a 2d plane, the movements are fixed (i.e. pressing UP always increases the y axis). This breaks in first person 3D since the camera changes your forward direction
    - Potential idea for a solution is to globally store and always use a camera rotation, and in the position calculations include this as a multiplier. The intent is that for 2d the camera would change nothing (i.e. multiplier of 1), however for 3D it would adjust the movement to account for the camera changing direction
- Camera movement with mouse does work, just not inside WSL