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