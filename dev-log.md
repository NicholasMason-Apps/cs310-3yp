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