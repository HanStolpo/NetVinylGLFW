NetVinylGLFW
====================
This is a small experiment application used to explore functional reactive game programming in Haskell using the Netwire, Vinyl, Vinyl-GL, 
Lens, Linear and GLFW packages. I is also did it to have something for show and tell at my functional programming users group.

**Disclaimer**: If you accidentally stumble upon this page do take note that I am still a novice Haskaller, and that I am capturing 
here my limited understanding of things, but hopefully you might find something valuable here like I have when I stumbled upon so 
many other resources. On that note here is a list of some resources (that I can remember) that helped me.
* [Postmodern Haskell and OpenGL: Introducing vinyl-gl](http://www.arcadianvisions.com/blog/?p=388)
* [Vinyl: Modern Records for Haskell](http://www.jonmsterling.com/posts/2013-04-06-vinyl-modern-records-for-haskell.html)
* [Haskell/Understanding arrows](http://en.wikibooks.org/wiki/Haskell/Understanding_arrows)
* [Learning Modern 3D Graphics Programming](http://www.arcsynthesis.org/gltut/)
* [OpenGL in Haskell: GLFW-b Boilerplate](http://www.tapdancinggoats.com/opengl-in-haskell-glfw-b-boilerplate.htm)
* [Modern OpenGL with Haskell](http://www.arcadianvisions.com/blog/?p=224)
* [Asteroids & Netwire](http://ocharles.org.uk/blog/posts/2013-08-18-asteroids-in-netwire.html)
* [Getting Started with Netwire and SDL](http://ocharles.org.uk/blog/posts/2013-08-01-getting-started-with-netwire-and-sdl.html)

Why
==================
*Why Haskell?* I wanted to learn functional programming because its different than my background (OOP, imperitive, C++). At first I picked
Haskell by accident but stuck to it because it was, still is, a bit scary and I like the air of "computer science formalism" it has about it.
Oh and also I am getting the feeling if I ever even halfway master it I will become a ninja with safety goggles.

*Why game programming?* Well why not? It is I guess one of the more fun ways to explore a new domain and I sort of do something similar for
my day job. So for good or bad I know what kinds of concepts I would like to map to the functional space.

*Why OpenGL?* Haskell has support for it, and it follows more closely the Haskell community's tenet of portability than lets say the other
choice.

*Why Vinyl and Vinyl-GL?* The road to Vinyl leads through Vinyl-GL, and to it from the fact that one does not use the fix function pipeline
any more, even though that is what it seems like usually used and supported in the community. It comes down to using shaders and marshaling
a lot of data to OpenGL and this is a pain and error prone in any language or platform except if you build sensible abstractions. I was about
to try and write my own (the blind leading the blind?) when I came across a guys blog where he had an example using Vinyl-GL and it was 
exactly what I wanted.

*Why FRP and why Netwire and why arrows?*
